type options = {
  no_default_ignore : bool;
  ignores : string list;
}

let default_options = { no_default_ignore = false; ignores = [] }

(* ==========================================================================
   Filesystem walk
   ========================================================================== *)

let walk_files root =
  let rec go acc dir =
    let handle = Unix.opendir dir in
    let finish () = try Unix.closedir handle with _ -> () in
    let rec loop acc =
      match Unix.readdir handle with
      | exception End_of_file -> acc
      | ("." | "..") -> loop acc
      | name ->
        let path = Filename.concat dir name in
        let acc =
          match (Unix.lstat path).st_kind with
          | S_DIR -> go acc path
          | S_REG -> path :: acc
          | _ -> acc
        in
        loop acc
    in
    match loop acc with
    | exception e -> finish (); raise e
    | result -> finish (); result
  in
  List.rev (go [] root)

(* ==========================================================================
   Gitignore: shell out to `git check-ignore --stdin`
   ========================================================================== *)

let check_git_ignored ~root rels =
  let set = Hashtbl.create 16 in
  match rels with
  | [] -> set
  | _ ->
    let stdin_r, stdin_w = Unix.pipe ~cloexec:true () in
    let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
    let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
    let prev_sigpipe = Sys.signal Sys.sigpipe Sys.Signal_ignore in
    let pid =
      Unix.create_process "git"
        [| "git"; "-C"; root; "check-ignore"; "--stdin" |]
        stdin_r stdout_w devnull
    in
    Unix.close stdin_r;
    Unix.close stdout_w;
    Unix.close devnull;
    let payload = String.concat "\n" rels ^ "\n" in
    let plen = String.length payload in
    let rec write_all off =
      if off >= plen then ()
      else
        match Unix.write_substring stdin_w payload off (plen - off) with
        | exception Unix.Unix_error ((EPIPE | EBADF), _, _) -> ()
        | 0 -> ()
        | n -> write_all (off + n)
    in
    write_all 0;
    (try Unix.close stdin_w with Unix.Unix_error _ -> ());
    let buf = Buffer.create 256 in
    let bytes = Bytes.create 4096 in
    let rec read_all () =
      match Unix.read stdout_r bytes 0 4096 with
      | exception Unix.Unix_error (EINTR, _, _) -> read_all ()
      | 0 -> ()
      | n ->
        Buffer.add_subbytes buf bytes 0 n;
        read_all ()
    in
    read_all ();
    (try Unix.close stdout_r with Unix.Unix_error _ -> ());
    let _, status = Unix.waitpid [] pid in
    Sys.set_signal Sys.sigpipe prev_sigpipe;
    match status with
    | WEXITED (0 | 1) ->
      Buffer.contents buf
      |> String.split_on_char '\n'
      |> List.iter (fun line ->
             if line <> "" then Hashtbl.replace set line ());
      set
    (* Not a git repo or git unavailable: behave as if nothing is ignored. *)
    | _ -> set

(* ==========================================================================
   Tree construction
   ========================================================================== *)

type node =
  | Dir of (string, node) Hashtbl.t
  | Leaf of Yojson.Safe.t

let new_dir () = Hashtbl.create 8

let rec node_to_yojson = function
  | Leaf v -> v
  | Dir tbl ->
    let kvs =
      Hashtbl.fold (fun k v acc -> (k, node_to_yojson v) :: acc) tbl []
    in
    `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b) kvs)

let split_path rel =
  String.split_on_char '/' rel |> List.filter (fun s -> s <> "")

let strip_extension name =
  match String.rindex_opt name '.' with
  | Some i when i > 0 -> String.sub name 0 i
  | _ -> name

let rec yaml_to_yojson (v : Yaml.value) : Yojson.Safe.t =
  match v with
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Float f ->
    if Float.is_integer f && Float.abs f < 1e18 then `Int (int_of_float f)
    else `Float f
  | `String s -> `String s
  | `A xs -> `List (List.map yaml_to_yojson xs)
  | `O kvs -> `Assoc (List.map (fun (k, v) -> (k, yaml_to_yojson v)) kvs)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let load_value abs file_name =
  let content = read_file abs in
  let ext =
    match String.rindex_opt file_name '.' with
    | Some i when i > 0 ->
      Some
        (String.lowercase_ascii
           (String.sub file_name (i + 1) (String.length file_name - i - 1)))
    | _ -> None
  in
  match ext with
  | Some "json" ->
    (try Yojson.Safe.from_string content
     with Yojson.Json_error msg ->
       failwith (Printf.sprintf "parsing JSON %s: %s" abs msg))
  | Some ("yaml" | "yml") ->
    (match Yaml.of_string content with
     | Ok v -> yaml_to_yojson v
     | Error (`Msg msg) ->
       failwith (Printf.sprintf "parsing YAML %s: %s" abs msg))
  | _ -> `String content

let insert_file root rel_components abs =
  match List.rev rel_components with
  | [] -> failwith (Printf.sprintf "empty relative path for %s" abs)
  | file_name :: rev_dirs ->
    let dirs = List.rev rev_dirs in
    let cursor =
      List.fold_left
        (fun cur dir ->
          match Hashtbl.find_opt cur dir with
          | Some (Dir sub) -> sub
          | Some (Leaf _) ->
            failwith
              (Printf.sprintf
                 "key collision at %s: directory conflicts with existing value"
                 abs)
          | None ->
            let sub = new_dir () in
            Hashtbl.add cur dir (Dir sub);
            sub)
        root dirs
    in
    let key = strip_extension file_name in
    if Hashtbl.mem cursor key then
      failwith
        (Printf.sprintf "key collision: %s maps to key %s which already exists"
           abs key);
    Hashtbl.add cursor key (Leaf (load_value abs file_name))

(* ==========================================================================
   build_tree
   ========================================================================== *)

let relativize ~root abs =
  let prefix =
    if String.length root > 0 && root.[String.length root - 1] = '/' then root
    else root ^ "/"
  in
  let plen = String.length prefix in
  if String.length abs >= plen && String.sub abs 0 plen = prefix then
    String.sub abs plen (String.length abs - plen)
  else abs

let has_dot_component rel =
  List.exists
    (fun seg -> String.length seg > 0 && seg.[0] = '.')
    (split_path rel)

let build_tree root opts =
  let root = Unix.realpath root in
  let abs_files = walk_files root in
  let entries = List.map (fun abs -> (abs, relativize ~root abs)) abs_files in

  let use_defaults = not opts.no_default_ignore in
  let entries =
    if use_defaults then
      List.filter (fun (_, rel) -> not (has_dot_component rel)) entries
    else entries
  in
  let entries =
    if use_defaults then
      let ignored = check_git_ignored ~root (List.map snd entries) in
      List.filter (fun (_, rel) -> not (Hashtbl.mem ignored rel)) entries
    else entries
  in
  let entries =
    match opts.ignores with
    | [] -> entries
    | pats ->
      let res =
        List.map
          (fun p -> Re.compile (Re.Glob.glob ~anchored:true p))
          pats
      in
      List.filter
        (fun (_, rel) -> not (List.exists (fun re -> Re.execp re rel) res))
        entries
  in

  let root_dir = new_dir () in
  List.iter
    (fun (abs, rel) -> insert_file root_dir (split_path rel) abs)
    entries;
  node_to_yojson (Dir root_dir)
