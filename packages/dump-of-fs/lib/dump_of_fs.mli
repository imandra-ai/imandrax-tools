type options = {
  no_default_ignore : bool;
  ignores : string list;
}

val default_options : options

(** [build_tree root opts] walks [root] and returns a JSON tree whose
    keys mirror the filesystem layout (extensions stripped). JSON/YAML
    files are parsed and inlined. Raises [Failure] on key collisions
    or parse errors. *)
val build_tree : string -> options -> Yojson.Safe.t
