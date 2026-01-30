(** Inlined helper lib *)

[@@@ocamlformat "break-string-literals=never"]

(*$
  let pascal_to_snake s =
    let buf = Buffer.create (String.length s * 2) in
    String.iteri
      (fun i c ->
        if Char.uppercase_ascii c = c && c <> '_' && i > 0
        then Buffer.add_char buf '_';
        Buffer.add_char buf (Char.lowercase_ascii c))
      s;
    Buffer.contents buf
  in

  let escape_quote s =
    let buf = Buffer.create (String.length s * 2) in
    String.iter
      (function
        | '\\' -> Buffer.add_string buf "\\\\"
        | '"' -> Buffer.add_string buf "\\\""
        | c -> Buffer.add_char buf c)
      s;
    Buffer.contents buf
  in

  let lib_names = [ "DefaultMap"; "Option" ] in
  let read_lib_content lib =
    let file_path = Sys.getcwd () ^ "/helper_lib/" ^ lib ^ ".ts" in
    CCIO.File.read_exn file_path
  in
  let lib_content =
    List.map
      (fun lib_name -> escape_quote (read_lib_content lib_name))
      lib_names
  in
  let var_names = List.map pascal_to_snake lib_names in
  let gen_code var lib_content =
    [%string "
let %{var} =
\"%{lib_content}\"
;;"]
  in
  let codes = List.map2 gen_code var_names lib_content in
  List.iter print_endline codes
*)
let default_map =
"export class DefaultMap<K, V extends NonNullable<unknown>>
	implements Map<K, V>
{
	private map: Map<K, V>;

	constructor(
		private readonly defaultFactory: () => V,
		entries?: Iterable<readonly [K, V]>,
	) {
		this.map = new Map(entries);
	}

	get size(): number {
		return this.map.size;
	}

	get(key: K): V {
		if (this.map.has(key)) {
			const value = this.map.get(key);
			if (value === undefined) {
				throw new Error(\"DefaultMap contains undefined value\");
			}
			return value;
		}
		const defaultV = this.defaultFactory();
		this.map.set(key, defaultV);
		return defaultV;
	}

	set(key: K, value: V): this {
		this.map.set(key, value);
		return this;
	}

	has(key: K): boolean {
		return this.map.has(key);
	}

	delete(key: K): boolean {
		return this.map.delete(key);
	}

	clear(): void {
		this.map.clear();
	}

	forEach(
		callbackfn: (value: V, key: K, map: Map<K, V>) => void,
		thisArg?: unknown,
	): void {
		this.map.forEach(callbackfn, thisArg);
	}

	keys(): IterableIterator<K> {
		return this.map.keys();
	}

	values(): IterableIterator<V> {
		return this.map.values();
	}

	entries(): IterableIterator<[K, V]> {
		return this.map.entries();
	}

	[Symbol.iterator](): IterableIterator<[K, V]> {
		return this.map[Symbol.iterator]();
	}

	get [Symbol.toStringTag](): string {
		return \"DefaultMap\";
	}
}
"
;;

let option =
"export type Option<T> = { tag: \"Some\"; payload: T } | null;
"
;;
(*$*)
