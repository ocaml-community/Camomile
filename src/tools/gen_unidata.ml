(* Generates the OCaml types whose constructors are dictated by the Unicode
   data itself, so that a data refresh does not require editing sources. Kept
   separate from the parse_* tools, which link against camomileLib and would
   therefore be a cyclic dependency. *)

let range_pat =
  Str.regexp
    "\\([0-9A-Fa-f]+\\)\\.\\.\\([0-9A-Fa-f]+\\)[ \t]*;[ \t]*\\([^ \t]+\\)"

let num_pat = Str.regexp "\\([0-9A-Za-z]+\\)+[ \t]*;[ \t]*\\([^ \t]+\\)"

let script_names fname =
  let ic = open_in fname in
  let rec f names =
    try
      let s = input_line ic in
      if Str.string_match range_pat s 0 then f (Str.matched_group 3 s :: names)
      else if Str.string_match num_pat s 0 then
        f (Str.matched_group 2 s :: names)
      else f names
    with End_of_file ->
      close_in ic;
      names
  in
  List.sort_uniq Stdlib.compare (f [])

let constr name = "`" ^ String.capitalize_ascii name

let gen_script_type fname =
  let names = script_names fname in
  Printf.printf "type t = [\n";
  List.iter (fun name -> Printf.printf "  | %s\n" (constr name)) names;
  Printf.printf "]\n\n";

  Printf.printf "let name_of_script_type = function\n";
  List.iter
    (fun name ->
      Printf.printf "  | %s -> %S\n" (constr name) (String.lowercase_ascii name))
    names;
  Printf.printf "\n\n";

  Printf.printf
    "let script_type_of_name name = match String.lowercase_ascii name with\n";
  List.iter
    (fun name ->
      Printf.printf "  | %S -> %s\n" (String.lowercase_ascii name) (constr name))
    names;
  Printf.printf "  | _ -> raise Not_found\n\n";

  Printf.printf "let num_of_script = function\n";
  List.iteri
    (fun pos name -> Printf.printf "  | %s -> %d\n" (constr name) pos)
    names;
  Printf.printf "\n\n";

  Printf.printf "let script_of_num = function\n";
  List.iteri
    (fun pos name -> Printf.printf "  | %d -> %s\n" pos (constr name))
    names;
  Printf.printf "  | _ -> raise Not_found\n\n"

let age_pat = Str.regexp "[^#]*;[ \t]*\\([0-9]+\\)\\.\\([0-9]+\\)"

let versions fname =
  let ic = open_in fname in
  let rec f versions =
    try
      let s = input_line ic in
      if Str.string_match age_pat s 0 then
        f
          (( int_of_string (Str.matched_group 1 s),
             int_of_string (Str.matched_group 2 s) )
          :: versions)
      else f versions
    with End_of_file ->
      close_in ic;
      versions
  in
  List.sort_uniq Stdlib.compare (f [])

(* Versions are encoded as a single byte in the age table, ascending so that
   [older] is a plain char comparison. `Nc sorts last: everything is older
   than an unassigned code point. *)
let gen_version_type fname =
  let versions = versions fname in
  let constr (major, minor) = Printf.sprintf "`v%d_%d" major minor in
  if List.length versions > 254 then failwith "too many Unicode versions";
  Printf.printf "type t = [\n  | `Nc\n";
  List.iter (fun v -> Printf.printf "  | %s\n" (constr v)) versions;
  Printf.printf "]\n\n";

  Printf.printf "let undefined = '\\xff'\n\n";

  Printf.printf "let char_of_version = function\n";
  List.iteri
    (fun pos v -> Printf.printf "  | %s -> '\\x%02x'\n" (constr v) pos)
    versions;
  Printf.printf "  | `Nc -> undefined\n\n";

  Printf.printf "let version_of_char = function\n";
  List.iteri
    (fun pos v -> Printf.printf "  | '\\x%02x' -> %s\n" pos (constr v))
    versions;
  Printf.printf "  | c when c = undefined -> `Nc\n";
  Printf.printf
    "  | c -> failwith (Printf.sprintf \"unknown version v%%x\" (Char.code c))\n\n";

  Printf.printf "let char_of_major_minor = function\n";
  List.iteri
    (fun pos (major, minor) ->
      Printf.printf "  | %d, %d -> '\\x%02x'\n" major minor pos)
    versions;
  Printf.printf
    "  | major, minor -> failwith (Printf.sprintf \"unknown version v%%d.%%d\" \
     major minor)\n\n"

let () =
  match Sys.argv with
    | [| _; "--gen-script-type"; fname |] -> gen_script_type fname
    | [| _; "--gen-version-type"; fname |] -> gen_version_type fname
    | _ -> failwith "invalid command line"
