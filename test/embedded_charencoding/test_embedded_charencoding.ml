(* Tests that charset conversion works with only the charmap data embedded. *)

open Camomile_embedded_charencoding

let check name result expected =
  if result <> expected then begin
    Printf.eprintf "FAIL: %s\n%!" name;
    exit 1
  end

module Conv = CharEncoding.Make (CamomileLib.UTF8)

(* charmaps/*.mar *)
let () =
  List.iter
    (fun name ->
      let enc = CharEncoding.of_name name in
      let s = "hello" in
      check
        (name ^ " round-trip")
        (CamomileLib.UTF8.compare s (Conv.decode enc (Conv.encode enc s)))
        0)
    ["ASCII"; "ISO-8859-1"; "UTF-8"]

(* Non-ASCII through a charmap that has to map code points around. *)
let () =
  let enc = CharEncoding.of_name "ISO-8859-1" in
  let e_acute = CamomileLib.UTF8.init 1 (fun _ -> CamomileLib.UChar.of_int 0xe9) in
  check "ISO-8859-1 encodes é as one byte" (Conv.encode enc e_acute) "\xe9";
  check "ISO-8859-1 decodes é"
    (CamomileLib.UTF8.compare e_acute (Conv.decode enc "\xe9"))
    0

(* Unknown encodings still report as such rather than as missing data. *)
let () =
  check "unknown encoding"
    (try
       ignore (CharEncoding.of_name "NO-SUCH-ENCODING");
       false
     with Not_found -> true)
    true

let () = print_endline "All embedded charencoding tests passed."
