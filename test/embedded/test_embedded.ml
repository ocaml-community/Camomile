(* Tests that the embedded API works without any filesystem data dependency. *)

open Camomile_embedded

let check name result expected =
  if result <> expected then begin
    Printf.eprintf "FAIL: %s\n%!" name;
    exit 1
  end

(* UTF8 basic operations require no data files. *)
let () =
  let s = "hello" in
  check "UTF8.length" (Camomile.UTF8.length s) 5

(* general_category requires database/general_category.mar from embedded data. *)
let () =
  let cat = Camomile.UCharInfo.general_category in
  check "general_category 'A'" (cat (Camomile.UChar.of_char 'A')) `Lu;
  check "general_category 'a'" (cat (Camomile.UChar.of_char 'a')) `Ll;
  check "general_category '0'" (cat (Camomile.UChar.of_char '0')) `Nd;
  check "general_category ' '" (cat (Camomile.UChar.of_char ' ')) `Zs

(* NFC normalization requires database/decomposition.mar and related files. *)
let () =
  let module NF = Camomile.UNF.Make (Camomile.UTF8) in
  (* U+00E9 = é (precomposed); NFC of it should be itself. *)
  let e_acute = Camomile.UTF8.init 1 (fun _ -> Camomile.UChar.of_int 0xe9) in
  let nfc = NF.nfc e_acute in
  check "NFC of precomposed é" (Camomile.UTF8.compare e_acute nfc) 0;
  (* U+0065 U+0301 = e + combining acute; NFC should yield U+00E9. *)
  let e_plus_combining =
    Camomile.UTF8.init 2 (function
      | 0 -> Camomile.UChar.of_int 0x65
      | _ -> Camomile.UChar.of_int 0x301)
  in
  let nfc2 = NF.nfc e_plus_combining in
  check "NFC of e + combining acute" (Camomile.UTF8.compare e_acute nfc2) 0

(* CharEncoding.of_name requires charmaps/*.mar from embedded data. *)
let () =
  let module Conv = Camomile.CharEncoding.Make (Camomile.UTF8) in
  let enc = Camomile.CharEncoding.of_name "ASCII" in
  let s = "hello" in
  let encoded = Conv.encode enc s in
  let decoded = Conv.decode enc encoded in
  check "CharEncoding ASCII round-trip" (Camomile.UTF8.compare s decoded) 0

let () = print_endline "All embedded tests passed."
