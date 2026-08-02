(* $Id: test-uCol.ml,v 1.13 2006/08/13 21:23:08 yori Exp $ *)
(* Copyright 2002,2003,2004,2005,2006 Yamagata Yoriyuki *)

open Camomile
open UPervasives
open Blender
open Printf
open TestUColJapanese

let blank = Str.regexp "[ \t]+"

(* The code points, up to the comment. The _SHORT files carry no comment at
   all, so the separator is optional. *)
let line_pat = Str.regexp "\\([^;#]+\\)"
let comment_pat = Str.regexp "^#.*"
let uchar_of_code code = uchar_of_int (int_of_string ("0x" ^ code))
let us_of_cs cs = List.map uchar_of_code cs

let parse_line line =
  if Str.string_match line_pat line 0 then (
    let s = Str.matched_group 1 line in
    let cs = Str.split blank s in
    let us = us_of_cs cs in
    UText.init (List.length us) (fun i -> List.nth us i))
  else invalid_arg (sprintf "Malformed_line %s:" line)

let print_char u = sprintf "%04X " (int_of_uchar u)

let print_text t =
  let buf = Buffer.create (5 * UText.length t) in
  UText.iter (fun u -> Buffer.add_string buf (print_char u)) t;
  Buffer.contents buf

let sgn_of i =
  if i < 0 then -1 else if i = 0 then 0 else if i > 0 then 1 else assert false

module Ucomp = UCol.Make (UText : UnicodeString.Type with type t = UText.t)

let uca ~desc variable c =
  let prev = ref (UText.init 0 (fun _ -> uchar_of_int 0)) in
  let prev_key = ref (Ucomp.sort_key ~variable !prev) in
  let prev_line = ref "" in
  try
    while true do
      let line = input_line c in
      if line = "" then ()
      else if Str.string_match comment_pat line 0 then ()
      else (
        let t = parse_line line in
        let t_key = Ucomp.sort_key ~variable t in
        let sgn = compare !prev_key t_key in
        let sgn1 = Ucomp.compare ~variable !prev t in
        let sgn2 = Ucomp.compare_with_key ~variable !prev_key t in
        let sgn3 = ~-(Ucomp.compare_with_key ~variable t_key !prev) in
        test ~desc ~body:(fun () ->
            expect_pass ~body:(fun () ->
                expect_true
                  ~msg:
                    (lazy
                      (sprintf
                         "the previous line is greater than the current:\n\
                          value: %i\n\
                          previous line:%s\n\
                          %s \n\
                          key %s\n\
                          current lins:%s\n\
                          %s \n\
                          key %s\n"
                         sgn !prev_line (print_text !prev)
                         (String.escaped !prev_key) line (print_text t)
                         (String.escaped t_key)))
                  (sgn <= 0);
                expect_true
                  ~msg:
                    (lazy
                      (sprintf
                         "comparison by compare is different from comparison \
                          by keys.\n\
                          value by compare: %i\n\
                          value by sort key: %i\n\
                          previous line:%s\n\
                          %s \n\
                          key %s\n\
                          current lins:%s\n\
                          %s \n\
                          key %s\n"
                         sgn1 sgn !prev_line (print_text !prev)
                         (String.escaped !prev_key) line (print_text t)
                         (String.escaped t_key)))
                  ((sgn > 0 && sgn1 > 0)
                  || (sgn = 0 && sgn1 = 0)
                  || (sgn < 0 && sgn1 < 0));
                expect_true
                  ~msg:
                    (lazy
                      (sprintf
                         "comparison by compare_with_key prev_key current is \
                          different from comparison by keys.\n\
                          value by compare_with_key prev_key current: %i\n\
                          value by sort key: %i\n\
                          previous line:%s\n\
                          %s \n\
                          key %s\n\
                          current lins:%s\n\
                          %s \n\
                          key %s\n"
                         sgn2 sgn !prev_line (print_text !prev)
                         (String.escaped !prev_key) line (print_text t)
                         (String.escaped t_key)))
                  ((sgn > 0 && sgn2 > 0)
                  || (sgn = 0 && sgn2 = 0)
                  || (sgn < 0 && sgn2 < 0));
                expect_true
                  ~msg:
                    (lazy
                      (sprintf
                         "comparison by compare_with_key current_key prev is \
                          different from comparison by keys.\n\
                          value by compare_with_key current_key prev: %i\n\
                          value by sort key: %i\n\
                          previous line:%s\n\
                          %s \n\
                          key %s\n\
                          current lins:%s\n\
                          %s \n\
                          key %s\n"
                         sgn3 sgn !prev_line (print_text !prev)
                         (String.escaped !prev_key) line (print_text t)
                         (String.escaped t_key)))
                  ((sgn > 0 && sgn3 > 0)
                  || (sgn = 0 && sgn3 = 0)
                  || (sgn < 0 && sgn3 < 0))));
        prev := t;
        prev_key := t_key;
        prev_line := line)
    done
  with End_of_file -> ()

(* The UCA conformance suites and the Thai word list below are known to fail
   against the current DUCET, so they only run under `dune build
   @uca-conformance`. Two independent causes:

   - Camomile drops completely ignorable characters before matching
     contractions, which defeats the blocking rule of UAX #10 S2.1.1-S2.1.3:
     `0B47 1D165 0B3E` wrongly contracts to 0B4B. Keeping them costs about as
     many failures elsewhere, so the fix is a rework of uCol, not a one-liner.

   - the Thai tailoring in src/locales/th.txt dates from 2001 and conflicts
     with the prevowel handling DUCET now does natively. *)
let conformance = Sys.getenv_opt "CAMOMILE_UCA_CONFORMANCE" <> None

let _ =
  if conformance then
    read_file
      (input_filename "unidata/CollationTest_SHIFTED_SHORT.txt")
      (uca ~desc:"Shifted" `Shifted)

let _ =
  if conformance then
    read_file
      (input_filename "unidata/CollationTest_NON_IGNORABLE_SHORT.txt")
      (uca ~desc:"Non ignorable" `Non_ignorable)

module UTF8Comp = UCol.Make (UTF8)

let print_text_utf8 t =
  let buf = Buffer.create (5 * UTF8.length t) in
  UTF8.iter (fun u -> Buffer.add_string buf (print_char u)) t;
  Buffer.contents buf

let locale_test ~desc ?variable ~locale c =
  let prev = ref "" in
  let prev_key = ref (UTF8Comp.sort_key ?variable ~locale "") in
  try
    while true do
      let line = input_line c in
      if Str.string_match comment_pat line 0 then ()
      else (
        let key = UTF8Comp.sort_key ?variable ~locale line in
        let sgn = sgn_of (UTF8Comp.compare ?variable ~locale !prev line) in
        let sgn1 = sgn_of (Stdlib.compare !prev_key key) in
        let sgn2 =
          sgn_of (UTF8Comp.compare_with_key ?variable ~locale !prev_key line)
        in
        let sgn3 =
          -sgn_of (UTF8Comp.compare_with_key ?variable ~locale key !prev)
        in
        test ~desc ~body:(fun () ->
            expect_pass ~body:(fun () ->
                expect_true
                  ~msg:
                    (lazy
                      (sprintf
                         "the previous key is greater than the current:\n\
                          value: %i\n\
                          previous: %s \n\
                          code : %s \n\
                          key %s\n\
                          current: %s \n\
                          code : %s \n\
                          key %s\n"
                         sgn !prev (print_text_utf8 !prev)
                         (String.escaped !prev_key) line (print_text_utf8 line)
                         (String.escaped key)))
                  (sgn1 <= 0);
                expect_true
                  ~msg:
                    (lazy
                      (sprintf
                         "The comparison results differ\n\
                          value: %i\n\
                          previous: %s \n\
                          code : %s \n\
                          key %s\n\
                          current: %s \n\
                          code : %s \n\
                          key %s\n\
                          previous - current comparison : %d\n\
                          previous key - current key comparison : %d\n\
                          previous key - current comparison : %d\n\
                          previous - current key comparison : %d\n"
                         sgn !prev (print_text_utf8 !prev)
                         (String.escaped !prev_key) line (print_text_utf8 line)
                         (String.escaped key) sgn sgn1 sgn2 sgn3))
                  (sgn = sgn1 && sgn1 = sgn2 && sgn2 = sgn3)));
        prev := line;
        prev_key := key)
    done
  with End_of_file -> ()

(*
let _ =
  read_file
    (input_filename "data/fr_CA")
    (locale_test ~desc:"Canadian French" ~variable:`Shift_Trimmed
       ~locale:"fr_CA")
*)

let _ =
  if conformance then
    read_file
      (input_filename "data/th18057")
      (locale_test ~desc:"Thai" ~variable:`Non_ignorable ~locale:"th_TH")

let test_list ~desc ?variable ~locale list =
  let rec loop prev prev_key = function
    | [] -> ()
    | t :: rest ->
        let key = UTF8Comp.sort_key ?variable ~locale t in
        let sgn = sgn_of (UTF8Comp.compare ?variable ~locale prev t) in
        let sgn1 = sgn_of (Stdlib.compare prev_key key) in
        let sgn2 =
          sgn_of (UTF8Comp.compare_with_key ?variable ~locale prev_key t)
        in
        let sgn3 =
          -sgn_of (UTF8Comp.compare_with_key ?variable ~locale key prev)
        in
        test ~desc ~body:(fun () ->
            expect_pass ~body:(fun () ->
                expect_true
                  ~msg:
                    (lazy
                      (sprintf
                         "the previous key is greater than the current:\n\
                          value: %i\n\
                          previous: %s \n\
                          code : %s \n\
                          key %s\n\
                          current: %s \n\
                          code : %s \n\
                          key %s\n"
                         sgn prev (print_text_utf8 prev)
                         (String.escaped prev_key) t (print_text_utf8 t)
                         (String.escaped key)))
                  (sgn1 < 0);
                if sgn1 = 0 then
                  expect_true
                    ~msg:
                      (lazy
                        (sprintf
                           "the previous line and the current are equal \
                            butcode point order is not correct.\n\
                            previous line:%s\n\
                            %s \n\
                            key %s\n\
                            current lins:%s\n\
                            %s \n\
                            key %s\n"
                           prev (print_text_utf8 prev) (String.escaped prev_key)
                           t (print_text_utf8 t) (String.escaped key)))
                    (Stdlib.compare prev t <= 0);
                expect_true
                  ~msg:
                    (lazy
                      (sprintf
                         "The comparison results differ\n\
                          value: %i\n\
                          previous: %s \n\
                          code : %s \n\
                          key %s\n\
                          current: %s \n\
                          code : %s \n\
                          key %s\n\n\
                          \t\t      previous - current comparison : %d\n\n\
                          \t\t      previous key - current key comparison : %d\n\n\
                          \t\t      previous key - current comparison : %d\n\n\
                          \t\t      previous - current key comparison : %d\n"
                         sgn prev (print_text_utf8 prev)
                         (String.escaped prev_key) t (print_text_utf8 t)
                         (String.escaped key) sgn sgn1 sgn2 sgn3))
                  (sgn = sgn1 && sgn1 = sgn2 && sgn2 = sgn3)));
        loop t key rest
  in
  loop "" (UTF8Comp.sort_key ?variable ~locale "") list

(* Test for Scandinavian languages*)

let () = test_list ~desc:"German: ä<b" ~locale:"de" ["a"; "ä"; "b"; "z"]
let () = test_list ~desc:"Finish: b<ä" ~locale:"fi_FI" ["a"; "b"; "z"; "ä"]
let () = test_list ~desc:"German: Ä<B" ~locale:"de" ["A"; "Ä"; "B"; "Z"]
let () = test_list ~desc:"Finish: B<Ä" ~locale:"fi_FI" ["A"; "B"; "Z"; "Ä"]
let () = test_list ~desc:"JISX 4061 test1" ~locale:"ja" jisx4061_test1
let () = test_list ~desc:"JISX 4061 test2" ~locale:"ja" jisx4061_test2

let () =
  test_list ~desc:"test_1 y < x" ~locale:"test_1"
    ["aaaaXbbbb"; "aaaaybbbb"; "aaaaxbbbb"; "aaaaZbbbb"; "aaaazbbbb"]

let () = test_list ~desc:test_desc_1 ~locale:"test_1" test_list_1
let () = test_list ~desc:test_desc_2 ~locale:"test_1" test_list_2
let () = test_list ~desc:test_desc_3 ~locale:"test_1" test_list_3
let () = test_list ~desc:test_desc_4 ~locale:"test_1" test_list_4

let () =
  test_list ~desc:"test_1 &b <<< b|*" ~locale:"test_1"
    ["aaaabbbb"; "aaaab*b*"; "aaaabcbc"]

let () = test_list ~desc:test_desc_5 ~locale:"test_1" test_list_5
let () = Blender.main ()
