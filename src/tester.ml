(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Program
open Helpers

exception Assertion_failed

let assert_eq s1 s2 =
  if s1 <> s2 then begin
    Printf.printf "Not equal: %s %s\n%!" s1 s2;
    raise Assertion_failed;
  end

let assert_eq_parse prg parsed =
  let my_parsed = Program.parse prg in
  if my_parsed <> parsed then (
    Printf.printf "Parse failed:\nInp: %s\nExp: %s\nGot: %s\n%!" prg (Program.program_to_s parsed) (Program.program_to_s my_parsed);
    raise Assertion_failed
  )

let sample_program_1 = "(lambda (foo) 0)"
let sample_parsed_1:program = "foo", E_0

let sample_program_2 = "(lambda (foo) (not 0))"
let sample_parsed_2:program = "foo", (Not (E_0))

let sample_program_3 = "(lambda (x_3626) (not (or 0 x_3626)))"
let sample_parsed_3:program =
  "x_3626",
  (Not (Or (E_0, Identifier "x_3626")))


let complicated_prg = "(lambda (x_71206) (fold (or (if0 (shl1 (not (and 1 (and (not (shr4 (or (xor (shr1 0) (shr4 0)) 1))) x_71206)))) x_71206 x_71206) 0) x_71206 (lambda (x_71207 x_71208) (plus (shr1 x_71207) x_71208))))"

let run_tests () =


  try (
    assert_eq (Program.expr_to_s (And ((Identifier "one"), (Identifier "two")))) "(and one two)";
    assert_eq (Helpers.ltrim "saule") "saule";
    assert_eq (Helpers.ltrim "    saule") "saule";
    assert_eq (Helpers.left "left1" 1) "l";
    assert_eq (Helpers.left "left2" 2) "le";
    assert_eq (Helpers.left "left2000" 2000) "left2000";
    assert_eq (Helpers.starting_from "saule" 0) "saule";
    assert_eq (Helpers.starting_from "saule" 1) "aule";
    assert_eq (Helpers.starting_from "saule" 10) "";
    assert_eq_parse sample_program_1 sample_parsed_1;
    assert_eq_parse sample_program_2 sample_parsed_2;
    assert_eq_parse sample_program_3 sample_parsed_3;
    assert_eq complicated_prg (Program.program_to_s (Program.parse complicated_prg));
    Helpers.say "Parsing tests passed.";
    true
  ) with Assertion_failed -> false
