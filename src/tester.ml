(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Program

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

let sample_program = "(lambda (x_3626) (not (or 0 x_3626)))"
let sample_parsed:program =
  "x_3626",
  (Op1 (Not, (Op2 (Or, E_0, Identifier "x_3626"))))



let run_tests () =


  try (
    assert_eq (Program.op1_to_s Program.Not) "not";
    assert_eq (Program.expr_to_s (Op2 (And, (Identifier "one"), (Identifier "two")))) "(and one two)";
    assert_eq_parse sample_program sample_parsed;
    Printf.printf "All tests passed.\n%!";
    true
  ) with Assertion_failed -> false
