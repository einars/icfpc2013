(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Program

let _ =

  if Tester.run_tests() then begin

    let complicated_prg = "(lambda (x_71206) (fold (or (if0 (shl1 (not (and 1 (and (not (shr4 (or (xor (shr1 0) (shr4 0)) 1))) x_71206)))) x_71206 x_71206) 0) x_71206 (lambda (x_71207 x_71208) (plus (shr1 x_71207) x_71208))))" in
    let parse_tree = Program.parse complicated_prg in

    printf "%s\n%!" (Program.program_to_s parse_tree);
    printf "%s\n%!" (complicated_prg);

    if complicated_prg = (Program.program_to_s parse_tree) then printf "This if fucking exquisite!\n%!";

    printf "raging mushrooms!";

  end

