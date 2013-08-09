(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Program

let run_with (vals:int64 list) (program_source:string) =
  let parsed = Program.parse program_source in
  List.iter (fun n -> printf "%16Lx -> %16Lx\n" n (Program.eval parsed n)) vals


let _ =

  if Tester.run_tests() then begin

    run_with [1L; 2L; 3L; 4L; 5L]
      "(lambda (x_17550) (xor (shr1 (and (if0 (and (xor (shr1 x_17550) 0) x_17550) x_17550 x_17550) x_17550)) x_17550))";

     run_with [0xffffffffffffffffL]
      "(lambda (x_60455) (fold (shl1 1) x_60455 (lambda (x_60456 x_60457) (shr1 (plus (and (and (shr1 (xor (if0 (shr16 (or (not x_60456) x_60456)) x_60456 x_60456) x_60457)) x_60457) x_60457) x_60457)))))";
    (* printf "raging mushrooms!\n"; *)

  end

