(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Program

let _ =

  if Tester.run_tests() then begin
    printf "raging mushrooms!";
  end

