(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Program
open Server

let (==) a b = failwith "Use [=] instead of [==]!"
let (!=) a b = failwith "Use [<>] instead of [!=]!"

let run_with (vals:int64 list) (program_source:string) =
  let parsed = Program.parse program_source in
  List.iter (fun n -> Helpers.say "%16Lx -> %16Lx" n (Program.eval parsed n)) vals

let prg = "(lambda (x_60455) (fold (shl1 1) x_60455 (lambda (x_60456 x_60457) (shr1 (plus (and (and (shr1 (xor (if0 (shr16 (or (not x_60456) x_60456)) x_60456 x_60456) x_60457)) x_60457) x_60457) x_60457)))))"

let _ =

  if Tester.run_tests() then begin

    let op = (try Sys.argv.(1) with x -> "default") in

    Server.set_key "0471LR96Uo35Eet4lsIzkYr8bcVDtdWjbv9WyBsovpsH1H";

    if op = "train" then begin
      let te = Server.get_training ~use_cached_copy:true ~size:5 () in
      Helpers.say "challenge = %s" te.challenge;
      Helpers.say "size      = %d" te.size;
      Helpers.say "id        = %s" te.id;
      Helpers.say "operators = %s" (ExtString.String.join ", " te.operators);

    end;

    if op = "status" then begin
      let st = Server.get_status ~use_cached_copy:false () in
      Helpers.say "contest_score  = %d" st.contest_score;
      Helpers.say "training_score = %d" st.training_score;
      Helpers.say "num_request    = %d" st.num_requests;
    end;

    if op = "xxx" then begin
      run_with [0xffffffffffffffffL] prg;
    end;

    if op = "benchmark" then begin
      let times = 10000 in
      Helpers.say "Running %d times" times;
      Helpers.set_silent_mode true;
      for i = 1 to times do
        ignore(Program.eval_s prg 0xffffffffffffffffL);
      done;
    end;


    (* printf "raging mushrooms!\n"; *)

  end

