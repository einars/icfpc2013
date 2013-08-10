(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Server
open Guesser

let (==) a b = failwith "Use [=] instead of [==]!"
let (!=) a b = failwith "Use [<>] instead of [!=]!"


let run_with (vals:int64 list) (program_source:string) =
  let parsed = Program.parse program_source in
  List.iter (fun n -> Helpers.say "%16Lx -> %16Lx" n (Program.eval parsed n)) vals

let prg = "(lambda (x_60455) (fold (shl1 1) x_60455 (lambda (x_60456 x_60457) (shr1 (plus (and (and (shr1 (xor (if0 (shr16 (or (not x_60456) x_60456)) x_60456 x_60456) x_60457)) x_60457) x_60457) x_60457)))))"

let _ =

  if Tester.run_tests() then begin

    Random.self_init ();

    let op = (try Sys.argv.(1) with x -> "default") in

    Server.set_key "0471LR96Uo35Eet4lsIzkYr8bcVDtdWjbv9WyBsovpsH1H";

    let rec oompaloompa box =
      Helpers.say "solving...";
      let best = Guesser.solve box in
      Helpers.say "reiterating...";
      let box = Guesser.step2 box best in
      oompaloompa box
    in

    if op = "ouch" then begin
      (* oompaloompa (Guesser.start 5 ["shl1"; "xor"] "0UTVqJH86wBbJljF2shldH1A") *)
      (* oompaloompa (Guesser.start 8 ["or"; "plus"; "shl1"; "shr16"] "0WnMyvJVhmCAOiaZREBsMiWs") *)
      oompaloompa (Guesser.start 22 ["if0"; "not"; "or"; "plus"; "shl1"; "shr1"; "shr16"; "xor"] "zHsDD05sZUSBROeuEnKGUAkj")

    end;

    if op = "run5" then begin
      let base = Program.parse "(lambda (x) (xor (shl1 x) x))" in
      let box = Guesser.start 5 ["shl1"; "xor" ] "" in
      box.inputs <- [ 0x6666666666666666L; 0xfedcba9876543210L; 0x0123456789abcdefL; 0x0101010101010101L ];
      box.outputs <- List.map (Program.eval base) box.inputs;

      Helpers.say "Solved %s" (Program.program_to_s (Guesser.solve box));
    end;


    if op = "guess" then begin
      let te = Server.get_training ~use_cached_copy:true ~size:16 () in
      Helpers.say "challenge = %s" te.challenge;
      Helpers.say "size      = %d" te.size;
      Helpers.say "id        = %s" te.id;
      Helpers.say "operators = %s" (ExtString.String.join ", " te.operators);
      oompaloompa (Guesser.start te.size te.operators te.id)
    end;


    if op = "train" then begin
      let te = Server.get_training ~use_cached_copy:true ~size:25 () in
      Helpers.say "challenge = %s" te.challenge;
      Helpers.say "size      = %d" te.size;
      Helpers.say "id        = %s" te.id;
      Helpers.say "operators = %s" (ExtString.String.join ", " te.operators);

      let inputs = Guesser.suitable_first_inputs () in
      let outputs = Server.get_eval ~use_cached_copy:true te.id inputs in
      Helpers.say "Inputs:  %s" (ExtString.String.join ", " (List.map (Printf.sprintf "0x%016Lx") inputs));
      Helpers.say "Outputs: %s" (ExtString.String.join ", " (List.map (Printf.sprintf "0x%016Lx") outputs));
      Helpers.say "Guess results: %s" (ExtString.String.join ", " (List.map (Printf.sprintf "0x%016Lx") outputs));

      let challenge = Program.parse te.challenge in
      List.iter2 (fun guess expected ->
        let got = Program.eval challenge guess in
        if got <> expected
          then Helpers.say "Challenge self-check failed: input %016Lx expected %016Lx got %016Lx" guess expected got;
      ) inputs outputs;

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

