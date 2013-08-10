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
      try (
      let best = Guesser.solve box in
      let box = Guesser.step2 box best in
      oompaloompa box
      ) with Server.Solved _ -> Helpers.say "Solved!";
    in

    if op = "ouch" then begin
      (* oompaloompa (Guesser.start 5 ["shl1"; "xor"] "0UTVqJH86wBbJljF2shldH1A") *)
      (* oompaloompa (Guesser.start 8 ["or"; "plus"; "shl1"; "shr16"] "0WnMyvJVhmCAOiaZREBsMiWs") *)
      (* oompaloompa (Guesser.start 22 ["if0"; "not"; "or"; "plus"; "shl1"; "shr1"; "shr16"; "xor"] "zHsDD05sZUSBROeuEnKGUAkj") *)
      (* oompaloompa (Guesser.start 11 ["fold"; "not"; "shr1"; "shr16"; "xor"] "02biTMAo9m60zbZxZfUjkuAM") *)
      (* oompaloompa (Guesser.start 8 ["or"; "plus"] "0awKUJaqmczw11R2pfIE2X3K") *)

    end;


    if op = "live" then begin
      let p = Server.get_real_problem Sys.argv.(2) in
      Helpers.say "id   = %s" p.real_id;
      Helpers.say "size = %d" p.real_size;
      Helpers.say "ops  = %s" (ExtString.String.join "; " p.real_operators);

      for seconds = 3 downto 1 do
        Printf.printf "Will solve a REAL LIVE problem in %d seconds. Ctrl-C to cancel!\r%!" seconds;
        ignore(Unix.select [] [] [] 1.0);
      done;
      Printf.printf "                                                                  \r%!";

      (* oompaloompa (Guesser.start (p.real_size + 2) (List.append p.real_operators [ "or"; "xor"; "and"; "shl1"; "shr1"; "shr4"; "shr16" ]) p.real_id) *)
      oompaloompa (Guesser.start (p.real_size + 2) p.real_operators p.real_id)

    end;

    if op = "guess" then begin
      let te = Server.get_training ~use_cached_copy:true (int_of_string Sys.argv.(2)) (try Sys.argv.(3) with _ -> "") in
      Helpers.say "challenge = %s" te.challenge;
      Helpers.say "size      = %d" te.size;
      Helpers.say "id        = %s" te.id;
      Helpers.say "operators = %s" (ExtString.String.join ", " te.operators);
      oompaloompa (Guesser.start (te.size + 8) te.operators te.id)
    end;


    if op = "status" then begin
      let st = Server.get_status ~use_cached_copy:false () in
      Helpers.say "contest_score  = %d" st.contest_score;
      Helpers.say "training_score = %d" st.training_score;
      Helpers.say "num_request    = %d" st.num_requests;
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

