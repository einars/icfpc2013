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

    if op = "live" then begin
      (*
      let p = Server.get_real_problem Sys.argv.(2) in
      Helpers.say "id   = %s" p.real_id;
      Helpers.say "size = %d" p.real_size;
      Helpers.say "ops  = %s" (ExtString.String.join "; " p.real_operators);
      *)

      (*
      for seconds = 2 downto 1 do
        Printf.printf "Will solve a REAL LIVE problem in %d seconds. Ctrl-C to cancel!\r%!" seconds;
        ignore(Unix.select [] [] [] 1.0);
      done;
      Printf.printf "                                                                  \r%!";
      *)

      (* oompaloompa (Guesser.start (p.real_size + 2) (List.append p.real_operators [ "or"; "xor"; "and"; "shl1"; "shr1"; "shr4"; "shr16" ]) p.real_id) *)
      (* oompaloompa (Guesser.start (p.real_size + 2) p.real_operators p.real_id) *)

      let te = Server.get_real Sys.argv.(2) in
      Helpers.say "size      = %d" te.problem_size;
      Helpers.say "id        = %s" te.problem_id;
      Helpers.say "operators = %s" (ExtString.String.join ", " (Array.to_list te.operators));
      ignore( Guesser.do_your_thing te );
    end;

    if (op = "guess" || op = "train") then begin
      let te = Server.get_training (int_of_string Sys.argv.(2)) (try Sys.argv.(3) with _ -> "") in
      Helpers.say "size      = %d" te.problem_size;
      Helpers.say "id        = %s" te.problem_id;
      Helpers.say "operators = %s" (ExtString.String.join ", " (Array.to_list te.operators));
      ignore( Guesser.do_your_thing te );
    end;


    if op = "status" then begin
      let st = Server.get_status () in
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

