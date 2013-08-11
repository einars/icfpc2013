(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Program
open Server

(*
val start : int -> string list -> string -> guessbox_t
val solve : guessbox_t -> program
val step2  : guessbox_t -> program -> guessbox_t

*)
exception Nuff
exception Solved of program

type guess_context_t =
  { allow_zero : bool
  ; use_fold : bool
  ; inside_fold : bool
  ; allow_not : bool
  ; allow_fold : bool
  ; allow_const : bool
  }

let default_guess_context =
  { allow_zero = true
  ; use_fold = true
  ; inside_fold = false
  ; allow_not = true
  ; allow_fold = true
  ; allow_const = false
  }

let rec process_random_stuff desc verify_fn : unit =

  let have_operator op =
    let have = ref false in
    for i = 0 to (Array.length desc.operators - 1) do
      if desc.operators.(i) = op then have := true;
    done;
    !have in

  let is_bonus_mode = have_operator "bonus" in


  let build_program () =

    let choose_random l =
      let len = List.length l in
      List.nth l (Random.int len) in

    let rec get_expr context ptr =
      if ptr > desc.problem_size then raise Nuff;

      let n_ops = Array.length desc.operators in
      let adj = if ptr = 0 then 0 else 3 in
      let r = (Random.int (n_ops + adj)) - adj in
      let nptr = ptr + 1 in

      let n_context = { context with allow_not = true; allow_const = true } in (* reset allow_not status *)
      let context_skip_zero = { n_context with allow_zero = false } in

      (* Helpers.say "%d" r; *)

      let terms = [ (Identifier "x") ] in
      let terms = if context.inside_fold then (Identifier "y1") :: (Identifier "y2") :: terms else terms in
      let terms = if context.allow_const && context.allow_zero then E_0 :: terms else terms in
      let terms = if context.allow_const then E_1 :: terms else terms in

      if r < 0 then (choose_random terms, nptr)

      else

      let op = desc.operators.(r) in
      let op = if is_bonus_mode && ptr = 0 then "if0" else op in

      if op = "not" && (not context.allow_not) then get_expr context ptr else
      if op = "fold" && (not context.allow_fold) then get_expr context ptr else
      if op = "tfold" && (not context.allow_fold) then get_expr context ptr else
      if op = "fold" && nptr > (n_ops - 3) then get_expr context ptr else
      if op = "tfold" && nptr > (n_ops - 3) then get_expr context ptr else

      if op = "if0" then
        let e1, nptr = get_expr {n_context with allow_const = false } nptr in
        let e2, nptr = get_expr n_context nptr in
        let e3, nptr = get_expr n_context nptr in
        If0 (e1, e2, e3), nptr
      else
        if op = "not" then let e1, nptr = get_expr { n_context with allow_not = false } nptr in (Not e1), nptr
      else if op = "fold" then
          let e1, nptr = get_expr { n_context with allow_fold = false } nptr in
          let e2, nptr = get_expr { n_context with allow_fold = false } nptr in
          let e3, nptr = get_expr { n_context with inside_fold = true; allow_fold = false } nptr in
          Fold (e1, e2, "y1", "y2", e3), nptr
      else if op = "tfold" then
          let e1, nptr = get_expr { n_context with allow_fold = false } nptr in
          let e3, nptr = get_expr { n_context with inside_fold = true; allow_fold = false } nptr in
          Fold (e1, E_0, "y1", "y2", e3), nptr
      else if op = "shl1" then let e1, nptr = get_expr context_skip_zero nptr in (Shl1 e1), nptr
      else if op = "shr1" then let e1, nptr = get_expr context_skip_zero nptr in (Shr1 e1), nptr
      else if op = "shr4" then let e1, nptr = get_expr context_skip_zero nptr in (Shr4 e1), nptr
      else if op = "shr16" then let e1, nptr = get_expr context_skip_zero nptr in (Shr16 e1), nptr
      else if op = "and" then
        let e1, nptr = get_expr context_skip_zero nptr in
        let e2, nptr = get_expr context_skip_zero nptr in
        And (e1, e2), nptr
      else if op = "or" then
        let e1, nptr = get_expr context_skip_zero nptr in
        let e2, nptr = get_expr context_skip_zero nptr in
        Or (e1, e2), nptr
      else if op = "xor" then
        let e1, nptr = get_expr context_skip_zero nptr in
        let e2, nptr = get_expr context_skip_zero nptr in
        Xor (e1, e2), nptr
      else if op = "plus" then
        let e1, nptr = get_expr context_skip_zero nptr in
        let e2, nptr = get_expr context_skip_zero nptr in
        Plus (e1, e2), nptr
      else if op = "bonus" then get_expr context nptr
      else ( Helpers.say "What is %s?" op; raise Nuff )

    in

    let expr, expr_size =
    if is_bonus_mode then begin
      (* Helpers.say "Running in bonus mode"; *)
      get_expr { default_guess_context with allow_fold = false } 0
    end else begin
      (* Helpers.say "Running in classic mode"; *)
      get_expr default_guess_context 0
    end in
    (* Helpers.say "%s" (Program.expr_to_s expr); *)

    if expr_size == 1 then (
      (* Helpers.say "uncool (%d, want %d) %s" expr_size size (Program.expr_to_s expr); *)
      raise Nuff;
    );
    "x", expr in



  let rot = Helpers.make_rotator () in
  if is_bonus_mode then Helpers.say "Running in bonus mode";
  if not is_bonus_mode then Helpers.say "Running in normal mode";
  let rec loop () =
    (try
      verify_fn ( build_program () );
      rot ();
    with Nuff -> ());
    loop ()
  in
  loop ()




let suitable_first_inputs () =
    [ 0xffffffffffffffffL
    ; 0x0000000000000000L
    ; 0x0000000000000001L
    ; 0x0000000000000002L
    ; 0x0000000000000003L
    ; 0x8000000000000000L
    ; 0xf000000000000000L
    ; 0x1111111111111111L
    ; 0x0101010101010101L
    ; 0x787078f0e0c00300L
    ; Random.int64 0x7ffffffffffffffeL
    ; Random.int64 0x7ffffffffffffffeL
    ; Random.int64 0x7ffffffffffffffeL
    ]





let improve_via_server_guess desc guess =
  Helpers.say "improve %s" (Program.program_to_s guess);
  (* will throw Server.Solved on success *)
  let n, r, act = Server.guess desc.problem_id (Program.program_to_s guess) in
  let had = (Program.eval guess n) in
  Helpers.say "  -- input:  %016Lx" n;
  Helpers.say "  -- output: %016Lx" r;
  Helpers.say "  -- had:    %016Lx" had;
  Helpers.say "  -- act:    %016Lx" act;
  if had = r then failwith "Something fishy, please check";
  if had <> act then failwith "EVALFAIL, please check";
  n, r



let do_your_thing desc =

  let inputs = ref []
  and outputs = ref [] in

  let verifier some_guess =
      if List.for_all2 (fun a b -> Program.eval some_guess a = b) !inputs !outputs then (
        let new_guess, new_result = improve_via_server_guess desc some_guess in
        inputs := new_guess :: !inputs;
        outputs := new_guess :: !outputs;
      )
  in

  if desc.problem_id = "" then failwith "I really need a problem ID";

  inputs := suitable_first_inputs ();
  outputs := Server.get_eval desc.problem_id !inputs;
  try
    process_random_stuff desc verifier;
    failwith "Nothing found, really";
  with (Server.Solved (problem_id, excellent_source)) -> (
    Helpers.say "SOLVED  %s\n" problem_id;
    excellent_source
  )


