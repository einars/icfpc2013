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
  ; allow_const = true
  }

let build_program desc =

  let rec get_expr context ptr =
    if ptr > desc.problem_size then raise Nuff;

    let n_ops = Array.length desc.operators in
    let adj = if context.inside_fold then 5 else 3 in
    let r = (Random.int (n_ops + adj)) - adj in
    let nptr = ptr + 1 in

    let n_context = { context with allow_not = true; allow_const = true } in (* reset allow_not status *)
    let context_skip_zero = { n_context with allow_zero = false } in

    (* Helpers.say "%d" r; *)

    if r = -5 then if context.inside_fold then Identifier "y1", nptr else raise Nuff
    else if r = -4 then if context.inside_fold then Identifier "y2", nptr else raise Nuff
    else if r = -3 then if context.allow_const && context.allow_zero then E_0, nptr else raise Nuff
    else if r = -2 then if context.allow_const then E_1, nptr else raise Nuff
    else if r = -1 then Identifier "x", nptr
    else

    let op = desc.operators.(r) in

    if op = "not" && (not context.allow_not) then raise Nuff;
    if op = "fold" && (not context.allow_fold) then raise Nuff;
    if op = "fold" && (context.inside_fold) then raise Nuff;
    if op = "tfold" && (not context.allow_fold) then raise Nuff;
    if op = "tfold" && (context.inside_fold) then raise Nuff;

    if op = "if0" then
      let e1, nptr = get_expr {n_context with allow_const = false } nptr in
      let e2, nptr = get_expr context_skip_zero nptr in
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
    else if op = "bonus" then raise Nuff
    else ( Helpers.say "What is %s?" op; raise Nuff )

  in
  let expr, expr_size = get_expr default_guess_context 0 in
  if expr_size == 1 then (
    (* Helpers.say "uncool (%d, want %d) %s" expr_size size (Program.expr_to_s expr); *)
    raise Nuff;
  );
  "x", expr


let good_random_guess desc =

  let rec stumble () =
    try build_program desc with _ -> stumble ()
  in
  stumble ()



let suitable_first_inputs () =
  let base =
    [ 0xffffffffffffffffL
    ; 0x0000000000000000L
    ; 0x8000000000000000L
    ; 0x2000000000000000L
    ; 0x1111111111111111L
    ; 0x0101010101010101L
    ; 0x0123456789abcdefL
    ; 0xfedcba9876543210L
    ; 0x01020305070b0d11L
    ] in

  let rec append_random accum = function
    | 0 -> accum
    | n -> append_random ((Random.int64 0x7ffffffffffffffeL) :: accum) (n - 1)
  in
  append_random base 10




let improve_via_server_guess desc guess =
  Helpers.say "improve %s" (Program.to_s guess);
  (* will throw Server.Solved on success *)
  let n, r = Server.guess desc.problem_id (Program.to_s guess) in
  let had = (Program.eval guess n) in
  Helpers.say "  -- input:  %016Lx" n;
  Helpers.say "  -- output: %016Lx" r;
  Helpers.say "  -- had:    %016Lx" had;
  if had = r then failwith "Something fishy, please check";
  n, r


let rot = Helpers.make_rotator ()

exception Abort_this_crap

let rec smart_iterate_problemspace desc verify_fn : unit =

  let n_ops = Array.length desc.operators in

  let ttl = ref 1000 in

  let rec with_matching_exprs ptr context builder_f =

    if ptr > 0 then (
      ttl := !ttl - 1;
      if !ttl = 0 then raise Abort_this_crap;
    );

    let nptr = ptr + 1 in
    let n_context = { context with allow_not = true; allow_const = true } in (* reset allow_not status *)
    let context_skip_zero = { n_context with allow_zero = false } in

    let do_expr r =
      if ptr > desc.problem_size then raise Nuff;

      if r = -1 then if context.allow_const && context.allow_zero then (builder_f E_0 nptr);
      if r = -2 then if context.allow_const then (builder_f E_1 nptr);
      if r = -3 then ( builder_f (Identifier "x") nptr );
      if r = -3 && context.inside_fold then begin
        (builder_f (Identifier "y1") nptr );
        (builder_f (Identifier "y2") nptr );
      end;

      if r >= 0 then begin
        (* let op = desc.operators.(r) in *)

        let op = desc.operators.(r) in
        if op = "not" && (not context.allow_not) then raise Nuff;
        if op = "fold" && (not context.allow_fold) then raise Nuff;
        if op = "fold" && (context.inside_fold) then raise Nuff;
        if op = "tfold" && (not context.allow_fold) then raise Nuff;
        if op = "tfold" && (context.inside_fold) then raise Nuff;
        if op = "bonus" then raise Nuff;

        (* op3 *)
        if op = "if0" then (
          with_matching_exprs nptr {n_context with allow_const = false }
            (fun e1 nptr -> with_matching_exprs nptr context_skip_zero
                (fun e2 nptr -> with_matching_exprs nptr n_context
                    (fun e3 nptr -> builder_f (If0 (e1, e2, e3)) nptr )))
        ) else
        (* op1 *)
        if op = "not" then (
          with_matching_exprs nptr {n_context with allow_not = false}
            (fun e1 nptr -> builder_f (Not e1) nptr )
        ) else
        if op = "shl1" then (
          with_matching_exprs nptr context_skip_zero
            (fun e1 nptr -> builder_f (Shl1 e1) nptr )
        ) else
        if op = "shr1" then (
          with_matching_exprs nptr context_skip_zero
            (fun e1 nptr -> builder_f (Shr1 e1) nptr )
        ) else
        if op = "shr4" then (
          with_matching_exprs nptr context_skip_zero
            (fun e1 nptr -> builder_f (Shr4 e1) nptr )
        ) else
        if op = "shr16" then (
          with_matching_exprs nptr context_skip_zero
            (fun e1 nptr -> builder_f (Shr16 e1) nptr )
        ) else
        (* op2 *)
        if op = "and" then (
          with_matching_exprs nptr context_skip_zero
            (fun e1 nptr -> with_matching_exprs nptr context_skip_zero
                (fun e2 nptr -> builder_f (And (e1, e2)) nptr ))
        ) else
        if op = "or" then (
          with_matching_exprs nptr context_skip_zero
            (fun e1 nptr -> with_matching_exprs nptr context_skip_zero
                (fun e2 nptr -> builder_f (And (e1, e2)) nptr ))
        ) else
        if op = "xor" then (
          with_matching_exprs nptr context_skip_zero
            (fun e1 nptr -> with_matching_exprs nptr context_skip_zero
                (fun e2 nptr -> builder_f (And (e1, e2)) nptr ))
        ) else
        if op = "plus" then (
          with_matching_exprs nptr context_skip_zero
            (fun e1 nptr -> with_matching_exprs nptr context_skip_zero
                (fun e2 nptr -> builder_f (And (e1, e2)) nptr ))
        ) else
        if op = "tfold" then (
          with_matching_exprs nptr { n_context with allow_fold = false }
            (fun e1 nptr -> with_matching_exprs nptr { n_context with allow_fold = false; inside_fold = true }
                (fun e2 nptr -> builder_f (Fold (e1, E_0, "y1", "y2", e2)) nptr ))
        ) else
        if op = "fold" then (
          with_matching_exprs nptr { n_context with allow_fold = false }
            (fun e1 nptr -> with_matching_exprs nptr { n_context with allow_fold = false }
                (fun e2 nptr -> with_matching_exprs nptr { n_context with allow_fold = false; inside_fold = true }
                    (fun e3 nptr -> builder_f (Fold (e1, e2, "y1", "y2", e3)) nptr )))
        ) else failwith ("what is " ^ op);
      end

      in


      (*
      let shuffled d =
          let nd = List.map (fun c -> (Random.bits (), c)) d in
          let sond = List.sort compare nd in
          List.map snd sond
      in

      List.iter (fun x -> try do_expr x with Nuff -> ()) (shuffled [-4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17])
      *)
      (*
      for i = -4 to 15 do
        try do_expr i with Nuff -> ()
      done
      *)
      for i = 1 to (* n_ops - ptr + *) 2 do
        try do_expr ((Random.int (n_ops + 3)) - 3) with Nuff -> ();
      done

  in

  while true do
      ttl := 50000;
      try
        (* with_matching_exprs 0 default_guess_context (fun e nptr -> rot() ; Helpers.say "%s" (Program.to_s ("x", e)); if nptr > 1 then verify_fn ("x", e)); *)
        with_matching_exprs 0 default_guess_context (fun e nptr -> if nptr > 1 then ( rot(); verify_fn ("x", e)));
      with Abort_this_crap -> ();
  done

let rec process_random_stuff desc verify_fn : unit =
  Helpers.say "Using process_random_stuff solver";
  let rec loop () =
    verify_fn ( good_random_guess desc );
    rot ();
    loop ()
  in
  loop ()


let do_your_thing desc =

  let inputs = ref []
  and outputs = ref [] in

  let verifier some_guess =
    (* Helpers.say "???? %s" (Program.to_s some_guess); *)
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
    (* process_random_stuff desc verifier; *)
    smart_iterate_problemspace desc verifier;
  with (Server.Solved (problem_id, excellent_source)) -> (
    Helpers.say "SOLVED  %s\n" problem_id;
    ()
  )


