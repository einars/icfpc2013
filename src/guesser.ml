(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Program

type guess_t =
  { dna: int array
  ; parsed: program
  ; mutable score : int
  }

type guessbox_t =
  { available_ops : string list
  ; program_id : string
  ; program_size : int
  ; mutable guesses : guess_t list
  ; mutable inputs : int64 list
  ; mutable outputs : int64 list
  }

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

let build_program size allowed_ops dna =
  (* transform dna to program *)

  let rec get_expr context ptr =
    if ptr > size then raise Nuff;

    let n_ops = List.length allowed_ops in
    let adj = if context.inside_fold then 5 else 3 in
    let r = dna.(ptr) mod (n_ops + adj) - adj in
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

    let op = List.nth allowed_ops r in

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


let good_random_guess size allowed_ops =

  let make_dna () = Array.init size (fun _ -> Random.int 255) in
  let rec stumble iterations =
    (* if iterations = 1000000
    then failwith "Cannot make a good guess"
    else *) let dna = make_dna () in
    try ignore(build_program size allowed_ops dna); dna with _ -> (stumble (iterations + 1))
  in
  let good_dna = stumble 0 in
  { dna = good_dna
  ; parsed = (build_program size allowed_ops good_dna)
  ; score = 0 }



let suitable_first_inputs () =
  let base =
    [ 0xffffffffffffffffL
    ; 0x0000000000000000L
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

let start size ops id =
  let initial_inputs = if id = "" then [] else suitable_first_inputs () in

  { available_ops = ops
  ; program_id = id
  ; program_size = size
  ; guesses = []
  (* ; guesses = (initial_guesses size) *)
  ; inputs = initial_inputs
  ; outputs = Server.get_eval id initial_inputs
  }


let solve guessbox =
  Helpers.say "Guesser.solve %s %d, %d tests" guessbox.program_id guessbox.program_size (List.length guessbox.inputs);
  (*
  Helpers.say "inputs = %s" (ExtString.String.join ", " (List.map (Printf.sprintf "%16Lx") guessbox.inputs));
  Helpers.say "outputs = %s" (ExtString.String.join ", " (List.map (Printf.sprintf "%16Lx") guessbox.outputs));
  *)

  try (
  let rot = Helpers.make_rotator () in
  while true do
    let g = (build_program guessbox.program_size guessbox.available_ops (good_random_guess guessbox.program_size guessbox.available_ops).dna) in
    (* Helpers.say "checking %s" (Program.program_to_s g); *)
    rot ();
    if List.for_all2 (fun a b -> Program.eval g a = b) guessbox.inputs guessbox.outputs
    then raise (Solved g)
  done;
  "hack", E_0
  ) with (Solved g) -> Helpers.say "Probably solved: %s" (Program.program_to_s g); g


let step2 guessbox solution_tree =
  Helpers.say ">> -- %s" (Program.program_to_s solution_tree);
  let n, r = Server.guess ~use_cached_copy:true guessbox.program_id (Program.program_to_s solution_tree) in
  let had = (Program.eval solution_tree n) in
  Helpers.say "  -- input:  %016Lx" n;
  Helpers.say "  -- output: %016Lx" r;
  Helpers.say "  -- had:    %016Lx" had;
  if had = r then failwith "Something fishy, please check";
  { guessbox with inputs = n :: guessbox.inputs; outputs = r :: guessbox.outputs }


