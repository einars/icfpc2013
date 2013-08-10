(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Program

type guess_t =
  { dna: int array
  ; parsed: program
  ; mutable score : int
  }

type guessbox_t =
  { available_ops : string list
  ; id : string
  ; size : int
  ; mutable guesses : guess_t list
  ; mutable inputs : int64 list
  ; mutable outputs : int64 list
  }

exception Nuff
exception Solved of program

let build_program size dna =
  (* transform dna to program *)

  let rec get_expr ptr =
    if ptr >= size then raise Nuff;
    let r = dna.(ptr) in
    let nptr = ptr + 1 in

    if r = 0 then E_0, (ptr+1)
    else if r = 1 then E_1, (ptr+1)
    else if r = 2 then Identifier "x", (ptr+1)
    else if r = 3 then
      let e1, nptr = get_expr nptr in
      let e2, nptr = get_expr nptr in
      let e3, nptr = get_expr nptr in
      If0 (e1, e2, e3), nptr
    else if r = 4 then let e1, nptr = get_expr nptr in (Not e1), nptr
    else if r = 5 then let e1, nptr = get_expr nptr in (Shl1 e1), nptr
    else if r = 6 then let e1, nptr = get_expr nptr in (Shr1 e1), nptr
    else if r = 7 then let e1, nptr = get_expr nptr in (Shr4 e1), nptr
    else if r = 8 then let e1, nptr = get_expr nptr in (Shr16 e1), nptr
    else if r = 9 then
      let e1, nptr = get_expr nptr in
      let e2, nptr = get_expr nptr in
      And (e1, e2), nptr
    else if r = 10 then
      let e1, nptr = get_expr nptr in
      let e2, nptr = get_expr nptr in
      Or (e1, e2), nptr
    else if r = 11 then
      let e1, nptr = get_expr nptr in
      let e2, nptr = get_expr nptr in
      Xor (e1, e2), nptr
    else if r = 12 then
      let e1, nptr = get_expr nptr in
      let e2, nptr = get_expr nptr in
      Plus (e1, e2), nptr else raise Nuff

  in
  let expr, expr_size = get_expr 0 in
  if expr_size <> size - 1 then (
    (* Helpers.say "uncool (%d) %s" expr_size (Program.expr_to_s expr); *)
    raise Nuff;
  );
  "x", expr


let good_random_guess size =

  let make_dna () = Array.init size (fun _ -> Random.int 12) in
  let rec stumble iterations =
    if iterations = 1000000
    then failwith "Cannot make a good guess"
    else let dna = make_dna () in
    try ignore(build_program size dna); dna with _ -> (stumble (iterations + 1))
  in
  let good_dna = stumble 0 in
  { dna = good_dna
  ; parsed = (build_program size good_dna)
  ; score = 0 }


let initial_guesses size =
  let guesses = ref [] in
  for i = 1 to size do
    guesses := (good_random_guess size) :: !guesses;
  done;
  !guesses


let start size ops id =
  let first_inputs = [ 0x6666666666666666L; 0xfedcba9876543210L; 0x0123456789abcdefL; 0x0101010101010101L ] in

  { available_ops = ops
  ; id = id
  ; size = size
  ; guesses = (initial_guesses size)
  ; inputs = first_inputs
  ; outputs = Server.get_eval id first_inputs
  }


let solve guessbox =
  Helpers.say "Guesser";
  Helpers.say "id   = %s" guessbox.id;
  Helpers.say "size = %d" guessbox.size;
  Helpers.say "inputs = %s" (ExtString.String.join ", " (List.map (Printf.sprintf "%16Lx") guessbox.inputs));
  Helpers.say "outputs = %s" (ExtString.String.join ", " (List.map (Printf.sprintf "%16Lx") guessbox.outputs));

  while true do
    let g = (build_program guessbox.size (good_random_guess guessbox.size).dna) in
    (* Helpers.say "checking %s" (Program.program_to_s g); *)
    try
      List.iter2 (fun a b -> if Program.eval g a <> b then raise Nuff) guessbox.inputs guessbox.outputs;
      Helpers.say "have a solution %s" (Program.program_to_s g);
      raise (Solved g);
    with Nuff -> ();
  done



