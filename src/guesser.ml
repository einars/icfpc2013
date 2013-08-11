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

  let n_ops = Array.length desc.operators in

  let rec get_expr context ptr =
    if ptr > desc.problem_size then raise Nuff;

    let adj = if context.inside_fold then 5 else 3 in
    let adj = if adj = 3 && not context.allow_zero then 2 else 3 in
    let adj = if (adj = 3 || adj = 2) && not context.allow_const then 1 else adj in
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
    else if op = "bonus" then raise Nuff
    else ( Helpers.say "What is %s?" op; raise Nuff )

  in
  let expr, expr_size = get_expr default_guess_context 0 in
  if expr_size = 1 then (
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
    [ 0xffffffffffffffffL
    ; 0x0000000000000000L
    ; 0x8000000000000000L
    ; 0x2000000000000000L
    ; 0x1111111111111111L
    ; 0x0101010101010101L
    ; 0x0123456789abcdefL
    ; 0xfedcba9876543210L
    ; 0x01020305070b0d11L
    ]

  (*
  let rec append_random accum = function
    | 0 -> accum
    | n -> append_random ((Random.int64 0x7ffffffffffffffeL) :: accum) (n - 1)
  in
  append_random base 10
  *)




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

type pool_entry_t =
  { pe_dna : int array
  ; pe_program : program
  ; pe_score : int
  }


exception Ret_something of program
exception Candidate_update

let rec smart_iterate_problemspace desc verify_fn input_g : unit =

  let n_ops = Array.length desc.operators in

  let have_bonus_op () =
    let rec loop n =
      if desc.operators.(n) = "bonus" then true
      else if n = 0 then false
      else loop (n - 1)
    in loop (n_ops - 1) in

  let is_bonus = have_bonus_op () in

  if is_bonus then Helpers.say "AAA the bonus game" else Helpers.say "This is not a bonus game";


  let problem_of_dna dna =


    let rec with_matching_exprs is_terminal ptr context builder_f =


      if ptr >= desc.problem_size then raise Nuff;

      let nptr = ptr + 1 in
      let n_context = { context with allow_not = true; allow_const = true } in (* reset allow_not status *)
      let context_skip_zero = { n_context with allow_zero = false } in

      let do_expr r =

        let r = if is_bonus && (ptr = 0) then 0 else r in (* force if0 *)

        if r < 0 && ((not is_terminal) || (ptr > desc.problem_size - 10)) then begin
          if r = -1 then if context.allow_const && context.allow_zero then (builder_f E_0 nptr);
          if r = -2 then if context.allow_const then (builder_f E_1 nptr);
          if r = -3 then ( builder_f (Identifier "x") nptr );
          if (r = -4 && context.inside_fold) then ( builder_f (Identifier "y1") nptr );
          if (r = -5 && context.inside_fold) then ( builder_f (Identifier "y2") nptr );
        end else

        if r >= 0 then begin
          (* let op = desc.operators.(r) in *)

          let op = desc.operators.(r) in

          (*
          let op =
            if (op = "not") || (op = "shl1") || (op = "shr1") || (op = "shr4") || op = "shr16" then (
            [| "not"; "shl1"; "shr1"; "shr4"; "shr16" |].( Random.int 5 )) else op in
      *)

          let op = if is_bonus && (ptr = 0) then "if0" else op in

          if op = "not" && (not context.allow_not) then raise Nuff;
          if op = "fold" && (not context.allow_fold) then raise Nuff;
          if op = "fold" && (context.inside_fold) then raise Nuff;
          if op = "tfold" && (not context.allow_fold) then raise Nuff;
          if op = "tfold" && (context.inside_fold) then raise Nuff;

          if op = "bonus" then raise Nuff;

          (* op3 *)
          if op = "if0" then (
            with_matching_exprs false nptr {n_context with allow_const = false }
              (fun e1 nptr -> with_matching_exprs false nptr n_context
                  (fun e2 nptr -> with_matching_exprs is_terminal nptr n_context
                      (fun e3 nptr -> builder_f (If0 (e1, e2, e3)) nptr )))
          ) else
          (* op1 *)
            if op = "not" then (
              with_matching_exprs is_terminal nptr {n_context with allow_not = false}
                (fun e1 nptr -> builder_f (Not e1) nptr )
            ) else if op = "shl1" then (
              with_matching_exprs is_terminal nptr context_skip_zero
                (fun e1 nptr -> builder_f (Shl1 e1) nptr )
            ) else if op = "shr1" then (
              with_matching_exprs is_terminal nptr context_skip_zero
                (fun e1 nptr -> builder_f (Shr1 e1) nptr )
            ) else if op = "shr4" then (
              with_matching_exprs is_terminal nptr context_skip_zero
                (fun e1 nptr -> builder_f (Shr4 e1) nptr )
            ) else if op = "shr16" then (
              with_matching_exprs is_terminal nptr context_skip_zero
                (fun e1 nptr -> builder_f (Shr16 e1) nptr )
          ) else
          (* op2 *)
          if op = "and" then (
            with_matching_exprs false nptr context_skip_zero
              (fun e1 nptr -> with_matching_exprs is_terminal nptr context_skip_zero
                  (fun e2 nptr -> builder_f (And (e1, e2)) nptr ))
          ) else
          if op = "or" then (
            with_matching_exprs false nptr context_skip_zero
              (fun e1 nptr -> with_matching_exprs is_terminal nptr context_skip_zero
                  (fun e2 nptr -> builder_f (And (e1, e2)) nptr ))
          ) else
          if op = "xor" then (
            with_matching_exprs false nptr context_skip_zero
              (fun e1 nptr -> with_matching_exprs is_terminal nptr context_skip_zero
                  (fun e2 nptr -> builder_f (And (e1, e2)) nptr ))
          ) else
          if op = "plus" then (
            with_matching_exprs false nptr context_skip_zero
              (fun e1 nptr -> with_matching_exprs is_terminal nptr context_skip_zero
                  (fun e2 nptr -> builder_f (And (e1, e2)) nptr ))
          ) else
          if op = "tfold" then (
            with_matching_exprs false nptr { n_context with allow_fold = false }
              (fun e1 nptr -> with_matching_exprs is_terminal nptr { n_context with allow_fold = false; inside_fold = true }
                  (fun e2 nptr -> builder_f (Fold (e1, E_0, "y1", "y2", e2)) nptr ))
          ) else
          if op = "fold" then (
            with_matching_exprs false nptr { n_context with allow_fold = false }
              (fun e1 nptr -> with_matching_exprs false nptr { n_context with allow_fold = false }
                  (fun e2 nptr -> with_matching_exprs is_terminal nptr { n_context with allow_fold = false; inside_fold = true }
                      (fun e3 nptr -> builder_f (Fold (e1, e2, "y1", "y2", e3)) nptr )))
          ) else failwith ("what is " ^ op);
        end

        in
          if ptr >= desc.problem_size then raise Nuff;
          let r = (( dna.(ptr) mod (n_ops + 5) ) - 5) in
          (* Printf.printf "%3d %!" r; *)
          do_expr r

    in
        try
          ignore( with_matching_exprs true 0 default_guess_context ( fun e nptr -> if nptr > 2 then raise (Ret_something ("x", e))) );
          raise Nuff
        with (Ret_something prob) -> prob

    in

  let random_dna () =
    Array.init desc.problem_size (fun idx -> Random.int 0x100000) in

  let rec usable_random_dna () =
    let dna = random_dna () in
    (try ignore(problem_of_dna dna); dna with Nuff -> usable_random_dna ())
  in

  let count_ones a =
    let rec loop accu elem =
      if elem = 0L then accu else
      if Int64.logand elem 1L = 1L
        then loop (accu + 1) (Int64.shift_right_logical elem 1)
        else loop accu (Int64.shift_right_logical elem 1) in
    (loop 0 a) in

  let count_matching_bits a b = 64 - (count_ones (Int64.logxor a b)) in


  let calculate_single_score p arg exp =
    (* 1 in*)
    let res = (Program.eval p arg) in
    let bits = (count_matching_bits res exp) in
    let bit_difference = abs ( (count_ones res) - (count_ones exp)) in
    let bit_difference = 0 in

    (* if res = exp then 100 else 0 in *)
    if bits = 64 then 10000 else bits in


  (*
    let bits = if bits = 64 then 10000 else bits * 10 in
    let bits = (count_matching_bits (Program.eval p arg) exp) in
    let bits = if bits = 64 then 10000 else bits * 10 in
    2 * (10 - abs ( (count_ones res) - (count_ones exp))) + bits in
    (* bits - (String.length (Program.to_s p)) in *)
    (* (count_matching_bits (Program.eval p arg) exp) in *)*)

  let calculate_score p args exps =
    if List.for_all2 (fun arg exp -> Program.eval p arg = exp) args exps then (
      verify_fn p;
      raise Candidate_update;
    );
    (List.fold_right2 (fun arg exp acc -> acc + calculate_single_score p arg exp) args exps 0 )
    + (String.length (Program.to_s p) / 10)
  in

  let update_pool_score entry =
    let input, output = input_g () in
    (* Helpers.say "ups: %d" (List.length input); *)
    { entry with pe_score = calculate_score entry.pe_program input output } in


  let new_pool_entry () =
    let dna = usable_random_dna () in
    let p = problem_of_dna dna in
    update_pool_score { pe_dna = dna
    ; pe_program = p
    ; pe_score = 0
    } in

  let print_pool pool =
    Helpers.say "I have a dream. I have a pool:";
    List.iter (fun pe -> Helpers.say "Score: %d %s" pe.pe_score (Program.to_s pe.pe_program)) pool in

  let take_best n l =
    let rec take elems all =
      if elems = 0 then []
      else (match all with
      | h :: t -> h :: (take (elems - 1) t)
      | _ -> []
      )
    in

    take n (List.sort (fun a b -> if a.pe_score <> b.pe_score then b.pe_score - a.pe_score else (if Random.bool () then 1 else -1) ) l)
    (* take n (List.sort (fun a b -> b.pe_score - a.pe_score) l) *)
    in

  let morph_pool pool =


    let rec single_morph pe =
      let d1 = Array.copy pe.pe_dna in
      let idx1 = Random.int desc.problem_size in
      d1.(idx1) <- Random.int 0xffffff;
      try
        let p = problem_of_dna d1 in
        update_pool_score { pe with pe_dna = d1; pe_program = p; }
      with Nuff -> pe in

    let rec shiftorph pe =
      let dnas = Array.to_list pe.pe_dna in
      let pos = 1 + Random.int ( Array.length pe.pe_dna - 2 ) in
      let new_dna = Array.of_list ( (Random.int 0xffff) :: dnas) in
      try
        let p = problem_of_dna new_dna in
        update_pool_score { pe with pe_dna = new_dna; pe_program = p; }
      with Nuff -> pe in

    let rec unshiftorph pe =
      let dt = List.tl ( Array.to_list pe.pe_dna ) in
      let new_dna = Array.of_list ( List.append dt  [Random.int 0xffff] ) in
      try
        let p = problem_of_dna new_dna in
        update_pool_score { pe with pe_dna = new_dna; pe_program = p; }
      with Nuff -> pe in


    let rec cutmorph pe =
      let dnas = Array.to_list pe.pe_dna in
      let pos = 1 + Random.int ( Array.length pe.pe_dna - 2 ) in
      let k1, k2 = ExtList.List.split_nth pos dnas in
      let new_dna = Array.of_list ( List.append k1 ( (Random.int 0xffff) :: k2) ) in
      try
        let p = problem_of_dna new_dna in
        update_pool_score { pe with pe_dna = new_dna; pe_program = p; }
      with Nuff -> pe in

    let rec uniq x =
      let rec uniq_help l n =
        match l with
        | [] -> []
        | h :: t -> if n = h then uniq_help t n else h::(uniq_help t n) in
    match x with
        | [] -> []
        | h::t -> h::(uniq_help (uniq t) h) in

    uniq (List.fold_right (fun elem accu ->
      (cutmorph (cutmorph elem)) :: (cutmorph (cutmorph elem)) :: (cutmorph (cutmorph elem)) ::
      (cutmorph (cutmorph elem)) :: (cutmorph (cutmorph elem)) :: (cutmorph (cutmorph elem)) ::
      (cutmorph elem) :: (cutmorph elem) :: (cutmorph elem) ::
      (cutmorph elem) :: (cutmorph elem) :: (cutmorph elem) ::
      (shiftorph (shiftorph elem)) :: (shiftorph (shiftorph elem)) :: (shiftorph (shiftorph elem)) ::
      (shiftorph elem) :: (shiftorph elem) :: (shiftorph elem) ::
      (shiftorph elem) :: (shiftorph elem) :: (shiftorph elem) ::
      (unshiftorph (unshiftorph elem)) :: (unshiftorph (unshiftorph elem)) :: (unshiftorph (unshiftorph elem)) ::
      (unshiftorph elem) :: (unshiftorph elem) :: (unshiftorph elem) ::
      (unshiftorph elem) :: (unshiftorph elem) :: (unshiftorph elem) ::
      (single_morph (single_morph elem)) :: (single_morph (single_morph elem)) :: (single_morph (single_morph elem)) ::
      (single_morph (single_morph elem)) :: (single_morph (single_morph elem)) :: (single_morph (single_morph elem)) ::
      (single_morph elem) :: (single_morph elem) :: (single_morph elem) ::
      (single_morph elem) :: (single_morph elem) :: (single_morph elem) :: elem :: accu
    ) pool [
      new_pool_entry (); new_pool_entry (); new_pool_entry (); new_pool_entry (); new_pool_entry ();
      new_pool_entry (); new_pool_entry (); new_pool_entry (); new_pool_entry (); new_pool_entry ();
        ])  in


  let rec improve pool =
    (* print_pool pool; *)
    let p = (
      try (take_best 10 (morph_pool pool))
    with Candidate_update -> pool
    ) in

    rot();
    (*
    ignore( Unix.system "clear" );
    print_pool p; *)
    let f = List.hd p in
    Printf.printf "%d %s   \r%!" f.pe_score (Helpers.left (Program.to_s f.pe_program) 120);

    improve p
  in

  improve []


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

  let input_getter () = !inputs, !outputs in

  let verifier some_guess =
    (* Helpers.say "???? %s" (Program.to_s some_guess); *)
    if List.for_all2 (fun a b -> Program.eval some_guess a = b) !inputs !outputs then (
      let new_guess, new_result = improve_via_server_guess desc some_guess in
      inputs := new_guess :: !inputs;
      outputs := new_result :: !outputs;
    )
  in

  if desc.problem_id = "" then failwith "I really need a problem ID";

  inputs := suitable_first_inputs ();
  outputs := Server.get_eval desc.problem_id !inputs;
  try
    if false
    then process_random_stuff desc verifier
    else smart_iterate_problemspace desc verifier input_getter;
  with (Server.Solved (problem_id, excellent_source)) -> (
  (* ignore(Unix.system "/usr/bin/aplay /home/e/ding.wav"); *)
    Helpers.say "SOLVED  %s\n" problem_id;
    ()
  )


