(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Helpers

exception Parse_failed of string

type op2 = And | Or | Xor | Plus
type expr = E_0 | E_1 | Identifier of string
  | If0 of expr * expr * expr
  | Fold of expr * expr * string * string * expr
  | Not of expr | Shl1 of expr | Shr1 of expr | Shr4 of expr | Shr16 of expr
  | And of expr * expr | Or of expr * expr | Xor of expr * expr | Plus of expr * expr

type program = string * expr


let sample_parsing = ("test", (Not (Identifier "test")))


let rec p_expect_s expectation remainder:string =
  let remainder = ltrim remainder in
  let len = String.length remainder in
  let len_exp = String.length expectation in
  if len < len_exp then raise (Parse_failed (sprintf "Expected %s, got empty" expectation));
  if remainder.[0] = ' '
    then p_expect_s expectation (String.sub remainder 1 (len - 1))
    else (
      if String.sub remainder 0 len_exp <> expectation then raise (Parse_failed (sprintf "Expected %s, got some crap" expectation));
      String.sub remainder len_exp (len - len_exp)
    )


let p_expect_id s  =

  let s = ltrim s in

  let goodchars_0 = "abcdefghijklmnoppqrstuvwxyz"
  and goodchars_1 = "abcdefghijklmnoppqrstuvwxyz0123456789_" in
  let rec collect pos identifier =
    if String.contains (if pos = 0 then goodchars_0 else goodchars_1) s.[pos]
      then collect (pos + 1) (identifier ^ (String.make 1 s.[pos]))
      else pos, identifier
  in

  let pos, identifier = collect 0 "" in

  if pos = 0
    then raise (Parse_failed "No identifier found")
    else (identifier, (String.sub s pos ((String.length s) - pos)))


exception Parser_found of expr * string

let rec p_expect_expr s =

  let parse_exp_0 s =
    if starts_with "0" s then (E_0, (starting_from s 1))
      else raise (Parse_failed "Expected E_0") in

  let parse_exp_1 s =
    if starts_with "1" s then (E_1, (starting_from s 1))
      else raise (Parse_failed "Expected E_1") in

  let parse_exp_id s =
    let s, rmd = p_expect_id s in Identifier s, rmd in

  let parse_exp_if0 s =
    let s = p_expect_s "(" s in
    let s = p_expect_s "if0" s in
    let e1, s = p_expect_expr s in
    let e2, s = p_expect_expr s in
    let e3, s = p_expect_expr s in
    let s = p_expect_s ")" s in
    If0 (e1, e2, e3), s in

  let parse_exp_op1 s =
    let s = p_expect_s "(" s in
    let rec try_ops = function
      | (op,code) :: rest -> begin try (
        let s = p_expect_s op s in
        let e, s = p_expect_expr s in
        let s = p_expect_s ")" s in
        (code e), s
      ) with (Parse_failed n) -> try_ops rest end
      | _ -> raise (Parse_failed "No Op1 here")
    in
    try_ops
    [ "not", (fun x -> Not x)
    ; "shl1", (fun x -> Shl1 x)
    ; "shr1", (fun x -> Shr1 x)
    ; "shr4", (fun x -> Shr4 x)
    ; "shr16", (fun x -> Shr16 x)
    ] in

  let parse_exp_op2 s =
    let s = p_expect_s "(" s in
    let rec try_ops = function
      | (op, code) :: rest -> begin try (
        let s = p_expect_s op s in
        let e1, s = p_expect_expr s in
        let e2, s = p_expect_expr s in
        let s = p_expect_s ")" s in
        (code e1 e2), s
      ) with (Parse_failed n) -> try_ops rest end
      | _ -> raise (Parse_failed "No Op1 here")
    in
    try_ops
    [ "and", (fun e1 e2 -> And (e1, e2))
    ; "or", (fun e1 e2 -> Or (e1, e2))
    ; "xor", (fun e1 e2 -> Xor (e1, e2))
    ; "plus", (fun e1 e2 -> Plus (e1, e2))
    ] in

  let parse_exp_fold s =
    let s = p_expect_s "(" s in
    let s = p_expect_s "fold" s in
    let e1, s = p_expect_expr s in
    let e2, s = p_expect_expr s in
    let s = p_expect_s "(" s in
    let s = p_expect_s "lambda" s in
    let s = p_expect_s "(" s in
    let id1, s = p_expect_id s in
    let id2, s = p_expect_id s in
    let s = p_expect_s ")" s in
    let e3, s = p_expect_expr s in
    let s = p_expect_s ")" s in
    let s = p_expect_s ")" s in
    Fold (e1, e2, id1, id2, e3), s in


  let s = ltrim s in

  let expr_parsers = [ parse_exp_0; parse_exp_1; parse_exp_id ; parse_exp_if0; parse_exp_op1; parse_exp_op2; parse_exp_fold] in


  let parser_searcher parse_fn = (
      try (
        let e,s = parse_fn s in
        raise (Parser_found (e, s) )
      ) with (Parse_failed p) -> ()
  ) in


  try (
    List.iter parser_searcher expr_parsers;
    raise (Parse_failed ("No expression found, looking at " ^ (left s 30)));
  ) with (Parser_found (e, s)) -> (e, s)





let parse_program s =
  let s = p_expect_s "(" s in
  let s = p_expect_s "lambda" s in
  let s = p_expect_s "(" s in
  let (identifier, s) = p_expect_id s in
  let s = p_expect_s ")" s in
  let expr, s = p_expect_expr s in
  (identifier, expr)

let parse s = parse_program s

let rec expr_to_s = function
| E_0 -> "0"
| E_1 -> "1"
| Identifier id -> id
| If0 (e1, e2, e3) -> Printf.sprintf "(if0 %s %s %s)" (expr_to_s e1) (expr_to_s e2) (expr_to_s e3)
| Fold (e1, e2, id1, id2, e3) ->
    Printf.sprintf "(fold %s %s (lambda (%s %s) %s))"
      (expr_to_s e1)
      (expr_to_s e2)
      id1
      id2
      (expr_to_s e3)
  | Not e -> Printf.sprintf "(not %s)" (expr_to_s e)
  | Shl1 e -> Printf.sprintf "(shl1 %s)" (expr_to_s e)
  | Shr1 e -> Printf.sprintf "(shr1 %s)" (expr_to_s e)
  | Shr4 e -> Printf.sprintf "(shr4 %s)" (expr_to_s e)
  | Shr16 e -> Printf.sprintf "(shr16 %s)" (expr_to_s e)
  | And (e1, e2) -> Printf.sprintf "(%s %s %s)" "and" (expr_to_s e1) (expr_to_s e2)
  | Or (e1, e2) -> Printf.sprintf "(%s %s %s)" "or" (expr_to_s e1) (expr_to_s e2)
  | Xor (e1, e2) -> Printf.sprintf "(%s %s %s)" "xor" (expr_to_s e1) (expr_to_s e2)
  | Plus (e1, e2) -> Printf.sprintf "(%s %s %s)" "plus" (expr_to_s e1) (expr_to_s e2)

let program_to_s (id, e) = Printf.sprintf "(lambda (%s) %s)" id (expr_to_s e)

module Bindings = Map.Make(String)

let rec eval_expr e bindings = match e with
  | E_0 -> Int64.zero
  | E_1 -> Int64.one
  | Identifier id -> Bindings.find id bindings
  | If0 (e1, e2, e3) -> eval_expr (if eval_expr e1 bindings = Int64.zero then e2 else e3) bindings
  | Not e -> Int64.lognot (eval_expr e bindings)
  | Shl1 e -> Int64.shift_left (eval_expr e bindings) 1
  | Shr1 e -> Int64.shift_right_logical (eval_expr e bindings) 1
  | Shr4 e -> Int64.shift_right_logical (eval_expr e bindings) 4
  | Shr16 e -> Int64.shift_right_logical (eval_expr e bindings) 16
  | And (e1, e2) -> Int64.logand (eval_expr e1 bindings) (eval_expr e2 bindings)
  | Or (e1, e2) -> Int64.logor (eval_expr e1 bindings) (eval_expr e2 bindings)
  | Xor (e1, e2) -> Int64.logxor (eval_expr e1 bindings) (eval_expr e2 bindings)
  | Plus (e1, e2) -> Int64.add (eval_expr e1 bindings) (eval_expr e2 bindings)
  | Fold (e_to_fold, e_initial, id1, id2, e_fold) -> (
      let v_run = ref (eval_expr e_to_fold bindings) in
      let v_result = ref (eval_expr e_initial bindings) in
      for i = 1 to 8 do
        let new_bindings = bindings in
        let new_bindings = Bindings.add id1 (Int64.logand !v_run (Int64.of_int 0xff))  new_bindings in
        let new_bindings = Bindings.add id2 !v_result new_bindings in
        let res = eval_expr e_fold new_bindings in
        v_run := Int64.shift_right !v_run 8;
        v_result := res;
      done;
      !v_result
  )

let eval p param =
  let id, exp = p in
  let bindings = Bindings.add id param Bindings.empty in
  eval_expr exp bindings

let eval_s p param = eval (parse p) param
