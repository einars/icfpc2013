(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf

exception Parse_failed

type op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
type op2 = And | Or | Xor | Plus
type expr = E_0 | E_1 | Identifier of string
  | If0 of expr * expr * expr
  | Fold of expr * expr * string * string * expr
  | Op1 of op1 * expr | Op2 of op2 * expr * expr
type program = string * expr

let parse s = ("test", (Op1 (Not, Identifier "test")))

let op1_to_s = function
| Not -> "not"
| Shl1 -> "shl1"
| Shr1 -> "shr1"
| Shr4 -> "shr4"
| Shr16 -> "shr16"

let op2_to_s = function
| And -> "and"
| Or -> "or"
| Xor -> "xor"
| Plus -> "plus"

let rec expr_to_s = function
| E_0 -> "0"
| E_1 -> "1"
| Identifier id -> id
| If0 (e1, e2, e3) -> Printf.sprintf "(if0 %s %s %s" (expr_to_s e1) (expr_to_s e2) (expr_to_s e3)
| Fold (e1, e2, id1, id2, e3) ->
    Printf.sprintf "(fold %s %s (lambda (%s %s) %s)"
      (expr_to_s e1)
      (expr_to_s e2)
      id1
      id2
      (expr_to_s e3)
| Op1 (op, e) ->
    Printf.sprintf "(%s %s)" (op1_to_s op) (expr_to_s e)
| Op2 (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (op2_to_s op) (expr_to_s e1) (expr_to_s e2)

let program_to_s p = match p with
| id, e ->
    Printf.sprintf "(lambda (%s) %s)"
      id
      (expr_to_s e)
