
  type op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
  type op2 = And | Or | Xor | Plus
  type expr = E_0
    | E_1
    | Identifier of string
    | If0 of expr * expr * expr
    | Fold of expr * expr * string * string * expr
    | Op1 of op1 * expr
    | Op2 of op2 * expr * expr
  type program = string * expr

  val parse : string -> program

  val expr_to_s : expr -> string
  val op1_to_s : op1 -> string
  val op2_to_s : op2 -> string
  val program_to_s : program -> string
