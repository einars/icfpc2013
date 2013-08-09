
  type expr
    = E_0
    | E_1
    | Identifier of string
    | If0 of expr * expr * expr
    | Fold of expr * expr * string * string * expr
    | Not of expr | Shl1 of expr | Shr1 of expr | Shr4 of expr | Shr16 of expr
    | And of expr * expr | Or of expr * expr | Xor of expr * expr | Plus of expr * expr

  type program = string * expr

  val parse : string -> program

  val expr_to_s : expr -> string
  val program_to_s : program -> string

  (* helpers *)
