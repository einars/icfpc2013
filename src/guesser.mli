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

val start : int -> string list -> string -> guessbox_t
val solve : guessbox_t -> program
val step2  : guessbox_t -> program -> guessbox_t

val suitable_first_inputs : unit -> int64 list