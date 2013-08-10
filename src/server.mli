
val set_key : string -> unit

type contest_status_t =
  { contest_score : int
  ; training_score : int
  ; num_requests : int }

type problem_description_t =
  { problem_id : string
  ; problem_size : int
  ; operators : string list }


val get_status : unit -> contest_status_t

val get_training : int -> string -> problem_description_t
val get_real : string -> problem_description_t


exception Eval_failed of string
exception Solved of string*string

val get_eval : string -> int64 list -> int64 list
val guess : string -> string -> int64 * int64

