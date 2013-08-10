
val set_key : string -> unit

type contest_status_t =
  { contest_score : int
  ; training_score : int
  ; num_requests : int }

type training_entry_t =
  { challenge : string
  ; id : string
  ; size : int
  ; operators : string list }

type real_entry_t =
  { real_id : string
  ; real_size: int
  ; real_operators: string list }


val get_real_problem : string -> real_entry_t

val get_status : ?use_cached_copy:bool -> unit -> contest_status_t

val get_training : ?use_cached_copy:bool -> int -> string -> training_entry_t

exception Eval_failed of string
exception Solved of string*string

val get_eval : ?use_cached_copy:bool -> string -> int64 list -> int64 list
val guess : ?use_cached_copy:bool -> string -> string -> int64 * int64

