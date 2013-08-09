
val set_key : string -> unit

type contest_status =
  { contest_score : int
  ; training_score : int
  ; num_requests : int }

type training_entry =
  { challenge : string
  ; id : string
  ; size : int
  ; operators : string list }

val get_status : ?use_cached_copy:bool -> unit -> contest_status

val get_training : ?use_cached_copy:bool -> ?size:int -> ?operators:string list -> unit -> training_entry
