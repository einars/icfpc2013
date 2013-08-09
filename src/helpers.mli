open Yojson.Safe

val ltrim : string -> string
val left : string -> int -> string
val starting_from : string -> int -> string
val starts_with : string -> string -> bool

val set_silent_mode : bool -> unit
val say : ('a, unit, string, unit) format4 -> 'a

val json_get_json : json -> string -> json
val json_get_string : json -> string -> string
val json_get_string_list : json -> string -> string list
val json_get_int : json -> string -> int

