(* vim: set ts=2 tw=0 foldmethod=marker : *)

let silent = ref false

let set_silent_mode is_silent = silent := is_silent

let say s  = Printf.kprintf (fun n -> if not !silent then Printf.printf "%s\n%!" n) s
let shout s  = Printf.kprintf (fun n -> Printf.printf "%s\n%!" n) s

let starting_from s pos =
  let l = String.length s in
  if l <= pos then ""
  else String.sub s pos (l - pos)


let left s n_chars =
  let len = String.length s in
  if n_chars >= len then s else String.sub s 0 n_chars


let starts_with needle haystack =
  left haystack (String.length needle) = needle


let ltrim s =
  let len = String.length s in
  let rec start_of_nonspace pos =
    if len > pos && s.[pos] = ' ' then start_of_nonspace (pos + 1)
    else pos
  in
  let pos = start_of_nonspace 0 in
  if pos = 0 then s else String.sub s pos (len - pos)


let json_get_json json_obj search_key =

  let rec search_in_assoc = function
    | (key, s) :: rest -> if key = search_key then s else search_in_assoc rest
    | [] -> `Null
  in

  match json_obj with
  | `Assoc l -> search_in_assoc l
  | _ -> shout "json_get_assoc(%s): crap instead of json-object" search_key; `Null


let json_get_string json_obj search_key =
  match json_get_json json_obj search_key with
  | `String s -> s
  | _ -> ""

let json_get_string_list json_obj search_key =
  let rec eat_strings accum = function
    | `String foo :: rest -> eat_strings (List.append accum [foo]) rest
    | _ :: rest -> eat_strings accum rest
    | [] -> accum
  in

  match json_get_json json_obj search_key with
  | `List s -> eat_strings [] s
  | _ -> []


let json_get_int json_obj search_key =
  match json_get_json json_obj search_key with
  | `Int s -> s
  | _ -> 0


let json_list_of_strings sl =
  `List (List.map (fun n -> `String n) sl)

let make_rotator () =
  let hi = ref 0
  and rot = ref 0
  and ks = ref 0
  and rot_chars = "\\|/-" in
  fun () ->
    hi := !hi + 1;
    if !hi = 10000 then (
      hi := 0;
      ks := !ks + !hi;
      rot := (!rot + 1) mod 4;
      Printf.printf "Solving (%dm) %c\r%!" (!ks / 1000000) rot_chars.[ !rot ]
    );


