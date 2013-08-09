(* vim: set ts=2 tw=0 foldmethod=marker : *)

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



