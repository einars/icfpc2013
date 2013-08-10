open Printf
open Http_client.Convenience

exception Eval_failed of string
exception Solved of string*string

let key = ref ""

let set_key s = key := s

let assert_key_set () =
  Http_client.Convenience.http_trials := 1;
  if !key = "" then failwith "Use Server.set_key to set the competition key"


type contest_status =
  { contest_score : int
  ; training_score : int
  ; num_requests : int }

type training_entry =
  { challenge : string
  ; id : string
  ; size : int
  ; operators : string list }

let is_usable_cache f =
  if not (Sys.file_exists "cache") then Unix.mkdir "cache" 0o755;
  if Sys.file_exists f then
    true
  else
    false

let rec with_http_error_handling fn = fun (x) -> (try
  printf "trying!%!";
  fn x
  with (Http_client.Http_error (id, msg)) ->
    printf "dead!%!";
    if msg = "Too many requests" then begin
      printf "Too many requests, sleeping for 20s%!";
      for i = 0 to 20 do
        printf ".%!";
        ignore( Unix.select [] [] [] 1.0 );
      done;
      printf "\n%!";
      (with_http_error_handling fn) x;
    end else failwith (sprintf "%d: %s" id msg)
  )


let cached_http_get ?(use_cached_copy = true) url =

  assert_key_set ();

  let md = Digest.to_hex ( Digest.string url ) in
  let cache_file = sprintf "cache/%s" md in


  if use_cached_copy && is_usable_cache cache_file then begin
    let f = open_in cache_file in
    let ret = Std.input_all f in
    close_in f;
    ret
  end else begin
    let ret = (with_http_error_handling http_get) url in
    let f = open_out cache_file in
    output_string f ret;
    close_out f;
    ret
  end


let rec cached_http_post ?(use_cached_copy = true) url body =

  printf "train: %s %s" url body;

  assert_key_set ();

  let md = Digest.to_hex ( Digest.string (url ^ body) ) in
  let cache_file = sprintf "cache/%s" md in

  if use_cached_copy && is_usable_cache cache_file then begin
    let f = open_in cache_file in
    let ret = Std.input_all f in
    close_in f;
    ret
  end else begin

    let run_req = fun url body -> (
      let req = new Http_client.post_raw url body in
      let pipe = new Http_client.pipeline in
      req # set_accept_encoding ();
      pipe # add req;
      pipe # run ();
      req # get_resp_body () ) in

    let ret = try
      run_req url body 
    with Http_client.Http_error (id, msg) ->
      if msg = "Too many requests" then begin
        printf "Too many requests, sleeping for 20s%!";
        for i = 0 to 20 do
          printf ".%!";
          ignore( Unix.select [] [] [] 1.0 );
        done;
        printf "\n%!";
        cached_http_post url body
      end else failwith (sprintf "%d: %s" id msg)
    in

    let f = open_out cache_file in
    output_string f ret;
    close_out f;
    ret
  end

let get_status ?(use_cached_copy=true) () =

  let status_json = cached_http_get ~use_cached_copy:use_cached_copy (sprintf "http://icfpc2013.cloudapp.net/status?auth=%s" !key)  in
  let parsed = Yojson.Safe.from_string status_json in
  let cs = Helpers.json_get_int parsed "contestScore"
  and ts = Helpers.json_get_int parsed "trainingScore"
  and nr = Helpers.json_get_int parsed "numRequests" in

  { contest_score = cs
  ; training_score = ts
  ; num_requests = nr }


let get_training ?(use_cached_copy:bool = true) size =

  (* let req_json = sprintf "{\"size\":%d,\"operators\":[\"tfold\"]}" size in *)
  let req_json = sprintf "{\"size\":%d,\"operators\":[\"fold\"]}" size in
  (* let req_json = sprintf "{\"size\":%d,\"operators\":[\"fold\"]}" size in *)
  (* let req_json = Yojson.Safe.to_string ( `Assoc [ ("size", `Int size) ] ) in *)
  Helpers.say "%s" req_json;


  let status_json = cached_http_post ~use_cached_copy:use_cached_copy (sprintf "http://icfpc2013.cloudapp.net/train?auth=%s" !key) req_json in
  let parsed = Yojson.Safe.from_string status_json in
  { challenge = Helpers.json_get_string parsed "challenge"
  ; id = Helpers.json_get_string parsed "id"
  ; size = Helpers.json_get_int parsed "size"
  ; operators = Helpers.json_get_string_list parsed "operators" }


let get_eval ?(use_cached_copy:bool=true) id params =

  if params = [] || id = "" then []
  else begin
    let req_json = Yojson.Safe.to_string( `Assoc
      [ ("id", `String id)
      ; ("arguments", Helpers.json_list_of_strings (List.map (fun x -> sprintf "0x%016Lx" x) params))
      ]) in
    let status_json = cached_http_post ~use_cached_copy:use_cached_copy (sprintf "http://icfpc2013.cloudapp.net/eval?auth=%s" !key) req_json in
    let parsed = Yojson.Safe.from_string status_json in

    let status = Helpers.json_get_string parsed "status"
    and message = Helpers.json_get_string parsed "message" in
    if status <> "ok" then raise (Eval_failed message);

    List.map Int64.of_string (Helpers.json_get_string_list parsed "outputs");
  end



let guess ?(use_cached_copy:bool=true) id program_source =
  let req_json = Yojson.Safe.to_string ( `Assoc
    [ ( "id", `String id)
    ; ("program", `String program_source) ]) in
  let status_json = cached_http_post ~use_cached_copy:use_cached_copy (sprintf "http://icfpc2013.cloudapp.net/guess?auth=%s" !key) req_json in
  let status_p = Yojson.Safe.from_string status_json in
  let status  = Helpers.json_get_string      status_p "status"
  and values  = Helpers.json_get_string_list status_p "values"
  and message = Helpers.json_get_string      status_p "message"
  in
  if status = "win" then raise (Solved (id, program_source))
  else if status = "error" then failwith message
  else if status = "mismatch" then
    let input = Int64.of_string (List.nth values 0)
    and expected = Int64.of_string (List.nth values 2)
    in (input, expected)

  else failwith (Printf.sprintf "Unknown status %s" status)

