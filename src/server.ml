open Printf
open Http_client.Convenience

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


let cached_http_get ?(use_cached_copy = true) url =

  let md = Digest.to_hex ( Digest.string url ) in
  let cache_file = sprintf "cache/%s" md in

  if not (Sys.file_exists "cache") then Unix.mkdir "cache" 0o755;

  if use_cached_copy && Sys.file_exists cache_file then begin
    let f = open_in cache_file in
    let ret = Std.input_all f in
    close_in f;
    ret
  end else begin
    let ret = http_get url in
    let f = open_out cache_file in
    output_string f ret;
    close_out f;
    ret
  end


let cached_http_post ?(use_cached_copy = true) url body =

  let md = Digest.to_hex ( Digest.string (url ^ body) ) in
  let cache_file = sprintf "cache/%s" md in

  if not (Sys.file_exists "cache") then Unix.mkdir "cache" 0o755;

  if use_cached_copy && Sys.file_exists cache_file then begin
    let f = open_in cache_file in
    let ret = Std.input_all f in
    close_in f;
    ret
  end else begin
    let req = new Http_client.post_raw url body in
    let pipe = new Http_client.pipeline in
    req # set_accept_encoding ();
    pipe # add req;
    pipe # run ();
    let ret = req # get_resp_body () in

    let f = open_out cache_file in
    output_string f ret;
    close_out f;
    ret
  end


let get_status ?(use_cached_copy=true) () =

  assert_key_set ();
  let status_json = cached_http_get ~use_cached_copy:use_cached_copy (sprintf "http://icfpc2013.cloudapp.net/status?auth=%s" !key)  in
  let parsed = Yojson.Safe.from_string status_json in
  let cs = Helpers.json_get_int parsed "contestScore"
  and ts = Helpers.json_get_int parsed "trainingScore"
  and nr = Helpers.json_get_int parsed "numRequests" in

  { contest_score = cs
  ; training_score = ts
  ; num_requests = nr }



let get_training ?(use_cached_copy:bool = true) ?(size:int=5) ?(operators:string list=[]) () =
  assert_key_set ();

  let req_json = Yojson.Safe.to_string ( `Assoc [ ("size", `Int size) ] ) in
  (* Helpers.say "%s" req_json; *)


  let status_json = cached_http_post ~use_cached_copy:use_cached_copy (sprintf "http://icfpc2013.cloudapp.net/train?auth=%s" !key) req_json in
  let parsed = Yojson.Safe.from_string status_json in
  { challenge = Helpers.json_get_string parsed "challenge"
  ; id = Helpers.json_get_string parsed "id"
  ; size = Helpers.json_get_int parsed "size"
  ; operators = Helpers.json_get_string_list parsed "operators" }
