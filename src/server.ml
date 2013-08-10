open Printf
open Http_client.Convenience

exception Eval_failed of string
exception Solved of string*string

let key = ref ""

let set_key s = key := s

let assert_key_set () =
  Http_client.Convenience.http_trials := 1;
  if !key = "" then failwith "Use Server.set_key to set the competition key"

type problem_description_t =
  { problem_id : string
  ; problem_size : int
  ; operators : string array }


type contest_status_t =
  { contest_score : int
  ; training_score : int
  ; num_requests : int }


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
      printf "Too many requests, sleeping for 3s%!";
      for i = 1 to 3 do
        printf ".%!";
        ignore( Unix.select [] [] [] 1.0 );
      done;
      printf "\n%!";
      (with_http_error_handling fn) x;
    end else failwith (sprintf "%d: %s" id msg)
  )


let http_get url =
  assert_key_set ();
  (with_http_error_handling http_get) url


let rec http_post url body =

  assert_key_set ();

    let run_req = fun url body -> (
      let req = new Http_client.post_raw url body in
      let pipe = new Http_client.pipeline in
      req # set_accept_encoding ();
      pipe # add req;
      pipe # run ();
      req # get_resp_body () ) in

    (
      try
        run_req url body
      with Http_client.Http_error (id, msg) ->
        if msg = "Too many requests" then begin
          printf "Too many requests, sleeping for 3s%!";
          for i = 1 to 3 do
            printf ".%!";
            ignore( Unix.select [] [] [] 1.0 );
          done;
          printf "\n%!";
          http_post url body
        end else failwith (sprintf "%d: %s" id msg)
    )


let get_status () =

  let status_json = http_get (sprintf "http://icfpc2013.cloudapp.net/status?auth=%s" !key)  in
  let parsed = Yojson.Safe.from_string status_json in
  let cs = Helpers.json_get_int parsed "contestScore"
  and ts = Helpers.json_get_int parsed "trainingScore"
  and nr = Helpers.json_get_int parsed "numRequests" in

  { contest_score = cs
  ; training_score = ts
  ; num_requests = nr }


let get_training size operator_mode =

  let req_json =
    if operator_mode = "" || size = 42 then sprintf "{\"size\":%d}" size
    else if operator_mode = "none" then sprintf "{\"size\":%d,\"operators\":[]}" size
    else if operator_mode = "fold" then sprintf "{\"size\":%d,\"operators\":[\"fold\"]}" size
    else if operator_mode = "tfold" then sprintf "{\"size\":%d,\"operators\":[\"tfold\"]}" size
    else failwith ("Unknown operator mode " ^ operator_mode) in

  let status_json = http_post (sprintf "http://icfpc2013.cloudapp.net/train?auth=%s" !key) req_json in
  let parsed = Yojson.Safe.from_string status_json in
  Helpers.say " :: challenge %s" (Helpers.json_get_string parsed "challenge");
  { problem_id = Helpers.json_get_string parsed "id"
  ; problem_size = Helpers.json_get_int parsed "size"
  ; operators = Array.of_list ( Helpers.json_get_string_list parsed "operators" ) }


let get_eval id params =

  if params = [] || id = "" then []
  else begin
    let req_json = Yojson.Safe.to_string( `Assoc
      [ ("id", `String id)
      ; ("arguments", Helpers.json_list_of_strings (List.map (fun x -> sprintf "0x%016Lx" x) params))
      ]) in
    let status_json = http_post (sprintf "http://icfpc2013.cloudapp.net/eval?auth=%s" !key) req_json in
    let parsed = Yojson.Safe.from_string status_json in

    let status = Helpers.json_get_string parsed "status"
    and message = Helpers.json_get_string parsed "message" in
    if status <> "ok" then raise (Eval_failed message);

    List.map Int64.of_string (Helpers.json_get_string_list parsed "outputs");
  end



let guess id program_source =
  let req_json = Yojson.Safe.to_string ( `Assoc
    [ ( "id", `String id)
    ; ("program", `String program_source) ]) in
  let status_json = http_post (sprintf "http://icfpc2013.cloudapp.net/guess?auth=%s" !key) req_json in
  let status_p = Yojson.Safe.from_string status_json in
  let status  = Helpers.json_get_string      status_p "status"
  and values  = Helpers.json_get_string_list status_p "values"
  and message = Helpers.json_get_string      status_p "message"
  in
  if status = "win" then raise (Solved (id, program_source))
  else if status = "error" then failwith message
  else if status = "mismatch" then
    let input = Int64.of_string (List.nth values 0)
    and expected = Int64.of_string (List.nth values 1)
    in (input, expected)

  else failwith (Printf.sprintf "Unknown status %s" status)


let get_real id =
  let json_p = Yojson.Safe.from_file "web/problems/current.js" in

  let rec find_matching_problem = function
    | elem :: rest -> if Helpers.json_get_string elem "id" = id then
      { problem_id = id
      ; problem_size = Helpers.json_get_int elem "size"
      ; operators = Array.of_list (Helpers.json_get_string_list elem "operators")
      } else find_matching_problem rest
    | [] -> failwith "No such problem"
  in
  match json_p with
  | `List problems -> find_matching_problem problems
  | _ -> failwith "Weird real problem structure"

