open Api_server_common
module Server = Server.Make (Actions) (Forwarder)

let ( let@ ) f x = f x

(* This bit is called directly by the fake_rpc callback *)
let callback1 ?(json_rpc_version = Jsonrpc.V1) is_json req fd call =
  let@ req = Http.Request.with_tracing ~name:__FUNCTION__ req in
  (* We now have the body string, the xml and the call name, and can also tell *)
  (* if we're a master or slave and whether the call came in on the unix domain socket or the tcp socket *)
  (* If we're a slave, and the call is from the unix domain socket or from the HIMN, and the call *isn't* *)
  (* in the whitelist, then forward *)
  let whitelisted = List.mem call.Rpc.name whitelist in
  let emergency_call = List.mem call.Rpc.name emergency_call_list in
  let is_slave = not (Pool_role.is_master ()) in
  if !Xapi_globs.slave_emergency_mode && not emergency_call then
    raise !Xapi_globs.emergency_mode_error ;
  if
    is_slave
    && ((Context.is_unix_socket fd && not whitelisted)
       || (is_himn_req req && not emergency_call)
       )
  then
    forward req call is_json
  else
    let response =
      let@ req = Http.Request.with_tracing ~name:"Server.dispatch_call" req in
      Server.dispatch_call req fd call
    in
    let translated =
      if
        is_json
        && json_rpc_version = Jsonrpc.V2
        && (not response.Rpc.success)
        && call.Rpc.name <> "system.listMethods"
      then
        let message, data =
          match response.Rpc.contents with
          | Rpc.Enum (Rpc.String s :: tl) ->
              (s, Rpc.Enum tl)
          | _ ->
              ("", response.Rpc.contents)
        in
        {
          response with
          Rpc.contents=
            json_of_error_object ~data:(Some data) error_code_lit message
        }
      else
        response
    in
    translated

(* debug(fmt "response = %s" response); *)

let is_host_is_slave_error (response : Rpc.response) =
  match response.contents with
  (* RPC response returned by the rpc endpoint *)
  | Rpc.(Enum [String x; String _]) when x = Api_errors.host_is_slave ->
      true
  (* RPC response returned by the jsonrpc endpoint *)
  | Rpc.(Dict [_; ("message", String x); _]) when x = Api_errors.host_is_slave
    ->
      true
  | _ ->
      false

let create_thumbprint_header req response =
  let hash_type_opt =
    match
      List.assoc_opt
        !Xapi_globs.cert_thumbprint_header_request
        req.Http.Request.additional_headers
    with
    | Some x when x = !Xapi_globs.cert_thumbprint_header_value_sha256 ->
        Some `Sha256
    | Some x when x = !Xapi_globs.cert_thumbprint_header_value_sha1 ->
        Some `Sha1
    | _ ->
        None
  in
  Option.bind hash_type_opt (fun hash_type ->
      if is_host_is_slave_error response then
        Helpers.external_certificate_thumbprint_of_master ~hash_type
      else
        None
  )
  |> Option.fold ~none:[] ~some:(fun x ->
         [(!Xapi_globs.cert_thumbprint_header_response, x)]
     )

(** HTML callback that dispatches an RPC and returns the response. *)
let callback is_json req bio _ =
  let@ req = Http.Request.with_tracing ~name:__FUNCTION__ req in
  let span = Http.Request.traceparent_of req in
  let fd = Buf_io.fd_of bio in
  (* fd only used for writing *)
  let body =
    Http_svr.read_body ~limit:Constants.http_limit_max_rpc_size req bio
  in
  try
    let rpc =
      let attributes = [("size", string_of_int (String.length body))] in
      let@ _ =
        Tracing.with_child_trace ~attributes ~name:"Xmlrpc.call_of_string" span
      in
      Xmlrpc.call_of_string body
    in
    let response = callback1 is_json req fd rpc in
    let response_str =
      let@ _ =
        Tracing.with_child_trace ~name:"Xmlrpc.string_of_response" span
      in
      if rpc.Rpc.name = "system.listMethods" then
        let inner = Xmlrpc.to_string response.Rpc.contents in
        Printf.sprintf
          "<?xml \
           version=\"1.0\"?><methodResponse><params><param>%s</param></params></methodResponse>"
          inner
      else
        Xmlrpc.string_of_response response
    in
    let thumbprint_header = create_thumbprint_header req response in
    Http_svr.response_fct req
      ~hdrs:
        ((Http.Hdr.content_type, "text/xml")
        :: ("Access-Control-Allow-Origin", "*")
        :: ("Access-Control-Allow-Headers", "X-Requested-With")
        :: thumbprint_header
        )
      fd
      (Int64.of_int @@ String.length response_str)
      (fun fd -> Unixext.really_write_string fd response_str |> ignore)
  with
  | Api_errors.Server_error (err, params) ->
      Http_svr.response_str req
        ~hdrs:[(Http.Hdr.content_type, "text/xml")]
        fd
        (Xmlrpc.string_of_response
           (Rpc.failure
              (Rpc.Enum (List.map (fun s -> Rpc.String s) (err :: params)))
           )
        )
  | e ->
      Backtrace.is_important e ; raise e

(** HTML callback that dispatches an RPC and returns the response. *)
let jsoncallback req bio _ =
  let@ req = Http.Request.with_tracing ~name:__FUNCTION__ req in
  let fd = Buf_io.fd_of bio in
  (* fd only used for writing *)
  let body =
    Http_svr.read_body ~limit:Xapi_database.Db_globs.http_limit_max_rpc_size req
      bio
  in
  try
    let json_rpc_version, id, rpc =
      Jsonrpc.version_id_and_call_of_string body
    in
    let rpc_response = callback1 ~json_rpc_version true req fd rpc in
    let response =
      Jsonrpc.string_of_response ~id ~version:json_rpc_version rpc_response
    in
    let thumbprint_header = create_thumbprint_header req rpc_response in
    Http_svr.response_fct req
      ~hdrs:
        ((Http.Hdr.content_type, "application/json")
        :: ("Access-Control-Allow-Origin", "*")
        :: ("Access-Control-Allow-Headers", "X-Requested-With")
        :: thumbprint_header
        )
      fd
      (Int64.of_int @@ String.length response)
      (fun fd -> Unixext.really_write_string fd response |> ignore)
  with Api_errors.Server_error (err, params) ->
    Http_svr.response_str req
      ~hdrs:[(Http.Hdr.content_type, "application/json")]
      fd
      (Jsonrpc.string_of_response ~version:Jsonrpc.V2
         (Rpc.failure
            (Rpc.Enum (List.map (fun s -> Rpc.String s) (err :: params)))
         )
      )

let options_callback req bio _ =
  let fd = Buf_io.fd_of bio in
  Http_svr.respond_to_options req fd
