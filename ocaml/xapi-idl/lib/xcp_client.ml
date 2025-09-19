(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Generic RPC marshalling functions for XCP services *)

module Request = Cohttp.Request.Make (Cohttp_posix_io.Buffered_IO)
module Response = Cohttp.Response.Make (Cohttp_posix_io.Buffered_IO)

let get_user_agent () = Sys.argv.(0)

let switch_path = ref "/var/run/message-switch/sock"

let use_switch = ref true

let get_ok = function
  | Ok x ->
      x
  | Error e ->
      let b = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer b in
      Message_switch_unix.Protocol_unix.Client.pp_error fmt e ;
      Format.pp_print_flush fmt () ;
      failwith (Buffer.contents b)

let switch_rpc ?timeout queue_name string_of_call response_of_string =
  let t =
    get_ok
      (Message_switch_unix.Protocol_unix.Client.connect ~switch:!switch_path ())
  in
  fun (call : Rpc.call) ->
    let _span_parent =
      call.params
      |> List.find_map (function Rpc.Dict kv_list -> Some kv_list | _ -> None)
      |> Fun.flip Option.bind
           (List.find_map (function
             | "debug_info", Rpc.String debug_info ->
                 let di = debug_info |> Debug_info.of_string in
                 di.tracing
             | _ ->
                 None
             ))
    in
    let rpc_service = "message_switch" in
    Tracing.with_tracing
      ~attributes:
        [
          ("rpc.system", "ocaml-rpc")
        ; ("rpc.service", rpc_service)
        ; ("server.address", queue_name)
        ; ("rpc.method", call.name)
        ]
      ~parent:_span_parent
      ~name:(rpc_service ^ "/" ^ call.name)
    @@ fun _span_parent ->
    response_of_string
      (get_ok
         (Message_switch_unix.Protocol_unix.Client.rpc ?_span_parent ~t ?timeout
            ~queue:queue_name ~body:(string_of_call call) ()
         )
      )

let split_colon str =
  try
    let x = String.index str ':' in
    let uname = String.sub str 0 x in
    let passwd = String.sub str (x + 1) (String.length str - x - 1) in
    [uname; passwd]
  with Not_found -> [str]

(* Use HTTP to frame RPC messages *)
[@@@ocaml.warning "-27"]

let http_rpc string_of_call response_of_string ?(srcstr = "unset")
    ?(dststr = "unset") ?verify_cert url call =
  let uri = Uri.of_string (url ()) in
  let req = string_of_call call in
  let headers =
    Cohttp.Header.of_list
      [
        ("User-agent", get_user_agent ())
      ; ("content-length", string_of_int (String.length req))
      ]
  in
  (* If we have a username:password@ then use basic authentication *)
  let userinfo = Uri.userinfo uri in
  let headers =
    match userinfo with
    | Some x -> (
      match split_colon x with
      | [username; password] ->
          Cohttp.Header.add_authorization headers (`Basic (username, password))
      | _ ->
          headers
    )
    | None ->
        headers
  in
  let http_req =
    Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri
  in
  Open_uri.with_open_uri ?verify_cert uri (fun fd ->
      let ic = Unix.in_channel_of_descr fd in
      let oc = Unix.out_channel_of_descr fd in
      Request.write (fun writer -> Request.write_body writer req) http_req oc ;
      match Response.read ic with
      | `Eof ->
          failwith
            (Printf.sprintf "Failed to read HTTP response from: %s" (url ()))
      | `Invalid x ->
          failwith
            (Printf.sprintf "Failed to read HTTP response from: %s (got '%s')"
               (url ()) x
            )
      | `Ok response -> (
          let body = Buffer.create 16 in
          let reader = Response.make_body_reader response ic in
          let rec loop () =
            match Response.read_body_chunk reader with
            | Cohttp.Transfer.Chunk x ->
                Buffer.add_string body x ; loop ()
            | Cohttp.Transfer.Final_chunk x ->
                Buffer.add_string body x
            | Cohttp.Transfer.Done ->
                ()
          in
          loop () ;
          let body = Buffer.contents body |> response_of_string in
          match Cohttp.Response.status response with
          | `OK ->
              body
          | bad ->
              failwith
                (Printf.sprintf "Unexpected HTTP response code: %s"
                   (Cohttp.Code.string_of_status bad)
                )
        )
  )

let xml_http_rpc = http_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string

let json_switch_rpc ?timeout queue_name =
  switch_rpc ?timeout queue_name Jsonrpc.string_of_call
    Jsonrpc.response_of_string

let () =
  Printexc.register_printer (function
    | Xmlm.Error ((line, col), error) ->
        Some
          (Printf.sprintf "Xmlm.Error(%d:%d, \"%s\")" line col
             (Xmlm.error_message error)
          )
    | _ ->
        None
    )

(* Use a binary 16-byte length to frame RPC messages *)
let binary_rpc string_of_call response_of_string ?(srcstr = "unset")
    ?(dststr = "unset") url (call : Rpc.call) : Rpc.response =
  let uri = Uri.of_string (url ()) in
  Open_uri.with_open_uri uri (fun fd ->
      let ic = Unix.in_channel_of_descr fd in
      let oc = Unix.out_channel_of_descr fd in
      let msg_buf = string_of_call call in
      let len = Printf.sprintf "%016d" (String.length msg_buf) in
      output_string oc len ;
      output_string oc msg_buf ;
      flush oc ;
      let len_buf = Bytes.make 16 '\000' in
      really_input ic len_buf 0 16 ;
      let len = int_of_string (Bytes.unsafe_to_string len_buf) in
      let msg_buf = Bytes.make len '\000' in
      really_input ic msg_buf 0 len ;
      let (response : Rpc.response) =
        response_of_string (Bytes.unsafe_to_string msg_buf)
      in
      response
  )

let json_binary_rpc =
  binary_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

let rec retry_econnrefused f =
  try f () with
  | Unix.Unix_error (Unix.ECONNREFUSED, "connect", _) ->
      (* debug "Caught ECONNREFUSED; retrying in 5s"; *)
      Thread.delay 5. ; retry_econnrefused f
  | e ->
      (* error "Caught %s: does the service need restarting?"
         (Printexc.to_string e); *)
      raise e

let retry_and_switch_rpc call ~use_switch ~queue_name ~dststr ~uri =
  retry_econnrefused (fun () ->
      if use_switch then
        json_switch_rpc queue_name call
      else
        xml_http_rpc ~srcstr:(get_user_agent ()) ~dststr uri call
  )
