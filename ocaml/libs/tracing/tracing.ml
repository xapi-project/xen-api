(*
 * Copyright (C) 2023 Cloud Software Group
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
type config = Undetermined | Disabled | Url of string

let tracing_config = ref Undetermined

let url_file = "/etc/xapi-tracing-url"

module Http = struct
  module Request = Cohttp.Request.Make (Cohttp_posix_io.Buffered_IO)
  module Response = Cohttp.Response.Make (Cohttp_posix_io.Buffered_IO)

  let get_user_agent () = Sys.argv.(0)

  let post ~url ~route ~body : (string, exn) result =
    try
      let uri = Printf.sprintf "%s%s" url route in
      let uri' = Uri.of_string uri in
      Open_uri.with_open_uri uri' (fun fd ->
          let headers =
            Cohttp.Header.of_list
              [
                ("User-agent", get_user_agent ())
              ; ("content-type", "application/body")
              ; ("content-length", string_of_int (String.length body))
              ]
          in
          let request =
            Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri'
          in
          let ic = Unix.in_channel_of_descr fd in
          let oc = Unix.out_channel_of_descr fd in
          Request.write
            (fun writer -> Request.write_body writer body)
            request oc ;
          match Response.read ic with
          | `Eof ->
              Ok ""
          | `Invalid x ->
              Error (Failure ("invalid read: " ^ x))
          | `Ok response ->
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
              Ok (Buffer.contents body)
      )
    with e -> Error e
end

type span_context = {
    uber_trace_id: string [@key "uber-trace-id"]
  ; traceparent: string [@key "traceparent"]
}
[@@deriving rpcty]

type blob = {span_context: span_context; stamp: float; operation_name: string}
[@@deriving rpcty]

type t = blob option [@@deriving rpcty]

type start_span_body = {operation_name: string; parent: t} [@@deriving rpcty]

let start_span_json ~operation_name ~parent : string =
  {operation_name; parent}
  |> Rpcmarshal.marshal start_span_body.Rpc.Types.ty
  |> Jsonrpc.to_string

let blob_json x : string =
  Rpcmarshal.marshal blob.Rpc.Types.ty x |> Jsonrpc.to_string

let json_of_t : t -> string option = Option.map blob_json

let string_of_t x : string = Option.value ~default:"(empty)" (json_of_t x)

let t_of_string x : t =
  x
  |> Jsonrpc.of_string
  |> Rpcmarshal.unmarshal blob.Rpc.Types.ty
  |> Result.to_option

let empty : t = None

let null = function None -> true | Some _ -> false

let start ~name ~parent : (t, exn) result =
  let call url =
    let body = start_span_json ~operation_name:name ~parent in
    match Http.post ~url ~body ~route:"/start-span" with
    | Ok x ->
        Ok (t_of_string x)
    | Error _ as e ->
        e
  in
  match !tracing_config with
  | Disabled ->
      Ok None
  | Undetermined -> (
    try
      let url =
        Xapi_stdext_unix.Unixext.string_of_file url_file |> String.trim
      in
      tracing_config := Url url ;
      call url
    with e ->
      tracing_config := Disabled ;
      Error e
  )
  | Url url ->
      call url

let finish x : (unit, exn) result =
  match (x, !tracing_config) with
  | Some blob, Url url -> (
      let body = blob_json blob in
      match Http.post ~url ~route:"/finish-span" ~body with
      | Ok x ->
          Ok ()
      | Error _ as e ->
          e
    )
  | _ ->
      Ok ()
