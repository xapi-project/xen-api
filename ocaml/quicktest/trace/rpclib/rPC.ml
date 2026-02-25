(*
 * Copyright (c) Cloud Software Group, Inc
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
open Quicktest_trace
open Conventions

let id_sent = Atomic.make 1

let id_received = Atomic.make 1

let rpc_event ~id kind =
  let id = Atomic.fetch_and_add id 1 in
  Opentelemetry.Event.make
    ~attrs:[(rpc_message_id, `Int id); (rpc_message_type, `String kind)]
    rpc_message

let code_of_response response =
  match response.Rpc.contents with
  | Rpc.Enum (Rpc.String code :: _) ->
      code
  | _ ->
      other

let attrs_of_error response =
  [(error_type, `String (code_of_response response))]

let rec any_value_of_rpc =
  let open Opentelemetry_proto.Common in
  function
  | Rpc.Null ->
      None
  | Rpc.Bool b ->
      Some (Bool_value b)
  | Rpc.Int32 i32 ->
      Some (Int_value (Int64.of_int32 i32))
  | Rpc.Int i ->
      Some (Int_value i)
  | Rpc.Float f ->
      Some (Double_value f)
  | Rpc.Base64 s ->
      (* don't log the full Base64 entry, the UEFI NVRAM is huge *)
      Some (String_value (Printf.sprintf "base64(len=%d)" (String.length s)))
  | Rpc.DateTime s | Rpc.String s ->
      Some (String_value s)
  | Rpc.Enum lst ->
      let values = lst |> List.filter_map any_value_of_rpc in
      Some (Array_value (make_array_value ~values ()))
  | Rpc.Dict dict ->
      let values =
        dict
        |> List.map (fun (key, v) ->
            make_key_value ~key ~value:(any_value_of_rpc v) ()
        )
      in
      Some (Kvlist_value (make_key_value_list ~values ()))

let log_rpc ?(time_unix_nano = Opentelemetry.Timestamp_ns.now_unix_ns ()) scope
    key rpc =
  let trace_id = Scope.trace_id scope |> Opentelemetry.Trace_id.to_bytes
  and span_id = Scope.span_id scope |> Opentelemetry.Span_id.to_bytes in
  let open Opentelemetry_proto in
  let body =
    Some
      Common.(
        Kvlist_value
          (make_key_value_list
             ~values:[make_key_value ~key ~value:(any_value_of_rpc rpc) ()]
             ()
          )
      )
  in
  Logs.make_log_record ~time_unix_nano ~observed_time_unix_nano:time_unix_nano
    ~severity_number:Severity_number_trace ~severity_text:"TRACE" ~body
    ~attributes:[] ~dropped_attributes_count:0l ~flags:0l ~trace_id ~span_id ()

let wrap ?(log_body = false) rpc call =
  let attrs =
    [(rpc_system_name, `String "xmlrpc"); (rpc_method, `String call.Rpc.name)]
  in
  let () =
    if log_body then
      match Scope.get_ambient_scope () with
      | None ->
          ()
      | Some scope ->
          (* log the actual bodies of the RPC,
           this is only for testing purposes, since they may contain secrets *)
          Scope.add_log scope (fun () ->
              log_rpc scope call.Rpc.name (Rpc.Enum call.Rpc.params)
          )
  in
  Trace.with_ ~kind:Opentelemetry.Span.Span_kind_client ~attrs call.Rpc.name
  @@ fun scope ->
  Scope.add_event scope (fun () -> rpc_event ~id:id_sent sent) ;

  let (response : Rpc.response) = rpc call in

  Scope.add_event scope (fun () -> rpc_event ~id:id_received received) ;
  if log_body then
    Scope.add_delayed_log scope (fun () ->
        log_rpc scope "response" response.contents
    ) ;

  if not response.Rpc.success then begin
    Scope.set_status scope
    @@ Span_status.make ~code:Status_code_error
         ~message:(code_of_response response) ;
    Scope.add_attrs scope (fun () -> attrs_of_error response)
  end ;
  response

let http_headers () =
  match Scope.get_ambient_scope () with
  | None ->
      []
  | Some scope ->
      let open Opentelemetry.Trace_context.Traceparent in
      let traceparent =
        to_value ~trace_id:(Scope.trace_id scope)
          ~parent_id:(Scope.span_id scope) ()
      in
      Scope.add_attrs scope (fun () -> [(name, `String traceparent)]) ;
      [(name, traceparent)]
