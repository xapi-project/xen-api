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

module MyScope = Scope
open Opentelemetry
include Opentelemetry.Trace

let ok = Ok ()

let add_event = `use_Scope

let add_attrs = `use_Scope

let no_scope =
  MyScope.make
    Scope.
      {span_id= Span_id.dummy; trace_id= Trace_id.dummy; events= []; attrs= []}
    Sampling.DROP

(* workaround for old Opentelemetry version, newer versions support this
   directly *)
let set_status_from_result scope result =
  if Option.is_none @@ MyScope.status scope then
    MyScope.set_status scope
    @@
    match result with
    | Ok () ->
        Span_status.(make ~code:Status_code_ok ~message:"")
    | Error (message, _) ->
        Span_status.(make ~code:Status_code_error ~message)

let trace_result_of scope result =
  match (result, MyScope.status scope) with
  | Ok (), Some Span_status.{code= Status_code_error; message} ->
      Error (message, Printexc.get_callstack 0)
  | _ ->
      result

let with_' ?(force_new_trace_id = false) ?trace_state ?service_name ?attrs ?kind
    ?trace_id ?parent ?scope ?links name f =
  let scope = MyScope.get_ambient_scope ?scope () in
  let parent_scope =
    if force_new_trace_id then
      None
    else
      scope
  in
  let sampling =
    Sampler.should_sample ?parent_scope ?trace_id ?kind ?attrs ?links
      ?trace_state name
  in
  let my_scope = ref no_scope in

  let f oscope =
    let scope = MyScope.make oscope sampling.decision in
    my_scope := scope ;
    MyScope.with_ambient_scope scope @@ fun () ->
    if MyScope.is_recording scope then SpanProcessor.on_start scope ;
    MyScope.add_attrs scope (fun () -> sampling.attrs) ;
    try f scope
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Backtrace.is_important e ;
      MyScope.add_attrs scope (fun () ->
          [
            ( "exception.stacktrace"
            , `String (e |> Backtrace.get |> Backtrace.to_string_hum)
            )
          ]
      ) ;
      Printexc.raise_with_backtrace e bt
  in

  let thunk, finally =
    with_' ~force_new_trace_id ?trace_state:sampling.trace_state ?service_name
      ?kind ?trace_id ?parent
      ?scope:Option.(map MyScope.scope scope)
      ?links name f
  in

  let finally result =
    let scope = !my_scope in
    set_status_from_result scope result ;
    MyScope.finish_span scope ;
    if MyScope.is_recording scope then SpanProcessor.on_end scope ;

    if MyScope.is_sampled scope then
      (* only send sampled spans to the backend, drop others *)
      trace_result_of scope result |> finally ;
    (* emit span first, logs and metrics after, to retain
       order when printed to console *)
    MyScope.finish_logs_metrics scope
  in
  (thunk, finally)

let with_ ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind ?trace_id
    ?parent ?scope ?links name f =
  let thunk, finally =
    with_' ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind ?trace_id
      ?parent ?scope ?links name f
  in
  try
    let r = thunk () in
    finally ok ; r
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Backtrace.is_important exn ;
    finally (Error (Printexc.to_string exn, bt)) ;
    Printexc.raise_with_backtrace exn bt

let with_result ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind
    ?trace_id ?parent ?scope ?links error_to_string name f =
  let thunk, finally =
    with_' ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind ?trace_id
      ?parent ?scope ?links name f
  in
  match thunk () with
  | Ok _ as r ->
      finally ok ; r
  | Error e as r ->
      finally (Error (error_to_string e, Printexc.get_callstack 0)) ;
      r
