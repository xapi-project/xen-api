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

open Opentelemetry

let capacity = Atomic.make 100

let set_capacity = Atomic.set capacity

type t = {
    scope: Scope.t
  ; attrs: key_value list Bounded.t
  ; events: Event.t Bounded.t
  ; metrics: Metrics.t Bounded.t
  ; logs: Logs.t Bounded.t
  ; delayed_logs: (unit -> Logs.t) Bounded.t
  ; mutable status: Span_status.t option
  ; mutable decision: Sampling.decision
}

let make scope decision =
  let capacity = Atomic.get capacity in
  {
    scope
  ; attrs= Bounded.make capacity
  ; events= Bounded.make capacity
  ; metrics= Bounded.make capacity
  ; logs= Bounded.make capacity
  ; delayed_logs= Bounded.make capacity
  ; decision
  ; status= None
  }

let ambient_scope_key = Ambient_context.create_key ()

let[@inline always] get_ambient_scope ?scope () =
  match scope with
  | Some _ ->
      scope
  | None ->
      Ambient_context.get ambient_scope_key

let[@inline always] with_ambient_scope t f =
  Ambient_context.with_binding ambient_scope_key t (fun _ -> f ())

let[@inline always] trace_id t = t.scope.Scope.trace_id

let[@inline always] span_id t = t.scope.Scope.span_id

let[@inline always] to_span_ctx t = Scope.to_span_ctx t.scope

let[@inline always] is_recording t =
  match t.decision with
  | DROP ->
      false
  | RECORD_ONLY | RECORD_AND_SAMPLE ->
      true

let[@inline always] is_sampled t =
  match t.decision with
  | DROP | RECORD_ONLY ->
      false
  | RECORD_AND_SAMPLE ->
      true

let[@inline always] set_decision t decision = t.decision <- decision

let[@inline always] scope t = t.scope

let[@inline always] status t = t.status

let[@inline always] set_status t status = t.status <- Some status

let[@inline always] bounded_add_ignore_exn ~__FUNCTION__ queue t f =
  if is_recording t then Bounded.add queue @@ f ()

let[@inline always] add_attrs t = bounded_add_ignore_exn ~__FUNCTION__ t.attrs t

let[@inline always] add_event t =
  bounded_add_ignore_exn ~__FUNCTION__ t.events t

let[@inline always] add_metrics t =
  bounded_add_ignore_exn ~__FUNCTION__ t.metrics t

let[@inline always] add_log t f =
  if is_recording t then
    let log = f () in
    if log.Opentelemetry_proto.Logs.severity_number >= Severity_number_info then
      Logs.emit [log]
    else
      Bounded.add t.logs log

let[@inline always] add_delayed_log t f =
  if is_recording t then Bounded.add t.delayed_logs f

let update_log t log =
  let span_id = t |> span_id |> Span_id.to_bytes
  and trace_id = t |> trace_id |> Trace_id.to_bytes in
  Opentelemetry_proto.Logs.{log with span_id; trace_id}

(* may need to update or add exemplars to link *)
let update_metric = Fun.id

let bounded_iter_delayed b f = b |> Bounded.to_seq |> Seq.iter f

let bounded_iter b f = bounded_iter_delayed b @@ fun item -> f (fun () -> item)

let bounded_emit b update f =
  b |> Bounded.to_seq |> Seq.map update |> List.of_seq |> f

let finish_span t =
  bounded_iter t.attrs (Scope.add_attrs t.scope) ;
  bounded_iter t.events (Scope.add_event t.scope) ;
  Bounded.clear t.attrs ;
  Bounded.clear t.events

let finish_logs_metrics t =
  if is_sampled t then begin
    bounded_emit t.metrics update_metric Metrics.emit ;

    (* we don't have access to report the dropped counts in the proto *)

    (* transfer delayed logs to regular logs, must be done before regular logs
       are emitted *)
    bounded_iter_delayed t.delayed_logs (add_log t) ;
    Bounded.clear t.delayed_logs ;

    bounded_emit t.logs (update_log t) Logs.emit
  end ;

  (* ensure we don't emit duplicates if this function gets called multiple
     times *)
  Bounded.clear t.metrics ;
  Bounded.clear t.logs ;
  Bounded.clear t.delayed_logs
