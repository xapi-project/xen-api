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

(** Wraps {!module:Opentelemetry.Scope}.
    Adds support for bounded items (attributes, events, logs and metrics)
    associated with a span, and sampling.
*)

val set_capacity : int -> unit
(** [set_capacity capacity] sets the maximum number of
    attributes/events/logs/metrics that a span scope can store.
    Changes will take effect for newly created span scopes only.
*)

(** Current span scope *)
type t

val make : Opentelemetry.Scope.t -> Sampling.decision -> t
(** [make oscope decision] initializes a scope with the Opentelemetry [oscope] and a sampling decision.

  {ul
    {- {!val:Sampling.DROP} won't record any attributes,events,metrics,logs and drops the span itself}
    {- {!val:Sampling.RECORD_AND_SAMPLE} records all attributes,events,metrics,logs and emits the span,metrics and logs.
    {- {!val:Sampling.RECORD_ONLY} records all attributes,events,metrics,logs but doesn't emit the span, metrics or logs.
      This can be changed to {!val:Sampling.RECORD_AND_SAMPLE} by a Tail-based
      sampling processor (e.g. to always sample all errors) using {!val:set_decision}
  }
*)

val trace_id : t -> Opentelemetry.Trace_id.t
(** [trace_id t] the {!val:Trace_id.t} of the current span scope *)

val span_id : t -> Opentelemetry.Span_id.t
(** [span_id t] the {!val:Span_id.t} of the current span scope *)

val to_span_ctx : t -> Opentelemetry.Span_ctx.t
(** [to_span_ctx t] the {!val:Span_ctx.t} of the current span scope *)

val is_recording : t -> bool
(** [is_recording t] is true when the sampling decision is
    {!val:Sampling.RECORD_ONLY} or {!val:Sampling.RECORD_AND_SAMPLE} *)

val is_sampled : t -> bool
(** [is_sampled t] is true when the sampling decision is {!val:Sampling.RECORD_AND_SAMPLE} *)

val set_decision : t -> Sampling.decision -> unit
(** [set_decision t] changes the sampling decision on the current span scope. *)

val status : t -> Span_status.t option
(** [status t] is the current span status, if it has been set *)

val set_status : t -> Span_status.t -> unit
(** [set_status t status] sets the span's status to [t].
  If a span's sampling decision is {!val:RECORD_ONLY} this
  automatically upgrades it to {!val:RECORD_AND_SAMPLE}
*)

val add_attrs : t -> (unit -> Opentelemetry.key_value list) -> unit
(** [add_attrs t f] calls [f ()] when [is_recording t] is true.
    If this exceeds the span capacity then the oldest attributes are dropped.

    The call may be deferred to span completion since it relies only on
    immutable values, and contains no timestamps.
    (if an attribute can only be obtain via a side effect then it should be
    a metric or an event instead).
    However for consistency with the other [add_*] functions [f ()] is
    currently called immediately too.
*)

val add_event : t -> (unit -> Opentelemetry.Event.t) -> unit
(** [add_events t f] calls [f ()] when [is_recording t] is true.
    If this exceeds the span capacity then the oldest events are dropped.
    The call to [f ()] is done immediately, because events contain a timestamp
    recorded at creation time, and this cannot be deferred.
*)

val add_metrics : t -> (unit -> Opentelemetry.Metrics.t) -> unit
(** [add_metrics t f] calls [f ()] when [is_recording t] is true.
    If this exceeds the span capacity then the oldest metrics are dropped.
    The call to [f ()] is done immediately, because events contain a timestamp
    recorded at creation time, and may have side effects to sample state,
    and this cannot be deferred.
*)

val add_log : t -> (unit -> Opentelemetry.Logs.t) -> unit
(** [add_log t f] may call [f ()] when [is_recording t] is true.
    The call to [f ()] is done immediately, because formatting logs may have
    side-effects, e.g. reading global state.
    If the log is expensive to compute, consider using {!val:add_delayed_log}
    instead.
    Logs at level info and above are always emitted immediately.
*)

val add_delayed_log : t -> (unit -> Opentelemetry.Logs.t) -> unit
(** [add_log t f] may call [f ()] when [is_sampled t] is true at span completion. *)

(* for internal use in Trace *)

val scope : t -> Opentelemetry.Scope.t
(** [scope t] is the underlying Opentelemetry Scope *)

val finish_span : t -> unit
(** [finish_span t] when [is_sampled ()] it transfers all attributes/events to
    {!val:Opentelemetry.Scope.t}.

    This function should only be called once, otherwise span capacity limits
    cannot be guaranteed.
*)

val finish_logs_metrics : t -> unit
(** [finish_logs_metrics t] when [is_sampled ()] it emits all metrics/logs/delayed logs.

    This function should only be called once, otherwise span capacity limits
    cannot be guaranteed.
*)

val ambient_scope_key : t Ambient_context.key

val get_ambient_scope : ?scope:t -> unit -> t option

val with_ambient_scope : t -> (unit -> 'a) -> 'a
