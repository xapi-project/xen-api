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

(** @see <https://opentelemetry.io/docs/specs/otel/trace/sdk/#sampler> *)

val should_sample :
     ?parent_scope:Scope.t
  -> ?trace_id:Opentelemetry.Trace_id.t
  -> ?kind:Opentelemetry.Span.kind
  -> ?attrs:Opentelemetry.key_value list
  -> ?links:Opentelemetry.Span_link.t list
  -> ?trace_state:string
  -> string
  -> Sampling.result
(** [should_sample ?parent_scope ?trace_id ?kind ?attrs ?links ?trace_state name]
  returns a (head) sampling decision for the given span.
    It should pass through [trace_state] unchanged if it doesn't intend to
    change it, otherwise it overrides the caller supplied one.
    The sampling decision can be changed later.
*)

val get_description : unit -> string
(** [get_description ()] is the name of the current sampler. *)
