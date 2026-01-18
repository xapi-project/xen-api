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

include module type of Opentelemetry.Trace

val add_event : [`use_Scope]
(** [add_event] removed, deprecated upstream *)

val add_attrs : [`use_Scope]
(** [add_attrs] removed, deprecated upstream *)

val with_' :
     ?force_new_trace_id:bool
  -> ?trace_state:string
  -> ?service_name:string
  -> ?attrs:Opentelemetry.key_value list
  -> ?kind:Opentelemetry.Span.kind
  -> ?trace_id:Opentelemetry.Trace_id.t
  -> ?parent:Opentelemetry.Span.id
  -> ?scope:Scope.t
  -> ?links:Opentelemetry.Span_link.t list
  -> string
  -> (Scope.t -> 'a)
  -> (unit -> 'a) * ((unit, string * Printexc.raw_backtrace) result -> unit)
(** [with_'] is like {!val:Opentelemetry.Trace.with'}, but with a custom {!module:Scope}.
    This scope supports metrics and logs, and (tail) sampling.
*)

val with_ :
     ?force_new_trace_id:bool
  -> ?trace_state:string
  -> ?service_name:string
  -> ?attrs:Opentelemetry.key_value list
  -> ?kind:Opentelemetry.Span.kind
  -> ?trace_id:Opentelemetry.Trace_id.t
  -> ?parent:Opentelemetry.Span.id
  -> ?scope:Scope.t
  -> ?links:Opentelemetry.Span_link.t list
  -> string
  -> (Scope.t -> 'a)
  -> 'a
(** [with_] is like {!val:Opentelemetry.Trace.with_}, but with a custom {!module:Scope}.
    This scope supports metrics and logs, and (tail) sampling.
*)

val with_result :
     ?force_new_trace_id:bool
  -> ?trace_state:string
  -> ?service_name:string
  -> ?attrs:Opentelemetry.key_value list
  -> ?kind:Opentelemetry.Span.kind
  -> ?trace_id:Opentelemetry.Trace_id.t
  -> ?parent:Opentelemetry.Span.id
  -> ?scope:Scope.t
  -> ?links:Opentelemetry.Span_link.t list
  -> ('b -> string)
  -> string
  -> (Scope.t -> ('a, 'b) result)
  -> ('a, 'b) result
(** [with_] is like {!val:with_}, but it considers spans failed when they
    return an error result, not just when they raise an exception.
*)
