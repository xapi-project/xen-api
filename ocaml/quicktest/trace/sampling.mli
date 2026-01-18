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

(** @see <https://opentelemetry.io/docs/specs/otel/trace/sdk/#shouldsample> *)

open Opentelemetry

type decision =
  | DROP
      (** all events, attributes, logs and metrics associated with the span
          will be dropped, and the span itself won't be emitted to the backend *)
  | RECORD_ONLY
      (** all events, attributes, logs and metrics are recorded, but
          will be dropped at the end, unless the sampling decision is changed *)
  | RECORD_AND_SAMPLE
      (** record all events, attributes, logs and metrics, and emit the span to the
          backend*)

type result = {
    decision: decision  (** the sampling decision for the Span *)
  ; attrs: (string * value) list  (** attributes to add to the Span *)
  ; trace_state: string option  (** override the trace state *)
}
