(*
 * Copyright (C) Cloud Software Group
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

type t = {log: string; tracing: Tracing.Span.t option}

let separator = '\n'

let make ~log ~tracing = {log; tracing}

(* This must support the upgrade case, where no tracing data is included *)
let of_string s =
  let open Tracing in
  match String.split_on_char separator s with
  | [log; traceparent] ->
      let spancontext = SpanContext.of_traceparent traceparent in
      let tracing =
        Option.map (fun tp -> Tracer.span_of_span_context tp log) spancontext
      in
      {log; tracing}
  | _ ->
      {log= s; tracing= None}

let filter_separator = Astring.String.filter (( <> ) separator)

(* Construct a string that includes both log data as well as the traceparent *)
let to_string t =
  Option.fold ~none:t.log
    ~some:(fun span ->
      let traceparent =
        Tracing.Span.get_context span |> Tracing.SpanContext.to_traceparent
      in
      Printf.sprintf "%s%c%s" (filter_separator t.log) separator
        (filter_separator traceparent)
    )
    t.tracing

(* Used for xapi-idl servers that do not yet accept tracing data in the debuginfo *)
let to_log_string t = t.log
