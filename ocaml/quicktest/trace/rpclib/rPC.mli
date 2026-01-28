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

val log_rpc :
     ?time_unix_nano:Opentelemetry.Timestamp_ns.t
  -> Quicktest_trace.Scope.t
  -> string
  -> Rpc.t
  -> Opentelemetry.Logs.t
(** [log_rpc ?time_unix_nano scope key rpc] logs the [rpc] at timestamp
    [time_unix_nano] with the specific [key].

    This is useful for structured logging (the full nested dictionary is
    retained in Opentelemetry format, it isn't converted to a string).
*)

val wrap :
  ?log_body:bool -> (Rpc.call -> Rpc.response) -> Rpc.call -> Rpc.response
(** [wrap ?log_body rpc] wraps an [rpc] call: creates an Opentelemetry span
    following the semantic {!module:Conventions} for RPCs.

    [log_body] can be set to [true] to also logs the body of the request, and on error the body of the reply.
    This is suitable for testing, but not production use (it may log sensitive information)
*)

val http_headers : unit -> (string * string) list
(** [http_headers ()] returns the additional HTTP headers to insert for W3C
  TraceContext propagation.
  @see <https://www.w3.org/TR/trace-context/>
*)
