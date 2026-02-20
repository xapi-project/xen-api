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

val create_backend :
     ?severity:Opentelemetry_proto.Logs.severity_number
  -> ?formatter:Format.formatter
  -> unit
  -> (module Opentelemetry.Collector.BACKEND)
(** [create_backend ?severity ?formatter ()] is an Opentelemetry backend that prints
  trace spans and logs that have at least [severity] on [formatter], default
  {!val:Fmt.stderr}. Error spans are always printed,
  and successful trace spans are considered {!val:Opentelemetry_proto.Logs.Severity_number_debug} *)

val with_setup :
     ?severity:Opentelemetry_proto.Logs.severity_number
  -> ?formatter:Format.formatter
  -> ?enable:bool
  -> unit
  -> (unit -> 'a)
  -> 'a
