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

type t = Opentelemetry_proto.Trace.status

type code = Opentelemetry_proto.Trace.status_status_code =
  | Status_code_unset
  | Status_code_ok
  | Status_code_error

val make :
     message:string
  -> code:Opentelemetry_proto.Trace.status_status_code
  -> Opentelemetry_proto.Trace.status
