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
  filename:string -> unit -> (module Opentelemetry.Collector.BACKEND)
(** [create_backend ~filename ()] creates a backend that stores output in
    [filename.logs.otel], [filename.trace.otel], [filename.metrics.otel]. *)

val with_setup : filename:string -> ?enable:bool -> unit -> (unit -> 'a) -> 'a
(**[with_setup ~filename ?enable () f] calls {!val:create_backend ~filename],
   and then sets up the created backend as the current backend for the duration
   of the call to [f ()].
*)
