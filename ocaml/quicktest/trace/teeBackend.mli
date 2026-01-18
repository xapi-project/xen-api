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

open Opentelemetry.Collector

(** Backend(B1)(B2) sends data to both B1 and B2.
  If B1 fails then B2 is never called.
*)
module Backend : functor (_ : BACKEND) (_ : BACKEND) -> BACKEND

val with_setup :
     (module BACKEND)
  -> (module BACKEND)
  -> ?enable:bool
  -> unit
  -> (unit -> 'a)
  -> 'a
(** [with_setup b1 b2 ?enable () f] creates a new {!module:Backend} that outputs
to both [b1] and [b2] backends, and sets it as the current backend for the
duration of the call to [f ()]*)

val setup_tick : ?interval:float -> unit -> unit
(** [setup_tick ?interval ()] calls [B.tick ()] on the current Backend every
    [interval] seconds. *)

val with_default_setup :
     ?filename:string
  -> ?interval:float
  -> ?enable:bool
  -> unit
  -> (unit -> 'a)
  -> 'a
(** [with_default_setup ?filename ?interval ?enable () f]
  sets up 3 backends: a console backend that logs only info and above, a text dump, and a binary (OTLP) dump.
  Periodic flushing every [interval] is also enabled.

  The backends are only set during the call to [f ()]
*)
