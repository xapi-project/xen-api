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

(** @see <https://opentelemetry.io/docs/specs/otel/trace/sdk/#span-processor>.

  Only an approximation of the interface.

  Currently implements <https://opentelemetry.io/docs/concepts/sampling/#tail-sampling>
  that samples all spans containing an error only.
*)

val on_start : Scope.t -> unit
(** [on_start scope] gets called synchronously when a span (scope) is created.
   Can change [scope].
*)

val on_end : Scope.t -> unit
(** [on_end scope] gets called synchronously when a span (scope) finishes.
   Should not change the [scope], except for calling [Scope.set_decision].
*)

val force_flush : unit -> unit
(** [force_flush ()] tells the backend to flush.
     This is not directly supported, it currently calls [Backend.tick ()].
*)

val shutdown : unit -> unit
(** [shutdown ()] shuts down the backend *)
