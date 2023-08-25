(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** Represents a type of resource a thread has either allocated or is waiting for. *)
type resource

val lock : string -> resource
(** [lock name] a per-VM lock or a queue *)

val process : string * int -> resource
(** [process (name, pid)] e.g a an stunnel process with the given pid *)

val is_process : string -> resource -> bool
(** [is_process name resource] checks whether [resource] is a process named [name]. *)

val kill_resource : resource -> unit
(** Best-effort attempt to kill a resource *)

val string_of_resource : resource -> string
(** [string_of_resource resource] a string representation of the resource for debugging *)

(** Records per-thread diagnostic information *)
module Thread_state : sig
  type waiting

  type acquired

  val with_named_thread : string -> API.ref_task -> (unit -> 'a) -> 'a
  (** Called when a thread becomes associated with a particular task *)

  val waiting_for : ?parent:Tracing.Span.t -> resource -> waiting
  (** Called when a thread is about to block waiting for a resource to be free *)

  val acquired : resource -> waiting -> acquired
  (** Called when a thread acquires a resource *)

  val released : resource -> acquired -> unit
  (** Called when a thread releases a resource *)

  val get_all_acquired_resources : unit -> resource list

  val get_acquired_resources_by_task : API.ref_task -> resource list

  val to_graphviz : unit -> string

  val known_threads : unit -> int
end

module Named_mutex : sig
  type t

  val create : string -> t

  val execute :
    ?__context:Context.t -> ?parent:Tracing.Span.t -> t -> (unit -> 'a) -> 'a
end
