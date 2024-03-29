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
type resource =
  | Lock of string  (** e.g. a per-VM lock or a queue *)
  | Process of string * int  (** e.g. an stunnel process with the given pid *)

val kill_resource : resource -> unit
(** Best-effort attempt to kill a resource *)

(** Records per-thread diagnostic information *)
module Thread_state : sig
  val with_named_thread : string -> API.ref_task -> (unit -> 'a) -> 'a
  (** Called when a thread becomes associated with a particular task *)

  val waiting_for : resource -> unit
  (** Called when a thread is about to block waiting for a resource to be free *)

  val acquired : resource -> unit
  (** Called when a thread acquires a resource *)

  val released : resource -> unit
  (** Called when a thread releases a resource *)

  val get_all_acquired_resources : unit -> resource list

  val get_acquired_resources_by_task : API.ref_task -> resource list

  val to_graphviz : unit -> string
end

module Named_mutex : sig
  type t

  val create : string -> t

  val execute : t -> (unit -> 'a) -> 'a
end

module Semaphore : sig
  (** a semaphore that allows at most N operations to proceed at a time *)
  type t

  val create : string -> t
  (** [create name] creates a semaphore with an initial count of 1.
    @see {!set_max} *)

  val execute : t -> (unit -> 'a) -> 'a
  (** [execute sem f] executes [f] after acquiring the [sem]aphore.
    Releases the semaphore on all codepaths. *)

  val set_max : t -> int -> unit
  (** [set_max sem n] sets the semaphore's maximum count to [n].
    Once all threads that are inside {!execute} release the semaphore then the semaphore will accept
    at most [n] {!execute} calls in parallel until it blocks.

    Increasing a semaphore's count is done immediately,
    whereas decreasing the count may block until sufficient number of threads release the semaphore.    

    It is safe to call this function in parallel.
  *)
end
