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
(** Represents the current "lock" *)
type token 

(** Calls a provided function with the VM locked. The abstract locking token is provided
    so the caller can call 'assert_locked' to make sure they don't perform side-effects outside
    the locked region. *)
val with_lock : API.ref_VM -> (token -> 'a -> 'b) -> 'a -> 'b

(** Raised by a call to 'assert_locked' if the VM is not locked by the provided token *)
exception Lock_not_held

(** Can be called to ensure we are still in the region protected by the lock and haven't accidentally
    dropped outside, eg by an accidental partial function application. *)
val assert_locked : API.ref_VM -> token -> unit


module Per_VM_Qs : sig

  (** Attempt to push the work item to the per-VM queue if it exists. If successful, return true.
      If unsuccessful i.e. the per-VM queue doesn't exist then return false *)
  val maybe_push: API.ref_VM -> string -> (token -> unit) -> bool
end

(** Represents a type of resource a thread has either allocated or is waiting for. *)
type resource = 
	| Lock of string (** e.g. a per-VM lock or a queue *)
	| Process of string * int (** e.g. an stunnel process with the given pid *)

(** Best-effort attempt to kill a resource *)
val kill_resource: resource -> unit

(** Records per-thread diagnostic information *)
module Thread_state : sig

	(** Called when a thread becomes associated with a particular task *)
	val with_named_thread: string -> API.ref_task -> (unit -> 'a) -> 'a

	(** Called when a thread is about to block waiting for a resource to be free *)
	val waiting_for: resource -> unit

	(** Called when a thread acquires a resource *)
	val acquired: resource -> unit

	(** Called when a thread releases a resource *)
	val released: resource -> unit

	val get_all_acquired_resources: unit -> resource list
	val get_acquired_resources_by_task: API.ref_task -> resource list
	val to_graphviz: unit -> string
end

module Named_mutex : sig
	type t
	val create: string -> t
	val execute: t -> (unit -> 'a) -> 'a
end
