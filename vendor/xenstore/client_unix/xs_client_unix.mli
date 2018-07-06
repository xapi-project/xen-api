(*
 * Copyright (C) Citrix Systems Inc.
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

(** A multiplexing XenStore protocol client over a byte-level transport, using Unix. *)

module type IO = sig
  type 'a t = 'a
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t

  type channel
  val create: unit -> channel t
  val destroy: channel -> unit t
  val read: channel -> bytes -> int -> int -> int t
  val write: channel -> bytes -> int -> int -> unit t
end

exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

exception Cancelled

module Task : sig
  type 'a u

  val make: unit -> 'a u
  val wakeup: 'a u -> 'a -> unit
  val on_cancel: 'a u -> (unit -> unit) -> unit
  val cancel: 'a u -> unit
  val wait: 'a u -> 'a
end


type watch_callback = string * string -> unit
(** Clients can opt to manage watches manually via this optional
    callback. *)

module Client : functor(IO: IO) -> sig
  type client
  (** A multiplexing xenstore client. *)

  val set_logger : (string -> unit) -> unit
  (** Set a callback in order to get logging from the library *)

  val make : unit -> client IO.t
  (** [make ()] initialises and returns a xenstore client. *)

  val set_watch_callback : client -> watch_callback -> unit
  (** [set_watch_callback cb] registers a manual watch callback. *)

  type handle
  (** A handle represents a single thread's xenstore access. *)

  val immediate : client -> (handle -> 'a IO.t) -> 'a IO.t
  (** Access xenstore with individual operations. *)

  val transaction_one_try : client -> (handle -> 'a IO.t) -> 'a IO.t
  (** Access xenstore with a single transaction.  On conflict the
      Eagain error will not be handled but will be passed up to the
      caller. *)

  val transaction_attempts : int -> client -> (handle -> 'a IO.t) -> 'a IO.t
  (** Access xenstore with a single transaction.  On conflict the
      operation may be attempted again, up to a total of (max attempts 1)
      attempts. If the last of those fails with a conflict, the Eagain
      exception will be raised to the caller. *)

  val transaction : client -> (handle -> 'a IO.t) -> 'a IO.t
  (** DEPRECATED!
      Access xenstore with a single transaction.  On conflict the
      operation will be repeated INDEFINITELY, with no guarantee
      of eventual success or termination. *)

  val wait : client -> (handle -> 'a IO.t) -> 'a Task.u
  (** Wait for some condition to become true and return a value.  The
      function argument should throw Eagain if the condition is not
      met, and the condition will be re-evaluated when paths
      change. *)

  val directory : handle -> string -> string list IO.t
  (** [directory h path] returns the directory listing of [path]. *)

  val read : handle -> string -> string IO.t
  (** [read h path] returns the value at [path] or raises Enoent. *)

  val write : handle -> string -> string -> unit IO.t
  (** [write h k v] writes [v] at [k]. *)

  val rm : handle -> string -> unit IO.t
  (** [rm h k] removes the node [k]. *)

  val mkdir : handle -> string -> unit IO.t
  (** [mkdir h k] creates the node [k] with an empty value. *)

  val setperms : handle -> string -> Xs_protocol.ACL.t -> unit IO.t
  (** [setperms h k acl] sets the permissions of [k] to [acl]. *)

  val debug : handle -> string list -> string list IO.t
  (** [debug cmd_args] invokes a debug command. *)

  val restrict : handle -> int -> unit IO.t
  (** [restrict h domid] restricts the current connection to have only
	  the priviledges associated with domain [domid]. *)

  val getdomainpath : handle -> int -> string IO.t
  (** [getdomainpath domid] returns the local directory of domain
      [domid]. *)

  val watch : handle -> string -> string -> unit IO.t
  (** [watch h path token] registers a manual watch at [path] with
      [token]. *)

  val unwatch : handle -> string -> string -> unit IO.t
  (** [unwatch h path token] unregisters a manual watch at [path] with
      [token]. *)

  val introduce : handle -> int -> nativeint -> int -> unit IO.t
  (** [introduce h domid store_mfn store_port] called by a toolstack
      to signal the construction of a new domain. *)

  val set_target : handle -> int -> int -> unit IO.t
(** [set_target h stubdom_domid domid] called by a toolstack to
    grant [stubdom_domid] the permissions owned by [domid]. *)
end
