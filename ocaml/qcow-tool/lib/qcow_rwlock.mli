(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** A lock which permits multiple concurrent threads to acquire it for reading
    but demands exclusivity for writing *)
type t [@@deriving sexp_of]

type ts = t list [@@deriving sexp_of]

val make : (unit -> string) -> t
(** [make describe_fn] creates a new lock, where [describe_fn ()] returns a
    human-readable description string suitable for debug output. *)

(** A value which represents holding a lock *)
type lock

val unlock : lock -> unit
(** [unlock locked] releases the lock associated with [locked] *)

module Client : sig
  (** An entity which holds a set of locks *)
  type t

  val make : (unit -> string) -> t
  (** [make describe_fn] creates an entity where [describe_fn ()] returns
      a human-readable description of the client for use in debugging. *)
end

module Read : sig
  val with_lock : ?client:Client.t -> t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_lock ?client t f] executes [f ()] when no other client has held
      the lock exclusively for writing. Note this means that I may hold the lock
      for writing and then re-lock it for reading.
  *)

  val lock : ?client:Client.t -> t -> lock Lwt.t
  (** [lock ?client t] locks [t]. This function blocks while another client
      holds the lock for writing. The lock must be released with [unlock] *)
end

module Write : sig
  val with_lock : ?client:Client.t -> t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_lock ?client t f] executes [f ()] when no-other client is holding
      the lock for reading or writing. Note this means that I may hold the lock
      for reading and then re-lock it for writing. *)

  val try_lock : ?client:Client.t -> t -> lock option
  (** [try_lock ?client t] acquires a write lock on [t] if immediately possible,
      or returns None *)
end

module Debug : sig
  val assert_no_locks_held : Client.t -> unit
  (** Check that all locks have been explicitly released. *)
end
