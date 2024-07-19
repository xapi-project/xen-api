(*
 * Copyright (C) 2023 Cloud Software Group
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

open Xapi_fdcaps
open Properties
open Operations

(** {1 Generate test resources} *)

val with_kind_ro :
     Unix.file_kind
  -> (([> rdonly], kind) make -> ([> writable], kind) make option -> 'a)
  -> 'a
(** [with_kind_ro kind f] creates file descriptors of [kind] type, and calls [f] with it.
  For sockets and pipes [f] receives both sides.
  For regular files and block devices it receives a writable file.
  For character devices it receives a {!val:null} device.
*)

val with_kind_wo :
     Unix.file_kind
  -> (([> wronly], kind) make -> ([> readable], kind) make option -> 'a)
  -> 'a
(** [with_kind_wo kind f] is like {!val:with_kind_ro} but creates a write only file.
*)

val with_kind_rw :
  Unix.file_kind -> (([> rdwr], kind) make -> ([> rdwr], kind) make -> 'a) -> 'a
(** [with_kind_rw kind f] is like {!val:with_kind_ro} but creates a read-write file.
*)

(** {1 Observe operations} *)

val observe_read :
     Buffer.t
  -> ((([< readable], _) Properties.t as 'a), bytes) operation
  -> ('a, bytes) operation
(** [observe_read buf op] wraps the operation [op], and stores all substrings read into [buf]. *)

val observe_write :
     Buffer.t
  -> ((([< writable], _) Properties.t as 'a), string) operation
  -> ('a, string) operation
(** [observe_write buf op] wraps the operation [op], and stores all substrings written into [buf]. *)

(** {1 Concurrency helpers} *)

(** a successful result ['a], or an exception with its backtrace on error.

@see {!val:unwrap_exn} to reraise the exception with its original backtrace
 *)
type 'a or_exn = ('a, Rresult.R.exn_trap) result

val unwrap_exn : 'a or_exn -> 'a
(** [unwrap_exn t] returns the underlying successful result, or reraises the exception *)

val concurrently : ('a -> 'b) * ('c -> 'd) -> 'a * 'c -> 'b or_exn * 'd or_exn
(** [concurrently (f, g) (farg, garg)] calls [f farg] and [g garg] in separate threads,
  and returns their results.
*)

(** Sleep that can be interrupted from another thread.

  This uses file descriptors internally, so shouldn't be used as is in XAPI,
  because it'd use up 2 file descriptors every time a [with_] is called.

  `pthread_cond_timedwait` could've been used instead, but that is not available in OCaml,
  and `pthread_cond*` is known to have deadlock bugs on glibc >= 2.27
  https://sourceware.org/bugzilla/show_bug.cgi?id=25847
*)
module CancellableSleep : sig
  (** cancel signal *)
  type t

  val with_ : (t -> 'a) -> 'a
  (** [with f] creates a cancellable sleep value and calls [f] with it. *)

  val sleep : t -> Mtime.span -> unit
  (** [sleep t duration] sleeps until [duration] has elapsed or [t] has been signaled. *)

  val cancel : t -> unit
  (** [cancel t] signals [t] to cancel any sleeps *)
end

(** 1 Introduce delays

These are needed to trigger short reads on sockets.
*)

module Delay : sig
  (** a delay specification *)
  type t

  val v : duration:Mtime.span -> every_bytes:int -> t
  (** [v ~duration ~every_bytes] inserts a sleep for [duration] every [every_bytes] interval.
    The sleep can be canceled with [cancel].

    Note that the time taken to send or receive [after_bytes] is not taken into account to guarantee the insertion of the delay.
  *)

  val every_bytes : t -> int
  (** [every_bytes t] is how often delays are inserted *)

  val apply_read :
       CancellableSleep.t
    -> t
    -> ((([< readable], _) Properties.t as 'a), bytes) operation
    -> ('a, bytes) operation
  (** [apply_read cancel delay op] returns a new operation which calls [op] and every [delay.after_bytes]
    calls sleep for [duration] *)

  val apply_write :
       CancellableSleep.t
    -> t
    -> ((([< writable], _) Properties.t as 'a), string) operation
    -> ('a, string) operation
  (** [apply_write cancel delay op] returns a new operation which calls [op] and every [delay.after_bytes]
    calls sleep for [duration] *)

  val pp : t Fmt.t
  (** [pp formatter t] is a pretty printer for [t] on [formatter]. *)
end

(** {1 Observe file descriptor actions}

  File descriptors are created in pairs, and we can observe the actions from the other end of a pipe or socketpair.
  For regular files we can prepare some data before, or inspect the data at the end.
 *)

(** an observation from the point of view of the observer *)
type 'a observation = {
    elapsed: Mtime.span
        (** the elapsed time for the observer until EOF was encountered *)
  ; data: string  (** the data that was sent or received *)
  ; is_read: [< rdonly | wronly] as 'a
        (** observer's point of view, so observing actions on a readonly pipe will be a write action *)
}

val pp : _ observation Fmt.t
(**[pp formatter obs] pretty prints [obs]ervation on [formatter]. *)

(** read and write observations, and the time elapsed for the function under test *)
type ('a, 'b) observations = {read: 'a; write: 'b; elapsed: Mtime.span}

val observe_ro :
     (([> writable], kind) Properties.t, string) operation
  -> f:(([< readable > `rdonly], kind) make -> 'a)
  -> Unix.file_kind
  -> string
  -> (unit, [> wronly] observation option) observations * 'a or_exn
(** [observe_ro write ~f kind expected] generates a file descriptor of [kind] type,
  and calls [f] with it.
  It observes [f]'s actions from the other side of a pipe, socket, file descriptor,
  or block device if possible.

  @param write the operation used for writing, allows insertion of delays
  @param expected the string to write to the file descriptor
  @returns an observation of [f]'s actions on the file descriptor and [f]'s result
 *)

val observe_wo :
     (([> readable], kind) Properties.t, bytes) operation
  -> f:(([< writable > `wronly], kind) make -> 'a)
  -> size:int
  -> Unix.file_kind
  -> ([> rdonly] observation option, unit) observations * 'a or_exn
(** [observe_wo read ~f ~size kind] generates a file descriptor of [kind] type,
  and calls [f] with it.
  It observes [f]'s actions from the other side of a pipe, socket, file descriptor,
  or block device if possible.
  It expects [size] bytes written by [f].

  @returns an observation of [f]'s actions on the file descriptor and [f]'s result
 *)

val observe_rw :
     (([> readable], kind) Properties.t, bytes) operation
  -> (([> writable], kind) Properties.t, string) operation
  -> f:((rdwr, kind) make -> 'a)
  -> size:int
  -> Unix.file_kind
  -> string
  -> ([> rdonly] observation option, [> wronly] observation option) observations
     * 'a or_exn
(** [observe_rw read write ~f ~size kind expected] generates a file descriptor of [kind] type,
  and calls [f] with it.
  It observes [f]'s actions from the other side of a pipe, socket, file descriptor,
  or block device if possible.

  @param read the operation used for reading, allows insertion of delays
  @param write the operation used for writing, allows insertion of delays
  @param expected the string to write to the file descriptor
  @returns an observation of [f]'s actions on the file descriptor and [f]'s result
 *)
