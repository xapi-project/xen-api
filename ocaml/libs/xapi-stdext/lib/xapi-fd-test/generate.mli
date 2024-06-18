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
open Observations

(** file descriptor behaviour specification *)
type t = {
    size: int
  ; delay_read: Delay.t option
  ; delay_write: Delay.t option
  ; kind: Unix.file_kind
}

val timeouts : Mtime.Span.t QCheck2.Gen.t
(** [timeouts] is a generator for small timeouts *)

val make :
     size:int
  -> delay_read:Delay.t option
  -> delay_write:Delay.t option
  -> Unix.file_kind
  -> t
(** [make ~size ~delay_read ~delay_write kind] is a file descriptor test.

 @param size the size of the file, or the amount of data sent on a socket/pipe
 @param delay_read whether to insert sleeps to trigger short reads
 @param delay_write whether to insert sleeps to trigger short writes
 @param kind the {!type:Unix.file_kind} of the file descriptor to create
*)

val t : t QCheck2.Gen.t
(** [t] is a {!mod:QCheck2} generator for {!type:t}.

 This doesn't yet open any file descriptors (there'd be too many leaks and we'd run out),
 that is done by {!val:run}

 Follows the naming convention to name generators after the type they generate. 
*)

val print : t QCheck2.Print.t
(** [print] is a QCheck2 pretty printer for [t] *)

val run_ro :
     t
  -> string
  -> f:(([< readable > `rdonly], kind) make -> 'a)
  -> (unit, [> wronly] observation option) observations * 'a or_exn
(** [run_ro t data ~f] creates a file descriptor according to [t] and calls the function under test [f].
 The file descriptor should be treated as readonly.

 @returns observations about [f]'s actions the file descriptor
*)

val run_wo :
     t
  -> f:(([< writable > `wronly], kind) make -> 'a)
  -> ([> rdonly] observation option, unit) observations * 'a or_exn
(** [run_wo t ~f] creates a file descriptor according to [t] and calls the function under test [f].
 The file descriptor should be treated as writeonly.

 @returns observations about [f]'s actions on the file descriptor
*)

val run_rw :
     t
  -> string
  -> f:((rdwr, kind) make -> 'a)
  -> ([> rdonly] observation option, [> wronly] observation option) observations
     * 'a or_exn
(** [run_rw t data ~f] creates a file descriptor according to [t] and calls the function under test [f].
 The file descriptor should be treated as read-write.

 @returns observations about [f]'s actions the file descriptor
*)
