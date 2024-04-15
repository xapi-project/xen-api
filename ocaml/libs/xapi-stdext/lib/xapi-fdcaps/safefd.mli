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

(** Safe wrapper around {!type:Unix.file_descr} that detects "use after close" errors

  {!type:Unix.file_descr} is just an integer and cannot track whether {!val:Unix.close} has been called.
  File descriptor numbers are reused by newly open file descriptors, so using a file descriptor that is already closed
  doesn't always result in a visible error, but is nevertheless a programming error that should be detected.

  E.g. the following sequence would write data to the wrong file ([fd2] instead of [fd1]),
  and raise no errors at runtime:
  {[    
    let fd1 = Unix.openfile "fd1" [Unix.O_WRONLY; Unix.O_CREAT] 0o700 in
    Unix.close fd1;
    let fd2 = Unix.openfile "fd2" [Unix.O_WRONLY; Unix.O_CREAT] 0o700 in
    Unix.write_substring fd1 "test" 0 4;
    Unix.close fd2
  ]}

  This module introduces a lightweight wrapper around {!type:Unix.file_descr},
   and detects attempts to use a file descriptor after it has been closed:
  {[
    open Xapi_fdcaps
    
    let fd1 = Unix.openfile "fd1" [Unix.O_WRONLY; Unix.O_CREAT] 0o700 |> Safefd.of_file_descr in
    Safefd.close_exn fd1;
    let fd2 = Unix.openfile "fd2" [Unix.O_WRONLY; Unix.O_CREAT] 0o700 |> Safefd.of_file_descr in
    Safefd.with_fd_exn fd1 (fun fd -> Unix.write_substring fd "test" 0 4);
  ]}

  It raises {!val:Unix.EBADF}:
  {[ Exception: Unix.Unix_error(Unix.EBADF, "unsafe_to_file_descr_exn", "") ]}

  The callback of {!val:with_fd_exn} has access to the underlying {!type:Unix.file_descr},
  and may accidentally call {!val:Unix.close}.

  To detect that {!val:with_fd_exn} calls {!val:Unix.LargeFile.fstat} to check that the file descriptor
  remained the "same" after the call.
  File descriptors are considered to be the same if their kind, device and inode remain unchanged
  (obviously other parts of the stat structure such as timestamps and size may change between calls).
  This doesn't detect all bugs, but detects most common bugs
  (hardlinked files will still show up as the same but the file position may have been different, which is not checked).

  The extra system calls have an overhead so an unsafe version is available, but not documented (it should only be used internally by other modules in {!mod:Xapi_fdcaps}).

  With the safe wrapper we also have a non-integer type that we can attach a finaliser too. 
  This is used to detect and close leaked file descriptors safely (by checking that it is "the same" that we originally opened).
*)

(** a file descriptor that is safe against double close *)
type t

val of_file_descr : Unix.file_descr -> t
(** [of_file_descr fd] wraps [fd].

 *)

val idempotent_close_exn : t -> unit
(** [idempotent_close_exn t] closes [t], and doesn't raise an exception if [t] is already closed.
  Other exceptions may still escape (e.g. if the underlying [close] has reported an [ENOSPC] or [EIO] error).
*)

val close_exn : t -> unit
(** [close_exn t] closes t and raises an exception if [t] is already closed.

  @raises Unix_error(Unix.EBADF,_,_) if [t] is already closed.
*)

val with_fd_exn : t -> (Unix.file_descr -> 'a) -> 'a
(** [with_fd_exn t f] calls [f fd] with the underlying file descriptor.
   [f] must not close [fd].

  @raises Unix_error(Unix.EBADF,_,_) if the file descriptor is not the same after [f] terminates.
*)

val nop : t
(** [nop] is a file descriptor that is always closed and no operations are valid on it. *)

val pp_kind : Format.formatter -> Unix.file_kind -> unit
(** [pp_kind formatter kind] pretty prints [kind] on [formatter]. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter t] pretty prints information about [t] on [formatter]. *)

val dump : Format.formatter -> t -> unit
(** [dump formatter t] prints all the debug information available about [t] on [formatter] *)

(**/**)

(* For small wrappers and high frequency calls like [read] and [write].
   Should only be used by the wrappers in {!mod:Operations}, hence hidden from the documentation.
*)

val setup : unit -> unit
(** [setup ()] sets up a [SIGPIPE] handler.
   With the handler set up a broken pipe will result in a [Unix.EPIPE] exception instead of killing the program *)

val leaked : unit -> int
(** [leaked ()] is a count of leaked file descriptors detected.
  Run [Gc.full_major ()] to get an accurate count before calling this *)

(**/**)

val unsafe_to_file_descr_exn : t -> Unix.file_descr
