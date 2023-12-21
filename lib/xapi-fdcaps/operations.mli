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

(** Statically enforce file descriptor capabilities using type parameters.

 *)

open Properties

(** {1 Type and pretty printers } *)

(** a file descriptor with properties
  Upper bounds are avoided here so that this type can be used in functors
 *)
type +!'a props constraint 'a = (_, _) Properties.props

(** like {!type:props} but with upper bounds on properties *)
type +!'a t = 'a props constraint 'a = (_, _) Properties.t

(** convenience type for declaring properties *)
type (+!'a, +!'b) make = ('a, 'b) Properties.t t

val pp : _ t Fmt.t
(** [pp formatter t] pretty prints [t] on [formatter]. *)

val dump : _ t Fmt.t
(** [dump formatter t] prints a debug representation of [t] on [formatter]. *)

(** {1 Initialization} *)

val setup : unit -> unit
(** [setup ()] installs a SIGPIPE handler.

  By default a SIGPIPE would kill the program, this makes it return [EPIPE] instead.
 *)

(** {1 With resource wrappers} *)

val with_fd : 'a t -> ('a t -> 'b) -> 'b
(** [with_fd t f] calls [f t] and always closes [t] after [f] finishes.
  [f] can also close [t] earlier if it wants to without a double close error.
*)

val with_fd2 : 'a t * 'b t -> ('a t * 'b t -> 'c) -> 'c
(** [with_fd2 fd1 fd2 f] calls [f fd1 fd2] and always closes [t] after [f] finishes. *)

module Syntax : sig
  val ( let@ ) : ('a -> 'b) -> 'a -> 'b
  (** [let@ fd = with_fd t in ... use fd] *)
end

(** {1 {!mod:Unix} wrappers} *)

val stdin : ([> rdonly], kind) make
(** [stdin] is a readonly file descriptor of unknown kind *)

val stdout : ([> wronly], kind) make
(** [stdout] is a writeonly file descriptor of unknown kind *)

val stderr : ([> wronly], kind) make
(** [stderr] is a writeonly file descriptor of unknown kind *)

val close : _ t -> unit
(** [close t] closes t. Doesn't raise an exception if it is already closed.
  Other errors from the underlying {!val:Unix.close} are propagated.
 *)

val fsync : _ t -> unit
(** [fsync t] flushes [t] buffer to disk.

  Note that the file doesn't necessarily have to be writable, e.g. you can fsync a readonly open directory.
 *)

val pipe : unit -> ([> rdonly], [> fifo]) make * ([> wronly], [> fifo]) make
(** [pipe ()] creates an unnamed pipe.
  @see {!val:Unix.pipe}
 *)

val socketpair :
     Unix.socket_domain
  -> Unix.socket_type
  -> int
  -> ([> rdwr], [> sock]) make * ([> rdwr], [> sock]) make
(** [socketpair domain type protocol] creates a socket pair.
  @see {!val:Unix.socketpair}
 *)

val openfile_ro : 'a -> string -> Unix.open_flag list -> ([> rdonly], 'a) make
(** [openfile_ro kind path flags] opens an existing [path] readonly.

  @param kind [path] is expected to be this file kind
  @see {!val:Unix.openfile}

  @raises Invalid_argument if [path] is not kind
 *)

val openfile_rw :
     ?custom_ftruncate:(int64 -> unit)
  -> 'a
  -> string
  -> Unix.open_flag list
  -> ([> rdwr], 'a) make
(** [openfile_rw kind path flags] opens an existing [path] readwrite.

  @param kind [path] is expected to be this file kind
  @see {!val:Unix.openfile}

  @raises Invalid_argument if [path] is not kind
 *)

val openfile_wo : 'a -> string -> Unix.open_flag list -> ([> wronly], 'a) make
(** [openfile_wo kind path flags] opens an existing [path] writeonly.

  @param kind [path] is expected to be this file kind
  @see {!val:Unix.openfile}

  @raises Invalid_argument if [path] is not kind
 *)

val creat : string -> Unix.open_flag list -> int -> ([> rdwr], [> reg]) make
(** [creat path flags perms] creates [path] readwrite. The path must not already exist.

  @param perms initial permissions for [path]
  @see {!val:Unix.openfile}

  @raises Invalid_argument if [path] is not kind
 *)

val dev_null_out : unit -> ([> wronly], [> chr]) make
(** [dev_null_out ()] is "/dev/null" opened for writing *)

val dev_null_in : unit -> ([> rdonly], [> chr]) make
(** [dev_null_in ()] is "/dev/null" opened for reading *)

val dev_zero : unit -> ([> rdonly], [> chr]) make
(** [dev_zero ()] is "/dev/zero" opened for reading *)

val shutdown_recv : ([< readable], [< sock]) make -> unit
(** [shutdown_recv t] shuts down receiving on [t].

  @see {!Unix.shutdown}
 *)

val shutdown_send : ([< writable], [< sock]) make -> unit
(** [shutdown_send t] shuts down sending on [t].

  @see {!Unix.shutdown}
 *)

val shutdown_all : ([< rdwr], [< sock]) make -> unit
(** [shutdown_all t] shuts down both receiving and sending on [t].

  @see {!Unix.shutdown}
 *)

val ftruncate : ([< writable], [< truncatable]) make -> int64 -> unit
(** [ftruncate t size] sets the size of the regular file [t] to [size].

  @see {!Unix.ftruncate}
 *)

val lseek : (_, [< seekable]) make -> int64 -> Unix.seek_command -> int64
(** [lseek t off whence] sets the position of [t] to [off] with origin specified by [whence].

  @see {!Unix.lseek}
*)

val read : ([< readable], _) make -> bytes -> int -> int -> int
(** [read t buf off len]
  @see {!Unix.read}
 *)

val single_write_substring :
  ([< writable], _) make -> string -> int -> int -> int
(** [single_write_substring t buf off len]

  @see {!Unix.single_write_substring}
*)

val fstat : _ t -> Unix.LargeFile.stats
(** [fstat t] is {!val:Unix.LargeFile.fstat}  *)

val dup : 'a t -> 'a t
(** [dup t] is {!val:Unix.dup} on [t]. *)

val set_nonblock : (_, [< espipe]) make -> unit
(** [set_nonblock t].

  Only pipes, FIFOs and sockets are guaranteed to not block when this flag is set.
  Although it is possible to set regular files and block devices as non-blocking, they currently still block
  (although according to the manpage this may change in the future)

  @see {!Unix.set_nonblock}
 *)

val clear_nonblock : _ t -> unit
(** [clear_nonblock t].

  We do not restrict clearing the non-blocking flag: that is just reverting back to default behaviour.
  
  @see {!Unix.clear_nonblock}
 *)

val setsockopt_float :
  (_, [< sock]) make -> Unix.socket_float_option -> float -> unit
(** [set_sockopt_float t opt val] sets the socket option [opt] to [val] for [t]. *)

(** {1 Temporary files} *)

val with_tempfile :
  ?size:int64 -> unit -> (string * ([> wronly], [> reg]) make -> 'a) -> 'a
(** [with_tempfile () f] calls [f (name, outfd)] with the name of a temporary file and a file descriptor opened for writing.
  Deletes the temporary file when [f] finishes. *)

val with_temp_blk :
  ?sector_size:int -> string -> (string * ([> rdwr], [> blk]) make -> 'a) -> 'a
(** [with_temp_blk ?sector_size path f] calls [f (name, fd)] with a name and file descriptor pointing to a block device.
  The block device is temporarily created on top of [path].

  Deletes the block device when [f] finishes.
  Only works when run as root.

  @param sector_size between 512 and 4096
*)

(** {1 Operation wrappers}

  The low-level {!val:read} and {!val:single_write_substring} can raise different exceptions
  to mean end-of-file/disconnected depending on the file's kind.

  If you want to consider disconnectins as end-of-file then use these wrappers.
 *)

(** a buffered operation on a file descriptors.

  @see {!val:read} and {!val:single_write_substring}
 *)
type ('a, 'b) operation = 'a t -> 'b -> int -> int -> int

val repeat_read : ('a, bytes) operation -> ('a, bytes) operation
(** [repeat_read op buf off len] repeats [op] on the supplied buffer until EOF or a connection error is encountered.
  The following connection errors are treated as EOF and are not reraised:
  {!val:Unix.ECONNRESET}, {!val:Unix.ENOTCONN}.
  {!val:Unix.EAGAIN} and {!val:Unix.EWOULDBLOCK} also cause the iteration to stop.

  The returned value may be less than [len] if EOF was encountered.
*)

val repeat_write : ('a, string) operation -> ('a, string) operation
(** [repeat_write op buf off len] repeats [op] on the supplied buffer until a connection error is encountered or the entire buffer is written.
  The following are treated as connection errors and not reraised:
  {!val:Unix.ECONNRESET}, {!val:Unix.EPIPE}, {!val:Unix.ENETDOWN}, {!val:Unix.ENETUNREACH}
  {!val:Unix.EAGAIN} and {!val:Unix.EWOULDBLOCK} also cause the iteration to stop.

  The returned value may be less than [len] if we were not able to complete the write due to a connection error.
*)

(**/**)

module For_test : sig
  val unsafe_fd_exn : _ t -> Unix.file_descr
end
