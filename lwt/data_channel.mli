(*
 * Copyright (c) 2012 Citrix Inc
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

type t = {
  really_read: Cstruct.t -> unit Lwt.t;
  (** [really_read buffer] reads a whole [buffer] of data from the channel *)

  really_write: Cstruct.t -> unit Lwt.t;
  (** [really_write buffer writes a whole [buffer] to the channel *)

  really_write_offset: int64 ref;
  (** the current file write offset *)

  skip: int64 -> unit Lwt.t;
  (** [skip bytes] seeks past the next [bytes] bytes in the output *)

  close: unit -> unit Lwt.t
  (** [close ()] closes the channel *)
}
(** a bidirectional channel which allows reading into and writing from
    Cstruct.t buffers *)

val of_fd: Lwt_unix.file_descr -> seekable:bool -> t Lwt.t
(** [of_fd fd seekable] creates a channel from a Lwt_unix.file_descr.
    If [seekable] then seek() will be called on the fd *)

val of_ssl_fd: Lwt_unix.file_descr -> t Lwt.t
(** [of_ssl_fd fd] creates a channel from an SSL Lwt_unix.file_descr. *)
