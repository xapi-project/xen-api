(*
 * Copyright (C) 2013 Citrix Systems Inc
 * Copyright (C) 2016 Docker Inc
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
 *)

open Result

(** Block device on top of Lwt_unix *)

include Mirage_block_lwt.S

(** {0} low-level convenience functions *)

val really_read: Lwt_unix.file_descr -> Cstruct.t -> unit Lwt.t

val really_write: Lwt_unix.file_descr -> Cstruct.t -> unit Lwt.t

val blkgetsize: string -> Unix.file_descr -> (int64, error) result
(** [blkgetsize path fd]: returns the size of the open block device
    given by [fd]. [path] is only used to construct a human-readable error
    message. *)

val ftruncate: Lwt_unix.file_descr -> int64 -> unit Lwt.t
(** [ftruncate fd size]: changes the size of the file backed by [fd]
    to [size]. This function works on Unix and Windows. *)

module Config: sig
  type sync_behaviour = [
    | `ToOS (** flush to the operating system, not necessarily the drive *)
    | `ToDrive (** flush to the drive *)
  ]

  val string_of_sync: sync_behaviour option -> string

  type t = {
    buffered: bool; (** true if I/O hits the OS disk caches, false if "direct" *)
    sync: sync_behaviour option;
    path: string; (** path to the underlying file *)
    lock: bool; (** true if the file should be locked preventing concurrent modification *)
  }
  (** Configuration of a device *)

  val create: ?buffered:bool -> ?sync:(sync_behaviour option) -> ?lock:bool -> string -> t
  (** [create ?buffered ?sync ?lock path] constructs a configuration referencing the
      file stored at [path]. *)

  val to_string: t -> string
  (** Marshal a config into a string of the form
      file://<path>?sync=(0|1)&buffered=(0|1) *)

  val of_string: string -> (t, [`Msg of string ]) result
  (** Parse the result of a previous [to_string] invocation *)
end

val connect : ?buffered:bool -> ?sync:(Config.sync_behaviour option) -> ?lock:bool -> string -> t io
(** [connect ?buffered ?sync ?lock path] connects to a block device on the filesystem
    at [path]. By default I/O is buffered and asynchronous. By default the file
    is unlocked. These defaults
    can be changed by supplying the optional arguments [~buffered:false] and
    [~sync:false] [~lock:true] *)

val resize : t -> int64 -> (unit, write_error) result io
(** [resize t new_size_sectors] attempts to resize the connected device
    to have the given number of sectors. If successful, subsequent calls
    to [get_info] will reflect the new size. *)

val flush : t -> (unit, write_error) result io
(** [flush t] flushes any buffers, if the file has been opened in buffered
    mode *)

val seek_unmapped: t -> int64 -> (int64, error) result io
(** [seek_unmapped t start] returns the sector offset of the next guaranteed
    zero-filled region (typically guaranteed because it is unmapped) *)

val seek_mapped: t -> int64 -> (int64, error) result io
(** [seek_mapped t start] returns the sector offset of the next regoin of the
    device which may have data in it (typically this is the next mapped
    region) *)

val discard: t -> int64 -> int64 -> (unit, write_error) result io
(** [discard sector n] signals that the [n] sectors starting at [sector]
    are no longer needed and the contents may be discarded. Note the contents
    may not actually be deleted: this is not a "secure erase". *)

val to_config: t -> Config.t
(** [to_config t] returns the configuration of a device *)

val of_config: Config.t -> t io
(** [of_config config] creates a fresh device from [config] *)
