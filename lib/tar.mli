(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** Tar utilities

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

module Header : sig
  (** Process and create tar file headers *)

  (** tar format assumptions. Default is [V7] (for compatibility with versions of ocaml-tar before this type was introduced). See http://www.gnu.org/software/tar/manual/html_section/tar_68.html for more information. *)
  type compatibility =
    | OldGNU (** GNU tar < 1.12 *)
    | GNU (** GNU tar 1.12 - 1.13.25 *)
    | V7 (** Origin 7th Release format *)
    | Ustar (** POSIX.1-1988 *)
    | Posix (** POSIX.1-2001 *)

  (** Default compatibility if [?level] is omitted. Defaults to [V7] *)
  val compatibility_level : compatibility ref

  module Link : sig
    (** Determines the type of the file *)
    type t =
      | Normal
      | Hard (** a hard link *)
      | Symbolic (** a symbolic link *)
      | Character (** a character device node *)
      | Block (** a block device node *)
      | Directory (** a directory (also indicated by trailing [/] in [file_name]) *)
      | FIFO (** a FIFO node *)
      | GlobalExtendedHeader (** a PaxExtension global header *)
      | PerFileExtendedHeader (** a PaxExtension per-file header *)
      | LongLink (** a @LongLink i.e. a very long filename *)
    val to_string: t -> string
  end

  module Extended: sig
    type t = {
      access_time: int64 option;
      charset: string option;
      comment: string option;
      group_id: int option;
      gname: string option;
      header_charset: string option;
      link_path: string option;
      mod_time: int64 option;
      path: string option;
      file_size: int64 option;
      user_id: int option;
      uname: string option;
    }
    (** Represents a "Pax" extended header *)

    val unmarshal : Cstruct.t -> t
  end

  (** Represents a standard archive (note checksum not stored) *)
  type t = {
    file_name : string;
    file_mode: int;
    user_id: int;
    group_id: int;
    file_size: int64;
    mod_time: int64;
    link_indicator: Link.t;
    link_name: string;
    uname: string;
    gname: string;
    devmajor: int;
    devminor: int;
    extended: Extended.t option;
  }

  (** Helper function to make a simple header *)
  val make : ?file_mode:int -> ?user_id:int -> ?group_id:int -> ?mod_time:int64 -> ?link_indicator:Link.t -> ?link_name:string -> ?uname:string -> ?gname:string -> ?devmajor:int -> ?devminor:int -> string -> int64 -> t

  (** Length of a header block *)
  val length : int

  (** A blank header block (two of these in series mark the end of the tar) *)
  val zero_block : Cstruct.t

  (** Pretty-print the header record *)
  val to_detailed_string : t -> string

  (** For debugging: pretty-print a string as hex *)
  val to_hex : string -> string

  (** Thrown when unmarshalling a header if the checksums don't match *)
  exception Checksum_mismatch

  (** Thrown if we detect the end of the tar (at least two zero blocks in sequence) *)
  exception End_of_stream

  (** Unmarshal a header block, returning None if it's all zeroes.
      This header block may be preceeded by an [?extended] block which
      will override some fields. *)
  val unmarshal : ?level:compatibility -> ?extended:Extended.t -> Cstruct.t -> t option

  (** Marshal a header block, computing and inserting the checksum *)
  val marshal : ?level:compatibility -> Cstruct.t -> t -> unit

  (** Compute the amount of zero-padding required to round up the file size
      to a whole number of blocks *)
  val compute_zero_padding_length : t -> int

  (** Return the required zero-padding as a string *)
  val zero_padding : t -> Cstruct.t

  (** [to_sectors t] is the number of sectors occupied by the data *)
  val to_sectors: t -> int64
end

module type ASYNC = sig
  type 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

module type READER = sig
  type in_channel
  type 'a t
  val really_read: in_channel -> Cstruct.t -> unit t
  val skip: in_channel -> int -> unit t
end

module type WRITER = sig
  type out_channel
  type 'a t
  val really_write: out_channel -> Cstruct.t -> unit t
end

module HeaderReader(Async: ASYNC)(Reader: READER with type 'a t = 'a Async.t) :
 sig
  (** Returns the next header block or throws End_of_stream if two consecutive
      zero-filled blocks are discovered. Assumes stream is positioned at the
      possible start of a header block. End_of_file is thrown if the stream
      unexpectedly fails *)
  val read : ?level:Header.compatibility -> Reader.in_channel -> (Header.t, [`Eof]) Result.result Async.t
end

module HeaderWriter(Async: ASYNC)(Writer: WRITER with type 'a t = 'a Async.t) : sig
  val write : ?level:Header.compatibility -> Header.t -> Writer.out_channel -> unit Async.t
end

module type IO = sig
  type in_channel
  type out_channel

  val really_input : in_channel -> bytes -> int -> int -> unit
  val input : in_channel -> bytes -> int -> int -> int
  val output : out_channel -> bytes -> int -> int -> unit
  val close_out : out_channel -> unit
end

module Make (IO : IO) : sig
  val really_read: IO.in_channel -> Cstruct.t -> unit
  (** [really_read fd buf] fills [buf] with data from [fd] or raises
      End_of_file *)

  val really_write: IO.out_channel -> Cstruct.t -> unit
  (** [really_write fd buf] writes the full contents of [buf] to [fd]
      or raises End_of_file *)

  module Header : sig
    include module type of Header

    (** Returns the next header block or fails with `Eof if two consecutive
        zero-filled blocks are discovered. Assumes stream is positioned at the
        possible start of a header block. End_of_file is thrown if the stream
        unexpectedly fails *)
    val get_next_header : ?level:compatibility -> IO.in_channel -> t
  end

  val write_block: ?level:Header.compatibility -> Header.t -> (IO.out_channel -> unit) -> IO.out_channel -> unit
    [@@ocaml.deprecated "Deprecated: use Tar.HeaderWriter"]
  (** Write [hdr], then call [write_body fd] to write the body,
      then zero-pads so the stream is positioned for the next block. *)

  val write_end: IO.out_channel -> unit
    [@@ocaml.deprecated "Deprecated: use Tar.HeaderWriter"]
  (** Writes a stream terminator to [fd]  *)

  module Archive : sig
    (** Utility functions for operating over whole tar archives *)

    (** Read the next header, apply the function 'f' to the fd and the header. The function
        should leave the fd positioned immediately after the datablock. Finally the function
        skips past the zero padding to the next header *)
    val with_next_file : IO.in_channel -> (IO.in_channel -> Header.t -> 'a) -> 'a

    (** List the contents of a tar *)
    val list : ?level:Header.compatibility -> IO.in_channel -> Header.t list

    (** [extract_gen dest] extract the contents of a tar.
        Apply 'dest' on each header to get a handle to the file to write to *)
    val extract_gen : (Header.t -> IO.out_channel) -> IO.in_channel -> unit

    (** Create a tar on file descriptor fd from the stream of headers.  *)
    val create_gen : ?level:Header.compatibility -> (Header.t * (IO.out_channel -> unit)) Stream.t -> IO.out_channel -> unit

    (** [copy_n ifd odf n] copies exactly [n] bytes from [ifd] to [ofd] *)
    val copy_n : IO.in_channel -> IO.out_channel -> int64 -> unit
      [@@ocaml.deprecated "Deprecated: use your own helper function"]

    (** [skip fd n] reads and throws away [n] bytes from [fd] *)
    val skip : IO.in_channel -> int -> unit
      [@@ocaml.deprecated "Deprecated: use your own helper function"]
  end
end
