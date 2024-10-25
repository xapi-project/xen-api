(* Copyright (C) Cloud Software Group Inc.
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

val sizeof : int

(** [t] is the metadata of a chunk of disk that's meant to be streamed. These
    are used in a protocol that interleaves the metadata and the data until an
    empty metadata block is sent, which signals the end of the stream. *)
type t

val end_of_stream : t
(** [end_of_stream] is the value that signals the end of the stream of chunks
    being transferred. *)

val make : sector:int64 -> ?size:int64 -> Cstruct.t -> t
(** [make ~sector ?size data] creates a chunk of disk that needs to be
    transferred, starting at the sector [sector]. [size] is the sector size, in
    bytes. The default is 512. *)

val marshal : Cstruct.t -> t -> unit
(** [marshall buffer chunk] writes the metadata of [chunk] to [buffer]. When
    transferring a whole disk, this is called a header and is written before
    the data. *)

val is_last_chunk : Cstruct.t -> bool
(** [is_last_chunk buffer] returns whether the current [buffer] is
    {end_of_stream} *)

val get_offset : Cstruct.t -> int64

val get_len : Cstruct.t -> int32
