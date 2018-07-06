(*
 * Copyright (C) 2013 Citrix Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

(** CRC library for strings and cstructs. *)

exception Invalid_length
exception Invalid_offset

(** This is the algorithm used by HDLC, ANSI X3.66, ITU-T V.42,
	Ethernet, Serial ATA, MPEG-2, PKZIP, Gzip, Bzip2, PNG and
	others:
	http://en.wikipedia.org/wiki/Cyclic_redundancy_check#Commonly_used_and_standardized_CRCs
*)
module Crc32 : sig
	val cstruct: ?crc:int32 -> Cstruct.t -> int32
	(** [cstruct ?crc buf] computes the CRC of [buf] with optional
	    initial value [crc] *)

	val string: ?crc:int32 -> string -> int -> int -> int32
	(** [string ?crc buf ofs len] computes the CRC of the substring
	    of length [len] starting at offset [ofs] in string [buf] with
	    optional initial value [crc] *)
end
