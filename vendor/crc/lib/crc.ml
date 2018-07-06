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

exception Invalid_length
exception Invalid_offset

let check_bounds buffer_length offset data_length =
	if offset > buffer_length then raise Invalid_offset;
	if data_length < 0 then raise Invalid_length;
	if offset + data_length > buffer_length then raise Invalid_length;
	if offset < 0 then raise Invalid_offset

(* This will be done by the Cstruct library itself after 1.0.1 *)
let check_bounds_cstruct t =
	check_bounds (Bigarray.Array1.dim t.Cstruct.buffer) t.Cstruct.off t.Cstruct.len

let check_bounds_string str offset length =
	check_bounds (String.length str) offset length

external unsafe_crc32_cstruct : int32 -> Cstruct.buffer -> int -> int -> int32 =
	"crc32_cstruct"

external unsafe_crc32_string : int32 -> string -> int -> int -> int32 =
	"crc32_string"

module Crc32 = struct
	let cstruct ?(crc=0l) t =
		check_bounds_cstruct t;
		unsafe_crc32_cstruct crc t.Cstruct.buffer t.Cstruct.off t.Cstruct.len

	let string ?(crc=0l) str offset length =
		check_bounds_string str offset length;
		unsafe_crc32_string crc str offset length
end
