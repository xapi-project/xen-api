(*
 * Copyright (C) 2015 David Scott <dave@recoil.org>
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
open Sexplib.Std
open Qcow_error

let big_enough_for name buf needed =
  let length = Cstruct.length buf in
  if length < needed then
    error_msg "%s: buffer too small (%d < %d)" name length needed
  else
    return ()

module Int8 = struct
  type t = int [@@deriving sexp]

  let sizeof _ = 1

  let read buf =
    big_enough_for "Int8.read" buf 1 >>= fun () ->
    return (Cstruct.get_uint8 buf 0, Cstruct.shift buf 1)

  let write t buf =
    big_enough_for "Int8.write" buf 1 >>= fun () ->
    Cstruct.set_uint8 buf 0 t ;
    return (Cstruct.shift buf 1)
end

module Int16 = struct
  type t = int [@@deriving sexp]

  let sizeof _ = 2

  let read buf =
    big_enough_for "Int16.read" buf 2 >>= fun () ->
    return (Cstruct.BE.get_uint16 buf 0, Cstruct.shift buf 2)

  let write t buf =
    big_enough_for "Int16.write" buf 2 >>= fun () ->
    Cstruct.BE.set_uint16 buf 0 t ;
    return (Cstruct.shift buf 2)
end

module Int32 = struct
  include Int32

  type _t = int32 [@@deriving sexp]

  let sexp_of_t = sexp_of__t

  let t_of_sexp = _t_of_sexp

  let sizeof _ = 4

  let read buf =
    big_enough_for "Int32.read" buf 4 >>= fun () ->
    return (Cstruct.BE.get_uint32 buf 0, Cstruct.shift buf 4)

  let write t buf =
    big_enough_for "Int32.read" buf 4 >>= fun () ->
    Cstruct.BE.set_uint32 buf 0 t ;
    return (Cstruct.shift buf 4)
end

module Int64 = Qcow_int64
module Int = Qcow_int

(*
module Cluster = struct
  include Qcow_word_size.Cluster
end
*)
module Cluster = Qcow_int64
