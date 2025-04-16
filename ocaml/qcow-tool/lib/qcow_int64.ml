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

module M = struct
  include Int64

  type _t = int64 [@@deriving sexp]

  let sexp_of_t = sexp_of__t

  let t_of_sexp = _t_of_sexp

  let to_int64 x = x

  let of_int64 x = x
end

module IntervalSet = Qcow_diet.Make (M)
module Map = Map.Make (M)
include M

let round_up x size = mul (div (add x (pred size)) size) size

let round_down x size = mul (div x size) size

let sizeof _ = 8

let read buf =
  big_enough_for "Int64.read" buf 8 >>= fun () ->
  return (Cstruct.BE.get_uint64 buf 0, Cstruct.shift buf 8)

let write t buf =
  big_enough_for "Int64.read" buf 8 >>= fun () ->
  Cstruct.BE.set_uint64 buf 0 t ;
  return (Cstruct.shift buf 8)
