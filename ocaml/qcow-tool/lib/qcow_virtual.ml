(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
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

(* An address in a qcow image is broken into 3 levels: *)
type t = {
    l1_index: int64 (* index in the L1 table *)
  ; l2_index: int64 (* index in the L2 table *)
  ; cluster: int64 (* index within the cluster *)
}
[@@deriving sexp]

let ( <| ) = Int64.shift_left

let ( |> ) = Int64.shift_right_logical

let make ~cluster_bits x =
  let l2_bits = cluster_bits - 3 in
  let l1_index = x |> l2_bits + cluster_bits in
  let l2_index = x <| 64 - l2_bits - cluster_bits |> 64 - l2_bits in
  let cluster = x <| 64 - cluster_bits |> 64 - cluster_bits in
  {l1_index; l2_index; cluster}

let to_offset ~cluster_bits t =
  let l2_bits = cluster_bits - 3 in
  let l1_index = t.l1_index <| l2_bits + cluster_bits in
  let l2_index = t.l2_index <| cluster_bits in
  Int64.(logor (logor l1_index l2_index) t.cluster)

let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)
