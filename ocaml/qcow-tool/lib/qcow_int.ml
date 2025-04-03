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

module M = struct
  type t = int [@@deriving sexp]

  let zero = 0

  let succ x = x + 1

  let pred x = x - 1

  let add x y = x + y

  let sub x y = x - y

  let compare (x : t) (y : t) = Stdlib.compare x y

  let mul x y = x * y

  let div x y = x / y

  let to_int64 = Int64.of_int

  let of_int64 = Int64.to_int

  let to_int x = x

  let of_int x = x

  let to_string = string_of_int

  let shift_left x n = x lsl n

  let shift_right_logical x n = x lsr n

  let logor x y = x lor y

  let rem x y = x mod y
end

module IntervalSet = Qcow_diet.Make (M)
module Map = Map.Make (M)
include M

let round_up x size = mul (div (add x (pred size)) size) size
