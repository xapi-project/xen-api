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
open Sexplib.Std
open Int64

type t = {
  start: int64;
  length: int64;
} [@@deriving sexp]
type ts = t list [@@deriving sexp]

let to_string t = Sexplib.Sexp.to_string_hum (sexp_of_ts t)

type overlap =
  | AABB
  | BBAA
  | BABA
  | BAAB
  | ABBA
  | ABAB
[@@deriving sexp]

let classify { start = a_start; length = a_length } { start = b_start; length = b_length } =
  let a_end = add a_start a_length in
  let b_end = add b_start b_length in
  if b_end < a_start
  then BBAA
  else if a_end < b_start
  then AABB
  else begin
    (* there is some overlap *)
    if b_start < a_start then begin
      if b_end < a_end then BABA else BAAB
    end else begin
      if b_end < a_end then ABBA else ABAB
    end
  end

let difference ({ start = a_start; length = a_length } as a) ({ start = b_start; length = b_length } as b) =
  let a_end = add a_start a_length in
  let b_end = add b_start b_length in
  match classify a b with
  | BBAA | AABB -> [ a ]
  | BABA -> [ { start = b_end; length = sub a_end b_end } ]
  | BAAB -> [ ]
  | ABBA -> [ { start = a_start; length = sub b_start a_start; };
              { start = b_end; length = sub a_end b_end } ]
  | ABAB -> [ { start = a_start; length = sub b_start a_start } ]

let intersect ({ start = a_start; length = a_length } as a) ({ start = b_start; length = b_length } as b) : t list =
  let a_end = add a_start a_length in
  let b_end = add b_start b_length in
  match classify a b with
  | BBAA | AABB -> [ ]
  | BABA -> [ { start = a_start; length = sub b_end a_start } ]
  | BAAB -> [ { start = a_start; length = sub a_end a_start } ]
  | ABBA -> [ { start = b_start; length = sub b_end b_start } ]
  | ABAB -> [ { start = b_start; length = sub a_end b_start } ]
