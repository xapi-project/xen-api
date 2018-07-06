(*
 * Copyright (C) 2016-present David Scott <dave.scott@docker.com>
 * Copyright (c) 2011-present Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-present Thomas Gazagnaire <thomas@gazagnaire.org>
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

type write_error = [ `Closed ]

let pp_write_error ppf = function
  | `Closed -> Fmt.pf ppf "attempted to write to a closed flow"

type 'a or_eof = [`Data of 'a | `Eof ]

let pp_or_eof d ppf = function
  | `Data a -> d ppf a
  | `Eof    -> Fmt.string ppf "End-of-file"

module type ABSTRACT = sig
  type +'a io
  type error
  val pp_error: error Fmt.t
  type write_error
  val pp_write_error: write_error Fmt.t
  type buffer
  type flow
  val read: flow -> (buffer or_eof, error) result io
  val write: flow -> buffer -> (unit, write_error) result io
  val writev: flow -> buffer list -> (unit, write_error) result io
  val close: flow -> unit io
end

module type S = ABSTRACT with type write_error = private [> write_error]
  [@@ocaml.warning "-34"]

module type CONCRETE = ABSTRACT
  with type error = [`Msg of string]
   and type write_error = [write_error | `Msg of string]

module Concrete (S: S) (IO: sig
       type 'a t = 'a S.io
       val map: ('a -> 'b) -> 'a t -> 'b t
  end) =
struct
  type 'a io = 'a S.io
  type error = [`Msg of string]
  type write_error = [ `Closed | `Msg of string]
  type buffer = S.buffer
  type flow = S.flow

  let pp_error ppf = function
    | `Msg s -> Fmt.string ppf s

  let pp_write_error ppf = function
    | #error as e -> pp_error ppf e
    | `Closed     -> pp_write_error ppf `Closed

  let lift_read = function
    | Ok x    -> Ok x
    | Error e -> Error (`Msg (Fmt.strf "%a" S.pp_error e))

  let lift_write = function
    | Ok ()         -> Ok ()
    | Error `Closed -> Error `Closed
    | Error e       -> Error (`Msg (Fmt.strf "%a" S.pp_write_error e))

  let read t = IO.map lift_read (S.read t)
  let write t b = IO.map lift_write (S.write t b)
  let writev t bs = IO.map lift_write (S.writev t bs)
  let close t = S.close t
end

module type SHUTDOWNABLE = sig
  include S
  val shutdown_write: flow -> unit io
  val shutdown_read: flow -> unit io
end

type stats = {
  read_bytes: int64;
  read_ops: int64;
  write_bytes: int64;
  write_ops: int64;
  duration: int64;
}

let kib = 1024L
let ( ** ) = Int64.mul
let mib = kib ** 1024L
let gib = mib ** 1024L
let tib = gib ** 1024L

let suffix = [
  kib, "KiB";
  mib, "MiB";
  gib, "GiB";
  tib, "TiB";
]

let add_suffix x =
  List.fold_left (fun acc (y, label) ->
      if Int64.div x y > 0L
      then Printf.sprintf "%.1f %s" Int64.((to_float x) /. (to_float y)) label
      else acc
    ) (Printf.sprintf "%Ld bytes" x) suffix

let pp_stats ppf s =
  Fmt.pf ppf "%s bytes at %s/nanosec and %Lu IOPS/nanosec"
    (add_suffix s.read_bytes)
    (add_suffix Int64.(div s.read_bytes s.duration))
    (Int64.div s.read_ops s.duration)
