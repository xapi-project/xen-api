(*
 * Copyright (c) Citrix Systems Inc.
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

(** Non-blocking logging functions. Note you must start the
    [logging_thread] if you want the output to go somewhere.
    Note the log messages are written to a fixed-length buffer
    which can overflow. If it overflows then log messages are
    dropped and this fact is guaranteed to be logged. *)

val debug: ('a, unit, string, unit) format4 -> 'a
val info:  ('a, unit, string, unit) format4 -> 'a
val warn:  ('a, unit, string, unit) format4 -> 'a
val error: ('a, unit, string, unit) format4 -> 'a

type traced_operation = [
  | `Set of string * string * [ `Producer | `Consumer | `Suspend |
    `Suspend_ack ] * [ `Int64 of int64 | `Bool of bool ]
  | `Get of string * string * [ `Producer | `Consumer | `Suspend |
    `Suspend_ack ] * [ `Int64 of int64 | `Bool of bool ]
] [@@deriving sexp]
type traced_operation_list = traced_operation list [@@deriving sexp]
val trace: traced_operation_list -> unit Lwt.t

val logging_thread: unit -> unit Lwt.t
(** starts the background logging thread: this reads from a
    fixed-size logging buffer and writes to the physical log.
    If log items are dropped due to the buffer overflowing then
    this fact is guaranteed to be logged. *)

module Lwt_logger : sig
  val debug: ('a, unit, string, unit Lwt.t) format4 -> 'a
  val info:  ('a, unit, string, unit Lwt.t) format4 -> 'a
  val warn:  ('a, unit, string, unit Lwt.t) format4 -> 'a
  val error: ('a, unit, string, unit Lwt.t) format4 -> 'a

  val trace: traced_operation_list -> unit Lwt.t
end
