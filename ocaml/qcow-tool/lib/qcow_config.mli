(*
 * Copyright (C) 2017 David Scott <dave@recoil.org>
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

type t = {
  id: string;
  (** unique name for the prometheus metrics *)

  discard: bool;
  (** discard (aka TRIM) is enabled *)

  keep_erased: int64 option;
  (** maintain a free pool of this many erased sectors *)

  compact_after_unmaps: int64 option;
  (** once more than this many sectors are free, perform a compact *)

  check_on_connect: bool;
  (** perform an integrity check on connect *)

  runtime_asserts: bool;
  (** constantly verify GC invariants are held *)

  read_only: bool;
  (** guarantee to not modify the file *)
}

val create:
  ?id:string ->
  ?discard:bool -> ?keep_erased:int64 ->
  ?compact_after_unmaps:int64 -> ?check_on_connect:bool ->
  ?runtime_asserts:bool -> ?read_only:bool ->
  unit -> t

val default: unit -> t
(** default configuration values *)

val to_string: t -> string
(** convert the configuration to a string *)

val of_string: string -> (t, [> `Msg of string ]) result
(** parse the output of [to_string t] *)
