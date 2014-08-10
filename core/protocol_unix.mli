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

open Protocol

val whoami : unit -> string

module IO : sig
	type ic
	type oc

	val connect : int -> (ic * oc)
	(** [connect port] connects to a switch listening on [port] *)
end

module Connection : sig
	val rpc: (IO.ic * IO.oc) -> In.t -> (string, exn) result
end

module Client : sig
	type t

	val connect: int -> t

	val rpc: t -> ?timeout:int -> dest:string -> string  -> string

	val list: t -> string -> string list
end

module Server : sig

	val listen: (string -> string) -> int -> string -> unit
end
