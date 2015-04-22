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

val whoami: unit -> string

module M : S
  with type 'a IO.t = 'a Lwt.t

module Connection : sig
  val rpc: (M.IO.ic * M.IO.oc) -> In.t -> [ `Ok of string | `Error of exn ] Lwt.t
end

module Client : sig
  type t

  val connect: int -> string -> [ `Ok of t | `Error of exn ] Lwt.t

  val disconnect: t -> unit Lwt.t
  (** [disconnect] closes the connection *)

  val rpc: t -> ?timeout:int -> string  -> [ `Ok of string | `Error of exn ] Lwt.t

  val list: t -> string -> [ `Ok of string list | `Error of exn ] Lwt.t

  val destroy: t -> string -> [ `Ok of unit | `Error of exn ] Lwt.t
  (** [destroy t queue_name] destroys the named queue, and all associated
      messages. *)

  val shutdown: t -> [ `Ok of unit | `Error of exn ] Lwt.t
  (** [shutdown t] requests that the message switch shuts down *)
end

module Server : sig
  type t
  (** A listening server *)

  val listen: (string -> string Lwt.t) -> int -> string -> [ `Ok of t | `Error of exn ] Lwt.t

  val shutdown: t -> unit Lwt.t
  (** [shutdown t] shutdown a server *)
end
