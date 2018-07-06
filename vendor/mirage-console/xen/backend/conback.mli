(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012-14 Citrix Systems Inc
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

module type ACTIVATIONS = sig

  (** Event channels handlers. *)

  type event
  (** identifies the an event notification received from xen *)

  val program_start: event
  (** represents an event which 'fired' when the program started *)

  val after: Eventchn.t -> event -> event Lwt.t
  (** [next channel event] blocks until the system receives an event
      newer than [event] on channel [channel]. If an event is received
      while we aren't looking then this will be remembered and the
      next call to [after] will immediately unblock. If the system is
      suspended and then resumed, all event channel bindings are
      invalidated and this function will fail with
      Generation.Invalid *)
end

type stats = {
  mutable total_read: int; (** bytes read *)
  mutable total_write: int; (** bytes written *)
}

module type CONSOLE = sig
  include Mirage_console_lwt.S
  val connect: string -> t Lwt.t
end

module Make(A: ACTIVATIONS)(X: Xs_client_lwt.S)(C: CONSOLE): sig
  val exists: X.client -> string -> bool Lwt.t
  val request_close: string -> int * int -> unit Lwt.t
  val force_close: int * int -> unit Lwt.t
  val run: string -> string -> int * int -> stats Lwt.t
  val destroy: string -> int * int -> unit Lwt.t
  val create:
    ?backend_domid:int -> ?name:string -> string -> int * int -> unit Lwt.t
end
