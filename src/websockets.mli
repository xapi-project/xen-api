(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module Wsprotocol : functor (IO : Iteratees.Monad) -> sig
  type 'a t = 'a Iteratees.Iteratee(IO).t

  val writer : (string -> unit IO.t) -> string -> unit t
  (** Exposing the writer from the IO Iteratee *)

  val base64encode : 'a t -> 'a t t
  (** enumeratee that base64 encodes individual chunks
        and passes them onto the sub-iteratee *)

  val base64decode : 'a t -> 'a t t
  (** enumeratee that base64 decodes individual chunks
        and passes them onto the sub-iteratee *)

  val wsframe : 'a t -> 'a t t
  (** enumeratee that websocket-encodes data and
        passes the encoded data to the sub-iteratee *)

  val wsframe_old : 'a t -> 'a t t
  (** enumeratee that old-style websocket-encodes data and
        passes the encoded data to the sub-iteratee *)

  val wsunframe : 'a t -> 'a t t
  (** enumeratee that websocket-decodes data and
        passes the decoded data to the sub-iteratee *)

  val wsunframe_old : 'a t -> 'a t t
  (** enumeratee that old-style websocket-decodes data and
        passes the decoded data to the sub-iteratee *)
end
