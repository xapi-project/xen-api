(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Marshal functions for converting OCaml values to and from XML-RPC. *)

(** Run time type errors are raised when XML of an unexpected structure is
    encountered. This error conveys the expected "type" as a string and the
    actual XML encountered. *)
exception RunTimeTypeError of string * Xml.xml

type xmlrpc = Xml.xml

(** An XML-RPC response: *)
type response =
  | Success of xmlrpc list           (** normal result *)
  | Failure of string * (string list) (** failure/ exception in high-level code *)
  | Fault of (int32 * string)         (** error in the XMLRPC handling *)
  | Raw of xmlrpc list

(** Functions to marshal some ocaml values to strings, suitable for
    keys in XMLRPC structs *)
module ToString : sig
  val int64 : int64 -> string
  val double : float -> string
  val string : string -> string
end

(** Functions to unmarshal some ocaml values from strings, suitable for
    keys in XMLRPC structs *)
module FromString : sig
  val int64 : string -> int64
  val double : string -> float
  val string : string -> string
end

(** Functions to marshal OCaml values to our subset of XML-RPC. *)
module To : sig
  (** Marshal a nil value *)
  val nil : unit -> xmlrpc

  (** Marshal a homogeneous array. *)
  val array : xmlrpc list -> xmlrpc

  (** Marshal a boolean. *)
  val boolean : bool -> xmlrpc

  (** Marshal a date-time. *)
  val datetime : Xapi_stdext_date.Date.iso8601 -> xmlrpc

  (** Marshal a double. *)
  val double : float -> xmlrpc

  (** Marshal a int. *)
  val int : int32 -> xmlrpc

  (** Marshal a method call. *)
  val methodCall : string -> xmlrpc list -> xmlrpc

  (** Marshal a string. *)
  val string : string -> xmlrpc

  (** Marshal a struct. *)
  val structure : (string * xmlrpc) list -> xmlrpc

  (** Marshal a method response. *)
  val methodResponse : response -> xmlrpc

end

(** Higher-order functions to marshal values from our subset of XML-RPC. *)
module From : sig
  (** Parse a homogeneous array, applying f to the XML in element. *)
  val array : (xmlrpc -> 'a) -> xmlrpc -> 'a list

  val id : 'a -> 'a
  val pcdata : (string -> 'a) -> xmlrpc -> 'a
  val value : (xmlrpc -> 'a) -> xmlrpc -> 'a

  (** Parse a nil (XMLRPC extension) *)
  val nil : xmlrpc -> unit

  (** Parse a boolean. *)
  val boolean : xmlrpc -> bool

  (** Parse a date-time. *)
  val datetime : xmlrpc -> Xapi_stdext_date.Date.iso8601

  (** Parse a double. *)
  val double : xmlrpc -> float

  (** Parse a int. *)
  val int : xmlrpc -> int32

  (** Parse a method call. *)
  val methodCall : xmlrpc -> string * xmlrpc list

  (** Parse a string. *)
  val string : xmlrpc -> string

  (** Parse a struct. *)
  val structure : xmlrpc -> (string * xmlrpc) list

  (** Parse a method response. *)
  val methodResponse : xmlrpc -> response

end
