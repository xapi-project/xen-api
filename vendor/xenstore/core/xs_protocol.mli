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

(** XenStore protocol. *)

type t
(** A valid packet. *)

module Op : sig
  type t =
    | Debug
    | Directory
    | Read
    | Getperms
    | Watch
    | Unwatch
    | Transaction_start
    | Transaction_end
    | Introduce
    | Release
    | Getdomainpath
    | Write
    | Mkdir
    | Rm
    | Setperms
    | Watchevent
    | Error
    | Isintroduced
    | Resume
    | Set_target
    | Restrict
  (** The type of xenstore operation. *)

  val to_string: t -> string
  val of_int32: int32 -> t option
  val to_int32: t -> int32
end

module ACL : sig

  type perm =
    | NONE
    | READ
    | WRITE
    | RDWR

  val char_of_perm: perm -> char
  val perm_of_char: char -> perm option

  type domid = int

  type t = {
    owner: domid;             (** domain which "owns", has full access *)
    other: perm;              (** default permissions for all others... *)
    acl: (domid * perm) list; (** ... unless overridden in the ACL *)
  }

  val of_string: string -> t option
  val to_string: t -> string
end
(** Access control lists. *)

module Parser : sig

  val allow_oversize_packets: bool ref

  type state =
    | Unknown_operation of int32 (** received an unexpected message type *)
    | Parser_failed of string    (** we failed to parse a header *)
    | Need_more_data of int      (** we still need 'n' bytes *)
    | Packet of t                (** successfully decoded a packet *)

  type parse
  (** The internal state of the parser. *)

  val start: unit -> parse
  (** Create a parser set to the initial state. *)

  val state: parse -> state
  (** Query the state of the parser. *)

  val input: parse -> string -> parse
  (** Input some bytes into the parser. Must be no more than needed
      (see Need_more_data above). *)
end
(** Incrementally parse packets. *)

module type IO = sig
  type 'a t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t

  type channel
  val read: channel -> bytes -> int -> int -> int t
  val write: channel -> bytes -> int -> int -> unit t
end

exception Unknown_xenstore_operation of int32
exception Response_parser_failed of string

type ('a, 'b) result =
	| Ok of 'a
	| Exception of 'b

module PacketStream : functor(IO: IO) -> sig
  type stream
  val make: IO.channel -> stream
  val recv: stream -> (t, exn) result IO.t
  val send: stream -> t -> unit IO.t
end

val to_bytes : t -> bytes
val get_tid : t -> int32
val get_ty : t -> Op.t
val get_data : t -> string
val get_rid : t -> int32

val create : int32 -> int32 -> Op.t -> string -> t

module Token : sig
  type t
  (** A token is associated with every watch and returned in the
      callback. *)

  val to_debug_string: t -> string
  (** [to_string token] is a debug-printable version of [token]. *)

  val to_user_string: t -> string
  (** [to_user_string token] is the user-supplied part of [token]. *)

  val of_string: string -> t
  (** [of_string str_rep] is the token resulting from the
      unmarshalling of [str_rep]. *)

  val to_string: t -> string
  (** [to_string token] is the marshalled representation of [token]. *)
end

module Response : sig
  type payload =
  | Read of string
  | Directory of string list
  | Getperms of ACL.t
  | Getdomainpath of string
  | Transaction_start of int32
  | Write
  | Mkdir
  | Rm
  | Setperms
  | Watch
  | Unwatch
  | Transaction_end
  | Debug of string list
  | Introduce
  | Resume
  | Release
  | Set_target
  | Restrict
  | Isintroduced of bool
  | Error of string
  | Watchevent of string * string

  val ty_of_payload: payload -> Op.t

  val prettyprint_payload: payload -> string

  val print: payload -> int32 -> int32 -> t
end

module Request : sig

  type path_op =
  | Read
  | Directory
  | Getperms
  | Write of string
  | Mkdir
  | Rm
  | Setperms of ACL.t

  type payload =
  | PathOp of string * path_op
  | Getdomainpath of int
  | Transaction_start
  | Watch of string * string
  | Unwatch of string * string
  | Transaction_end of bool
  | Debug of string list
  | Introduce of int * Nativeint.t * int
  | Resume of int
  | Release of int
  | Set_target of int * int
  | Restrict of int
  | Isintroduced of int
  | Error of string
  | Watchevent of string

  val ty_of_payload: payload -> Op.t

  val prettyprint_payload: payload -> string
  val prettyprint: t -> string

  val parse: t -> payload option
  val print: payload -> int32 -> int32 -> t
end

module Unmarshal : sig
  val string : t -> string option
  val list : t -> string list option
  val acl : t -> ACL.t option
  val int : t -> int option
  val int32 : t -> int32 option
  val unit : t -> unit option
  val ok : t -> unit option
end

exception Enoent of string (** Raised when a named key does not exist. *)
exception Eagain           (** Raised when a transaction must be repeated. *)
exception Eexist           (** Raised when a watch already exists. *)
exception Invalid
exception Error of string  (** Generic catch-all error. *)

val response: string -> t -> t -> (t -> 'a option) -> 'a
(** [response debug_hint sent received unmarshal] returns the
    [unmarshal]led response corresponding to the [received] packet
    relative to the [sent] packet. *)

type address =
| Unix of string
| Domain of int

val string_of_address: address -> string
val domain_of_address: address -> int
