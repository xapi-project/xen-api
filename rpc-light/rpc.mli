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

(** {2 Value} *)

type t =
    Int of int64
  | Bool of bool
  | Float of float
  | String of string
  | Enum of t list
  | Dict of (string * t) list
  | Null

val to_string : t -> string

(** {2 Basic constructors} *)

val int64_of_rpc : t -> int64
val rpc_of_int64 : int64 -> t

val int32_of_rpc : t -> int32
val rpc_of_int32 : int32 -> t

val int_of_rpc : t -> int
val rpc_of_int : int -> t

val bool_of_rpc : t -> bool
val rpc_of_bool : bool -> t

val float_of_rpc : t -> float
val rpc_of_float : float -> t

val string_of_rpc : t -> string
val rpc_of_string : string -> t

val t_of_rpc : t -> t
val rpc_of_t : t -> t

val unit_of_rpc : t -> unit
val rpc_of_unit : unit -> t

(** {2 Calls} *)

type callback = string list -> t -> unit

type call = { name : string; params : t list }

val call : string -> t list -> call

val string_of_call : call -> string

(** {2 Responses} *)

type response = { success : bool; contents : t }

val string_of_response : response -> string

val success : t -> response
val failure : t -> response

(** {2 Run-time errors} *)

exception Runtime_error of string * t
exception Runtime_exception of string * string

(** {2 Debug options} *)
val set_debug : bool -> unit
val get_debug : unit -> bool
