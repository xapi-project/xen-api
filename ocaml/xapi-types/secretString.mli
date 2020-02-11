(*
 * Copyright (C) 2020 Citrix Systems Inc.
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

(* Prevent direct conversions to string to avoid accidental misuse.
 * It is still possible to convert it to Rpc.t and recover it that way,
 * it is not a protection against willfully recovering the protected string
 * (we do need to send these as parameters in RPCs).
 * *)

type t
(** a type with no direct conversions to string *)

val of_string : string -> t

val equal : t -> t -> bool

val t_of_rpc : Rpc.t -> t

(** [rpc_of_t secret] serializes [secret]. Should be used with caution! *)
val rpc_of_t : t -> Rpc.t

val of_request: Http.Request.t -> t option

(** [with_cookies secret request] adds a cookie containing [secret] to [request] *)
val with_cookie : t -> Http.Request.t -> Http.Request.t

(** [write_to_file path secret] should be used with caution! *)
val write_to_file : string -> t -> unit
