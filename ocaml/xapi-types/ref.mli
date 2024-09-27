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

type without_secret = Uuidx.without_secret

type secret = Uuidx.secret

type not_secret =
  [ without_secret
  | `session of [`use_make_secret_or_ref_of_secret_string_instead] ]

type all = Uuidx.all

type 'a t constraint 'a = [< all]

val rpc_of_t : ('a -> Rpc.t) -> 'a t -> Rpc.t

val t_of_rpc : (Rpc.t -> 'a) -> Rpc.t -> 'a t

val ref_prefix : string

val make : unit -> [< not_secret] t

val make_secret : unit -> [< secret] t

val null : _ t

val compare : 'a t -> 'a t -> int
(** [compare a b] returns [0] if [a] and [b] are equal, a negative integer if
    [a] is less than [b], and a positive integer if [a] is greater than [b]. *)

val string_of : 'a t -> string

val to_option : 'a t -> 'a t option
(** [to_option ref] returns [None] when [ref] is [Ref.Null] or [Some ref]
    otherwise *)

val short_string_of : 'a t -> string

val of_string : string -> [< not_secret] t

val of_secret_string : string -> [< secret] t

val make_dummy : string -> [< not_secret] t

val is_real : 'a t -> bool

val is_dummy : 'a t -> bool

val name_of_dummy : 'a t -> string

val really_pretty_and_small : 'a t -> string

val pp : Format.formatter -> 'a t -> unit
