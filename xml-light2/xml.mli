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
(** tree representation *)
type xml =
	| Element of (string * (string * string) list * xml list)
	| PCData of string

type error_pos
type error = string * error_pos

exception Error of error

val error : error -> string

(** input functions *)
val parse_file : string -> xml
val parse_in : in_channel -> xml
val parse_string : string -> xml
val parse_bigbuffer : Xapi_stdext_bigbuffer.Bigbuffer.t -> xml

(** output functions *)
val to_fct : xml -> (string -> unit) -> unit
val to_fct_fmt : xml -> (string -> unit) -> unit
val to_string : xml -> string
val to_string_fmt : xml -> string
val to_bigbuffer : xml -> Xapi_stdext_bigbuffer.Bigbuffer.t

(** helper functions *)
exception Not_pcdata of string
exception Not_element of string
val pcdata : xml -> string
val children : xml -> xml list
val tag : xml -> string
