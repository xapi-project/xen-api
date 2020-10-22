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

val split : string -> int -> string * string

val break : (char -> bool) -> string -> string * string

val str_drop_while : (char -> bool) -> string -> string

val marshal_int : ?bigendian:bool -> int -> int64 -> string

val marshal_int8 : int -> string

val marshal_int16 : int -> string

val marshal_int32 : int32 -> string

val marshal_int64 : int64 -> string

val unmarshal_int : ?bigendian:bool -> int -> string -> int64

val unmarshal_int8 : string -> int

val unmarshal_int16 : string -> int

val unmarshal_int32 : string -> int32

val unmarshal_int64 : string -> int64

val unmask : string -> string -> string
