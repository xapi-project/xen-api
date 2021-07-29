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

(* Module to parse result from command line output
 * The result should be key value pairs seperated by sep like ':' *)

type t

(* Empty result set *)
val empty : t

(* Add one line to the result set *)
val add : ?sep:char -> string -> t -> t

(* Find result from the result set *)
val find : string -> t -> string option

(* find result from the whole result lines *)
val of_output_opt : sep:char -> key:string -> lines:string -> string option

(* find result from the whole result lines *)
val of_output : sep:char -> key:string -> string -> string
