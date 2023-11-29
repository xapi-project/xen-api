(*
 * Copyright (C) Cloud Software Group
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

type dict = (string * string) list

val typ_of_dict : (string * string) list Rpc.Types.typ

val dict : (string * string) list Rpc.Types.def

val option :
     ?name:string
  -> ?description:string list
  -> 'a Rpc.Types.def
  -> 'a option Rpc.Types.def

val list :
     ?name:string
  -> ?description:string list
  -> 'a Rpc.Types.def
  -> 'a list Rpc.Types.def

val pair :
     ?name:string
  -> ?description:string list
  -> 'a Rpc.Types.def * 'b Rpc.Types.def
  -> ('a * 'b) Rpc.Types.def

val triple :
     ?name:string
  -> ?description:string list
  -> 'a Rpc.Types.def * 'b Rpc.Types.def * 'c Rpc.Types.def
  -> ('a * 'b * 'c) Rpc.Types.def
