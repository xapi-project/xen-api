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
(** BIOS strings handling *)

(** Obtains the BIOS strings of localhost. *)
val get_host_bios_strings :
  __context:Context.t -> (string * string) list

(** Exposed to test the module *)

type record = { name : string; values : (string * string) list; }

module P : sig
    val records : record list Angstrom.t
end

val get_output_of_type : string -> string

val get_baseboard_strings :
  (string -> string -> (string * string) list) -> (string * string) list

val get_oem_strings :
  (string -> string -> (string * string) list) -> (string * string) list
