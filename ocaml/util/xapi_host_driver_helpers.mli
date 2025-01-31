(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

type note = {typ: int32; name: string; desc: string}

(* Parse an ELF notes section, returning the specially-encoded driver version.

   The kernel does not reveal the location from where it loaded an active
   driver. Hence the name is not sufficient to observe the currently active
   version. For this, XS uses ELF notes, with the kernel presenting a particular
   note section in `/sys/module/<name>/notes/.note.XenServer` *)
val get_version : string -> (string, string) result

val get_notes : string -> (note list, string) result

(* Dumps JSON-formatted parsed ELF notes of a driver *)
val dump_notes : string -> unit
