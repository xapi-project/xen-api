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
type indexrec = {name_label:string option; uuid: string; _ref: string}
val string_of : indexrec -> string
val insert : indexrec -> unit
val remove : string (* ref or uuid *) -> unit
val update_name_label : string (* ref *) -> string -> unit
val update_uuid : string (* ref *) -> string -> unit
val lookup : string (* ref or uuid *) -> indexrec option
