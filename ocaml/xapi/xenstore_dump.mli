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
exception Invalid_path of string

(** Dump a xenstore subtree as XML *)
val dump : xs:Xenstore.Xs.xsh -> string -> Xml.xml

(** Restore a xenstore subtree from XML at a new path. Permissions are not restored
    and therefore will inherit from the parent node. *)
val restore : xs:Xenstore.Xs.xsh -> string -> Xml.xml -> unit
