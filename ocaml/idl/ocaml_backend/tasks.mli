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

(** [wait_for_all ~rpc ~session_id ~tasks] returns when all of [tasks]
    are in some non-pending state. *)
val wait_for_all : rpc:(Xml.xml -> Xml.xml) -> session_id:API.ref_session -> tasks:API.ref_task list -> unit
