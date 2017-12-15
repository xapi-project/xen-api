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
val wait_for_all : rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> tasks:API.ref_task list -> unit

(** [with_tasks_destroy ~rpc ~session_id ~timeout ~tasks] is like [wait_for_all] except after [timeout] has elapsed
    it will cancel pending tasks and return false.
    Finally it will destroy all tasks *)
val with_tasks_destroy: rpc:(Rpc.call -> Rpc.response) -> session_id:API.ref_session -> timeout:float -> tasks:API.ref_task list -> bool
