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

module TaskSet : Set.S with type elt = API.ref_task

val wait_for_all :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> tasks:API.ref_task list
  -> unit
(** [wait_for_all ~rpc ~session_id ~tasks] returns when all of [tasks]
    are in some non-pending state. *)

val wait_for_all_with_callback :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> tasks:API.ref_task list
  -> callback:(int -> API.ref_task -> API.ref_task list)
  -> unit
(** [wait_for_all_with_callback ~rpc ~session_id ~tasks ~callback] returns when
    all of [tasks] are in some non-pending state. When one of the [tasks] is
    completed, [callback overall_completed_task_count] is invoked, which returns
    a list of tasks that need to be added to [tasks] and waited on as well.

    This allows, for example, to implement a system where tasks are processed
    in batches of *constant* size X, with new tasks being started as soon as at
    least one slot in the batch is freed, instead of waiting for the whole batch
    to finish (and potentially being slowed down by a single worst performer).

    The callback could instead just perform some side-effect (set the progress
    of the overall task representing progress of individual units, for example)
    and return an empty list.
    *)

val with_tasks_destroy :
     rpc:(Rpc.call -> Rpc.response)
  -> session_id:API.ref_session
  -> timeout:float
  -> tasks:API.ref_task list
  -> bool
(** [with_tasks_destroy ~rpc ~session_id ~timeout ~tasks] is like [wait_for_all] except after [timeout] has elapsed
    it will cancel pending tasks and return false.
    Finally it will destroy all tasks *)
