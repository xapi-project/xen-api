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
(* task constructor is hidden : it us used internally by context.ml*)
(* val string_of_task: string -> API.ref_task -> string *)
val get_name : __context:Context.t -> string
val operate_on_db_task :
  __context:Context.t -> (API.ref_task -> unit) -> unit
val destroy : __context:Context.t -> API.ref_task -> unit
val set_description : __context:Context.t -> string -> unit
val add_to_other_config : __context:Context.t -> string -> string -> unit
val set_progress : __context:Context.t -> float -> unit
val set_external_pid : __context:Context.t -> int -> unit
val clear_external_pid : __context:Context.t -> unit
val set_result_on_task :
  __context:Context.t -> [ `task ] Ref.t -> Rpc.t option -> unit
val set_result : __context:Context.t -> Rpc.t option -> unit
val status_is_completed : [> `cancelled | `failure | `success ] -> bool
val complete : __context:Context.t -> Rpc.t option -> unit
val set_cancellable : __context:Context.t -> unit
val set_not_cancellable : __context:Context.t -> unit
val is_cancelling : __context:Context.t -> bool
val raise_cancelled : __context:Context.t -> 'a
val exn_if_cancelling : __context:Context.t -> unit
val cancel : __context:Context.t -> unit

val failed : __context:Context.t -> exn -> unit
(** Call this when a task fails with [exn] *)

val init : unit -> unit
val rbac_assert_permission_fn : (__context:Context.t -> permission:Db_actions.role_t -> unit) option ref
val assert_op_valid :  ?ok_if_no_session_in_context:bool -> __context:Context.t ->  [ `task ] Ref.t -> unit

type id =
  | Sm of string
  | Xenops of string * string (** queue name * VM id *)

val id_to_task_exn : id -> API.ref_task
val task_to_id_exn : API.ref_task -> id
val register_task : Context.t -> ?cancellable:bool -> id -> unit
val unregister_task : Context.t -> id -> unit


