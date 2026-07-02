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

val exec_with_new_task :
     ?http_other_config:(string * string) list
  -> ?quiet:bool
  -> ?subtask_of:API.ref_task
  -> ?session_id:API.ref_session
  -> ?task_in_database:bool
  -> ?task_description:string
  -> ?origin:Context.origin
  -> string
  -> (Context.t -> 'a)
  -> 'a

val exec_with_forwarded_task :
     ?http_other_config:(string * string) list
  -> ?session_id:API.ref_session
  -> ?origin:Context.origin
  -> API.ref_task
  -> (Context.t -> 'a)
  -> 'a

val exec_with_subtask :
     __context:Context.t
  -> ?task_in_database:bool
  -> string
  -> (__context:Context.t -> 'b)
  -> 'b

(* used by auto-generated code in server.ml *)
val my_assoc : string -> (string * 'a) list -> 'a

val sync_ty_and_maybe_remove_prefix :
  string -> [> `Async | `InternalAsync | `Sync] * string

val unknown_rpc_failure : string -> Rpc.response

val parameter_count_mismatch_failure :
  string -> string -> string -> Rpc.response

val dispatch_exn_wrapper : (unit -> Rpc.response) -> Rpc.response

val do_dispatch :
     ?session_id:API.ref_session
  -> ?forward_op:
       (local_fn:(__context:Context.t -> 'a) -> __context:Context.t -> 'a)
  -> ?self:'b
  -> bool
  -> string
  -> (__context:Context.t -> 'a)
  -> ('a -> Rpc.t)
  -> Unix.file_descr option
  -> Http.Request.t
  -> string
  -> [< `Async | `InternalAsync | `Sync > `Sync `InternalAsync]
  -> bool
  -> Rpc.response

val forward_extension :
  __context:'a -> ('a -> (unit -> Rpc.response) -> 'b) -> Rpc.call -> 'b
