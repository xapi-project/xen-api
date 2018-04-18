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
open Locking_helpers
open Stdext
open Xstringext
open Pervasiveext
open Threadext

module D = Debug.Make(struct let name = "dispatcher" end)
open D

exception Dispatcher_FieldNotFound of string
let my_assoc fld assoc_list =
  try
    List.assoc fld assoc_list
  with
    Not_found -> raise (Dispatcher_FieldNotFound fld)

exception Nth (* should never be thrown externally *)
let rec nth n l =
  match l with
    [] -> raise Nth
  | x::xs -> if n=1 then x else nth (n-1) xs

let async_wire_name = "Async."

let async_length = String.length async_wire_name

(* hardcode the wire-name messages that we want to supress the printing of in the logs to avoid log spam: *)
let supress_printing_for_these_messages : (string,unit) Hashtbl.t =
  let tbl = Hashtbl.create 20 in
  List.iter (fun k -> Hashtbl.replace tbl k ()) ["host.tickle_heartbeat"; "session.login_with_password"; "session.logout"; "session.local_logout"; "session.slave_local_login"; "session.slave_local_login_with_password"];
  tbl

let is_async x =
  String.length x > async_length && (String.sub x 0 async_length = async_wire_name)

let remove_async_prefix x =
  String.sub x async_length (String.length x - async_length)

let unknown_rpc_failure func =
  API.response_of_failure Api_errors.message_method_unknown [func]

let parameter_count_mismatch_failure func expected received =
  API.response_of_failure Api_errors.message_parameter_count_mismatch [func; expected; received]


(** WARNING: the context is destroyed when execution is finished if the task is not forwarded, in database and not called asynchronous. *)
(*  FIXME: This function should not be used for external call : we should add a proper .mli file to hide it. *)
let exec_with_context ~__context ?marshaller ?f_forward ?(called_async=false) ?(has_task=false) f =
  (* Execute fn f in specified __context, marshalling result with "marshaller".
     If has_task is set then __context has a real task in it that has to be completed. *)
  let exec () =
    (* NB:
       1. If we are a slave we process the call locally assuming the locks have
       already been taken by the master
       2. If we are the master, locks are only necessary for the potentially-forwarded
       (ie side-effecting) operations and not things like the database layer *)

    (* For forwarded task, we should not complete it here, the server which forward the task will complete it. However for the task forwarded by client, which param `has_task` is set with `true`, we have to complete it also. *)
    let need_complete = has_task || (not (Context.forwarded_task __context)) in
    try
      let result =
        if not(Pool_role.is_master ())
        then f ~__context (* slaves process everything locally *)
        else match f_forward with
          | None ->
            (* this operation cannot be forwarded (eg database lookup); do it now *)
            f ~__context
          | Some forward ->
            (* use the forwarding layer (NB this might make a local call ultimately) *)
            forward ~local_fn:f ~__context
      in
      if need_complete then begin
        match marshaller with
        | None    -> TaskHelper.complete ~__context None
        | Some fn -> TaskHelper.complete ~__context (Some (fn result))
      end;
      result
    with
    | Api_errors.Server_error (a,b) as e when a = Api_errors.task_cancelled ->
      Backtrace.is_important e;
      if need_complete then TaskHelper.cancel ~__context;
      raise e
    | e ->
      Backtrace.is_important e;
      if need_complete then TaskHelper.failed ~__context e;
      raise e
  in
  Locking_helpers.Thread_state.with_named_thread (TaskHelper.get_name ~__context) (Context.get_task_id __context)
    (fun () ->
       Debug.with_thread_associated (Context.string_of_task __context)
         (fun () ->
            (* CP-982: promote tracking debug line to info status *)
            if called_async then info "spawning a new thread to handle the current task%s" (Context.trackid ~with_brackets:true ~prefix:" " __context);
            finally exec (fun () ->
                if not called_async then Context.destroy __context
                (* else debug "nothing more to process for this thread" *)
              )
         )
         ()
    )

let dispatch_exn_wrapper f =
  try
    f()
  with exn -> let code, params = ExnHelper.error_of_exn exn in API.response_of_failure code params

let do_dispatch ?session_id ?forward_op ?self called_async supports_async called_fn_name op_fn
    marshaller_fn fd http_req label generate_task_for =

  if (called_async && (not supports_async))
  then API.response_of_fault ("No async mode for this operation (rpc: "^called_fn_name^")")
  else
    let __context = Context.of_http_req ?session_id ~generate_task_for ~supports_async ~label ~http_req ~fd in
    if called_async
    then begin
      (* Fork thread in which to execute async call *)
      ignore (Thread.create
                (fun () ->
                   exec_with_context ~__context ~called_async ?f_forward:forward_op ~marshaller:marshaller_fn op_fn)
                ());
      (* Return task id immediately *)
      Rpc.success (API.rpc_of_ref_task (Context.get_task_id __context))
    end else
      let result =
        exec_with_context ~__context ~called_async ?f_forward:forward_op ~marshaller:marshaller_fn op_fn
      in
      Rpc.success (marshaller_fn result)

let exec_with_new_task ?http_other_config ?quiet ?subtask_of ?session_id ?task_in_database ?task_description ?origin task_name f =
  exec_with_context
    ~__context:(Context.make ?http_other_config ?quiet ?subtask_of ?session_id ?task_in_database ?task_description ?origin task_name)
    (fun ~__context -> f __context)

let exec_with_forwarded_task ?http_other_config ?session_id ?origin task_id f =
  exec_with_context
    ~__context:(Context.from_forwarded_task ?http_other_config ?session_id ?origin task_id)
    ~has_task:true
    (fun ~__context -> f __context)

let exec_with_subtask ~__context ?task_in_database ?task_description task_name f =
  let subtask_of = Context.get_task_id __context in
  let session_id = try Some (Context.get_session_id __context) with _ -> None in
  let new_context = Context.make ~subtask_of ?session_id ?task_in_database ?task_description task_name in
  exec_with_context ~__context:new_context f

let forward_extension ~__context rbac call =
  rbac __context (fun () -> Xapi_extensions.call_extension call)
