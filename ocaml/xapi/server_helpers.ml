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

module D = Debug.Make (struct let name = "dispatcher" end)

open D

exception Dispatcher_FieldNotFound of string

let ( let@ ) f x = f x

let my_assoc fld assoc_list =
  try List.assoc fld assoc_list
  with Not_found -> raise (Dispatcher_FieldNotFound fld)

let async_wire_name = "Async."

let async_length = String.length async_wire_name

let internal_async_wire_name = "InternalAsync."

let internal_async_length = String.length internal_async_wire_name

(** removes Async. or InternalAsync. prefixes if they exist
  * NB. X_y api call format does not work with Async (and didn't previously) *)
let sync_ty_and_maybe_remove_prefix x =
  (* expect more async calls than internal_async, so check for async first *)
  if Astring.String.is_prefix ~affix:async_wire_name x then
    (`Async, String.sub x async_length (String.length x - async_length))
  else if Astring.String.is_prefix ~affix:internal_async_wire_name x then
    ( `InternalAsync
    , String.sub x internal_async_length
        (String.length x - internal_async_length)
    )
  else
    (`Sync, x)

let unknown_rpc_failure func =
  API.response_of_failure Api_errors.message_method_unknown [func]

let parameter_count_mismatch_failure func expected received =
  API.response_of_failure Api_errors.message_parameter_count_mismatch
    [func; expected; received]

(** WARNING: the context is destroyed when execution is finished if the task is not forwarded, in database and not called asynchronous. *)
let exec_with_context ~__context ~need_complete ?marshaller ?f_forward
    ?(called_async = false) ?quiet f =
  (* Execute fn f in specified __context, marshalling result with "marshaller" *)
  let exec () =
    (* NB:
       1. If we are a slave we process the call locally assuming the locks have
       already been taken by the master
       2. If we are the master, locks are only necessary for the potentially-forwarded
       (ie side-effecting) operations and not things like the database layer *)
    try
      let result =
        if not (Pool_role.is_master ()) then
          f ~__context (* slaves process everything locally *)
        else
          match f_forward with
          | None ->
              (* this operation cannot be forwarded (eg database lookup); do it now *)
              f ~__context
          | Some forward ->
              (* use the forwarding layer (NB this might make a local call ultimately) *)
              forward ~local_fn:f ~__context
      in
      if need_complete then
        match marshaller with
        | None ->
            TaskHelper.complete ~__context None
        | Some fn ->
            TaskHelper.complete ~__context (Some (fn result))
      else
        Context.complete_tracing __context ;
      result
    with
    | Api_errors.Server_error (a, _) as e when a = Api_errors.task_cancelled ->
        Backtrace.is_important e ;
        if need_complete then TaskHelper.cancel ~__context ;
        raise e
    | e ->
        Backtrace.is_important e ;
        if need_complete then TaskHelper.failed ~__context e ;
        raise e
  in
  Locking_helpers.Thread_state.with_named_thread
    (TaskHelper.get_name ~__context) (Context.get_task_id __context) (fun () ->
      let client = Context.get_client __context in
      Debug.with_thread_associated ?client ?quiet
        (Context.string_of_task __context)
        (fun () ->
          (* CP-982: promote tracking debug line to info status *)
          if called_async then
            info "spawning a new thread to handle the current task%s"
              (Context.trackid ~with_brackets:true ~prefix:" " __context) ;
          Xapi_stdext_pervasives.Pervasiveext.finally exec (fun () ->
              if not called_async then Context.destroy __context
              (* else debug "nothing more to process for this thread" *)
          )
        )
        ()
  )

let dispatch_exn_wrapper f =
  try f ()
  with exn ->
    let code, params = ExnHelper.error_of_exn exn in
    API.response_of_failure code params

module Helper = struct
  include Tracing.Propagator.Make (struct
    include Tracing_propagator.Propagator.Http

    let name_span req = req.Http.Request.uri
  end)
end

let do_dispatch ?session_id ?forward_op ?self:_ supports_async called_fn_name
    op_fn marshaller fd http_req label sync_ty generate_task_for =
  (* if the call has been forwarded to us, then they are responsible for completing the task, so we don't need to complete it *)
  let@ http_req = Helper.with_tracing ~name:__FUNCTION__ http_req in
  let called_async = sync_ty <> `Sync in
  if called_async && not supports_async then
    API.response_of_fault
      ("No async mode for this operation (rpc: " ^ called_fn_name ^ ")")
  else
    let internal_async_subtask = sync_ty = `InternalAsync in
    let __context =
      Context.of_http_req ?session_id ~internal_async_subtask ~generate_task_for
        ~supports_async ~label ~http_req ~fd ()
    in
    let sync () =
      let need_complete = not (Context.forwarded_task __context) in
      exec_with_context ~__context ~need_complete ~called_async
        ?f_forward:forward_op ~marshaller op_fn
      |> marshaller
      |> Rpc.success
    in
    let async ~need_complete =
      (* Fork thread in which to execute async call *)
      ignore
        (Thread.create
           (fun () ->
             exec_with_context ~__context ~need_complete ~called_async
               ?f_forward:forward_op ~marshaller op_fn
           )
           ()
        ) ;
      (* Return task id immediately *)
      Rpc.success (API.rpc_of_ref_task (Context.get_task_id __context))
    in
    match sync_ty with
    | `Sync ->
        sync ()
    | `Async ->
        let need_complete = not (Context.forwarded_task __context) in
        async ~need_complete
    | `InternalAsync ->
        async ~need_complete:true

(* regardless of forwarding, we are expected to complete the task *)

(* in the following functions, it is our responsibility to complete any tasks we create *)
let exec_with_new_task ?http_other_config ?quiet ?subtask_of ?session_id
    ?task_in_database ?task_description ?origin task_name f =
  exec_with_context ?quiet
    ~__context:
      (Context.make ?http_other_config ?quiet ?subtask_of ?session_id
         ?task_in_database ?task_description ?origin task_name
      ) ~need_complete:true (fun ~__context -> f __context
  )

let exec_with_forwarded_task ?http_other_config ?session_id ?origin task_id f =
  exec_with_context
    ~__context:
      (Context.from_forwarded_task ?http_other_config ?session_id ?origin
         task_id
      ) ~need_complete:true (fun ~__context -> f __context
  )

let exec_with_subtask ~__context ?task_in_database task_name f =
  let subcontext =
    Context.make_subcontext ~__context ?task_in_database task_name
  in
  exec_with_context ~__context:subcontext ~need_complete:true f

let forward_extension ~__context rbac call =
  rbac __context (fun () -> Xapi_extensions.call_extension call)
