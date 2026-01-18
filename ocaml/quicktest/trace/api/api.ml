(*
 * Copyright (c) Cloud Software Group, Inc
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
open Quicktest_trace
open Quicktest_trace_rpc
open Client.Client
open Types

module Task = struct
  type 'a t = {
      task: API.ref_task
    ; scope: Scope.t
    ; log_obj: client -> Scope.t -> unit
    ; of_rpc: Rpc.t -> 'a
    ; finally: (unit, string * Printexc.raw_backtrace) result -> unit
    ; mutable finished: ('a, exn) result option
  }

  let task t = t.task

  let v name of_rpc log_obj f =
    (* TODO: should mark span kind as async, but not supported yet *)
    let thunk, finally = Trace.with_' name @@ fun scope -> (scope, f ()) in
    let scope, task = thunk () in
    {task; scope; log_obj; of_rpc; finally; finished= None}

  let try_cancel t task =
    try
      Trace.with_ ~scope:task.scope "Task.try_cancel" @@ fun _ ->
      let self = task.task in
      let allowed = call t @@ Task.get_allowed_operations ~self in
      if List.mem `cancel allowed then call t @@ Task.cancel ~task:task.task
    with Api_errors.Server_error _ ->
      (* race: ignore errors, may have terminated on its own already *)
      ()

  (*let destroy t task =
    try
      let span_id = Scope.span_id task.scope
      and trace_id = Scope.trace_id task.scope in
      (* the scope has finished, so link to it instead of creating a child trace *)
      let links = [Opentelemetry.Span_link.make ~trace_id ~span_id ()] in
      Trace.with_ ~links "Task.destroy" @@ fun _ ->
      call t @@ Task.destroy ~self:task.task
    with Api_errors.Server_error _ ->
      (* task may have been GC-ed already, error logged by Trace.with_ above *)
      ()*)

  (* for remote calls the local callstack is not interesting on the remote span. *)
  let no_bt = Printexc.get_callstack 0

  let on_progress task progress =
    Scope.add_metrics task.scope @@ fun () ->
    Opentelemetry.Metrics.(gauge ~name:"task.progress" [float progress])

  let has_result task = Option.is_some task.finished

  let result t task =
    match task.finished with
    | Some r ->
        r
    | None ->
        Trace.with_ ~scope:task.scope "Task.result" @@ fun scope ->
        let remote_task = task.task in
        let status = call t @@ Task.get_status ~self:remote_task in
        let outcome =
          match status with
          | `cancelling | `cancelled ->
              Error
                (Api_errors.Server_error
                   (Api_errors.task_cancelled, [Ref.string_of remote_task])
                )
          | `pending ->
              Error
                (Failure
                   "wait_for_task_completion failed; task is still\n\
                   \          pending"
                )
          | `success ->
              call t @@ Task.get_result ~self:remote_task
              |> Xmlrpc.of_string
              |> task.of_rpc
              |> Result.ok
          | `failure ->
              let exn =
                try
                  let error_info =
                    call t @@ Task.get_error_info ~self:remote_task
                  in
                  let trace = call t @@ Task.get_backtrace ~self:remote_task in
                  let exn =
                    match error_info with
                    | code :: params ->
                        Api_errors.Server_error (code, params)
                    | [] ->
                        Failure
                          (Printf.sprintf
                             "Task failed but no error recorded: %s"
                             (Ref.string_of remote_task)
                          )
                  in
                  let bt =
                    Backtrace.(t_of_sexp (Sexplib.Sexp.of_string trace))
                  in
                  Scope.add_attrs scope (fun () ->
                      [
                        ( "exception.stacktrace"
                        , `String (Backtrace.to_string_hum bt)
                        )
                      ]
                  ) ;
                  task.log_obj t scope ;
                  task.finally (Error (Printexc.to_string exn, no_bt)) ;
                  exn
                with exn ->
                  Backtrace.is_important exn ;
                  let bt = Printexc.get_raw_backtrace () in
                  task.finally (Error (Printexc.to_string exn, no_bt)) ;
                  Printexc.raise_with_backtrace exn bt
              in
              Error exn
        in
        let () =
          task.finally
          @@
          match outcome with
          | Ok _ ->
              Ok ()
          | Error e ->
              Error (Printexc.to_string e, no_bt)
        in
        task.finished <- Some outcome ;
        outcome
end

module Object (O : OBJECT) = struct
  include O

  let log t scope ~self =
    (* best-effort: try to log the object status after the failed call *)
    try
      (* create a new span to log any espacing errors *)
      Trace.with_ ~scope "get_record" @@ fun scope ->
      let dbobj = call t @@ O.get_record ~self in
      Scope.set_decision scope Sampling.RECORD_AND_SAMPLE ;
      Scope.add_delayed_log scope @@ fun () ->
      RPC.log_rpc scope (O.string_of_ref self) (O.rpc_of_t dbobj)
    with _ -> ()

  let with_call t name self f =
    Trace.with_ name @@ fun scope ->
    try call t @@ f
    with Api_errors.Server_error _ as e ->
      let bt = Printexc.get_raw_backtrace () in
      Backtrace.is_important e ;
      Scope.add_attrs scope (fun () ->
          [
            ( "exception.stacktrace"
            , `String (e |> Backtrace.get |> Backtrace.to_string_hum)
            )
          ]
      ) ;
      log t scope ~self ;
      (* call backend tick callbacks, if any. These would sample metrics. *)
      SpanProcessor.force_flush () ;
      Printexc.raise_with_backtrace e bt

  let call_set t (f : (self:dbref -> value:'a -> unit) api) ~self ~value : unit
      =
    with_call t "set" self @@ f ~self ~value

  let call_get t (f : (self:dbref -> 'a) api) ~self =
    Trace.with_ "get" @@ fun _ -> call t @@ f ~self

  module Task = struct
    include Task

    let of_call t name of_rpc obj f =
      let log_obj t scope = log t scope ~self:obj in
      Task.v name of_rpc log_obj @@ fun () -> with_call t "task" obj f
  end

  let task = Task.of_call
end

module VM = struct
  include Object (struct
    type t = API.vM_t

    let rpc_of_t = API.rpc_of_vM_t

    type dbref = API.ref_VM

    let dbref_of_rpc = Ref.t_of_rpc (fun _ -> `VM)

    let string_of_ref = Ref.string_of

    let get_record = VM.get_record
  end)

  include VM

  module Async = struct
    let clone t ~vm ~new_name =
      task t __FUNCTION__ dbref_of_rpc vm @@ Async.VM.clone ~vm ~new_name

    let pool_migrate t ~vm ~host ~options =
      task t __FUNCTION__ ignore vm @@ Async.VM.pool_migrate ~vm ~host ~options

    let hard_reboot t ~vm =
      task t __FUNCTION__ ignore vm @@ Async.VM.hard_reboot ~vm

    let hard_shutdown t ~vm =
      task t __FUNCTION__ ignore vm @@ Async.VM.hard_shutdown ~vm

    let start_on t ~vm ~host ~start_paused ~force =
      task t __FUNCTION__ ignore vm
      @@ Async.VM.start_on ~vm ~host ~start_paused ~force
  end
end

module TaskMap = Map.Make (struct
  type t = API.ref_task

  let compare = Ref.compare
end)

(*let test t ~host vm =
  VM.task t "start_on" ignore vm
  @@ Async.VM.start_on ~vm ~host ~start_paused:true ~force:false*)

module P = Cli_progress_bar.Make (struct
  type t = float

  let to_float = Fun.id
end)

let on_progress_cli name ~total =
  let p = P.create 80 0. 1. in
  Printf.eprintf "\n%s\n%!" name ;
  (* ensure we're at beginning of line *)
  fun completed progress ->
    if P.update p progress then Printf.eprintf "\r%s%!" P.(string_of_bar p) ;
    if completed = total then
      Printf.eprintf "\r%s\r%s%!" (String.make p.width ' ') P.(summarise p)

let run_with_progress t name _scope ?(on_progress = on_progress_cli name)
    ~callback ~total tasks =
  (* Waiting for tasks to complete generates a lot of read queries,
     that we don't want to clutter the output, hence use a new trace id here.
     Launching and completeing tasks will still use their own scopes.
   *)
  Trace.with_ ~force_new_trace_id:true (name ^ "/run_with_progress")
  @@ fun scope ->
  (* total can be > List.length tasks in case [callback] inserts more tasks *)
  let count = float_of_int total in
  let tbl = Hashtbl.create 7 in
  let overall = ref 0. in
  let on_progress = on_progress ~total in
  let on_task_progress task completed progress =
    let old = Hashtbl.find_opt tbl task |> Option.value ~default:0. in
    (* update running sum by removing old and adding new progress of task *)
    overall := !overall -. old +. progress ;
    Hashtbl.replace tbl task progress ;
    (* progress is between [0, 1], so divide by [count] *)
    let progress = !overall /. count in
    let () =
      Scope.add_metrics scope @@ fun () ->
      Opentelemetry.Metrics.(gauge ~name:"task.progress" [float progress])
    in
    on_progress completed progress
  in
  call t
  @@ Tasks.wait_for_all_with_progress ~tasks:(List.map Task.task tasks)
       ~callback ~on_progress:on_task_progress ;
  on_progress total 1.

let iter_lazy f tasks =
  tasks
  |> List.iter @@ fun task ->
     if Lazy.is_val task then
       let task = Lazy.force task in
       f task

let cancel_pending t scope tasks =
  Trace.with_ ~scope "Cancel pending" @@ fun _ ->
  tasks
  |> iter_lazy @@ fun task ->
     if not (Task.has_result task) then Task.try_cancel t task

let batched_run_or_cancel t name ?(batch_size = 32) ?on_progress apifns =
  (* default batch_size= 32 = 2*Dom0 vCPUs *)
  let total = List.length apifns in
  Trace.with_ (name ^ "/batched")
    ~attrs:[("total", `Int total); ("batch_size", `Int batch_size)]
  @@ fun scope ->
  let taskref_to_task = Hashtbl.create total in

  let launch_task i f =
    Trace.with_ ~scope "launch task" ~attrs:[("i", `Int i)] @@ fun _scope ->
    let task = f t in
    Hashtbl.replace taskref_to_task (Task.task task) task ;
    task
  in

  let all_tasks = apifns |> List.mapi (fun i f -> lazy (launch_task i f)) in

  let finally () =
    cancel_pending t scope all_tasks
    (* FIXME: this seems to always raise an exception, did XAPI already destroy
       these?
    Trace.with_ ~scope "destroy tasks" @@ fun _ ->
    all_tasks |> iter_lazy Task.(destroy t)*)
  in

  Fun.protect ~finally @@ fun () ->
  (* launches a new task, every time an element from the sequence is retrieved *)
  let next =
    all_tasks |> List.to_seq |> Seq.map Lazy.force |> Seq.to_dispenser
  in

  let callback completed task =
    let () =
      match Hashtbl.find_opt taskref_to_task task with
      | None ->
          (* unknown task, shouldn't happen *)
          ()
      | Some task ->
          let outcome = Task.result t task in
          Scope.add_event scope (fun () ->
              let launched = Hashtbl.length taskref_to_task in
              Opentelemetry.Event.make
                ~attrs:
                  [
                    ("completed", `Int completed)
                  ; ("success", `Bool (Result.is_ok outcome))
                  ; ("live", `Int (launched - completed))
                  ; ("remaining", `Int (total - launched))
                  ]
                "subtask.completed"
          ) ;

          if Result.is_error outcome then cancel_pending t scope all_tasks
    in

    (* we've completed one, replace it with a new one, if any,
       that way we always have at most batch_size tasks running
     *)
    match next () with
    | None ->
        []
    | Some task ->
        [task.task]
  in

  (* Start batch_size tasks *)
  let tasks =
    Trace.with_ ~scope "launch initial" ~attrs:[("batch_size", `Int batch_size)]
    @@ fun _ -> next |> Seq.of_dispenser |> Seq.take batch_size |> List.of_seq
  in

  run_with_progress t name scope ?on_progress ~callback ~total tasks ;

  Trace.with_ ~scope "retrieve task results" @@ fun _ ->
  let results =
    all_tasks
    |> List.map (fun task ->
        if Lazy.is_val task then
          Task.result t (Lazy.force task)
        else
          Error (Failure "Task group cancelled")
    )
  in
  if List.exists Result.is_error results then
    Scope.set_status scope
    @@ Span_status.make ~code:Status_code_error ~message:"task(s) failed" ;
  results

let rec wait_no_active_tasks ?on_progress t ~host =
  let tasks =
    call t @@ Client.Client.Task.get_all_records
    |> List.filter_map @@ fun (taskref, task) ->
       if
         task.API.task_resident_on = host
         && (task.API.task_status = `pending
            || task.API.task_status = `cancelling
            )
       then
         Some
           ( Task.v task.API.task_name_label ignore (fun _ _ -> ()) @@ fun () ->
             taskref
           )
       else
         None
  in
  match tasks with
  | [] ->
      ()
  | _ ->
      (* we can't efficiently wait for `canceling to become `canceled.
         Avoid busy loop
       *)
      Thread.delay 0.1 ;
      let () =
        Trace.with_ __FUNCTION__ @@ fun scope ->
        run_with_progress t "wait_no_active_tasks" scope ?on_progress
          ~total:(List.length tasks)
          ~callback:(fun _ _ -> [])
          tasks
      in
      (* more tasks could've been created meanwhile *)
      (wait_no_active_tasks [@tailcall]) ?on_progress t ~host
