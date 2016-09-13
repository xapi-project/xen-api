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
(**
 * @group Main Loop and Start-up
*)

open Stdext.Threadext

module D=Debug.Make(struct let name="startup" end)
open D

type flag = OnlyMaster | OnlySlave | NoExnRaising | OnThread

let thread_exn_wrapper thread_name f =
  begin try
      f ();
    with exn ->
      warn "thread [%s] dying on exception: %s" thread_name (Printexc.to_string exn);
      raise exn
  end;
  warn "thread [%s] died" thread_name;
  ()

(* run all list of tasks sequentially. every function is wrapped in a try with handler with
 * the possibility to specify to run only on master or slave, and that the exception should
 * not be raised *)
let run ~__context tasks =
  let task_id = Context.get_task_id __context in
  let dummy_task = Ref.is_dummy task_id in

  let get_flags_of_list flags =
    let only_master = ref false and only_slave = ref false
    and exnraise = ref true and onthread = ref false in
    List.iter (fun flag ->
        match flag with
        | OnlyMaster -> only_master := true
        | OnlySlave -> only_slave := true
        | NoExnRaising -> exnraise := false
        | OnThread -> onthread := true
      ) flags;
    !only_master, !only_slave, !exnraise, !onthread
  in

  (* get pool role status *)
  let is_master = Pool_role.is_master() in

  (* iterate tasks *)
  List.iter (fun (tsk_name, tsk_flags, tsk_fct) ->
      (* Wrap the function with a timer *)
      let tsk_fct () = Stats.time_this tsk_name tsk_fct in

      let only_master, only_slave, exnraise, onthread = get_flags_of_list tsk_flags in
      try
        if (only_master && is_master)
        || (only_slave && (not is_master))
        || ((not only_slave) && (not only_master)) then (
          if not dummy_task then begin
            Db.Task.remove_from_other_config ~__context ~self:task_id ~key:"startup_operation";
            Db.Task.add_to_other_config ~__context ~self:task_id ~key:"startup_operation" ~value:tsk_name
          end;
          if onthread then (
            debug "task [starting thread %s]" tsk_name;
            ignore (Thread.create (fun tsk_fct ->
                Server_helpers.exec_with_new_task ~subtask_of:(Context.get_task_id __context) tsk_name (fun __context ->
                    thread_exn_wrapper tsk_name tsk_fct)) tsk_fct)
          ) else (
            debug "task [%s]" tsk_name;
            Server_helpers.exec_with_new_task tsk_name ~subtask_of:(Context.get_task_id __context) (fun __context -> tsk_fct ())
          )
        )
      with exn ->
        warn "task [%s] exception: %s" tsk_name (Printexc.to_string exn);
        if exnraise then
          raise exn
    ) tasks

let run ~__context tasks = Stats.time_this "overall xapi startup" (fun () -> run ~__context tasks)
