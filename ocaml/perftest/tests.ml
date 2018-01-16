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
(* Tests *)

open Client
open Xapi_stdext_std
open Xapi_stdext_threads.Threadext
open Xapi_stdext_pervasives.Pervasiveext
open Testtypes
open Perfdebug

type test = {
  run: bool;
  key: string;
  testname : string;
  func : (Rpc.call -> Rpc.response) -> API.ref_session -> test -> result list;
}

let time f =
  let now = Unix.gettimeofday () in
  f ();
  let elapsed = Unix.gettimeofday () -. now in
  elapsed

let subtest_string key tag =
  if tag = ""
  then key
  else Printf.sprintf "%s (%s)" key tag

let startall rpc session_id test =
  let vms = Client.VM.get_all_records rpc session_id in
  let tags = List.map (fun (vm,vmr) -> vmr.API.vM_tags) vms in
  let tags = Listext.List.setify (List.flatten tags) in
  List.map
    (fun tag ->
       debug "Starting VMs with tag: %s" tag;
       let vms = List.filter (fun (vm,vmr) -> List.mem tag vmr.API.vM_tags) vms in
       let vms = List.sort (fun (vm1,vmr1) (vm2,vmr2) -> compare vmr1.API.vM_affinity vmr2.API.vM_affinity) vms in
       let vms_names_uuids = List.map (fun (vm,vmr) -> (vm,vmr.API.vM_name_label, vmr.API.vM_uuid)) vms in
       let times = List.map
           (fun (vm,name_label,uuid) ->
              debug "Starting VM uuid '%s' (%s)" uuid name_label;
              let result = time (fun () -> Client.VM.start rpc session_id vm false false) in
              debug "Elapsed time: %f" result;
              result)
           vms_names_uuids in
       {
         resultname=test.testname;
         subtest=subtest_string test.key tag;
         xenrtresult=(List.fold_left (+.) 0.0 times);
         rawresult=StartTest times
       })
    tags

let parallel_with_vms async_op opname n vms rpc session_id test subtest_name =
  (* Not starting in affinity order *)
  let vms_names_uuids = List.map (fun (vm,vmr) -> (vm,vmr.API.vM_name_label, vmr.API.vM_uuid)) vms in

  (* Manage a set of active tasks using the event system. This could be factored out into a more generic
     service if/when necessary *)

  (* Start 'n' at a time *)
  let active_tasks = ref [] in
  let vms_to_start = ref vms_names_uuids in
  let vm_to_start_time = Hashtbl.create 10 in
  let tasks_to_vm = Hashtbl.create 10 in
  let m = Mutex.create () in
  let c = Condition.create () in

  let results = ref [] in

  (* Take a set of tasks which have finished, update structures and return true if there are no more active tasks
     left. *)
  let process_finished_tasks finished =
    let to_delete = ref [] in
    let finished = Mutex.execute m
        (fun () ->
           List.iter
             (fun task ->
                if List.mem task !active_tasks then begin
                  if not(Hashtbl.mem tasks_to_vm task)
                  then debug ~out:stderr "Ignoring completed task which doesn't correspond to a VM %s" opname
                  else begin
                    let uuid = Hashtbl.find tasks_to_vm task in
                    let started = Hashtbl.find vm_to_start_time uuid in
                    let time_taken = Unix.gettimeofday () -. started in
                    results := time_taken :: !results;
                    debug "%sing VM uuid '%s'" opname uuid;
                    debug "Elapsed time: %f" time_taken;
                    Hashtbl.remove vm_to_start_time uuid;
                    Hashtbl.remove tasks_to_vm task;
                  end;
                  active_tasks := List.filter (fun x -> x <> task) !active_tasks;
                  Condition.signal c;
                  to_delete := task :: !to_delete
                end
             ) finished;
           !active_tasks = [] (* true if no active tasks left *)
        ) in
    List.iter (fun task -> Client.Task.destroy ~rpc ~session_id ~self:task) !to_delete;
    finished in

  (* Run this in a thread body to create a thread which will process each task completion and then terminate when all the
     tasks have finished. *)
  let check_active_tasks () =
    let classes = [ "task" ] in
    finally
      (fun () ->
         let finished = ref false in
         while not(!finished) do
           Client.Event.register ~rpc ~session_id ~classes;
           try
             (* Need to check once after registering to avoid a race *)
             let finished_tasks =
               List.filter
                 (fun task -> Client.Task.get_status ~rpc ~session_id ~self:task <> `pending)
                 (Mutex.execute m (fun () -> !active_tasks)) in
             finished := process_finished_tasks finished_tasks;

             while not(!finished) do
               (* debug ~out:stderr "Polling for events (%d active tasks)" (Mutex.execute m (fun () -> List.length !active_tasks)); *)
               let events = Event_types.events_of_rpc (Client.Event.next ~rpc ~session_id) in
               let events = List.map Event_helper.record_of_event events in
               let finished_tasks =
                 List.concat
                   (List.map
                      (function
                        | Event_helper.Task (t, Some t_rec) -> if t_rec.API.task_status <> `pending || t_rec.API.task_current_operations <> [] then [ t ] else [ ]
                        | Event_helper.Task (t, None) -> [ t ]
                        | _ -> []) events) in
               finished := process_finished_tasks finished_tasks;
             done
           with Api_errors.Server_error(code, _) when code = Api_errors.events_lost ->
             debug ~out:stderr "Caught EVENTS_LOST; reregistering";
             Client.Event.unregister ~rpc ~session_id ~classes
         done)
      (fun () -> Client.Event.unregister ~rpc ~session_id ~classes) in


  let control_task = Client.Task.create rpc session_id ("Parallel VM "^opname^" test") "" in
  active_tasks := [ control_task ];
  let thread = Thread.create check_active_tasks () in

  while !vms_to_start <> [] do
    let start_one () =
      let vm, name, uuid = List.hd !vms_to_start in
      vms_to_start := List.tl !vms_to_start;
      Mutex.execute m
        (fun () ->
           let task = async_op ~rpc ~session_id ~vm in
           debug ~out:stderr "Issued VM %s for '%s'" opname uuid;
           Hashtbl.add tasks_to_vm task uuid;
           Hashtbl.add vm_to_start_time uuid (Unix.gettimeofday ());
           active_tasks := task :: !active_tasks;
        ) in
    (* Only start at most 'n' at once. Note that the active_task list includes a master control task *)
    Mutex.execute m (fun () -> while List.length !active_tasks > n do Condition.wait c m done);
    start_one ()
  done;
  Client.Task.cancel ~rpc ~session_id ~task:control_task;

  debug ~out:stderr "Finished %sing VMs" opname;
  Thread.join thread;

  {resultname=test.testname; subtest=subtest_name; xenrtresult=(List.fold_left (+.) 0.0 !results); rawresult=StartTest !results}

(** @param n the maximum number of concurrent invocations of async_op *)
let parallel async_op opname n rpc session_id test =
  let vms = Client.VM.get_all_records rpc session_id in
  let tags = List.map (fun (vm,vmr) -> vmr.API.vM_tags) vms in
  let tags = Listext.List.setify (List.flatten tags) in
  Printf.printf "Tags are [%s]\n%!" (String.concat "; " tags);
  List.map (fun tag ->
      let vms = List.filter (fun (vm,vmr) -> List.mem tag vmr.API.vM_tags) vms in
      Printf.printf "%sing %d VMs with tag: %s\n%!" opname (List.length vms) tag;
      parallel_with_vms async_op opname n vms rpc session_id test (subtest_string test.key tag)
    ) tags

let parallel_startall = parallel (Client.Async.VM.start ~start_paused:false ~force:false) "start"
let parallel_stopall = parallel Client.Async.VM.hard_shutdown "stop"

let stopall rpc session_id test =
  let vms = Client.VM.get_all_records rpc session_id in
  let tags = List.map (fun (vm,vmr) -> vmr.API.vM_tags) vms in
  let tags = Listext.List.setify (List.flatten tags) in
  List.map (fun tag ->
      debug "Starting VMs with tag: %s" tag;
      let vms = List.filter (fun (vm,vmr) -> List.mem tag vmr.API.vM_tags) vms in
      let vms = List.sort (fun (vm1,vmr1) (vm2,vmr2) -> compare vmr1.API.vM_affinity vmr2.API.vM_affinity) vms in
      let vms_names_uuids = List.map (fun (vm,vmr) -> (vm,vmr.API.vM_name_label, vmr.API.vM_uuid)) vms in
      let times = List.map
          (fun (vm,name_label,uuid) ->
             debug "Stopping VM uuid '%s' (%s)" uuid name_label;
             let result = time (fun () -> Client.VM.hard_shutdown rpc session_id vm) in
             debug "Elapsed time: %f" result;
             result) vms_names_uuids in
      {resultname=test.testname; subtest=subtest_string test.key tag; xenrtresult=(List.fold_left (+.) 0.0 times); rawresult=ShutdownTest times}
    ) tags

let clone num_clones rpc session_id test =
  Printf.printf "Doing clone test\n%!";
  let vms = Client.VM.get_all_records rpc session_id in
  let tags = List.map (fun (vm,vmr) -> vmr.API.vM_tags) vms in
  let tags = Listext.List.setify (List.flatten tags) in
  Printf.printf "Tags are [%s]\n%!" (String.concat "; " tags);
  List.flatten (List.map (fun tag ->
      let vms = List.filter (fun (vm,vmr) -> List.mem tag vmr.API.vM_tags) vms in
      Printf.printf "We've got %d VMs\n%!" (List.length vms);

      (* Start a thread to clone each one n times *)
      let body (vm, vmr, res, clone_refs) =
        let name_label = vmr.API.vM_name_label in
        Printf.printf "Performing %d clones of '%s' within thread...\n%!" num_clones name_label;
        for j=0 to num_clones-1 do
          let result = time (fun () ->
              let clone = Client.VM.clone ~rpc ~session_id ~vm ~new_name:"clone" in
              clone_refs := clone :: !clone_refs
            ) in
          Printf.printf "clone %d of '%s' finished: %f\n%!" j name_label result;
          res := result :: !res
        done
      in
      let threads_and_results = List.map (fun (vm,vmr) ->
          let res : float list ref = ref [] in
          let clones : API.ref_VM list ref = ref [] in
          let t = Thread.create body (vm, vmr, res, clones) in
          (t, (res, clones))
        ) vms in
      let (threads, times_and_clones) = List.split threads_and_results in
      let (times, clones) = List.split times_and_clones in
      Printf.printf "Waiting for threads to finish...\n%!";
      List.iter (fun t -> Thread.join t) threads;
      Printf.printf "Threads have finished\n%!";

      (* times is a list of (list of floats, each being the time to clone a VM), one per SR *)
      let times = List.map (fun x -> !x) times in
      Printf.printf "Times are: [%s]\n%!" (String.concat ", " (List.map (fun x -> Printf.sprintf "[%s]" (String.concat ", " (List.map (fun x -> Printf.sprintf "%f" x) x))) times));
      let clones = List.map (fun x -> !x) clones in

      (* Output the results for cloning each gold VM as a separate record *)
      let results = List.map
          (fun x -> {resultname=test.testname; subtest=subtest_string test.key tag; xenrtresult=(List.fold_left (+.) 0.0 (List.flatten times)); rawresult=CloneTest x})
          times
      in

      (* Best-effort clean-up *)
      ignore_exn
        (fun () ->
           Printf.printf "Cleaning up...\n%!";
           (* Create a thread to clean up each set of clones *)
           let threads = List.mapi
               (fun i clones ->
                  Thread.create (fun clones ->
                      List.iteri
                        (fun j clone ->
                           Printf.printf "Thread %d destroying VM %d...\n%!" i j;
                           let vbds = Client.VM.get_VBDs ~rpc ~session_id ~self:clone in
                           let vdis = List.map (fun vbd -> Client.VBD.get_VDI rpc session_id vbd) vbds in
                           List.iter (fun vdi -> Client.VDI.destroy ~rpc ~session_id ~self:vdi) vdis;
                           Client.VM.destroy ~rpc ~session_id ~self:clone
                        )
                        clones
                    ) clones
               ) clones in
           Printf.printf "Waiting for clean-up threads to finish...\n%!";
           List.iter (fun t -> Thread.join t) threads;
           Printf.printf "Clean-up threads have finished\n%!";
        );

      (* Finally, return the results *)
      results
    ) tags)

let recordssize rpc session_id test =
  let doxmlrpctest (subtestname,testfn) =
    testfn ();
    let res = (Int64.to_float !Http_client.last_content_length) in
    {resultname=test.testname;
     subtest=subtestname;
     xenrtresult=res;
     rawresult=SizeTest res}
  in
  List.map doxmlrpctest
    [("VM records", fun () -> ignore(Client.VM.get_all_records rpc session_id ));
     ("VBD records", fun () -> ignore(Client.VBD.get_all_records rpc session_id ));
     ("VIF records", fun () -> ignore(Client.VIF.get_all_records rpc session_id));
     ("VDI records", fun () -> ignore(Client.VDI.get_all_records rpc session_id));
     ("SR records", fun () -> ignore(Client.SR.get_all_records rpc session_id))]

let tests key = [
  {run=true;
   key=key;
   testname="clone";
   func=clone 200};
  {run=true;
   key=key;
   testname="startall";
   func=startall};
  {run=true;
   key=key;
   testname="recordssize";
   func=recordssize;};
  {run=true;
   key=key;
   testname="stopall";
   func=stopall;};
  {run=false;
   key=key;
   testname="parallel_startall";
   func=parallel_startall 10};
  {run=false;
   key=key;
   testname="parallel_stopall";
   func=parallel_stopall 10;};
]

let testnames =
  List.map (fun t -> t.testname) (tests "")

let runtestnames =
  List.map (fun t -> t.testname) (List.filter (fun t -> t.run) (tests ""))

let runone rpc session_id test =
  debug "Running test: %s" test.testname;
  let results = test.func rpc session_id test in
  debug "Finished: Results=[%s]" (String.concat "; " (List.map (fun result -> Printf.sprintf "subtest '%s': %f" result.subtest result.xenrtresult) results));
  results

let run rpc session_id key run_all iter =
  let tests =
    if run_all
    then tests key
    else List.filter (fun t -> t.run) (tests key)
  in
  let rec iter_tests n =
    if n = 1
    then tests
    else tests @ iter_tests (n-1)
  in
  List.fold_left (fun acc test -> (runone rpc session_id test) @ acc) [] (iter_tests iter)

