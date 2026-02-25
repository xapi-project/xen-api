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

open Client.Client
open Quicktest_trace
open Quicktest_trace_api
open Quicktest_trace_rpc

(** [points_between var_min var_max] generates a sequence of values between
    [[var_min, var_max]], where [var_max] is not necessarily a power of 2.
    The points inbetween are [var_min * 2**i]
    *)
let points_between var_min var_max =
  Seq.append
    (Seq.unfold
       (fun i ->
         if i < var_max then
           Some (i, Int64.shift_left i 1)
         else
           None
       )
       var_min
    )
    (Seq.return var_max)

let localhost_free_pages scope =
  Xenctrl.with_intf @@ fun xc ->
  let host_info = Xenctrl.physinfo xc in
  let pages = host_info.Xenctrl.free_pages |> Int64.of_nativeint in
  Scope.add_metrics scope (fun () ->
      Opentelemetry.Metrics.(
        gauge ~name:"host_free_pages" [int (Int64.to_int pages)]
      )
  ) ;
  pages

let rec stable_localhost_free_pages scope t ~host =
  let rec loop scope delay =
    let () =
      Trace.with_ ~force_new_trace_id:true "wait_no_active_tasks" @@ fun _ ->
      Api.wait_no_active_tasks t ~host
    in
    let v0 = localhost_free_pages scope in
    if delay > Float.epsilon then
      (* there may be some pending tasks in Xen, wait for them to finish *)
      Thread.delay delay ;
    let v1 = localhost_free_pages scope in
    if v1 <> v0 then begin
      let () =
        Scope.add_event scope @@ fun () ->
        Opentelemetry.Event.make
          ~attrs:
            [
              ("free_pages0", `Int (Int64.to_int v0))
            ; ("free_pages1", `Int (Int64.to_int v1))
            ; ("delay", `Float delay)
            ]
          "Free pages is not stable"
      in
      let delay =
        if delay > Float.epsilon then
          delay *. 2.
        else
          0.005
      in
      loop scope (delay *. 2.)
    end else
      v1
  in
  Trace.with_ ~scope __FUNCTION__ @@ fun scope -> loop scope 0.

let with_measure_memory_pages scope t ~localhost f =
  Trace.with_ ~scope __FUNCTION__ @@ fun scope ->
  let pages0 = stable_localhost_free_pages scope t ~host:localhost in
  f () ;
  let pages1 = stable_localhost_free_pages scope t ~host:localhost in
  let delta = Int64.sub pages0 pages1 in
  Scope.add_event scope (fun () ->
      Opentelemetry.Event.make
        ~attrs:
          [
            ("pages0", `Int (Int64.to_int pages0))
          ; ("pages1", `Int (Int64.to_int pages1))
          ; ("delta", `Int (Int64.to_int delta))
          ]
        "memory usage delta"
  ) ;
  delta

let check_tasks tasks =
  tasks |> List.map @@ function Ok x -> x | Error exn -> raise exn

let prefix = "temp_quicktest"

module NameMap = Map.Make (String)

let ensure_vm_clones t ~vm n purpose =
  Trace.with_ __FUNCTION__ ~attrs:[("purpose", `String purpose); ("n", `Int n)]
  @@ fun _scope ->
  let names =
    List.init n (fun i -> Printf.sprintf "%s-%s-%03d" prefix purpose i)
  in
  (* we might want to look up hundreds of VMs, faster retrieve all and filter
     locally *)
  let all_vms =
    call t @@ VM.get_all_records
    |> List.to_seq
    |> Seq.map (fun (vmref, vm) -> (vm.API.vM_name_label, (vmref, vm)))
    |> NameMap.of_seq
  in
  let existing_vms, tasks =
    names
    |> List.partition_map @@ fun label ->
       match NameMap.find_opt label all_vms with
       | Some found ->
           Either.left found
       | None ->
           Either.right (Api.VM.Async.clone ~vm ~new_name:label)
  in

  let new_vms =
    Api.batched_run_or_cancel t "Cloning new VM(s)" tasks |> check_tasks
  in

  (* May have been used by a previous test: hard shutdown.
     We cleanup at the beginning of a test, not the end,
     because we want to make 'pause-on-fail' possible.
     (e.g. it may take a while to clone and boot hundreds of VMs to trigger a
     test, so we shouldn't throw that all away on an error, to make it easier
     to reproduce and debug)
   *)
  let tasks =
    existing_vms
    |> List.filter_map (fun (vmref, vm) ->
        if vm.API.vM_power_state <> `Halted then
          Some (Api.VM.Async.hard_shutdown ~vm:vmref)
        else
          None
    )
  in
  let (_ : _ list) =
    Api.batched_run_or_cancel t "Hard shutdown existing VM(s)" tasks
    |> check_tasks
  in
  List.concat [existing_vms |> List.map fst; new_vms]

let div_round_up a b = Int64.(div (add a @@ pred b) b)

let ignore_list (_ : _ list) = ()

let pagesize () = Int64.shift_left (Xenctrl.pages_to_kib 1L) 10

let lifecycle_attrs _t items = [("n", `Int (List.length items))]

let start_vms_parallel t host_vms =
  Trace.with_ __FUNCTION__ ~attrs:(lifecycle_attrs t host_vms) @@ fun _ ->
  let tasks =
    host_vms
    |> List.map @@ fun (host, vm) ->
       Api.VM.Async.start_on ~host ~vm ~start_paused:true ~force:false
  in
  Api.batched_run_or_cancel t "Start VM(s)/parallel" tasks
  |> check_tasks
  |> ignore_list

let hard_reboot_vms t vms =
  Trace.with_ __FUNCTION__ ~attrs:(lifecycle_attrs t vms) @@ fun _ ->
  let tasks = vms |> List.map @@ fun (_, vm) -> Api.VM.Async.hard_reboot ~vm in
  Api.batched_run_or_cancel t "Hard reboot VMs/parallel" tasks
  |> check_tasks
  |> ignore_list

let start_vm t ~host ~vm =
  (* invoke the async version so we can see the progress,
     starting big VMs can be slow
   *)
  Api.batched_run_or_cancel t "Start VM"
    [Api.VM.Async.start_on ~vm ~host ~force:false ~start_paused:true]
  |> check_tasks
  |> fun (_ : _ list) -> ()

let start_vms_seq t host_vms =
  Trace.with_ __FUNCTION__ ~attrs:(lifecycle_attrs t host_vms) @@ fun _ ->
  host_vms
  |> List.iter @@ fun (host, vm) ->
     if call t @@ VM.get_power_state ~self:vm = `Halted then
       start_vm t ~host ~vm

let start_vms t host_vms =
  Trace.with_ __FUNCTION__ @@ fun scope ->
  try start_vms_parallel t host_vms
  with Api_errors.Server_error _ as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Backtrace.is_important exn ;
    Scope.add_event scope (fun () ->
        Opentelemetry.Event.make "Parallel start failed"
    ) ;
    (* Try to start the remainig VMs sequentially *)
    start_vms_seq t host_vms ;
    Scope.add_event scope (fun () ->
        Opentelemetry.Event.make "Sequential start succeeded"
    ) ;
    (* raise the parallel failure *)
    Printexc.raise_with_backtrace exn bt

let workload_host' t ~host ~workload_vm =
  let vm = workload_vm in
  Trace.with_ __FUNCTION__ @@ fun scope ->
  let host_cpus = call t @@ Host.get_host_CPUs ~self:host |> List.length in
  let vcpus =
    (* can't use more vCPUs than the host has, and we support a maximum of 32
    for untrusted VMs *)
    min 32 host_cpus |> Int64.of_int
  in
  let free_mem = call t @@ Host.compute_free_memory ~host in
  Api.VM.call_set t VM.set_VCPUs_max ~self:vm ~value:vcpus ;
  Api.VM.call_set t VM.set_VCPUs_at_startup ~self:vm ~value:vcpus ;
  (* ensure that all host CPUs are busy with at least 1 vCPU.
     For simplicity we rounding up, so the last VM may actually overload the host *)
  let n = div_round_up (Int64.of_int host_cpus) vcpus |> Int64.to_int in
  let memory_min = Api.VM.call_get t VM.get_memory_static_min ~self:vm in
  let value =
    (* Try to use at least 10% of host free memory with our N workloads in
       total, or N*memory_min, whichever is higher. *)
    max memory_min (Int64.div (Int64.div free_mem 10L) (Int64.of_int n))
  in
  Api.VM.call_set t VM.set_memory ~self:vm ~value ;

  Scope.add_attrs scope (fun () ->
      [
        ("vcpus", `Int (Int64.to_int vcpus))
      ; ("memory_bytes", `Int (Int64.to_int memory_min))
      ; ("n", `Int n)
      ]
  ) ;
  ensure_vm_clones t ~vm n (Printf.sprintf "workload-%s" @@ Ref.string_of host)
  |> List.map @@ fun vm -> (host, vm)

let run_vms t host_vms =
  start_vms t host_vms ;
  let tasks =
    List.map
      (fun (_, vm) t ->
        Api.VM.task t "unpause" ignore vm @@ Async.VM.unpause ~vm
      )
      host_vms
  in
  Api.batched_run_or_cancel t "unpause" tasks |> check_tasks |> ignore_list

let workload t ~host ~workload_vm =
  workload_host' t ~host ~workload_vm |> run_vms t

let workload_pool t ~workload_vm =
  let hosts = call t @@ Host.get_all in
  hosts
  |> List.concat_map (fun host -> workload_host' t ~host ~workload_vm)
  |> run_vms t

let shutdown_vms t = function
  | [] ->
      ()
  | vms ->
      let tasks = vms |> List.map @@ fun vm -> Api.VM.Async.hard_shutdown ~vm in
      Api.batched_run_or_cancel t "hard_shutdown VM(s)" tasks
      |> check_tasks
      |> ignore_list

let fill_mem_pow2' ?total t ~host ~vm =
  let memory_min = call t @@ VM.get_memory_static_min ~self:vm in
  let host_free_mem = call t @@ Host.compute_free_memory ~host in
  let total = Option.value total ~default:host_free_mem in
  Trace.with_ __FUNCTION__
    ~attrs:
      [
        ("vm_memory_min", `Int (Int64.to_int memory_min))
      ; ("host_free_bytes", `Int (Int64.to_int host_free_mem))
      ; ("total", `Int (Int64.to_int total))
      ]
  @@ fun scope ->
  let sizes =
    Trace.with_ ~scope "compute and set VM sizes" @@ fun _scope ->
    Seq.unfold
      (fun total ->
        let half = Int64.div total 2L in
        let next =
          if half < memory_min then
            total
          else
            half
        in
        if next <= 0L then
          None
        else begin
          let value =
            Api.VM.with_call t "maximise_memory" vm
            @@ VM.maximise_memory ~self:vm ~approximate:false ~total:next
          in
          if value < memory_min then
            None
          else begin
            Api.VM.call_set t VM.set_memory ~self:vm ~value ;
            let overhead =
              Api.VM.with_call t "compute_memory_overhead" vm
              @@ VM.compute_memory_overhead ~vm
            in
            Scope.add_event scope (fun () ->
                Opentelemetry.Event.make
                  ~attrs:
                    [
                      ("amount to fill (bytes)", `Int (Int64.to_int next))
                    ; ("VM memory (bytes)", `Int (Int64.to_int value))
                    ; ("computed overhead (bytes)", `Int (Int64.to_int overhead))
                    ]
                  "try to fill host memory"
            ) ;
            Some (value, Int64.sub total (Int64.add value overhead))
          end
        end
      )
      total
    |> List.of_seq
  in
  let vms = ensure_vm_clones t ~vm (List.length sizes) "fillmem" in
  let () =
    List.combine vms sizes
    |> List.iter @@ fun (self, value) ->
       Api.VM.call_set t VM.set_memory ~self ~value
  in
  vms |> List.map (fun vm -> (host, vm)) |> start_vms t ;

  Scope.add_event scope (fun () ->
      Opentelemetry.Event.make "Parallel start\n  succeeded"
  ) ;
  vms

let fill_mem_pow2 ?total t ~host ~vm =
  Trace.with_ __FUNCTION__ @@ fun scope ->
  let vms = fill_mem_pow2' ?total t ~host ~vm in
  Trace.with_ ~scope "Shutdown VMs on success" @@ fun _ -> shutdown_vms t vms

let maximise_memory t ~vm ~total =
  let value =
    Api.VM.with_call t "maximise_memory" vm
    @@ VM.maximise_memory ~self:vm ~approximate:false ~total
  in
  (* XS8+ bug workaround: migration double counts overhead *)
  Api.VM.call_set t VM.set_memory ~self:vm ~value ;
  let overhead =
    Api.VM.with_call t "compute_memory_overhead" vm
    @@ VM.compute_memory_overhead ~vm
  in
  let total = Int64.sub total overhead in
  let value =
    Api.VM.with_call t "maximise_memory" vm
    @@ VM.maximise_memory ~self:vm ~approximate:false ~total
  in
  value

let fill_mem_n ?(workaround_migration = false) ?total t ~host ~vm ~n =
  Trace.with_ __FUNCTION__ @@ fun scope ->
  assert (n > 0) ;
  let host_free_mem = call t @@ Host.compute_free_memory ~host in
  let total = Option.value total ~default:host_free_mem in
  let value =
    (*  rounded down, will fill remainder below in last_value *)
    let total = Int64.div total (Int64.of_int n) in
    maximise_memory t ~vm ~total
  in

  let last_value =
    Api.VM.call_set t VM.set_memory ~self:vm ~value ;
    let overhead =
      Api.VM.with_call t "compute_memory_overhead" vm
      @@ VM.compute_memory_overhead ~vm
    in
    let overhead =
      if workaround_migration then
        Int64.mul 2L overhead
      else
        overhead
      (* XS8+ bug: double counts overhead for migration *)
    in
    (* division may not be exact, fill remainder *)
    let total =
      Int64.(sub total @@ mul (add value overhead) @@ Int64.of_int @@ (n - 1))
    in
    maximise_memory t ~vm ~total
  in
  let sizes =
    List.init n @@ fun i ->
    if i = n - 1 then
      last_value
    else
      value
  in
  let vms =
    ensure_vm_clones t ~vm (List.length sizes) (Printf.sprintf "fillmem-%d" n)
  in
  let full_sizes =
    List.combine vms sizes
    |> List.map @@ fun (self, value) ->
       Api.VM.call_set t VM.set_memory ~self ~value ;
       let overhead =
         Api.VM.with_call t "compute_memory_overhead" vm
         @@ VM.compute_memory_overhead ~vm:self
       in
       Int64.(add value overhead)
  in
  let sum = List.fold_left Int64.add 0L sizes in
  let sum_full_sizes = List.fold_left Int64.add 0L full_sizes in
  Scope.add_event scope (fun () ->
      Opentelemetry.Event.make "fill_mem_n_sizes"
        ~attrs:
          [
            ("host_free_memory_bytes", `Int (Int64.to_int host_free_mem))
          ; ("total", `Int (Int64.to_int total))
          ; ("sum", `Int (Int64.to_int sum))
          ; ("n", `Int n)
          ; ("vm_memory_bytes", `Int (Int64.to_int value))
          ; ("last_vm_memory_bytes", `Int (Int64.to_int last_value))
          ; ("sum", `Int (Int64.to_int sum))
          ; ("sum_full_sizes", `Int (Int64.to_int sum_full_sizes))
          ; ( "host_free_remaining_bytes"
            , `Int Int64.(sub host_free_mem sum_full_sizes |> to_int)
            )
          ]
  ) ;

  let host_vms = vms |> List.map (fun vm -> (host, vm)) in
  start_vms t host_vms ;
  let host_free_mem = call t @@ Host.compute_free_memory ~host in
  let actual_free_mem = Int64.mul (pagesize ()) @@ localhost_free_pages scope in
  Scope.add_event scope (fun () ->
      Opentelemetry.Event.make "afterfill_mem"
        ~attrs:
          [
            ("host_free_mem", `Int (Int64.to_int host_free_mem))
          ; ("actual_free_mem", `Int (Int64.to_int actual_free_mem))
          ]
  ) ;
  host_vms

let cleanup rpc session_id () =
  Trace.with_ __FUNCTION__ @@ fun _ ->
  let t = {rpc= RPC.wrap rpc; session_id} in
  let vms = call t @@ VM.get_all_records in
  let vms =
    vms
    |> List.filter (fun (_, vm) ->
        String.starts_with ~prefix vm.API.vM_name_label
    )
  in
  let not_halted =
    vms
    |> List.filter_map @@ fun (self, vm) ->
       if vm.API.vM_power_state <> `Halted then
         Some self
       else
         None
  in
  shutdown_vms t not_halted ;
  vms
  |> List.iter @@ fun (self, _) ->
     (* TODO: vm-uninstall instead? but it is slow, and we have no disks *)
     Api.VM.with_call t "destroy" self @@ VM.destroy ~self
