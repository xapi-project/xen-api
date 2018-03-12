module D=Debug.Make(struct let name="xapi" end)
open D

let estimate_evacuate_timeout ~__context ~host =
  let mref = Db.Host.get_metrics ~__context ~self:host in
  let metrics = Db.Host_metrics.get_record ~__context ~self:mref in
  let memory_used = Int64.sub metrics.API.host_metrics_memory_total metrics.API.host_metrics_memory_free in
  (* Conservative estimation based on 1000Mbps link, and the memory usage of
     Dom0 (which is not going to be transferred) is an intentional surplus *)
  let t = ((Int64.to_float memory_used) *. 8. /. (1000. *. 1024. *. 1024.)) in
  max 240. t

(* Returns a tuple of lists: The first containing the control domains, and the second containing the regular VMs *)
let get_resident_vms ~__context ~self =
  let my_resident_vms = Db.Host.get_resident_VMs ~__context ~self in
  List.partition (fun vm -> Db.VM.get_is_control_domain ~__context ~self:vm) my_resident_vms

let ensure_no_vms ~__context ~rpc ~session_id ~evacuate_timeout =
  let open Client in

  let is_running vm =
    Db.VM.get_power_state ~__context ~self:vm = `Running
  in

  let host = Helpers.get_localhost ~__context in
  let self_managed_poweroff vm =
    let result = Db.VM.get_other_config ~__context ~self:vm
                 |> List.mem_assoc "auto_poweroff" in
    if result then
      debug "Skip running VM %s: has self-managed poweroff" (Db.VM.get_name_label ~__context ~self:vm);
    result
  in
  let get_running_domains () =
    get_resident_vms ~__context ~self:host |> snd
    |> List.filter (fun vm -> is_running vm && not (self_managed_poweroff vm))
  in

  let cancel_vm_tasks self =
    Db.VM.get_current_operations ~__context ~self
    |> List.rev_map fst
    |> List.rev_map Ref.of_string
    |> List.iter (fun (task:[`task] Ref.t) ->
        let name = Db.VM.get_name_label ~__context ~self in
        debug "Canceling operation on VM %s" name;
        log_and_ignore_exn (fun () -> Client.Task.cancel ~rpc ~session_id ~task))
  in

  let evacuate () =
    TaskHelper.exn_if_cancelling ~__context; (* First check if _we_ have been cancelled *)
    info "Requesting evacuation of host";
    let timeout = if evacuate_timeout > 0. then evacuate_timeout
      else estimate_evacuate_timeout ~__context ~host in
    let tasks = [ Client.Async.Host.evacuate ~rpc ~session_id ~host ] in
    if not (Tasks.with_tasks_destroy ~rpc ~session_id ~timeout ~tasks) then begin
      get_running_domains ()
      |> List.iter cancel_vm_tasks
    end
  in

  let clean_shutdown vms =
    TaskHelper.exn_if_cancelling ~__context; (* First check if _we_ have been cancelled *)
    let tasks =
      vms
      |> List.filter (fun vm ->
          List.mem `clean_shutdown (Client.VM.get_allowed_operations ~rpc ~session_id ~self:vm))
      |> List.map (fun vm ->
          let name_label = Client.VM.get_name_label ~rpc ~session_id ~self:vm in
          debug "Requesting clean shutdown of VM: %s" name_label;
          Client.Async.VM.clean_shutdown ~rpc ~session_id ~vm) in
    Tasks.with_tasks_destroy ~rpc ~session_id ~timeout:60. ~tasks |> ignore
  in

  let hard_shutdown vms =
    TaskHelper.exn_if_cancelling ~__context; (* First check if _we_ have been cancelled *)
    let tasks =
      vms
      |> List.map (fun vm ->
          let name_label = Client.VM.get_name_label ~rpc ~session_id ~self:vm in
          debug "Requesting hard shutdown of VM: %s" name_label;
          Client.Async.VM.hard_shutdown ~rpc ~session_id ~vm) in
    (* no timeout: we need the VMs to be off *)
    Tasks.wait_for_all ~rpc ~session_id ~tasks;
    vms
    |> List.filter is_running
    |> List.iter (fun vm ->
        let name_label = Client.VM.get_name_label ~rpc ~session_id ~self:vm in
        info "Failure performing hard shutdown of VM: %s" name_label)
  in

  let shutdown vms =
    log_and_ignore_exn (fun () -> clean_shutdown vms);
    (* We can unplug the PBD if a VM is suspended or halted, but not if
      * it is running or paused, i.e. "live" *)
    vms
    |> List.filter (fun self -> Xapi_vm_lifecycle.is_live ~__context ~self)
    |> hard_shutdown
  in

  log_and_ignore_exn (fun () ->
      Client.Host.get_vms_which_prevent_evacuation ~rpc ~session_id ~self:host
      |> Xapi_stdext_std.Listext.List.filter_map (fun (vm, _) ->
          if self_managed_poweroff vm then None
          else Some vm)
      |> shutdown;

      evacuate ());

  log_and_ignore_exn (fun () -> get_running_domains () |> shutdown)

let ensure_no_vms ~__context ~evacuate_timeout =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      ensure_no_vms ~__context ~rpc ~session_id ~evacuate_timeout)

