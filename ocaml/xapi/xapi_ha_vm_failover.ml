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

module D = Debug.Make (struct let name = "xapi_ha_vm_failover" end)

open D

type task_result = (Rpc.t, exn) result

(** [result_of_task ~__context task] returns the status of [task]
 * unless it is still pending. Exceptions are converted into [Error] *)
let result_of_task ~__context self : task_result option =
  try
    match Db.Task.get_status ~__context ~self with
    | `pending ->
        None
    | `success ->
        Some
          (Ok
             ( match Db.Task.get_result ~__context ~self with
             | "" ->
                 Rpc.Null
             | s ->
                 Xmlrpc.of_string s
             )
          )
    | `cancelled ->
        Some (Error (Failure "Cancelled"))
    | `cancelling ->
        Some (Error (Failure "Cancelling"))
    | `failure ->
        Some
          ( match Db.Task.get_error_info ~__context ~self with
          | [] ->
              Error (Failure "Unknown error")
          | code :: params ->
              Error (Api_errors.Server_error (code, params))
          )
  with e ->
    (* cannot fetch task status, maybe task got destroyed *)
    Backtrace.is_important e ; Some (Error e)

module TaskChains : sig
  (** the type of delayed task related actions *)
  type +'a t

  val ok : 'a -> ('a, exn) result t

  val fail : exn -> ('a, exn) result t

  val task : (unit -> API.ref_task) -> task_result t
  (**[task f] is an action that evaluates to the result of task [f ()] *)

  (** this module is meant to be opened, defines only an infix operator,
   * avoids polluting the namespace with other functions *)
  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [m >>= k] monadic bind: executes [k] with the result of [m] computation.
     * It is not guaranteed that [k] is executed as soon as the result of [m] is available.
     * *)
  end

  val parallel :
       __context:Context.t
    -> rpc:(Rpc.call -> Rpc.response)
    -> session_id:API.ref_session
    -> ('a * ('b, exn) result t) list
    -> ('a * ('b, exn) result) list
  (** [parallel ~__context lst] execute actions in [lst] in parallel.
   * Returns the results from [lst], in an unspecified order, hence each item in [lst] is a tuple
   * with arbitrary data (e.g. a VM ref) as first element, and the action as second:
   * this way the results will be associated with some meaningful data.
   *)
end = struct
  (** structured like a Freer Monad *)
  type +'a t =
    | Completed of 'a
        (** [Completed result] an action that has finished, sometimes refered to as 'pure' *)
    | Task of API.ref_task * (task_result -> 'a t)
        (** [Task(task, next)] a task, and a function to call when the task finishes (continuation).
     * This is an impure action. *)

  (* Constructors *)

  let return x = Completed x

  let ok x = Completed (Ok x)

  let fail e = Completed (Error e)

  let wrap f = try f () with e -> Backtrace.is_important e ; fail e

  let task f = wrap (fun () -> Task (f (), return))

  (* Operations *)

  module Infix = struct
    let rec ( >>= ) m k =
      match m with
      | Completed x ->
          k x
      | Task (task, next) ->
          (* similar reasoning as above, when we get the result we need to chain the computations,
             * refer to http://okmij.org/ftp/Computation/free-monad.html for a deeper theoretical explanation *)
          Task (task, fun x -> next x >>= k)
  end

  let eval ~__context = function
    | Completed _ as t ->
        t
    | Task (task, next) as t -> (
      match result_of_task ~__context task with
      | None ->
          t
      | Some result ->
          log_and_ignore_exn (fun () -> Xapi_task.destroy ~__context ~self:task) ;
          next result
    )

  let classify completed lst =
    List.fold_left
      (fun (completed, pending, tasks) -> function
        | k, Completed r ->
            ((k, r) :: completed, pending, tasks)
        | (_, Task (task, _)) as t ->
            (completed, t :: pending, task :: tasks)
      )
      (completed, [], []) lst

  let parallel ~__context ~rpc ~session_id lst =
    let rec loop completed lst =
      (* find any pending tasks we can wait for right now *)
      match classify completed lst with
      | completed, [], [] ->
          debug "All %d parallel actions have completed" (List.length completed) ;
          completed
      | completed, pending, tasks ->
          debug "Waiting for %d tasks" (List.length tasks) ;
          Tasks.wait_for_all ~rpc ~session_id ~tasks ;
          debug "Waited for %d tasks, evaluating next actions"
            (List.length tasks) ;
          pending
          |> List.rev_map (fun (k, v) -> (k, wrap (fun () -> eval ~__context v)))
          |> loop completed
    in
    debug "Starting to process %d parallel actions" (List.length lst) ;
    loop [] lst
end

(* Return a list of (ref, record) pairs for all VMs which are marked as always_run *)
let all_protected_vms ~__context =
  let vms = Db.VM.get_all_records ~__context in
  List.filter
    (fun (_, vm_rec) ->
      Helpers.vm_should_always_run vm_rec.API.vM_ha_always_run
        vm_rec.API.vM_ha_restart_priority
    )
    vms

(* Comparison function which can be used to sort a list of VM ref, record by order *)
let order_f (_, vm_rec) =
  let negative_high x = if x < 0L then Int64.max_int else x in
  negative_high vm_rec.API.vM_order

let ( $ ) x y = x y

(*****************************************************************************************************)
(* Planning code follows                                                                             *)

(* Compute the total memory required of a VM (Running or not) *)
let total_memory_of_vm ~__context policy snapshot =
  let main, shadow =
    Memory_check.vm_compute_start_memory ~__context ~policy snapshot
  in
  Int64.add main shadow

(** Return a VM -> Host plan for the Host.evacuate code. We assume the VMs are all agile. The returned plan may
    be incomplete if there was not enough memory. *)
let compute_evacuation_plan ~__context total_hosts remaining_hosts
    vms_and_snapshots =
  let hosts =
    List.map
      (fun host ->
        ( host
        , Memory_check.host_compute_free_memory_with_maximum_compression
            ~__context ~host None
        )
      )
      remaining_hosts
  in
  let vms =
    List.map
      (fun (vm, snapshot) ->
        let policy =
          match Helpers.check_domain_type snapshot.API.vM_domain_type with
          | `hvm | `pv ->
              Memory_check.Dynamic_min
          | `pv_in_pvh | `pvh ->
              Memory_check.Static_max
        in
        (vm, total_memory_of_vm ~__context policy snapshot)
      )
      vms_and_snapshots
  in
  let config =
    {Binpack.hosts; vms; placement= []; total_hosts; num_failures= 1}
  in
  Binpack.check_configuration config ;
  debug "Planning configuration for offline agile VMs = %s"
    (Binpack.string_of_configuration
       (fun x ->
         Printf.sprintf "%s (%s)" (Ref.short_string_of x)
           (Db.Host.get_hostname ~__context ~self:x)
       )
       (fun x ->
         Printf.sprintf "%s (%s)" (Ref.short_string_of x)
           (Db.VM.get_name_label ~__context ~self:x)
       )
       config
    ) ;
  debug "VMs to attempt to evacuate: [ %s ]"
    (String.concat "; "
       (List.map
          (fun (r, record) ->
            Printf.sprintf "%s (%s)" (Ref.short_string_of r)
              record.API.vM_name_label
          )
          vms_and_snapshots
       )
    ) ;
  let h = Binpack.choose_heuristic config in
  h.Binpack.get_specific_plan config (List.map fst vms_and_snapshots)

(** Passed to the planner to reason about other possible configurations, used to block operations which would
    destroy the HA VM restart plan. *)
type configuration_change = {
    old_vms_leaving: (API.ref_host * (API.ref_VM * API.vM_t)) list
        (** existing VMs which are leaving *)
  ; old_vms_arriving: (API.ref_host * (API.ref_VM * API.vM_t)) list
        (** existing VMs which are arriving *)
  ; hosts_to_disable: API.ref_host list  (** hosts to pretend to disable *)
  ; num_failures: int option  (** new number of failures to consider *)
  ; new_vms_to_protect: API.ref_VM list  (** new VMs to restart *)
}

let no_configuration_change =
  {
    old_vms_leaving= []
  ; old_vms_arriving= []
  ; hosts_to_disable= []
  ; num_failures= None
  ; new_vms_to_protect= []
  }

let string_of_configuration_change ~__context (x : configuration_change) =
  let string_of_host h =
    Printf.sprintf "%s (%s)" (Ref.short_string_of h)
      (Db.Host.get_name_label ~__context ~self:h)
  in
  Printf.sprintf
    "configuration_change = { old_vms_leaving = [ %s ]; new_vms_arriving = [ \
     %s ]; hosts_to_disable = [ %s ]; num_failures = %s; new_vms = [ %s ] }"
    (String.concat "; "
       (List.map
          (fun (h, (vm_ref, vm_t)) ->
            Printf.sprintf "%s %s (%s)" (string_of_host h)
              (Ref.short_string_of vm_ref)
              vm_t.API.vM_name_label
          )
          x.old_vms_leaving
       )
    )
    (String.concat "; "
       (List.map
          (fun (h, (vm_ref, vm_t)) ->
            Printf.sprintf "%s %s (%s)" (string_of_host h)
              (Ref.short_string_of vm_ref)
              vm_t.API.vM_name_label
          )
          x.old_vms_arriving
       )
    )
    (String.concat "; " (List.map string_of_host x.hosts_to_disable))
    (Option.fold ~none:"no change" ~some:string_of_int x.num_failures)
    (String.concat "; " (List.map Ref.short_string_of x.new_vms_to_protect))

(* Deterministic function which chooses a single host to 'pin' a non-agile VM to. Note we don't consider only live hosts:
   otherwise a non-agile VM may 'move' between several hosts which it can actually run on, which is not what we need for
   the planner. *)
let host_of_non_agile_vm ~__context all_hosts_and_snapshots_sorted (vm, snapshot)
    =
  match
    List.filter
      (fun (host, _) ->
        try
          Xapi_vm_helpers.assert_can_boot_here ~__context ~self:vm ~host
            ~snapshot ~do_memory_check:false ~do_cpuid_check:false () ;
          true
        with _ -> false
      )
      all_hosts_and_snapshots_sorted
  with
  | (host, host_snapshot) :: _ ->
      (* Multiple hosts are possible because "not agile" means "not restartable on every host". It is
         	 possible to unplug PBDs so that only a proper subset of hosts (not the singleton element) supports a VM. *)
      debug "Non-agile VM %s (%s) considered pinned to Host %s (%s)"
        (Ref.short_string_of vm) snapshot.API.vM_name_label
        (Ref.short_string_of host) host_snapshot.API.host_hostname ;
      [(vm, host)]
  | [] ->
      warn "No host could support protected xHA VM: %s (%s)"
        (Ref.short_string_of vm) snapshot.API.vM_name_label ;
      []

let get_live_set ~__context =
  let all_hosts = Db.Host.get_all_records ~__context in
  let live_hosts =
    List.filter
      (fun (_, r) ->
        r.API.host_enabled
        &&
        try Db.Host_metrics.get_live ~__context ~self:r.API.host_metrics
        with _ -> false
      )
      all_hosts
  in
  List.map (fun (rf, _) -> rf) live_hosts

(** Given the current number of host failures to consider (only useful for passing to the binpacker to influence its
    choice of heuristic), return an instantaneous VM restart plan which includes all protected offline VMs, and a
    planning configuration corresponding to the state of the world after the starts are complete, for use in further
    planning.
    Returns: (VM restart plan, new planning configuration, true if some protected non-agile VMs exist)
*)
let compute_restart_plan ~__context ~all_protected_vms ~live_set
    ?(change = no_configuration_change) num_failures =
  (* This function must be deterministic: for the same set of hosts and set of VMs it must produce the same output.
     	   We rely partially on the binpacker enforcing its own ordering over hosts and vms, so it's not critical for us
     	   to sort the result of Db.*.get_all calls generally. However the handling of non-agile VMs needs special care. *)

  (* We first must deal with protected but currently offline VMs: we need to simulate the start of these VMs before we can
     	   ask any questions about future host failures, since we need to know on which hosts these VMs will end up.
     	   Note this is only useful in the initial startup transient: assuming all protected VMs actually are restarted then
     	   this code will do nothing. *)

  (* Note further that we simulate the start of offline protected VMs *using this function* (ie by the background HA
     	   thread). If the user makes their own poor placement decisions via explicit VM.start/VM.start_on then the plan
     	   may evaporate. This is no different to (eg) the user migrating a VM and breaking the plan. *)

  (* Note further that we consider the amount of host memory free using the current VM configurations (thanks to the
     	   semantics of the Memory_check.host_compute_free_memory call) but *crucially* consider that VMs requiring a restart
     	   will use their new memory_static_max: so we always use a live 'VM.get_record' and not a 'last_booted_record' *)

  (* Allow the num_failures to be overriden *)
  let (num_failures : int) =
    Option.value ~default:num_failures change.num_failures
  in
  (* All the VMs to protect; these VMs may or may not be currently running anywhere: they will be offline when a host has
     	   failed and possibly initially during the enable-ha transient. *)
  let vms_to_ensure_running = all_protected_vms in
  (* Add in any extra VMs which aren't already protected *)
  let extra_vms =
    List.map
      (fun vm -> (vm, Db.VM.get_record ~__context ~self:vm))
      change.new_vms_to_protect
  in
  let vms_to_ensure_running = vms_to_ensure_running @ extra_vms in
  (* For each leaving VM unset the resident_on (so 'is_accounted_for' returns false) *)
  (* For each arriving VM set the resident_on again (so 'is_accounted_for' returns true) *)
  (* For each arriving VM make sure we use the new VM configuration (eg new memory size) *)
  (* NB host memory is adjusted later *)
  let vms_to_ensure_running =
    List.map
      (fun (vm_ref, vm_t) ->
        let leaving =
          List.filter (fun (_, (vm, _)) -> vm_ref = vm) change.old_vms_leaving
        in
        let leaving_host =
          List.map (fun (host, (vm, _)) -> (vm, host)) leaving
        in
        (* let leaving_snapshots = List.map snd leaving in *)
        let arriving =
          List.filter (fun (_, (vm, _)) -> vm_ref = vm) change.old_vms_arriving
        in
        let arriving_host =
          List.map (fun (host, (vm, _)) -> (vm, host)) arriving
        in
        let arriving_snapshots = List.map snd arriving in
        match
          ( List.mem_assoc vm_ref leaving_host
          , List.mem_assoc vm_ref arriving_host
          )
        with
        | _, true ->
            ( vm_ref
            , {
                (List.assoc vm_ref arriving_snapshots) with
                API.vM_resident_on= List.assoc vm_ref arriving_host
              }
            )
        | true, false ->
            (vm_ref, {vm_t with API.vM_resident_on= Ref.null})
        | _, _ ->
            (vm_ref, vm_t)
      )
      vms_to_ensure_running
  in
  let all_hosts_and_snapshots = Db.Host.get_all_records ~__context in
  let total_hosts = List.length all_hosts_and_snapshots in
  (* Any deterministic ordering is fine here: *)
  let all_hosts_and_snapshots =
    List.sort
      (fun (_, a) (_, b) -> compare a.API.host_uuid b.API.host_uuid)
      all_hosts_and_snapshots
  in
  let is_alive (rf, r) =
    (* We exclude: (i) online disabled hosts; (ii) online proposed disabled hosts; and (iii) offline hosts *)
    true
    && r.API.host_enabled
    && (not (List.mem rf change.hosts_to_disable))
    && ( try Db.Host_metrics.get_live ~__context ~self:r.API.host_metrics
         with _ -> false
       )
    && List.mem rf live_set
  in
  let live_hosts_and_snapshots, _dead_hosts_and_snapshots =
    List.partition is_alive all_hosts_and_snapshots
  in
  let live_hosts =
    List.map fst live_hosts_and_snapshots
    (* and dead_hosts = List.map fst dead_hosts_and_snapshots *)
  in
  (* Any deterministic ordering is fine here: *)
  let vms_to_ensure_running =
    List.sort
      (fun (_, a) (_, b) -> compare a.API.vM_uuid b.API.vM_uuid)
      vms_to_ensure_running
  in
  let agile_vms, not_agile_vms =
    Agility.partition_vm_ps_by_agile ~__context vms_to_ensure_running
  in
  (* If a VM is marked as resident on a live_host then it will already be accounted for in the host's current free memory. *)
  let vm_accounted_to_host vm =
    let vm_t = List.assoc vm vms_to_ensure_running in
    if List.mem vm_t.API.vM_resident_on live_hosts then
      Some vm_t.API.vM_resident_on
    else
      let scheduled =
        Db.VM.get_scheduled_to_be_resident_on ~__context ~self:vm
      in
      if List.mem scheduled live_hosts then
        Some scheduled
      else
        None
  in
  let string_of_vm vm =
    Printf.sprintf "%s (%s)" (Ref.short_string_of vm)
      (List.assoc vm vms_to_ensure_running).API.vM_name_label
  in
  let string_of_host host =
    let name = (List.assoc host all_hosts_and_snapshots).API.host_name_label in
    Printf.sprintf "%s (%s)" (Ref.short_string_of host) name
  in
  let string_of_plan p =
    String.concat "; "
      (List.map
         (fun (vm, host) ->
           Printf.sprintf "%s -> %s" (string_of_vm vm) (string_of_host host)
         )
         p
      )
  in
  debug "Protected VMs: [ %s ]"
    (String.concat "; "
       (List.map (fun (vm, _) -> string_of_vm vm) vms_to_ensure_running)
    ) ;
  (* Current free memory on all hosts (does not include any for *offline* protected VMs ie those for which (vm_accounted_to_host vm)
     	   returns None) Also apply the supplied counterfactual-reasoning changes (if any) *)
  let hosts_and_memory =
    List.map
      (fun host ->
        (* Ultra-conservative assumption: plan using VM static_max values for normal domains,
           		   and dynamic_max for control domains. *)
        let summary = Memory_check.get_host_memory_summary ~__context ~host in
        let currently_free =
          Memory_check.host_compute_free_memory_with_policy ~__context summary
            Memory_check.Static_max
        in
        let sum = List.fold_left Int64.add 0L in
        let arriving =
          List.filter (fun (h, _) -> h = host) change.old_vms_arriving
        in
        let arriving_memory =
          sum
            (List.map
               (fun (_, (vm_ref, snapshot)) ->
                 total_memory_of_vm ~__context
                   ( if not $ Db.VM.get_is_control_domain ~__context ~self:vm_ref
                   then
                       Memory_check.Static_max
                   else
                     Memory_check.Dynamic_max
                   )
                   snapshot
               )
               arriving
            )
        in
        let leaving =
          List.filter (fun (h, _) -> h = host) change.old_vms_leaving
        in
        let leaving_memory =
          sum
            (List.map
               (fun (_, (vm_ref, snapshot)) ->
                 total_memory_of_vm ~__context
                   ( if not $ Db.VM.get_is_control_domain ~__context ~self:vm_ref
                   then
                       Memory_check.Static_max
                   else
                     Memory_check.Dynamic_max
                   )
                   snapshot
               )
               leaving
            )
        in
        ( host
        , Int64.sub (Int64.add currently_free leaving_memory) arriving_memory
        )
      )
      live_hosts
  in
  (* Memory required by all protected VMs *)
  let vms_and_memory =
    List.map
      (fun (vm, snapshot) ->
        (vm, total_memory_of_vm ~__context Memory_check.Static_max snapshot)
      )
      vms_to_ensure_running
  in
  (* For each non-agile VM, consider it pinned it to one host (even if it /could/ run on several). Note that if it is
     	   actually running somewhere else (very strange semi-agile situation) then it will be counted as overhead there and
     	   plans will be made for it running on the host we choose. *)
  let pinned =
    List.concat
      (List.map
         (host_of_non_agile_vm ~__context all_hosts_and_snapshots)
         not_agile_vms
      )
  in
  (* The restart plan for offline non-agile VMs is just the map VM -> pinned Host *)
  let non_agile_restart_plan =
    List.filter (fun (vm, _) -> vm_accounted_to_host vm = None) pinned
  in
  debug "Restart plan for non-agile offline VMs: [ %s ]"
    (string_of_plan non_agile_restart_plan) ;
  (* Update the host free memory to take this plan into account. Note we don't update the VM placement because that only
     	   considers agile VMs. Non-agile VMs are treated as per-host overhead. *)
  let hosts_and_memory =
    Binpack.account hosts_and_memory vms_and_memory non_agile_restart_plan
  in
  (* Now that we've considered the overhead of the non-agile (pinned) VMs, we can perform some binpacking of the agile VMs. *)
  let agile_vms_and_memory =
    List.map (fun (vm, _) -> (vm, List.assoc vm vms_and_memory)) agile_vms
  in
  (* Compute the current placement for all agile VMs. VMs which are powered off currently are placed nowhere *)
  let agile_vm_accounted_to_host =
    List.map (fun (vm, _snapshot) -> (vm, vm_accounted_to_host vm)) agile_vms
  in
  (* All these hosts are live and the VMs are running (or scheduled to be running): *)
  let agile_vm_placement =
    List.concat
      (List.map
         (fun (vm, host) -> match host with Some h -> [(vm, h)] | _ -> [])
         agile_vm_accounted_to_host
      )
  in
  (* These VMs are not running on any host (either in real life or only hypothetically) *)
  let agile_vm_failed =
    List.concat
      (List.map
         (fun (vm, host) -> if host = None then [vm] else [])
         agile_vm_accounted_to_host
      )
  in
  let config =
    {
      Binpack.hosts= hosts_and_memory
    ; vms= agile_vms_and_memory
    ; placement= agile_vm_placement
    ; total_hosts
    ; num_failures
    }
  in
  Binpack.check_configuration config ;
  debug "Planning configuration for offline agile VMs = %s"
    (Binpack.string_of_configuration string_of_host string_of_vm config) ;
  let h = Binpack.choose_heuristic config in
  (* Figure out how we could start as many of the agile VMs as possible *)
  debug "Computing a specific plan for the failure of VMs: [ %s ]"
    (String.concat "; " (List.map string_of_vm agile_vm_failed)) ;
  let agile_restart_plan = h.Binpack.get_specific_plan config agile_vm_failed in
  debug "Restart plan for agile offline VMs: [ %s ]"
    (string_of_plan agile_restart_plan) ;
  let vms_restarted = List.map fst agile_restart_plan in
  (* List the protected VMs which are not already running and weren't in the restart plan *)
  let vms_not_restarted =
    List.map fst
      (List.filter
         (fun (vm, _) ->
           vm_accounted_to_host vm = None && not (List.mem vm vms_restarted)
         )
         vms_to_ensure_running
      )
  in
  if vms_not_restarted <> [] then
    warn "Some protected VMs could not be restarted: [ %s ]"
      (String.concat "; " (List.map string_of_vm vms_not_restarted)) ;
  (* Applying the plan means:
     	   1. subtract from each host the memory needed to start the VMs in the plan; and
     	   2. modifying the VM placement map to reflect the plan. *)
  let config = Binpack.apply_plan config agile_restart_plan in
  (* All agile VMs which were offline have all been 'restarted' provided vms_not_restarted <> []
     	   If vms_not_restarted = [] then some VMs will have been left out. *)
  Binpack.check_configuration config ;
  debug "Planning configuration for future failures = %s"
    (Binpack.string_of_configuration string_of_host string_of_vm config) ;
  ( non_agile_restart_plan @ agile_restart_plan
  , config
  , vms_not_restarted
  , not_agile_vms <> []
  )

(** Returned by the plan_for_n_failures function *)
type plan_status =
  | Plan_exists_for_all_VMs
  | Plan_exists_excluding_non_agile_VMs
  | No_plan_exists

(** Given a number of host failures to consider, return a single value indicating whether a plan could be found
    and if so, what type. Note some protected VMs may currently be offline and we should first attempt to place those
    before considering future failures.
    This function also supports a limited counterfactual reasoning mode where:
    1. hosts can be given more/less free memory than they currently have in order to check that a proposed
       VM operation would not break the failover plan.
    2. hosts can be omitted from the plan in order to check that a host can be disabled/shutdown without
       breaking the plan.
*)

let plan_for_n_failures ~__context ~all_protected_vms ?live_set
    ?(change = no_configuration_change) n =
  let live_set =
    match live_set with None -> get_live_set ~__context | Some s -> s
  in
  try
    (* 'changes' are applied by the compute_restart_plan function *)
    let _plan, config, vms_not_restarted, non_agile_protected_vms_exist =
      compute_restart_plan ~__context ~all_protected_vms ~live_set ~change n
    in
    (* Could some VMs not be started? If so we're overcommitted before we started. *)
    if vms_not_restarted <> [] then (
      error
        "Even with no Host failures this Pool cannot start the configured \
         protected VMs." ;
      No_plan_exists
    ) else (
      debug "plan_for_n_failures config = %s"
        (Binpack.string_of_configuration
           (fun x ->
             Printf.sprintf "%s (%s)" (Ref.short_string_of x)
               (Db.Host.get_hostname ~__context ~self:x)
           )
           (fun x ->
             Printf.sprintf "%s (%s)" (Ref.short_string_of x)
               (Db.VM.get_name_label ~__context ~self:x)
           )
           config
        ) ;
      Binpack.check_configuration config ;
      let h = Binpack.choose_heuristic config in
      match
        (h.Binpack.plan_always_possible config, non_agile_protected_vms_exist)
      with
      | true, false ->
          Plan_exists_for_all_VMs
      | true, true ->
          Plan_exists_excluding_non_agile_VMs
      | false, _ ->
          No_plan_exists
    )
  with e ->
    error "Unexpected error in HA VM failover planning function: %s"
      (ExnHelper.string_of_exn e) ;
    No_plan_exists

let compute_max_host_failures_to_tolerate ~__context ?live_set ?protected_vms ()
    =
  let protected_vms =
    match protected_vms with
    | None ->
        all_protected_vms ~__context
    | Some vms ->
        vms
  in
  let total_hosts = List.length (Db.Host.get_all ~__context) in
  (* For corosync HA less than half of the pool can fail whilst maintaining quorum *)
  let corosync_ha_max_hosts =
    Xapi_clustering.compute_corosync_max_host_failures ~__context
  in
  let nhosts =
    match Db.Cluster.get_all ~__context with
    | [] ->
        total_hosts
    | _ ->
        corosync_ha_max_hosts
  in
  (* We assume that if not(plan_exists(n)) then \forall.x>n not(plan_exists(n))
     although even if we screw this up it's not a disaster because all we need is a
     safe approximation (so ultimately "0" will do but we'd prefer higher) *)
  Helpers.bisect
    (fun n ->
      plan_for_n_failures ~__context ~all_protected_vms:protected_vms ?live_set
        (Int64.to_int n)
      = Plan_exists_for_all_VMs
    )
    0L (Int64.of_int nhosts)

(* Make sure the pool is marked as overcommitted and the appropriate alert is generated. Return
   true if something changed, false otherwise *)
let mark_pool_as_overcommitted ~__context ~live_set =
  Xapi_clustering.with_clustering_lock_if_cluster_exists ~__context __LOC__
    (fun () ->
      let pool = Helpers.get_pool ~__context in
      let overcommitted = Db.Pool.get_ha_overcommitted ~__context ~self:pool in
      let planned_for = Db.Pool.get_ha_plan_exists_for ~__context ~self:pool in
      let to_tolerate =
        Db.Pool.get_ha_host_failures_to_tolerate ~__context ~self:pool
      in
      let max_failures =
        compute_max_host_failures_to_tolerate ~__context ~live_set ()
      in
      if planned_for <> max_failures then (
        Db.Pool.set_ha_plan_exists_for ~__context ~self:pool
          ~value:(min to_tolerate max_failures) ;
        if max_failures < planned_for then
          Xapi_alert.add ~msg:Api_messages.ha_pool_drop_in_plan_exists_for
            ~cls:`Pool
            ~obj_uuid:(Db.Pool.get_uuid ~__context ~self:pool)
            ~body:(Int64.to_string max_failures)
      ) ;
      if not overcommitted then (
        Db.Pool.set_ha_overcommitted ~__context ~self:pool ~value:true ;
        (* On the transition generate a message *)
        let obj_uuid = Db.Pool.get_uuid ~__context ~self:pool in
        let pool_name_label = Db.Pool.get_name_label ~__context ~self:pool in
        (* Note -- it's OK to look up stuff in the database when generating the alert text, because this code runs on the master; therefore there is no
           danger of blocking for db.* calls to return *)
        let name, priority = Api_messages.ha_pool_overcommitted in
        let (_ : 'a Ref.t) =
          Xapi_message.create ~__context ~name ~priority ~cls:`Pool ~obj_uuid
            ~body:
              (Printf.sprintf
                 "The failover tolerance for pool '%s' has dropped and the \
                  initially specified number of host failures to tolerate can \
                  no longer be guaranteed"
                 pool_name_label
              )
        in
        () ;
        (* Call a hook to allow someone the opportunity to bring more capacity online *)
        Xapi_hooks.pool_ha_overcommitted_hook ~__context
      ) ;
      planned_for <> max_failures || not overcommitted
  )

(* Update the pool's HA fields *)
let update_pool_status ~__context ?live_set () =
  let live_set =
    match live_set with None -> get_live_set ~__context | Some s -> s
  in
  let pool = Helpers.get_pool ~__context in
  let overcommitted = Db.Pool.get_ha_overcommitted ~__context ~self:pool in
  let to_tolerate =
    Db.Pool.get_ha_host_failures_to_tolerate ~__context ~self:pool
  in
  let planned_for = Db.Pool.get_ha_plan_exists_for ~__context ~self:pool in
  let all_protected_vms = all_protected_vms ~__context in
  (* Check whether we can generate plans for the number of host failures we care about.
     i.e. have we become overcommitted (in the sense of having too few resources to satisfy
     demand) or not?
  *)
  match
    plan_for_n_failures ~__context ~all_protected_vms ~live_set
      (Int64.to_int to_tolerate)
  with
  | Plan_exists_for_all_VMs ->
      debug "HA failover plan exists for all protected VMs" ;
      Db.Pool.set_ha_overcommitted ~__context ~self:pool ~value:false ;
      debug "to_tolerate = %Ld planned_for = %Ld" to_tolerate planned_for ;
      if planned_for <> to_tolerate then
        Db.Pool.set_ha_plan_exists_for ~__context ~self:pool ~value:to_tolerate ;
      (* return true if something changed *)
      overcommitted || planned_for <> to_tolerate
  | Plan_exists_excluding_non_agile_VMs ->
      debug
        "HA failover plan exists for all protected VMs, excluding some \
         non-agile VMs" ;
      mark_pool_as_overcommitted ~__context ~live_set
      (* might define this as false later *)
  | No_plan_exists ->
      debug "No HA failover plan exists" ;
      mark_pool_as_overcommitted ~__context ~live_set

let assert_configuration_change_preserves_ha_plan ~__context c =
  debug "assert_configuration_change_preserves_ha_plan c = %s"
    (string_of_configuration_change ~__context c) ;
  (* Only block the operation if a plan exists now but would evaporate with the proposed changes.
     This prevents us blocking all operations should be suddenly become overcommitted eg through
     multiple host failures *)
  let live_set = get_live_set ~__context in
  let pool = Helpers.get_pool ~__context in
  if
    Db.Pool.get_ha_enabled ~__context ~self:pool
    && not (Db.Pool.get_ha_allow_overcommit ~__context ~self:pool)
  then
    let to_tolerate =
      Int64.to_int (Db.Pool.get_ha_plan_exists_for ~__context ~self:pool)
    in
    let all_protected_vms = all_protected_vms ~__context in
    match
      plan_for_n_failures ~__context ~all_protected_vms ~live_set to_tolerate
    with
    | Plan_exists_excluding_non_agile_VMs | No_plan_exists ->
        debug
          "assert_configuration_change_preserves_ha_plan: no plan currently \
           exists; cannot get worse"
    | Plan_exists_for_all_VMs -> (
      (* Does the plan break? *)
      match
        plan_for_n_failures ~__context ~all_protected_vms ~live_set ~change:c
          to_tolerate
      with
      | Plan_exists_for_all_VMs ->
          debug
            "assert_configuration_change_preserves_ha_plan: plan exists after \
             change"
      | Plan_exists_excluding_non_agile_VMs | No_plan_exists ->
          debug
            "assert_configuration_change_preserves_ha_plan: proposed change \
             breaks plan" ;
          raise
            (Api_errors.Server_error
               (Api_errors.ha_operation_would_break_failover_plan, [])
            )
    )

let assert_host_disable_preserves_ha_plan ~__context host =
  assert_configuration_change_preserves_ha_plan ~__context
    {no_configuration_change with hosts_to_disable= [host]}

let assert_vm_placement_preserves_ha_plan ~__context ?(leaving = [])
    ?(arriving = []) () =
  assert_configuration_change_preserves_ha_plan ~__context
    {
      no_configuration_change with
      old_vms_leaving= leaving
    ; old_vms_arriving= arriving
    }

let assert_nfailures_change_preserves_ha_plan ~__context n =
  assert_configuration_change_preserves_ha_plan ~__context
    {no_configuration_change with num_failures= Some n}

let assert_new_vm_preserves_ha_plan ~__context new_vm =
  assert_configuration_change_preserves_ha_plan ~__context
    {no_configuration_change with new_vms_to_protect= [new_vm]}

(* If a VM fails to start then we remember this fact to avoid sending duplicate alerts. *)
let restart_failed : (API.ref_VM, unit) Hashtbl.t = Hashtbl.create 10

(* We also limit the rate we attempt to retry starting the VM. *)
let last_start_attempt : (API.ref_VM, float) Hashtbl.t = Hashtbl.create 10

(* Takes the current live_set and number of hosts we're planning to handle, updates the host records in the database
   and restarts any offline protected VMs *)
let restart_auto_run_vms ~__context live_set n =
  (* ensure we have live=false on the host_metrics for those hosts not in the live_set; and force state to Halted for
     	   all VMs that are "running" or "paused" with resident_on set to one of the hosts that is now dead
  *)
  debug "restart_auto_run_vms called" ;
  let hosts = Db.Host.get_all ~__context in
  (* Keep a list of all the VMs whose power-states we force to Halted to use later in the
     	   'best-effort' restart code. Note that due to the weakly consistent database this is not
     	   an accurate way to determine 'failed' VMs but it will suffice for our 'best-effort'
     	   category. *)
  let reset_vms = ref [] in
  let dead_hosts = ref [] in
  List.iter
    (fun h ->
      if not (List.mem h live_set) then (
        let hostname = Db.Host.get_hostname ~__context ~self:h in
        debug "Setting host %s to dead" hostname ;
        (* Sample this before calling any hook scripts *)
        let resident_on_vms =
          List.filter
            (fun vm -> not (Db.VM.get_is_control_domain ~__context ~self:vm))
            (Db.Host.get_resident_VMs ~__context ~self:h)
        in
        reset_vms := resident_on_vms @ !reset_vms ;
        (* ensure live=false *)
        try
          let h_metrics = Db.Host.get_metrics ~__context ~self:h in
          let current = Db.Host_metrics.get_live ~__context ~self:h_metrics in
          if current then (
            (* Fire off a ha_host_failed message if the host hasn't just shut itself down *)
            let shutting_down =
              Xapi_stdext_threads.Threadext.Mutex.execute
                Xapi_globs.hosts_which_are_shutting_down_m (fun () ->
                  !Xapi_globs.hosts_which_are_shutting_down
              )
            in
            ( if not (List.exists (fun x -> x = h) shutting_down) then
                let obj_uuid = Db.Host.get_uuid ~__context ~self:h in
                let host_name = Db.Host.get_name_label ~__context ~self:h in
                Xapi_alert.add ~msg:Api_messages.ha_host_failed ~cls:`Host
                  ~obj_uuid
                  ~body:(Printf.sprintf "Server '%s' has failed" host_name)
            ) ;
            (* Call external host failed hook (allows a third-party to use power-fencing if desired) *)
            Xapi_hooks.host_pre_declare_dead ~__context ~host:h
              ~reason:Xapi_hooks.reason__fenced ;
            Db.Host_metrics.set_live ~__context ~self:h_metrics ~value:false ;
            (* since slave is fenced, it will not set this to true again itself *)
            Xapi_host_helpers.update_allowed_operations ~__context ~self:h ;
            dead_hosts := h :: !dead_hosts
          )
        with _ -> ()
        (* if exn assume h_metrics doesn't exist, then "live" is defined to be false implicitly, so do nothing *)
      )
    )
    hosts ;
  debug "Setting all VMs running or paused to Halted" ;
  (* ensure all vms resident_on this host running or paused have their powerstates reset *)
  List.iter
    (fun vm ->
      if Xapi_vm_lifecycle_helpers.is_live ~__context ~self:vm then
        Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted
    )
    !reset_vms ;
  (* host_post_declare_dead may take a long time if the SR is locked *)
  dead_hosts := List.rev !dead_hosts ;
  List.iter
    (fun h ->
      Xapi_hooks.host_post_declare_dead ~__context ~host:h
        ~reason:Xapi_hooks.reason__fenced
    )
    !dead_hosts ;
  (* If something has changed then we'd better refresh the pool status *)
  if !reset_vms <> [] then ignore (update_pool_status ~__context ~live_set ()) ;
  (* At this point failed protected agile VMs are Halted, not resident_on anywhere *)
  let all_protected_vms = all_protected_vms ~__context in
  let plan, plan_is_complete =
    try
      if Xapi_fist.simulate_planner_failure () then
        failwith "fist_simulate_planner_failure" ;
      (* CA-23981: if the pool-pre-ha-vm-restart hook exists AND if we're about to auto-start some VMs then
         			   call the script hook first and then recompute the plan aftwards. Note that these VMs may either
         			   be protected or best-effort. For the protected ones we assume that these are included in the VM
         			   restart plan-- we ignore the possibility that the planner may fail here (even through there is some
         			   last-ditch code later to perform best-effort VM.starts). This is ok since it should never happen and
         			   this particular hook is really a 'best-effort' integration point since it conflicts with the overcommit
         			   protection.
         			   For the best-effort VMs we call the script
         			   when we have reset some VMs to halted (no guarantee there is enough resource but better safe than sorry) *)
      let plan, config, vms_not_restarted, non_agile_protected_vms_exist =
        compute_restart_plan ~__context ~all_protected_vms ~live_set n
      in
      let plan, _config, vms_not_restarted, _non_agile_protected_vms_exist =
        if
          true
          && Xapi_hooks.pool_pre_ha_vm_restart_hook_exists ()
          && (plan <> [] || !reset_vms <> [])
        then (
          (* We're about to soak up some resources for 'Level 1' VMs somewhere; before we do that give 'Level 2' VMs a shot *)
          (* Whatever this script does we don't let it break our restart thread *)
          ( try Xapi_hooks.pool_pre_ha_vm_restart_hook ~__context
            with e ->
              error "pool-pre-ha-vm-restart-hook failed: %s: continuing anyway"
                (ExnHelper.string_of_exn e)
          ) ;
          debug
            "Recomputing restart plan to take into account new state of the \
             world after running the script" ;
          compute_restart_plan ~__context ~all_protected_vms ~live_set n
        ) else
          (plan, config, vms_not_restarted, non_agile_protected_vms_exist)
        (* nothing needs recomputing *)
      in
      (* If we are undercommitted then vms_not_restarted = [] and plan will include all offline protected_vms *)
      let plan_is_complete = vms_not_restarted = [] in
      (plan, plan_is_complete)
    with e ->
      error "Caught unexpected exception in HA planner: %s"
        (ExnHelper.string_of_exn e) ;
      ([], false)
  in
  (* Send at most one alert per protected VM failure *)
  let consider_sending_failed_alert_for vm =
    debug "We failed to restart protected VM %s: considering sending an alert"
      (Ref.string_of vm) ;
    if not (Hashtbl.mem restart_failed vm) then (
      Hashtbl.replace restart_failed vm () ;
      let obj_uuid = Db.VM.get_uuid ~__context ~self:vm in
      Xapi_alert.add ~msg:Api_messages.ha_protected_vm_restart_failed ~cls:`VM
        ~obj_uuid ~body:""
    )
  in
  let open TaskChains.Infix in
  (* execute the plan *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      (* Helper function to start a VM somewhere. If the HA overcommit protection stops us then disable it and try once more.
         			   Returns true if the VM was restarted and false otherwise. *)
      let restart_vm vm ?host () =
        let go () =
          ( if Xapi_fist.simulate_restart_failure () then
              match Random.int 3 with
              | 0 ->
                  raise
                    (Api_errors.Server_error
                       (Api_errors.ha_operation_would_break_failover_plan, [])
                    )
              | 1 ->
                  raise
                    (Api_errors.Server_error ("FIST: unexpected exception", []))
              | _ ->
                  ()
          ) ;
          (* If we tried before and failed, don't retry again within 2 minutes *)
          let attempt_restart =
            if Hashtbl.mem last_start_attempt vm then
              Unix.gettimeofday () -. Hashtbl.find last_start_attempt vm > 120.
            else
              true
          in
          if attempt_restart then (
            Hashtbl.replace last_start_attempt vm (Unix.gettimeofday ()) ;
            match host with
            | None ->
                Client.Client.Async.VM.start ~rpc ~session_id ~vm
                  ~start_paused:false ~force:true
            | Some host ->
                Client.Client.Async.VM.start_on ~rpc ~session_id ~vm ~host
                  ~start_paused:false ~force:true
          ) else
            failwith
              (Printf.sprintf "VM: %s restart attempt delayed for 120s"
                 (Ref.string_of vm)
              )
        in
        TaskChains.task go >>= function
        | Error (Api_errors.Server_error (code, _))
          when code = Api_errors.ha_operation_would_break_failover_plan -> (
            (* This should never happen since the planning code would always allow the restart of a protected VM... *)
            error
              "Caught exception HA_OPERATION_WOULD_BREAK_FAILOVER_PLAN: \
               setting pool as overcommitted and retrying" ;
            ignore (mark_pool_as_overcommitted ~__context ~live_set : bool) ;
            TaskChains.task go >>= function
            | Ok _ ->
                TaskChains.ok ()
            | Error e ->
                error "Caught exception trying to restart VM %s: %s"
                  (Ref.string_of vm)
                  (ExnHelper.string_of_exn e) ;
                TaskChains.fail e
          )
        | Error e ->
            error "Caught exception trying to restart VM %s: %s"
              (Ref.string_of vm)
              (ExnHelper.string_of_exn e) ;
            TaskChains.fail e
        | Ok _ ->
            TaskChains.ok ()
      in
      (* [ordered_map_concat f lst]
       * traverses the list in order, maps each inner list through [f]
       * and concatenates the result *)
      let ordered_map_concat f lst =
        debug "Processing %d parallel groups" (List.length lst) ;
        (* the iteration order is important here for preserving the VM start order *)
        List.fold_left
          (fun accum inner ->
            (* the order of elements in the result doesn't matter,
             * they were launched in parallel *)
            List.rev_append (f inner) accum
          )
          [] lst
      in

      (* [map_parallel ~order_f f lst] groups objects in [lst] by order number
          (provided by applying [order_f] to each object). For each group of objects
          it then applies [f] to them to produce an 'a TaskChain.t and then executes this
          set of TaskChain.t values in parallel until they each of the TaskChain.t
          values has been executed to completion. It then begins work on the next group
          of objects.

          The return is a list of 'a result
      *)
      let map_parallel ~order_f f lst =
        lst
        |> Helpers.group_by ~ordering:`ascending order_f
        |> ordered_map_concat (fun same_order ->
               same_order
               |> List.rev_map fst
               |> List.rev_map f
               |> TaskChains.parallel ~__context ~rpc ~session_id
           )
      in
      (* Build a list of bools, one per Halted protected VM indicating whether we managed to start it or not *)
      let started =
        if not plan_is_complete then (
          (* If the Pool is overcommitted the restart priority will make the difference between a VM restart or not,
             					   while if we're undercommitted the restart priority only affects the timing slightly. *)
          let all =
            List.filter
              (fun (_, r) -> r.API.vM_power_state = `Halted)
              all_protected_vms
          in
          warn
            "Failed to find plan to restart all protected VMs: falling back to \
             simple VM.start in priority order" ;
          map_parallel ~order_f
            (fun (vm, vmr) -> ((vm, vmr), restart_vm vm ()))
            all
        ) else
          (* Walk over the VMs in priority order, starting each on the planned host *)
          let all =
            List.map
              (fun (vm, _) -> (vm, Db.VM.get_record ~__context ~self:vm))
              plan
          in
          map_parallel ~order_f
            (fun (vm, vmr) ->
              ( (vm, vmr)
              , if List.mem_assoc vm plan then
                  restart_vm vm ~host:(List.assoc vm plan) ()
                else
                  TaskChains.fail (Failure "VM has no plan")
              )
            )
            all
      in
      (* Perform one final restart attempt of any that weren't started. *)
      let started =
        map_parallel
          ~order_f:(fun (vminfo, _) -> order_f vminfo)
          (function
            | (vm, _), Ok () ->
                (vm, TaskChains.ok ())
            | (vm, _), Error _ ->
                (vm, restart_vm vm ())
            )
          started
      in
      (* Send an alert for any failed VMs *)
      List.iter
        (fun (vm, started) ->
          if started <> Ok () then consider_sending_failed_alert_for vm
        )
        started ;
      (* Forget about previously failed VMs which have gone *)
      let vms_we_know_about = List.map fst started in
      let gc_table tbl =
        let vms_in_table = Hashtbl.fold (fun vm _ acc -> vm :: acc) tbl [] in
        List.iter
          (fun vm ->
            if not (List.mem vm vms_we_know_about) then (
              debug "Forgetting VM: %s" (Ref.string_of vm) ;
              Hashtbl.remove tbl vm
            )
          )
          vms_in_table
      in
      gc_table last_start_attempt ;
      gc_table restart_failed ;
      (* Consider restarting the best-effort VMs we *think* have failed (but we might get this wrong --
         			   ok since this is 'best-effort'). NOTE we do not use the restart_vm function above as this will mark the
         			   pool as overcommitted if an HA_OPERATION_WOULD_BREAK_FAILOVER_PLAN is received (although this should never
         			   happen it's better safe than sorry) *)
      map_parallel
        ~order_f:(fun vm -> order_f (vm, Db.VM.get_record ~__context ~self:vm))
        (fun vm ->
          ( vm
          , if
              Db.VM.get_power_state ~__context ~self:vm = `Halted
              && Db.VM.get_ha_restart_priority ~__context ~self:vm
                 = Constants.ha_restart_best_effort
            then
              TaskChains.task (fun () ->
                  Client.Client.Async.VM.start ~rpc ~session_id ~vm
                    ~start_paused:false ~force:true
              )
            else
              TaskChains.ok Rpc.Null
          )
        )
        !reset_vms
      |> List.iter (fun (vm, result) ->
             match result with
             | Error e ->
                 error "Failed to restart best-effort VM %s (%s): %s"
                   (Db.VM.get_uuid ~__context ~self:vm)
                   (Db.VM.get_name_label ~__context ~self:vm)
                   (ExnHelper.string_of_exn e)
             | Ok _ ->
                 ()
         )
  )
