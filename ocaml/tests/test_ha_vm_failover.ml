(*
 * Copyright (C) Citrix Systems Inc.
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

open Test_common
open Test_highlevel

let ( *** ) = Int64.mul

let kib x = 1024L *** x

let mib x = x |> kib |> kib

let gib x = x |> kib |> kib |> kib

type vbd = {agile: bool}

type vif = {agile: bool}

type placement_policy = AntiAffinity | Normal

type group = {name_label: string; placement: placement_policy}

type vm = {
    ha_always_run: bool
  ; ha_restart_priority: string
  ; memory: int64
  ; vm_name_label: string
  ; vbds: vbd list
  ; vifs: vif list
  ; groups: group list
  ; power_state: string
}

let basic_vm =
  {
    ha_always_run= true
  ; ha_restart_priority= "restart"
  ; memory= gib 1L
  ; vm_name_label= "vm"
  ; vbds= [{agile= true}]
  ; vifs= [{agile= true}]
  ; groups= []
  ; power_state= "running"
  }

type host = {memory_total: int64; name_label: string; vms: vm list}

type pool = {
    master: host
  ; slaves: host list
  ; ha_host_failures_to_tolerate: int64
  ; cluster: int
  ; keep_localhost: bool
        (* true to get a Features.Pool_size enabled pool which can have more than 3 hosts *)
}

let master = "master"

let slave = "slave"

let slave1 = "slave1"

let slave2 = "slave2"

let slave3 = "slave3"

let grp1 = "grp1"

let grp2 = "grp2"

(** vmX_grpY: in test case for anti_affinity, the VM is the Xth smallest of slave1's VMs of
    the same placement type in terms of VM's memory size, and it belows to VM group: grpY. *)
let vm1_grp1 = "vm1_grp1"

let vm2_grp1 = "vm2_grp1"

let vm3_grp1 = "vm3_grp1"

let vm4_grp1 = "vm4_grp1"

let vm5_grp1 = "vm5_grp1"

let vm6_grp1 = "vm6_grp1"

let vm8_grp1 = "vm8_grp1"

let vm2_grp2 = "vm2_grp2"

let vm3_grp2 = "vm3_grp2"

let vm4_grp2 = "vm4_grp2"

let vm5_grp2 = "vm5_grp2"

let vm7_grp2 = "vm7_grp2"

(** In test case for anti_affinity, it is a VM resident on host other than slave1 *)
let vm_grp1 = "vm_grp1"

(** vmX: in test case for anti_affinity, it is a VM not in any VM group, and it is the Xth
    largest of slave1's VMs not in any VM group in terms of VM's memory size. *)
let vm1 = "vm1"

let vm2 = "vm2"

let vm3 = "vm3"

let vm4 = "vm4"

let basic_pool =
  {
    master= {memory_total= gib 256L; name_label= master; vms= []}
  ; slaves= []
  ; ha_host_failures_to_tolerate= 0L
  ; cluster= 0
  ; keep_localhost= false
  }

let string_of_group {name_label; placement} =
  Printf.sprintf "{name_label = %S; placement = %S}" name_label
    (match placement with AntiAffinity -> "anti_affinity" | Normal -> "normal")

let string_of_vm {memory; vm_name_label; groups; _} =
  Printf.sprintf "{memory = %Ld; name_label = %S; groups = [%s]}" memory
    vm_name_label
    (Test_printers.list string_of_group groups)

let string_of_host {memory_total; name_label; vms} =
  Printf.sprintf "{memory_total = %Ld; name_label = %S; vms = [%s]}"
    memory_total name_label
    (Test_printers.list string_of_vm vms)

let string_of_pool
    {master; slaves; ha_host_failures_to_tolerate; cluster; keep_localhost} =
  Printf.sprintf
    "{master = %s; slaves = %s; ha_host_failures_to_tolerate = %Ld; cluster = \
     %d; keep_localhost = %B}"
    (string_of_host master)
    (Test_printers.list string_of_host slaves)
    ha_host_failures_to_tolerate cluster keep_localhost

let load_group ~__context ~group =
  let placement =
    match group.placement with
    | AntiAffinity ->
        `anti_affinity
    | Normal ->
        `normal
  in
  match
    Db.VM_group.get_all ~__context
    |> List.find_opt (fun g ->
           Db.VM_group.get_name_label ~__context ~self:g = group.name_label
           && Db.VM_group.get_placement ~__context ~self:g = placement
       )
  with
  | None ->
      make_vm_group ~__context ~name_label:group.name_label ~placement ()
  | Some g ->
      g

let load_vm ~__context ~(vm : vm) ~local_sr ~shared_sr ~local_net ~shared_net =
  let vm_ref =
    make_vm ~__context ~ha_always_run:vm.ha_always_run
      ~ha_restart_priority:vm.ha_restart_priority ~memory_static_min:vm.memory
      ~memory_dynamic_min:vm.memory ~memory_dynamic_max:vm.memory
      ~memory_static_max:vm.memory ~name_label:vm.vm_name_label ()
  in
  let (_ : API.ref_VIF list) =
    List.mapi
      (fun index (vif : vif) ->
        make_vif ~__context ~device:(string_of_int index) ~vM:vm_ref
          ~network:(if vif.agile then shared_net else local_net)
          ()
      )
      vm.vifs
  in
  let (_ : API.ref_VBD list) =
    List.mapi
      (fun index (vbd : vbd) ->
        let vdi_ref =
          make_vdi ~__context ~sR:(if vbd.agile then shared_sr else local_sr) ()
        in
        make_vbd ~__context ~device:(string_of_int index) ~vM:vm_ref
          ~vDI:vdi_ref ()
      )
      vm.vbds
  in
  let groups =
    List.fold_left
      (fun acc group -> load_group ~__context ~group :: acc)
      [] vm.groups
  in
  Db.VM.set_groups ~__context ~self:vm_ref ~value:groups ;
  if "running" = vm.power_state then
    Db.VM.set_power_state ~__context ~self:vm_ref ~value:`Running ;
  vm_ref

let load_host ~__context ~host ~local_sr ~shared_sr ~local_net ~shared_net =
  let host_ref = make_host ~__context ~name_label:host.name_label () in
  Db.Host.set_enabled ~__context ~self:host_ref ~value:true ;
  let metrics = Db.Host.get_metrics ~__context ~self:host_ref in
  Db.Host_metrics.set_live ~__context ~self:metrics ~value:true ;
  Db.Host_metrics.set_memory_total ~__context ~self:metrics
    ~value:host.memory_total ;
  let (_ : API.ref_VM list) =
    List.map
      (fun vm ->
        let vm_ref =
          load_vm ~__context ~vm ~local_sr ~shared_sr ~local_net ~shared_net
        in
        Db.VM.set_resident_on ~__context ~self:vm_ref ~value:host_ref ;
        vm_ref
      )
      host.vms
  in
  host_ref

let setup ~__context
    {master; slaves; ha_host_failures_to_tolerate; cluster; keep_localhost} =
  let shared_sr = make_sr ~__context ~shared:true () in
  let shared_net = make_network ~__context ~bridge:"xenbr0" () in
  if not keep_localhost then (* Remove all hosts added by make_test_database *)
    List.iter
      (fun host -> Db.Host.destroy ~__context ~self:host)
      (Db.Host.get_all ~__context) ;
  let load_host_and_local_resources host =
    let local_sr = make_sr ~__context ~shared:false () in
    let local_net = make_network ~__context ~bridge:"xapi0" () in
    let host_ref =
      load_host ~__context ~host ~local_sr ~shared_sr ~local_net ~shared_net
    in
    let (_ : API.ref_PBD) =
      make_pbd ~__context ~host:host_ref ~sR:local_sr ()
    in
    let (_ : API.ref_PBD) =
      make_pbd ~__context ~host:host_ref ~sR:shared_sr ()
    in
    let (_ : API.ref_PIF) =
      make_pif ~__context ~host:host_ref ~network:local_net ()
    in
    let (_ : API.ref_PIF) =
      make_pif ~__context ~host:host_ref ~network:shared_net ()
    in
    host_ref
  in
  let master_ref = load_host_and_local_resources master in
  let (_ : API.ref_host list) = List.map load_host_and_local_resources slaves in
  if cluster > 0 then
    Test_common.make_cluster_and_cluster_host ~__context () |> ignore ;
  for i = 0 to cluster - 1 do
    let host = List.nth (Db.Host.get_all ~__context) i in
    Test_common.make_cluster_host ~__context ~host () |> ignore
  done ;
  let pool = Db.Pool.get_all ~__context |> List.hd in
  Db.Pool.set_master ~__context ~self:pool ~value:master_ref ;
  Db.Pool.set_ha_enabled ~__context ~self:pool ~value:true ;
  Db.Pool.set_ha_host_failures_to_tolerate ~__context ~self:pool
    ~value:ha_host_failures_to_tolerate ;
  Db.Pool.set_ha_plan_exists_for ~__context ~self:pool
    ~value:ha_host_failures_to_tolerate

module AllProtectedVms = Generic.MakeStateful (struct
  module Io = struct
    type input_t = pool

    type output_t = string list

    let string_of_input_t = string_of_pool

    let string_of_output_t = Test_printers.(list string)
  end

  module State = Test_state.XapiDb

  let load_input __context input = setup ~__context input

  let extract_output __context _ =
    Xapi_ha_vm_failover.all_protected_vms ~__context
    |> List.map (fun (_, vm_rec) -> vm_rec.API.vM_name_label)
    |> List.sort compare

  let tests =
    `QuickAndAutoDocumented
      [
        (* No VMs and a single host. *)
        ( {
            basic_pool with
            master= {memory_total= gib 256L; name_label= master; vms= []}
          ; slaves= []
          }
        , []
        )
      ; (* One unprotected VM. *)
        ( {
            basic_pool with
            master=
              {
                memory_total= gib 256L
              ; name_label= master
              ; vms=
                  [
                    {basic_vm with ha_always_run= false; ha_restart_priority= ""}
                  ]
              }
          ; slaves= []
          }
        , []
        )
      ; (* One VM which would be protected if it was running. *)
        ( {
            basic_pool with
            master=
              {
                memory_total= gib 256L
              ; name_label= master
              ; vms= [{basic_vm with ha_always_run= false}]
              }
          ; slaves= []
          }
        , []
        )
      ; (* One protected VM. *)
        ( {
            basic_pool with
            master=
              {memory_total= gib 256L; name_label= master; vms= [basic_vm]}
          ; slaves= []
          }
        , ["vm"]
        )
      ; (* One protected VM and one unprotected VM. *)
        ( {
            basic_pool with
            master=
              {
                memory_total= gib 256L
              ; name_label= master
              ; vms=
                  [
                    {basic_vm with vm_name_label= vm1}
                  ; {
                      basic_vm with
                      ha_always_run= false
                    ; ha_restart_priority= ""
                    ; vm_name_label= vm2
                    }
                  ]
              }
          ; slaves= []
          }
        , [vm1]
        )
      ]
end)

module PlanForNFailures = Generic.MakeStateful (struct
  module Io = struct
    open Xapi_ha_vm_failover

    type input_t = pool

    type output_t = plan_status

    let string_of_input_t = string_of_pool

    let string_of_output_t = function
      | Plan_exists_for_all_VMs ->
          "Plan_exists_for_all_VMs"
      | Plan_exists_excluding_non_agile_VMs ->
          "Plan_exists_excluding_non_agile_VMs"
      | No_plan_exists ->
          "No_plan_exists"
  end

  module State = Test_state.XapiDb

  let load_input __context = setup ~__context

  let extract_output __context pool =
    let all_protected_vms = Xapi_ha_vm_failover.all_protected_vms ~__context in
    Xapi_ha_vm_failover.plan_for_n_failures ~__context ~all_protected_vms
      (Int64.to_int pool.ha_host_failures_to_tolerate)

  (* TODO: Add a test which causes plan_for_n_failures to return
        * Plan_exists_excluding_non_agile_VMs. *)
  let tests =
    `QuickAndAutoDocumented
      [
        (* Two host pool with no VMs. *)
        ( {
            basic_pool with
            master= {memory_total= gib 256L; name_label= master; vms= []}
          ; slaves= [{memory_total= gib 256L; name_label= slave; vms= []}]
          ; ha_host_failures_to_tolerate= 1L
          }
        , Xapi_ha_vm_failover.Plan_exists_for_all_VMs
        )
      ; (* Two host pool, with one VM taking up just under half of one host's
                * memory. *)
        ( {
            basic_pool with
            master=
              {
                memory_total= gib 256L
              ; name_label= master
              ; vms= [{basic_vm with memory= gib 120L; vm_name_label= vm1}]
              }
          ; slaves= [{memory_total= gib 256L; name_label= slave; vms= []}]
          ; ha_host_failures_to_tolerate= 1L
          }
        , Xapi_ha_vm_failover.Plan_exists_for_all_VMs
        )
      ; (* Two host pool, with two VMs taking up almost all of one host's memory. *)
        ( {
            basic_pool with
            master=
              {
                memory_total= gib 256L
              ; name_label= master
              ; vms=
                  [
                    {basic_vm with memory= gib 120L; vm_name_label= vm1}
                  ; {basic_vm with memory= gib 120L; vm_name_label= vm2}
                  ]
              }
          ; slaves= [{memory_total= gib 256L; name_label= slave; vms= []}]
          ; ha_host_failures_to_tolerate= 1L
          }
        , Xapi_ha_vm_failover.Plan_exists_for_all_VMs
        )
      ; (* Two host pool, overcommitted. *)
        ( {
            basic_pool with
            master=
              {
                memory_total= gib 256L
              ; name_label= master
              ; vms=
                  [
                    {basic_vm with memory= gib 120L; vm_name_label= vm1}
                  ; {basic_vm with memory= gib 120L; vm_name_label= vm2}
                  ]
              }
          ; slaves=
              [
                {
                  memory_total= gib 256L
                ; name_label= slave
                ; vms=
                    [
                      {basic_vm with memory= gib 120L; vm_name_label= vm3}
                    ; {basic_vm with memory= gib 120L; vm_name_label= vm4}
                    ]
                }
              ]
          ; ha_host_failures_to_tolerate= 1L
          }
        , Xapi_ha_vm_failover.No_plan_exists
        )
      ]
end)

let string_of_unit_result =
  Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))

module AssertNewVMPreservesHAPlan = Generic.MakeStateful (struct
  module Io = struct
    type input_t = pool * vm

    type output_t = (unit, exn) result

    let string_of_input_t = Test_printers.pair string_of_pool string_of_vm

    let string_of_output_t = string_of_unit_result
  end

  module State = Test_state.XapiDb

  let load_input __context (pool, _) = setup ~__context pool

  let extract_output __context (_pool, vm) =
    let open Db_filter_types in
    let local_sr =
      Db.SR.get_refs_where ~__context
        ~expr:(Eq (Field "shared", Literal "false"))
      |> List.hd
    in
    let shared_sr =
      Db.SR.get_refs_where ~__context ~expr:(Eq (Field "shared", Literal "true"))
      |> List.hd
    in
    let local_net =
      Db.Network.get_refs_where ~__context
        ~expr:(Eq (Field "bridge", Literal "xapi0"))
      |> List.hd
    in
    let shared_net =
      Db.Network.get_refs_where ~__context
        ~expr:(Eq (Field "bridge", Literal "xenbr0"))
      |> List.hd
    in
    let vm_ref =
      load_vm ~__context ~vm ~local_sr ~shared_sr ~local_net ~shared_net
    in
    try
      Ok (Xapi_ha_vm_failover.assert_new_vm_preserves_ha_plan ~__context vm_ref)
    with e -> Error e

  (* n.b. incoming VMs have ha_always_run = false; otherwise they will be
        * included when computing the plan for the already-running VMs. *)
  let tests =
    `QuickAndAutoDocumented
      [
        (* 2 host pool, one VM using just under half of one host's memory;
                * test that another VM can be added. *)
        ( ( {
              basic_pool with
              master=
                {
                  memory_total= gib 256L
                ; name_label= master
                ; vms= [{basic_vm with memory= gib 120L; vm_name_label= vm1}]
                }
            ; slaves= [{memory_total= gib 256L; name_label= slave; vms= []}]
            ; ha_host_failures_to_tolerate= 1L
            }
          , {
              basic_vm with
              ha_always_run= false
            ; ha_restart_priority= "restart"
            ; memory= gib 120L
            ; vm_name_label= vm2
            }
          )
        , Ok ()
        )
      ; (* 2 host pool, two VMs using almost all of one host's memory;
                * test that another VM cannot be added. *)
        ( ( {
              basic_pool with
              master=
                {
                  memory_total= gib 256L
                ; name_label= master
                ; vms=
                    [
                      {basic_vm with memory= gib 120L; vm_name_label= vm1}
                    ; {basic_vm with memory= gib 120L; vm_name_label= vm2}
                    ]
                }
            ; slaves= [{memory_total= gib 256L; name_label= slave; vms= []}]
            ; ha_host_failures_to_tolerate= 1L
            }
          , {
              basic_vm with
              ha_always_run= false
            ; ha_restart_priority= "restart"
            ; memory= gib 120L
            ; vm_name_label= vm2
            }
          )
        , Error
            Api_errors.(
              Server_error (ha_operation_would_break_failover_plan, [])
            )
        )
      ; (* 2 host pool which is already overcommitted. Attempting to add another VM
                * should not throw an exception. *)
        ( ( {
              basic_pool with
              master=
                {
                  memory_total= gib 256L
                ; name_label= master
                ; vms=
                    [
                      {basic_vm with memory= gib 120L; vm_name_label= vm1}
                    ; {basic_vm with memory= gib 120L; vm_name_label= vm2}
                    ]
                }
            ; slaves=
                [
                  {
                    memory_total= gib 256L
                  ; name_label= slave
                  ; vms= [{basic_vm with memory= gib 120L; vm_name_label= vm1}]
                  }
                ]
            ; ha_host_failures_to_tolerate= 1L
            }
          , {
              basic_vm with
              ha_always_run= false
            ; ha_restart_priority= "restart"
            ; memory= gib 120L
            ; vm_name_label= vm2
            }
          )
        , Ok ()
        )
      ]
end)

module ComputeMaxFailures = Generic.MakeStateful (struct
  module Io = struct
    type input_t = pool

    type output_t = int

    let string_of_input_t = string_of_pool

    let string_of_output_t = string_of_int
  end

  module State = Test_state.XapiDb

  let load_input __context = setup ~__context

  let extract_output __context pool =
    let max_hosts =
      Xapi_ha_vm_failover.compute_max_host_failures_to_tolerate ~__context ()
    in
    (* Struct requires input_t but not used here *)
    pool |> ignore ; Int64.to_int max_hosts

  let tests =
    `QuickAndAutoDocumented
      [
        (* Three host pool with no VMs. *)
        ( {
            basic_pool with
            master= {memory_total= gib 256L; name_label= master; vms= []}
          ; slaves=
              [
                {memory_total= gib 256L; name_label= slave1; vms= []}
              ; {memory_total= gib 256L; name_label= slave2; vms= []}
              ]
          ; (* Placeholder value that is overridden when we call the compute function *)
            ha_host_failures_to_tolerate= 3L
          ; cluster= 3
          }
        , (* Assert that compute ha host failures to tolerate returns 1 *)
          1
        )
      ; (* Two hosts pool with no VMs  *)
        ( {
            basic_pool with
            master= {memory_total= gib 256L; name_label= master; vms= []}
          ; slaves= [{memory_total= gib 256L; name_label= slave1; vms= []}]
          ; ha_host_failures_to_tolerate= 2L
          ; cluster= 2
          }
        , (* Assert that compute ha host failures to tolerate returns 0 *)
          0
        )
      ; (* Two host pool with one down  *)
        ( {
            basic_pool with
            master= {memory_total= gib 256L; name_label= master; vms= []}
          ; slaves= [{memory_total= gib 256L; name_label= slave1; vms= []}]
          ; ha_host_failures_to_tolerate= 2L
          ; cluster= 1
          }
        , (* Assert that compute ha host failures to tolerate returns 1 *)
          1
        )
      ]
end)

let extract_output_for_anti_affinity_plan __context anti_affinity_plan =
  let slv1 =
    Db.Host.get_all ~__context
    |> List.find (fun self -> Db.Host.get_name_label ~__context ~self = slave1)
  in
  let slave1_anti_affinity_vms =
    Db.Host.get_resident_VMs ~__context ~self:slv1
    |> List.map (fun self -> (self, Db.VM.get_record ~__context ~self))
    |> List.filter (fun (_, record) -> not record.API.vM_is_control_domain)
    |> List.map (fun (self, record) ->
           (self, Xapi_ha_vm_failover.vm_memory ~__context record)
       )
    |> Xapi_ha_vm_failover.anti_affinity_vms_increasing ~__context
  in
  try
    anti_affinity_plan ~__context slave1_anti_affinity_vms []
    |> List.map (fun (vm, host) ->
           ( Db.VM.get_name_label ~__context ~self:vm
           , Db.Host.get_name_label ~__context ~self:host
           )
       )
  with Api_errors.Server_error ("NO_HOSTS_AVAILABLE", []) as e ->
    [("Anti-affinity VMs plan failed", Printexc.to_string e)]

let anti_affinity_grp1 = {name_label= grp1; placement= AntiAffinity}

let anti_affinity_plan_test_cases =
  [
    (* Test 0: No VMs in slave1 to be evacuated. *)
    ( {
        basic_pool with
        master= {memory_total= gib 256L; name_label= master; vms= []}
      ; slaves=
          [
            {memory_total= gib 256L; name_label= slave1; vms= []}
          ; {memory_total= gib 256L; name_label= slave2; vms= []}
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      []
    , (* Assert that no_breach_plan returns as expected *)
      []
    )
  ; (* Test 1: No anti-affinity VMs in slave1 to be evacuated *)
    ( {
        basic_pool with
        master= {memory_total= gib 256L; name_label= master; vms= []}
      ; slaves=
          [
            {
              memory_total= gib 256L
            ; name_label= slave1
            ; vms=
                [
                  {basic_vm with memory= gib 120L; vm_name_label= vm1}
                ; {basic_vm with memory= gib 120L; vm_name_label= vm2}
                ]
            }
          ; {memory_total= gib 256L; name_label= slave2; vms= []}
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      []
    , (* Assert that no_breach_plan returns as expected *)
      []
    )
  ; (* Test 2: One anti-affinity VM in slave1 to be evacuated *)
    ( {
        basic_pool with
        master= {memory_total= gib 512L; name_label= master; vms= []}
      ; slaves=
          [
            {
              memory_total= gib 256L
            ; name_label= slave1
            ; vms=
                [
                  {
                    basic_vm with
                    memory= gib 120L
                  ; vm_name_label= vm1_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {basic_vm with memory= gib 120L; vm_name_label= vm1}
                ]
            }
          ; {memory_total= gib 256L; name_label= slave2; vms= []}
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      [(vm1_grp1, slave2)]
    , (* Assert that no_breach_plan returns as expected *)
      [(vm1_grp1, slave2)]
    )
  ; (* Test 3: One anti-affinity VM in slave1 to be evacuated, the smallest host already has anti-affinity VM in the same group *)
    ( {
        basic_pool with
        master= {memory_total= gib 512L; name_label= master; vms= []}
      ; slaves=
          [
            {
              memory_total= gib 256L
            ; name_label= slave1
            ; vms=
                [
                  {
                    basic_vm with
                    memory= gib 120L
                  ; vm_name_label= vm1_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {basic_vm with memory= gib 120L; vm_name_label= "vm2"}
                ]
            }
          ; {
              memory_total= gib 256L
            ; name_label= slave2
            ; vms=
                [
                  {
                    basic_vm with
                    memory= gib 120L
                  ; vm_name_label= vm_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ]
            }
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      [(vm1_grp1, master)]
    , (* Assert that no_breach_plan returns as expected *)
      [(vm1_grp1, master)]
    )
  ; (* Test 4: Two anti-affinity VMs belong to one group in slave1 to be evacuated *)
    ( {
        basic_pool with
        master= {memory_total= gib 512L; name_label= master; vms= []}
      ; slaves=
          [
            {
              memory_total= gib 256L
            ; name_label= slave1
            ; vms=
                [
                  {
                    basic_vm with
                    memory= gib 120L
                  ; vm_name_label= vm1_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 130L
                  ; vm_name_label= vm2_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ]
            }
          ; {memory_total= gib 256L; name_label= slave2; vms= []}
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      [(vm2_grp1, master); (vm1_grp1, slave2)]
    , (* Assert that no_breach_plan returns as expected *)
      [(vm2_grp1, master); (vm1_grp1, slave2)]
    )
  ; (* Test 5: Two anti-affinity VMs belong to one group in slave1 to be evacuated, only 1 can be planed *)
    ( {
        basic_pool with
        master= {memory_total= gib 512L; name_label= master; vms= []}
      ; slaves=
          [
            {
              memory_total= gib 256L
            ; name_label= slave1
            ; vms=
                [
                  {
                    basic_vm with
                    memory= gib 120L
                  ; vm_name_label= vm1_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 513L
                  ; vm_name_label= vm2_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ]
            }
          ; {memory_total= gib 256L; name_label= slave2; vms= []}
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      [
        ( "Anti-affinity VMs plan failed"
        , "Server_error(NO_HOSTS_AVAILABLE, [  ])"
        )
      ]
    , (* Assert that no_breach_plan returns as expected *)
      [(vm1_grp1, slave2)]
    )
  ; (* Test 6: 6 anti-affinity VMs belong to one group in slave1 to be evacuated, only 5 can be planned *)
    ( {
        basic_pool with
        master= {memory_total= gib 640L; name_label= master; vms= []}
      ; slaves=
          [
            {
              memory_total= gib 256L
            ; name_label= slave1
            ; vms=
                [
                  {
                    basic_vm with
                    memory= gib 120L
                  ; vm_name_label= vm2_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 60L
                  ; vm_name_label= vm1_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 400L
                  ; vm_name_label= vm6_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 250L
                  ; vm_name_label= vm4_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 260L
                  ; vm_name_label= vm5_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 130L
                  ; vm_name_label= vm3_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ]
            }
          ; {memory_total= gib 256L; name_label= slave2; vms= []}
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      [
        ( "Anti-affinity VMs plan failed"
        , "Server_error(NO_HOSTS_AVAILABLE, [  ])"
        )
      ]
    , (* Assert that no_breach_plan returns as expected *)
      [(vm2_grp1, master); (vm1_grp1, slave2)]
    )
  ; (* Test 7: Two groups anti-affinity VMs in slave1 to be evacuated *)
    ( {
        basic_pool with
        master= {memory_total= gib 512L; name_label= master; vms= []}
      ; slaves=
          [
            {
              memory_total= gib 256L
            ; name_label= slave1
            ; vms=
                [
                  {
                    basic_vm with
                    memory= gib 120L
                  ; vm_name_label= vm6_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 60L
                  ; vm_name_label= vm5_grp2
                  ; groups= [{name_label= grp2; placement= AntiAffinity}]
                  }
                ; {
                    basic_vm with
                    memory= gib 130L
                  ; vm_name_label= vm7_grp2
                  ; groups= [{name_label= grp2; placement= AntiAffinity}]
                  }
                ; {
                    basic_vm with
                    memory= gib 1L
                  ; vm_name_label= vm1_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 2L
                  ; vm_name_label= vm2_grp2
                  ; groups= [{name_label= grp2; placement= AntiAffinity}]
                  }
                ; {
                    basic_vm with
                    memory= gib 3L
                  ; vm_name_label= vm3_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 4L
                  ; vm_name_label= vm4_grp2
                  ; groups= [{name_label= grp2; placement= AntiAffinity}]
                  }
                ]
            }
          ; {memory_total= gib 256L; name_label= slave2; vms= []}
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      [
        (vm7_grp2, master)
      ; (vm6_grp1, slave2)
      ; (vm5_grp2, slave2)
      ; (vm4_grp2, master)
      ; (vm3_grp1, master)
      ; (vm2_grp2, slave2)
      ; (vm1_grp1, slave2)
      ]
    , (* Assert that no_breach_plan returns as expected *)
      [
        (vm4_grp2, master)
      ; (vm3_grp1, master)
      ; (vm2_grp2, slave2)
      ; (vm1_grp1, slave2)
      ]
    )
  ; (* Test 8: Two groups anti-affinity VMs in slave1 to be evacuated, master is bigger than slave2 in size when started, but becomes smaller during planning *)
    ( {
        basic_pool with
        master= {memory_total= gib 512L; name_label= master; vms= []}
      ; slaves=
          [
            {
              memory_total= gib 256L
            ; name_label= slave1
            ; vms=
                [
                  {
                    basic_vm with
                    memory= gib 120L
                  ; vm_name_label= vm6_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 60L
                  ; vm_name_label= vm5_grp2
                  ; groups= [{name_label= grp2; placement= AntiAffinity}]
                  }
                ; {
                    basic_vm with
                    memory= gib 130L
                  ; vm_name_label= vm7_grp2
                  ; groups= [{name_label= grp2; placement= AntiAffinity}]
                  }
                ; {
                    basic_vm with
                    memory= gib 1L
                  ; vm_name_label= vm1_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 6L
                  ; vm_name_label= vm3_grp2
                  ; groups= [{name_label= grp2; placement= AntiAffinity}]
                  }
                ; {
                    basic_vm with
                    memory= gib 5L
                  ; vm_name_label= vm2_grp1
                  ; groups= [anti_affinity_grp1]
                  }
                ; {
                    basic_vm with
                    memory= gib 7L
                  ; vm_name_label= vm4_grp2
                  ; groups= [{name_label= grp2; placement= AntiAffinity}]
                  }
                ]
            }
          ; {memory_total= gib 510L; name_label= slave2; vms= []}
          ]
      }
    , (* Assert that spread_evenly_plan returns as expected *)
      [
        (vm7_grp2, slave2)
      ; (vm6_grp1, master)
      ; (vm5_grp2, master)
      ; (vm4_grp2, slave2)
      ; (vm3_grp2, master)
      ; (vm2_grp1, master)
      ; (vm1_grp1, slave2)
      ]
    , (* Assert that no_breach_plan returns as expected *)
      [
        (vm4_grp2, slave2)
      ; (vm3_grp2, master)
      ; (vm2_grp1, master)
      ; (vm1_grp1, slave2)
      ]
    )
  ]

module Slave1EvacuationVMAntiAffinitySpreadEvenlyPlan =
Generic.MakeStateful (struct
  module Io = struct
    type input_t = pool

    type output_t = (string * string) list

    let string_of_input_t = string_of_pool

    let string_of_output_t = Test_printers.(list (pair string string))
  end

  module State = Test_state.XapiDb

  let load_input __context = setup ~__context

  let extract_output __context _ =
    let slv1 =
      Db.Host.get_all ~__context
      |> List.find (fun self -> Db.Host.get_name_label ~__context ~self = slave1)
    in
    let hosts =
      Db.Host.get_all ~__context
      |> List.filter (fun h ->
             h <> slv1
             && Db.Host.get_name_label ~__context ~self:h <> "localhost"
         )
      |> List.map (fun host ->
             (host, Xapi_ha_vm_failover.host_free_memory ~__context ~host)
         )
    in
    let pool_state =
      Xapi_ha_vm_failover.init_spread_evenly_plan_pool_state ~__context hosts
    in
    extract_output_for_anti_affinity_plan __context
      (Xapi_ha_vm_failover.compute_spread_evenly_plan pool_state)

  let tests =
    `QuickAndAutoDocumented
      (anti_affinity_plan_test_cases
      |> List.map (fun (pool, spread_evenly_plan, _no_breach_plan) ->
             (pool, spread_evenly_plan)
         )
      )
end)

module Slave1EvacuationVMAntiAffinityNoBreachPlan = Generic.MakeStateful (struct
  module Io = struct
    type input_t = pool

    type output_t = (string * string) list

    let string_of_input_t = string_of_pool

    let string_of_output_t = Test_printers.(list (pair string string))
  end

  module State = Test_state.XapiDb

  let load_input __context = setup ~__context

  let extract_output __context _ =
    let total_hosts =
      Db.Host.get_all ~__context
      |> List.filter (fun h ->
             Db.Host.get_name_label ~__context ~self:h <> "localhost"
         )
      |> List.length
    in
    let slv1 =
      Db.Host.get_all ~__context
      |> List.find (fun self -> Db.Host.get_name_label ~__context ~self = slave1)
    in
    let hosts =
      Db.Host.get_all ~__context
      |> List.filter (fun h ->
             h <> slv1
             && Db.Host.get_name_label ~__context ~self:h <> "localhost"
         )
      |> List.map (fun host ->
             (host, Xapi_ha_vm_failover.host_free_memory ~__context ~host)
         )
    in
    let pool_state =
      Xapi_ha_vm_failover.init_spread_evenly_plan_pool_state ~__context hosts
      |> Xapi_ha_vm_failover.init_no_breach_plan_pool_state
    in
    extract_output_for_anti_affinity_plan __context
      (Xapi_ha_vm_failover.compute_no_breach_plan total_hosts pool_state)

  let tests =
    `QuickAndAutoDocumented
      (anti_affinity_plan_test_cases
      |> List.map (fun (pool, _spread_evenly_plan, no_breach_plan) ->
             (pool, no_breach_plan)
         )
      )
end)

module Slave1EvacuationPlan = Generic.MakeStateful (struct
  module Io = struct
    type input_t = pool

    type output_t = (string * string) list

    let string_of_input_t = string_of_pool

    let string_of_output_t = Test_printers.(list (pair string string))
  end

  module State = Test_state.XapiDb

  let load_input __context = setup ~__context

  let extract_output __context _ =
    let all_hosts =
      Db.Host.get_all ~__context
      |> List.filter (fun h ->
             Db.Host.get_name_label ~__context ~self:h <> "localhost"
         )
    in
    let slv1 =
      Db.Host.get_all ~__context
      |> List.find (fun self -> Db.Host.get_name_label ~__context ~self = slave1)
    in
    let slave1_vms =
      Db.Host.get_resident_VMs ~__context ~self:slv1
      |> List.map (fun self -> (self, Db.VM.get_record ~__context ~self))
      |> List.filter (fun (_, record) -> not record.API.vM_is_control_domain)
      |> List.map (fun (self, record) ->
             (self, Xapi_ha_vm_failover.vm_memory ~__context record)
         )
    in
    let hosts =
      all_hosts
      |> List.filter (( <> ) slv1)
      |> List.map (fun host ->
             (host, Xapi_ha_vm_failover.host_free_memory ~__context ~host)
         )
    in
    Xapi_ha_vm_failover.compute_anti_affinity_evacuation_plan ~__context
      (List.length all_hosts) hosts slave1_vms
    |> List.map (fun (vm, host) ->
           ( Db.VM.get_name_label ~__context ~self:vm
           , Db.Host.get_name_label ~__context ~self:host
           )
       )

  let tests =
    `QuickAndAutoDocumented
      [
        (* Test 0: Spread evenly plan is taken. *)
        ( {
            basic_pool with
            master= {memory_total= gib 200L; name_label= master; vms= []}
          ; slaves=
              [
                {
                  memory_total= gib 256L
                ; name_label= slave1
                ; vms=
                    [
                      {
                        basic_vm with
                        memory= gib 24L
                      ; vm_name_label= vm4_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ; {
                        basic_vm with
                        memory= gib 23L
                      ; vm_name_label= vm3_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ; {
                        basic_vm with
                        memory= gib 22L
                      ; vm_name_label= vm2_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ; {
                        basic_vm with
                        memory= gib 21L
                      ; vm_name_label= vm1_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ]
                }
              ; {memory_total= gib 60L; name_label= slave2; vms= []}
              ]
          }
        , (* Assert that spread_evenly_plan is taken. *)
          [
            (vm4_grp1, master)
          ; (vm3_grp1, slave2)
          ; (vm2_grp1, master)
          ; (vm1_grp1, slave2)
          ]
        )
        (* Test 1: No breach plan is taken. *)
      ; ( {
            basic_pool with
            master= {memory_total= gib 100L; name_label= master; vms= []}
          ; slaves=
              [
                {
                  memory_total= gib 256L
                ; name_label= slave1
                ; vms=
                    [
                      {basic_vm with memory= gib 85L; vm_name_label= vm1}
                    ; {basic_vm with memory= gib 65L; vm_name_label= vm2}
                    ; {
                        basic_vm with
                        memory= gib 30L
                      ; vm_name_label= vm3_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ; {
                        basic_vm with
                        memory= gib 20L
                      ; vm_name_label= vm2_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ; {
                        basic_vm with
                        memory= gib 10L
                      ; vm_name_label= vm1_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ]
                }
              ; {memory_total= gib 90L; name_label= slave2; vms= []}
              ; {memory_total= gib 70L; name_label= slave3; vms= []}
              ]
          ; keep_localhost= true
          }
        , (* Assert that no-breach-plan is taken *)
          [
            (vm2_grp1, slave2)
          ; (vm1_grp1, slave3)
          ; (vm3_grp1, slave3)
          ; (vm2, slave2)
          ; (vm1, master)
          ]
        )
        (* Test 2: Fallback to binpack plan. *)
      ; ( {
            basic_pool with
            master= {memory_total= gib 100L; name_label= master; vms= []}
          ; slaves=
              [
                {
                  memory_total= gib 256L
                ; name_label= slave1
                ; vms=
                    [
                      {basic_vm with memory= gib 95L; vm_name_label= vm1}
                    ; {basic_vm with memory= gib 75L; vm_name_label= vm2}
                    ; {
                        basic_vm with
                        memory= gib 30L
                      ; vm_name_label= vm3_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ; {
                        basic_vm with
                        memory= gib 20L
                      ; vm_name_label= vm2_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ; {
                        basic_vm with
                        memory= gib 10L
                      ; vm_name_label= vm1_grp1
                      ; groups= [anti_affinity_grp1]
                      }
                    ]
                }
              ; {memory_total= gib 80L; name_label= slave2; vms= []}
              ; {memory_total= gib 70L; name_label= slave3; vms= []}
              ]
          ; keep_localhost= true
          }
        , (* Assert that binpack-plan is taken *)
          [
            (vm1_grp1, slave3)
          ; (vm2_grp1, slave3)
          ; (vm3_grp1, slave3)
          ; (vm2, slave2)
          ; (vm1, master)
          ]
        )
      ]
end)

let tests =
  [
    ("plan_for_n_failures", PlanForNFailures.tests)
  ; ( "anti-affinity spread evenly plan"
    , Slave1EvacuationVMAntiAffinitySpreadEvenlyPlan.tests
    )
  ; ( "anti-affinity no breach plan"
    , Slave1EvacuationVMAntiAffinityNoBreachPlan.tests
    )
  ; ( "3 phases plan: spread evenly plan, no breach plan, binpacking plan"
    , Slave1EvacuationPlan.tests
    )
  ]
