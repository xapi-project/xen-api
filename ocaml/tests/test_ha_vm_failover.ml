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

open OUnit
open Stdext
open Test_common
open Test_highlevel

let ( *** ) = Int64.mul
let kib x = 1024L *** x
let mib x = x |> kib |> kib
let gib x = x |> kib |> kib |> kib

type vbd = {
  agile : bool;
}

type vif = {
  agile : bool;
}

type vm = {
  ha_always_run : bool;
  ha_restart_priority : string;
  memory : int64;
  name_label : string;
  vbds : vbd list;
  vifs : vif list;
}

let basic_vm = {
  ha_always_run = true;
  ha_restart_priority = "restart";
  memory = gib 1L;
  name_label = "vm";
  vbds = [{agile = true}];
  vifs = [{agile = true}];
}

type host = {
  memory_total : int64;
  name_label : string;
  vms : vm list;
}

type pool = {
  master: host;
  slaves: host list;
  ha_host_failures_to_tolerate: int64;
}

let string_of_vm {memory; name_label} =
  Printf.sprintf "{memory = %Ld; name_label = %S}" memory name_label

let string_of_host {memory_total; name_label; vms} =
  Printf.sprintf "{memory_total = %Ld; name_label = %S; vms = [%s]}"
    memory_total name_label
    (Test_printers.list string_of_vm vms)

let string_of_pool {master; slaves; ha_host_failures_to_tolerate} =
  Printf.sprintf
    "{master = %s; slaves = %s; ha_host_failures_to_tolerate = %Ld}"
    (string_of_host master)
    (Test_printers.list string_of_host slaves)
    ha_host_failures_to_tolerate

let load_vm ~__context ~(vm:vm) ~local_sr ~shared_sr ~local_net ~shared_net =
  let vm_ref = make_vm ~__context
      ~ha_always_run:vm.ha_always_run
      ~ha_restart_priority:vm.ha_restart_priority
      ~memory_static_min:vm.memory
      ~memory_dynamic_min:vm.memory
      ~memory_dynamic_max:vm.memory
      ~memory_static_max:vm.memory
      ~name_label:vm.name_label ()
  in
  let (_ : API.ref_VIF list) =
    List.mapi
      (fun index (vif:vif) ->
         make_vif ~__context ~device:(string_of_int index) ~vM:vm_ref
           ~network:(if vif.agile then shared_net else local_net) ())
      vm.vifs
  in
  let (_ : API.ref_VBD list) =
    List.mapi
      (fun index (vbd:vbd) ->
         let vdi_ref =
           make_vdi ~__context ~sR:(if vbd.agile then shared_sr else local_sr) ()
         in
         make_vbd ~__context ~device:(string_of_int index) ~vM:vm_ref
           ~vDI:vdi_ref ())
      vm.vbds
  in
  vm_ref

let load_host ~__context ~host ~local_sr ~shared_sr ~local_net ~shared_net =
  let host_ref = make_host ~__context ~name_label:host.name_label () in
  Db.Host.set_enabled ~__context ~self:host_ref ~value:true;
  let metrics = Db.Host.get_metrics ~__context ~self:host_ref in
  Db.Host_metrics.set_live ~__context ~self:metrics ~value:true;
  Db.Host_metrics.set_memory_total ~__context
    ~self:metrics ~value:host.memory_total;

  let (_ : API.ref_VM list) =
    List.map
      (fun vm ->
         load_vm ~__context ~vm ~local_sr ~shared_sr ~local_net ~shared_net)
      host.vms
  in
  host_ref

let setup ~__context {master; slaves; ha_host_failures_to_tolerate} =
  let shared_sr = make_sr ~__context ~shared:true () in
  let shared_net = make_network ~__context ~bridge:"xenbr0" () in

  (* Remove all hosts added by make_test_database *)
  List.iter (fun host -> Db.Host.destroy ~__context ~self:host) (Db.Host.get_all ~__context);

  let load_host_and_local_resources host =
    let local_sr = make_sr ~__context ~shared:false () in
    let local_net = make_network ~__context ~bridge:"xapi0" () in

    let host_ref =
      load_host ~__context ~host ~local_sr ~shared_sr ~local_net ~shared_net in

    let (_ : API.ref_PBD) =
      make_pbd ~__context ~host:host_ref ~sR:local_sr () in
    let (_ : API.ref_PBD) =
      make_pbd ~__context ~host:host_ref ~sR:shared_sr () in

    let (_ : API.ref_PIF) =
      make_pif ~__context ~host:host_ref ~network:local_net () in
    let (_ : API.ref_PIF) =
      make_pif ~__context ~host:host_ref ~network:shared_net () in
    host_ref
  in

  let master_ref = load_host_and_local_resources master in
  let (_ : API.ref_host list) = List.map load_host_and_local_resources slaves in

  let pool = Db.Pool.get_all ~__context |> List.hd in

  Db.Pool.set_master ~__context ~self:pool ~value:master_ref;
  Db.Pool.set_ha_enabled ~__context ~self:pool ~value:true;
  Db.Pool.set_ha_host_failures_to_tolerate ~__context ~self:pool ~value:ha_host_failures_to_tolerate;
  Db.Pool.set_ha_plan_exists_for ~__context ~self:pool ~value:ha_host_failures_to_tolerate


module AllProtectedVms = Generic.Make(Generic.EncapsulateState(struct
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

                                        let tests = [
                                          (* No VMs and a single host. *)
                                          {
                                            master = {memory_total = gib 256L; name_label = "master"; vms = []};
                                            slaves = [];
                                            ha_host_failures_to_tolerate = 0L;
                                          },
                                          [];
                                          (* One unprotected VM. *)
                                          {
                                            master = {
                                              memory_total = gib 256L; name_label = "master";
                                              vms = [{basic_vm with
                                                      ha_always_run = false;
                                                      ha_restart_priority = "";
                                                     }];
                                            };
                                            slaves = [];
                                            ha_host_failures_to_tolerate = 0L;
                                          },
                                          [];
                                          (* One VM which would be protected if it was running. *)
                                          {
                                            master = {
                                              memory_total = gib 256L; name_label = "master";
                                              vms = [{basic_vm with ha_always_run = false}];
                                            };
                                            slaves = [];
                                            ha_host_failures_to_tolerate = 0L;
                                          },
                                          [];
                                          (* One protected VM. *)
                                          {
                                            master = {
                                              memory_total = gib 256L; name_label = "master";
                                              vms = [basic_vm];
                                            };
                                            slaves = [];
                                            ha_host_failures_to_tolerate = 0L;
                                          },
                                          ["vm"];
                                          (* One protected VM and one unprotected VM. *)
                                          {
                                            master = {
                                              memory_total = gib 256L; name_label = "master";
                                              vms = [
                                                {basic_vm with name_label = "vm1"};
                                                {basic_vm with
                                                 ha_always_run = false;
                                                 ha_restart_priority = "";
                                                 name_label = "vm2"
                                                }
                                              ];
                                            };
                                            slaves = [];
                                            ha_host_failures_to_tolerate = 0L;
                                          },
                                          ["vm1"];
                                        ]
                                      end))

module PlanForNFailures = Generic.Make(Generic.EncapsulateState(struct
                                         module Io = struct
                                           open Xapi_ha_vm_failover

                                           type input_t = pool
                                           type output_t = result

                                           let string_of_input_t = string_of_pool
                                           let string_of_output_t = function
                                             | Plan_exists_for_all_VMs -> "Plan_exists_for_all_VMs"
                                             | Plan_exists_excluding_non_agile_VMs -> "Plan_exists_excluding_non_agile_VMs"
                                             | No_plan_exists -> "No_plan_exists"
                                         end

                                         module State = Test_state.XapiDb

                                         let load_input __context = setup ~__context

                                         let extract_output __context pool =
                                           let all_protected_vms = Xapi_ha_vm_failover.all_protected_vms ~__context in
                                           Xapi_ha_vm_failover.plan_for_n_failures ~__context
                                             ~all_protected_vms (Int64.to_int pool.ha_host_failures_to_tolerate)

                                         (* TODO: Add a test which causes plan_for_n_failures to return
                                            	 * Plan_exists_excluding_non_agile_VMs. *)
                                         let tests = [
                                           (* Two host pool with no VMs. *)
                                           (
                                             {
                                               master = {memory_total = gib 256L; name_label = "master"; vms = []};
                                               slaves = [
                                                 {memory_total = gib 256L; name_label = "slave"; vms = []}
                                               ];
                                               ha_host_failures_to_tolerate = 1L;
                                             },
                                             Xapi_ha_vm_failover.Plan_exists_for_all_VMs
                                           );
                                           (* Two host pool, with one VM taking up just under half of one host's
                                              		 * memory. *)
                                           (
                                             {
                                               master = {
                                                 memory_total = gib 256L; name_label = "master";
                                                 vms = [{basic_vm with
                                                         memory = gib 120L;
                                                         name_label = "vm1";
                                                        }];
                                               };
                                               slaves = [
                                                 {memory_total = gib 256L; name_label = "slave"; vms = []}
                                               ];
                                               ha_host_failures_to_tolerate = 1L;
                                             },
                                             Xapi_ha_vm_failover.Plan_exists_for_all_VMs
                                           );
                                           (* Two host pool, with two VMs taking up almost all of one host's memory. *)
                                           (
                                             {
                                               master = {
                                                 memory_total = gib 256L; name_label = "master";
                                                 vms = [
                                                   {basic_vm with
                                                    memory = gib 120L;
                                                    name_label = "vm1";
                                                   };
                                                   {basic_vm with
                                                    memory = gib 120L;
                                                    name_label = "vm2";
                                                   };
                                                 ];
                                               };
                                               slaves = [
                                                 {memory_total = gib 256L; name_label = "slave"; vms = []}
                                               ];
                                               ha_host_failures_to_tolerate = 1L;
                                             },
                                             Xapi_ha_vm_failover.Plan_exists_for_all_VMs
                                           );
                                           (* Two host pool, overcommitted. *)
                                           (
                                             {
                                               master = {
                                                 memory_total = gib 256L; name_label = "master";
                                                 vms = [
                                                   {basic_vm with
                                                    memory = gib 120L;
                                                    name_label = "vm1";
                                                   };
                                                   {basic_vm with
                                                    memory = gib 120L;
                                                    name_label = "vm2";
                                                   };
                                                 ];
                                               };
                                               slaves = [
                                                 {
                                                   memory_total = gib 256L; name_label = "slave";
                                                   vms = [
                                                     {basic_vm with
                                                      memory = gib 120L;
                                                      name_label = "vm3";
                                                     };
                                                     {basic_vm with
                                                      memory = gib 120L;
                                                      name_label = "vm4";
                                                     };
                                                   ];
                                                 }
                                               ];
                                               ha_host_failures_to_tolerate = 1L;
                                             },
                                             Xapi_ha_vm_failover.No_plan_exists
                                           );
                                         ]
                                       end))

module AssertNewVMPreservesHAPlan = Generic.Make(Generic.EncapsulateState(struct
                                                   module Io = struct
                                                     open Xapi_ha_vm_failover

                                                     type input_t = (pool * vm)
                                                     type output_t = (exn, unit) Either.t

                                                     let string_of_input_t = Test_printers.pair string_of_pool string_of_vm
                                                     let string_of_output_t = Test_printers.(either Printexc.to_string unit)
                                                   end

                                                   module State = Test_state.XapiDb

                                                   let load_input __context (pool, _) = setup ~__context pool

                                                   let extract_output __context (pool, vm) =
                                                     let open Db_filter_types in
                                                     let local_sr =
                                                       Db.SR.get_refs_where ~__context
                                                         ~expr:(Eq (Field "shared", Literal "false"))
                                                       |> List.hd
                                                     in
                                                     let shared_sr =
                                                       Db.SR.get_refs_where ~__context
                                                         ~expr:(Eq (Field "shared", Literal "true"))
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
                                                       load_vm ~__context ~vm ~local_sr ~shared_sr ~local_net ~shared_net in
                                                     try Either.Right
                                                           (Xapi_ha_vm_failover.assert_new_vm_preserves_ha_plan ~__context vm_ref)
                                                     with e -> Either.Left e

                                                   (* n.b. incoming VMs have ha_always_run = false; otherwise they will be
                                                      	 * included when computing the plan for the already-running VMs. *)
                                                   let tests = [
                                                     (* 2 host pool, one VM using just under half of one host's memory;
                                                        		 * test that another VM can be added. *)
                                                     (
                                                       {
                                                         master = {
                                                           memory_total = gib 256L; name_label = "master";
                                                           vms = [
                                                             {basic_vm with
                                                              memory = gib 120L;
                                                              name_label = "vm1";
                                                             };
                                                           ];
                                                         };
                                                         slaves = [
                                                           {memory_total = gib 256L; name_label = "slave"; vms = []}
                                                         ];
                                                         ha_host_failures_to_tolerate = 1L;
                                                       },
                                                       {basic_vm with
                                                        ha_always_run = false;
                                                        ha_restart_priority = "restart";
                                                        memory = gib 120L;
                                                        name_label = "vm2";
                                                       }
                                                     ),
                                                     Either.Right ();
                                                     (* 2 host pool, two VMs using almost all of one host's memory;
                                                        		 * test that another VM cannot be added. *)
                                                     (
                                                       {
                                                         master = {
                                                           memory_total = gib 256L; name_label = "master";
                                                           vms = [
                                                             {basic_vm with
                                                              memory = gib 120L;
                                                              name_label = "vm1";
                                                             };
                                                             {basic_vm with
                                                              memory = gib 120L;
                                                              name_label = "vm2";
                                                             };
                                                           ];
                                                         };
                                                         slaves = [
                                                           {memory_total = gib 256L; name_label = "slave"; vms = []}
                                                         ];
                                                         ha_host_failures_to_tolerate = 1L;
                                                       },
                                                       {basic_vm with
                                                        ha_always_run = false;
                                                        ha_restart_priority = "restart";
                                                        memory = gib 120L;
                                                        name_label = "vm2";
                                                       }
                                                     ),
                                                     Either.Left (Api_errors.(Server_error (ha_operation_would_break_failover_plan, [])));
                                                     (* 2 host pool which is already overcommitted. Attempting to add another VM
                                                        		 * should not throw an exception. *)
                                                     (
                                                       {
                                                         master = {
                                                           memory_total = gib 256L; name_label = "master";
                                                           vms = [
                                                             {basic_vm with
                                                              memory = gib 120L;
                                                              name_label = "vm1";
                                                             };
                                                             {basic_vm with
                                                              memory = gib 120L;
                                                              name_label = "vm2";
                                                             };
                                                           ];
                                                         };
                                                         slaves = [
                                                           {
                                                             memory_total = gib 256L; name_label = "slave";
                                                             vms = [
                                                               {basic_vm with
                                                                memory = gib 120L;
                                                                name_label = "vm1";
                                                               };
                                                             ]
                                                           };
                                                         ];
                                                         ha_host_failures_to_tolerate = 1L;
                                                       },
                                                       {basic_vm with
                                                        ha_always_run = false;
                                                        ha_restart_priority = "restart";
                                                        memory = gib 120L;
                                                        name_label = "vm2";
                                                       }
                                                     ),
                                                     Either.Right ();
                                                   ]
                                                 end))

let test =
  "test_ha_vm_failover" >:::
  [
    "test_all_protected_vms" >::: AllProtectedVms.tests;
    "test_plan_for_n_failures" >::: PlanForNFailures.tests;
    "test_assert_new_vm_preserves_ha_plan" >:::
    AssertNewVMPreservesHAPlan.tests;
  ]
