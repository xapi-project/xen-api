(*
 * Copyright (C) Cloud Software Group
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

open Bechamel

let () =
  Suite_init.harness_init () ;
  Debug.set_level Syslog.Warning

let gib x = Int64.mul x (Int64.mul 1073741824L 1L)

let make_pool ~__context ~num_hosts ~num_vms =
  let open Test_common in
  let shared_sr = make_sr ~__context ~shared:true () in
  let shared_net = make_network ~__context ~bridge:"xenbr0" () in
  List.iter
    (fun host -> Db.Host.destroy ~__context ~self:host)
    (Db.Host.get_all ~__context) ;
  let hosts =
    Array.init num_hosts (fun i ->
        let local_sr = make_sr ~__context ~shared:false () in
        let local_net = make_network ~__context ~bridge:"xapi0" () in
        let host_ref =
          make_host ~__context ~name_label:(Printf.sprintf "host%d" i) ()
        in
        Db.Host.set_enabled ~__context ~self:host_ref ~value:true ;
        let metrics = Db.Host.get_metrics ~__context ~self:host_ref in
        Db.Host_metrics.set_live ~__context ~self:metrics ~value:true ;
        Db.Host_metrics.set_memory_total ~__context ~self:metrics
          ~value:(gib 256L) ;
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
    )
  in
  for i = 0 to num_vms - 1 do
    let host_ref = hosts.(i mod num_hosts) in
    let vm_ref =
      make_vm ~__context ~ha_always_run:true ~ha_restart_priority:"restart"
        ~memory_static_min:(gib 1L) ~memory_dynamic_min:(gib 1L)
        ~memory_dynamic_max:(gib 1L) ~memory_static_max:(gib 1L)
        ~name_label:(Printf.sprintf "vm%d" i) ()
    in
    Db.VM.set_power_state ~__context ~self:vm_ref ~value:`Running ;
    Db.VM.set_resident_on ~__context ~self:vm_ref ~value:host_ref ;
    let vdi_ref = make_vdi ~__context ~sR:shared_sr () in
    let (_ : API.ref_VBD) = make_vbd ~__context ~vM:vm_ref ~vDI:vdi_ref () in
    let (_ : API.ref_VIF) =
      make_vif ~__context ~vM:vm_ref ~network:shared_net
        ~device:(string_of_int i) ()
    in
    ()
  done ;
  let pool = Helpers.get_pool ~__context in
  let master_ref = hosts.(0) in
  Db.Pool.set_master ~__context ~self:pool ~value:master_ref ;
  Db.Pool.set_ha_enabled ~__context ~self:pool ~value:true ;
  Db.Pool.set_ha_host_failures_to_tolerate ~__context ~self:pool ~value:1L ;
  Db.Pool.set_ha_plan_exists_for ~__context ~self:pool ~value:1L

let allocate ~num_hosts ~num_vms () =
  let __context = Test_common.make_test_database () in
  make_pool ~__context ~num_hosts ~num_vms ;
  __context

let run_plan_for_n_failures __context =
  let all_protected_vms = Xapi_ha_vm_failover.all_protected_vms ~__context in
  let (_ : Xapi_ha_vm_failover.plan_status) =
    Xapi_ha_vm_failover.plan_for_n_failures ~__context ~all_protected_vms 1
  in
  ()

let benchmarks =
  let make ~num_hosts ~num_vms =
    let name =
      Printf.sprintf "plan_for_n_failures (%d hosts, %d VMs)" num_hosts num_vms
    in
    Test.make_with_resource ~name
      ~allocate:(allocate ~num_hosts ~num_vms)
      ~free:ignore Test.uniq
      (Staged.stage run_plan_for_n_failures)
  in
  [
    make ~num_hosts:3 ~num_vms:10
  ; make ~num_hosts:3 ~num_vms:50
  ; make ~num_hosts:3 ~num_vms:100
  ; make ~num_hosts:3 ~num_vms:500
  ; make ~num_hosts:8 ~num_vms:100
  ; make ~num_hosts:8 ~num_vms:500
  ]

let () = Bechamel_simple_cli.cli benchmarks
