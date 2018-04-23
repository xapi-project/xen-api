(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

let alco_power_state = Alcotest_comparators.from_rpc_of_t API.rpc_of_vm_power_state
let run_test vm =
  let (nameLabel, isControlDomain, powerState) = vm in
  let __context = Test_common.make_test_database () in
  let vm = Test_common.make_vm ~__context ~name_label:nameLabel ()
  in
  Db.VM.set_is_control_domain ~__context ~self:vm ~value:isControlDomain;
  Db.VM.set_power_state ~__context ~self:vm ~value:powerState;
  Db.VM.set_resident_on ~__context ~self:vm ~value:Ref.null;

  Dbsync_master.reset_vms_running_on_missing_hosts ~__context;

  Alcotest.check alco_power_state
    (Printf.sprintf "The VM %s is not halted" nameLabel)
    (Db.VM.get_power_state ~__context ~self:vm) `Halted

let test_cases = [
  ("vm1", false, `Running);
  ("vm2", false, `Halted);
  ("dom1", true, `Running);
  ("dom2", true, `Halted)
]

let test_reset_vms_on_missing_host () =
  List.iter run_test test_cases

let test =
  [ "test_reset_vms_on_missing_host", `Quick, test_reset_vms_on_missing_host
  ]
