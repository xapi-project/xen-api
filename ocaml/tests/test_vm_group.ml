(*
 * Copyright (c) Cloud Software Group, Inc.
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

module T = Test_common

let test_associate_vm_with_vm_group () =
  let __context = T.make_test_database () in
  let rpc, session_id = Test_client.make_client_params ~__context in
  let vm1 = T.make_vm ~__context () in
  let vm2 = T.make_vm ~__context () in
  let vm3 = T.make_vm ~__context () in
  let vm_group = T.make_vm_group ~__context ~placement:`anti_affinity () in
  Client.Client.VM.set_groups ~rpc ~session_id ~self:vm1 ~value:[vm_group] ;
  Client.Client.VM.set_groups ~rpc ~session_id ~self:vm2 ~value:[vm_group] ;
  Client.Client.VM.set_groups ~rpc ~session_id ~self:vm3 ~value:[vm_group] ;
  let vms = Db.VM_group.get_VMs ~__context ~self:vm_group in
  let extract_vm_strings vms =
    List.sort String.compare (List.map Ref.string_of vms)
  in
  Alcotest.(check (slist string String.compare))
    "check VMs are in the group" (extract_vm_strings vms)
    (extract_vm_strings [vm1; vm2; vm3])

let test_vm_can_only_belong_to_one_group () =
  let __context = T.make_test_database () in
  let rpc, session_id = Test_client.make_client_params ~__context in
  let vm = T.make_vm ~__context () in
  let vm_group1 = T.make_vm_group ~__context ~placement:`anti_affinity () in
  let vm_group2 = T.make_vm_group ~__context ~placement:`anti_affinity () in
  Alcotest.check_raises "should fail"
    (Api_errors.Server_error (Api_errors.too_many_groups, []))
    (fun () ->
      Client.Client.VM.set_groups ~rpc ~session_id ~self:vm
        ~value:[vm_group1; vm_group2]
    )

let test =
  [
    ("test_associate_vm_with_vm_group", `Quick, test_associate_vm_with_vm_group)
  ; ( "test_vm_can_only_belong_to_one_group"
    , `Quick
    , test_vm_can_only_belong_to_one_group
    )
  ]
