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

open Storage_impl
open Vdi

let state_pp fmt state =
  Format.fprintf fmt "%s" (Vdi_automaton.string_of_state state)

let vdi_pp fmt {dps; leaked; dpv; _} =
  Format.fprintf fmt "{attach_info=_;dps=%a;leaked=%a;dpv=%a}"
    Fmt.(Dump.list @@ pair ~sep:comma string state_pp)
    dps
    Fmt.(Dump.list @@ string)
    leaked
    Fmt.(Dump.list @@ pair ~sep:comma string Storage_interface.vm_pp)
    dpv

let test_add_dp () =
  let vdi = Vdi.empty () in
  let dp = "vbd/1/vdaa" in
  let vm = Storage_interface.Vm.of_string "1" in
  let vdi =
    Vdi.add_or_update_dp dp vm (Vdi_automaton.Attached Vdi_automaton.RW) vdi
  in
  let vdi_record = Alcotest.testable vdi_pp ( = ) in
  Alcotest.(check @@ vdi_record)
    "dp has been added to vdi"
    {
      attach_info= None
    ; dps= [(dp, Vdi_automaton.Attached Vdi_automaton.RW)]
    ; leaked= []
    ; dpv= [(dp, vm)]
    }
    vdi

let test_add_dp_detached () =
  let vdi = Vdi.empty () in
  let dp = "vbd/1/vdaa" in
  let vm = Storage_interface.Vm.of_string "1" in
  let vdi = Vdi.add_or_update_dp dp vm Vdi_automaton.Detached vdi in
  let vdi_record = Alcotest.testable vdi_pp ( = ) in
  Alcotest.(check @@ vdi_record)
    "Detached dp is not added to the Vdi"
    {attach_info= None; dps= []; leaked= []; dpv= []}
    vdi

let test_remove_dp () =
  let vdi = Vdi.empty () in
  let dp = "vbd/1/vdaa" in
  let vm = Storage_interface.Vm.of_string "1" in
  let vdi =
    Vdi.add_or_update_dp dp vm (Vdi_automaton.Attached Vdi_automaton.RW) vdi
  in
  let vdi = Vdi.add_or_update_dp dp vm Vdi_automaton.Detached vdi in
  let vdi_record = Alcotest.testable vdi_pp ( = ) in
  Alcotest.(check @@ vdi_record)
    "dp has been added to vdi"
    {attach_info= None; dps= []; leaked= []; dpv= []}
    vdi

let test_bad_dp_query () =
  let vdi = Vdi.empty () in
  let vm = Vdi.get_dp_vm "I am not a real dp" vdi in
  let state = Vdi.get_dp_state "I am not a real dp" vdi in
  let vm_record = Alcotest.testable Storage_interface.vm_pp ( = ) in
  Alcotest.(check @@ vm_record)
    "Blank vm and Detached state are returned from a dp that is not on the vdi"
    (Storage_interface.Vm.of_string "")
    vm ;
  let state_record = Alcotest.testable state_pp ( = ) in
  Alcotest.(check @@ state_record)
    "Detached state returned from bad dp" Vdi_automaton.Detached state

let test_vm_query () =
  let vdi = Vdi.empty () in
  let dp = "vbd/1/vdaa" in
  let vm = Storage_interface.Vm.of_string "1" in
  let vdi =
    Vdi.add_or_update_dp dp vm (Vdi_automaton.Attached Vdi_automaton.RW) vdi
  in
  let vm = Vdi.get_dp_vm dp vdi in
  let vm_record = Alcotest.testable Storage_interface.vm_pp ( = ) in
  Alcotest.(check @@ vm_record)
    "Blank vm and Detached state are returned from a dp that is not on the vdi"
    (Storage_interface.Vm.of_string "1")
    vm

let test_modification =
  [
    ("Adding a dp to a Vdi", `Quick, test_add_dp)
  ; ("Adding a detached dp to a Vdi", `Quick, test_add_dp_detached)
  ; ("Removing a dp from a Vdi", `Quick, test_remove_dp)
  ]

let test_querying =
  [
    ("Querying for a non existing dp", `Quick, test_bad_dp_query)
  ; ("Querying for an existing dp", `Quick, test_vm_query)
  ]

let () =
  Alcotest.run "Testing storage_impl Vdi"
    [
      ("Testing add_or_replace_dp to modify dps on a vdi", test_modification)
    ; ("Testing get_dp_vm to query vms on a vdi", test_querying)
    ]
