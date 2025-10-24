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

open Network_device_order
open Network_interface

let pci_addr0 = Pciaddr.of_string "0000:01:0f.0" |> Result.get_ok

let pci_addr1 = Pciaddr.of_string "0000:01:0f.1" |> Result.get_ok

let pci_addr2 = Pciaddr.of_string "0000:01:0f.2" |> Result.get_ok

let pci_addr3 = Pciaddr.of_string "0000:01:0f.3" |> Result.get_ok

let pci_addr4 = Pciaddr.of_string "0000:05:0f.0" |> Result.get_ok

let mac_addr0 = Macaddr.of_string "ec:f4:bb:e6:d7:b8" |> Result.get_ok

let mac_addr1 = Macaddr.of_string "ec:f4:bb:e6:d7:b9" |> Result.get_ok

let mac_addr2 = Macaddr.of_string "ec:f4:bb:e6:d7:ba" |> Result.get_ok

let mac_addr3 = Macaddr.of_string "ec:f4:bb:e6:d7:bb" |> Result.get_ok

let mac_addr4 = Macaddr.of_string "00:02:c9:ed:fd:f0" |> Result.get_ok

let mac_addr5 = Macaddr.of_string "00:02:c9:ed:fd:f1" |> Result.get_ok

let name0 = "eno1"

let name1 = "eno2"

let name2 = "eno3"

let name3 = "eno4"

let name4 = "enp5s0"

let name5 = "enp5s0d1"

let seen_dev0 =
  {name= name0; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}

let seen_dev1 =
  {name= name1; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}

let seen_dev2 =
  {name= name2; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}

let seen_dev3 =
  {name= name3; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}

let seen_dev4 =
  {name= name4; pci= pci_addr4; mac= mac_addr4; position= 4; present= true}

let seen_dev5 =
  {name= name5; pci= pci_addr4; mac= mac_addr5; position= 5; present= true}

let dev0 =
  {
    Dev.name= name0
  ; pci= pci_addr0
  ; mac= mac_addr0
  ; bios_eth_order= 0
  ; multi_nic= false
  }

let dev1 =
  {
    Dev.name= name1
  ; pci= pci_addr1
  ; mac= mac_addr1
  ; bios_eth_order= 1
  ; multi_nic= false
  }

let dev2 =
  {
    Dev.name= name2
  ; pci= pci_addr2
  ; mac= mac_addr2
  ; bios_eth_order= 2
  ; multi_nic= false
  }

let dev3 =
  {
    Dev.name= name3
  ; pci= pci_addr3
  ; mac= mac_addr3
  ; bios_eth_order= 3
  ; multi_nic= false
  }

let dev4 =
  {
    Dev.name= name4
  ; pci= pci_addr4
  ; mac= mac_addr4
  ; bios_eth_order= 4
  ; multi_nic= true (* multinic: share PCI address with dev4 *)
  }

let dev5 =
  {
    Dev.name= name5
  ; pci= pci_addr4
  ; mac= mac_addr5
  ; bios_eth_order= 5
  ; multi_nic= true (* multinic: share PCI address with dev4 *)
  }

let plug dev devices = List.cons dev devices

let unplug d devices = List.filter (fun dev -> dev.Dev.mac <> d.Dev.mac) devices

let pos_of_mac mac order =
  match List.find_opt (fun dev -> dev.mac = mac) order with
  | Some {position; _} ->
      position
  | _ ->
      -1

let present_of_mac mac order =
  match List.find_opt (fun dev -> dev.mac = mac) order with
  | Some {present; _} ->
      present
  | _ ->
      failwith "Can't find the device!"

let test_postion_and_present expected_position expected_present dev order =
  let mac = dev.Dev.mac in
  let name = Format.asprintf "Position assigned for %a" Macaddr.pp mac in
  Alcotest.(check int) name expected_position (pos_of_mac mac order) ;
  Alcotest.(check bool) name expected_present (present_of_mac mac order)

let test_default () =
  let currents = [dev0; dev1; dev2; dev3; dev4; dev5] in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  (* The dev4 and dev5 are multinic functions. To assign initial positions,
     they are sorted by MAC addresses. *)
  test_postion_and_present 4 true dev4 order ;
  test_postion_and_present 5 true dev5 order

let test_unstable_bios_eth_order () =
  let dev4 = {dev4 with mac= mac_addr5; bios_eth_order= 9} in
  let dev5 = {dev5 with mac= mac_addr4; bios_eth_order= 10} in
  let currents = [dev0; dev1; dev2; dev3; dev4; dev5] in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  (* The dev4 and dev5 are multinic functions. To assign initial positions,
     they are sorted by MAC addresses. *)
  test_postion_and_present 5 true dev4 order ;
  test_postion_and_present 4 true dev5 order

let test_initial_rules_via_mac () =
  let currents = [dev0; dev1; dev2; dev3; dev4; dev5] in
  let rules =
    Rule.
      [
        {position= 0; index= Mac_addr mac_addr5}
      ; {position= 1; index= Mac_addr mac_addr4}
      ; {position= 2; index= Mac_addr mac_addr3}
      ; {position= 3; index= Mac_addr mac_addr2}
      ; {position= 4; index= Mac_addr mac_addr1}
      ; {position= 5; index= Mac_addr mac_addr0}
      ]
  in
  let order = sort' ~currents ~rules ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 5 true dev0 order ;
  test_postion_and_present 4 true dev1 order ;
  test_postion_and_present 3 true dev2 order ;
  test_postion_and_present 2 true dev3 order ;
  test_postion_and_present 1 true dev4 order ;
  test_postion_and_present 0 true dev5 order

let test_initial_rules_via_label () =
  let currents = [dev0; dev1; dev2; dev3; dev4; dev5] in
  let rules =
    Rule.
      [
        {position= 0; index= Label name5}
      ; {position= 1; index= Label name4}
      ; {position= 2; index= Label name3}
      ; {position= 3; index= Label name2}
      ; {position= 4; index= Label name1}
      ; {position= 5; index= Label name0}
      ]
  in
  let order = sort' ~currents ~rules ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 5 true dev0 order ;
  test_postion_and_present 4 true dev1 order ;
  test_postion_and_present 3 true dev2 order ;
  test_postion_and_present 2 true dev3 order ;
  test_postion_and_present 1 true dev4 order ;
  test_postion_and_present 0 true dev5 order

let test_replacement () =
  let mac_addr0' = Macaddr.of_string "fc:f4:bb:e6:d7:b8" |> Result.get_ok in
  let mac_addr1' = Macaddr.of_string "fc:f4:bb:e6:d7:b9" |> Result.get_ok in
  let dev0' =
    {
      Dev.name= "eno10"
    ; pci= pci_addr0
    ; mac= mac_addr0'
    ; bios_eth_order=
        1 (* this order is not expected to take effect in this case *)
    ; multi_nic= false
    }
  in
  let dev1' =
    {
      Dev.name= "eno11"
    ; pci= pci_addr1
    ; mac= mac_addr1'
    ; bios_eth_order=
        0 (* this order is not expected to take effect in this case *)
    ; multi_nic= false
    }
  in
  let last_order =
    [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4; seen_dev5]
  in
  let currents =
    [dev0; dev1; dev2; dev3; dev4; dev5]
    |> unplug dev0
    |> plug dev0'
    |> unplug dev1
    |> plug dev1'
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;

  test_postion_and_present 0 true dev0' order ;
  test_postion_and_present 1 true dev1' order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4 order ;
  test_postion_and_present 5 true dev5 order

let test_adding () =
  let pci_addr6 = Pciaddr.of_string "0000:06:0f.0" |> Result.get_ok in
  let mac_addr6 = Macaddr.of_string "fc:f4:bb:e6:d7:b8" |> Result.get_ok in
  let pci_addr7 = Pciaddr.of_string "0000:06:0f.1" |> Result.get_ok in
  let mac_addr7 = Macaddr.of_string "fc:f4:bb:e6:d7:b9" |> Result.get_ok in
  let dev6 =
    {
      Dev.name= "eno6"
    ; pci= pci_addr6
    ; mac= mac_addr6
    ; bios_eth_order= 1 (* This impacts the initial position *)
    ; multi_nic= false
    }
  in
  let dev7 =
    {
      Dev.name= "eno7"
    ; pci= pci_addr7
    ; mac= mac_addr7
    ; bios_eth_order= 0 (* This impacts the initial position *)
    ; multi_nic= false
    }
  in
  let last_order =
    [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4; seen_dev5]
  in
  (* Add two devices *)
  let currents =
    [dev0; dev1; dev2; dev3; dev4; dev5] |> plug dev6 |> plug dev7
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "8 devices in the order" 8 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4 order ;
  test_postion_and_present 5 true dev5 order ;
  (* The positions of newly added devices are impacted by the bios_eth_order *)
  test_postion_and_present 6 true dev7 order ;
  test_postion_and_present 7 true dev6 order

let test_removing () =
  let last_order =
    [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4; seen_dev5]
  in
  (* Remove two devices *)
  let currents =
    [dev0; dev1; dev2; dev3; dev4; dev5] |> unplug dev0 |> unplug dev1
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 0 false dev0 order ;
  test_postion_and_present 1 false dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4 order ;
  test_postion_and_present 5 true dev5 order

let test_replug_removed () =
  (* Mark the devices as removed. *)
  let seen_dev0 = {seen_dev0 with present= false} in
  let seen_dev1 = {seen_dev1 with present= false} in
  let last_order =
    [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4; seen_dev5]
  in
  let currents = [dev2; dev3; dev4; dev5] |> plug dev0 |> plug dev1 in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4 order ;
  test_postion_and_present 5 true dev5 order

let test_multi_nic_inplace_reorder () =
  (* The MAC addresses of multi_nic functions change *)
  let mac_addr4' = Macaddr.of_string "01:02:c9:ed:fd:f0" |> Result.get_ok in
  let mac_addr5' = Macaddr.of_string "01:02:c9:ed:fd:f1" |> Result.get_ok in
  let dev4' = Dev.{dev4 with mac= mac_addr4'; bios_eth_order= 5} in
  let dev5' = Dev.{dev5 with mac= mac_addr5'; bios_eth_order= 4} in
  let last_order =
    [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4; seen_dev5]
  in
  let currents = [dev0; dev1; dev2; dev3; dev4'; dev5'] in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4' order ;
  test_postion_and_present 5 true dev5' order

let test_multi_nic_new_devices () =
  let mac_addr6 = Macaddr.of_string "01:02:c9:ed:fd:f0" |> Result.get_ok in
  let mac_addr7 = Macaddr.of_string "01:02:c9:ed:fd:f1" |> Result.get_ok in
  let dev6 =
    Dev.
      {
        name= "enp5s0d2"
      ; pci= pci_addr4
      ; mac= mac_addr6
      ; bios_eth_order= 1
      ; multi_nic= true (* multinic: share PCI address with dev4 *)
      }
  in
  let dev7 =
    Dev.
      {
        name= "enp5s0d3"
      ; pci= pci_addr4
      ; mac= mac_addr7
      ; bios_eth_order= 0
      ; multi_nic= true (* multinic: share PCI address with dev4*)
      }
  in
  (* New devices are reported on the same PCI address.
     It's equivalent to plugging new devices but locate at the same PCI address. *)
  let last_order =
    [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4; seen_dev5]
  in
  let currents =
    [dev0; dev1; dev2; dev3; dev4; dev5] |> plug dev6 |> plug dev7
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "8 devices in the order" 8 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4 order ;
  test_postion_and_present 5 true dev5 order ;
  test_postion_and_present 6 true dev6 order ;
  test_postion_and_present 7 true dev7 order

let test_pci_changes () =
  let move_bus_by_1 pci_addr = Xcp_pci.{pci_addr with bus= pci_addr.bus + 1} in
  let last_order =
    [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4; seen_dev5]
  in
  let currents =
    [dev0; dev1; dev2; dev3; dev4; dev5]
    |> List.map (fun dev -> Dev.{dev with pci= move_bus_by_1 dev.pci})
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4 order ;
  test_postion_and_present 5 true dev5 order

let test_pci_addr_compare () =
  let addr0 = Pciaddr.of_string "0000:01:0e.0" |> Result.get_ok in
  let addr1 = Pciaddr.of_string "0000:01:0e.0" |> Result.get_ok in
  let addr2 = Pciaddr.of_string "0000:01:0e.2" |> Result.get_ok in
  let addr3 = Pciaddr.of_string "0000:01:0e.3" |> Result.get_ok in
  let addr4 = Pciaddr.of_string "0000:01:0f.0" |> Result.get_ok in
  let addr5 = Pciaddr.of_string "0000:02:0f.0" |> Result.get_ok in
  let addr6 = Pciaddr.of_string "0001:02:0f.0" |> Result.get_ok in
  Alcotest.(check bool) "equal" true (Pciaddr.compare addr0 addr1 = 0) ;
  Alcotest.(check bool) "less than" true (Pciaddr.compare addr0 addr2 < 0) ;
  Alcotest.(check bool) "greater than" true (Pciaddr.compare addr3 addr2 > 0) ;
  Alcotest.(check bool) "greater than" true (Pciaddr.compare addr4 addr3 > 0) ;
  Alcotest.(check bool) "greater than" true (Pciaddr.compare addr5 addr4 > 0) ;
  Alcotest.(check bool) "less than" true (Pciaddr.compare addr6 addr5 > 0)

let tests =
  [
    ( "test_known_cases"
    , [
        ("test_default", `Quick, test_default)
      ; ("test_unstable_bios_eth_order", `Quick, test_unstable_bios_eth_order)
      ; ("test_initial_mapping_via_mac", `Quick, test_initial_rules_via_mac)
      ; ("test_initial_mapping_via_name", `Quick, test_initial_rules_via_label)
      ; ("test_replacement", `Quick, test_replacement)
      ; ("test_adding", `Quick, test_adding)
      ; ("test_removing", `Quick, test_removing)
      ; ("test_replug_removed", `Quick, test_replug_removed)
      ; ( "test_multi_nic_inplace_reorder"
        , `Quick
        , test_multi_nic_inplace_reorder
        )
      ; ("test_multi_nic_new_devices", `Quick, test_multi_nic_new_devices)
      ; ("test_pci_changes", `Quick, test_pci_changes)
      ; ("test_pci_addr_compare", `Quick, test_pci_addr_compare)
      ]
    )
  ]
