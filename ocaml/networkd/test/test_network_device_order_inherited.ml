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

let pos_of_mac mac_addr order =
  match List.find_opt (fun d -> d.mac = mac_addr) order with
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

let test_newhw_norules_1eth () =
  let mac_addr = Macaddr.of_string "ab:cd:ef:12:34:56" |> Result.get_ok in
  let dev0 =
    {
      Dev.name= "side-12-eth1"
    ; pci= Pciaddr.of_string "0000:00:0f.0" |> Result.get_ok
    ; mac= mac_addr
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let currents = [dev0] in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  test_postion_and_present 0 true dev0 order

let test_newhw_norules_2eth () =
  let dev0 =
    {
      Dev.name= "side-12-eth1"
    ; pci= Pciaddr.of_string "0000:00:0f.0" |> Result.get_ok
    ; mac= Macaddr.of_string "ab:cd:ef:12:34:56" |> Result.get_ok
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let dev1 =
    {
      Dev.name= "side-33-eth0"
    ; pci= Pciaddr.of_string "0000:00:01.0" |> Result.get_ok
    ; mac= Macaddr.of_string "ab:cd:ef:12:34:57" |> Result.get_ok
    ; bios_eth_order= 1
    ; multi_nic= false
    }
  in
  let currents = [dev0; dev1] in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "2 devices in the order" 2 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order

let test_newhw_2srule_2eth () =
  let mac_addr0 = Macaddr.of_string "12:34:56:78:90:12" |> Result.get_ok in
  let mac_addr1 = Macaddr.of_string "ab:cd:ef:12:34:56" |> Result.get_ok in
  let rules =
    Rule.
      [
        {position= 0; index= Mac_addr mac_addr1}
      ; {position= 1; index= Mac_addr mac_addr0}
      ]
  in
  let dev0 =
    {
      Dev.name= "eth0"
    ; pci= Pciaddr.of_string "0000:00:01.0" |> Result.get_ok
    ; mac= mac_addr0
    ; bios_eth_order= 1
    ; multi_nic= false
    }
  in
  let dev1 =
    {
      Dev.name= "side-12-eth1"
    ; pci= Pciaddr.of_string "0000:00:0f.0" |> Result.get_ok
    ; mac= mac_addr1
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let currents = [dev0; dev1] in
  let order = sort' ~currents ~rules ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  test_postion_and_present 1 true dev0 order ;
  test_postion_and_present 0 true dev1 order

let test_nosrules_1eth_incorrect_udev () =
  let mac_addr = Macaddr.of_string "ab:cd:ef:12:34:56" |> Result.get_ok in
  let pci_addr = Pciaddr.of_string "0000:00:0f.0" |> Result.get_ok in
  let dev0 =
    {
      Dev.name= "side-12-eth0"
    ; pci= pci_addr
    ; mac= mac_addr
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let currents = [dev0] in
  let seen_dev0 =
    {name= "eth2"; pci= pci_addr; mac= mac_addr; position= 3; present= true}
  in
  let last_order = [seen_dev0] in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  test_postion_and_present 3 true dev0 order

let test_1srule_1eth_1last_correct_udev () =
  let mac_addr = Macaddr.of_string "ab:cd:ef:12:34:56" |> Result.get_ok in
  let pci_addr = Pciaddr.of_string "0000:00:0f.0" |> Result.get_ok in
  let dev0 =
    {
      Dev.name= "eth1"
    ; pci= pci_addr
    ; mac= mac_addr
    ; bios_eth_order= 1
    ; multi_nic= false
    }
  in
  let currents = [dev0] in
  let rules = Rule.[{position= 0; index= Mac_addr mac_addr}] in
  let seen_dev0 =
    {name= "eth1"; pci= pci_addr; mac= mac_addr; position= 1; present= true}
  in
  let last_order = [seen_dev0] in
  let order = sort' ~currents ~rules ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  test_postion_and_present 0 true dev0 order

let test_1srule_1eth_already_complete () =
  let mac_addr = Macaddr.of_string "00:13:72:2d:2a:ec" |> Result.get_ok in
  let pci_addr = Pciaddr.of_string "0000:04:00.0" |> Result.get_ok in
  let dev0 =
    {
      Dev.name= "eth0"
    ; pci= pci_addr
    ; mac= mac_addr
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let currents = [dev0] in
  let rules = Rule.[{position= 0; index= Mac_addr mac_addr}] in
  let order = sort' ~currents ~rules ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  test_postion_and_present 0 true dev0 order

let test_1drule_1eth_already_complete () =
  let mac_addr = Macaddr.of_string "00:13:72:2d:2a:ec" |> Result.get_ok in
  let pci_addr = Pciaddr.of_string "0000:04:00.0" |> Result.get_ok in
  let dev0 =
    {
      Dev.name= "eth0"
    ; pci= pci_addr
    ; mac= mac_addr
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let currents = [dev0] in
  let seen_dev0 =
    {name= "eth0"; pci= pci_addr; mac= mac_addr; position= 0; present= true}
  in
  let last_order = [seen_dev0] in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  test_postion_and_present 0 true dev0 order

let test_usecase1 () =
  let mac_addr0 = Macaddr.of_string "01:23:45:67:89:01" |> Result.get_ok in
  let mac_addr1 = Macaddr.of_string "11:23:45:67:89:01" |> Result.get_ok in
  let mac_addr2 = Macaddr.of_string "21:23:45:67:89:01" |> Result.get_ok in
  let mac_addr3 = Macaddr.of_string "31:23:45:67:89:01" |> Result.get_ok in
  let mac_addr4 = Macaddr.of_string "41:23:45:67:89:01" |> Result.get_ok in
  let pci_addr0 = Pciaddr.of_string "0000:01:00.0" |> Result.get_ok in
  let pci_addr1 = Pciaddr.of_string "0000:02:00.0" |> Result.get_ok in
  let pci_addr2 = Pciaddr.of_string "0000:03:00.0" |> Result.get_ok in
  let pci_addr3 = Pciaddr.of_string "0000:04:00.0" |> Result.get_ok in
  let pci_addr4 = Pciaddr.of_string "0000:05:00.0" |> Result.get_ok in

  let dev0 =
    {
      Dev.name= "eth0"
    ; pci= pci_addr0
    ; mac= mac_addr0
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let dev1 =
    {
      Dev.name= "eth1"
    ; pci= pci_addr1
    ; mac= mac_addr1
    ; bios_eth_order= 1
    ; multi_nic= false
    }
  in
  let dev2 =
    {
      Dev.name= "eth2"
    ; pci= pci_addr2
    ; mac= mac_addr2
    ; bios_eth_order= 2
    ; multi_nic= false
    }
  in
  let dev3 =
    {
      Dev.name= "eth3"
    ; pci= pci_addr3
    ; mac= mac_addr3
    ; bios_eth_order= 3
    ; multi_nic= false
    }
  in
  let dev4 =
    {
      Dev.name= "eth4"
    ; pci= pci_addr4
    ; mac= mac_addr4
    ; bios_eth_order= 4
    ; multi_nic= false
    }
  in
  let currents = [dev0; dev1; dev2; dev3; dev4] in
  let seen_dev0 =
    {name= "eth0"; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}
  in
  let seen_dev1 =
    {name= "eth1"; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}
  in
  let seen_dev2 =
    {name= "eth2"; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}
  in
  let seen_dev3 =
    {name= "eth3"; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}
  in
  let seen_dev4 =
    {name= "eth4"; pci= pci_addr4; mac= mac_addr4; position= 4; present= true}
  in
  let last_order = [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4] in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "5 devices in the order" 5 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4 order

let test_usecase5 () =
  let mac_addr0' = Macaddr.of_string "02:23:45:67:89:01" |> Result.get_ok in
  let mac_addr1' = Macaddr.of_string "12:23:45:67:89:01" |> Result.get_ok in
  let mac_addr2' = Macaddr.of_string "22:23:45:67:89:01" |> Result.get_ok in
  let mac_addr3' = Macaddr.of_string "32:23:45:67:89:01" |> Result.get_ok in
  let mac_addr4' = Macaddr.of_string "42:23:45:67:89:01" |> Result.get_ok in

  let mac_addr0 = Macaddr.of_string "01:23:45:67:89:01" |> Result.get_ok in
  let mac_addr1 = Macaddr.of_string "11:23:45:67:89:01" |> Result.get_ok in
  let mac_addr2 = Macaddr.of_string "21:23:45:67:89:01" |> Result.get_ok in
  let mac_addr3 = Macaddr.of_string "31:23:45:67:89:01" |> Result.get_ok in
  let mac_addr4 = Macaddr.of_string "41:23:45:67:89:01" |> Result.get_ok in

  let pci_addr0 = Pciaddr.of_string "0000:01:00.0" |> Result.get_ok in
  let pci_addr1 = Pciaddr.of_string "0000:02:00.0" |> Result.get_ok in
  let pci_addr2 = Pciaddr.of_string "0000:03:00.0" |> Result.get_ok in
  let pci_addr3 = Pciaddr.of_string "0000:04:00.0" |> Result.get_ok in
  let pci_addr4 = Pciaddr.of_string "0000:05:00.0" |> Result.get_ok in

  let dev0 =
    {
      Dev.name= "side-1-eth0"
    ; pci= pci_addr0
    ; mac= mac_addr0'
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let dev1 =
    {
      Dev.name= "side-34-eth1"
    ; pci= pci_addr1
    ; mac= mac_addr1'
    ; bios_eth_order= 1
    ; multi_nic= false
    }
  in
  let dev2 =
    {
      Dev.name= "side-71-eth2"
    ; pci= pci_addr2
    ; mac= mac_addr2'
    ; bios_eth_order= 2
    ; multi_nic= false
    }
  in
  let dev3 =
    {
      Dev.name= "side-3012-eth3"
    ; pci= pci_addr3
    ; mac= mac_addr3'
    ; bios_eth_order= 3
    ; multi_nic= false
    }
  in
  let dev4 =
    {
      Dev.name= "side-4332-eth4"
    ; pci= pci_addr4
    ; mac= mac_addr4'
    ; bios_eth_order= 4
    ; multi_nic= false
    }
  in
  let currents = [dev0; dev1; dev2; dev3; dev4] in
  let seen_dev0 =
    {name= "eth0"; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}
  in
  let seen_dev1 =
    {name= "eth1"; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}
  in
  let seen_dev2 =
    {name= "eth2"; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}
  in
  let seen_dev3 =
    {name= "eth3"; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}
  in
  let seen_dev4 =
    {name= "eth4"; pci= pci_addr4; mac= mac_addr4; position= 4; present= true}
  in
  let last_order = [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4] in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "5 devices in the order" 5 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 3 true dev3 order ;
  test_postion_and_present 4 true dev4 order

let test_CA_94279 () =
  let mac_addr0 = Macaddr.of_string "00:1b:21:aa:ef:f0" |> Result.get_ok in
  let mac_addr1 = Macaddr.of_string "00:1b:21:aa:ef:f1" |> Result.get_ok in
  let mac_addr2 = Macaddr.of_string "00:1b:21:aa:ef:f4" |> Result.get_ok in
  let mac_addr3 = Macaddr.of_string "00:1b:21:aa:ef:f5" |> Result.get_ok in
  let mac_addr4 = Macaddr.of_string "60:eb:69:ed:9a:16" |> Result.get_ok in
  let mac_addr5 = Macaddr.of_string "60:eb:69:ed:9a:17" |> Result.get_ok in

  let pci_addr0 = Pciaddr.of_string "0000:03:00.0" |> Result.get_ok in
  let pci_addr1 = Pciaddr.of_string "0000:03:00.1" |> Result.get_ok in
  let pci_addr2 = Pciaddr.of_string "0000:04:00.0" |> Result.get_ok in
  let pci_addr3 = Pciaddr.of_string "0000:04:00.1" |> Result.get_ok in
  let pci_addr4 = Pciaddr.of_string "0000:06:00.0" |> Result.get_ok in
  let pci_addr5 = Pciaddr.of_string "0000:06:00.1" |> Result.get_ok in

  let dev0 =
    {
      Dev.name= "side-1-eth0"
    ; pci= pci_addr0
    ; mac= mac_addr0
    ; bios_eth_order= 2
    ; multi_nic= false
    }
  in
  let dev1 =
    {
      Dev.name= "side-2-eth1"
    ; pci= pci_addr1
    ; mac= mac_addr1
    ; bios_eth_order= 3
    ; multi_nic= false
    }
  in
  let dev2 =
    {
      Dev.name= "side-3-eth2"
    ; pci= pci_addr2
    ; mac= mac_addr2
    ; bios_eth_order= 4
    ; multi_nic= false
    }
  in
  let dev3 =
    {
      Dev.name= "side-4-eth3"
    ; pci= pci_addr3
    ; mac= mac_addr3
    ; bios_eth_order= 5
    ; multi_nic= false
    }
  in
  let dev4 =
    {
      Dev.name= "side-5-eth4"
    ; pci= pci_addr4
    ; mac= mac_addr4
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let dev5 =
    {
      Dev.name= "side-6-eth5"
    ; pci= pci_addr5
    ; mac= mac_addr5
    ; bios_eth_order= 1
    ; multi_nic= false
    }
  in
  let currents = [dev0; dev1; dev2; dev3; dev4; dev5] in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  test_postion_and_present 2 true dev0 order ;
  test_postion_and_present 3 true dev1 order ;
  test_postion_and_present 4 true dev2 order ;
  test_postion_and_present 5 true dev3 order ;
  test_postion_and_present 0 true dev4 order ;
  test_postion_and_present 1 true dev5 order

let test_rshp_new_hardware () =
  let mac_addr0' = Macaddr.of_string "02:23:45:67:89:01" |> Result.get_ok in
  let mac_addr1' = Macaddr.of_string "12:23:45:67:89:01" |> Result.get_ok in
  let mac_addr2' = Macaddr.of_string "22:23:45:67:89:01" |> Result.get_ok in
  let mac_addr3' = Macaddr.of_string "32:23:45:67:89:01" |> Result.get_ok in
  let mac_addr4' = Macaddr.of_string "32:23:45:67:89:02" |> Result.get_ok in

  let pci_addr0 = Pciaddr.of_string "0000:01:00.0" |> Result.get_ok in
  let pci_addr1 = Pciaddr.of_string "0000:02:00.0" |> Result.get_ok in
  let pci_addr2 = Pciaddr.of_string "0000:03:00.0" |> Result.get_ok in
  let pci_addr3 = Pciaddr.of_string "0000:04:00.0" |> Result.get_ok in

  let mac_addr0 = Macaddr.of_string "01:23:45:67:89:01" |> Result.get_ok in
  let mac_addr1 = Macaddr.of_string "11:23:45:67:89:01" |> Result.get_ok in
  let mac_addr2 = Macaddr.of_string "21:23:45:67:89:01" |> Result.get_ok in
  let mac_addr3 = Macaddr.of_string "31:23:45:67:89:02" |> Result.get_ok in
  let mac_addr4 = Macaddr.of_string "31:23:45:67:89:01" |> Result.get_ok in

  let dev0 =
    {
      Dev.name= "side-1-eth0"
    ; pci= pci_addr0
    ; mac= mac_addr0'
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in

  let dev1 =
    {
      Dev.name= "side-34-eth1"
    ; pci= pci_addr1
    ; mac= mac_addr1'
    ; bios_eth_order= 1
    ; multi_nic= false
    }
  in
  let dev2 =
    {
      Dev.name= "side-71-eth2"
    ; pci= pci_addr2
    ; mac= mac_addr2'
    ; bios_eth_order= 2
    ; multi_nic= false
    }
  in

  let dev3 =
    {
      Dev.name= "side-3012-eth3"
    ; pci= pci_addr3
    ; mac= mac_addr3'
    ; bios_eth_order= 3
    ; multi_nic= true
    }
  in
  let dev4 =
    {
      Dev.name= "side-4332-eth4"
    ; pci= pci_addr3
    ; mac= mac_addr4'
    ; bios_eth_order= 4
    ; multi_nic= true
    }
  in
  let seen_dev0 =
    {name= "eth0"; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}
  in
  let seen_dev1 =
    {name= "eth1"; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}
  in
  let seen_dev2 =
    {name= "eth2"; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}
  in
  let seen_dev3 =
    {name= "eth3"; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}
  in
  let seen_dev4 =
    {name= "eth4"; pci= pci_addr3; mac= mac_addr4; position= 4; present= true}
  in
  let currents = [dev0; dev1; dev2; dev3; dev4] in
  let last_order = [seen_dev0; seen_dev1; seen_dev2; seen_dev3; seen_dev4] in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "5 devices in the order" 5 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 4 true dev3 order ;
  test_postion_and_present 3 true dev4 order

let test_bad_biosdevname_order () =
  let pci_addr0 = Pciaddr.of_string "0000:01:00.0" |> Result.get_ok in
  let pci_addr1 = Pciaddr.of_string "0000:02:00.0" |> Result.get_ok in
  let pci_addr4 = Pciaddr.of_string "0000:03:00.0" |> Result.get_ok in
  let pci_addr5 = Pciaddr.of_string "0000:04:00.0" |> Result.get_ok in
  let pci_addr8 = Pciaddr.of_string "0000:05:00.0" |> Result.get_ok in

  let mac_addr0 = Macaddr.of_string "00:00:00:00:00:01" |> Result.get_ok in
  let mac_addr1 = Macaddr.of_string "00:00:44:00:01:01" |> Result.get_ok in
  let mac_addr2 = Macaddr.of_string "00:00:44:00:01:02" |> Result.get_ok in
  let mac_addr3 = Macaddr.of_string "00:00:44:00:01:03" |> Result.get_ok in
  let mac_addr4 = Macaddr.of_string "00:00:00:00:02:01" |> Result.get_ok in
  let mac_addr5 = Macaddr.of_string "00:00:22:00:03:01" |> Result.get_ok in
  let mac_addr6 = Macaddr.of_string "00:00:22:00:03:02" |> Result.get_ok in
  let mac_addr7 = Macaddr.of_string "00:00:22:00:03:03" |> Result.get_ok in
  let mac_addr8 = Macaddr.of_string "00:00:00:00:04:01" |> Result.get_ok in

  let dev0 =
    {
      Dev.name= "side-0-eth0"
    ; pci= pci_addr0
    ; mac= mac_addr0
    ; bios_eth_order= 0
    ; multi_nic= false
    }
  in
  let dev1 =
    {
      Dev.name= "side-0-eth2"
    ; pci= pci_addr1
    ; mac= mac_addr1
    ; bios_eth_order= 2
    ; multi_nic= true
    }
  in
  let dev2 =
    {
      Dev.name= "side-0-eth6"
    ; pci= pci_addr1
    ; mac= mac_addr2
    ; bios_eth_order= 6
    ; multi_nic= true
    }
  in
  let dev3 =
    {
      Dev.name= "side-0-eth1"
    ; pci= pci_addr1
    ; mac= mac_addr3
    ; bios_eth_order= 1
    ; multi_nic= true
    }
  in
  let dev4 =
    {
      Dev.name= "side-0-eth4"
    ; pci= pci_addr4
    ; mac= mac_addr4
    ; bios_eth_order= 4
    ; multi_nic= true
    }
  in
  let dev5 =
    {
      Dev.name= "side-0-eth5"
    ; pci= pci_addr5
    ; mac= mac_addr5
    ; bios_eth_order= 7
    ; multi_nic= true
    }
  in
  let dev6 =
    {
      Dev.name= "side-0-eth3"
    ; pci= pci_addr5
    ; mac= mac_addr6
    ; bios_eth_order= 3
    ; multi_nic= true
    }
  in
  let dev7 =
    {
      Dev.name= "side-0-eth7"
    ; pci= pci_addr5
    ; mac= mac_addr7
    ; bios_eth_order= 5
    ; multi_nic= true
    }
  in
  let dev8 =
    {
      Dev.name= "side-0-eth8"
    ; pci= pci_addr8
    ; mac= mac_addr8
    ; bios_eth_order= 8
    ; multi_nic= false
    }
  in
  let currents = [dev0; dev1; dev2; dev3; dev4; dev5; dev6; dev7; dev8] in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check bool) "is Ok" true (Result.is_ok order) ;
  let order = Result.get_ok order in
  Alcotest.(check int) "9 devices in the order" 9 (List.length order) ;
  test_postion_and_present 0 true dev0 order ;
  test_postion_and_present 1 true dev1 order ;
  test_postion_and_present 2 true dev2 order ;
  test_postion_and_present 6 true dev3 order ;
  test_postion_and_present 4 true dev4 order ;
  test_postion_and_present 3 true dev5 order ;
  test_postion_and_present 5 true dev6 order ;
  test_postion_and_present 7 true dev7 order ;
  test_postion_and_present 8 true dev8 order

let tests =
  [
    ( "test_simple_logic"
    , [
        ("test_newhw_norules_1eth", `Quick, test_newhw_norules_1eth)
      ; ("test_newhw_norules_2eth", `Quick, test_newhw_norules_2eth)
      ; ("test_newhw_2srule_2eth", `Quick, test_newhw_2srule_2eth)
      ; ( "test_nosrules_1eth_incorrect_udev"
        , `Quick
        , test_nosrules_1eth_incorrect_udev
        )
      ; ( "test_1srule_1eth_1last_correct_udev"
        , `Quick
        , test_1srule_1eth_1last_correct_udev
        )
      ; ( "test_1srule_1eth_already_complete"
        , `Quick
        , test_1srule_1eth_already_complete
        )
      ; ( "test_1drule_1eth_already_complete"
        , `Quick
        , test_1drule_1eth_already_complete
        )
      ]
    )
  ; ( "test_use_cases"
    , [
        ("test_usecase1", `Quick, test_usecase1)
      ; ("test_usecase5", `Quick, test_usecase5)
      ; ("test_CA_94279", `Quick, test_CA_94279)
      ; ("test_rshp_new_hardware", `Quick, test_rshp_new_hardware)
      ; ("test_bad_biosdevname_order", `Quick, test_bad_biosdevname_order)
      ]
    )
  ]
