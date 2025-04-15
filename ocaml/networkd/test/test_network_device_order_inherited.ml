open Network_device_order
open Network_interface

let pos_of_mac mac_addr order =
  match List.find_opt (fun d -> d.mac = mac_addr) order with
  | Some {position; _} ->
      position
  | _ ->
      -1

let test_newhw_norules_1eth () =
  let mac_addr = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let currents =
    let open Dev in
    [
      {
        name= "side-12-eth1"
      ; pci= Pciaddr.of_string_exn "0000:00:0f.0"
      ; mac= mac_addr
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ]
  in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  let position = pos_of_mac mac_addr order in
  Alcotest.(check int) "position assigned" 0 position

let test_newhw_norules_2eth () =
  let mac_addr = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let currents =
    let open Dev in
    [
      {
        name= "side-12-eth1"
      ; pci= Pciaddr.of_string_exn "0000:00:0f.0"
      ; mac= mac_addr
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ; {
        name= "side-33-eth0"
      ; pci= Pciaddr.of_string_exn "0000:00:01.0"
      ; mac= Macaddr.of_string_exn "ab:cd:ef:12:34:57"
      ; bios_eth_order= 1
      ; multi_nic= false
      }
    ]
  in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check int) "2 devices in the order" 2 (List.length order) ;
  let position = pos_of_mac mac_addr order in
  Alcotest.(check int) "position assigned" 0 position

let test_newhw_2srule_2eth () =
  let mac_addr0 = Macaddr.of_string_exn "12:34:56:78:90:12" in
  let mac_addr1 = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let rules =
    Rule.
      [
        {position= 0; index= Mac_addr mac_addr1}
      ; {position= 1; index= Mac_addr mac_addr0}
      ]
  in
  let currents =
    let open Dev in
    [
      {
        name= "eth0"
      ; pci= Pciaddr.of_string_exn "0000:00:01.0"
      ; mac= mac_addr0
      ; bios_eth_order= 1
      ; multi_nic= false
      }
    ; {
        name= "side-12-eth1"
      ; pci= Pciaddr.of_string_exn "0000:00:0f.0"
      ; mac= mac_addr1
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ]
  in
  let order = sort' ~currents ~rules ~last_order:[] in
  Alcotest.(check int) "2 devices in the order" 2 (List.length order) ;
  let position = pos_of_mac mac_addr1 order in
  Alcotest.(check int) "position assigned" 0 position

let test_nosrules_1eth_incorrect_udev () =
  let mac_addr = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let pci_addr = Pciaddr.of_string_exn "0000:00:0f.0" in
  let currents =
    let open Dev in
    [
      {
        name= "side-12-eth0"
      ; pci= pci_addr
      ; mac= mac_addr
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ]
  in
  let last_order =
    [{name= "eth2"; pci= pci_addr; mac= mac_addr; position= 3; present= true}]
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  let position = pos_of_mac mac_addr order in
  Alcotest.(check int) "position assigned" 3 position

let test_1srule_1eth_1last_correct_udev () =
  let mac_addr = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let pci_addr = Pciaddr.of_string_exn "0000:00:0f.0" in
  let currents =
    let open Dev in
    [
      {
        name= "eth1"
      ; pci= pci_addr
      ; mac= mac_addr
      ; bios_eth_order= 1
      ; multi_nic= false
      }
    ]
  in
  let rules = Rule.[{position= 0; index= Mac_addr mac_addr}] in
  let last_order =
    [{name= "eth1"; pci= pci_addr; mac= mac_addr; position= 1; present= true}]
  in
  let order = sort' ~currents ~rules ~last_order in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  let position = pos_of_mac mac_addr order in
  Alcotest.(check int) "position assigned" 0 position

let test_1srule_1eth_already_complete () =
  let mac_addr = Macaddr.of_string_exn "00:13:72:2d:2a:ec" in
  let pci_addr = Pciaddr.of_string_exn "0000:04:00.0" in
  let currents =
    let open Dev in
    [
      {
        name= "eth0"
      ; pci= pci_addr
      ; mac= mac_addr
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ]
  in
  let rules = Rule.[{position= 0; index= Mac_addr mac_addr}] in
  let order = sort' ~currents ~rules ~last_order:[] in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  let position = pos_of_mac mac_addr order in
  Alcotest.(check int) "position assigned" 0 position

let test_1drule_1eth_already_complete () =
  let mac_addr = Macaddr.of_string_exn "00:13:72:2d:2a:ec" in
  let pci_addr = Pciaddr.of_string_exn "0000:04:00.0" in
  let currents =
    let open Dev in
    [
      {
        name= "eth0"
      ; pci= pci_addr
      ; mac= mac_addr
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ]
  in
  let last_order =
    [{name= "eth0"; pci= pci_addr; mac= mac_addr; position= 0; present= true}]
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "1 device in the order" 1 (List.length order) ;
  let position = pos_of_mac mac_addr order in
  Alcotest.(check int) "position assigned" 0 position

let test_usecase1 () =
  let mac_addr0 = Macaddr.of_string_exn "01:23:45:67:89:01" in
  let mac_addr1 = Macaddr.of_string_exn "11:23:45:67:89:01" in
  let mac_addr2 = Macaddr.of_string_exn "21:23:45:67:89:01" in
  let mac_addr3 = Macaddr.of_string_exn "31:23:45:67:89:01" in
  let mac_addr4 = Macaddr.of_string_exn "41:23:45:67:89:01" in
  let pci_addr0 = Pciaddr.of_string_exn "0000:01:00.0" in
  let pci_addr1 = Pciaddr.of_string_exn "0000:02:00.0" in
  let pci_addr2 = Pciaddr.of_string_exn "0000:03:00.0" in
  let pci_addr3 = Pciaddr.of_string_exn "0000:04:00.0" in
  let pci_addr4 = Pciaddr.of_string_exn "0000:05:00.0" in

  let currents =
    let open Dev in
    [
      {
        name= "eth0"
      ; pci= pci_addr0
      ; mac= mac_addr0
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ; {
        name= "eth1"
      ; pci= pci_addr1
      ; mac= mac_addr1
      ; bios_eth_order= 1
      ; multi_nic= false
      }
    ; {
        name= "eth2"
      ; pci= pci_addr2
      ; mac= mac_addr2
      ; bios_eth_order= 2
      ; multi_nic= false
      }
    ; {
        name= "eth3"
      ; pci= pci_addr3
      ; mac= mac_addr3
      ; bios_eth_order= 3
      ; multi_nic= false
      }
    ; {
        name= "eth4"
      ; pci= pci_addr4
      ; mac= mac_addr4
      ; bios_eth_order= 4
      ; multi_nic= false
      }
    ]
  in
  let last_order =
    [
      {name= "eth0"; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}
    ; {name= "eth1"; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}
    ; {name= "eth2"; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}
    ; {name= "eth3"; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}
    ; {name= "eth4"; pci= pci_addr4; mac= mac_addr4; position= 4; present= true}
    ]
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "5 devices in the order" 5 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 order)

let test_usecase5 () =
  let mac_addr0' = Macaddr.of_string_exn "02:23:45:67:89:01" in
  let mac_addr1' = Macaddr.of_string_exn "12:23:45:67:89:01" in
  let mac_addr2' = Macaddr.of_string_exn "22:23:45:67:89:01" in
  let mac_addr3' = Macaddr.of_string_exn "32:23:45:67:89:01" in
  let mac_addr4' = Macaddr.of_string_exn "42:23:45:67:89:01" in

  let mac_addr0 = Macaddr.of_string_exn "01:23:45:67:89:01" in
  let mac_addr1 = Macaddr.of_string_exn "11:23:45:67:89:01" in
  let mac_addr2 = Macaddr.of_string_exn "21:23:45:67:89:01" in
  let mac_addr3 = Macaddr.of_string_exn "31:23:45:67:89:01" in
  let mac_addr4 = Macaddr.of_string_exn "41:23:45:67:89:01" in

  let pci_addr0 = Pciaddr.of_string_exn "0000:01:00.0" in
  let pci_addr1 = Pciaddr.of_string_exn "0000:02:00.0" in
  let pci_addr2 = Pciaddr.of_string_exn "0000:03:00.0" in
  let pci_addr3 = Pciaddr.of_string_exn "0000:04:00.0" in
  let pci_addr4 = Pciaddr.of_string_exn "0000:05:00.0" in

  let currents =
    let open Dev in
    [
      {
        name= "side-1-eth0"
      ; pci= pci_addr0
      ; mac= mac_addr0'
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ; {
        name= "side-34-eth1"
      ; pci= pci_addr1
      ; mac= mac_addr1'
      ; bios_eth_order= 1
      ; multi_nic= false
      }
    ; {
        name= "side-71-eth2"
      ; pci= pci_addr2
      ; mac= mac_addr2'
      ; bios_eth_order= 2
      ; multi_nic= false
      }
    ; {
        name= "side-3012-eth3"
      ; pci= pci_addr3
      ; mac= mac_addr3'
      ; bios_eth_order= 3
      ; multi_nic= false
      }
    ; {
        name= "side-4332-eth4"
      ; pci= pci_addr4
      ; mac= mac_addr4'
      ; bios_eth_order= 4
      ; multi_nic= false
      }
    ]
  in
  let last_order =
    [
      {name= "eth0"; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}
    ; {name= "eth1"; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}
    ; {name= "eth2"; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}
    ; {name= "eth3"; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}
    ; {name= "eth4"; pci= pci_addr4; mac= mac_addr4; position= 4; present= true}
    ]
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "5 devices in the order" 5 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0' order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1' order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2' order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3' order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4' order)

let test_CA_94279 () =
  let mac_addr0 = Macaddr.of_string_exn "00:1b:21:aa:ef:f0" in
  let mac_addr1 = Macaddr.of_string_exn "00:1b:21:aa:ef:f1" in
  let mac_addr2 = Macaddr.of_string_exn "00:1b:21:aa:ef:f4" in
  let mac_addr3 = Macaddr.of_string_exn "00:1b:21:aa:ef:f5" in
  let mac_addr4 = Macaddr.of_string_exn "60:eb:69:ed:9a:16" in
  let mac_addr5 = Macaddr.of_string_exn "60:eb:69:ed:9a:17" in

  let pci_addr0 = Pciaddr.of_string_exn "0000:03:00.0" in
  let pci_addr1 = Pciaddr.of_string_exn "0000:03:00.1" in
  let pci_addr2 = Pciaddr.of_string_exn "0000:04:00.0" in
  let pci_addr3 = Pciaddr.of_string_exn "0000:04:00.1" in
  let pci_addr4 = Pciaddr.of_string_exn "0000:06:00.0" in
  let pci_addr5 = Pciaddr.of_string_exn "0000:06:00.1" in

  let currents =
    let open Dev in
    [
      {
        name= "side-1-eth0"
      ; pci= pci_addr0
      ; mac= mac_addr0
      ; bios_eth_order= 2
      ; multi_nic= false
      }
    ; {
        name= "side-2-eth1"
      ; pci= pci_addr1
      ; mac= mac_addr1
      ; bios_eth_order= 3
      ; multi_nic= false
      }
    ; {
        name= "side-3-eth2"
      ; pci= pci_addr2
      ; mac= mac_addr2
      ; bios_eth_order= 4
      ; multi_nic= false
      }
    ; {
        name= "side-4-eth3"
      ; pci= pci_addr3
      ; mac= mac_addr3
      ; bios_eth_order= 5
      ; multi_nic= false
      }
    ; {
        name= "side-5-eth4"
      ; pci= pci_addr4
      ; mac= mac_addr4
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ; {
        name= "side-6-eth5"
      ; pci= pci_addr5
      ; mac= mac_addr5
      ; bios_eth_order= 1
      ; multi_nic= false
      }
    ]
  in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr5 order)

let test_rshp_new_hardware () =
  let mac_addr0' = Macaddr.of_string_exn "02:23:45:67:89:01" in
  let mac_addr1' = Macaddr.of_string_exn "12:23:45:67:89:01" in
  let mac_addr2' = Macaddr.of_string_exn "22:23:45:67:89:01" in
  let mac_addr3' = Macaddr.of_string_exn "32:23:45:67:89:01" in
  let mac_addr4' = Macaddr.of_string_exn "32:23:45:67:89:02" in

  let pci_addr0 = Pciaddr.of_string_exn "0000:01:00.0" in
  let pci_addr1 = Pciaddr.of_string_exn "0000:02:00.0" in
  let pci_addr2 = Pciaddr.of_string_exn "0000:03:00.0" in
  let pci_addr3 = Pciaddr.of_string_exn "0000:04:00.0" in

  let mac_addr0 = Macaddr.of_string_exn "01:23:45:67:89:01" in
  let mac_addr1 = Macaddr.of_string_exn "11:23:45:67:89:01" in
  let mac_addr2 = Macaddr.of_string_exn "21:23:45:67:89:01" in
  let mac_addr3 = Macaddr.of_string_exn "31:23:45:67:89:02" in
  let mac_addr4 = Macaddr.of_string_exn "31:23:45:67:89:01" in

  let currents =
    let open Dev in
    [
      {
        name= "side-1-eth0"
      ; pci= pci_addr0
      ; mac= mac_addr0'
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ; {
        name= "side-34-eth1"
      ; pci= pci_addr1
      ; mac= mac_addr1'
      ; bios_eth_order= 1
      ; multi_nic= false
      }
    ; {
        name= "side-71-eth2"
      ; pci= pci_addr2
      ; mac= mac_addr2'
      ; bios_eth_order= 2
      ; multi_nic= false
      }
    ; {
        name= "side-3012-eth3"
      ; pci= pci_addr3
      ; mac= mac_addr3'
      ; bios_eth_order= 3
      ; multi_nic= true
      }
    ; {
        name= "side-4332-eth4"
      ; pci= pci_addr3
      ; mac= mac_addr4'
      ; bios_eth_order= 4
      ; multi_nic= true
      }
    ]
  in
  let last_order =
    [
      {name= "eth0"; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}
    ; {name= "eth1"; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}
    ; {name= "eth2"; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}
    ; {name= "eth3"; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}
    ; {name= "eth4"; pci= pci_addr3; mac= mac_addr4; position= 4; present= true}
    ]
  in
  let order = sort' ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "5 devices in the order" 5 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0' order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1' order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2' order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr3' order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr4' order)

let test_bad_biosdevname_order () =
  let pci_addr0 = Pciaddr.of_string_exn "0000:01:00.0" in
  let pci_addr1 = Pciaddr.of_string_exn "0000:02:00.0" in
  let pci_addr4 = Pciaddr.of_string_exn "0000:03:00.0" in
  let pci_addr5 = Pciaddr.of_string_exn "0000:04:00.0" in
  let pci_addr8 = Pciaddr.of_string_exn "0000:05:00.0" in

  let mac_addr0 = Macaddr.of_string_exn "00:00:00:00:00:01" in
  let mac_addr1 = Macaddr.of_string_exn "00:00:44:00:01:01" in
  let mac_addr2 = Macaddr.of_string_exn "00:00:44:00:01:02" in
  let mac_addr3 = Macaddr.of_string_exn "00:00:44:00:01:03" in
  let mac_addr4 = Macaddr.of_string_exn "00:00:00:00:02:01" in
  let mac_addr5 = Macaddr.of_string_exn "00:00:22:00:03:01" in
  let mac_addr6 = Macaddr.of_string_exn "00:00:22:00:03:02" in
  let mac_addr7 = Macaddr.of_string_exn "00:00:22:00:03:03" in
  let mac_addr8 = Macaddr.of_string_exn "00:00:00:00:04:01" in

  let currents =
    let open Dev in
    [
      {
        name= "side-0-eth0"
      ; pci= pci_addr0
      ; mac= mac_addr0
      ; bios_eth_order= 0
      ; multi_nic= false
      }
    ; {
        name= "side-0-eth2"
      ; pci= pci_addr1
      ; mac= mac_addr1
      ; bios_eth_order= 2
      ; multi_nic= true
      }
    ; {
        name= "side-0-eth6"
      ; pci= pci_addr1
      ; mac= mac_addr2
      ; bios_eth_order= 6
      ; multi_nic= true
      }
    ; {
        name= "side-0-eth1"
      ; pci= pci_addr1
      ; mac= mac_addr3
      ; bios_eth_order= 1
      ; multi_nic= true
      }
    ; {
        name= "side-0-eth4"
      ; pci= pci_addr4
      ; mac= mac_addr4
      ; bios_eth_order= 4
      ; multi_nic= true
      }
    ; {
        name= "side-0-eth5"
      ; pci= pci_addr5
      ; mac= mac_addr5
      ; bios_eth_order= 7
      ; multi_nic= true
      }
    ; {
        name= "side-0-eth3"
      ; pci= pci_addr5
      ; mac= mac_addr6
      ; bios_eth_order= 3
      ; multi_nic= true
      }
    ; {
        name= "side-0-eth7"
      ; pci= pci_addr5
      ; mac= mac_addr7
      ; bios_eth_order= 5
      ; multi_nic= true
      }
    ; {
        name= "side-0-eth8"
      ; pci= pci_addr8
      ; mac= mac_addr8
      ; bios_eth_order= 8
      ; multi_nic= false
      }
    ]
  in
  let order = sort' ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check int) "9 devices in the order" 9 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 6 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr5 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr6 order) ;
  Alcotest.(check int) "position assigned" 7 (pos_of_mac mac_addr7 order) ;
  Alcotest.(check int) "position assigned" 8 (pos_of_mac mac_addr8 order)

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
