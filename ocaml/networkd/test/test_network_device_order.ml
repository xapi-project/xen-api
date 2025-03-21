open Network_device_order

let pci_addr0 = PciAddr.of_string_exn "0000:01:0f.0"

let pci_addr1 = PciAddr.of_string_exn "0000:01:0f.1"

let pci_addr2 = PciAddr.of_string_exn "0000:01:0f.2"

let pci_addr3 = PciAddr.of_string_exn "0000:01:0f.3"

let pci_addr4 = PciAddr.of_string_exn "0000:05:0f.0"

let pci_addr5 = pci_addr4

let mac_addr0 = Macaddr.of_string_exn "ec:f4:bb:e6:d7:b8"

let mac_addr1 = Macaddr.of_string_exn "ec:f4:bb:e6:d7:b9"

let mac_addr2 = Macaddr.of_string_exn "ec:f4:bb:e6:d7:ba"

let mac_addr3 = Macaddr.of_string_exn "ec:f4:bb:e6:d7:bb"

let mac_addr4 = Macaddr.of_string_exn "00:02:c9:ed:fd:f0"

let mac_addr5 = Macaddr.of_string_exn "00:02:c9:ed:fd:f1"

let name0 = "eno1"

let name1 = "eno2"

let name2 = "eno3"

let name3 = "eno4"

let name4 = "enp5s0"

let name5 = "enp5s0d1"

let lasts =
  let open NetDev in
  OrderedNetDev.
    [
      {
        net_dev= {name= name0; pci_addr= pci_addr0; mac_addr= mac_addr0}
      ; position= 0
      }
    ; {
        net_dev= {name= name1; pci_addr= pci_addr1; mac_addr= mac_addr1}
      ; position= 1
      }
    ; {
        net_dev= {name= name2; pci_addr= pci_addr2; mac_addr= mac_addr2}
      ; position= 2
      }
    ; {
        net_dev= {name= name3; pci_addr= pci_addr3; mac_addr= mac_addr3}
      ; position= 3
      }
    ; {
        net_dev= {name= name4; pci_addr= pci_addr4; mac_addr= mac_addr4}
      ; position= 4
      }
    ; {
        net_dev= {name= name5; pci_addr= pci_addr5; mac_addr= mac_addr5}
      ; position= 5
      }
    ]
  |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.OrderedNetDev.net_dev.mac_addr)

let currents =
  let open NetDev in
  UnOrderedNetDev.
    [
      {
        net_dev= {name= name0; pci_addr= pci_addr0; mac_addr= mac_addr0}
      ; bios_eth_order= 0
      }
    ; {
        net_dev= {name= name1; pci_addr= pci_addr1; mac_addr= mac_addr1}
      ; bios_eth_order= 1
      }
    ; {
        net_dev= {name= name2; pci_addr= pci_addr2; mac_addr= mac_addr2}
      ; bios_eth_order= 2
      }
    ; {
        net_dev= {name= name3; pci_addr= pci_addr3; mac_addr= mac_addr3}
      ; bios_eth_order= 3
      }
    ; {
        net_dev= {name= name4; pci_addr= pci_addr4; mac_addr= mac_addr4}
      ; bios_eth_order= 4
      }
    ; {
        net_dev= {name= name5; pci_addr= pci_addr5; mac_addr= mac_addr5}
      ; bios_eth_order= 5
      }
    ]
  |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.UnOrderedNetDev.net_dev.mac_addr)

let pos_of_mac mac_addr ordered =
  let open OrderedNetDev in
  match List.find_opt (fun d -> d.net_dev.mac_addr = mac_addr) ordered with
  | Some {position; _} ->
      position
  | _ ->
      -1

let test_default () =
  let max, ordered, olds =
    generate_order ~currents ~mappings:[] ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "0 olds" 0 (List.length olds) ;
  Alcotest.(check int) "6 devices in the ordered" 6 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 5 max ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 ordered)

let test_initial_mapping_via_mac () =
  let mappings =
    [
      NetDevMapping.{position= 0; index= Mac_addr mac_addr5}
    ; NetDevMapping.{position= 1; index= Mac_addr mac_addr4}
    ; NetDevMapping.{position= 2; index= Mac_addr mac_addr3}
    ; NetDevMapping.{position= 3; index= Mac_addr mac_addr2}
    ; NetDevMapping.{position= 4; index= Mac_addr mac_addr1}
    ; NetDevMapping.{position= 5; index= Mac_addr mac_addr0}
    ]
  in
  let max, ordered, olds =
    generate_order ~currents ~mappings ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "0 olds" 0 (List.length olds) ;
  Alcotest.(check int) "6 devices in the ordered" 6 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 5 max ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr5 ordered)

let test_initial_mapping_via_name () =
  let mappings =
    [
      NetDevMapping.{position= 0; index= Label name5}
    ; NetDevMapping.{position= 1; index= Label name4}
    ; NetDevMapping.{position= 2; index= Label name3}
    ; NetDevMapping.{position= 3; index= Label name2}
    ; NetDevMapping.{position= 4; index= Label name1}
    ; NetDevMapping.{position= 5; index= Label name0}
    ]
  in
  let max, ordered, olds =
    generate_order ~currents ~mappings ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "0 olds" 0 (List.length olds) ;
  Alcotest.(check int) "6 devices in the ordered" 6 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 5 max ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr5 ordered)

let test_replacement () =
  let mac_addr0' = Macaddr.of_string_exn "fc:f4:bb:e6:d7:b8" in
  let mac_addr1' = Macaddr.of_string_exn "fc:f4:bb:e6:d7:b9" in
  let dev0 =
    let open NetDev in
    UnOrderedNetDev.
      {
        net_dev= {name= "eno10"; pci_addr= pci_addr0; mac_addr= mac_addr0'}
      ; bios_eth_order= 1
      }
  in
  let dev1 =
    let open NetDev in
    UnOrderedNetDev.
      {
        net_dev= {name= "eno11"; pci_addr= pci_addr1; mac_addr= mac_addr1'}
      ; bios_eth_order= 0
      }
  in
  let currents =
    currents
    |> MacaddrMap.remove mac_addr0
    |> MacaddrMap.remove mac_addr1
    |> MacaddrMap.add mac_addr0' dev0
    |> MacaddrMap.add mac_addr1' dev1
  in
  let max, ordered, olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "2 olds" 2 (List.length olds) ;
  Alcotest.(check int) "old position" 0 (pos_of_mac mac_addr0 olds) ;
  Alcotest.(check int) "old position" 1 (pos_of_mac mac_addr1 olds) ;
  Alcotest.(check int) "6 devices in the ordered" 6 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 5 max ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0' ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1' ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 ordered)

let test_adding () =
  let pci_addr6 = PciAddr.of_string_exn "0000:06:0f.0" in
  let pci_addr7 = PciAddr.of_string_exn "0000:06:0f.1" in
  let mac_addr6 = Macaddr.of_string_exn "fc:f4:bb:e6:d7:b8" in
  let mac_addr7 = Macaddr.of_string_exn "fc:f4:bb:e6:d7:b9" in
  let dev6 =
    let open NetDev in
    UnOrderedNetDev.
      {
        net_dev= {name= "eno6"; pci_addr= pci_addr6; mac_addr= mac_addr6}
      ; bios_eth_order= 1
      }
  in
  let dev7 =
    let open NetDev in
    UnOrderedNetDev.
      {
        net_dev= {name= "eno7"; pci_addr= pci_addr7; mac_addr= mac_addr7}
      ; bios_eth_order= 0
      }
  in
  let currents =
    currents |> MacaddrMap.add mac_addr6 dev6 |> MacaddrMap.add mac_addr7 dev7
  in
  let max, ordered, olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "0 olds" 0 (List.length olds) ;
  Alcotest.(check int) "8 devices in the ordered" 8 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 7 max ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 ordered) ;
  Alcotest.(check int) "position assigned" 6 (pos_of_mac mac_addr7 ordered) ;
  Alcotest.(check int) "position assigned" 7 (pos_of_mac mac_addr6 ordered)

let test_removing () =
  let currents =
    currents |> MacaddrMap.remove mac_addr0 |> MacaddrMap.remove mac_addr1
  in
  let max, ordered, olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "2 olds" 2 (List.length olds) ;
  Alcotest.(check int) "old position" 0 (pos_of_mac mac_addr0 olds) ;
  Alcotest.(check int) "old position" 1 (pos_of_mac mac_addr1 olds) ;
  Alcotest.(check int) "4 devices in the ordered" 4 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 5 max ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 ordered)

let test_replug () =
  let dev0 =
    let open NetDev in
    OrderedNetDev.
      {
        net_dev= {name= "eno1"; pci_addr= pci_addr0; mac_addr= mac_addr0}
      ; position= 0
      }
  in
  let dev1 =
    let open NetDev in
    OrderedNetDev.
      {
        net_dev= {name= "eno2"; pci_addr= pci_addr1; mac_addr= mac_addr1}
      ; position= 1
      }
  in
  let olds =
    MacaddrMap.empty
    |> MacaddrMap.add mac_addr0 dev0
    |> MacaddrMap.add mac_addr1 dev1
  in
  let lasts =
    lasts |> MacaddrMap.remove mac_addr0 |> MacaddrMap.remove mac_addr1
  in
  let max, ordered, olds = generate_order ~currents ~mappings:[] ~lasts ~olds in
  Alcotest.(check int) "0 olds" 0 (List.length olds) ;
  Alcotest.(check int) "6 devices in the ordered" 6 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 5 max ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 ordered)

let test_multinic_inplace_reorder () =
  let mac_addr4' = Macaddr.of_string_exn "01:02:c9:ed:fd:f0" in
  let mac_addr5' = Macaddr.of_string_exn "01:02:c9:ed:fd:f1" in
  let dev4 =
    let open NetDev in
    UnOrderedNetDev.
      {
        net_dev= {name= "eno14"; pci_addr= pci_addr4; mac_addr= mac_addr4'}
      ; bios_eth_order= 1
      }
  in
  let dev5 =
    let open NetDev in
    UnOrderedNetDev.
      {
        net_dev= {name= "eno15"; pci_addr= pci_addr5; mac_addr= mac_addr5'}
      ; bios_eth_order= 0
      }
  in
  let currents =
    currents
    |> MacaddrMap.remove mac_addr4
    |> MacaddrMap.remove mac_addr5
    |> MacaddrMap.add mac_addr4' dev4
    |> MacaddrMap.add mac_addr5' dev5
  in
  let max, ordered, olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "2 olds" 2 (List.length olds) ;
  Alcotest.(check int) "6 devices in the ordered" 6 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 5 max ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4' ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5' ordered)

let test_multinic_new_devices () =
  let mac_addr6 = Macaddr.of_string_exn "01:02:c9:ed:fd:f0" in
  let mac_addr7 = Macaddr.of_string_exn "01:02:c9:ed:fd:f1" in
  let dev6 =
    let open NetDev in
    UnOrderedNetDev.
      {
        net_dev= {name= "enp5s0d2"; pci_addr= pci_addr4; mac_addr= mac_addr6}
      ; bios_eth_order= 1
      }
  in
  let dev7 =
    let open NetDev in
    UnOrderedNetDev.
      {
        net_dev= {name= "enp5s0d3"; pci_addr= pci_addr5; mac_addr= mac_addr7}
      ; bios_eth_order= 0
      }
  in
  let currents =
    currents |> MacaddrMap.add mac_addr6 dev6 |> MacaddrMap.add mac_addr7 dev7
  in
  let max, ordered, olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "0 olds" 0 (List.length olds) ;
  Alcotest.(check int) "8 devices in the ordered" 8 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 7 max ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 ordered) ;
  Alcotest.(check int) "position assigned" 6 (pos_of_mac mac_addr6 ordered) ;
  Alcotest.(check int) "position assigned" 7 (pos_of_mac mac_addr7 ordered)

let test_pci_changes () =
  let open NetDev in
  let move_bus_by_1 pci_addr = PciAddr.{pci_addr with bus= pci_addr.bus + 1} in
  let currents =
    currents
    |> MacaddrMap.map (fun dev ->
           UnOrderedNetDev.
             {
               dev with
               net_dev=
                 {dev.net_dev with pci_addr= move_bus_by_1 dev.net_dev.pci_addr}
             }
       )
  in
  let max, ordered, olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "0 olds" 0 (List.length olds) ;
  Alcotest.(check int) "6 devices in the ordered" 6 (List.length ordered) ;
  Alcotest.(check int) "The max pos" 5 max ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 ordered)

let tests =
  [
    ( "test_known_cases"
    , [
        ("test_default", `Quick, test_default)
      ; ("test_initial_mapping_via_mac", `Quick, test_initial_mapping_via_mac)
      ; ("test_initial_mapping_via_name", `Quick, test_initial_mapping_via_name)
      ; ("test_replacement", `Quick, test_replacement)
      ; ("test_adding", `Quick, test_adding)
      ; ("test_removing", `Quick, test_removing)
      ; ("test_replug", `Quick, test_replug)
      ; ("test_multinic_inplace_reorder", `Quick, test_multinic_inplace_reorder)
      ; ("test_multinic_new_devices", `Quick, test_multinic_new_devices)
      ; ("test_pci_changes", `Quick, test_pci_changes)
      ]
    )
  ]
