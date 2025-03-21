open Network_device_order
open Network_interface

let pci_addr0 = Pciaddr.of_string_exn "0000:01:0f.0"

let pci_addr1 = Pciaddr.of_string_exn "0000:01:0f.1"

let pci_addr2 = Pciaddr.of_string_exn "0000:01:0f.2"

let pci_addr3 = Pciaddr.of_string_exn "0000:01:0f.3"

let pci_addr4 = Pciaddr.of_string_exn "0000:05:0f.0"

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

let last_order =
  [
    {name= name0; pci= pci_addr0; mac= mac_addr0; position= 0; present= true}
  ; {name= name1; pci= pci_addr1; mac= mac_addr1; position= 1; present= true}
  ; {name= name2; pci= pci_addr2; mac= mac_addr2; position= 2; present= true}
  ; {name= name3; pci= pci_addr3; mac= mac_addr3; position= 3; present= true}
  ; {name= name4; pci= pci_addr4; mac= mac_addr4; position= 4; present= true}
  ; {name= name5; pci= pci_addr5; mac= mac_addr5; position= 5; present= true}
  ]

let currents =
  let open Dev in
  [
    {
      name= name0
    ; pci= pci_addr0
    ; mac= mac_addr0
    ; bios_eth_order= 0
    ; multinic= false
    }
  ; {
      name= name1
    ; pci= pci_addr1
    ; mac= mac_addr1
    ; bios_eth_order= 1
    ; multinic= false
    }
  ; {
      name= name2
    ; pci= pci_addr2
    ; mac= mac_addr2
    ; bios_eth_order= 2
    ; multinic= false
    }
  ; {
      name= name3
    ; pci= pci_addr3
    ; mac= mac_addr3
    ; bios_eth_order= 3
    ; multinic= false
    }
  ; {
      name= name4
    ; pci= pci_addr4
    ; mac= mac_addr4
    ; bios_eth_order= 5
    ; multinic= false
    }
  ; {
      name= name5
    ; pci= pci_addr5
    ; mac= mac_addr5
    ; bios_eth_order= 4
    ; multinic= false
    }
  ]

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

let test_default () =
  let order = generate_order ~currents ~rules:[] ~last_order:[] in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr5 order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order)

let test_initial_rules_via_mac () =
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
  let order = generate_order ~currents ~rules ~last_order:[] in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr5 order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order)

let test_initial_rules_via_label () =
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
  let order = generate_order ~currents ~rules ~last_order:[] in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr5 order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order)

let test_replacement () =
  let mac_addr0' = Macaddr.of_string_exn "fc:f4:bb:e6:d7:b8" in
  let mac_addr1' = Macaddr.of_string_exn "fc:f4:bb:e6:d7:b9" in
  let open Dev in
  let dev0 =
    {
      name= "eno10"
    ; pci= pci_addr0
    ; mac= mac_addr0'
    ; bios_eth_order= 1
    ; multinic= false
    }
  in
  let dev1 =
    {
      name= "eno11"
    ; pci= pci_addr1
    ; mac= mac_addr1'
    ; bios_eth_order= 0
    ; multinic= false
    }
  in
  let currents =
    currents
    |> List.filter (fun dev -> dev.mac <> mac_addr0)
    |> List.filter (fun dev -> dev.mac <> mac_addr1)
    |> List.cons dev0
    |> List.cons dev1
  in
  let order = generate_order ~currents ~rules:[] ~last_order in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;

  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0' order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1' order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0' order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1' order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order)

let test_adding () =
  let pci_addr6 = Pciaddr.of_string_exn "0000:06:0f.0" in
  let mac_addr6 = Macaddr.of_string_exn "fc:f4:bb:e6:d7:b8" in
  let pci_addr7 = Pciaddr.of_string_exn "0000:06:0f.1" in
  let mac_addr7 = Macaddr.of_string_exn "fc:f4:bb:e6:d7:b9" in
  let open Dev in
  let dev6 =
    {
      name= "eno6"
    ; pci= pci_addr6
    ; mac= mac_addr6
    ; bios_eth_order= 1
    ; multinic= false
    }
  in
  let dev7 =
    {
      name= "eno7"
    ; pci= pci_addr7
    ; mac= mac_addr7
    ; bios_eth_order= 0
    ; multinic= false
    }
  in
  (* Add two devices *)
  let currents = List.rev_append currents [dev6; dev7] in
  let order = generate_order ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "8 devices in the order" 8 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 order) ;
  Alcotest.(check int) "position assigned" 6 (pos_of_mac mac_addr7 order) ;
  Alcotest.(check int) "position assigned" 7 (pos_of_mac mac_addr6 order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr6 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr7 order)

let test_removing () =
  (* Remove two devices *)
  let currents =
    let open Dev in
    currents
    |> List.filter (fun dev -> dev.mac <> mac_addr0)
    |> List.filter (fun dev -> dev.mac <> mac_addr1)
  in
  let order = generate_order ~currents ~rules:[] ~last_order in

  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 order) ;

  Alcotest.(check bool) "present" false (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" false (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order)

let test_replug () =
  let dev0 =
    {name= "eno1"; pci= pci_addr0; mac= mac_addr0; position= 0; present= false}
  in
  let dev1 =
    {name= "eno2"; pci= pci_addr1; mac= mac_addr1; position= 1; present= false}
  in
  (* Mark two devices with present = false *)
  let last_order =
    last_order
    |> List.filter (fun dev -> dev.mac <> mac_addr0)
    |> List.filter (fun dev -> dev.mac <> mac_addr1)
    |> List.cons dev0
    |> List.cons dev1
  in
  let order = generate_order ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order)

let test_multinic_inplace_reorder () =
  let mac_addr4' = Macaddr.of_string_exn "01:02:c9:ed:fd:f0" in
  let mac_addr5' = Macaddr.of_string_exn "01:02:c9:ed:fd:f1" in
  let open Dev in
  let dev4 =
    {
      name= "eno14"
    ; pci= pci_addr4
    ; mac= mac_addr4'
    ; bios_eth_order= 1
    ; multinic= true
    }
  in
  let dev5 =
    {
      name= "eno15"
    ; pci= pci_addr5
    ; mac= mac_addr5'
    ; bios_eth_order= 0
    ; multinic= true
    }
  in
  (* The MAC addresses of multinic functions change *)
  let currents =
    currents
    |> List.filter (fun dev -> dev.mac <> mac_addr4)
    |> List.filter (fun dev -> dev.mac <> mac_addr5)
    |> List.cons dev4
    |> List.cons dev5
  in
  let order = generate_order ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4' order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5' order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4' order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5' order)

let test_multinic_new_devices () =
  let mac_addr6 = Macaddr.of_string_exn "01:02:c9:ed:fd:f0" in
  let mac_addr7 = Macaddr.of_string_exn "01:02:c9:ed:fd:f1" in
  let dev6 =
    Dev.
      {
        name= "enp5s0d2"
      ; pci= pci_addr4
      ; mac= mac_addr6
      ; bios_eth_order= 1
      ; multinic= true
      }
  in
  let dev7 =
    Dev.
      {
        name= "enp5s0d3"
      ; pci= pci_addr5
      ; mac= mac_addr7
      ; bios_eth_order= 0
      ; multinic= true
      }
  in
  (* New devices are reported on the same PCI address. *)
  let currents = currents |> List.cons dev6 |> List.cons dev7 in
  let order = generate_order ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "8 devices in the order" 8 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 order) ;
  Alcotest.(check int) "position assigned" 6 (pos_of_mac mac_addr6 order) ;
  Alcotest.(check int) "position assigned" 7 (pos_of_mac mac_addr7 order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr6 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr7 order)

let test_pci_changes () =
  let move_bus_by_1 pci_addr = Xcp_pci.{pci_addr with bus= pci_addr.bus + 1} in
  let currents =
    currents |> List.map (fun dev -> Dev.{dev with pci= move_bus_by_1 dev.pci})
  in
  let order = generate_order ~currents ~rules:[] ~last_order in
  Alcotest.(check int) "6 devices in the order" 6 (List.length order) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 order) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 order) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 order) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 order) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 order) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr5 order) ;

  Alcotest.(check bool) "present" true (present_of_mac mac_addr0 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr1 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr2 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr3 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr4 order) ;
  Alcotest.(check bool) "present" true (present_of_mac mac_addr5 order)

let tests =
  [
    ( "test_known_cases"
    , [
        ("test_default", `Quick, test_default)
      ; ("test_initial_mapping_via_mac", `Quick, test_initial_rules_via_mac)
      ; ("test_initial_mapping_via_name", `Quick, test_initial_rules_via_label)
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
