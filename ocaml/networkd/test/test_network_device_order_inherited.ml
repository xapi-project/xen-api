open Network_device_order

let pos_of_mac mac_addr ordered =
  let open OrderedNetDev in
  match List.find_opt (fun d -> d.net_dev.mac_addr = mac_addr) ordered with
  | Some {position; _} ->
      position
  | _ ->
      -1

let test_newhw_norules_1eth () =
  let open NetDev in
  let mac_addr = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let currents =
    UnOrderedNetDev.
      [
        {
          net_dev=
            {
              name= "side-12-eth1"
            ; pci_addr= PciAddr.of_string_exn "0000:00:0f.0"
            ; mac_addr
            }
        ; bios_eth_order= 0
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "1 device in the ordered list" 1 (List.length ordered) ;
  let position = pos_of_mac mac_addr ordered in
  Alcotest.(check int) "position assigned" 0 position

let test_newhw_norules_2eth () =
  let open NetDev in
  let mac_addr = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let currents =
    UnOrderedNetDev.
      [
        {
          net_dev=
            {
              name= "side-12-eth1"
            ; pci_addr= PciAddr.of_string_exn "0000:00:0f.0"
            ; mac_addr
            }
        ; bios_eth_order= 0
        }
      ; {
          net_dev=
            {
              name= "side-33-eth0"
            ; pci_addr= PciAddr.of_string_exn "0000:00:01.0"
            ; mac_addr= Macaddr.of_string_exn "ab:cd:ef:12:34:57"
            }
        ; bios_eth_order= 1
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "2 devices in the ordered" 2 (List.length ordered) ;
  let position = pos_of_mac mac_addr ordered in
  Alcotest.(check int) "position assigned" 0 position

let test_newhw_2srule_2eth () =
  let open NetDev in
  let mac_addr0 = Macaddr.of_string_exn "12:34:56:78:90:12" in
  let mac_addr1 = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let mappings =
    [
      NetDevMapping.{position= 0; index= Mac_addr mac_addr1}
    ; NetDevMapping.{position= 1; index= Mac_addr mac_addr0}
    ]
  in
  let currents =
    UnOrderedNetDev.
      [
        {
          net_dev=
            {
              name= "eth0"
            ; pci_addr= PciAddr.of_string_exn "0000:00:01.0"
            ; mac_addr= mac_addr0
            }
        ; bios_eth_order= 1
        }
      ; {
          net_dev=
            {
              name= "side-12-eth1"
            ; pci_addr= PciAddr.of_string_exn "0000:00:0f.0"
            ; mac_addr= mac_addr1
            }
        ; bios_eth_order= 0
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "2 devices in the ordered" 2 (List.length ordered) ;
  let position = pos_of_mac mac_addr1 ordered in
  Alcotest.(check int) "position assigned" 0 position

let test_nosrules_1eth_incorrect_udev () =
  let open NetDev in
  let mac_addr = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let pci_addr = PciAddr.of_string_exn "0000:00:0f.0" in
  let currents =
    UnOrderedNetDev.
      [{net_dev= {name= "side-12-eth0"; pci_addr; mac_addr}; bios_eth_order= 0}]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let lasts =
    OrderedNetDev.
      [{net_dev= {name= "side-12-eth0"; pci_addr; mac_addr}; position= 3}]
    |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.OrderedNetDev.net_dev.mac_addr)
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "1 device in the ordered" 1 (List.length ordered) ;
  let position = pos_of_mac mac_addr ordered in
  Alcotest.(check int) "position assigned" 3 position

let test_1srule_1eth_1last_correct_udev () =
  let open NetDev in
  let mac_addr = Macaddr.of_string_exn "ab:cd:ef:12:34:56" in
  let pci_addr = PciAddr.of_string_exn "0000:00:0f.0" in
  let currents =
    UnOrderedNetDev.
      [{net_dev= {name= "eth1"; pci_addr; mac_addr}; bios_eth_order= 1}]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let mappings = [NetDevMapping.{position= 0; index= Mac_addr mac_addr}] in
  let lasts =
    OrderedNetDev.[{net_dev= {name= "eth1"; pci_addr; mac_addr}; position= 1}]
    |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.OrderedNetDev.net_dev.mac_addr)
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "1 device in the ordered" 1 (List.length ordered) ;
  let position = pos_of_mac mac_addr ordered in
  Alcotest.(check int) "position assigned" 0 position

let test_1srule_1eth_already_complete () =
  let open NetDev in
  let mac_addr = Macaddr.of_string_exn "00:13:72:2d:2a:ec" in
  let pci_addr = PciAddr.of_string_exn "0000:04:00.0" in
  let currents =
    UnOrderedNetDev.
      [{net_dev= {name= "eth0"; pci_addr; mac_addr}; bios_eth_order= 0}]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let mappings = [NetDevMapping.{position= 0; index= Mac_addr mac_addr}] in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "1 device in the ordered" 1 (List.length ordered) ;
  let position = pos_of_mac mac_addr ordered in
  Alcotest.(check int) "position assigned" 0 position

let test_1drule_1eth_already_complete () =
  let open NetDev in
  let mac_addr = Macaddr.of_string_exn "00:13:72:2d:2a:ec" in
  let pci_addr = PciAddr.of_string_exn "0000:04:00.0" in
  let currents =
    UnOrderedNetDev.
      [{net_dev= {name= "eth0"; pci_addr; mac_addr}; bios_eth_order= 0}]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let lasts =
    OrderedNetDev.[{net_dev= {name= "eth0"; pci_addr; mac_addr}; position= 0}]
    |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.OrderedNetDev.net_dev.mac_addr)
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "1 device in the ordered" 1 (List.length ordered) ;
  let position = pos_of_mac mac_addr ordered in
  Alcotest.(check int) "position assigned" 0 position

let test_usecase1 () =
  let open NetDev in
  let mac_addr0 = Macaddr.of_string_exn "01:23:45:67:89:01" in
  let mac_addr1 = Macaddr.of_string_exn "11:23:45:67:89:01" in
  let mac_addr2 = Macaddr.of_string_exn "21:23:45:67:89:01" in
  let mac_addr3 = Macaddr.of_string_exn "31:23:45:67:89:01" in
  let mac_addr4 = Macaddr.of_string_exn "41:23:45:67:89:01" in
  let pci_addr0 = PciAddr.of_string_exn "0000:01:00.0" in
  let pci_addr1 = PciAddr.of_string_exn "0000:02:00.0" in
  let pci_addr2 = PciAddr.of_string_exn "0000:03:00.0" in
  let pci_addr3 = PciAddr.of_string_exn "0000:04:00.0" in
  let pci_addr4 = PciAddr.of_string_exn "0000:05:00.0" in

  let currents =
    UnOrderedNetDev.
      [
        {
          net_dev= {name= "eth0"; pci_addr= pci_addr0; mac_addr= mac_addr0}
        ; bios_eth_order= 0
        }
      ; {
          net_dev= {name= "eth1"; pci_addr= pci_addr1; mac_addr= mac_addr1}
        ; bios_eth_order= 1
        }
      ; {
          net_dev= {name= "eth2"; pci_addr= pci_addr2; mac_addr= mac_addr2}
        ; bios_eth_order= 2
        }
      ; {
          net_dev= {name= "eth3"; pci_addr= pci_addr3; mac_addr= mac_addr3}
        ; bios_eth_order= 3
        }
      ; {
          net_dev= {name= "eth4"; pci_addr= pci_addr4; mac_addr= mac_addr4}
        ; bios_eth_order= 4
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let lasts =
    OrderedNetDev.
      [
        {
          net_dev= {name= "eth0"; pci_addr= pci_addr0; mac_addr= mac_addr0}
        ; position= 0
        }
      ; {
          net_dev= {name= "eth1"; pci_addr= pci_addr1; mac_addr= mac_addr1}
        ; position= 1
        }
      ; {
          net_dev= {name= "eth2"; pci_addr= pci_addr2; mac_addr= mac_addr2}
        ; position= 2
        }
      ; {
          net_dev= {name= "eth3"; pci_addr= pci_addr3; mac_addr= mac_addr3}
        ; position= 3
        }
      ; {
          net_dev= {name= "eth4"; pci_addr= pci_addr4; mac_addr= mac_addr4}
        ; position= 4
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.OrderedNetDev.net_dev.mac_addr)
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "5 devices in the ordered" 5 (List.length ordered) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered)

let test_usecase5 () =
  let open NetDev in
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

  let pci_addr0 = PciAddr.of_string_exn "0000:01:00.0" in
  let pci_addr1 = PciAddr.of_string_exn "0000:02:00.0" in
  let pci_addr2 = PciAddr.of_string_exn "0000:03:00.0" in
  let pci_addr3 = PciAddr.of_string_exn "0000:04:00.0" in
  let pci_addr4 = PciAddr.of_string_exn "0000:05:00.0" in

  let currents =
    UnOrderedNetDev.
      [
        {
          net_dev=
            {name= "side-1-eth0"; pci_addr= pci_addr0; mac_addr= mac_addr0'}
        ; bios_eth_order= 0
        }
      ; {
          net_dev=
            {name= "side-34-eth1"; pci_addr= pci_addr1; mac_addr= mac_addr1'}
        ; bios_eth_order= 1
        }
      ; {
          net_dev=
            {name= "side-71-eth2"; pci_addr= pci_addr2; mac_addr= mac_addr2'}
        ; bios_eth_order= 2
        }
      ; {
          net_dev=
            {name= "side-3012-eth3"; pci_addr= pci_addr3; mac_addr= mac_addr3'}
        ; bios_eth_order= 3
        }
      ; {
          net_dev=
            {name= "side-4332-eth4"; pci_addr= pci_addr4; mac_addr= mac_addr4'}
        ; bios_eth_order= 4
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let lasts =
    OrderedNetDev.
      [
        {
          net_dev= {name= "eth0"; pci_addr= pci_addr0; mac_addr= mac_addr0}
        ; position= 0
        }
      ; {
          net_dev= {name= "eth1"; pci_addr= pci_addr1; mac_addr= mac_addr1}
        ; position= 1
        }
      ; {
          net_dev= {name= "eth2"; pci_addr= pci_addr2; mac_addr= mac_addr2}
        ; position= 2
        }
      ; {
          net_dev= {name= "eth3"; pci_addr= pci_addr3; mac_addr= mac_addr3}
        ; position= 3
        }
      ; {
          net_dev= {name= "eth4"; pci_addr= pci_addr4; mac_addr= mac_addr4}
        ; position= 4
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.OrderedNetDev.net_dev.mac_addr)
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "5 devices in the ordered" 5 (List.length ordered) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0' ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1' ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2' ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr3' ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4' ordered)

let test_CA_94279 () =
  let open NetDev in
  let mac_addr0 = Macaddr.of_string_exn "00:1b:21:aa:ef:f0" in
  let mac_addr1 = Macaddr.of_string_exn "00:1b:21:aa:ef:f1" in
  let mac_addr2 = Macaddr.of_string_exn "00:1b:21:aa:ef:f4" in
  let mac_addr3 = Macaddr.of_string_exn "00:1b:21:aa:ef:f5" in
  let mac_addr4 = Macaddr.of_string_exn "60:eb:69:ed:9a:16" in
  let mac_addr5 = Macaddr.of_string_exn "60:eb:69:ed:9a:17" in

  let pci_addr0 = PciAddr.of_string_exn "0000:03:00.0" in
  let pci_addr1 = PciAddr.of_string_exn "0000:03:00.1" in
  let pci_addr2 = PciAddr.of_string_exn "0000:04:00.0" in
  let pci_addr3 = PciAddr.of_string_exn "0000:04:00.1" in
  let pci_addr4 = PciAddr.of_string_exn "0000:06:00.0" in
  let pci_addr5 = PciAddr.of_string_exn "0000:06:00.1" in

  let currents =
    UnOrderedNetDev.
      [
        {
          net_dev=
            {name= "side-1-eth0"; pci_addr= pci_addr0; mac_addr= mac_addr0}
        ; bios_eth_order= 2
        }
      ; {
          net_dev=
            {name= "side-2-eth1"; pci_addr= pci_addr1; mac_addr= mac_addr1}
        ; bios_eth_order= 3
        }
      ; {
          net_dev=
            {name= "side-3-eth2"; pci_addr= pci_addr2; mac_addr= mac_addr2}
        ; bios_eth_order= 4
        }
      ; {
          net_dev=
            {name= "side-4-eth3"; pci_addr= pci_addr3; mac_addr= mac_addr3}
        ; bios_eth_order= 5
        }
      ; {
          net_dev=
            {name= "side-5-eth4"; pci_addr= pci_addr4; mac_addr= mac_addr4}
        ; bios_eth_order= 0
        }
      ; {
          net_dev=
            {name= "side-6-eth5"; pci_addr= pci_addr5; mac_addr= mac_addr5}
        ; bios_eth_order= 1
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "6 devices in the ordered" 6 (List.length ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr5 ordered)

let test_rshp_new_hardware () =
  let open NetDev in
  let mac_addr0' = Macaddr.of_string_exn "02:23:45:67:89:01" in
  let mac_addr1' = Macaddr.of_string_exn "12:23:45:67:89:01" in
  let mac_addr2' = Macaddr.of_string_exn "22:23:45:67:89:01" in
  let mac_addr3' = Macaddr.of_string_exn "32:23:45:67:89:01" in
  let mac_addr4' = Macaddr.of_string_exn "32:23:45:67:89:02" in

  let pci_addr0 = PciAddr.of_string_exn "0000:01:00.0" in
  let pci_addr1 = PciAddr.of_string_exn "0000:02:00.0" in
  let pci_addr2 = PciAddr.of_string_exn "0000:03:00.0" in
  let pci_addr3 = PciAddr.of_string_exn "0000:04:00.0" in
  let pci_addr4 = PciAddr.of_string_exn "0000:04:00.0" in

  let mac_addr0 = Macaddr.of_string_exn "01:23:45:67:89:01" in
  let mac_addr1 = Macaddr.of_string_exn "11:23:45:67:89:01" in
  let mac_addr2 = Macaddr.of_string_exn "21:23:45:67:89:01" in
  let mac_addr3 = Macaddr.of_string_exn "31:23:45:67:89:02" in
  let mac_addr4 = Macaddr.of_string_exn "31:23:45:67:89:01" in

  let currents =
    UnOrderedNetDev.
      [
        {
          net_dev=
            {name= "side-1-eth0"; pci_addr= pci_addr0; mac_addr= mac_addr0'}
        ; bios_eth_order= 0
        }
      ; {
          net_dev=
            {name= "side-34-eth1"; pci_addr= pci_addr1; mac_addr= mac_addr1'}
        ; bios_eth_order= 1
        }
      ; {
          net_dev=
            {name= "side-71-eth2"; pci_addr= pci_addr2; mac_addr= mac_addr2'}
        ; bios_eth_order= 2
        }
      ; {
          net_dev=
            {name= "side-3012-eth3"; pci_addr= pci_addr3; mac_addr= mac_addr3'}
        ; bios_eth_order= 3
        }
      ; {
          net_dev=
            {name= "side-4332-eth4"; pci_addr= pci_addr4; mac_addr= mac_addr4'}
        ; bios_eth_order= 4
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let lasts =
    OrderedNetDev.
      [
        {
          net_dev= {name= "eth0"; pci_addr= pci_addr0; mac_addr= mac_addr0}
        ; position= 0
        }
      ; {
          net_dev= {name= "eth1"; pci_addr= pci_addr1; mac_addr= mac_addr1}
        ; position= 1
        }
      ; {
          net_dev= {name= "eth2"; pci_addr= pci_addr2; mac_addr= mac_addr2}
        ; position= 2
        }
      ; {
          net_dev= {name= "eth3"; pci_addr= pci_addr3; mac_addr= mac_addr3}
        ; position= 3
        }
      ; {
          net_dev= {name= "eth4"; pci_addr= pci_addr4; mac_addr= mac_addr4}
        ; position= 4
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.OrderedNetDev.net_dev.mac_addr)
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "5 devices in the ordered" 5 (List.length ordered) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0' ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1' ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2' ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr3' ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr4' ordered)

let test_bad_biosdevname_order () =
  let open NetDev in
  let pci_addr0 = PciAddr.of_string_exn "0000:01:00.0" in
  let pci_addr1 = PciAddr.of_string_exn "0000:02:00.0" in
  let pci_addr2 = PciAddr.of_string_exn "0000:02:00.0" in
  let pci_addr3 = PciAddr.of_string_exn "0000:02:00.0" in
  let pci_addr4 = PciAddr.of_string_exn "0000:03:00.0" in
  let pci_addr5 = PciAddr.of_string_exn "0000:04:00.0" in
  let pci_addr6 = PciAddr.of_string_exn "0000:04:00.0" in
  let pci_addr7 = PciAddr.of_string_exn "0000:04:00.0" in
  let pci_addr8 = PciAddr.of_string_exn "0000:05:00.0" in

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
    UnOrderedNetDev.
      [
        {
          net_dev=
            {name= "side-0-eth0"; pci_addr= pci_addr0; mac_addr= mac_addr0}
        ; bios_eth_order= 0
        }
      ; {
          net_dev=
            {name= "side-0-eth2"; pci_addr= pci_addr1; mac_addr= mac_addr1}
        ; bios_eth_order= 2
        }
      ; {
          net_dev=
            {name= "side-0-eth6"; pci_addr= pci_addr2; mac_addr= mac_addr2}
        ; bios_eth_order= 6
        }
      ; {
          net_dev=
            {name= "side-0-eth1"; pci_addr= pci_addr3; mac_addr= mac_addr3}
        ; bios_eth_order= 1
        }
      ; {
          net_dev=
            {name= "side-0-eth4"; pci_addr= pci_addr4; mac_addr= mac_addr4}
        ; bios_eth_order= 4
        }
      ; {
          net_dev=
            {name= "side-0-eth5"; pci_addr= pci_addr5; mac_addr= mac_addr5}
        ; bios_eth_order= 7
        }
      ; {
          net_dev=
            {name= "side-0-eth3"; pci_addr= pci_addr6; mac_addr= mac_addr6}
        ; bios_eth_order= 3
        }
      ; {
          net_dev=
            {name= "side-0-eth7"; pci_addr= pci_addr7; mac_addr= mac_addr7}
        ; bios_eth_order= 5
        }
      ; {
          net_dev=
            {name= "side-0-eth8"; pci_addr= pci_addr8; mac_addr= mac_addr8}
        ; bios_eth_order= 8
        }
      ]
    |> ListToMacaddrMap.to_11_map ~by:(fun v ->
           v.UnOrderedNetDev.net_dev.mac_addr
       )
  in
  let _max, ordered, _olds =
    generate_order ~currents ~mappings:[] ~lasts:MacaddrMap.empty
      ~olds:MacaddrMap.empty
  in
  Alcotest.(check int) "9 devices in the ordered" 9 (List.length ordered) ;
  Alcotest.(check int) "position assigned" 0 (pos_of_mac mac_addr0 ordered) ;
  Alcotest.(check int) "position assigned" 1 (pos_of_mac mac_addr1 ordered) ;
  Alcotest.(check int) "position assigned" 2 (pos_of_mac mac_addr2 ordered) ;
  Alcotest.(check int) "position assigned" 6 (pos_of_mac mac_addr3 ordered) ;
  Alcotest.(check int) "position assigned" 4 (pos_of_mac mac_addr4 ordered) ;
  Alcotest.(check int) "position assigned" 3 (pos_of_mac mac_addr5 ordered) ;
  Alcotest.(check int) "position assigned" 5 (pos_of_mac mac_addr6 ordered) ;
  Alcotest.(check int) "position assigned" 7 (pos_of_mac mac_addr7 ordered) ;
  Alcotest.(check int) "position assigned" 8 (pos_of_mac mac_addr8 ordered)

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
