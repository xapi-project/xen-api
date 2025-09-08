(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Test_common
open Test_highlevel

type pif = {
    device: string
  ; management: bool
  ; other_config: (string * string) list
}

module DetermineGateway = Generic.MakeStateful (struct
  module Io = struct
    (* The type of inputs to a system being tested. *)
    type input_t = pif list * string option

    (* The type of outputs from a system being tested. *)
    type output_t = string option * string option

    (* Helper functions for printing error messages on test failure. *)
    let string_of_pif pif =
      Printf.sprintf "[device = %s; management = %b; other_config = %s]"
        pif.device pif.management
        (Test_printers.(assoc_list string string) pif.other_config)

    let string_of_input_t =
      Test_printers.(assoc_pair (list string_of_pif) (option string))

    let string_of_output_t =
      Test_printers.(assoc_pair (option string) (option string))
  end

  module State = Test_state.XapiDb

  let load_input __context (pifs, _) =
    make_localhost ~__context () ;
    List.iter
      (fun pif ->
        let network = make_network ~__context () in
        let _ =
          make_pif ~__context ~network ~host:!Xapi_globs.localhost_ref
            ~ip_configuration_mode:`DHCP ~device:pif.device
            ~management:pif.management ~other_config:pif.other_config ()
        in
        ()
      )
      pifs

  let extract_output __context (_, mgmt) =
    let management_interface =
      Option.map
        (fun device ->
          let open Xapi_database.Db_filter_types in
          let pifs =
            Db.PIF.get_refs_where ~__context
              ~expr:(Eq (Field "device", Literal device))
          in
          List.hd pifs
        )
        mgmt
    in
    let gateway, dns =
      Helpers.determine_gateway_and_dns_ifs ~__context ?management_interface ()
    in
    let get_device =
      Option.map (fun (self, _) -> Db.PIF.get_device ~__context ~self)
    in
    (get_device gateway, get_device dns)

  let tests =
    `QuickAndAutoDocumented
      [
        ( ( [
              {device= "eth0"; management= true; other_config= []}
            ; {device= "eth1"; management= false; other_config= []}
            ]
          , None
          )
        , (Some "eth0", Some "eth0")
        )
      ; ( ( [
              {device= "eth0"; management= true; other_config= []}
            ; {device= "eth1"; management= false; other_config= []}
            ]
          , Some "eth1"
          )
        , (Some "eth1", Some "eth1")
        )
      ; ( ( [
              {device= "eth0"; management= true; other_config= []}
            ; {
                device= "eth1"
              ; management= false
              ; other_config= [("defaultroute", "true")]
              }
            ]
          , None
          )
        , (Some "eth1", Some "eth0")
        )
      ; ( ( [
              {device= "eth0"; management= true; other_config= []}
            ; {
                device= "eth1"
              ; management= false
              ; other_config= [("peerdns", "true")]
              }
            ]
          , None
          )
        , (Some "eth0", Some "eth1")
        )
      ; ( ( [
              {device= "eth0"; management= false; other_config= []}
            ; {
                device= "eth1"
              ; management= false
              ; other_config= [("defaultroute", "true")]
              }
            ]
          , Some "eth0"
          )
        , (Some "eth1", Some "eth0")
        )
      ; ( ( [
              {device= "eth0"; management= false; other_config= []}
            ; {
                device= "eth1"
              ; management= false
              ; other_config= [("peerdns", "true")]
              }
            ]
          , Some "eth0"
          )
        , (Some "eth0", Some "eth1")
        )
      ]
end)

let string_of_unit_result =
  Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))

module PortCheckers = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int * string

    type output_t = (unit, exn) result

    let string_of_input_t = Fmt.(str "%a" Dump.(pair int string))

    let string_of_output_t = string_of_unit_result
  end

  open Api_errors

  let transform (port, name) =
    try Ok (Helpers.assert_is_valid_tcp_udp_port ~port ~name)
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ( (-22, "myport")
        , Error
            (Server_error
               (value_not_supported, ["myport"; "-22"; "Port out of range"])
            )
        )
      ; ( (0, "myport")
        , Error
            (Server_error
               (value_not_supported, ["myport"; "0"; "Port out of range"])
            )
        )
      ; ((1, "myport"), Ok ())
      ; ((1234, "myport"), Ok ())
      ; ((65535, "myport"), Ok ())
      ; ( (65536, "myport")
        , Error
            (Server_error
               (value_not_supported, ["myport"; "65536"; "Port out of range"])
            )
        )
      ; ( (123456, "myport")
        , Error
            (Server_error
               (value_not_supported, ["myport"; "123456"; "Port out of range"])
            )
        )
      ]
end)

module PortRangeCheckers = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (int * string) * (int * string)

    type output_t = (unit, exn) result

    let string_of_input_t =
      Fmt.(str "%a" Dump.(pair (pair int string) (pair int string)))

    let string_of_output_t = string_of_unit_result
  end

  open Api_errors

  let transform ((first_port, first_name), (last_port, last_name)) =
    try
      Ok
        (Helpers.assert_is_valid_tcp_udp_port_range ~first_port ~first_name
           ~last_port ~last_name
        )
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ( ((-22, "first_port"), (1234, "last_port"))
        , Error
            (Server_error
               (value_not_supported, ["first_port"; "-22"; "Port out of range"])
            )
        )
      ; (((1, "first_port"), (1234, "last_port")), Ok ())
      ; (((1234, "first_port"), (1234, "last_port")), Ok ())
      ; (((1234, "first_port"), (5678, "last_port")), Ok ())
      ; (((1234, "first_port"), (65535, "last_port")), Ok ())
      ; ( ((1234, "first_port"), (123456, "last_port"))
        , Error
            (Server_error
               ( value_not_supported
               , ["last_port"; "123456"; "Port out of range"]
               )
            )
        )
      ; ( ((5678, "first_port"), (1234, "last_port"))
        , Error
            (Server_error
               ( value_not_supported
               , ["last_port"; "1234"; "last_port smaller than first_port"]
               )
            )
        )
      ]
end)

module IPCheckers = Generic.MakeStateless (struct
  module Io = struct
    type input_t = [`ipv4 | `ipv6 | `ipv4or6] * string * string

    type output_t = (unit, exn) result

    let string_of_input_t =
      let open Test_printers in
      let kind : [`ipv4 | `ipv6 | `ipv4or6] printer = function
        | `ipv4 ->
            "IPv4"
        | `ipv6 ->
            "IPv6"
        | `ipv4or6 ->
            "IP*"
      in
      tuple3 kind string string

    let string_of_output_t = string_of_unit_result
  end

  open Api_errors

  let transform (kind, field, address) =
    try Ok (Helpers.assert_is_valid_ip kind field address) with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ((`ipv4, "address", "192.168.0.1"), Ok ())
      ; ((`ipv4, "address", "255.255.255.0"), Ok ())
      ; ( (`ipv4, "address1", "")
        , Error (Server_error (invalid_ip_address_specified, ["address1"]))
        )
      ; ( (`ipv4, "address2", "192.168.0.300")
        , Error (Server_error (invalid_ip_address_specified, ["address2"]))
        )
      ; ( (`ipv4, "address3", "192.168.0")
        , Error (Server_error (invalid_ip_address_specified, ["address3"]))
        )
      ; ( (`ipv4, "address4", "bad-address")
        , Error (Server_error (invalid_ip_address_specified, ["address4"]))
        )
      ; ( (`ipv6, "address5", "192.168.0.1")
        , Error (Server_error (invalid_ip_address_specified, ["address5"]))
        )
      ; ((`ipv6, "address", "fe80::bae8:56ff:fe29:894a"), Ok ())
      ; ((`ipv6, "address", "fe80:0000:0000:0000:bae8:56ff:fe29:894a"), Ok ())
      ; ((`ipv6, "address", "::1"), Ok ())
      ; ( (`ipv6, "address1", "")
        , Error (Server_error (invalid_ip_address_specified, ["address1"]))
        )
      ; ( (`ipv6, "address2", "fe80:0000:0000:0000:bae8:56ff:fe29:894a:0000")
        , Error (Server_error (invalid_ip_address_specified, ["address2"]))
        )
      ; ( (`ipv6, "address3", "bad-address")
        , Error (Server_error (invalid_ip_address_specified, ["address3"]))
        )
      ; ( (`ipv4, "address4", "fe80::bae8:56ff:fe29:894a")
        , Error (Server_error (invalid_ip_address_specified, ["address4"]))
        )
      ; ( (`ipv6, "address5", "ze80::bae8:56ff:fe29:894a")
        , Error (Server_error (invalid_ip_address_specified, ["address5"]))
        )
      ; ((`ipv4or6, "address5", "192.168.0.1"), Ok ())
      ; ((`ipv4or6, "address6", "fe80::bae8:56ff:fe29:894a"), Ok ())
      ; ( (`ipv4or6, "address7", "ze80::bae8:56ff:fe29:894a")
        , Error (Server_error (invalid_ip_address_specified, ["address7"]))
        )
      ; ( (`ipv4or6, "address8", "192.168.0.300")
        , Error (Server_error (invalid_ip_address_specified, ["address8"]))
        )
      ]
end)

module CIDRCheckers = Generic.MakeStateless (struct
  module Io = struct
    type input_t = [`ipv4 | `ipv6 | `ipv4or6] * string * string

    type output_t = (unit, exn) result

    let string_of_input_t =
      let open Test_printers in
      let kind : [`ipv4 | `ipv6 | `ipv4or6] printer = function
        | `ipv4 ->
            "IPv4"
        | `ipv6 ->
            "IPv6"
        | `ipv4or6 ->
            "IP*"
      in
      tuple3 kind string string

    let string_of_output_t = string_of_unit_result
  end

  open Api_errors

  let transform (kind, field, cidr) =
    try Ok (Helpers.assert_is_valid_cidr kind field cidr) with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ((`ipv4, "address", "192.168.0.1/24"), Ok ())
      ; ((`ipv4, "address", "255.255.255.0/32"), Ok ())
      ; ( (`ipv4, "address1", "")
        , Error (Server_error (invalid_cidr_address_specified, ["address1"]))
        )
      ; ( (`ipv4, "address1", "192.168.0.2")
        , Error (Server_error (invalid_cidr_address_specified, ["address1"]))
        )
      ; ( (`ipv4, "address1", "192.168.0.2/33")
        , Error (Server_error (invalid_cidr_address_specified, ["address1"]))
        )
      ; ( (`ipv4, "address1", "192.168.0.2/x")
        , Error (Server_error (invalid_cidr_address_specified, ["address1"]))
        )
      ; ( (`ipv4, "address2", "192.168.0.300/10")
        , Error (Server_error (invalid_cidr_address_specified, ["address2"]))
        )
      ; ( (`ipv4, "address3", "192.168.0/20")
        , Error (Server_error (invalid_cidr_address_specified, ["address3"]))
        )
      ; ( (`ipv4, "address4", "bad-address/24")
        , Error (Server_error (invalid_cidr_address_specified, ["address4"]))
        )
      ; ( (`ipv6, "address5", "192.168.0.1/24")
        , Error (Server_error (invalid_cidr_address_specified, ["address5"]))
        )
      ; ((`ipv6, "address", "fe80::bae8:56ff:fe29:894a/64"), Ok ())
      ; ((`ipv6, "address", "fe80:0000:0000:0000:bae8:56ff:fe29:894a/80"), Ok ())
      ; ((`ipv6, "address", "::1/128"), Ok ())
      ; ( (`ipv6, "address1", "")
        , Error (Server_error (invalid_cidr_address_specified, ["address1"]))
        )
      ; ( (`ipv6, "address2", "fe80::bae8:56ff:fe29:894a:0000/129")
        , Error (Server_error (invalid_cidr_address_specified, ["address2"]))
        )
      ; ( (`ipv6, "address2", "fe80::bae8:56ff:fe29:894a:0000")
        , Error (Server_error (invalid_cidr_address_specified, ["address2"]))
        )
      ; ( (`ipv6, "address3", "bad-address/64")
        , Error (Server_error (invalid_cidr_address_specified, ["address3"]))
        )
      ; ( (`ipv4, "address4", "fe80::bae8:56ff:fe29:894a/64")
        , Error (Server_error (invalid_cidr_address_specified, ["address4"]))
        )
      ; ( (`ipv6, "address5", "ze80::bae8:56ff:fe29:894a/64")
        , Error (Server_error (invalid_cidr_address_specified, ["address5"]))
        )
      ; ( (`ipv4or6, "address6", "bad-address/64")
        , Error (Server_error (invalid_cidr_address_specified, ["address6"]))
        )
      ; ((`ipv4or6, "address7", "fe80::bae8:56ff:fe29:894a/64"), Ok ())
      ; ( (`ipv4or6, "address8", "ze80::bae8:56ff:fe29:894a/64")
        , Error (Server_error (invalid_cidr_address_specified, ["address8"]))
        )
      ; ((`ipv4or6, "address9", "255.255.255.0/32"), Ok ())
      ; ( (`ipv4or6, "address10", "192.168.0.2/33")
        , Error (Server_error (invalid_cidr_address_specified, ["address10"]))
        )
      ]
end)

module RunInParallel = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (unit -> string) list * int

    type output_t = (string list, exn) result

    let string_of_input_t (_, c) = string_of_int c

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(list string) ~error:exn))
  end

  let transform (funs, capacity) =
    try Ok (Helpers.run_in_parallel ~funs ~capacity) with e -> Error e

  let f () = "hello"

  exception Unittest_exception

  let fe () =
    match f () with "not-hello" -> "hello" | _ -> raise Unittest_exception

  let range_f_64 = List.init 64 (fun i () -> string_of_int i)

  let range_64 = List.init 64 string_of_int

  let tests =
    `QuickAndAutoDocumented
      [
        (([], 0), Ok [])
      ; (([f], 0), Ok [])
      ; (([f], 1), Ok ["hello"])
      ; (([f], 2), Ok ["hello"])
      ; (([f; f], 1), Ok ["hello"; "hello"])
      ; (([f; f], 2), Ok ["hello"; "hello"])
      ; (([f; f], 3), Ok ["hello"; "hello"])
      ; ((range_f_64, 64), Ok range_64)
      ; (([fe], 0), Ok [])
      ; (([fe], 1), Error Unittest_exception)
      ; (([f; fe], 1), Error Unittest_exception)
      ; (([f; fe], 2), Error Unittest_exception)
      ; (([f; fe], 3), Error Unittest_exception)
      ]
end)

module Version = struct
  let test_compare_int_list () =
    let test_cases =
      [
        ("Equal Lists", [1; 2; 3], [1; 2; 3], 0)
      ; ("Empty Lists", [], [], 0)
      ; ("'a' is smaller (first element)", [1; 10; 100], [2; 0; 0], -1)
      ; ("'a' is smaller (later element)", [1; 2; 3], [1; 2; 4], -1)
      ; ("'a' is greater (first element)", [5; 1; 1], [2; 10; 10], 1)
      ; ("'a' is greater (later element)", [1; 3; 3], [1; 2; 4], 1)
      ; ("Lists with negative numbers", [0; -5; 10], [0; -2; -10], -1)
      ; ("Single element lists (equal)", [42], [42], 0)
      ; ("Single element lists (unequal)", [42], [43], -1)
      ; ("Different number of element in lists", [25; 27], [25; 27; 1], -1)
      ]
    in
    let test_compare (description, list1, list2, expected) =
      let actual = Helpers.compare_int_lists list1 list2 in
      let description = Printf.sprintf "compate_int_lists: %s" description in
      Alcotest.(check int) description expected actual
    in
    List.iter test_compare test_cases

  let test_version_numbers_of_string () =
    let test_cases =
      [
        ( "Standard major.minor.patch version, e.g. xapi build version stored in\n\
          \        the database"
        , "25.30.0"
        , [25; 30; 0]
        )
      ; ( "Dev build version, e.g. xapi build version stored in\n\
          \        the database"
        , "25.30.0.6.gb239bd75a"
        , [25; 30; 0; 6]
        )
      ; ( "Version with a patch identifier e.g. xen versions stored in the\n\
          \        database"
        , "25.15.0-13"
        , [25; 15; 0; 13]
        )
      ; ("Default version", "0.0.0", [0; 0; 0])
      ]
    in
    let test_version_numbers (description, version_string, expected) =
      let actual = Helpers.version_numbers_of_string version_string in
      let description =
        Printf.sprintf "version_numbers_of_string: %s" description
      in
      Alcotest.(check @@ list int) description expected actual
    in
    List.iter test_version_numbers test_cases

  let test_compare_versions () =
    let sw_vers_a =
      Xapi_globs.[(_platform_version, "2.4.0"); (_xen_version, "4.14.0-13")]
    in
    let sw_vers_b = Xapi_globs.[(_xen_version, "4.13.0-13")] in
    let test_cases =
      Xapi_globs.
        [
          ( "Software versions 'b' are missing platform version"
          , _platform_version
          , sw_vers_a
          , sw_vers_b
          , 1
          )
        ; ( "Software versions 'a' are missing platform version"
          , _platform_version
          , sw_vers_b
          , sw_vers_a
          , -1
          )
        ; ( "xen version exists in both (`a` is greater)"
          , _xen_version
          , sw_vers_a
          , sw_vers_b
          , 1
          )
        ; ( "xapi build version is missing from both (equal)"
          , _xapi_build_version
          , sw_vers_a
          , sw_vers_b
          , 0
          )
        ]
    in
    let test_compare (description, key, value_a, value_b, expected) =
      let actual = Helpers.compare_versions ~version_key:key value_a value_b in
      let description = Printf.sprintf "compare_versions: %s" description in
      Alcotest.(check int) description expected actual
    in
    List.iter test_compare test_cases

  let test_compare_all_versions () =
    let current =
      Xapi_globs.[(_platform_version, "8.1.0"); (_xen_version, "4.13.0-15")]
    in
    let newer =
      Xapi_globs.[(_platform_version, "8.2.0"); (_xen_version, "4.13.0-15")]
    in
    let mixed =
      Xapi_globs.[(_platform_version, "8.2.0"); (_xen_version, "4.12.0-15")]
    in
    let test_cases =
      [
        ("Newer is greater or equal than Current", newer, current, true)
      ; ("Current is greater or equal than Current", current, current, true)
      ; ("Current is not greater or equal than Newer", current, newer, false)
      ; ("Mixed is not greater or equal then Current", mixed, current, false)
      ; ("Current is not greater or equal than Mixed", current, mixed, false)
      ]
    in
    let test_compare (description, vers_a, vers_b, expected) =
      let actual =
        Helpers.compare_all_versions ~is_greater_or_equal:vers_a ~than:vers_b
      in
      let description = Printf.sprintf "compare_all_versions: %s" description in
      Alcotest.(check bool) description expected actual
    in
    List.iter test_compare test_cases

  let test =
    [
      ("Compare int list", `Quick, test_compare_int_list)
    ; ("Version numbers from string", `Quick, test_version_numbers_of_string)
    ; ("Compare versions", `Quick, test_compare_versions)
    ; ("Compare all versions", `Quick, test_compare_all_versions)
    ]

  let tests = [("Version compare tests", test)]
end

let tests =
  make_suite "helpers_"
    [
      ("determine_gateway", DetermineGateway.tests)
    ; ("assert_is_valid_tcp_udp_port", PortCheckers.tests)
    ; ("assert_is_valid_tcp_udp_port_range", PortRangeCheckers.tests)
    ; ("assert_is_valid_ip", IPCheckers.tests)
    ; ("assert_is_valid_cidr", CIDRCheckers.tests)
    ; ("run_in_parallel", RunInParallel.tests)
    ]
  @ Version.tests
