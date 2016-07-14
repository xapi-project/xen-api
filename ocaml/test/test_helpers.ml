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

open OUnit
open Test_common
open Test_highlevel
open Stdext

type pif = {device: string; management: bool; other_config: (string * string) list}

module DetermineGateway = Generic.Make(Generic.EncapsulateState(struct
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
                                             Test_printers.(assoc_pair
                                                              (list string_of_pif)
                                                              (option string))

                                           let string_of_output_t =
                                             Test_printers.(assoc_pair
                                                              (option string)
                                                              (option string))
                                         end
                                         module State = Test_state.XapiDb

                                         let load_input __context (pifs, _) =
                                           make_localhost ~__context ();
                                           List.iter (fun pif ->
                                               let network = make_network ~__context () in
                                               let _ = make_pif ~__context ~network ~host:!Xapi_globs.localhost_ref
                                                   ~ip_configuration_mode:`DHCP ~device:pif.device
                                                   ~management:pif.management ~other_config:pif.other_config () in
                                               ()
                                             ) pifs

                                         let extract_output __context (_, mgmt) =
                                           let management_interface = Stdext.Opt.map (fun device ->
                                               let open Db_filter_types in
                                               let pifs = Db.PIF.get_refs_where ~__context ~expr:(Eq (Field "device", Literal device)) in
                                               List.hd pifs
                                             ) mgmt in
                                           let gateway, dns = Helpers.determine_gateway_and_dns_ifs ~__context ?management_interface () in
                                           let get_device = Stdext.Opt.map (fun (self, _) -> Db.PIF.get_device ~__context ~self) in
                                           get_device gateway,
                                           get_device dns

                                         let tests = [
                                           ([
                                             {device="eth0"; management=true; other_config=[]};
                                             {device="eth1"; management=false; other_config=[]}],
                                             None
                                           ),
                                           (Some "eth0", Some "eth0");

                                           ([
                                             {device="eth0"; management=true; other_config=[]};
                                             {device="eth1"; management=false; other_config=[]}],
                                             Some "eth1"
                                           ),
                                           (Some "eth1", Some "eth1");

                                           ([
                                             {device="eth0"; management=true; other_config=[]};
                                             {device="eth1"; management=false; other_config=["defaultroute","true"]}],
                                             None
                                           ),
                                           (Some "eth1", Some "eth0");

                                           ([
                                             {device="eth0"; management=true; other_config=[]};
                                             {device="eth1"; management=false; other_config=["peerdns","true"]}],
                                             None
                                           ),
                                           (Some "eth0", Some "eth1");

                                           ([
                                             {device="eth0"; management=false; other_config=[]};
                                             {device="eth1"; management=false; other_config=["defaultroute","true"]}],
                                             Some "eth0"
                                           ),
                                           (Some "eth1", Some "eth0");

                                           ([
                                             {device="eth0"; management=false; other_config=[]};
                                             {device="eth1"; management=false; other_config=["peerdns","true"]}],
                                             Some "eth0"
                                           ),
                                           (Some "eth0", Some "eth1");
                                         ]
                                       end))

module PortCheckers = Generic.Make (struct
    module Io = struct
      type input_t = (int * string)
      type output_t = (exn, unit) Either.t

      let string_of_input_t = Test_printers.(pair int string)
      let string_of_output_t = Test_printers.(either exn unit)
    end

    open Api_errors
    open Either

    let transform (port, name) =
      try
        Right (Helpers.assert_is_valid_tcp_udp_port ~port ~name)
      with e ->
        Left e

    let tests = [
      (-22, "myport"),
      left (Server_error
              (value_not_supported,
               ["myport"; "-22"; "Port out of range"]));
      (0, "myport"),
      left (Server_error
              (value_not_supported,
               ["myport"; "0"; "Port out of range"]));
      (1, "myport"),
      Right ();
      (1234, "myport"),
      Right ();
      (65535, "myport"),
      Right ();
      (65536, "myport"),
      left (Server_error
              (value_not_supported,
               ["myport"; "65536"; "Port out of range"]));
      (123456, "myport"),
      left (Server_error
              (value_not_supported,
               ["myport"; "123456"; "Port out of range"]));
    ]
  end)

module PortRangeCheckers = Generic.Make (struct
    module Io = struct
      type input_t = ((int * string) * (int * string))
      type output_t = (exn, unit) Either.t

      let string_of_input_t =
        Test_printers.(pair (pair int string) (pair int string))
      let string_of_output_t = Test_printers.(either exn unit)
    end

    open Api_errors
    open Either

    let transform ((first_port, first_name), (last_port, last_name)) =
      try
        Right (Helpers.assert_is_valid_tcp_udp_port_range
                 ~first_port ~first_name ~last_port ~last_name)
      with e -> Left e

    let tests = [
      ((-22, "first_port"), (1234, "last_port")),
      left (Server_error
              (value_not_supported,
               ["first_port"; "-22"; "Port out of range"]));
      ((1, "first_port"), (1234, "last_port")),
      Right ();
      ((1234, "first_port"), (1234, "last_port")),
      Right ();
      ((1234, "first_port"), (5678, "last_port")),
      Right ();
      ((1234, "first_port"), (65535, "last_port")),
      Right ();
      ((1234, "first_port"), (123456, "last_port")),
      Left (Server_error
              (value_not_supported,
               ["last_port"; "123456"; "Port out of range"]));
      ((5678, "first_port"), (1234, "last_port")),
      Left (Server_error
              (value_not_supported,
               ["last_port"; "1234"; "last_port smaller than first_port"]));
    ]
  end)

module IPCheckers = Generic.Make (struct
    module Io = struct
      type input_t = [`ipv4 | `ipv6] * string * string
      type output_t = (exn, unit) Either.t

      let string_of_input_t =
        let open Test_printers in
        let kind : [`ipv4 | `ipv6] printer = function `ipv4 -> "IPv4" | `ipv6 -> "IPv6" in
        tuple3 kind string string

      let string_of_output_t = Test_printers.(either exn unit)
    end

    open Either
    open Api_errors

    let transform (kind, field, address) =
      try
        Right (Helpers.assert_is_valid_ip kind field address)
      with e ->
        Left e

    let tests = [
      (`ipv4, "address", "192.168.0.1"), (Right ());
      (`ipv4, "address", "255.255.255.0"), (Right ());
      (`ipv4, "address1", ""), (Left (Server_error(invalid_ip_address_specified, ["address1"])));
      (`ipv4, "address2", "192.168.0.300"), (Left (Server_error(invalid_ip_address_specified, ["address2"])));
      (`ipv4, "address3", "192.168.0"), (Left (Server_error(invalid_ip_address_specified, ["address3"])));
      (`ipv4, "address4", "bad-address"), (Left (Server_error(invalid_ip_address_specified, ["address4"])));
      (`ipv6, "address5", "192.168.0.1"), (Left (Server_error(invalid_ip_address_specified, ["address5"])));

      (`ipv6, "address", "fe80::bae8:56ff:fe29:894a"), (Right ());
      (`ipv6, "address", "fe80:0000:0000:0000:bae8:56ff:fe29:894a"), (Right ());
      (`ipv6, "address", "::1"), (Right ());
      (`ipv6, "address1", ""), (Left (Server_error(invalid_ip_address_specified, ["address1"])));
      (`ipv6, "address2", "fe80:0000:0000:0000:bae8:56ff:fe29:894a:0000"), (Left (Server_error(invalid_ip_address_specified, ["address2"])));
      (`ipv6, "address3", "bad-address"), (Left (Server_error(invalid_ip_address_specified, ["address3"])));
      (`ipv4, "address4", "fe80::bae8:56ff:fe29:894a"), (Left (Server_error(invalid_ip_address_specified, ["address4"])));
      (`ipv6, "address5", "ze80::bae8:56ff:fe29:894a"), (Left (Server_error(invalid_ip_address_specified, ["address5"])));
    ]
  end)

module CIDRCheckers = Generic.Make (struct
    module Io = struct
      type input_t = [`ipv4 | `ipv6] * string * string
      type output_t = (exn, unit) Either.t

      let string_of_input_t =
        let open Test_printers in
        let kind : [`ipv4 | `ipv6] printer = function `ipv4 -> "IPv4" | `ipv6 -> "IPv6" in
        tuple3 kind string string

      let string_of_output_t = Test_printers.(either exn unit)
    end

    open Either
    open Api_errors

    let transform (kind, field, cidr) =
      try
        Right (Helpers.assert_is_valid_cidr kind field cidr)
      with e ->
        Left e

    let tests = [
      (`ipv4, "address", "192.168.0.1/24"), (Right ());
      (`ipv4, "address", "255.255.255.0/32"), (Right ());
      (`ipv4, "address1", ""), (Left (Server_error(invalid_cidr_address_specified, ["address1"])));
      (`ipv4, "address1", "192.168.0.2"), (Left (Server_error(invalid_cidr_address_specified, ["address1"])));
      (`ipv4, "address1", "192.168.0.2/33"), (Left (Server_error(invalid_cidr_address_specified, ["address1"])));
      (`ipv4, "address1", "192.168.0.2/x"), (Left (Server_error(invalid_cidr_address_specified, ["address1"])));
      (`ipv4, "address2", "192.168.0.300/10"), (Left (Server_error(invalid_cidr_address_specified, ["address2"])));
      (`ipv4, "address3", "192.168.0/20"), (Left (Server_error(invalid_cidr_address_specified, ["address3"])));
      (`ipv4, "address4", "bad-address/24"), (Left (Server_error(invalid_cidr_address_specified, ["address4"])));
      (`ipv6, "address5", "192.168.0.1/24"), (Left (Server_error(invalid_cidr_address_specified, ["address5"])));

      (`ipv6, "address", "fe80::bae8:56ff:fe29:894a/64"), (Right ());
      (`ipv6, "address", "fe80:0000:0000:0000:bae8:56ff:fe29:894a/80"), (Right ());
      (`ipv6, "address", "::1/128"), (Right ());
      (`ipv6, "address1", ""), (Left (Server_error(invalid_cidr_address_specified, ["address1"])));
      (`ipv6, "address2", "fe80::bae8:56ff:fe29:894a:0000/129"), (Left (Server_error(invalid_cidr_address_specified, ["address2"])));
      (`ipv6, "address2", "fe80::bae8:56ff:fe29:894a:0000"), (Left (Server_error(invalid_cidr_address_specified, ["address2"])));
      (`ipv6, "address3", "bad-address/64"), (Left (Server_error(invalid_cidr_address_specified, ["address3"])));
      (`ipv4, "address4", "fe80::bae8:56ff:fe29:894a/64"), (Left (Server_error(invalid_cidr_address_specified, ["address4"])));
      (`ipv6, "address5", "ze80::bae8:56ff:fe29:894a/64"), (Left (Server_error(invalid_cidr_address_specified, ["address5"])));
    ]
  end)

let test =
  "test_helpers" >:::
  [
    "test_determine_gateway" >::: DetermineGateway.tests;
    "test_assert_is_valid_tcp_udp_port" >::: PortCheckers.tests;
    "test_assert_is_valid_tcp_udp_port_range" >::: PortRangeCheckers.tests;
    "test_assert_is_valid_ip" >::: IPCheckers.tests;
    "test_assert_is_valid_cidr" >::: CIDRCheckers.tests;
  ]
