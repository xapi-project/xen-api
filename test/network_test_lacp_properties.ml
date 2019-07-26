(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

open Network_utils

let check_string_list = Alcotest.(check (list string))
let to_test_string prefix ps = Format.sprintf "%s: %s" prefix (String.concat ";" ps)

let run_bond_prop_test props c_props c_per_iface =
  let props, per_iface_props =
    Ovs.make_bond_properties "bond_test" props in
  check_string_list (to_test_string "c_props" c_props) c_props props;
  check_string_list (to_test_string "c_per_iface" c_per_iface) c_per_iface per_iface_props

let test_lacp_timeout_prop arg () =
  let props = [ "mode", "lacp" ; "lacp-time", arg ; ]
  and correct_props =
    [ "lacp=active";
      "bond_mode=balance-tcp";
      Printf.sprintf "other-config:lacp-time=\"%s\"" arg ]
  and correct_iface_props = [ ] in

  run_bond_prop_test props correct_props correct_iface_props

let test_lacp_aggregation_key arg () =
  let props, per_iface_props = Ovs.make_bond_properties "bond_test"
      [ "mode", "lacp" ; "lacp-aggregation-key", arg ]
  and correct_props = [
    "lacp=active";
    "bond_mode=balance-tcp";
  ]
  and correct_iface_props = [
    Printf.sprintf "other-config:lacp-aggregation-key=\"%s\"" arg ;
  ] in

  check_string_list "lacp_aggregation_key_correct_props" correct_props props;
  check_string_list "lacp_aggregation_key_correct_iface_props" correct_iface_props per_iface_props

module OVS_Cli_test = struct
  include Ovs.Cli
  let vsctl_output = ref []
  let vsctl ?log args =
    vsctl_output := args ;
    String.concat " " args
end

(* XXX TODO write this test *)
let test_lacp_aggregation_key_vsctl arg () =
  let module Ovs = Ovs.Make(OVS_Cli_test) in
  let bond = "bond0"
  and ifaces = ["eth0"; "eth1"]
  and bridge = "xapi1"
  and props = [ "mode", "lacp" ; "lacp-aggregation-key", arg ]
  (* other-config:lacp-aggregation-key=42 *)
  and answer = "other-config:lacp-aggregation-key=" ^ arg
  in
  Ovs.create_bond bond ifaces bridge props |> ignore ;
  List.iter print_endline !OVS_Cli_test.vsctl_output ;
  print_endline answer ;
  (* todo: pass -> replace with bool *)
  Alcotest.(check pass "lacp_aggregation_key is passed to ovs-vsctl command" true
    (List.exists
       (fun s -> (String.trim s) == answer)
       !OVS_Cli_test.vsctl_output))

(* Test case for bond_create with default lacp-{time,aggregation-key} settings.
   This should not call ovs-vsctl with unfinished key=value arguments. So we
   shouldn't have somthing like "other-config:lacp-aggregation-key= ". *)
let test_lacp_defaults_bond_create () =
  let module Ovs = Ovs.Make(OVS_Cli_test) in
  let bond = "bond0"
  and ifaces = ["eth0"; "eth1"]
  and bridge = "xapi1"
  and default_props = [
    "mode", "lacp";
    "lacp-time", "slow";
    "hashing_algorithm", "tcpudp_ports";
  ]
  in
  Ovs.create_bond bond ifaces bridge default_props |> ignore;
  (* should not have any strings which contain lacp-aggregation-key *)
  (*assert_bool "no default property for lacp_aggregation_key"
    		List.exists (fun s -> String.*)
  List.iter
    (fun arg ->
       Alcotest.(check bool "key=value argument pairs can't have missing values" true
         (let open Astring.String in
          arg |> trim |> is_suffix ~affix:"=" |> not)))
    !OVS_Cli_test.vsctl_output

let suite = [ "test_lacp", [ "timeout_prop(slow)", `Quick,  test_lacp_timeout_prop "slow";
                             "timeout_prop(fast)", `Quick, test_lacp_timeout_prop "fast";
                             "aggregation_key(42)", `Quick,  test_lacp_aggregation_key "42";
                             "aggregation_key_vsctl", `Quick, test_lacp_aggregation_key_vsctl "42";
                             "defaults_bond_create", `Quick, test_lacp_defaults_bond_create;
                           ]
            ]
