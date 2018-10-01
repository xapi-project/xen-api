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

open OUnit
open Network_utils

(* Example of using OUnitDiff with a String Set *)
module StringDiff =
struct
  type t = string
  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module OSSet = OUnitDiff.SetMake(StringDiff)

let run_bond_prop_test props c_props c_per_iface =
  let props, per_iface_props =
    Ovs.make_bond_properties "bond_test" props in

  let propset = OSSet.of_list props in
  let correctset = OSSet.of_list c_props in
  OSSet.assert_equal correctset propset ;

  let propset = OSSet.of_list per_iface_props in
  let correctset = OSSet.of_list c_per_iface in
  OSSet.assert_equal correctset propset

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

  let propset = OSSet.of_list props in
  let correctset = OSSet.of_list correct_props in
  OSSet.assert_equal correctset propset ;

  let propset = OSSet.of_list per_iface_props in
  let correctset = OSSet.of_list correct_iface_props in
  OSSet.assert_equal correctset propset

module OVS_Cli_test = struct
  include Ovs.Cli
  let vsctl_output = ref []
  let vsctl args =
    vsctl_output := args ;
    String.concat " " args
end

(* XXX TODO write this test *)
let test_lacp_aggregation_key_vsctl arg () = skip_if true "Unimplemented" ;
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
  assert_bool "lacp_aggregation_key is passed to ovs-vsctl command"
    (List.exists
       (fun s -> (String.trim s) == answer)
       !OVS_Cli_test.vsctl_output)

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
       assert_bool "key=value argument pairs can't have missing values"
         (let open Astring.String in
          arg |> trim |> is_suffix ~affix:"=" |> not))
    !OVS_Cli_test.vsctl_output

let suite =
  "lacp_properties" >:::
  [
    "test_lacp_timeout_prop(slow)" >:: test_lacp_timeout_prop "slow";
    "test_lacp_timeout_prop(fast)" >:: test_lacp_timeout_prop "fast";
    "test_lacp_aggregation_key(42)" >:: test_lacp_aggregation_key "42";
    "test_lacp_aggregation_key_vsctl" >:: test_lacp_aggregation_key_vsctl "42";
    "test_lacp_defaults_bond_create" >:: test_lacp_defaults_bond_create;
  ]
