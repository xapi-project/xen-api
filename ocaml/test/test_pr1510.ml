(* Test cases for PR-1510 *)

open Fun
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
	let props =	[ "mode", "lacp" ; "lacp-time", arg ; ]
	and correct_props =
		[ "lacp=active";
			"bond_mode=balance-tcp";
			Printf.sprintf "other-config:lacp-time=%s" arg ]
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
		Printf.sprintf "other-config:lacp-aggregation-key=%s" arg ;
	] in

	let propset = OSSet.of_list props in
	let correctset = OSSet.of_list correct_props in
	OSSet.assert_equal correctset propset ;

	let propset = OSSet.of_list per_iface_props in
	let correctset = OSSet.of_list correct_iface_props in
	OSSet.assert_equal correctset propset

module OVS_Cli_test = struct
	include Ovs.Cli
	let vsctl ?(log=true) args =
		List.iter (fun a -> print_endline a) args ;
		"ok."
end

let test_lacp_aggregation_key_vsctl arg () =
	let module Ovs = Ovs.Make(OVS_Cli_test) in
	let bond = "bond0"
	and ifaces = ["eth0"; "eth1"]
	and bridge = "xapi1"
	and props = [ "mode", "lacp" ; "lacp-aggregation-key", arg ]
	in
	Ovs.create_bond bond ifaces bridge props |> ignore

let suite =
	"pr1510_suite" >:::
		[
			"test_lacp_timeout_prop(slow)" >:: test_lacp_timeout_prop "slow";
			"test_lacp_timeout_prop(fast)" >:: test_lacp_timeout_prop "fast";
			"test_lacp_aggregation_key(42)" >:: test_lacp_aggregation_key "42";
			"test_lacp_aggregation_key_vsctl" >:: test_lacp_aggregation_key_vsctl "42";
		]
