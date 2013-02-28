open Xmlrpc_client
open Fun
open Network_interface

let make_rpc path  =
	let module Rpc = struct
		let transport = ref (Unix path)
		let rpc call =
			XMLRPC_protocol.rpc ~transport:!transport ~http:(xmlrpc ~version:"1.0" "/") call
	end in
	(module Rpc : Network_interface.RPC)

module Rpc = (val (make_rpc (Filename.concat Fhs.vardir "xcp-networkd")) : Network_interface.RPC)
module Client = Client(Rpc)
open Client

let _ =
	let ifs = Interface.get_all () in
	List.iter print_endline ifs;
	Interface.get_mac "eth0" |> print_endline;
	Printf.printf "eth0 is up? %b\n" (Interface.is_up "eth0");
	let ipv4 = Interface.get_ipv4_addr "xenbr0" in
	Printf.printf "IPv4 of eth0: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	let ipv4 = Interface.get_ipv4_addr "xenbr1" in
	Printf.printf "IPv4 of eth1: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	Interface.set_ipv4_addr "xenbr1" (DHCP4 []);
	let ipv4 = Interface.get_ipv4_addr "xenbr1" in
	Printf.printf "IPv4 of eth1: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	Interface.set_ipv6_addr "xenbr1" (Static6 [Unix.inet_addr_of_string "fd::2", 64]);
	Interface.set_ipv6_gateway "xenbr1" (Unix.inet_addr_of_string "fd::1");
	let ipv6 = Interface.get_ipv6_addr "xenbr1" in
	Printf.printf "IPv6 of eth1: %s\n" (String.concat ", " (List.map (fun (ipv6, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv6) prefixlen) ipv6));
	Interface.set_ipv4_addr "xenbr1" None4;
	Interface.set_persistent "xenbr0" true;
	Interface.set_persistent "xenbr1" true;
	let ipv4 = Interface.get_ipv4_addr "xenbr1" in
	Printf.printf "IPv4 of eth1: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	Interface.set_ipv4_addr "xenbr0" None4;
	let ipv4 = Interface.get_ipv4_addr "xenbr0" in
	Printf.printf "IPv4 of eth0: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	Interface.set_ipv4_addr "xenbr0" (DHCP4 [`set_gateway; `set_dns]);
	Interface.set_ipv4_addr "xenbr1" (DHCP4 []);
	(match Interface.get_ipv4_gateway "xenbr0" with
	| None -> Printf.printf "Xenbr0 has no gateway\n"
	| Some g -> Printf.printf "The gateway of xenbr0 is %s\n" (Unix.string_of_inet_addr g));
	(match Interface.get_ipv4_gateway "xenbr1" with
	| None -> Printf.printf "Xenbr1 has no gateway\n"
	| Some g -> Printf.printf "The gateway of xenbr0 is %s\n" (Unix.string_of_inet_addr g));
	Printf.printf "DNS servers: %s\n" (String.concat ", " (List.map Unix.string_of_inet_addr (Interface.get_dns "eth1")));
	Printf.printf "MTU of eth0: %d\n" (Interface.get_mtu "eth0");
	Interface.set_ipv4_gateway "xenbr1" (Unix.inet_addr_of_string "10.80.224.1");
	let dns = Interface.get_dns "eth0" @ [Unix.inet_addr_of_string "10.80.16.127"] in
	Interface.set_dns "eth0" dns;
	(try
		ignore (Interface.is_up "blabla")
	with RpcFailure(msg, params) ->
		Printf.printf "%s [%s]\n" msg (String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ v) params))
	);

	Bridge.create None "test";
	Bridge.add_port "test" "port1" ["vif1.0"];
	List.iter print_endline (Bridge.get_all ());
	Bridge.set_persistent "test" true;
	print_endline "end."

