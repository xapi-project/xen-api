open Fun
open Network_interface

module Client = Network_client.Client
open Client

let _ =
	let ifs = Interface.get_all "test" () in
	List.iter print_endline ifs;
	Interface.get_mac "test" "eth0" |> print_endline;
	Printf.printf "eth0 is up? %b\n" (Interface.is_up "test" "eth0");
	let ipv4 = Interface.get_ipv4_addr "test" "xenbr0" in
	Printf.printf "IPv4 of eth0: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	let ipv4 = Interface.get_ipv4_addr "test" "xenbr1" in
	Printf.printf "IPv4 of eth1: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	Interface.set_ipv4_addr "test" "xenbr1" (DHCP4 []);
	let ipv4 = Interface.get_ipv4_addr "test" "xenbr1" in
	Printf.printf "IPv4 of eth1: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	Interface.set_ipv6_addr "test" "xenbr1" (Static6 [Unix.inet_addr_of_string "fd::2", 64]);
	Interface.set_ipv6_gateway "test" "xenbr1" (Unix.inet_addr_of_string "fd::1");
	let ipv6 = Interface.get_ipv6_addr "test" "xenbr1" in
	Printf.printf "IPv6 of eth1: %s\n" (String.concat ", " (List.map (fun (ipv6, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv6) prefixlen) ipv6));
	Interface.set_ipv4_addr "test" "xenbr1" None4;
	Interface.set_persistent "test" "xenbr0" true;
	Interface.set_persistent "test" "xenbr1" true;
	let ipv4 = Interface.get_ipv4_addr "test" "xenbr1" in
	Printf.printf "IPv4 of eth1: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	Interface.set_ipv4_addr "test" "xenbr0" None4;
	let ipv4 = Interface.get_ipv4_addr "test" "xenbr0" in
	Printf.printf "IPv4 of eth0: %s\n" (String.concat ", " (List.map (fun (ipv4, prefixlen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ipv4) prefixlen) ipv4));
	Interface.set_ipv4_addr "test" "xenbr0" (DHCP4 [`set_gateway; `set_dns]);
	Interface.set_ipv4_addr "test" "xenbr1" (DHCP4 []);
	(match Interface.get_ipv4_gateway "test" "xenbr0" with
	| None -> Printf.printf "Xenbr0 has no gateway\n"
	| Some g -> Printf.printf "The gateway of xenbr0 is %s\n" (Unix.string_of_inet_addr g));
	(match Interface.get_ipv4_gateway "test" "xenbr1" with
	| None -> Printf.printf "Xenbr1 has no gateway\n"
	| Some g -> Printf.printf "The gateway of xenbr0 is %s\n" (Unix.string_of_inet_addr g));
	Printf.printf "DNS servers: %s\n" (String.concat ", " (List.map Unix.string_of_inet_addr (Interface.get_dns "test" "eth1")));
	Printf.printf "MTU of eth0: %d\n" (Interface.get_mtu "test" "eth0");
	Interface.set_ipv4_gateway "test" "xenbr1" (Unix.inet_addr_of_string "10.80.224.1");
	let dns = Interface.get_dns "test" "eth0" @ [Unix.inet_addr_of_string "10.80.16.127"] in
	Interface.set_dns "test" "eth0" dns;
	(try
		ignore (Interface.is_up "test" "blabla")
	with RpcFailure(msg, params) ->
		Printf.printf "%s [%s]\n" msg (String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ v) params))
	);

	Bridge.create "test" None "test";
	Bridge.add_port "test" "test" "port1" ["vif1.0"];
	List.iter print_endline (Bridge.get_all "test" ());
	Bridge.set_persistent "test" "test" true;
	print_endline "end."

