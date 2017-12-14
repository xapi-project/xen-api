open Network_interface
open Network_client
open Cmdliner

let dbg = "cli"

let (|>) x f = f x

(* Interface commands *)

let iface_arg =
	let doc = "Interface name" in
	Arg.(required & pos 0 (some string) None & info [] ~docv:"INTERFACE" ~doc)

let list_iface () =
	let all = Client.Interface.get_all dbg () in
	List.iter print_endline all

let list_iface_cmd =
	let doc = "List all interfaces" in
	let man = [] in
	Term.(pure list_iface $ pure ()),
	Term.info "list-iface" ~doc ~man

let get_mac iface =
	try
		let mac = Client.Interface.get_mac dbg iface in
		`Ok (print_endline mac)
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_mac_cmd =
	let doc = "Get the MAC address of an interface" in
	let man = [] in
	Term.(ret (pure get_mac $ iface_arg)),
	Term.info "get-mac" ~doc ~man

let is_up iface =
	try
		let up = Client.Interface.is_up dbg iface in
		`Ok (print_endline (if up then "up" else "not up"))
	with _ ->
		`Error (false, iface ^ " is not an interface")

let is_up_cmd =
	let doc = "Check whether an interface is up or down" in
	let man = [] in
	Term.(ret (pure is_up $ iface_arg)),
	Term.info "is-up" ~doc ~man

let get_ipv4_addr iface =
	try
		let addrs = Client.Interface.get_ipv4_addr dbg iface in
		List.iter (fun (addr, prefix) ->
			Printf.printf "%s/%d\n" (Unix.string_of_inet_addr addr) prefix
		) addrs;
		`Ok ()
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_ipv4_addr_cmd =
	let doc = "Get IPv4 addresses (CIDRs) of an interface" in
	let man = [] in
	Term.(ret (pure get_ipv4_addr $ iface_arg)),
	Term.info "get-ipv4-addr" ~doc ~man

let set_ipv4_addr iface conf =
	try
		let conf' =
			if conf = "none" then
				None4
			else if conf = "dhcp" then
				DHCP4
			else
				let i = String.index conf '/' in
				let n = String.length conf in
				let addr = Unix.inet_addr_of_string (String.sub conf 0 i) in
				let prefix = String.sub conf (i + 1) (n - i - 1) |> int_of_string in
				Static4 [addr, prefix]
		in
		Client.Interface.set_ipv4_conf dbg iface conf';
		`Ok ()
	with _ ->
		`Error (false, "something went wrong")

let set_ipv4_addr_cmd =
	let doc = "Interface name (none|dhcp|<cidr>)" in
	let conf_arg = Arg.(required & pos 1 (some string) None & info [] ~docv:"IPV4-CONF" ~doc) in
	let doc = "Set IPv4 configuration of an interface" in
	let man = [] in
	Term.(ret (pure set_ipv4_addr $ iface_arg $ conf_arg)),
	Term.info "set-ipv4-addr" ~doc ~man

let get_ipv4_gateway iface =
	try
		let addr = Client.Interface.get_ipv4_gateway dbg iface in
		(match addr with
		| Some addr -> Printf.printf "%s\n" (Unix.string_of_inet_addr addr)
		| None -> ()
		);
		`Ok ()
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_ipv4_gateway_cmd =
	let doc = "If there is an IPv4 default route through the interface, get the gateway address" in
	let man = [] in
	Term.(ret (pure get_ipv4_gateway $ iface_arg)),
	Term.info "get-ipv4-gateway" ~doc ~man

let set_ipv4_gateway iface addr =
	try
		let addr' = Unix.inet_addr_of_string addr in
		Client.Interface.set_ipv4_gateway dbg iface addr';
		`Ok ()
	with _ ->
		`Error (false, "something went wrong")

let set_ipv4_gateway_cmd =
	let doc = "Gateway IPv4 address" in
	let addr_arg = Arg.(required & pos 1 (some string) None & info [] ~docv:"IPV4-GATEWAY" ~doc) in
	let doc = "Set IPv4 gateway for an interface" in
	let man = [] in
	Term.(ret (pure set_ipv4_gateway $ iface_arg $ addr_arg)),
	Term.info "set-ipv4-gateway" ~doc ~man

let get_ipv6_addr iface =
	try
		let addrs = Client.Interface.get_ipv6_addr dbg iface in
		List.iter (fun (addr, prefix) ->
			Printf.printf "%s/%d\n" (Unix.string_of_inet_addr addr) prefix
		) addrs;
		`Ok ()
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_ipv6_addr_cmd =
	let doc = "Get IPv6 addresses (CIDRs) of an interface" in
	let man = [] in
	Term.(ret (pure get_ipv6_addr $ iface_arg)),
	Term.info "get-ipv6-addr" ~doc ~man

let set_ipv6_addr iface conf =
	try
		let conf' =
			if conf = "none" then
				None6
			else if conf = "linklocal" then
				Linklocal6
			else if conf = "dhcp" then
				DHCP6
			else if conf = "autoconf" then
				Autoconf6
			else
				let i = String.index conf '/' in
				let n = String.length conf in
				let addr = Unix.inet_addr_of_string (String.sub conf 0 i) in
				let prefix = String.sub conf (i + 1) (n - i - 1) |> int_of_string in
				Static6 [addr, prefix]
		in
		Client.Interface.set_ipv6_conf dbg iface conf';
		`Ok ()
	with _ ->
		`Error (false, "something went wrong")

let set_ipv6_addr_cmd =
	let doc = "Interface name (none|linklocal|dhcp|autoconf|<cidr>)" in
	let conf_arg = Arg.(required & pos 1 (some string) None & info [] ~docv:"IPV6-CONF" ~doc) in
	let doc = "Set IPv6 configuration of an interface" in
	let man = [] in
	Term.(ret (pure set_ipv6_addr $ iface_arg $ conf_arg)),
	Term.info "set-ipv6-addr" ~doc ~man

let get_ipv6_gateway iface =
	try
		let addr = Client.Interface.get_ipv6_gateway dbg iface in
		(match addr with
		| Some addr -> Printf.printf "%s\n" (Unix.string_of_inet_addr addr)
		| None -> ()
		);
		`Ok ()
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_ipv6_gateway_cmd =
	let doc = "If there is an IPv6 default route through the interface, get the gateway address" in
	let man = [] in
	Term.(ret (pure get_ipv6_gateway $ iface_arg)),
	Term.info "get-ipv6-gateway" ~doc ~man

let set_ipv6_gateway iface addr =
	try
		let addr' = Unix.inet_addr_of_string addr in
		Client.Interface.set_ipv6_gateway dbg iface addr';
		`Ok ()
	with _ ->
		`Error (false, "something went wrong")

let set_ipv6_gateway_cmd =
	let doc = "Gateway IPv6 address" in
	let addr_arg = Arg.(required & pos 1 (some string) None & info [] ~docv:"IPV6-GATEWAY" ~doc) in
	let doc = "Set IPv6 gateway for an interface" in
	let man = [] in
	Term.(ret (pure set_ipv6_gateway $ iface_arg $ addr_arg)),
	Term.info "set-ipv6-gateway" ~doc ~man

let get_dns () =
	let nameservers, domains = Client.Interface.get_dns dbg "" in
	Printf.printf "nameservers: %s\n" (String.concat ", " (List.map Unix.string_of_inet_addr nameservers));
	Printf.printf "domains: %s\n" (String.concat ", " domains);
	`Ok ()

let get_dns_cmd =
	let doc = "Get DNS nameservers and domains" in
	let man = [] in
	Term.(ret (pure get_dns $ pure ())),
	Term.info "get-dns" ~doc ~man

let set_dns iface nameservers domains =
	try
		let ns = match nameservers with
			| Some x -> List.map Unix.inet_addr_of_string (Astring.String.cuts ~empty:false ~sep:"," x)
			| None -> []
		in
		let d = match domains with
			| Some x -> Astring.String.cuts ~empty:false ~sep:"," x
			| None -> []
		in
		Client.Interface.set_dns dbg iface ns d;
		`Ok ()
	with _ ->
		`Error (false, "something went wrong")

let set_dns_cmd =
	let doc = "Comma-separated list of nameservers" in
	let nameservers_arg = Arg.(value & opt (some string) None & info ["nameservers"] ~docv:"NAMESERVERS" ~doc) in
	let doc = "Comma-separated list of domains" in
	let domains_arg = Arg.(value & opt (some string) None & info ["domains"] ~docv:"DOMAINS" ~doc) in
	let doc = "Set DNS nameservers and domains" in
	let man = [] in
	Term.(ret (pure set_dns $ iface_arg $ nameservers_arg $ domains_arg)),
	Term.info "set-dns" ~doc ~man

let get_mtu iface =
	try
		let mtu = Client.Interface.get_mtu dbg iface in
		Printf.printf "%d\n" mtu;
		`Ok ()
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_mtu_cmd =
	let doc = "Get MTU" in
	let man = [] in
	Term.(ret (pure get_mtu $ iface_arg)),
	Term.info "get-mtu" ~doc ~man

let set_mtu iface mtu =
	try
		Client.Interface.set_mtu dbg iface mtu;
		`Ok ()
	with _ ->
		`Error (false, iface ^ " is not an interface")

let set_mtu_cmd =
	let doc = "The MTU" in
	let mtu_arg = Arg.(required & pos 1 (some int) None & info [] ~docv:"MTU" ~doc) in
	let doc = "Get MTU" in
	let man = [] in
	Term.(ret (pure set_mtu $ iface_arg $ mtu_arg)),
	Term.info "set-mtu" ~doc ~man

let get_persistence iface =
	try
		let persistent = Client.Interface.is_persistent dbg iface in
		Printf.printf "%s\n" (if persistent then "persistent" else "not persistent");
		`Ok ()
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_persistence_cmd =
	let doc = "Get persistence" in
	let man = [] in
	Term.(ret (pure get_persistence $ iface_arg)),
	Term.info "get-persistence" ~doc ~man

let set_persistence iface persistence =
	try
		if persistence = "on" then
			`Ok (Client.Interface.set_persistent dbg iface true)
		else if persistence = "off" then
			`Ok (Client.Interface.set_persistent dbg iface false)
		else
			`Error (false, "'on' or 'off' please")
	with _ ->
		`Error (false, iface ^ " is not an interface")

let set_persistence_cmd =
	let doc = "Persistence (on|off)" in
	let persistence_arg = Arg.(required & pos 1 (some string) None & info [] ~docv:"PERSISTENCE" ~doc) in
	let doc = "Set persistence" in
	let man = [] in
	Term.(ret (pure set_persistence $ iface_arg $ persistence_arg)),
	Term.info "set-persistence" ~doc ~man

(* Bridge commands *)

let list_br () =
	let all = Client.Bridge.get_all dbg () in
	List.iter print_endline all

let list_br_cmd =
	let doc = "List all bridges" in
	let man = [] in
	Term.(pure list_br $ pure ()),
	Term.info "list-br" ~doc ~man

let read_config path =
	let config_json = Xapi_stdext_unix.Unixext.string_of_file path in
	config_json |> Jsonrpc.of_string |> config_t_of_rpc

let config path =
	let config = read_config path in
	Client.Bridge.make_config dbg ~config:config.bridge_config ();
	Client.Interface.make_config dbg ~config:config.interface_config ();
	`Ok ()

let config_cmd =
	let doc = "Path to JSON config file" in
	let config_arg = Arg.(required & pos 0 (some file) None & info [] ~docv:"CONFIG-FILE" ~doc) in
	let doc = "Set network configuration based on a config file" in
	let man = [] in
	Term.(ret (pure config $ config_arg)),
	Term.info "config" ~doc ~man

let default_cmd =
	let doc = "CLI for xcp-networkd" in
	let man = [] in
	Term.(ret (pure (`Help (`Pager, None)))),
	Term.info "network-cli" ~version:"0.1" ~doc ~man

let cmds = [
	list_iface_cmd; get_mac_cmd; is_up_cmd;
	get_ipv4_addr_cmd; set_ipv4_addr_cmd; get_ipv4_gateway_cmd; set_ipv4_gateway_cmd;
	get_ipv6_addr_cmd; set_ipv6_addr_cmd; get_ipv6_gateway_cmd; set_ipv6_gateway_cmd;
	get_dns_cmd; set_dns_cmd; get_mtu_cmd; set_mtu_cmd;
	get_persistence_cmd; set_persistence_cmd;
	list_br_cmd;
	config_cmd]

let _ =
	Coverage.init "network_cli";
	match Term.eval_choice default_cmd cmds with
	| `Error _ -> exit 1 | _ -> exit 0
