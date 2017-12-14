(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

open Network_utils

open Xapi_stdext_pervasives
open Xapi_stdext_std.Listext
open Xapi_stdext_threads.Threadext

module D = Debug.Make(struct let name = "network_monitor_thread" end)
open D

(** Table for bonds status. *)
let bonds_status : (string, (int * int)) Hashtbl.t = Hashtbl.create 10

let monitor_whitelist = ref [
	"eth";
	"vif"; (* This includes "tap" owing to the use of standardise_name below *)
]

let xapi_rpc request =
	Rpc_client.do_rpc_unix
		~content_type:(Rpc_client.content_type_of_string "text/xml")
		~filename:(Filename.concat "/var/lib/xcp" "xapi")
		~path:"/" request

let send_bond_change_alert dev interfaces message =
	let ifaces = String.concat "+" (List.sort String.compare interfaces) in
	let module XenAPI = Client.Client in
	let session_id = XenAPI.Session.login_with_password
		~rpc:xapi_rpc ~uname:"" ~pwd:"" ~version:"1.4" ~originator:("xcp-networkd v" ^ Version.version) in
	Pervasiveext.finally
		(fun _ ->
			let obj_uuid = Inventory.lookup Inventory._installation_uuid in
			let body = Printf.sprintf	"The status of the %s bond %s" ifaces message in
			try
				let (name, priority) = Api_messages.bond_status_changed in
				let (_ : API.ref_message) = XenAPI.Message.create ~rpc:xapi_rpc ~session_id
					~name ~priority ~cls:`Host ~obj_uuid ~body in ()
			with _ ->
				warn "Exception sending a bond-status-change alert."
		)
		(fun _ -> XenAPI.Session.logout ~rpc:xapi_rpc ~session_id)

let check_for_changes ~(dev : string) ~(stat : Network_monitor.iface_stats) =
	let open Network_monitor in
	match Astring.String.is_prefix ~affix:"vif" dev with true -> () | false ->
	if stat.nb_links > 1 then ( (* It is a bond. *)
		if Hashtbl.mem bonds_status dev then ( (* Seen before. *)
			let nb_links_old, links_up_old = Hashtbl.find bonds_status dev in
			if links_up_old <> stat.links_up then (
				info "Bonds status changed: %s nb_links %d up %d up_old %d" dev stat.nb_links
				stat.links_up links_up_old;
				Hashtbl.replace bonds_status dev (stat.nb_links,stat.links_up);
				let msg = Printf.sprintf "changed: %d/%d up (was %d/%d)" stat.links_up stat.nb_links
					links_up_old nb_links_old in
				try
					send_bond_change_alert dev stat.interfaces msg
				with e ->
					debug "Error while sending alert BONDS_STATUS_CHANGED: %s\n%s"
						(Printexc.to_string e) (Printexc.get_backtrace ())
			)
		) else ( (* Seen for the first time. *)
			Hashtbl.add bonds_status dev (stat.nb_links,stat.links_up);
			info "New bonds status: %s nb_links %d up %d" dev stat.nb_links stat.links_up;
			if stat.links_up <> stat.nb_links then
				(let msg = Printf.sprintf "is: %d/%d up" stat.links_up stat.nb_links in
					try
						send_bond_change_alert dev stat.interfaces msg
					with e ->
						debug "Error while sending alert BONDS_STATUS_CHANGED: %s\n%s"
							(Printexc.to_string e) (Printexc.get_backtrace ()))
		)
	)

let failed_again = ref false

let standardise_name name =
	try
		let (d1,d2) = Scanf.sscanf name "tap%d.%d"
			(fun d1 d2 -> d1,d2) in
		let newname = Printf.sprintf "vif%d.%d" d1 d2 in
		newname
	with _ -> name

let get_link_stats () =
	let open Network_monitor in
	let open Netlink in
	let s = Socket.alloc () in
	Socket.connect s Socket.NETLINK_ROUTE;

	let cache = Link.cache_alloc s in
	let links = Link.cache_to_list cache in
	let links =
		let is_whitelisted name =
			List.exists (fun s -> Astring.String.is_prefix ~affix:s name) !monitor_whitelist
		in
		let is_vlan name =
			Astring.String.is_prefix ~affix:"eth" name && String.contains name '.'
		in
		List.map (fun link ->
			(standardise_name (Link.get_name link)), link
		) links |>
		(* Only keep interfaces with prefixes on the whitelist, and exclude VLAN
		   devices (ethx.y). *)
		List.filter (fun (name, _) ->
			is_whitelisted name && not (is_vlan name)
		)
	in

	let devs = List.map (fun (name,link) ->
		let convert x = Int64.of_int (Unsigned.UInt64.to_int x) in
		let eth_stat = {default_stats with
			rx_bytes = Link.get_stat link Link.RX_BYTES |> convert;
			rx_pkts = Link.get_stat link Link.RX_PACKETS |> convert;
			rx_errors = Link.get_stat link Link.RX_ERRORS |> convert;
			tx_bytes = Link.get_stat link Link.TX_BYTES |> convert;
			tx_pkts = Link.get_stat link Link.TX_PACKETS |> convert;
			tx_errors = Link.get_stat link Link.TX_ERRORS |> convert;
		} in
		name, eth_stat
	) links in

	Cache.free cache;
	Socket.close s;
	Socket.free s;
	devs

let rec monitor dbg () =
	let open Network_interface in
	let open Network_monitor in
	(try
		let make_bond_info devs (name, interfaces) =
			let devs' = List.filter (fun (name', _) -> List.mem name' interfaces) devs in
			let eth_stat = {default_stats with
				rx_bytes = List.fold_left (fun ac (_, stat) -> Int64.add ac stat.rx_bytes) 0L devs';
				rx_pkts = List.fold_left (fun ac (_, stat) -> Int64.add ac stat.rx_pkts) 0L devs';
				rx_errors = List.fold_left (fun ac (_, stat) -> Int64.add ac stat.rx_errors) 0L devs';
				tx_bytes = List.fold_left (fun ac (_, stat) -> Int64.add ac stat.tx_bytes) 0L devs';
				tx_pkts = List.fold_left (fun ac (_, stat) -> Int64.add ac stat.tx_pkts) 0L devs';
				tx_errors = List.fold_left (fun ac (_, stat) -> Int64.add ac stat.tx_errors) 0L devs';
			} in
			name, eth_stat
		in
		let add_bonds bonds devs =
			(List.map (make_bond_info devs) bonds) @ devs
		in
		let transform_taps devs =
			let newdevnames = List.setify (List.map fst devs) in
			List.map (fun name ->
				let devs' = List.filter (fun (n,x) -> n=name) devs in
				let tot = List.fold_left (fun acc (_,b) ->
					{default_stats with
					 rx_bytes = Int64.add acc.rx_bytes b.rx_bytes;
					 rx_pkts = Int64.add acc.rx_pkts b.rx_pkts;
					 rx_errors = Int64.add acc.rx_errors b.rx_errors;
					 tx_bytes = Int64.add acc.tx_bytes b.tx_bytes;
					 tx_pkts = Int64.add acc.tx_pkts b.tx_pkts;
					 tx_errors = Int64.add acc.tx_errors b.tx_errors}
				) default_stats devs' in
				(name,tot)
			) newdevnames
		in
		let add_other_stats bonds devs =
			List.map (fun (dev, stat) ->
				if not (Astring.String.is_prefix ~affix:"vif" dev) then begin
					let open Network_server.Bridge in
					let bond_slaves =
						if List.mem_assoc dev bonds then
							get_bond_link_info () dbg dev
						else
							[]
					in
					let stat =
						if bond_slaves = [] then
							let carrier = Sysfs.get_carrier dev in
							let speed, duplex =
								if carrier then
									Sysfs.get_status dev
								else
									(0, Duplex_unknown)
							in
							let pci_bus_path = Sysfs.get_pcibuspath dev in
							let vendor_id, device_id = Sysfs.get_pci_ids dev in
							let nb_links = 1 in
							let links_up = if carrier then 1 else 0 in
							let interfaces = [dev] in
							{stat with carrier; speed; duplex; pci_bus_path; vendor_id; device_id; nb_links; links_up; interfaces}
						else
							let carrier = List.exists (fun info -> info.up) bond_slaves in
							let speed, duplex =
								let combine_duplex = function
									| Duplex_full, Duplex_full -> Duplex_full
									| Duplex_unknown, a | a, Duplex_unknown -> a
									| _ -> Duplex_half
								in
								List.fold_left (fun (speed, duplex) info ->
									try
										if info.active then
											let speed', duplex' = Sysfs.get_status info.slave in
											speed + speed', combine_duplex (duplex, duplex')
										else
											speed, duplex
									with _ ->
										speed, duplex
								) (0, Duplex_unknown) bond_slaves
							in
							let pci_bus_path = "" in
							let vendor_id, device_id = "", "" in
							let nb_links = List.length bond_slaves in
							let links_up = List.length (List.filter (fun info -> info.up) bond_slaves) in
							let interfaces = List.map (fun info -> info.slave) bond_slaves in
							{stat with carrier; speed; duplex; pci_bus_path; vendor_id; device_id; nb_links; links_up; interfaces}
					in
					check_for_changes ~dev ~stat;
					dev, stat
				end else
					dev, stat
			) devs
		in
		
		let bonds : (string * string list) list = Network_server.Bridge.get_all_bonds () dbg ~from_cache:true () in
		let devs =
			get_link_stats () |>
			add_bonds bonds |>
			transform_taps |>
			add_other_stats bonds
		in

		if (List.length bonds) <> (Hashtbl.length bonds_status) then begin
			let dead_bonds = Hashtbl.fold (fun k _ acc -> if List.mem_assoc k bonds then acc else k :: acc) 
				bonds_status [] in
			List.iter (fun b -> info "Removing bond %s" b; Hashtbl.remove bonds_status b) dead_bonds
		end;

		write_stats devs;
		failed_again := false
	with e ->
		if not !failed_again then begin
			failed_again := true;
			debug "Error while collecting stats (suppressing further errors): %s\n%s"
				(Printexc.to_string e) (Printexc.get_backtrace ())
		end
	);

	Thread.delay interval;
	monitor dbg ()

let watcher_m = Mutex.create ()
let watcher_pid = ref None

let signal_networking_change () =
	let module XenAPI = Client.Client in
	let session = XenAPI.Session.slave_local_login_with_password ~rpc:xapi_rpc ~uname:"" ~pwd:"" in
	Pervasiveext.finally
		(fun () -> XenAPI.Host.signal_networking_change xapi_rpc session)
		(fun () -> XenAPI.Session.local_logout xapi_rpc session)

(* Remove all outstanding reads on a file descriptor *)
let clear_input fd =
	let buf = String.make 255 ' ' in
	let rec loop () =
		try
			ignore (Unix.read fd buf 0 255);
			loop ()
		with _ -> ()
	in
	Unix.set_nonblock fd;
	loop ();
	Unix.clear_nonblock fd

let ip_watcher () =
	let cmd = Network_utils.iproute2 in
	let args = ["monitor"; "address"] in
	let readme, writeme = Unix.pipe () in
	Mutex.execute watcher_m (fun () ->
		watcher_pid := Some (Forkhelpers.safe_close_and_exec ~env:(Unix.environment ()) None (Some writeme) None [] cmd args)
	);
	Unix.close writeme;
	let in_channel = Unix.in_channel_of_descr readme in
	let rec loop () =
		let line = input_line in_channel in
		(* Do not send events for link-local IPv6 addresses, and removed IPs *)
		if Astring.String.is_infix ~affix:"inet" line && not (Astring.String.is_infix ~affix:"inet6 fe80" line) then begin
			(* Ignore changes for the next second, since they usually come in bursts,
			 * and signal only once. *)
			Thread.delay 1.;
			clear_input readme;
			signal_networking_change ()
		end;
		loop ()
	in
	while true do
		try
			info "(Re)started IP watcher thread";
			loop ()
		with e ->
			warn "Error in IP watcher: %s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ())
	done

let start () =
	let dbg = "monitor_thread" in
	Debug.with_thread_associated dbg (fun () ->
		debug "Starting network monitor";
		let (_ : Thread.t) = Thread.create (monitor dbg) () in
		let (_ : Thread.t) = Thread.create ip_watcher () in
		()
	) ()

let stop () =
	Mutex.execute watcher_m (fun () ->
		match !watcher_pid with
		| None -> ()
		| Some pid -> Unix.kill (Forkhelpers.getpid pid) Sys.sigterm
	)

