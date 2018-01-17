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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Xapi_stdext_pervasives.Pervasiveext
open Network_utils

module D = Debug.Make(struct let name = "networkd" end)
open D


let resources = [
  { Xcp_service.name = "network-conf";
    description = "used to select the network backend";
    essential = true;
    path = Network_server.network_conf;
    perms = [ Unix.R_OK ];
  };
  { Xcp_service.name = "brctl";
    description = "used to set up bridges";
    essential = true;
    path = Network_utils.brctl;
    perms = [ Unix.X_OK ];
  };
  { Xcp_service.name = "ethtool";
    description = "used to configure network interfaces";
    essential = true;
    path = Network_utils.ethtool;
    perms = [ Unix.X_OK ];
  };
  { Xcp_service.name = "fcoedriver";
    description = "used to identify fcoe interfaces";
    essential = false;
    path = Network_utils.fcoedriver;
    perms = [ Unix.X_OK ];
  };
  { Xcp_service.name = "inject-igmp-query-script";
    description = "used to inject an IGMP query message for a bridge";
    essential = false;
    path = Network_utils.inject_igmp_query_script;
    perms = [ Unix.X_OK ];
  }
]

let options = [
	"monitor_whitelist", Arg.String (fun x -> Network_monitor_thread.monitor_whitelist := Astring.String.cuts ~empty:false ~sep:"," x), (fun () -> String.concat "," !Network_monitor_thread.monitor_whitelist), "List of prefixes of interface names that are to be monitored";
	"mac-table-size", Arg.Set_int Network_utils.mac_table_size, (fun () -> string_of_int !Network_utils.mac_table_size), "Default value for the mac-table-size openvswitch parameter (see ovs-vswitchd.conf.db.5)";
	"enic-workaround-until-version", Arg.Set_string Network_server.enic_workaround_until_version, (fun () -> !Network_server.enic_workaround_until_version), "The version till enic driver workaround will be applied or the version set to an empty string for not applying the workaround.";
	"pvs-proxy-socket", Arg.Set_string Network_server.PVS_proxy.path, (fun () -> !Network_server.PVS_proxy.path), "Path to the Unix domain socket for the PVS-proxy daemon";
	"igmp-query-maxresp-time", Arg.Set_string Network_utils.igmp_query_maxresp_time, (fun () -> !Network_utils.igmp_query_maxresp_time), "Maximum Response Time in IGMP Query message to send";
	"enable-ipv6-mcast-snooping", Arg.Bool (fun x -> Network_utils.enable_ipv6_mcast_snooping := x), (fun () -> string_of_bool !Network_utils.enable_ipv6_mcast_snooping), "IPv6 multicast snooping toggle";
	"mcast-snooping-disable-flood-unregistered", Arg.Bool (fun x -> Network_utils.mcast_snooping_disable_flood_unregistered := x), (fun () -> string_of_bool !Network_utils.mcast_snooping_disable_flood_unregistered), "Set OVS bridge configuration mcast-snooping-disable-flood-unregistered as 'true' or 'false'";
]

let start server =
	Network_monitor_thread.start ();
	Network_server.on_startup ();
	let (_: Thread.t) = Thread.create (fun () ->
		Xcp_service.serve_forever server
	) () in
	()

let stop signal =
	Network_server.on_shutdown signal;
	Network_monitor_thread.stop ();
	exit 0

let handle_shutdown () =
	Sys.set_signal Sys.sigterm (Sys.Signal_handle stop);
	Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
	Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let doc = String.concat "\n" [
	"This is the xapi toolstack network management daemon.";
	"";
	"This service looks after host network configuration, including setting up bridges and/or openvswitch instances, configuring IP addresses etc.";
]


let bind () =
  let open Network_server in
  S.clear_state clear_state;
  S.reset_state reset_state;
  S.set_gateway_interface set_gateway_interface;
  S.set_dns_interface set_dns_interface;
  S.Interface.get_all Interface.get_all;
  S.Interface.exists Interface.exists;
  S.Interface.get_mac Interface.get_mac;
  S.Interface.is_up Interface.is_up;
  S.Interface.get_ipv4_addr Interface.get_ipv4_addr;
  S.Interface.set_ipv4_conf Interface.set_ipv4_conf;
  S.Interface.get_ipv4_gateway Interface.get_ipv4_gateway;
  S.Interface.get_ipv6_addr Interface.get_ipv6_addr;
  S.Interface.get_dns Interface.get_dns;
  S.Interface.get_mtu Interface.get_mtu;
  S.Interface.get_capabilities Interface.get_capabilities;
  S.Interface.is_connected Interface.is_connected;
  S.Interface.is_physical Interface.is_physical;
  S.Interface.has_vlan Interface.has_vlan;
  S.Interface.bring_down Interface.bring_down;
  S.Interface.set_persistent Interface.set_persistent;
  S.Interface.make_config Interface.make_config;
  S.Bridge.get_all Bridge.get_all;
  S.Bridge.create Bridge.create;
  S.Bridge.destroy Bridge.destroy;
  S.Bridge.get_kind Bridge.get_kind;
  S.Bridge.get_all_ports Bridge.get_all_ports;
  S.Bridge.get_all_bonds Bridge.get_all_bonds;
  S.Bridge.set_persistent Bridge.set_persistent;
  S.Bridge.add_port Bridge.add_port;
  S.Bridge.remove_port Bridge.remove_port;
  S.Bridge.get_interfaces Bridge.get_interfaces;
  S.Bridge.get_physical_interfaces Bridge.get_physical_interfaces;
  S.Bridge.make_config Bridge.make_config;
  S.PVS_proxy.configure_site PVS_proxy.configure_site;
  S.PVS_proxy.remove_site PVS_proxy.remove_site

let _ =
	Coverage.init "networkd";
	begin match Xcp_service.configure2
		~name:Sys.argv.(0)
		~version:Version.version
		~doc ~options ~resources () with
	| `Ok () -> ()
	| `Error m ->
		Printf.fprintf stderr "%s\n" m;
		exit 1
	end;

	bind ();
	let server = Xcp_service.make
		~path:!Network_interface.default_path
		~queue_name:!Network_interface.queue_name
		~rpc_fn:(Idl.server Network_server.S.implementation)
		() in

	Xcp_service.maybe_daemonize ~start_fn:(fun () ->
		Debug.set_facility Syslog.Local5;

		(* We should make the following configurable *)
		Debug.disable "http";

		handle_shutdown ();
		Debug.with_thread_associated "main" start server
	) ();

	ignore (Daemon.notify Daemon.State.Ready);

	while true do
		Thread.delay 300.;
		Network_server.on_timer ()
	done

