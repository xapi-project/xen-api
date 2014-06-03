(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
module D = Debug.Debugger(struct let name = "netman" end)
open D

type netty = Bridge of string | Vswitch of string | DriverDomain | Nat

let netty_of_bridge bridge = 
  match Netdev.network.Netdev.kind with
  | Netdev.Bridge -> (Bridge bridge)
  | Netdev.Vswitch -> (Vswitch bridge)

let online vif netty =
	let setup_bridge_port dev =
		Netdev.Link.down dev;
		Netdev.Link.arp dev false;
		Netdev.Link.multicast dev false;
		Netdev.Link.set_addr dev 
		 (if(Xenctrl.is_fake ()) then "fe:fe:fe:fe:fe:fe" else "fe:ff:ff:ff:ff:ff");
		Netdev.Addr.flush dev
		in
	match netty with
	| Bridge bridgename ->
		let add_to_bridge br dev =
			Netdev.network.Netdev.set_forward_delay br 0;
			Netdev.network.Netdev.intf_add br dev;
			Netdev.Link.up dev
			in
		debug "Adding %s to bridge %s" vif bridgename;
		setup_bridge_port vif;
		add_to_bridge bridgename vif
	| Vswitch bridgename ->
		let add_to_bridge br dev =
			Netdev.network.Netdev.intf_add br dev;
			Netdev.Link.up dev
			in
		debug "Adding %s to bridge %s" vif bridgename;
		setup_bridge_port vif;
		add_to_bridge bridgename vif
	| DriverDomain -> ()
	| Nat -> failwith "not supported yet"

let offline vif netty =
	match netty with
	| Bridge bridgename ->
		debug "Removing %s from bridge %s" vif bridgename;
		begin try
			Netdev.network.Netdev.intf_del bridgename vif;
			Netdev.Link.down vif
		with _ ->
			warn "interface %s already removed from bridge %s" vif bridgename;
		end;
	| Vswitch bridgename ->
		debug "Removing %s from bridge %s" vif bridgename;
		begin try
			Netdev.network.Netdev.intf_del bridgename vif;
			Netdev.Link.down vif
		with _ ->
			warn "interface %s already removed from bridge %s" vif bridgename;
		end;
	| DriverDomain -> ()
	| Nat -> failwith "not supported yet"
