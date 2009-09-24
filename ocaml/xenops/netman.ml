module D = Debug.Debugger(struct let name = "netman" end)
open D


type netty = Bridge of string | DriverDomain | Nat

let online vif netty =
	match netty with
	| Bridge bridgename ->
		let setup_bridge_port dev =
			Netdev.Link.down dev;
			Netdev.Link.arp dev false;
			Netdev.Link.multicast dev false;
    			Netdev.Link.set_addr dev 
			  (if(Xc.using_injection ()) then "fe:fe:fe:fe:fe:fe" else "fe:ff:ff:ff:ff:ff");
			Netdev.Addr.flush dev
			in
		let add_to_bridge br dev =
			Netdev.Bridge.set_forward_delay br 0;
			Netdev.Bridge.intf_add br dev;
			Netdev.Link.up dev
			in
		debug "Adding %s to bridge %s" vif bridgename;
		setup_bridge_port vif;
		add_to_bridge bridgename vif
	| DriverDomain -> ()
	| _ ->
		failwith "not supported yet"

let offline vif netty =
	match netty with
	| Bridge bridgename ->
		debug "Removing %s from bridge %s" vif bridgename;
		begin try
			Netdev.Bridge.intf_del bridgename vif;
			Netdev.Link.down vif
		with _ ->
			warn "interface %s already removed from bridge %s" vif bridgename;
		end;
	| DriverDomain -> ()
	| _                 ->
		failwith "not supported yet"
