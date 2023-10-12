+++
title = "Networkd"
weight = 40
+++

The `xcp-networkd` daemon (hereafter simply called "networkd") is a component in the xapi toolstack that is responsible for configuring network interfaces and virtual switches (bridges) on a host.

The code is in `ocaml/networkd`.


Principles
----------

1. **Distro-agnostic**.  Networkd is meant to work on at least CentOS/RHEL as well a Debian/Ubuntu based distros. It therefore should not use any network configuration features specific to those distros.

2. **Stateless**.  By default, networkd should not maintain any state. If you ask networkd anything about a network interface or bridge, or any other network sub-system property, it will always query the underlying system (e.g. an IP address), rather than returning any cached state. However, if you want networkd to configure networking at host boot time, the you can ask it to remember your configuration you have set for any interface or bridge you choose.

3. **Idempotent**.  It should be possible to call any networkd function multiple times without breaking things. For example, calling a function to set an IP address on an interface twice in a row should have the same outcome as calling it just once.

4. **Do no harm**.  Networkd should only configure what you ask it to configure. This means that it can co-exist with other network managers.


Usage
-----

Networkd is a daemon that is typically started at host-boot time. In the same way as the other daemons in the xapi toolstack, it is controlled by RPC requests. It typically receives requests from the xapi daemon, on behalf of which it configures host networking.

Networkd's RCP API is fully described by the [network_interface.ml](https://github.com/xapi-project/xen-api/blob/master/ocaml/xapi-idl/network/network_interface.ml) file. The API has two main namespaces: `Interface` and `Bridge`, which are implemented in two modules in [network_server.ml](https://github.com/xapi-project/xen-api/blob/master/ocaml/networkd/bin/network_server.ml).

In line with other xapi daemons, all API functions take an argument of type `debug_info` (a string) as their first argument. The debug string appears in any log lines that are produced as a side effort of calling the function.

Network Interface API
---------------------

The Interface API has functions to query and configure properties of Linux network devices, such as IP addresses, and bringing them up or down. Most Interface functions take a `name` string as a reference to a network interface as their second argument, which is expected to be the name of the Linux network device. There is also a special function, called `Interface.make_config`, that is able to configure a number of interfaces at once. It takes an argument called `config` of type `(iface * interface_config_t) list`, where `iface` is an interface name, and `interface_config_t` is a compound type containing the full configuration for an interface (as far as networkd is able to configure them), currently defined as follows:

```
type interface_config_t = {
	ipv4_conf: ipv4;
	ipv4_gateway: Unix.inet_addr option;
	ipv6_conf: ipv6;
	ipv6_gateway: Unix.inet_addr option;
	ipv4_routes: (Unix.inet_addr * int * Unix.inet_addr) list;
	dns: Unix.inet_addr list * string list;
	mtu: int;
	ethtool_settings: (string * string) list;
	ethtool_offload: (string * string) list;
	persistent_i: bool;
}
```

When the function returns, it should have completely configured the interface, and have brought it up. The idempotency principle applies to this function, which means that it can be used to successively modify interface properties; any property that has not changed will effectively be ignored. In fact, `Interface.make_config` is the main function that xapi uses to configure interfaces, e.g. as a result of a `PIF.plug` or a `PIF.reconfigure_ip` call.

Also note the `persistent` property in the interface config. When an interface is made "persistent", this means that any configuration that is set on it is remembered by networkd, and the interface config is written to disk. When networkd is started, it will read the persistent config and call `Interface.make_config` on it in order to apply it (see Startup below).

_The full networkd API should be documented separately somewhere on this site._

Bridge API
----------

The Bridge API functions are all about the management of virtual switches, also known as "bridges". The shape of the Bridge API roughly follows that of the Open vSwitch in that it treats a bridge as a collection of "ports", where a port can contain one or more "interfaces".

NIC bonding and VLANs are all configured on the Bridge level. There are functions for creating and destroying bridges, adding and removing ports, and configuring bonds and VLANs. Like interfaces, bridges and ports are addressed by name in the Bridge functions. Analogous to the Interface function with the same name, there is a `Bridge.make_config` function, and bridges can be made `persistent`.

```
type port_config_t = {
	interfaces: iface list;
	bond_properties: (string * string) list;
	bond_mac: string option;
}
type bridge_config_t = {
	ports: (port * port_config_t) list;
	vlan: (bridge * int) option;
	bridge_mac: string option;
	other_config: (string * string) list;
	persistent_b: bool;
}
```

Backends
--------

Networkd currently has two different backends: the "Linux bridge" backend and the "Open vSwitch" backend. The former is the "classic" backend based on the bridge module that is available in the Linux kernel, plus additional standard Linux functionality for NIC bonding and VLANs. The latter backend is newer and uses the [Open vSwitch (OVS)](http://www.openvswitch.org) for bridging as well as other functionality. Which backend is currently in use is defined by the file `/etc/xensource/network.conf`, which is read by networkd when it starts. The choice of backend (currently) only affects the Bridge API: every function in it has a separate implementation for each backend.

Low-level Interfaces
--------------------

Networkd uses standard networking commands and interfaces that are available in most modern Linux distros, rather than relying on any distro-specific network tools (see the distro-agnostic principle). These are tools such as `ip` (iproute2), `dhclient` and `brctl`, as well as the `sysfs` files system, and `netlink` sockets. To control the OVS, the `ovs-*` command line tools are used. All low-level functions are called from [network_utils.ml](https://github.com/xapi-project/xen-api/blob/master/ocaml/networkd/lib/network_utils.ml).

Configuration on Startup
------------------------

Networkd, periodically as well as on shutdown, writes the current configuration of all bridges and interfaces (see above) in a JSON format to a file called `networkd.db` (currently in `/var/lib/xcp`). The contents of the file are completely described by the following type:

```
type config_t = {
	interface_config: (iface * interface_config_t) list;
	bridge_config: (bridge * bridge_config_t) list;
	gateway_interface: iface option;
	dns_interface: iface option;
}
```

The `gateway_interface` and `dns_interface` in the config are global host-level options to define from which interfaces the default gateway and DNS configuration is taken. This is especially important when multiple interfaces are configured by DHCP.

When networkd starts up, it first reads `network.conf` to determine the network backend. It subsequently attempts to parse `networkd.db`, and tries to call `Bridge.make_config` and `Interface.make_config` on it, with a special options to only apply the config for `persistent` bridges and interfaces, as well as bridges related to those (for example, if a VLAN bridge is configured, then also its parent bridge must be configured).

Networkd also supports upgrades from older versions of XenServer that used a network configuration script called `interface-configure`. If `networkd.db` is not found on startup, then networkd attempts to call this tool (via the `/etc/init.d/management-interface` script) in order to set up networking at boot time. This is normally followed immediately by a call from xapi instructing networkd to take over.

Finally, if no network config (old or new) is found on disk at all, networkd looks for a XenServer "firstboot" data file, which is written by XenServer's host installer, and tries to apply it to set up the management interface.

Monitoring
----------

Besides the ability to configure bridges and network interfaces, networkd has facilities for monitoring interfaces and bonds. When networkd starts, a monitor thread is started, which does several things (see [network_monitor_thread.ml](https://github.com/xapi-project/xen-api/blob/master/ocaml/networkd/bin/network_monitor_thread.ml)):

* Every 5 seconds, it gathers send/receive counters and link state of all network interfaces. It then writes these stats to a shared-memory file, to be picked up by other components such as `xcp-rrdd` and `xapi` (see documentation about "xenostats" elsewhere).
* It monitors NIC bonds, and sends alerts through xapi in case of link state changes within a bond.
* It uses `ip monitor address` to watch for an IP address changes, and if so, it calls xapi (`Host.signal_networking_change`) for it to update the IP addresses of the PIFs in its database that were configured by DHCP.
