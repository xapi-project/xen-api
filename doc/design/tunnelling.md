Tunnelling API design
=====================

Background
----------

To isolate network traffic between VMs (e.g. for security reasons) one can use
VLANs. The number of possible VLANs on a network, however, is limited, and
setting up a VLAN requires configuring the physical switches in the network.
GRE tunnels provide a similar, though more flexible solution. This document
proposes a design that integrates the use of tunnelling in the XenAPI. The
design relies on the recent introduction of the Open vSwitch in XCP, and
requires an Open vSwitch ([[OpenFlow]]) controller (further referred to as
_the controller_) to set up and maintain the actual GRE tunnels.

We suggest following the way VLANs are modelled in the datamodel. Introducing a
VLAN involves creating a Network object for the VLAN, that VIFs can connect to.
The `VLAN.create` API call takes references to a PIF and Network to use and a
VLAN tag, and creates a VLAN object and a PIF object. We propose something
similar for tunnels; the resulting objects and relations for two hosts would
look like this:

    PIF (transport) -- Tunnel -- PIF (access) \          / VIF
                                                Network -- VIF
    PIF (transport) -- Tunnel -- PIF (access) /          \ VIF


XenAPI changes
--------------

### New tunnel class

#### Fields

* `string uuid` (read-only)
* `PIF ref access_PIF` (read-only)
* `PIF ref transport_PIF` (read-only)
* `(string -> string) map status` (read/write); owned by the controller, containing at least the
  key `active`, and `key` and `error` when appropriate (see below)
* `(string -> string) map other_config` (read/write)

New fields in PIF class (automatically linked to the corresponding `tunnel`
fields):

* `PIF ref set tunnel_access_PIF_of` (read-only)
* `PIF ref set tunnel_transport_PIF_of` (read-only)

#### Messages

* `tunnel ref create (PIF ref, network ref)`
* `void destroy (tunnel ref)`

### Backends

For clients to determine which network backend is in use (to decide whether
tunnelling functionality is enabled) a key `network_backend` is added to the
`Host.software_version` map on each host. The value of this key can be:

* `bridge`: the Linux bridging backend is in use;
* `openvswitch`: the [Open vSwitch] backend is in use.

### Notes

* The user is responsible for creating tunnel and network objects, associating
  VIFs with the right networks, and configuring the physical PIFs, all using
  the XenAPI/CLI/XC.

* The `tunnel.status` field is owned by the controller. It
  may be possible to define an RBAC role for the controller, such that only the
  controller is able to write to it.

* The `tunnel.create` message does not take
  a tunnel identifier (GRE key). The controller is responsible for assigning
  the right keys transparently. When a tunnel has been set up, the controller
  will write its key to `tunnel.status:key`, and it will set
  `tunnel.status:active` to `"true"` in the same field.

* In case a tunnel could
  not be set up, an error code (to be defined) will be written to
  `tunnel.status:error`, and `tunnel.status:active` will be `"false"`.

XAPI
----

### tunnel.create

* Fails with OPENVSWITCH_NOT_ACTIVE if the Open vSwitch networking sub-system
  is not active (the host uses linux bridging).
* Fails with IS_TUNNEL_ACCESS_PIF if the specified transport PIF is a tunnel access PIF.
* Takes care of creating and connecting the new tunnel and PIF objects.
  * Sets a random MAC on the access PIF.
  * IP configuration of the tunnel
    access PIF is left blank. (The IP configuration on a PIF is normally used for
    the interface in dom0. In this case, there is no tunnel interface for dom0 to
    use. Such functionality may be added in future.)
  * The `tunnel.status:active`
    field is initialised to `"false"`, indicating that no actual tunnelling
    infrastructure has been set up yet.
* Calls `PIF.plug` on the new tunnel access PIF.

### tunnel.destroy

* Calls `PIF.unplug` on the tunnel access PIF.  Destroys the `tunnel` and
  tunnel access PIF objects.

### PIF.plug on a tunnel access PIF

* Fails with TRANSPORT_PIF_NOT_CONFIGURED if the underlying transport PIF has
  `PIF.ip_configuration_mode = None`, as this interface needs to be configured
  for the tunnelling to work. Otherwise, the transport PIF will be plugged.
* XAPI requests `interface-reconfigure` to "bring up" the tunnel access PIF,
  which causes it to create a local bridge.
* No link will be made between the
  new bridge and the physical interface by `interface-reconfigure`. The
  controller is responsible for setting up these links. If the controller is
  not available, no links can be created, and the tunnel network degrades to an
  internal network (only intra-host connectivity).
* `PIF.currently_attached` is set to `true`.

### PIF.unplug on a tunnel access PIF

* XAPI requests `interface-reconfigure` to "bring down" the tunnel PIF, which
  causes it to destroy the local bridge.
* `PIF.currently_attached` is set to `false`.

### PIF.unplug on a tunnel transport PIF

* Calls `PIF.unplug` on the associated tunnel access PIF(s).

### PIF.forget on a tunnel access of transport PIF

* Fails with PIF_TUNNEL_STILL_EXISTS.

### VLAN.create

* Tunnels can only exist on top of physical/VLAN/Bond PIFs, and not the other
  way around. `VLAN.create` fails with IS_TUNNEL_ACCESS_PIF if given an
  underlying PIF that is a tunnel access PIF.

### Pool join

* As for VLANs, when a host joins a pool, it will inherit the tunnels that are
  present on the pool master.
* Any tunnels (tunnel and access PIF objects)
  configured on the host are removed, which will leave their networks
  disconnected (the networks become internal networks). As a joining host is
  always a single host, there is no real use for having had tunnels on it, so
  this probably will never be an issue.

The controller
--------------

* The controller tracks the `tunnel` class to determine which bridges/networks
  require GRE tunnelling.
  * On start-up, it calls `tunnel.get_all` to obtain the information about all
    tunnels.
  * Registers for events on the `tunnel` class to stay up-to-date.
* A tunnel network is organised as a star topology. The controller is free to
  decide which host will be the central host ("switching host").
* If the
  current switching host goes down, a new one will be selected, and GRE tunnels
  will be reconstructed.
* The controller creates GRE tunnels connecting each
  existing Open vSwitch bridge that is associated with the same tunnel network,
  after assigning the network a unique GRE key.
* The controller destroys GRE
  tunnels if associated Open vSwitch bridges are destroyed. If the destroyed
  bridge was on the switching host, and other hosts are still using the same
  tunnel network, a new switching host will be selected, and GRE tunnels will
  be reconstructed.
* The controller sets `tunnel.status:active` to `"true"` for
  all tunnel links that have been set up, and `"false"` if links are broken.
* The controller writes an appropriate error code (to be defined) to
  `tunnel.status:error` in case something went wrong.
* When an access PIF is
  plugged, and the controller succeeds to set up the tunnelling infrastructure,
  it writes the GRE key to `tunnel.status:key` on the associated tunnel object
  (at the same time `tunnel.status:active` will be set to `"true"`).
* When the
  tunnel infrastructure is not up and running, the controller may remove the
  key `tunnel.status:key` (optional; the key should anyway be disregarded if
  `tunnel.status:active` is `"false"`).

CLI
---

New `xe` commands (analogous to `xe vlan-`):
* `tunnel-create`
* `tunnel-destroy`
* `tunnel-list`
* `tunnel-param-get`
* `tunnel-param-list`

