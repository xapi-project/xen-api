---
title: Bonding Improvements design
layout: default
design_doc: true
revision: 1
status: released (6.0)
---

This document describes design details for the
PR-1006 requirements.

XAPI and XenAPI
===============

Creating a Bond
---------------

### Current Behaviour on Bond creation

Steps for a user to create a bond:

1.  Shutdown all VMs with VIFs using the interfaces that will be bonded,
    in order to unplug those VIFs.
2.  Create a Network to be used by the bond: `Network.create`
3.  Call `Bond.create` with a ref to this Network, a list of refs of
    slave PIFs, and a MAC address to use.
4.  Call `PIF.reconfigure_ip` to configure the bond master.
5.  Call `Host.management_reconfigure` if one of the slaves is the
    management interface. This command will call `interface-reconfigure`
    to bring up the master and bring down the slave PIFs, thereby
    activating the bond. Otherwise, call `PIF.plug` to activate the
    bond.

`Bond.create` XenAPI call:

1.  Remove duplicates in the list of slaves.
2.  Validate the following:
    -   Slaves must not be in a bond already.
    -   Slaves must not be VLAN masters.
    -   Slaves must be on the same host.
    -   Network does not already have a PIF on the same host as the
        slaves.
    -   The given MAC is valid.

3.  Create the master PIF object.
    -   The device name of this PIF is `bond`*x*, with *x* the smallest
        unused non-negative integer.
    -   The MAC of the first-named slave is used if no MAC was
        specified.

4.  Create the Bond object, specifying a reference to the master. The
    value of the `PIF.master_of` field on the master is dynamically
    computed on request.
5.  Set the `PIF.bond_slave_of` fields of the slaves. The value of the
    `Bond.slaves` field is dynamically computed on request.

### New Behaviour on Bond creation

Steps for a user to create a bond:

1.  Create a Network to be used by the bond: `Network.create`
2.  Call `Bond.create` with a ref to this Network, a list of refs of
    slave PIFs, and a MAC address to use.\
     The new bond will automatically be plugged if one of the slaves was
    plugged.

In the following, for a host *h*, a *VIF-to-move* is a VIF associated
with a VM that is either

-   running, suspended or paused on *h*, OR
-   halted, and *h* is the only host that the VM can be started on.

The `Bond.create` XenAPI call is updated to do the following:

1.  Remove duplicates in the list of slaves.
2.  Validate the following, and raise an exception if any of these check
    fails:
    -   Slaves must not be in a bond already.
    -   Slaves must not be VLAN masters.
    -   Slaves must not be Tunnel access PIFs.
    -   Slaves must be on the same host.
    -   Network does not already have a PIF on the same host as the
        slaves.
    -   The given MAC is valid.

3.  Try unplugging all currently attached VIFs of the set of VIFs that
    need to be moved. Roll back and raise an exception of one of the
    VIFs cannot be unplugged (e.g. due to the absence of PV drivers in
    the VM).
4.  Determine the *primary slave*: the management PIF (if among the
    slaves), or the first slave with IP configuration.
5.  Create the master PIF object.
    -   The device name of this PIF is `bond`*x*, with *x* the smallest
        unused non-negative integer.
    -   The MAC of the primary slave is used if no MAC was specified.
    -   Include the IP configuration of the primary slave.
    -   If any of the slaves has `PIF.disallow_unplug = true`, this will
        be copied to the master.

6.  Create the Bond object, specifying a reference to the master. The
    value of the `PIF.master_of` field on the master is dynamically
    computed on request. Also a reference to the primary slave is
    written to `Bond.primary_slave` on the new Bond object.
7.  Set the `PIF.bond_slave_of` fields of the slaves. The value of the
    `Bond.slaves` field is dynamically computed on request.
8.  Move VLANs, plus the VIFs-to-move on them, to the master.
    -   If all VLANs on the slaves have different tags, all VLANs will
        be moved to the bond master, while the same Network is used. The
        network effectively moves up to the bond and therefore no VIFs
        need to be moved.
    -   If multiple VLANs on different slaves have the same tag, they
        necessarily have different Networks as well. Only one VLAN with
        this tag is created on the bond master. All VIFs-to-move on the
        remaining VLAN networks are moved to the Network that was moved
        up.

9.  Move Tunnels to the master. The tunnel Networks move up with the
    tunnels. As tunnel keys are different for all tunnel networks, there
    are no complications as in the VLAN case.
10. Move VIFs-to-move on the slaves to the master.
11. If one of the slaves is the current management interface, move
    management to the master; the master will automatically be plugged.
    If none of the slaves is the management interface, plug the master
    if any of the slaves was plugged. In both cases, the slaves will
    automatically be unplugged.
12. On all slaves, reset the IP configuration and set `disallow_unplug`
    to false.

*Note: "moving" a VIF, VLAN or tunnel means "re-creating somewhere else,
and destroying the old one".*

Destroying a Bond
-----------------

### Current Behaviour on Bond destruction

Steps for a user to destroy a bond:

1.  If the management interface is on the bond, move it to another PIF
    using `PIF.reconfigure_ip` and `Host.management_reconfigure`.
    Otherwise, no `PIF.unplug` needs to be called on the bond master, as
    `Bond.destroy` does this automatically.
2.  Call `Bond.destroy` with a ref to the Bond object.
3.  If desired, bring up the former slave PIFs by calls to `PIF.plug`
    (this is does not happen automatically).

`Bond.destroy` XenAPI call:

1.  Validate the following constraints:
    -   No VLANs are attached to the bond master.
    -   The bond master is not the management PIF.

2.  Bring down the master PIF and clean up the underlying network
    devices.
3.  Remove the Bond and master PIF objects.

### New Behaviour on Bond destruction

Steps for a user to destroy a bond:

1.  Call `Bond.destroy` with a ref to the Bond object.
2.  If desired, move VIFs/VLANs/tunnels/management from (former) primary
    slave to other PIFs.

`Bond.destroy` XenAPI call is updated to do the following:

1.  Try unplugging all currently attached VIFs of the set of VIFs that
    need to be moved. Roll back and raise an exception of one of the
    VIFs cannot be unplugged (e.g. due to the absence of PV drivers in
    the VM).
2.  Copy the IP configuration of the master to the primary slave.
3.  Move VLANs, with their Networks, to the primary slave.
4.  Move Tunnels, with their Networks, to the primary slave.
5.  Move VIFs-to-move on the master to the primary slave.
6.  If the master is the current management interface, move management
    to the primary slave. The primary slave will automatically be
    plugged.
7.  If the master was plugged, plug the primary slave. This will
    automatically clean up the underlying devices of the bond.
8.  If the master has `PIF.disallow_unplug = true`, this will be copied
    to the primary slave.
9.  Remove the Bond and master PIF objects.

Using Bond Slaves
-----------------

### Current Behaviour for Bond Slaves

-   It possible to plug any existing PIF, even bond slaves. Any other
    PIFs that cannot be attached at the same time as the PIF that is
    being plugged, are automatically unplugged.
-   Similarly, it is possible to make a bond slave the management
    interface. Any other PIFs that cannot be attached at the same time
    as the PIF that is being plugged, are automatically unplugged.
-   It is possible to have a VIF on a Network associated with a bond
    slave. When the VIF's VM is started, or the VIF is hot-plugged, the
    PIF is relies on is automatically plugged, and any other PIFs that
    cannot be attached at the same time as this PIF are automatically
    unplugged.
-   It is possible to have a VLAN on a bond slave, though the bond
    (master) and the VLAN may not be simultaneously attached. This is
    not currently enforced (which may be considered a bug).

### New behaviour for Bond Slaves

-   It is no longer possible to plug a bond slave. The exception
    CANNOT\_PLUG\_BOND\_SLAVE is raised when trying to do so.
-   It is no longer possible to make a bond slave the management
    interface. The exception CANNOT\_PLUG\_BOND\_SLAVE is raised when
    trying to do so.
-   It is still possible to have a VIF on the Network of a bond slave.
    However, it is not possible to start such a VIF's VM on a host, if
    this would need a bond slave to be plugged. Trying this will result
    in a CANNOT\_PLUG\_BOND\_SLAVE exception. Likewise, it is not
    possible to hot-plug such a VIF.
-   It is no longer possible to place a VLAN on a bond slave. The
    exception CANNOT\_ADD\_VLAN\_TO\_BOND\_SLAVE is raised when trying
    to do so.
-   It is no longer possible to place a tunnel on a bond slave. The
    exception CANNOT\_ADD\_TUNNEL\_TO\_BOND\_SLAVE is raised when trying
    to do so.

Actions on Start-up
-------------------

### Current Behaviour on Start-up

When a pool slave starts up, bonds and VLANs on the pool master are
replicated on the slave:

-   Create all VLANs that the master has, but the slave has not. VLANs
    are identified by their tag, the device name of the slave PIF, and
    the Networks of the master and slave PIFs.
-   Create all bonds that the master has, but the slave has not. If the
    interfaces needed for the bond are not all available on the slave, a
    partial bond is created. If some of these interface are already
    bonded on the slave, this bond is destroyed first.

### New Behaviour on Start-up

-   The current VLAN/tunnel/bond recreation code is retained, as it uses
    the new Bond.create and Bond.destroy functions, and therefore does
    what it needs to do.
-   Before VLAN/tunnel/bond recreation, any violations of the rules
    defined in R2 are rectified, by moving VIFs, VLANs, tunnels or
    management up to bonds.

CLI
===

The behaviour of the `xe` CLI commands `bond-create`, `bond-destroy`,
`pif-plug`, and `host-management-reconfigure` is changed to match their
associated XenAPI calls.

XenCenter
=========

XenCenter already automatically moves the management interface when a
bond is created or destroyed. This is no longer necessary, as the
`Bond.create/destroy` calls already do this. XenCenter only needs to
copy any `PIF.other_config` keys that is needs between primary slave and
bond master.

Manual Tests
============

-   Create a bond of two interfaces...
    -   without VIFs/VLANs/management on them;
    -   with management on one of them;
    -   with a VLAN on one of them;
    -   with two VLANs on two different interfaces, having the same VLAN
        tag;
    -   with a VIF associated with a halted VM on one of them;
    -   with a VIF associated with a running VM (with and without PV
        drivers) on one of them.
-   Destroy a bond of two interfaces...
    -   without VIFs/VLANs/management on it;
    -   with management on it;
    -   with a VLAN on it;
    -   with a VIF associated with a halted VM on it;
    -   with a VIF associated with a running VM (with and without PV
        drivers) on it.
-   In a pool of two hosts, having VIFs/VLANs/management on the
    interfaces of the pool slave, create a bond on the pool master, and
    restart XAPI on the slave.
-   Restart XAPI on a host with a networking configuration that has
    become illegal due to these requirements.

