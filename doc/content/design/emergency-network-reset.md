---
title: Emergency Network Reset Design
layout: default
design_doc: true
revision: 1
status: released (6.0.2)
---

This document describes design details for the PR-1032 requirements.

The design consists of four parts:

1.  A new XenAPI call `Host.reset_networking`, which removes all the
    PIFs, Bonds, VLANs and tunnels associated with the given host, and a
    call `PIF.scan_bios` to bring back the PIFs with device names as
    defined in the BIOS.
2.  A `xe-reset-networking` script that can be executed on a XenServer
    host, which prepares the reset and causes the host to reboot.
3.  An xsconsole page that essentially does the same as
    `xe-reset-networking`.
4.  A new item in the XAPI start-up sequence, which when triggered by
    `xe-reset-networking`, calls `Host.reset_networking` and re-creates
    the PIFs.

Command-Line Utility
--------------------

The `xe-reset-networking` script takes the following parameters:

<table><tr>
<th>Parameter</th>
<th>Description</th>
</tr>
<tr>
<td><code>-m</code>, <code>--master</code></td>
<td>The IP address of the master. Optional if the host is pool slave, ignored otherwise.</td>
</tr>
<tr>
<td><code>--device</code></td>
<td>Device name of management interface. Optional. If not specified, it is taken from the firstboot data.</td>
</tr>
<tr>
<td><code>--mode</code></td>
<td>IP configuration mode for management interface. Optional. Either <code>dhcp</code> or <code>static</code> (default is <code>dhcp</code>).</td>
</tr>
<tr>
<td><code>--ip</code></td>
<td>IP address for management interface. Required if <code>--mode=static</code>, ignored otherwise.</td>
</tr>
<tr>
<td><code>--netmask</code></td>
<td>Netmask for management interface. Required if <code>--mode=static</code>, ignored otherwise.</td>
</tr>
<tr>
<td><code>--gateway</code></td>
<td>Gateway for management interface. Optional; ignored if <code>--mode=dhcp</code>.</td>
</tr>
<tr>
<td><code>--dns</code></td>
<td>DNS server for management interface. Optional; ignored if <code>--mode=dhcp</code>.</td>
</tr>
</table>

DNS server for management interface. Optional; ignored if `--mode=dhcp`.

The script takes the following steps after processing the given
parameters:

1.  Inform the user that the host will be restarted, and that any
    running VMs should be shut down. Make the user confirm that they
    really want to reset the networking by typing 'yes'.
2.  Read `/etc/xensource/pool.conf` to determine whether the host is a
    pool master or pool slave.
3.  If a pool slave, update the IP address in the `pool.conf` file to
    the one given in the `-m` parameter, if present.
4.  Shut down networking subsystem (`service network stop`).
5.  If no management device is specified, take it from
    /etc/firstboot.d/data/management.conf.
6.  If XAPI is running, stop it.
7.  Reconfigure the management interface and associated bridge by
    `interface-reconfigure --force`.
8.  Update `MANAGEMENT_INTERFACE` and clear `CURRENT_INTERFACES` in
    `/etc/xensource-inventory`.
9.  Create the file `/tmp/network-reset` to trigger XAPI to complete the
    network reset after the reboot. This file should contain the full
    configuration details of the management interface as key/value pairs
    (format: `<key>=<value>\n`), and looks similar to the firstboot data
    files. The file contains at least the keys `DEVICE` and `MODE`, and
    `IP`, `NETMASK`, `GATEWAY`, or `DNS` when appropriate.
10. Reboot

XAPI
----

### XenAPI

A new *hidden* API call:

-   `Host.reset_networking`
    -   Parameter: host reference `host`
    -   Calling this function removes all the PIF, Bond, VLAN and tunnel
        objects associated with the given host from the master database.
        All Network and VIF objects are maintained, as these do not
        necessarily belong to a single host.

### Start-up Sequence

After reboot, in the XAPI start-up sequence trigged by the presence of
`/tmp/network-reset`:

1.  Read the desired management configuration from `/tmp/network-reset`.
2.  Call `Host.reset_networking` with a ref to the localhost.
3.  Call `PIF.scan` with a ref to the localhost to recreate the
    (physical) PIFs.
4.  Call `PIF.reconfigure_ip` to configure the management interface.
5.  Call `Host.management_reconfigure`.
6.  Delete `/tmp/network-reset`.

xsconsole
---------

Add an "Emergency Network Reset" option under the "Network and
Management Interface" menu. Selecting this option will show some
explanation in the pane on the right-hand side. Pressing \<Enter\> will
bring up a dialogue to select the interfaces to use as management
interface after the reset. After choosing a device, the dialogue
continues with configuration options like in the "Configure Management
Interface" dialogue. After completing the dialogue, the same steps as
listed for `xe-reset-networking` are executed.

Notes
-----

-   On a pool slave, the management interface should be the same as on
    the master (the same device name, e.g. eth0).
-   Resetting the networking configuration on the master should be
    ideally be followed by resets of the pool slaves as well, in order
    to synchronise their configuration (especially bonds/VLANs/tunnels).
    Furthermore, in case the IP address of the master has changed, as a
    result of a network reset or `Host.management_reconfigure`, pool
    slaves may also use the network reset functionality to reconnect to
    the master on its new IP.

