---
title: Management Interface on VLAN
layout: default
design_doc: true
revision: 3
status: proposed
revision_history:
- revision_number: 1
  description: Initial version
- revision_number: 2
  description: Addition of `networkd_db` update for Upgrade
- revision_number: 3
  description: More info on `networkd_db` and API Errors
---

This document describes design details for the
REQ-42: Support Use of VLAN on XAPI Management Interface.

XAPI and XCP-Networkd
===============

### Creating a VLAN

Creating a VLAN is already there, Lisiting the steps to create a VLAN which is used later in the document.
Steps:

1.  Check the PIFs created on a Host for physical devices `eth0`, `eth1`.
    `xe pif-list params=uuid physical=true host-uuid=UUID` this will list `pif-UUID`
2.  Create a new network for the VLAN interface.
    `xe network-create name-label=VLAN1`
    It returns a new `network-UUID`
3.  Create a VLAN PIF.
    `xe vlan-create pif-uuid=pif-UUID network-uuid=network-UUID vlan=VLAN-ID`
    It returns a new VLAN PIF `new-pif-UUID`
4.  Plug the VLAN PIF.
    `xe pif-plug uuid=new-pif-UUID`
5.  Configure IP on the VLAN PIF.
    `xe pif-reconfigure-ip uuid=new-pif-UUID mode= IP= netmask= gateway= DNS= `
    This will configure IP on the PIF, here `mode` is must and other parametrs are needed on selecting mode=static

Similarly, creating a vlan pif can be achieved by corresponding XenAPI calls.

Recognise VLAN config from management.conf
----------------------------------------------

For a newly installed host, If host installer was asked to put the management interface on given VLAN.
We will expect a new entry `VLAN=ID` under `/etc/firstboot.d/data/management.conf`.

Listing current contents of management.conf which will be used later in the document.
`LABEL`=`eth0`          -> Represents Pyhsical device on which Management Interface must reside.
`MODE`=`dhcp`||`static` -> Represents IP configuration mode for the Management Interface. There can be other parameters like IP, NETMASK, GATEWAY and DNS when we have `static` mode.
`VLAN`=`ID`             -> New entry for specifying VLAN TAG going to be configured on device `LABEL`.
                           Management interface going to be configured on this VLAN ID with specified mode.

### Firstboot script need to recognise VLAN config

Firstboot script `/etc/firstboot.d/30-prepare-networking` need to be updated for configuring
management interface to be on provided VLAN ID.

Steps to be followed:

1.  `PIF.scan` performed in the script must have created the PIFs for the underlying pyhsical devices.
2.  Get the PIF UUID for physical device `LABEL`.
3.  Repeat the steps mentioned in `Creating a VLAN`, i.e. network-create, vlan-create and pif-plug. Now we have a new PIF for the VLAN.
4.  Perform `pif-reconfigure-ip` for the new VLAN PIF.
5.  Perform `host-management-reconfigure` using new VLAN PIF.

### XCP-Networkd need to recognise VLAN config during startup

XCP-Networkd during first boot and boot after pool eject gets the initial network setup from the `management.conf` and `xensource-inventory` file to update the network.db for management interface info.
XCP-Networkd must honour the new VLAN config.

Steps to be followed:

1.  During startup `read_config` step tries to read the `/var/lib/xcp/networkd.db` file which is not yet created just after host installation.
2.  Since `networkd.db` read throws `Read_Error`, it tries to read `network.dbcache` which is also not available hence it goes to read `read_management_conf` file.
3.  There can be two possible MODE `static` or `dhcp` taken from management.conf.
4.  `bridge_name` is taken as `MANAGEMENT_INTERFACE` from xensource-inventory, further `bridge_config` and `interface_config` are build based on MODE.
5.  Call `Bridge.make_config()` and `Interface.make_config()` are performed with respective `bridge_config` and `interface_config`.

Updating networkd_db program
----------------------------

`networkd_db` provides the management interface info to the host installer during upgrade.
It reads `/var/lib/xcp/networkd.db` file to output the Management Interface information. Here we need to update the networkd_db to output the VLAN information when vlan bridge is a input.

Steps to be followed:

1.  Currently VLAN interface IP information is provided correctly on passing VLAN bridge as input.
    `networkd_db -iface xapi0` this will list `mode` as dhcp or static, if mode=static then it will provide `ipaddr` and `netmask` too.
2.  We need to udpate this program to provide VLAN ID and parent bridge info on passing VLAN bridge as input.
    `networkd_db -bridge xapi0` It should output the VLAN info like:
    `interfaces=`
    `vlan=vlanID`
    `parent=xenbr0` using the parent bridge user can identify the physical interfaces.
    Here we will extract VLAN and parent bridge from `bridge_config` under `networkd.db`.

Additional VLAN parameter for Emergency Network Reset
-----------------------------------------------------

Detail design is mentioned on http://xapi-project.github.io/xapi/design/emergency-network-reset.html
For using `xe-reset-networking` utility to configure management interface on VLAN, We need to add one more parameter `--vlan=vlanID` to the utility.
There are certain parameters need to be passed to this utility: --master, --device, --mode, --ip, --netmask, --gateway, --dns and new one --vlan.

### VLAN parameter addition to xe-reset-networking

Steps to be followed:

1.  Check if `VLANID` is passed then let bridge=`xapi0`.
2.  Write the `bridge=xapi0` into xensource-inventory file, This should work as Xapi check avialable bridges while creating networks.
3.  Write the `VLAN=vlanID` into `management.conf` and `/tmp/network-reset`.
4.  Modify `check_network_reset` under xapi.ml to perform steps `Creating a VLAN` and perform `management_reconfigure` on vlan pif.
    Step `Creating a VLAN` must have created the VLAN record in Xapi DB similar to firstboot script.
5.  If no VLANID is specified then retain the current one, This utility must take the management interface info from `networkd_db` program and handle the VLAN config.

### VLAN parameter addition to xsconsole Emergency Network Reset

Under `Emergency Network Reset` option under the `Network and Management Interface` menu.
Selecting this option will show some explanation in the pane on the right-hand side.
Pressing <Enter> will bring up a dialogue to select the interfaces to use as management interface after the reset.
After choosing a device, the dialogue continues with configuration options like in the `Configure Management Interface` dialogue.
There will be an additionall option for VLAN in the dialogue.
After completing the dialogue, the same steps as listed for xe-reset-networking are executed.

Updating Pool Join/Eject operations
-----------------------------------

### Pool Join while Pool having Management Interface on a VLAN

Currently `pool-join` fails if VLANs are present on the host joining a pool.
We need to allow pool-join only if Pool and host joining a pool both has management interface on same VLAN.

Steps to be followed:

1.  Under `pre_join_checks` update function `assert_only_physical_pifs` to check Pool master management_interface is on same VLAN.
2.  Call `Host.get_management_interface` on Pool master and get the vlanID, match it with `localhost` management_interface VLAN ID.
    If it matches then allow pool-join.
3.  In case if there are multiple VLANs on host joining a pool, fail the pool-join gracefully.
4.  After the pool-join, Host xapi db will get sync from pool master xapi db, This will be fine to have management interface on VLAN.

### Pool Eject while host ejected having Management Interface on a VLAN

Currently managament interface VLAN config on host is not been retained in `xensource-inventory` or `management.conf` file.
We need to retain the vlanID under config files.

Steps to be followed:

1.  Under call `Pool.eject` we need to update `write_first_boot_management_interface_configuration_file` function.
2.  Check if management_interface is on VLAN then get the VLANID from the pif.
3.  Update the VLANID into the `managament.conf` file and the `bridge` into `xensource-inventory` file.
    In order to be retained by XCP-Networkd on startup after the host is ejected.

New API for Pool Management Reconfigure
---------------------------------------

Currently there is no Pool Level API to reconfigure management_interface for all of the Hosts in a Pool at once.
API `Pool.management_reconfigure` will be needed in order to reconfigure `manamegemnt_interface` on all hosts in a Pool to the same Network either VLAN or Physical.
 

### Current behaviour to change the Management Interface on Host

Currently call `Host.management_reconfigure` with VLAN pif-uuid can change the management_interface to specified VLAN.
Listing the steps to understand the workflow of `management_interface` reconfigure. We will be using `Host.management_reconfigure` call inside the new API.

Steps performed during management_reconfigure:

1.  `bring_pif_up` get called for the pif.
2.  `xensource-inventory` get updated with the latest info of interface.
3   `update-mh-info` updates the management_mac into xenstore.
4.  Http server gets restarted, even though xapi listen on all IP addresses, This new interface as `_the_ management` interface is used by slaves to connect to pool master.
5.  `on_dom0_networking_change` refreshes console URIs for the new IP address.  
6.  Xapi db is updated with new management interface info.

### Management Reconfigure on Pool from Physical Network to VLAN Network or from VLAN Network to Other VLAN Network or from VLAN Network to Physical Network

Listing steps to be performed manually on each Host or Pool as a prerequisite to use the New API.
We need to make sure that new network which is going to be a management interface has PIFs configured on each Host.
In case of pyhsical network we will assume pifs are configured on each host, In case of vlan network we need to create vlan pifs on each Host.
We would assume that VLAN is available on the switch/network.

Manual steps to be performed before calling new API:

1.  Create a vlan network on pool via `network.create`, In case of pyhsical NICs network must be present.
2.  Create a vlan pif on each host via `VLAN.create` using above network ref, physical PIF ref  and vlanID, Not needed in case of pyhsical network.
    Or An Alternate call `pool.create_VLAN` providing `device` and above `network` will create vlan PIFs for all hosts in a pool.
3.  Perform `PIF.reconfigure_ip` for each new Network PIF on each Host.

If User wishes to change the management interface manually on each Host in a Pool, We should allow it, There will be a guideline for that:

User can individually change management interface on each host calling `Host.management_reconfigure` using pifs on physical devices or vlan pifs.
This must be perfomed on slaves first and lastly on Master, As changing management_interface on master will disconnect slaves from master then further calls `Host.management_reconfigure` cannot be performed till master recover slaves via call `pool.recover_slaves`.

### API Details

-   `Pool.management_reconfigure`
    -   Parameter: network reference `network`.
    -   Calling this function configures `management_interface` on each host of a pool.
    -   For the `network` provided it will check pifs are present on each Host,
        In case of VLAN network it will check vlan pifs on provided network are present on each Host of Pool.
    -   Check IP is configured on above pifs on each Host.
    -   If PIFs are not present or IP is not configured on PIFs this call must fail gracefully, Asking user to configure them.
    -   Call `Host.management_reconfigure` on each slave then lastly on master.
    -   Call `pool.recover_slaves` on master inorder to recover slaves which might have lost the connection to master.

### API errors

Possible API errors that may be raised by `pool.management_reconfigure`:

-   `INTERFACE_HAS_NO_IP` : the specified PIF (`pif` parameter) has no IP configuration. The new API checks for all PIFs on the new Network has IP configured. There might be a case when user has forgotten to configure IP on PIF on one or many of the Hosts in a Pool.

New API ERROR:

-   `REQUIRED_PIF_NOT_PRESENT` : the specified Network (`network` parameter) has no PIF present on the host in pool. There might be a case when user has forgotten to create vlan pif on one or many of the Hosts in a Pool.

CP-Tickets
----------

1.  CP-14027
2.  CP-14028
3.  CP-14029
4.  CP-14030
5.  CP-14031
6.  CP-14032
7.  CP-14033
