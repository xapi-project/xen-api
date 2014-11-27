# Copyright (c) 2009,2010,2011 Juniper Networks.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; version 2.1 only. with the special
# exception on linking described in file LICENSE.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
from InterfaceReconfigure import *
import os
import re
import subprocess

#
# Bare Network Devices -- network devices without IP configuration
#

def netdev_down(netdev):
    """Bring down a bare network device"""
    if not netdev_exists(netdev):
        log("netdev: down: device %s does not exist, ignoring" % netdev)
        return
    run_command(["/sbin/ifconfig", netdev, 'down'])

def netdev_up(netdev, mtu=None):
    """Bring up a bare network device"""
    if not netdev_exists(netdev):
        raise Error("netdev: up: device %s does not exist" % netdev)

    if mtu:
        mtu = ["mtu", mtu]
    else:
        mtu = []

    run_command(["/sbin/ifconfig", netdev, 'up'] + mtu)

#
# PIF miscellanea
#

def pif_currently_in_use(pif):
    """Determine if a PIF is currently in use.

    A PIF is determined to be currently in use if
    - PIF.currently-attached is true
    - Any bond master is currently attached
    - Any VLAN master is currently attached
    """
    rec = db().get_pif_record(pif)
    if rec['currently_attached']:
        log("configure_datapath: %s is currently attached" % (pif_netdev_name(pif)))
        return True
    for b in pif_get_bond_masters(pif):
        if pif_currently_in_use(b):
            log("configure_datapath: %s is in use by BOND master %s" % (pif_netdev_name(pif),pif_netdev_name(b)))
            return True
    for v in pif_get_vlan_masters(pif):
        if pif_currently_in_use(v):
            log("configure_datapath: %s is in use by VLAN master %s" % (pif_netdev_name(pif),pif_netdev_name(v)))
            return True
    return False

#
# Datapath Configuration
#

def pif_datapath(pif):
    """Return the datapath PIF associated with PIF.
A non-VLAN PIF is its own datapath PIF, except that a bridgeless PIF has
no datapath PIF at all.
A VLAN PIF's datapath PIF is its VLAN slave's datapath PIF.
"""
    if pif_is_vlan(pif):
        return pif_datapath(pif_get_vlan_slave(pif))

    pifrec = db().get_pif_record(pif)
    nwrec = db().get_network_record(pifrec['network'])
    if not nwrec['bridge']:
        return None
    else:
        return pif

def datapath_get_physical_pifs(pif):
    """Return the PIFs for the physical network device(s) associated with a datapath PIF.
For a bond master PIF, these are the bond slave PIFs.
For a non-VLAN, non-bond master PIF, the PIF is its own physical device PIF.

A VLAN PIF cannot be a datapath PIF.
"""
    if pif_is_tunnel(pif):
        return []
    elif pif_is_vlan(pif):
        # Seems like overkill...
        raise Error("get-physical-pifs should not get passed a VLAN")
    elif pif_is_bond(pif):
        return pif_get_bond_slaves(pif)
    else:
        return [pif]

def datapath_deconfigure_physical(netdev):
    return ['--', '--with-iface', '--if-exists', 'del-port', netdev]

def vrctl_escape(s):
    if s.isalnum():
        return s

    def escape(match):
        c = match.group(0)
        if c == '\0':
            raise Error("strings may not contain null bytes")
        elif c == '\\':
            return r'\\'
        elif c == '\n':
            return r'\n'
        elif c == '\r':
            return r'\r'
        elif c == '\t':
            return r'\t'
        elif c == '\b':
            return r'\b'
        elif c == '\a':
            return r'\a'
        else:
            return r'\x%02x' % ord(c)
    return '"' + re.sub(r'["\\\000-\037]', escape, s) + '"'

def datapath_configure_tunnel(pif):
    pass

def datapath_configure_bond(pif,slaves):
    pass

def datapath_deconfigure_bond(netdev):
    return ['--', '--with-iface', '--if-exists', 'del-port', netdev]

def datapath_deconfigure_ipdev(interface):
    return ['--', '--with-iface', '--if-exists', 'del-port', interface]

def datapath_modify_config(commands):
    rc = run_command(['/opt/contrail/bin/contrail-vrouter-ctl'] + ['--timeout=20']
                     + [c for c in commands if not c.startswith('#')])
    if not rc:       
        raise Error("Failed to modify vrouter configuration")
    return True

#
# Toplevel Datapath Configuration.
#

def configure_datapath(pif):
    """Bring up the configuration for 'pif', which must not be a VLAN PIF, by:
    - Tearing down other PIFs that use the same physical devices as 'pif'.
    - Ensuring that 'pif' itself is set up.
    - *Not* tearing down any PIFs that are stacked on top of 'pif' (i.e. VLANs
      on top of 'pif'.

    Returns a tuple containing
    - A list containing the necessary vrctl command line arguments
    - A list of additional devices which should be brought up after
      the configuration is applied.
    - A list containing flows to apply to the pif bridge, note that
      port numbers may need to be substituted once ofport is known
    """

    vrctl_argv = []
    extra_up_ports = []
    bridge_flows = []

    assert not pif_is_vlan(pif)
    bridge = pif_bridge_name(pif)

    physical_devices = datapath_get_physical_pifs(pif)

    vrctl_argv += ['## configuring datapath %s' % bridge]

    # Determine additional devices to deconfigure.
    #
    # Given all physical devices which are part of this PIF we need to
    # consider:
    # - any additional bond which a physical device is part of.
    # - any additional physical devices which are part of an additional bond.
    #
    # Any of these which are not currently in use should be brought
    # down and deconfigured.
    extra_down_bonds = []
    extra_down_ports = []
    for p in physical_devices:
        for bond in pif_get_bond_masters(p):
            if bond == pif:
                log("configure_datapath: leaving bond %s up" % pif_netdev_name(bond))
                continue
            if bond in extra_down_bonds:
                continue
            if db().get_pif_record(bond)['currently_attached']:
                log("configure_datapath: implicitly tearing down currently-attached bond %s" % pif_netdev_name(bond))

            extra_down_bonds += [bond]

            for s in pif_get_bond_slaves(bond):
                if s in physical_devices:
                    continue
                if s in extra_down_ports:
                    continue
                if pif_currently_in_use(s):
                    continue
                extra_down_ports += [s]

    log("configure_datapath: bridge      - %s" % bridge)
    log("configure_datapath: physical    - %s" % [pif_netdev_name(p) for p in physical_devices])
    log("configure_datapath: extra ports - %s" % [pif_netdev_name(p) for p in extra_down_ports])
    log("configure_datapath: extra bonds - %s" % [pif_netdev_name(p) for p in extra_down_bonds])

    # Need to fully deconfigure any bridge which any of the:
    # - physical devices
    # - bond devices
    # - sibling devices
    # refers to
    for brpif in physical_devices + extra_down_ports + extra_down_bonds:
        if brpif == pif:
            continue
        b = pif_bridge_name(brpif)
        #ifdown(b)
        # XXX
        netdev_down(b)
        vrctl_argv += ['# remove bridge %s' % b]
        vrctl_argv += ['--', '--if-exists', 'del-br', b]

    for n in extra_down_ports:
        dev = pif_netdev_name(n)
        vrctl_argv += ['# deconfigure sibling physical device %s' % dev]
        vrctl_argv += datapath_deconfigure_physical(dev)
        netdev_down(dev)

    for n in extra_down_bonds:
        dev = pif_netdev_name(n)
        vrctl_argv += ['# deconfigure bond device %s' % dev]
        vrctl_argv += datapath_deconfigure_bond(dev)
        netdev_down(dev)

    for p in physical_devices:
        dev = pif_netdev_name(p)
        vrctl_argv += ['# deconfigure physical port %s' % dev]
        vrctl_argv += datapath_deconfigure_physical(dev)

    vrctl_argv += ['--', '--may-exist', 'add-br', bridge]

    if len(physical_devices) > 1:
        vrctl_argv += ['# deconfigure bond %s' % pif_netdev_name(pif)]
        vrctl_argv += datapath_deconfigure_bond(pif_netdev_name(pif))
        vrctl_argv += ['# configure bond %s' % pif_netdev_name(pif)]
        vrctl_argv += datapath_configure_bond(pif, physical_devices)
        extra_up_ports += [pif_netdev_name(pif)]
    elif len(physical_devices) == 1:
        iface = pif_netdev_name(physical_devices[0])
        vrctl_argv += ['# add physical device %s' % iface]
        vrctl_argv += ['--', '--may-exist', 'add-port', bridge, iface]
    elif pif_is_tunnel(pif):
        datapath_configure_tunnel(pif)

    vrctl_argv += ['# configure Bridge MAC']
    vrctl_argv += ['--', 'set', 'Bridge', bridge,
                   'other-config:hwaddr=%s' % vrctl_escape(db().get_pif_record(pif)['MAC'])]

    pool = db().get_pool_record()
    network = db().get_network_by_bridge(bridge)
    network_rec = None
    fail_mode = None
    valid_fail_modes = ['standalone', 'secure']

    if network:
        network_rec = db().get_network_record(network)
        fail_mode = network_rec['other_config'].get('vswitch-controller-fail-mode')

    vrctl_argv += ['--', 'set', 'Bridge', bridge, 'fail_mode=%s' % fail_mode]

    if network_rec:
        dib = network_rec['other_config'].get('vswitch-disable-in-band')
        if not dib:
            vrctl_argv += ['--', 'remove', 'Bridge', bridge, 'other_config', 'disable-in-band']
        elif dib in ['true', 'false']:
            vrctl_argv += ['--', 'set', 'Bridge', bridge, 'other_config:disable-in-band=' + dib]
        else:
            log('"' + dib + '"' "isn't a valid setting for other_config:disable-in-band on " + bridge)

    vrctl_argv += set_br_external_ids(pif)
    vrctl_argv += ['## done configuring datapath %s' % bridge]

    return vrctl_argv,extra_up_ports,bridge_flows

def deconfigure_bridge(pif):
    vrctl_argv = []

    bridge = pif_bridge_name(pif)

    log("deconfigure_bridge: bridge           - %s" % bridge)

    vrctl_argv += ['# deconfigure bridge %s' % bridge]
    vrctl_argv += ['--', '--if-exists', 'del-br', bridge]

    return vrctl_argv

def set_br_external_ids(pif):
    pifrec = db().get_pif_record(pif)
    dp = pif_datapath(pif)
    dprec = db().get_pif_record(dp)

    xs_network_uuids = []
    for nwpif in db().get_pifs_by_device(pifrec['device']):
        rec = db().get_pif_record(nwpif)

        # When state is read from dbcache PIF.currently_attached
        # is always assumed to be false... Err on the side of
        # listing even detached networks for the time being.
        #if nwpif != pif and not rec['currently_attached']:
        #    log("Network PIF %s not currently attached (%s)" % (rec['uuid'],pifrec['uuid']))
        #    continue
        nwrec = db().get_network_record(rec['network'])

        uuid = nwrec['uuid']
        if pif_is_vlan(nwpif):
            xs_network_uuids.append(uuid)
        else:
            xs_network_uuids.insert(0, uuid)

    vrctl_argv = []
    vrctl_argv += ['# configure xs-network-uuids']
    vrctl_argv += ['--', 'br-set-external-id', pif_bridge_name(pif),
            'xs-network-uuids', ';'.join(xs_network_uuids)]

    return vrctl_argv

#
#
#

class DatapathVrouter(Datapath):
    def __init__(self, pif):
        Datapath.__init__(self, pif)
        self._dp = pif_datapath(pif)
        self._ipdev = pif_ipdev_name(pif)
        self._bridge_flows = []

        if pif_is_vlan(pif) and not self._dp:
            raise Error("Unbridged VLAN devices not implemented yet")
        
        log("Configured for Vrouter datapath")

    @classmethod
    def rewrite(cls):
        vrctl_argv = []
        for pif in db().get_all_pifs():
            pifrec = db().get_pif_record(pif)
            if not pif_is_vlan(pif) and pifrec['currently_attached']:
                vrctl_argv += set_br_external_ids(pif)

        if vrctl_argv != []:
            datapath_modify_config(vrctl_argv)

    def configure_ipdev(self, cfg):
        cfg.write("TYPE=Ethernet\n")

    def preconfigure(self, parent):
        vrctl_argv = []
        extra_ports = []
        bridge_flows = []

        pifrec = db().get_pif_record(self._pif)
        dprec = db().get_pif_record(self._dp)

        ipdev = self._ipdev
        c,e,f = configure_datapath(self._dp)
        bridge = pif_bridge_name(self._pif)
        vrctl_argv += c
        extra_ports += e
        bridge_flows += f

        dpname = pif_bridge_name(self._dp)
        
        if pif_is_vlan(self._pif):
            # In some cases XAPI may misguidedly leave an instance of
            # 'bridge' which should be deleted.
            vrctl_argv += ['--', '--if-exists', 'del-br', bridge]

            # configure_datapath() set up the underlying datapath bridge.
            # Stack a VLAN bridge on top of it.
            vrctl_argv += ['--', '--may-exist', 'add-br',
                           bridge, dpname, pifrec['VLAN']]

            vrctl_argv += set_br_external_ids(self._pif)

        if ipdev != bridge:
            vrctl_argv += ["# deconfigure ipdev %s" % ipdev]
            vrctl_argv += datapath_deconfigure_ipdev(ipdev)
            vrctl_argv += ["# reconfigure ipdev %s" % ipdev]
            vrctl_argv += ['--', 'add-port', bridge, ipdev]

        if ipdev != dpname:
            vrctl_argv += ['# configure Interface MAC']
            vrctl_argv += ['--', 'set', 'Interface', pif_ipdev_name(self._pif),
                           'MAC=%s' % vrctl_escape(dprec['MAC'])]

        self._vrctl_argv = vrctl_argv
        self._extra_ports = extra_ports
        self._bridge_flows = bridge_flows

    def bring_down_existing(self):
        # interface-reconfigure is never explicitly called to down a
        # bond master.  However, when we are called to up a slave it
        # is implicit that we are destroying the master.  Conversely,
        # when we are called to up a bond is is implicit that we are
        # taking down the slaves.
        #
        # This is (only) important in the case where the device being
        # implicitly taken down uses DHCP.  We need to kill the
        # dhclient process, otherwise performing the inverse operation
        # later later will fail because ifup will refuse to start a
        # duplicate dhclient.
        bond_masters = pif_get_bond_masters(self._pif)
        for master in bond_masters:
            log("action_up: bring down bond master %s" % (pif_netdev_name(master)))
            run_command(["/sbin/ifdown", pif_bridge_name(master)])

        bond_slaves = pif_get_bond_slaves(self._pif)
        for slave in bond_slaves:
            log("action_up: bring down bond slave %s" % (pif_netdev_name(slave)))
            run_command(["/sbin/ifdown", pif_bridge_name(slave)])

    def configure(self):
        # Bring up physical devices. ovs-vswitchd initially enables or
        # disables bond slaves based on whether carrier is detected
        # when they are added, and a network device that is down
        # always reports "no carrier".
        physical_devices = datapath_get_physical_pifs(self._dp)

        if pif_is_bond(self._dp):
            brec = db().get_pif_record(self._dp)
            bond_mtu = mtu_setting(brec['network'], "PIF", brec['other_config'])
        else:
            bond_mtu = None
        
        for p in physical_devices:
            prec = db().get_pif_record(p)
            oc = prec['other_config']

            dev = pif_netdev_name(p)

            if bond_mtu:
                mtu = bond_mtu
            else:
                mtu = mtu_setting(prec['network'], "PIF", oc)

            netdev_up(dev, mtu)

            settings, offload = ethtool_settings(oc, PIF_OTHERCONFIG_DEFAULTS)
            if len(settings):
                run_command(['/sbin/ethtool', '-s', dev] + settings)
            if len(offload):
                run_command(['/sbin/ethtool', '-K', dev] + offload)

        datapath_modify_config(self._vrctl_argv)

    def post(self):
        for p in self._extra_ports:
            log("action_up: bring up %s" % p)
            netdev_up(p)

    def bring_down(self):
        vrctl_argv = []

        dp = self._dp
        ipdev = self._ipdev
        
        bridge = pif_bridge_name(dp)

        log("deconfigure ipdev %s on %s" % (ipdev,bridge))
        vrctl_argv += ["# deconfigure ipdev %s" % ipdev]
        vrctl_argv += datapath_deconfigure_ipdev(ipdev)

        if pif_is_vlan(self._pif):
            # Delete the VLAN bridge.
            vrctl_argv += deconfigure_bridge(self._pif)

            # If the VLAN's slave is attached, leave datapath setup.
            slave = pif_get_vlan_slave(self._pif)
            if db().get_pif_record(slave)['currently_attached']:
                log("action_down: vlan slave is currently attached")
                dp = None

            # If the VLAN's slave has other VLANs that are attached, leave datapath setup.
            for master in pif_get_vlan_masters(slave):
                if master != self._pif and db().get_pif_record(master)['currently_attached']:
                    log("action_down: vlan slave has other master: %s" % pif_netdev_name(master))
                    dp = None

            # Otherwise, take down the datapath too (fall through)
            if dp:
                log("action_down: no more masters, bring down slave %s" % bridge)
        else:
            # Stop here if this PIF has attached VLAN masters.
            masters = [db().get_pif_record(m)['VLAN'] for m in pif_get_vlan_masters(self._pif) if db().get_pif_record(m)['currently_attached']]
            if len(masters) > 0:
                log("Leaving datapath %s up due to currently attached VLAN masters %s" % (bridge, masters))
                dp = None

        if dp:
            vrctl_argv += deconfigure_bridge(dp)

            physical_devices = [pif_netdev_name(p) for p in datapath_get_physical_pifs(dp)]

            log("action_down: bring down physical devices - %s" % physical_devices)
        
            for p in physical_devices:
                netdev_down(p)

        datapath_modify_config(vrctl_argv)
