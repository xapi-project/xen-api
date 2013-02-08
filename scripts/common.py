#!/usr/bin/python
#
# Copyright (c) 2011-2013 Citrix Systems, Inc.
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
import os
import os.path
import sys
import syslog

path = "/sbin:/usr/sbin:/bin:/usr/bin"
vif_hotplug = "/etc/xapi.d/vif-hotplug"
xenops_path = "/var/run/nonpersistent/xenops"

command_name = sys.argv[0]
short_command_name = os.path.basename(command_name)

def send_to_syslog(msg):
    pid = os.getpid()
    syslog.syslog("%s[%d] - %s" %(command_name, pid, msg))

def doexec(args):
    """Execute a subprocess, then return its return code, stdout and stderr"""
    send_to_syslog(args)
    proc = subprocess.Popen([ "/usr/bin/env", "PATH=%s" % path ] + args,stdin=None,stdout=subprocess.PIPE,stderr=subprocess.PIPE,close_fds=True)
    rc = proc.wait()
    stdout = proc.stdout
    stderr = proc.stderr
    if rc <> 0:
    return (rc, stdout, stderr)

ON_ERROR_LOG  = 1
ON_ERROR_FAIL = 2

def run(on_error, args):
    """Execute a subprocess and, on failure, either log (ON_ERROR_LOG) or throw (ON_ERROR_FAIL)"""
    (rc, stdout, stderr) = doexec(args)
    if rc <> 0:
        if on_error == ON_ERROR_FAIL:
             send_to_syslog("FATAL: %s exitted with %d: %s" % (" ".join(args), rc, stderr))
             sys.exit(1)
        send_to_syslog("%s exitted with %d: %s" % (" ".join(args), rc, stderr))
        if on_error <> ON_ERROR_LOG:
             send_to_syslog("... and an unrecognised on_error code was used (%d)" % on_error)

MODE_OPENVSWITCH = 1
MODE_BRIDGE      = 2

def set_promiscuous(mode, dev, value):
    if mode == MODE_OPENVSWITCH and value:
        send_to_syslog("%s: Promiscuous ports are not supported via Open vSwitch")
    elif mode == MODE_BRIDGE:
        f = open("/sys/class/net/%s/brport/promisc" % dev)
        try:
            x = "0"
            if value:
                x = "1"
            f.write(x)
        finally:
            f.close()

def set_ethtool(mode, dev, key, value):
    x = "off"
    if value:
        x = "on"
    run(ON_ERROR_LOG, ["ethtool", "-K", dev, key, x])

def set_mtu(mode, dev, mtu):
    send_to_syslog("Setting %s MTU %d" % (dev, mtu))
    run(ON_ERROR_LOG, ["ip", "link", "set", dev, "mtu", "%d" % mtu])

def add_to_bridge(mode, dev, bridge, address, external_ids):
    send_to_syslog("Adding %s to %s with address %s" % (dev, bridge, address))
    for cmd in [ ["down"], ["arp", "off"], ["multicast", "off"], ["address", address] ]:
        run(ON_ERROR_LOG, ["ip", "link", "set", dev] + cmd)
    run(ON_ERROR_LOG, ["ip", "addr", "flush", dev])

    if mode == MODE_BRIDGE:
        run(ON_ERROR_LOG, ["brctl", "setfd", bridge, "0"])
        run(ON_ERROR_LOG, ["brctl", "addif", bridge, dev])
    elif mode == MODE_OPENVSWITCH:
        cmd = ["ovs-vsctl", "--timeout=30", "--", "--if-exists", "del-port", dev, "--", "add-port", bridge, dev]
        for (key, value) in external_ids:
             cmd = cmd + ["--", "set", "interface", dev, 'external-ids:"%s"="%s"' % (key, value) ]
	run(ON_ERROR_LOG, cmd)

def remove_from_bridge(mode, dev, bridge):
    if mode == MODE_BRIDGE:
        run(ON_ERROR_LOG, ["brctl", "delif", bridge, dev])
    elif mode == MODE_OPENVSWITCH:
        run(ON_ERROR_LOG, ["ovs-vsctl", "--timeout=30", "--", "--if-exists", "del-port", dev])


def call_hook_script(action, vif_uuid, vm_uuid):
    if os.path.exists(vif_hotplug):
        send_to_syslog("Calling VIF hotplug hook for VM %s, VIF %s" % (vm_uuid, vif_uuid))
        run(ON_ERROR_LOG, [vif_hotplug, "-action", action, "-vifuuid", vif_uuid, "-vmuuid", vm_uuid])

## Read xenopsd internal VIF metadata
import json
class VIF:
    def __init__(self, vm_uuid, devid):
        self.vm_uuid = vm_uuid
        self.devid = devid
        f = open("%s/VM/%s/vif.%d" % (xenops_path, vm_uuid, devid))
        try:
            self.json = json.loads(f.read())
        finally:
            f.close()
    def get_mode(self):
        x = MODE_BRIDGE
        if "network-backend" in self.json["extra_private_keys"]:
            b = self.json["extra_private_keys"]["network-backend"]
            if b == "openvswitch" or b == "vswitch":
                x = MODE_OPENVSWITCH
            elif b == "bridge":
                x = MODE_BRIDGE
            else:
                send_to_syslog("VIF %s/%s: unknown network-backend %s, assuming bridge" % (self.vm_uuid, self.devid))
        else:
            send_to_syslog("VIF %s/%d: no network-backend provided, assuming bridge" % (self.vm_uuid, self.devid))
        return x
    def get_bridge(self):
        network = self.json["backend"]
        if network[0] <> "Local":
            send_to_syslog("VIF %s/%d: don't support network backend %s" % (self.vm_uuid, self.devid, repr(network)))
            return None
        return network[1]
    def get_address(self):
        return "fe:ff:ff:ff:ff:ff"
    def get_ethtool(self):
        results = []
        for (k, v) in self.json["other_config"]:
            if k.startswith("ethtool-"):
                k = k[len("ethtool-"):]
                if v == "true" or v == "on":
                    results.append(k, True)
                elif v == "false" or v == "off":
                    results.append(k, False)
                else:
                    send_to_syslog("VIF %s/%d: ignoring ethtool argument %s=%s (use true/false)" % (self.vm_uuid, self.devid, k, v))
        return results
    def get_mtu(self):
        return self.json["mtu"]
    def get_promiscuous(self):
        if "promiscuous" in self.json["other_config"]:
            x = self.json["other_config"]["promiscuous"]
            if x == "true" or x == "on":
                return True
        return False
    def get_external_ids(self):
        results = {}
        results["xs-vm-uuid"] = self.vm_uuid
        if "vif-uuid" in self.json["extra_private_keys"]:
            results["xs-vif-uuid"] = self.json["extra_private_keys"]["vif-uuid"]
        if "network-uuid" in self.json["extra_private_keys"]:
            results["xs-network-uuid"] = self.json["extra_private_keys"]["network-uuid"]
        results["attached-mac"] = self.json["mac"]
        return results


#def online(mode, dev, bridge, address, ethtool, mtu, promiscuous, external_ids):
#    for (key, value) in ethtool:
#        set_ethtool(mode, dev, key, value)
#    set_mtu(mode, dev, mtu)
#    add_to_bridge(mode, dev, bridge, address, external_ids)
#    set_promiscuous(mode, dev, promiscuous)

#def add(mode, dev, bridge, address, external_ids):
#    add_to_bridge(mode, dev, bridge, address, external_ids)


