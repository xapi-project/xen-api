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
import subprocess

path = "/sbin:/usr/sbin:/bin:/usr/bin:" + os.path.dirname(sys.argv[0])
vif_hotplug = "/etc/xapi.d/vif-hotplug"
xs_read = "/usr/bin/xenstore-read"
xenops_base_path = "/var/run/nonpersistent/xenopsd"
xenops_path = None

command_name = sys.argv[0]
short_command_name = os.path.basename(command_name)

def set_xenopsd_backend(b):
    global xenops_path
    xenops_path = os.path.join(xenops_base_path, b)

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

MODE_OPENVSWITCH = "openvswitch"
MODE_BRIDGE      = "bridge"

def set_promiscuous(mode, dev, value):
    if mode == MODE_OPENVSWITCH and value:
        send_to_syslog("%s: Promiscuous ports are not supported via Open vSwitch")
    elif mode == MODE_BRIDGE:
        promisc = "/sys/class/net/%s/brport/promisc" % dev
        if os.path.exists(promisc):
            f = open("/sys/class/net/%s/brport/promisc" % dev)
            try:
                x = "0"
                if value:
                    x = "1"
                f.write(x)
            finally:
                f.close()
        else:
            send_to_syslog("cannot set promiscuous mode: %s doesn't exist" % promisc)

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

def add_vif_rules(name):
    # ideally we would convert setup-vif-rules into a shared library and call
    # the functions directly
    type = name[0:3]
    rest = name[3:]
    domid, devid = rest.split(".")
    run(ON_ERROR_FAIL, ["setup-vif-rules", type, domid, devid, "filter"])     

## Read xenopsd internal VIF metadata
try:
	import json
except ImportError:
	import simplejson as json

class VIF:
    def __init__(self, vif_name, vm_uuid, devid):
        self.vif_name = vif_name
        self.vm_uuid = vm_uuid
        self.devid = devid
        f = open("%s/VM/%s/vif.%d" % (xenops_path, vm_uuid, devid))
        try:
            self.json = json.loads(f.read())
        finally:
            f.close()
    def get_mode(self):
        x = MODE_BRIDGE
        if(os.path.exists("/sys/module/openvswitch")):
            x = MODE_OPENVSWITCH
        send_to_syslog("common.py: Detected network backend as: '%s'" % x)
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
    def get_mac(self):
        return self.json["mac"]
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
        # if this is a migration target VM, we have to use final-uuid as value of `xs-vm-uuid`
        rc, stdout, _ = doexec([xs_read, '/vm/%s/final-uuid' % self.vm_uuid])
        if rc == 0:
            vm_uuid = stdout.readline().strip()
            send_to_syslog("This is a migration target VM, so fix value of 'xs-vm-uuid' from %s to %s" % (self.vm_uuid, vm_uuid))
        else:
            vm_uuid = self.vm_uuid
        results["xs-vm-uuid"] = vm_uuid
        if "vif-uuid" in self.json["extra_private_keys"]:
            results["xs-vif-uuid"] = self.json["extra_private_keys"]["vif-uuid"]
        if "network-uuid" in self.json["extra_private_keys"]:
            results["xs-network-uuid"] = self.json["extra_private_keys"]["network-uuid"]
        results["attached-mac"] = self.get_mac()
        return results
    def get_locking_mode(self):
        def get_words(value, separator):
            if string.strip(value) == "":
                return []
            else:
               return string.split(value, separator)
        results = {
            "mac": self.get_mac(),
            "locking_mode": "",
            "ipv4_allowed": [],
            "ipv6_allowed": []
        }
        private = self.json["extra_private_keys"]
        if "locking_mode" in self.json:
            if type(self.json["locking_mode"]) is list:
		# Must be type=locked here
                results["locking_mode"] = self.json["locking_mode"][0].lower()
		locked_params=self.json["locking_mode"][1]
		results["ipv4_allowed"] = locked_params["ipv4"]
		results["ipv6_allowed"] = locked_params["ipv6"]
            else:
                results["locking_mode"] = self.json["locking_mode"].lower()
        send_to_syslog("Got locking config: %s" % (repr(results)))
        return results

class Interface:
    def __init__(self, vif_name, uuid, devid):
        self.uuid = uuid
        self.devid = int(devid)
        self.vif = VIF(vif_name, uuid, int(devid))
    def get_vif(self):
        return self.vif
    def online(self):
        v = self.get_vif()
        mode = v.get_mode()
        for (key, value) in v.get_ethtool():
            set_ethtool(mode, self.name, key, value)
        set_mtu(mode, self.name, v.get_mtu())
        add_to_bridge(mode, self.name, v.get_bridge(), v.get_address(), v.get_external_ids())
        add_vif_rules(self.name)
        set_promiscuous(mode, self.name, v.get_promiscuous())

#def add(mode, dev, bridge, address, external_ids):
#    add_to_bridge(mode, dev, bridge, address, external_ids)


