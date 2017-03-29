#!/usr/bin/env python

# Copyright (c) Citrix Systems, Inc.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#   1) Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#   2) Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions and the following
#      disclaimer in the documentation and/or other materials
#      provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.


# The example chooses a Debian template at random and installs a VM from it,
# placing the disks on the Pool's default SR. Adds a VIF to the guest connecting
# it to a network on the host and boots the VM. The script waits until the guest
# agent has reported both the OS version and eth0's IP address by monitoring the
# guest metrics.


import sys
import time

import XenAPI
import provision


def main(session):

    # Choose the PIF with the alphabetically lowest device
    # (we assume that plugging the debian VIF into the same network will allow
    # it to get an IP address by DHCP)
    pifs = session.xenapi.PIF.get_all_records()
    lowest = None
    for pifRef in pifs.keys():
        if (lowest is None) or (pifs[pifRef]['device'] < pifs[lowest]['device']):
            lowest = pifRef
    print "Choosing PIF with device: ", pifs[lowest]['device']

    network = session.xenapi.PIF.get_network(lowest)
    print "Chosen PIF is connected to network: ", session.xenapi.network.get_name_label(network)

    # List all the VM objects
    vms = session.xenapi.VM.get_all_records()
    print "Server has %d VM objects (this includes templates):" % (len(vms))

    templates = []
    for vm in vms:
        record = vms[vm]
        ty = "VM"
        if record["is_a_template"]:
            ty = "Template"
            # Look for a debian template
            if record["name_label"].startswith("Debian"):
                templates.append(vm)
        print "  Found %8s with name_label = %s" % (ty, record["name_label"])

    print "Choosing a Debian template to clone"
    if not templates:
        print "Could not find any Debian templates. Exiting."
        sys.exit(1)

    template = templates[0]
    print "  Selected template: ", session.xenapi.VM.get_name_label(template)
    print "Installing new VM from the template"
    vm = session.xenapi.VM.clone(template, "new")
    print "  New VM has name: new"
    print "Creating VIF"
    vif = { 'device': '0',
            'network': network,
            'VM': vm,
            'MAC': "",
            'MTU': "1500",
            "qos_algorithm_type": "",
            "qos_algorithm_params": {},
            "other_config": {} }
    session.xenapi.VIF.create(vif)
    print "Adding non-interactive to the kernel commandline"
    session.xenapi.VM.set_PV_args(vm, "non-interactive")
    print "Choosing an SR to instantiate the VM's disks"
    pool = session.xenapi.pool.get_all()[0]
    default_sr = session.xenapi.pool.get_default_SR(pool)
    default_sr = session.xenapi.SR.get_record(default_sr)
    print "Choosing SR: %s (uuid %s)" % (default_sr['name_label'], default_sr['uuid'])
    print "Rewriting the disk provisioning XML"
    spec = provision.getProvisionSpec(session, vm)
    spec.setSR(default_sr['uuid'])
    provision.setProvisionSpec(session, vm, spec)
    print "Asking server to provision storage from the template specification"
    session.xenapi.VM.provision(vm)
    print "Starting VM"
    session.xenapi.VM.start(vm, False, True)
    print "  VM is booting"

    print "Waiting for the installation to complete"
    # Here we poll because we don't generate events for metrics objects currently
    
    def read_os_name(a_vm):
        vgm = session.xenapi.VM.get_guest_metrics(a_vm)
        try:
            os = session.xenapi.VM_guest_metrics.get_os_version(vgm)
            if "name" in os.keys():
                return os["name"]
            return None
        except:
            return None

    def read_ip_address(a_vm):
        vgm = session.xenapi.VM.get_guest_metrics(a_vm)
        try:
            os = session.xenapi.VM_guest_metrics.get_networks(vgm)
            if "0/ip" in os.keys():
                return os["0/ip"]
            return None
        except:
            return None

    while read_os_name(vm) is None:
        time.sleep(1)
    print "Reported OS name: ", read_os_name(vm)
    while read_ip_address(vm) is None:
        time.sleep(1)
    print "Reported IP: ", read_ip_address(vm)


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)
    url = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]
    # First acquire a valid session by logging in:
    new_session = XenAPI.Session(url)
    try:
        new_session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-install.py")
    except XenAPI.Failure as f:
        print "Failed to acquire a session: %s" % f.details
        sys.exit(1)
    try:
        main(new_session)
    except Exception, e:
        print str(e)
        raise
    finally:
        new_session.xenapi.session.logout()

