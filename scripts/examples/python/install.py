#!/usr/bin/env python
# Copyright (c) 2006-2007 XenSource, Inc.
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


# Choose a Debian template at random and use it to install a VM on the
# Pool's default-SR. Add a VIF to the guest and wait until we have confirmed
# the reported OS type and IP address by monitoring the guest metrics.

import sys, time

import XenAPI, provision

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

    print "Choosing a template to clone"
    if templates == []:
        print "Could not find any Debian templates. Exitting"
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
    print "Adding noniteractive to the kernel commandline"
    session.xenapi.VM.set_PV_args(vm, "noninteractive")
    print "Choosing an SR to instaniate the VM's disks"
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
    
    def read_os_name(vm):
        vgm = session.xenapi.VM.get_guest_metrics(vm)
        try:
            os = session.xenapi.VM_guest_metrics.get_os_version(vgm)
            if "name" in os.keys():
                return os["name"]
            return None
        except:
            return None
    def read_ip_address(vm):
        vgm = session.xenapi.VM.get_guest_metrics(vm)
        try:
            os = session.xenapi.VM_guest_metrics.get_networks(vgm)
            if "0/ip" in os.keys():
                return os["0/ip"]
            return None
        except:
            return None

    while read_os_name(vm) == None: time.sleep(1)
    print "Reported OS name: ", read_os_name(vm)
    while read_ip_address(vm) == None: time.sleep(1)
    print "Reported IP: ", read_ip_address(vm)

    session.xenapi.session.logout()


if __name__ == "__main__":
    if len(sys.argv) <> 4:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)
    url = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]
    # First acquire a valid session by logging in:
    session = XenAPI.Session(url)
    session.xenapi.login_with_password(username, password)
    try:
        main(session)
    except Exception, e:
        print str(e)
        raise


