#!/usr/bin/env python
# Copyright (c) 2008 XenSource, Inc.
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

# Allow the user to change the MAC address -> interface mapping

from __future__ import print_function
import XenAPI, inventory, sys

def warn(txt):
    print(txt, file=sys.stderr)

def show_pifs(pifs):
    print("NIC MAC               Notes")
    print("----------------------------------------------")
    for ref in pifs.keys():
        notes = []
        if pifs[ref]['management']:
            notes.append("management interface")
        nic = pifs[ref]['device'][3:]
        try:
            metrics = session.xenapi.PIF_metrics.get_record(session.xenapi.PIF.get_metrics(ref))
            if metrics['carrier']:
                notes.append("carrier detected")
            else:
                notes.append("no carrier detected")
        except:
            pass
                
        print("%3s %s %s" % (nic, pifs[ref]['MAC'], ", ".join(notes)))

def select(pifs, key):
    """Select a PIF by device name or MAC"""
    for ref in pifs.keys():
        if pifs[ref]['device'][3:] == key:
            return ref
        if pifs[ref]['MAC'].upper() == key.upper():
            return ref
    return None

def save(session, host, pifs):
    """Commit changes"""
    # Check that device names are unique
    devices = []
    for ref in pifs.keys():
        devices.append(pifs[ref]['device'][3:])
    for i in set(devices):
        devices.remove(i)
    if devices != []:
        print("ERROR: cannot assign two interfaces the same NIC number (%s)" % (", ".join(i)))
        print("Aborted.")
        sys.exit(1)
    vifs = []
    for ref in pifs.keys():
        net = pifs[ref]['network']
        for vif in session.xenapi.network.get_VIFs(net):
            if session.xenapi.VIF.get_currently_attached(vif):
                vifs.append(vif)
    if len(vifs) > 0:
        plural = ""
        if len(vifs) > 1:
            plural = "s"
        print("WARNING: this operation requires unplugging %d guest network interface%s" % (len(vifs), plural))
        print("Are you sure you want to continue? (yes/no) > ", end=' ')
        if sys.stdin.readline().strip().lower() != "yes":
            print("Aborted.")
            sys.exit(1)
    for vif in vifs:
        dev = session.xenapi.VIF.get_device(vif)
        vm = session.xenapi.VIF.get_VM(vif)
        uuid = session.xenapi.VM.get_uuid(vm)
        print("Hot-unplugging interface %s on VM %s" % (dev, uuid))
        session.xenapi.VIF.unplug(vif)
        
    for ref in pifs.keys():
        mac = pifs[ref]['MAC']        
        if pifs[ref]['management']:
            print("Disabling management NIC (%s)" % mac)
            session.xenapi.host.management_disable()
        session.xenapi.PIF.forget(ref)
    for ref in pifs.keys():
        mac = pifs[ref]['MAC']
        device = pifs[ref]['device']
        mode = pifs[ref]['ip_configuration_mode']
        IP = pifs[ref]['IP']
        netmask = pifs[ref]['IP']
        gateway = pifs[ref]['gateway']
        DNS = pifs[ref]['DNS']
        new_ref = session.xenapi.PIF.introduce(host, mac, device)
        session.xenapi.PIF.reconfigure_ip(new_ref, mode, IP, netmask, gateway, DNS)
        if pifs[ref]['management']:
            print("Re-enabling management NIC (%s)" % mac)
            session.xenapi.host.management_reconfigure(new_ref)

    for vif in vifs:
        dev = session.xenapi.VIF.get_device(vif)
        vm = session.xenapi.VIF.get_VM(vif)
        uuid = session.xenapi.VM.get_uuid(vm)
        print("Hot-plugging interface %s on VM %s" % (dev, uuid))
        session.xenapi.VIF.plug(vif)

def renameif(session):
    uuid = inventory.get_localhost_uuid ()
    host = session.xenapi.host.get_by_uuid(uuid)
    pool = session.xenapi.pool.get_all()[0]
    master = session.xenapi.pool.get_master(pool)
    if host != master:
        warn("This host is a slave; it is not possible to rename the management interface")

    pifs = session.xenapi.PIF.get_all_records()
    for ref in pifs.keys():
        if pifs[ref]['host'] != host or pifs[ref]['physical'] != True:
            del pifs[ref]
            
    while True:
        print("Current mappings:")
        show_pifs(pifs)
        print()
        print("Type 'quit' to quit; 'save' to save; or a NIC number or MAC address to edit")
        print("> ", end=' ')
        x = sys.stdin.readline().strip()
        if x.lower() == 'quit':
            sys.exit(0)
        if x.lower() == 'save':
            # If a slave, filter out the management PIF
            if host != master:
                for ref in pifs.keys():
                    if pifs[ref]['management']:
                        del pifs[ref]
            save(session, host, pifs)
            sys.exit(0)
        pif = select(pifs, x)
        if pif != None:
            # Make sure this is not a slave's management PIF
            if host != master and pifs[pif]['management']:
                print("ERROR: cannot modify the management interface of a slave.")
            else:
                print("Selected NIC with MAC '%s'. Enter new NIC number:" % pifs[pif]['MAC'])
                print("> ", end=' ')
                nic = sys.stdin.readline().strip()
                if not(nic.isdigit()):
                    print("ERROR: must enter a number (e.g. 0, 1, 2, 3, ...)")
                else:
                    pifs[pif]['device'] = "eth" + nic
        else:
            print("NIC '%s' not found" % (x))
        print()
        

if __name__ == "__main__":
    session = XenAPI.xapi_local()
    session.login_with_password("", "", "1.0", "xen-api-scripts-renameifs.py")
    try:
        renameif(session)
    finally:
        session.logout()
        

