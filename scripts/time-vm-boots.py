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


# Simple python example to demonstrate the event system. Logs into the server,
# registers for events on the VM_guest_metrics and computes the time taken for
# the guest agent to report an IP address.

import XenAPI
import sys
import time

vgm_to_vm = {}


def register_vm_metrics(session, vm_ref, vgm):
    global vgm_to_vm

    try:
        # avoid putting invalid references in the cache
        tmp = session.xenapi.VM_guest_metrics.get_other(vgm)
        vgm_to_vm[vgm] = vm_ref
    except:
        pass


def vm_of_metrics(ref):
    global vgm_to_vm
    if not(ref in vgm_to_vm.keys()):
        return None
    return vgm_to_vm[ref]    

interesting_vms = []
vm_boot_times = {}
boots_seen = 0


def dump_table(session):
    global vm_boot_times
    for vm_ref in vm_boot_times.keys():
        name = session.xenapi.VM.get_name_label(vm_ref)
        print "%s %s" % (name, vm_boot_times[vm_ref])


def seen_possible_boot(session, vm_ref):
    global vm_boot_times
    global interesting_vms
    global boots_seen
    if not(vm_ref in vm_boot_times.keys()) and vm_ref in interesting_vms:
        t = time.strftime( "%Y%m%dT%H:%M:%SZ", time.gmtime())
        vm_boot_times[vm_ref] = t
        boots_seen += 1
        
        name = session.xenapi.VM.get_name_label(vm)        
        print >>sys.stdout, "%d %s %s" % (boots_seen, name, t)
        print >>sys.stderr, "%d %s %s" % (boots_seen, name, t)
        sys.stderr.flush()


def process_guest_metrics(session, ref, snapshot):
    if "other" in snapshot.keys():
        other = snapshot["other"]
        if "feature-shutdown" in other.keys():
            the_vm = vm_of_metrics(ref)
            seen_possible_boot(session, the_vm)


def poll_metrics(session):
    while True:
        time.sleep(10)
        all_recs = session.xenapi.VM_guest_metrics.get_all_records()
        for ref in all_recs.keys():
            snapshot = all_recs[ref]
            process_guest_metrics(session, ref, snapshot)


def process_metrics_event(session, ref):
    vm_ref = vm_of_metrics(ref)
    if vm_ref is None:
        return
    if session.xenapi.VM.get_power_state(vm_ref) != "Running":
        return
    other = {}
    try:
        other=session.xenapi.VM_guest_metrics.get_other(ref)
    except Exception, e:
        print repr(e)
        
    if "feature-shutdown" in other.keys():
        seen_possible_boot(session, vm_ref)
        

def watch_events_on_vm(session):
    try:
        token = ''
        call_timeout = 30.0
        while True:
            output = session.xenapi.event_from(["VM", "VM_guest_metrics"], token, call_timeout)
            events = output['events']
            token = output['token']

            for event in events:
                if event['operation'] == 'del':
                    continue
                if event['class'] == 'vm' and event['operation'] == 'mod':
                    register_vm_metrics(session, event['ref'], event['snapshot']['guest_metrics'])
                    continue
                if event['class'] == 'vm_guest_metrics':
                    process_metrics_event(session, event['ref'])
                    continue

    except XenAPI.Failure as e:
        print e.details
        sys.exit(1)
    finally:
        session.xenapi.session.logout()


if __name__ == "__main__":
    if len(sys.argv) > 4 or len(sys.argv) < 2:
        print """
Watches all offline VMs for boots
Usage:
    %s <url> <username> <password>
or
    %s [http://]localhost [<username>] [<password>]
""" % (sys.argv[0], sys.argv[0])
        sys.exit(1)

    url = sys.argv[1]
    username = sys.argv[2] if len(sys.argv) > 2 else ""
    password = sys.argv[3] if len(sys.argv) > 3 else ""

    if url == "http://localhost" or url == "localhost":
        new_session = XenAPI.xapi_local()
    else:
        new_session = XenAPI.Session(url)

    # First acquire a valid session by logging in
    try:
        new_session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-timevmboots.py")
    except XenAPI.Failure as f:
        print "Failed to acquire a session: %s" % f.details
        sys.exit(1)

    # We start watching all Halted VMs
    all_halted_vms = new_session.xenapi.VM.get_all_records()
    for vm in all_halted_vms.keys():
        vm_rec = all_halted_vms[vm]
        if vm_rec["power_state"] == "Halted" and not vm_rec["is_a_template"]:
            interesting_vms.append(vm)
    print >> sys.stderr, "Watching %d offline VMs" % (len(interesting_vms))

    watch_events_on_vm(new_session)
