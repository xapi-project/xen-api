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

import XenAPI, sys, time, inventory

vgm_to_vm = {}

def register_vm_metrics(session, vm_ref, vgm):
    global vgm_to_vm

    try:
	# avoid putting invalid references in the cache
	tmp = session.xenapi.VM_guest_metrics.get_other(vgm)
    	vgm_to_vm[vgm] = vm_ref
    except:
	pass

def vm_of_metrics(session, ref):
    global vgm_to_vm
    if not(ref in vgm_to_vm.keys()):
	return None
    return vgm_to_vm[ref]    

interesting_vms = []
vm_boot_times = {}
boots_seen = 0

def dump_table(session):
    global vm_boot_times
    for vm in vm_boot_times.keys():
        name = session.xenapi.VM.get_name_label(vm)        
        print "%s %s" % (name, vm_boot_times[vm])
        
def seen_possible_boot(session, vm):
    global vm_boot_times
    global interesting_vms
    global boots_seen
    if not(vm in vm_boot_times.keys()) and vm in interesting_vms:
        t = time.strftime( "%Y%m%dT%H:%M:%SZ", time.gmtime())
        vm_boot_times[vm] = t
        boots_seen = boots_seen + 1
        
        name = session.xenapi.VM.get_name_label(vm)        
        print >>sys.stdout, "%d %s %s" % (boots_seen, name, t)
        print >>sys.stderr, "%d %s %s" % (boots_seen, name, t)
        sys.stderr.flush()

def process_guest_metrics(session, ref, snapshot):
    
    if "other" in snapshot.keys():
        other = snapshot["other"]
        if "feature-shutdown" in other.keys():
            vm = vm_of_metrics(session, ref)
            seen_possible_boot(session, vm)    

def poll_metrics(session):
    while True:
        time.sleep(10)
        all = session.xenapi.VM_guest_metrics.get_all_records()
        for ref in all.keys():
            snapshot = all[ref]
            process_guest_metrics(session, ref, snapshot)

def process_metrics_event(session, ref, snapshot):
    vm_ref = vm_of_metrics(session, ref)
    if vm_ref == None:
	return
    if session.xenapi.VM.get_power_state(vm_ref) <> "Running":
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
        # Register for events on all classes:
        session.xenapi.event.register(["VM","VM_guest_metrics"])
        while True:
            try:
                events = session.xenapi.event.next()
                for event in events:
                    if event['operation'] == 'del':
                        continue
		    if event['class'] == 'vm' and event['operation'] == 'mod':
			register_vm_metrics(session, event['ref'], event['snapshot']['guest_metrics'])
			continue
		    if event['class'] == 'vm_guest_metrics':
			process_metrics_event(session, event['ref'], event['snapshot'])
			continue

            except XenAPI.Failure, e:
                if e.details <> [ "EVENTS_LOST" ]: raise
                print "** Caught EVENTS_LOST error: some events may be lost"
                # Check for the "EVENTS_LOST" error (happens if the event queue fills up on the
                # server and some events have been lost). The only thing we can do is to
                # unregister and then re-register again for future events.
                # NB: A program which is waiting for a particular condition to become true would
                # need to explicitly poll the state to make sure the condition hasn't become
                # true in the gap.
                session.xenapi.event.unregister(["VM", "VM_guest_metrics"])
                session.xenapi.event.register(["VM", "VM_guest_metrics"])
    finally:
        session.xenapi.session.logout()
        

if __name__ == "__main__":
    if len(sys.argv) <> 1:
        print "Usage:"
        print sys.argv[0]
        print "  -- watches all offline VMs for boots"
        sys.exit(1)
    # First acquire a valid session by logging in:
    session = XenAPI.xapi_local()
    session.xenapi.login_with_password("", "", "1.0", "xen-api-scripts-timevmboots.py")

    # We start watching all Halted VMs
    all = session.xenapi.VM.get_all_records()
    for vm in all.keys():
        vm_rec = all[vm]
        if vm_rec["power_state"] == "Halted" and vm_rec["is_a_template"] == False:
            interesting_vms.append(vm)
    print >> sys.stderr, "Watching %d offline VMs" % (len(interesting_vms))

    #poll_metrics(session)
    watch_events_on_vm(session)
