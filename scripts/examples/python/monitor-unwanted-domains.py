#!/usr/bin/env python

import os, subprocess, XenAPI, inventory, time, sys

# Script which monitors the domains running on a host, looks for
# paused domains which don't correspond to VMs which are running here
# or are about to run here, logs them and optionally destroys them.

# Return a list of (domid, uuid) tuples, one per paused domain on this host
def list_paused_domains():
    results = []
    all = subprocess.Popen(["@BINDIR@/list_domains"], stdout=subprocess.PIPE).communicate()[0]
    lines = all.split("\n")
    for domain in lines[1:]:
        bits = domain.split()
        if bits <> []:
            domid = bits[0]
            uuid = bits[2]
            state = bits[4]
            if 'P' in state:
                results.append( (domid, uuid) )
    return results

# Given localhost's uuid and a (domid, uuid) tuple, return True if the domain
# be somewhere else i.e. we think it may have leaked here
def should_domain_be_somewhere_else(localhost_uuid, (domid, uuid)):
    try:
        x = XenAPI.xapi_local()
        x.xenapi.login_with_password("root", "")
        try:
            try:
                vm = x.xenapi.VM.get_by_uuid(uuid)
                resident_on = x.xenapi.VM.get_resident_on(vm)
                current_operations = x.xenapi.VM.get_current_operations(vm)
                result = current_operations == {} and resident_on <> localhost_uuid
                if result:
                    log("domid %s uuid %s: is not being operated on and is not resident here" % (domid, uuid))
                    return result
            except XenAPI.Failure, e:
                if e.details[0] == "UUID_INVALID":
                    # VM is totally bogus
                    log("domid %s uuid %s: is not in the xapi database" % (domid, uuid))
                    return True
                # fail safe for now
                return False
        finally:
            x.xenapi.logout()
    except:
        return False

def log(str):
    print str

# Destroy the given domain
def destroy_domain((domid, uuid)):
    log("destroying domid %s uuid %s" % (domid, uuid))
    all = subprocess.Popen(["@OPTDIR@/debug/destroy_domain", "-domid", domid], stdout=subprocess.PIPE).communicate()[0]

# Keep track of when a domain first looked like it should be here
domain_first_noticed = {}

# Number of seconds after which we conclude that a domain really shouldn't be here
threshold = 60

if __name__ == "__main__":
    localhost_uuid = inventory.get_localhost_uuid ()
    while True:
        time.sleep(1)
        paused = list_paused_domains ()
        # GC the domain_first_noticed map
        for d in domain_first_noticed.keys():
            if d not in paused:
                log("domid %s uuid %s: looks ok now, forgetting about it" % d)
                del domain_first_noticed[d]

        for d in list_paused_domains():
            if should_domain_be_somewhere_else(localhost_uuid, d):
                if d not in domain_first_noticed:
                    domain_first_noticed[d] = time.time()
                noticed_for = time.time() - domain_first_noticed[d]
                if noticed_for > threshold:
                    log("domid %s uuid %s: has been in bad state for over threshold" % d)
                    if "-destroy" in sys.argv:
                        destroy_domain(d)


