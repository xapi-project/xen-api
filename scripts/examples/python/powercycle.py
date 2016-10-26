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

# Enumerate all existing VMs, shut them down and do some aggressive power-cycling

import sys
import time

import XenAPI


def main(session):
    # Find a non-template VM object
    vms = session.xenapi.VM.get_all_records()
    print "Server has %d VM objects (this includes templates):" % (len(vms))

    for vm in vms:
        record = vms[vm]
        # We cannot power-cycle templates and we should avoid touching control domains
        # unless we are really sure of what we are doing...
        if not(record["is_a_template"]) and not(record["is_control_domain"]):
            name = record["name_label"]
            print "Found VM uuid %s called %s" % (record["uuid"], name)

            # Make sure the VM has powered down
            print "  VM '%s' is in power state '%s'" % (name, record["power_state"])
            if record["power_state"] == "Suspended":
                session.xenapi.VM.resume(vm, False, True) # start_paused = False; force = True
                session.xenapi.VM.clean_shutdown(vm)
            elif record["power_state"] == "Paused":
                session.xenapi.VM.unpause(vm)
                session.xenapi.VM.clean_shutdown(vm)
            elif record["power_state"] == "Running":
                session.xenapi.VM.clean_shutdown(vm)                
                
            # Power-cycle the VM a few times
            for i in range(1, 10):
                print "  beginning iteration %d" % i
                print "      restarting..."
                session.xenapi.VM.start(vm, False, True)  # start_paused = False; force = True
                while True:
                    rec = session.xenapi.VM.get_record(vm)
                    if 'suspend' in rec['allowed_operations']:
                        break
                    time.sleep(1)
                print "      suspending..."
                session.xenapi.VM.suspend(vm)
                print "      resuming..."
                session.xenapi.VM.resume(vm, False, True) # start_paused = False; force = True
                print "      shutting down..."
                session.xenapi.VM.clean_shutdown(vm)

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
        new_session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-powercycle.py")
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
