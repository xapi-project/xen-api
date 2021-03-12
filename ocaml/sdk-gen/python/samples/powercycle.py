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


# This example lists all existing VMs, shuts them down and then performs some
# aggressive power-cycling on each one


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
