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


# Simple example to demonstrate how to use an aasynchronous operation,
# namely the asynchronous version of the VM start method.
# The example assumes the presence of a halted VM named "new", starts
# the VM, retrieves the task reference and queries the task's status.


import pprint
import time
import sys
import XenAPI


def main(session):

    print "Looking for a halted VM..."
    vms = session.xenapi.VM.get_all_records()
    vm = None
    for vm_ref in vms:
        vm_rec = vms[vm_ref]
        if not vm_rec['is_a_template'] and not vm_rec['is_control_domain']\
                and vm_rec["power_state"] == "Halted":
            print "Found it:"
            pprint.pprint(vm_rec)
            vm = vm_ref
            break
        
    if vm is None:
        print "Unable to find a halted VM"
        return

    print "Attempting to start the halted VM asynchronously"
    task = session.xenapi.Async.VM.start(vm, False, True)
    task_record = session.xenapi.task.get_record(task)

    print "The initial contents of the task record:"
    pprint.pprint(task_record)

    print "Waiting for the task to complete..."
    while session.xenapi.task.get_status(task) == "pending":
        time.sleep(1)

    task_record = session.xenapi.task.get_record(task)
    print "The final contents of the task record:"
    pprint.pprint(task_record)
    
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
        new_session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-vm-start-async.py")
    except XenAPI.Failure as f:
        print "Failed to acquire a session: %s" % f.details
        sys.exit(1)
    try:
        main(new_session)
    finally:
        new_session.xenapi.session.logout()
