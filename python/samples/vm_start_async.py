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

# Simple example using the asynchronous version of the VM start method
# Assumes the presence of a halted VM

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
