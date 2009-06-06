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
# Assumes the presence of a VM called 'new'
import pprint, time, sys

import XenAPI

def main(session):

    print "Listing all VM references:"
    vms = session.xenapi.VM.get_all()
    pprint.pprint(vms)

    print "Dumping all VM records:"
    for vm in vms:
        pprint.pprint(session.xenapi.VM.get_record(vm))
        
    print "Attempting to start a VM called 'new' (if it doesn't exist this will throw an exception)"
    vm = session.xenapi.VM.get_by_name_label('new')[0]
    session.xenapi.VM.start(vm, False, True)

    print "Attempting to start the VM asynchronously"
    task = session.xenapi.Async.VM.start(vm, False, True)
    task_record = session.xenapi.task.get_record(task)

    print "The initial contents of the task record:"
    pprint.pprint(task_record)
    print "Waiting for the task to complete"
    while session.xenapi.task.get_status(task) == "pending": time.sleep(1)

    task_record = session.xenapi.task.get_record(task)
    print "The final contents of the task record:"
    pprint.pprint(task_record)
    
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
    main(session)
 
