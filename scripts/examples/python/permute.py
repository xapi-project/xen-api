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

# Take all running VMS, their resident_on hosts, permute them and migrate
# all VMS simultaneously. Repeat N times.

import sys
import time

import XenAPI


def main(session, iteration):
    # Find a non-template VM object
    all_vms = session.xenapi.VM.get_all_records()
    vms = []
    hosts = []
    for vm in all_vms:
        record = all_vms[vm]
        if not(record["is_a_template"]) and not(record["is_control_domain"]) and record["power_state"] == "Running":
            vms.append(vm)
            hosts.append(record["resident_on"])
    print "%d: Found %d suitable running VMs" % (iteration, len(vms))
    # use a rotation as a permutation
    hosts = [hosts[-1]] + hosts[:(len(hosts)-1)]

    tasks = []
    for j in range(0, len(vms)):
        vm = vms[j]
        host = hosts[j]
        task = session.xenapi.Async.VM.pool_migrate(vm, host, { "live": "true" })
        tasks.append(task)
    finished = False
    records = {}
    while not finished:
        finished = True
        for task in tasks:
            record = session.xenapi.task.get_record(task)
            records[task] = record
            if record["status"] == "pending":
                finished = False
        time.sleep(1)
    allok = True
    for task in tasks:
        record = records[task]
        if record["status"] != "success":
            allok = False
    if not allok:
        print "One of the tasks didn't succeed at", time.strftime("%F:%HT%M:%SZ", time.gmtime())
        idx = 0
        for task in tasks:
            record = records[task]
            vm_name = session.xenapi.VM.get_name_label(vms[idx])
            host_name = session.xenapi.host.get_name_label(hosts[idx])
            print "%s : %12s %s -> %s [ status: %s; result = %s; error = %s ]" % (record["uuid"], record["name_label"], vm_name, host_name, record["status"], record["result"], repr(record["error_info"]))
            idx += 1
        raise XenAPI.Failure("Task failed")
    else:
        for task in tasks:
            session.xenapi.task.destroy(task)

if __name__ == "__main__":
    if len(sys.argv) != 5:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password> <iterations>"
        sys.exit(1)
    url = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]
    iterations = int(sys.argv[4])
    # First acquire a valid session by logging in:
    new_session = XenAPI.Session(url)
    try:
        new_session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-permute.py")
    except XenAPI.Failure as f:
        print "Failed to acquire a session: %s" % f.details
        sys.exit(1)

    try:
        for i in range(iterations):
            main(new_session, i)
    except XenAPI.Failure as e:
        print e.details
        sys.exit(1)
    finally:
        new_session.xenapi.session.logout()
