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


# This example takes a set of running VMs, finds the hosts on which the VMs are
# running, permutes the list of hosts and then simultaneously migrates each VM
# to the chosen new host. The program waits for all parallel migration tasks to
# complete before continuing.
#
# NB This script requires a fourth argument: an integer number of iterations.


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
