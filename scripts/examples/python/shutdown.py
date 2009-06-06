#!/usr/bin/env python
# Copyright (c) 2006-2008 Citrix Systems.
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

# Take all running VMS on a host, if force is not passed then those
# which advertise a clean_shutdown capability are clean_shutdown'ed
# and waited for on a timeout.
#
# If force is passed than all VMS are hard_shutdown'ed.

import sys, time
import signal

import XenAPI

TIMEOUT_SECS=30

# class for determining whether xe is responsive
class TimeoutFunctionException(Exception):
    """Exception to raise on a timeout"""
    pass

class TimeoutFunction:

    def __init__(self, function, timeout):
        self.timeout = timeout
        self.function = function

    def handle_timeout(self, signum, frame):
        raise TimeoutFunctionException()

    def __call__(self, *args):
        old = signal.signal(signal.SIGALRM, self.handle_timeout)
        signal.alarm(self.timeout)
        try:
            result = self.function(*args)
        finally:
            signal.signal(signal.SIGALRM, old)
        signal.alarm(0)
        return result


def task_is_pending(session, task):
    try:
        return session.xenapi.task.get_status(task) == "pending"
    except:
        return false

# returns true if all tasks are nolonger pending (ie success/failure/cancelled)
# and false if a timeout occurs
def wait_for_tasks(session, tasks, timeout):
    finished = False
    start = time.time ()
    while not(finished) and ((time.time () - start) < timeout):
        finished = True
        for (task,_) in tasks:
            if task_is_pending(session, task):
                finished = False
        time.sleep(1)        
    return finished

def get_running_domains(session, host):
    vms = []
    for vm in session.xenapi.host.get_resident_VMs(host):
        record = session.xenapi.VM.get_record(vm)
        if not(record["is_control_domain"]) and record["power_state"] == "Running":
            vms.append((vm,record))
    return vms

def main(session, host_uuid, force):
    rc = 0
    host = session.xenapi.host.get_by_uuid(host_uuid)

    if not force:

        tasks = []
        
        try:
            for vm,record in get_running_domains(session, host):
                if not "clean_shutdown" in record["allowed_operations"]:
                    continue
                
                print "\n  * %s" % (record["name_label"]),
                task = session.xenapi.Async.VM.clean_shutdown(vm)
                tasks.append((task,vm))

            if tasks == []:
                return 0

            if not(wait_for_tasks(session, tasks, 60)):
                # Cancel any remaining tasks.
                for (task,_) in tasks:
                    try:
                        if task_is_pending(session, task):
                            session.xenapi.task.cancel(task)
                    except:
                        pass

            if not(wait_for_tasks(session, tasks, 60)):
                for (_,vm) in tasks:
                    if session.xenapi.VM.get_power_state(vm) == "Running":
                        rc = rc + 1
                        
        finally:
            for (task,_) in tasks:
                session.xenapi.task.destroy(task)
    else:
        for (vm,record) in get_running_domains(session, host):
            print "\n  * %s" % (record["name_label"]),
        
            try:
                session.xenapi.VM.hard_shutdown(vm)
            except:
                rc = rc + 1

    return rc

if __name__ == "__main__":
    if len(sys.argv) <> 2 and len(sys.argv) <> 3:
        print "Usage:"
        print sys.argv[0], " [--force] <host uuid>"

        sys.exit(1)

    force = False
    if sys.argv[1] == "--force":
        force = True
        uuid = sys.argv[2]
    else:
        uuid = sys.argv[1]

    session = XenAPI.xapi_local()

    connect_to_master_with_timeout = TimeoutFunction(session.xenapi.login_with_password, TIMEOUT_SECS)

    try:
        connect_to_master_with_timeout("root", "")
    except TimeoutFunctionException:
        print "Unable to connect to master within %d seconds. Exiting." % (TIMEOUT_SECS)
        sys.exit(1)
    except Exception:
        print 'Failed to connect to master.'
        sys.exit(2)
        
    try:
        rc = main(session, uuid, force)
    finally:
        session.xenapi.session.logout()

    sys.exit(rc)

