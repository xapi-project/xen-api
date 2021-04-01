#!/usr/bin/env python
# Copyright (c) Citrix Systems.
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

# Prepare to shutdown a host (assumed to exist) by:
# 1. attempting to evacuate running VMs to other hosts
# 2. remaining VMs are shutdown cleanly
# 3. remaining VMs are shutdown forcibly
#
# Step (1) and (2) can be skipped if the "force" option is specified.
# Clean shutdown attempts are run in parallel and are cancelled on
# timeout.

import sys, time
import signal
import syslog

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
        return False


def wait_for_tasks(session, tasks, timeout):
    """returns true if all tasks are no longer pending (ie success/failure/cancelled)
    and false if a timeout occurs"""
    finished = False
    start = time.time ()
    while not(finished) and ((time.time () - start) < timeout):
        finished = True
        for task in tasks:
            if task_is_pending(session, task):
                finished = False
        time.sleep(1)
    return finished


def get_running_domains(session, host):
    """Return a list of (vm, record) pairs for all VMs running on the given host"""
    vms = []
    for vm in session.xenapi.host.get_resident_VMs(host):
        record = session.xenapi.VM.get_record(vm)
        if not record["is_control_domain"] and record["power_state"] == "Running":
            if record['other_config'].has_key('auto_poweroff') and record['other_config'].get('auto_poweroff') == "false":
                print "\n  Skip running VM %s has self-managed power-off" % record["name_label"],
                sys.stdout.flush()
                continue
            vms.append((vm,record))
    return vms

def estimate_evacuate_timeout(session, host):
    """ Rough estimation of the evacuate uplimit based on live VMs memory """
    mref = session.xenapi.host.get_metrics(host)
    metrics = session.xenapi.host_metrics.get_record(mref)
    memory_used = int(metrics['memory_total']) - int(metrics['memory_free'])
    # Conservative estimation based on 1000Mbps link, and the memory usage of
    # Dom0 (which is not going to be transferred) is an intentional surplus
    return (memory_used * 8. / (1000. * 1024 * 1024))

def host_evacuate(session, host):
    """Attempts a host evacuate. If the timeout expires then it attempts to cancel
    any in-progress tasks it can find."""
    rc = 0
    print "\n  Requesting evacuation of host",
    sys.stdout.flush()
    task = session.xenapi.Async.host.evacuate(host)
    timeout = 240
    try:
        timeout = max(estimate_evacuate_timeout(session, host), timeout)
    except Exception, e:
        syslog.syslog(syslog.LOG_WARNING, "Evacuate timeout estimation error: %s, use default." % e)
    try:
        if not(wait_for_tasks(session, [ task ], timeout)):
            print "\n  Cancelling evacuation of host",
            sys.stdout.flush()
            session.xenapi.task.cancel(task)
            for vm, record in get_running_domains(session, host):
                current = record["current_operations"]
                for t in current.keys():
                    try:
                        print "\n  Cancelling operation on VM: %s" % record["name_label"],
                        sys.stdout.flush()
                        session.xenapi.task.cancel(t)
                    except:
                        print "Failed to cancel task: %s" % t,
                        sys.stdout.flush()
    finally:
        try:
            session.xenapi.task.destroy(task)
        except Exception:
            # db gc thread in xapi may delete task from tasks table
            print "\n Task %s has been destroyed" % task
            sys.stdout.flush()
        return rc


def parallel_clean_shutdown(session, vms):
    """Performs a parallel VM.clean_shutdown of all running VMs on a given host.
    If the timeout expires then any in-progress tasks are cancelled."""
    tasks = []
    rc = 0

    try:
        for vm,record in vms:
            if not "clean_shutdown" in record["allowed_operations"]:
                continue

            print "\n  Requesting clean shutdown of VM: %s" % (record["name_label"]),
            sys.stdout.flush()
            task = session.xenapi.Async.VM.clean_shutdown(vm)
            tasks.append((task,vm,record))

        if not tasks:
            return 0

        if not(wait_for_tasks(session, map(lambda x:x[0], tasks), 60)):
            # Cancel any remaining tasks.
            for (task,_,record) in tasks:
                try:
                    if task_is_pending(session, task):
                        print "\n  Cancelling clean shutdown of VM: %s" % (record["name_label"]),
                        sys.stdout.flush()
                        session.xenapi.task.cancel(task)
                except:
                    pass

        if not(wait_for_tasks(session, map(lambda x:x[0], tasks), 60)):
            for (_,vm,_) in tasks:
                if session.xenapi.VM.get_power_state(vm) == "Running":
                    rc += 1

    finally:
        for (task,_,_) in tasks:
            try:
                session.xenapi.task.destroy(task)
            except Exception:
                # db gc thread in xapi may delete task from tasks table
                print "\n Task %s has been destroyed" % task
                sys.stdout.flush()
        return rc


def serial_hard_shutdown(session, vms):
    """Performs a serial VM.hard_shutdown of all running VMs on a given host."""
    rc = 0
    try:
        for (vm,record) in vms:
            print "\n  Requesting hard shutdown of VM: %s" % (record["name_label"]),
            sys.stdout.flush()

            try:
                session.xenapi.VM.hard_shutdown(vm)
            except:
                print "\n  Failure performing hard shutdown of VM: %s" % (record["name_label"]),
                rc += 1
    finally:
        return rc


def main(session, host_uuid, force):
    rc = 0
    host = session.xenapi.host.get_by_uuid(host_uuid)

    if not force:
        # VMs which can't be evacuated should be shutdown first
        vms = []
        for vm in session.xenapi.host.get_vms_which_prevent_evacuation(host).keys():
            r = session.xenapi.VM.get_record(vm)

            # check for self-managed power off
            if 'auto_poweroff' in r['other_config'] and r['other_config'].get('auto_poweroff') == "false":
                print "\n  VM %s has self-managed power-off" % r["name_label"],
                sys.stdout.flush()
                continue

            print "\n  VM %s cannot be evacuated" % r["name_label"],
            sys.stdout.flush()
            vms.append((vm, r))
        rc += parallel_clean_shutdown(session, vms)
        vms_f = filter(lambda (vm, _): session.xenapi.VM.get_power_state(vm) == "Running", vms)

        # check for self-managed power off
        vms = []
        for (vm,record) in vms_f:
            if 'auto_poweroff' in record['other_config'] and record['other_config'].get('auto_poweroff') == "false":
                print "\n  VM %s has self-managed power-off" % record["name_label"],
                sys.stdout.flush()
                continue
            vms.append((vm, record))

        rc += serial_hard_shutdown(session, vms)

        # VMs which can be evacuated should be evacuated
        rc += host_evacuate(session, host)

        # Any remaining VMs should be shutdown
        rc += parallel_clean_shutdown(session, get_running_domains(session, host))
    else:
        rc += serial_hard_shutdown(session, get_running_domains(session, host))

    return rc

if __name__ == "__main__":
    if len(sys.argv) != 2 and len(sys.argv) != 3:
        print "Usage:"
        print sys.argv[0], "<host-uuid> [--force]"
        sys.exit(1)

    uuid = sys.argv[1]

    force = False
    if len(sys.argv) == 3 and sys.argv[2] == "--force":
        force = True

    new_session = XenAPI.xapi_local()

    connect_to_master_with_timeout = TimeoutFunction(new_session.xenapi.login_with_password, TIMEOUT_SECS)

    try:
        connect_to_master_with_timeout("root", "")
    except TimeoutFunctionException:
        print "Unable to connect to master within %d seconds. Exiting." % TIMEOUT_SECS
        sys.exit(1)
    except Exception:
        print 'Failed to connect to master.'
        sys.exit(2)

    try:
        rc = main(new_session, uuid, force)
        sys.exit(rc)
    except Exception, e:
        print "Caught %s" % str(e)
        sys.exit(1)
    finally:
        new_session.xenapi.session.logout()
