#!/usr/bin/env python3

# Receive multiple VMs
# Issue parallel loops of: reboot, suspend/resume, migrate

from __future__ import print_function
import xmlrpc.client
from threading import Thread
import time, sys

iso8601 = "%Y%m%dT%H:%M:%SZ"

stop_on_first_failure = True
stop = False

class Operation:
    def __init__(self):
        raise NotImplementedError
    def execute(self, server, session_id):
        raise NotImplementedError

class Reboot(Operation):
    def __init__(self, vm):
        self.vm = vm
    def execute(self, server, session_id):
        return server.VM.clean_reboot(session_id, self.vm)
    def __str__(self):
        return "clean_reboot(%s)" % self.vm

class SuspendResume(Operation):
    def __init__(self, vm):
        self.vm = vm
    def execute(self, server, session_id):
        x = { "ErrorDescription": [ "VM_MISSING_PV_DRIVERS" ] }
        while "ErrorDescription" in x and x["ErrorDescription"][0] == "VM_MISSING_PV_DRIVERS":
            x = server.VM.suspend(session_id, self.vm)
            if "ErrorDescription" in x:
                time.sleep(1)
        if x["Status"] != "Success":
            return x
        return server.VM.resume(session_id, self.vm, False, False)
    def __str__(self):
        return "suspendresume(%s)" % self.vm

class ShutdownStart(Operation):
    def __init__(self, vm):
        self.vm = vm
    def execute(self, server, session_id):
        x = server.VM.clean_shutdown(session_id, self.vm)
        if x["Status"] != "Success":
            return x
        return server.VM.start(session_id, self.vm, False, False)
        #return { "Status": "bad", "ErrorDescription": "foo" }
    def __str__(self):
        return "shutdownstart(%s)" % self.vm

class LocalhostMigrate(Operation):
    def __init__(self, vm):
        self.vm = vm
    def execute(self, server, session_id):
        return server.VM.pool_migrate(session_id, self.vm, server.VM.get_resident_on(session_id, self.vm)["Value"], { "live": "true" } )
    def __str__(self):
        return "localhostmigrate(%s)" % self.vm

# Use this to give each thread a different ID
worker_count = 0

class Worker(Thread):
    def __init__(self, server, session_id, operations):
        Thread.__init__(self)
        self.server = server
        self.session_id = session_id
        self.operations = operations
        self.num_successes = 0
        self.num_failures = 0
        global worker_count
        self.id = worker_count
        worker_count = worker_count + 1
    def run(self):
        global iso8601
        global stop_on_first_failure, stop
        for op in self.operations:
            description = str(op)

            if stop:
                return
            
            start = time.strftime(iso8601, time.gmtime(time.time ()))
            result = op.execute(self.server, self.session_id)
            end = time.strftime(iso8601, time.gmtime(time.time ()))

            if result["Status"] == "Success":
                print("SUCCESS %d %s %s %s" % (self.id, start, end, description))
                self.num_successes = self.num_successes + 1
            else:
                error_descr = result["ErrorDescription"]
                print("FAILURE %d %s %s %s %s" % (self.id, start, end, error_descr[0], description))
                self.num_failures = self.num_failures + 1
                if stop_on_first_failure:
                    stop = True

def make_operation_list(vm):
    return [ Reboot(vm), SuspendResume(vm), LocalhostMigrate(vm) ] * 100

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage:")
        print("  %s <URL> <other-config key>" % (sys.argv[0]))
        print("  -- performs parallel operations on VMs with the specified other-config key")
        sys.exit(1)
    
    x = xmlrpc.client.ServerProxy(sys.argv[1])
    key = sys.argv[2]
    session = x.session.login_with_password("root", "xenroot", "1.0", "xen-api-scripts-minixenrt.py")["Value"]
    vms = x.VM.get_all_records(session)["Value"]

    workers = []
    for vm in vms.keys():
        if key in vms[vm]["other_config"]:
            allowed_ops = vms[vm]["allowed_operations"]
            for op in [ "clean_reboot", "suspend", "pool_migrate" ]:
                if op not in allowed_ops:
                    raise RuntimeError("VM %s is not in a state where it can %s" % (vms[vm]["name_label"], op))
            workers.append(Worker(x, session, make_operation_list(vm)))
    for w in workers:
        w.start()
    for w in workers:
        w.join()
    successes = 0
    failures = 0
    for w in workers:
        successes = successes + w.num_successes
        failures = failures + w.num_failures
    print("Total successes = %d" % successes)
    print("Total failures = %d" % failures)
    if failures == 0:
        print("PASS")
        sys.exit(0)
    else:
        print("FAIL")
        sys.exit(1)  
