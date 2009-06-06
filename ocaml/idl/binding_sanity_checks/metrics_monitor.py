#!/usr/bin/env python

import XenAPI
import sanitychecklib
import time, sys
from pprint import pprint

#Generally, we wish to announce the name of this file.
#When running in the interpreter, however, this doesn't exist
try:
    this_test_name = __file__
except NameError:
    this_test_name = "unknown"

print "------------", this_test_name

#log in to the master
print "logging in to ",sanitychecklib.server
session=sanitychecklib.getsession()
sx=session.xenapi

def get_vm_metrics_dictionary(vm):
    dict={}
    dict["name"]=sx.VM.get_name_label(vm)
    dict["cpus"]=sx.VM_metrics.get_record(sx.VM.get_metrics(vm))['VCPUs_utilisation']
    return dict

def get_host_data(host):
    return (sx.host.get_name_label(host), [get_vm_metrics_dictionary(vm) for vm in sx.VM.get_all() if sx.VM.get_resident_on(vm)==host])

# Every second or so, print out the metrics    
# For, the time, we just print POSIX time. Contemplation of the expression
# (datetime.datetime.fromtimestamp(float((time.time().__str__())))).isoformat()
# can be enlightening
while True:
    print time.time(),
    pprint([get_host_data(host) for host in sx.host.get_all()])
    sys.stdout.flush()
    time.sleep(1)
