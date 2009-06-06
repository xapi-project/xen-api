#!/usr/bin/env python

import XenAPI
import sanitychecklib

#Generally, we wish to announce the name of this file.
#When running in the interpreter, however, this doesn't exist, and we
#probably shouldn't log out either
try:
    this_test_name = __file__
    logout_after_test = True
except NameError:
    this_test_name = "unknown"
    logout_after_test = False

print "------------", this_test_name

#log in to the master
print "logging in to ",sanitychecklib.server
session=sanitychecklib.getsession()
sx=session.xenapi

#find all VMs which claim to be resident on machines other that the primary server
vms_with_residency=[(x,sx.VM.get_resident_on(x)) for x in sx.VM.get_all()]
resident_vms=[(a,b) for a,b in vms_with_residency if not "NULL" in b]
resident_vms_with_hostnames=[(a,sx.host.get_name_label(b)) for (a,b) in resident_vms]
secondary_server_vms=[ a for a,b in resident_vms_with_hostnames if b!=sanitychecklib.server]


print "VMs resident on hosts other than", sanitychecklib.server

for x in secondary_server_vms:
    name=sx.VM.get_name_label(x)
    print name, 
    if sx.VM.get_is_control_domain(x):
        print "(  can't move the control domain )"
    else:
        power_state=sx.VM.get_power_state(x)
        print "  (",power_state,")"
        
        if (power_state== "Paused"):
            print "Vm is paused, and so can't be migrated. Unpausing ..."
            sx.VM.unpause(x)
            print "  ",sx.VM.get_power_state(x)
            
        print "  attempting to migrate to ", sanitychecklib.server
	primary_host=sx.host.get_by_name_label(sanitychecklib.server)[0]
        sx.VM.pool_migrate(x,primary_host,{})

#log out
if logout_after_test:
    print "logging out"
    session.logout()

print "End of------", this_test_name
