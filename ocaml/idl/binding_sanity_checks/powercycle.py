#!/usr/bin/env python

import XenAPI
import provision
import sanitychecklib
import random

session=sanitychecklib.getsession()
sx=session.xenapi

vms = sx.VM.get_all()

sanity_check_vm_list = [ x for x in vms if (sanitychecklib.test_vm_name in sx.VM.get_name_label(x)) ]

print "Existing sanity check VMs"
print [sx.VM.get_name_label(x) for x in sanity_check_vm_list]

for svm in sanity_check_vm_list:
    power_state=sx.VM.get_power_state(svm)
    print power_state
    
    if (power_state=="Running"):
        if(random.choice(("heads","tails"))=="heads"):
              print "shutting down...."
              sx.VM.clean_shutdown(svm)
        else:
              print "suspending....."
              sx.VM.suspend(svm)                    
      
    elif (power_state=="Suspended"):
        print "resuming...."
        sx.VM.resume(svm, False, False)

    elif(power_state=="Halted"):
      print "starting...."
      sx.VM.start(svm, False, False)        
        
power_state=sx.VM.get_power_state(svm)
print power_state

print "logging out"
session.logout()

















