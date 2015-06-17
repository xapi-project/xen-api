#!/usr/bin/python

import xmlrpclib
server = xmlrpclib.Server("http://melton:8086");
session = server.session.login_with_password("root", "xenroot", "1.0", "xen-api-list-vms.py")['Value']
print session
vms = server.VM.get_all(session)['Value']
print vms
#for vm in vms:
#    print vm,server.VM.get_kernel__kernel(session, vm)
