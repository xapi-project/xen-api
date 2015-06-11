#!/usr/bin/python

import getopt, sys, xmlrpclib

url = "http://dhcp108:70000" #default
parsed = getopt.getopt(sys.argv[1:], "u:url")
if len(parsed[0]) == 1:
   url = parsed[0][0][1]

# Create an object to represent our server.
server = xmlrpclib.Server(url);

# Call the server and get our result.
print "Logging in... ",
session = server.Session.do_login_with_password("user", "passwd", "1.0", "xen-api-test-client.py")
print "OK"
print "Session ID: \""+session+"\""
vm_list = server.VM.do_list(session)

print "VM list = " + repr(vm_list)

for vm in vm_list:
    print "VM ", vm, " in state: ", server.VM.get_power_state(session, vm)

first_vm = vm_list[0]
other = server.VM.get_otherConfig(session, first_vm)
print repr(other)


#state = server.VM.get_power_state(session, first_vm)
#if state == "Halted":
#        print "Starting first VM... ",
#        server.VM.do_start(session, first_vm, 1==0)
#elif state == "Suspended":
#        print "Restoring first VM..."
#        server.VM.do_unhibernate(session, first_vm, 1==0)
#elif state == "Running":
#	print "Suspending first VM... ",
#	server.VM.do_hibernate(session, first_vm, 1==1)
#print "OK"

print "Logging out... ",
server.Session.do_logout(session)
print "OK"
