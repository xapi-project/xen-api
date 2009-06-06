#!/usr/bin/python

print "Program attempts to log into an XAPI server to fetch a list of VMs and"
print "a list of debug objects. It then chooses the first debug object, "
print "queries the int->float map and then calls the 'recycle' message using"
print "that map as an argument"
print

import getopt, sys, xapi

url = "http://localhost:8086" #default
parsed = getopt.getopt(sys.argv[1:], "u:url")
if len(parsed[0]) == 1:
   url = parsed[0][0][1]

print "Connecting to server on URL: ", url
print "(change with -u argument)"

# Create an object to represent our server.
server = xapi.Server(url);

# Call the server and get our result.
print "Logging in... ",
session = server.Session.login_with_password("user", "passwd")
print "OK"
print "Session ID: \""+session+"\""
vm_list = server.VM.get_all(session)

print "VM list = " + repr(vm_list)

for vm in vm_list:
    print "VM ", vm, " in state: ", server.VM.get_power_state(session, vm)

first_vm = vm_list[0]

debug_objs = server.Debug.get_all(session)
debug = debug_objs[0]
ifm = server.Debug.get_int_float_map(session, debug)
print "Got an int->float map: " + repr(ifm)

print "doing the int_float_map recycle thing"

attempt = 0
while 1:
	this = server.Debug.recycle_int_float_map(ifm)
	if ifm <> this:
		print "Got a different response!"
		print "this = ", repr(this)
		print "ifm  = ", repr(ifm)
		raise "Failed"
	attempt = attempt + 1
	print attempt
