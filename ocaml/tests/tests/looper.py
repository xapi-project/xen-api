#!/usr/bin/python

print "Program attempts to log into an XAPI server, fetch a list of VMs and"
print "then calls VM.get_otherConfig on the first one in a loop"
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
session = server.Session.login_with_password("user", "passwd", "1.0", "xen-api-tests-looper")
print "OK"
print "Session ID: \""+session+"\""
vm_list = server.VM.get_all(session)

print "VM list = " + repr(vm_list)

for vm in vm_list:
    print "VM ", vm, " in state: ", server.VM.get_power_state(session, vm)

first_vm = vm_list[0]

print "Getting the otherConfig of " + first_vm

attempt = 0
last = server.VM.get_otherConfig(session, first_vm)
while 1:
	this = server.VM.get_otherConfig(session, first_vm)
	if last <> this:
		print "Got a different response!"
		print "this = ", repr(this)
		print "last = ", repr(last)
		raise "Failed"
	attempt = attempt + 1
	print attempt
