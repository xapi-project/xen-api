#!/usr/bin/env python

import XenAPI
import sanitychecklib
import sys

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

#log in to the slave
print "logging in to ",sanitychecklib.server
session=sanitychecklib.getsession()
sx=session.xenapi

#Work out which of the hosts in the pool are alive
#FIXME:
#If we get a host-is-slave error, we should be able to retry on the real master, but there seems to be a 
#bug in the bindings which prevents the error getting back. 

hosts=sx.host.get_all()
hosts_with_status=[(sx.host.get_name_label(x),sx.host_metrics.get_live( sx.host.get_metrics(x) )) for x in hosts]

live_hosts=[name for (name,status) in hosts_with_status if (status==True)]
dead_hosts=[name for (name,status) in hosts_with_status if not (status==True)]

print "live hosts",
print  live_hosts,
print "dead hosts",
print dead_hosts


#log out
if logout_after_test:
    print "logging out"
    session.logout()

print "End of------", this_test_name
