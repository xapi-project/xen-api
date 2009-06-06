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


#code goes here

#log out
if logout_after_test:
    print "logging out"
    session.logout()

print "End of------", this_test_name
