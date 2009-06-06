#!/usr/bin/env python

import XenAPI
import sanitychecklib

#log in to the server which is to become a slave
try:

  print "logging in to ",sanitychecklib.secondaryserver
  session=sanitychecklib.getsecondarysession()
  sx=session.xenapi

  try:
    #tell it to join the other server's pool
    print "asking ", sanitychecklib.secondaryserver, "to join a pool with ", sanitychecklib.server
    sx.pool.join(sanitychecklib.server, sanitychecklib.username, sanitychecklib.password)
    #this appears to be a non-blocking call that takes ages, so just bail and hope it worked

  finally:
    session.logout()

except XenAPI.Failure, f:
  #if the secondary server is already a slave then that's ok for binding sanity check purposes
  if f.details[0]=="HOST_IS_SLAVE":
          print sanitychecklib.secondaryserver, "is already a slave: aborting"


