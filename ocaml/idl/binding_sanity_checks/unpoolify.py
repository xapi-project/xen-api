#!/usr/bin/env python

import XenAPI
import sanitychecklib

#log in to the master
print "logging in to ",sanitychecklib.server
session=sanitychecklib.getsession()
sx=session.xenapi

#find the secondary host by name
secondaryserver_list=sx.host.get_by_name_label(sanitychecklib.secondaryserver)

if len(secondaryserver_list)==1:
	secondaryserver=secondaryserver_list[0]
	#eject it from the pool
	print "ejecting", sanitychecklib.secondaryserver, "from the pool"
	sx.pool.eject(secondaryserver)
else:
	print "there is no host", sanitychecklib.secondaryserver, "in the pool associated with", sanitychecklib.server

#the eject operation takes ages.....
  
#log out
session.logout()
