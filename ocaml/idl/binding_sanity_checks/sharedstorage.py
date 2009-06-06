#!/usr/bin/env python

import datetime
import XenAPI
import sanitychecklib

#parameters for the shared storage to be created
storage_type='nfs'
device_config={'server':sanitychecklib.network_storage_server, 'serverpath':sanitychecklib.network_storage_path }
physical_size = '100000'
name_label   = 'created by sharedstorage.py '+ datetime.datetime.now().strftime("%X on %a %x")
name_description = 'shared storage created for testing purposes by the script sharedstorage.py'
content_type = 'content type field, wonder what goes here'
shared=True
sm_config={}

#log in 
session=sanitychecklib.getsession()
sx=session.xenapi
server=sanitychecklib.server

#find the reference to our host
hosts=sx.host.get_all()
print "According to "+server+" the following hosts exist: ", [sx.host.get_name_label(x) for x in hosts], "\n"
host = [x for x in hosts if sx.host.get_name_label(x)==server][0]
print "We assume that the reference", host, "is the server itself, since its name label is\"%s\"\n" % sx.host.get_name_label(host)

#create a new networked storage repository
new_sr=sx.SR.create(  host, device_config, physical_size,  name_label,  name_description, storage_type,  content_type,  shared, sm_config)
new_sr_record = sx.SR.get_record(new_sr)
print "Created new shared storage:"
print new_sr_record

#when an sr is created, it appears with PBDs already attached.
print "\nPBD(s) created along with the new SR"
for pbd in new_sr_record['PBDs']:
	print "    ", sx.PBD.get_record(new_sr_record['PBDs'][0])

#now we should set this as the default for our pool
pools=sx.pool.get_all()
print "There are ", len(pools), "pools"

our_pool=pools[0]
print "Assuming our pool is ",our_pool
print "Setting new storage to be the default sr for this pool"
sx.pool.set_default_SR(our_pool, new_sr)
print "Setting it to be the default for suspend and crashdump images too"
sx.pool.set_suspend_image_SR(our_pool, new_sr)
sx.pool.set_crash_dump_SR(our_pool, new_sr)



#log out
session.logout()
