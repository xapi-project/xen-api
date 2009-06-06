#!/usr/bin/env python

import XenAPI
import provision
import sanitychecklib

#log in
session=sanitychecklib.getsession()
sx=session.xenapi

#find the template for Debian Etch
vms = sx.VM.get_all()
print "Server", sanitychecklib.server, "has ", len(vms), "VMs", 
etch_template_list = [x for x in vms if (('Etch' in sx.VM.get_name_label(x)) and (sx.VM.get_is_a_template(x)))]
print "including", len(etch_template_list), "template for 'Etch'"
etch_template=etch_template_list[0]
print "We pick the first template: "
print "name: ", sx.VM.get_name_label(etch_template)
print "description:", sx.VM.get_name_description(etch_template)

#Make a copy of the template
print "Cloning..."
clone=sx.VM.clone(etch_template, sanitychecklib.test_vm_name)

#find out where to put the new machine's disks by getting the first pool (I don't think there can be more than one)
#and using its default storage repository
pool_list=sx.pool.get_all()
if len(pool_list)==1:
	print "There's only one pool"
else:
	print "There are", len(pool_list), "pools"
	print "We pick the first one:"

first_pool=pool_list[0]
print "name:", sx.pool.get_name_label(first_pool)
print "description: ", sx.pool.get_name_description(first_pool)

default_SR=sx.pool.get_default_SR(first_pool)
print "The default SR is: "
print "Name:", sx.SR.get_name_label(default_SR)
print "Description:", sx.SR.get_name_description(default_SR)

#set the new copy to have its disks in the default SR
#this is a debian template specific hack which allows us to create Debian VMs easily
spec=provision.getProvisionSpec(session, clone)
spec.setSR(sx.SR.get_uuid(default_SR))
provision.setProvisionSpec(session, clone, spec)

#now 'provision' it, which causes the disks to actually be created.
print "provisioning...."
sx.VM.provision(clone)
print "provisioned"

#now find out which network to attach the new machine to 
#by finding out what the pool master host is connected to.
pool_master=sx.pool.get_master(first_pool)
master_PIFs=sx.host.get_PIFs(pool_master)
primary_PIF=master_PIFs[0]
master_network=sx.PIF.get_network(primary_PIF)

#attach new VM to default SR and master network
print "Creating VIF..."
new_vif = { 'device': '0',
            'network': master_network,
            'VM': clone,
            'MAC': "",
            'MTU': "1500",
            "qos_algorithm_type": "",
            "qos_algorithm_params": {},
            "other_config": {} }
sx.VIF.create(new_vif)

#Another Debian template specific hack. If 'noninteractive' is passed on the kernel command line, 
#the new machine will boot without asking for its root and VNC passwords to be set, and just use 'xensource'.
print "Adding noninteractive to the kernel commandline"
print "This is a hack in the template to enable the root account to be created with password 'xensource'"
sx.VM.set_PV_args(clone, "noninteractive")

#Should be all set now. Fire up the machine.
print "booting..."
sx.VM.start(clone, False, True)

#log out
print "logging out"
session.logout()















