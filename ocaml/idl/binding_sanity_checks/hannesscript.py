#!/usr/bin/env python

SERVER="http://server"
USERNAME="root"
PASSWORD="password"

import XenAPI
from pprint import pprint, pformat

#given a list of dictionaries, print selected keys in order from each one, nicely formatted with a title
def dictionary_list_partial_print(title, dictionary_list, keys ):
    bar='-' * len(title)
    print bar ,'\n', title ,'\n', bar
    print "\n--\n".join(["\n".join(["%s  : %s" % (k, pformat( d[k] )) for k in keys]) for d in dictionary_list])
    print bar

#log in to the master
session=XenAPI.Session(SERVER)
session.login_with_password(USERNAME, PASSWORD, '1.0', 'xen-api-hannesscript.py')
sx=session.xenapi

#first, we'll find all the hosts, and get the information we care about from each
hosts=sx.host.get_all()
host_metrics=[{
    "name_label"    : sx.host.get_name_label(x),
    "metrics"       : sx.host_metrics.get_record(sx.host.get_metrics(x)),
    "host_cpus"     : [sx.host_cpu.get_record(x) for x in sx.host.get_host_CPUs(x)]
    } for x in hosts]

#and print out the interesting bits
dictionary_list_partial_print("Host Metrics", host_metrics, ["name_label","metrics", "host_cpus"])

# x, 'VM', 'guest_metrics' -> guest_metrics_record of the VM x
# catch the NULL if the record doesn't exist for some reason, and return the string 'NULL'
def fetch_metrics_record(object_reference, type_string, metrics_name):
    record_reference=sx.__getattr__(type_string).__getattr__('get_'+metrics_name)(object_reference)
    if record_reference=='OpaqueRef:NULL':
        return 'NULL'
    else:
        return sx.__getattr__(type_string+'_'+metrics_name).get_record(record_reference)

#find all the virtual machines which are resident on the hosts
resident_vms=set()
for host in hosts:
    resident_vms.update(sx.host.get_resident_VMs(host))

#get and print their info
vm_metrics = [{
    "name_label"     : sx.VM.get_name_label(x),
    "metrics"            : fetch_metrics_record(x, 'VM', 'metrics'),
    "guest_metrics" : fetch_metrics_record(x, 'VM', 'guest_metrics'),
    } for x in resident_vms]

dictionary_list_partial_print("Virtual Machine Metrics", vm_metrics, ["name_label", "metrics", "guest_metrics"])

#from the list of resident VMs we can find all the active VIFs and VBDs
#however these don't have useful names, so we have to make them up
active_vifs=[vif for vif in sx.VIF.get_all() if sx.VIF.get_VM(vif) in resident_vms]

vif_metrics = [{
    "name_label"    : "VIF connecting \"%s\" to \"%s\"" % (sx.network.get_name_label(sx.VIF.get_network(x)), sx.VM.get_name_label(sx.VIF.get_VM(x))),
    "metrics" : fetch_metrics_record(x, 'VIF', 'metrics')
    } for x in active_vifs]

dictionary_list_partial_print("VIF metrics", vif_metrics, ["name_label","metrics"])

#the names of the vbds are a little more complicated, because there is the possiblility that a VBD connects
#a VM to a CD drive, which may be empty, and thus not have a VDI to represent it.
def get_vbd_name(vbd):
    if sx.VBD.get_type(vbd)=="CD" and sx.VBD.get_empty(vbd)==True:
        device_name="empty cd drive"
    else:
        device_name=sx.VDI.get_name_label(sx.VBD.get_VDI(vbd))
    return "VBD connecting \"%s\" to \"%s\"" % (sx.VM.get_name_label(sx.VBD.get_VM(vbd)), device_name)

active_vbds=[vbd for vbd in sx.VBD.get_all() if sx.VBD.get_VM(vbd) in resident_vms]

vbd_metrics = [{
    "name_label"     : get_vbd_name(x),
    "metrics"  : fetch_metrics_record(x, 'VBD', 'metrics') 
    } for x in active_vbds ]

dictionary_list_partial_print("VBD Metrics", vbd_metrics, ["name_label","metrics"])

#from the VIFs we can find the active networks, which don't actually have any metrics
active_networks=set()
for vif in active_vifs:
    active_networks.add(sx.VIF.get_network(vif))

network_metrics=[{
    "name_label": sx.network.get_name_label(x)
    } for x in active_networks]
dictionary_list_partial_print("Network Metrics", network_metrics, ["name_label"])

#and from the active networks we can get all the relevant pifs
active_pifs=set()
for network in active_networks:
    active_pifs.update(sx.network.get_PIFs(network))

pif_metrics = [{
    "name_label"     : "%s on %s " % (sx.PIF.get_device(x), sx.host.get_name_label( sx.PIF.get_host(x) )),
    "metrics"  : fetch_metrics_record(x, 'PIF', 'metrics')
    } for x in active_pifs ]

dictionary_list_partial_print("PIF Metrics", pif_metrics, ["name_label","metrics"])

#finish off by printing out a concise list of all the active objects
print "Active Objects"
for x in  ["host_metrics" , "vm_metrics", "vif_metrics", "vbd_metrics", "network_metrics", "pif_metrics" ]:
    print x, [(y['name_label']) for y in globals()[x]]

        
session.logout()

