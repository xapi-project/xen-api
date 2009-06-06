#!/usr/bin/env python

# NOT FINISHED (Needs tidying, can only follow OpaqueRefs, whereas some references are by uuid)
# This is supposed to be a general xapi database graphing tool.
# I've got a bit distracted, so I'm checking it in so it doesn't get lost.
# sample command line
# ./graphxapi.py | dot -Tpng >doom.png ; eog doom.png &

import XenAPI
import sanitychecklib
from pprint import pprint,pformat

#I've written a lot of database graphing programs recently.
#For instance there's a program called newmetrics graph which:
#finds all host objects, and then looks up all their resident VMs, and then for all those, looks up all VIFs and VBDs, then for all these VIFs looks up the network, and then for all those networks finds all the PIFs.

#I'd like to abstract this procedure of constructing a subgraph of the database graph by chasing keys from starting points.
#so for instance, a description of newmetricsgraph might be:

#considering objects (host, VM, VIF, VBD, network, PIF)
#starting from all host objects
#follow-keys (host, resident_VMs) (VM, VIFs) (VM, VBDs) (VIF, network) (network, PIFs)

#Similarly Richard is interested in the trees of VBDs which are created, so we might wish to say:

#considering objects (VBD)
#starting from all VBD where true
#follow-keys (VBD, parent)

#More formally, here are some functions which return 3-tuples describing these algorithms
#find ivory's pifs and associated networks
def ivorys_pifs_and_networks():
    return (
        ['PIF','host','network'],
        [('host','PIFs'),('PIF', 'network')],
        [('host',lambda(x): x['name_label']=='ivory')]
    )

#find all the objects with associated metrics  
def all_metrics():
    return( 
        ['host', 'VM', 'VIF', 'VBD', 'network', 'PIF', 
            'host_metrics','VIF_metrics','PIF_metrics','VBD_metrics','VM_metrics','VM_guest_metrics'],
        [   ('host', 'resident_VMs'), ('host','PIFs'), ('host','metrics'), 
            ('VM','VIFs'), ('VM','VBDs'), ('VM','metrics'), ('VM','guest_metrics'), 
            ('VIF','network'),('VIF','metrics'), 
            ('PIF', 'network'),('PIF','metrics')],
        [('host',lambda(x): True)]
    )

#this should have been Richard's vdi tree, but the parent field is apparently a uuid, not an OpaqueRef, so the program will need to be modified to deal with this. Grrr.
#this command may come in handy when fixing it
#xe vdi-param-set uuid=895ba851-6a04-c06c-49ac-1bbd3021668c other-config:foo=bar
def vdi_tree(uuid):
    return(
        ['VDI'],
        [('VDI',('other_config','parent'))],
        [('VDI', lambda(x): x['uuid']==uuid) ]
    )

#Choose the subgraph we want to walk through
(object_types, keys_to_chase, start_from)=all_metrics()
##(object_types, keys_to_chase, start_from)=vdi_tree('d295fe98-eea4-e5bd-a776-d1c335612256')
##(object_types, keys_to_chase, start_from)=ivorys_pifs_and_networks()
    
#Generally, we wish to announce the name of this file.
#When running in the interpreter, however, this doesn't exist, and we
#probably shouldn't log out either
try:
    this_test_name = __file__
    logout_after_test = True
except NameError:
    this_test_name = "unknown"
    logout_after_test = False

print "/*------------", this_test_name, "*/"

#log in to the master
print "/*logging in to ",sanitychecklib.server, "*/"
session=sanitychecklib.getsession()
sx=session.xenapi

class typed_record():
    def __init__(self, type_string, record, marked=False):
        self.type_string=type_string
        self.record=record
        self.marked=marked #when chasing through the graph we need to avoid loops, so mark when visited
    def __repr__(self):
        return "typed_record("+self.type_string.__repr__()+","+self.record.__repr__()+","+self.marked.__repr__()+")"
    
#first fill a database with the data of all the objects we might be interested in
object_database={}
for obj_type in object_types:
    objs=sx.__getattr__(obj_type).get_all_records()
    for k,v in objs.items():
        object_database[k]=typed_record(obj_type, v)

def mark_object_and_chase_keys(ref, keys_to_chase):
        if(ref=='OpaqueRef:NULL'):
          return
        obj=object_database[ref]
        if(obj.marked==True):
            return
        else:
            obj.marked=True
            type_string=obj.type_string
            record=obj.record
            for t,k in keys_to_chase:
                if type_string==t:
                    try:
                        if (type(k)==type("key")):
                            value=record[k]
                        elif(type(k)==type(("key","subkey"))):
                            value=record[k[0]][k[1]]
                        else:
                            error("key "+k+" should be either string or tuple")
                    except KeyError:
                        return
                    try:
                        if(type(value)==type([])):
                            for x in value:
                                mark_object_and_chase_keys(x, keys_to_chase)
                        else:
                            mark_object_and_chase_keys(value, keys_to_chase)
                    except:
                        print "Error while chasing key ",k, "in type", t
                        raise 
                
    
#mark all starting objects in the database, and for each one chase the key list through the database
for ref,obj in object_database.items():
    for t,f in start_from:
        if (obj.type_string == t ) and (f(obj.record)):
            mark_object_and_chase_keys(ref, keys_to_chase)

def print_edge(x, y, label=None):
    print '"%s" -> "%s"' % (x,y),     
    if label != None:
        print ' [label="%s"]' % label,
    print ';'

def print_node(x, label=None):
    print '"%s"' % x,     
    if label != None:
        print ' [label="%s"]' % label,
    print ';'

#emit the relevant subgraph in dot format
print "digraph graphxapi { "
for ref,obj in object_database.items():
    if (obj.marked):
        print_node(ref, obj.record.get('name_label', obj.type_string))
        for t,k in keys_to_chase:
            if obj.type_string==t:
                if (type(k)==type("key")):
                    value=obj.record[k]
                elif(type(k)==type(("key","subkey"))):
                    try:
                        value=obj.record[k[0]][k[1]]
                    except KeyError:
                        continue;                        
                else:
                        error("key "+k+" should be either string or tuple")
                if(type(value)==type([])):
                    for x in value:
                        print_edge(ref, x, k)
                else:
                    print_edge(ref, value, k)
print "}"

#log out
if logout_after_test:
    print "/*logging out*/"
    session.logout()

print "/*End of------", this_test_name, "*/"

    






