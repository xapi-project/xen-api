#!/usr/bin/env python

# XenAPI graphing tool. Produces database graph in graphviz format.

# Sample command line:
# ./dbgraph.py https://ivory root password | dot -Tpng -Kfdp >graph.png && eog graph.png

import sys
import XenAPI
from pprint import pprint,pformat

# Construct a subgraph of the database graph by:
#  Retrieving all the records of a given set of classes.
#  Picking a set of start nodes.
#  Following specified links from those start nodes to create a graph.
#   being careful of infinite loops.


#These three (constant) variables specify the graph we want to draw.

"""Every object type that we want to consider. List taken from the diagram at:
http://docs.xensource.com/XenServer/4.1.0/1.0/en_gb/api/docs/html/browser.html
"""
interesting_objects = [
    'host', 'PIF', 'network', 'VIF', 'VM', 'VBD', 'VDI', 'SR', 'PBD',
    'host_metrics', 'PIF_metrics', 'VIF_metrics','console','VM_guest_metrics','VM_metrics','crashdump','VBD_metrics','SM', 'host_cpu',
    ]

"""What our starting objects are:"""
to_be_marked= lambda (typed_obj): typed_obj.type_string=='host'

"""Which links we will follow."""
keys_to_chase = {
    'host': ['resident_VMs', 'PIFs', 'metrics', 'PBDs'],
    'VM'  : ['VIFs', 'VBDs', 'metrics', 'guest_metrics'],
    'PIF' : ['network', 'metrics'],
    'VIF' : ['network', 'metrics'],
}

#These are genuine global variables, since almost every function needs them.

"""XAPI function dispatcher."""
sx=None
"""XAPI object database. A dictionary of typed records representing every object on the server that we're interested in."""
object_database=None


#data acquisition and storage structure

class typed_record():

    """This is a class of the 'named tuple' sort holding records from the XAPI database.
    The elements are:
        a string to hold the type of the object ('host', 'VIF', etc..)
        the record itself
        a boolean to record whether the object has yet been visited during the graph traversal
    """

    def __init__(self, type_string, record, marked=False):
        self.type_string=type_string
        self.record=record
        self.marked=marked #when chasing through the graph we need to avoid loops, so mark when visited
    def __repr__(self):
        return "typed_record("+self.type_string.__repr__()+","+self.record.__repr__()+","+self.marked.__repr__()+")"


def make_object_database(sx, object_types):
    """Return a dictionary of typed_records, keyed by each object's opaque reference.
    object_types is a list of strings. make_object_database(sx,["host", "VM"]) returns all the host and
    VM objects from the connection sx, as typed_records."""
    object_database={}
    for obj_type in object_types:
        objs=sx.__getattr__(obj_type).get_all_records()
        for k,v in objs.items():
            object_database[k]=typed_record(obj_type, v)
    return object_database

#Recursive graph traversal

def listify(value):
    """Make single string into list of one string. Leave lists alone."""
    if(type(value)==type("OpaqueRef:")): #single OpaqueRef case
        return [value]
    else:
        return value

def mark_object_and_chase_keys(ref):
    """Do depth-first search, marking the visited nodes as we go."""
    try:
        obj=object_database[ref] #catch the case where there's no node to go to.
    except KeyError:
        return

    if(obj.marked==True): #if we've already been here then we don't need to do anything.
        return

    else:
        obj.marked=True #note that we've been here.
        for k in keys_to_chase.get(obj.type_string,[]):
            try:
                value=obj.record[k]
                for x in listify(value):
                    mark_object_and_chase_keys(x)
            except KeyError, e: #catch a bad or misspelled key value
                print "Error %r" % e
                print "chasing key ", k, "in type", obj.type_string
                raise


#Print the object database in graphviz format

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

def graphviz_print(object_database):
    """emit the marked subgraph in dot format."""
    print "digraph graphxapi { "
    for ref,obj in object_database.items():
        #for each object which has been marked
        if obj.marked:
            #print the name, or the type if there is no name
            print_node(ref, obj.record.get('name_label', obj.type_string))
            for k in keys_to_chase.get(obj.type_string,[]):
                for value in listify(obj.record[k]):
                    print_edge(ref, value, k)
    print "}"


#Command line parsing and program control

def main(url, username, password):
    global sx
    global object_database

    session = XenAPI.Session(url)
    session.login_with_password(username, password, '1.0', 'xen-api-tutorial-dbgraph.py')
    sx=session.xenapi

    #read all the objects we're interested in from Xapi.
    object_database=make_object_database(sx, interesting_objects)

    #for each starting object chase the key list through the database, marking as we go.
    for ref,obj in object_database.items():
        if to_be_marked(obj):
            mark_object_and_chase_keys(ref)

    print "/*database graph of", url, "*/"
    graphviz_print(object_database)

    session.logout()


if __name__ == "__main__":
    if   len(sys.argv) == 4:
        main(*sys.argv[1:])
    elif len(sys.argv) == 1:
        main("http://ivory", "root", "password")
    else:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)
