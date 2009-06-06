#!/usr/bin/env python

# XenAPI plugin boilerplat code

import sys, xmlrpclib, XenAPI

class Failure:
    def __init__(self, code, params):
        self.code = code
        self.params = params
    def __str__(self):
        s = { 'Status': 'Failure', 'ErrorDescription': [ self.code ] + self.params }        
        return xmlrpclib.dumps((s, ), "", True)

def dispatch(fn_table):
    if len(sys.argv) <> 2:
        raise "Incorrect number of commandline arguments"
    params, methodname = xmlrpclib.loads(sys.argv[1])
    session_id = params[0]
    args = params[1]
    try:
        if methodname in fn_table.keys():
            x = XenAPI.xapi_local()
            x._session = session_id
            result = fn_table[methodname](x, args)
            s = { 'Status': 'Success', 'Value': result }
            print xmlrpclib.dumps((s, ), "", True)
        else:
            raise Failure("UNKNOWN_XENAPI_PLUGIN_FUNCTION", [ methodname ])
    except Failure, e:
        print str(e)
