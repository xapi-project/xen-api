#!/usr/bin/python

# XenAPI python plugin boilerplate code

import sys, xmlrpclib, XenAPI

class Failure(Exception):
    """Provide compatibilty with plugins written against XenServer 5.5 API"""

    def __init__(self, code, params):
        Exception.__init__(self)
        self.params = [ code ] + params
    def __str__(self):
        return str(self.params)

def success_message(result):
    rpcparams = { 'Status': 'Success', 'Value': result }
    return xmlrpclib.dumps((rpcparams, ), '', True)

def failure_message(description):
    rpcparams = { 'Status': 'Failure', 'ErrorDescription': description }
    return xmlrpclib.dumps((rpcparams, ), '', True)

def dispatch(fn_table):
    if len(sys.argv) <> 2:
        raise "Incorrect number of commandline arguments"
    params, methodname = xmlrpclib.loads(sys.argv[1])
    session_id = params[0]
    args = params[1]
    if methodname in fn_table:
        x = XenAPI.xapi_local()
        x._session = session_id
        try:
            result = fn_table[methodname](x, args)
            print success_message(result)
        except SystemExit:
            # SystemExit should not be caught, as it is handled elsewhere in the plugin system.
            raise
        except Failure, e:
            print failure_message(e.params)
        except Exception, e:
            print failure_message(['XENAPI_PLUGIN_FAILURE',
                                   methodname, e.__class__.__name__, str(e)])
    else:
        print failure_message(['UNKNOWN_XENAPI_PLUGIN_FUNCTION', methodname])
