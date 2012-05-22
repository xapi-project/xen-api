#!/usr/bin/env python

import os, sys, time, socket, traceback

log_f = os.fdopen(os.dup(sys.stdout.fileno()), "aw")
pid = None

def reopenlog(log_file):
    global log_f
    if log_f:
        log_f.close()
    if log_file:
        log_f = open(log_file, "aw")
    else:
        log_f = os.fdopen(os.dup(sys.stdout.fileno()), "aw")

def log(txt):
    global log_f, pid
    if not pid:
        pid = os.getpid()
    t = time.strftime("%Y%m%dT%H:%M:%SZ", time.gmtime())
    print >>log_f, "%s [%d] %s" % (t, pid, txt)
    log_f.flush()

def success(result):
    return { "Status": "Success", "Value": result }

class Rpc_light_failure(Exception):
    def __init__(self, name, args):
        self.name = name
        self.args = args
    def failure(self):
        return { 'Status': 'Failure',
                 'ErrorDescription': [ self.name, list(self.args) ] }

class UnknownMethod(Rpc_light_failure):
    def __init__(self, name):
        Rpc_light_failure.__init__(self, "UnknownMethod", [ name ])

class UnimplementedException(Rpc_light_failure):
    def __init__(self, cls, name):
        Rpc_light_failure.__init__(self, "UnimplementedException", [ cls, name ])

class InternalError(Rpc_light_failure):
    def __init__(self, error):
        Rpc_light_failure.__init__(self, "InternalError", [ error ])

class UnmarshalException(Rpc_light_failure):
    def __init__(self, thing, ty, desc):
        Rpc_light_failure.__init__(self, "UnmarshalException", [ thing, ty, desc ])

class TypeError(Rpc_light_failure):
    def __init__(self, expected, actual):
        Rpc_light_failure.__init__(self, "TypeError", [ expected, actual ])
    def __str__(self):
        return "TypeError expected=%s actual=%s" % (self.expected, self.actual)

def is_long(x):
    try:
        long(x)
        return True
    except:
        return False
