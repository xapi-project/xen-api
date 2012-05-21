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

class UnknownMethod(Exception):
    def __init__(self, name):
        self.name = name

class UnimplementedException(Exception):
    def __init__(self, cls, name):
        self.cls = cls
        self.name

class InternalError(Exception):
    def __init__(self, error):
        self.error = error

class UnmarshalException(Exception):
    def __init__(self, thing, ty, desc):
        self.thing = thing
        self.ty = ty
        self.desc = desc
