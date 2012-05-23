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
        # rpc-light marshals a single result differently to a list of results
        args = list(self.args)
        marshalled_args = args
        if len(args) == 1:
            marshalled_args = args[0]
        return { 'Status': 'Failure',
                 'ErrorDescription': [ self.name, marshalled_args ] }

class InternalError(Rpc_light_failure):
    def __init__(self, error):
        Rpc_light_failure.__init__(self, "Internal_error", [ error ])

class UnmarshalException(InternalError):
    def __init__(self, thing, ty, desc):
        InternalError.__init__(self, "UnmarshalException thing=%s ty=%s desc=%s" % (thing, ty, desc))

class TypeError(InternalError):
    def __init__(self, expected, actual):
        InternalError.__init__(self, "TypeError expected=%s actual=%s" % (expected, actual))

class UnknownMethod(InternalError):
    def __init__(self, name):
        InternalError.__init__(self, "Unknown method %s" % name)


def is_long(x):
    try:
        long(x)
        return True
    except:
        return False

# Helper function to daemonise ##############################################
def daemonize():
    def fork():
        try:
            if os.fork() > 0:
                # parent
                sys.exit(0)
        except Exception, e:
            print >>sys.stderr, "fork() failed: %s" % e
            traceback.print_exc()
            raise
    fork()
    os.umask(0)
    os.chdir("/")
    os.setsid()
    fork()
    devnull = open("/dev/null", "r")
    os.dup2(devnull.fileno(), sys.stdin.fileno())
    devnull = open("/dev/null", "aw")
    os.dup2(devnull.fileno(), sys.stdout.fileno())
    os.dup2(devnull.fileno(), sys.stderr.fileno())

from SocketServer import UnixStreamServer
from SimpleXMLRPCServer import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler, SimpleXMLRPCDispatcher
from xmlrpclib import ServerProxy, Fault, Transport
from socket import socket, SOL_SOCKET, SO_REUSEADDR, AF_UNIX, SOCK_STREAM

# Server XMLRPC from any HTTP POST path #####################################

class RequestHandler(SimpleXMLRPCRequestHandler):
    rpc_paths = []

class UnixServer(UnixStreamServer, SimpleXMLRPCDispatcher):
    def __init__(self, addr, requestHandler=RequestHandler):
        self.logRequests = 0
        if os.path.exists(addr):
            os.unlink(addr)
        SimpleXMLRPCDispatcher.__init__(self)
        UnixStreamServer.__init__(self, addr, requestHandler)

class TCPServer(SimpleXMLRPCServer):
    def __init__(self, ip, port):
        SimpleXMLRPCServer.__init__(self, (ip, port), requestHandler=RequestHandler, allow_none=True)
    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        SimpleXMLRPCServer.server_bind(self)

# This is a hack to patch slow socket.getfqdn calls that
# BaseHTTPServer (and its subclasses) make.
# See: http://bugs.python.org/issue6085
# See: http://www.answermysearches.com/xmlrpc-server-slow-in-python-how-to-fix/2140/
import BaseHTTPServer

def _bare_address_string(self):
    host, port = self.client_address[:2]
    return '%s' % host

BaseHTTPServer.BaseHTTPRequestHandler.address_string = \
        _bare_address_string

# This is a hack to allow_none by default, which only became settable in
# python 2.5's SimpleXMLRPCServer

import xmlrpclib

original_dumps = xmlrpclib.dumps
def dumps(params, methodname=None, methodresponse=None, encoding=None,
          allow_none=1):
    return original_dumps(params, methodname, methodresponse, encoding, allow_none)
xmlrpclib.dumps = dumps

# Well-known feature flags understood by xapi ##############################
# XXX: add an enum to the IDL?

feature_sr_probe = "SR_PROBE"
feature_sr_update = "SR_UPDATE"
feature_sr_supports_local_caching = "SR_SUPPORTS_LOCAL_CACHING"
feature_vdi_create = "VDI_CREATE"
feature_vdi_delete = "VDI_DELETE"
feature_vdi_attach = "VDI_ATTACH"
feature_vdi_detach = "VDI_DETACH"
feature_vdi_resize = "VDI_RESIZE"
feature_vdi_resize_online = "VDI_RESIZE_ONLINE"
feature_vdi_clone = "VDI_CLONE"
feature_vdi_snapshot = "VDI_SNAPSHOT"
feature_vdi_activate = "VDI_ACTIVATE"
feature_vdi_deactivate = "VDI_DEACTIVATE"
feature_vdi_update = "VDI_UPDATE"
feature_vdi_introduce = "VDI_INTRODUCE"
feature_vdi_generate_config = "VDI_GENERATE_CONFIG"
feature_vdi_reset_on_boot = "VDI_RESET_ON_BOOT"
