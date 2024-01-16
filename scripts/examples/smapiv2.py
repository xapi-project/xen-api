#!/usr/bin/env python3

from __future__ import print_function
import os, sys, time, socket, traceback

log_f = open(os.dup(sys.stdout.fileno()), "w")
pid = None

def reopenlog(log_file):
    global log_f
    if log_f:
        log_f.close()
    if log_file:
        try:
            log_f = open(log_file, "a")
        except IOError:
            log_f = open(log_file, "w")
    else:
        log_f = open(os.dup(sys.stdout.fileno()), "a")

def log(txt):
    global log_f, pid
    if not pid:
        pid = os.getpid()
    t = time.strftime("%Y%m%dT%H:%M:%SZ", time.gmtime())
    print("%s [%d] %s" % (t, pid, txt), file=log_f)
    log_f.flush()

# Functions to construct SMAPI return types #################################

unit = [ "Success", "Unit" ]

# Throw this to return an SR_BACKEND_FAILURE to the caller ##################

class BackendError(Exception):
    def __init__(self, code, params):
        self.code = code
        self.params = params
    def __str__(self):
        return "BackendError(%s, %s)" % (self.code, ", ".join(self.params))

class Vdi_does_not_exist(Exception):
    def __init__(self, vdi):
        self.vdi = vdi
    def __str__(self):
        return "Vdi_does_not_exist(%s)" % self.vdi

def vdi(vdi_info):
#    return ['Success', ['Vdi', {'vdi': location, 'virtual_size': str(virtual_size) }]]
    return ['Success', ['Vdi', vdi_info]]

def vdis(vis):
    return ['Success', ['Vdis', vis]]

def params(params):
    return ['Success', ['Params', params ]]

def value(result):
    return { "Status": "Success", "Value": result }

def backend_error(code, params):
    return [ "Failure", [ "Backend_error", code, params ] ]

def internal_error(txt):
    return [ "Failure", "Internal_error", txt ]

def vdi_does_not_exist():
    return [ "Failure", "Vdi_does_not_exist" ]

# Type-checking helper functions ############################################

vdi_info_types = {
    "vdi": type(""),
    "name_label": type(""),
    "name_description": type(""),
    "ty": type(""),
    "metadata_of_pool": type(""),
    "is_a_snapshot": type(True),
    "snapshot_time": type(""),
    "snapshot_of": type(""),
    "read_only": type(True),
    "cbt_enabled": type(True),
    "virtual_size": type(""),
    "physical_utilisation": type("")
}

def make_vdi_info(v):
    global vdi_info_types
    for k in vdi_info_types:
        t = vdi_info_types[k]
        if t == type(""):
            v[k] = str(v[k])
        elif t == type(True):
            v[k] = str(v[k]).lower() == "true"
        else:
            raise BackendError("make_vdi_info unknown type", [ str(t) ])
    return v

def vdi_info(v):
    global vdi_info_types
    for k in vdi_info_types:
        if k not in v:
            raise BackendError("vdi_info missing key", [ k, repr(v) ])
        t = vdi_info_types[k]
        if type(v[k]) != t:
            raise BackendError("vdi_info key has wrong type", [ k, str(t), str(type(v[k])) ])
    return v

def expect_none(x):
    if x != None:
        raise BackendError("type error", [ "None", repr(x) ])

def expect_long(x):
    if type(x) != type(0):
        raise BackendError("type error", [ "long int", repr(x) ])

def expect_string(x):
    if type(x) != type(""):
        raise BackendError("type error", [ "string", repr(x) ])

# Well-known feature flags understood by xapi ##############################

feature_sr_probe = "SR_PROBE"
feature_sr_update = "SR_UPDATE"
feature_sr_supports_local_caching = "SR_SUPPORTS_LOCAL_CACHING"
feature_vdi_create = "VDI_CREATE"
feature_vdi_destroy = "VDI_DESTROY"
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

# Unmarshals arguments and marshals results (including exceptions) ##########

class Marshall:
    def __init__(self, x):
        self.x = x

    def query(self, args):
        result = self.x.query()
        return value(result)

    def sr_attach(self, args):
        result = self.x.sr_attach(args["task"], args["sr"], args["device_config"])
        expect_none(result)
        return value(unit)
    def sr_detach(self, args):
        result = self.x.sr_detach(args["task"], args["sr"])
        expect_none(result)
        return value(unit)
    def sr_destroy(self, args):
        result = self.x.sr_destroy(args["task"], args["sr"])
        expect_none(result)
        return value(unit)
    def sr_scan(self, args):
        vis = self.x.sr_scan(args["task"], args["sr"])
        result = [vdi_info(vi) for vi in vis]
        return value(vdis(result))

    def vdi_create(self, args):
        vi = self.x.vdi_create(args["task"], args["sr"], vdi_info(args["vdi_info"]), args["params"])
        return value(vdi(vdi_info(vi)))
    def vdi_destroy(self, args):
        result = self.x.vdi_destroy(args["task"], args["sr"], args["vdi"])
        expect_none(result)
        return value(unit)

    def vdi_attach(self, args):
        result = self.x.vdi_attach(args["task"], args["dp"], args["sr"], args["vdi"], args["read_write"])
        expect_string(result)
        return value(params(result))
    def vdi_activate(self, args):
        result = self.x.vdi_activate(args["task"], args["dp"], args["sr"], args["vdi"])
        expect_none(result)
        return value(unit)
    def vdi_deactivate(self, args):
        result = self.x.vdi_deactivate(args["task"], args["dp"], args["sr"], args["vdi"])
        expect_none(result)
        return value(unit)
    def vdi_detach(self, args):
        result = self.x.vdi_detach(args["task"], args["dp"], args["sr"], args["vdi"])
        expect_none(result)
        return value(unit)


    def _dispatch(self, method, params):
        try:
            log("method = %s params = %s" % (method, repr(params)))
            args = params[0]
            if method == "query":
                return self.query(args)
            elif method == "SR.attach":
                return self.sr_attach(args)
            elif method == "SR.detach":
                return self.sr_detach(args)
            elif method == "SR.scan":
                return self.sr_scan(args)
            elif method == "VDI.create":
                return self.vdi_create(args)
            elif method == "VDI.destroy":
                return self.vdi_destroy(args)
            elif method == "VDI.attach":
                return self.vdi_attach(args)
            elif method == "VDI.activate":
                return self.vdi_activate(args)
            elif method == "VDI.deactivate":
                return self.vdi_deactivate(args)
            elif method == "VDI.detach":
                return self.vdi_detach(args)
        except BackendError as e:
            log("caught %s" % e)
            traceback.print_exc()
            return value(backend_error(e.code, e.params))
        except Vdi_does_not_exist as e:
            log("caught %s" %e)
            return value(vdi_does_not_exist())
        except Exception as e:
            log("caught %s" % e)
            traceback.print_exc()
            return value(internal_error(str(e)))

# Helper function to daemonise ##############################################
def daemonize():
    def fork():
        try:
            if os.fork() > 0:
                # parent
                sys.exit(0)
        except Exception as e:
            print("fork() failed: %s" % e, file=sys.stderr)
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

from xmlrpc.server import SimpleXMLRPCServer, SimpleXMLRPCRequestHandler

# Server XMLRPC from any HTTP POST path #####################################

class RequestHandler(SimpleXMLRPCRequestHandler):
    rpc_paths = []

# SimpleXMLRPCServer with SO_REUSEADDR ######################################

class Server(SimpleXMLRPCServer):
    def __init__(self, ip, port):
        SimpleXMLRPCServer.__init__(self, (ip, port), requestHandler=RequestHandler)
    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        SimpleXMLRPCServer.server_bind(self)

# This is a hack to patch slow socket.getfqdn calls that
# BaseHTTPServer (and its subclasses) make.
# See: http://bugs.python.org/issue6085
# See: http://www.answermysearches.com/xmlrpc-server-slow-in-python-how-to-fix/2140/
import http.server

def _bare_address_string(self):
    host, port = self.client_address[:2]
    return '%s' % host

http.server.BaseHTTPRequestHandler.address_string = \
        _bare_address_string

# Given an implementation, serve requests forever ###########################

def start(impl, ip, port, daemon):
    if daemon:
        log("daemonising")
        daemonize()
    log("will listen on %s:%d" % (ip, port))
    server = Server(ip, port)
    log("server registered on %s:%d" % (ip, port))
    server.register_introspection_functions() # for debugging
    server.register_instance(Marshall(impl))
    log("serving requests forever")
    server.serve_forever()
