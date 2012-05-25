#!/usr/bin/env python
#
# Copyright (C) Citrix Inc
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Example storage backend using SMAPIv2 using raw files and Linux losetup

# WARNING: this API is considered to be unstable and may be changed at-will

import os, sys, commands, xmlrpclib

import xcp
from storage import *

import vhdutil

# [run dbg cmd] executes [cmd], throwing a BackendError if exits with
# a non-zero exit code.
def run(dbg, cmd):
    code, output = commands.getstatusoutput(cmd)
    if code <> 0:
        log("%s: %s exitted with code %d: %s" % (dbg, cmd, code, output))
        raise (BackendError("command failed", [ str(code), output ]))
    log("%s: %s" % (dbg, cmd))
    return output

# Use Linux "losetup" to create block devices from files
class Loop:
    # [_find dbg path] returns the loop device associated with [path]
    def _find(self, root, dbg, path):
        for line in run(dbg, "losetup -a").split("\n"):
            line = line.strip()
            if line <> "":
                bits = line.split()
                loop = bits[0][0:-1]
                this_path = bits[2][1:-1]
                if this_path == path:
                    return loop
        return None
    # [add dbg path] creates a new loop device for [path] and returns it
    def add(self, root, dbg, path):
        run(dbg, "losetup -f %s" % path)
        return self._find(root, dbg, path)
    # [remove dbg path] removes the loop device associated with [path]
    def remove(self, root, dbg, path):
        loop = self._find(root, dbg, path)
        run(dbg, "losetup -d %s" % loop)

# [path_of_vdi vdi] returns the path in the local filesystem corresponding
# to vdi location [vdi]
def path_of_vdi(root, vdi):
    return root + "/" + vdi

raw_suffix = ".raw"
vhd_suffix = ".vhd"
metadata_suffix = ".xml"

class Query(Query_skeleton):
    def __init__(self):
        Query_skeleton.__init__(self)
    def query(self, dbg):
        return { "driver": "fs",
                 "name": "filesystem SR",
                 "description": "VDIs are stored as files in an existing filesystem",
                 "vendor": "XCP",
                 "copyright": "see the source code",
                 "version": "2.0",
                 "required_api_version": "2.0",
                 "features": [
                feature_vdi_create,
                feature_vdi_delete,
                feature_vdi_attach,
                feature_vdi_detach,
                feature_vdi_activate,
                feature_vdi_deactivate
                ],
                 "configuration": { "path": "filesystem path where the VDIs are stored" }
                 }

class Metadata:
    """Stores in memory all relevant SR metadata"""

    def __init__(self, path):
        self.path = path
        self.vdi_info = {}
        highest_id = 0
        for name in os.listdir(path):
            if name.endswith(metadata_suffix):
                md = path + "/" + name
                f = open(md, "r")
                try:
                    vdi_info = xmlrpclib.loads(f.read())[0][0]
                    log("vdi_info = %s" % repr(vdi_info))
                    vdi_id = name[0:-(len(metadata_suffix))]
                    self.vdi_info[vdi_id] = vdi_info
                    try:
                        if long(vdi_id) > highest_id:
                            highest_id = long(vdi_id)
                    except:
                        pass
                finally:
                    f.close()
        self._highest_id = highest_id

    def make_fresh_vdi_name(self):
        name = str(self._highest_id)
        self._highest_id = self._highest_id + 1
        return name

# A map from attached SR -> Metadata instance
metadata = {}

class SR(SR_skeleton):
    def __init__(self):
        SR_skeleton.__init__(self)

    def list(self, dbg):
        return metadata.keys()
    def create(self, dbg, sr, device_config, physical_size):
        path = device_config["path"]
        if path in metadata:
            raise (Sr_attached(sr))
        if not(os.path.exists(path)):
            raise Backend_error("SR directory doesn't exist", [ path ])
    def attach(self, dbg, sr, device_config):
        path = device_config["path"]
        if not(os.path.exists(path)):
            raise Backend_error("SR directory doesn't exist", [ path ])
        metadata[sr] = Metadata(path)

    def detach(self, dbg, sr):
        if not sr in metadata:
            log("SR isn't attached, returning success")
        else:
            del metadata[sr]
    def destroy(self, dbg, sr):
        pass
    def reset(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.reset")
    def scan(self, dbg, sr):
        if not sr in metadata:
            raise Sr_not_attached(sr)
        return metadata[sr].vdi_info.values()

def unlink_safe(path):
    try:
        os.unlink(path)
        log("os.unlink %s OK" % path)
    except Exception, e:
        log("os.unlink %s: %s" % (path, str(e)))

class VDI(VDI_skeleton):
    def __init__(self):
        VDI_skeleton.__init__(self)
        self.device = Loop()

    def create(self, dbg, sr, vdi_info, params):
        if not sr in metadata:
            raise Sr_not_attached(sr)
        filename = metadata[sr].make_fresh_vdi_name()
        root = metadata[sr].path

        virtual_size = long(vdi_info["virtual_size"])
        stem = path_of_vdi(root, filename)
        if "type" in params and params["type"] == "raw":
            f = open(stem + raw_suffix, "wc")
            try:
                f.truncate(virtual_size)
            finally:
                f.close()
        else:
            vdi_info["virtual_size"] = vhdutil.create(virtual_size, stem + vhd_suffix)

        vdi_info["vdi"] = filename
        f = open(stem + metadata_suffix, "w")
        try:
            try:
                f.write(xmlrpclib.dumps((vdi_info,), allow_none=True))
                metadata[sr].vdi_info[filename] = vdi_info
            except Exception, e:
                log("Exception writing metadata: %s" % (str(e)))
        finally:
            f.close()
            return vdi_info
    def snapshot(self, dbg, sr, vdi, vdi_info, params):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.snapshot")
    def clone(self, dbg, sr, vdi, vdi_info, params):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.clone")
    def destroy(self, dbg, sr, vdi):
        if not sr in metadata:
            raise Sr_not_attached(sr)
        root = metadata[sr].path
        
        stem = path_of_vdi(root, vdi)
        if not (os.path.exists(stem + raw_suffix)) and not(os.path.exists(stem + vhd_suffix)):
            raise Vdi_does_not_exist(vdi)
        del metadata[sr].vdi_info[vdi]
        unlink_safe(stem + vhd_suffix)
        unlink_safe(stem + raw_suffix)
        unlink_safe(stem + metadata_suffix)
    def attach(self, dbg, dp, sr, vdi, read_write):
        root = metadata[sr].path
        path = path_of_vdi(root, vdi) + disk_suffix
        loop = self.device.add(root, dbg, path)
        log("loop = %s" % repr(loop))
        return { "params": loop,
                 "xenstore_data": {} }
    def activate(self, dbg, dp, sr, vdi):
        pass
    def deactivate(self, dbg, dp, sr, vdi):
        pass
    def detach(self, dbg, dp, sr, vdi):
        root = metadata[sr].path
        path = path_of_vdi(root, vdi) + disk_suffix
        self.device.remove(root, dbg, path)
        
if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("-l", "--log", dest="logfile", help="log to LOG", metavar="LOG")
    parser.add_option("-p", "--port", dest="port", help="listen on PORT", metavar="PORT")
    parser.add_option("-i", "--ip-addr", dest="ip", help="listen on IP", metavar="IP")
    parser.add_option("-s", "--socket", dest="socket", help="listen on Unix domain socket", metavar="SOCK")
    parser.add_option("-d", "--daemon", action="store_true", dest="daemon", help="run as a background daemon", metavar="DAEMON")
    (options, args) = parser.parse_args()
    if options.logfile:
        from xcp import reopenlog
        reopenlog(options.logfile)
    tcp = options.ip and options.port
    unix = options.socket
    if not tcp and not unix:
        print >>sys.stderr, "Need an --ip-addr and --port or a --socket. Use -h for help"
        sys.exit(1)

    if options.daemon:
        log("daemonising")
        xcp.daemonize()

    server = None
    if tcp:
        log("will listen on %s:%d" % (ip, port))
        server = xcp.TCPServer(ip, port)
    else:
        log("will listen on %s" % options.socket)
        server = xcp.UnixServer(options.socket)

    server.register_introspection_functions() # for debugging
    server.register_instance(storage_server_dispatcher(Query = Query_server_dispatcher(Query()), VDI = VDI_server_dispatcher(VDI()), SR = SR_server_dispatcher(SR())))
    log("serving requests forever")
    server.serve_forever()
