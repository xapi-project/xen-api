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

# Example storage backend using SMAPIv2

# WARNING: this API is considered to be unstable and may be changed at-will

import os, sys, commands, xmlrpclib

import xcp
from storage import *

import vhd, tapdisk

# [run dbg cmd] executes [cmd], throwing a BackendError if exits with
# a non-zero exit code.
def run(dbg, cmd):
    code, output = commands.getstatusoutput(cmd)
    if code <> 0:
        log("%s: %s exitted with code %d: %s" % (dbg, cmd, code, output))
        raise (BackendError("command failed", [ str(code), output ]))
    log("%s: %s" % (dbg, cmd))
    return output

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
                feature_vdi_deactivate,
                feature_vdi_clone
                ],
                 "configuration": { "path": "filesystem path where the VDIs are stored" }
                 }

raw_suffix = ".raw"
vhd_suffix = ".vhd"
metadata_suffix = ".xml"

metadata_dir = "metadata"
data_dir = "data"

def highest_id(xs):
    highest = 0L
    for x in xs:
        try:
            y = long(x)
            if y > highest:
                highest = y
        except:
            pass
    return highest

def unlink_safe(path):
    try:
        os.unlink(path)
        log("os.unlink %s OK" % path)
    except Exception, e:
        log("os.unlink %s: %s" % (path, str(e)))


class Repo:
    """Encapsulates all relevant SR operations

The on-disk structure looks like this:

<root>/metadata/name.xml    -- VDI metadata for a disk with name "name"
<root>/metadata/name.1.xml  -- VDI metadata for another disk with name "name"

<root>/data/1.vhd           -- vhd format disk (parent or leaf)
<root>/data/2.vhd           -- vhd format disk (parent or leaf)
<root>/data/3.raw           -- raw format disk

There is an injective relationship between metadata/ files and data/
i.e. all metadata/ files reference exactly one data/ file but not all
data/ files are referenced directly by a metadata/ file.
"""
    def __init__(self, path):
        self.path = path
        # Load the metadata first
        self.metadata = {}
        metadata_path = path + "/" + metadata_dir
        if not (os.path.exists(metadata_path)):
            os.mkdir(metadata_path)
        for name in os.listdir(metadata_path):
            if name.endswith(metadata_suffix):
                md = metadata_path + "/" + name
                f = open(md, "r")
                try:
                    vdi_info = xmlrpclib.loads(f.read())[0][0]
                    self.metadata[name[0:-len(metadata_suffix)]] = vdi_info
                finally:
                    f.close()
        # Load data about the data second
        self.data = {}
        data_path = path + "/" + data_dir
        if not(os.path.exists(data_path)):
            os.mkdir(data_path)
        for name in os.listdir(data_path):
            if name.endswith(raw_suffix):
                self.data[name[0:-len(raw_suffix)]] = { "type": "raw" }
        all = vhd.list(data_path + "/*.vhd")
        for name in all.keys():
            log("data[%s] = %s" % (name, repr(all[name])))
            self.data[name] = all[name]
        self._data_highest_id = highest_id(self.data.keys()) + 1L
        # Query running tapdisks. We assume that all tapdisks in this domain
        # reference this SR.
        self.tapdisks = {}
        for tap in tapdisk.list():
            if not tap.file:
                log("%s has no file open: detaching and freeing" % str(tap))
                tap.detach()
                tap.free()
            else:
                ty, path = tap.file
                if path.startswith(data_path):
                    filename = os.path.basename(path)
                    if ty == "raw":
                        name = filename[0:-len(raw_suffix)]
                        log("%s open by %s" % (name, str(tap)))
                        self.tapdisks[name] = tap
                    elif ty == "vhd":
                        name = filename[0:-len(vhd_suffix)]
                        log("%s open by %s" % (name, str(tap)))
                        self.tapdisks[name] = tap
                    else:
                        log("Unknown tapdisk type: %s" % ty)

        # Integrity check: each metadata record should point to a data record
        for vdi_info in self.metadata.values():
            if vdi_info["data"] not in self.data:
                log("WARNING: vdi %s has data %s but this does not exist" % (vdi_info["vdi"], vdi_info["data"]))

    def get_deletable(self):
        """Return a set of the data blobs which are not reachable from
        any of the VDIs """
        reachable = set()
        for vdi_info in self.metadata.values():
            x = vdi_info["data"]
            while x not in reachable:
                reachable.add(x)
                info = self.data[x]
                if "parent" in info:
                    x = info["parent"]
        all = set(self.data.keys())
        return all.difference(reachable)

    def make_fresh_metadata_name(self, vdi_info):
        # To make this vaguely human-readable, sanitise the first 32 chars of the
        # VDI's name and then add a numerical suffix to make unique.
        name = vdi_info["name_label"]
        if len(name) > 32:
            # arbitrary cut-off
            name = name[0:32]
        valid = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
        name = ''.join(c for c in name if c in valid)
        taken = self.metadata.keys()
        if name not in taken:
            return name
        else:
            superstrings = filter(lambda x:x.startswith(name + "."), taken)
            suffixes = map(lambda x:x[len(name) + 1:], superstrings)
            suffix = highest_id(suffixes) + 1L
            return "%s.%Ld" % (name, suffix)

    def make_fresh_data_name(self):
        name = str(self._data_highest_id)
        self._data_highest_id = self._data_highest_id + 1
        return name

    def metadata_path_of_vdi(self, vdi):
        return self.path + "/" + metadata_dir + "/" + vdi + metadata_suffix

    def data_path_of_key(self, key):
        data = self.data[key]
        if data["type"] == "raw":
            return self.path + "/" + data_dir + "/" + key + raw_suffix
        elif data["type"] == "vhd":
            return self.path + "/" + data_dir + "/" + key + vhd_suffix
        else:
            raise (Vdi_does_not_exist(vdi))

    def data_path_of_vdi(self, vdi):
        vdi_info = self.metadata[vdi]
        key = vdi_info["data"]
        return self.data_path_of_key(key)

    def update_vdi_info(self, vdi, vdi_info):
        f = open(self.metadata_path_of_vdi(vdi), "w")
        try:
            try:
                f.write(xmlrpclib.dumps((vdi_info,), allow_none=True))
                self.metadata[vdi] = vdi_info
            except Exception, e:
                log("Exception writing metadata: %s" % (str(e)))
        finally:
            f.close()

    def create(self, vdi_info, params):
        vdi = self.make_fresh_metadata_name(vdi_info)
        vdi_info["vdi"] = vdi

        data = self.make_fresh_data_name()
        vdi_info["data"] = data

        stem = self.path + "/" + data_dir + "/" + data
        virtual_size = long(vdi_info["virtual_size"])
        if "type" in params and params["type"] == "raw":
            f = open(stem + raw_suffix, "wc")
            try:
                f.truncate(virtual_size)
                self.data[data] = { "type": "raw" }
            finally:
                f.close()
        else:
            vdi_info["virtual_size"] = vhd.create(virtual_size, stem + vhd_suffix)
            self.data[data] = { "type": "vhd" }

        self.update_vdi_info(vdi, vdi_info)
        return vdi_info

    def destroy(self, vdi):
        meta = self.metadata_path_of_vdi(vdi)
        unlink_safe(meta)
        del self.metadata[vdi]
        deletable = self.get_deletable()
        for x in deletable:
            log("Data %s is unreachable now" % x)
            path = self.data_path_of_key(x)
            unlink_safe(path)
            del self.data[x]

    def clone(self, vdi, vdi_info, params):
        parent = self.data_path_of_vdi(vdi)

        # Create two vhd leaves whose parent is [vdi]
        left = self.make_fresh_data_name()
        self.data[left] = vhd.make_leaf(self.path + "/" + data_dir + "/" + left + vhd_suffix, parent)

        right = self.make_fresh_data_name()
        self.data[right] = vhd.make_leaf(self.path + "/" + data_dir + "/" + right + vhd_suffix, parent)

        # Remap the original [vdi]'s location to point to the first leaf's path
        parent_info = self.metadata[vdi]
        parent_info["data"] = left
        self.update_vdi_info(vdi, parent_info)

        # The cloned vdi's location points to the second leaf's path
        clone = self.make_fresh_metadata_name(vdi_info)
        vdi_info["vdi"] = clone
        vdi_info["data"] = right
        self.update_vdi_info(clone, vdi_info)

        return vdi_info

    def attach(self, vdi, read_write):
        md = self.metadata[vdi]
        data = md["data"]
        if data in self.tapdisks:
            raise Backend_error("VDI_ALREADY_ATTACHED", [ vdi ])
        self.tapdisks[data] = tapdisk.Tapdisk()
        device = self.tapdisks[data].get_device()
        return { "params": device,
                 "xenstore_data": {} }

    def activate(self, vdi):
        md = self.metadata[vdi]
        data = md["data"]
        if data not in self.tapdisks:
            raise Backend_error("VDI_NOT_ATTACHED", [ vdi ])
        self.tapdisks[data].open(self.data[data]["type"], self.data_path_of_key(data))

    def deactivate(self, vdi):
        md = self.metadata[vdi]
        data = md["data"]
        if data not in self.tapdisks:
            raise Backend_error("VDI_NOT_ATTACHED", [ vdi ])
        self.tapdisks[data].close()

    def detach(self, vdi):
        md = self.metadata[vdi]
        data = md["data"]
        if data not in self.tapdisks:
            raise Backend_error("VDI_NOT_ATTACHED", [ vdi ])
        self.tapdisks[data].detach()
        self.tapdisks[data].free()

# A map from attached SR reference -> Repo instance
repos = {}

class SR(SR_skeleton):
    def __init__(self):
        SR_skeleton.__init__(self)

    def list(self, dbg):
        return repos.keys()
    def create(self, dbg, sr, device_config, physical_size):
        path = device_config["path"]
        if path in repos:
            raise (Sr_attached(sr))
        if not(os.path.exists(path)):
            raise Backend_error("SR directory doesn't exist", [ path ])
    def attach(self, dbg, sr, device_config):
        path = device_config["path"]
        if not(os.path.exists(path)):
            raise Backend_error("SR directory doesn't exist", [ path ])
        repos[sr] = Repo(path)

    def detach(self, dbg, sr):
        if not sr in repos:
            log("SR isn't attached, returning success")
        else:
            del repos[sr]
    def destroy(self, dbg, sr):
        pass
    def reset(self, dbg, sr):
        """Operations which act on Storage Repositories"""
        raise Unimplemented("SR.reset")
    def scan(self, dbg, sr):
        if not sr in repos:
            raise Sr_not_attached(sr)
        return repos[sr].metadata.values()

class VDI(VDI_skeleton):
    def __init__(self):
        VDI_skeleton.__init__(self)

    def create(self, dbg, sr, vdi_info, params):
        if not sr in repos:
            raise Sr_not_attached(sr)
        return repos[sr].create(vdi_info, params)

    def snapshot(self, dbg, sr, vdi, vdi_info, params):
        """Operations which operate on Virtual Disk Images"""
        raise Unimplemented("VDI.snapshot")
    def clone(self, dbg, sr, vdi, vdi_info, params):
        """Create a writable clone of a VDI"""
        if not sr in repos:
            raise Sr_not_attached(sr)
        return repos[sr].clone(vdi, vdi_info, params)
    def destroy(self, dbg, sr, vdi):
        if not sr in repos:
            raise Sr_not_attached(sr)
        repos[sr].destroy(vdi)
    def attach(self, dbg, dp, sr, vdi, read_write):
        if not sr in repos:
            raise Sr_not_attached(sr)
        return repos[sr].attach(vdi, read_write)
    def activate(self, dbg, dp, sr, vdi):
        if not sr in repos:
            raise Sr_not_attached(sr)
        return repos[sr].activate(vdi)
    def deactivate(self, dbg, dp, sr, vdi):
        if not sr in repos:
            raise Sr_not_attached(sr)
        return repos[sr].deactivate(vdi)
    def detach(self, dbg, dp, sr, vdi):
        if not sr in repos:
            raise Sr_not_attached(sr)
        return repos[sr].detach(vdi)
        
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
