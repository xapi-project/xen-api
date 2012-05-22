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

import os, sys, commands, json

from storage import *

root = "/sr/"

# [run task cmd] executes [cmd], throwing a BackendError if exits with
# a non-zero exit code.
def run(task, cmd):
    code, output = commands.getstatusoutput(cmd)
    if code <> 0:
        log("%s: %s exitted with code %d: %s" % (task, cmd, code, output))
        raise (BackendError("command failed", [ str(code), output ]))
    log("%s: %s" % (task, cmd))
    return output

# Use Linux "losetup" to create block devices from files
class Loop:
    # [_find task path] returns the loop device associated with [path]
    def _find(self, task, path):
        global root
        for line in run(task, "losetup -a").split("\n"):
            line = line.strip()
            if line <> "":
                bits = line.split()
                loop = bits[0][0:-1]
                this_path = bits[2][1:-1]
                if this_path == path:
                    return loop
        return None
    # [add task path] creates a new loop device for [path] and returns it
    def add(self, task, path):
        run(task, "losetup -f %s" % path)
        return self._find(task, path)
    # [remove task path] removes the loop device associated with [path]
    def remove(self, task, path):
        loop = self._find(task, path)
        run(task, "losetup -d %s" % loop)

# Use FreeBSD "mdconfig" to create block devices from files
class Mdconfig:
    # [_find task path] returns the unit (mdX) associated with [path]
    def _find(self, task, path):
        # md0	vnode	 1024M	/root/big.img
        for line in run(task, "mdconfig -l -v").split("\n"):
            if line == "":
                continue
            bits = line.split()
            this_path = bits[3]
            if this_path == path:
                return bits[0] # md0
        return None
    # [add task path] returns a block device associated with [path]
    def add(self, task, path):
        return "/dev/" + run(task, "mdconfig -a -t vnode -f %s" % path)
    # [remove task path] removes the block device associated with [path]
    def remove(self, task, path):
        md = self._find(task, path)
        if md:
            run(task, "mdconfig -d -u %s" % md) 

# [path_of_vdi vdi] returns the path in the local filesystem corresponding
# to vdi location [vdi]
def path_of_vdi(vdi):
    global root
    return root + vdi

disk_suffix = ".raw"
metadata_suffix = ".json"

class Query(Query_skeleton):
    def __init__(self):
        Query_skeleton.__init__(self)
    def query(self):
        result = {}
        result["query_result"] = { "name": "RawFiles",
                                   "vendor": "XCP",
                                   "version": "0.1",
                                   "features": []
                                   }

class SR(SR_skeleton):
    def __init__(self):
        SR_skeleton.__init__(self)

    def create(self, sr, device_config, physical_size):
        """Operations which act on Storage Repositories"""
        raise UnimplementedException("SR", "create")
    def attach(self, sr, device_config):
        if not(os.path.exists(root)):
            raise BackendError("SR directory doesn't exist", [ root ])
    def detach(self, sr):
        pass
    def destroy(self, sr):
        pass
    def reset(self, sr):
        """Operations which act on Storage Repositories"""
        raise UnimplementedException("SR", "reset")
    def scan(self, sr):
        global root
        log("scanning")
        results = []
        for name in os.listdir(root):
            if name.endswith(metadata_suffix):
                path = root + "/" + name
                f = open(path, "r")
                try:
                    vdi_info = json.loads(f.read())
                    results.append(smapiv2.make_vdi_info(vdi_info))
                finally:
                    f.close()
        return results

class VDI(VDI_skeleton):
    def __init__(self):
        VDI_skeleton.__init__(self)

    def create(self, sr, vdi_info, params):
        filename = run(task, "uuidgen")
        run(task, "dd if=/dev/zero of=%s%s bs=1 count=0 seek=%s" % (path_of_vdi(filename), disk_suffix, vdi_info["virtual_size"]))
        vdi_info["vdi"] = filename
        f = open(path_of_vdi(filename) + metadata_suffix, "w")
        try:
            f.write(json.dumps(vdi_info))
        finally:
            f.close()
            return vdi_info
    def snapshot(self, sr, vdi, vdi_info, params):
        """Operations which operate on Virtual Disk Images"""
        raise UnimplementedException("VDI", "snapshot")
    def clone(self, sr, vdi, vdi_info, params):
        """Operations which operate on Virtual Disk Images"""
        raise UnimplementedException("VDI", "clone")
    def destroy(self, sr, vdi):
        if not (os.path.exists(path_of_vdi(vdi) + disk_suffix)):
            raise Vdi_does_not_exist(vdi)
        run(task, "rm -f %s%s" % (path_of_vdi(vdi), disk_suffix))
        run(task, "rm -f %s%s" % (path_of_vdi(vdi), metadata_suffix))
    def attach(self, dp, sr, vdi, read_write):
        path = path_of_vdi(vdi) + disk_suffix
        loop = self.device.add(task, path)
        log("loop = %s" % repr(loop))
        return loop
    def activate(self, dp, sr, vdi):
        pass
    def deactivate(self, dp, sr, vdi):
        pass
    def detach(self, dp, sr, vdi):
        path = path_of_vdi(vdi) + disk_suffix
        self.device.remove(task, path)
        
if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("-l", "--log", dest="logfile", help="log to LOG", metavar="LOG")
    parser.add_option("-p", "--port", dest="port", help="listen on PORT", metavar="PORT")
    parser.add_option("-i", "--ip-addr", dest="ip", help="listen on IP", metavar="IP")
    parser.add_option("-d", "--daemon", action="store_true", dest="daemon", help="run as a background daemon", metavar="DAEMON")
    (options, args) = parser.parse_args()
    if options.logfile:
        from smapiv2 import reopenlog
        reopenlog(options.logfile)
    if not options.ip and not options.ip:
        print >>sys.stderr, "Need an --ip-addr and --port. Use -h for help"
        sys.exit(1)

    ip = options.ip
    port = int(options.port)

    arch = run("startup", "uname")
    if arch == "Linux":
        log("startup: Using loop devices")
        start(RawFiles(Loop()), ip, port, options.daemon)
    elif arch == "FreeBSD":
        log("startup: Using mdconfig devices")
        start(RawFiles(Mdconfig()), ip, port, options.daemon)
    else:
        log("startup: Unknown architecture: %s" % arch)
