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

import os
import util
import errno

MOUNT_NFS = "/sbin/mount.nfs"
UMOUNT = "/bin/umount"
MTAB = "/etc/mtab"
MKDIR = "/bin/mkdir"

class Mount:
    def __init__(self, remote, local):
        self.remote = remote
        self.local = local

    def mount(self):
        cmd = [ MOUNT_NFS, self.remote, self.local ]
        if not(os.path.exists(self.local)):
            util.pread([ MKDIR, "-p", self.local ])
        util.pread2(cmd)

    def umount(self):
        cmd = [ UMOUNT, self.local ]
        util.pread2(cmd)

def list(root):
    """Return a list of Mount instances with local mounts in
    the filesystem tree rooted at [root]"""
    f = open(MTAB, "r")
    try:
        results = []
        for line in f.readlines():
            # remote local nfs options 0 0
            bits = line.split()
            if bits[2] == "nfs" and bits[1].startswith(root):
                results.append(Mount(bits[0], bits[1]))
        return results
    finally:
        f.close()

