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

import util, errno, os.path
from xcp import log, MissingDependency

TAP_CTL="/usr/sbin/tap-ctl"

if not(os.path.exists(TAP_CTL)):
    log("%s does not exist: do you need to install tapdisk/blktap?" % TAP_CTL)
    raise MissingDependency(TAP_CTL)

class Tapdisk:
    def __init__(self, minor = None, pid = None, file = None):
        if minor == None:
            minor = self._allocate()
        if pid == None:
            pid = self._spawn()
            self._attach(minor, pid)
        self.minor = minor
        self.pid = pid
        self.file = file
        self.mirror = None

    def __str__(self):
        return "Tapdisk(minor=%s, pid=%s, file=%s)" % (str(self.minor), str(self.pid), repr(self.file))

    def _allocate(self):
        cmd = [TAP_CTL, "allocate"]
        return util.pread2(cmd)[len("/dev/xen/blktap-2/tapdev"):].strip()

    def get_device(self):
        return "/dev/xen/blktap-2/tapdev%s" % (str(self.minor))

    def _spawn(self):
        cmd = [TAP_CTL, "spawn"]
        line = util.pread2(cmd)
        return line.strip()

    def _attach(self, minor, pid):
        cmd = [TAP_CTL, "attach", "-m", str(minor), "-p", str(pid)]
        util.pread2(cmd)

    def open(self, ty, path):
        cmd = [TAP_CTL, "open", "-m", str(self.minor), "-p", str(self.pid), "-a", "%s:%s" % (ty, path)]
        util.pread2(cmd)
        self.file = (ty, path)

    def close(self):
        cmd = [TAP_CTL, "close", "-m", str(self.minor), "-p", str(self.pid)]
        util.pread2(cmd)
        self.file = None

    def pause(self):
        cmd = [TAP_CTL, "pause", "-p", str(self.pid), "-m", str(self.minor) ]
        util.pread2(cmd)

    def unpause(self):
        (ty, path) = self.file
        cmd = [TAP_CTL, "unpause", "-p", str(self.pid), "-m", str(self.minor), "-a", "%s:%s" % (ty, path) ]
        if self.mirror:
            cmd.extend(["-2", self.mirror])
        util.pread2(cmd)

    def reopen(self):
        self.pause()
        self.unpause()

    def detach(self):
        cmd = [TAP_CTL, "detach", "-m", str(self.minor), "-p", str(self.pid)]
        util.pread2(cmd)

    def free(self):
        cmd = [TAP_CTL, "free", "-m", str(self.minor)]
        util.pread2(cmd)

def list(path = None):
    """Return a list of all the tapdisks on a system (if path == None) or
       only those which have opened a file in a particular directory tree
       (if path == "/some/tree")."""
    cmd = [TAP_CTL, "list"]
    output = util.ioretry(lambda: util.pread2(cmd), errlist = [errno.EPROTO, errno.ENOENT])
    results = []
    for line in output.split("\n"):
        # FIXME: tap-ctl writes error messages to stdout and
        # confuses this parser
        if line == "blktap kernel module not installed\n":
            # This isn't pretty but (a) neither is confusing stdout/stderr
            # and at least causes the error to describe the fix
            raise Exception, "blktap kernel module not installed: try 'modprobe blktap'"

        minor = None
        pid = None
        file = None
        # pid=16775 minor=0 state=0 args=vhd:/foo/data/108.vhd
        # line = [minor=1]
        for field in line.rstrip().split(' '):
            bits = field.split('=')
            if len(bits) == 2:
                key, val = field.split('=')

                if key == "pid":
                    pid = int(val, 10)
                elif key == "minor":
                    minor = int(val, 10)
                elif key == "args":
                    x = val.split(":", 2)
                    file = (x[0], x[1])

            else:
                log("Ignoring unexpected tap-ctl output: %s" % repr(field))
        if minor or pid:
            if not path or (file and file[1].startswith(path)):
                c = Tapdisk(minor = minor, pid = pid, file = file)
                results.append(c)
    return results
