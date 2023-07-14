#!/usr/bin/env python2
#
# Copyright (C) Citrix Systems Inc.
#
# This program is free software; you can redistribute it and/or modify 
# it under the terms of the GNU Lesser General Public License as published 
# by the Free Software Foundation; version 2.1 only.
#
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

from __future__ import print_function
import os
import sys
import urlparse

import xapi.storage.api.v5.datapath
from xapi.storage.common import call
from xapi.storage import log

class Loop(object):
    """An active loop device"""

    def __init__(self, path, loop):
        self.path = path
        self.loop = loop

    def destroy(self, dbg):
        call(dbg, ["losetup", "-d", self.loop])

    def block_device(self):
        return self.loop

    @staticmethod
    def from_path(dbg, path):
        path = os.path.realpath(path)
        for line in call(dbg, ["losetup", "-a"]).split("\n"):
            line = line.strip()
            if line != "":
                bits = line.split()
                loop = bits[0][0:-1]
                open_bracket = line.find('(')
                close_bracket = line.find(')')
                this_path = line[open_bracket + 1:close_bracket]
                if this_path == path:
                    return Loop(path, loop)
        return None


class Implementation(xapi.storage.api.v5.datapath.Datapath_skeleton):
    """
    Datapath implementation
    """
    def _find_loop(self, path):
        path = os.path.realpath(path)

    def activate(self, dbg, uri, domain):
        pass

    def attach(self, dbg, uri, domain):
        parsed_url = urlparse.urlparse(uri)
        query = urlparse.parse_qs(parsed_url.query)

        file_path = os.path.realpath(parsed_url.path)

        cmd = ['losetup', '-f', file_path]
        if 'size' in query:
            cmd.extend(['--sizelimit', query['size'][0]])
        call(dbg, cmd)

        loop = Loop.from_path(dbg, file_path)

        return {"implementations": [
            [
                'XenDisk',
                {
                    'backend_type': 'vbd',
                    'params': loop.block_device(),
                    'extra': {}
                }
            ],
            [
                'BlockDevice',
                {
                    'path': loop.block_device()
                }
            ]
        ]}

    def deactivate(self, dbg, uri, domain):
        pass

    def detach(self, dbg, uri, domain):
        parsed_url = urlparse.urlparse(uri)

        file_path = os.path.realpath(parsed_url.path)

        if not(os.path.exists(file_path)):
            raise xapi.storage.api.volume.Volume_does_not_exist(file_path)

        loop = Loop.from_path(dbg, file_path)
        loop.destroy(dbg)

    def open(self, dbg, uri, domain):
        pass

    def close(self, dbg, uri):
        pass


if __name__ == "__main__":
    log.log_call_argv()
    cmd = xapi.storage.api.v5.datapath.Datapath_commandline(Implementation())
    base = os.path.basename(sys.argv[0])

    base_class, op = base.split('.')

    if base_class == 'Datapath':
        op = op.lower()
        fn = getattr(cmd, op, None)
        fn()
    else:
        cmds = ['Datapath.{}'.format(x) for x in dir(cmd) if not x.startswith('_')]
        for name in cmds:
            print(name)
