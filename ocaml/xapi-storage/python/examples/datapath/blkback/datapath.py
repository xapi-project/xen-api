#!/usr/bin/env python3
#
# Copyright (C) Cloud Software Group.
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


import os
import stat
import sys
import urllib.parse

import xapi.storage.api.v5.datapath
from xapi.storage.common import call
from xapi.storage import log


class BlockDevice:

    def __init__(self, dev):
        self.dev = dev

    def destroy(self, dbg):
        call(dbg, ["losetup", "-d", self.dev])

    def block_device(self):
        return self.dev


class Loop(BlockDevice):
    """An active loop device"""

    def __init__(self, path, loop):
        self.path = path
        super().__init__(loop)

    def destroy(self, dbg):
        call(dbg, ["losetup", "-d", self.block_device()])

    @staticmethod
    def create_loop_device(dbg, target_path, query):
        cmd = ['losetup', '--show', '-f', target_path]
        if 'size' in query:
            cmd.extend(['--sizelimit', query['size'][0]])
        stdout = call(dbg, cmd)
        if stdout == '':
            return None

        return Loop(target_path, stdout.decode().strip())

    @staticmethod
    def from_path(dbg, path, query):
        path = os.path.realpath(path)
        for line in call(dbg, ["losetup", "-n", '--list']).splitlines():
            line = line.strip().decode()
            if line != "":
                bits = line.split()
                loop = bits[0]
                this_path = bits[5]
                if this_path == path:
                    return Loop(path, loop)
        return Loop.create_loop_device(dbg, path, query)


class Implementation(xapi.storage.api.v5.datapath.Datapath_skeleton):
    """
    Datapath implementation
    """
    def get_provider(self, dbg, target_path, query):
        statinfo = os.stat(target_path)

        if stat.S_ISBLK(statinfo.st_mode):
            return BlockDevice(target_path)

        return Loop.from_path(dbg, target_path, query)

    def activate(self, dbg, uri, domain):
        pass

    def attach(self, dbg, uri, domain):
        parsed_url = urllib.parse.urlparse(uri)
        query = urllib.parse.parse_qs(parsed_url.query)

        target_path = os.path.realpath(parsed_url.path)

        provider = self.get_provider(dbg, target_path, query)

        return {"implementations": [
            [
                "XenDisk",
                {
                    "backend_type": "vbd",
                    "params": provider.block_device(),
                    "extra": {}
                }
            ],
            [
                "BlockDevice",
                {
                    "path": provider.block_device()
                }
            ]
        ]}

    def deactivate(self, dbg, uri, domain):
        pass

    def detach(self, dbg, uri, domain):
        parsed_url = urllib.parse.urlparse(uri)

        file_path = os.path.realpath(parsed_url.path)

        if not(os.path.exists(file_path)):
            raise xapi.storage.api.volume.Volume_does_not_exist(file_path)

        provider = self.get_provider(dbg, file_path, {})
        provider.destroy(dbg)

    def open(self, dbg, uri, persistent):
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
