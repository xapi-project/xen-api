#!/usr/bin/env python3

"""
 Copyright (C) Citrix Systems, Inc.
"""

import os
import sys
import urllib.parse
import xapi.storage.api.volume

import plugin


class Implementation(xapi.storage.api.volume.SR_skeleton):

    def attach(self, dbg, uri):
        return "file:///tmp/dummy"

    def create(self, dbg, uri, name, description, configuration):
        return

    def detach(self, dbg, sr):
        urllib.parse.urlparse(sr)
        return

    def ls(self, dbg, sr):
        urllib.parse.urlparse(sr)
        qr = plugin.Implementation().query(dbg)
        return [{
            "name": qr['name'],
            "description": qr['description'],
            "key": "file1",
            "uuid": "file1",
            "read_write": True,
            "virtual_size": 0,
            "physical_utilisation": 0,
            "uri": ["raw+file:///tmp/disk.raw"],
            "keys": {},
            }]

    def stat(self, dbg, sr):
        urllib.parse.urlparse(sr)
        qr = plugin.Implementation().query(dbg)
        return {
            "sr": sr,
            "name": qr['name'],
            "description": qr['description'],
            "total_space": 0,
            "free_space": 0,
            "datasources": [],
            "clustered": False,
            "health": ["Healthy", ""]
        }


if __name__ == "__main__":
    cmd = xapi.storage.api.volume.SR_commandline(Implementation())
    base = os.path.basename(sys.argv[0])
    if base == 'SR.attach':
        cmd.attach()
    elif base == 'SR.create':
        cmd.create()
    elif base == 'SR.detach':
        cmd.detach()
    elif base == 'SR.ls':
        cmd.ls()
    elif base == 'SR.stat':
        cmd.stat()
    else:
        raise xapi.storage.api.volume.Unimplemented(base)
