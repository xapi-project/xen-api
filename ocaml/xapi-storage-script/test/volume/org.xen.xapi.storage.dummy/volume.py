#!/usr/bin/env python3

"""
 Copyright (C) Citrix Systems, Inc.
"""

import uuid
import urllib.parse
import os
import sys
import xapi.storage.api.volume
import xapi

import plugin


class Implementation(xapi.storage.api.volume.Volume_skeleton):

    def create(self, dbg, sr, name, description, size):
        urllib.parse.urlparse(sr)
        voluuid = str(uuid.uuid4())
        return {
            "name": name,
            "description": description,
            "key": voluuid,
            "uuid": voluuid,
            "read_write": True,
            "virtual_size": 0,
            "physical_utilisation": 0,
            "uri": ["raw+file:///tmp/disk.raw"],
            "keys": {},
        }

    def destroy(self, dbg, sr, key):
        urllib.parse.urlparse(sr)
        return

    def stat(self, dbg, sr, key):
        urllib.parse.urlparse(sr)
        qr = plugin.Implementation().query(dbg)
        return {
                "name": qr['name'],
                "description": qr['description'],
                "key": key,
                "uuid": key,
                "read_write": True,
                "virtual_size": 0,
                "physical_utilisation": 0,
                "uri": ["raw+file:///tmp/disk.raw"],
                "keys": {},
        }


if __name__ == "__main__":
    cmd = xapi.storage.api.volume.Volume_commandline(Implementation())
    base = os.path.basename(sys.argv[0])
    if base == "Volume.create":
        cmd.create()
    elif base == "Volume.destroy":
        cmd.destroy()
    elif base == "Volume.stat":
        cmd.stat()
    else:
        raise xapi.storage.api.volume.Unimplemented(base)
