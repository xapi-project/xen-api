#!/usr/bin/env python

"""
 Copyright (C) Citrix Systems, Inc.
"""

import uuid
import urlparse
import os
import sys
import xapi.storage.api.v5.volume
import xapi

import plugin


class Implementation(xapi.storage.api.v5.volume.Volume_skeleton):

    def create(self, dbg, sr, name, description, size, sharable):
        urlparse.urlparse(sr)
        voluuid = str(uuid.uuid4())
        return {
            "name": name,
            "description": description,
            "key": voluuid,
            "uuid": voluuid,
            "read_write": True,
            "sharable": sharable,
            "virtual_size": 0,
            "physical_utilisation": 0,
            "uri": ["raw+file:///tmp/disk.raw"],
            "keys": {},
        }

    def destroy(self, dbg, sr, key):
        urlparse.urlparse(sr)
        return

    def stat(self, dbg, sr, key):
        urlparse.urlparse(sr)
        qr = plugin.Implementation().query(dbg)
        return {
                "name": qr['name'],
                "description": qr['description'],
                "key": key,
                "uuid": key,
                "read_write": True,
                "virtual_size": 0,
                "physical_utilisation": 0,
                "sharable": False,
                "uri": ["raw+file:///tmp/disk.raw"],
                "keys": {},
        }

    def set(self, dbg, sr, key, k, v):
        pass

    def unset(self, dbg, sr, key, k):
        pass


if __name__ == "__main__":
    cmd = xapi.storage.api.v5.volume.Volume_commandline(Implementation())
    base = os.path.basename(sys.argv[0])
    if base == "Volume.create":
        cmd.create()
    elif base == "Volume.destroy":
        cmd.destroy()
    elif base == "Volume.stat":
        cmd.stat()
    elif base == "Volume.set":
	cmd.set()
    elif base == "Volume.unset":
	cmd.unset()
    else:
        raise xapi.storage.api.v5.volume.Unimplemented(base)
