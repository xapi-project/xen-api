#!/usr/bin/env python

"""
 Copyright (C) Citrix Systems, Inc.
"""

import os
import sys
import xapi.storage.api.plugin


class Implementation(xapi.storage.api.plugin.Plugin_skeleton):

    def diagnostics(self, dbg):
        return "Dummy diagnostics"

    def query(self, dbg):
        return {
                "plugin": "dummy",
                "name": "dummy SR plugin",
                "description": ("Dummy v5 SR for unit tests."),
                "vendor": "Citrix Systems Inc",
                "copyright": "(C) 2018 Citrix Inc",
                "version": "1.0",
                "required_api_version": "5.0",
                "features": [
                    "SR_ATTACH",
                    "SR_DETACH",
                    "SR_CREATE",
                    "SR_PROBE",
                    "VDI_CREATE",
                    "VDI_DESTROY"],
                "configuration": {},
                "required_cluster_stack": []}


if __name__ == "__main__":
    cmd = xapi.storage.api.plugin.Plugin_commandline(Implementation())
    base = os.path.basename(sys.argv[0])
    if base == 'Plugin.diagnostics':
        cmd.diagnostics()
    elif base == 'Plugin.Query':
        cmd.query()
    else:
        raise xapi.storage.api.plugin.Unimplemented(base)
