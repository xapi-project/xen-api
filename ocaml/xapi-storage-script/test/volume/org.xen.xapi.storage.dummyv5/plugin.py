#!/usr/bin/env python3

"""
 Copyright (C) Citrix Systems, Inc.
"""

import os
import sys
import xapi.storage.api.v5.plugin  # pylint: disable=no-name-in-module


class Implementation(xapi.storage.api.v5.plugin.Plugin_skeleton):

    def diagnostics(self, dbg):  # pylint: disable=unused-argument
        return "Dummy diagnostics"

    def query(self, dbg):  # pylint: disable=unused-argument
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
                "required_cluster_stack": [],
                "supported_image_formats": []}


if __name__ == "__main__":
    cmd = xapi.storage.api.v5.plugin.Plugin_commandline(Implementation())
    base = os.path.basename(sys.argv[0])
    if base == 'Plugin.diagnostics':
        cmd.diagnostics()
    elif base == 'Plugin.Query':
        cmd.query()
    else:
        raise xapi.storage.api.v5.plugin.Unimplemented(base)
