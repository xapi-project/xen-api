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

import xapi.storage.api.v5.plugin
from xapi.storage import log


class Implementation(xapi.storage.api.v5.plugin.Plugin_skeleton):
    def query(self, dbg):
        return {
            "plugin": "loop+blkback",
            "name": "Sample loop + blkback datapath",
            "description": ("This plugin is an example using "
                            "loop devices (from losetup) and "
                            "blkback to create virtual block devices"),
            "vendor": "Citrix",
            "copyright": "(C) 2019 Citrix Inc",
            "version": "3.0",
            "required_api_version": "5.0",
            "features": [],
            "configuration": {},
            "required_cluster_stack": []}


if __name__ == "__main__":
    log.log_call_argv()
    cmd = xapi.storage.api.v5.plugin.Plugin_commandline(Implementation())
    base = os.path.basename(sys.argv[0])

    base_class, op = base.split('.')

    if base_class == 'Plugin':
        op = op.lower()
        fn = getattr(cmd, op, None)
        fn()
    else:
        cmds = ['Plugin.{}'.format(x) for x in dir(cmd) if not x.startswith('_')]
        for name in cmds:
            print(name)
