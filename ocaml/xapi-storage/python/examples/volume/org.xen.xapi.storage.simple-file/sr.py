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
import urllib
import urlparse

import xapi.storage.api.v5.volume
from xapi import InternalError
from xapi.storage import log
from xapi.storage.common import call
from xapi.storage.api.v5.volume import SR_skeleton, SR_does_not_exist

import volume

class Implementation(SR_skeleton):

    def probe(self, dbg, configuration):
        """
        [probe configuration]: can be used iteratively to narrow down configurations
        to use with SR.create, or to find existing SRs on the backing storage
        """
        # The only configuration required for this SR type is a path
        # to a mounted filesystem
        return []

    def create(self, dbg, uuid, configuration, name, description):
        """
        [create uuid configuration name description]: creates a fresh SR
        """

        # Some simple validation
        sr_path = configuration['path']
        if not os.path.exists(sr_path) or not os.path.isdir(sr_path):
            raise SR_does_not_exist(sr_path)

        # Add extra parameters back to the configuration as simple
        # storage. Could also go into a database or data file in the
        # SR storage
        configuration['uuid'] = uuid
        configuration['name'] = name
        configuration['description'] = description
        return configuration

    def attach(self, dbg, configuration):
        """
        [attach configuration]: attaches the SR to the local host. Once an SR is
        attached then volumes may be manipulated.
        """
        # As a simple "stateless" implementation, encode all the
        # configuration into the URI returned. This is passed back
        # into volume interface APIs and the stat and ls operations.
        return urlparse.urlunparse((
            'file',
            '',
            configuration['path'],
            '',
            urllib.urlencode(configuration, True),
            None))

    def detach(self, dbg, sr):
        """
        [detach sr]: detaches the SR, clearing up any associated resources.
        Once the SR is detached then volumes may not be manipulated.
        """
        # No action required to detach
        pass

    def destroy(self, dbg, sr):
        """
        [destroy sr]: destroys the [sr] and deletes any volumes associated
        with it. Note that an SR must be attached to be destroyed; otherwise
        Sr_not_attached is thrown.
        """
        # No action required to destroy
        pass

    def stat(self, dbg, sr):
        """
        [stat sr] returns summary metadata associated with [sr]. Note this
        call does not return details of sub-volumes, see SR.ls.
        """
        parsed_url = urlparse.urlparse(sr)
        config = urlparse.parse_qs(parsed_url.query)

        description = (config['description'][0]
                       if 'description' in config
                       else "")

        # Read the free and total space in the path
        statvfs = os.statvfs(parsed_url.path)
        psize = statvfs.f_blocks * statvfs.f_frsize
        fsize = statvfs.f_bfree * statvfs.f_frsize

        return {
            "sr": sr,
            "name": config['name'][0],
            "uuid": config['uuid'][0],
            "description": description,
            "free_space": fsize,
            "total_space": psize,
            "datasources": [],
            "clustered": False,
            "health": ['Healthy', '']}

    def set_name(self, dbg, sr, new_name):
        """
        [set_name sr new_name] changes the name of [sr]
        """
        # This won't work with separate persistent storage, database
        # or datafile in SR storage
        pass

    def set_description(self, dbg, sr, new_description):
        """
        [set_description sr new_description] changes the description of [sr]
        """
        # This won't work with separate persistent storage, database
        # or datafile in SR storage
        pass

    def ls(self, dbg, sr):
        """
        [ls sr] returns a list of volumes contained within an attached SR.
        """
        vol = volume.Implementation()
        return vol.ls(dbg, sr)


if __name__ == "__main__":
    log.log_call_argv()
    cmd = xapi.storage.api.v5.volume.SR_commandline(Implementation())
    base = os.path.basename(sys.argv[0])

    base_class, op = base.split('.')

    if base_class == 'SR':
        op = op.lower()
        fn = getattr(cmd, op, None)
        log.debug('Calling fn {}'.format(fn))
        assert(fn)
        fn()
    else:
        cmds = ['SR.{}'.format(x) for x in dir(cmd) if not x.startswith('_')]
        for name in cmds:
            print(name)
