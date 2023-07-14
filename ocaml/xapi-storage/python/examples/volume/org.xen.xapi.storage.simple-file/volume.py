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
import glob
import json
import os
import sys
import uuid
import urllib
import urlparse

import xapi.storage.api.v5.volume
from xapi.storage import log


class Implementation(xapi.storage.api.v5.volume.Volume_skeleton):

    def parse_sr(self, sr_uri):
        parsed_url = urlparse.urlparse(sr_uri)
        config = urlparse.parse_qs(parsed_url.query)
        return parsed_url, config

    def create_volume_data(self, name, description, size, uris, uuid):
        return {
            'uuid': uuid,
            'key': uuid,
            'name': name,
            'description': description,
            'read_write': True,
            'virtual_size': size,
            'physical_utilisation': size,
            'uri': uris,
            'keys': {},
            'sharable': False
        }

    def volume_uris(self, sr_path, name, size):
        query = urllib.urlencode({'size': size}, True)
        return [urlparse.urlunparse(
            ('loop+blkback', None, os.path.join(sr_path, name),
             None, query, None))]

    def create(self, dbg, sr, name, description, size, sharable):
        """
        [create sr name description size] creates a new volume in [sr] with
        [name] and [description]. The volume will have size >= [size] i.e. it
        is always permissable for an implementation to round-up the volume to
        the nearest convenient block size
        """
        # No support for shareable mulit-access volumes in this SR
        assert(not sharable)

        parsed_url, config = self.parse_sr(sr)

        volume_uuid = str(uuid.uuid4())
        file_path = os.path.join(parsed_url.path, volume_uuid)

        with open(file_path, 'w') as f:
            os.ftruncate(f.fileno(), size)

        with open(file_path + '.inf', 'w') as json_f:
            meta = {
                'name': name,
                'description': description,
                'size': size
            }
            json.dump(meta, json_f)

        return self.create_volume_data(
            name, description,
            size, self.volume_uris(parsed_url.path, name, size),
            volume_uuid)

    def destroy(self, dbg, sr, key):
        """
        [destroy sr volume] removes [volume] from [sr]
        """
        parsed_url, config = self.parse_sr(sr)

        file_path = os.path.join(parsed_url.path, key)

        os.unlink(file_path + '.inf')
        os.unlink(file_path)

    def _stat_volume(self, sr_path, volume_id):
        file_path = os.path.join(sr_path, volume_id)

        with open(file_path + '.inf', 'r') as json_f:
            meta = json.load(json_f)

        return self.create_volume_data(
            meta['name'],
            meta['description'],
            meta['size'],
            self.volume_uris(sr_path, volume_id, meta['size']),
            volume_id)

    def stat(self, dbg, sr, key):
        """
        [stat sr volume] returns metadata associated with [volume].
        """
        parsed_url, config = self.parse_sr(sr)
        sr_path = parsed_url.path
        return self._stat_volume(sr_path, key)

    def set_name(self, dbg, sr, key, new_name):
        """
        [set_name sr key new_name] changes the name of [volume]
        """
        parsed_url, config = self.parse_sr(sr)

        file_path = os.path.join(parsed_url.path, key)

        with open(file_path + '.inf', 'r') as json_f:
            meta = json.load(json_f)
            meta['name'] = new_name

        with open(file_path + '.inf', 'w') as json_f:
            # Note this is not crash consistent, use temp file
            json.dump(meta, json_f)

    def set_description(self, dbg, sr, key, new_description):
        """
        [set_description sr key new_name] changes the description of [volume]
        """
        parsed_url, config = self.parse_sr(sr)

        file_path = os.path.join(parsed_url.path, key)

        with open(file_path + '.inf', 'r') as json_f:
            meta = json.load(json_f)
            meta['description'] = new_description

        with open(file_path + '.inf', 'w') as json_f:
            # Note this is not crash consistent, use temp file
            json.dump(meta, json_f)

    def set(self, dbg, sr, key, k, v):
        """
        [set sr volume key value] associates [key] with [value] in the
        metadata of [volume] Note these keys and values are not interpreted
        by the plugin; they are intended for the higher-level software only.
        """
        pass

    def resize(self, dbg, sr, key, new_size):
        """
        [resize sr volume new_size] enlarges [volume] to be at least
        [new_size].
        """
        parsed_url, config = self.parse_sr(sr)

        file_path = os.path.join(parsed_url.path, key)

        with open(file_path + '.inf', 'r') as json_f:
            meta = json.load(json_f)

        if new_size < meta['size']:
            raise xapi.XenAPIException("SR_BACKEND_FAILURE_79",
                                       ["VDI Invalid size",
                                        "shrinking not allowed"])

        with open(file_path, 'w') as f:
            os.ftruncate(f.fileno(), new_size)

        meta['size'] = new_size
        with open(file_path + '.inf', 'w') as json_f:
            # Note this is not crash consistent, use temp file
            json.dump(meta, json_f)

    def ls(self, dbg, sr):
        """
        [ls sr] lists the volumes from [sr]
        """
        parsed_url = urlparse.urlparse(sr)
        sr_path = parsed_url.path
        files = glob.glob(os.path.join(sr_path, '*.inf'))
        log.debug('files to list {}'.format(files))
        return [self._stat_volume(
            sr_path, os.path.basename(x[:-4])) for x in files]


if __name__ == "__main__":
    log.log_call_argv()
    cmd = xapi.storage.api.v5.volume.Volume_commandline(Implementation())
    base = os.path.basename(sys.argv[0])

    base_class, op = base.split('.')

    if base_class == 'Volume':
        op = op.lower()
        fn = getattr(cmd, op, None)
        log.debug('Calling fn {}'.format(fn))
        assert(fn)
        fn()
    else:
        cmds = ['Volume.{}'.format(x) for x in dir(cmd) if not x.startswith('_')]
        for name in cmds:
            print(name)
