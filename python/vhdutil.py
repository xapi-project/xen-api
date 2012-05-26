#!/usr/bin/python
# Copyright (C) 2006-2007 XenSource Ltd.
# Copyright (C) 2008-2009 Citrix Ltd.
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
# Helper functions pertaining to VHD operations
#


import os
import util
import errno

VHD_UTIL = "/usr/bin/vhd-util"
OPT_LOG_ERR = "--debug"
VHD_BLOCK_SIZE = 2 * 1024 * 1024
VHD_FOOTER_SIZE = 512
 
def calcOverheadEmpty(virtual_size):
    """Calculate the VHD space overhead (metadata size) for an empty VDI of
    size virtual_size"""
    overhead = 0
    size_mb = virtual_size / (1024 * 1024)

    # Footer + footer copy + header + possible CoW parent locator fields
    overhead = 3 * 1024

    # BAT 4 Bytes per block segment
    overhead += (size_mb / 2) * 4
    overhead = util.roundup(512, overhead)

    # BATMAP 1 bit per block segment
    overhead += (size_mb / 2) / 8
    overhead = util.roundup(4096, overhead)

    return overhead

def calcOverheadBitmap(virtual_size):
    num_blocks = virtual_size / VHD_BLOCK_SIZE
    if virtual_size % VHD_BLOCK_SIZE:
        num_blocks += 1
    return num_blocks * 4096

def calcOverheadFull(virtual_size):
    """Calculate the VHD space overhead for a full VDI of size virtual_size
    (this includes bitmaps, which constitute the bulk of the overhead)"""
    return calcOverheadEmpty(virtual_size) + calcOverheadBitmap(virtual_size)

def ioretry(cmd):
    return util.ioretry(lambda: util.pread2(cmd),
            errlist = [errno.EIO, errno.EAGAIN])

def coalesce(path):
    cmd = [VHD_UTIL, "coalesce", OPT_LOG_ERR, "-n", path]
    ioretry(cmd)

TAPDISK_UTIL = '/usr/sbin/td-util'

MAX_DISK_MB = 2 * 1024 * 1024
MAX_DISK_METADATA = 4092
VHD_SIZE_INC = 2 * 1024 * 1024

def create(size, path):
    assert (type(size) == type(0L))

    overhead = calcOverheadFull(size)

    mb = 1024L * 1024L
    size_mb = util.roundup(VHD_SIZE_INC, size) / mb
    if size_mb < 1 or (size_mb + (overhead / mb)) >= MAX_DISK_MB:
        raise 'VDI size must be between 1 MB and %d MB' % ((MAX_DISK_MB - MAX_DISK_METADATA) - 1)

    cmd = [TAPDISK_UTIL, "create", "vhd", str(size_mb), path]
    ioretry(cmd)

    cmd = [TAPDISK_UTIL, "query", "vhd", "-v", path]
    return long(ioretry(cmd)) * mb

def vhd_info_of_string(line):
    valueMap = line.split()
    if len(valueMap) < 1 or valueMap[0].find("vhd=") == -1:
        return None
    info = { "type": "vhd" }
    for keyval in valueMap:
        (key, val) = keyval.split('=')
        if key == "vhd":
            info["name"] = val[0:-len(".vhd")]
        elif key == "scan-error":
            util.SMlog("***** VHD scan error: %s" % line)
            return None
        elif key == "capacity":
            info["virt"] = int(val)
        elif key == "size":
            info["phys"] = int(val)
        elif key == "hidden":
            info["hidden"] = int(val)
        elif key == "parent" and val != "none":
            info["parent"] = val[0:-len(".vhd")]
    return info

def list(pattern):
    vhds = {}
    cmd = [VHD_UTIL, "scan", "-f", "-c", "-m", pattern]
    ret = ioretry(cmd)
    for line in ret.split('\n'):
        info = vhd_info_of_string(line)
        if info:
            vhds[info["name"]] = info
    return vhds

def make_leaf(child, parent):
    cmd = [TAPDISK_UTIL, "snapshot", "vhd", child, parent]
    ioretry(cmd)
    return list(child).values()[0]

