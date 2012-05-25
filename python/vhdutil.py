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

MAX_CHAIN_SIZE = 30 # max VHD parent chain size
VHD_UTIL = "/usr/bin/vhd-util"
OPT_LOG_ERR = "--debug"
VHD_BLOCK_SIZE = 2 * 1024 * 1024
VHD_FOOTER_SIZE = 512
 
VDI_TYPE_VHD = 'vhd'
VDI_TYPE_RAW = 'aio'

FILE_EXTN_VHD = ".vhd"
FILE_EXTN_RAW = ".raw"
FILE_EXTN = {
        VDI_TYPE_VHD: FILE_EXTN_VHD,
        VDI_TYPE_RAW: FILE_EXTN_RAW
}


class VHDInfo:
    uuid = ""
    path = ""
    sizeVirt = -1
    sizePhys = -1
    hidden = False
    parentUuid = ""
    parentPath = ""
    error = 0

    def __init__(self, uuid):
        self.uuid = uuid


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

def fullSizeVHD(virtual_size):
    return virtual_size + calcOverheadFull(virtual_size)

def ioretry(cmd):
    return util.ioretry(lambda: util.pread2(cmd),
            errlist = [errno.EIO, errno.EAGAIN])

def getVHDInfo(path, extractUuidFunction, includeParent = True):
    """Get the VHD info. The parent info may optionally be omitted: vhd-util
    tries to verify the parent by opening it, which results in error if the VHD
    resides on an inactive LV"""
    opts = "-vsf"
    if includeParent:
        opts += "p"
    cmd = [VHD_UTIL, "query", OPT_LOG_ERR, opts, "-n", path]
    ret = ioretry(cmd)
    fields = ret.strip().split('\n')
    uuid = extractUuidFunction(path)
    vhdInfo = VHDInfo(uuid)
    vhdInfo.sizeVirt = int(fields[0]) * 1024 * 1024
    vhdInfo.sizePhys = int(fields[1])
    nextIndex = 2
    if includeParent:
        if fields[nextIndex].find("no parent") == -1:
            vhdInfo.parentPath = fields[nextIndex]
            vhdInfo.parentUuid = extractUuidFunction(fields[nextIndex])
        nextIndex += 1
    vhdInfo.hidden = int(fields[nextIndex].replace("hidden: ", ""))
    vhdInfo.path = path
    return vhdInfo

def getAllVHDs(pattern, extractUuidFunction, vgName = None, \
        parentsOnly = False):
    vhds = dict()
    cmd = [VHD_UTIL, "scan", "-f", "-c", "-m", pattern]
    if vgName:
        cmd.append("-l")
        cmd.append(vgName)
    if parentsOnly:
        cmd.append("-a")
    ret = ioretry(cmd)
    for line in ret.split('\n'):
        vhdInfo = _parseVHDInfo(line, extractUuidFunction)
        if vhdInfo:
            vhds[vhdInfo.uuid] = vhdInfo
    return vhds

def getParentChain(lvName, extractUuidFunction, vgName):
    """Get the chain of all VHD parents of 'path'. Safe to call for raw VDI's
    as well"""
    chain = dict()
    vdis = getAllVHDs(lvName, extractUuidFunction, vgName, True)
    for uuid, vdi in vdis.iteritems():
        chain[uuid] = vdi.path
    #util.SMlog("Parent chain for %s: %s" % (lvName, chain))
    return chain

def getParent(path, extractUuidFunction):
    cmd = [VHD_UTIL, "query", OPT_LOG_ERR, "-p", "-n", path]
    ret = ioretry(cmd)
    if ret.find("query failed") != -1 or ret.find("Failed opening") != -1:
        raise util.SMException("VHD query returned %s" % ret)
    if ret.find("no parent") != -1:
        return None
    return extractUuidFunction(ret)

def setParent(path, parentPath, parentRaw):
    realPPath = util.get_real_path(parentPath)
    cmd = [VHD_UTIL, "modify", OPT_LOG_ERR, "-p", realPPath, "-n", path]
    if parentRaw:
        cmd.append("-m")
    ioretry(cmd)

def getHidden(path):
    cmd = [VHD_UTIL, "query", OPT_LOG_ERR, "-f", "-n", path]
    ret = ioretry(cmd)
    hidden = int(ret.split(':')[-1].strip())
    return hidden

def setHidden(path, hidden = True):
    opt = "1"
    if not hidden:
        opt = "0"
    cmd = [VHD_UTIL, "set", OPT_LOG_ERR, "-n", path, "-f", "hidden", "-v", opt]
    ret = ioretry(cmd)

def getSizeVirt(path):
    cmd = [VHD_UTIL, "query", OPT_LOG_ERR, "-v", "-n", path]
    ret = ioretry(cmd)
    size = long(ret) * 1024 * 1024
    return size

def setSizeVirt(path, size, jFile):
    "resize VHD offline"
    size_mb = size / 1024 /1024
    cmd = [VHD_UTIL, "resize", OPT_LOG_ERR, "-s", str(size_mb), "-n", path,
            "-j", jFile]
    ioretry(cmd)

def setSizeVirtFast(path, size):
    "resize VHD online"
    size_mb = size / 1024 /1024
    cmd = [VHD_UTIL, "resize", OPT_LOG_ERR, "-s", str(size_mb), "-n", path, "-f"]
    ioretry(cmd)

def getMaxResizeSize(path):
    """get the max virtual size for fast resize"""
    cmd = [VHD_UTIL, "query", OPT_LOG_ERR, "-S", "-n", path]
    ret = ioretry(cmd)
    return int(ret)

def getSizePhys(path):
    cmd = [VHD_UTIL, "query", OPT_LOG_ERR, "-s", "-n", path]
    ret = ioretry(cmd)
    return int(ret)

def setSizePhys(path, size):
    "set physical utilisation (applicable to VHD's on fixed-size files)"
    cmd = [VHD_UTIL, "modify", OPT_LOG_ERR, "-s", str(size), "-n", path]
    ioretry(cmd)

def killData(path):
    "zero out the disk (kill all data inside the VHD file)"
    cmd = [VHD_UTIL, "modify", OPT_LOG_ERR, "-z", "-n", path]
    ioretry(cmd)

def getDepth(path):
    "get the VHD parent chain depth"
    cmd = [VHD_UTIL, "query", OPT_LOG_ERR, "-d", "-n", path]
    text = ioretry(cmd)
    depth = -1
    if text.startswith("chain depth:"):
        depth = int(text.split(':')[1].strip())
    return depth

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

def make_leaf(child, parent):
    cmd = [TAPDISK_UTIL, "snapshot", "vhd", child, parent]
    ioretry(cmd)

def snapshot(path, parent, parentRaw, msize = 0, checkEmpty = True):
    cmd = [VHD_UTIL, "snapshot", OPT_LOG_ERR, "-n", path, "-p", parent]
    if parentRaw:
        cmd.append("-m")
    if msize:
        cmd.append("-S")
        cmd.append(str(msize))
    if not checkEmpty:
        cmd.append("-e")
    text = ioretry(cmd)

def check(path):
    cmd = [VHD_UTIL, "check", OPT_LOG_ERR, "-n", path]
    try:
        ioretry(cmd)
        return True
    except util.CommandException:
        return False

def revert(path, jFile):
    cmd = [VHD_UTIL, "revert", OPT_LOG_ERR, "-n", path, "-j", jFile]
    ioretry(cmd)

def _parseVHDInfo(line, extractUuidFunction):
    vhdInfo = None
    valueMap = line.split()
    if len(valueMap) < 1 or valueMap[0].find("vhd=") == -1:
        return None
    for keyval in valueMap:
        (key, val) = keyval.split('=')
        if key == "vhd":
            uuid = extractUuidFunction(val)
            if not uuid:
                util.SMlog("***** malformed output, no UUID: %s" % valueMap)
                return None
            vhdInfo = VHDInfo(uuid)
            vhdInfo.path = val
        elif key == "scan-error":
            vhdInfo.error = line
            util.SMlog("***** VHD scan error: %s" % line)
            break
        elif key == "capacity":
            vhdInfo.sizeVirt = int(val)
        elif key == "size":
            vhdInfo.sizePhys = int(val)
        elif key == "hidden":
            vhdInfo.hidden = int(val)
        elif key == "parent" and val != "none":
            vhdInfo.parentPath = val
            vhdInfo.parentUuid = extractUuidFunction(val)
    return vhdInfo

def _getVHDParentNoCheck(path):
    cmd = ["vhd-util", "read", "-p", "-n", "%s" % path]
    text = util.pread(cmd)
    util.SMlog(text)
    for line in text.split('\n'):
        if line.find("decoded name :") != -1:
            val = line.split(':')[1].strip()
            vdi = val.replace("--", "-")[-40:]
            if vdi[1:].startswith("LV-"):
                vdi = vdi[1:]
            return vdi
    return None
