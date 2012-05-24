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
# Miscellaneous utility functions
#

import os, re, sys, subprocess, shutil, tempfile, commands, signal
import time, datetime
import errno, socket
import statvfs
import stat
import syslog
import resource
import exceptions
import traceback
import glob

IORETRY_MAX = 20 # retries
IORETRY_PERIOD = 1.0 # seconds

SYSLOG_ON = False
LOGGING = True
LOGFILE = '/var/log/SMlog'
STDERRLOG = LOGFILE

def roundup(divisor, value):
    """Retruns the rounded up value so it is divisible by divisor."""

    if value == 0:
        value = 1
    if value % divisor != 0:
        return ((value / divisor) + 1) * divisor
    return value

def SMlog(str, logfile=LOGFILE, facility="SM"):
    if SYSLOG_ON:
        syslog.openlog(facility)
        syslog.syslog("[%d] %s" % (os.getpid(), str))
        syslog.closelog()
    if LOGGING:
        f=open(logfile, 'a')
        f.write("[%d] %s\t%s\n" % (os.getpid(),datetime.datetime.now(),str))
        f.close()
        
def doexec(args, inputtext=None):
    """Execute a subprocess, then return its return code, stdout and stderr"""
    proc = subprocess.Popen(args,stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE,close_fds=True)
    (stdout,stderr) = proc.communicate(inputtext)
    rc = proc.returncode
    return (rc,stdout,stderr)

def is_string(value):
    return isinstance(value,basestring)

class CommandException(Exception):
    def __init__(self, code, cmd = "", reason='exec failed'):
        self.code = code
        self.cmd = cmd
        self.reason = reason
        Exception.__init__(self, code)

# These are partially tested functions that replicate the behaviour of
# the original pread,pread2 and pread3 functions. Potentially these can
# replace the original ones at some later date.
#
# cmdlist is a list of either single strings or pairs of strings. For
# each pair, the first component is passed to exec while the second is
# written to the logs.
def pread(cmdlist, close_stdin = False, scramble = None, expect_rc = 0,
        quiet = False):
    cmdlist_for_exec = []
    cmdlist_for_log = []
    for item in cmdlist:
        if is_string(item):
            cmdlist_for_exec.append(item)
            if scramble:
                if item.find(scramble) != -1:
                    cmdlist_for_log.append("<filtered out>")
                else:
                    cmdlist_for_log.append(item)
            else:
                cmdlist_for_log.append(item)
        else:
            cmdlist_for_exec.append(item[0])
            cmdlist_for_log.append(item[1])

    if not quiet:
        SMlog(cmdlist_for_log)
    (rc,stdout,stderr) = doexec(cmdlist_for_exec)
    if rc != expect_rc:
        SMlog("FAILED in util.pread: (rc %d) stdout: '%s', stderr: '%s'" % \
                (rc, stdout, stderr))
        if quiet:
            SMlog("Command was: %s" % cmdlist_for_log)
        if '' == stderr:
            stderr = stdout
        raise CommandException(rc, str(cmdlist), stderr.strip())
    if not quiet:
        SMlog("  pread SUCCESS")
    return stdout

#Read STDOUT from cmdlist and discard STDERR output
def pread2(cmdlist, quiet = False):
    return pread(cmdlist, quiet = quiet)

def listdir(path, quiet = False):
    cmd = ["ls", path, "-1", "--color=never"]
    try:
        text = pread2(cmd, quiet = quiet)[:-1]
        if len(text) == 0:
            return []
        return text.split('\n')
    except CommandException, inst:
        if inst.code == errno.ENOENT:
            raise CommandException(errno.EIO, inst.cmd, inst.reason)
        else:
            raise CommandException(inst.code, inst.cmd, inst.reason)

def gen_uuid():
    cmd = ["uuidgen", "-r"]
    return pread(cmd)[:-1]

def match_uuid(s):
    regex = re.compile("^[0-9a-f]{8}-(([0-9a-f]{4})-){3}[0-9a-f]{12}")
    return regex.search(s, 0)

def exactmatch_uuid(s):
    regex = re.compile("^[0-9a-f]{8}-(([0-9a-f]{4})-){3}[0-9a-f]{12}$")
    return regex.search(s, 0)

def ioretry(f, errlist=[errno.EIO], maxretry=IORETRY_MAX, period=IORETRY_PERIOD, **ignored):
    retries = 0
    while True:
        try:
            return f()
        except OSError, inst:
             errno = int(inst.errno)
             if not errno in errlist:
                 raise CommandException(errno, str(f), "OSError")
        except CommandException, inst:
            if not int(inst.code) in errlist:
                raise

        retries += 1
        if retries >= maxretry:
            break
        
        time.sleep(period)

    raise inst

def ioretry_stat(f, maxretry=IORETRY_MAX):
    # this ioretry is similar to the previous method, but
    # stat does not raise an error -- so check its return
    retries = 0
    while retries < maxretry:
        stat = f()
        if stat[statvfs.F_BLOCKS] != -1:
            return stat
        time.sleep(1)
        retries += 1
    raise CommandException(errno.EIO, str(f))

def pathexists(path):
    try:
        os.stat(path)
        return True
    except OSError, inst:
        if inst.errno == errno.EIO:
            raise CommandException(errno.EIO, "os.stat(%s)" % path, "failed")
        return False

def get_real_path(path):
    "Follow symlinks to the actual file"
    absPath = path
    directory = ''
    while os.path.islink(absPath):
        directory = os.path.dirname(absPath)
        absPath = os.readlink(absPath)
        absPath = os.path.join(directory, absPath)
    return absPath

def isdir(path):
    try:
        st = os.stat(path)
        return stat.S_ISDIR(st.st_mode)
    except OSError, inst:
        if inst.errno == errno.EIO:
            raise CommandException(errno.EIO, "os.stat(%s)" % path, "failed")
        return False

def get_fs_size(path):
    st = ioretry_stat(lambda: os.statvfs(path))
    return st[statvfs.F_BLOCKS] * st[statvfs.F_FRSIZE]

def get_fs_utilisation(path):
    st = ioretry_stat(lambda: os.statvfs(path))
    return (st[statvfs.F_BLOCKS] - st[statvfs.F_BFREE]) * \
            st[statvfs.F_FRSIZE]

def ismount(path):
    """Test whether a path is a mount point"""
    try:
        s1 = os.stat(path)
        s2 = os.stat(os.path.join(path, '..'))
    except OSError, inst:
        raise CommandException(inst.errno, "os.stat")
    dev1 = s1.st_dev
    dev2 = s2.st_dev
    if dev1 != dev2:
        return True     # path/.. on a different device as path
    ino1 = s1.st_ino
    ino2 = s2.st_ino
    if ino1 == ino2:
        return True     # path/.. is the same i-node as path
    return False

def makedirs(name, mode=0777):
    head, tail = os.path.split(name)
    if not tail:
        head, tail = os.path.split(head)
    if head and tail and not pathexists(head):
        makedirs(head, mode)
        if tail == os.curdir:
            return
    os.mkdir(name, mode)

