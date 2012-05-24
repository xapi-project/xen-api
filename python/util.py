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

import os, re, sys, popen2, subprocess, shutil, tempfile, commands, signal
import time, datetime
import errno, socket
import xml.dom.minidom
import SR, scsiutil
import statvfs
import stat
import xs_errors
import XenAPI,xmlrpclib
import base64
import syslog
import resource
import exceptions
import traceback
import glob

NO_LOGGING_STAMPFILE='/etc/xensource/no_sm_log'
ENABLE_SYSLOG_STAMPFILE='/etc/xensource/yes_sm_syslog'

IORETRY_MAX = 20 # retries
IORETRY_PERIOD = 1.0 # seconds

SYSLOG_ON = os.path.exists(ENABLE_SYSLOG_STAMPFILE)
LOGGING = not (os.path.exists(NO_LOGGING_STAMPFILE))
LOGFILE = '/var/log/SMlog'
VMPR_LOGFILE = '/var/log/VMPRlog'
STDERRLOG = LOGFILE
ISCSI_REFDIR = '/var/run/sr-ref'

CMD_DD = "/bin/dd"

FIST_PAUSE_PERIOD = 30 # seconds

class SMException(Exception):
    """Base class for all SM exceptions for easier catching & wrapping in 
    XenError"""
    pass

class CommandException(SMException):
    def __init__(self, code, cmd = "", reason='exec failed'):
        self.code = code
        self.cmd = cmd
        self.reason = reason
        Exception.__init__(self, code)

class SRBusyException(SMException):
    """The SR could not be locked"""
    pass

def logException(tag):
    info = sys.exc_info()
    if info[0] == exceptions.SystemExit:
        # this should not be happening when catching "Exception", but it is
        sys.exit(0)
    tb = reduce(lambda a, b: "%s%s" % (a, b), traceback.format_tb(info[2]))
    str = "***** %s: EXCEPTION %s, %s\n%s" % (tag, info[0], info[1], tb)
    SMlog(str)

def roundup(divisor, value):
    """Retruns the rounded up value so it is divisible by divisor."""

    if value == 0:
        value = 1
    if value % divisor != 0:
        return ((value / divisor) + 1) * divisor
    return value

def to_plain_string(obj):
    if obj is None:
        return None
    if type(obj) == str:
        return obj
    if type(obj) == unicode:
        return obj.encode("utf-8")
    return str(obj)

def shellquote(arg):
    return '"%s"' % arg.replace('"', '\\"')

def make_WWN(name):
    hex_prefix = name.find("0x")
    if (hex_prefix >=0):
        name = name[name.find("0x")+2:len(name)]
    # inject dashes for each nibble
    if (len(name) == 16): #sanity check
        name = name[0:2] + "-" + name[2:4] + "-" + name[4:6] + "-" + \
               name[6:8] + "-" + name[8:10] + "-" + name[10:12] + "-" + \
               name[12:14] + "-" + name[14:16]
    return name

def SMlog(str, logfile=LOGFILE, facility="SM"):
    if SYSLOG_ON:
        syslog.openlog(facility)
        syslog.syslog("[%d] %s" % (os.getpid(), str))
        syslog.closelog()
    if LOGGING:
        f=open(logfile, 'a')
        f.write("[%d] %s\t%s\n" % (os.getpid(),datetime.datetime.now(),str))
        f.close()
        
def VMPRlog(str, logfile=VMPR_LOGFILE, facility="VMPR"):
    SMlog(str, logfile, facility)

def _getDateString():
    d = datetime.datetime.now()
    t = d.timetuple()
    return "%s-%s-%s:%s:%s:%s" % \
          (t[0],t[1],t[2],t[3],t[4],t[5])

def doexec(args, inputtext=None):
    """Execute a subprocess, then return its return code, stdout and stderr"""
    proc = subprocess.Popen(args,stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE,close_fds=True)
    (stdout,stderr) = proc.communicate(inputtext)
    rc = proc.returncode
    return (rc,stdout,stderr)

def is_string(value):
    return isinstance(value,basestring)

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

#Read STDOUT from cmdlist, feeding 'text' to STDIN
def pread3(cmdlist, text):
    SMlog(cmdlist)
    (rc,stdout,stderr) = doexec(cmdlist,text)
    if rc:
        SMlog("FAILED in util.pread3: (errno %d) stdout: '%s', stderr: '%s'" % \
                (rc, stdout, stderr))
        if '' == stderr:
            stderr = stdout
        raise CommandException(rc, str(cmdlist), stderr.strip())
    SMlog("  pread3 SUCCESS")
    return stdout

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

def start_log_entry(srpath, path, args):
    logstring = str(datetime.datetime.now())
    logstring += " log: "
    logstring += srpath 
    logstring +=  " " + path
    for element in args:
        logstring += " " + element
    try:
        file = open(srpath + "/filelog.txt", "a")
        file.write(logstring)
        file.write("\n")
        file.close()
    except:
        pass
        # failed to write log ... 

def end_log_entry(srpath, path, args):
    # for teminating, use "error" or "done"
    logstring = str(datetime.datetime.now())
    logstring += " end: "
    logstring += srpath 
    logstring +=  " " + path
    for element in args:
        logstring += " " + element
    try:
        file = open(srpath + "/filelog.txt", "a")
        file.write(logstring)
        file.write("\n")
        file.close()
    except:
        pass
        # failed to write log ... 
    # for now print
    # print "%s" % logstring

def rotate_string(x, n):
    transtbl = ""
    for a in range(0, 256):
        transtbl = transtbl + chr(a)
    transtbl = transtbl[n:] + transtbl[0:n]
    return x.translate(transtbl)

def untransform_string(str, remove_trailing_nulls=False):
    """De-obfuscate string. To cope with an obfuscation bug in Rio, the argument
    remove_trailing_nulls should be set to True"""
    tmp = base64.decodestring(str)
    if remove_trailing_nulls:
        tmp = tmp.rstrip('\x00')
    return rotate_string(tmp, -13)

def transform_string(str):
    """Re-obfuscate string"""
    tmp = rotate_string(str, 13)
    return base64.encodestring(tmp)

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

def sr_get_driver_info(driver_info):
    results = {}
    # first add in the vanilla stuff
    for key in [ 'name', 'description', 'vendor', 'copyright', \
                 'driver_version', 'required_api_version' ]:
        results[key] = driver_info[key]
    # add the capabilities (xmlrpc array)
    # enforcing activate/deactivate for blktap2
    caps = driver_info['capabilities']
    if "ATOMIC_PAUSE" in caps:
        for cap in ("VDI_ACTIVATE", "VDI_DEACTIVATE"):
            if not cap in caps:
                caps.append(cap)
    elif "VDI_ACTIVATE" in caps or "VDI_DEACTIVATE" in caps:
        SMlog("Warning: vdi_[de]activate present for %s" % driver_info["name"])

    results['capabilities'] = caps
    # add in the configuration options
    options = []
    for option in driver_info['configuration']:
        options.append({ 'key': option[0], 'description': option[1] })
    results['configuration'] = options
    return xmlrpclib.dumps((results,), "", True)

def return_nil():
    return xmlrpclib.dumps((None,), "", True, allow_none=True)

def SRtoXML(SRlist):
    dom = xml.dom.minidom.Document()
    driver = dom.createElement("SRlist")
    dom.appendChild(driver)

    for key in SRlist.keys():
        dict = SRlist[key]
        entry = dom.createElement("SR")
        driver.appendChild(entry)
        
        e = dom.createElement("UUID")
        entry.appendChild(e)
        textnode = dom.createTextNode(key)
        e.appendChild(textnode)

        if dict.has_key('size'):
            e = dom.createElement("Size")
            entry.appendChild(e)
            textnode = dom.createTextNode(str(dict['size']))
            e.appendChild(textnode)
            
        if dict.has_key('storagepool'):
            e = dom.createElement("StoragePool")
            entry.appendChild(e)
            textnode = dom.createTextNode(str(dict['storagepool']))
            e.appendChild(textnode)
            
        if dict.has_key('aggregate'):
            e = dom.createElement("Aggregate")
            entry.appendChild(e)
            textnode = dom.createTextNode(str(dict['aggregate']))
            e.appendChild(textnode)
            
    return dom.toprettyxml()

def pathexists(path):
    try:
        os.stat(path)
        return True
    except OSError, inst:
        if inst.errno == errno.EIO:
            raise CommandException(errno.EIO, "os.stat(%s)" % path, "failed")
        return False

def create_secret(session, secret):
    ref = session.xenapi.secret.create({'value' : secret})
    return session.xenapi.secret.get_uuid(ref)

def get_secret(session, uuid):
    try:
        ref = session.xenapi.secret.get_by_uuid(uuid)
        return session.xenapi.secret.get_value(ref)
    except:
        raise xs_errors.XenError('InvalidSecret', opterr='Unable to look up secret [%s]' % uuid)

def get_real_path(path):
    "Follow symlinks to the actual file"
    absPath = path
    directory = ''
    while os.path.islink(absPath):
        directory = os.path.dirname(absPath)
        absPath = os.readlink(absPath)
        absPath = os.path.join(directory, absPath)
    return absPath

def wait_for_path(path,timeout):
    for i in range(0,timeout):
        if len(glob.glob(path)):
            return True
        time.sleep(1)
    return False

def wait_for_nopath(path,timeout):
    for i in range(0,timeout):
        if not os.path.exists(path):
            return True
        time.sleep(1)
    return False

def wait_for_path_multi(path,timeout):
    for i in range(0,timeout):
        paths = glob.glob(path)
        SMlog( "_wait_for_paths_multi: paths = %s" % paths )
        if len(paths):
            SMlog( "_wait_for_paths_multi: return first path: %s" % paths[0] )
            return paths[0]
        time.sleep(1)
    return ""

def isdir(path):
    try:
        st = os.stat(path)
        return stat.S_ISDIR(st.st_mode)
    except OSError, inst:
        if inst.errno == errno.EIO:
            raise CommandException(errno.EIO, "os.stat(%s)" % path, "failed")
        return False

def get_single_entry(path):
    f = open(path, 'r')
    line = f.readline()
    f.close()
    return line.rstrip()

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

def zeroOut(path, fromByte, bytes):
    """write 'bytes' zeros to 'path' starting from fromByte (inclusive)"""
    blockSize = 4096

    fromBlock = fromByte / blockSize
    if fromByte % blockSize:
        fromBlock += 1
        bytesBefore = fromBlock * blockSize - fromByte
        if bytesBefore > bytes:
            bytesBefore = bytes
        bytes -= bytesBefore
        cmd = [CMD_DD, "if=/dev/zero", "of=%s" % path, "bs=1", \
                "seek=%s" % fromByte, "count=%s" % bytesBefore]
        try:
            text = pread2(cmd)
        except CommandException:
            return False

    blocks = bytes / blockSize
    bytes -= blocks * blockSize
    fromByte = fromBlock + blocks * blockSize
    if blocks:
        cmd = [CMD_DD, "if=/dev/zero", "of=%s" % path, "bs=%s" % blockSize, \
                "seek=%s" % fromBlock, "count=%s" % blocks]
        try:
            text = pread2(cmd)
        except CommandException:
            return False

    if bytes:
        cmd = [CMD_DD, "if=/dev/zero", "of=%s" % path, "bs=1", \
                "seek=%s" % fromByte, "count=%s" % bytes]
        try:
            text = pread2(cmd)
        except CommandException:
            return False

    return True

def match_rootdev(s):
    regex = re.compile("^PRIMARY_DISK")
    return regex.search(s, 0)

def getrootdev():
    filename = '/etc/xensource-inventory'
    try:
        f = open(filename, 'r')
    except:
        raise xs_errors.XenError('EIO', \
              opterr="Unable to open inventory file [%s]" % filename)
    rootdev = ''
    for line in filter(match_rootdev, f.readlines()):
        rootdev = line.split("'")[1]
    if not rootdev:
        raise xs_errors.XenError('NoRootDev')
    return rootdev

def getrootdevID():
    rootdev = getrootdev()
    try:
        rootdevID = scsiutil.getSCSIid(rootdev)
    except:
        SMlog("util.getrootdevID: Unable to verify serial or SCSIid of device: %s" \
                   % rootdev)
        return ''

    if not len(rootdevID):
        SMlog("util.getrootdevID: Unable to identify scsi device [%s] via scsiID" \
                   % rootdev)

    return rootdevID

def get_localAPI_session():
    # First acquire a valid session
    session = XenAPI.xapi_local()
    try:
        session.xenapi.login_with_password('root','')
    except:
        raise xs_errors.XenError('APISession')
    return session

def get_this_host():
    uuid = None
    f = open("/etc/xensource-inventory", 'r')
    for line in f.readlines():
        if line.startswith("INSTALLATION_UUID"):
            uuid = line.split("'")[1]
    f.close()
    return uuid

# XXX: this function doesn't do what it claims to do
def get_localhost_uuid(session):
    filename = '/etc/xensource-inventory'
    try:
        f = open(filename, 'r')
    except:
        raise xs_errors.XenError('EIO', \
              opterr="Unable to open inventory file [%s]" % filename)
    domid = ''
    for line in filter(match_domain_id, f.readlines()):
        domid = line.split("'")[1]
    if not domid:
        raise xs_errors.XenError('APILocalhost')

    vms = session.xenapi.VM.get_all_records_where('field "uuid" = "%s"' % domid)
    for vm in vms:
        record = vms[vm]
        if record["uuid"] == domid:
            hostid = record["resident_on"]
            return hostid
    raise xs_errors.XenError('APILocalhost')

def match_domain_id(s):
    regex = re.compile("^CONTROL_DOMAIN_UUID")
    return regex.search(s, 0)

def get_hosts_attached_on(session, vdi_uuids):
    host_refs = {}
    for vdi_uuid in vdi_uuids:
        try:
            vdi_ref = session.xenapi.VDI.get_by_uuid(vdi_uuid)
        except XenAPI.Failure:
            SMlog("VDI %s not in db, ignoring" % vdi_uuid)
            continue
        sm_config = session.xenapi.VDI.get_sm_config(vdi_ref)
        for key in filter(lambda x: x.startswith('host_'), sm_config.keys()):
            host_refs[key[len('host_'):]] = True
    return host_refs.keys()

def get_this_host_ref(session):
    host_uuid = get_this_host()
    host_ref = session.xenapi.host.get_by_uuid(host_uuid)
    return host_ref

def get_slaves_attached_on(session, vdi_uuids):
    "assume this host is the SR master"
    host_refs = get_hosts_attached_on(session, vdi_uuids)
    master_ref = get_this_host_ref(session)
    return filter(lambda x: x != master_ref, host_refs)

def get_online_hosts(session):
    online_hosts = []
    hosts = session.xenapi.host.get_all_records()
    for host_ref, host_rec in hosts.iteritems():
        metricsRef = host_rec["metrics"]
        metrics = session.xenapi.host_metrics.get_record(metricsRef)
        if metrics["live"]:
            online_hosts.append(host_ref)
    return online_hosts

def get_all_slaves(session):
    "assume this host is the SR master"
    host_refs = get_online_hosts(session)
    master_ref = get_this_host_ref(session)
    return filter(lambda x: x != master_ref, host_refs)

def is_attached_rw(sm_config):
    for key, val in sm_config.iteritems():
        if key.startswith("host_") and val == "RW":
            return True
    return False

def find_my_pbd_record(session, host_ref, sr_ref):
    try:
        pbds = session.xenapi.PBD.get_all_records()
        for pbd_ref in pbds.keys():
            if pbds[pbd_ref]['host'] == host_ref and pbds[pbd_ref]['SR'] == sr_ref:
                return [pbd_ref,pbds[pbd_ref]]
        return None
    except Exception, e:
        SMlog("Caught exception while looking up PBD for host %s SR %s: %s" % (str(host_ref), str(sr_ref), str(e)))
        return None    

def find_my_pbd(session, host_ref, sr_ref):
    ret = find_my_pbd_record(session, host_ref, sr_ref)
    if ret <> None:
        return ret[0]
    else:
        return None

def test_hostPBD_devs(session, devs):
    host = get_localhost_uuid(session)
    try:
        pbds = session.xenapi.PBD.get_all_records()
    except:
        raise xs_errors.XenError('APIPBDQuery')
    for dev in devs.split(','):
        for pbd in pbds:
            record = pbds[pbd]
            if record["host"] == host:
                devconfig = record["device_config"]
                if devconfig.has_key('device'):
                    for device in devconfig['device'].split(','):
                        if os.path.realpath(device) == os.path.realpath(dev):
                            return True;
    return False

def test_hostPBD_lun(session, targetIQN, LUNid):
    host = get_localhost_uuid(session)
    try:
        pbds = session.xenapi.PBD.get_all_records()
    except:
        raise xs_errors.XenError('APIPBDQuery')
    for pbd in pbds:
        record = pbds[pbd]
        if record["host"] == host:
            devconfig = record["device_config"]
            if devconfig.has_key('targetIQN') and devconfig.has_key('LUNid'):
                if devconfig['targetIQN'] == targetIQN and \
                       devconfig['LUNid'] == LUNid:
                    return True;
    return False

def test_SCSIid(session, SCSIid):
    try:
        pbds = session.xenapi.PBD.get_all_records()
    except:
        raise xs_errors.XenError('APIPBDQuery')
    for pbd in pbds:
        record = pbds[pbd]
        devconfig = record["device_config"]
        sm_config = session.xenapi.SR.get_sm_config(record["SR"])
        if devconfig.has_key('SCSIid') and devconfig['SCSIid'] == SCSIid:
                    return True;
        elif sm_config.has_key('SCSIid') and sm_config['SCSIid'] == SCSIid:
                    return True;
        elif sm_config.has_key('scsi-' + SCSIid):
                    return True;
    return False

def _incr_iscsiSR_refcount(targetIQN, uuid):
    if not os.path.exists(ISCSI_REFDIR):
        os.mkdir(ISCSI_REFDIR)
    filename = os.path.join(ISCSI_REFDIR, targetIQN)
    try:
        f = open(filename, 'a+')
    except:
        raise xs_errors.XenError('LVMRefCount', \
                                 opterr='file %s' % filename)
    
    found = False
    refcount = 0
    for line in filter(match_uuid, f.readlines()):
        refcount += 1
        if line.find(uuid) != -1:
            found = True
    if not found:
        f.write("%s\n" % uuid)
        refcount += 1
    f.close()
    return refcount

def _decr_iscsiSR_refcount(targetIQN, uuid):
    filename = os.path.join(ISCSI_REFDIR, targetIQN)
    if not os.path.exists(filename):
        return 0
    try:
        f = open(filename, 'a+')
    except:
        raise xs_errors.XenError('LVMRefCount', \
                                 opterr='file %s' % filename)
    output = []
    refcount = 0
    for line in filter(match_uuid, f.readlines()):
        if line.find(uuid) == -1:
            output.append(line[:-1])
            refcount += 1
    if not refcount:
        os.unlink(filename)
        return refcount

    # Re-open file and truncate
    f.close()
    f = open(filename, 'w')
    for i in range(0,refcount):
        f.write("%s\n" % output[i])
    f.close()
    return refcount

# The agent enforces 1 PBD per SR per host, so we
# check for active SR entries not attached to this host
def test_activePoolPBDs(session, host, uuid):
    try:
        pbds = session.xenapi.PBD.get_all_records()
    except:
        raise xs_errors.XenError('APIPBDQuery')
    for pbd in pbds:
        record = pbds[pbd]
        if record["host"] != host and record["SR"] == uuid \
               and record["currently_attached"]:
            return True
    return False

def remove_mpathcount_field(session, host_ref, sr_ref, SCSIid):
    try:
        pbdref = find_my_pbd(session, host_ref, sr_ref)
        if pbdref <> None:
            key = "mpath-" + SCSIid
            session.xenapi.PBD.remove_from_other_config(pbdref, key)
    except:
        pass

def _testHost(hostname, port, errstring):
    SMlog("_testHost: Testing host/port: %s,%d" % (hostname,port))
    try:
        addr = socket.gethostbyname(hostname)
    except:
        raise xs_errors.XenError('DNSError')

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # Only allow the connect to block for up to 2 seconds
    sock.settimeout(2)
    try:
        sock.connect((addr, port))
        # Fix for MS storage server bug
        sock.send('\n')
        sock.close()
    except socket.error, reason:
        SMlog("_testHost: Connect failed after 2 seconds (%s)" \
                   % hostname)
        raise xs_errors.XenError(errstring)

def match_scsiID(s, id):
    regex = re.compile(id)
    return regex.search(s, 0)

def _isSCSIid(s):
    regex = re.compile("^scsi-")
    return regex.search(s, 0)    

def test_scsiserial(session, device):
    device = os.path.realpath(device)
    if not scsiutil._isSCSIdev(device):
        SMlog("util.test_scsiserial: Not a serial device: %s" % device)
        return False
    serial = ""
    try:
        serial += scsiutil.getserial(device)
    except:
        # Error allowed, SCSIid is the important one
        pass

    try:
        scsiID = scsiutil.getSCSIid(device)
    except:
        SMlog("util.test_scsiserial: Unable to verify serial or SCSIid of device: %s" \
                   % device)
        return False
    if not len(scsiID):
        SMlog("util.test_scsiserial: Unable to identify scsi device [%s] via scsiID" \
                   % device)
        return False
    
    try:
        SRs = session.xenapi.SR.get_all_records()
    except:
        raise xs_errors.XenError('APIFailure')
    for SR in SRs:
        record = SRs[SR]
        conf = record["sm_config"]
        if conf.has_key('devserial'):
            for dev in conf['devserial'].split(','):
                if _isSCSIid(dev):
                    if match_scsiID(dev, scsiID):
                        return True
                elif len(serial) and dev == serial:
                    return True
    return False

def default(self, field, thunk):
    try:
        return getattr(self, field)
    except:
        return thunk ()
    
def list_VDI_records_in_sr(sr):
    """Helper function which returns a list of all VDI records for this SR
    stored in the XenAPI server, useful for implementing SR.scan"""
    sr_ref = sr.session.xenapi.SR.get_by_uuid(sr.uuid)
    vdis = sr.session.xenapi.VDI.get_all_records_where("field \"SR\" = \"%s\"" % sr_ref)
    return vdis

# Given a partition (e.g. sda1), get a disk name:
def diskFromPartition(partition):
    numlen = 0 # number of digit characters
    m = re.match("\D+(\d+)", partition)
    if m != None:
        numlen = len(m.group(1))

    # is it a cciss?
    if True in [partition.startswith(x) for x in ['cciss', 'ida', 'rd']]:
        numlen += 1 # need to get rid of trailing 'p'

    # is it a mapper path?
    if partition.startswith("mapper"):
        if re.search("p[0-9]*$",partition):
            numlen = len(re.match("\d+", partition[::-1]).group(0)) + 1
            SMlog("Found mapper part, len %d" % numlen)
        else:
            numlen = 0

    # is it /dev/disk/by-id/XYZ-part<k>?
    if partition.startswith("disk/by-id"):
        return partition[:partition.rfind("-part")]

    return partition[:len(partition) - numlen]

def dom0_disks():
    """Disks carrying dom0, e.g. ['/dev/sda']"""
    disks = []
    for line in open("/etc/mtab").readlines():
        (dev, mountpoint, fstype, opts, freq, passno) = line.split(' ')
        if mountpoint == '/':
            disk = diskFromPartition(dev)
            if not (disk in disks): disks.append(disk)
    SMlog("Dom0 disks: %s" % disks)
    return disks

def set_scheduler(dev, str):
    devices = []
    if not scsiutil.match_dm(dev):
        # Remove partition numbers
        devices.append(diskFromPartition(dev).replace('/', '!'))
    else:
        rawdev = diskFromPartition(dev)
        devices = map(lambda x: os.path.realpath(x)[5:], scsiutil._genReverseSCSIidmap(rawdev.split('/')[-1]))

    for d in devices:
        path = "/sys/block/%s/queue/scheduler" % d
        if not os.path.exists(path):
            SMlog("no path %s" % path)
            return
        try:
            f = open(path, 'w')
            f.write("%s\n" % str)
            f.close()
            SMlog("Set scheduler to [%s] on dev [%s]" % (str,d))
        except:
            SMlog("Error setting scheduler to [%s] on dev [%s]" % (str,d))
            pass

# This function queries XAPI for the existing VDI records for this SR
def _getVDIs(srobj):
    VDIs = []
    try:
        sr_ref = getattr(srobj,'sr_ref')
    except AttributeError:
        return VDIs
        
    refs = srobj.session.xenapi.SR.get_VDIs(sr_ref)
    for vdi in refs:
        ref = srobj.session.xenapi.VDI.get_record(vdi)
        ref['vdi_ref'] = vdi
        VDIs.append(ref)
    return VDIs

def _getVDI(srobj, vdi_uuid):
    vdi = srobj.session.xenapi.VDI.get_by_uuid(vdi_uuid)
    ref = srobj.session.xenapi.VDI.get_record(vdi)
    ref['vdi_ref'] = vdi
    return ref
    
def _convertDNS(name):
    addr = socket.gethostbyname(name)
    return addr

def _containsVDIinuse(srobj):
    VDIs = _getVDIs(srobj)
    for vdi in VDIs:
        if not vdi['managed']:
            continue
        sm_config = vdi['sm_config']
        if sm_config.has_key('SRRef'):
            try:
                PBDs = srobj.session.xenapi.SR.get_PBDs(sm_config['SRRef'])
                for pbd in PBDs:
                    record = PBDs[pbd]
                    if record["host"] == srobj.host_ref and \
                       record["currently_attached"]:
                        return True
            except:
                pass
    return False


#########################
# Daemon helper functions
def p_id_fork():
    try:
        p_id = os.fork()
    except OSError, e:
        print "Fork failed: %s (%d)" % (e.strerror,e.errno)
        sys.exit(-1)
  
    if (p_id == 0):
        os.setsid()
        try:
            p_id = os.fork()
        except OSError, e:
            print "Fork failed: %s (%d)" % (e.strerror,e.errno)
            sys.exit(-1)
        if (p_id == 0):
            os.chdir('/opt/xensource/sm')
            os.umask(0)
        else:
            os._exit(0)                             
    else:
        os._exit(0)

def daemon():
    p_id_fork()
    # Query the max file descriptor parameter for this process
    maxfd = resource.getrlimit(resource.RLIMIT_NOFILE)[1]

    # Close any fds that are open
    for fd in range(0, maxfd):
        try:
            os.close(fd)
        except:
            pass

    # Redirect STDIN to STDOUT and STDERR
    os.open('/dev/null', os.O_RDWR)
    os.dup2(0, 1)
    os.dup2(0, 2)
#########################

if __debug__:
    try:
        XE_IOFI_IORETRY
    except NameError:
        XE_IOFI_IORETRY = os.environ.get('XE_IOFI_IORETRY', None)
    if __name__ == 'util' and XE_IOFI_IORETRY is not None:
        import iofi


################################################################################
#
#  Fist points
#

# * The global variable 'fistpoint' define the list of all possible fistpoints;
#
# * To activate a fistpoint called 'name', you need to create the file '/tmp/fist_name'
#   on the SR master;
#
# * At the moment, activating a fist point can lead to two possible behaviors:
#   - if '/tmp/fist_LVHDRT_exit' exists, then the function called during the fistpoint is _exit;
#   - otherwise, the function called is _pause.

def _pause(secs, name):
    SMlog("Executing fist point %s: sleeping %d seconds ..." % (name, secs))
    time.sleep(secs)
    SMlog("Executing fist point %s: done" % name)

def _exit(name):
    SMlog("Executing fist point %s: exiting the current process ..." % name)
    raise xs_errors.XenError('FistPoint', opterr='%s' % name)

class FistPoint:
    def __init__(self, points):
        #SMlog("Fist points loaded")
        self.points = points

    def is_active(self, name):
        return os.path.exists("/tmp/fist_%s" % name)

    def mark_sr(self, name, sruuid, started):
        session=get_localAPI_session()
        sr=session.xenapi.SR.get_by_uuid(sruuid)
        if started:
            session.xenapi.SR.add_to_other_config(sr,name,"active")
        else:
            session.xenapi.SR.remove_from_other_config(sr,name)

    def activate(self, name, sruuid):
        if name in self.points:
            if self.is_active(name): 
                self.mark_sr(name,sruuid,True)
                if self.is_active("LVHDRT_exit"):
                    self.mark_sr(name,sruuid,False)
                    _exit(name)
                else:
                    _pause(FIST_PAUSE_PERIOD, name) 
                self.mark_sr(name,sruuid,False)
        else:
            SMlog("Unknown fist point: %s" % name)

    def activate_custom_fn(self, name, fn):
        if name in self.points:
            if self.is_active(name):
                SMlog("Executing fist point %s: starting ..." % name)
                fn()
                SMlog("Executing fist point %s: done" % name)
        else:
            SMlog("Unknown fist point: %s" % name)

def list_find(f, seq):
    for item in seq:
        if f(item): 
            return item

fistpoint = FistPoint( ["LVHDRT_finding_a_suitable_pair", 
                        "LVHDRT_inflating_the_parent", 
                        "LVHDRT_resizing_while_vdis_are_paused", 
                        "LVHDRT_coalescing_VHD_data", 
                        "LVHDRT_coalescing_before_inflate_grandparent", 
                        "LVHDRT_relinking_grandchildren",
                        "LVHDRT_before_create_relink_journal",
                        "LVHDRT_xapiSM_serialization_tests",
                        "LVHDRT_clone_vdi_after_create_journal",
                        "LVHDRT_clone_vdi_after_shrink_parent",
                        "LVHDRT_clone_vdi_after_first_snap",
                        "LVHDRT_clone_vdi_after_second_snap",
                        "LVHDRT_clone_vdi_after_parent_hidden",
                        "LVHDRT_clone_vdi_after_parent_ro",
                        "LVHDRT_clone_vdi_before_remove_journal",
                        "LVHDRT_clone_vdi_after_lvcreate",
                        "LVHDRT_clone_vdi_before_undo_clone",
                        "LVHDRT_clone_vdi_after_undo_clone",
                        "LVHDRT_inflate_after_create_journal",
                        "LVHDRT_inflate_after_setSize",
                        "LVHDRT_inflate_after_zeroOut",
                        "LVHDRT_inflate_after_setSizePhys",
                        "LVHDRT_inflate_after_setSizePhys",
                        "LVHDRT_coaleaf_before_coalesce",
                        "LVHDRT_coaleaf_after_coalesce",
                        "LVHDRT_coaleaf_one_renamed",
                        "LVHDRT_coaleaf_both_renamed",
                        "LVHDRT_coaleaf_after_vdirec",
                        "LVHDRT_coaleaf_before_delete",
                        "LVHDRT_coaleaf_after_delete",
                        "LVHDRT_coaleaf_before_remove_j",
                        "LVHDRT_coaleaf_undo_after_rename",
                        "LVHDRT_coaleaf_undo_after_rename2",
                        "LVHDRT_coaleaf_undo_after_refcount",
                        "LVHDRT_coaleaf_undo_after_deflate",
                        "LVHDRT_coaleaf_undo_end",
                        "LVHDRT_coaleaf_stop_after_recovery",
                        "LVHDRT_coaleaf_finish_after_inflate",
                        "LVHDRT_coaleaf_finish_end",
                        "LVHDRT_coaleaf_delay_1",
                        "LVHDRT_coaleaf_delay_2",
                        "LVHDRT_coaleaf_delay_3",
                        "testsm_clone_allow_raw",
                        "xenrt_default_vdi_type_legacy"] )

def set_dirty(session, sr):
    try:
        session.xenapi.SR.add_to_other_config(sr, "dirty", "")
        SMlog("set_dirty %s succeeded" % (repr(sr)))
    except:
        SMlog("set_dirty %s failed (flag already set?)" % (repr(sr)))        

def doesFileHaveOpenHandles(fileName):
    SMlog("Entering doesFileHaveOpenHandles with file: %s" % fileName)
    (retVal, processAndPidTuples) = \
        findRunningProcessOrOpenFile(fileName, False)
    
    if not retVal:
        SMlog("Failed to determine if file %s has open handles." % \
                   fileName)
        # err on the side of caution
        return True
    else:
        if len(processAndPidTuples) > 0:
            return True
        else:
            return False
    
# extract SR uuid from the passed in devmapper entry and return
# /dev/mapper/VG_XenStorage--c3d82e92--cb25--c99b--b83a--482eebab4a93-MGT
def extractSRFromDevMapper(path):
    try:
        path=os.path.basename(path)
        path=path[len('VG_XenStorage-')+1:]
        path=path.replace('--','/')
        path=path[0:path.rfind('-')]
        return path.replace('/','-')
    except:
        return ''

# Looks at /proc and figures either
#   If a process is still running (default), returns open file names
#   If any running process has open handles to the given file (process = False)
#       returns process names and pids
def findRunningProcessOrOpenFile(name, process = True):
    retVal = True
    try:
        SMlog("Entering findRunningProcessOrOpenFile with params: %s" % \
                   [name, process])
        links = []
        processandpids = []
        
        # Look at all pids
        pids = [pid for pid in os.listdir('/proc') if pid.isdigit()]
        for pid in sorted(pids):
            try:
                try:
                    f = None
                    f = open(os.path.join('/proc', pid, 'cmdline'), 'rb')
                    prog = f.read()[:-1]
                except IOError, e:
                    if e.errno != errno.ENOENT:
                        SMlog("ERROR %s reading %s, ignore" % (e.errno, pid))
                    continue
            finally:
                if f != None:
                    f.close()
            
            try:
                fd_dir = os.path.join('/proc', pid, 'fd')
                files = os.listdir(fd_dir)
            except Exception, e:
                raise Exception(str(e))
            
            for file in files:
                try:
                    link = os.readlink(os.path.join(fd_dir, file))
                except OSError:
                    continue
                
                if process and name == prog:
                    links.append(link)
                else:
                    # need to return process name and pid tuples
                    if link == name:
                        SMlog("File %s has an open handle with process %s "
                              "with pid %s" % (name, prog, pid))
                        processandpids.append((prog, pid))
    except Exception, e:
        SMlog("Exception checking running process or open file handles. "\
                   "Error: %s" % str(e))
        retVal = False
        
    if process:
        return (retVal, links)
    else:
        return (retVal, processandpids)

def retry(f, maxretry=20, period=3):
    retries = 0
    while True:
        try:            
            return f()
        except Exception, e:
            SMlog("Got exception: %s. Retry number: %s" % (str(e),retries))

        retries += 1
        if retries >= maxretry:
            break

        time.sleep(period)

    return f()

class extractXVA:
    # streams files as a set of file and checksum, caller should remove 
    # the files, if not needed. The entire directory (Where the files 
    # and checksum) will only be deleted as part of class cleanup.
    HDR_SIZE = 512
    BLOCK_SIZE = 512
    SIZE_LEN = 12 - 1 # To remove \0 from tail
    SIZE_OFFSET = 124
    ZERO_FILLED_REC = 2
    NULL_IDEN = '\x00'
    DIR_IDEN = '/'
    CHECKSUM_IDEN = '.checksum'
    OVA_FILE = 'ova.xml'

    # Init gunzips the file using a subprocess, and reads stdout later 
    # as and when needed
    def __init__(self, filename):
        self.__extract_path = ''
        self.__filename = filename
        cmd = 'gunzip -cd %s' % filename
        try:
            self.spawn_p = subprocess.Popen(
                            cmd, shell=True, \
                            stdin=subprocess.PIPE, stdout=subprocess.PIPE, \
                            stderr=subprocess.PIPE, close_fds=True)
        except Exception, e:
            SMlog("Error: %s. Uncompress failed for %s" % (str(e), filename))
            raise Exception(str(e))

        # Create dir to extract the files
        self.__extract_path = tempfile.mkdtemp()

    def __del__(self):
        shutil.rmtree(self.__extract_path)

    # Class supports Generator expression. 'for f_name, checksum in getTuple()'
    #   returns filename, checksum content. Returns filename, '' in case  
    #   of checksum file missing. e.g. ova.xml
    def getTuple(self):
        zerod_record = 0
        ret_f_name = ''
        ret_base_f_name = ''

        try:
            # Read tar file as sets of file and checksum. 
            while True:
                # Read the output of spawned process, or output of gunzip
                f_hdr = self.spawn_p.stdout.read(self.HDR_SIZE)

                # Break out in case of end of file
                if f_hdr == '':
                    if zerod_record == extractXVA.ZERO_FILLED_REC:
                        break
                    else:
                        SMlog('Error. Expects %d zero records', \
                               extractXVA.ZERO_FILLED_REC)
                        raise Exception('Unrecognized end of file')

                # Watch out for zero records, two zero records 
                # denote end of file.
                if f_hdr == extractXVA.NULL_IDEN * extractXVA.HDR_SIZE:
                    zerod_record += 1
                    continue

                f_name = f_hdr[:f_hdr.index(extractXVA.NULL_IDEN)]
                # File header may be for a folder, if so ignore the header
                if not f_name.endswith(extractXVA.DIR_IDEN):
                    f_size_octal = f_hdr[extractXVA.SIZE_OFFSET: \
                                 extractXVA.SIZE_OFFSET + extractXVA.SIZE_LEN]
                    f_size = int(f_size_octal, 8)
                    if f_name.endswith(extractXVA.CHECKSUM_IDEN):
                        if f_name.rstrip(extractXVA.CHECKSUM_IDEN) == \
                                                        ret_base_f_name:
                            checksum = self.spawn_p.stdout.read(f_size)
                            yield(ret_f_name, checksum)
                        else:
                            # Expects file followed by its checksum
                            SMlog('Error. Sequence mismatch starting with %s', \
                                     ret_f_name)
                            raise Exception(\
                                    'Files out of sequence starting with %s', \
                                    ret_f_name)
                    else:
                        # In case of ova.xml, read the contents into a file and 
                        # return the file name to the caller. For other files, 
                        # read the contents into a file, it will
                        # be used when a .checksum file is encountered.
                        ret_f_name = '%s/%s' % (self.__extract_path, f_name)
                        ret_base_f_name = f_name

                        # Check if the folder exists on the target location,
                        # else create it.
                        folder_path = ret_f_name[:ret_f_name.rfind('/')]
                        if not os.path.exists(folder_path):
                            os.mkdir(folder_path)

                        # Store the file to the tmp folder, strip the tail \0 
                        f = open(ret_f_name, 'w')
                        f.write(self.spawn_p.stdout.read(f_size))
                        f.close()
                        if f_name == extractXVA.OVA_FILE:
                            yield(ret_f_name, '')

                    # Skip zero'd portion of data block
                    round_off = f_size % extractXVA.BLOCK_SIZE
                    if round_off != 0:
                        zeros = self.spawn_p.stdout.read(
                                extractXVA.BLOCK_SIZE - round_off)
        except Exception, e:
            SMlog("Error: %s. File set extraction failed %s" % (str(e), \
                                                     self.__filename))

            # Kill and Drain stdout of the gunzip process, 
            # else gunzip might block on stdout
            os.kill(self.spawn_p.pid, signal.SIGTERM)
            self.spawn_p.communicate()
            raise Exception(str(e))

