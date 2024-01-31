#!/usr/bin/env python3
#
# Copyright (C) Citrix Systems Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; version 2.1 only. #
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
# attach
# ./usb_reset.py attach device -d dom-id -p pid [-r]
# ./usb_reset.py attach 2-2 -d 12 -p 4130
# ./usb_reset.py attach 2-2 -d 12 -p 4130 -r
# 1. reset device
# if without -r, do step 2~4
# 2. if it's the first USB device to pass-through
#      a) bind mount /dev /sys in chroot directory (/var/xen/qemu/root-<domid>)
#      b) create new cgroup devices:/qemu-<domid>,
#      c) blacklist all and add default device whitelist,
#      d) join current qemu process to this cgroup
# 3. save device uid/gid to /var/run/nonpersistent/usb/<device>
# 4. set device file uid/gid to (qemu_base + dom-id)
# 5. add current device to whitelist
#
# detach
# ./usb_reset.py detach device -d dom-id
# ./usb_reset.py detach 2-2 -d 12
# 1. restore device file uid/gid from /var/run/nonpersistent/usb/<device>
# 2. remove current device from whitelist
#
# cleanup
# ./usb_reset.py cleanup -d dom-id
# ./usb_reset.py cleanup -d 12
# 1.remove the cgroup if one has been created.
# 2.umount /dev, /sys from chroot directory if they are mounted.

import argparse
import ctypes
import ctypes.util
import errno
import fcntl
import grp
import xcp.logger as log
import logging
import os
import pwd
import re
from stat import S_ISCHR, S_ISBLK


def parse_arg():
    parser = argparse.ArgumentParser(
        description="script to attach, detach, cleanup for USB pass-through")
    subparsers = parser.add_subparsers(help="command", dest="command")

    attach = subparsers.add_parser("attach", help="attach a USB device")
    attach.add_argument("device", help="the target usb device")
    attach.add_argument("-d", dest="domid", type=int, required=True,
                        help="specify the domid of the VM")
    attach.add_argument("-p", dest="pid", type=int, required=True,
                        help="the process id of QEMU")
    attach.add_argument("-r", dest="reset_only", action="store_true",
                        help="reset device only, for privileged mode")

    detach = subparsers.add_parser("detach", help="detach a USB device")
    detach.add_argument("device", help="the target usb device")
    detach.add_argument("-d", dest="domid", type=int, required=True,
                        help="specify the domid of the VM")

    cleanup = subparsers.add_parser("cleanup", help="clean up chroot directory")
    cleanup.add_argument("-d", dest="domid", type=int, required=True,
                         help="specify the domid of the VM")

    return parser.parse_args()


def get_root_dir(domid):
    return "/var/xen/qemu/root-{}".format(domid)


def get_cg_dir(domid):
    return "/sys/fs/cgroup/devices/qemu-{}".format(domid)


def get_ids_path(device):
    usb_dir = "/var/run/nonpersistent/usb"
    try:
        os.makedirs(usb_dir)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

    return os.path.join(usb_dir, device)


def save_device_ids(device):
    path = dev_path(device)

    try:
        stat = os.stat(path)
        ids_info = "{} {}".format(stat.st_uid, stat.st_gid)
    except OSError as e:
        log.error("Failed to stat {}: {}".format(path, str(e)))
        exit(1)

    try:
        with open(get_ids_path(device), "w") as f:
            f.write(ids_info)
    except IOError as e:
        log.error("Failed to save device ids {}: {}".format(path, str(e)))
        exit(1)


def load_device_ids(device):
    ids_path = get_ids_path(device)
    try:
        with open(ids_path) as f:
            uid, gid = list(map(int, f.readline().split()))
    except (IOError, ValueError) as e:
        log.error("Failed to load device ids: {}".format(str(e)))

    try:
        os.remove(ids_path)
    except OSError as e:
        # ignore and continue
        log.warning("Failed to remove device ids: {}".format(str(e)))

    return uid, gid


# throw IOError, ValueError
def read_int(path):
    with open(path) as f:
        return int(f.readline())


def dev_path(device):
    # check device node pattern
    # example: "4-1", "1-2.1.2"
    pat = re.compile(r"\d+-\d+(\.\d+)*$")
    if pat.match(device) is None:
        log.error("Unexpected device node: {}".format(device))
        exit(1)
    try:
        bus = read_int("/sys/bus/usb/devices/{}/busnum".format(device))
        dev = read_int("/sys/bus/usb/devices/{}/devnum".format(device))
        return "/dev/bus/usb/{0:03d}/{1:03d}".format(bus, dev)
    except (IOError, ValueError) as e:
        log.error("Failed to get device path {}: {}".format(device, str(e)))
        exit(1)


def get_ctl(path, mode):  # type:(str, str) -> str
    """get the string to control device access for cgroup
    :param path: the device file path
    :param mode: either "r" or "rw"
    :return: the string to control device access
    """
    try:
        st = os.stat(path)
    except OSError as e:
        log.error("Failed to get stat of {}: {}".format(path, str(e)))
        raise

    t = ""
    if S_ISBLK(st.st_mode):
        t = "b"
    elif S_ISCHR(st.st_mode):
        t = "c"
    if t and mode in ("r", "rw"):
        return "{} {}:{} {}".format(t, os.major(st.st_rdev), os.minor(
            st.st_rdev), mode)
    raise RuntimeError("Failed to get control string of {}".format(path))


def _device_ctl(path, domid, allow):
    cg_dir = get_cg_dir(domid)
    file_name = "/devices.allow" if allow else "/devices.deny"
    try:
        with open(cg_dir + file_name, "w") as f:
            f.write(get_ctl(path, "rw"))
    except (IOError, OSError, RuntimeError) as e:
        log.error("Failed to {} {}: {}".format(
            "allow" if allow else "deny", path, str(e)))
        exit(1)


def allow_device(path, domid):
    _device_ctl(path, domid, True)


def deny_device(path, domid):
    _device_ctl(path, domid, False)


def setup_cgroup(domid, pid):  # type:(str, str) -> None
    """
    Associate the given process id (pid) with the given Linux kernel control group
    and limit it's device access to only /dev/null.

    :param domid (str): The control group ID string (passed on from the command line)
    :param pid (str): The process ID string (passed on from the command line)

    If the control group directory does not exist yet, the control group is created.

    - The pid goes into the file "tasks" to associate the process with the cgroup.
    - Deny device access by default by writing "a" to devices.deny.
    - Grant read-write access to /dev/null, writing it's device IDs to devices.allow.

    If any error occur during the setup process, the error is logged and
    the program exits with a status code of 1.
    """
    cg_dir = get_cg_dir(domid)

    try:
        os.mkdir(cg_dir, 0o755)
    except OSError as e:
        if e.errno != errno.EEXIST:
            log.error("Failed to create cgroup: {}".format(cg_dir))
            exit(1)

    try:
        # unbuffered write to ensure each one is flushed immediately
        # to the kernel's control group filesystem:
        #
        # The order of writes is likely not important, but the writes
        # may have to be a single write() system call for the entire string.
        #
        # Using the unbuffered Raw IO mode, we know the write was done
        # in exactly this way by the write function call itself, not later.
        #
        # With small writes like this , splitting them because of overflowing the
        # buffer is not expected to happen. To stay safe and keep using unbuffered I/O
        # We have to migrate to binary mode in python3,as python3 supports unbuffered 
        # raw I/O in binary mode.
        #
        with open(cg_dir + "/tasks", "wb", 0) as tasks, \
                open(cg_dir + "/devices.deny", "wb", 0) as deny, \
                open(cg_dir + "/devices.allow", "wb", 0) as allow:

            # deny all
            deny.write(b"a")

            # To write bytes, we've to encode the strings to bytes below:

            # grant rw access to /dev/null by default
            allow.write(get_ctl("/dev/null", "rw").encode())

            tasks.write(str(pid).encode())

    except (IOError, OSError, RuntimeError) as e:
        log.error("Failed to setup cgroup: {}".format(str(e)))
        exit(1)


def mount(source, target, fs, flags=0):
    if ctypes.CDLL(ctypes.util.find_library("c"), use_errno=True
                   ).mount(source.encode(), target.encode(), fs.encode(), flags, None) < 0:
        log.error("Failed to mount {} ({}) to {} with flags {}: {}".
                  format(source, fs, target, flags,
                         os.strerror(ctypes.get_errno())))
        exit(1)


def umount(target):
    if ctypes.CDLL(ctypes.util.find_library("c"), use_errno=True
                   ).umount(target.encode()) < 0:
        # log and continue
        log.error("Failed to umount {}: {}".
                  format(target, os.strerror(ctypes.get_errno())))


def attach(device, domid, pid, reset_only):
    path = dev_path(device)

    # reset device
    try:
        with open(path, "w") as f:
            # USBDEVFS_RESET _IO('U', 20)
            USBDEVFS_RESET = (ord('U') << 8) | 20
            fcntl.ioctl(f.fileno(), USBDEVFS_RESET, 0)
    except IOError as e:
        # log and continue
        log.error("Failed to reset {}: {}".format(path, str(e)))

    if reset_only:
        return

    save_device_ids(device)

    # set device file uid/gid
    try:
        os.chown(path, pwd.getpwnam("qemu_base").pw_uid + domid,
                 grp.getgrnam("qemu_base").gr_gid + domid)
    except OSError as e:
        log.error("Failed to chown device file {}: {}".format(path, str(e)))
        exit(1)

    root_dir = get_root_dir(domid)
    dev_dir = root_dir + "/dev"
    if not os.path.isdir(root_dir) or not os.path.isdir(dev_dir):
        log.error("Error: The chroot or dev directory doesn't exist")
        exit(1)

    if not os.path.isdir(dev_dir + "/bus"):
        # first USB device to pass-through
        MS_BIND = 4096  # mount flags, from fs.h
        mount("/dev", dev_dir, "", MS_BIND)
        setup_cgroup(domid, pid)

    sys_dir = root_dir + "/sys"
    # sys_dir could already be mounted because of PCI pass-through
    if not os.path.isdir(sys_dir):
        try:
            os.mkdir(sys_dir, 0o755)
        except OSError:
            log.error("Failed to create sys dir in chroot")
            exit(1)
    if not os.path.isdir(sys_dir + "/devices"):
        mount("/sys", sys_dir, "sysfs")

    # add device to cgroup allow list
    allow_device(path, domid)


def detach(device, domid):
    path = dev_path(device)
    uid, gid = load_device_ids(device)

    # restore uid, gid of the device file.
    try:
        os.chown(path, uid, gid)
    except OSError as e:
        log.error("Failed to chown device file {}: {}".format(path, str(e)))
        exit(1)

    # remove device from cgroup allow list
    deny_device(path, domid)


def cleanup(domid):
    # remove the cgroup if one has been created.
    if os.path.isdir(get_cg_dir(domid)):
        try:
            os.rmdir(get_cg_dir(domid))
        except OSError as e:
            # log and continue
            log.error("Failed to remove cgroup qemu-{}: {}"
                      .format(domid, str(e)))

    # umount /dev, /sys from chroot directory if they are mounted.
    root_dir = get_root_dir(domid)
    dev_dir = root_dir + "/dev"
    sys_dir = root_dir + "/sys"
    if os.path.isdir(dev_dir + "/bus"):
        umount(dev_dir)
    if os.path.isdir(sys_dir + "/devices"):
        umount(sys_dir)


if __name__ == "__main__":
    log.logToSyslog(level=logging.DEBUG)

    arg = parse_arg()

    if "attach" == arg.command:
        attach(arg.device, arg.domid, arg.pid, arg.reset_only)
    elif "detach" == arg.command:
        detach(arg.device, arg.domid)
    elif "cleanup" == arg.command:
        cleanup(arg.domid)
    else:
        log.error("Unexpected command: {}".format(arg.command))
        exit(1)
