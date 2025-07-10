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
# if without -r, do step 2~3
# 2. if it's the first USB device to pass-through
#      a) bind mount /sys in chroot directory (/var/xen/qemu/root-<domid>)
#      b) clone (create the device with same major/minor number and mode)
#         in chroot directory with same path
#      c) bind mount /proc/<pid> to chroot directory (/var/xen/qemu/root-<domid>/proc/self)
# 3. set device file uid/gid to (qemu_base + dom-id)
#
# detach
# ./usb_reset.py detach device -d dom-id
# ./usb_reset.py detach 2-2 -d 12
# 1. Remove the cloned device file in chroot directory
# 2. Umount /proc/self from chroot directory if it is mounted
#
# cleanup
# ./usb_reset.py cleanup -d dom-id
# ./usb_reset.py cleanup -d 12
# 1.umount /sys from chroot directory if they are mounted.
# 2.umount /proc/self from chroot directory if they are mounted.
# 3.remove /dev/bus directory in chroot directory if it exists

import argparse
import ctypes
import ctypes.util
import fcntl
import grp
import logging
import os
import pwd
import re
import shutil
import sys

import xcp.logger as log  # pytype: disable=import-error

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
        sys.exit(1)
    try:
        bus = read_int("/sys/bus/usb/devices/{}/busnum".format(device))
        dev = read_int("/sys/bus/usb/devices/{}/devnum".format(device))
        return "/dev/bus/usb/{0:03d}/{1:03d}".format(bus, dev)
    except (IOError, ValueError) as e:
        log.error("Failed to get device path {}: {}".format(device, str(e)))
        sys.exit(1)


def mount(source, target, fs, flags=0):
    if ctypes.CDLL(ctypes.util.find_library("c"), use_errno=True
                   ).mount(source.encode(), target.encode(), fs.encode(), flags, None) < 0:
        log.error("Failed to mount {} ({}) to {} with flags {}: {}".
                  format(source, fs, target, flags,
                         os.strerror(ctypes.get_errno())))
        sys.exit(1)


def umount(target):
    if ctypes.CDLL(ctypes.util.find_library("c"), use_errno=True
                   ).umount(target.encode()) < 0:
        # log and continue
        log.error("Failed to umount {}: {}".
                  format(target, os.strerror(ctypes.get_errno())))


def clone_device(path, root_dir, domid):
    """
    Clone the device file into the chroot directory.

    :param path: The source device file under system /dev to clone.
    :param root_dir: The root directory of the chroot environment.
    :param domid: The domain ID of the VM, used to set the device file's uid/gid.
    """
    target_path = os.path.join(root_dir, path.lstrip(os.path.sep))
    if os.path.exists(target_path):
        log.info("Device file {} already exists in chroot".format(target_path))
        return

    os.makedirs(os.path.dirname(target_path), exist_ok=True, mode=0o755)

    try:
        st = os.stat(path)
    except OSError as e:
        log.error("Failed to get stat of {}: {}".format(path, str(e)))
        sys.exit(1)

    mode = st.st_mode
    major = os.major(st.st_rdev)
    minor = os.minor(st.st_rdev)
    clone_device_id = os.makedev(major, minor)
    os.mknod(target_path, mode, clone_device_id)

    # set device file uid/gid
    try:
        os.chown(target_path, pwd.getpwnam("qemu_base").pw_uid + domid,
                 grp.getgrnam("qemu_base").gr_gid + domid)
    except OSError as e:
        log.error("Failed to chown device file {}: {}".format(path, str(e)))
        sys.exit(1)


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

    root_dir = get_root_dir(domid)
    dev_dir = root_dir + "/dev"
    if not os.path.isdir(root_dir) or not os.path.isdir(dev_dir):
        log.error("Error: The chroot or dev directory doesn't exist")
        sys.exit(1)

    clone_device(path, root_dir, domid)

    sys_dir = root_dir + "/sys"
    proc_dir = root_dir + "/proc"
    # sys_dir could already be mounted because of PCI pass-through
    os.makedirs(sys_dir, exist_ok=True, mode=0o755)
    if not os.path.isdir(sys_dir + "/devices"):
        mount("/sys", sys_dir, "sysfs")

    self_dir = os.path.join(proc_dir, "self")
    os.makedirs(self_dir , exist_ok=True, mode=0o755)
    fd_dir = os.path.join(self_dir, "fd")
    if not os.path.isdir(fd_dir):
        MS_BIND = 4096  # mount flags, from fs.h
        mount(f"/proc/{pid}/", self_dir, "", MS_BIND)


def detach(device, domid):
    path = dev_path(device)
    root_dir = get_root_dir(domid)
    target_path = os.path.join(root_dir, path.lstrip(os.path.sep))
    os.remove(target_path)


def cleanup(domid):
    # umount /dev, /sys from chroot directory if they are mounted.
    root_dir = get_root_dir(domid)
    dev_dir = root_dir + "/dev"
    sys_dir = root_dir + "/sys"
    bus_dir = dev_dir + "/bus"
    proc_dir = root_dir + "/proc"
    self_dir = proc_dir + "/self"
    if os.path.isdir(bus_dir):
        log.info("Removing bus directory: {} for cleanup".format(bus_dir))
        shutil.rmtree(bus_dir)
    if os.path.isdir(sys_dir + "/devices"):
        umount(sys_dir)
    if os.path.exists(sys_dir) and os.path.ismount(self_dir):
        umount(self_dir)
        log.info("Removing proc directory: {} for cleanup".format(proc_dir))
        shutil.rmtree(proc_dir)


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
        sys.exit(1)
