#!/usr/bin/env python
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
# ./usb_reset.py 1-2
# reset USB device 1-2

import fcntl
import xcp.logger as log
import logging
import re
import sys
import traceback

#USBDEVFS_RESET _IO('U', 20)
USBDEVFS_RESET = (ord('U') << 8) | 20


def read_int(path):
    with open(path) as f:
        return int(f.readline())


def reset_device(bus, dev):
    with open("/dev/bus/usb/{0:03d}/{1:03d}".format(bus, dev), "w") as f:
        fcntl.ioctl(f.fileno(), USBDEVFS_RESET, 0)


if __name__ == "__main__":
    log.logToSyslog(level=logging.DEBUG)

    if len(sys.argv) != 2:
        sys.exit("usage: {} device_node".format(sys.argv[0]))

    device = sys.argv[1]
    pattern = re.compile(r"^\d+-\d+(\.\d+)*$")
    if pattern.match(device) is None:
        log.debug("unexpected device node: {}".format(device))
        exit(1)

    try:
        bus = read_int("/sys/bus/usb/devices/{}/busnum".format(device))
        dev = read_int("/sys/bus/usb/devices/{}/devnum".format(device))
        reset_device(bus, dev)
    except (IOError, ValueError):
        log.debug(traceback.format_exc())
        exit(1)
