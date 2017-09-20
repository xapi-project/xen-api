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
# package dependency: python-pyudev
#
# called by XAPI to do
# 1. a "lsusb" operation: get all usb device and interface information
# 2. check if device can be passed through based on policy file
# 3. return the device info to XAPI in json format

import abc
import argparse
import json
import xcp.logger as log
import logging
import pyudev
import re
import traceback

# True to print log to user.log
__DIAGNOSTIC = False
log.logToSyslog(level=logging.DEBUG)


def log_wrapper(s):
    if __DIAGNOSTIC:
        log.debug(str(s))


def log_list_wrapper(l):
    for s in l:
        log_wrapper(s)


def hex_equal(h1, h2):
    """ check if the value of hex string are equal

    :param h1:(str) lhs hex string
    :param h2:(str) rhs hex string
    :return: bool, if equal
    """
    try:
        return int(h1, 16) == int(h2, 16)
    except (TypeError, ValueError):
        return False


class UsbObject(dict):
    """ Base class of USB classes, save USB properties in dict

    node(str): the key, device node
    """
    __metaclass__ = abc.ABCMeta

    def __init__(self, node):
        super(UsbObject, self).__init__()
        self.node = node

    def get_node(self):
        return self.node

    def __hash__(self):
        return hash(self.node)

    def __eq__(self, other):
        return self.node == other.node

    def __str__(self):
        return self.debug_str()

    @staticmethod
    def indent(level):
        if level <= 0:
            return ""
        return "\n" + "\t" * level + "|__ "

    def debug_str(self, level=0):
        """helper for __str__ to print tree style debug string

        :param level: the indent level
        :return: the debug string
        """
        return self.indent(level) + self.__class__.__name__ + ": " + \
            str((self.node, self))

    def is_initialized(self):
        """ check if all properties are properly set

        :return: bool, if properties are ready
        """
        return bool(self.node)

    def _is_class_hub(self, key_class):
        __VALUE_CLASS_HUB = "09"
        cls = self.get(key_class)
        return cls is not None and hex_equal(__VALUE_CLASS_HUB, cls)

    @abc.abstractmethod
    def is_class_hub(self):
        """ check if this belongs to a hub

        :return: bool, if this belongs to a hub
        """
        pass

    @abc.abstractmethod
    def is_child_of(self, parent):
        """ check if this is a child of parent

        :param parent:(UsbObject) the parent to check against
        :return:
        """
        pass

    @staticmethod
    def validate_int(s, base=10):
        """ validate if a string can be converted to int

        :param s:(str) the string to be converted
        :param base:(int) the radix base of integer to convect
        :return:(bool) if conversion is OK
        """
        try:
            int(s, base)
            return True
        except (TypeError, ValueError):
            return False


class UsbDevice(UsbObject):
    """ Class for USB device, save USB properties in UsbObject dict

    interfaces:([UsbInterface]) list of USB interfaces belonging to this device
    allow:(bool) if passthrough is permitted
    """
    _DESC_VENDOR = "ID_VENDOR_FROM_DATABASE"
    _DESC_PRODUCT = "ID_MODEL_FROM_DATABASE"

    _VERSION = "version"
    _ID_VENDOR = "idVendor"
    _ID_PRODUCT = "idProduct"
    _BCD_DEVICE = "bcdDevice"
    _SERIAL = "serial"
    _CLASS = "bDeviceClass"
    _CONF_VALUE = "bConfigurationValue"
    _NUM_INTERFACES = "bNumInterfaces"

    _PROPS_1 = [_DESC_VENDOR, _DESC_PRODUCT]
    _PROPS_2 = [_VERSION, _ID_VENDOR, _ID_PRODUCT, _BCD_DEVICE, _SERIAL,
                _CLASS, _CONF_VALUE, _NUM_INTERFACES]
    _PROPS = _PROPS_1 + _PROPS_2
    _PROPS_NONABLE = _PROPS_1 + [_SERIAL]

    def __init__(self, node, props1, props2):
        """ initialise UsbDevice, set node and properties

        :param node(str): device node
        :param props1(pyudev.Device): device, to get properties from UDEV
        database
        :param props2(pyudev.Device.attributes): device attributes, to get
        properties from sysfs
        """
        super(UsbDevice, self).__init__(node)

        for p in self._PROPS_1:
            if props1.get(p) is not None:
                self[p] = props1.get(p)
        for p in self._PROPS_2:
            if props2.get(p) is not None:
                self[p] = props2.get(p)
        for p in self._PROPS_NONABLE:
            if p not in self:
                self[p] = ""

        self.interfaces = set()
        self.allow = False

    def debug_str(self, level=0):
        s = super(UsbDevice, self).debug_str(level)
        for i in self.interfaces:
            s += i.debug_str(level + 1)
        s += self.indent(level + 1) + "Allow: " + str(self.allow)
        return s

    def is_initialized(self):
        # usb_descriptor_attr_le16(idVendor, "%04x\n");
        # usb_descriptor_attr_le16(idProduct, "%04x\n");
        # usb_descriptor_attr_le16(bcdDevice, "%04x\n");
        for p in [self._ID_VENDOR, self._ID_PRODUCT, self._ID_PRODUCT]:
            if not self.validate_int(self[p], 16):
                return False
        # usb_actconfig_show(bConfigurationValue, "%u\n");
        # usb_descriptor_attr(bNumConfigurations, "%d\n");
        for p in [self._CONF_VALUE, self._NUM_INTERFACES]:
            if not self.validate_int(self[p]):
                return False

        return super(UsbDevice, self).is_initialized()

    def is_class_hub(self):
        return self._is_class_hub(self._CLASS)

    def is_child_of(self, parent):
        # USB device is at the very top, since we don't care hub or bus info
        return False

    def add_interface(self, interface):
        """ add an interface to this device

        :param interface:(UsbInterface) the UsbInterface to add
        :return: None
        """
        if interface in self.interfaces:
            log_wrapper("overriding existing interface: " + interface)
            self.interfaces.remove(interface)
        self.interfaces.add(interface)

    def del_interface(self, interface):
        """remove an interface from this device

        :param interface:(UsbInterface) the UsbInterface to remove
        :return: None
        """
        if interface in self.interfaces:
            self.interfaces.remove(interface)

    def get_all_interfaces(self):
        """ get all interfaces attached of this device

        :return: set of all interfaces
        """
        return self.interfaces

    def is_ready(self):
        """ check if this device has all the interfaces attached

        :return: bool, if it's ready to do policy check now
        """
        n = int(self[self._NUM_INTERFACES])
        return n > 0 and n == len(self.interfaces)

    def is_allow(self):
        """ getter for self.allow

        :return:(bool), if it can be passed through
        """
        return self.allow

    def set_allow(self, allow):
        """setter for self.allow

        :param allow:(bool) if it can be passed through
        :return: None
        """
        self.allow = allow


class UsbInterface(UsbObject):
    """ Class for USB interface, save USB properties in UsbObject dict

    """
    _NUMBER = "bInterfaceNumber"
    _CLASS = "bInterfaceClass"
    _SUB_CLASS = "bInterfaceSubClass"
    _PROTOCOL = "bInterfaceProtocol"

    _PROPS = [_NUMBER, _CLASS, _SUB_CLASS, _PROTOCOL]

    def __init__(self, node, props):
        """ initialise UsbInterface, set node and properties

        :param node(str): device node
        :param props(pyudev.Device.attributes): device attributes, to get
        properties from sysfs
        """
        super(UsbInterface, self).__init__(node)
        for p in self._PROPS:
            if props.get(p) is not None:
                self[p] = props.get(p)

    def debug_str(self, level=0):
        s = super(UsbInterface, self).debug_str(level)
        return s

    def is_class_hub(self):
        return self._is_class_hub(self._CLASS)

    def is_initialized(self):
        # usb_intf_attr(bInterfaceNumber, "%02x\n");
        # usb_intf_attr(bInterfaceClass, "%02x\n");
        # usb_intf_attr(bInterfaceSubClass, "%02x\n");
        # usb_intf_attr(bInterfaceProtocol, "%02x\n");
        for p in self._PROPS:
            if p not in self or not self.validate_int(self[p], 16):
                return False
        return super(UsbInterface, self).is_initialized()

    def is_child_of(self, parent):
        if type(parent) is UsbDevice and parent.is_initialized():
            conf_value = parent[UsbDevice._CONF_VALUE]
            pattern = r"^{}:{}\.\d+$".format(re.escape(parent.get_node()),
                                             re.escape(conf_value))
            return re.match(pattern, self.get_node()) is not None
        return False


def get_usb_info():
    context, devices, interfaces = pyudev.Context(), [], []

    for d in context.list_devices(subsystem="usb", DEVTYPE="usb_device"):
        device = UsbDevice(d.sys_name, d, d.attributes)
        if device.is_initialized() and not device.is_class_hub():
            devices.append(device)
        else:
            log_wrapper("ignore usb device:" + str(device))

    for d in context.list_devices(subsystem="usb", DEVTYPE="usb_interface"):
        interface = UsbInterface(d.sys_name, d.attributes)
        if interface.is_initialized() and not interface.is_class_hub():
            interfaces.append(interface)
        else:
            log_wrapper("ignore usb interface:" + str(interface))

    return devices, interfaces


class Policy(object):
    """ Parse policy file, and check if a UsbDevice can be passed through

    Policy file spec reference:
    https://support.citrix.com/article/CTX119722

    rule_list: the list of parsed rule
    """
    _PATH = "/etc/xensource/usb-policy.conf"

    _CLASS = "class"
    _SUBCLASS = "subclass"
    _PROTOCOL = "prot"
    _ID_VENDOR = "vid"
    _ID_PRODUCT = "pid"
    _BCD_DEVICE = "rel"

    # key in policy <--> key in usb device
    _KEY_MAP_DEVICE = {_ID_VENDOR: UsbDevice._ID_VENDOR,
                       _ID_PRODUCT: UsbDevice._ID_PRODUCT,
                       _BCD_DEVICE: UsbDevice._BCD_DEVICE}

    # key in policy <--> key in usb interface
    _KEY_MAP_INTERFACE = {_CLASS: UsbInterface._CLASS,
                          _SUBCLASS: UsbInterface._SUB_CLASS,
                          _PROTOCOL: UsbInterface._PROTOCOL}

    _PAT_KEY = r"\s*({}|{}|{}|{}|{}|{})\s*".format(_CLASS, _SUBCLASS,
                                                   _PROTOCOL, _ID_VENDOR,
                                                   _ID_PRODUCT, _BCD_DEVICE)
    _PATTERN = r"{}=\s*([0-9a-f]+)".format(_PAT_KEY)

    _ALLOW = "allow"

    def __init__(self):
        """ parse policy file, generate rule list

        Note: hubs are never allowed to pass through
        """
        self.rule_list = []
        try:
            with open(self._PATH, "r") as f:
                log_wrapper("=== policy file begin")
                for line in f:
                    log_wrapper(line[0:-1])
                    self.parse_line(line)
                log_wrapper("=== policy file end")
        except IOError:
            # without policy file, no device will be allowed to passed through
            log_wrapper(traceback.format_exc())

        log_wrapper("=== rule list begin")
        log_list_wrapper(self.rule_list)
        log_wrapper("=== rule list end")

    def parse_line(self, line):
        """ parse one line of policy file, generate rule, and append it to
        self.rule_list

        Example:
        ALLOW:vid=056a pid=0315 class=03 # Wacom Intuos tablet
            => {'pid': '0315', 'vid': '056a', 'allow': True, 'class': '03'}
        DENY: class=03 subclass=01 prot=01 # HID Boot keyboards
            => {'prot': '01', 'class': '03', 'allow': False, 'subclass': '01'}
        ALLOW: # Otherwise allow everything else
            => {'allow': True}

        :param line: (str) single line of policy file
        :return: None
        """
        # 1. remove comments
        # ^([^#]*)(#.*)?$
        i = line.find("#")
        if i != -1:
            line = line[0:i]

        # 2. split action and match field
        # ^\s*(ALLOW|DENY)\s*:\s*([^:]*)$
        if ":" not in line:
            return
        parts = line.split(":")
        n = len(parts)
        if n == 0 or n > 2:
            return

        # 3. parse action
        # \s*(ALLOW|DENY)\s*
        rule = {}
        action = parts[0].strip()
        if action.lower() == "allow":
            rule[self._ALLOW] = True
        elif action.lower() == "deny":
            rule[self._ALLOW] = False
        else:
            return

        # 4. parse key=value pairs
        # pattern = r"\s*(class|subclass|prot|vid|pid|rel)\s*=\s*([0-9a-f]+)"
        if n == 2:
            beg, target = 0, parts[1].strip()
            for matchNum, match in enumerate(re.finditer(self._PATTERN, target,
                                                         re.IGNORECASE)):
                if beg != match.start():
                    log_wrapper("policy line ignored: " + line)
                    return
                beg = match.end()
                if len(match.groups()) != 2 \
                        or not match.groups()[0] \
                        or not match.groups()[1]:
                    log_wrapper("policy line ignored: " + line)
                    return
                if match.groups()[0].lower() not in self.rule_list:
                    rule[match.groups()[0].lower()] = match.groups()[1].lower()
            if beg != len(target):
                log_wrapper("policy line ignored: " + line)
                return

        self.rule_list.append(rule)

    def match_device_interface(self, rule, device, interface):
        """check if a rule can match a device and one of its interface
        for device, check its vid, pid, rel
        for interface, check its class, subclass, prot

        :param rule: (dict) the rule to match
        :param device:(UsbDevice) the device to check
        :param interface:(UsbInterface) the interface to check
        :return:(bool) if they match
        """
        for k in [k for k in rule if k in self._KEY_MAP_DEVICE]:
            log_wrapper("check {} props[{}] against {}".format(
                interface.get_node(), k, str(rule)))
            if not hex_equal(rule[k], device[self._KEY_MAP_DEVICE[k]]):
                return False

        for k in [k for k in rule if k in self._KEY_MAP_INTERFACE]:
            log_wrapper("check {} props[{}] against {}".format(
                interface.get_node(), k, str(rule)))
            if not hex_equal(rule[k], interface[self._KEY_MAP_INTERFACE[k]]):
                return False

        log_wrapper("found matching rule: " + str(rule))
        return True

    def check(self, device):
        """check if a usb device can be passed through

        if one of device's interfaces is denied by rule, the device is denied.
        Normally, there should be a catch all rule at last line of policy file:
            ALLOW: # Otherwise allow everything else
        If there isn't, and no matching rule is found, the interface will also
        be denied.

        Only if all interfaces are allowed, the device is allowed to passed
        through.

        Example of a device before checking
        UsbDevice: (u'1-2', {'idVendor': '17ef', 'ID_VENDOR_FROM_DATABASE':
        u'Lenovo', 'bConfigurationValue': '1', 'bDeviceClass': '00',
        'ID_MODEL_FROM_DATABASE': '', 'version': ' 2.00', 'idProduct': '383c',
          'bNumInterfaces': ' 1', 'serial': 'H8RU2TGE', 'bcdDevice': '0100'})
            |__ UsbInterface: (u'1-2:1.0', {'bInterfaceNumber': '00',
            'bInterfaceClass': '08', 'bInterfaceProtocol': '50',
            'bInterfaceSubClass': '06'})
            |__ Allow: False

        UsbDevice: (u'4-1.2', {'idVendor': '2717', 'ID_VENDOR_FROM_DATABASE':
         '' , 'bConfigurationValue': '1', 'bDeviceClass': '00',
        'ID_MODEL_FROM_DATABASE': '', 'version': ' 2.00', 'idProduct': 'ff48',
          'bNumInterfaces': ' 2', 'serial': '7b06864', 'bcdDevice': '0404'})
            |__ UsbInterface: (u'4-1.2:1.0', {'bInterfaceNumber': '00',
            'bInterfaceClass': 'ff', 'bInterfaceProtocol': '00',
            'bInterfaceSubClass': 'ff'})
            |__ UsbInterface: (u'4-1.2:1.1', {'bInterfaceNumber': '01',
            'bInterfaceClass': 'ff', 'bInterfaceProtocol': '01',
            'bInterfaceSubClass': '42'})
            |__ Allow: False

        :param device:(UsbDevice) the device to check
        :return:(bool) if allow pass through
        """
        log_wrapper("policy check: " + device.get_node())
        for i in device.get_all_interfaces():
            allow_interface = False
            for r in self.rule_list:
                if self.match_device_interface(r, device, i):
                    if r[self._ALLOW]:
                        log_wrapper("allow " + i.get_node())
                        allow_interface = True
                        break
                    else:
                        log_wrapper("deny " + i.get_node())
                        return False
            if not allow_interface:
                log_wrapper("deny " + i.get_node() + ", no matching rule")
                return False

        log_wrapper("allow " + device.get_node())
        return True


def parse_args():
    parser = argparse.ArgumentParser(
        description="scanner to get USB devices info")
    parser.add_argument("-d", "--diagnostic", dest="diagnostic",
                        action="store_true",
                        help="enable diagnostic mode")
    return parser.parse_args()


def to_pusb(device):
    """ convert UsbDevice to pusb dict

    Example pusb dict:
    [{"product-desc": "", "product-id": "383c", "description":
    "Lenovo_H8RU2TGE", "vendor-desc": "Lenovo", "version": "2.00", "vendor-id":
    "17ef", "path": "1-2", "serial": "H8RU2TGE"}, {"product-desc":
    "TransMemory-Mini / Kingston DataTraveler 2.0 Stick (2GB)",
    "product-id": "6544", "description":"Toshiba Corp._TransMemory-Mini /
    Kingston DataTraveler 2.0 Stick (2GB)_9875B1B3F9F4CD301917964B",
      "vendor-desc": "Toshiba Corp.", "version": "2.00", "vendor-id": "0930",
      "path": "4-1.2", "serial": "9875B1B3F9F4CD301917964B"}]

    :param device:(UsbDevice) the device to convert
    :return:(dict) the key value pairs for pusb
    """
    pusb = {}

    pusb["path"] = device.get_node()
    # strip the leading space of "version"
    pusb["version"] = device[UsbDevice._VERSION].strip()
    pusb["vendor-id"] = device[UsbDevice._ID_VENDOR]
    pusb["product-id"] = device[UsbDevice._ID_PRODUCT]
    pusb["vendor-desc"] = device[UsbDevice._DESC_VENDOR]
    pusb["product-desc"] = device[UsbDevice._DESC_PRODUCT]
    pusb["serial"] = device[UsbDevice._SERIAL]

    pusb["description"] = pusb["vendor-desc"]
    if pusb["product-desc"]:
        pusb["description"] += "_" + pusb["product-desc"]
    if pusb["serial"]:
        pusb["description"] += "_" + pusb["serial"]

    return pusb


if __name__ == "__main__":
    args = parse_args()
    if args.diagnostic:
        __DIAGNOSTIC = True

    # get usb info
    devices, interfaces = get_usb_info()

    # debug info
    log_list_wrapper(devices)
    log_list_wrapper(interfaces)

    # match interface to device
    for i in interfaces:
        for d in devices:
            if i.is_child_of(d):
                d.add_interface(i)

    log_list_wrapper(devices)

    # do policy check
    policy = Policy()
    pusbs = []
    for d in devices:
        if d.is_ready():
            d.set_allow(policy.check(d))
            if d.is_allow():
                pusbs.append(to_pusb(d))
        else:
            log_wrapper("ignore not ready device: " + str(d))

    # pass pusbs in json to XAPI
    print(json.dumps(pusbs))
