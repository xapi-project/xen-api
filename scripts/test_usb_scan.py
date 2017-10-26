#!/usr/bin/env python
#
# unittest for usb_scan.py

from collections import Mapping, Container, Iterable
import mock
from nose.tools import nottest
import os
import shutil
import sys
import tempfile
import unittest

sys.modules["xcp"] = mock.Mock()
sys.modules["xcp.logger"] = mock.Mock()
sys.modules["pyudev"] = mock.Mock()

log_file = None


class MocDeviceAttrs(Mapping):
    def __init__(self, device):
        self.d = device.get_attr()

    def __iter__(self):
        for name in self.d:
            yield name

    def __len__(self):
        return len(self.d)

    def __getitem__(self, name):
        return self.d.get(name)


class MocDevice(Mapping):

    def __init__(self, d):
        self.d = d

    @property
    def sys_name(self):
        return self.d.get("name")

    def get_prop(self):
        return self.d.get("props")

    def get_attr(self):
        return self.d.get("attrs")

    @property
    def attributes(self):
        return MocDeviceAttrs(self)

    def __iter__(self):
        for name in self.get_prop():
            yield name

    def __len__(self):
        return len(self.get_prop())

    def __getitem__(self, name):
        return self.get_prop().get(name)


class MocEnumerator(object):

    def __init__(self, ds):
        self.ds = ds

    def __iter__(self):
        for d in self.ds:
            yield MocDevice(d)


class MocContext(object):

    def __init__(self, devices, interfaces):
        self.devices = devices
        self.interfaces = interfaces

    def list_devices(self, **kwargs):
        if "usb" == kwargs.pop("subsystem"):
            dev_type = kwargs.pop("DEVTYPE")
            if "usb_device" == dev_type:
                return MocEnumerator(self.devices)
            elif "usb_interface" == dev_type:
                return MocEnumerator(self.interfaces)
        return MocEnumerator([])


def mock_setup(mod, devices, interfaces, path):
    mod.log.error = test_log
    mod.log.debug = test_log
    mod.Policy._PATH = path
    mod.pyudev.Context = mock.Mock(return_value=MocContext(
        devices, interfaces))


@nottest
def test_log(m):
    global log_file
    with open(log_file, "a+") as f:
        f.write("{}: {}\n".format(sys.argv[0], m))


class TestUsbScan(unittest.TestCase):

    def setUp(self):
        global log_file
        try:
            self.work_dir = tempfile.mkdtemp(prefix="test_usb_scan")
            log_file = os.path.join(self.work_dir, "user.log")
        except:
            raise

    def tearDown(self):
        shutil.rmtree(self.work_dir, ignore_errors=True)

    @nottest
    def test_usb_common(self, moc_devices, moc_interfaces, moc_results,
                        path="./scripts/usb-policy.conf"):
        import usb_scan
        mock_setup(usb_scan, moc_devices, moc_interfaces, path)

        devices, interfaces = usb_scan.get_usb_info()

        # debug info
        usb_scan.log_list(devices)
        usb_scan.log_list(interfaces)

        # match interface to device
        for i in interfaces:
            for d in devices:
                if i.is_child_of(d):
                    d.add_interface(i)

        usb_scan.log_list(devices)

        # do policy check
        policy = usb_scan.Policy()
        pusbs = [usb_scan.to_pusb(d) for d in devices if
                 d.is_ready() and policy.check(d)]

        # pass pusbs in json to XAPI
        self.assertItemsEqual(pusbs, moc_results)

    @nottest
    def test_usb_exit(self, moc_devices, moc_interfaces, moc_results,
                        path="./scripts/usb-policy.conf"):
        with self.assertRaises(SystemExit):
            self.test_usb_common(moc_devices, moc_interfaces, moc_results, path)

    def test_usb_dongle(self):
        devices = [
            {
                "name": "1-2",
                "props": {
                    "ID_VENDOR_FROM_DATABASE": "Feitian Technologies, Inc."
                },
                "attrs": {
                    "idVendor": "096e",
                    "bNumInterfaces": " 1",
                    "bConfigurationValue": "1",
                    "bcdDevice": "010a",
                    "version": " 1.10",
                    "idProduct": "0302",
                    "bDeviceClass": "00",
                }
            }
        ]
        interfaces = [
            {
                "name": "1-2:1.0",
                "attrs": {
                    "bInterfaceClass": "03",
                    "bInterfaceSubClass": "00",
                    "bInterfaceProtocol": "00",
                    "bInterfaceNumber": "00",
                }
            }
        ]
        results = [
            {
                "product-desc": "",
                "product-id": "0302",
                "description": "Feitian Technologies, Inc.",
                "vendor-desc": "Feitian Technologies, Inc.",
                "version": "1.10",
                "vendor-id": "096e",
                "path": "1-2",
                "serial": ""
            }
        ]
        self.test_usb_common(devices, interfaces, results)

    def test_usb_dongle_on_hub(self):
        devices = [
            {
                "name": "1-2.1",
                "props": {
                    "ID_VENDOR_FROM_DATABASE": "Feitian Technologies, Inc."
                },
                "attrs": {
                    "idVendor": "096e",
                    "bNumInterfaces": " 1",
                    "bConfigurationValue": "1",
                    "bcdDevice": "010a",
                    "version": " 1.10",
                    "idProduct": "0302",
                    "bDeviceClass": "00",
                }
            }
        ]
        interfaces = [
            {
                "name": "1-2.1:1.0",
                "attrs": {
                    "bInterfaceClass": "03",
                    "bInterfaceSubClass": "00",
                    "bInterfaceProtocol": "00",
                    "bInterfaceNumber": "00",
                }
            }
        ]
        results = [
            {
                "product-desc": "",
                "product-id": "0302",
                "description": "Feitian Technologies, Inc.",
                "vendor-desc": "Feitian Technologies, Inc.",
                "version": "1.10",
                "vendor-id": "096e",
                "path": "1-2.1",
                "serial": ""
            }
        ]
        self.test_usb_common(devices, interfaces, results)

    def test_usb_dongle_unbinded(self):
        devices = [
            {
                "name": "1-2",
                "props": {
                    "ID_VENDOR_FROM_DATABASE": "Feitian Technologies, Inc."
                },
                "attrs": {
                    "idVendor": "096e",
                    "bNumInterfaces": "",
                    "bConfigurationValue": "",
                    "bcdDevice": "010a",
                    "version": " 1.10",
                    "idProduct": "0302",
                    "bDeviceClass": "00",
                }
            }
        ]
        interfaces = [
        ]
        results = [
        ]
        self.test_usb_common(devices, interfaces, results)

    def test_usb_keyboard(self):
        devices = [
            {
                "name": "1-2",
                "props": {
                    "ID_VENDOR_FROM_DATABASE": "Dell Computer Corp."
                },
                "attrs": {
                    "idVendor": "413c",
                    "bNumInterfaces": " 2",
                    "bConfigurationValue": "1",
                    "bcdDevice": "0110",
                    "version": " 2.00",
                    "idProduct": "2113",
                    "bDeviceClass": "00",
                }
            }
        ]
        interfaces = [
            {
                "name": "1-2:1.0",
                "attrs": {
                    "bInterfaceClass": "03",
                    "bInterfaceSubClass": "01",
                    "bInterfaceProtocol": "01",
                    "bInterfaceNumber": "00",
                }
            },
            {
                "name": "1-2:1.1",
                "attrs": {
                    "bInterfaceClass": "03",
                    "bInterfaceSubClass": "00",
                    "bInterfaceProtocol": "00",
                    "bInterfaceNumber": "01",
                }
            }
        ]
        results = [
        ]
        self.test_usb_common(devices, interfaces, results)

    def test_usb_config_missing(self):
        self.test_usb_exit([], [], [], "not_exit.conf")

    @nottest
    def test_usb_config_error_common(self, content):
        path = os.path.join(self.work_dir, "usb-policy.conf")
        with open(path, "w") as f:
            f.write(content)
        self.test_usb_exit([], [], [], path)

    def test_usb_config_error_01(self):
        content = """ss# unexpected words with comment
ALLOW:vid=056a pid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_02(self):
        content = """# duplicated key word
ALLOW:vid=056a vid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_03(self):
        content = """# invalid key word
ALLOW:vid=056a psid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_04(self):
        content = """# hex length not 4
ALLOW:vid=056a pid=031 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_05(self):
        content = """# hex length not 2
DENY:vid=056a pid=0314 class=035 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_06(self):
        content = """# wrong action key word
ALLOWED:vid=056a pid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_07(self):
        content = """# unexpected words in the end
ALLOW:vid=056a pid=0314 class=03 kk # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_08(self):
        content = """# unexpected words at the beginning
ii ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_09(self):
        content = """# unexpected words in the middle
ALLOW:vid=056a pid=0314 jj class=03  # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_10(self):
        content = """# unexpected non empty line
ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
aa
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

    def test_usb_config_error_11(self):
        content = """# missing comma after action
ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
ALLOW # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content)

