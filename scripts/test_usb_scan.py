#!/usr/bin/env python
#
# unittest for usb_scan.py

try:
    from collections.abc import Mapping, Container, Iterable
except ImportError: # python2
    from collections import Mapping, Container, Iterable
import mock
import os
import shutil
import sys
import tempfile
import unittest

def nottest(obj):
    obj.__test__ = False
    return obj

sys.modules["xcp"] = mock.Mock()
sys.modules["xcp.logger"] = mock.Mock()
sys.modules["pyudev"] = mock.Mock()


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
    pass


class TestUsbScan(unittest.TestCase):

    def setUp(self):
        try:
            self.work_dir = tempfile.mkdtemp(prefix="test_usb_scan")
        except:
            raise

    def tearDown(self):
        shutil.rmtree(self.work_dir, ignore_errors=True)

    @nottest
    def test_usb_common(self, moc_devices, moc_interfaces, moc_results,
                        path="./scripts/usb-policy.conf"):
        from . import usb_scan
        mock_setup(usb_scan, moc_devices, moc_interfaces, path)

        devices, interfaces = usb_scan.get_usb_info()

        pusbs = usb_scan.make_pusbs_list(devices, interfaces)

        # pass pusbs in json to XAPI
        self.assertEqual(sorted(pusbs), sorted(moc_results))

    @nottest
    def test_usb_exit(self, devices, interfaces, results,
                      path="./scripts/usb-policy.conf", msg=""):
        with self.assertRaises(SystemExit) as cm:
            self.test_usb_common(devices, interfaces, results, path)
        if msg:
            self.assertIn(msg, str(cm.exception))

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
                    "speed": "480"
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
                "serial": "",
                "speed": "480"
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
                    "speed": "12"
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
                "serial": "",
                "speed": "12"
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
        self.test_usb_exit([], [], [], "not_exist.conf")

    @nottest
    def test_usb_config_error_common(self, content, msg):
        path = os.path.join(self.work_dir, "usb-policy.conf")
        with open(path, "w") as f:
            f.write(content)
        self.test_usb_exit([], [], [], path, msg)

    def test_usb_config_error_unexpected_chars_with_comment(self):
        content = """ss# unexpected words with comment
ALLOW:vid=056a pid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content,
                                          "to unpack")

    def test_usb_config_error_duplicated_key(self):
        content = """# duplicated key word
ALLOW:vid=056a vid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content, "duplicated tag")

    def test_usb_config_error_invalid_key(self):
        content = """# invalid key word
ALLOW:vid=056a psid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content, "Malformed policy rule, "
                                                   "unable to parse")

    def test_usb_config_error_hex_length_4(self):
        content = """# hex length not 4
ALLOW:vid=056a pid=031 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content, "length error")

    def test_usb_config_error_hex_length_2(self):
        content = """# hex length not 2
DENY:vid=056a pid=0314 class=035 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content, "length error")

    def test_usb_config_error_action_key(self):
        content = """# wrong action key word
ALLOWED:vid=056a pid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content, "Malformed action")

    def test_usb_config_error_unexpected_chars_end(self):
        content = """# unexpected words in the end
ALLOW:vid=056a pid=0314 class=03 kk # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content, "Malformed policy rule, "
                                                   "unable to parse")

    def test_usb_config_error_unexpected_chars_beg(self):
        content = """# unexpected words at the beginning
ii ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content, "Malformed action")

    def test_usb_config_error_unexpected_chars_mid(self):
        content = """# unexpected words in the middle
ALLOW:vid=056a pid=0314 jj class=03  # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content, "Malformed policy rule, "
                                                   "unable to parse")

    def test_usb_config_error_unexpected_non_empty_line(self):
        content = """# unexpected non empty line
ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
aa
ALLOW: # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content,
                                          "to unpack")

    def test_usb_config_error_missing_colon(self):
        content = """# missing colon after action
ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
ALLOW # Otherwise allow everything else
"""
        self.test_usb_config_error_common(content,
                                          "to unpack")
