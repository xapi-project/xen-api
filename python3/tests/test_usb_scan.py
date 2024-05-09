#!/usr/bin/env python
#
# unittest for usb_scan.py

import os
import shutil
import sys
import tempfile
import unittest
from collections.abc import Mapping
from typing import cast

import mock

from python3.tests.import_helper import import_file_as_module
# mock modules to avoid dependencies
sys.modules["xcp"] = mock.Mock()
sys.modules["xcp.logger"] = mock.Mock()
sys.modules["pyudev"] = mock.Mock()
usb_scan = import_file_as_module("python3/libexec/usb_scan.py")


class MocDeviceAttrs(Mapping):
    def __init__(self, device):
        self.d = device.get_attr()

    def __iter__(self):  # pragma: no cover
        yield from self.d

    def __len__(self):  # pragma: no cover
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

    def __iter__(self):  # pragma: no cover
        yield from self.get_prop()

    def __len__(self):  # pragma: no cover
        return len(self.get_prop())

    def __getitem__(self, name):
        return self.get_prop().get(name)


class MocEnumerator():
    def __init__(self, ds):
        self.ds = ds

    def __iter__(self):
        for d in self.ds:
            yield MocDevice(d)


class MocContext():
    def __init__(self, devices, interfaces):
        self.devices = devices
        self.interfaces = interfaces

    def list_devices(self, **kwargs):
        assert kwargs.pop("subsystem") == "usb"
        dev_type = kwargs.pop("DEVTYPE")
        if dev_type == "usb_device":
            return MocEnumerator(self.devices)
        elif dev_type == "usb_interface":
            return MocEnumerator(self.interfaces)
        raise AssertionError(f"unexpected {dev_type}")  # pragma: no cover


def mock_setup(mod, devices, interfaces, path):
    mod.log.error = verify_log
    mod.log.debug = verify_log
    mod.Policy._PATH = path
    mod.pyudev.Context = mock.Mock(
            return_value=MocContext(devices, interfaces))


def verify_log(_):
    pass


class TestUsbScan(unittest.TestCase):
    def setUp(self):
        self.work_dir = tempfile.mkdtemp(prefix="test_usb_scan")

    def tearDown(self):
        shutil.rmtree(self.work_dir, ignore_errors=True)

    def verify_usb_common(
        self, moc_devices,
        moc_interfaces,
        moc_results,
        # Use relative path to allow tests to be started in subdirectories
        path = os.path.dirname(__file__) + "/../../scripts/usb-policy.conf"
    ):

        mock_setup(usb_scan, moc_devices, moc_interfaces, path)

        devices, interfaces = usb_scan.get_usb_info()

        usb_scan.log_list(devices)
        usb_scan.log_list(interfaces)

        pusbs = usb_scan.make_pusbs_list(devices, interfaces)

        # pass pusbs in json to XAPI
        self.assertEqual(sorted(pusbs), sorted(moc_results))

    def verify_usb_exit(
            self, devices, interfaces, results,
            path="./scripts/usb-policy.conf",
            msg=""
            ):
        with self.assertRaises(SystemExit) as cm:
            self.verify_usb_common(devices, interfaces, results, path)
        if msg:
            # cm.exception.code is int type whose format
            # looks like "duplicated tag'vid' found,
            # malformed line ALLOW:vid=056a vid=0314 class=03"
            self.assertIn(msg, cast(str, cm.exception.code))  # code is a str

    def test_usb_dongle(self):
        devices = [
            {
                "name": "1-2",
                "props": {"ID_VENDOR_FROM_DATABASE": "Feitian Technologies, Inc."},
                "attrs": {
                    "idVendor": b"096e",
                    "bNumInterfaces": b" 1",
                    "bConfigurationValue": b"1",
                    "bcdDevice": b"010a",
                    "version": b" 1.10",
                    "idProduct": b"0302",
                    "bDeviceClass": b"00",
                    "speed": b"480",
                },
            }
        ]
        interfaces = [
            {
                "name": "1-2:1.0",
                "attrs": {
                    "bInterfaceClass": b"03",
                    "bInterfaceSubClass": b"00",
                    "bInterfaceProtocol": b"00",
                    "bInterfaceNumber": b"00",
                },
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
                "speed": "480",
            }
        ]
        self.verify_usb_common(devices, interfaces, results)

    def test_usb_dongle_on_hub(self):
        devices = [
            {
                "name": "1-2.1",
                "props": {"ID_VENDOR_FROM_DATABASE": "Feitian Technologies, Inc."},
                "attrs": {
                    "idVendor": b"096e",
                    "bNumInterfaces": b" 1",
                    "bConfigurationValue": b"1",
                    "bcdDevice": b"010a",
                    "version": b" 1.10",
                    "idProduct": b"0302",
                    "bDeviceClass": b"00",
                    "speed": b"12",
                },
            }
        ]
        interfaces = [
            {
                "name": "1-2.1:1.0",
                "attrs": {
                    "bInterfaceClass": b"03",
                    "bInterfaceSubClass": b"00",
                    "bInterfaceProtocol": b"00",
                    "bInterfaceNumber": b"00",
                },
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
                "speed": "12",
            }
        ]
        self.verify_usb_common(devices, interfaces, results)

    def test_usb_dongle_unbinded(self):
        devices = [
            {
                "name": "1-2",
                "props": {"ID_VENDOR_FROM_DATABASE": "Feitian Technologies, Inc."},
                "attrs": {
                    "idVendor": b"096e",
                    "bNumInterfaces": b"",
                    "bConfigurationValue": b"",
                    "bcdDevice": b"010a",
                    "version": b" 1.10",
                    "idProduct": b"0302",
                    "bDeviceClass": b"00",
                },
            }
        ]
        interfaces = []
        results = []
        self.verify_usb_common(devices, interfaces, results)

    def test_usb_keyboard(self):
        devices = [
            {
                "name": "1-2",
                "props": {"ID_VENDOR_FROM_DATABASE": "Dell Computer Corp."},
                "attrs": {
                    "idVendor": b"413c",
                    "bNumInterfaces": b" 2",
                    "bConfigurationValue": b"1",
                    "bcdDevice": b"0110",
                    "version": b" 2.00",
                    "idProduct": b"2113",
                    "bDeviceClass": b"00",
                },
            }
        ]
        interfaces = [
            {
                "name": "1-2:1.0",
                "attrs": {
                    "bInterfaceClass": b"03",
                    "bInterfaceSubClass": b"01",
                    "bInterfaceProtocol": b"01",
                    "bInterfaceNumber": b"00",
                },
            },
            {
                "name": "1-2:1.1",
                "attrs": {
                    "bInterfaceClass": b"03",
                    "bInterfaceSubClass": b"00",
                    "bInterfaceProtocol": b"00",
                    "bInterfaceNumber": b"01",
                },
            },
        ]
        results = []
        self.verify_usb_common(devices, interfaces, results)

    def test_usb_config_missing(self):
        self.verify_usb_exit([], [], [], "not_exist.conf")

    def verify_usb_config_error_common(self, content, msg):
        path = os.path.join(self.work_dir, "usb-policy.conf")
        with open(path, "w") as f:
            f.write(content)
        self.verify_usb_exit([], [], [], path, msg)

    def test_usb_config_error_unexpected_chars_with_comment(self):
        content = """ss# unexpected words with comment
ALLOW:vid=056a pid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(content, "to unpack")

    def test_usb_config_error_duplicated_key(self):
        content = """# duplicated key word
ALLOW:vid=056a vid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(content, "duplicated tag")

    def test_usb_config_error_invalid_key(self):
        content = """# invalid key word
ALLOW:vid=056a psid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(
                content, "Malformed policy rule, unable to parse")

    def test_usb_config_error_hex_length_4(self):
        content = """# hex length not 4
ALLOW:vid=056a pid=031 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(content, "length error")

    def test_usb_config_error_hex_length_2(self):
        content = """# hex length not 2
DENY:vid=056a pid=0314 class=035 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(content, "length error")

    def test_usb_config_error_action_key(self):
        content = """# wrong action key word
ALLOWED:vid=056a pid=0314 class=03 # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(content, "Malformed action")

    def test_usb_config_error_unexpected_chars_end(self):
        content = """# unexpected words in the end
ALLOW:vid=056a pid=0314 class=03 kk # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(
                content, "Malformed policy rule, unable to parse")

    def test_usb_config_error_unexpected_chars_beg(self):
        content = """# unexpected words at the beginning
ii ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(content, "Malformed action")

    def test_usb_config_error_unexpected_chars_mid(self):
        content = """# unexpected words in the middle
ALLOW:vid=056a pid=0314 jj class=03  # Wacom Intuos tablet
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(
                content, "Malformed policy rule, unable to parse")

    def test_usb_config_error_unexpected_non_empty_line(self):
        content = """# unexpected non empty line
ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
aa
ALLOW: # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(content, "to unpack")

    def test_usb_config_error_missing_colon(self):
        content = """# missing colon after action
ALLOW:vid=056a pid=0314 class=03  # Wacom Intuos tablet
ALLOW # Otherwise allow everything else
"""
        self.verify_usb_config_error_common(content, "to unpack")
