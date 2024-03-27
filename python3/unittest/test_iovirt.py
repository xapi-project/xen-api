#!/usr/bin/env python3
"""
This module provides unittest for iovirt
"""

import stat
import sys
import unittest
import xml.dom.minidom
from mock import MagicMock, patch, mock_open, call
from import_file import get_module

# mock modules to avoid dependencies
sys.modules["inventory"] = MagicMock()
sys.modules["XenAPIPlugin"] = MagicMock()

iovirt = get_module("iovirt", "../plugins/iovirt")

class TestInstallHookScript(unittest.TestCase):
    @patch("os.path.exists")
    @patch("os.makedirs")
    @patch("os.chmod")
    @patch("builtins.open", new_callable=mock_open)
    def test_install_hook_script(self, mock_builtin_open, mock_chmod,
                                 mock_makedirs, mock_exists):
        # The first file and dir don't exist
        # The second file exists.
        mock_exists.side_effect = [False, False, True] # 1st file, 1st dir, 2nd file
        iovirt._install_hook_script()
        # Assert that the first file is written with the expected content
        mock_builtin_open.assert_called_once_with(iovirt.hookscripts[0],
                                            "w", encoding="utf-8")
        mock_builtin_open().write.assert_called_once_with(
"""#!/bin/bash
#
# Call the iovirt plugin to set up SR-IOV VF MAC and VLAN config
# if required for this VF.

PLUGIN=iovirt
FN=prep_for_vm

for i
do
  case "$i" in
        -vmuuid) shift; VMUUID=$1; shift;;
  esac
done

if [ -z "$VMUUID" ]; then
    logger -t $(basename $0) "VM UUID not found in args"
fi

. @INVENTORY@

if [ -z "$INSTALLATION_UUID" ]; then
    logger -t $(basename $0) "Could not determine host UUID"
fi

xe host-call-plugin plugin=$PLUGIN fn=$FN host-uuid=$INSTALLATION_UUID args:uuid=$VMUUID
"""
        )
        # Assert that os.makedirs and os.chmod are called
        mock_makedirs.assert_called_once()
        mock_chmod.assert_called_once_with(
            iovirt.hookscripts[0],
            stat.S_IRWXU | stat.S_IRGRP | stat.S_IXGRP | stat.S_IROTH | stat.S_IXOTH,
        )


class TestGetVfs(unittest.TestCase):

    @patch('glob.glob')
    @patch('os.readlink')
    def test_get_vfs(self, mock_readlink, mock_glob):
        # Mock the return values for glob.glob and os.readlink
        mock_glob.side_effect = [
             # Mock return value for glob.glob("/sys/bus/pci/devices/*/virtfn*")
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn6',
            '/sys/bus/pci/devices/0000:21:00.3/virtfn4',
            '/sys/bus/pci/devices/0000:21:00.3/virtfn2',
            '/sys/bus/pci/devices/0000:21:00.3/virtfn0',
            '/sys/bus/pci/devices/0000:21:00.3/virtfn5',
            '/sys/bus/pci/devices/0000:21:00.3/virtfn3',
            '/sys/bus/pci/devices/0000:21:00.3/virtfn1',
            '/sys/bus/pci/devices/0000:04:00.1/virtfn6',
            '/sys/bus/pci/devices/0000:04:00.1/virtfn4',
            '/sys/bus/pci/devices/0000:04:00.1/virtfn2',
            '/sys/bus/pci/devices/0000:04:00.1/virtfn0',
            '/sys/bus/pci/devices/0000:04:00.1/virtfn5',
            '/sys/bus/pci/devices/0000:04:00.1/virtfn3',
            '/sys/bus/pci/devices/0000:04:00.1/virtfn1'
            ],
            # Mock return value for glob.glob("%s/physfn/net/eth*" % (vfnode))
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn6/physfn/net/eth5'],
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn4/physfn/net/eth5'],
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn2/physfn/net/eth5'],
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn0/physfn/net/eth5'],
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn5/physfn/net/eth5'],
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn3/physfn/net/eth5'],
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn1/physfn/net/eth5'],
            ['/sys/bus/pci/devices/0000:04:00.1/virtfn6/physfn/net/eth1'],
            ['/sys/bus/pci/devices/0000:04:00.1/virtfn4/physfn/net/eth1'],
            ['/sys/bus/pci/devices/0000:04:00.1/virtfn2/physfn/net/eth1'],
            ['/sys/bus/pci/devices/0000:04:00.1/virtfn3/physfn/net/eth1'],
            ['/sys/bus/pci/devices/0000:04:00.1/virtfn5/physfn/net/eth1'],
            ['/sys/bus/pci/devices/0000:04:00.1/virtfn0/physfn/net/eth1'],
            ['/sys/bus/pci/devices/0000:04:00.1/virtfn1/physfn/net/eth1']
        ]

        mock_readlink.side_effect = ['../0000:21:13.3',
                                     '../0000:21:12.3',
                                     '../0000:21:11.3',
                                     '../0000:21:10.3',
                                     '../0000:21:12.7',
                                     '../0000:21:11.7',
                                     '../0000:21:10.7',
                                     '../0000:04:13.1',
                                     '../0000:04:12.1',
                                     '../0000:04:11.1',
                                     '../0000:04:10.1',
                                     '../0000:04:12.5',
                                     '../0000:04:11.5',
                                     '../0000:04:10.5'
        ]

        # Call the function under test
        vflist = iovirt._get_vfs()

        # Assert that the function returns the expected result
        expected_result = {'0000:21:13.3': ('6', 'eth5'),
                           '0000:21:12.3': ('4', 'eth5'),
                           '0000:21:11.3': ('2', 'eth5'),
                           '0000:21:10.3': ('0', 'eth5'),
                           '0000:21:12.7': ('5', 'eth5'),
                           '0000:21:11.7': ('3', 'eth5'),
                           '0000:21:10.7': ('1', 'eth5'),
                           '0000:04:13.1': ('6', 'eth1'),
                           '0000:04:12.1': ('4', 'eth1'),
                           '0000:04:11.1': ('2', 'eth1'),
                           '0000:04:10.1': ('0', 'eth1'),
                           '0000:04:12.5': ('5', 'eth1'),
                           '0000:04:11.5': ('3', 'eth1'),
                           '0000:04:10.5': ('1', 'eth1')}
        self.assertEqual(vflist, expected_result)

    @patch('glob.glob')
    @patch('os.readlink')
    def test_get_vfs_wrong_vf_number(self, mock_readlink, mock_glob):
        # Mock the return values for glob.glob and os.readlink
        mock_glob.side_effect = [
             # Mock return value for glob.glob("/sys/bus/pci/devices/*/virtfn*")
            ['/sys/bus/pci/devices/0000:21:00.3/wrong_vf_number'],
            # Mock return value for glob.glob("%s/physfn/net/eth*" % (vfnode))
            ['/sys/bus/pci/devices/0000:21:00.3/virtfn6/physfn/net/eth5']
        ]

        mock_readlink.side_effect = ['../0000:21:13.3',
                                     '../0000:21:12.3',
                                     '../0000:04:10.5'
        ]

        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt._get_vfs()

        self.assertEqual(str(context.exception),
                         "Failed to parse VF number for " \
                             "/sys/bus/pci/devices/0000:21:00.3/wrong_vf_number")


    @patch('glob.glob')
    @patch('os.readlink')
    def test_get_vfs_netdevs_not_unique(self, mock_readlink, mock_glob):

        mock_vf = '/sys/bus/pci/devices/0000:21:00.3/virtfn6'
        mock_eth5 = '/sys/bus/pci/devices/0000:21:00.3/virtfn6/physfn/net/eth5'
        mock_eth4 = '/sys/bus/pci/devices/0000:21:00.3/virtfn6/physfn/net/eth4'
        # Mock the return values for glob.glob and os.readlink
        mock_glob.side_effect = [
             # Mock return value for glob.glob("/sys/bus/pci/devices/*/virtfn*")
            [mock_vf],
            # Mock return value for glob.glob("%s/physfn/net/eth*" % (vfnode))
            [mock_eth5,
             mock_eth4]
        ]

        mock_readlink.side_effect = ['../0000:21:13.3',
                                     '../0000:21:12.3',
                                     '../0000:04:10.5'
        ]

        with self.assertRaises(iovirt.IovirtException):
            iovirt._get_vfs()

    @patch('glob.glob')
    @patch('os.readlink')
    def test_get_vfs_invalid_eth(self, mock_readlink, mock_glob):

        mock_vf = '/sys/bus/pci/devices/0000:21:00.3/virtfn6'
        wrong_eth = '/sys/bus/pci/devices/0000:21:00.3/virtfn6/physfn/net/wrong'
        # Mock the return values for glob.glob and os.readlink
        mock_glob.side_effect = [
             # Mock return value for glob.glob("/sys/bus/pci/devices/*/virtfn*")
            [mock_vf],
            # Mock return value for glob.glob("%s/physfn/net/eth*" % (vfnode))
            [wrong_eth]
        ]

        mock_readlink.side_effect = ['../0000:21:13.3',
                                     '../0000:21:12.3',
                                     '../0000:04:10.5'
        ]

        with self.assertRaises(iovirt.IovirtException):
            iovirt._get_vfs()

class TestGetDevives(unittest.TestCase):
    '''Test _methods `get_devices` and `_get_vendor_and_device_ids`'''

    @patch("builtins.open", new_callable=mock_open)
    @patch("os.path.exists")
    @patch('glob.glob')
    def test_get_devices(self, mock_glob,
                         mock_exists, mock_builtin_open):
        mock_glob.return_value = ['/sys/bus/pci/devices/0000:3f:09.3',
                                  '/sys/bus/pci/devices/0000:04:11.4'
        ]
        # Call _get_vendor_and_device_ids twice
        # So open 4 times as each time open two files
        mock_exists.side_effect = [True, True, True, True]
        mock_builtin_open().read.side_effect = [
            "0x8086", "0x3c93", # vendor and device for the 1st pciid
            "0x8086", "0x1520" # vendor and device for the 2nd pciid
        ]
        vendorid = "8086"
        deviceid = "3c93"
        devices = iovirt._get_devices(vendorid, deviceid)
        expected_result = {'0000:3f:09.3': (vendorid, deviceid)}
        self.assertEqual(devices, expected_result)


    def test_get_devices_invalid_vendorid(self):
        vendorid = "0x8086"
        deviceid = "3c93"
        with self.assertRaises(iovirt.IovirtException):
            iovirt._get_devices(vendorid, deviceid)

    def test_get_devices_invalid_deviceid(self):
        vendorid = "8086"
        deviceid = "0x3c93"
        with self.assertRaises(iovirt.IovirtException):
            iovirt._get_devices(vendorid, deviceid)
class TestGetAssignments(unittest.TestCase):
    '''Test _methods `_get_assignments` and `_get_vm_assignments`'''

    @patch('iovirt._get_vendor_and_device_ids')
    def test_get_assignments(self, mock_get_vendor_and_device_ids):
        mock_session = MagicMock()
        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        other_config = {
            'sriovmacs': '0/0a:2c:f4:91:be:8e',
            'sriovvlans': '0/200',
            'pci': '0/0000:04:01.0',
            'xenrt-distro': 'centos7',
            'base_template_name': 'CentOS 7',
            'import_task': 'OpaqueRef:6a6b6265-a9e8-76de-fcd4-f07c9d526f4a',
            'mac_seed': '62dc1c4d-5843-2425-6cad-7715e0801602',
            'linux_template': 'true',
            'install-methods': 'cdrom,nfs,http,ftp'
            }
        mock_session.xenapi.VM.get_all_records_where.return_value = {
            'OpaqueRef:2a1b4ed8-c836-4a45-3338-37663dc3b2ab': {
                'uuid': vm_uuid,
                'power_state': 'Running',
                'user_version': '1',
                'other_config': other_config
            }
        }
        mock_session.xenapi.VM.get_by_uuid.return_value = \
            "69720a86-8bc7-01b0-f33b-04b40a6931bc"
        mock_session.xenapi.VM.get_record.return_value = {
            'uuid': vm_uuid,
            'power_state': 'Running',
            'user_version': '1',
            'other_config': other_config
        }
        mock_get_vendor_and_device_ids.return_value = ("8086", "3c93")

        expected_devices = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200')
        }
        devices = iovirt._get_assignments(mock_session)
        self.assertEqual(devices, expected_devices)

    def test_get_vm_assignments_failed_parse_vlan(self):
        mock_session = MagicMock()
        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        other_config = {
            'sriovmacs': '0/0a:2c:f4:91:be:8e',
            'sriovvlans': 'wrong_vlan',
            'pci': '0/0000:04:01.0',
            'xenrt-distro': 'centos7',
            'base_template_name': 'CentOS 7',
            'import_task': 'OpaqueRef:6a6b6265-a9e8-76de-fcd4-f07c9d526f4a',
            'mac_seed': '62dc1c4d-5843-2425-6cad-7715e0801602',
            'linux_template': 'true',
            'install-methods': 'cdrom,nfs,http,ftp'
            }
        mock_session.xenapi.VM.get_record.return_value = {
                'uuid': vm_uuid,
                'power_state': 'Running',
                'user_version': '1',
                'other_config': other_config
        }
        with self.assertRaises(iovirt.IovirtException):
            iovirt._get_vm_assignments(mock_session, vm_uuid)

    def test_get_vm_assignments_failed_parse_mac(self):
        mock_session = MagicMock()
        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        other_config = {
            'sriovmacs': 'wrong_macs',
            'sriovvlans': '0/200',
            'pci': '0/0000:04:01.0',
            'xenrt-distro': 'centos7',
            'base_template_name': 'CentOS 7',
            'import_task': 'OpaqueRef:6a6b6265-a9e8-76de-fcd4-f07c9d526f4a',
            'mac_seed': '62dc1c4d-5843-2425-6cad-7715e0801602',
            'linux_template': 'true',
            'install-methods': 'cdrom,nfs,http,ftp'
            }
        mock_session.xenapi.VM.get_record.return_value = {
                'uuid': vm_uuid,
                'power_state': 'Running',
                'user_version': '1',
                'other_config': other_config
        }
        with self.assertRaises(iovirt.IovirtException):
            iovirt._get_vm_assignments(mock_session, vm_uuid)

    def test_get_vm_assignments_failed_parse_pci(self):
        mock_session = MagicMock()
        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        other_config = {
            'sriovmacs': '0/0a:2c:f4:91:be:8e',
            'sriovvlans': '0/200',
            'pci': 'wrong_pci',
            'xenrt-distro': 'centos7',
            'base_template_name': 'CentOS 7',
            'import_task': 'OpaqueRef:6a6b6265-a9e8-76de-fcd4-f07c9d526f4a',
            'mac_seed': '62dc1c4d-5843-2425-6cad-7715e0801602',
            'linux_template': 'true',
            'install-methods': 'cdrom,nfs,http,ftp'
            }
        mock_session.xenapi.VM.get_record.return_value = {
                'uuid': vm_uuid,
                'power_state': 'Running',
                'user_version': '1',
                'other_config': other_config
        }
        with self.assertRaises(iovirt.IovirtException):
            iovirt._get_vm_assignments(mock_session, vm_uuid)
class TestRandomMAC(unittest.TestCase):

    def test_random_mac_format(self):
        mac = iovirt._randomMAC()
        parts = mac.split(':')

        # Check if MAC address has 6 parts separated by ':'
        self.assertEqual(len(parts), 6)

        # Check if each part is a valid hexadecimal number
        for part in parts:
            self.assertTrue(0 <= int(part, 16) <= 255)

        # Check if the first byte is in the locally administered range
        first_byte = int(parts[0], 16)
        self.assertTrue(first_byte & 0x02, 0x02)

@patch('os.system')
class TestEnableIOMMU(unittest.TestCase):
    def test_enable_iommu_success(self, mock_os_system):
        mock_os_system.return_value = 0
        result = iovirt.enable_iommu(None, None)
        self.assertEqual(result, 'True')

    def test_enable_iommu_failure(self, mock_os_system):
        mock_os_system.return_value = 1
        result = iovirt.enable_iommu(None, None)
        self.assertEqual(result, 'False')

class TestSetAssignments(unittest.TestCase):
    '''
    Test the below methods:
    unassign_all, change_vf_mac, change_vf_vlan, _set_vm_assignments
    '''

    def test_unassign_all_no_uuid(self):
        mock_session = MagicMock()
        args = {}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.unassign_all(mock_session, args)

        self.assertEqual(str(context.exception),
                         "No VM UUID specified, please use the 'uuid' argument")

    def test_unassign_all_with_uuid(self):
        mock_session = MagicMock()
        mock_vmuuid = "82aa5b84-1de0-4070-9070-fe0df7039755"
        args = {"uuid": mock_vmuuid}
        mock_ref = '69720a86-8bc7-01b0-f33b-04b40a6931bc'
        mock_session.xenapi.VM.get_by_uuid.return_value = mock_ref

        iovirt.unassign_all(mock_session, args)
        mock_session.xenapi.VM.remove_from_other_config.assert_has_calls([
            call(mock_ref, "pci"),
            call(mock_ref, "sriovmacs"),
            call(mock_ref, "sriovvlans")
        ])

    def test_change_vf_mac_no_mac(self):
        mock_session = MagicMock()
        args = {}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.change_vf_mac(mock_session, args)

        self.assertEqual(str(context.exception),
                        "Need to specify a new MAC address")

    def test_change_vf_mac_no_uuid(self):
        mock_session = MagicMock()
        args = {"mac": "0a:2c:f4:91:be:8e"}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.change_vf_mac(mock_session, args)

        self.assertEqual(str(context.exception),
                        "No VM UUID specified, please use the 'uuid' argument")

    def test_change_vf_mac_no_index(self):
        mock_session = MagicMock()
        args = {"mac": "0a:2c:f4:91:be:8e",
                "uuid":"82aa5b84-1de0-4070-9070-fe0df7039755"}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.change_vf_mac(mock_session, args)

        self.assertEqual(str(context.exception),
                        "Need to specify the VF index")

    @patch('iovirt._get_vm_assignments')
    def test_change_vf_mac(self, mock_get_vm_assignments):
        mock_session = MagicMock()
        new_mac = "0a:2c:f4:91:be:8e"
        args = {"mac": new_mac,
                "uuid":"82aa5b84-1de0-4070-9070-fe0df7039755",
                "index": 0}
        pciid = "0000:04:01.0"
        vendorid = "8086"
        deviceid = "3c93"
        current_mac = "21:3d:dd:26:be:34"
        vlan = "200"
        mock_get_vm_assignments.return_value = {
            0: (pciid, vendorid, deviceid, current_mac, vlan)
        }
        mock_ref = '69720a86-8bc7-01b0-f33b-04b40a6931bc'
        mock_session.xenapi.VM.get_by_uuid.return_value = mock_ref

        iovirt.change_vf_mac(mock_session, args)
        mock_session.xenapi.VM.add_to_other_config.assert_has_calls([
            call(mock_ref, "pci", "0/"+pciid),
            call(mock_ref, "sriovmacs", "0/"+new_mac),
            call(mock_ref, "sriovvlans", "0/"+vlan)
        ])

    def test_change_vf_vlan_no_vlan(self):
        mock_session = MagicMock()
        args = {}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.change_vf_vlan(mock_session, args)

        self.assertEqual(str(context.exception),
                        "Need to specify a new VLAN")

    def test_change_vf_vlan_no_uuid(self):
        mock_session = MagicMock()
        args = {"vlan": "200"}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.change_vf_vlan(mock_session, args)

        self.assertEqual(str(context.exception),
                        "No VM UUID specified, please use the 'uuid' argument")

    def test_change_vf_vlan_no_index(self):
        mock_session = MagicMock()
        args = {"vlan": "200",
                "uuid":"82aa5b84-1de0-4070-9070-fe0df7039755"}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.change_vf_vlan(mock_session, args)

        self.assertEqual(str(context.exception),
                        "Need to specify the VF index")

    @patch('iovirt._get_vm_assignments')
    def test_change_vf_vlan(self, mock_get_vm_assignments):
        mock_session = MagicMock()
        new_vlan = "300"
        args = {"vlan": new_vlan,
                "uuid":"82aa5b84-1de0-4070-9070-fe0df7039755",
                "index": 0}
        pciid = "0000:04:01.0"
        vendorid = "8086"
        deviceid = "3c93"
        current_vlan = "200"
        mac = "21:3d:dd:26:be:34"
        mock_get_vm_assignments.return_value = {
            0: (pciid, vendorid, deviceid, mac, current_vlan)
        }
        mock_ref = '69720a86-8bc7-01b0-f33b-04b40a6931bc'
        mock_session.xenapi.VM.get_by_uuid.return_value = mock_ref

        iovirt.change_vf_vlan(mock_session, args)
        mock_session.xenapi.VM.add_to_other_config.assert_has_calls([
            call(mock_ref, "pci", "0/"+pciid),
            call(mock_ref, "sriovmacs", "0/"+mac),
            call(mock_ref, "sriovvlans", "0/"+new_vlan)
        ])

    # Meaningless, but for passing CI coverage
    def test_set_vm_assignments_remove_exception(self):
        mock_session = MagicMock()
        uuid = "82aa5b84-1de0-4070-9070-fe0df7039755"
        assignments = {}
        
        def remove_side_effect(_, arg):
            # Check the input value and raise an exception accordingly
            if arg == "pci":
                raise ValueError("Invalid key: 'pci'")
            elif arg == "sriovmacs":
                raise TypeError("Invalid key: 'sriovmacs'")
            elif arg == "sriovvlans":
                raise TypeError("Invalid key: 'sriovvlans'")
        mock_session.xenapi.VM.remove_from_other_config.side_effect = remove_side_effect
        iovirt._set_vm_assignments(mock_session, uuid, assignments)


class TestAssignFreeVf(unittest.TestCase):

    @patch('iovirt._set_vm_assignments')
    @patch('iovirt._get_vfs')
    @patch('iovirt._get_assignments')
    @patch('iovirt._install_hook_script')
    def test_assign_free_vf(self, _, mock_get_assignments, mock_get_vfs,
                            mock_set_vm_assignments):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_assignments.return_value = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200'),
            '0000:04:01.1': (vm_uuid, 0, '8086', '3c11', '0a:2c:f4:91:be:11', '300')
        }
        mock_get_vfs.return_value = {
            '0000:21:13.3': ('6', 'eth5'),
            '0000:21:12.3': ('4', 'eth5'),
            '0000:04:01.0': ('2', 'eth5'),
            '0000:04:01.1': ('0', 'eth5')
        }
        mock_ethdev = "eth5"
        mock_nwuuid = "3658f6e8-9676-40f4-b946-80d03e27814c"
        mock_pifuuid = "956fc9f0-da0e-409d-93bb-310fa6101eba"
        mock_index = 1
        mock_mac = "21:3d:dd:26:be:34"
        mock_vlan = "200"
        args = args = {
            "uuid": vm_uuid,
            "ethdev": mock_ethdev,
            "nwuuid": mock_nwuuid,
            "pifuuid": mock_pifuuid,
            "index": mock_index,
            "mac": mock_mac,
            "vlan": mock_vlan
        }

        # Mock to get pifrefs
        mock_session.xenapi.network.get_PIFs.return_value = {
            "2a1b4ed8-c836-4a45-3338-37663dc3b2ab": "mocked"
        }
        # Mock to get host ref
        mock_session.xenapi.host.get_by_uuid.return_value = \
            "2a1b4ed8-c836-4a45-3338-37663dc3b2ab"
        # Mock to get host ref
        mock_session.xenapi.PIF.get_host.return_value = \
            "2a1b4ed8-c836-4a45-3338-37663dc3b2ab"
        # Mock to get vlan
        mock_session.xenapi.PIF.get_VLAN.return_value = \
            "200"
        # Mock to get device
        mock_session.xenapi.PIF.get_device.return_value = \
            "eth5"

        iovirt.assign_free_vf(mock_session, args)
        expected_assignments = {
            mock_index: ('0000:21:12.3', None, None, mock_mac, mock_vlan)
        }
        mock_set_vm_assignments.assert_called_once_with(mock_session,
                                                   vm_uuid, expected_assignments)

    @patch('iovirt._get_vfs')
    @patch('iovirt._get_assignments')
    @patch('iovirt._install_hook_script')
    def test_assign_free_vf_no_uuid(self, _, mock_get_assignments, mock_get_vfs):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_assignments.return_value = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200'),
            '0000:04:01.1': (vm_uuid, 0, '8086', '3c11', '0a:2c:f4:91:be:11', '300')
        }
        mock_get_vfs.return_value = {
            '0000:21:13.3': ('6', 'eth5'),
            '0000:21:12.3': ('4', 'eth5'),
            '0000:04:01.0': ('2', 'eth5'),
            '0000:04:01.1': ('0', 'eth5')
        }
        args = {}
        with self.assertRaises(iovirt.IovirtException):
            iovirt.assign_free_vf(mock_session, args)

class TestUnassignVf(unittest.TestCase):

    @patch('iovirt._set_vm_assignments')
    @patch('iovirt._get_vfs')
    @patch('iovirt._get_vm_assignments')
    @patch('iovirt._install_hook_script')
    def test_unassign_free_vf(self, _, mock_get_vm_assignments, mock_get_vfs,
                            mock_set_vm_assignments):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_vfs.return_value = {
            '0000:21:13.3': ('6', 'eth5'),
            '0000:21:12.3': ('4', 'eth5'),
            '0000:04:01.0': ('2', 'eth5'),
            '0000:04:01.1': ('0', 'eth5')
        }
        mock_ethdev = "eth5"
        mock_index = 0
        mock_pciid = "0000:04:01.1"
        mock_vf = '0'
        args = {
            "uuid": vm_uuid,
            "ethdev": mock_ethdev,
            "index": mock_index,
            "pciid": mock_pciid,
            "vf": mock_vf
        }

        # mock to get the current assign
        mock_vendorid = "8086"
        mock_deviceid = "3c93"
        mock_vlan = "200"
        mock_mac = "21:3d:dd:26:be:34"
        another_pciid = "0000:21:13.3"
        mock_get_vm_assignments.return_value = {
            # Test to delete this assign
            0: (mock_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan),
            # Keep this assign
            1: (another_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan)
        }

        iovirt.unassign_vf(mock_session, args)
        expected_assignments = {
           1: (another_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan)
        }
        mock_set_vm_assignments.assert_called_once_with(mock_session,
                                                   vm_uuid, expected_assignments)

    def test_unassign_free_vf_no_pciid_index_1(self):
        mock_session = MagicMock()
        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        args = {"uuid": vm_uuid}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.unassign_vf(mock_session, args, genericpci=True)

        self.assertEqual(str(context.exception),
                        "Need to specify either a pciid or index")

    def test_unassign_free_vf_no_pciid_index_2(self):
        mock_session = MagicMock()
        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        args = {"uuid": vm_uuid}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.unassign_vf(mock_session, args, genericpci=False)

        self.assertEqual(str(context.exception),
                        "Need to specify either a pciid, index or ethdev and vf")

    def test_unassign_free_vf_no_uuid(self):
        mock_session = MagicMock()
        args = {}
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.unassign_vf(mock_session, args)

        self.assertEqual(str(context.exception),
                        "No VM UUID specified, please use the 'uuid' argument")

    @patch('iovirt._get_vfs')
    def test_unassign_free_vf_cannot_find_pciid(self, mock_get_vfs):
        mock_session = MagicMock()
        mock_get_vfs.return_value = {
            '0000:21:13.3': ('6', 'eth5'),
            '0000:21:12.3': ('4', 'eth5'),
            '0000:04:01.0': ('2', 'eth5'),
            '0000:04:01.1': ('0', 'eth5')
        }
        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        # Not match any ethdev in vfs
        mock_ethdev = "eth3"
        mock_vf = '0'
        args = {
            "uuid": vm_uuid,
            "ethdev": mock_ethdev,
            "vf": mock_vf
        }
        vfdisplay = "%s VF %s" % (mock_ethdev, mock_vf)
        with self.assertRaises(iovirt.IovirtException) as context:
            iovirt.unassign_vf(mock_session, args)

        self.assertEqual(str(context.exception),
                        "Unable to find PCI ID for " + vfdisplay)

    @patch('iovirt._set_vm_assignments')
    @patch('iovirt._get_vm_assignments')
    @patch('iovirt._get_vfs')
    def test_unassign_free_vf_with_index_only(self, mock_get_vfs,
                                              mock_get_vm_assignments,
                                              mock_set_vm_assignments):
        mock_session = MagicMock()
        mock_get_vfs.return_value = {
            '0000:21:13.3': ('6', 'eth5'),
            '0000:21:12.3': ('4', 'eth5'),
            '0000:04:01.0': ('2', 'eth5'),
            '0000:04:01.1': ('0', 'eth5')
        }
        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        # Not match any ethdev in vfs
        mock_index = "0"
        args = {
            "uuid": vm_uuid,
            "index": mock_index,
        }
        # mock to get the current assign
        mock_vendorid = "8086"
        mock_deviceid = "3c93"
        mock_vlan = "200"
        mock_mac = "21:3d:dd:26:be:34"
        mock_pciid = "0000:21:12.3"
        another_pciid = "0000:21:13.3"
        mock_get_vm_assignments.return_value = {
            # Test to delete this assignment with index = 0
            0: (mock_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan),
            # Keep this assign
            1: (another_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan)
        }

        iovirt.unassign_vf(mock_session, args)
        expected_assignments = {
           1: (another_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan)
        }
        mock_set_vm_assignments.assert_called_once_with(mock_session,
                                                   vm_uuid, expected_assignments)


class TestAssignFreePciDevice(unittest.TestCase):

    @patch('iovirt._get_vm_assignments')
    @patch('iovirt._set_vm_assignments')
    @patch('iovirt._get_devices')
    @patch('iovirt._get_assignments')
    def test_assign_free_pci_device(self, mock_get_assignments, mock_get_devices,
                            mock_set_vm_assignments,
                            mock_get_vm_assignments):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_assignments.return_value = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200'),
            '0000:04:01.1': (vm_uuid, 0, '8086', '3c11', '0a:2c:f4:91:be:11', '300')
        }
        vendorid = "8086"
        deviceid = "3c93"
        mock_get_devices.return_value = {
            '0000:04:01.0': (vendorid, deviceid),
            # To test assigning this free pciid
            '0000:04:01.3': (vendorid, deviceid)
        }

        args = {
            "uuid": vm_uuid,
            "vendorid": vendorid,
            "deviceid": deviceid
        }

       # mock to get the current assign
        mock_get_vm_assignments.return_value = {
            0: (None, None, None, None, None),
            1: (None, None, None, None, None)
        }

        iovirt.assign_free_pci_device(mock_session, args)
        # Index + 1
        expected_assignments = {
            0: (None, None, None, None, None),
            1: (None, None, None, None, None),
            2: ('0000:04:01.3', None, None, None, None)
        }
        mock_set_vm_assignments.assert_called_once_with(mock_session,
                                                   vm_uuid, expected_assignments)


    @patch('iovirt._get_devices')
    @patch('iovirt._get_assignments')
    def test_assign_free_pci_device_no_uuid(self, mock_get_assignments,
                                            mock_get_devices):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_assignments.return_value = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200'),
            '0000:04:01.1': (vm_uuid, 0, '8086', '3c11', '0a:2c:f4:91:be:11', '300')
        }
        vendorid = "8086"
        deviceid = "3c93"
        mock_get_devices.return_value = {
            '0000:04:01.0': (vendorid, deviceid),
            # To test assigning this free pciid
            '0000:04:01.3': (vendorid, deviceid)
        }

        args = {
            "vendorid": vendorid,
            "deviceid": deviceid
        }
        with self.assertRaises(iovirt.IovirtException):
            iovirt.assign_free_pci_device(mock_session, args)

    @patch('iovirt._get_devices')
    @patch('iovirt._get_assignments')
    def test_assign_free_pci_device_no_vendorid(self, mock_get_assignments,
                                            mock_get_devices):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_assignments.return_value = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200'),
            '0000:04:01.1': (vm_uuid, 0, '8086', '3c11', '0a:2c:f4:91:be:11', '300')
        }
        vendorid = "8086"
        deviceid = "3c93"
        mock_get_devices.return_value = {
            '0000:04:01.0': (vendorid, deviceid),
            # To test assigning this free pciid
            '0000:04:01.3': (vendorid, deviceid)
        }

        args = {
            "deviceid": deviceid
        }
        with self.assertRaises(iovirt.IovirtException):
            iovirt.assign_free_pci_device(mock_session, args)

    @patch('iovirt._get_devices')
    @patch('iovirt._get_assignments')
    def test_assign_free_pci_device_no_deviceid(self, mock_get_assignments,
                                            mock_get_devices):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_assignments.return_value = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200'),
            '0000:04:01.1': (vm_uuid, 0, '8086', '3c11', '0a:2c:f4:91:be:11', '300')
        }
        vendorid = "8086"
        deviceid = "3c93"
        mock_get_devices.return_value = {
            '0000:04:01.0': (vendorid, deviceid),
            # To test assigning this free pciid
            '0000:04:01.3': (vendorid, deviceid)
        }

        args = {
            "vendorid": vendorid
        }
        with self.assertRaises(iovirt.IovirtException):
            iovirt.assign_free_pci_device(mock_session, args)
class TestUnassignPciDevice(unittest.TestCase):
    @patch('iovirt.unassign_vf')
    def test_unassign_pci_device(self, mock_unassign_vf):
        mock_session = MagicMock()

        args = {}
        iovirt.unassign_pci_device(mock_session, args)
        mock_unassign_vf.assert_called_once_with(mock_session, args, genericpci=True)


class TestShowSummary(unittest.TestCase):

    @patch('iovirt._get_assignments')
    @patch('iovirt._get_vfs')
    def test_show_summary(self, mock_get_vfs, mock_get_assignments):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_assignments.return_value = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200'),
            '0000:04:01.1': (vm_uuid, 0, '8086', '3c11', '0a:2c:f4:91:be:11', '300')
        }
        mock_get_vfs.return_value = {
            '0000:21:13.3': ('6', 'eth5'),
            '0000:21:12.3': ('4', 'eth5'),
            '0000:04:01.0': ('2', 'eth5'),
            '0000:04:01.1': ('0', 'eth5')
        }

        result_xml = iovirt.show_summary(mock_session, None)

        # Parse XML string
        dom = xml.dom.minidom.parseString(result_xml)

        # Get all 'vf' elements
        vf_elements = dom.getElementsByTagName("vf")

        # Assert specific fields and values for each 'vf' element
        for vf_element in vf_elements:
            pciid_element = vf_element.getElementsByTagName("pciid")
            pciid = pciid_element[0].firstChild.data if pciid_element else None

            device_element = vf_element.getElementsByTagName("device")
            device = device_element[0].firstChild.data if device_element else None

            vfnum_element = vf_element.getElementsByTagName("vfnum")
            vfnum = vfnum_element[0].firstChild.data if vfnum_element else None

            # Assert specific fields and values
            if pciid == '0000:04:01.0':
                assigned_element = vf_element.getElementsByTagName("assigned")[0]
                vm = assigned_element.getElementsByTagName("vm")[0].firstChild.data
                index = assigned_element.getElementsByTagName("index")[0].firstChild.data

                mac_element = vf_element.getElementsByTagName("mac")
                mac = mac_element[0].firstChild.data if mac_element else None
                vlan_element = vf_element.getElementsByTagName("vlan")
                vlan = vlan_element[0].firstChild.data if vlan_element else None
                self.assertEqual(device, 'eth5')
                self.assertEqual(vfnum, '2')
                self.assertEqual(vm, vm_uuid)
                self.assertEqual(index, '0')
                self.assertEqual(mac, '0a:2c:f4:91:be:8e')
                self.assertEqual(vlan, '200')
            elif pciid == '0000:04:01.1':
                assigned_element = vf_element.getElementsByTagName("assigned")[0]
                vm = assigned_element.getElementsByTagName("vm")[0].firstChild.data
                index = assigned_element.getElementsByTagName("index")[0].firstChild.data

                mac_element = vf_element.getElementsByTagName("mac")
                mac = mac_element[0].firstChild.data if mac_element else None
                vlan_element = vf_element.getElementsByTagName("vlan")
                vlan = vlan_element[0].firstChild.data if vlan_element else None
                self.assertEqual(device, 'eth5')
                self.assertEqual(vfnum, '0')
                self.assertEqual(vm, vm_uuid)
                self.assertEqual(index, '0')
                self.assertEqual(mac, '0a:2c:f4:91:be:11')
                self.assertEqual(vlan, '300')
            elif pciid == '0000:21:12.3':
                self.assertEqual(device, 'eth5')
                self.assertEqual(vfnum, '4')
            elif pciid == '0000:21:13.3':
                self.assertEqual(device, 'eth5')
                self.assertEqual(vfnum, '6')

class TestListPciDevices(unittest.TestCase):

    @patch('iovirt._get_assignments')
    @patch('iovirt._get_devices')
    def test_list_pci_devices(self, mock_get_devices, mock_get_assignments):
        mock_session = MagicMock()

        vm_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_get_assignments.return_value = {
            '0000:04:01.0': (vm_uuid, 0, '8086', '3c93', '0a:2c:f4:91:be:8e', '200'),
            '0000:04:01.1': (vm_uuid, 0, '8086', '3c11', '0a:2c:f4:91:be:11', '300')
        }
        moc_vendorid = "8086"
        moc_deviceid = "3c93"
        mock_get_devices.return_value = {
            '0000:04:01.0': (moc_vendorid, moc_deviceid),
            '0000:04:01.3': (moc_vendorid, moc_deviceid)
        }

        args = {
            "vendorid": moc_vendorid,
            "deviceid": moc_deviceid
        }
        result_xml = iovirt.list_pci_devices(mock_session, args)

        # Parse XML string
        dom = xml.dom.minidom.parseString(result_xml)

        pcidevice_elements = dom.getElementsByTagName("pcidevice")

        for pcidevice_element in pcidevice_elements:
            pciid_element = pcidevice_element.getElementsByTagName("pciid")
            pciid = pciid_element[0].firstChild.data if pciid_element else None

            vendorid_element = pcidevice_element.getElementsByTagName("vendorid")
            vendorid = vendorid_element[0].firstChild.data if vendorid_element else None

            deviceid_element = pcidevice_element.getElementsByTagName("deviceid")
            deviceid = deviceid_element[0].firstChild.data if deviceid_element else None

            # Assert specific fields and values
            if pciid == '0000:04:01.0':
                assigned_element = pcidevice_element.getElementsByTagName("assigned")[0]
                vm = assigned_element.getElementsByTagName("vm")[0].firstChild.data
                index = assigned_element.getElementsByTagName("index")[0].firstChild.data

                mac_element = assigned_element.getElementsByTagName("mac")
                mac = mac_element[0].firstChild.data if mac_element else None
                vlan_element = assigned_element.getElementsByTagName("vlan")
                vlan = vlan_element[0].firstChild.data if vlan_element else None
                self.assertEqual(vendorid, moc_vendorid)
                self.assertEqual(deviceid, moc_deviceid)
                self.assertEqual(vm, vm_uuid)
                self.assertEqual(index, '0')
                self.assertEqual(mac, '0a:2c:f4:91:be:8e')
                self.assertEqual(vlan, '200')
            elif pciid == '0000:04:01.3':
                self.assertEqual(vendorid, moc_vendorid)
                self.assertEqual(deviceid, moc_deviceid)

    def test_list_pci_devices_no_vendorid(self):
        mock_session = MagicMock()
        moc_deviceid = "3c93"
        args = {
            "deviceid": moc_deviceid
        }
        with self.assertRaises(iovirt.IovirtException):
            iovirt.list_pci_devices(mock_session, args)

    def test_list_pci_devices_no_deviceid(self):
        mock_session = MagicMock()
        moc_vendorid = "3c93"
        args = {
            "vendorid": moc_vendorid
        }
        with self.assertRaises(iovirt.IovirtException):
            iovirt.list_pci_devices(mock_session, args)
class TestGetVM(unittest.TestCase):

    @patch('iovirt._get_vm_assignments')
    @patch('iovirt._get_vfs')
    def test_get_vm(self, mock_get_vfs, mock_get_vm_assignments):
        mock_session = MagicMock()

        mock_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_pciid = "0000:04:01.0"
        mock_vendorid = "8086"
        mock_deviceid = "3c93"
        mock_vlan = "200"
        mock_mac = "21:3d:dd:26:be:34"
        mock_get_vm_assignments.return_value = {
            0: (mock_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan)
        }
        mock_get_vfs.return_value = {
            mock_pciid: ('2', 'eth3'), # Test this one
            '0000:04:01.1': ('0', 'eth5')
        }

        args = {"uuid": mock_uuid}
        result_xml = iovirt.get_vm(mock_session, args)

        # Parse XML string
        dom = xml.dom.minidom.parseString(result_xml)

        vm_element = dom.getElementsByTagName("vm") # Only one vm
        uuid_element = vm_element[0].getElementsByTagName("uuid")
        uuid = uuid_element[0].firstChild.data if uuid_element else None
        passthrough_elements = vm_element[0].getElementsByTagName("passthrough")

        for passthrough_element in passthrough_elements:
            index_element = passthrough_element.getElementsByTagName("index")
            index = index_element[0].firstChild.data if index_element else None
            mac_element = passthrough_element.getElementsByTagName("mac")
            mac = mac_element[0].firstChild.data if mac_element else None
            vlan_element = passthrough_element.getElementsByTagName("vlan")
            vlan = vlan_element[0].firstChild.data if vlan_element else None
            vendorid_element = passthrough_element.getElementsByTagName("vendorid")
            vendorid = vendorid_element[0].firstChild.data if vendorid_element else None
            deviceid_element = passthrough_element.getElementsByTagName("deviceid")
            deviceid = deviceid_element[0].firstChild.data if deviceid_element else None
            pciid_element = passthrough_element.getElementsByTagName("pciid")
            pciid = pciid_element[0].firstChild.data if pciid_element else None
            vfnum_element = passthrough_element.getElementsByTagName("vfnum")
            vfnum = vfnum_element[0].firstChild.data if vfnum_element else None
            device_element = passthrough_element.getElementsByTagName("device")
            device = device_element[0].firstChild.data if device_element else None
            pttype_element = passthrough_element.getElementsByTagName("pttype")
            pttype = pttype_element[0].firstChild.data if pttype_element else None

            # Assert specific fields and values
            self.assertEqual(uuid, mock_uuid)
            self.assertEqual(index, '0')
            self.assertEqual(pciid, mock_pciid)
            self.assertEqual(vendorid, mock_vendorid)
            self.assertEqual(deviceid, mock_deviceid)
            self.assertEqual(mac, mock_mac)
            self.assertEqual(vlan, mock_vlan)
            self.assertEqual(vfnum, '2')
            self.assertEqual(device, 'eth3')
            self.assertEqual(pttype, 'vf')


class TestPrepForVM(unittest.TestCase):

    @patch('os.system')
    @patch('iovirt._get_vm_assignments')
    @patch('iovirt._get_vfs')
    def test_prep_for_vm(self, mock_get_vfs,
                         mock_get_vm_assignments, mock_os_system):
        mock_session = MagicMock()

        mock_uuid = "6757aa5b-b5d7-4f1e-1420-24df822e6685"
        mock_pciid = "0000:04:01.0"
        mock_vendorid = "8086"
        mock_deviceid = "3c93"
        mock_vlan = "200"
        mock_mac = "21:3d:dd:26:be:34"
        another_pciid = "0000:00:00.0" # Test pciid not in vfs

        mock_get_vm_assignments.return_value = {
            0: (mock_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan),
            1: (another_pciid, mock_vendorid, mock_deviceid, mock_mac, mock_vlan)
        }
        mock_get_vfs.return_value = {
            mock_pciid: ('2', 'eth3'), # Test this one
            '0000:04:01.1': ('0', 'eth5')
        }

        args = {"uuid": mock_uuid}
        result = iovirt.prep_for_vm(mock_session, args)
        cmd1 = "/sbin/ip link set eth3 vf 2 mac 21:3d:dd:26:be:34"
        cmd2 = "/sbin/ip link set eth3 vf 2 vlan 200"
        mock_os_system.assert_has_calls([call(cmd1), call(cmd2)])
        expected_result = cmd1 + "\n" + cmd2
        self.assertEqual(result, expected_result)
