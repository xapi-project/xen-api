#!/usr/bin/env python3
"""
This module provides unittest for nbd_client_manager.py
"""

import unittest
import subprocess
from mock import MagicMock, patch, mock_open, call
from python3.tests.import_helper import import_file_as_module

nbd_client_manager = import_file_as_module("python3/libexec/nbd_client_manager.py")

@patch('subprocess.Popen')
class TestCallFunction(unittest.TestCase):

    def test_call_success(self, mock_popen):
        mock_process = mock_popen.return_value
        mock_process.communicate.return_value = ("ls -l output", "")
        mock_process.returncode = 0

        returncode = nbd_client_manager._call(["ls", "-l"])

        self.assertEqual(returncode, 0)

    def test_call_failure(self, mock_popen):
        mock_process = mock_popen.return_value
        mock_process.communicate.return_value = ("", "err")
        mock_process.returncode = 1

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            nbd_client_manager._call(["invalid_cmd"])

        self.assertEqual(cm.exception.returncode, 1)

@patch('nbd_client_manager.os.path.exists')
class TestIsNbdDeviceConnected(unittest.TestCase):

    @patch('nbd_client_manager._call')
    def test_nbd_device_connected(self, mock_call, mock_exists):
        mock_exists.return_value = True
        mock_call.return_value = 0

        result = nbd_client_manager._is_nbd_device_connected('/dev/nbd0')

        self.assertTrue(result)
        mock_call.assert_called_once_with(["nbd-client", "-check", "/dev/nbd0"],
                                          raise_err=False, log_err=False)

    @patch('nbd_client_manager._call')
    def test_nbd_device_not_connected(self, mock_call, mock_exists):
        mock_exists.return_value = True
        mock_call.return_value = 1

        result = nbd_client_manager._is_nbd_device_connected('/dev/nbd1')

        self.assertFalse(result)
        mock_call.assert_called_once_with(["nbd-client", "-check", "/dev/nbd1"],
                                          raise_err=False, log_err=False)

    def test_nbd_device_not_found(self, mock_exists):
        mock_exists.return_value = False

        # Testing the function with a non-existent device
        with self.assertRaises(nbd_client_manager.NbdDeviceNotFound):
            nbd_client_manager._is_nbd_device_connected('/dev/nbd2')

@patch('nbd_client_manager._is_nbd_device_connected')
class TestFindUnusedNbdDevice(unittest.TestCase):
    def test_find_unused_nbd_device(self, mock_is_nbd_device_connected):
        # Mocking the function to return True for /dev/nbd0 and False for /dev/nbd1
        mock_is_nbd_device_connected.side_effect = [True, False]

        # Testing the function
        unused_device = nbd_client_manager._find_unused_nbd_device()

        # Assertion
        self.assertEqual(unused_device, "/dev/nbd1")

    def test_no_unused_nbd_device(self, mock_is_nbd_device_connected):
        # Mocking the function to always raise NbdDeviceNotFound
        mock_is_nbd_device_connected.side_effect = nbd_client_manager.NbdDeviceNotFound('/dev/nbd1')

        # Testing the function when no unused devices are found
        with self.assertRaises(nbd_client_manager.NbdDeviceNotFound):
            nbd_client_manager._find_unused_nbd_device()

@patch('nbd_client_manager._is_nbd_device_connected')
class TestWaitForNbdDevice(unittest.TestCase):
    def test_wait_for_nbd_device_connected(self, mock_is_nbd_device_connected):
        mock_is_nbd_device_connected.return_value = True
        nbd_client_manager._wait_for_nbd_device('/dev/nbd0', connected=True)
        mock_is_nbd_device_connected.assert_called_once_with(nbd_device='/dev/nbd0')

    def test_wait_for_nbd_device_disconnected(self, mock_is_nbd_device_connected):
        mock_is_nbd_device_connected.return_value = False
        nbd_client_manager._wait_for_nbd_device('/dev/nbd1', connected=False)
        mock_is_nbd_device_connected.assert_called_once_with(nbd_device='/dev/nbd1')

class TestGetPersistentConnectInfoFilename(unittest.TestCase):
    def test_get_persistent_connect_info_filename(self):
        # Test for device /dev/nbd0
        device = "/dev/nbd0"
        expected_filename = "/var/run/nonpersistent/nbd/0"
        self.assertEqual(nbd_client_manager._get_persistent_connect_info_filename(device),
                         expected_filename)

@patch('nbd_client_manager.os.makedirs')
@patch('nbd_client_manager.os.path.exists')
class TestPersistConnectInfo(unittest.TestCase):

    def test_persist_connect_info(self, mock_exists, mock_makedirs):
        mock_exists.return_value = False

        # Test data
        device = "/dev/nbd0"
        path = "/some/path"
        exportname = "example_export"

        # Setting up mock for file write
        mock_file = mock_open()
        with patch('builtins.open', mock_file):
            # Run the function
            nbd_client_manager._persist_connect_info(device, path, exportname)

        # Assertions
        mock_makedirs.assert_called_once_with(nbd_client_manager.PERSISTENT_INFO_DIR)
        mock_file.assert_called_once_with('/var/run/nonpersistent/nbd/0', 'w', encoding='utf-8')
        mock_file().write.assert_called_once_with(
            '{"path": "/some/path", "exportname": "example_export"}'
            )

    def test_persist_connect_info_directory_exists(self, mock_exists, mock_makedirs):
        mock_exists.return_value = True

        # Test data
        device = "/dev/nbd0"
        path = "/some/path"
        exportname = "example_export"

        # Setting up mock for file write
        mock_file = mock_open()
        with patch('builtins.open', mock_file):
            # Run the function
            nbd_client_manager._persist_connect_info(device, path, exportname)

        # Assertions
        mock_makedirs.assert_not_called()
        mock_file.assert_called_once_with('/var/run/nonpersistent/nbd/0', 'w', encoding='utf-8')
        mock_file().write.assert_called_once_with(
            '{"path": "/some/path", "exportname": "example_export"}'
            )

class TestRemovePersistentConnectInfo(unittest.TestCase):
    @patch('nbd_client_manager.os.remove')
    def test_remove_persistent_connect_info(self, mock_os_remove):
        nbd_client_manager._remove_persistent_connect_info('/dev/nbd0')
        mock_os_remove.assert_called_once_with('/var/run/nonpersistent/nbd/0')

class TestConnectNbd(unittest.TestCase):
    @patch('nbd_client_manager._call')
    @patch('nbd_client_manager._find_unused_nbd_device')
    @patch('nbd_client_manager._wait_for_nbd_device')
    @patch('nbd_client_manager._persist_connect_info')
    @patch('nbd_client_manager.open')
    @patch('nbd_client_manager.FILE_LOCK', MagicMock())  # Mocking FILE_LOCK
    # pylint: disable=too-many-arguments
    def test_connect_nbd(self, mock_openfile, mock_persist_info,
                         mock_wait_for_nbd, mock_find_unused, mock_call):
        # Mocking necessary functions and file operations
        mock_find_unused.return_value = "/dev/nbd0"
        mock_call.return_value = 0
        mock_file_scheduler = MagicMock()
        mock_file_max_sectors_kb = MagicMock()
        mock_file_nr_requests = MagicMock()
        mock_openfile.side_effect = [mock_file_scheduler,
                                 mock_file_max_sectors_kb,
                                 mock_file_nr_requests]

        # Testing the function
        result = nbd_client_manager.connect_nbd("/path/of/socket/file", "export_name")

        # Assertions
        self.assertEqual(result, "/dev/nbd0")
        mock_find_unused.assert_called_once()
        mock_call.assert_called()
        mock_wait_for_nbd.assert_called_once_with(nbd_device="/dev/nbd0", connected=True)
        mock_persist_info.assert_called_once_with(
            "/dev/nbd0", "/path/of/socket/file", "export_name"
            )
        # Checking open calls
        mock_openfile.assert_has_calls([
            call("/sys/block/nbd0/queue/scheduler", "w", encoding="utf-8"),
            call("/sys/block/nbd0/queue/max_sectors_kb", "w", encoding="utf-8"),
            call("/sys/block/nbd0/queue/nr_requests", "w", encoding="utf-8")
        ], any_order=True)

@patch('nbd_client_manager._is_nbd_device_connected')
@patch('nbd_client_manager._remove_persistent_connect_info')
@patch('nbd_client_manager._call')
@patch('nbd_client_manager._wait_for_nbd_device')
class TestDisconnectNbdDevice(unittest.TestCase):

    def test_disconnect_nbd_device_connected(self, mock_wait_for_nbd,
                                             mock_call, mock_remove_persistent, mock_is_connected):
        # Mocking _is_nbd_device_connected to return True
        mock_is_connected.return_value = True

        # Testing the function when device is connected
        nbd_client_manager.disconnect_nbd_device("/dev/nbd0")

        # Assertions
        mock_is_connected.assert_called_once_with(nbd_device="/dev/nbd0")
        mock_remove_persistent.assert_called_once_with("/dev/nbd0")
        mock_call.assert_called_once_with(["nbd-client", "-disconnect", "/dev/nbd0"])
        mock_wait_for_nbd.assert_called_once_with(nbd_device="/dev/nbd0", connected=False)

    def test_disconnect_nbd_device_disconnected(self, mock_wait_for_nbd, mock_call,
                                                mock_remove_persistent, mock_is_connected):
        # Mocking _is_nbd_device_connected to return False
        mock_is_connected.return_value = False

        # Testing the function when device is already disconnected
        nbd_client_manager.disconnect_nbd_device("/dev/nbd0")

        # Assertions
        mock_is_connected.assert_called_once_with(nbd_device="/dev/nbd0")
        mock_remove_persistent.assert_not_called()
        mock_call.assert_not_called()
        mock_wait_for_nbd.assert_not_called()

    def test_disconnect_nbd_device_not_found(self, mock_wait_for_nbd, mock_call,
                                             mock_remove_persistent, mock_is_connected):
        # Mocking _is_nbd_device_connected to raise NbdDeviceNotFound
        mock_is_connected.side_effect = nbd_client_manager.NbdDeviceNotFound('/dev/nbd0')

        # Testing the function when device is not found
        nbd_client_manager.disconnect_nbd_device("/dev/nbd0")

        # Assertions
        mock_is_connected.assert_called_once_with(nbd_device="/dev/nbd0")
        mock_remove_persistent.assert_not_called()
        mock_call.assert_not_called()
        mock_wait_for_nbd.assert_not_called()
