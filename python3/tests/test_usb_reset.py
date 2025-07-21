import unittest
from unittest import mock
from unittest.mock import MagicMock
import sys

# some mocked arguemtn is not used in the tests, but as side-effects
# disabled pylint warning for unused arguments
# pylint: disable=unused-argument

from python3.tests.import_helper import import_file_as_module
# mock modules to avoid dependencies
sys.modules["xcp"] = MagicMock()
sys.modules["xcp.logger"] = MagicMock()
usb_reset = import_file_as_module("python3/libexec/usb_reset.py")


class TestUsbReset(unittest.TestCase):
    @mock.patch("usb_reset.open", new_callable=mock.mock_open, read_data="5\n")
    def test_read_int(self, mock_open):
        self.assertEqual(usb_reset.read_int("/fake/path"), 5)
        mock_open.assert_called_with("/fake/path")

    @mock.patch("usb_reset.read_int", side_effect=[1, 2])
    @mock.patch("usb_reset.log")
    def test_dev_path_valid(self, mock_log, mock_read_int):
        device = "1-2.3"
        path = usb_reset.dev_path(device)
        self.assertEqual(path, "/dev/bus/usb/001/002")
        mock_log.error.assert_not_called()

    @mock.patch("usb_reset.log")
    def test_dev_path_invalid(self, mock_log):
        with self.assertRaises(SystemExit):
            usb_reset.dev_path("invalid-device")
        mock_log.error.assert_called()

    @mock.patch("usb_reset.ctypes.CDLL")
    @mock.patch("usb_reset.ctypes.util.find_library", return_value="libc.so.6")
    @mock.patch("usb_reset.log")
    def test_mount_success(self, mock_log, mock_find_lib, mock_cdll):
        mock_cdll.return_value.mount.return_value = 0
        usb_reset.mount("src", "tgt", "fs")
        mock_cdll.return_value.mount.assert_called()

    @mock.patch("usb_reset.ctypes.CDLL")
    @mock.patch("usb_reset.ctypes.util.find_library", return_value="libc.so.6")
    @mock.patch("usb_reset.log")
    def test_mount_fail(self, mock_log, mock_find_lib, mock_cdll):
        mock_cdll.return_value.mount.return_value = -1
        with self.assertRaises(SystemExit):
            usb_reset.mount("src", "tgt", "fs")
        mock_log.error.assert_called()

    @mock.patch("usb_reset.ctypes.CDLL")
    @mock.patch("usb_reset.ctypes.util.find_library", return_value="libc.so.6")
    @mock.patch("usb_reset.log")
    def test_umount(self, mock_log, mock_find_lib, mock_cdll):
        mock_cdll.return_value.umount.return_value = -1
        usb_reset.umount("tgt")
        mock_log.error.assert_called()

    @mock.patch("usb_reset.os")
    @mock.patch("usb_reset.pwd.getpwnam")
    @mock.patch("usb_reset.grp.getgrnam")
    @mock.patch("usb_reset.log")
    def test_clone_device(self, mock_log, mock_grp, mock_pwd, mock_os):
        mock_os.path.exists.return_value = False
        mock_os.path.sep = "/"
        mock_os.stat.return_value.st_mode = 0o600
        mock_os.stat.return_value.st_rdev = 0
        mock_os.major.return_value = 1
        mock_os.minor.return_value = 2
        mock_os.makedev.return_value = 1234
        mock_pwd.return_value.pw_uid = 1000
        mock_grp.return_value.gr_gid = 1000
        usb_reset.clone_device("/dev/bus/usb/001/002", "/root", 1)
        mock_os.mknod.assert_called()
        mock_os.chown.assert_called()

    @mock.patch("usb_reset.dev_path", return_value="/dev/bus/usb/001/002")
    @mock.patch("usb_reset.open", new_callable=mock.mock_open)
    @mock.patch("usb_reset.fcntl.ioctl")
    @mock.patch("usb_reset.log")
    def test_attach_reset_only(self, mock_log, mock_ioctl, mock_open, mock_dev_path):
        usb_reset.attach("1-2", 1, 123, True)
        mock_open.assert_called()
        mock_ioctl.assert_called()

    @mock.patch("usb_reset.dev_path", return_value="/dev/bus/usb/001/002")
    @mock.patch("usb_reset.os.remove")
    @mock.patch("usb_reset.get_root_dir", return_value="/root")
    def test_detach(self, mock_get_root_dir, mock_remove, mock_dev_path):
        usb_reset.detach("1-2", 1)
        mock_remove.assert_called()

    @mock.patch("usb_reset.shutil.rmtree")
    @mock.patch("usb_reset.os.path.isdir", return_value=True)
    @mock.patch("usb_reset.os.path.exists", return_value=True)
    @mock.patch("usb_reset.os.path.ismount", return_value=True)
    @mock.patch("usb_reset.umount")
    @mock.patch("usb_reset.log")
    #pylint: disable=too-many-arguments
    def test_cleanup(self, mock_log, mock_umount, mock_ismount,
                     mock_exists, mock_isdir, mock_rmtree):
        usb_reset.cleanup(1)
        mock_rmtree.assert_called()

if __name__ == "__main__":
    unittest.main()
