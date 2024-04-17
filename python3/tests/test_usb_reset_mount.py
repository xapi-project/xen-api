"""scripts/unit_test/test_usb_reset_mount.py: Test usb_reset.mount and .umount"""
from __future__ import print_function

from .import_helper import import_file_as_module, mocked_modules


def test_usb_reset_mount_umount(private_mount_namespace):
    """Test usb_reset.mount and .umount"""
    assert private_mount_namespace
    with mocked_modules("xcp", "xcp.logger"):
        usb_reset = import_file_as_module("python3/libexec/usb_reset.py")
        usb_reset.log.error = print
        usb_reset.mount(source="tmpfs", target="/tmp", fs="tmpfs")
        usb_reset.umount("/tmp")
