"""Unit test for usb_reset.py, covers only setup_cgroup() and the functions it calls"""
import logging
import os
import shutil
import sys
from io import BytesIO


# Module need pytest, python -m unittest will not traverse into this subdir,
# but in case something passes it this directory anyway, send it away:
if hasattr(sys, "_called_from_pytest"):  # See conftest.py
    import pytest
else:
    import unittest

    raise unittest.SkipTest("pytest not installed, skipped")


# For mocking open: builtins vs __builtin__ differs between Python2 and Python3:
BUILTIN = "__builtin__" if sys.version_info < (3,) else "builtins"


def assert_error(captured_logs, error_message):
    """Assert that the captured logs contain exactly the specified error message.

    :param captured_logs (logging.LogCapture): The captured logs.
    :param error_message (str): The expected error message.
    :raises AssertionError: If the captured logs differ from the expected message.
    """
    assert captured_logs.record_tuples == [
        (
            "root",
            logging.ERROR,
            error_message,
        ),
    ]
    captured_logs.clear()


def assert_valid_populated_cgroup_dir(cg_path):
    """Asserts that the cgroup directory is populated with the necessary files.

    The function checks the following:
    - The cgroup directory contains "tasks", "devices.allow", and "devices.deny".
    - The "tasks" file contains the process ID (PID) of the current process.
    - The "devices.allow" file grants read and write access to /dev/null.
    - The "devices.deny" file negates the default "a *:* rwm" rule by writing "a".

    :param expected_cg_path (str): The path to the cgroup directory.
    :raises AssertionError: When any of the assertions fail.
    """
    checks = {
        # Assert read and write access to /dev/null in devices.allow
        "devices.allow": "c 1:3 rw",
        # Assert that default 'a *:* rwm' was negated by writing "a" om devices.deny
        "devices.deny": "a",
        # Assert the tasks file to have the passed pid:
        "tasks": str(os.getpid()),
    }
    for name, content in checks.items():
        with open(cg_path + "/" + name) as i:  # pylint: disable=unspecified-encoding
            assert i.read() == content
    # Assert that no other files were created:
    assert sorted(os.listdir(cg_path)) == sorted(checks.keys())


def test_populate_cgroup_directory(usb_reset, tmp_path, mocker):
    """Test creating and populating the cgroup directory for USB pass-through.

    Verify that the necessary files and permissions are set correctly
    within the cgroup directory.

    :param usb_reset (object): usb_reset module object as pytest fixture
    :param tmp_path (path): The pytest's temporary directory path for testing.
    :param mocker: The mocker object for mocking functions (from pytest-mock)
    """
    # Prepare the test: Mock usb_reset.get_cg_dir to return a subdir of tmp_path:
    tmpdir = tmp_path.as_posix()
    mocker.patch("usb_reset.get_cg_dir", return_value=tmpdir + "/qemu-dom_id")
    tmp_cgroup_dir = tmpdir + "/qemu-dom_id"
    get_ctl = mocker.spy(usb_reset, "get_ctl")

    # Assert that if the qemu-dom_id dir does not exist, it is created with permissions:
    usb_reset.setup_cgroup("dom_id", str(os.getpid()))
    assert os.stat(tmp_cgroup_dir).st_mode == 0o40755
    assert_valid_populated_cgroup_dir(tmp_cgroup_dir)

    # Assert that usb_reset.get_ctl has been called once with ("/dev/null", "rw")
    get_ctl.assert_called_once_with("/dev/null", "rw")
    assert get_ctl.spy_return == "c 1:3 rw"

    # Clean the tmp_cgroup_dir for the 2nd part of this test case
    shutil.rmtree(tmp_cgroup_dir)

    # Repeat, now check that mkdir(cg_path) causing EEXIST is tolerated properly:
    mocker.resetall()
    os.mkdir(tmp_cgroup_dir)
    usb_reset.setup_cgroup("dom_id", str(os.getpid()))
    assert_valid_populated_cgroup_dir(tmp_cgroup_dir)
    # Assert that usb_reset.get_ctl has been called once with ("/dev/null", "rw")
    get_ctl.assert_called_once_with("/dev/null", "rw")
    assert get_ctl.spy_return == "c 1:3 rw"


def test_handle_cgroup_directory_creation_error(usb_reset, caplog, mocker):
    """Assert usb_reset.setup_cgroup() handling OSError from mkdir of cgroup directory.

    :param usb_reset (object): usb_reset module object as pytest fixture
    :param caplog: The pytest caplog fixture capturing log messages during the test
    :param mocker: The mocker object for patching functions.
    """
    # Define the values we use in the test and expect to see in the test's error:
    expected_dom_id = "expected_dom_id"
    expected_cgroup_dir = "/sys/fs/cgroup/devices/qemu-" + expected_dom_id

    # Assert that we receive exit(1), caused by os.mkdir() raising OSError:
    mocker.patch("os.mkdir", side_effect=OSError)
    with pytest.raises(SystemExit) as exc_info:
        usb_reset.setup_cgroup(expected_dom_id, "pid is not accessed in this test")
    assert exc_info.value.args == (1,)  # assert that exit(1) caused the SystemExit

    # Assert an error with the required cgroup path, expected by mkdir raising OSError:
    assert_error(caplog, "Failed to create cgroup: " + expected_cgroup_dir)


def test_handle_file_open_errors(usb_reset, tmp_path, mocker, caplog):
    """Assert handling of OSError while attempting to open error

    :param usb_reset (object): usb_reset module object as pytest fixture
    :param tmp_path: The temporary path used for mocking the cgroup directory.
    :param mocker: The mocker object used for patching functions.
    :param caplog: The pytest fixture object used for capturing log messages.
    """
    # Error message we raise and expect to see from mocked open() raising IOError:
    open_error = "Error message raised using IOError when opening a cgroup file"
    # For mocking open: builtins vs __builtin__ differs between Python2 and Python3:
    builtin_open = "__builtin__.open" if sys.version_info < (3,) else "builtins.open"

    # Mock get_cg_dir() to return our temp_path, otherwise we fail before opening files:
    mocker.patch("usb_reset.get_cg_dir", return_value=tmp_path.as_posix())

    # Assert that if open() raises IOError, setup_cgroup() raises SystemExit(1) in turn
    for open_side_effects in [  # Parameterize three calls to open() and test each one
        (IOError(open_error), BytesIO(), BytesIO()),
        (BytesIO(), IOError(open_error), BytesIO()),
        (BytesIO(), BytesIO(), IOError(open_error)),
    ]:
        # Setup open to return IOError for one of the calls and BytesIO() for the others
        mocker.patch(builtin_open, side_effect=open_side_effects)

        with pytest.raises(SystemExit) as exc_info:
            usb_reset.setup_cgroup("some valid dom_id", "pid not used in this test")

        assert exc_info.value.args == (1,)  # assert that exit(1) caused the SystemExit

        # Assert that the expected error with the error message that we raised was logged
        assert_error(caplog, "Failed to setup cgroup: " + open_error)


def test_write_order(usb_reset, tmp_path, mocker, capfd):
    """Test that when writing unbuffered, the writes are

    :param usb_reset (object): usb_reset module object as pytest fixture
    :param tmp_path: The temporary path used for mocking the cgroup directory.
    :param mocker: The mocker object used for patching functions.
    :param capfd: The pytest fixture capturing stdout messages on the file descriptor
    """

    # Mock get_cg_dir() to return our temp_path, otherwise we fail before opening files:
    mocker.patch("usb_reset.get_cg_dir", return_value=tmp_path.as_posix())

    open_side_effects = (
        os.fdopen(os.dup(sys.stdout.fileno()), "wb", 0),
        os.fdopen(os.dup(sys.stdout.fileno()), "wb", 0),
        os.fdopen(os.dup(sys.stdout.fileno()), "wb", 0),
    )
    mocker.patch(BUILTIN + ".open", side_effect=open_side_effects)
    usb_reset.setup_cgroup("dom_id is not significant in this this test case", "<pid>")
    assert capfd.readouterr().out == "ac 1:3 rw<pid>"


def test_open_unbuffered(usb_reset, tmp_path, mocker):
    """Test that when writing unbuffered, the writes are

    :param usb_reset (object): usb_reset module object as pytest fixture
    :param tmp_path: The temporary path used for mocking the cgroup directory.
    :param mocker: The mocker object used for patching functions.
    """
    # Mock get_cg_dir() to return our temp_path, otherwise we fail before opening files:
    tmp = tmp_path.as_posix()
    mocker.patch("usb_reset.get_cg_dir", return_value=tmp_path.as_posix())
    mock_open_func = mocker.mock_open()
    mocker.patch(BUILTIN + ".open", mock_open_func)
    usb_reset.setup_cgroup("dom_id is not significant in this this test case", "<pid>")

    # Note: The order should not be important, so in case you need to change the
    # implementation, change the testcase too. The only important detail is that
    # cgroupfs writes should done on unbuffered files, which these checks ensure.

    # Assert that write and open were called with the expected args in expected order
    assert mock_open_func().write.call_args_list == [
        mocker.call(b"a"),
        mocker.call(b"c 1:3 rw"),
        mocker.call(b"<pid>"),
    ]
    mock_open_func.assert_has_calls(
        [
            mocker.call(tmp + "/tasks", "wb", 0),
            mocker.call().__enter__(),  # pylint: disable=unnecessary-dunder-call
            mocker.call(tmp + "/devices.deny", "wb", 0),
            mocker.call().__enter__(),  # pylint: disable=unnecessary-dunder-call
            mocker.call(tmp + "/devices.allow", "wb", 0),
            mocker.call().__enter__(),  # pylint: disable=unnecessary-dunder-call
            mocker.call().write(b"a"),
            mocker.call().write(b"c 1:3 rw"),
            mocker.call().write(b"<pid>"),
            mocker.call().__exit__(None, None, None),
            mocker.call().__exit__(None, None, None),
            mocker.call().__exit__(None, None, None),
            mocker.call(),
        ]
    )
