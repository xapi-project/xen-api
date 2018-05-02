#!/usr/bin/python

"""
Provides functions and a CLI for safely connecting to and disconnecting from
NBD devices.
"""

import argparse
import logging
import logging.handlers
import os
import subprocess
import time
import fcntl
import json
import re


LOGGER = logging.getLogger("nbd_client_manager")
LOGGER.setLevel(logging.DEBUG)

LOCK_FILE = '/var/run/nonpersistent/nbd_client_manager'


class NbdDeviceNotFound(Exception):
    """
    The NBD device file does not exist. Raised when there are no free NBD
    devices.
    """
    def __init__(self, nbd_device):
        super(NbdDeviceNotFound, self).__init__(
            "NBD device '{}' does not exist".format(nbd_device))
        self.nbd_device = nbd_device


class FileLock(object):
    """Container for data relating to a file lock"""
    def __init__(self, path):
        self._path = path
        self._lock_file = None

    def _lock(self):
        """Acquire the lock"""
        flags = fcntl.LOCK_EX
        self._lock_file = open(self._path, 'w+')
        fcntl.flock(self._lock_file, flags)

    def _unlock(self):
        """Unlock and remove the lock file"""
        if self._lock_file:
            fcntl.flock(self._lock_file, fcntl.LOCK_UN)
            self._lock_file.close()
            self._lock_file = None

    def __enter__(self):
        self._lock()

    def __exit__(self, *args):
        self._unlock()


FILE_LOCK = FileLock(path=LOCK_FILE)


def _call(cmd_args, error=True):
    """
    [call cmd_args] executes [cmd_args] and returns the exit code.
    If [error] and exit code != 0, log and throws a CalledProcessError.
    """
    LOGGER.debug("Running cmd %s", cmd_args)
    p = subprocess.Popen(
        cmd_args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        close_fds=True
    )

    stdout, stderr = p.communicate()

    if error and p.returncode != 0:
        LOGGER.error(
            "%s exitted with code %d: %s",
            ' '.join(cmd_args),
            p.returncode,
            stderr)

        raise subprocess.CalledProcessError(
            returncode=p.returncode,
            cmd=cmd_args,
            output=stderr)

    return p.returncode


def _is_nbd_device_connected(nbd_device):
    """
    Checks whether the specified nbd device is connected according to
    nbd-client.
    """
    # First check if the file exists, because "nbd-client -c" returns
    # 1 for a non-existent file.
    if not os.path.exists(nbd_device):
        raise NbdDeviceNotFound(nbd_device)
    cmd = ['nbd-client', '-check', nbd_device]
    returncode = _call(cmd, error=False)
    if returncode == 0:
        return True
    if returncode == 1:
        return False
    raise subprocess.CalledProcessError(returncode=returncode, cmd=cmd)


def _find_unused_nbd_device():
    """
    Returns the path of the first /dev/nbdX device that is not
    connected according to nbd-client.
    """
    for device_no in range(0, 100):
        nbd_device = "/dev/nbd{}".format(device_no)
        if not _is_nbd_device_connected(nbd_device=nbd_device):
            return nbd_device


def _wait_for_nbd_device(nbd_device, connected):
    while _is_nbd_device_connected(nbd_device=nbd_device) != connected:
        LOGGER.debug(
            'Connection status of NBD device %s not yet %s, waiting',
            nbd_device,
            connected)
        time.sleep(0.1)

PERSISTENT_INFO_DIR = "/var/run/nonpersistent/nbd"

def _get_persistent_connect_info_filename(device):
    """
    Return the full path for the persistent file containing
    the connection details. This is based on the device
    name, so /dev/nbd0 -> /var/run/nonpersistent/nbd/0
    """
    number = re.search('/dev/nbd([0-9]+)', device).group(1)
    return PERSISTENT_INFO_DIR + '/' + number

def _persist_connect_info(device, path, exportname):
    if not os.path.exists(PERSISTENT_INFO_DIR):
        os.makedirs(PERSISTENT_INFO_DIR)
    filename = _get_persistent_connect_info_filename(device)
    with open(filename, 'w') as f:
        f.write(json.dumps({'path':path, 'exportname':exportname}))

def _remove_persistent_connect_info(device):
    try:
        os.remove(_get_persistent_connect_info_filename(device))
    except OSError:
        pass

def connect_nbd(path, exportname):
    """Connects to a free NBD device using nbd-client and returns its path"""
    _call(['modprobe', 'nbd'])
    with FILE_LOCK:
        nbd_device = _find_unused_nbd_device()
        cmd = ['nbd-client', '-unix', path, nbd_device, '-name', exportname]
        _call(cmd)
        _wait_for_nbd_device(nbd_device=nbd_device, connected=True)
        _persist_connect_info(nbd_device, path, exportname)
    return nbd_device


def disconnect_nbd_device(nbd_device):
    """
    Disconnects the given device using nbd-client.
    This function is idempotent: calling it on an already disconnected device
    does nothing.
    """
    with FILE_LOCK:
        if _is_nbd_device_connected(nbd_device=nbd_device):
            _remove_persistent_connect_info(nbd_device)
            cmd = ['nbd-client', '-disconnect', nbd_device]
            _call(cmd)
            _wait_for_nbd_device(nbd_device=nbd_device, connected=False)


def _connect_cli(args):
    device = connect_nbd(path=args.path, exportname=args.exportname)
    print device


def _disconnect_cli(args):
    disconnect_nbd_device(nbd_device=args.device)


def _main():
    # Configure the root logger to log into syslog
    # (Specifically, into /var/log/user.log)
    syslog_handler = logging.handlers.SysLogHandler(
        address='/dev/log',
        facility=logging.handlers.SysLogHandler.LOG_USER)
    # Ensure the program name is included in the log messages:
    formatter = logging.Formatter('%(name)s: [%(levelname)s] %(message)s')
    syslog_handler.setFormatter(formatter)
    logging.getLogger().addHandler(syslog_handler)

    try:
        parser = argparse.ArgumentParser(
            description="Connect to and disconnect from an NBD device")

        subparsers = parser.add_subparsers(dest='command_name')

        parser_connect = subparsers.add_parser(
            'connect',
            help='Connect to a free NBD device and return its path')
        parser_connect.add_argument(
            '--path',
            required=True,
            help="The path of the Unix domain socket of the NBD server")
        parser_connect.add_argument(
            '--exportname',
            required=True,
            help="The export name of the device to connect to")
        parser_connect.set_defaults(func=_connect_cli)

        parser_disconnect = subparsers.add_parser(
            'disconnect',
            help='Disconnect from the given NBD device')
        parser_disconnect.add_argument(
            '--device',
            required=True,
            help="The path of the NBD device to disconnect")
        parser_disconnect.set_defaults(func=_disconnect_cli)

        args = parser.parse_args()
        args.func(args)
    except Exception as e:
        LOGGER.exception(e)
        raise


if __name__ == '__main__':
    _main()
