#!/usr/bin/env python3

import os.path
import sys

import xcp.cmd as cmd


class DRAC_NO_SUPP_PACK(Exception):
    """Base Exception class for all transfer plugin errors."""

    def __init__(self, *args):
        Exception.__init__(self, *args)


class DRAC_POWERON_FAILED(Exception):
    """Base Exception class for all transfer plugin errors."""

    def __init__(self, *args):
        Exception.__init__(self, *args)


drac_path = "/opt/dell/srvadmin/sbin/racadm"


def DRAC(power_on_ip, user, password):
    if not os.path.exists(drac_path):
        raise DRAC_NO_SUPP_PACK()

    (rc, stdout, stderr) = cmd.runCmd(
        [
            drac_path,
            "-r",
            power_on_ip,
            "-u",
            user,
            "-p",
            password,
            "serveraction",
            "powerup",
        ],
        with_stdout=True,
        with_stderr=True,
    )
    if rc != 0:
        raise DRAC_POWERON_FAILED(stderr)
    return stdout


def main():
    if len(sys.argv) < 3:
        exit(0)
    ip = sys.argv[1]
    user = sys.argv[2]
    password = sys.argv[3]
    print(DRAC(ip, user, password))


if __name__ == "__main__":
    main()
