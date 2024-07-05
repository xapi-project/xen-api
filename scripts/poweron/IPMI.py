#!/usr/bin/env python3

import sys
from xcp import cmd

class IPMI_POWERON_FAILED(Exception):
    """IPMI Poweron exception"""
    pass

ipmi_path = "/usr/bin/ipmitool"

def IPMI(power_on_ip, user, password):
    (rc, stdout, stderr) = cmd.runCmd(
        [
            ipmi_path,
            "-H",
            power_on_ip,
            "-I", "lanplus",
            "-U",
            user,
            "-P",
            password,
            "chassis", "power", "on"
        ],
        with_stdout=True,
        with_stderr=True,
    )
    if rc != 0:
        raise IPMI_POWERON_FAILED(stderr)
    return stdout


def main():
    if len(sys.argv) < 3:
        sys.exit(1)
    ip = sys.argv[1]
    user = sys.argv[2]
    password = sys.argv[3]
    print(IPMI(ip, user, password))


if __name__ == "__main__":
    main()
