#!/usr/bin/env python3
# Populate a directory of symlinks partitioning VMs by SR
# (c) Anil Madhavapeddy, Citrix Systems Inc, 2008

from __future__ import print_function

import argparse
import atexit
import contextlib
import os
import sys
from pathlib import Path

import XenAPI


def logout(session):
    with contextlib.suppress(Exception):
        session.xenapi.session.logout()


def get_input_dir():
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", dest="input_dir", required=True, help="Specify the input directory")
    args = parser.parse_args()
    return args.input_dir


def get_vms_in_sr(session):
    vms = session.xenapi.VM.get_all_records()
    vbds = session.xenapi.VBD.get_all_records()
    vdis = session.xenapi.VDI.get_all_records()
    srs = session.xenapi.SR.get_all_records()

    vms_in_sr = {}

    for vm in vms:
        vmrec = vms[vm]
        # Ignore built-in templates
        if vmrec["other_config"].get("default_template") == "true":
            continue

        # Ignore dom0 and Ignore snapshots
        if vmrec["is_control_domain"] or vmrec["is_a_snapshot"]:
            continue

        # for each VM, figure out the set of SRs it uses
        for vbd in vmrec["VBDs"]:
            if vbd not in vbds:
                continue

            # Ignore VBDs with no VDI such as an empty CD VBD
            vdi = vbds[vbd]["VDI"]
            if vdi == "" or vdi not in vdis:
                continue

            sr = vdis[vdi]["SR"]
            if sr not in srs:
                continue

            sruuid = srs[sr]["uuid"]
            vmuuid = vmrec["uuid"]

            vms_in_sr.setdefault(sruuid, {})[vmuuid] = 1

    return vms_in_sr


def main():
    session = XenAPI.xapi_local()
    session.xenapi.login_with_password("", "", "1.0", "xen-api-scripts-linkvmsbysr.py")
    atexit.register(logout, session)

    input_dir = get_input_dir()
    vms_in_sr = get_vms_in_sr(session)

    for sruuid in list(vms_in_sr.keys()):
        linkdir = "{}/by-sr/{}".format(input_dir, sruuid)
        if Path(linkdir).is_dir():
            print("Directory %s already exists, skipping" % linkdir, file=sys.stderr)
            continue

        try:
            Path(linkdir).mkdir(parents=True)
        except:
            print("Failed to create directory: %s" % linkdir, file=sys.stderr)

        for vmuuid in list(vms_in_sr[sruuid].keys()):
            try:
                src = "../../all/{}.vmmeta".format(vmuuid)
                targ = "{}/{}.vmmeta".format(linkdir, vmuuid)
                os.symlink(src, targ)
            except:
                print("Failed to create symlink: %s -> %s" % (src, targ), file=sys.stderr)

    session.xenapi.logout()


if __name__ == "__main__":
    main()
