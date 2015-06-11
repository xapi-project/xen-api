#!/usr/bin/python
# Populate a directory of symlinks partitioning VMs by SR
# (c) Anil Madhavapeddy, Citrix Systems Inc, 2008

import atexit
import XenAPI
import os, sys
import getopt

def logout():
    try:
        session.xenapi.session.logout()
    except:
        pass
atexit.register(logout)

def usage():
    print >> sys.stderr, "%s [-d <directory>]" % sys.argv[0]
    sys.exit(1)
   
def main(argv):
    session = XenAPI.xapi_local()
    session.xenapi.login_with_password("", "", "1.0", "xen-api-scripts-linkvmsbysr.py")

    try:
        opts, args = getopt.getopt(sys.argv[1:], "hd:", [])
    except getopt.GetoptError, err:
        print str(err)
        usage()

    dir = None
    for o,a in opts:
        if o == "-d":
            dir = a

    if dir == None:
        usage()
 
    vms = session.xenapi.VM.get_all_records()
    vbds = session.xenapi.VBD.get_all_records()
    vdis = session.xenapi.VDI.get_all_records()
    srs = session.xenapi.SR.get_all_records()

    vms_in_sr = {}

    for vm in vms:
        vmrec = vms[vm]
        # Ignore built-in templates
        if vmrec['other_config'].has_key('default_template'):
            if vmrec['other_config']['default_template'] == 'true':
                continue
        # Ignore dom0
        if vmrec['is_control_domain']:
            continue
        # Ignore snapshots
        if vmrec['is_a_snapshot']:
            continue

        # for each VM, figure out the set of SRs it uses
        for vbd in vmrec['VBDs']:
            if not vbds.has_key(vbd):
                continue
            vdi = vbds[vbd]['VDI']

            # Ignore VBDs with no VDI such as an empty CD VBD
            if vdi == '':
                continue

            if not vdis.has_key(vdi):
                continue

            sr = vdis[vdi]['SR']
            if not srs.has_key(sr):
                continue

            sruuid = srs[sr]['uuid']
            vmuuid = vmrec['uuid']

            if not vms_in_sr.has_key(sruuid):
                vms_in_sr[sruuid] = {}
            vms_in_sr[sruuid][vmuuid] = 1
    
    for sruuid in vms_in_sr.keys():
        linkdir = "%s/by-sr/%s" % (dir, sruuid)
        if os.path.isdir(linkdir):
            print >> sys.stderr, "Directory %s already exists, skipping" % linkdir
            continue

        try:
            os.makedirs(linkdir)
        except:
            print >> sys.stderr, "Failed to create directory: %s" % linkdir
        for vmuuid in vms_in_sr[sruuid].keys():
            try:
                src = "../../all/%s.vmmeta" % vmuuid
                targ = "%s/%s.vmmeta" % (linkdir, vmuuid)
                os.symlink(src, targ)
            except:
                print >> sys.stderr, "Failed to create symlink: %s -> %s" % (src, targ)

    session.xenapi.logout()

if __name__ == "__main__":
    main(sys.argv[1:])


