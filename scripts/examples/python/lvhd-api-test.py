#!/usr/bin/env python

from __future__ import print_function
import XenAPI, sys

def go(x, name):
    vm = x.xenapi.VM.get_by_name_label(name)[0]
    vbds = x.xenapi.VM.get_VBDs(vm)
    non_empty = filter(lambda y:not(x.xenapi.VBD.get_empty(y)), vbds)
    vdis = map(lambda y:x.xenapi.VBD.get_VDI(y), non_empty)
    
    print("Calling API call on %s" % (repr(vdis)))
    result = x.xenapi.SR.lvhd_stop_using_these_vdis_and_call_script(vdis, "echo", "main", { "hello": "there", "sleep": "10" })
    print(repr(result))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage:", file=sys.stderr)
        print(" %s <VM name-label>" % (sys.argv[0]), file=sys.stderr)
        print("     -- Call SR.lvhd_stop_using_these_vdis_and_call_script with all VDIs with VBDs (attached or not) linking to specified VM", file=sys.stderr)
        sys.exit(1)
    name = sys.argv[1]
    x = XenAPI.xapi_local()
    x.xenapi.login_with_password("root", "", "1.0", "xen-api-scripts-lvhd-api-test.py")
    try:
        go(x, name)
    finally:
        x.xenapi.logout()
