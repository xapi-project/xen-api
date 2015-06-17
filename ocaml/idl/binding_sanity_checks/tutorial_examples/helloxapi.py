#!/usr/bin/env python

import sys
import XenAPI
from pprint import pprint

def main(sx):
    vms = sx.VM.get_all()
    real_vms = [ x for x in vms if not sx.VM.get_is_a_template(x)]
    pprint ( [sx.VM.get_name_label(x) for x in real_vms] )

if __name__ == "__main__":
    if len(sys.argv) <> 4:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)
    url = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]

    print "List of non-template VMs on %s" % url
    session = XenAPI.Session(url)
    session.login_with_password(username, password, '1.0', 'xen-api-tutorial-helloxapi.py')
    main(session.xenapi)
    session.logout()
