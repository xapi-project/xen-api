#!/usr/bin/env python

# Copyright (c) Citrix Systems, Inc.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#   1) Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#   2) Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions and the following
#      disclaimer in the documentation and/or other materials
#      provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.


# This exmaple demonstrates how to apply a license file to a server. It assumes
# the license is contained within the file "license" in the current directory.


import sys
import time
import base64

import XenAPI

filename = "license" # filename containing the license data


def main(session):
    f = open(filename, "r")
    data = f.read()
    f.close()

    # base64 encode the license file
    data = base64.encodestring(data)
    
    # Pick a host at random
    hosts = session.xenapi.host.get_all()
    host = hosts[0]
    print "Applying license to host: %s" % (session.xenapi.host.get_name_label(host))
    session.xenapi.host.license_apply(host, data)

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)
    url = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]
    # First acquire a valid session by logging in:
    new_session = XenAPI.Session(url)
    try:
        new_session.xenapi.login_with_password(username, password, '1.0', 'xen-api-scripts-license.py')
    except XenAPI.Failure as e:
        print "Failed to acquire a session: %s" % e.details
        sys.exit(1)
    try:
        main(new_session)
    except XenAPI.Failure as e:
        print e.details
        sys.exit(1)
    finally:
        new_session.xenapi.session.logout()

