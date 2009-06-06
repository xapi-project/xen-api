#!/usr/bin/env python
# Copyright (c) 2006-2007 XenSource, Inc.
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Apply a new license to a XenServer

import sys, time, base64

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
    if len(sys.argv) <> 4:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)
    url = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]
    # First acquire a valid session by logging in:
    session = XenAPI.Session(url)
    session.xenapi.login_with_password(username, password)
    main(session)
    session.xenapi.session.logout()

