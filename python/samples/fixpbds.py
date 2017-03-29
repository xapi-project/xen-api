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


# This example demonstrates how to alter the device config parameters of PBD
# objects, for example, in the case the storage server configuration has changed.
#
# This is somewhat more convoluted than simple parameter changes, as PBDs
# are read-only. This is to ensure they are always consistent with the 
# state of the world.
# The parameters to change are defined in the variable 'mapping'


import XenAPI
import sys


def main(session, sr, mapping):
    # Get all the PBDs associated with the SR
    sr = session.xenapi.SR.get_by_uuid(sr)
    pbds = session.xenapi.SR.get_PBDs(sr)
    
    # Unplug them all
    for pbd in pbds:
        session.xenapi.PBD.unplug(pbd)

    # Now delete and recreate them one by one, updating the dconf
    for pbd in pbds:
        rec=session.xenapi.PBD.get_record(pbd)
        newdconf=rec['device_config']
        newdconf.update(mapping)
        session.xenapi.PBD.destroy(pbd)
        print "host=",rec['host']," sr=",rec['SR'],"newdconf=",newdconf
        pbd=session.xenapi.PBD.create({'host':rec['host'],'SR':rec['SR'],'device_config':newdconf})
        session.xenapi.PBD.plug(pbd)

if __name__ == "__main__":
    if len(sys.argv) < 5:
        print "Usage:"
        print sys.argv[0], "<url> <username> <password> <sr-uuid>"
        print "Note that the device-config parameters that are updated are located in the source file."
        sys.exit(1)
    url = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]
    the_sr = sys.argv[4]
    
    # This could be parsed from the command line. 
    the_mapping = {"target": "127.0.0.2"}

    # First acquire a valid session by logging in:
    new_session = XenAPI.Session(url)
    try:
        new_session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-fixpbds.py")
    except XenAPI.Failure as f:
        print "Failed to acquire a session: %s" % f.details
        sys.exit(1)

    try:
        main(new_session, the_sr, the_mapping)
    except XenAPI.Failure as e:
        print e.details
        sys.exit(1)
    finally:
        new_session.xenapi.session.logout()
