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

# Update the PBDs of a particular SR.
# This is somewhat more convoluted than simple parameter changes, as PBDs
# are read-only. This is to ensure they are always consistent with the 
# state of the world.
# The parameters to change are defined in the variable 'map'

import XenAPI, sys

def main(session,sr,map):
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
        newdconf.update(map)
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
    sr = sys.argv[4]
    
    # This could be parsed from the command line. 
    map = { "target":"127.0.0.2" }

    # First acquire a valid session by logging in:
    session = XenAPI.Session(url)
    session.xenapi.login_with_password(username, password)
    main(session,sr,map)


    
