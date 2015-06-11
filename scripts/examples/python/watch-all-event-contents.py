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


# Simple python example to demonstrate the event system. Logs into the server,
# registers for all events and prints them to the screen.

import XenAPI, sys, time

iso8601 = "%Y%m%dT%H:%M:%SZ"

def main(session):
    try:
        # Register for events on all classes:
        session.xenapi.event.register(["*"])
        while True:
            try:
                events = session.xenapi.event.next()
                now = time.strftime(iso8601, time.gmtime(time.time()))
                # Print the events out in a nice format:
                fmt = "%18s %8s %s %20s  %5s  %s %s"
                hdr = fmt % ("time", "id", "ref", "class", "type", "name of object (if available)", "snapshot")
                print "-" * (len(hdr))
                print hdr
                print "-" * (len(hdr))
                for event in events:
                    name = "(unknown object name)"
                    if "snapshot" in event.keys():
                        snapshot = event['snapshot']
                        if "name_label" in snapshot.keys():
                            name = snapshot['name_label']
                    print fmt % (now, event['id'], event["ref"], event['class'], event['operation'], name, repr(snapshot))

            except XenAPI.Failure, e:
                if e.details <> [ "EVENTS_LOST" ]: raise
                print "** Caught EVENTS_LOST error: some events may be lost"
                # Check for the "EVENTS_LOST" error (happens if the event queue fills up on the
                # server and some events have been lost). The only thing we can do is to
                # unregister and then re-register again for future events.
                # NB: A program which is waiting for a particular condition to become true would
                # need to explicitly poll the state to make sure the condition hasn't become
                # true in the gap.
                session.xenapi.event.unregister(["*"])
                session.xenapi.event.register(["*"])
    finally:
        session.xenapi.session.logout()
        
def usage_and_quit():
    print "Usage:"
    print sys.argv[0], " url <username> <password>"
    sys.exit(1)
    
if __name__ == "__main__":
    if len(sys.argv) < 2:
        usage_and_quit()
    url = sys.argv[1]
    username = ""
    password = ""
    if len(sys.argv) > 2:
        username = sys.argv[2]
    if len(sys.argv) > 3:
        password = sys.argv[3]
    # First acquire a valid session by logging in:
    if url == "http://localhost" or url == "localhost":
        session = XenAPI.xapi_local()
    else:
        session = XenAPI.Session(url)
    session.xenapi.login_with_password(username, password, '1.0', 'xen-api-scripts-watch-event-contents.py')
    main(session)
