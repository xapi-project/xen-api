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

import XenAPI
import sys
import time

iso8601 = "%Y%m%dT%H:%M:%SZ"


def main(session):
    try:
        # interval in seconds, after which the event_from call should time out
        call_timeout = 30.0

        # interval (in seconds) between two subsequent event_from calls
        polling_interval = 10

        # the output of event_from includes a token, which can be passed into a
        # subsequent event_from call to retrieve only the events that have occurred
        # since the last call; if an empty string is passed, event_from will return
        # all events (this is normally done for the very first call)
        token = ''

        # get events for all classes
        event_types = ["*"]

        while True:
            try:
                print "Polling for events..."
                output = session.xenapi.event_from(event_types, token, call_timeout)
                events = output['events']
                token = output['token']

                print "Number of events retrieved: %s" % len(events)

                now = time.strftime(iso8601, time.gmtime(time.time()))
                # Print the events out in a nice format:
                fmt = "%18s %8s %s %20s  %5s  %s %s"

                if len(events) > 0:
                    hdr = fmt % ("time", "id", "ref", "class", "type", "name of object", "snapshot")
                    print "-" * (len(hdr))
                    print hdr
                    print "-" * (len(hdr))

                for event in events:
                    name = "n/a"
                    snapshot = ''
                    if "snapshot" in event.keys():
                        snapshot = event['snapshot']
                        if "name_label" in snapshot.keys():
                            name = snapshot['name_label']
                    print fmt % (now, event['id'], event["ref"], event['class'], event['operation'], name, repr(snapshot))

                print "Waiting for %s seconds before next poll..." % polling_interval
                time.sleep(polling_interval)

            except KeyboardInterrupt:
                break

            except XenAPI.Failure as e:
                print e.details
                break
    finally:
        session.xenapi.session.logout()


def print_usage():
    print """
Usage:
    %s <url> <username> <password>
or
    %s [http://]localhost [<username>] [<password>]
""" % (sys.argv[0], sys.argv[0])

    
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print_usage()
        sys.exit(1)

    url = sys.argv[1]
    username = sys.argv[2] if len(sys.argv) > 2 else ""
    password = sys.argv[3] if len(sys.argv) > 3 else ""

    # First acquire a valid session by logging in:
    if url == "http://localhost" or url == "localhost":
        new_session = XenAPI.xapi_local()
    else:
        new_session = XenAPI.Session(url)
    try:
        new_session.xenapi.login_with_password(username, password, '1.0', 'xen-api-scripts-watch-event-contents.py')
    except XenAPI.Failure as f:
        print "Failed to acquire a session: %s" % f.details
        sys.exit(1)
    main(new_session)
