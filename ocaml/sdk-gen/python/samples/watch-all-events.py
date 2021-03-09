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


# Simple python example to demonstrate how to use the event system.
# Logs into the server, registers for events on all classes and prints
# them to the screen as they are received.


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
        sys.exit(1)
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
        new_session.xenapi.login_with_password(username, password, '1.0', 'xen-api-scripts-watch-all-events.py')
    except XenAPI.Failure as f:
        print "Failed to acquire a session: %s" % f.details
        sys.exit(1)
    main(new_session)
