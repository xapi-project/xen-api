#!/usr/bin/env python

# Count the number of events received from the master

import XenAPI, sys, time

iso8601 = "%Y-%m-%dT%H:%M:%SZ"


def main(session):
    global iso8601

    token = ''
    call_timeout = 30.0

    while True:
        sys.stdout.flush()

        now = time.time()
        now_string = time.strftime(iso8601, time.gmtime(now))

        try:
            output = session.xenapi.event_from(["*"], token, call_timeout)
            events = output['events']
            token = output['token']
            print "%s %10d 0" % (now_string, len(events))
            time.sleep(5)

        except KeyboardInterrupt:
            break

        except XenAPI.Failure as e:
            print e.details
            sys.exit(1)


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)

    url = sys.argv[1]
    if url[:5] != "https":
        raise Exception("Must use SSL for a realistic test")
    
    username = sys.argv[2]
    password = sys.argv[3]
    
    new_session = XenAPI.Session(url)
    try:
        new_session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-eventcount.py")
    except XenAPI.Failure as f:
        print "Failed to acquire a session: %s" % f.details
        sys.exit(1)

    try:
        main(new_session)
    finally:
        new_session.xenapi.logout()
