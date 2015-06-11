#!/usr/bin/env python

# Count the number of events received from the master

import XenAPI, sys, time

iso8601 = "%Y%m%dT%H:%M:%SZ"

def main(session):
    global iso8601

    # Register for events on all classes:
    session.xenapi.event.register(["*"])
    while True:
        sys.stdout.flush()
        time.sleep(5)
        now = time.time()
        now_string = time.strftime(iso8601, time.gmtime(now))
        try:
            events = session.xenapi.event.next()
            print "%s %d 0" % (now_string, len(events))
            
        except XenAPI.Failure, e:
            if e.details <> [ "EVENTS_LOST" ]: raise
            print "%s 0 1" % now_string
            session.xenapi.event.unregister(["*"])
            session.xenapi.event.register(["*"])


if __name__ == "__main__":
    if len(sys.argv) <> 4:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)
    url = sys.argv[1]
    if url[:5] <> "https":
        raise "Must use SSL for a realistic test"
    
    username = sys.argv[2]
    password = sys.argv[3]
    
    session = XenAPI.Session(url)
    session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-eventcount.py")
    try:
        main(session)
    finally:
        session.xenapi.logout()
        
