#!/usr/bin/env python

# Send back-to-back 'Host.get_servertime' calls to simulate the GUI's heartbeat and record latency.

import XenAPI, sys, time

iso8601 = "%Y%m%dT%H:%M:%SZ"

def main(session):
    global iso8601
    pool = session.xenapi.pool.get_all()[0]
    host = session.xenapi.pool.get_master(pool)
    while True:
        start = time.time()
        session.xenapi.host.get_servertime(host)
        latency = time.time() - start
        date = time.strftime(iso8601, time.gmtime(start))
        print "%s %.2f" % (date, latency)
        sys.stdout.flush()
        time.sleep(5)


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
    session.xenapi.login_with_password(username, password, "1.0", "xen-api-scripts-pingmaster.py")
    try:
        main(session)
    finally:
        session.xenapi.logout()
        
