#!/usr/bin/env python

# License check - loads in the xapi license file and creates a message if the license
# is about to expire. No error checking done at the moment.

import os, re, subprocess, sys, syslog, time, traceback
from xml.dom import minidom

gpg_binary_path="/usr/bin/gpg"
gpg_homedir="@OPTDIR@/gpg"
gpg_pub_keyring=gpg_homedir + "/pubring.gpg"
license_file="@ETCDIR@/license"
expiry_message_name="LICENSE_EXPIRES_SOON"

def match_installation(s):
    regex = re.compile("^INSTALLATION_UUID")
    return regex.search(s, 0)

def get_localhost():
    filename = '@INVENTORY@'
    try:
        f = open(filename, 'r')
    except:
        raise "Couldn't open inventory!"
    localhost = ''
    for line in filter(match_installation, f.readlines()):
        localhost = line.split("'")[1]
    if not localhost:
        raise "Couldn't find installation uuid!"
    return localhost

def log_err(err):
    print >>sys.stderr, err
    syslog.syslog(syslog.LOG_USER | syslog.LOG_ERR, "%s: %s" % (sys.argv[0], err))

def doexec(args, inputtext=None):
    """Execute a subprocess, then return its return code, stdout and stderr"""
    proc = subprocess.Popen(args,stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE,close_fds=True)
    (stdout,stderr) = proc.communicate(inputtext)
    rc = proc.returncode
    return (rc,stdout,stderr)

def unsign_license(license_file):
    (rc,stdout,stderr) = doexec([gpg_binary_path, "--homedir", gpg_homedir, "--no-default-keyring",
                                 "--keyring", gpg_pub_keyring, "--decrypt", license_file])
    return (rc,stdout,stderr)

def main():
    (rc,stdout,stderr) = unsign_license(license_file)
    if rc == 0:
        xmldoc = minidom.parseString(stdout)
        currenttime=time.time()
        lic=xmldoc.getElementsByTagName("xe_license")[0]
        expiry=float(lic.getAttribute("expiry"))
        timeleft=expiry-currenttime
        if (timeleft < 24.0 * 3600.0 * 30.0 and timeleft > 0):
            localhost=get_localhost()
            doexec(["xe","message-create","host-uuid=%s" % localhost, "name=%s" % expiry_message_name, "priority=10", "body=Your license will expire in %.0f days" % (timeleft / (24.0 * 3600.0))])
    return 0

if __name__ == '__main__':
    rc = 1
    try:
        rc = main()
    except:
        ex = sys.exc_info()
        err = traceback.format_exception(*ex)
        for exline in err:
            log_err(exline)

    sys.exit(rc)
