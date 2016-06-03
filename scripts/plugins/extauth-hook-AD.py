#!/usr/bin/env python
#
# extauth-hook-AD.py
#
# This module can be called directly as a plugin.  It handles
# Active Directory being enabled or disabled as the hosts external_auth_type,
# or subjects being added or removed while AD is the external_auth_type,
# or xapi starting or stopping while AD is the external_auth_type.
#
# Alternatively, the extauth-hook module can be called, which will
# dispatch to the correct extauth-hook-<type>.py module automatically.

import XenAPIPlugin
import XenAPI
import sys
import os
import getopt
import tempfile
import commands
import syslog

etc_pamd_sshd_start_boilerplate = """#%PAM-1.0
auth        required      pam_env.so
auth        sufficient    pam_unix.so try_first_pass nullok
auth        sufficient    /lib/security/pam_lsass.so try_first_pass
auth        required      pam_deny.so

session     optional      pam_keyinit.so force revoke
session     required      pam_limits.so
session     [success=1 default=ignore] pam_succeed_if.so service in crond quiet use_uid
session     required      pam_unix.so
session     required      pam_loginuid.so
session     sufficient    /lib/security/pam_lsass.so

account    required       pam_nologin.so
account     required      /lib/security/pam_lsass.so unknown_ok
# Start of list of allowed AD groups and users
"""

etc_pamd_sshd_end_boilerplate = """
# End of list of allowed AD groups and users
account     required      pam_unix.so
"""

etc_pamd_sshd_ad_disabled = """#%PAM-1.0
auth       include      system-auth
account    required     pam_nologin.so
account    include      system-auth
password   include      system-auth
session    optional     pam_keyinit.so force revoke
session    include      system-auth
session    required     pam_loginuid.so
"""

def log_err(err):
    print >>sys.stderr, err
    syslog.syslog(syslog.LOG_USER | syslog.LOG_ERR, "%s: %s" % (sys.argv[0], err))


class PamSshConfig:
    def __init__(self):
        # Create a temporary file for staging, and start it off
        self.temp_fd, self.temp_fname = tempfile.mkstemp(prefix="sshd-", dir="/etc/pam.d")
        os.write(self.temp_fd, etc_pamd_sshd_start_boilerplate)
        self.installed = False

    def install(self):
        # Complete the temporary file and move it to /etc/pam.d/sshd
        os.write(self.temp_fd, etc_pamd_sshd_end_boilerplate)
        os.rename(self.temp_fname, "/etc/pam.d/sshd")
        self.installed = True

    def add_subject(self, sid):
        # Add a subject to the temporary file
        if self.installed:
            raise Exception, "Cannot add subject once installed "
        lines = commands.getoutput("/opt/pbis/bin/find-by-sid %s" % sid).split("\n")
        name_lines = filter(lambda x: x.startswith("Name:"), lines)
        if len(name_lines) != 1:
            # Just warn, don't raise exception - there may be others that work
            log_err("Could not find user/group corresponding to SID %s (%s)" % (sid,str(lines)))
            return
        name = name_lines[0].split(":")[1].strip()
        uid_lines = filter(lambda x: x.startswith("Uid:"), lines)
        if len(uid_lines) == 0:
            is_group = True
        else:
            is_group = False

        if is_group:
            os.write(self.temp_fd, "account sufficient pam_succeed_if.so user ingroup %s\n" % name)
        else:
            os.write(self.temp_fd, "account sufficient pam_succeed_if.so user = %s\n" % name)


def rewrite_etc_pamd_ssh(session, args):
    # Rewrite the PAM SSH config using the latest info from Active Directory
    # and the list of subjects from xapi
    try:
        config = PamSshConfig()
        subjects = session.xenapi.subject.get_all()
        admin_role = session.xenapi.role.get_by_name_label('pool-admin')[0]
        # Add each subject which contains the admin role
        for opaque_ref in subjects:
            subject_rec = session.xenapi.subject.get_record(opaque_ref)
            sid = subject_rec['subject_identifier']
            if admin_role in subject_rec['roles']:
                config.add_subject(sid)
        config.install()
        return str(True)
    except:
        return "ERROR_0: rewrite_etc_pamd_ssh failed"

def revert_etc_pamd_ssh(session, args):
    # Revert the PAM SSH config to the default version that doesn't support
    # Active Directory
    try:
        fd, fname = tempfile.mkstemp(prefix="sshd-", dir="/etc/pam.d")
        os.write(fd, etc_pamd_sshd_ad_disabled)
        os.rename(fname, "/etc/pam.d/sshd")
        return str(True)
    except:
        return "ERROR_1: revert_etc_pamd_ssh failed"


def after_extauth_enable(session, args):
    return rewrite_etc_pamd_ssh(session, args)

def after_xapi_initialize(session, args):
    return rewrite_etc_pamd_ssh(session, args)

def after_subject_add(session, args):
    return rewrite_etc_pamd_ssh(session, args)

def after_subject_remove(session, args):
    return rewrite_etc_pamd_ssh(session, args)

def after_roles_update(session, args):
    return rewrite_etc_pamd_ssh(session, args)

def before_extauth_disable(session, args):
    return revert_etc_pamd_ssh(session, args)

# The dispatcher
if __name__ == "__main__":
    dispatch_tbl = {
        "after-extauth-enable":  after_extauth_enable,
        "after-xapi-initialize": after_xapi_initialize,
        "after-subject-add":     after_subject_add,
        "after-subject-remove":  after_subject_remove,
        "after-roles-update":    after_roles_update,
        "before-extauth-disable":before_extauth_disable,
    }
    XenAPIPlugin.dispatch(dispatch_tbl)

