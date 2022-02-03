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
import abc
import XenAPIPlugin
import XenAPI
import sys
import subprocess
import os
import tempfile
import logging
import logging.handlers
from collections import OrderedDict
from enum import Enum

# this plugin manage following configuration files for external auth
# - /etc/nsswitch.conf
# - /etc/pam.d/sshd
# - /etc/pam.d/hcp_users
# - /etc/ssh/ssh_config


def setup_logger():
    logger = logging.getLogger()
    logging.basicConfig(format='%(asctime)s %(levelname)s %(name)s %(funcName)s %(message)s', level=logging.DEBUG)
    # Send to syslog local5, which will be redirected to xapi log /var/log/xensource.log
    handler = logging.handlers.SysLogHandler(facility='local5', address='/dev/log')
    std_err = logging.StreamHandler(sys.stderr)
    # Send to authpriv, which will be redirected to /var/log/secure
    auth_log = logging.handlers.SysLogHandler(facility="authpriv", address='/dev/log')
    logger.addHandler(handler)
    logger.addHandler(std_err)
    logger.addHandler(auth_log)


setup_logger()
logger = logging.getLogger(__name__)


def run_cmd(cmd, log_cmd=True):
    try:
        result = subprocess.check_output(cmd)
        if log_cmd:
            logger.debug("{} -> {}".format(cmd, result))
        return result.strip()
    except Exception:
        logger.exception("Failed to run command %s", cmd)
        return None


class ADBackend(Enum):
    bd_pbis = 0
    bd_winbind = 1


class ADConfig(object):
    def __init__(self, path, session, args, ad_enabled=True, load_existing=True, file_mode=0o644):
        self._file_path = path
        self._session = session
        self._args = args
        self._lines = []
        self._backend = self._get_ad_backend(args)
        self._ad_enabled = ad_enabled
        self._file_mode = file_mode
        if load_existing and os.path.exists(self._file_path):
            with open(self._file_path, 'r') as f:
                lines = f.readlines()
                self._lines = [l.strip() for l in lines]

    def _get_ad_backend(self, args):
        if  args.get("ad_backend", "pbis") == "pbis":
            logger.debug("pbis is used as AD backend")
            return ADBackend.bd_pbis

        logger.debug("winbind is used as AD backend")
        return ADBackend.bd_winbind

    @abc.abstractmethod
    def _apply_to_cache(self):
        pass

    def apply(self):
        self._apply_to_cache()
        self._install()

    def _install(self):
        with tempfile.NamedTemporaryFile(prefix="extauth-", delete=False) as f:
            f.write("\n".join(self._lines).encode())
            f.flush()
            os.rename(f.name, self._file_path)
            os.chmod(self._file_path, self._file_mode)


class StaticPam(ADConfig):
    ad_pam_format = """#%PAM-1.0
auth        required      pam_env.so
auth        sufficient    pam_unix.so try_first_pass nullok
auth        sufficient    {ad_module} try_first_pass try_authtok
auth        required      pam_deny.so

session     optional      pam_keyinit.so force revoke
session     required      pam_limits.so
session     [success=1 default=ignore] pam_succeed_if.so service in crond quiet use_uid
session     required      pam_unix.so
session     required      pam_loginuid.so
session     sufficient    {ad_module}

account     required      pam_nologin.so
account     required      {ad_module} unknown_ok
account     include       hcp_users
account     required      pam_unix.so"""

    no_ad_pam = """#%PAM-1.0
auth       include      system-auth
account    required     pam_nologin.so
account    include      system-auth
password   include      system-auth
session    optional     pam_keyinit.so force revoke
session    include      system-auth
session    required     pam_loginuid.so"""

    def __init__(self, session, args, ad_enabled=True):
        super(StaticPam, self).__init__("/etc/pam.d/sshd", session, args, ad_enabled,
                                        load_existing=False)

    def _apply_to_cache(self):
        if self._ad_enabled:
            if self._backend == ADBackend.bd_pbis:
                ad_pam_module = "/lib/security/pam_lsass.so"
            else:
                ad_pam_module = "pam_winbind.so"
            content = self.ad_pam_format.format(ad_module=ad_pam_module)
        else:
            content = self.no_ad_pam
        self._lines = content.split("\n")

class DynamicPam(ADConfig):
    def __init__(self, session, arg, ad_enabled=True):
        super(DynamicPam, self).__init__("/etc/pam.d/hcp_users", session, arg, ad_enabled,
                                         load_existing=False)

    def _apply_to_cache(self):
        # AD is not enabled, just return as the configure file will be removed during install
        if not self._ad_enabled:
            return
        # Rewrite the PAM SSH config using the latest info from Active Directory
        # and the list of subjects from xapi
        subjects = self._session.xenapi.subject.get_all()
        admin_role = self._session.xenapi.role.get_by_name_label('pool-admin')[0]
        # Add each subject which contains the admin role
        for opaque_ref in subjects:
            subject_rec = self._session.xenapi.subject.get_record(opaque_ref)
            if admin_role in subject_rec['roles']:
                self._add_subject(subject_rec)

    def _add_subject(self, subject_rec):
        try:
            sid = subject_rec['subject_identifier']
            name = subject_rec["other_config"]["subject-name"]
            # CA-363207: Pam cannot handle space, put it inside []
            if " " in name:
                name = "[{}]".format(name)
            is_group = subject_rec["other_config"]["subject-is-group"] == "true"
            logger.debug("Permit %s with sid %s is_group as %s", name, sid, is_group)
            condition = "ingroup" if is_group else "="
            self._lines.append("account sufficient pam_succeed_if.so user {} {}".format(condition, name))
        except Exception:
            logger.warning("Failed to check subject %s for dynamic pam", subject_rec)

    def _install(self):
        if self._ad_enabled:
            super(DynamicPam, self)._install()
        else:
            if os.path.exists(self._file_path):
                os.remove(self._file_path)


class KeyValueConfig(ADConfig):
    # Only support configure files with key value in each line, seperated by sep
    # Otherwise, it will be just copied and un-configurable
    # If multiple lines with the same key exists, only the first line will be configured
    _special_line_prefix = "__key_value_config_sp_line_prefix_" # Presume normal config does not have such keys
    _empty_value = ""

    def __init__(self, path, session, args, ad_enabled=True, load_existing=True, file_mode=0o644, sep=": ", comment="#"):
        super(KeyValueConfig, self).__init__(path, session, args, ad_enabled, load_existing, file_mode)
        self._sep = None if sep.isspace() else sep  # Ignore number/type of spaces
        self._comment = comment
        self._values = OrderedDict()
        self._load_values()

    def _is_comment_line(self, line):
        return line.startswith(self._comment)

    def is_special_line(self, line):
        return line.startswith(self._special_line_prefix)

    def _load_values(self):
        for idx, line in enumerate(self._lines):
            sp_key = "{}{}".format(self._special_line_prefix, str(idx)) # Generate a unique key to store multiple special lines
            if line == "": # Empty line
                self._values[sp_key] = self._empty_value
            elif self._is_comment_line(line):
                self._values[sp_key] = line
            else: # Parse the key, value pair
                kv = line.split(self._sep)
                if len(kv) != 2:
                    # Taken as raw line
                    self._values[sp_key] = line
                else:
                    k , v = kv[0].strip(), kv[1].strip()
                    if k not in self._values:
                        self._values[k] = v
                    else:
                        # Key already exists, Not supported as configurable
                        self._values[sp_key] = line

    def _update_key_value(self, key, value):
        self._values[key] = value

    def _apply_value(self, key, value):
        if self.is_special_line(key):
            line = value
        else: # normal line, construct the key value pair
            sep = self._sep if self._sep else  " "
            line = "{}{}{}".format(key, sep, value)
        self._lines.append(line)

    def _apply_to_cache(self):
        self._lines = []
        for k, v in self._values.items():
            self._apply_value(k, v)


class NssConfig(KeyValueConfig):
    def __init__(self, session, args, ad_enabled=True):
        super(NssConfig, self).__init__("/etc/nsswitch.conf", session, args, ad_enabled)
        modules = "files sss"
        if ad_enabled:
            if self._backend == ADBackend.bd_pbis:
                modules = "files sss lsass"
            else:
                modules = "files hcp winbind"
        self._update_key_value("passwd", modules)
        self._update_key_value("group", modules)
        self._update_key_value("shadow", modules)


class SshdConfig(KeyValueConfig):
    def __init__(self, session, args, ad_enabled=True):
        super(SshdConfig, self).__init__("/etc/ssh/sshd_config", session, args, ad_enabled,
                                         sep=" ")
        value = "yes" if ad_enabled else "no"
        self._update_key_value("ChallengeResponseAuthentication", value)
        self._update_key_value("GSSAPIAuthentication", value)
        self._update_key_value("GSSAPICleanupCredentials", value)

    def apply(self):
        super(SshdConfig, self).apply()
        run_cmd(["/usr/bin/systemctl", "reload-or-restart", "sshd"])


class ConfigManager:
    def __init__(self, session, args, ad_enabled=True):
        self._build_config(session, args, ad_enabled)

    def _build_config(self, session, args, ad_enabled):
        self._nss = NssConfig(session, args, ad_enabled)
        self._sshd = SshdConfig(session, args, ad_enabled)
        self._static_pam = StaticPam(session, args, ad_enabled)
        self._dynamic_pam = DynamicPam(session, args, ad_enabled)

    def refresh_all(self):
        self._nss.apply()
        self._sshd.apply()
        self._dynamic_pam.apply()
        self._static_pam.apply()

    def refresh_dynamic_pam(self):
        self._dynamic_pam.apply()


def refresh_all_configurations(session, args, name, ad_enabled=True):
    try:
        logger.info("refresh_all_configurations for %s", name)
        ConfigManager(session, args, ad_enabled).refresh_all()
        return str(True)
    except Exception:
        msg = "Failed to refresh all configurations"
        logger.exception(msg)
        return msg


def refresh_dynamic_pam(session, args, name):
    try:
        logger.info("refresh_dynamic_pam for %s", name)
        ConfigManager(session, args).refresh_dynamic_pam()
        return str(True)
    except Exception:
        msg = "Failed to refresh dynamic pam configuration"
        logger.exception(msg)
        return msg


def after_extauth_enable(session, args):
    return refresh_all_configurations(session, args, "after_extauth_enable")


def after_xapi_initialize(session, args):
    return refresh_all_configurations(session, args, "after_xapi_initialize")


def after_subject_add(session, args):
    return refresh_dynamic_pam(session, args, "after_subject_add")


def after_subject_remove(session, args):
    return refresh_dynamic_pam(session, args, "after_subject_remove")


def after_subject_update(session, args):
    return refresh_dynamic_pam(session, args, "after_subject_update")


def after_roles_update(session, args):
    return refresh_dynamic_pam(session, args, "after_roles_update")


def before_extauth_disable(session, args):
    return refresh_all_configurations(session, args, "before_extauth_disable", False)


# The dispatcher
if __name__ == "__main__":
    dispatch_tbl = {
        "after-extauth-enable":  after_extauth_enable,
        "after-xapi-initialize": after_xapi_initialize,
        "after-subject-add":     after_subject_add,
        "after-subject-update":  after_subject_update,
        "after-subject-remove":  after_subject_remove,
        "after-roles-update":    after_roles_update,
        "before-extauth-disable":before_extauth_disable,
    }
    XenAPIPlugin.dispatch(dispatch_tbl)
