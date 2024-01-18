#!/usr/bin/env python3
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
import sys
import subprocess
import os
import shutil
import tempfile
import logging
import logging.handlers
from collections import OrderedDict
from enum import Enum
import XenAPIPlugin
import XenAPI


# this plugin manage following configuration files for external auth
# - /etc/nsswitch.conf
# - /etc/pam.d/sshd
# - /etc/pam.d/hcp_users
# - /etc/ssh/ssh_config

# pylint: disable=super-with-arguments


HCP_USERS = "/etc/security/hcp_ad_users.conf"
HCP_GROUPS = "/etc/security/hcp_ad_groups.conf"


def setup_logger():
    """Helper function setup logger"""
    addr = "/dev/log"

    logging.basicConfig(
        format='%(asctime)s %(levelname)s %(name)s %(funcName)s %(message)s', level=logging.DEBUG)
    log = logging.getLogger()

    if not os.path.exists(addr):
        log.warning("{} not available, logs are not redirected".format(addr))
        return

    # Send to syslog local5, which will be redirected to xapi log /var/log/xensource.log
    handler = logging.handlers.SysLogHandler(
        facility='local5', address=addr)
    # Send to authpriv, which will be redirected to /var/log/secure
    auth_log = logging.handlers.SysLogHandler(
        facility="authpriv", address=addr)
    log.addHandler(handler)
    log.addHandler(auth_log)


setup_logger()
logger = logging.getLogger(__name__)


def run_cmd(cmd, log_cmd=True):
    """Helper function to run command"""
    try:
        result = subprocess.check_output(cmd)
        if log_cmd:
            msg = "{} -> {}".format(cmd, result)
            logger.debug(msg)
        return result.strip()
    except Exception:  # pylint: disable=broad-except
        logger.exception("Failed to run command %s", cmd)
        return None


class ADBackend(Enum):
    """Enum for AD backend"""
    BD_PBIS = 0
    BD_WINBIND = 1


# pylint: disable=useless-object-inheritance, too-few-public-methods
class ADConfig(object):
    """Base class for AD configuration"""
    #pylint: disable=too-many-arguments

    def __init__(self, path, session, args, ad_enabled=True, load_existing=True, file_mode=0o644):
        self._file_path = path
        self._session = session
        self._args = args
        self._lines = []
        self._backend = self._get_ad_backend()
        self._ad_enabled = ad_enabled
        self._file_mode = file_mode
        if load_existing and os.path.exists(self._file_path):
            with open(self._file_path, 'r') as file:
                lines = file.readlines()
                self._lines = [l.strip() for l in lines]

    def _get_ad_backend(self):
        """Get active AD backend"""
        if self._args.get("ad_backend", "winbind") == "pbis":
            logger.debug("pbis is used as AD backend")
            return ADBackend.BD_PBIS

        logger.debug("winbind is used as AD backend")
        return ADBackend.BD_WINBIND

    @abc.abstractmethod
    def _apply_to_cache(self):
        pass

    def apply(self):
        """Apply configuration"""
        self._apply_to_cache()
        self._install()

    def _install(self):
        """Install configuration"""
        with tempfile.NamedTemporaryFile(prefix="extauth-", delete=False) as file:
            file.write("\n".join(self._lines).encode("utf-8"))
            file.flush()
            shutil.move(file.name, self._file_path)
            os.chmod(self._file_path, self._file_mode)


class StaticSSHPam(ADConfig):
    """
    Class to manage ssh pam configuration
    """
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
account     sufficient    pam_listfile.so item=user file={user_list} sense=allow onerr=fail
account     sufficient    pam_listfile.so item=group file={group_list} sense=allow onerr=fail
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
        super(StaticSSHPam, self).__init__("/etc/pam.d/sshd", session, args, ad_enabled,
                                           load_existing=False)

    def _apply_to_cache(self):
        if self._ad_enabled:
            if self._backend == ADBackend.BD_PBIS:
                ad_pam_module = "/lib/security/pam_lsass.so"
            else:
                ad_pam_module = "pam_winbind.so"
            content = self.ad_pam_format.format(ad_module=ad_pam_module,
                                                user_list=HCP_USERS, group_list=HCP_GROUPS)
        else:
            content = self.no_ad_pam
        self._lines = content.split("\n")


class DynamicPam(ADConfig):
    #pylint: disable=too-few-public-methods
    """Base class to manage AD users and groups configure which permit pool admin ssh"""

    def __init__(self, path, session, args, ad_enabled=True):
        super(DynamicPam, self).__init__(path, session, args, ad_enabled,
                                         load_existing=False)
        self.admin_role = self._session.xenapi.role.get_by_name_label(
            'pool-admin')[0]

    def _apply_to_cache(self):
        # AD is not enabled, just return as the configure file will be removed during install
        if not self._ad_enabled:
            return

        try:
            # Rewrite the PAM SSH config using the latest info from Active Directory
            # and the list of subjects from xapi
            subjects = self._session.xenapi.subject.get_all()
            # Add each subject which contains the admin role
            for opaque_ref in subjects:
                subject_rec = self._session.xenapi.subject.get_record(
                    opaque_ref)
                if self._is_pool_admin(subject_rec) and self._is_responsible_for(subject_rec):
                    self._add_subject(subject_rec)
            self._lines.append("")  # ending new line
        except Exception as exp:  # pylint: disable=broad-except
            logger.info("Failed to add subjects %s", str(exp))

    def _is_pool_admin(self, subject_rec):
        try:
            return self.admin_role in subject_rec['roles']
        except KeyError:
            logger.warning("subject %s does not have role", subject_rec)
            return False

    def _format_item(self, item):
        space_replacement = "+"
        if self._backend == ADBackend.BD_PBIS:
            if space_replacement in item:
                raise ValueError(
                    "{} is not permitted in subject name".format(space_replacement))
            # PBIS relace space with "+", eg "ab  cd" -> "ab++cd"
            # PBIS pam module will reverse it back
            return item.replace(" ", space_replacement)
        return item

    def _is_responsible_for(self, subject_rec):
        try:
            return self._match_subject(subject_rec)
        except KeyError:
            logger.exception("Failed to match subject %s", subject_rec)
            return False

    @abc.abstractmethod
    def _match_subject(self, subject_rec):
        pass

    @abc.abstractmethod
    def _add_subject(self, subject_rec):
        pass

    def _install(self):
        if self._ad_enabled:
            super(DynamicPam, self)._install()
        else:
            if os.path.exists(self._file_path):
                os.remove(self._file_path)


class UsersList(DynamicPam):
    #pylint: disable=too-few-public-methods
    """Class manage users which permit pool admin ssh"""

    def __init__(self, session, arg, ad_enabled=True):
        super(UsersList, self).__init__(HCP_USERS, session, arg, ad_enabled)

    def _match_subject(self, subject_rec):
        return subject_rec["other_config"]["subject-is-group"] != "true"

    def _add_upn(self, subject_rec):
        sep = "@"
        upn = ""
        try:
            upn = subject_rec["other_config"]["subject-upn"]
            user, domain = upn.split(sep)
            if self._backend == ADBackend.BD_PBIS:
                # PBIS convert domain to UPPER case, we revert it back
                domain = domain.lower()
            self._lines.append(u"{}{}{}".format(user, sep, domain))
        except KeyError:
            logger.info("subject does not have upn %s", subject_rec)
        except ValueError:
            logger.info("UPN format is not right %s", upn)

    def _add_subject(self, subject_rec):
        try:
            sid = subject_rec['subject_identifier']
            name = subject_rec["other_config"]["subject-name"]
            formatted_name = self._format_item(name)
            logger.debug("Permit user %s, Current sid is %s",
                         formatted_name, sid)
            self._lines.append(formatted_name)
            # If ssh key is permittd in authorized_keys,
            # The original name is compared, add UPN and original name
            if self._backend == ADBackend.BD_PBIS and name != formatted_name:
                self._lines.append(name)
            self._add_upn(subject_rec)
        # pylint: disable=broad-except
        except Exception as exp:
            logger.warning("Failed to add user %s: %s", subject_rec, str(exp))


class GroupsList(DynamicPam):
    #pylint: disable=too-few-public-methods
    """Class manage groups which permit pool admin ssh"""

    def __init__(self, session, arg, ad_enabled=True):
        super(GroupsList, self).__init__(HCP_GROUPS, session, arg, ad_enabled)

    def _match_subject(self, subject_rec):
        return subject_rec["other_config"]["subject-is-group"] == "true"

    def _add_subject(self, subject_rec):
        try:
            sid = subject_rec['subject_identifier']
            name = self._format_item(
                subject_rec["other_config"]["subject-name"])
            logger.debug("Permit group %s, Current sid is %s", name, sid)
            self._lines.append(name)
       # pylint: disable=broad-except
        except Exception as exp:
            logger.warning("Failed to add group %s:%s", subject_rec, str(exp))


class KeyValueConfig(ADConfig):
    """
     Only support configure files with key value in each line, seperated by sep
     Otherwise, it will be just copied and un-configurable
     If multiple lines with the same key exists, only the first line will be configured
    """
    # Presume normal config does not have such keys
    _special_line_prefix = "__key_value_config_sp_line_prefix_"
    _empty_value = ""

    #pylint: disable=too-many-arguments
    def __init__(self, path, session, args, ad_enabled=True, load_existing=True,
                 file_mode=0o644, sep=": ", comment="#"):
        super(KeyValueConfig, self).__init__(path, session,
                                             args, ad_enabled, load_existing, file_mode)
        self._sep = None if sep.isspace() else sep  # Ignore number/type of spaces
        self._comment = comment
        self._values = OrderedDict()
        self._load_values()

    def _is_comment_line(self, line):
        return line.startswith(self._comment)

    def _is_special_line(self, line):
        """Whether the line is a special line"""
        return line.startswith(self._special_line_prefix)

    def _load_values(self):
        for idx, line in enumerate(self._lines):
            # Generate a unique key to store multiple special lines
            sp_key = "{}{}".format(self._special_line_prefix, str(idx))
            if line == "":  # Empty line
                self._values[sp_key] = self._empty_value
            elif self._is_comment_line(line):
                self._values[sp_key] = line
            else:  # Parse the key, value pair
                item = line.split(self._sep)
                if len(item) != 2:
                    # Taken as raw line
                    self._values[sp_key] = line
                else:
                    key, value = item[0].strip(), item[1].strip()
                    if key not in self._values:
                        self._values[key] = value
                    else:
                        # Key already exists, Not supported as configurable
                        self._values[sp_key] = line

    def _update_key_value(self, key, value):
        self._values[key] = value

    def _apply_value(self, key, value):
        if self._is_special_line(key):
            line = value
        else:  # normal line, construct the key value pair
            sep = self._sep if self._sep else " "
            line = "{}{}{}".format(key, sep, value)
        self._lines.append(line)

    def _apply_to_cache(self):
        self._lines = []
        for key, value in self._values.items():
            self._apply_value(key, value)


class NssConfig(KeyValueConfig):
    """Class to manage NSS configuration"""

    def __init__(self, session, args, ad_enabled=True):
        super(NssConfig, self).__init__(
            "/etc/nsswitch.conf", session, args, ad_enabled)
        modules = "files sss"
        if ad_enabled:
            if self._backend == ADBackend.BD_PBIS:
                modules = "files sss lsass"
            else:
                modules = "files hcp winbind"
        self._update_key_value("passwd", modules)
        self._update_key_value("group", modules)
        self._update_key_value("shadow", modules)


class SshdConfig(KeyValueConfig):
    """Class to manage sshd configuration"""

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


class PamWinbindConfig(KeyValueConfig):
    """Class to manage winbind pam configuration"""

    def __init__(self, session, args, ad_enabled=True):
        super(PamWinbindConfig, self).__init__("/etc/security/pam_winbind.conf", session, args,
                                               ad_enabled, sep=" = ")
        self._update_key_value("krb5_auth", "yes")


class ConfigManager:
    """Class to manage all the AD configurations"""

    def __init__(self, session, args, ad_enabled=True):
        self._build_config(session, args, ad_enabled)

    def _build_config(self, session, args, ad_enabled):
        self._nss = NssConfig(session, args, ad_enabled)
        self._sshd = SshdConfig(session, args, ad_enabled)
        self._static_pam = StaticSSHPam(session, args, ad_enabled)
        self._users = UsersList(session, args, ad_enabled)
        self._groups = GroupsList(session, args, ad_enabled)
        self._pam_winbind = PamWinbindConfig(session, args, ad_enabled)

    def refresh_all(self):
        """Update all the configurations"""
        self._nss.apply()
        self._sshd.apply()
        self._users.apply()
        self._groups.apply()
        self._static_pam.apply()
        self._pam_winbind.apply()

    def refresh_dynamic_pam(self):
        """Only refresh the dynanic configurations"""
        self._users.apply()
        self._groups.apply()


def refresh_all_configurations(session, args, name, ad_enabled=True):
    """Update all configurations"""
    try:
        logger.info("refresh_all_configurations for %s", name)
        ConfigManager(session, args, ad_enabled).refresh_all()
        return str(True)
    except Exception:  # pylint: disable=broad-except
        msg = "Failed to refresh all configurations"
        logger.exception(msg)
        return msg


def refresh_dynamic_pam(session, args, name):
    """Refresh dynamic pam configurations"""
    try:
        logger.info("refresh_dynamic_pam for %s", name)
        ConfigManager(session, args).refresh_dynamic_pam()
        return str(True)
    except Exception:  # pylint: disable=broad-except
        msg = "Failed to refresh dynamic pam configuration"
        logger.exception(msg)
        return msg


def after_extauth_enable(session, args):
    """Callback for after enable external auth"""
    return refresh_all_configurations(session, args, "after_extauth_enable")


def after_xapi_initialize(session, args):
    """Callback afer xapi initialize"""
    return refresh_all_configurations(session, args, "after_xapi_initialize")


def after_subject_add(session, args):
    """Callback after add subject"""
    return refresh_dynamic_pam(session, args, "after_subject_add")


def after_subject_remove(session, args):
    """Callbackk after remove subject"""
    return refresh_dynamic_pam(session, args, "after_subject_remove")


def after_subject_update(session, args):
    """Callback after subject update"""
    return refresh_dynamic_pam(session, args, "after_subject_update")


def after_roles_update(session, args):
    """Callback after roles update"""
    return refresh_dynamic_pam(session, args, "after_roles_update")


def before_extauth_disable(session, args):
    """Callback before disable external auth"""
    return refresh_all_configurations(session, args, "before_extauth_disable", False)


# The dispatcher
if __name__ == "__main__":
    dispatch_tbl = {
        "after-extauth-enable":   after_extauth_enable,
        "after-xapi-initialize":  after_xapi_initialize,
        "after-subject-add":      after_subject_add,
        "after-subject-update":   after_subject_update,
        "after-subject-remove":   after_subject_remove,
        "after-roles-update":     after_roles_update,
        "before-extauth-disable": before_extauth_disable,
    }
    XenAPIPlugin.dispatch(dispatch_tbl)
