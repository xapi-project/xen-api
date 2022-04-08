"""
Test module for extauth_hook_ad
"""
#pylint: disable=invalid-name
import sys
import os
from unittest import TestCase
from mock import MagicMock, patch
# mock modules to avoid dependencies
sys.modules["XenAPIPlugin"] = MagicMock()
sys.modules["XenAPI"] = MagicMock()
# pylint: disable=wrong-import-position
# Import must after mock modules
from extauth_hook_ad import StaticSSHPam, NssConfig, SshdConfig, UsersList, GroupsList


def line_exists_in_config(lines, line):
    """
    Helper function to detect whether configration match expectation
    """
    return any(line.split() == l.split() for l in lines)


domain = "conappada.local"
args_bd_winbind = {'auth_type': 'AD',
                   'service_name': domain, 'ad_backend': 'winbind'}
args_bd_pbis = {'auth_type': 'AD',
                'service_name': domain, 'ad_backend': 'pbis'}
mock_session = MagicMock()

subjects = ['OpaqueRef:96ae4be5-8815-4de8-a40f-d5e5c531dda9']
mock_session.xenapi.subject.get_all.return_value = subjects
admin_role = 'OpaqueRef:0165f154-ba3e-034e-6b27-5d271af109ba'
admin_roles = [admin_role]
mock_session.xenapi.role.get_by_name_label.return_value = admin_roles

# pylint: disable=unused-argument, protected-access, redefined-outer-name, missing-function-docstring
# pylint: disable=too-many-arguments, missing-class-docstring, no-self-use


def build_user(domain_netbios, domain, name, is_admin=True):
    return {
        'subject_identifier': 'S-1-5-21-3143668282-2591278241-912959342-1179',
        'other_config': {
            'subject-password-expired': 'FALSE',
            'subject-gecos': name,
            'subject-name': '{}\\{}'.format(domain_netbios, name),
            'subject-account-disabled': 'FALSE',
            'subject-account-locked': 'FALSE',
            'subject-is-group': 'false',
            'subject-account-expired': 'FALSE',
            'subject-sid': 'S-1-5-21-3143668282-2591278241-912959342-1179',
            'subject-uid': '1659372699',
            'subject-displayname': name,
            'subject-gid': '1659372033',
            'subject-upn': '{}@{}'.format(name, domain)
        },
        'uuid': '684c868e-cf6a-2311-570d-b6d082443e40',
        'roles': [admin_role] if is_admin else []
    }


def build_group(domain, name, is_admin):
    return {
        'subject_identifier': 'S-1-5-21-3143668282-2591278241-912959342-1174',
        'other_config': {
            'subject-name': '{}\\{}'.format(domain, name),
            'subject-sid': 'S-1-5-21-3143668282-2591278241-912959342-1174',
            'subject-gid': '1659372694',
            'subject-is-group': 'true'
        },
        'uuid': '469d8887-7054-ae4c-524a-8063410552f5',
        'roles': [admin_role] if is_admin else []
    }


def mock_rename_to_clean(src, dest):
    """"Some unittest do create temporary files, this mock function remove the temporary files"""
    os.remove(src)


@patch("os.chmod")
@patch("os.rename")
class TestStaicPamConfig(TestCase):
    def test_ad_not_enabled(self, mock_rename, mock_chmod):
        # No hcp_users file should be included
        mock_rename.side_effect = mock_rename_to_clean
        static = StaticSSHPam(mock_session, args_bd_winbind, ad_enabled=False)
        static.apply()
        enabled_keyward = "account     include       hcp_users"
        self.assertFalse(line_exists_in_config(static._lines, enabled_keyward))

    def test_ad_enabled_with_winbind(self, mock_rename, mock_chmod):
        # pam_winbind should be used
        mock_rename.side_effect = mock_rename_to_clean
        static = StaticSSHPam(mock_session, args_bd_winbind)
        static.apply()
        enabled_keyward = "auth sufficient    pam_winbind.so try_first_pass try_authtok"
        self.assertTrue(line_exists_in_config(static._lines, enabled_keyward))

    def test_ad_enabled_with_pbis(self, mock_rename, mock_chmod):
        # pam_lsass should be used
        mock_rename.side_effect = mock_rename_to_clean
        static = StaticSSHPam(mock_session, args_bd_pbis)
        static.apply()
        enabled_keyward = "auth sufficient /lib/security/pam_lsass.so try_first_pass try_authtok"
        self.assertTrue(line_exists_in_config(static._lines, enabled_keyward))


@patch("extauth_hook_ad.ADConfig._install")
class TestUsersList(TestCase):
    @patch("extauth_hook_ad.open")
    @patch("os.path.exists")
    @patch("os.remove")
    def test_ad_not_enabled(self, mock_remove, mock_exists, mock_open, mock_install):
        # dynamic pam file should be removed
        mock_exists.return_value = True
        dynamic = UsersList(mock_session, args_bd_winbind, ad_enabled=False)
        dynamic.apply()
        mock_remove.assert_called()
        mock_install.assert_not_called()

    def test_permit_admin_user(self, mock_install):
        # Domain user with admin role should be included in config file
        user = build_user("CONNAPP", "CONAPPADA.LOCAL", "radmin", True)
        mock_session.xenapi.subject.get_record.return_value = user
        dynamic = UsersList(mock_session, args_bd_pbis)
        dynamic.apply()
        self.assertIn(r"CONNAPP\radmin", dynamic._lines)
        self.assertIn(r"radmin@conappada.local", dynamic._lines)
        mock_install.assert_called()

    def test_pbis_permit_admin_user_with_space(self, mock_install):
        # Domain user name with space should be repalced by "+" with PBIS
        user = build_user("CONNAPP", "conappada.local", "radmin  l1", True)
        mock_session.xenapi.subject.get_record.return_value = user
        permit_user = r"CONNAPP\radmin++l1"
        dynamic = UsersList(mock_session, args_bd_pbis)
        dynamic.apply()
        self.assertIn(permit_user, dynamic._lines)
        mock_install.assert_called()

    def test_winbind_permit_admin_user_with_space(self, mock_install):
        # Domain user name with space should be surrounded by [] with winbind
        user = build_user("CONNAPP", "conappada.local", "radmin  l1", True)
        mock_session.xenapi.subject.get_record.return_value = user
        permit_user = r"CONNAPP\radmin  l1"
        dynamic = UsersList(mock_session, args_bd_winbind)
        dynamic.apply()
        self.assertIn(permit_user, dynamic._lines)
        mock_install.assert_called()

    def test_not_permit_non_admin_user(self, mock_install):
        # Domain user without admin role should be included in config file
        user = build_user("CONNAPP", "conappada.local", "radmin", False)
        mock_session.xenapi.subject.get_record.return_value = user
        permit_user = r"CONNAPP\radmin"
        dynamic = UsersList(mock_session, args_bd_winbind)
        dynamic.apply()
        self.assertNotIn(permit_user, dynamic._lines)

    def test_pbis_not_permit_pool_admin_with_plus_in_name(self, mock_install):
        """
        Domain user name should not contain "+"
        """
        user = build_user("CONNAPP", "conappada.local", "radm+in", True)
        mock_session.xenapi.subject.get_record.return_value = user
        permit_user = r"CONNAPP\radm+in"
        dynamic = UsersList(mock_session, args_bd_pbis)
        dynamic.apply()
        self.assertNotIn(permit_user, dynamic._lines)

    def test_failed_to_add_one_admin_should_not_affact_others(self, mock_install):
        """
        Failed to add one bad domain users should not affact others
        """
        bad_user = build_user("CONNAPP", "conappada.local", "bad+in", True)
        good_user = build_user("CONNAPP", "conappada.local", "good", True)

        mock_session_with_multi_users = MagicMock()

        subjects = ['OpaqueRef:96ae4be5-8815-4de8-a40f-d5e5c531dda9',
                    'OpaqueRef:96ae4be5-8815-4de8-a40f-d5e5c531dda1']
        mock_session_with_multi_users.xenapi.subject.get_all.return_value = subjects
        mock_session_with_multi_users.xenapi.subject.get_record.side_effect = [
            bad_user, good_user]
        mock_session_with_multi_users.xenapi.role.get_by_name_label.return_value = admin_roles

        bad_user = r"CONNAPP\bad+in"
        good_user = r"CONNAPP\good"
        dynamic = UsersList(mock_session_with_multi_users, args_bd_pbis)
        dynamic.apply()
        self.assertIn(good_user, dynamic._lines)
        self.assertNotIn(bad_user, dynamic._lines)


@patch("extauth_hook_ad.ADConfig._install")
class TestGroups(TestCase):
    def test_permit_admin_group(self, mock_install):
        # Domain group with admin role should be included in config file
        group = build_group("CONNAPP", "test_group", True)
        mock_session.xenapi.subject.get_record.return_value = group
        permit_group = r"CONNAPP\test_group"
        dynamic = GroupsList(mock_session, args_bd_winbind)
        dynamic.apply()
        self.assertIn(permit_group, dynamic._lines)

    def test_not_permit_non_admin_group(self, mock_install):
        # Domain group without admin role should not be included in config file
        group = build_group("CONNAPP", "test_group", False)
        mock_session.xenapi.subject.get_record.return_value = group
        bad_group = r"CONNAPP\test_group"
        dynamic = GroupsList(mock_session, args_bd_winbind)
        dynamic.apply()
        self.assertNotIn(bad_group, dynamic._lines)

    def test_permit_admin_group_with_space(self, mock_install):
        # Domain group name with space should be included in config file
        group = build_group("CONNAPP", "test group", True)
        mock_session.xenapi.subject.get_record.return_value = group
        permit_group = r"CONNAPP\test group"
        dynamic = GroupsList(mock_session, args_bd_winbind)
        dynamic.apply()
        self.assertIn(permit_group, dynamic._lines)


@patch("extauth_hook_ad.ADConfig._install")
class TestNssConfig(TestCase):
    def test_ad_not_enabled(self, mock_install):
        expected_config = "passwd:  files sss"
        nss = NssConfig(mock_session, args_bd_winbind, False)
        nss.apply()
        self.assertTrue(line_exists_in_config(nss._lines, expected_config))

    def test_ad_enabled(self, mock_install):
        expected_config = "passwd: files hcp winbind"
        nss = NssConfig(mock_session, args_bd_winbind, True)
        nss.apply()
        self.assertTrue(line_exists_in_config(nss._lines, expected_config))


@patch("extauth_hook_ad.run_cmd")
@patch("extauth_hook_ad.ADConfig._install")
@patch("extauth_hook_ad.open")
class TestSshdConfig(TestCase):
    def test_ad_not_enabled(self, mock_open, mock_install, mock_run_cmd):
        expected_config = "ChallengeResponseAuthentication no"
        # mock empty file exists
        mock_open.return_value.__enter__.return_value.readlines.return_value = []
        sshd = SshdConfig(mock_session, args_bd_winbind, False)
        sshd.apply()
        self.assertTrue(line_exists_in_config(sshd._lines, expected_config))
        mock_run_cmd.assert_called()

    def test_ad_enabled(self, mock_open, mock_install, mock_run_cmd):
        expected_config = "ChallengeResponseAuthentication yes"
        # mock empty file exists
        mock_open.return_value.__enter__.return_value.readlines.return_value = []
        sshd = SshdConfig(mock_session, args_bd_winbind, True)
        sshd.apply()
        self.assertTrue(line_exists_in_config(sshd._lines, expected_config))
        mock_run_cmd.assert_called()
