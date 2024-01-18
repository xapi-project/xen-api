#!/usr/bin/env python3
#
# unittest for perfmon

import unittest
from mock import MagicMock, patch
import sys
import math
from import_file import get_module

# mock modules to avoid dependencies
sys.modules["XenAPI"] = MagicMock()

perfmon = get_module("perfmon", "/perfmon")
@unittest.skipIf(sys.version_info < (3, 0), reason="requires python3")
@patch("subprocess.getoutput")
class TestGetPercentage(unittest.TestCase):
    def mock_subprocess_getoutput(self, cmd):
        df_etc_passwd = r"""Filesystem     1K-blocks    Used Available Use% Mounted on
        /dev/sda1       18402132 2244748  15213668  13% /
        """
        df_var_log = r"""Filesystem     1K-blocks  Used Available Use% Mounted on
        /dev/sda5        4054752 59820   3785220   2% /var/log
        """
        if cmd == "df /etc/passwd":
            return df_etc_passwd
        if cmd == "df /var/log":
            return df_var_log
        
    def mock_subprocess_getoutput_same_file_system(self, cmd):
        df_etc_passwd = r"""Filesystem     1K-blocks    Used Available Use% Mounted on
        /dev/sda5       18402132 2244748  15213668  13% /
        """
        df_var_log = r"""Filesystem     1K-blocks  Used Available Use% Mounted on
        /dev/sda5        4054752 59820   3785220   2% /var/log
        """
        if cmd == "df /etc/passwd":
            return df_etc_passwd
        if cmd == "df /var/log":
            return df_var_log

    def test_get_percent_log_fs_usage(self, mock_getoutput):
        """Assert that get_percent_log_fs_usage returns as expected"""
        mock_getoutput.side_effect = self.mock_subprocess_getoutput

        expected_percentage = 0.02
        test_percentage = perfmon.get_percent_log_fs_usage(None)
        self.assertAlmostEqual(test_percentage, expected_percentage, 7)
        
    def test_get_percent_log_fs_usage_same_file_system(self, mock_getoutput):
        """Test where /etc/passwd and /var/log are in the same filesystem"""
        mock_getoutput.side_effect = self.mock_subprocess_getoutput_same_file_system

        test_percentage = perfmon.get_percent_log_fs_usage(None)
        self.assertTrue(math.isnan(test_percentage))
        
    def test_get_percent_fs_usage(self, mock_getoutput):
        """Assert that get_percent_fs_usage returns as expected"""
        mock_getoutput.side_effect = self.mock_subprocess_getoutput

        expected_percentage = 0.13
        test_percentage = perfmon.get_percent_fs_usage(None)
        self.assertAlmostEqual(test_percentage, expected_percentage, 7)
@unittest.skipIf(sys.version_info < (3, 0), reason="requires python3")
@patch("perfmon.Variable.update")
@patch("perfmon.get_percent_fs_usage")
@patch("perfmon.get_percent_log_fs_usage")
@patch("perfmon.get_percent_mem_usage")
class TestVMMonitor(unittest.TestCase):   
    def test_process_rrd_updates(self, mock_get_percent_fs_usage, mock_get_percent_log_fs_usage, 
                                 mock_get_percent_mem_usage, mock_update):
        uuid = 'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e'
        perfmon.all_xmlconfigs = {'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e': '''<config><variable><name value="fs_usage"/>
                                                                            <alarm_trigger_level value="0.9"/>
                                                                            <alarm_trigger_period value="60"/>
                                                                            <alarm_auto_inhibit_period value="3600"/>
                                                                            </variable><variable><name value="mem_usage"/>
                                                                            <alarm_trigger_level value="0.95"/>
                                                                            <alarm_trigger_period value="60"/>
                                                                            <alarm_auto_inhibit_period value="3600"/>
                                                                            </variable><variable><name value="log_fs_usage"/>
                                                                            <alarm_trigger_level value="0.9"/>
                                                                            <alarm_trigger_period value="60"/>
                                                                            <alarm_auto_inhibit_period value="3600"/>
                                                                            </variable></config>'''}
        monitor = perfmon.VMMonitor(uuid)
        rrd_updates = perfmon.RRDUpdates()
        obj_report = perfmon.ObjectReport("vm", uuid)
        obj_report.vars = {'cpu0': [0.0063071, 0.0048038, 0.0045862, 0.0048865, 0.0048923],
                           'cpu1': [0.0067629, 0.0055811, 0.0058988, 0.0058809, 0.0053645], 
                           'cpu2': [0.0088599, 0.0078701, 0.0058573, 0.0063993, 0.0056833], 
                           'cpu3': [0.0085826, 0.0056874, 0.005697, 0.0061155, 0.0048769], 
                           'cpu4': [0.0051265, 0.0045452, 0.0046137, 0.0066399, 0.0050993], 
                           'cpu5': [0.0062369, 0.0053982, 0.0056624, 0.00606, 0.0062017], 
                           'cpu6': [0.006235, 0.0041764, 0.0048101, 0.0053798, 0.0050934], 
                           'cpu7': [0.0050709, 0.005482, 0.0058926, 0.0052934, 0.0049544], 
                           'memory': [2785000000.0, 2785000000.0, 2785000000.0, 2785000000.0, 2785000000.0]}
        rrd_updates.report.obj_reports[uuid] = obj_report
        rrd_updates.report.rows = 5  
        session = perfmon.XapiSession()
        
        mock_get_percent_fs_usage.return_value = 0.12
        mock_get_percent_log_fs_usage.return_value = float("NaN")
        mock_get_percent_mem_usage.return_value = 0.17380
        monitor.process_rrd_updates(rrd_updates, session)
        mock_get_percent_fs_usage.assert_called()
        mock_get_percent_log_fs_usage.assert_called()
        mock_get_percent_mem_usage.assert_called()
        mock_update.assert_called()
       
