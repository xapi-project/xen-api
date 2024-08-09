#!/usr/bin/env python3
"""
This module provides unittest for perfmon
"""

# pyright: reportAttributeAccessIssue=false

import sys
import math
import unittest
from mock import MagicMock, patch, mock_open
from python3.tests.import_helper import import_file_as_module

# mock modules to avoid dependencies
sys.modules["XenAPI"] = MagicMock()

perfmon = import_file_as_module("python3/bin/perfmon")


@patch("subprocess.getoutput")
class TestGetFsUsage(unittest.TestCase):
    '''Test get_percent_log_fs_usage and get_percent_fs_usage'''
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
        return None

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
        return None

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


class TestGetMemUsage(unittest.TestCase):
    '''Test get_percent_mem_usage '''

    meminfo = '''MemTotal:        2580464 kB
                MemFree:         1511024 kB
                MemAvailable:    2210924 kB
                Buffers:           95948 kB
                Cached:           518164 kB
                SwapCached:            0 kB
                Active:           424468 kB
                Inactive:         390016 kB
                Active(anon):     207944 kB
                Inactive(anon):     8740 kB
                Active(file):     216524 kB
                Inactive(file):   381276 kB
                Unevictable:       13620 kB
                Mlocked:           13620 kB
                SwapTotal:       1048572 kB
                SwapFree:        1048572 kB'''
    @patch("builtins.open", new_callable=mock_open, read_data=meminfo)
    def test_get_percent_mem_usage(self, _):
        self.assertAlmostEqual(perfmon.get_percent_mem_usage([]), 0.17645198692948244)

    @patch('builtins.open', side_effect=Exception)
    def test_get_percent_mem_usage_exception(self, _):
        self.assertEqual(perfmon.get_percent_mem_usage(None), 0.0)


class TestGetPercentSRUsage(unittest.TestCase):
    '''Test get_percent_sr_usage '''

    def test_get_percent_sr_usage_correct_input(self):
        input_list = [100, 200]
        expected_result = 0.5
        self.assertAlmostEqual(perfmon.get_percent_sr_usage(input_list),
                               expected_result)

    def test_get_percent_sr_usage_incorrect_input(self):
        input_list = [100]  # Incorrect input, expecting two values
        expected_result = 0.0
        self.assertAlmostEqual(perfmon.get_percent_sr_usage(input_list),
                               expected_result)

    def test_get_percent_sr_usage_zero_division(self):
        input_list = [0, 200]  # Physical utilization is 0
        expected_result = 0.0
        self.assertAlmostEqual(perfmon.get_percent_sr_usage(input_list),
                               expected_result)

    def test_get_percent_sr_usage_exception_handling(self):
        input_list = ["invalid", 200]  # Invalid input, should raise an exception
        expected_result = 0.0  # Since exception is handled, function should return 0.0
        self.assertAlmostEqual(perfmon.get_percent_sr_usage(input_list),
                               expected_result)


class TestAverage(unittest.TestCase):
    '''Test get_percent_sr_usage '''
    def test_average_empty_list(self):
        result = perfmon.average([])
        self.assertEqual(result, 0.0)

    def test_average_single_element_list(self):
        result = perfmon.average([5])
        self.assertEqual(result, 5.0)

    def test_average_positive_numbers(self):
        result = perfmon.average([1, 2, 3, 4, 5])
        self.assertEqual(result, 3.0)


class TestUpdateAllXMLConfigs(unittest.TestCase):
    '''Test update_all_xmlconfigs'''
    def test_update_all_xmlconfigs(self):

        perfmon.all_xmlconfigs = {}
        perfmon.sruuids_by_hostuuid = {}

        host_uuid = '28a574e4-bf57-4476-a83d-72cba7578d23'
        vm_uuid = '2cf37285-57bc-4633-a24f-0c6c825dda66'
        sr_uuid = '0e7f8fb3-1ba2-4bce-9889-48812273a316'
        perfmon_config = '<config><variable>' \
                    '<name value="fs_usage"/><alarm_trigger_level value="0.9"/>' \
                    '<alarm_trigger_period value="60"/>' \
                    '<alarm_auto_inhibit_period value="3600"/></variable>' \
                    '<variable><name value="mem_usage"/>' \
                    '<alarm_trigger_level value="0.95"/>' \
                    '<alarm_trigger_period value="60"/>' \
                    '<alarm_auto_inhibit_period value="3600"/>' \
                    '</variable><variable><name value="log_fs_usage"/>' \
                    '<alarm_trigger_level value="0.9"/>' \
                    '<alarm_trigger_period value="60"/>' \
                    '<alarm_auto_inhibit_period value="3600"/></variable></config>'

        mock_session = MagicMock()
        mock_session.xenapi.host.get_all_records.return_value = {
            'OpaqueRef:8be06dc8-bed5-4d81-d030-937eca11094a':{
                'uuid': host_uuid,
                'name_label': 'xrtuk-11-43',
                'name_description': 'Default install',
                'memory_overhead': '631816192',
                'software_version': {
                    'product_version': '8.4.0', 'product_version_text': '8',
                    'product_version_text_short': '8', 'platform_name': 'XCP',
                    'platform_version': '3.4.0', 'product_brand': 'XenServer',
                    'build_number': 'stream', 'git_id': '0', 'hostname': 'localhost',
                    'date': '20240229T15:07:05Z', 'dbv': '2024.0229',
                    'is_preview_release': 'false', 'xapi': '24.11',
                    'xapi_build': '24.11.0', 'xen': '4.17.3-4',
                    'linux': '4.19.0+1', 'xencenter_min': '2.21',
                    'xencenter_max': '2.21', 'network_backend': 'openvswitch',
                    'db_schema': '5.775'},
                'other_config': {
                    'iscsi_iqn': 'iqn.2024-03.xenrtcloud:339cd227',
                    'agent_start_time': '1710910331.',
                    'boot_time': '1710910266.',
                    'perfmon': perfmon_config}
             }
            }
        mock_session.xenapi.VM.get_all_records.return_value = {
            'OpaqueRef:fffc65bb-b909-03b2-c20a-8277434a4495': {
                'uuid': vm_uuid,
                'other_config': {
                    'storage_driver_domain': 'OpaqueRef:11de3275-b5e4-a56c-a295',
                    'is_system_domain': 'true', 'perfmon': perfmon_config
                    }
                }
            }
        mock_session.xenapi.SR.get_all_records.return_value = {
            'OpaqueRef:fffc65bb-b909-03b2-c20a-8277434a4495': {
                'uuid': sr_uuid,
                'other_config': {
                    'storage_driver_domain': 'OpaqueRef:11de3275-b5e4-a56c-a295',
                    'is_system_domain': 'true', 'perfmon': perfmon_config
                    },
                'PBDs': ['pbd1', 'pbd2']
                }
            }
        # One SR is connected to two hosts
        mock_session.xenapi.PBD.get_host.return_value = \
            'OpaqueRef:8be06dc8-bed5-4d81-d030-937eca11094a'


        # Call the function to test
        perfmon.update_all_xmlconfigs(mock_session)

        # Check that all_xmlconfigs and sruuids_by_hostuuid were updated correctly
        expect_xmlconfigs = {
            host_uuid: perfmon_config,
            vm_uuid: perfmon_config,
            sr_uuid: perfmon_config
            }
        self.assertEqual(perfmon.all_xmlconfigs, expect_xmlconfigs)
        print(perfmon.sruuids_by_hostuuid)
        self.assertEqual(perfmon.sruuids_by_hostuuid, {host_uuid: {sr_uuid}})

class TestObjectReport(unittest.TestCase):
    '''Test Class ObjectReport '''
    def setUp(self):
        # Create an instance of ObjectReport for testing
        self.obj_report = perfmon.ObjectReport(objtype="vm",
                                       uuid="e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e")

    def test_get_uuid(self):
        self.assertEqual(self.obj_report.get_uuid(),
                         "e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e")

    def test_get_var_names(self):
        # Initially, there are no variables, so the list should be empty
        self.assertEqual(self.obj_report.get_var_names(), [])

        # Insert a variable and check if it appears in the list
        self.obj_report.insert_value("cpu_usage", 0, 0.5)
        self.assertEqual(self.obj_report.get_var_names(), ["cpu_usage"])

    def test_get_value(self):
        # Insert a value for a variable and retrieve it
        self.obj_report.insert_value("cpu_usage", 0, 0.5)
        self.assertEqual(self.obj_report.get_value("cpu_usage", 0), 0.5)

        # Trying to retrieve a value for a non-existing variable should return 0.0
        self.assertEqual(self.obj_report.get_value("memory_usage", 0), 0.0)

    def test_insert_value(self):
        # Insert a value for a variable and check if it's stored correctly
        self.obj_report.insert_value("cpu_usage", 0, 0.5)
        self.assertEqual(self.obj_report.vars["cpu_usage"], [0.5])

        # Insert another value for the same variable and check if it's stored correctly
        self.obj_report.insert_value("cpu_usage", 1, 0.6)
        self.assertEqual(self.obj_report.vars["cpu_usage"], [0.5, 0.6])


@patch("perfmon.XapiSession")
@patch("perfmon.get_percent_fs_usage")
@patch("perfmon.get_percent_log_fs_usage")
@patch("perfmon.get_percent_mem_usage")
class TestVMMonitor(unittest.TestCase):
    '''Test getting VM performance data from VMMonitor'''

    def test_process_rrd_updates(self, mock_get_percent_mem_usage,
                                 mock_get_percent_log_fs_usage,
                                 mock_get_percent_fs_usage,
                                 mock_xapisession):
        uuid = 'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e'
        perfmon.all_xmlconfigs = {'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e':
            '''<config><variable><name value="fs_usage"/>
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
        obj_report.vars = {
            'cpu0': [0.0063071, 0.0048038, 0.0045862, 0.0048865, 0.0048923],
            'cpu1': [0.0067629, 0.0055811, 0.0058988, 0.0058809, 0.0053645],
            'cpu2': [0.0088599, 0.0078701, 0.0058573, 0.0063993, 0.0056833],
            'cpu3': [0.0085826, 0.0056874, 0.005697, 0.0061155, 0.0048769],
            'cpu4': [0.0051265, 0.0045452, 0.0046137, 0.0066399, 0.0050993],
            'cpu5': [0.0062369, 0.0053982, 0.0056624, 0.00606, 0.0062017],
            'cpu6': [0.006235, 0.0041764, 0.0048101, 0.0053798, 0.0050934],
            'cpu7': [0.0050709, 0.005482, 0.0058926, 0.0052934, 0.0049544],
            'memory': [2785000000.0, 2785000000.0, 2785000000.0,
                       2785000000.0, 2785000000.0]
        }
        rrd_updates.report.obj_reports[uuid] = obj_report
        rrd_updates.report.rows = 1
        session = mock_xapisession()

        mock_get_percent_fs_usage.return_value = 0.12
        mock_get_percent_mem_usage.return_value = 0.17380
        mock_get_percent_log_fs_usage.return_value = float("NaN")
        monitor.process_rrd_updates(rrd_updates, session)
        mock_get_percent_fs_usage.assert_called()
        mock_get_percent_log_fs_usage.assert_called()
        mock_get_percent_mem_usage.assert_called()
        self.assertAlmostEqual(monitor.variables[0].value, 0.12)
        self.assertAlmostEqual(monitor.variables[1].value, 0.17380)
        self.assertTrue(math.isnan(monitor.variables[2].value))


class TestHOSTMonitor(unittest.TestCase):
    '''Test getting HOST performance data from HOSTMonitor'''

    @patch("perfmon.XapiSession")
    def test_process_rrd_updates(self, mock_xapisession):
        uuid = 'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e'
        perfmon.all_xmlconfigs = {'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e':
            '''<config><variable><name value="cpu_usage"/>
            <alarm_trigger_level value="0.5"/></variable></config>'''}
        monitor = perfmon.HOSTMonitor(uuid)
        rrd_updates = perfmon.RRDUpdates()
        obj_report = perfmon.ObjectReport("vm", uuid)
        obj_report.vars = {
            'cpu0': [0.0063071, 0.0048038, 0.0045862, 0.0048865, 0.0048923],
            'cpu1': [0.0067629, 0.0055811, 0.0058988, 0.0058809, 0.0053645],
            'cpu2': [0.0088599, 0.0078701, 0.0058573, 0.0063993, 0.0056833],
            'cpu3': [0.0085826, 0.0056874, 0.005697, 0.0061155, 0.0048769],
            'cpu4': [0.0051265, 0.0045452, 0.0046137, 0.0066399, 0.0050993],
            'cpu5': [0.0062369, 0.0053982, 0.0056624, 0.00606, 0.0062017],
            'cpu6': [0.006235, 0.0041764, 0.0048101, 0.0053798, 0.0050934],
            'cpu7': [0.0050709, 0.005482, 0.0058926, 0.0052934, 0.0049544],
            'memory': [2785000000.0, 2785000000.0, 2785000000.0,
                       2785000000.0, 2785000000.0]
        }
        rrd_updates.report.obj_reports[uuid] = obj_report
        rrd_updates.report.rows = 5
        session = mock_xapisession()

        monitor.process_rrd_updates(rrd_updates, session)
        # Average of cpu0-cpu7 (row 5)
        # [0.0048923, 0.0053645, 0.0056833, 0.0048769,
        # 0.0050993, 0.0062017, 0.0050934, 0.0049544]
        self.assertAlmostEqual(monitor.variables[0].value, 0.005270725)

    def test_refresh_config(self):
        perfmon.all_xmlconfigs = {}
        perfmon.sruuids_by_hostuuid = {}

        host_uuid = '28a574e4-bf57-4476-a83d-72cba7578d23'
        sr_uuid = '0e7f8fb3-1ba2-4bce-9889-48812273a316'
        perfmon_config = '<config><variable>' \
                    '<name value="fs_usage"/><alarm_trigger_level value="0.9"/>' \
                    '<alarm_trigger_period value="60"/>' \
                    '<alarm_auto_inhibit_period value="3600"/></variable>' \
                    '<variable><name value="mem_usage"/>' \
                    '<alarm_trigger_level value="0.95"/>' \
                    '<alarm_trigger_period value="60"/>' \
                    '<alarm_auto_inhibit_period value="3600"/>' \
                    '</variable><variable>' \
                    '<name value="sr_io_throughput_total_per_host"/>' \
                    '<alarm_trigger_level value="0.9"/>' \
                    '<alarm_trigger_period value="60"/>' \
                    '<alarm_auto_inhibit_period value="3600"/></variable></config>'

        mock_session = MagicMock()
        mock_session.xenapi.host.get_all_records.return_value = {
            'OpaqueRef:8be06dc8-bed5-4d81-d030-937eca11094a':{
                'uuid': host_uuid,
                'other_config': {
                    'iscsi_iqn': 'iqn.2024-03.xenrtcloud:339cd227',
                    'agent_start_time': '1710910331.',
                    'boot_time': '1710910266.',
                    'perfmon': perfmon_config}
             }
            }
        mock_session.xenapi.SR.get_all_records.return_value = {
            'OpaqueRef:fffc65bb-b909-03b2-c20a-8277434a4495': {
                'uuid': sr_uuid,
                'other_config': {
                    'storage_driver_domain': 'OpaqueRef:11de3275-b5e4-a56c-a295',
                    'is_system_domain': 'true', 'perfmon': perfmon_config
                    },
                'PBDs': ['pbd1', 'pbd2']
                }
            }
        mock_session.xenapi.PBD.get_host.return_value = \
            'OpaqueRef:8be06dc8-bed5-4d81-d030-937eca11094a'
        perfmon.update_all_xmlconfigs(mock_session)
        monitor = perfmon.HOSTMonitor(host_uuid)
        monitor.refresh_config()
        expected_sruuids = {sr_uuid}
        self.assertEqual(set(monitor.secondary_xmlconfigs), expected_sruuids)


@patch("perfmon.XapiSession")
class TestSRMonitor(unittest.TestCase):
    '''Test getting SR performance data from SrMonitor'''
    def test_process_rrd_updates(self, mock_xapisession):
        uuid = 'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e'
        perfmon.all_xmlconfigs = {'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e':
            '''<config><variable><name value="physical_utilisation"/>
            <alarm_trigger_level value="0.8"/></variable></config>'''}
        monitor = perfmon.SRMonitor(uuid)
        rrd_updates = perfmon.RRDUpdates()
        obj_report = perfmon.ObjectReport("vm", uuid)
        obj_report.vars = {
            'size': [100, 200, 300, 400, 500],
            'physical_utilisation': [2000, 3000, 4000, 5000, 6000],
        }
        rrd_updates.report.obj_reports[uuid] = obj_report
        rrd_updates.report.rows = 5
        session = mock_xapisession()

        monitor.process_rrd_updates(rrd_updates, session)
        # get_percent_sr_usage([500, 6000])
        self.assertAlmostEqual(monitor.variables[0].value, 0.08333333333333333)


class TestRRDUpdates(unittest.TestCase):
    '''Test Class RRDUpdates and RRDContentHandler'''

    @patch('time.time', return_value=100000)
    def test_init(self, _):
        rrd_updates = perfmon.RRDUpdates()

        expected_start = 100000 - perfmon.interval
        self.assertEqual(rrd_updates.params['start'], expected_start)
        self.assertEqual(rrd_updates.params["host"], "true")
        self.assertEqual(rrd_updates.params["sr_uuid"], "all")
        self.assertEqual(rrd_updates.params["cf"], "AVERAGE")
        self.assertEqual(rrd_updates.params["interval"], str(perfmon.rrd_step))


    @patch('time.time', return_value=100000)
    @patch("perfmon.XapiSession")
    @patch('urllib.request.urlopen')
    def test_refresh(self, mock_urlopen, mock_xapisession, _):
        rrd_updates = perfmon.RRDUpdates()

        # mock_session
        mock_session = mock_xapisession()
        mock_session.id.return_value = "mocked_session_id"

        # mock  xmlsource
        xml = r'''<xport>
  <meta>
    <start>1213578000</start>
    <step>3600</step>
    <end>1213617600</end>
    <rows>2</rows>
    <columns>12</columns>
    <legend>
      <entry>AVERAGE:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu1</entry>
      <entry>AVERAGE:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu0</entry>
      <entry>AVERAGE:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:memory</entry>
      <entry>MIN:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu1</entry>
      <entry>MIN:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu0</entry>
      <entry>MIN:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:memory</entry>
      <entry>MAX:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu1</entry>
      <entry>MAX:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu0</entry>
      <entry>MAX:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:memory</entry>
      <entry>LAST:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu1</entry>
      <entry>LAST:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:cpu0</entry>
      <entry>LAST:vm:ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3:memory</entry>
    </legend>
  </meta>
  <data>
    <row>
      <t>1213617600</t> # The first row corresponds to end time
      <v>0.0</v>
      <v>0.0282</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0201</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0445</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0243</v>
      <v>209715200.0000</v>
    </row>
    <row>
      <t>1213616600</t> #The last row corresponds to Start time
      <v>0.0</v>
      <v>0.0282</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0201</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0445</v>
      <v>209715200.0000</v>
      <v>0.0</v>
      <v>0.0243</v>
      <v>209715200.0000</v>
    </row>
  </data>
</xport>'''
        xml_rrdupdates = xml.encode(encoding='utf-8')
        cm = MagicMock()
        cm.read.return_value = xml_rrdupdates
        cm.__enter__.return_value = cm
        mock_urlopen.return_value = cm
        rrd_updates.refresh(mock_session)

        # Test __repr__
        print(rrd_updates)

        self.assertEqual(rrd_updates.get_num_rows(), 2)
        self.assertIsNotNone(
            rrd_updates.get_obj_report_by_uuid("ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3")
            )
        self.assertIsNone(
            rrd_updates.get_obj_report_by_uuid("123345")
            )
        self.assertEqual(rrd_updates.get_uuid_list_by_objtype("vm"),
                         ["ecd8d7a0-1be3-4d91-bd0e-4888c0e30ab3"])


class TestVariable(unittest.TestCase):
    '''Test Class Varible'''

    def test_set_active(self):
        # Construct varible node for VaribleConfig
        # Not used, just for input
        xmlconfig = b'<config><variable><name value="cpu_usage"/>' \
            b'<alarm_trigger_level value="0.5"/></variable></config>'
        xmldoc = perfmon.minidom.parseString(xmlconfig)
        variable_nodes = xmldoc.getElementsByTagName("variable")
        node = variable_nodes[0]

        # Construct function alarm_create and mock_get_default_varible_config
        # Not used, just for input
        uuid = 'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e'
        monitor = perfmon.VMMonitor(uuid)
        var = perfmon.Variable(node, monitor.alarm_create,
                               monitor.get_default_variable_config)

        # Call set_active with active=True
        var.set_active(True)
        self.assertTrue(var.active)

        # Call set_active with active=False
        var.set_active(False)
        self.assertFalse(var.active)

    @patch("perfmon.XapiSession")
    def test_update(self, mock_xapisession):
        xmlconfig = b'<config><variable><name value="fs_usage"/>' \
            b'<alarm_trigger_level value="0.9"/>' \
            b'<alarm_trigger_period value="60"/>' \
            b'<alarm_auto_inhibit_period value="3600"/>' \
            b'</variable><variable><name value="mem_usage"/>' \
            b'<alarm_trigger_level value="0.95"/>' \
            b'<alarm_trigger_period value="60"/>' \
            b'<alarm_auto_inhibit_period value="3600"/>' \
            b'</variable><variable><name value="log_fs_usage"/>' \
            b'<alarm_trigger_level value="0.9"/>' \
            b'<alarm_trigger_period value="60"/>' \
            b'<alarm_auto_inhibit_period value="3600"/>' \
            b'</variable></config>'
        xmldoc = perfmon.minidom.parseString(xmlconfig)
        variable_nodes = xmldoc.getElementsByTagName("variable")
        node = variable_nodes[0]

        uuid = 'e1ae3f5d-4c8b-4575-bbb8-2af7e8a2c31e'
        monitor = perfmon.VMMonitor(uuid)
        var = perfmon.Variable(node, monitor.alarm_create,
                               monitor.get_default_variable_config)

        session = mock_xapisession()

        # Trigger alarm
        var.trigger_down_counter = 50
        var.update(0.95,session)
        self.assertEqual(var.trigger_down_counter, 60)

        # Not trigger alarm - time isn't up
        var.trigger_down_counter = 100
        var.update(0.95,session)
        self.assertEqual(var.trigger_down_counter, 40)

        # Not trigger alarm - level good
        var.trigger_down_counter = 50
        var.update(0.8,session)
        self.assertEqual(var.trigger_down_counter, 60)

if __name__ == '__main__':
    unittest.main()
