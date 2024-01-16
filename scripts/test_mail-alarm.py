#
# test_mail-alarm.py: uses unittest to test script "mail-alarm"
#

import tempfile
import os
import shutil
import sys
import unittest
from scripts.unit_tests.import_helper import import_file_as_module

import mock

def nottest(obj):
    obj.__test__ = False
    return obj

sys.path.append("./scripts/examples/python")
sys.modules["xcp"] = mock.Mock()

log_file_global = None

XML_MESSAGE_TEMPLATE = """<?xml version="1.0" encoding="UTF-8"?>
<message><generation>63102</generation><ref>OpaqueRef:46be74f4-3a26-31a8-a629-d52584fe6ed3</ref><name>{alarm}</name><priority>3</priority><cls>{cls}</cls><obj_uuid>2e00443d-ac29-4940-8433-a15dda1e8f8e</obj_uuid><timestamp>20170516T16:30:00Z</timestamp><uuid>0d985f5e-6d91-3410-f853-040d0906a4b9</uuid><body>{body}</body></message>"""

XML_BODY_COMMON = """value: 0.128963
config:
&lt;variable&gt;
    &lt;name value=&quot;{name}&quot;/&gt;
    &lt;alarm_trigger_level value=&quot;0.05&quot;/&gt;
    &lt;alarm_trigger_period value=&quot;60&quot;/&gt;
    &lt;alarm_auto_inhibit_period value=&quot;300&quot;/&gt;
&lt;/variable&gt;
"""

test_vm_obj = {"name_label": "test_VM", "is_control_domain": False}

def mock_setup(module):
    # Emulate functions with Mock
    module.log_err = log_err
    module.get_pool_name = mock.Mock(return_value="test_pool")
    module.get_sr_name_by_uuid = mock.Mock(return_value="test_sr")
    module.get_VM_params = mock.Mock(return_value=test_vm_obj)
    module.get_host_params = mock.Mock(return_value={"name_label": "test_host"})
    module.mail_language_pack_path = "./scripts/mail-languages"
    module.branding.PRODUCT_BRAND = "XenServer"
    module.branding.BRAND_CONSOLE = "XenCenter"


def get_alarm_xml(xmlalarm_str, xmlcls_str, xmlname_str, xmlbody_str):
    return XML_MESSAGE_TEMPLATE.format(
        alarm=xmlalarm_str, cls=xmlcls_str, body=xmlbody_str.format(name=xmlname_str)
    )


def log_err(err):
    global log_file_global
    with open(log_file_global, "a+") as fileh:
        fileh.write("%s: %s\n" % (sys.argv[0], err))


class TestXapiMessage(unittest.TestCase):
    def setUp(self):
        global log_file_global
        try:
            self.work_dir = tempfile.mkdtemp(prefix="test-mail-alarm-")
            log_file_global = os.path.join(self.work_dir, "user.log")
            self.mail_alarm = import_file_as_module("scripts/mail-alarm")
        except:
            raise

    def tearDown(self):
        shutil.rmtree(self.work_dir, ignore_errors=True)

    @nottest
    def common_test_good_input(
        self,
        xmlalarm_str,
        xmlcls_str,
        xmlname_str,
        subject_str,
        body_str,
        xmlbody_str=XML_BODY_COMMON,
    ):
        mailalarm = self.mail_alarm

        # Emulate functions with Mock
        mock_setup(mailalarm)

        session = mock.Mock()

        tst_xml = get_alarm_xml(xmlalarm_str, xmlcls_str, xmlname_str, xmlbody_str)
        obj_XapiMessage = mailalarm.XapiMessage(tst_xml, "en-US", session)
        self.assertIsInstance(obj_XapiMessage, mailalarm.XapiMessage)

        mail_subject = obj_XapiMessage.generate_email_subject()
        mail_body = obj_XapiMessage.generate_email_body()

        self.assertIn(subject_str, mail_subject)
        self.assertIn(body_str, mail_body)

    @nottest
    def common_test_bad_input(
        self,
        xmlalarm_str,
        xmlcls_str,
        xmlname_str,
        title_str,
        subtitle_str,
        xmlbody_str=XML_BODY_COMMON,
    ):
        global log_file_global
        mailalarm = self.mail_alarm

        # Emulate functions with Mock
        mock_setup(mailalarm)

        session = mock.Mock()

        tst_xml = get_alarm_xml(xmlalarm_str, xmlcls_str, xmlname_str, xmlbody_str)
        obj_XapiMessage = mailalarm.XapiMessage(tst_xml, "en-USzzz", session)
        self.assertIsInstance(obj_XapiMessage, mailalarm.XapiMessage)

        mail_subject = obj_XapiMessage.generate_email_subject()
        mail_body = obj_XapiMessage.generate_email_body()

        with open(log_file_global, "r") as fileh:
            log_strs = fileh.read()

        self.assertIn("Read mail language pack error", log_strs)
        self.assertIn(
            "[{title}] error in looking for ({subtitle}:subject). Mail language pack:(None)".format(
                title=title_str, subtitle=subtitle_str
            ),
            log_strs,
        )
        self.assertIn(
            "[{title}] error in looking for ({subtitle}:body). Mail language pack:(None)".format(
                title=title_str, subtitle=subtitle_str
            ),
            log_strs,
        )

        os.remove(log_file_global)

    def test_good_mail_language(self):
        ## Test cpu_usage alarm
        cpu_usage_alarm_subject = '[test_pool] XenServer Alarm: CPU usage on {cls} "{name_label}"'
        cpu_usage_alarm_body = 'CPU usage on {cls} "{name_label}" has been on average 12.9% for the last 60 seconds.\nThis alarm is set to be triggered when CPU usage is more than 5.0%.\n\nFor Alarm Settings, please log into your XenCenter Console and click on "{cls_name}"->\n"Properties"->"Alerts"\n'

        # test Host
        mail_subject_expected = cpu_usage_alarm_subject.format(cls='Host', name_label='test_host')
        mail_body_expected = cpu_usage_alarm_body.format(cls='Host', name_label='test_host', cls_name='Server')
        self.common_test_good_input(
            "ALARM", "Host", "cpu_usage", mail_subject_expected, mail_body_expected
        )

        # test common VM
        mail_subject_expected = cpu_usage_alarm_subject.format(cls='VM', name_label='test_VM')
        mail_body_expected = cpu_usage_alarm_body.format(cls='VM', name_label='test_VM', cls_name='VM')
        self.common_test_good_input(
            "ALARM", "VM", "cpu_usage", mail_subject_expected, mail_body_expected
        )

        # test dom0 VM
        test_vm_obj['is_control_domain'] = True
        mail_subject_expected = cpu_usage_alarm_subject.format(cls='VM', name_label='test_VM')
        mail_body_expected = cpu_usage_alarm_body.format(cls='VM', name_label='test_VM', cls_name='Server')
        self.common_test_good_input(
            "ALARM", "VM", "cpu_usage", mail_subject_expected, mail_body_expected
        )
        test_vm_obj['is_control_domain'] = False

        # Test network_usage alarm
        mail_subject_expected = (
            '[test_pool] XenServer Alarm: Network usage on Host "test_host"'
        )
        mail_body_expected = 'Network usage on Host "test_host" has been on average 0 B/s for the last 60 seconds.\nThis alarm is set to be triggered when Network usage is more than 0 B/s.\n\nFor Alarm Settings, please log into your XenCenter Console and click on "Server"->\n"Properties"->"Alerts"'
        self.common_test_good_input(
            "ALARM", "Host", "network_usage", mail_subject_expected, mail_body_expected
        )

        memory_usage_alarm_subject = '[test_pool] XenServer Alarm: Memory usage on {cls} "{name_label}"'
        memory_usage_alarm_body = 'Free memory on {cls} "{name_label}" has been on average 0 KiB for the last 60 seconds.\nThis alarm is set to be triggered when free memory is less than 0 KiB.\n\nFor Alarm Settings, please log into your XenCenter Console and click on "{cls_name}"->\n"Properties"->"Alerts"'

        ## Test memory_free_kib alarm for Host
        mail_subject_expected = memory_usage_alarm_subject.format(cls='Host', name_label='test_host')
        mail_body_expected = memory_usage_alarm_body.format(cls='Host', name_label='test_host', cls_name='Server')
        self.common_test_good_input(
            "ALARM", "Host", "memory_free_kib", mail_subject_expected, mail_body_expected,
        )

        ## Test memory_internal_free alarm for VM
        # test common VM
        mail_subject_expected = memory_usage_alarm_subject.format(cls='VM', name_label='test_VM')
        mail_body_expected = memory_usage_alarm_body.format(cls='VM', name_label='test_VM', cls_name='VM')
        self.common_test_good_input(
            "ALARM", "VM", "memory_internal_free", mail_subject_expected, mail_body_expected,
        )

        # test dom0 VM
        test_vm_obj['is_control_domain'] = True
        mail_subject_expected = memory_usage_alarm_subject.format(cls='VM', name_label='test_VM')
        mail_body_expected = memory_usage_alarm_body.format(cls='VM', name_label='test_VM', cls_name='Server')
        self.common_test_good_input(
            "ALARM", "VM", "memory_internal_free", mail_subject_expected, mail_body_expected,
        )
        test_vm_obj['is_control_domain'] = False

        # Test disk_usage alarm
        mail_subject_expected = (
            '[test_pool] XenServer Alarm: Disk usage on VM "test_VM"'
        )
        mail_body_expected = 'Disk usage on VM "test_VM" has been on average 0 B/s for the last 60 seconds.\nThis alarm is set to be triggered when Disk usage is more than 0 B/s.\n\nFor Alarm Settings, please log into your XenCenter Console and click on "VM"->\n"Properties"->"Alerts"'
        self.common_test_good_input(
            "ALARM", "VM", "disk_usage", mail_subject_expected, mail_body_expected
        )

        # Test fs_usage alarm
        mail_subject_expected = (
            '[test_pool] XenServer Alarm: Filesystem nearly full on "test_VM"'
        )
        mail_body_expected = 'The filesystem usage on "test_VM" is at 12.9%.\nThis alarm is set to be triggered when filesystem usage is more than 5.0%.'
        self.common_test_good_input(
            "ALARM", "VM", "fs_usage", mail_subject_expected, mail_body_expected
        )

        # Test log_fs_usage alarm
        mail_subject_expected = (
            '[test_pool] XenServer Alarm: Log partition nearly full on "test_VM"'
        )
        mail_body_expected = 'The log partition usage on "test_VM" is at 12.9%.\nThis alarm is set to be triggered when log partition usage is more than 5.0%.'
        self.common_test_good_input(
            "ALARM", "VM", "log_fs_usage", mail_subject_expected, mail_body_expected
        )

        # Test mem_usage alarm
        mail_subject_expected = (
            '[test_pool] XenServer Alarm: Dom0 memory demand is high on "test_VM"'
        )
        mail_body_expected = 'The memory required by the control domain on "test_VM" is about 12.9% of its allocated memory. Occasional performance degradation can be expected when memory swapping is forced to happen.\nThis alarm is set to be triggered when the memory required by the control domain is above 5.0% of its allocated memory.'
        self.common_test_good_input(
            "ALARM", "VM", "mem_usage", mail_subject_expected, mail_body_expected
        )

        # Test sr_io_throughput_total_11111111 alarm
        mail_subject_expected = (
            '[test_pool] Storage Throughput Alarm: The total IO throughput on "test_sr"'
        )
        mail_body_expected = 'The total read and write throughput of server "test_host" on storage repository "test_sr" has been 0.1 MB/s for the last 60 seconds.\nThis alarm is set to be triggered when the total throughput exceeds 51 KB/s.\n\nFor Alarm Settings, please log into your XenCenter Console and click on "Storage"->\n"Properties"->"Alerts"'
        self.common_test_good_input(
            "ALARM",
            "Host",
            "sr_io_throughput_total_11111111",
            mail_subject_expected,
            mail_body_expected,
        )

        # Test HA_HOST_FAILED
        mail_subject_expected = "[test_pool] XenServer HA Alarm: HA failed"
        mail_body_expected = "HA failed\n\nThis alarm is set to be triggered when a host belonging to a high availability pool fails."
        xmlbody_str = "HA failed"
        self.common_test_good_input(
            "HA_HOST_FAILED",
            "Host",
            "",
            mail_subject_expected,
            mail_body_expected,
            xmlbody_str,
        )

        # Test WLB_CONSULTATION_FAILED
        mail_subject_expected = '[test_pool] XenServer Alarm: Attempt to consult WLB for VM "test_VM" failed'
        mail_body_expected = "A workload balancing consultation for VM test_VM failed.\nThe operation was completed using the default algorithm instead of a workload balancing recommendation."
        xmlbody_str = "WLB consultation failed"
        self.common_test_good_input(
            "WLB_CONSULTATION_FAILED",
            "VM",
            "",
            mail_subject_expected,
            mail_body_expected,
            xmlbody_str,
        )

        # Test WLB_OPTIMIZATION_ALERT
        mail_subject_expected = (
            "Workload Balancing Alert: Optimization alert from pool test_pool"
        )
        mail_body_expected = "The Workload Balancing Server has reported that pool test_pool is in need of optimization.\ntest_pool is in optimization mode MaxPerformance and is in a Critical state."
        xmlbody_str = "severity:Critical mode:MaxPerformance"
        self.common_test_good_input(
            "WLB_OPTIMIZATION_ALERT",
            "Host",
            "",
            mail_subject_expected,
            mail_body_expected,
            xmlbody_str,
        )

    def test_bad_mail_language(self):
        # Test cpu_usage alarm
        title_expected = "CpuUsageAlarmETG"
        subtitle_expected = "cpu_usage_alarm"
        self.common_test_bad_input(
            "ALARM", "Host", "cpu_usage", title_expected, subtitle_expected
        )
        self.common_test_bad_input(
            "ALARM", "VM", "cpu_usage", title_expected, subtitle_expected
        )

        # Test network_usage alarm
        title_expected = "NetworkUsageAlarmETG"
        subtitle_expected = "network_usage_alarm"
        self.common_test_bad_input(
            "ALARM", "Host", "network_usage", title_expected, subtitle_expected
        )

        # Test memory_free_kib alarm
        title_expected = "MemoryUsageAlarmETG"
        subtitle_expected = "memory_usage_alarm"
        self.common_test_bad_input(
            "ALARM", "Host", "memory_free_kib", title_expected, subtitle_expected
        )

        # Test memory_internal_free alarm
        title_expected = "MemoryUsageAlarmETG"
        subtitle_expected = "memory_usage_alarm"
        self.common_test_bad_input(
            "ALARM", "VM", "memory_internal_free", title_expected, subtitle_expected
        )

        # Test disk_usage alarm
        title_expected = "DiskUsageAlarmETG"
        subtitle_expected = "disk_usage_alarm"
        self.common_test_bad_input(
            "ALARM", "VM", "disk_usage", title_expected, subtitle_expected
        )

        # Test fs_usage alarm
        title_expected = "Dom0FSUsageAlarmETG"
        subtitle_expected = "dom0fs_usage_alarm"
        self.common_test_bad_input(
            "ALARM", "VM", "fs_usage", title_expected, subtitle_expected
        )

        # Test log_fs_usage alarm
        title_expected = "Dom0LogFSUsageAlarmETG"
        subtitle_expected = "dom0logfs_usage_alarm"
        self.common_test_bad_input(
            "ALARM", "VM", "log_fs_usage", title_expected, subtitle_expected
        )

        # Test mem_usage alarm
        title_expected = "Dom0MemUsageAlarmETG"
        subtitle_expected = "dom0memory_usage_alarm"
        self.common_test_bad_input(
            "ALARM", "VM", "mem_usage", title_expected, subtitle_expected
        )

        # Test sr_io_throughput_total_11111111 alarm
        title_expected = "SRIOThroughputTotalAlertETG"
        subtitle_expected = "sr_io_throughput_total_alert"
        self.common_test_bad_input(
            "ALARM",
            "Host",
            "sr_io_throughput_total_11111111",
            title_expected,
            subtitle_expected,
        )

        # Test HA_HOST_FAILED
        title_expected = "HAHostFailedETG"
        subtitle_expected = "ha_host_failed"
        xmlbody_str = "HA failed"
        self.common_test_bad_input(
            "HA_HOST_FAILED", "Host", "", title_expected, subtitle_expected, xmlbody_str
        )

        # Test WLB_CONSULTATION_FAILED
        title_expected = "WlbConsultationFailure"
        subtitle_expected = "wlb_consultation_failure"
        xmlbody_str = "WLB consultation failed"
        self.common_test_bad_input(
            "WLB_CONSULTATION_FAILED",
            "VM",
            "",
            title_expected,
            subtitle_expected,
            xmlbody_str,
        )

        # Test WLB_OPTIMIZATION_ALERT
        title_expected = "WlbOptimizationAlert"
        subtitle_expected = "wlb_optimization_alert"
        xmlbody_str = "severity:Critical mode:MaxPerformance"
        self.common_test_bad_input(
            "WLB_OPTIMIZATION_ALERT",
            "Host",
            "",
            title_expected,
            subtitle_expected,
            xmlbody_str,
        )
