#
# test_mail-alarm.py: uses unittest to test script "mail-alarm"
#

import tempfile
import os
import shutil
import sys
import unittest
import mock

sys.path.append('./scripts/examples/python')
sys.modules['xcp'] = mock.Mock()

log_file_global = None

def mock_setup(module):
    # Emulate functions with Mock
    module.log_err = log_err
    module.get_pool_name = mock.Mock(return_value="test_pool")
    module.get_sr_name_by_uuid = mock.Mock(return_value="test_sr")
    module.get_VM_params = mock.Mock(return_value={"name_label":"test_VM"})
    module.get_host_params = mock.Mock(return_value={"name_label":"test_host"})
    module.mail_language_pack_path = './scripts/mail-languages'
    module.branding.PRODUCT_BRAND = "XenServer"
    module.branding.BRAND_CONSOLE = "XenCenter"

def log_err(err):
    global log_file_global
    with open(log_file_global, 'a+') as fileh:
        fileh.write("%s: %s\n" % (sys.argv[0], err))

class TestXapiMessage(unittest.TestCase):
    def setUp(self):
        global log_file_global
        try:
            self.work_dir = tempfile.mkdtemp(prefix='test-mail-alarm-')
            log_file_global = os.path.join(self.work_dir, 'user.log')
            src_file = './scripts/mail-alarm'
            dst_file = os.path.join(self.work_dir, 'mailalarm.py')
            shutil.copyfile(src_file, dst_file)
            sys.path.append(self.work_dir)
        except:
            raise

    def tearDown(self):
            shutil.rmtree(self.work_dir, ignore_errors=True)

    def test_good_mail_language(self):
        global log_file_global
        import mailalarm

        tst_xml = '''<?xml version="1.0" encoding="UTF-8"?>
<message><generation>63102</generation><ref>OpaqueRef:46be74f4-3a26-31a8-a629-d52584fe6ed3</ref><name>ALARM</name><priority>3</priority><cls>Host</cls><obj_uuid>2e00443d-ac29-4940-8433-a15dda1e8f8e</obj_uuid><timestamp>20170516T16:30:00Z</timestamp><uuid>0d985f5e-6d91-3410-f853-040d0906a4b9</uuid><body>value: 0.128963
config:
&lt;variable&gt;
    &lt;name value=&quot;cpu_usage&quot;/&gt;
    &lt;alarm_trigger_level value=&quot;0.05&quot;/&gt;
    &lt;alarm_trigger_period value=&quot;60&quot;/&gt;
    &lt;alarm_auto_inhibit_period value=&quot;300&quot;/&gt;
&lt;/variable&gt;
</body></message>'''

        # Emulate functions with Mock
        mock_setup(mailalarm)

        obj_XapiMessage = mailalarm.XapiMessage(tst_xml, 'en-US')
        self.assertIsNotNone(obj_XapiMessage)

        mail_subject = obj_XapiMessage.generate_email_subject()
        mail_body = obj_XapiMessage.generate_email_body()

        self.assertEqual(mail_subject, '[test_pool] XenServer Alarm: CPU usage on Host "test_host"')
        self.assertEqual(mail_body, 'CPU usage on Host "test_host" has been on average 12.9% for the last 60 seconds.\nThis alarm is set to be triggered when CPU usage is more than 5.0%.\n\nFor Alarm Settings, please log into your XenCenter Console and click on "Server"->\n"Properties"->"Alerts"\n')

    def test_bad_mail_language(self):
        global log_file_global
        import mailalarm

        tst_xml = '''<?xml version="1.0" encoding="UTF-8"?> <message><ref>OpaqueRef:2dea9e18-36c2-9325-12ae-a8e96d30a526</ref><name>WLB_OPTIMIZATION_ALERT</name><priority>5</priority><cls>Pool</cls><obj_uuid>be2f83d6-324f-ed16-2762-09faeb1f01b9</obj_uuid><timestamp>20170526T07:23:43Z</timestamp><uuid>c2166e19-3154-e2f9-1f5e-74d296d0a3de</uuid><body>severity:Critical mode:MaxPerformance</body></message>'''

        # Emulate functions with Mock
        mock_setup(mailalarm)

        obj_XapiMessage = mailalarm.XapiMessage(tst_xml, 'zh-CNaaa')
        self.assertIsNotNone(obj_XapiMessage)

        mail_subject = obj_XapiMessage.generate_email_subject()
        mail_body = obj_XapiMessage.generate_email_body()

        with open(log_file_global, 'r') as fileh:
            log_strs = fileh.read()

        self.assertIn('Read mail language pack error', log_strs)
        self.assertIn('[WlbOptimizationAlert] error in looking for (wlb_optimization_alert:subject). Mail language pack:(None)', log_strs)
        self.assertIn('[WlbOptimizationAlert] error in looking for (wlb_optimization_alert:body). Mail language pack:(None)', log_strs)

