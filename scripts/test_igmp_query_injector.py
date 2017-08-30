import unittest
from contextlib import contextmanager
import time

import sys

import binascii
from mock import patch, MagicMock

# mock modules since this repo does not contain them
sys.modules['xcp'] = MagicMock()
sys.modules['xen'] = MagicMock()
sys.modules['xen.lowlevel'] = MagicMock()
sys.modules['xen.lowlevel.xs'] = MagicMock()

sys.path.append('./scripts')

from igmp_query_injector import Cleanup, IGMPQueryGenerator, VifsInjector, \
    BridgeInjector, XSUtil, XSVifStateWatch, Vif


class TestCleanup(unittest.TestCase):
    def test_cleanup(self):
        with Cleanup():
            pass

        XSUtil.xs_handler.close.assert_called()


class TestXSVifStateWatch(unittest.TestCase):
    def test_watch(self):
        watch = XSVifStateWatch('1')
        path = '/a/b/c'
        watch.watch(path, '000')
        XSUtil.xs_handler.watch.assert_called()
        self.assertIn(path, watch.watches)

    def test_unwatch(self):
        watch = XSVifStateWatch('1')
        path = '/a/b/c'
        token = '000'
        watch.watch(path, token)
        XSUtil.xs_handler.watch.assert_called()
        self.assertIn(path, watch.watches)

        watch.unwatch(path, token)
        XSUtil.xs_handler.unwatch.assert_called()
        self.assertNotIn(path, watch.watches)


class TestIGMPQueryGenerator(unittest.TestCase):
    def test_create_igmp_layer(self):
        gen = IGMPQueryGenerator('00:00:00:00:00:00', -1, 10000)
        layer = gen.create_igmp_layer()
        expect_hex_str = "1164ee9b00000000"
        self.assertEqual(layer, binascii.unhexlify(expect_hex_str))

    def test_generate_without_vlan(self):
        gen = IGMPQueryGenerator('01:00:5e:00:00:01', -1, 10000)
        packet = gen.generate()
        expect_hex_str = '01005e00000100000000000008004600002000010000010244d600000000e0000001940400001164ee9b00000000'
        self.assertEqual(packet, binascii.unhexlify(expect_hex_str))

    def test_generate_with_vlan(self):
        gen = IGMPQueryGenerator('01:00:5e:00:00:01', 1209, 10000)
        packet = gen.generate()
        expect_hex_str = '01005e000001000000000000810024b908004600002000010000010244d600000000e0000001940400001164ee9b00000000'
        self.assertEqual(packet, binascii.unhexlify(expect_hex_str))

    def test_generate_max_resp_time_100ms(self):
        gen = IGMPQueryGenerator('01:00:5e:00:00:01', -1, 100)
        packet = gen.generate()
        expect_hex_str = '01005e00000100000000000008004600002000010000010244d600000000e0000001940400001101eefe00000000'
        self.assertEqual(packet, binascii.unhexlify(expect_hex_str))


class TestVifsInjector(unittest.TestCase):
    @contextmanager
    def assert_time_elapse_in(self, min, max):
        start = time.time()
        yield
        elapse = time.time() - start
        self.assertTrue(min <= elapse <= max, 'elapse time %f should in [%f, %f]' % (elapse, min, max))

    @patch('igmp_query_injector.Vif')
    @patch('igmp_query_injector.IGMPQueryInjector.inject_to_vif')
    @patch('igmp_query_injector.VifsInjector._inject_with_connection_state_check')
    def test_inject_without_connection_state_check(self, mock_inject_with_connection_state_check,
                                                   mock_inject_to_vif, MockVif):
        injector = VifsInjector(100, ['vif1.1', 'vif2.1'], 0)
        injector.inject()
        mock_inject_with_connection_state_check.assert_not_called()
        self.assertEqual(mock_inject_to_vif.call_count, 2)

    @patch('igmp_query_injector.IGMPQueryInjector.inject_to_vif')
    @patch('threading.Thread')
    def test_inject_with_connection_state_check_timeout(self, mockThread, mock_inject_to_vif):
        injector = VifsInjector(100, ['vif1.1', 'vif2.1'], 3)
        # should timeout in 3 seconds
        with self.assert_time_elapse_in(2, 4):
            injector.inject()
        # won't inject query to any vif
        mock_inject_to_vif.assert_not_called()

    @patch('igmp_query_injector.IGMPQueryInjector.inject_to_vif')
    @patch('igmp_query_injector.XSUtil.read_watch')
    @patch('igmp_query_injector.XSVifStateWatch.val_change_to_expected')
    def test_inject_with_connection_state_check_succ(self, mock_val_change_to_expected, mock_read_watch, mock_inject_to_vif):
        mock_val_change_to_expected.return_value = True
        vifs = []
        side_effect = []
        for domid, vifid in [(1, 2), (2, 3)]:
            vif = 'vif%d.%d' % (domid, vifid)
            side_effect.append((XSUtil.vif_state_path(domid, vifid), Vif(vif)))
            vifs.append(vif)
        mock_read_watch.side_effect = side_effect
        injector = VifsInjector(100, vifs, 3)
        with self.assert_time_elapse_in(0, 1):
            injector.inject()
        self.assertEqual(mock_inject_to_vif.call_count, 2)


class TestBridgeInjector(unittest.TestCase):
    @patch('subprocess.check_output')
    def test_get_vifs_on_bridge(self, mock_subprocess_check_output):
        mock_subprocess_check_output.return_value = 'vif1.1\ntap1.0\nvif1.2'
        injector = BridgeInjector(100, ['xenbr0'])
        vifs = injector.get_vifs_on_bridge('xenbr0')
        self.assertEqual(len(vifs), 2)


if __name__ == '__main__':
    unittest.main()
