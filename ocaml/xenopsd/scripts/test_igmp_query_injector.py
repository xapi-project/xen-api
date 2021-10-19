import unittest
from contextlib import contextmanager
import time
import sys
from mock import patch, MagicMock

# mock modules since this repo does not contain them
sys.modules['xcp'] = MagicMock()
sys.modules['xen'] = MagicMock()
sys.modules['xen.lowlevel'] = MagicMock()
sys.modules['xen.lowlevel.xs'] = MagicMock()
sys.modules['scapy'] = MagicMock()
sys.modules['scapy.all'] = MagicMock()
sys.modules['scapy.contrib'] = MagicMock()
sys.modules['scapy.contrib.igmp'] = MagicMock()


sys.path.append('./scripts')

from igmp_query_injector import XSWatcher, IGMPQueryInjector, get_vif_state_path


class TestXSWatcher(unittest.TestCase):
    def test_watch(self):
        watcher = XSWatcher()
        path = '/a/b/c'
        watcher.watch(path, '000')
        self.assertIn(path, watcher.watches)

    def test_unwatch(self):
        watcher = XSWatcher()
        path = '/a/b/c'
        token = '000'
        watcher.watch(path, token)
        self.assertIn(path, watcher.watches)

        watcher.unwatch(path, token)
        self.assertNotIn(path, watcher.watches)


class TestIGMPQueryInjector(unittest.TestCase):
    @contextmanager
    def assert_time_elapse_in(self, min, max):
        start = time.time()
        yield
        elapse = time.time() - start
        self.assertTrue(min <= elapse <= max, 'elapse time %f should in [%f, %f]' % (elapse, min, max))

    @patch('igmp_query_injector.IGMPQueryInjector.inject_to_vif')
    @patch('igmp_query_injector.IGMPQueryInjector._inject_with_connection_state_check')
    def test_inject_without_connection_state_check(self, mock_inject_with_connection_state_check,
                                                   mock_inject_to_vif):
        injector = IGMPQueryInjector(100, ['vif1.1', 'vif2.1'], 0)
        injector.inject()
        mock_inject_with_connection_state_check.assert_not_called()
        self.assertEqual(mock_inject_to_vif.call_count, 2)

    @patch('igmp_query_injector.IGMPQueryInjector.inject_to_vif')
    @patch('igmp_query_injector.XSWatcher.read_watch')
    def test_inject_with_connection_state_check_timeout(self, mock_read_watch, mock_inject_to_vif):
        mock_read_watch.return_value = ('path', 'vif1.1')
        injector = IGMPQueryInjector(100, ['vif1.1', 'vif2.1'], 3)
        # should timeout in 3 seconds
        with self.assert_time_elapse_in(2, 4):
            injector.inject()
        # won't inject query to any vif
        mock_inject_to_vif.assert_not_called()

    @patch('igmp_query_injector.IGMPQueryInjector.inject_to_vif')
    @patch('igmp_query_injector.XSWatcher.read_watch')
    @patch('igmp_query_injector.g_xs_handler')
    def test_inject_with_connection_state_check_succ(self, mock_gs_handler, mock_read_watch, mock_inject_to_vif):
        mock_gs_handler.read.return_value = '4'

        def get_domain_path(domid):
            return '/local/domain/%d' % domid

        mock_gs_handler.get_domain_path = get_domain_path
        vifs = []
        side_effect = []
        for vif in ('vif1.2', 'vif2.3'):
            state_path, backend_state_path = get_vif_state_path(vif)
            side_effect.append((state_path, vif))
            side_effect.append((backend_state_path, vif))
            vifs.append(vif)
        mock_read_watch.side_effect = side_effect
        injector = IGMPQueryInjector(100, vifs, 3)
        with self.assert_time_elapse_in(0, 1):
            injector.inject()
        self.assertEqual(mock_inject_to_vif.call_count, 2)


if __name__ == '__main__':
    unittest.main()
