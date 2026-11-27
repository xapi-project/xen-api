#!/usr/bin/python3
import unittest
from unittest import mock

from common import ON_ERROR_FAIL, ON_ERROR_LOG, run


class TestRun(unittest.TestCase):
    def _failed_process(self):
        stderr = mock.Mock()
        stderr.read.return_value = "some error\n"
        return (1, mock.Mock(), stderr)

    @mock.patch('common.send_to_syslog')
    @mock.patch('common.doexec')
    def test_run_on_error_fail_exits(self, mock_doexec, mock_syslog):
        mock_doexec.return_value = self._failed_process()
        with self.assertRaises(SystemExit):
            run(ON_ERROR_FAIL, ['bad command'])
        self.assertIn(
            'FATAL: bad command exitted with 1: some error\n',
            [c.args[0] for c in mock_syslog.call_args_list])

    @mock.patch('common.send_to_syslog')
    @mock.patch('common.doexec')
    def test_run_on_error_log_does_not_exit(self, mock_doexec, mock_syslog):
        mock_doexec.return_value = self._failed_process()
        run(ON_ERROR_LOG, ['bad command'])
        self.assertIn(
            'bad command exitted with 1: some error\n',
            [c.args[0] for c in mock_syslog.call_args_list])
