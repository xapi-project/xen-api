"""Test python3/packages/observer.py"""

import os
import sys
import unittest

from mock import MagicMock, mock_open, patch

# Ensure observer is initialised as noop
with patch("os.listdir") as mock_listdir:
    # Prevent it finding an observer.conf
    mock_listdir.return_value = []
    from python3.packages import observer

TEST_CONFIG = """
    XS_EXPORTER_BUGTOOL_ENDPOINT='/var/log/dt/test'
    OTEL_SERVICE_NAME='test-observer'
    OTEL_RESOURCE_ATTRIBUTES='service.name=sm'
    """
TEST_OBSERVER_CONF = "test-observer.conf"
OBSERVER_OPEN = "python3.packages.observer.open"

#
# These are the modules that are mocked to avoid dependencies.
# Note: wrapt is not mocked: It is used to wrap the traced script.
# These modules are not imported at the top of observer.py, but are
# imported inside the observer._init_tracing(). This is why they are mocked
# in the test class before calling observer._init_tracing() and then deleted
# in the tearDown of the test class to avoid affecting other tests.
#
MOCKED_MODULES = [
    "opentelemetry",
    "opentelemetry.sdk.resources",
    "opentelemetry.sdk.trace",
    "opentelemetry.sdk.trace.export",
    "opentelemetry.exporter.zipkin.json",
    "opentelemetry.baggage.propagation",
    "opentelemetry.trace.propagation.tracecontext",
    "opentelemetry.context",
    "opentelemetry.trace",
]


# pylint: disable=missing-function-docstring,protected-access
class TestObserver(unittest.TestCase):
    """Test python3/packages/observer.py"""

    def setUp(self) -> None:
        # As setup for this class, mock modules to avoid dependencies
        for mock in MOCKED_MODULES:
            sys.modules[mock] = MagicMock()
        return super().setUp()

    def tearDown(self) -> None:
        # On teardown, delete mocks so they do not affect other tests
        # Otherwise, the mocks will be used in other tests and cause errors
        for mock in MOCKED_MODULES:
            del sys.modules[mock]
        return super().tearDown()

    def simple_method(self):
        """A simple helper method for tests to wrap using observer.span"""
        return 5

    def init_tracing_and_run_simple_method(self, read_data):
        """Run the init_tracing method with the given read_data"""

        with patch(OBSERVER_OPEN, mock_open(read_data=read_data)):
            span, _ = observer._init_tracing([TEST_OBSERVER_CONF], ".")

        simple_method = span(self.simple_method)
        self.assertEqual(simple_method(), 5)

    def test_span_with_parameters(self):
        span, _ = observer._init_tracing([], ".")

        # This needs to use the decorator sugar so that wrapped is initially None
        @span(span_name_prefix="test")
        def simple_method():
            return 5

        self.assertEqual(simple_method(), 5)

    def test_span_of_tracers_with_parameters(self):
        with patch(OBSERVER_OPEN, mock_open(read_data="empty")):
            span, _ = observer._init_tracing([TEST_OBSERVER_CONF], ".")

        # This needs to use the decorator sugar so that wrapped is initially None
        @span(span_name_prefix="test")
        def simple_method():
            return 5

        self.assertEqual(simple_method(), 5)

    # Use the span method defined on import with no configs
    def test_span_after_import(self):
        simple_method = observer.span(self.simple_method)

        self.assertEqual(simple_method(), 5)

    # Check the span is a noop when configs is empty
    def test_configs_empty(self):
        span, _ = observer._init_tracing([], ".")

        simple_method = span(self.simple_method)

        self.assertEqual(simple_method(), 5)

    @patch("os.path.isfile")
    @patch("os.listdir")
    def test_get_configs_list(self, mock_ls, mock_isfile):
        mock_ls.return_value = [
            "first-observer.conf",
            "ignore.conf",
            "second-observer.conf",
        ]
        mock_isfile.return_value = True
        configs = observer._get_configs_list("test-dir")

        self.assertEqual(
            configs, ["test-dir/first-observer.conf", "test-dir/second-observer.conf"]
        )

    def test_configs_directory_not_found(self):
        configs = observer._get_configs_list("non-existent")

        self.assertEqual(configs, [])

    def test_configs_exist(self):
        self.init_tracing_and_run_simple_method(read_data=TEST_CONFIG)
        self.assertEqual(os.environ["OTEL_RESOURCE_ATTRIBUTES"], "service.name=sm")

    def test_all_conf_missing(self):
        with patch(OBSERVER_OPEN) as mock_file:
            mock_file.return_value.__enter__.side_effect = [
                FileNotFoundError,
                mock_open(read_data="empty").return_value,
            ]
            span, _ = observer._init_tracing([TEST_OBSERVER_CONF], ".")

        simple_method = span(self.simple_method)

        self.assertEqual(simple_method(), 5)

    # A method to decorate with observer.span
    def simple_method_with_args(self, a, b=3):
        return a + b

    def test_tracing_with_arguments(self):
        with patch(OBSERVER_OPEN, mock_open(read_data="empty")):
            span, _ = observer._init_tracing([TEST_OBSERVER_CONF], ".")

        simple_method_with_args = span(self.simple_method_with_args)

        self.assertEqual(simple_method_with_args(5), 8)
