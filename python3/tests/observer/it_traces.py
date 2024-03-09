"""
Test that packages/observer.py, creates a tracer, calling it using call_observer()

The tests included in this module are:

1.  `it_creates_a_tracer`:

    This test verifies that when the observer.py script is called with a configuration
    file, it creates a tracer and sets the span and patch_module functions as expected.

The tests are run using the pytest framework and are executed by calling the
call_observer() function, which simulates running the observer.py script from the
command line.

The test directory contains a dummy `observer.conf` (currently empty) configuration
file that is used to enable tracing for the test.
"""

import os
import types
from typing import Any, Dict

from pytest import CaptureFixture, LogCaptureFixture

from . import TRACED_SCRIPT, TRACED_SCRIPT_PRINT, call_observer, testdir


def assert_imported_modules(globals_dict_of_observer: Dict[str, Any]):
    """Assert that the expected modules were imported by observer.py"""

    observer_modules = globals_dict_of_observer["sys"].modules
    imported_modules = [
        "opentelemetry.baggage.propagation",
        "opentelemetry.context",
        "opentelemetry.exporter.zipkin.json",
        "opentelemetry.sdk.resources",
        "opentelemetry.sdk.trace.export",
        "opentelemetry.trace",
    ]
    assert all(mod in observer_modules for mod in imported_modules)


def it_creates_a_tracer(caplog: LogCaptureFixture, capsys: CaptureFixture[str]):
    """
    Given that packages/observer.py is started with a configuration file, it:
    - imports the opentelemetry packages [checked by this test]
    - reads the configuration file       [checked by this test]
    - creates a tracer                   [checked by this test (using caplog)]
    - sets the span() and patch_module() [checked by this test]
    - runs the traced script             [checked by this test]
    - traces the script                  [not yet checked by this test]
    """
    os.environ["OBSERVER_CONFIG_DIR"] = os.path.dirname(__file__)

    # Passing 0 to the traced script will make it print() and exit with code 0
    globals_dict_of_observer = call_observer(TRACED_SCRIPT, "0")

    with capsys.disabled():
        # If this test fails in your environment without any changes to the repo,
        # check for import errors from observer.py:_init_tracing() in the pytest logs.

        # Get the span and patch_module functions from the module's globals
        span = globals_dict_of_observer.get("span")
        patch_module = globals_dict_of_observer.get("patch_module")

        # Assert that the span and patch_module are functions
        assert callable(span)
        assert callable(patch_module)
        assert isinstance(span, types.FunctionType)
        assert isinstance(patch_module, types.FunctionType)

        # Assert that span and patch_module are the expected tracing functions
        assert span.__name__ == "span_of_tracers"
        assert span.__qualname__ == "_init_tracing.<locals>.span_of_tracers"
        assert patch_module.__name__ == "_patch_module"
        assert patch_module.__qualname__ == "_init_tracing.<locals>._patch_module"

        # Assert that the captured output is as expected
        assert capsys.readouterr().out == TRACED_SCRIPT_PRINT

        assert_imported_modules(globals_dict_of_observer)
        assert_debug_logs(caplog)


def assert_debug_logs(caplog: LogCaptureFixture):
    """
    Assert that the observer.py script read the configuration file all.conf
    by expecting the configuration file and its content in the log messages.
    """

    msg = caplog.messages
    if not msg:  # pragma: no cover
        print("No logs found in caplog, check that debug logging is enabled!")
    expected_modules = "{'module_names': 'XenAPI,tests.observer.traced_script'}"
    assert msg[1] == f"{testdir}/all.conf: {expected_modules}"
    assert msg[2] == "module_names: ['XenAPI', 'tests.observer.traced_script']"

    # Assert that the observer.py script red the observer.conf configuration file
    config = """{'otel_resource_attributes': '"key1=value1,key2=value2"'}"""
    assert msg[0] == f"configs = ['{testdir}/observer.conf']"
    assert msg[3] == f"{testdir}/observer.conf: {config}"

    # Assert that the observer.py script created a tracer
    assert msg[4].startswith("tracers=[<opentelemetry.sdk.trace.Tracer object at")
