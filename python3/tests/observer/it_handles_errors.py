"""
Test error handing of python3/packages/observer.py, calling it using call_observer()

This module contains tests for the error handling functionality of the observer.py
script in the python3/packages directory.

The tests are executed by calling the observer.py script via the call_observer()
function. The primary focus of these tests is to verify the behavior of the observer.py
script when various errors occur.

The tests included in this module are:

1.  `it_handles_not_finding_the_script`:

    This test verifies that when the observer.py does not find the script to trace
    is not found, it exits with the correct exit code and produces the expected output.

2.  `it_prints_exception_traceback`:

    This test verifies that when the traced script raises an exception, the observer.py
    script captures the exception traceback and exits with the correct exit code.

3.  `it_shows_the_usage_message`:

    This test verifies that when the observer.py script is called without any arguments,
    it exits with the correct exit code and produces the expected output.

4.  `it_handles_error_exit`:

    This test verifies that when the traced script exits with a non-zero exit code, the
    observer.py script captures the exit code and produces the expected output.

5.  `it_does_not_trace_without_config`:

    This test verifies that when observer.py is called without a configuration
    file, it does not trace the traced script and produces the expected output.

The tests are run using the pytest framework and are executed by calling the
call_observer() function, which simulates running the observer.py script from the
command line.
"""

import os

import pytest
from pytest import CaptureFixture

from . import OBSERVER_PY, TRACED_SCRIPT, TRACED_SCRIPT_PRINT, call_observer


def it_handles_not_finding_the_script(capsys: CaptureFixture[str]):
    """
    Given that packages/observer.py is started with a configuration file,
    and the traced script is not found:
    - The test checks that the exit code and the captured output are as expected.
    """
    nonexisting_script = "nonexisting_traced_script.py"
    with pytest.raises(SystemExit) as exc_info:
        call_observer(nonexisting_script, "arg")

    assert exc_info.value.code == 2  # Set as the exit code for a missing script

    # Check that the error message is as expected
    with capsys.disabled():
        stderr = capsys.readouterr().err.splitlines()
        assert stderr[0] == f"{OBSERVER_PY} {nonexisting_script} arg:"
        assert stderr[1] == f"Script not found: {os.getcwd()}/{nonexisting_script}"


def it_prints_exception_traceback(capsys: CaptureFixture[str]):
    """
    Given that packages/observer.py is started with a configuration file,
    and an invalid argument is passed to to the traced script as its argument:

    - The traced script should raise an exception and exit with 139
    - The test checks that the exit code and the captured output are as expected.
    """
    with pytest.raises(SystemExit) as exc_info:
        call_observer(TRACED_SCRIPT, "not_an_int")

    # 139 is used as the exit code when an Exception in the traced script was caught
    assert exc_info.value.code == 139

    # Check that the error message is as expected
    with capsys.disabled():
        stderr = capsys.readouterr().err.splitlines()
        assert stderr[0] == f"{OBSERVER_PY} {TRACED_SCRIPT} not_an_int:"
        assert stderr[1] == "Exception in the traced script:"
        assert stderr[2] == "invalid literal for int() with base 10: 'not_an_int'"
        assert stderr[3] == "Traceback (most recent call last):"


def it_shows_the_usage_message(capsys: CaptureFixture[str]):
    """
    Given that packages/observer.py is started as a script without any arguments:
    - The test checks that the exit code and the captured output are as expected.
    """

    with pytest.raises(SystemExit) as exc_info:
        call_observer()
    assert exc_info.value.code == 31
    with capsys.disabled():
        stderr = capsys.readouterr().err
        assert stderr == f"{OBSERVER_PY}: usage: command argument list\n"


def it_handles_error_exit(capsys: CaptureFixture[str]):
    """
    Given that packages/observer.py is started with a configuration file,
    and the traced script exits with a non-zero exit code:
    - The expected exit code is passed to to the traced script as its argument.
    - The traced script should print a message and exit with the given exit code.
    - The test checks that the exit code and the captured output are as expected.
    """

    # Passing 1 to the traced script will make it print() and exit with code 1
    with pytest.raises(SystemExit) as exc_info:
        call_observer(TRACED_SCRIPT, "1")
    assert exc_info.value.code == 1
    with capsys.disabled():
        assert capsys.readouterr().out == TRACED_SCRIPT_PRINT


def it_does_not_trace_without_config(capsys: CaptureFixture[str]):
    """
    Given that packages/observer.py is started without a configuration file:

    - The expected exit code is passed to to the traced script as its argument.
    - The traced script should print a message and exit with 0
    - The test checks that the exit code and the captured output are as expected.
    """

    # Prepare the environment and run the observer.py script
    os.environ["OBSERVER_CONFIG_DIR"] = "nonexisting_config_directory"

    # Passing 0 to the traced script will make it print() and exit with code 0
    globs = call_observer(TRACED_SCRIPT, "0")

    with capsys.disabled():
        assert capsys.readouterr().out == TRACED_SCRIPT_PRINT

        # Check that the observer.py script didn't install the tracing functions
        span = globs.get("span")
        patch_module = globs.get("patch_module")
        assert span and patch_module
        assert span.__name__ == "_span_noop"
        assert patch_module.__name__ == "_patch_module_noop"
