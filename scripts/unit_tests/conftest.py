"""Pytest conftest module with functions and (in the future) pytest fixtures"""
import logging
import os
import sys

import mock
import pytest


# Add an attribute when running under pytest to make it possible for a module
# to check to know if unittest is used (then skip) or pytest (then run test):


def pytest_configure(config):  # pylint: disable=unused-argument
    """Configure pytest test cases

    This function is called by pytest during its configuration phase. It takes a
    single argument 'config', which is an instance of the pytest Config object.

    The purpose of this function is to set a flag in the sys module to indicate
    that it is being called from pytest. This flag can be used by other parts
    of the code to determine if they are running under pytest or not.

    :param config: The pytest configuration object.
    """
    # pylint: disable=protected-access
    sys._called_from_pytest = True  # type: ignore[attr-defined]


def pytest_unconfigure(config):  # pylint: disable=unused-argument
    """Perform cleanup or finalization steps after pytest has finished running.

    This function is called by pytest after all tests have been executed and
    pytest is about to exit. It can be used to perform any necessary cleanup
    or finalization steps.

    :param config: The pytest configuration object.
    """
    del sys._called_from_pytest  # type: ignore[attr-defined]


@pytest.fixture(scope="function")
def usb_reset():
    """
    This function is a pytest fixture that sets up the necessary environment
    for testing the 'usb_reset' script.

    It mocks the 'xcp' and 'xcp.logger' modules, and adds the directory of the
    unit test to the sys.path so that the tested script can be imported.

    The imported 'usb_reset.log' is redirected to the 'logging' module.

    :returns: The imported 'usb_reset' module.
    """
    sys.modules["xcp"] = mock.Mock()
    sys.modules["xcp.logger"] = mock.Mock()

    # Prepend the directory of the unit test (to import the tested script) to sys.path,
    # so we can do an absolute import and absolute patch without adding __init__.py:
    sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

    # Then, import usb_reset.py from the __file__'s directory (found first there):
    import usb_reset as usb_reset_module  # pylint: disable=import-outside-toplevel

    # Redirect usb_reset.log (is a mock xcp.logger for import) to logging
    usb_reset_module.log = logging
    return usb_reset_module
