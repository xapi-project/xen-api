"""
Package providing helper definitions and functions like call_observer()
to run python3/packages/observer.py as a script using runpy.run_path().
"""

import os
import runpy
import sys

from typing import Any, Dict

testdir = os.path.dirname(__file__)
OBSERVER_PY = os.path.relpath(testdir + "/../../packages/observer.py")
TRACED_SCRIPT = os.path.relpath(testdir + "/traced_script.py")
TRACED_SCRIPT_PRINT = "Hello, I am a print() in traced_script.py.\n"


def call_observer(*args: str) -> Dict[str, Any]:
    """
    Call the observer.py script and return its globals dictionary for checking it

    Note: This is only possible when the script is run using runpy.run_path()
    and the script exits normally (does not raise and Exception like SystemExit).

    Features:
    - __name__ is set to "__main__", so the module is run as a script.
    - sys.argv is set to the passed arguments
    - no mocks are used, so the actual observer.py script is run.
    - sets os.environ["OBSERVER_DEBUG"] = "True" to enable debug logging
      to let the tests check the debug messages for checking the reading
      of the configuration files and setting up tracing.
    """

    os.environ["OBSERVER_DEBUG"] = "True"
    sys.argv = [OBSERVER_PY, *args]
    return runpy.run_path(OBSERVER_PY, run_name="__main__")
