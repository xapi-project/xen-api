"""helpers for unit-testing functions in scripts without permanent global mocks"""
import os
import sys
from contextlib import contextmanager
from types import ModuleType

from typing import Generator
from mock import Mock


@contextmanager
def mocked_modules(*module_names):  # type:(str) -> Generator[None, None, None]
    """Context manager that temporarily mocks the specified modules.

    :param module_names: Variable number of names of the modules to be mocked.
    :yields: None

    During the context, the specified modules are added to the sys.modules
    dictionary as instances of the ModuleType class.
    This effectively mocks the modules, allowing them to be imported and used
    within the context. After the context, the mocked modules are removed
    from the sys.modules dictionary.

    Example usage:
    ```python
    with mocked_modules("module1", "module2"):
        # Code that uses the mocked modules
    ```
    """
    for module_name in module_names:
        sys.modules[module_name] = Mock()
    yield
    for module_name in module_names:
        sys.modules.pop(module_name)


def import_file_as_module(relative_script_path):  # type:(str) -> ModuleType
    """Import a Python script without the .py extension as a python module.

    :param relative_script_path (str): The relative path of the script to import.
    :returns module: The imported module.
    :raises: AssertionError: If the spec or loader is not available.

    Note:
    - This function uses different methods depending on the Python version.
    - For Python 2, it uses the imp module.
    - For Python 3, it uses the importlib module.

    Example:
    - import_script_as_module('scripts/mail-alarm')  # Returns the imported module.
    """
    script_path = os.path.dirname(__file__) + "/../../" + relative_script_path
    module_name = os.path.basename(script_path.replace(".py", ""))

    # For Python 3.11+: Import Python script without the .py extension:
    # https://gist.github.com/bernhardkaindl/1aaa04ea925fdc36c40d031491957fd3:
    # pylint: disable-next=import-outside-toplevel
    from importlib import (  # pylint: disable=no-name-in-module
        machinery,
        util,
    )

    loader = machinery.SourceFileLoader(module_name, script_path)
    spec = util.spec_from_loader(module_name, loader)
    assert spec
    assert spec.loader
    module = util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module
