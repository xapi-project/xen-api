# Used for importing a non-".py" file as a module

import sys
import os

def import_from_file(module_name, file_path):
    """Import a file as a module"""
    # Only for python3, but CI has python2 pytest check, so add this line
    if sys.version_info.major == 2:
        return None
    from importlib import machinery, util
    loader = machinery.SourceFileLoader(module_name, file_path)
    spec = util.spec_from_loader(module_name, loader)
    assert spec
    assert spec.loader
    module = util.module_from_spec(spec)
    # Probably a good idea to add manually imported module stored in sys.modules
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module

def get_module(module_name, file_path):
    testdir = os.path.dirname(__file__)
    return import_from_file(module_name, testdir + file_path)