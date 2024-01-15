#!/usr/bin/env python3
#
# unittest for static-vdis

import unittest
from mock import MagicMock
import sys
import os
import subprocess
import tempfile

# mock modules to avoid dependencies
sys.modules["XenAPI"] = MagicMock()
sys.modules["inventory"] = MagicMock()

def import_from_file(module_name, file_path):
    """Import a file as a module"""
    if sys.version_info.major == 2:
        return None
    else:
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

def get_module():
    """Import the static-vdis script as a module for executing unit tests on functions"""
    testdir = os.path.dirname(__file__)
    return import_from_file("static_vdis", testdir + "/static-vdis")

static_vdis = get_module()

@unittest.skipIf(sys.version_info < (3, 0), reason="requires python3")     
class TestReadWriteFile(unittest.TestCase):
    def test_write_and_read_whole_file(self):
        """Test read_whole_file and write_whole_file"""
        test_file = tempfile.NamedTemporaryFile(delete=True)
        filename = str(test_file.name)
        content = r"""def read_whole_file(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        return ''.join(f.readlines()).strip()
        
def write_whole_file(filename, contents):
    with open(filename, "w", encoding='utf-8') as f:
        f.write(contents)"""
        static_vdis.write_whole_file(filename, content)
        expected_content = static_vdis.read_whole_file(filename)
        self.assertEqual(expected_content, content)
        
        