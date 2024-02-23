#!/usr/bin/env python3

"""
This module provides unit test for static-vdis.
"""

import unittest
import sys
import tempfile
from mock import MagicMock
from import_file import get_module

# mock modules to avoid dependencies
sys.modules["XenAPI"] = MagicMock()
sys.modules["inventory"] = MagicMock()

static_vdis = get_module("static_vdis", "/static-vdis")

@unittest.skipIf(sys.version_info < (3, 0), reason="requires python3")
class TestReadWriteFile(unittest.TestCase):
    """Test function read_whole_file and write_whole_file"""
    def test_write_and_read_whole_file(self):
        """Test read_whole_file and write_whole_file"""
        with tempfile.NamedTemporaryFile(delete=True) as test_file:
            filename = str(test_file.name)
            # Construct a multi-line text containing indentation and spaces.
            content = r"""def read_whole_file(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        return ''.join(f.readlines()).strip()
        
def write_whole_file(filename, contents):
    with open(filename, "w", encoding='utf-8') as f:
        f.write(contents)"""

            static_vdis.write_whole_file(filename, content)
            expected_content = static_vdis.read_whole_file(filename)
            self.assertEqual(expected_content, content)
