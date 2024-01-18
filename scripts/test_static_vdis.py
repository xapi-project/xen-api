"""unittest for static-vdis"""
import sys
import tempfile
import unittest

from .unit_tests.import_helper import import_file_as_module, mocked_modules


# @unittest.skipIf(sys.version_info < (3, 0), reason="script migrated to python3")
class TestReadWriteFile(unittest.TestCase):
    def test_write_and_read_whole_file(self):
        """Test read_whole_file and write_whole_file"""

        content = r"""def read_whole_file(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        return ''.join(f.readlines()).strip()
def write_whole_file(filename, contents):
    with open(filename, "w", encoding='utf-8') as f:
        f.write(contents)"""

        # Mock and XenAPI and inventory for importing scripts/static-vdis:
        with mocked_modules("XenAPI", "inventory", "xmlrpc", "xmlrpc.client", "urllib.parse"):
            static_vdis = import_file_as_module("scripts/static-vdis")

        # Test the update write_whole_file and read_whole_file functions:
        with tempfile.NamedTemporaryFile() as temp:
            static_vdis.write_whole_file(temp.name, content)
            assert static_vdis.read_whole_file(temp.name) == content
