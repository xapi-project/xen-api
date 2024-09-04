"""python3/tests/test_static_vdis.py: Test the static-vdis script"""

import os
import sys
from pathlib import Path
from types import ModuleType

import pytest

from python3.tests.import_helper import import_file_as_module, mocked_modules

# ----------------------------  Test fixtures ---------------------------------


@pytest.fixture(scope="function")  # function scope: Re-run for each test function
def static_vdis() -> ModuleType:
    """Test fixture to return the static-vdis module, mocked to avoid dependencies."""
    with mocked_modules("XenAPI", "inventory"):
        return import_file_as_module("python3/bin/static-vdis")


# Hide pylint warnings for redefined-outer-name from using the static_vdis fixture:
# pylint: disable=redefined-outer-name
# Allow to access attributes of the static_vdis module from this test module:
# pyright: reportAttributeAccessIssue=false

# -----------------------------  Test cases -----------------------------------


def test_whole_file(static_vdis: ModuleType, tmp_path):
    """Test read_whole_file() and write_whole_file()"""

    with open(__file__, encoding="utf-8") as data:
        contents = data.read().strip()
        assert static_vdis.read_whole_file(__file__) == contents
        assert static_vdis.write_whole_file(tmp_path / "temp_file", contents) is None
        with open(tmp_path / "temp_file", encoding="utf-8") as written_data:
            assert written_data.read().strip() == contents


def test_attach(static_vdis: ModuleType, tmpdir, mocker, capsys):
    """Test five common and SMAPIv1 code paths in the attach() function"""

    # Path 1: When the VDI is not found, expect attach() to raise an exception:
    static_vdis.list_vdis = lambda: [{"vdi-uuid": "existing-uuid"}]
    with pytest.raises(Exception) as exc_info:
        static_vdis.attach("nonexisting-uuid")
    assert exc_info.value.args[0] == "Disk configuration not found"

    # Path 2: When the VDI is already attached, expect main():attach to return None\n:
    static_vdis.list_vdis = lambda: [{"vdi-uuid": "attached", "path": "/attached"}]
    sys.argv = ["static-vdis", "attach", "attached"]
    static_vdis.main()
    with capsys.disabled():
        assert capsys.readouterr().out == "None\n"

    # Path 3: When the VDI is not attached, attach() to return "the-id/disk":
    vdis: list[dict[str, str]] = [{"vdi-uuid": "attach-uuid", "id": "the-id"}]
    static_vdis.list_vdis = lambda: vdis
    static_vdis.call_backend_attach = lambda driver, config: "/mock-attached-path"
    static_vdis.read_whole_file = lambda path: '{"json":true}'
    disk = tmpdir.mkdir(vdis[0]["id"]).join("disk")
    static_vdis.main_dir = str(tmpdir)
    assert static_vdis.attach("attach-uuid") == disk
    assert os.readlink(disk) == "/mock-attached-path"
    os.unlink(disk)

    # Path 4: Create the disk file expect it to be deleted and replaced by a symlink:
    disk.write("mock-disk-contents-to-delete")
    assert static_vdis.attach("attach-uuid") == disk
    assert os.readlink(disk) == "/mock-attached-path"

    # Path 5: When the backend call returns None, expect attach() to raise TypeError
    static_vdis.call_backend_attach = lambda driver, config: None
    with pytest.raises(TypeError) as exc_info:
        static_vdis.attach("attach-uuid")

    # Path 6: When the backend returns an empty str, attach() raises FileNotFoundError:
    static_vdis.call_backend_attach = lambda driver, config: ""
    with pytest.raises(FileNotFoundError) as exc_info:
        static_vdis.attach("attach-uuid")

    # Path 7: If the smapiv3_config exists, but not the volume plugin, attach() fails:
    with pytest.raises(FileNotFoundError) as exc_info:
        mocker.patch("os.path.exists", return_value=True)
        static_vdis.MULTIPATH_FLAG = __file__
        static_vdis.attach("attach-uuid")


def test_fresh_name(static_vdis: ModuleType, tmp_path: Path):
    """Test fresh_name() and list_vdis() - all code paths"""

    # When the freshly created tmp_path is empty, expect [] and "0":
    static_vdis.main_dir = tmp_path.as_posix()
    assert static_vdis.list_vdis() == []
    assert static_vdis.fresh_name() == "0"

    # When main_dir contains a directory with name "0", the next name should be "1":
    os.mkdir(static_vdis.main_dir + "/0")
    assert static_vdis.fresh_name() == "1"

    # When main_dir contains a directory with name "1", the next name should be "2":
    os.mkdir(static_vdis.main_dir + "/1")
    assert static_vdis.fresh_name() == "2"

    # When main_dir does not exist, an empty list and 0 should be returned:
    static_vdis.main_dir = tmp_path.as_posix() + "/does-not-exist"
    assert static_vdis.list_vdis() == []
    assert static_vdis.fresh_name() == "0"


def test_sr_attach(static_vdis: ModuleType, mocker):
    """Test sr_attach()"""

    # We need to mock those as they would attempt to load the volume plugin and
    # check the clusterstack, which are not available in the test environment:
    static_vdis.call_volume_plugin = mocker.MagicMock()
    static_vdis.check_clusterstack = mocker.MagicMock()

    # Set the return value of the mocked functions to success:
    static_vdis.call_volume_plugin.return_value = "success"
    static_vdis.check_clusterstack.return_value = "success"

    # Call the sr_attach function
    device_config = {"key1": "value1", "key2": "value2"}
    result = static_vdis.sr_attach("plugin_name", device_config)

    # Assert the expected behavior
    assert result == "success"
    static_vdis.call_volume_plugin.assert_called_once_with(
        "plugin_name",
        "SR.attach",
        ["--configuration", "key1", "value1", "--configuration", "key2", "value2"],
    )
