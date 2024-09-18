# Test: pytest -v -s ocaml/xcp-rrdd/scripts/rrdd/test_api_wait_until_next_reading.py
"""Parametrized test exercising all conditions in rrdd.API.wait_until_next_reading()"""
import json
import socket
from io import BytesIO
from struct import pack, unpack
from warnings import catch_warnings as import_without_warnings, simplefilter
from zlib import crc32

# Dependencies:
# pip install pytest-mock
import pytest

# Handle DeprecationWarning from importing imp (it was removed with Python 3.12)
with import_without_warnings():
    simplefilter(action="ignore", category=DeprecationWarning)
    import rrdd


# pylint:disable=no-member,redefined-outer-name  # pytest fixture, see below


@pytest.fixture
def api(mocker):
    """Pytest fixture for creating a rrdd.API() instance"""
    instance = rrdd.API("plugin_id")
    instance.deregister = mocker.Mock()
    return instance


# pylint:disable=too-many-arguments  # pytest parametrized test, see below
@pytest.mark.parametrize(
    "neg_shift, interval, reading, sleep",
    [
        # Happy path tests with various realistic test values
        (None, 5, (6,), 5),  # Test the default value of neg_shift
        (1, 5, (6,), 5),  # to call in the same sleep as neg_shift=1
        (2.25, 5, (6,), 3.75),  # Test neg_shift as float to get sleep as float
        (0.5, 30, (30.5,), 30),  # Also as a fraction of a second
        (2, 120, (122,), 120),  # Test large interval and reading
        # Edge cases
        (11, 5, (1,), 0),  # large neg_shift results in no sleep
        (1, 10, (1,), 0),  # neg_shift equals reading from xcp-rrdd
        (1, 9, (10,), 9),  # wait_time is exactly one cycle
        (1, 10, (9,), 8),  # wait_time is negative, should wrap around
        # Error case
        (1, 7, (socket.error, 6), 5),  # first register raises socket.error
    ],
)
def test_params(api, mocker, neg_shift, interval, reading, sleep, capsys):
    """Test that wait_until_reading_from_xcp_rrd() works with various test values"""
    # Arrange
    api.frequency_in_seconds = interval
    api.lazy_complete_init = mocker.Mock()
    api.register = mocker.Mock(side_effect=reading)
    api.deregister = mocker.Mock()

    # Act
    mock_sleep = mocker.patch("time.sleep")
    if neg_shift is None:
        rrdd.API.wait_until_next_reading(api)
    else:
        rrdd.API.wait_until_next_reading(api, neg_shift)

    # Assert
    mock_sleep.assert_called_with(sleep)

    with capsys.disabled():
        stderr = capsys.readouterr().err
        stdout = capsys.readouterr().out
        if reading[0] is socket.error:
            assert stderr == "Failed to contact xcp-rrdd. Sleeping for 5 seconds ..\n"
        else:
            assert stderr == ""
        assert stdout == ""


def test_api_getter_functions(api):
    """Test that the API getter functions work (and cover the code)"""
    api.header = "header"
    api.path = "path"
    assert api.get_header() == "header"
    assert api.get_path() == "path"


class MockDataSource:
    """Mock class for testing the rrdd.API.update() method"""
    def __init__(self, name, metadata, packed_data):
        self.name = name
        self.metadata = metadata
        self.packed_data = packed_data

    def pack_data(self):
        """Simple substitute for the pack_data() method of the rrdd.DataSource class"""
        return self.packed_data


@pytest.mark.parametrize(
    "data_sources, expected_metadata",
    [
        pytest.param(
            [
                MockDataSource("ds1", {"key1": "value1"}, b"\x00\x01"),
                MockDataSource("ds2", {"key2": "value2"}, b"\x00\x02"),
            ],
            {"key1": "value1", "key2": "value2"},
        ),
        pytest.param(
            [MockDataSource("ds1", {"key1": "value1"}, b"\x00\x01")],
            {"key1": "value1"},
        ),
        pytest.param(
            [],
            {},
        ),
    ],
)
def test_update(
    mocker,
    data_sources,
    expected_metadata,
):
    """Test the update() method of the rrdd.API class"""
    # Arrange
    def checksum(*args):
        """Calculate the CRC32 checksum of the given arguments"""
        return crc32(*args) & 0xFFFFFFFF

    class MockAPI(rrdd.API):
        """Mock API class to test the update() method"""
        def __init__(self):  # pylint: disable=super-init-not-called
            self.dest = BytesIO()
            self.datasources = data_sources

        def pack_data(self, ds: MockDataSource):
            return ds.pack_data()

    testee = MockAPI()
    testee.deregister = mocker.Mock()
    fixed_time = 1234567890
    mocker.patch("time.time", return_value=fixed_time)

    # Act
    testee.update()

    # Assert

    # Read and unpack the header
    testee.dest.seek(0)
    # The header is 20 bytes long and has the following format:
    # 0-11: "DATASOURCES" (12 bytes)
    # 12-15: data_checksum (4 bytes)
    # 16-19: metadata_checksum (4 bytes)
    # 20-23: num_datasources (4 bytes)
    # 24-31: timestamp (8 bytes)
    header_len = len("DATASOURCES") + 4 + 4 + 4 + 8
    header = testee.dest.read(header_len)
    (
        unpacked_data_checksum,
        unpacked_metadata_checksum,
        unpacked_num_datasources,
        unpacked_timestamp,
    ) = unpack(">LLLQ", header[11:])

    # Assert the expected unpacked header value
    assert header.startswith(b"DATASOURCES")
    assert unpacked_num_datasources == len(data_sources)
    assert unpacked_timestamp == fixed_time

    #
    # Assert datasources and the expected data checksum
    #

    # Initialize the expected checksum with the fixed time
    expected_checksum = checksum(pack(">Q", fixed_time))
    # Loop over the datasources and assert the packed data
    testee.dest.seek(header_len)
    # sourcery skip: no-loop-in-tests
    for ds in data_sources:
        packed_data = testee.dest.read(len(ds.pack_data()))
        assert packed_data == ds.pack_data()
        # Update the checksum with the packed data
        expected_checksum = checksum(packed_data, expected_checksum)

    assert unpacked_data_checksum == expected_checksum

    #
    # Assert metadata and the expected metadata checksum
    #
    metadata_length = unpack(">L", testee.dest.read(4))[0]
    metadata_json = testee.dest.read(metadata_length)

    assert json.loads(metadata_json) == {"datasources": expected_metadata}
    assert unpacked_metadata_checksum == checksum(metadata_json)
