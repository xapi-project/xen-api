# Test: pytest -v -s ocaml/xcp-rrdd/scripts/rrdd/test_api_wait_until_next_reading.py
"""Parametrized test exercising all conditions in rrdd.API.wait_until_next_reading()"""
import socket
from warnings import catch_warnings as import_without_warnings, simplefilter

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
