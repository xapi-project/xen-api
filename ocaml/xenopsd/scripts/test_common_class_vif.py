"""Test ocaml/xenopsd/scripts/common.VIF.get_locking_mode()"""

from unittest.mock import patch  # to check the arguments passed to send_to_syslog()

import pytest  # for pytest.parametrize to run the same test with different parameters

import common  # Tested module


# Mock class to simulate the object containing the get_locking_mode method
class VifMockSubclass(common.VIF):
    """Mock class to simulate a VIF object containing the get_locking_mode method"""

    def __init__(self, json):  # pylint: disable=super-init-not-called
        """Do not call the parent constructor, it would open a file"""
        self.json = json

    def get_mac(self):
        return "00:11:22:33:44:55"  # Expected MAC address


@pytest.mark.parametrize(
    # Call the test case 3 times with two args:
    # inp: input for VIF.get_locking_mode()
    # expected_output: expected output of the get_locking_mode method
    # Asserted with:
    # assert expected_output == get_locking_mode(input)
    "input_params, expected_output",
    [
        # Happy path tests
        (
            # locked
            {  # input
                "locking_mode": [
                    "locked",
                    {"ipv4": ["1.1.1.1"], "ipv6": ["fe80::1"]},
                ]
            },  # expected output
            {
                "mac": "00:11:22:33:44:55",
                "locking_mode": "locked",
                "ipv4_allowed": ["1.1.1.1"],
                "ipv6_allowed": ["fe80::1"],
            },
        ),
        (
            # unlocked
            {"locking_mode": "unlocked"},
            {
                "mac": "00:11:22:33:44:55",
                "locking_mode": "unlocked",
                "ipv4_allowed": [],
                "ipv6_allowed": [],
            },
        ),
        (
            {},  # no locking_mode
            {
                "mac": "00:11:22:33:44:55",
                "locking_mode": "",
                "ipv4_allowed": [],
                "ipv6_allowed": [],
            },
        ),
    ],
)
def test_get_locking_mode(input_params, expected_output):
    """Test VIF.get_locking_mode() using the VIF class test parameters defined above."""

    # Act: Get the locking mode configuration for the input params from the VIF object:
    with patch("common.send_to_syslog") as send_to_syslog:
        test_result = VifMockSubclass(input_params).get_locking_mode()

    # Assert the expected output and the expected call to send_to_syslog():
    assert test_result == expected_output
    send_to_syslog.assert_called_once_with(
        "Got locking config: " + repr(expected_output)
    )
