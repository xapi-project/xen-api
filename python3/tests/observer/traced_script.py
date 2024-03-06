#!/usr/bin/env python3
"""A simple script to be traced by packages/observer.py as part of tests"""

import sys

# This import causes that more code of packages/observer.py is tested
# pylint: disable-next=import-error,unused-import
import XenAPI  # type: ignore[import-untyped]


class InstrumentMe:
    """A class to be traced by packages/observer.py as part of tests"""

    def print(self) -> "InstrumentMe":
        """A method to be traced by packages/observer.py as part of tests"""

        print("Hello, I am a print() in traced_script.py.")
        return self

    def return_int(self, return_int: str) -> int:
        """A method to be traced by packages/observer.py as part of tests"""
        return int(return_int)


def main(return_code_string: str) -> int:
    """Main of the tested script, to be traced by packages/observer.py."""

    return InstrumentMe().print().return_int(return_code_string)


if __name__ == "__main__":
    # Only use sys.exit(ret) raising SystemExit if the return code is not 0
    # to allow test_observer_as_script() to get the globals of observer.py:
    ret = main(sys.argv[-1])
    if ret:
        sys.exit(ret)
