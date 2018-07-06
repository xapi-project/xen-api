"""
Test for the generated Python commadline code.
Currently it just calls the only function in the interface called "add".
"""

import os
import sys

import bindings


class CalcImplementation(object):
    """
    Implementation of the test interface "Calc" in test_pythongen.ml
    """

    def __init__(self):
        pass

    def add(self, int1, int2):
        """Add two numbers"""
        return int1 + int2

    def land(self, bool1, bool2):
        """Logical and"""
        return bool1 and bool2

    def noop(self, bool1):
        """Do nothing"""
        pass


def _call_calc_command():
    """Parse the arguments and call the required command"""
    cmd = bindings.Calc_commandline(CalcImplementation())
    base = os.path.basename(sys.argv[0])
    if base == "Calc.add":
        cmd.add()
    elif base == "Calc.land":
        cmd.land()
    elif base == "Calc.noop":
        cmd.noop()
    else:
        raise bindings.Unimplemented(base)


if __name__ == "__main__":
    _call_calc_command()
