"""
Test for the generated Python commadline code.
Currently it just calls the only function in the interface called "add".
"""

import os
import sys

import bindings


def _call_calc_command():
    """Parse the arguments and call the required command"""
    cmd = bindings.Calc_commandline(bindings.Calc_test())
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
