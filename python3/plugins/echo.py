#!/usr/bin/env python3

# Simple XenAPI plugin
import time

import XenAPIPlugin

# The 1st argument is the session. This plugin does not use it, hence use _:
def main(_, args):
    if "sleep" in args:
        secs = int(args["sleep"])
        time.sleep(secs)
    return "args were: %s" % (repr(args))


if __name__ == "__main__":
    XenAPIPlugin.dispatch({"main": main})
