#!/usr/bin/env python3

# Simple XenAPI plugin
import time

import XenAPIPlugin

#First argument passed in session which is not used ,using _ to ignore
def main(_, args):
    if "sleep" in args:
        secs = int(args["sleep"])
        time.sleep(secs)
    return "args were: %s" % (repr(args))


if __name__ == "__main__":
    XenAPIPlugin.dispatch({"main": main})
