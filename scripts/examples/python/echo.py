#!/usr/bin/env python

# Simple XenAPI plugin
import XenAPIPlugin, time

def main(session, args):
    if "sleep" in args:
        secs = int(args["sleep"])
        time.sleep(secs)
    return "args were: %s" % (repr(args))

if __name__ == "__main__":
    XenAPIPlugin.dispatch({"main": main})


