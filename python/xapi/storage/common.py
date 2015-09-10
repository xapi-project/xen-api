#!/usr/bin/env python

from xapi.storage import log
import xapi
import subprocess


# [call dbg cmd_args] executes [cmd_args]
# if [error] and exit code != expRc, log and throws a BackendError
# if [simple], returns only stdout


def call(dbg, cmd_args, error=True, simple=True, expRc=0):
    log.debug("%s: Running cmd %s" % (dbg, cmd_args))
    p = subprocess.Popen(
        cmd_args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        close_fds=True)
    stdout, stderr = p.communicate()
    if error and p.returncode != expRc:
        log.error("%s: %s exitted with code %d: %s" %
                  (dbg, " ".join(cmd_args), p.returncode, stderr))
        raise xapi.InternalError("%s exitted with non-zero code %d: %s"
                                 % (" ".join(cmd_args), p.returncode, stderr))
    if simple:
        return stdout
    return stdout, stderr, p.returncode
