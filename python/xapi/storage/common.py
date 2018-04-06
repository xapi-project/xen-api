#!/usr/bin/env python

from xapi.storage import log
import xapi
import subprocess


# [call dbg cmd_args] executes [cmd_args]
# if [error] and exit code != expRc, log and throws a BackendError
# if [simple], returns only stdout


def call(dbg, cmd_args, error=True, simple=True, expRc=0):
    log.debug('{}: Running cmd {}'.format(dbg, cmd_args))
    proc = subprocess.Popen(
        cmd_args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        close_fds=True)
    stdout, stderr = proc.communicate()
    if error and proc.returncode != expRc:
        log.error('{}: {} exitted with code {}: {}'.format(
            dbg, " ".join(cmd_args), proc.returncode, stderr))
        raise xapi.InternalError('{} exitted with non-zero code {}: {}'.format(
            " ".join(cmd_args), proc.returncode, stderr))
    if simple:
        return stdout
    return stdout, stderr, proc.returncode
