# Copyright (c) 2013, Citrix Inc.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""Logging support with backwards compatibility for xelogging"""

import fcntl
import os
import os.path
import sys
import traceback
import logging
import logging.handlers
from typing import TYPE_CHECKING, TextIO

from .compat import open_with_codec_handling

if TYPE_CHECKING:
    from typing import IO, Any, Union
    from logging import StreamHandler
    from logging.handlers import RotatingFileHandler
    LoggingStreamHandler = Union[RotatingFileHandler, StreamHandler[IO[Any]], StreamHandler[TextIO]]

LOG = logging.getLogger()
LOG.setLevel(logging.NOTSET)
FORMAT = logging.Formatter(
        "%(levelname)- 9.9s[%(asctime)s] %(message)s",
        "%F %T")

our_handlers = []  # type: list[logging.Handler]

def openLog(lfile, level=logging.INFO):
    # type:(Union[str, TextIO], int) -> bool
    """Add a new file target to be logged to"""

    try:
        # if lfile is a string, assume we need to open() it
        if isinstance(lfile, str):
            h = open_with_codec_handling(lfile, "a")
            if h.isatty():
                # pytype: disable=wrong-arg-types   # False positive in pytype-2023.07.21
                handler = logging.StreamHandler(h)  # type: LoggingStreamHandler
                # pytype: enable=wrong-arg-types
            else:
                h.close()
                handler = logging.handlers.RotatingFileHandler(lfile,
                                                               maxBytes=2**31)
            old = fcntl.fcntl(handler.stream.fileno(), fcntl.F_GETFD)
            fcntl.fcntl(handler.stream.fileno(),
                        fcntl.F_SETFD, old | fcntl.FD_CLOEXEC)

        # or if it is not a string, assume its a file-like object
        else:
            handler = logging.StreamHandler(lfile)

    except Exception:
        if len(LOG.handlers):
            log("Error opening %s as a log output." % str(lfile))
        else:
            sys.stderr.write("Error opening %s as a log output." % str(lfile))
        return False

    handler.setFormatter(FORMAT)
    handler.setLevel(level)
    LOG.addHandler(handler)
    our_handlers.append(handler)
    return True

def closeLogs():
    """Close all logs"""
    handlers_to_remove = list(our_handlers)
    for h in handlers_to_remove:
        our_handlers.remove(h)
        LOG.removeHandler(h)
        h.close()

def logToStdout(level=logging.INFO):
    """Log to stdout"""
    return openLog(sys.stdout, level)

def logToStderr(level=logging.INFO):
    """Log to stderr"""
    return openLog(sys.stderr, level)

def logToSyslog(ident = os.path.basename(sys.argv[0]), level = logging.INFO,
                facility = logging.handlers.SysLogHandler.LOG_USER):
    """Log to syslog"""
    if os.path.exists("/dev/log"):
        syslog = logging.handlers.SysLogHandler("/dev/log", facility)
    else:
        syslog = logging.handlers.SysLogHandler(
            ('localhost', logging.handlers.SYSLOG_UDP_PORT), facility)
    syslog.setLevel(level)
    fmt = logging.Formatter(ident+" %(levelname)s: %(message)s")
    syslog.setFormatter(fmt)
    LOG.addHandler(syslog)

def log(txt):
    """ Write txt to the log(s) """
    LOG.info(txt)

def logException(e):
    """ Formats exception and logs it """
    ex = sys.exc_info()
    err = traceback.format_exception(*ex)
    errmsg = "\n".join([ str(x) for x in e.args ])

    LOG.critical(errmsg)
    LOG.critical(err)

# export the standard logging calls at the module level

def debug(*al, **ad):
    """debug"""
    LOG.debug(*al, **ad)

def info(*al, **ad):
    """info"""
    LOG.info(*al, **ad)

def warning(*al, **ad):
    """warning"""
    LOG.warning(*al, **ad)

def error(*al, **ad):
    """error"""
    LOG.error(*al, **ad)

def critical(*al, **ad):
    """critical"""
    LOG.critical(*al, **ad)
