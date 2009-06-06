#!/usr/bin/env python
# Copyright (c) 2006-2008 Citrix Systems.
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

import atexit
import cmd
import pprint
import readline
import shlex
import string
import sys

import XenAPI

def logout():
    try:
        session.xenapi.session.logout()
    except:
        pass
atexit.register(logout)

class Shell(cmd.Cmd):
    def __init__(self):
        cmd.Cmd.__init__(self)
        self.identchars = string.ascii_letters + string.digits + '_.'
        self.prompt = "xe> "

    def preloop(self):
        cmd.Cmd.preloop(self)
        readline.set_completer_delims(' ')

    def default(self, line):
        words = shlex.split(line)
        if len(words) > 0:
            res = session.xenapi_request(words[0], tuple(words[1:]))
            if res is not None and res != '':
                pprint.pprint(res)
        return False

    def completedefault(self, text, line, begidx, endidx):
        words = shlex.split(line[:begidx])
        clas, func = words[0].split('.')
        if len(words) > 1 or \
           func.startswith('get_by_') or \
           func == 'get_all':
            return []
        uuids = session.xenapi_request('%s.get_all' % clas, ())
        return [u + " " for u in uuids if u.startswith(text)]

    def emptyline(self):
        pass

    def do_EOF(self, line):
        print
        sys.exit(0)

def munge_types (str):
    if str == "True":
        return True
    elif str == "False":
        return False
    
    try:
        return int(str)
    except:
        return str

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1) 

    if sys.argv[1] <> "-" and len(sys.argv) < 4:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1) 

    if sys.argv[1] <> "-":
        url = sys.argv[1]
        username = sys.argv[2]
        password = sys.argv[3]
        session = XenAPI.Session(url)
        session.xenapi.login_with_password(username, password)
        cmdAt = 4
    else:
        session = XenAPI.xapi_local()
        session.xenapi.login_with_password("", "")
        cmdAt = 2

    # We want to support directly executing the cmd line,
    # where appropriate
    if len(sys.argv) > cmdAt:
        cmd = sys.argv[cmdAt]
        params = [munge_types(x) for x in sys.argv[(cmdAt + 1):]]
        try:
            print >> sys.stdout, session.xenapi_request(cmd, tuple(params))
        except XenAPI.Failure, x:
            print >> sys.stderr, x
            sys.exit(2)
        except Exception, e:
            print >> sys.stderr, e
            sys.exit(3)
        sys.exit(0)
    else:
        Shell().cmdloop('Welcome to the XenServer shell. (Try "VM.get_all")')
