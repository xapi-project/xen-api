#!/usr/bin/python

import xmlrpclib
server = xmlrpclib.Server("http://localhost:8086");
session = server.Session.do_login_with_password("user", "passwd", "1.0", "xen-api-unpause-vm.py")['Value']
server.VM.do_unpause(session, '7366a41a-e50e-b891-fa0c-ca5b4d2e3f1c')
