# Copyright (c) Citrix Systems, Inc.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#  1) Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#
#  2) Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# --------------------------------------------------------------------
# Parts of this file are based upon xmlrpclib.py, the XML-RPC client
# interface included in the Python distribution.
#
# Copyright (c) 1999-2002 by Secret Labs AB
# Copyright (c) 1999-2002 by Fredrik Lundh
#
# By obtaining, using, and/or copying this software and/or its
# associated documentation, you agree that you have read, understood,
# and will comply with the following terms and conditions:
#
# Permission to use, copy, modify, and distribute this software and
# its associated documentation for any purpose and without fee is
# hereby granted, provided that the above copyright notice appears in
# all copies, and that both that copyright notice and this permission
# notice appear in supporting documentation, and that the name of
# Secret Labs AB or the author not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# SECRET LABS AB AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD
# TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANT-
# ABILITY AND FITNESS.  IN NO EVENT SHALL SECRET LABS AB OR THE AUTHOR
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
# OF THIS SOFTWARE.
# --------------------------------------------------------------------

import gettext
import socket
import sys

if sys.version_info[0] == 2:
    import httplib as httplib
    import xmlrpclib as xmlrpclib
else:
    import http.client as httplib
    import xmlrpc.client as xmlrpclib


translation = gettext.translation('xen-xm', fallback = True)

API_VERSION_1_1 = '1.1'
API_VERSION_1_2 = '1.2'

class Failure(Exception):
    def __init__(self, details):
        self.details = details

    def __str__(self):
        try:
            return str(self.details)
        except Exception as exn:
            msg = "Xen-API failure: %s" % exn
            sys.stderr.write(msg)
            return msg

    def _details_map(self):
        return dict([(str(i), self.details[i])
                     for i in range(len(self.details))])


# Just a "constant" that we use to decide whether to retry the RPC
_RECONNECT_AND_RETRY = object()

class UDSHTTPConnection(httplib.HTTPConnection):
    """HTTPConnection subclass to allow HTTP over Unix domain sockets. """
    def connect(self):
        path = self.host.replace("_", "/")
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.sock.connect(path)

class UDSTransport(xmlrpclib.Transport):
    def add_extra_header(self, key, value):
        self._extra_headers += [ (key,value) ]
    def make_connection(self, host):
        return UDSHTTPConnection(host)

def notimplemented(name, *args, **kwargs):
    raise NotImplementedError("XMLRPC proxies do not support python magic methods", name, *args, **kwargs)

class Session(xmlrpclib.ServerProxy):
    """A server proxy and session manager for communicating with xapi using
    the Xen-API.

    Example:

    session = Session('http://localhost/')
    session.login_with_password('me', 'mypassword', '1.0', 'xen-api-scripts-xenapi.py')
    session.xenapi.VM.start(vm_uuid)
    session.xenapi.session.logout()
    """

    def __init__(self, uri, transport=None, encoding=None, verbose=0,
                 allow_none=1, ignore_ssl=False):
        if sys.version_info >= (3,):
            verbose = verbose != 0
            allow_none = allow_none != 0
        # Fix for CA-172901 (+ Python 2.4 compatibility)
        # Fix for context=ctx ( < Python 2.7.9 compatibility)
        if not (sys.version_info[0] <= 2 and sys.version_info[1] <= 7 and sys.version_info[2] <= 9 ) \
                and ignore_ssl:
            import ssl
            ctx = ssl._create_unverified_context()
            xmlrpclib.ServerProxy.__init__(self, uri, transport, encoding,
                                           verbose, allow_none, context=ctx)
        else:
            xmlrpclib.ServerProxy.__init__(self, uri, transport, encoding,
                                           verbose, allow_none)
        self.transport = transport
        self._session = None
        self.last_login_method = None
        self.last_login_params = None
        self.API_version = API_VERSION_1_1


    def xenapi_request(self, methodname, params):
        if methodname.startswith('login'):
            self._login(methodname, params)
            return None
        elif methodname == 'logout' or methodname == 'session.logout':
            self._logout()
            return None
        else:
            retry_count = 0
            while retry_count < 3:
                full_params = (self._session,) + params
                result = _parse_result(getattr(self, methodname)(*full_params))
                if result is _RECONNECT_AND_RETRY:
                    retry_count += 1
                    if self.last_login_method:
                        self._login(self.last_login_method,
                                    self.last_login_params)
                    else:
                        raise xmlrpclib.Fault(401, 'You must log in')
                else:
                    return result
            raise xmlrpclib.Fault(
                500, 'Tried 3 times to get a valid session, but failed')

    def _login(self, method, params):
        try:
            result = _parse_result(
                getattr(self, 'session.%s' % method)(*params))
            if result is _RECONNECT_AND_RETRY:
                raise xmlrpclib.Fault(
                    500, 'Received SESSION_INVALID when logging in')
            self._session = result
            self.last_login_method = method
            self.last_login_params = params
            self.API_version = self._get_api_version()
        except socket.error as e:
            if e.errno == socket.errno.ETIMEDOUT:  # pytype: disable=module-attr
                raise xmlrpclib.Fault(504, 'The connection timed out')
            else:
                raise e

    def _logout(self):
        try:
            if sys.version_info >= (3,):
                raise NotImplementedError
            if self.last_login_method.startswith("slave_local"):
                return _parse_result(self.session.local_logout(self._session))
            else:
                return _parse_result(self.session.logout(self._session))
        finally:
            self._session = None
            self.last_login_method = None
            self.last_login_params = None
            self.API_version = API_VERSION_1_1

    def _get_api_version(self):
        pool = self.xenapi.pool.get_all()[0]
        host = self.xenapi.pool.get_master(pool)
        major = self.xenapi.host.get_API_version_major(host)
        minor = self.xenapi.host.get_API_version_minor(host)
        return "%s.%s"%(major,minor)

    def __getattr__(self, name):
        if name == 'handle':
            return self._session
        elif name == 'xenapi':
            return _Dispatcher(self.API_version, self.xenapi_request, None)
        elif name.startswith('login') or name.startswith('slave_local'):
            return lambda *params: self._login(name, params)
        elif name == 'logout':
            return _Dispatcher(self.API_version, self.xenapi_request, "logout")
        elif name.startswith('__') and name.endswith('__'):
            return lambda *args, **kwargs: notimplemented(name, args, kwargs)
        else:
            return xmlrpclib.ServerProxy.__getattr__(self, name)

def xapi_local():
    return Session("http://_var_lib_xcp_xapi/", transport=UDSTransport())

def _parse_result(result):
    if type(result) != dict or 'Status' not in result:
        raise xmlrpclib.Fault(500, 'Missing Status in response from server' + result)
    if result['Status'] == 'Success':
        if 'Value' in result:
            return result['Value']
        else:
            raise xmlrpclib.Fault(500,
                                  'Missing Value in response from server')
    else:
        if 'ErrorDescription' in result:
            if result['ErrorDescription'][0] == 'SESSION_INVALID':
                return _RECONNECT_AND_RETRY
            else:
                raise Failure(result['ErrorDescription'])
        else:
            raise xmlrpclib.Fault(
                500, 'Missing ErrorDescription in response from server')


# Based upon _Method from xmlrpclib.
class _Dispatcher:
    def __init__(self, API_version, send, name):
        self.__API_version = API_version
        self.__send = send
        self.__name = name

    def __repr__(self):
        if self.__name:
            return '<XenAPI._Dispatcher for %s>' % self.__name
        else:
            return '<XenAPI._Dispatcher>'

    def __getattr__(self, name):
        if self.__name is None:
            return _Dispatcher(self.__API_version, self.__send, name)
        else:
            return _Dispatcher(self.__API_version, self.__send, "%s.%s" % (self.__name, name))

    def __call__(self, *args):
        return self.__send(self.__name, args)
