"""
Stub for the XenAPI module: https://xapi-project.github.io/xen-api/overview.html
"""


import http.client as httplib
import xmlrpc.client as xmlrpclib
from _typeshed import Incomplete as Incomplete

translation: Incomplete
API_VERSION_1_1: str
API_VERSION_1_2: str


class Failure(Exception):
    details: Incomplete

    def __init__(self, details) -> None: ...


class UDSHTTPConnection(httplib.HTTPConnection):
    sock: Incomplete

    def connect(self) -> None: ...


class UDSTransport(xmlrpclib.Transport):
    def add_extra_header(self, key, value) -> None: ...

    # def make_connection(self, host) -> None: ...


def notimplemented(name, *args, **kwargs) -> None: ...


class _Dispatcher:
    """A dispatcher for the Xen-API. It is used to call methods on the server"""
    def __init__(self, API_version, send, name) -> None: ...
    def __getattr__(self, name) -> None: ...
    def __call__(self, *args) -> None: ...
    def login_with_password(self, username, password, version, client_name) -> None:
        """Authenticate the session with the XenAPI server."""
    def logout(self) -> None:
        """End the session with the XenAPI server."""
    session: Incomplete
    secret: Incomplete
    SR: Incomplete
    PBD: Incomplete
    pool: Incomplete
    VM: Incomplete


class Session(xmlrpclib.ServerProxy):
    """A server proxy and session manager for communicating with xapi using
    the Xen-API.

    Example:

    session = Session('http://localhost/')
    session.login_with_password('me', 'mypassword', '1.0', 'xen-api-scripts-xenapi.py')
    session.xenapi.VM.start(vm_uuid)
    session.xenapi.session.logout()
    """

    transport: Incomplete
    last_login_method: Incomplete
    last_login_params: Incomplete
    API_version: Incomplete
    xenapi: _Dispatcher

    def __init__(
        self,
        uri,
        transport: Incomplete | None = ...,
        encoding: Incomplete | None = ...,
        verbose: int = ...,
        allow_none: int = ...,
        ignore_ssl: bool = ...,
    ) -> None: ...
    def xenapi_request(self, methodname, params) -> None: ...

    # def __getattr__(self, name) -> None: ...


def xapi_local() -> Session: ...
