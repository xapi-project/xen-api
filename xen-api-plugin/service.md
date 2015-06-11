API for contacting custom services
==================================

Summary
-------

This API allows you to interact securely and efficiently with a custom service
that has been installed in a XenServer/XCP system. This is an extension of
the existing "XenAPI plugin" mechanism which achieves lower overhead, by
avoiding calling fork() and exec() per request.

All requests to communicate with the custom service are authenticated by
the XenServer/XCP host before the request is forwarded.
In the current version of this API, individual HTTP requests are proxied,
In some future version we will support high-bandwidth communication by
passing authenticated channels to the custom service to avoid unnecessary
data copying.

Managing the lifecycle of the service itself (e.g. installing, uninstalling,
patching, upgrading) is all out of scope of this document.

Version history
---------------

    Date       State
    ----       -----
    2013-1-13  Draft

_Draft_: this API is not considered stable and is likely to change between
software versions and between hotfixes.

API description
---------------

The custom service should be installed into domain 0. It should be configured
to start on system boot. It should accept connections on the unix domain socket:

    /var/xapi/plugin/<well known service name>

where <well known service name> is chosen to be well-known and not clash
with anyone else's name. Including a unique organisation identifier is recommended.

Client applications who wish to invoke operations on this custom service
should first use the XenAPI to acquire a valid *session_id*, for example
using the XenAPI

    session.login_with_password(username, password, version, originator)

Once a valid session_id has been acquired, a client may issue
  1. HTTP GET
  2. HTTP PUT
  3. HTTP POST

requests to the URI:

    /services/plugin/<well known service name>

with either the following key/value pair in a query parameter or set as a cookie:

    session_id=<session_id>

The following HTTP error codes will be returned by the XS management software (in
priority order):
  * HTTP 403 forbidden: if a valid session_id is not found
  * HTTP 404 not found: if the session is valid but the named service isn't running
  * HTTP 302 redirect: if the service is running on a different IP/URI. Clients
    are expected to follow the redirect.

Once the request has been properly authenticated and forwarded, the custom service
may return any valid HTTP response, whether a success or failure.

Example code
------------

TBD

