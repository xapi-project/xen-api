API for contacting custom services
==================================

Summary
-------

This API allows you to interact securely with a custom service that has been
installed in a XenServer/XCP system.

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

    session.login_with_password(username, password)

Once a valid session_id has been acquired, a client may issue
  1. HTTP GET
  2. HTTP PUT
  3. HTTP POST

requests to the URI:

    /services/plugin/<well known service name>

with either the following key/value pair in a query parameter or set as a cookie:

    session_id=<session_id>

The following HTTP error codes may be returned by the XS management software:
  * if the service is not running then an HTTP 404 will be returned
  * if the session_id is not present or the session not recognised, then an HTTP 403
    forbidden will be returned
  * if the service is actually running on a different IP address (in a configuration
    where the XenServer has multiple management IP addresses), an HTTP 302 redirect
    will be issued. Clients are expected to follow the redirect.

The custom service may return any valid HTTP response, whether a success or failure.

Example code
------------

TBD

