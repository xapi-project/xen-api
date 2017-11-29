# Wire Protocol for Remote API Calls

API calls are sent over a network to a Xen-enabled host using an RPC protocol.
Here we describe how the higher-level types used in our API Reference are mapped
to primitive RPC types, covering the two supported wire formats
[XML-RPC](http://xmlrpc.scripting.com/spec.html) and [JSON-RPC](http://www.jsonrpc.org).

## XML-RPC Protocol

We specify the signatures of API functions in the following style:

```python
(VM ref set)  VM.get_all()
```

This specifies that the function with name `VM.get_all` takes
no parameters and returns a `set` of `VM ref`.
These types are mapped onto XML-RPC types in a straight-forward manner:

* the types `float`, `bool`, `datetime`, and `string` map directly to the XML-RPC
  `<double>`, `<boolean>`, `<dateTime.iso8601>`, and `<string>` elements.

* all `ref` types are opaque references, encoded as the
  XML-RPC's `<string>` type. Users of the API should not make assumptions
  about the concrete form of these strings and should not expect them to
  remain valid after the client's session with the server has terminated.

* fields named `uuid` of type `string` are mapped to
  the XML-RPC `<string>` type. The string itself is the OSF
  DCE UUID presentation format (as output by `uuidgen`).

* `int` is assumed to be 64-bit in our API and is encoded as a string
  of decimal digits (rather than using XML-RPC's built-in 32-bit `<i4>` type).

* values of `enum` types are encoded as strings. For example, the value
  `destroy` of `enum on_normal_exit`, would be conveyed as:

```xml
    <value><string>destroy</string></value>
```

* for all our types, `t`, our type `t set` simply maps to XML-RPC's `<array>`
  type, so, for example, a value of type `string set` would be transmitted like
  this:

```xml
    <array>
      <data>
        <value><string>CX8</string></value>
        <value><string>PSE36</string></value>
        <value><string>FPU</string></value>
      </data>
    </array>
```

* for types `k` and `v`, our type `(k -> v) map` maps onto an
  XML-RPC `<struct>`, with the key as the name of the struct.  Note that the
  `(k -> v) map` type is only valid when `k` is a `string`, `ref`, or
  `int`, and in each case the keys of the maps are stringified as
  above. For example, the `(string -> float) map` containing the mappings
  _Mike &#45;&gt; 2.3_ and _John &#45;&gt; 1.2_ would be represented as:

```xml
    <value>
      <struct>
        <member>
          <name>Mike</name>
          <value><double>2.3</double></value>
        </member>
        <member>
          <name>John</name>
          <value><double>1.2</double></value>
        </member>
      </struct>
    </value>
```

* our `void` type is transmitted as an empty string.

### XML-RPC Return Values and Status Codes

The return value of an RPC call is an XML-RPC `<struct>`.

* The first element of the struct is named `Status`; it contains a string value
  indicating whether the result of the call was a `Success` or a `Failure`.

If the `Status` is `Success` then the struct contains a second element named
`Value`:

* The element of the struct named `Value` contains the function's return value.

If the `Status` is `Failure` then the struct contains a second element named
`ErrorDescription`:

* The element of the struct named `ErrorDescription` contains an array of string
  values. The first element of the array is an error code; the rest of the
  elements are strings representing error parameters relating to that code.

For example, an XML-RPC return value from the `host.get_resident_VMs` function
may look like this:

```xml
    <struct>
       <member>
         <name>Status</name>
         <value>Success</value>
       </member>
       <member>
          <name>Value</name>
          <value>
            <array>
               <data>
                 <value>81547a35-205c-a551-c577-00b982c5fe00</value>
                 <value>61c85a22-05da-b8a2-2e55-06b0847da503</value>
                 <value>1d401ec4-3c17-35a6-fc79-cee6bd9811fe</value>
               </data>
            </array>
         </value>
       </member>
    </struct>
```

## JSON-RPC Protocol

We specify the signatures of API functions in the following style:

```python
(VM ref set)  VM.get_all()
```

This specifies that the function with name `VM.get_all` takes no parameters and
returns a `set` of `VM ref`. These types are mapped onto JSON-RPC types in the
following manner:

* the types `float` and `bool` map directly to the JSON types `number` and
  `boolean`, while `datetime` and `string` are represented as the JSON `string`
  type.

* all `ref` types are opaque references, encoded as the JSON `string` type.
  Users of the API should not make assumptions about the concrete form of these
  strings and should not expect them to remain valid after the client's session
  with the server has terminated.

* fields named `uuid` of type `string` are mapped to the JSON `string` type. The
  string itself is the OSF DCE UUID presentation format (as output by `uuidgen`).

* `int` is assumed to be 64-bit in our API and is encoded as a  JSON `number`
  without decimal point or exponent, preserved as a string.

* values of `enum` types are encoded as the JSON `string` type. For example, the
  value `destroy` of `enum on_normal_exit`, would be conveyed as:

```xml
  "destroy"
```

* for all our types, `t`, our type `t set` simply maps to the JSON `array`
  type, so, for example, a value of type `string set` would be transmitted like
  this:

```json
  [ "CX8", "PSE36", "FPU" ]
```

* for types `k` and `v`, our type `(k -> v) map` maps onto a JSON object which
  contains members with name `k` and value `v`. Note that the
  `(k -> v) map` type is only valid when `k` is a `string`, `ref`, or
  `int`, and in each case the keys of the maps are stringified as
  above. For example, the `(string -> float) map` containing the mappings
  _Mike &#45;&gt; 2.3_ and _John &#45;&gt; 1.2_ would be represented as:

```json
  {
    "Mike": 2.3,
    "John": 1.2
  }
```

* our `void` type is transmitted as an empty string.

Both versions 1.0 and 2.0 of the JSON-RPC wire format are recognised and,
depending on your client library, you can use either of them.

### JSON-RPC v1.0

#### JSON-RPC v1.0 Requests

An API call is represented by sending a single JSON object to the server, which
contains the members `method`, `params`, and `id`.

* `method`: A JSON `string` containing the name of the function to be invoked.

* `params`: A JSON `array` of values, which represents the parameters of the
  function to be invoked.

* `id`: A JSON `string` or `integer` representing the call id. Note that,
  diverging from the JSON-RPC v1.0 specification the API does not accept
  _notification_ requests (requests without responses), i.e. the id cannot be
  `null`.

For example, a JSON-RPC v1.0 request to retrieve the resident VMs of a host may
look like this:

```json
  {
    "method": "host.get_resident_VMs",
    "params": [
      "OpaqueRef:74f1a19cd-b660-41e3-a163-10f03e0eae67",
      "OpaqueRef:08c34fc9-f418-4f09-8274-b9cb25cd8550"
    ],
    "id": "xyz"
  }
```

In the above example, the first element of the `params` array is the reference
of the open session to the host, while the second is the host reference.

#### JSON-RPC v1.0 Return Values

The return value of a JSON-RPC v1.0 call is a single JSON object containing
the members `result`, `error`, and `id`.

* `result`: If the call is successful, it is a JSON value (`string`, `array`
  etc.) representing the return value of the invoked function. If an error has
  occurred, it is `null`.

* `error`: If the call is successful, it is `null`. If the call has failed, it
  a JSON `array` of `string` values. The first element of the array is an error
  code; the remainder of the array are strings representing error parameters
  relating to that code.

* `id`: The call id. It is a JSON `string` or `integer` and it is the same id
  as the request it is responding to.

For example, a JSON-RPC v1.0 return value from the `host.get_resident_VMs`
function may look like this:

```json
  {
    "result": [
        "OpaqueRef:604f51e7-630f-4412-83fa-b11c6cf008ab",
        "OpaqueRef:670d08f5-cbeb-4336-8420-ccd56390a65f"
    ],
    "error": null,
    "id": "xyz"
  }
```

while the return value of the same call made on a logged out session may look
like this:

```json
  {
    "result": null,
    "error": [
        "SESSION_INVALID",
        "OpaqueRef:93f1a23cd-a640-41e3-b163-10f86e0eae67"
    ],
    "id": "xyz"
  }
```

### JSON-RPC v2.0

#### JSON-RPC v2.0 Requests

An API call is represented by sending a single JSON object to the server, which
contains the members `jsonrpc`, `method`, `params`, and `id`.

* `jsonrpc`: A JSON `string` specifying the version of the JSON-RPC protocol. It
  is exactly "2.0".

* `method`: A JSON `string` containing the name of the function to be invoked.

* `params`: A JSON `array` of values, which represents the parameters of the
  function to be invoked. Although the JSON-RPC v2.0 specification allows this
  member to be ommitted, in practice all API calls accept at least one parameter.

* `id`: A JSON `string` or `integer` representing the call id. Note that,
  diverging from the JSON-RPC v2.0 specification it cannot be null. Neither can
  it be ommitted because the API does not accept _notification_ requests
  (requests without responses).

For example, a JSON-RPC v2.0 request to retrieve the VMs resident on a host may
may look like this:

```json
  {
    "jsonrpc": "2.0",
    "method": "host.get_resident_VMs",
    "params": [
      "OpaqueRef:c90cd28f-37ec-4dbf-88e6-f697ccb28b39",
      "OpaqueRef:08c34fc9-f418-4f09-8274-b9cb25cd8550"
    ],
    "id": 3
 }
```

As before, the first element of the `parameter` array is the reference
of the open session to the host, while the second is the host reference.

#### JSON-RPC v2.0 Return Values

The return value of a JSON-RPC v2.0 call is a single JSON object containing the
members `jsonrpc`, either `result` or `error` depending on the outcome of the
call, and `id`.

* `jsonrpc`: A JSON `string` specifying the version of the JSON-RPC protocol. It
  is exactly "2.0".

* `result`: If the call is successful, it is a JSON value (`string`, `array` etc.)
  representing the return value of the invoked function. If an error has
  occurred, it does not exist.

* `error`: If the call is successful, it does not exist. If the call has failed,
  it is a single structured JSON object (see below).

* `id`: The call id. It is a JSON `string` or `integer` and it is the same id
  as the request it is responding to.

The `error` object contains the members `code`, `message`, and `data`.

* `code`: The API does not make use of this member and only retains it for
  compliance with the JSON-RPC v2.0 specification. It is a JSON `integer`
  which has a non-zero value.

* `message`: A JSON `string` representing an API error code.

* `data`: A JSON array of `string` values representing error parameters
  relating to the aforementioned API error code.

For example, a JSON-RPC v2.0 return value from the `host.get_resident_VMs`
function may look like this:

```json
  {
    "jsonrpc": "2.0",
    "result": [
        "OpaqueRef:604f51e7-630f-4412-83fa-b11c6cf008ab",
        "OpaqueRef:670d08f5-cbeb-4336-8420-ccd56390a65f"
    ],
    "id": 3
  }
```

while the return value of the same call made on a logged out session may look
like this:

```json
  {
    "jsonrpc": "2.0",
    "error": {
        "code": 1,
        "message": "SESSION_INVALID",
        "data": [
            "OpaqueRef:c90cd28f-37ec-4dbf-88e6-f697ccb28b39"
        ]
    },
    "id": 3
  }
```

## Note on References vs UUIDs

References are opaque types - encoded as XML-RPC and JSON-RPC strings on the
wire - understood only by the particular server which generated them. Servers
are free to choose any concrete representation they find convenient; clients
should not make any assumptions or attempt to parse the string contents.
References are not guaranteed to be permanent identifiers for objects; clients
should not assume that references generated during one session are valid for any
future session. References do not allow objects to be compared for equality. Two
references to the same object are not guaranteed to be textually identical.

UUIDs are intended to be permanent names for objects. They are
guaranteed to be in the OSF DCE UUID presentation format (as output by `uuidgen`).
Clients may store UUIDs on disk and use them to lookup objects in subsequent sessions
with the server. Clients may also test equality on objects by comparing UUID strings.

The API provides mechanisms for translating between UUIDs and opaque references.
Each class that contains a UUID field provides:

* A `get_by_uuid` method that takes a UUID and returns an opaque reference
 to the server-side object that has that UUID;

* A `get_uuid` function (a regular "field getter" RPC) that takes an opaque reference
 and returns the UUID of the server-side object that is referenced by it.

## Making RPC Calls

### Transport Layer

The following transport layers are currently supported:

* HTTP/HTTPS for remote administration
* HTTP over Unix domain sockets for local administration

### Session Layer

The RPC interface is session-based; before you can make arbitrary RPC calls
you must login and initiate a session. For example:

```python
   (session ref) session.login_with_password(string uname, string pwd,
                   string version, string originator)
```

where `uname` and `password` refer to your username and password, as defined by
the Xen administrator, while `version` and `originator` are optional. The
`session ref` returned by `session.login_with_password` is passed
to subequent RPC calls as an authentication token. Note that a session
reference obtained by a login request to the XML-RPC backend can be used in
subsequent requests to the JSON-RPC backend, and vice-versa.

A session can be terminated with the `session.logout` function:

```python
   void  session.logout(session ref session_id)
```

### Synchronous and Asynchronous Invocation

Each method call (apart from methods on the `Session` and `Task` objects and
"getters" and "setters" derived from fields) can be made either synchronously or
asynchronously. A synchronous RPC call blocks until the
return value is received; the return value of a synchronous RPC call is
exactly as specified above.

Only synchronous API calls are listed explicitly in this document.
All their asynchronous counterparts are in the special `Async` namespace.
For example, the synchronous call `VM.clone(...)` has an asynchronous
counterpart, `Async.VM.clone(...)`, that is non-blocking.

Instead of returning its result directly, an asynchronous RPC call
returns an identifier of type `task ref` which is subsequently used
to track the status of a running asynchronous RPC.

Note that an asychronous call may fail immediately, before a task has even been
created. When using the XML-RPC wire protocol, this eventuality is represented
by wrapping the returned `task ref` in an XML-RPC struct with a `Status`,
`ErrorDescription`, and `Value` fields, exactly as specified above; the
`task ref` is provided in the `Value` field if `Status` is set to `Success`.
When using the JSON-RPC protocol, the `task ref` is wrapped in a response JSON
object as specified above and it is provided by the value of the `result` member
of a successful call.

The RPC call

```python
    (task ref set)  Task.get_all(session ref session_id)
```

returns a set of all task identifiers known to the system. The status (including any
returned result and error codes) of these can then be queried by accessing the
fields of the `Task` object in the usual way. Note that, in order to get a
consistent snapshot of a task's state, it is advisable to call the `get_record`
function.

## Example interactive session

This section describes how an interactive session might look, using python
XML-RPC and JSON-RPC client libraries.

First, initialise python:

```bash
$ python2.7
>>>
```

### Using the XML-RPC Protocol

Import the library `xmlrpclib` and create a
python object referencing the remote server as shown below:

```python
>>> import xmlrpclib
>>> xen = xmlrpclib.Server("https://localhost:443")
```

Acquire a session reference by logging in with a username and password; the
session reference is returned under the key `Value` in the resulting dictionary
(error-handling ommitted for brevity):

```python
>>> session = xen.session.login_with_password("user", "passwd",
...                                           "version", "originator")['Value']
```

This is what the call looks like when serialised

```xml
<?xml version='1.0'?>
<methodCall>
    <methodName>session.login_with_password</methodName>
    <params>
        <param><value><string>user</string></value></param>
        <param><value><string>passwd</string></value></param>
        <param><value><string>version</string></value></param>
        <param><value><string>originator</string></value></param>
    </params>
</methodCall>
```

Next, the user may acquire a list of all the VMs known to the system (note the
call takes the session reference as the only parameter):

```python
>>> all_vms = xen.VM.get_all(session)['Value']
>>> all_vms
['OpaqueRef:1', 'OpaqueRef:2', 'OpaqueRef:3', 'OpaqueRef:4' ]
```

The VM references here have the form `OpaqueRef:X` (though they may not be
that simple in reality) and you should treat them as opaque strings.
_Templates_ are VMs with the `is_a_template` field set to `true`. We can
find the subset of template VMs using a command like the following:

```python
>>> all_templates = filter(lambda x: xen.VM.get_is_a_template(session, x)['Value'],
                              all_vms)
```

Once a reference to a VM has been acquired, a lifecycle operation may be invoked:

```python
>>> xen.VM.start(session, all_templates[0], False, False)
{'Status': 'Failure', 'ErrorDescription': ['VM_IS_TEMPLATE', 'OpaqueRef:X']}
```

In this case the `start` message has been rejected, because the VM is
a template, and so an error response has been returned.  These high-level
errors are returned as structured data (rather than as XML-RPC faults),
allowing them to be internationalised.

Rather than querying fields individually, whole _records_ may be returned at once.
To retrieve the record of a single object as a python dictionary:

```python
>>> record = xen.VM.get_record(session, all_templates[0])['Value']
>>> record['power_state']
'Halted'
>>> record['name_label']
'Windows 10 (64-bit)'
```

To retrieve all the VM records in a single call:

```python
>>> records = xen.VM.get_all_records(session)['Value']
>>> records.keys()
['OpaqueRef:1', 'OpaqueRef:2', 'OpaqueRef:3', 'OpaqueRef:4' ]
>>> records['OpaqueRef:1']['name_label']
'Red Hat Enterprise Linux 7'
```

### Using the JSON-RPC Protocol

For this example we are making use of the package `python-jsonrpc` due to its
simplicity, although other packages can also be used.

First, import the library `pyjsonrpc` and create the object referencing the
remote server as follows:

```python
>>> import pyjsonrpc
>>> client = pyjsonrpc.HttpClient(url = "https://localhost/jsonrpc:443")
```

Acquire a session reference by logging in with a username and password; the
library `pyjsonrpc` returns the response's `result` member, which is the session
reference:

```python
>>> session = client.call("session.login_with_password",
...                       "user", "passwd", "version", "originator")
```

`pyjsonrpc` uses the JSON-RPC protocol v2.0, so this is what the serialised
request looks like:

```json
  {
    "jsonrpc": "2.0",
    "method": "session.login_with_password",
    "params": ["user", "passwd", "version", "originator"],
    "id": 0
  }
```

Next, the user may acquire a list of all the VMs known to the system (note the
call takes the session reference as the only parameter):

```python
>>> all_vms = client.call("VM.get_all", session)
>>> all_vms
['OpaqueRef:1', 'OpaqueRef:2', 'OpaqueRef:3', 'OpaqueRef:4' ]
```

The VM references here have the form `OpaqueRef:X` (though they may not be
that simple in reality) and you should treat them as opaque strings.
_Templates_ are VMs with the `is_a_template` field set to `true`. We can
find the subset of template VMs using a command like the following:

```python
>>> all_templates = filter(
...     lambda x: client.call("VM.get_is_a_template", session, x),
        all_vms)
```

Once a reference to a VM has been acquired, a lifecycle operation may be invoked:

```python
>>> from pyjsonrpc import JsonRpcError
>>> try:
...     client.call("VM.start", session, all_templates[0], False, False)
... except JsonRpcError as e:
...     e.message
...     e.data
...
'VM_IS_TEMPLATE'
[ 'OpaqueRef:1', 'start' ]
```

In this case the `start` message has been rejected because the VM is
a template, hence an error response has been returned. These high-level
errors are returned as structured data, allowing them to be internationalised.

Rather than querying fields individually, whole _records_ may be returned at once.
To retrieve the record of a single object as a python dictionary:

```python
>>> record = client.call("VM.get_record", session, all_templates[0])
>>> record['power_state']
'Halted'
>>> record['name_label']
'Windows 10 (64-bit)'
```

To retrieve all the VM records in a single call:

```python
>>> records = client.call("VM.get_all_records", session)
>>> records.keys()
['OpaqueRef:1', 'OpaqueRef:2', 'OpaqueRef:3', 'OpaqueRef:4' ]
>>> records['OpaqueRef:1']['name_label']
'Red Hat Enterprise Linux 7'
```
