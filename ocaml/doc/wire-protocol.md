# Wire Protocol for Remote API Calls

API calls are sent over a network to a Xen-enabled host using
the [XML-RPC](http://xmlrpc.scripting.com/spec.html) protocol. Here we describe
how the higher-level types used in our API Reference are mapped to primitive
XML-RPC types.

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

* for types `k` and `v`, our type `(k → v) map` maps onto an
  XML-RPC `<struct>`, with the key as the name of the struct.  Note that the
  `(k → v) map` type is only valid when `k` is a `string`, `ref`, or
  `int`, and in each case the keys of the maps are stringified as
  above. For example, the `(string → double) map` containing the mappings
  _Mike → 2.3_ and _John → 1.2_ would be represented as:

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

## Note on References vs UUIDs

References are opaque types - encoded as XML-RPC strings on the wire - understood
only by the particular server which generated them. Servers are free to choose
any concrete representation they find convenient; clients should not make any
assumptions or attempt to parse the string contents. References are not guaranteed
to be permanent identifiers for objects; clients should not assume that references
generated during one session are valid for any future session. References do not
allow objects to be compared for equality. Two references to the same object are
not guaranteed to be textually identical.

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

## Return Values and Status Codes

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

## Making XML-RPC Calls

### Transport Layer

The following transport layers are currently supported:

* HTTP/HTTPS for remote administration
* HTTP over Unix domain sockets for local administration

### Session Layer

The XML-RPC interface is session-based; before you can make arbitrary RPC calls
you must login and initiate a session. For example:

```python
   (session ref) session.login_with_password(string uname, string pwd,
                   string version, string originator)
```

where `uname` and `password` refer to your username and password, as defined by
the Xen administrator, while `version` and `originator` are optional. The
`session ref` returned by `session.login_with_password` is passed
to subequent RPC calls as an authentication token.

A session can be terminated with the `session.logout` function:

```python
   void  session.logout(session ref session_id)
```

### Synchronous and Asynchronous invocation

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
to track the status of a running asynchronous RPC. Note that an asychronous
call may fail immediately, before a task has even been created; to
represent this eventuality, the returned `task ref` is wrapped in an XML-RPC
struct with a `Status`, `ErrorDescription`, and `Value` fields, exactly as
specified above.

The `task ref` is provided in the `Value` field if `Status` is set to `Success`.

The RPC call

```python
    (task ref set)  Task.get_all(session ref session_id)
```

returns a set of all task identifiers known to the system. The status (including any
returned result and error codes) of these  can then be queried by accessing the
fields of the `Task` object in the usual way. Note that, in order to get a
consistent snapshot of a task's state, it is advisable to call the `get_record`
function.

## Example interactive session

This section describes how an interactive session might look, using the python
XML-RPC client library.

First, initialise python:

```bash
$ python2.7
>>>
```

and import the library `xmlrpclib`:

```python
>>> import xmlrpclib
```

Create a python object referencing the remote server:

```python
>>> xen = xmlrpclib.Server("https://localhost:443")
```

Acquire a session reference by logging in with a username and password; the
session reference is returned under the key `Value` in the resulting dictionary
(error-handling ommitted for brevity):

```python
>>> session = xen.session.login_with_password("user", "passwd")['Value']
```

When serialised, this call looks like the following:

```xml
    <?xml version='1.0'?>
    <methodCall>
      <methodName>session.login_with_password</methodName>
      <params>
        <param>
          <value><string>user</string></value>
        </param>
        <param>
          <value><string>passwd</string></value>
        </param>
        <param>
          <value><string>version</string></value>
        </param>
        <param>
          <value><string>originator</string></value>
        </param>
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
