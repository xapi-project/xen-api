---
title: Backtrace support
layout: default
design_doc: true
revision: 1
status: Confirmed
---

We want to make debugging easier by recording exception backtraces which are

- reliable
- cross-process (e.g. xapi to xenopsd)
- cross-language
- cross-host (e.g. master to slave)

We therefore need

- to ensure that backtraces are captured in our OCaml and python code
- a marshalling format for backtraces
- conventions for storing and retrieving backtraces

Backtraces in OCaml
===================

OCaml has fast exceptions which can be used for both

- control flow i.e. fast jumps from inner scopes to outer scopes
- reporting errors to users (e.g. the toplevel or an API user)

To keep the exceptions fast, exceptions and backtraces are decoupled:
there is a single active backtrace per-thread at any one time. If you
have caught an exception and then throw another exception, the backtrace
buffer will be reinitialised, destroying your previous records. For example
consider a 'finally' function:

```ocaml
let finally f cleanup =
  try
    let result = f () in
    cleanup ();
    result
  with e ->
    cleanup ();
    raise e (* <-- backtrace starts here now *)
```

This function performs some action (i.e. `f ()`) and guarantees to
perform some cleanup action (`cleanup ()`) whether or not an exception
is thrown. This is a common pattern to ensure resources are freed (e.g.
closing a socket or file descriptor). Unfortunately the `raise e` in
the exception handler loses the backtrace context: when the exception
gets to the toplevel, `Printexc.get_backtrace ()` will point at the
`finally` rather than the real cause of the error.

We will use a variant of the solution proposed by
[Jacques-Henri Jourdan](http://gallium.inria.fr/blog/a-library-to-record-ocaml-backtraces/)
where we will record backtraces when we catch exceptions, before the
buffer is reinitialised. Our `finally` function will now look like this:

```ocaml
let finally f cleanup =
  try
    let result = f () in
    cleanup ();
    result
  with e ->
    Backtrace.is_important e;
    cleanup ();
    raise e
```

The function `Backtrace.is_important e` associates the exception `e`
with the current backtrace before it gets deleted.

Xapi always has high-level exception handlers or other wrappers around all the
threads it spawns. In particular Xapi tries really hard to associate threads
with active tasks, so it can prefix all log lines with a task id. This helps
admins see the related log lines even when there is lots of concurrent activity.
Xapi also tries very hard to label other threads with names for the same reason
(e.g. `db_gc`). Every thread should end up being wrapped in `with_thread_named`
which allows us to catch exceptions and log stacktraces from `Backtrace.get`
on the way out.

OCaml design guidelines
-----------------------

Making nice backtraces requires us to think when we write our exception raising
and handling code. In particular:

- If a function handles an exception and re-raise it, you must call
  `Backtrace.is_important e` with the exception to capture the backtrace first.
- If a function raises a different exception (e.g. `Not_found` becoming a XenAPI
  `INTERNAL_ERROR`) then you must use `Backtrace.reraise <old> <new>` to
  ensure the backtrace is preserved.
- All exceptions should be printable -- if the generic printer doesn't do a good
  enough job then register a custom printer.
- If you are the last person who will see an exception (because you aren't going
  to rethrow it) then you *may* log the backtrace via `Debug.log_backtrace e`
  *if and only if* you reasonably expect the resulting backtrace to be helpful
  and not spammy.
- If you aren't the last person who will see an exception (because you are going
  to rethrow it or another exception), then *do not* log the backtrace; the
  next handler will do that.
- All threads should have a final exception handler at the outermost level
  for example `Debug.with_thread_named` will do this for you.


Backtraces in python
====================

Python exceptions behave similarly to the OCaml ones: if you raise a new
exception while handling an exception, the backtrace buffer is overwritten.
Therefore the same considerations apply.

Python design guidelines
------------------------

The function [sys.exc_info()](https://docs.python.org/2/library/sys.html#sys.exc_info)
can be used to capture the traceback associated with the last exception.
We must guarantee to call this before constructing another exception. In
particular, this does not work:

```python
  raise MyException(sys.exc_info())
```

Instead you must capture the traceback first:

```python
  exc_info = sys.exc_info()
  raise MyException(exc_info)
```

Marshalling backtraces
======================

We need to be able to take an exception thrown from python code, gather
the backtrace, transmit it to an OCaml program (e.g. xenopsd) and glue
it onto the end of the OCaml backtrace. We will use a simple json marshalling
format for the raw backtrace data consisting of

- a string summary of the error (e.g. an exception name)
- a list of filenames
- a corresponding list of lines

(Note we don't use the more natural list of pairs as this confuses the
"rpclib" code generating library)

In python:

```python
    results = {
      "error": str(s[1]),
      "files": files,
      "lines": lines,
    }
    print json.dumps(results)
```

In OCaml:

```ocaml
  type error = {
    error: string;
    files: string list;
    lines: int list;
  } with rpc
  print_string (Jsonrpc.to_string (rpc_of_error ...))
```

Retrieving backtraces
=====================

Backtraces will be written to syslog as usual. However it will also be
possible to retrieve the information via the CLI to allow diagnostic
tools to be written more easily.

The CLI
-------

We add a global CLI argument "--trace" which requests the backtrace be
printed, if one is available:

```
# xe vm-start vm=hvm --trace
Error code: SR_BACKEND_FAILURE_202
Error parameters: , General backend error [opterr=exceptions must be old-style classes or derived from BaseException, not str],
Raised Server_error(SR_BACKEND_FAILURE_202, [ ; General backend error [opterr=exceptions must be old-style classes or derived from BaseException, not str];  ])
Backtrace:
0/50 EXT @ st30 Raised at file /opt/xensource/sm/SRCommand.py, line 110
1/50 EXT @ st30 Called from file /opt/xensource/sm/SRCommand.py, line 159
2/50 EXT @ st30 Called from file /opt/xensource/sm/SRCommand.py, line 263
3/50 EXT @ st30 Called from file /opt/xensource/sm/blktap2.py, line 1486
4/50 EXT @ st30 Called from file /opt/xensource/sm/blktap2.py, line 83
5/50 EXT @ st30 Called from file /opt/xensource/sm/blktap2.py, line 1519
6/50 EXT @ st30 Called from file /opt/xensource/sm/blktap2.py, line 1567
7/50 EXT @ st30 Called from file /opt/xensource/sm/blktap2.py, line 1065
8/50 EXT @ st30 Called from file /opt/xensource/sm/EXTSR.py, line 221
9/50 xenopsd-xc @ st30 Raised by primitive operation at file "lib/storage.ml", line 32, characters 3-26
10/50 xenopsd-xc @ st30 Called from file "lib/task_server.ml", line 176, characters 15-19
11/50 xenopsd-xc @ st30 Raised at file "lib/task_server.ml", line 184, characters 8-9
12/50 xenopsd-xc @ st30 Called from file "lib/storage.ml", line 57, characters 1-156
13/50 xenopsd-xc @ st30 Called from file "xc/xenops_server_xen.ml", line 254, characters 15-63
14/50 xenopsd-xc @ st30 Called from file "xc/xenops_server_xen.ml", line 1643, characters 15-76
15/50 xenopsd-xc @ st30 Called from file "lib/xenctrl.ml", line 127, characters 13-17
16/50 xenopsd-xc @ st30 Re-raised at file "lib/xenctrl.ml", line 127, characters 56-59
17/50 xenopsd-xc @ st30 Called from file "lib/xenops_server.ml", line 937, characters 3-54
18/50 xenopsd-xc @ st30 Called from file "lib/xenops_server.ml", line 1103, characters 4-71
19/50 xenopsd-xc @ st30 Called from file "list.ml", line 84, characters 24-34
20/50 xenopsd-xc @ st30 Called from file "lib/xenops_server.ml", line 1098, characters 2-367
21/50 xenopsd-xc @ st30 Called from file "lib/xenops_server.ml", line 1203, characters 3-46
22/50 xenopsd-xc @ st30 Called from file "lib/xenops_server.ml", line 1441, characters 3-9
23/50 xenopsd-xc @ st30 Raised at file "lib/xenops_server.ml", line 1452, characters 9-10
24/50 xenopsd-xc @ st30 Called from file "lib/xenops_server.ml", line 1458, characters 48-60
25/50 xenopsd-xc @ st30 Called from file "lib/task_server.ml", line 151, characters 15-26
26/50 xapi @ st30 Raised at file "xapi_xenops.ml", line 1719, characters 11-14
27/50 xapi @ st30 Called from file "lib/pervasiveext.ml", line 22, characters 3-9
28/50 xapi @ st30 Raised at file "xapi_xenops.ml", line 2005, characters 13-14
29/50 xapi @ st30 Called from file "lib/pervasiveext.ml", line 22, characters 3-9
30/50 xapi @ st30 Raised at file "xapi_xenops.ml", line 1785, characters 15-16
31/50 xapi @ st30 Called from file "message_forwarding.ml", line 233, characters 25-44
32/50 xapi @ st30 Called from file "message_forwarding.ml", line 915, characters 15-67
33/50 xapi @ st30 Called from file "lib/pervasiveext.ml", line 22, characters 3-9
34/50 xapi @ st30 Raised at file "lib/pervasiveext.ml", line 26, characters 9-12
35/50 xapi @ st30 Called from file "message_forwarding.ml", line 1205, characters 21-199
36/50 xapi @ st30 Called from file "lib/pervasiveext.ml", line 22, characters 3-9
37/50 xapi @ st30 Raised at file "lib/pervasiveext.ml", line 26, characters 9-12
38/50 xapi @ st30 Called from file "lib/pervasiveext.ml", line 22, characters 3-9
9/50 xapi @ st30 Raised at file "rbac.ml", line 236, characters 10-15
40/50 xapi @ st30 Called from file "server_helpers.ml", line 75, characters 11-41
41/50 xapi @ st30 Raised at file "cli_util.ml", line 78, characters 9-12
42/50 xapi @ st30 Called from file "lib/pervasiveext.ml", line 22, characters 3-9
43/50 xapi @ st30 Raised at file "lib/pervasiveext.ml", line 26, characters 9-12
44/50 xapi @ st30 Called from file "cli_operations.ml", line 1889, characters 2-6
45/50 xapi @ st30 Re-raised at file "cli_operations.ml", line 1898, characters 10-11
46/50 xapi @ st30 Called from file "cli_operations.ml", line 1821, characters 14-18
47/50 xapi @ st30 Called from file "cli_operations.ml", line 2109, characters 7-526
48/50 xapi @ st30 Called from file "xapi_cli.ml", line 113, characters 18-56
49/50 xapi @ st30 Called from file "lib/pervasiveext.ml", line 22, characters 3-9
```

One can automatically set "--trace" for a whole shell session as follows:

```bash
export XE_EXTRA_ARGS="--trace"
```

The XenAPI
----------

We already store error information in the XenAPI "Task" object and so we
can store backtraces at the same time. We shall add a field "backtrace"
which will have type "string" but which will contain s-expression encoded
backtrace data. Clients should not attempt to parse this string: its
contents may change in future. The reason it is different from the json
mentioned before is that it also contains host and process information
supplied by Xapi, and may be extended in future to contain other diagnostic
information.


The Xenopsd API
---------------

We already store error information in the xenopsd API "Task" objects,
we can extend these to store the backtrace in an additional field ("backtrace").
This field will have type "string" but will contain s-expression encoded
backtrace data.


The SMAPIv1 API
---------------

Errors in SMAPIv1 are returned as XMLRPC "Faults" containing a code and
a status line. Xapi transforms these into XenAPI exceptions usually of the
form `SR_BACKEND_FAILURE_<code>`. We can extend the SM backends to use the
XenAPI exception type directly: i.e. to marshal exceptions as dictionaries:

```python
  results = {
    "Status": "Failure",
    "ErrorDescription": [ code, param1, ..., paramN ]
  }
```

We can then define a new backtrace-carrying error:

- code = `SR_BACKEND_FAILURE_WITH_BACKTRACE`
- param1 = json-encoded backtrace
- param2 = code
- param3 = reason

which is internally transformed into `SR_BACKEND_FAILURE_<code>` and
the backtrace is appended to the current Task backtrace. From the client's
point of view the final exception should look the same, but Xapi will have
a chance to see and log the whole backtrace.

As a side-effect, it is possible for SM plugins to throw XenAPI errors directly,
without interpretation by Xapi.
