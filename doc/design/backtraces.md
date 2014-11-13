Creating useful backtraces
==========================

OCaml has fast exceptions which can be used for both
- control flow i.e. fast jumps from inner scopes to outer scopes
- reporting errors to users (e.g. the toplevel or an API user)

To keep the exceptions fast, exceptions and backtraces are decoupled:
there is a single active backtrace per-thread at any one time. If you
have caught an exception and then throw another exception, the backtrace
buffer will be reinitialised, destroying your stacktrace. For example
consider a 'finally' function:

```
let finally f cleanup =
  try
    let result = f () in
    cleanup ();
    result
  with e ->
    cleanup ();
    raise e (* <-- backtrace starts here now *)
```

This function performs some action (i.e. ```f ()```) and guarantees to
perform some cleanup action (```cleanup ()```) whether or not an exception
is thrown. This is a common pattern to ensure resources are freed (e.g.
closing a socket or file descriptor). Unfortunately the ```raise e``` in
the exception handler loses the backtrace context: when the exception
gets to the toplevel, ```Printexc.get_backtrace ()``` will point at the
```finally``` rather than the real cause of the error.

The solution
------------

We will use a variant of the solution proposed by
[Jacques-Henri Jourdan](http://gallium.inria.fr/blog/a-library-to-record-ocaml-backtraces/)
where we will record backtraces when we catch exceptions, before the
buffer is reinitialised. Our ```finally``` function will now look like this:

```
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

The function ```Backtrace.is_important e``` associates the exception ```e```
with the current backtrace before it gets deleted.

Xapi always has high-level exception handlers or other wrappers around all the
threads it spawns. In particular Xapi tries really hard to associate threads
with active tasks, so it can prefix all log lines with a task id. This helps
admins see the related log lines even when there is lots of concurrent activity.
Xapi also tries very hard to label other threads with names for the same reason
(e.g. "db_gc"). Every thread should end up being wrapped in ```with_thread_named```
which allows us to catch exceptions and log stacktraces from ```Backtrace.get```
on the way out.

The design guidelines
---------------------

Making nice backtraces requires us to think when we write our exception raising
and handling code. In particular:

- If a function handles an exception and re-raise it, you must call
  ```Backtrace.is_important e``` with the exception to capture the backtrace first.
- If a function raises a different exception (e.g. Not_found becoming a XenAPI
  INTERNAL_ERROR) then you must use ```Backtrace.reraise <old> <new>``` to
  ensure the backtrace is preserved.
- All exceptions should be printable -- if the generic printer doesn't do a good
  enough job then register a custom printer.
- If you are the last person who will see an exception (because you aren't going
  to rethow it) then you *may* log the backtrace via ```Debug.log_backtrace e```
  *if and only if* you reasonably expect the resulting backtrace to be helpful
  and not spammy.
- If you aren't the last person who will see an exception (because you are going
  to rethrow it or another exception), then *do not* log the backtrace; the
  next handler will do that.
- All threads should have a final exception handler at the outermost level
  for example ```Debug.with_thread_named``` will do this for you.

The CLI
-------

Normally errors are converted into single-line XenAPI errors and sent to the CLI
without any code context. Developers and test suites would like to also see
a backtrace with the error.

Since we now have a reliable way to capture a thread's backtrace, we can expose
that via the CLI if the user supplies the "--trace" argument. For example:
```
djs@st30:~/djs55/xen-api$ sudo xe host-list params=enabled --trace --whatever
Error: Unknown field 'whatever'
Backtrace:
1/11: Raised at file "cli_operations.ml", line 31, characters 25-51
2/11: Called from file "cli_operations.ml", line 504, characters 8-40
3/11: Called from file "list.ml", line 84, characters 24-34
4/11: Called from file "cli_operations.ml", line 597, characters 18-80
5/11: Called from file "lib/pervasiveext.ml", line 22, characters 3-10
6/11: Raised at file "lib/pervasiveext.ml", line 26, characters 9-12
7/11: Called from file "xapi_cli.ml", line 114, characters 18-56
8/11: Called from file "lib/pervasiveext.ml", line 22, characters 3-10
9/11: Raised at file "lib/pervasiveext.ml", line 26, characters 9-12
10/11: Called from file "xapi_cli.ml", line 113, characters 2-138
11/11: Called from file "xapi_cli.ml", line 273, characters 14-51
```

Unfortunately this can only show the current thread's backtrace: any XenAPI calls
will have their own threads and their backtraces will only exist in the
Xapi log.

The XenAPI
----------

Normally errors are converted into single-line XenAPI errors and stored in
the database Task record. Developers and test suites would like to also
have programmatic access to the backtrace, to avoid having to hunt around
in the logfiles.

Since we already have functions to capture exceptions at the top level and
transform them into XenAPI exceptions stored in the Task object, we can
add an additional field ("backtrace") and store the backtrace in there too.
Interested clients -- such as the CLI -- can extract the trace from failed
tasks.

The Xenopsd API
---------------

Normally errors are converted into marshalled exceptions and stored in the
Task record. Developers and test suites would like to also have programmatic
access to the backtrace, to avoid having to hunt around in the logfiles.

Since we already have functions to capture exceptions while running Tasks,
we can extend these to store the backtrace in an additional field ("backtrace").
Callers, such as xapi, can extract the trace from failed Tasks and add it
to their own context via ```Backtrace.add```


