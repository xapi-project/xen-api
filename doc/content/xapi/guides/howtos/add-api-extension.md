+++
title = "Adding a XenAPI extension"
+++

A XenAPI extension is a new RPC which is implemented as a separate executable
(i.e. it is not part of `xapi`)
but which still benefits from `xapi` parameter type-checking, multi-language
stub generation, documentation generation, authentication etc.
An extension can be backported to previous versions by simply adding the
implementation, without having to recompile `xapi` itself.

A XenAPI extension is in two parts:

1. a declaration in the [xapi datamodel](https://github.com/xapi-project/xen-api/blob/07056d661bbf58b652e1da59d9adf67a778a5626/ocaml/idl/datamodel.ml#L5608).
This must use the `~forward_to:(Extension "filename")` parameter. The filename must be unique, and
should be the same as the XenAPI call name.
2. an implementation executable in the dom0 filesystem with path `/etc/xapi.d/extensions/filename`

To define an extension
----------------------

First write the declaration in the datamodel. The act of specifying the
types and writing the documentation will help clarify the intended meaning
of the call.

Second create a prototype of your implementation and put an executable file
in `/etc/xapi.d/extensions/filename`. The calling convention is:

- the file must be executable
- `xapi` will parse the XMLRPC call arguments received over the network and check the `session_id` is
  valid
- `xapi` will execute the named executable
- the XMLRPC call arguments will be sent to the executable on `stdin` and
  `stdin` will be closed afterwards
- the executable will run and print an XMLRPC response on `stdout`
- `xapi` will read the response and return it to the client.

See the [basic example](https://github.com/xapi-project/xen-api/blob/07056d661bbf58b652e1da59d9adf67a778a5626/scripts/extensions/Test.test).

Second make a [pull request](https://github.com/xapi-project/xen-api/pulls)
containing only the datamodel definitions (it is not necessary to include the
prototype too).
This will attract review comments which will help you improve your API further.
Once the pull request is merged, then the API call name and extension are officially
yours and you may use them on any xapi version which supports the extension mechanism.

Packaging your extension
------------------------

Your extension `/etc/xapi.d/extensions/filename` (and dependencies) should be
packaged for your target distribution (for XenServer dom0 this would be a CentOS
RPM). Once the package is unpacked on the target machine, the extension should
be immediately callable via the XenAPI, provided the `xapi` version supports
the extension mechanism. Note the `xapi` version does not need to know about
the specific extension in advance: it will always look in `/etc/xapi.d/extensions/` for
all RPC calls whose name it does not recognise.

Limitations
-----------

On type-checking

- if the `xapi` version is new enough to know about your specific extension:
  `xapi` will type-check the call arguments for you
- if the `xapi` version is too old to know about your specific extension:
  the extension will still be callable but the arguments will not be type-checked.

On access control

- if the `xapi` version is new enough to know about your specific extension:
  you can declare that a user must have a particular role (e.g. 'VM admin')
- if the `xapi` version is too old to know about your specific extension:
  the extension will still be callable but the client must have the 'Pool admin' role.

Since a `xapi` which knows about your specific extension is stricter than an older
`xapi`, it's a good idea to develop against the new `xapi` and then test older
`xapi` versions later.


