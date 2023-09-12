+++
title = "Interfaces"
+++

Communication between the Toolstack daemon is built upon libraries from a
component called
[xapi-idl](https://github.com/xapi-project/xen-api/tree/master/ocaml/xapi-idl).

- Abstracts communication between daemons over the message-switch using JSON/RPC.
- Contains the definition of the interfaces exposed by the daemons (except xapi).
