+++
title = "From RPC migration request to xapi internals"
linkTitle = "How XAPI handles migration request"
+++

## Overview

In this document we will use the `VM.pool_migrate` request to illustrate
the interaction between various components within the XAPI toolstack during
migration. However this schema can be applied to other requests as well.

Not all parts of the Xapi toolstack are shown here as not all are involved in
the migration process. For instance you won't see the ***squeezed***
nor ***mpathalert*** two daemons that belong to the toolstack but don't
participate in the migration of a VM.

## Anatomy of a VM migration

- Migration is initiated by a Xapi client that sends `VM.pool_migrate`, an RPC
XML request.
- The Xen API server handles this request and dispatches it to the server.
- The server is generated using `XAPI IDL` and requests are wrapped whithin a
context, either to be forwarded to a host or executed locally. Broadly, the
context follows RBAC rules. The executed function is related to the message of
the request (refer to [XenAPI Reference](https://xapi-project.github.io/xen-api/)).
- In the case of the migration you can refer to *ocaml/idl/datamodel_vm.ml*.
- The server will dispatch the operation to server helpers, executing the
operation synchronously or asynchronously and returning the RPC answer.
- *Message forwarding* decides if operation must be executed by another host
of the pool and then forward the call or if is executed locally.
- When executed locally the high-level migration operation is send to the
*Xenopsd daemon* by posting a message on a known queue on the *message switch*.
- *Xenopsd* will get the command and will split it into several *atomic*
operations that will be run by the *xenopsd backend*.
- *Xenopsd* with its *backend* can then access xenstore or execute hypercall to
interact with xen a server the micro operation.

## A diagram is worth a thousand words

```mermaid

flowchart TD

    %% First we are starting by a XAPI client that is sending an XML-RPC request
    client((Xapi client)) -. sends RPC XML request .->
        xapi_server{"`Dispatch RPC
                    **api_server.ml**`"}
    style client stroke:#CAFEEE,stroke-width:4px

    %% XAPI Toolstack internals
    subgraph "Xapi Toolstack (master of the pool)"
        style server stroke:#BAFA00,stroke-width:4px,stroke-dasharray: 5 5

            xapi_server --dispatch call (ie VM.pool_migrate)--> server("`Auto generated using *IDL*
                    **server.ml**`")

            server --do_dispatch (ie VM.pool_migrate)--> server_helpers["`server helpers
            **server_helpers.ml**`"]

            server_helpers -- call management (ie xapi_vm_migrate.ml)--> message_forwarding["`check where to run the call **message_forwarding.ml**`"]

            message_forwarding -- execute locally --> vm_management["`VM Mgmt
            like **xapi_vm_migrate.ml**`"]

            vm_management -- Call --> xapi_xenops["`Transform xenops
            see (**xapi_xenops.ml**)`"]
                xapi_xenops <-- Post following IDL model (see xenops_interface.ml) --> msg_switch


        subgraph "Message Switch Daemon"
            msg_switch[["Queues"]]
        end

        subgraph "Xenopsd Daemon"
            msg_switch <-- Push/Pop on org.xen.xapi.xenopsd.classic --> xenopsd_server

            xenopsd_server["`Xenposd *frontend*
            get & split high level opertion into atomics`"]  o-- linked at compile time --o xenopsd_backend
        end
    end

    %% Xenopsd backend is accessing xen and xenstore
    xenopsd_backend["`Xenopsd *backend*
    Backend XC (libxenctrl)`"] -. access to .-> xen_hypervisor["Xen hypervisor & xenstore"]
    style xen_hypervisor stroke:#BEEF00,stroke-width:2px

    %% Can send request to the host where call must be executed
    message_forwarding -.forward call to .-> elected_host["Host where call must be executed"]
    style elected_host stroke:#B0A,stroke-width:4px

```
