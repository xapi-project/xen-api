XCP inter-process communications
================================

An XCP system consists of a number of communicating services including
  1. xapi: the provider of the XenAPI and cluster manager
  2. xenopsd: the xen domain manager
  3. squeezed: the ballooning daemon
  4. rrdd: the statistics gathering daemon
  5. fe: the fork/exec service
  6. networkd: host network configuration manager

Currently all services have:
  1. typed RPC interfaces defined with
     [rpc-light](https://github.com/samoht/rpc)
  2. one or more unix domain sockets
  3. some message framing code, typically using HTTP
  4. client and server boilerplate code in OCaml

To support the future "Windsor" architecture we need the following extra
capabilities:
  1. support for services running in domains (out of the reach of
     Unix domain sockets)
  2. support for clients and servers written in python (for the
     convenience of the storage code)
  3. support for third-party clients and servers

We also wish to address the following deficiencies in our current system:
  1. service restarts causing propagating transient failures through
     other components
  2. service restarts causing operations (both unprocessed requests and
     pending tasks) to be lost
  3. it is difficult to trace and visualise RPCs as they bounce between
     components

This design will not be perfect, so we plan for evolvability by hiding
as many low-level details of the system from the application code as
possible. We should be able to upgrade the IPC mechanism with no (or
minimal) changes to the remaining XCP services.

High-level design
=================

Store and forward message switch
--------------------------------

An XCP host shall contain a single logical *message switch*. Each service
shall maintain a single *session* with the switch. Associated with the
session there will be set of individual *links* (connections)
managed via a shared library (available in python or OCaml). Link-level messages
will allow the creation and destruction of named *message queues" and
the creation and destruction of binary messages. All messages are
unidirectional so we shall have a well-known convention for bidirectional
Remote Procedure Calls, based on how OpenStack uses AMQP. Regular link-level
messages will serve as a source of basic liveness information which can
be consumed by any interested health and liveness monitoring infrastructure.

Services on an XCP host will be identified only by their well-known message
queue name. When a service moves between domains (e.g. through a reconfiguration
or as the result of a domain reboot), no client will observe the name changing.

Messages within a queue are persisted in memory of the message switch entity
only: they are not persisted to disk. If the message switch process restarts
then in-flight RPCs will suffer transient failures. However if any client or
server process restarts then the transient failures will be hidden by the
switch.

When the switch receives a link-level message requesting a message is
enqueued in a particular named queue, if the queue does not exist then the
message is dropped and the switch returns a QUEUE_DOES_NOT_EXIST error.
If the queue does exist but the number of messages in
the queue associated with this particular session (ie. client) exceeds a
configurable threshold, then the attempt fails with a QUEUE_FULL error.
If a client continually sends messages to one queue in a tight loop then it
will be the only one which sees the QUEUE_FULL error since other clients
have separate quotas.

When a message queue is created, it may be either a persistent queue with a
given well-known name, or a "transient" queue with a freshly-generated name.
The lifetime of such a "transient" queue -- and the lifetime of all the 
messages stored inside -- will be tied to the lifetime of the *session* which
created it. These temporary queues will be used for RPC replies by clients
which will "forget" about in-flight requests when the user hits Control+C.

When a message queue has been created, a service may *subscribe* to messages
sent to it. Once subscribed, a link-level "blocking poll"-style message is
used to wait for the arrival of new messages. Subscribed messages *are left
in the queue* until explicitly removed by sending a link-level *ack* message
to the message switch. This means that all services should be prepared to
receive the same message (which will be tagged with a unique id) more than
once. See the later section on "Responsibilities of a service". 

The message switch will support additional message introspecation/manipulation
interfaces intended for the system administrator. These interfaces will show:
  1. the current list of named queues, and the queue contents. It will be
     easy to see the age of messages in the queue as well as throughput and
     latency data, allowing an administrator to spot when something is overloaded
     or stuck.
  2. the current list of sessions, and their constituent links. It will be
     possible to see the low-level protocol endpoint information (IP address
     or V4V port number or whatever) and "heartbeat" information so the admin
     can spot a stuck service.
These interfaces will allow:
  1. all messages matching a particular filter to be logged to the terminal
  2. individual queues and/or messages to be deleted.

In all cases where performance data is queryable (e.g. throughput, latency,
queue length, time-since-last-heartbeat) it will be exported through the
standard XCP datasource API so it can be archived by the rrdd service.

Using IDL for service definitions
---------------------------------

An service may export one or more *interfaces* which are defined in a custom
*Interface Definition Language* (IDL). For the next few versions we wish to be
able to change the low-level implementation and therefore we will deliberately
restrict the expressivity of the IDL so it remains small and portable.

An interface definition shall consist of (possibly nested) namespaces, containing
definitions of types and functions. The types shall consist of primitives (such
as integers and strings) as well as aliases, dictionaries, lists and channels. Functions
shall take as inputs a named parameter list and return a named result list.

A set of tools shall parse the IDL syntax and generate (in both OCaml and python):
  1. client stubs
  2. server skeletons
  3. example server implementations
  4. example client invocations
A tool shall also create a hyperlinked HTML version of the docs, which includes the
generated example code.

An individual interface definition shall have a version number that should be
incremented when any backwards-incompatible change is made. It must be possible
to support multiple versions of the same interface simultaneously in the same
program. Clients should always specify the interface version explicitly in their
requests.

The IDL is only to be used for *control* messages, not *data*. All messages are to
be encoded in JSON for ease of parsing by both humans and machines. If a peer
wishes to send binary data to another peer then it should either send or receive
a value of type *channel*: as a side-effect of exchanging the message a high-bandwidth
channel will be established. See the later section, "High bandwidth channel communication".

Responsibilities of a service
-----------------------------

Dealing with duplicate messages
  simplest, preferred: ensure idempotency
  more complicated: journalling
Only access the switch through the provided switch access library. No direct wire protocol access.


Low-level design
================

The message switch
------------------

Link-level messages
Per-session statistics

The Interface Definition Language
---------------------------------

IDL: types, channels, inputs, outputs
IDL: documentation
Versioning
Change control policy

The OCaml generated code
------------------------

Modules and records for all arguments
Also generate labelled arguments for utop
Versioning
OCamlfind package layout

The python generated code
-------------------------
Versioning
package layout

The RPC pattern
---------------

High bandwidth channel communication
------------------------------------

Packaging
---------

Repository structure

Diagnostic tools
----------------


tcpdump
see the named queues
  and messages
  with activity timers
see sessions, link-level statistics



