Components
==========

We will have the following components:

name resolver

:    The name resolver will map well-known names to transient endpoint identifiers.
     For example a storage backend may have name *org/xen/sm/nfs/uuid* but this will be
     mapped at runtime to a *(domain id, port or IP address)*.

message switch

:    The message switch will allow components to send messages to other components
     by name, without knowing anything about their transient endpoint identifiers. The
     switch will buffer a finite number of messages and will be expected to handle the
     necessary retries while (for example) a driver domain is rebooting. The switch
     will act as a central point for logging, filtering, monitoring and record/replay
     of binary messages/

liveness monitor

:    The liveness monitor will monitor the accessibility of bound names corresponding
     to services. When a driver domain crashes and the name becomes inaccessible,
     the liveness monitor will take steps to reboot the domain and ensure the domain
     is rebound. The message switch will then be able to deliver queued messages.

The following sections describe the concepts in more detail:

Endpoints
---------

An endpoint is a transient address identifier of the following form:

    type idc_endpoint =
       | IP of string (* used for the HIMN *)
       | Vchan of unit
       | V4v of unit

    type domid = int
    type pid = int
    type unix_domain_socket_path = string

    type endpoint =
       | Process of unix_domain_socket_path * pid
       | Domain of idc_endpoint * domid

Endpoints are not visible to clients.

Names
-----

A name is a well-known name for a service:

    type name = string

By convention we structure names as reversed DNS names:

    org/xen/xenops
    com/citrix/wlb

Bindings
--------

A name may be unbound or it may have an associated binding:

    type binding = endpoint

If a message is sent to a name for which a binding does not (yet)
exist, a message is sent from the switch to the resolver. The
resolver may take some side-effecting action (e.g. it may boot
a VM which contains the requested service) and it will return the
new endpoint address. The switch will then bind the name to the
endpoint and start transmission.

Detecting service failure
-------------------------

A separate component, the "health and liveness monitor" monitors the
accessibility of bound names. The monitor will take corrective action
if an endpoint fails (e.g. a domain crashes) and it will rebind the
name to a new endpoint when one becomes available.


Connections
-----------

Clients establish connections to the broker but not directly to
each other. When a client establishes a connection it identifies
itself with a name:

    type hello_result =
       | Welcome
    type connection
    val hello: socket -> name -> connection

Internally the broker determines the endpoint and binds the name
to it. The connection is now in a state where messages may be
sent and received.

Messages
--------

A message is a binary blob sent to a name. The switch
associates each message with a handle which permits monitoring
for transmission success or failure.

    type message_id
    sendmsg: connection -> name -> string -> message_id

Message transmission will block if the queue associated with
the name fills up.

Clients can check the state of outstanding messages by polling

    type message_state =
       | Pending
       | Failed
       | Success
    get_message_state : message_id -> message_state

Clients can also arrange to receive callbacks when task state(s)
change.

Clients can receive incoming messages using:

    recvmsg: connection -> name * string

where the result is a pair of the remote peer name and the payload.

When finished with a message transmission, (which may or may not have
been completed), the client must explicitly free the queue slot
by explicitly deleting the message:

    deletemsg: message_id -> unit

Queueing and ordering
---------------------

Messages are always queued at the switch inputs, in a queue labelled
with both the input name and the output name. The queues have a fixed
size and message transmission will block if the queue is full. Note
that flooding messages to one output name will not cause messages
sent to other output names to block, since they will be kept in separate
queues. Similarly if a remote name is unresponsive then this will
apply backpressure through only one queue per input name.

