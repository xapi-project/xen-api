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
will allow the creation and destruction of named *message queues* and
the creation and destruction of binary messages. All messages are
unidirectional so we shall have a well-known convention for bidirectional
Remote Procedure Calls, based on how OpenStack uses AMQP. Regular link-level
messages will serve as a source of basic liveness information which can
be consumed by any interested health and liveness monitoring infrastructure.

![Message switch used to store and forward messages](https://raw.github.com/djs55/xcp-idl/master/designs/switch.png)

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

All services need to be able to deal with duplicate messages, if for example, they
receive a message and then reboot. The simplest way to handle this is to ensure the
requests are *idempotent* and simply execute the request again. In the event that
idempotency is not possible then the service must instead use some form of *journalling*
to record progress executing a request so that the processing may be restarted when
the service restarts. To test the abilities of services to deal with duplicate messages,
the switch shall have a special debug mode where it randomly duplicates messages.

All services must only access the switch through the provided switch access library,
available in python and OCaml. No direct wire protocol access is permitted, since
the wire protocol is considered *unstable* and will *change with zero notice*.


Low-level design
================

The message switch
------------------

The message switch is an OCaml daemon running in domain 0. In
later deployments we may move the daemon to another domU, we may split it up into
multiple federated daemons and we may choose to run the whole thing in kernel space
with [mirage](http://openmirage.org/).

The message switch will use lightweight threads via the "lwt" library and the link-level
protocol shall be based on top of HTTP using the "cohttp" library.

The message switch needs the following datastructures:
  1. a bidirectional mapping of raw protocol connections to session_ids. Sometimes
     we need to know the session_id associated with a raw connection. Sometimes we
     need to know how many raw protocol connections still exist, to determine whether
     a session should be deleted.
  2. a mapping of sessions to "transient" queues, to allow these queues to be removed
     when the sessions are destroyed.
  3. a mapping of sessions to "subscribed" queues
  3. a mapping of message queue name to queue structure, for fast enqueue.
  4. a mapping of unique message id to queue structure, for fast dequeue.
  5. a circular buffer of logged events for message tracing.
 
A functional prototype of the message switch can be found
[here](https://github.com/djs55/dbus-test/blob/master/broker/switch/switch.ml)


The link-level protocol
-----------------------

The link-level protocol is used by the python/OCaml protocol library to talk directly
to the message switch. The following link-level
messages are defined (in OCaml syntax):

    module Message = struct
        type t = {
                payload: string; (* switch to Rpc.t *)
                correlation_id: int;
                reply_to: string option;
        } with rpc
        
        type transfer = {
                messages: (int64 * Message.t) list;
        } with rpc
        
    end
    
    module Event = struct
        type message =
                | Message of int64 * Message.t
                | Ack of int64
        with rpc
        
        type trace = {
                events: (int64 * Event.t) list;
        } with rpc
        
    end
    
    val login: string -> unit
    (** Associate this transport-level channel with a session *)
    
    val create: string option -> string
    (** [create None] creates a queue with a fresh name; [create (Some x)]
        creates a queue with a well-known name x *)
    
    val subscribe: string -> unit
    (** Subscribes the current session to messages arriving in the named queue *)
    
    val send: string -> Message.t -> unit
    (** Send a message to a queue (never blocks). The only exception this can
        throw is Queue_full *)
    
    val transfer: int64 -> float -> Message.transfer
    (** [transfer id timeout] blocks for up to [timeout] seconds for messages
        with ids higher than [id] to arrive in any of this session's subscribed
        queues. This is the "blocking poll" pattern already used in the XenAPI *)
    
    val trace: int64 -> float -> Event.trace
    (** [trace id timeout] blocks for up to [timeout] seconds for event traces
        with ids higher than [id] to arrive. *)
    
    val Ack: int64 -> unit
    (** [ack id] acknowledges that message [id] has been processed and may
        be removed from the queue. *)
    
    val diagnostics: unit -> string
    (** creates a dump of diagnostic data suitable for inclusion in a bug report *)

Each of the above link-level messages consists of a request and a response,
encoded as an HTTP request and a response.
The request messages shall look as follows:

    Request                             HTTP encoding
    -------                             -------------
    
    Login token                         GET /login/token
    Create                              GET /create
    Create queue                        GET /create/queue
    Subscribe queue                     GET /subscribe/queue
    Ack id                              GET /ack/id
    Transfer(ack_to, timeout)           GET /transfer/ack_to/timeout
    Trace(ack_to, timeout)              GET /trace/ack_to/timeout
    Send(name, cid, payload)            POST /send/name/cid with payload as the body
    Send(name, cid, payload, reply_to)  POST /send/name/cid/reply_to with payload as the body
    Diagnostics                         GET /

If a request is processed successfully then an HTTP 200 OK response will
be returned. Any non-unit result value will be returned in the request
body using a json encoding.

The following errors are possible:
  * any request other "Login" must be preceeded by a successful Login,
    otherwise an HTTP 403 forbidden will be returned
  * an attempt to subscribe to a queue that doesn't exist will return
    an HTTP 404 not found.
  * an attempt to acknowledge a message id that doesn't exist will return
    an HTTP 404 not found.

Note it is not an error to:
  * create a queue which already exists: the creation of a well-known queue name
    is idempotent; or
  * send a message to a queue which doesn't exist: the message will be silently
    dropped.

Note no detailed documentation shall be created for this wire format because
no-one else should implement it. We should add an extra HTTP header to all
requests and responses giving the URL of a wiki page describing the library-based
API that people should use. Then, anyone who inspects the messages on the wire
will be able to find the documentation.

The Interface Definition Language
---------------------------------

The IDL shall support the following primitive types (in OCaml syntax):

    type basic =
      | Int64
      | String
      | Double
      | Boolean

A value passed as an input or output parameter shall have values drawn
from the following type (in OCaml syntax):

    type t =
      | Basic of basic
      | Struct of (string * t * string) * ((string * t * string) list)
        (** (name, t, description) :: rest *)
      | Variant of (string * t * string) * ((string * t * string) list)
        (** (name, t, description) :: rest *)
      | Array of t
      | Dict of basic * t
      | Name of string
      | Unit
      | Option of t
      | Pair of t * t
      | Abstract of string

The automatically generated code in python and OCaml will be able to
marshal and unmarshal values of these types. In the case of a value of
type "Abstract x", the marshalling and unmarshalling code shall be delegated
to the client library in a module called "Abstract_x" (in OCaml, python
may use a slightly different naming convention)

In the first version of the software the only provided "Abstract" type
will be a "channel". From the application's point of view, passing or
receiving a value of type "channel" will give them an entity which
supports "read" and "write" operations. In the python and client libraries,
we will expose channels as Unix file descriptors.

When a channel value is created for marshalling, the client-side library
will fork/exec a multi-protocol proxy which will take the file descriptor
and then listen() on a set of network connections including any available
unix domain sockets, internal networks and v4v (if available).
The wire representation of a
marshalled channel is a list of URLs, each of which describes a means of
contacting the proxy. When a channel value is unmarshalled, the client
side library will decide which of the protocols will be most efficient
and will connect() to it.

Interfaces in the IDL consist of sets of functions, each of which has zero
or more named input values and zero or more named output values. Each input and output
also has an english description. The names and descriptions shall be used
in automatically generated hyperlinked HTML documentation. Each function
will have generated boilerplate in both python and OCaml for both client
and server. There will also be example client and server code which will be
inline in the documentation, and which can be cut 'n pasted directly into
OCaml and python toplevels.

Each interface will have an associated version number. The code generators
will generate separate modules for each distinct interface version and it
will be possible to link multiple version of the interface modules into
application code.

The interface definitions themselves will live in a well-known public repository.
All changes to interface definitions shall be reviewed by a defined stakeholder
list. Each interface will have a (possibly different) stakeholder list.

The OCaml generated code
------------------------

Types in the IDL are mapped onto OCaml types as follows:

    IDL type       OCaml projection
    --------       ----------------
    Basic Int64    int64
    Basic String   string
    Basic Double   float
    Basic Boolean  bool
    Struct         record
    Variant        Closed (non-polymorphic) variant
    Array          list
    Dict           ('a * 'b) list
    Unit           unit
    Option         'a option
    Pair(a, b)     ('a * 'b)
    Abstract t     type t

To understand the mapping of an RPC, consider the xenops API
    val VM.start: id -> unit

There will be records for the arguments and the results, as follows:

    module Start = struct
         module In = struct
             type t = {
                 dbg: debug_info;
                 id: id;
             } with rpc
         end
         module Out = struct
             type t = id with rpc
         end
    end

Note the additional argument 'dbg' -- this is for associating low-level
operations with higher level (probably XenAPI) tasks.

There will be a "VM operation" request type:

    module In = struct
        type t =
            | Start of Start.In.t
    
        let of_call (call: Rpc.call) : (t, 'b) Result.t =
             match call.Rpc.name, call.Rpc.params with
                | "Vm.start", [ args ] ->
                    Result.return (Start(Start.In.t_of_rpc args))
        let call_of = function
            | Start args -> Rpc.call "Vm.start" [ Start.In.rpc_of_t args ]
    end

and a "VM operation" response type:

    module Out = struct
        type t =
            | Start of Start.Out.t
        let response_of = function
            | Result.Error exn ->
                Rpc.failure (Rpc.String (Printf.sprintf "Internal_error: %s" (Printexc.to_s
tring exn)))
            | Result.Ok (Start x) ->
                Rpc.success (Start.Out.rpc_of_t x)
    end

There will be an RPC client signature:

    module type Vm = sig
        val start: Types.Vm.Start.In.t -> (Types.Vm.Start.Out.t, exn) Result.t t
    end

and an RPC server functor:

    module Vm_server_dispatcher = functor(Impl: Vm) -> struct
        type 'a t = 'a Impl.t
        let (>>=) = Impl.(>>=)
        let return = Impl.return
        let dispatch (request: Types.Vm.In.t) : (Types.Vm.Out.t, 'b) Result.t Impl.t = match request with
        | Types.Vm.In.Start x ->
            Impl.start x
            >>= fun result ->
            return (Result.(>>=) result (fun ok ->
                Result.return (Types.Vm.Out.Start ok)
            ))
    end

And an example skeleton implementation which raises an exception:

    module Vm_skeleton = functor(M: Xcp.M) -> struct
        include M
        open Types
    
        let start x =
            let open Vm.Start in
            return (Result.Error (Unimplemented "Vm.start"))
    end

And an example implementation which returns successfully from each operation:

    module Vm_test = functor(M: Xcp.M) -> struct
        include M
        open Types
    
        let start x =
            let open Vm.Start in
            return (Result.Ok (Out.("string")))
    end

There will be a client functor which offers both record and labelled argument variants.
Note the labelled arguments work particularly well in "utop" since they can be
tab-completed:

    module Vm_client = functor(RPC: RPC) -> struct
        open RPC
        let start_r x =
            let call = Types.Vm.In.call_of (Types.Vm.In.Start x) in
            RPC.rpc call >>= fun response ->
            let result = Result.(>>=) (result_of_response response) (fun x -> Result.return (Types.Vm.Start.Out.t_of_rpc x)) in
            return result
        let start ~dbg ~id =
            let r = Types.Vm.Start.In.({ dbg = dbg; id = id }) in
            start_r r
    end

XXX: need to put protocol version numbers in here somewhere

The code will be arranged into nested OCamlfind packages as follows:

    $ ocamlfind list | grep xcp
    xcp                 (version: 0.1)
    xcp.storage         (version: 0.1)
    xcp.xen             (version: 0.1)

The parent package contains the client library and boilerplate code. There
is one subpackage per interface.

The python generated code
-------------------------

Values of IDL types are mapped onto python values as follows:

    IDL type       Example python value
    --------       --------------------
    Basic Int64    0L
    Basic String   "hello"
    Basic Double   0.0
    Basic Boolean  False
    Struct         { "key": "value" }
    Variant        <xcp.Running instance at 0xfoo>
    Array          list
    Dict           { "key": "value" }
    Unit           None
    Option         None
    Pair(a, b)     (1, 2)
    Abstract t     <xcp.Channel instance at 0xfoo>

Versioning
package layout

Since the IDL contains type information, generated python code will check these
types during marshalling and unmarshalling. If an invalid type is encountered then
a run-time type exception will be raised.

To understand the mapping of an RPC, consider the xenops API
    val VM.start: id -> unit

We will generate a server-side proxy object which will type-check and unmarshal
the request, call an implementation object and then type-check and marshal the
response:

	class Vm_server_dispatcher:
		"""Types used to represent a VM configuration"""
		def __init__(self, impl):
			"""impl is a proxy object whose methods contain the implementation"""
			self._impl = impl
		def start(self, args):
			"""type-check inputs, call implementation, type-check outputs and return"""
			if type(args) <> type({}):
				raise (UnmarshalException('arguments', 'dict', repr(args)))
			if not(args.has_key('dbg')):
				raise UnmarshalException('argument missing', 'dbg', '')
			dbg = args["dbg"]
			if type(dbg) <> type(""):
				raise (TypeError("string", repr(dbg)))
			if not(args.has_key('id')):
				raise UnmarshalException('argument missing', 'id', '')
			id = args["id"]
			if type(id) <> type(""):
				raise (TypeError("string", repr(id)))
			results = self._impl.start(dbg, id)
			if type(results) <> type(""):
				raise (TypeError("string", repr(results)))
			return results
		def _dispatch(self, method, params):
			"""type check inputs, call implementation, type check outputs and return"""
			args = params[0]
			elif method == "Vm.start":
				return success(self.start(args))

To make it easy to create new server implementations, we will create example
server skeletons, where each method raises an Unimplemented exception. New
applications may choose to subclass this skeleton and selectively override
methods:

	class Vm_skeleton:
		"""Types used to represent a VM configuration"""
		def __init__(self):
			pass
		def start(self, dbg, id):
			"""Types used to represent a VM configuration"""
			raise Unimplemented("Vm.start")

To facilitate very simple component testing, we will generate an example
implementation which returns a successful response (i.e. a non-exceptional
value is returned:)

	class Vm_test:
		"""Types used to represent a VM configuration"""
		def __init__(self):
			pass
		def start(self, dbg, id):
			"""Types used to represent a VM configuration"""
			result = {}
			result["task"] = "string"
			return result

To interface directly with a HTTP server's main loop, we will generate
a master dispatcher class, which looks up the class and method name by
inspecting the name given to the method on the wire. This class will
also catch and log any exception, paying particular attention to any
undeclared exceptions, converting them into InternalErrors:

	class domains_server_dispatcher:
		"""Demux calls to individual interface server_dispatchers"""
		def __init__(self, Vm = None):
			self.Vm = Vm
		def _dispatch(self, method, params):
			try:
				log("method = %s params = %s" % (method, repr(params)))
				if method.startswith("Vm") and self.Vm:
					return self.Vm._dispatch(method, params)
			except Exception, e:
				log("caught %s" % e)
				traceback.print_exc()
				try:
					# A declared (expected) failure will have a .failure() method
					log("returning %s" % (repr(e.failure())))
					return e.failure()
				except:
					# An undeclared (unexpected) failure is wrapped as InternalError
					return (InternalError(str(e)).failure())

The RPC pattern
---------------

Client:
  1. tmp <- Create: to create a temporary reply queue
  2. Subscribe tmp
  3. Create <service name>: to ensure the service queue exists
  4. Send <service name>, reply_to
  5. Transfer(0, timeout): to wait for a reply to the tmp queue

Server:
  1. Create <service name>: to ensure the service queue exists
  2. Subscribe <service name>
  3. Transfer(0, timeout):
  4. Send <reply_to>
  5. Ack <message id>


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



