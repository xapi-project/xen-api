A simple in-memory message broker/queue/switch
==============================================

[![Build Status](https://travis-ci.org/xapi-project/message-switch.svg?branch=master)](https://travis-ci.org/xapi-project/message-switch)
[![Coverage Status](https://coveralls.io/repos/xapi-project/message-switch/badge.svg?branch=master)](https://coveralls.io/r/xapi-project/message-switch?branch=master)

This is a simple in-memory message broker (or "queue" or "switch"). A deployment
consists of one switch server process and multiple clients. A low-level protocol
allows clients to connect to the switch server process and:

  1. create named message queues
  2. enqueue messages in named queues
  3. subscribe to new messages in queues

Messages contain:

  1. a unique "correlation id", allowing replies to be associated to requests
  2. an optional "reply to" queue name
  3. a string payload

The client library currently contains a single high-level pattern for synchronous
RPC clients and servers. Other patterns (e.g. for pub/sub) could be added.

Quick Start
-----------

```sh
git clone git://github.com/xapi-project/message-switch
cd message-switch
make
```

Run the server (by default it will bind to 127.0.0.1:8080)

```sh
_build/install/default/bin/message-switch
```

Use the CLI to list the available queues (there will be none):

```sh
_build/install/default/bin/message-cli list
```

Use the CLI to execute an RPC against a service with queue name "foo" (this will block):

```sh
_build/install/default/bin/message-cli call foo --body hello
```

Use the diagnostics command to see the queued message:

```sh
_build/install/default/bin/message-cli diagnostics
```

Use the CLI to start an RPC server servicing the queue name "foo"

```sh
_build/install/default/bin/message-cli serve foo --program /bin/cat
```

At this point the original RPC call will unblock and print "hello".

Use the diagnostics command to see the message has been removed:

```sh
_build/install/default/bin/message-cli diagnostics
```

Low-level protocol
------------------

(I'd like to improve this, if you have any ideas then let me know)

The low-level protocol is currently using HTTP. Perhaps in future we should
use something a bit quicker: a binary protocol using ZeroMQ? Clients make the
following requests:

* Login `<client identifier>`: allows the switch to associate the socket
  with a single client. When the last socket is closed resources associated
  with the client are deallocated. I'm currently using a Unix PID as an
  identifier.
* Create `<Some name>`: creates a persistent queue with name `<name>`
* Create `<None>`: creates a queue with a fresh name which will be deallocated
  when the last connection opened by the client closes. These queues are
  created temporarily for RPC replies, the idea being that a client that
  has closed all connections has given up waiting for the reply and so
  the queue is not needed.
* Send `<string, Message.t>`: enqueue a message in a particular queue
* Ack `<message id>`: signal that a message has been processed and should be
  deleted. A service should presumably only Ack a message once it has committed
  its effects to disk.
* Subscribe `<name>`: subscribe to new messages in a named queue
* Transfer `<id, timeout>`: blocking poll for new messages in subscribed queues

The RPC pattern
---------------

A client will:

* Login `<some client id>`
* Reply_to <- `Create <None>` (used for the RPC reply)
* Subscribe `<reply_to>`
* Create `<service name>` (in case it doesn't exist)
* Send `<payload, service name>`
* repeatedly poll `<Transfer>` until a reply turns up

The server will:

* Login `<client id>`
* Create `<service name>` (in case it doesn't exist)
* Subscribe `<service name>`
* repeatedly poll `<Transfer>` until a request turns up
* process a request, Send `<reply, reply_to>`
* Ack `<message id>` to mark the message as processed and delete it
