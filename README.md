A simple in-memory message broker/queue/switch
==============================================

This is a simple in-memory message broker (or "queue" or "switch"). A deployment
consists of one switch server process and multiple clients. A low-level protocol
allows clients to connect to the switch server process and:

  1. create named message queues
  2. enqueue messages in named queues
  3. subscribe to new messages in queues

Messages contain:

  1. a unique "correlation id", allowing replies to be asssociated to requests
  2. an optional "reply to" queue name
  3. a string payload

The client library currently contains a single high-level pattern for synchronous
RPC clients and servers. Other patterns (e.g. for pub/sub) could be added.

Quick Start
===========

  $ git clone git://github.com/djs55/message-switch
  $ cd message-switch
  $ make

Run the server (by default it will bind to 127.0.0.1:8080)

  $ ./switch.native 

Use the CLI to list the available queues (there will be none):

  $ ./main.native list

Use the CLI to execute an RPC against a service with queue name "foo" (this will block):

  $ ./main.native call foo --body hello

Use the diagnostics command to see the queued message:

  $ ./main.native diagnostics

Use the CLI to start an RPC server servicing the queue name "foo"

  $ ./main.native serve foo --program /bin/cat

At this point the original RPC call will unblock and print "hello".

Use the diagnostics command to see the message has been removed:

  $ ./main.native diagnostics

Low-level protocol
==================

The RPC pattern
===============



