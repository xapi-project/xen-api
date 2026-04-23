---
title: Rate Limiting
layout: default
design_doc: true
revision: 2
status: draft
---

<!--toc:start-->
- [Overview](#overview)
- [Approach](#approach)
  - [Rate limiting](#rate-limiting)
  - [Client classification](#client-classification)
  - [Statistics](#statistics)
- [API design](#api-design)
  - [Caller Datamodel](#caller-datamodel)
  - [API functions](#api-functions)
  - [Matching semantics](#matching-semantics)
  - [Caller lifecycle](#caller-lifecycle)
- [XAPI integration](#xapi-integration)
- [Library](#library)
  - [Token bucket](#token-bucket)
  - [Rate limit queue](#rate-limit-queue)
  - [Client table](#client-table)
- [Operational Description](#operational-description)
- [Pool Member / Multi-Host Considerations](#pool-member-multi-host-considerations)
<!--toc:end-->

## Overview

We have had several customer incidents in the past that have been attributed to
“overloading” Xapi. This effectively means that a client is making requests at
a rate that Xapi cannot handle. This can result in very bad response times (“we
tried to shut down 20 VMs and this took 2 hours!”) and general system
instability and unavailability.

In some cases, there is a mix of “well behaved” clients and others that are
either misconfigured or make improper use of the API, hammering the pool and
breaking use of the good guys. For example, a dodgy monitoring service may
lock out the control software, or slow down VM lifecycle operations.

Part of the problem is that Xapi and xenopsd are not very good at handling
load, in particular in a pool where the coordinator is often a bottleneck. A
lot of work has already been done make the Toolstack cope better under load.
This is important and a lot more can be done in this space.

However, even a very efficient Toolstack can be overloaded by a particularly
determined client, unless the client is deliberate slowed down in some way.
Hence, a way of throttling clients is needed in addition to performance
improvements, as a complimentary approach.

Last year, thread prioritisation was tried. This could be revisited, but we
also need an approach that allows us to pose hard constraints on clients. For
example, as an admin, I want to configure Xapi to give my control panel
unlimited access, but explicitly limit how much Monitoring App X can do.

The proposal here is to do a simpler kind of per-client rate limiting at the
API level. The objective is to introduce a simple mechanism to slow down
individual clients which are overloading the system. This design intentionally
enforces rate limits on a per-client basis rather than capping total system
throughput. System-wide admission control or global load shedding is explicitly
out of scope and would require separate mechanisms.

## Approach

### Rate limiting
The idea is to use a [Token Bucket algorithm](https://en.wikipedia.org/wiki/Token_bucket), where each client gets its own
Token Bucket with specific parameters to rate limit its requests. The Token
Bucket algorithm has two basic parameters:

The **capacity**: the maximum number of tokens that fit in the bucket.

The **fill rate**: the number of tokens per second that get added to the bucket (up
to its capacity).

Serving requests consumes tokens for the bucket - the token cost will be
proportional to the amount of work that the request requires. For example, a
simple DB call like VM.get_power_state may cost 1 token, while a VM.start call
may cost 100 tokens.

The basic principle is that for a client to get a request accepted, it needs to
have enough tokens in the bucket. If there are not enough tokens, then the
request will be queued until there are enough - the bucket is constantly
refilled, so all requests eventually get served. When a request is accepted, its
required number of tokens are taken out of the bucket.

Therefore, the bucket’s **fill rate** determines the **long-term average** rate at
which requests are served. For example, if the rate is 1 token/second, then on
average 1 DB call per second can be made.

The **capacity** is related to the maximum “**burst size**”. For example, a capacity of
100 tokens means that, if the bucket is full, the client can make 100 DB calls
in quick succession, before it is slowed done to match the rate.

One instance of a Token Bucket would apply to all calls from a given client,
potentially made on multiple connections.

### Client classification
In order to let pool administrators know who they should be rate limiting, we
will also introduce a **Caller** datamodel class which tracks all requests made
to Xapi.

Callers will be a high-level way of tracking clients. We allow callers to be
identified by a number of different parameters: AD user, IP address,
originator, user agent. When an unknown caller makes a request to Xapi, we
record their data in a new row. The pool administrator will be able to merge
related callers together and assign them labels.

The caller classification allows wildcards for any field, though we require
that at least one field be specified. This lets us, for example, combine all
accesses from the Xapi python API by specifying the user-agent .

A rate limiter can be associated with any number of callers, and the parameters
of the rate limiter can either be derived from the usage patterns of the
callers or selected from a number of preset profiles.

### Statistics
In order to assist with rate limiting, we can store statistics about callers.
We identify two kinds of statistics: volatile and stable. Volatile statistics
change over time without any input, e.g. sliding windows. These will be stored
in RRDs. By contrast, stable statistics only vary at most once per request, and
so are safe to store in the main database.

**Volatile statistics:**
- Tokens used over the last (5 minutes/hour/day).
- Most common requests over the last (5 minutes/hour/day).

**Stable statistics:**
- Last request timestamp

## API design

### Caller Datamodel
We propose two new datamodel tables: **Caller**, which stores the data associated with each caller, and **Rate limit**, which identifies one or more callers with a rate limiter.

Caller:
| Mutability | Name        | Type     | Description                                        |
| ---------: | ----------- | -------- | ---------------------------------------------------|
| RW         | name_label  | String   | User-assigned label for the caller |
| RO         | user_agent | String | user agent matching pattern  |
| RO         | host_ip  | String | IP address matching pattern  |
| RO         | last_access | DateTime | Last time the caller made a request        |
| SRW         | groups     | Set String | Set of labels used for cumulative metrics |
| RO         | rate_limit | Ref Rate_limit | Associated rate limiter - can be null |

Callers identify the origin of incoming requests, and they serve a dual purpose
of metrics gathering and providing a target for rate limiting. We allow users to query
the metrics for an individual caller, or for all callers belonging to a given
group. A new caller record will be added automatically whenever a request from
an unknown origin is made to Xapi, and callers can also be added manually by
users.

Rate limit:
| Mutability | Name        | Type     | Description |
|--------: | --------------|-----------|------------------|
| RW         | name_label  | String   | User-assigned label for the rate limiter |
| SRO        | callers | Set (Ref Caller) | Callers associated with this rate limiter |
| RO         | burst_size  | Float    | Amount of tokens that can be consumed in one burst |
| RO         | fill_rate   | Float    | Tokens added to the bucket per second |

A rate limit can be applied to a group of callers, which then have a collective
rate limit applied. Each caller can have at most one rate limiter applied,
which then becomes stored in its `rate_limit` field. We have two distinct
notions of groups here: rate limits store groups of callers, but groups of
callers are also represented in their `groups` field. We do this to allow for a
decoupling of rate limiting and data reporting, and to simplify the underlying
code by storing direct references to objects where possible.

### API functions
We define the following API functions for the caller datamodel:
- `Caller.create(name_label, user_agent, host_ip)`: Create a new caller.
- `Caller.set_name_label(caller, name_label)`: Set name label on the caller
- `Caller.destroy(caller)`: Destroy the caller
- `Caller.add_group(caller, group)`: Add caller to group
- `Caller.remove_group(caller, group)`: Remove caller from group
- `Caller.query_usage(caller, time_period)`: Obtain usage statistics for an individual caller
- `Caller.query_group_usage(group, time_period)`: Obtain usage statistics for a group of callers

And the following functions for the rate limiter datamodel:
- `Rate_limit.create(name_label, callers, burst_size, fill_rate)`: Create a
rate limiter with the supplied parameters.
- `Rate_limit.add_caller(rate_limit, caller)`: Add a caller to the callers set
- `Rate_limit.remove_caller(rate_limit, caller)`: Remove a caller from the callers set
- `Rate_limit.set_burst_size(rate_limit, burst_size)`: Set the burst size for the
  rate limiter
- `Rate_limit.set_fill_rate(rate_limit, burst_size)`: Set the fill rate for the
rate limiter
- `Rate_limit.destroy(rate_limit)`: Destroy the rate limiter - should also
clear the `rate_limit` field from its associated callers

### Matching semantics
When a request arrives, Xapi matches the request's metadata against the
`Caller` table. Each field in a `Caller` record is a pattern matched against
the corresponding field in the request using prefix matching: a pattern
matches iff it is a prefix of the request's field value. Prefix patterns are
specified by terminating with `*`. A pattern without `*` matches only exact
equals. A record matches a request iff all fields match.

We treat logging and rate limiting differently:
- **Logging**: All caller records that match with an incoming request track the
  request.
- **Rate limiting**: Only the most specific match (which has rate limiting
enabled) for any given request will trigger rate limiting and deduct tokens
from its token bucket.

The **most specific match** is the matching `Caller` record with the longest
total prefix length across all fields. For example, a record that matches
`user_agent` with a full value is more specific than one that matches only a
short prefix. We resolve ties through a lexicographic ordering amongst fields:
IP address first, then user agent.

This results in the statistics being stored by callers tracking everything that
matches, but only the most specific rate limiter to any given request will be
triggered.

### Caller lifecycle
- Users can proactively create callers with wildcards to identify groups of
callers.
- When a call comes in, a new Caller is automatically created if no existing
**fully specified** record (no wildcards) matches. We want to store the details
of all unique origins, even if they fall under a wildcard pattern.
- Toolstack startup behavior: On toolstack startup, an in-memory data structure
is created from the database fields which stores all the rate limiters and
callers, as described in the implementation section.

## XAPI integration
Calls into Xapi are intercepted at two points:
- RPC calls are intercepted at dispatch, in the `do_dispatch` function within
xapi/server_helpers.ml. At this point, we already have a session available if
the caller is logged in, and we know which Xapi call is being made.
- Other calls are intercepted by instrumenting the HTTP handlers as they are
added to the HTTP server in the `add_handler` function within
xapi/xapi_http.ml. Here we have less information, so the rate limiting is less
fine-grained, though there is scope to integrate more closely with the handlers
should it be necessary.

Both caller and rate limiter are implemented internally via a single table
which maps `caller -> caller_metrics.t * (rate_limit_data.t option)`. If a rate
limiter is being applied, then the cost of the call must come out of the
associated token bucket, and if there are not enough tokens in the bucket then
the response is delayed until the bucket is refilled.

An exception to this is async RPC calls, which always respond immediately but
the work that they initiate does get added to the rate limiting queue.

## Library
The internals of this are all implemented in a new library: libs/rate-limit,
which contains modules for token buckets, rate limiting queues, client
classification, and LRU cache, together with comprehensive unit tests.

### Token bucket
Token buckets enforce rate limiting by storing tokens that get depleted on
request and are refilled over time. They have two parameters
- Burst size: amount of tokens that the bucket can hold when full
- Fill rate: amount of tokens added to the bucket every second

In our implementation, we split the buckets into two types:

```ocaml
type state = {tokens: float; last_refill: Mtime.span}

type t = {
    burst_size: float
  ; fill_rate: float (* Tokens per second *)
  ; state: state Atomic.t
  }
```

The main type, type `t`, stores the token bucket parameters together with an
atomically wrapped state for thread safety. The state type itself contains the
token count at last refill, together with the last refill time.

By keeping track of the last refill time, we refill the tokens only when a new
request comes in. We provide two functions for consuming: one is non-blocking
and returns a boolean for whether the consume was successful, and the other
blocks until the tokens have been consumed.

### Rate limit queue
A token bucket enforces an overall request rate but provides no guarantees
about ordering or fairness under contention. When multiple callers compete for
tokens, whichever thread or process happens to run first can repeatedly consume
newly refilled tokens, while others may be delayed indefinitely. Under sustained
load, this can lead to starvation for less aggressively scheduled or slower
clients.

To alleviate this, we keep rate limited requests in a queue which get processed
as the token bucket is refilled over time:

```ocaml
type t = {
    bucket: Token_bucket.t
  ; process_queue:
      (float * (unit -> unit)) Queue.t (* contains token cost and callback *)
  ; process_queue_lock: Mutex.t
  ; worker_thread_cond: Condition.t
  ; should_terminate: bool ref (* signal termination to worker thread *)
  ; worker_thread: Thread.t
}
```

The worker thread is responsible for executing the callbacks in the process
queue when their time arrives. As above, we provide two functions for consuming
from the token bucket; `submit_async` returns immediately and places a function
to be executed at a later time in the queue, while `submit_sync` blocks until
its function is executed by the rate limiter but may return a value.

### Client table
We define a `Key` module to identify clients. This contains only user_agent and
host_ip at present, but can be expanded to cover AD user and originator, for
example.
```ocaml
module Key = struct
  type t = {user_agent: string; host_ip: string}
```

This module implements a `match : t -> t -> bool` function, which treats empty
strings as wildcards -- for example `match {"", "1.1.1.1"} {"firefox", "1.1.1.1"}`
will succeed. In order to store all recorded users, we use a simple association
list with a cache:
```ocaml
type 'a cached_table = {
    table: (Key.t * 'a) list
  ; cache: (Key.t, 'a option) Lru.t
}

type 'a t = 'a cached_table Atomic.t
```

We use the atomic type to allow thread-safe updates, and provide functions for
inserting, deleting, and obtaining items from the table.
