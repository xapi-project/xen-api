---
title: XAPI Rate Limiting
layout: default
design_doc: true
revision: 1
status: draft
---

## Overview

We have had several customer incidents in the past that have been attributed to
“overloading” xapi. This effectively means that a client is making requests at
a rate that xapi cannot handle. This can result in very bad response times (“we
tried to shut down 20 VMs and this took 2 hours!”) and general system
instability and unavailability.

In some cases, there is a mix of “well behaved” clients and others that are
either misconfigured or make improper use of the API, hammering the XS pool and
breaking use of the the good guys. For example, a dodgy monitoring service may
lock out XenCenter or break CVAD use cases.

Part of the problem is that xapi and xenopsd are not very good at handling
load, in particular in a pool where the coordinator is often a bottleneck. A
lot of work has already been done make the Toolstack cope better under load.
This is important and a lot more can be done in this space.

However, even a very efficient Toolstack can be overloaded by a particularly
determined client, unless the client is deliberate slowed down in some way.
Hence, a way of throttling clients is needed in addition to performance
improvements, as a complimentary approach.

Last year, thread prioritisation was tried. This could be revisited, but we
also need an approach that allows us to pose hard constraints on clients. For
example, as an admin, I want to configure XS to give XC unlimited access, but
explicitly limit how much Monitoring App X can do.

The proposal here is to do a simpler kind of per-client rate limiting at the
API level.

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
to xapi.

Callers will be a high-level way of tracking clients. We allow callers to be
identified by a number of different parameters: AD user, IP address,
originator, user agent. When an unknown caller makes a request to xapi, we
record their data in a new row. The pool administrator will be able to merge
related callers together and assign them labels.

The caller classification allows wildcards for any field, though we require
that at least one field be specified. This lets us, for example, combine all
accesses from XenCenter by only specifying the originator field,

In order to assist with rate limiting, we can store statistics about callers:
- Last request timestamp
- Tokens used over the last (5 minutes/hour/day).
- Most common API requests.

A rate limiter can then be attached to a particular caller, and the parameters
of the rate limiter can either be derived from the usage patterns of the caller
or selected from a number of preset profiles.

## API design
We propose two new datamodels: **Caller** and **Rate_limit**.

Caller:
| Mutability | Name        | Type     | Description                                     |
| ---------: | ----------- | -------- | ----------------------------------------------- |
| RW         | name_label  | String   | User-assigned label for the caller              |
| RO         | user_agent  | String   | User agent of throttled client; empty if “any”. |
| RO         | host_ip     | String   | IP address of throttled client; empty if “any”. |
| RO         | last_access | DateTime | Last time the caller made a request             |

Rate_limit:
| Mutability | Name       | Type        | Description                                        |
| ---------: | ---------- | ----------- | -------------------------------------------------- |
| RO         | caller     | Ref _caller | Caller being rate-limited                          |
| RO         | burst_size | Float       | Amount of tokens that can be consumed in one burst |
| RO         | fill_rate  | Float       | Tokens added to the bucket per second              |

In order to rate limit a client, they must first be registered as a caller. An
alternative design is to include the rate limit parameters in the caller table
and set them to 0 when a rate limit is not being applied --- I don't like this
coupling, but it would simplify the interface.

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

### Library
The internals of this are all implemented in a new library: libs/rate-limit,
which contains modules for token buckets, rate limiting queues, client
classification, and LRU cache, together with comprehensive unit tests.
