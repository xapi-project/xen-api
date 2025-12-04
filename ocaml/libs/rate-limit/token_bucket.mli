(*
 * Copyright (C) 2025 Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** This module implements a classic token-bucket rate limiter. Token buckets
    contain tokens that are refilled over time, and can be consumed in a
    thread-safe way. A token bucket accumulates [fill_rate] tokens per second,
    up to [burst_size]. Consumers may take tokens (if available), or query when
    enough tokens will become available.

    Token buckets implement rate limiting by allowing operations to proceed
    only when sufficient tokens are available - otherwise, the operations can
    be delayed until enough tokens are available.

    To avoid doing unnecessary work to refill the bucket, token amounts are
    only updated when a consume operation is carried out. The buckets keep a
    last_refill timestamp which is updated on consume in tandem with the token
    counts, and informs how many tokens should be added by the bucket refill.

    We include versions of functions that take a timestamp as a parameter for
    testing purposes only - consumers of this library should use the
    timestamp-less versions.
*)

type t

val create : burst_size:float -> fill_rate:float -> t option
(** Create token bucket with given parameters.
    @param burst_size Maximum number of tokens that can fit in the bucket
    @param fill_rate Number of tokens added to the bucket per second
    *)

val peek : t -> float
(** Retrieve current token amount
     @param tb Token bucket
     @return Amount of tokens in the token bucket
   *)

val consume : t -> float -> bool
(** Consume tokens from the bucket in a thread-safe manner.
     @param tb Token bucket
     @param amount How many tokens to consume
     @return Whether the tokens were successfully consumed
   *)

val get_delay_until_available : t -> float -> float
(** Get number of seconds that need to pass until bucket is expected to have
    enough tokens to fulfil the request
    @param tb Token bucket
    @param amount How many tokens we want to consume
    @return Number of seconds until tokens are available
*)

val delay_then_consume : t -> float -> unit

(**/**)

(* Fuctions accepting a timestamp are meant for testing only *)

val create_with_timestamp :
  Mtime.span -> burst_size:float -> fill_rate:float -> t option
(** Create token bucket with given parameters and supplied inital timestamp
    @param timestamp Initial timestamp
    @param burst_size Maximum number of tokens that can fit in the bucket
    @param fill_rate Number of tokens added to the bucket per second
    *)

val peek_with_timestamp : Mtime.span -> t -> float
(** Retrieve token amount in token bucket at given timestamp.
      Undefined behaviour when [timestamp] <= [tb.timestamp]
     @param timestamp Current time
     @param tb Token bucket
     @return Amount of tokens in the token bucket
   *)

val consume_with_timestamp : (unit -> Mtime.span) -> t -> float -> bool
(** Consume tokens from the bucket in a thread-safe manner, using supplied
      function for obtaining the current time
     @param get_time Function to obtain timestamp, e.g. Mtime_clock.elapsed
     @param tb Token bucket
     @param amount How many tokens to consume
     @return Whether the tokens were successfully consumed
   *)

val get_delay_until_available_timestamp : Mtime.span -> t -> float -> float
(** Get number of seconds that need to pass until bucket is expected to have
    enough tokens to fulfil the request
    @param timestamp
    @param tb Token bucket
    @param amount How many tokens we want to consume
    @return Number of seconds until tokens are available
*)

(**/**)
