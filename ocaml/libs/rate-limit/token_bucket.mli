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

type t

val create_with_timestamp :
  Mtime.span -> burst_size:float -> fill_rate:float -> t
(** Create token bucket with given parameters and supplied inital timestamp
    @param timestamp Initial timestamp
    @param burst_size Maximum number of tokens that can fit in the bucket
    @param fill_rate Number of tokens added to the bucket per second
    *)

val create : burst_size:float -> fill_rate:float -> t
(** Create token bucket with given parameters.
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

val peek : t -> float
(** Retrieve current token amount
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

val consume : t -> float -> bool
(** Consume tokens from the bucket in a thread-safe manner.
     @param tb Token bucket
     @param amount How many tokens to consume
     @return Whether the tokens were successfully consumed
   *)

val delay_until_available_timestamp : Mtime.span -> t -> float -> float
(** Get number of seconds that need to pass until bucket is expected to have
    enough tokens to fulfil the request
    @param timestamp
    @param tb Token bucket
    @param amount How many tokens we want to consume
    @return Number of seconds until tokens are available
*)

val delay_until_available : t -> float -> float
(** Get number of seconds that need to pass until bucket is expected to have
    enough tokens to fulfil the request
    @param tb Token bucket
    @param amount How many tokens we want to consume
    @return Number of seconds until tokens are available
*)
