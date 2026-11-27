(*
 * Copyright (C) 2026 Cloud Software Group
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

type state = {tokens: float; last_refill: Mtime.span}

type t = {burst_size: float; fill_rate: float; state: state Atomic.t}

(* NaN fails every comparison, so check that the values are valid rather
   than that they are invalid. An infinite fill rate is refused because the
   refill computes 0 *. infinity = NaN when no time has passed, and a NaN
   token count never admits a request again. A NaN burst size never admits
   a request either. *)
let create_with_timestamp timestamp ~burst_size ~fill_rate =
  if not (Float.is_finite fill_rate && fill_rate > 0.) then
    invalid_arg "Token_bucket.create: fill_rate must be positive and finite"
  else if Float.is_nan burst_size then
    invalid_arg "Token_bucket.create: burst_size must not be NaN"
  else
    let state = Atomic.make {tokens= burst_size; last_refill= timestamp} in
    {burst_size; fill_rate; state}

let create = create_with_timestamp (Mtime_clock.elapsed ())

let compute_tokens timestamp {tokens; last_refill} ~burst_size ~fill_rate =
  let time_delta = Mtime.Span.abs_diff last_refill timestamp in
  let time_delta_seconds = Mtime.Span.to_float_ns time_delta *. 1e-9 in
  min burst_size (tokens +. (time_delta_seconds *. fill_rate))

let peek_with_timestamp timestamp tb =
  let tb_state = Atomic.get tb.state in
  compute_tokens timestamp tb_state ~burst_size:tb.burst_size
    ~fill_rate:tb.fill_rate

let peek tb = peek_with_timestamp (Mtime_clock.elapsed ()) tb

(* A request whose cost exceeds [burst_size] can never wait for its full
   amount to accumulate (refill caps at [burst_size]). We admit such a
   request once the bucket is full and let [tokens] go negative; the debt
   is repaid at [fill_rate] before any later request can proceed. *)
let consume_with_timestamp get_time tb amount =
  let rec try_consume () =
    (* Read the state before the clock. A consume by another thread between
       the two reads then makes the compare-and-set below fail, and we retry
       with a fresh state. Read the other way round, the state could hold a
       newer [last_refill] than [timestamp]: the interval between them would
       be credited a second time, and [last_refill] would move backwards. *)
    let old_state = Atomic.get tb.state in
    let timestamp = get_time () in
    let new_tokens =
      compute_tokens timestamp old_state ~burst_size:tb.burst_size
        ~fill_rate:tb.fill_rate
    in
    let required = Float.min amount tb.burst_size in
    let success, final_tokens =
      if new_tokens >= required then
        (true, new_tokens -. amount)
      else
        (false, new_tokens)
    in
    let new_state = {tokens= final_tokens; last_refill= timestamp} in
    if Atomic.compare_and_set tb.state old_state new_state then
      success
    else
      try_consume ()
  in
  try_consume ()

let consume = consume_with_timestamp Mtime_clock.elapsed

let get_delay_until_available_timestamp timestamp tb amount =
  let {tokens; last_refill} = Atomic.get tb.state in
  let current_tokens =
    compute_tokens timestamp {tokens; last_refill} ~burst_size:tb.burst_size
      ~fill_rate:tb.fill_rate
  in
  let required = Float.min amount tb.burst_size in
  let required_tokens = max 0. (required -. current_tokens) in
  required_tokens /. tb.fill_rate

let get_delay_until_available tb amount =
  get_delay_until_available_timestamp (Mtime_clock.elapsed ()) tb amount

(* This implementation only works when there is only one thread trying to
   consume - fairness needs to be implemented on top of it with a queue.
   If there is no contention, it should only delay once. *)
let rec delay_then_consume tb amount =
  if not (consume tb amount) then (
    (* Thread.delay fails with EINVAL for a delay that does not fit in a
       timespec; the loop checks again after each sleep, so an hour at a
       time is enough. *)
    Thread.delay (Float.min 3600. (get_delay_until_available tb amount)) ;
    delay_then_consume tb amount
  )
