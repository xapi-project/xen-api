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

type state = {tokens: float; last_refill: Mtime.span}

type t = {burst_size: float; fill_rate: float; state: state Atomic.t}

let create_with_timestamp timestamp ~burst_size ~fill_rate =
  if fill_rate <= 0. then
    None
  else
    let state = Atomic.make {tokens= burst_size; last_refill= timestamp} in
    Some {burst_size; fill_rate; state}

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

let consume_with_timestamp get_time tb amount =
  let rec try_consume () =
    let timestamp = get_time () in
    let old_state = Atomic.get tb.state in
    let new_tokens =
      compute_tokens timestamp old_state ~burst_size:tb.burst_size
        ~fill_rate:tb.fill_rate
    in
    let success, final_tokens =
      if new_tokens >= amount then
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
  let required_tokens = max 0. (amount -. current_tokens) in
  required_tokens /. tb.fill_rate

let get_delay_until_available tb amount =
  get_delay_until_available_timestamp (Mtime_clock.elapsed ()) tb amount

(* This implementation only works when there is only one thread trying to
   consume - fairness needs to be implemented on top of it with a queue.
   If there is no contention, it should only delay once. *)
let rec delay_then_consume tb amount =
  if not (consume tb amount) then (
    Thread.delay (get_delay_until_available tb amount) ;
    delay_then_consume tb amount
  )
