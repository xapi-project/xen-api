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

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

type t = {
    burst_size: float
  ; fill_rate: float
  ; mutable tokens: float
  ; mutable last_refill: Mtime.span
  ; mutex: Mutex.t
}

let create_with_timestamp timestamp ~burst_size ~fill_rate =
  if fill_rate <= 0. then
    None
  else
    Some
      {
        burst_size
      ; fill_rate
      ; tokens= burst_size
      ; last_refill= timestamp
      ; mutex= Mutex.create ()
      }

let create = create_with_timestamp (Mtime_clock.elapsed ())

let peek_with_timestamp timestamp tb =
  let time_delta = Mtime.Span.abs_diff tb.last_refill timestamp in
  let time_delta_seconds = Mtime.Span.to_float_ns time_delta *. 1e-9 in
  min tb.burst_size (tb.tokens +. (time_delta_seconds *. tb.fill_rate))

let peek tb = peek_with_timestamp (Mtime_clock.elapsed ()) tb

let consume_with_timestamp get_time tb amount =
  let do_consume () =
    let timestamp = get_time () in
    let new_tokens = peek_with_timestamp timestamp tb in
    tb.last_refill <- timestamp ;
    if new_tokens >= amount then (
      tb.tokens <- new_tokens -. amount ;
      true
    ) else (
      tb.tokens <- new_tokens ;
      false
    )
  in
  with_lock tb.mutex do_consume

let consume = consume_with_timestamp Mtime_clock.elapsed

let delay_until_available_timestamp timestamp tb amount =
  let current_tokens = peek_with_timestamp timestamp tb in
  let required_tokens = max 0. (amount -. current_tokens) in
  required_tokens /. tb.fill_rate

let delay_until_available tb amount =
  delay_until_available_timestamp (Mtime_clock.elapsed ()) tb amount
