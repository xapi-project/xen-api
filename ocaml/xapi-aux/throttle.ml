(*
 * Copyright (C) Citrix Systems Inc.
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

module type SIZE = sig
  val n : unit -> int
end

module Make (Size : SIZE) = struct
  module Semaphore = Semaphore.Counting

  let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

  let execute = Xapi_stdext_threads.Threadext.Semaphore.execute

  let semaphore = ref None

  let m = Mutex.create ()

  let get_semaphore () =
    with_lock m @@ fun () ->
    match !semaphore with
    | None ->
        let result = Semaphore.make (Size.n ()) in
        semaphore := Some result ;
        result
    | Some s ->
        s

  let execute f = execute (get_semaphore ()) f
end

module Batching = struct
  type t = {
      delay_initial: Mtime.span
    ; delay_before: Mtime.span
    ; delay_between: Mtime.span
  }

  let make ~delay_before ~delay_between =
    (* we are dividing, cannot overflow *)
    let delay_initial =
      Mtime.Span.to_float_ns delay_between /. 16.
      |> Mtime.Span.of_float_ns
      |> Option.get
    in
    {delay_initial; delay_before; delay_between}

  let span_min a b = if Mtime.Span.is_shorter a ~than:b then a else b

  (** [perform_delay delay] calls {!val:Thread.delay} when [delay] is non-zero.

    Thread.delay 0 provides no fairness guarantees, the current thread may actually be the one that gets the global lock again.
    Instead {!val:Thread.yield} could be used, which does provide fairness guarantees, but it may also introduce large latencies
    when there are lots of threads waiting for the OCaml runtime lock. Only invoke this once, in the [delay_before] section.
   *)
  let perform_delay ~yield delay =
    if Mtime.Span.is_longer delay ~than:Mtime.Span.min_span then
      Thread.delay (Clock.Timer.span_to_s delay)
    else if yield then
      (* this is a low-priority thread, if there are any other threads waiting, then run them now.
         If there are no threads waiting then this a noop.
         Requires OCaml >= 4.09 (older versions had fairness issues in Thread.yield)
      *)
      Thread.yield ()

  let with_recursive_loop config f =
    let rec self arg input =
      let arg = span_min config.delay_between Mtime.Span.(2 * arg) in
      perform_delay ~yield:false arg ;
      (f [@tailcall]) (self arg) input
    in
    let self0 input = (f [@tailcall]) (self config.delay_initial) input in
    perform_delay ~yield:true config.delay_before ;
    f self0
end
