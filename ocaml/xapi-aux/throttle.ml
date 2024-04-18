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
  type t = {delay_before: float; delay_between: float}

  let make ~delay_before ~delay_between = {delay_before; delay_between}

  (** [perform_delay delay] calls {!val:Thread.delay} when [delay] is non-zero.

    To avoid issues with floating-point rounding, we consider everything smaller than {!val:Float.epsilon} equivalent to 0.
    Thread.delay 0 provides no fairness guarantees, the current thread may actually be the one that gets the global lock again.
    Instead {!val:Thread.yield} could be used, which does provide fairness guarantees, but it may also introduce large latencies
    when there are lots of threads waiting for the OCaml runtime lock.
   *)
  let perform_delay delay =
    if delay > Float.epsilon then
      Thread.delay delay

  let with_recursive config f arg =
    let rec self arg =
      perform_delay config.delay_between ;
      (f [@tailcall]) self arg
    in
    perform_delay config.delay_before ;
    f self arg
end
