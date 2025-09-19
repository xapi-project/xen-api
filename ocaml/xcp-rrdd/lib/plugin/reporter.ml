(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

module Delay = Xapi_stdext_threads.Threadext.Delay

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

(* This exception is setup to be raised on sigint by Process.initialise,
 * and is used to cancel the synchronous function Reporter.start. *)
exception Killed

let killed = ref false (* CA-309024, Killed might escape *)

module Xs = struct
  module Xs = Xs_client_unix.Client (Xs_transport_unix_client)
  include Xs

  type xs_state = {my_domid: int32; root_path: string; client: Xs.client}

  let cached_xs_state = ref None

  let cached_xs_state_m = Mutex.create ()

  let get_xs_state () =
    with_lock cached_xs_state_m (fun () ->
        match !cached_xs_state with
        | Some state ->
            state
        | None ->
            (* This creates a background thread, so must be done after daemonising. *)
            let client = Xs.make () in
            let my_domid =
              Xs.immediate client (fun handle -> Xs.read handle "domid")
              |> Int32.of_string
            in
            let root_path = Printf.sprintf "/local/domain/%ld/rrd" my_domid in
            let state = {my_domid; root_path; client} in
            cached_xs_state := Some state ;
            state
    )
end

(* Establish a XMLPRC interface with RRDD *)
module RRDD = Rrd_client.Client

type state =
  | Running
  | Cancelled
  | Stopped of [`New | `Cancelled | `Failed of exn]

type t = {
    mutable state: state
  ; lock: Mutex.t
  ; condition: Condition.t
  ; delay: Delay.t
}

let make () =
  {
    state= Stopped `New
  ; lock= Mutex.create ()
  ; condition= Condition.create ()
  ; delay= Delay.make ()
  }

let choose_protocol = function
  | Rrd_interface.V1 ->
      Rrd_protocol_v1.protocol
  | Rrd_interface.V2 ->
      Rrd_protocol_v2.protocol

let wait_until_next_reading (module D : Debug.DEBUG) ~neg_shift ~uid ~protocol
    ~overdue_count ~reporter =
  let next_reading = RRDD.Plugin.Local.register uid Rrd.Five_Seconds protocol in
  let wait_time = next_reading -. neg_shift in
  let wait_time = if wait_time < 0.1 then wait_time +. 5. else wait_time in
  (* overdue count - 0 if there is no overdue; +1 if there is overdue *)
  if wait_time > 0. then (
    ( match reporter with
    | Some reporter ->
        let (_ : bool) = Delay.wait reporter.delay wait_time in
        ()
    | None ->
        Thread.delay wait_time
    ) ;
    0
  ) else (
    if overdue_count > 1 then (
      (* if register returns negative more than once in a succession,
         				the thread should get delayed till things are normal back again *)
      let backoff_time = 2. ** (float_of_int overdue_count -. 1.) in
      D.debug
        "rrdd says next reading is overdue, seems like rrdd is busy;\n\
         \t\t\t\tBacking off for %.1f seconds"
        backoff_time ;
      match reporter with
      | Some reporter ->
          let (_ : bool) = Delay.wait reporter.delay backoff_time in
          ()
      | None ->
          Thread.delay backoff_time
    ) else
      D.debug "rrdd says next reading is overdue by %.1f seconds; not sleeping"
        (-.wait_time) ;
    overdue_count + 1 (* overdue count incremented *)
  )

let loop (module D : Debug.DEBUG) ~reporter ~report ~cleanup =
  let log_backtrace e =
    let trace = Printexc.(get_raw_backtrace () |> raw_backtrace_to_string) in
    D.error "%s: %s" (Printexc.to_string e) trace
  in

  let running = ref true in
  ( match reporter with
  | Some reporter ->
      with_lock reporter.lock (fun () -> reporter.state <- Running)
  | None ->
      ()
  ) ;
  while !running do
    try
      if !killed then raise Killed ;
      report () ;
      match reporter with
      | Some reporter ->
          (* Handle asynchronous cancellation. *)
          with_lock reporter.lock (fun () ->
              match reporter.state with
              | Running ->
                  ()
              | Stopped _ ->
                  ()
              | Cancelled ->
                  reporter.state <- Stopped `Cancelled ;
                  cleanup () ;
                  Condition.broadcast reporter.condition ;
                  running := false
          )
      | None ->
          ()
    with
    | Sys.Break | Killed ->
        (* Handle cancellation via signal handler. *)
        D.info "received exception Killed or Break - cleaning up" ;
        cleanup () ;
        running := false
    | e ->
        log_backtrace e ;
        D.error "Unexpected error %s, sleeping for 10 seconds..."
          (Printexc.to_string e) ;

        Thread.delay 10.0
  done ;
  D.info "leaving main loop"

let get_state ~reporter = with_lock reporter.lock (fun () -> reporter.state)

let cancel ~reporter =
  with_lock reporter.lock (fun () ->
      match reporter.state with
      | Running ->
          reporter.state <- Cancelled ;
          Delay.signal reporter.delay ;
          Condition.wait reporter.condition reporter.lock
      | Cancelled ->
          Delay.signal reporter.delay ;
          Condition.wait reporter.condition reporter.lock
      | Stopped _ ->
          ()
  )

let wait_until_stopped ~reporter =
  with_lock reporter.lock (fun () ->
      Condition.wait reporter.condition reporter.lock
  )
