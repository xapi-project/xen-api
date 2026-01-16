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

module D = Debug.Make (struct let name = "bucket_table" end)

module Make (Key : Map.OrderedType) = struct
  type rate_limit_data = {
      bucket: Token_bucket.t
    ; process_queue:
        (float * (unit -> unit)) Queue.t (* contains token cost and callback *)
    ; process_queue_lock: Mutex.t
    ; worker_thread_cond: Condition.t
    ; should_terminate: bool ref (* signal termination to worker thread *)
    ; worker_thread: Thread.t
  }

  module KeyMap = Map.Make (Key)

  type t = rate_limit_data KeyMap.t Atomic.t

  let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

  let create () = Atomic.make KeyMap.empty

  let mem t ~client_id =
    let map = Atomic.get t in
    KeyMap.mem client_id map

  (* The worker thread is responsible for calling the callback when the token
     amount becomes available *)
  let rec worker_loop ~bucket ~process_queue ~process_queue_lock
      ~worker_thread_cond ~should_terminate =
    let process_item cost callback =
      Token_bucket.delay_then_consume bucket cost ;
      callback ()
    in
    let item_opt =
      with_lock process_queue_lock (fun () ->
          while Queue.is_empty process_queue && not !should_terminate do
            Condition.wait worker_thread_cond process_queue_lock
          done ;
          Queue.take_opt process_queue
      )
    in
    match item_opt with
    | None ->
        (* Queue is empty only when termination was signalled *)
        ()
    | Some (cost, callback) ->
        process_item cost callback ;
        worker_loop ~bucket ~process_queue ~process_queue_lock
          ~worker_thread_cond ~should_terminate

  (* TODO: Indicate failure reason - did we get invalid config or try to add an
     already present client_id? *)
  let add_bucket t ~client_id ~burst_size ~fill_rate =
    let map = Atomic.get t in
    if KeyMap.mem client_id map then
      false
    else
      match Token_bucket.create ~burst_size ~fill_rate with
      | Some bucket ->
          let process_queue = Queue.create () in
          let process_queue_lock = Mutex.create () in
          let worker_thread_cond = Condition.create () in
          let should_terminate = ref false in
          let worker_thread =
            Thread.create
              (fun () ->
                worker_loop ~bucket ~process_queue ~process_queue_lock
                  ~worker_thread_cond ~should_terminate
              )
              ()
          in
          let data =
            {
              bucket
            ; process_queue
            ; process_queue_lock
            ; worker_thread_cond
            ; should_terminate
            ; worker_thread
            }
          in
          let updated_map = KeyMap.add client_id data map in
          Atomic.set t updated_map ; true
      | None ->
          false

  let delete_bucket t ~client_id =
    let map = Atomic.get t in
    match KeyMap.find_opt client_id map with
    | None ->
        ()
    | Some data ->
        with_lock data.process_queue_lock (fun () ->
            data.should_terminate := true ;
            Condition.signal data.worker_thread_cond
        ) ;
        Thread.join data.worker_thread ;
        Atomic.set t (KeyMap.remove client_id map)

  let try_consume t ~client_id amount =
    let map = Atomic.get t in
    match KeyMap.find_opt client_id map with
    | None ->
        false
    | Some data ->
        Token_bucket.consume data.bucket amount

  let peek t ~client_id =
    let map = Atomic.get t in
    Option.map
      (fun contents -> Token_bucket.peek contents.bucket)
      (KeyMap.find_opt client_id map)

  (* The callback should return quickly - if it is a longer task it is
     responsible for creating a thread to do the task *)
  let submit t ~client_id ~callback amount =
    let map = Atomic.get t in
    match KeyMap.find_opt client_id map with
    | None ->
        D.debug "Found no rate limited client_id, returning" ;
        callback ()
    | Some {bucket; process_queue; process_queue_lock; worker_thread_cond; _} ->
        let run_immediately =
          with_lock process_queue_lock (fun () ->
              let immediate =
                Queue.is_empty process_queue
                && Token_bucket.consume bucket amount
              in
              if not immediate then Queue.add (amount, callback) process_queue ;
              Condition.signal worker_thread_cond ;
              immediate
          )
        in
        if run_immediately then callback ()

  (* Block and execute on the same thread *)
  let submit_sync t ~client_id ~callback amount =
    let map = Atomic.get t in
    match KeyMap.find_opt client_id map with
    | None ->
        callback ()
    | Some bucket_data -> (
        let channel_opt =
          with_lock bucket_data.process_queue_lock (fun () ->
              if
                Queue.is_empty bucket_data.process_queue
                && Token_bucket.consume bucket_data.bucket amount
              then
                None
              (* Can run callback immediately after releasing lock *)
              else
                (* Rate limited, need to retrieve function result via channel *)
                let channel = Event.new_channel () in
                Queue.add
                  (amount, fun () -> Event.sync (Event.send channel ()))
                  bucket_data.process_queue ;
                Condition.signal bucket_data.worker_thread_cond ;
                Some channel
          )
        in
        match channel_opt with
        | None ->
            callback ()
        | Some channel ->
            Event.sync (Event.receive channel) ;
            callback ()
      )
end
