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

type rate_limit_data = {
    bucket: Token_bucket.t
  ; process_queue:
      (float * (unit -> unit)) Queue.t (* contains token cost and callback *)
  ; process_queue_lock: Mutex.t
  ; worker_thread_cond: Condition.t
  ; should_terminate: bool ref (* signal termination to worker thread *)
  ; worker_thread: Thread.t
}
[@@warning "-69"]

type t = {
    table: (string, rate_limit_data) Hashtbl.t
  ; mutable readers: int
  ; readers_lock: Mutex.t (* protects readers count *)
  ; table_lock: Mutex.t
        (* held collectively by readers, exclusively by writers *)
}

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let with_read_lock t f =
  with_lock t.readers_lock (fun () ->
      t.readers <- t.readers + 1 ;
      if t.readers = 1 then Mutex.lock t.table_lock
  ) ;
  Fun.protect f ~finally:(fun () ->
      with_lock t.readers_lock (fun () ->
          t.readers <- t.readers - 1 ;
          if t.readers = 0 then Mutex.unlock t.table_lock
      )
  )

let with_write_lock t f = with_lock t.table_lock f

let create () =
  {
    table= Hashtbl.create 10
  ; readers= 0
  ; readers_lock= Mutex.create ()
  ; table_lock= Mutex.create ()
  }

(* The worker thread is responsible for calling the callback when the token
   amount becomes available *)
let rec worker_loop ~bucket ~process_queue ~process_queue_lock
    ~worker_thread_cond ~should_terminate =
  let process_item cost callback =
    Token_bucket.delay_then_consume bucket cost ;
    callback ()
  in
  Mutex.lock process_queue_lock ;
  while Queue.is_empty process_queue && not !should_terminate do
    Condition.wait worker_thread_cond process_queue_lock
  done ;
  let item_opt = Queue.take_opt process_queue in
  Mutex.unlock process_queue_lock ;
  match item_opt with
  | None ->
      (* Queue is empty only when termination was signalled *)
      ()
  | Some (cost, callback) ->
      process_item cost callback ;
      worker_loop ~bucket ~process_queue ~process_queue_lock ~worker_thread_cond
        ~should_terminate

(* TODO: Indicate failure reason - did we get invalid config or try to add an
   already present user_agent? *)
let add_bucket t ~user_agent ~burst_size ~fill_rate =
  with_write_lock t (fun () ->
      if Hashtbl.mem t.table user_agent then
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
            Hashtbl.add t.table user_agent data ;
            true
        | None ->
            false
  )

let delete_bucket t ~user_agent =
  with_write_lock t (fun () ->
      match Hashtbl.find_opt t.table user_agent with
      | None ->
          ()
      | Some data ->
          Mutex.lock data.process_queue_lock ;
          data.should_terminate := true ;
          Condition.signal data.worker_thread_cond ;
          Mutex.unlock data.process_queue_lock ;
          Hashtbl.remove t.table user_agent
  )

let try_consume t ~user_agent amount =
  with_read_lock t (fun () ->
      match Hashtbl.find_opt t.table user_agent with
      | None ->
          false
      | Some data ->
          Token_bucket.consume data.bucket amount
  )

let peek t ~user_agent =
  with_read_lock t (fun () ->
      Option.map
        (fun contents -> Token_bucket.peek contents.bucket)
        (Hashtbl.find_opt t.table user_agent)
  )

(* The callback should return quickly - if it is a longer task it is
   responsible for creating a thread to do the task *)
let submit t ~user_agent ~callback amount =
  match with_read_lock t (fun () -> Hashtbl.find_opt t.table user_agent) with
  | None ->
      callback ()
  | Some {bucket; process_queue; process_queue_lock; worker_thread_cond; _} ->
      with_lock process_queue_lock (fun () ->
          if Queue.is_empty process_queue && Token_bucket.consume bucket amount
          then
            callback ()
          else
            let need_signal = Queue.is_empty process_queue in
            Queue.add (amount, callback) process_queue ;
            if need_signal then Condition.signal worker_thread_cond
      )

let submit_sync t ~user_agent ~callback amount =
  let result = ref None in
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let wrapped_callback () =
    let r = callback () in
    Mutex.lock mutex ;
    result := Some r ;
    Condition.signal condition ;
    Mutex.unlock mutex
  in
  submit t ~user_agent ~callback:wrapped_callback amount ;
  Mutex.lock mutex ;
  while Option.is_none !result do
    Condition.wait condition mutex
  done ;
  Mutex.unlock mutex ;
  Option.get !result
