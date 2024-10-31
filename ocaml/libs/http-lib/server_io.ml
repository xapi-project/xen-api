(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(* open Unix *)
open Xapi_stdext_pervasives.Pervasiveext

module D = Debug.Make (struct let name = "server_io" end)

open D

type handler = {
    name: string
  ; (* body should close the provided fd *)
    body: Unix.sockaddr -> Unix.file_descr -> unit
  ; lock: Semaphore.Counting.t
}

module WorkerPool = struct
  type task = unit -> unit

  type t = {
      queue: task Queue.t
    ; size: int
    ; lock: Mutex.t
    ; cond_non_empty: Condition.t
  }

  let worker p =
    let next () =
      Mutex.lock p.lock ;
      while Queue.is_empty p.queue do
        Condition.wait p.cond_non_empty p.lock
      done ;
      let task = Queue.pop p.queue in
      Mutex.unlock p.lock ;
      ignore (task ())
    in
    while true do
      next ()
    done

  let create ~size =
    let queue = Queue.create () in
    let lock = Mutex.create () in
    let cond_non_empty = Condition.create () in
    let pool = {queue; size; lock; cond_non_empty} in
    ignore (List.init size (fun _ -> Thread.create worker pool)) ;
    pool

  let enqueue p task =
    if Mutex.try_lock p.lock then (
      let len = Queue.length p.queue in
      if len = p.size then (
        Mutex.unlock p.lock ; false
      ) else
        let () = Queue.push task p.queue in
        Condition.signal p.cond_non_empty ;
        Mutex.unlock p.lock ;
        true
    ) else
      false
end

let handler_by_thread (h : handler) (s : Unix.file_descr)
    (caller : Unix.sockaddr) =
  let go () =
    Fun.protect
      ~finally:(fun () -> Semaphore.Counting.release h.lock)
      (Debug.with_thread_named h.name (fun () -> h.body caller s))
  in
  ignore (Thread.create go ())

let handler_by_thread_pool size =
  let pool = WorkerPool.create ~size in
  let dispatcher h s c =
    let task () =
      let finally () = Semaphore.Counting.release h.lock in
      let go = Debug.with_thread_named h.name (fun () -> h.body c s) in
      Fun.protect ~finally go
    in
    (* If we cannot access pool's queue, create a new thread. *)
    if not (WorkerPool.enqueue pool task) then
      ignore (Thread.create task ())
  in
  dispatcher

(* Decide between old-style forking versus creating and servicing
   requests using a worker pool. *)
let create_request_forker ?worker_pool_size () =
  match worker_pool_size with
  | Some size when size > 0 && size <= 16 ->
      handler_by_thread_pool size
  | _ ->
      handler_by_thread

(** Function with the main accept loop *)

exception PleaseClose

let establish_server ?(signal_fds = []) forker handler sock =
  let epoll = Polly.create () in
  List.iter (fun fd -> Polly.add epoll fd Polly.Events.inp) (sock :: signal_fds) ;
  while true do
    try
      ignore
      @@ Polly.wait epoll 2 (-1) (fun _ fd _ ->
             (* If any of the signal_fd is active then bail out *)
             if List.mem fd signal_fds then raise PleaseClose ;
             Semaphore.Counting.acquire handler.lock ;
             let s, caller = Unix.accept ~cloexec:true sock in
             try ignore (forker handler s caller)
             with exc ->
               (* NB provided 'forker' is configured to make a background thread then the
                  only way we can get here is if Thread.create fails.
                  This means we haven't executed any code which could close the fd therefore
                  we should do it ourselves. *)
               debug "Got exception in server_io.ml: %s" (Printexc.to_string exc) ;
               log_backtrace () ;
               Unix.close s ;
               Thread.delay 30.0
         )
    with
    | PleaseClose ->
        debug "Caught PleaseClose: shutting down server thread" ;
        Polly.close epoll ;
        raise PleaseClose
    | Unix.Unix_error (err, a, b) ->
        debug "Caught Unix exception in accept: %s in %s %s"
          (Unix.error_message err) a b ;
        Thread.delay 10.
    | e ->
        debug "Caught exception in except: %s" (Printexc.to_string e) ;
        Thread.delay 10.
  done

type server = {shutdown: unit -> unit}

let server ?worker_pool_size handler sock =
  let status_out, status_in = Unix.pipe () in
  let toclose = ref [sock; status_in; status_out] in
  let close' fd =
    if List.mem fd !toclose then (
      toclose := List.filter (fun x -> x <> fd) !toclose ;
      try Unix.close fd
      with exn ->
        warn "Caught exn in Server_io.server: %s" (Printexc.to_string exn)
    ) else
      warn "Attempt to double-shutdown Server_io.server detected; ignoring"
  in
  let forker = create_request_forker ?worker_pool_size () in
  let thread =
    Thread.create
      (fun () ->
        Debug.with_thread_named handler.name
          (fun () ->
            try establish_server ~signal_fds:[status_out] forker handler sock
            with PleaseClose -> debug "Server thread exiting"
          )
          ()
      )
      ()
  in
  let shutdown () =
    finally
      (fun () ->
        let len = Unix.write status_in (Bytes.of_string "!") 0 1 in
        if len <> 1 then failwith "Failed to signal to server to shutdown" ;
        Thread.join thread
      )
      (fun () -> List.iter close' !toclose)
  in
  {shutdown}
