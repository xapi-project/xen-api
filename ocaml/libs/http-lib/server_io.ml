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
  ; lock: Xapi_stdext_threads.Semaphore.t
}

let handler_by_thread ?nice (h : handler) (s : Unix.file_descr)
    (caller : Unix.sockaddr) =
  Thread.create
    (fun () ->
      Fun.protect
        ~finally:(fun () -> Xapi_stdext_threads.Semaphore.release h.lock 1)
        (Debug.with_thread_named h.name (fun () ->
             Option.iter
               (fun nice ->
                 let n = Unix.nice nice in
                 debug "New nice level for thread %s is %d" h.name n
               )
               nice ;
             h.body caller s
         )
        )
    )
    ()

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
             Xapi_stdext_threads.Semaphore.acquire handler.lock 1 ;
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

let server ?nice handler sock =
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
  let thread =
    Thread.create
      (fun () ->
        Debug.with_thread_named handler.name
          (fun () ->
            try
              Option.iter
                (fun nice ->
                  let n = Unix.nice nice in
                  debug "New nice level for thread %s is %d" handler.name n
                )
                nice ;
              establish_server ~signal_fds:[status_out]
                (handler_by_thread ?nice) handler sock
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
