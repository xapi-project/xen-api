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

module D = Debug.Make(struct let name = "server_io" end)
open D

type handler = {
  name: string;
  (* body should close the provided fd *)
  body: Unix.sockaddr -> Unix.file_descr -> unit
}

let handler_by_thread (h: handler) (s: Unix.file_descr) (caller: Unix.sockaddr) = 
  Thread.create
    (fun ()->
      Debug.with_thread_named h.name (fun () -> h.body caller s) ()
    ) ()  

let handler_inline (h: handler) (s: Unix.file_descr) (caller: Unix.sockaddr) =
	h.body caller s

(** Function with the main accept loop *)

exception PleaseClose

let set_intersect a b = List.filter (fun x -> List.mem x b) a

(** Establish a server; handler is either 'by_thread' or 'in_this_thread' *)
type sock_or_addr = Server_sockaddr of Unix.sockaddr | Server_fd of Unix.file_descr
let establish_server ?(signal_fds=[]) forker sockoraddr =
  let sock =
    match sockoraddr with
  Server_sockaddr sockaddr ->
	  let domain = match sockaddr with
	    | ADDR_UNIX _ -> (debug "Establishing Unix domain server"; Unix.PF_UNIX)
	    | ADDR_INET(_,_) -> (debug "Establishing inet domain server"; Unix.PF_INET) in
	  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
	    Unix.set_close_on_exec sock;
	    Unix.setsockopt sock Unix.SO_REUSEADDR true;
	    Unix.bind sock sockaddr;
	    Unix.listen sock 5;
	    sock
      | Server_fd fd -> fd in
  while true do  
    try
      let r, _, _ = Unix.select ([ sock ] @ signal_fds) [] [] (-1.) in
      (* If any of the signal_fd is active then bail out *)
      if set_intersect r signal_fds <> [] then raise PleaseClose;
      
      let (s, caller) = Unix.accept sock in
      begin
	try 
	  Unix.set_close_on_exec s;
	  ignore(forker s caller)	
	with exc -> 
	  (* NB provided 'forker' is configured to make a background thread then the
	     only way we can get here is if set_close_on_exec or Thread.create fails.
	     This means we haven't executed any code which could close the fd therefore
	     we should do it ourselves. *)
	  debug "Got exception in server_io.ml: %s" (Printexc.to_string exc);
	  log_backtrace ();
	  Unix.close s;
	  Thread.delay 30.0
      end
    with 
    | PleaseClose ->
	debug "Caught PleaseClose: shutting down server thread";
	raise PleaseClose
    | Unix.Unix_error(err, a, b) ->
	debug "Caught Unix exception in accept: %s in %s %s" (Unix.error_message err) a b;
	Thread.delay 10.
    | e ->
	debug "Caught exception in except: %s" (Printexc.to_string e);
	Thread.delay 10.
  done

type server = { 
  shutdown : unit -> unit
}

let server handler sock = 
  let status_out, status_in = Unix.pipe() in
  let toclose = ref [ sock; status_in; status_out ] in
  let close' fd = 
    if List.mem fd !toclose then begin
      toclose := List.filter (fun x -> x <> fd) !toclose;
      (try Unix.close fd with exn -> warn "Caught exn in Server_io.server: %s" (Printexc.to_string exn))
    end else warn "Attempt to double-shutdown Server_io.server detected; ignoring" in
  let thread = Thread.create 
    (fun () ->
       Debug.with_thread_named handler.name
         (fun () ->
           try
	     establish_server ~signal_fds:[status_out] (handler_by_thread handler) (Server_fd sock)
           with PleaseClose ->
	     debug "Server thread exiting") ()
    ) () in
  let shutdown () = 
    finally 
      (fun () ->
	 let len = Unix.write status_in "!" 0 1 in
	 if len <> 1 then failwith "Failed to signal to server to shutdown";
	 Thread.join thread)
      (fun () ->
	 List.iter close' !toclose
      ) in
  { shutdown = shutdown }
	 
