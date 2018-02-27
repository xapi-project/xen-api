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
open Http_daemon
open Http_types

let callback req outchan =
  (* Printf.fprintf stderr "Doing callback\n"; *)
  let response = "HELLO" in
  (*
    print_endline req#body;
  *)
  List.iter (output_string outchan)
    ["HTTP/1.0 200 OK\r\n";
     "content-type: text/html\r\n";
     "\r\n\r\n";
     response;
     "\r\n"];
  (*
    print_endline response;
  *)
  close_out outchan


let http_spec =
  { address = "0.0.0.0";
    auth = None;
    callback = callback;
    mode = `Thread;
    port = Xapi_globs.default_cleartext_port;
    root_dir = None;
    exn_handler = None;
    timeout = None }

(* start http server *)
let _ = Thread.create (fun ()->Http_daemon.main http_spec) ()

let rec allocator() =
  let x = Bytes.create 100000 in
  Thread.delay 0.01;
  allocator()

let rec minor() =
  Gc.minor();
  minor()

let rec compact() =
  Gc.compact();
  Thread.delay 1.0;
  compact()

let headers host path content_length = [
  Printf.sprintf "POST %s HTTP/1.0" path;
  (* User-Agent: Frontier/5.1.2 (WinNT) *)
  Printf.sprintf "Host: %s" host;
  "Content-Type: text/xml";
  Printf.sprintf "Content-length: %d" content_length;
]


let do_http_rpc host port path body f =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let he = Unix.gethostbyname host in
  if Array.length he.Unix.h_addr_list = 0
  then failwith (Printf.sprintf "Couldn't resolve hostname: %s" host);
  let ip = he.Unix.h_addr_list.(0) in
  let addr = Unix.ADDR_INET(ip, port) in
  Unix.handle_unix_error (Unix.connect s) addr;

  let oc = Unix.out_channel_of_descr s in
  let writeln x = output_string oc x; output_string oc "\r\n" in

  List.iter writeln (headers host path (String.length body));
  writeln "";
  writeln body;
  flush oc;

  let ic = Unix.in_channel_of_descr s in
  (try
     while true do
       let line = input_line ic in
       (* NB input_line removes the final '\n'.
                 RFC1945 says to expect a '\r\n' (- '\n' = '\r') *)
       match line with
       | "" | "\r" -> raise Not_found
       | _ -> ()
     done
   with Not_found -> () | End_of_file -> ());
  let result = f ic in
  Printf.printf ".";
  flush(stdout);
  Unix.close s;
  result

let bigbody = Bytes.create 20000

let rec requester() =
  do_http_rpc "localhost" Xapi_globs.default_cleartext_port "whateverdontcare" bigbody
    (fun x->x);
  requester()


(*
(* spawn threads to do lots of allocating *)
let _ = Thread.create (fun ()->allocator()) ()
let _ = Thread.create (fun ()->allocator()) ()
let _ = Thread.create (fun ()->allocator()) ()
let _ = Thread.create (fun ()->allocator()) ()

(* spawn threads to thrash GC *)
let _ = Thread.create minor ()
let _ = Thread.create compact ()

(* spawn threads to do lots of requests *)
let _ = Thread.create requester ()
let _ = Thread.create requester ()
let _ = Thread.create requester ()
let _ = Thread.create requester ()
let _ = Thread.create requester ()
let _ = Thread.create requester ()
let never_dies = Thread.create requester ()

(* this thread just waits... *)

let _ = Thread.join never_dies
*)

let rec spin() = spin ()

let _ = spin()
