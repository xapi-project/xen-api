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


let get_dir_path () = Printf.sprintf "/var/xapi/" 


module LwtWsIteratee = Websockets.Wsprotocol(Lwt)
open Lwt.Infix

let start path handler =
  let dir_path = get_dir_path () in
  let fd_sock_path = Printf.sprintf "%s%s" dir_path path in
  let fd_sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Lwt.catch 
    (fun () -> Lwt_unix.unlink fd_sock_path) 
    (fun _ -> Lwt.return ()) >>= fun () ->
  let () = Lwt_unix.bind fd_sock (Unix.ADDR_UNIX fd_sock_path) in
  let () = Lwt_unix.listen fd_sock 5 in

  let rec loop () =
    Lwt.catch 
      (fun () ->
         Lwt_unix.accept fd_sock >>= fun (fd_sock2,_) ->
         let buffer = String.make 16384 '\000' in
         let iov = Lwt_unix.io_vector buffer 0 16384 in
         (
           try Lwt_unix.recv_msg fd_sock2 [iov] 
           with e ->
             Lwt_unix.close fd_sock2 >>= fun () -> 
             Lwt.return (0,[])
         ) >>= fun (len,newfds) ->
         let msg = String.sub buffer 0 len in
         Lwt_unix.close fd_sock2 >>= fun _ ->
         List.iter (fun fd -> Printf.printf "got fd: %d\n%!" (Obj.magic fd)) newfds;
         Printf.printf "About to fixup the fd\n%!";
         ignore(handler (Lwt_unix.of_unix_file_descr (List.hd newfds)) msg);
         loop ()
      )
      (fun e ->
         Printf.printf "Caught exception: %s\n" (Printexc.to_string e);
         loop ()
      )
  in
  loop ()

let proxy (fd : Lwt_unix.file_descr) protocol ty localport =
  let open LwtWsIteratee in
  let open Lwt_support in
  let (frame,unframe) =
    match protocol with
    | "hixie76" -> 
      Printf.printf "old-style\n%!"; 
      (wsframe_old, wsunframe_old)
    | "hybi10" -> 
      Printf.printf "new-style\n%!";
      (wsframe, wsunframe)
    | _ -> 
      Printf.printf "unknown\n%!";
      (wsframe,wsunframe)
  in
  let (realframe,realunframe) = (frame, unframe) in
  open_connection_fd "localhost" localport >>= fun localfd -> 
  let thread1 = lwt_fd_enumerator localfd (realframe (writer (really_write fd) "thread1")) >>= fun _ -> 
    Lwt.return () in
  let thread2 = lwt_fd_enumerator fd (realunframe (writer (really_write localfd) "thread2")) >>= fun _ -> 
    Lwt.return () in
  Lwt.catch 
    (fun () -> Lwt.join [thread1; thread2] >>= fun _ -> 
      Lwt_unix.close fd)
    (fun e -> Lwt.return ())

let handler sock msg =
  Lwt_io.printf "Got msg: %s\n" msg >>= fun _ ->
  match Re_str.split (Re_str.regexp "[:]") msg with
  | [protocol;ty;sport] ->
    let port = int_of_string sport in
    proxy sock protocol ty port
  | [protocol;sport] ->
    let port = int_of_string sport in
    proxy sock protocol "hybi10" port
  | _ -> 
    Lwt_unix.close sock

let _ =
  if Array.length Sys.argv > 1 
  then Lwt.return (Websockets.runtest ())
  else 
    begin
      Lwt_daemon.daemonize ~stdout:`Dev_null ~stdin:`Close ~stderr:`Dev_null ();
      let filename = "/var/run/wsproxy.pid" in
      (try Unix.unlink filename with _ -> ());
      Lwt_main.run begin
        let pid = Unix.getpid () in
        Lwt_io.with_file filename ~mode:Lwt_io.output (fun chan ->
            Lwt_io.fprintf chan "%d" pid) >>= fun _ -> 
        start "wsproxy" handler
      end
    end
