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

let ignore_exn t () = Lwt.catch t (fun _ -> Lwt.return_unit)

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
         Lwt.catch
           (fun () ->
              let iov = Lwt_unix.io_vector ~buffer ~offset:0 ~length:16384 in
              Lwt_unix.recv_msg ~socket:fd_sock2 ~io_vectors:[iov])
           (fun e ->
              Printf.printf "Caught exception: %s\n" (Printexc.to_string e);
              ignore_exn (fun () -> Lwt_unix.close fd_sock2) () >>= fun () ->
              Lwt.return (0,[]))
         >>= fun (len,newfds) ->
         Lwt_unix.close fd_sock2 >>= fun () ->
         Lwt.catch
           (fun () ->
              let fdh :: fdt = newfds in
              let msg = String.sub buffer 0 len in
              let fdno = Printf.sprintf "%d" (Obj.magic fdh) in
              List.iter (fun fd -> Printf.printf "closing fds: %d\n%!" (Obj.magic fd)) fdt;
              List.iter (fun fd -> try Unix.close fd with _ -> ()) fdt;
              Printf.printf "About to fixup: %s\n%!" fdno;
              let fd = Lwt_unix.of_unix_file_descr fdh in
              Lwt_unix.setsockopt fd Lwt_unix.SO_KEEPALIVE true;
              handler fd msg)
           (fun e ->
              Printf.printf "Caught exception: %s\n" (Printexc.to_string e);
              List.iter (fun fd -> Printf.printf "closing fd: %d\n%!" (Obj.magic fd)) newfds;
              List.iter (fun fd -> try Unix.close fd with _ -> ()) newfds;
              Lwt.return ()) 
         >>= fun _ ->
         loop ()
      )
      (fun e ->
         Printf.printf "Caught exception: %s\n" (Printexc.to_string e);
         loop ()
      )
  in
  loop ()

let proxy (fd : Lwt_unix.file_descr) protocol _ty localport =
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
  let fdno = Printf.sprintf "%d" (Obj.magic fd) in
  open_connection_fd "localhost" localport >>= fun localfd ->
  Lwt.finalize
    (fun () ->
       let thread1 =
         lwt_fd_enumerator localfd (realframe (writer (really_write fd) "thread1")) >>= fun _ ->
         Lwt.return () in
       let thread2 =
         lwt_fd_enumerator fd (realunframe (writer (really_write localfd) "thread2")) >>= fun _ ->
         Lwt.return () in
       ignore_exn (fun () -> Lwt.join [thread1; thread2]) ()
    )
    (fun _ ->
       ignore_exn (fun () -> Lwt_unix.close fd) () >>= fun () ->
       ignore_exn (fun () -> Lwt_unix.close localfd) ())
  >>= fun () ->
  Printf.printf "FD %s pipe closed: %b %b\n" fdno Lwt_unix.(state fd == Closed) Lwt_unix.(state localfd == Closed)
  |> Lwt.return

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
    let sid = Printf.sprintf "%d" (Obj.magic (Lwt_unix.unix_file_descr sock)) in
    ignore_exn (fun () -> Lwt_unix.close sock) () >>= fun () ->
    Printf.printf "Malformed message. Socket %s closed: %b\n" sid Lwt_unix.(state sock == Closed)
    |> Lwt.return

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
