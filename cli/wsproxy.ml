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
open Wslib

let get_dir_path () = Printf.sprintf "/var/xapi/" 

module LwtWsIteratee = Wslib.Websockets.Wsprotocol(Lwt)
open Lwt.Infix

let with_fd = Lwt_support.with_fd

let start path handler =
  let dir_path = get_dir_path () in
  let fd_sock_path = Printf.sprintf "%s%s" dir_path path in
  Lwt_log.info_f "Starting wsproxy on %s" fd_sock_path >>= fun () ->
  let fd_sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Lwt.catch
    (fun () -> Lwt_unix.unlink fd_sock_path)
    (fun _ -> Lwt.return_unit) >>= fun () ->
  Lwt_unix.bind fd_sock (Unix.ADDR_UNIX fd_sock_path) >>= fun () ->
  let () = Lwt_unix.listen fd_sock 5 in

  let rec loop () =
    let ensure_close = function
      | [] -> Lwt.return_unit
      | fds ->
        Lwt_log.warning_f "Closing %d excess fds" (List.length fds) >>= fun () ->
        List.iter (fun fd -> try Unix.close fd with _ -> ()) fds;
        Lwt.return_unit
    in
    Lwt.catch
      (fun () ->
         Lwt_unix.accept fd_sock
         >>= fun (fd_sock',_) ->
         (* Background thread per connection *)
         let _ : unit Lwt.t =
           let buffer = String.make 16384 '\000' in
           with_fd fd_sock'
             (fun fd ->
                let iov = Lwt_unix.io_vector ~buffer ~offset:0 ~length:16384 in
                Lwt_unix.recv_msg ~socket:fd ~io_vectors:[iov])
           >>= fun (len, newfds) ->
           match newfds with
           | [] -> Lwt_log.warning "No fd to start a connection: not proxying"
           | ufd :: ufds ->
             ensure_close ufds >>= fun () ->
             with_fd (Lwt_unix.of_unix_file_descr ufd)
               (fun fd ->
                  Lwt_log.debug_f "About to start connection" >>= fun () ->
                  Lwt_unix.setsockopt fd Lwt_unix.SO_KEEPALIVE true;
                  let msg = String.sub buffer 0 len in
                  handler fd msg)
         in loop ())
      (fun e ->
         Lwt_log.error_f "Caught exception: %s" (Printexc.to_string e) >>= fun () ->
         Lwt.return_unit)
    >>= fun () -> loop ()

  in
  with_fd fd_sock (fun _ -> loop ())


let proxy (fd : Lwt_unix.file_descr) protocol localport =
  let open LwtWsIteratee in
  let open Lwt_support in
  begin match protocol with
    | "hixie76" ->
      Lwt_log.debug_f "Old-style (hixie76) protocol" >>= fun () ->
      Lwt.return (wsframe_old, wsunframe_old)
    | "hybi10" ->
      Lwt_log.debug_f "New-style (hybi10) protocol" >>= fun () ->
      Lwt.return (wsframe, wsunframe)
    | _ ->
      Lwt_log.warning_f "Unknown protocol, fallback to hybi10" >>= fun () ->
      Lwt.return (wsframe, wsunframe) 
  end >>= fun (frame,unframe) ->
  with_open_connection_fd "localhost" localport ~callback:(fun localfd ->
      let session_id = Uuidm.v `V4 |> Uuidm.to_string in
      Lwt_log.debug_f "Starting proxy session %s" session_id >>= fun () ->
      let thread1 =
        lwt_fd_enumerator localfd (frame (writer (really_write fd) "thread1")) >>= fun _ ->
        Lwt.return_unit in
      let thread2 =
        lwt_fd_enumerator fd (unframe (writer (really_write localfd) "thread2")) >>= fun _ ->
        Lwt.return_unit in
      (* closing the connection in one of the threads above in general leaves the other pending forever,
       * by using choose here, we make sure that as soon as one of the threads completes, both are closed *)
      Lwt.choose [thread1; thread2]
      >>= fun () -> Lwt_log.debug_f "Closing proxy session %s" session_id)


let handler sock msg =
  Lwt_log.debug_f "Got msg: %s" msg >>= fun () ->
  match Re.Str.(split @@ regexp "[:]") msg with
  | [protocol;_;sport]
  | [protocol;sport] ->
    let port = int_of_string sport in
    proxy sock protocol port
  | _ -> Lwt_log.warning "Malformed msg: not proxying"


let _ = 
  (* Enable logging for all levels *)
  Lwt_log.add_rule "*" Lwt_log.Debug;
  Lwt_daemon.daemonize ~stdout:`Dev_null ~stdin:`Close ~stderr:`Dev_null ();
  let filename = "/var/run/wsproxy.pid" in
  (try Unix.unlink filename with _ -> ());
  Lwt_main.run begin
    let pid = Unix.getpid () in
    Lwt_io.with_file filename ~mode:Lwt_io.output (fun chan ->
        Lwt_io.fprintf chan "%d" pid) >>= fun _ ->
    start "wsproxy" handler
  end
