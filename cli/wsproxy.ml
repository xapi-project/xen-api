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

module LwtWsIteratee = Wslib.Websockets.Wsprotocol(Lwt)
open Lwt.Infix

let with_fd = Lwt_support.with_fd

let start handler =
  let fd_sock_path = "/var/xapi/wsproxy" in
  Logs_lwt.info (fun m -> m "Starting wsproxy on %s" fd_sock_path) >>= fun () ->
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
        Logs_lwt.warn (fun m -> m "Closing %d excess fds" (List.length fds)) >>= fun () ->
        List.iter (fun fd -> try Unix.close fd with _ -> ()) fds;
        Lwt.return_unit
    in
    Lwt.catch
      (fun () ->
         Lwt_unix.accept fd_sock
         >>= fun (fd_sock',_) ->
         (* Background thread per connection *)
         let _ : unit Lwt.t =
           let buffer = Bytes.make 16384 '\000' in
           with_fd fd_sock'
             ~callback:(fun fd ->
                 let io_vectors = Lwt_unix.IO_vectors.create () in
                 Lwt_unix.IO_vectors.append_bytes io_vectors buffer 0 16384;
                 Lwt_unix.Versioned.recv_msg_2 ~socket:fd ~io_vectors)
           >>= fun (len, newfds) ->
           match newfds with
           | [] -> Logs_lwt.warn (fun m -> m "No fd to start a connection: not proxying")
           | ufd :: ufds ->
             ensure_close ufds >>= fun () ->
             with_fd (Lwt_unix.of_unix_file_descr ufd)
               ~callback:(fun fd ->
                   Logs_lwt.debug (fun m -> m "About to start connection") >>= fun () ->
                   Lwt_unix.setsockopt fd Lwt_unix.SO_KEEPALIVE true;
                   let msg = Bytes.(to_string @@ sub buffer 0 len) in
                   handler fd msg)
         in loop ())
      (fun e ->
         Logs_lwt.err (fun m -> m "Caught exception: %s" (Printexc.to_string e)) >>= fun () ->
         Lwt.return_unit)
    >>= fun () -> loop ()

  in
  with_fd fd_sock ~callback:(fun _ -> loop ())


let proxy (fd : Lwt_unix.file_descr) addr protocol =
  let open LwtWsIteratee in
  let open Lwt_support in
  begin match protocol with
    | "hixie76" ->
      Logs_lwt.debug (fun m -> m "Old-style (hixie76) protocol") >>= fun () ->
      Lwt.return (wsframe_old, wsunframe_old)
    | "hybi10" ->
      Logs_lwt.debug (fun m -> m "New-style (hybi10) protocol") >>= fun () ->
      Lwt.return (wsframe, wsunframe)
    | _ ->
      Logs_lwt.warn (fun m -> m "Unknown protocol, fallback to hybi10") >>= fun () ->
      Lwt.return (wsframe, wsunframe)
  end >>= fun (frame,unframe) ->
  with_open_connection_fd addr ~callback:(fun localfd ->
      let session_id = Uuidm.v `V4 |> Uuidm.to_string in
      Logs_lwt.debug (fun m -> m "Starting proxy session %s" session_id) >>= fun () ->
      let thread1 =
        lwt_fd_enumerator localfd (frame (writer (really_write fd) "thread1")) >>= fun _ ->
        Lwt.return_unit in
      let thread2 =
        lwt_fd_enumerator fd (unframe (writer (really_write localfd) "thread2")) >>= fun _ ->
        Lwt.return_unit in
      (* closing the connection in one of the threads above in general leaves the other pending forever,
       * by using choose here, we make sure that as soon as one of the threads completes, both are closed *)
      Lwt.choose [thread1; thread2]
      >>= fun () -> Logs_lwt.debug (fun m -> m "Closing proxy session %s" session_id))


module RX = struct
  let socket = Re.Str.regexp "^/var/run/xen/vnc-[0-9]+$"
  let port   = Re.Str.regexp "^[0-9]+$"
end

let handler sock msg =
  Logs_lwt.debug (fun m -> m "Got msg: '%s'" msg) >>= fun () ->
  match Re.Str.(split @@ regexp "[:]") msg with
  | [protocol;_;path]
  | [protocol;path] when Re.Str.string_match RX.socket path 0 ->
    let addr = Unix.ADDR_UNIX path in
    proxy sock addr protocol
  | [protocol;_;sport]
  | [protocol;sport] when Re.Str.string_match RX.port sport 0 ->
    let localhost = Unix.inet_addr_loopback in
    let addr = Unix.ADDR_INET(localhost, int_of_string sport) in
    proxy sock addr protocol
  | _ -> Logs_lwt.warn (fun m -> m "The message '%s' is malformed: not proxying" msg)

(* Reporter taken from
 * https://erratique.ch/software/logs/doc/Logs_lwt/index.html#report_ex
 * under ISC License *)
let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

let _ =
  Logs.set_reporter (lwt_reporter ());
  Logs.set_level ~all:true (Some Logs.Info);
  Lwt_main.run (start handler)
