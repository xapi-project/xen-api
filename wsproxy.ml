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


let start path handler =
  let dir_path = get_dir_path () in
  let fd_sock_path = Printf.sprintf "%s%s" dir_path path in
  let fd_sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  lwt () = (try_lwt Lwt_unix.unlink fd_sock_path with _ -> Lwt.return ()) in
  let () = Lwt_unix.bind fd_sock (Unix.ADDR_UNIX fd_sock_path) in
  let () = Lwt_unix.listen fd_sock 5 in
  
  let rec loop () =
    try_lwt 
		lwt (fd_sock2,_) = Lwt_unix.accept fd_sock in
        let buffer = String.make 16384 '\000' in
		let iov = Lwt_unix.io_vector buffer 0 16384 in
		lwt (len,newfds) = 
            try
				Lwt_unix.recv_msg fd_sock2 [iov] 
			with e ->
				lwt () = Lwt_unix.close fd_sock2 in
                Lwt.return (0,[])
        in
        let msg = String.sub buffer 0 len in
        lwt _ = Lwt_unix.close fd_sock2 in
        List.iter (fun fd -> Printf.printf "got fd: %d\n%!" (Obj.magic fd)) newfds;
        Printf.printf "About to fixup the fd\n%!";
        ignore(handler (Lwt_unix.of_unix_file_descr (List.hd newfds)) msg);
        loop ()
    with e ->
		Printf.printf "Caught exception: %s\n" (Printexc.to_string e);
		loop ()
  in
  loop ()

let ($) f x = f x

let proxy (fd : Lwt_unix.file_descr) protocol ty localport =
  let open LwtWsIteratee in
  let open Lwt_support in
  let (>>=) = Lwt.bind in
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
  let (realframe,realunframe) = ((fun s -> base64encode (frame s)), (fun s -> unframe (base64decode s))) in
  open_connection_fd "localhost" localport >>= fun localfd -> 
  let thread1 = lwt_fd_enumerator localfd $ realframe (writer (really_write fd) "thread1") >>= fun _ -> Lwt.return () in
  let thread2 = lwt_fd_enumerator fd (realunframe (writer (really_write localfd) "thread2")) >>= fun _ -> Lwt.return () in
  try_lwt 
	  Lwt.join [thread1; thread2] >>= (fun _ -> Lwt_unix.close fd)
  with e -> 
	Lwt.return ()

let handler sock msg =
  lwt _ = Lwt_io.printf "Got msg: %s\n" msg in
  match Stringext.String.split ':' msg with
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
  then Websockets.runtest ()
  else Lwt_main.run (start "wsproxy" handler)
