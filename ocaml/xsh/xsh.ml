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

module D = Debug.Make (struct let name = "xsh" end)

open D

type endpoint = {
    fdin: Unix.file_descr
  ; fdout: Unix.file_descr
  ; mutable buffer: bytes
  ; mutable buffer_len: int
}

let make_endpoint fdin fdout =
  {fdin; fdout; buffer= Bytes.make 4096 '\000'; buffer_len= 0}

open Safe_resources

let proxy (ain : Unix.file_descr) (aout : Unix.file_descr) (bin : Unixfd.t)
    (bout : Unix.file_descr) =
  let a' = make_endpoint ain aout and b' = make_endpoint Unixfd.(!bin) bout in
  Unix.set_nonblock ain ;
  Unix.set_nonblock aout ;
  Unix.set_nonblock Unixfd.(!bin) ;
  Unix.set_nonblock bout ;
  let can_read x = x.buffer_len < Bytes.length x.buffer - 1 in
  let can_write x = x.buffer_len > 0 in
  let write_from x y =
    let written = Unix.single_write y.fdout x.buffer 0 x.buffer_len in
    Bytes.blit x.buffer written x.buffer 0 (x.buffer_len - written) ;
    x.buffer_len <- x.buffer_len - written
  in
  let read_into x =
    let read =
      Unix.read x.fdin x.buffer x.buffer_len
        (Bytes.length x.buffer - x.buffer_len)
    in
    if read = 0 then raise End_of_file ;
    x.buffer_len <- x.buffer_len + read
  in
  try
    while true do
      let r =
        (if can_read a' then [ain] else [])
        @ if can_read b' then [Unixfd.(!bin)] else []
      in
      let w =
        (if can_write a' then [bout] else [])
        @ if can_write b' then [aout] else []
      in
      let epoll = Polly.create () in
      List.iter (fun fd -> Polly.add epoll fd Polly.Events.inp) r ;
      List.iter (fun fd -> Polly.add epoll fd Polly.Events.out) w ;
      Fun.protect
        ~finally:(fun () -> Polly.close epoll)
        (fun () ->
          ignore
          @@ Polly.wait epoll 4 (-1) (fun _ fd _ ->
                 (* Note: only one fd is handled *)
                 if aout = fd then
                   write_from b' a'
                 else if bout = fd then
                   write_from a' b'
                 else if ain = fd then
                   read_into a'
                 else
                   read_into b'
             )
        )
    done
  with _ -> (
    (try Unix.clear_nonblock ain with _ -> ()) ;
    (try Unix.clear_nonblock Unixfd.(!bin) with _ -> ()) ;
    (try Unix.clear_nonblock aout with _ -> ()) ;
    (try Unix.clear_nonblock bout with _ -> ()) ;
    (try Unix.close ain with _ -> ()) ;
    (try Unixfd.safe_close bin with _ -> ()) ;
    (try Unix.close aout with _ -> ()) ;
    try Unix.close bout with _ -> ()
  )

let init_tls_verification () =
  let file = Constants.verify_certificates_path in
  match Sys.file_exists file with
  | false ->
      warn "TLS verification is disabled on this host: %s is absent" file ;
      Stunnel_client.set_verify_by_default false
  | true ->
      info "TLS verification is enabled: %s is present" file ;
      Stunnel_client.set_verify_by_default true

let with_open_tcp_ssl server f =
  let port = 443 in
  (* We don't bother closing fds since this requires our close_and_exec wrapper *)
  Stunnel.with_connect ~use_fork_exec_helper:false
    ~write_to_log:(fun _ -> ())
    ~verify_cert:(Stunnel_client.pool ()) server port
  @@ fun x -> f x.Stunnel.fd

let _ =
  let host = Sys.argv.(1) in
  let cmd = Sys.argv.(2) in
  let session =
    try Sys.getenv "XSH_SESSION" with _ -> failwith "Session not provided"
  in
  let args =
    List.map
      (fun arg -> "&arg=" ^ arg)
      (List.tl (List.tl (List.tl (Array.to_list Sys.argv))))
  in
  let req =
    Printf.sprintf "CONNECT /remotecmd?session_id=%s&cmd=%s%s http/1.0\r\n\r\n"
      session cmd (String.concat "" args)
  in
  init_tls_verification () ;
  with_open_tcp_ssl host @@ fun fd ->
  Unix.write_substring Unixfd.(!fd) req 0 (String.length req) |> ignore ;
  proxy Unix.stdin Unix.stdout fd (Unix.dup Unixfd.(!fd))
