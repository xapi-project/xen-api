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
let init_term () =
  let attr = Unix.tcgetattr Unix.stdin in

  let nattr = { attr with
                Unix.c_ignbrk = false; Unix.c_brkint = false;
                Unix.c_parmrk = false; Unix.c_inlcr = false;
                Unix.c_igncr = false; Unix.c_icrnl = false;
                Unix.c_ixon = false;

                Unix.c_opost = false;

                Unix.c_echo = false; Unix.c_echonl = false;
                Unix.c_icanon = false; Unix.c_isig = false;
                (* Unix.c_iexten = false; *)
                Unix.c_csize = 8; Unix.c_parenb = true; (* Unix.c_cs = false *) } in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH nattr;
  attr

let console_loop fd =
  let buf = String.make 512 '\000' in
  let quit = ref false in
  while not !quit
  do
    let r,_,_ = Unix.select [ Unix.stdin; fd ] [] [] (-1.) in
    if List.mem fd r then (
      let rd = Unix.read fd buf 0 512 in
      ignore (Unix.write Unix.stdin buf 0 rd)
    ) else if List.mem Unix.stdin r then (
      let rd = Unix.read Unix.stdin buf 0 60 in
      if rd = 1 && buf.[0] = (Char.chr 0x1d) then
        quit := true
      else
        ignore (Unix.write fd buf 0 rd)
    )
  done

let restore_term attr =
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH attr

let _ =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "usage: console <hostname> <port>";
    exit 1
  );
  let host = Sys.argv.(1) and port = int_of_string (Sys.argv.(2)) in

  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let he = Unix.gethostbyname host in

  if Array.length he.Unix.h_addr_list = 0 then
    failwith (Printf.sprintf "Couldn't resolve hostname: %s" host);
  let ip = he.Unix.h_addr_list.(0) in
  let addr = Unix.ADDR_INET(ip, port) in

  Unix.connect fd addr;

  let oldattr = init_term () in
  console_loop fd;
  restore_term oldattr;
