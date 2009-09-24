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
let r1 = Str.regexp "> \\([0-9a-f:]+\\).*> \\([0-9.]+\\).bootpc: ";;
let r2 = Str.regexp "\\([0-9.]+\\) is-at \\([0-9a-f:]+\\)";;

let getip1 mac line =
  try
    ignore(Str.search_forward r1 line 0);
    if mac=Str.matched_group 1 line then
      Some (Str.matched_group 2 line)
    else None
  with _ -> None

let getip2 mac line =
  try
    ignore(Str.search_forward r2 line 0);
    if mac=Str.matched_group 2 line then
      Some (Str.matched_group 1 line)
    else None
  with _ -> None

let _ =
  let iface = Sys.argv.(1) in
  let mac = String.lowercase Sys.argv.(2) in
  let cmd = Printf.sprintf "tcpdump -lne -i %s arp or udp port bootps" iface in
  let ic = Unix.open_process_in cmd in
  
  let rec inner () =
    let line = String.lowercase (input_line ic) in
    
    (* Try first regexp *)
    let ip = getip1 mac line in
    match ip with 
	Some x -> x 
      | None ->
	  let ip2 = getip2 mac line in
	  match ip2 with
	      Some x -> x
	    | None -> inner ()
  in 
  let ip = inner () in
  Printf.printf "%s" ip

	
