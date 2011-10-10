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
(* HTTP handler for connecting to a VM's console (VT100 or VNC) *)

open Printf
open Debug
open Http
open Vmopshelpers
open Xenstore

exception Failure

let get_console_fd domid =
	let pty =
		let pty = ref "" in
		with_xs (fun xs ->
			let path = xs.Xs.getdomainpath domid ^ "/console/tty" in
			pty := xs.Xs.read path
		);
		!pty
		in
	Unix.openfile pty [ Unix.O_RDWR; Unix.O_NOCTTY ] 0o640

let vt100_proxy __context vm_ref s = 
  let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm_ref) in
  let fd = get_console_fd domid in
  
  debug(Printf.sprintf "attempting to connect to domid %d" domid);
  Unixext.proxy fd (Unix.dup s);
  debug("Proxy exited")
    
let register () = Http_svr.add_handler Http.Connect Constants.console_text_uri 
  (Console.handler vt100_proxy)
