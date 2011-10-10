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
(***************************************************************************
 *      Generic Common Code.
 * the following code provide common for the most basic interface to xenbus.
 * it provides sending and receiving functions, and don't add anything.
 **************************************************************************)
exception X
open Stringext
open Xenbus

let pkt_recv con =
	let workdone = ref false in
	while not !workdone
	do
		workdone := Xb.input con
	done;
	Xb.get_in_packet con

let pkt_send con =
	if Xb.has_old_output con then
		raise X;
	let workdone = ref false in
	while not !workdone
	do
		workdone := Xb.output con
	done

let send_packet con tid rid ty data = 
	Xb.queue con (Xb.Packet.create tid rid ty data);
	pkt_send con;
	()

let recv_packet con = Xb.Packet.unpack (pkt_recv con)

let daemon_socket = "/var/run/xenstored/socket"

let open_xb () =
	let sockaddr = Unix.ADDR_UNIX daemon_socket  in
	let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.connect sock sockaddr;
	Unix.set_close_on_exec sock;
	Xb.open_fd sock

let close_xb xb =
	Xb.close xb

