(*
 * Copyright (C) 2007 Citrix System
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 *)

(***************************************************************************
 *      Generic Common Code.
 * the following code provide common for the most basic interface to xenbus.
 * it provides sending and receiving functions, and don't add anything.
 **************************************************************************)
exception X
open Stringext

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

