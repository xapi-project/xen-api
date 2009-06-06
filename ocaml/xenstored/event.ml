(*
 * Copyright (c) 2006-2007 XenSource Inc.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 * All rights reserved.
 *)

(**************** high level binding ****************)
type t = {
	fd: Unix.file_descr;
	mutable virq_port: int;
}

let init () = { fd = Eventchn.init (); virq_port = -1; }
let bind_virq eventchn = eventchn.virq_port <- Eventchn.bind_virq eventchn.fd
let bind_interdomain eventchn domid port = Eventchn.bind_interdomain eventchn.fd domid port
let unbind eventchn port = Eventchn.unbind eventchn.fd port
let notify eventchn port = Eventchn.notify eventchn.fd port
let read_port eventchn = Eventchn.read_port eventchn.fd
let write_port eventchn port = Eventchn.write_port eventchn.fd port
