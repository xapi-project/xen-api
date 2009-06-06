(*
 * Copyright (c) 2006 XenSource Inc.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 * All rights reserved.
 *)

open Printf

let debug fmt = Logs.debug "general" fmt

type t =
{
	id: Xc.domid;
	mfn: nativeint;
	remote_port: int;
	interface: Mmap.mmap_interface;
	eventchn: Event.t;
	mutable port: int;
}

let get_path dom = "/local/domain/" ^ (sprintf "%u" dom.id)
let get_id domain = domain.id
let get_interface d = d.interface
let get_mfn d = d.mfn
let get_remote_port d = d.remote_port

let dump d chan =
	fprintf chan "dom,%d,%nd,%d\n" d.id d.mfn d.port

let notify dom = Event.notify dom.eventchn dom.port; ()

let bind_interdomain dom =
	dom.port <- Event.bind_interdomain dom.eventchn dom.id dom.remote_port;
	debug "domain %d bound port %d" dom.id dom.port


let close dom =
	debug "domain %d unbound port %d" dom.id dom.port;
	Event.unbind dom.eventchn dom.port;
	Mmap.unmap dom.interface;
	()

let make id mfn remote_port interface eventchn = {
	id = id;
	mfn = mfn;
	remote_port = remote_port;
	interface = interface;
	eventchn = eventchn;
	port = -1
}

let is_dom0 d = d.id = 0
