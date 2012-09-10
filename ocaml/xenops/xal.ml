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
(* Xen/Xenstored Abstraction Layer *)

module D = Debug.Debugger(struct let name = "xenops" end)
open D

open Stringext
open Printf
open Pervasiveext

type domid = int

exception Domain_not_dead of domid
exception Device_not_monitored
exception Timeout

type console_type =
	| Text
	| VNC

type dev_event =
	(* devices : backend / type / devid *)
	| DevEject of string
	(* device thread start : type / devid / pid *)
	| DevThread of string * int
	(* blkback and blktap now provide an explicit flush signal (type / devid) *)
	| DevShutdownDone of string * string
	(* uuid, data *)
	| ChangeRtc of string * string
	(* uuid, name, priority, data *)
	| Message of string * string * int64 * string
	| HotplugChanged of string * string option * string option
	| ChangeUncooperative of bool
	| PciChanged of string
	| Console of console_type * int

type xs_dev_state =
	| Connecting
	| Connected
	| Closing
	| Closed

type internal_dev_event =
	| Backend of xs_dev_state
	| BackThread of int (* pid *)
	| BackEject
	| BackShutdown
	| Frontend of xs_dev_state
	| Rtc of string * string (* uuid, data *)
	| IntMessage of string * string * int64 * string (* uuid, name, priority, body *)
	| HotplugBackend of string option
	| Uncooperative of bool
	| PciBackend of xs_dev_state * string
	| ConsolePort of console_type * int

let string_of_dev_state = function
	| Connecting -> "Connecting"
	| Connected  -> "Connected"
	| Closing    -> "Closing"
	| Closed     -> "Closed"

let string_of_dev_event ev =
	let string_of_string_opt = function None -> "\"\"" | Some s -> s in
	match ev with
	| DevEject i ->
		sprintf "device eject {%s}" i
	| DevThread (i, pid) ->
		sprintf "device thread {%s} pid=%d" i pid
	| DevShutdownDone (s, i) ->
		sprintf "device shutdown {%s,%s}" s i
	| ChangeRtc (uuid, data) ->
		sprintf "change rtc {%s,%s}" uuid data
	| Message (uuid, name, priority, body) ->
	        sprintf "message {%s,%Ld,%s}" name priority body
	| HotplugChanged (i, old, n) ->
		sprintf "HotplugChanged on %s {%s->%s}" i
		        (string_of_string_opt old)
		        (string_of_string_opt n)
	| ChangeUncooperative b ->
		sprintf "ChangeUncooperative %b" b
	| PciChanged s ->
		sprintf "PciChanged %s" s
	| Console (VNC, port) ->
		sprintf "Console(VNC, %d)" port
	| Console (Text, port) ->
		sprintf "Console(Text, %d)" port

type died_reason =
	| Crashed
	| Vanished
	| Halted          (* poweroff and halt *)
	| Rebooted
	| Suspended
	| Shutdown of int (* reason *)

let string_of_died_reason = function
	| Crashed    -> "crashed"
	| Vanished   -> "vanished"
	| Halted     -> "halted"
	| Rebooted   -> "rebooted"
	| Suspended  -> "suspended"
	| Shutdown i -> sprintf "shutdown[%d]" i

type devstate = {
	mutable hotplug: string option;
	mutable error: string option;
	mutable backstate: xs_dev_state;
	mutable frontstate: xs_dev_state;
}

type domstate = {
	mutable uuid: string;
	mutable data_next_window: int64;
	mutable dead_reason: died_reason option;
	mutable devices: ((string * string), devstate) Hashtbl.t;
	(* need a callback here *)
}

type state =
	| Running
	| Dead

type ctx = {
	xs: Xs.xsh;
	xc: Xc.handle;
	mutable callback_introduce: ctx -> domid -> string -> unit;
	mutable callback_release: ctx -> domid -> string -> unit;
	mutable callback_devices: ctx -> domid -> dev_event -> unit;
	mutable callback_guest_agent: ctx -> domid -> unit;
	mutable callback_memory_target: ctx -> domid -> unit;
	monitor_devices: bool;
	mutable currents: (domid * state) list;
	tbl: (domid, domstate) Hashtbl.t;
}

(* Internal debug function *)
let dump_tbl tbl = 
	Hashtbl.iter (fun domid domstate -> debug "%d -> { %s %s }" domid domstate.uuid (Opt.default "unknown" (Opt.map string_of_died_reason domstate.dead_reason))) tbl

let xc_of_ctx ctx = ctx.xc
let xs_of_ctx ctx = ctx.xs

let domstate_init uuid = {
	dead_reason = None;
	devices = Hashtbl.create 10;
	uuid = uuid;
	data_next_window = 0L;
}

(* some helper to find out if a domain is running or not reading xc dominfo *)
let is_running dom =
	not (dom.Xc.dying || dom.Xc.shutdown) &&
	((dom.Xc.paused || dom.Xc.running || dom.Xc.blocked) ||
	(* for hvm domain *)
	((dom.Xc.paused || dom.Xc.running || dom.Xc.blocked) = false))

let is_not_running dom =
	dom.Xc.dying || dom.Xc.shutdown

let dead_reason_of_xc dom =
	if dom.Xc.shutdown then
		match dom.Xc.shutdown_code with
		| 0 -> Halted
		| 1 -> Rebooted
		| 2 -> Suspended
		| 3 -> Crashed
		| 4 -> Halted
		| i -> Shutdown i
	else if dom.Xc.dying then
		Crashed
	else
		raise (Domain_not_dead dom.Xc.domid)


let xal_devstate_of_string s =
	let state = try Xenbus.of_int (int_of_string s)
		    with _ -> Xenbus.Unknown in
	match state with
	| Xenbus.Unknown      -> Closed
	| Xenbus.Initialising -> Connecting
	| Xenbus.InitWait     -> Connecting
	| Xenbus.Initialised  -> Connecting
	| Xenbus.Connected    -> Connected
	| Xenbus.Closing      -> Closing
	| Xenbus.Closed       -> Closed

let get_domstate ctx domid =
	try Hashtbl.find ctx.tbl domid
	with Not_found ->
		(* we don't have uuid, so fill it with a fake *)
		let dom = domstate_init "X" in
		Hashtbl.add ctx.tbl domid dom;
		dom

let get_devstate ctx domid ty devid =
	let key = (ty, devid) in
	let domstate = get_domstate ctx domid in
	if Hashtbl.mem domstate.devices key then
		Hashtbl.find domstate.devices key
	else (
		let devstate = {
			hotplug = None;
			error = None;
			frontstate = Closed;
			backstate = Closed;
		} in
		Hashtbl.add domstate.devices key devstate;
		devstate
	)

(* get the list of all domains associating them with domid for easy finding *)
let get_all_doms xc =
	List.map (fun dom ->
		let state = if is_running dom then Running else Dead in
		dom.Xc.domid, state, dom) (Xc.domain_getinfolist xc 0)

let get_doms doms =
	List.map (fun (domid, state, _) -> (domid, state)) doms

(* helpers for testing domain state *)
let domain_is_dead ctx domid =
	if Hashtbl.mem ctx.tbl domid then
		let dom = Hashtbl.find ctx.tbl domid in
		match dom.dead_reason with None -> false | Some _ -> true
	else (
		try
			let dom = Xc.domain_getinfo ctx.xc domid in
			not (is_running dom)
		with Xc.Error _ ->
			true
	)

let domain_get_dead ctx domid =
	if Hashtbl.mem ctx.tbl domid then
		let dom = Hashtbl.find ctx.tbl domid in
		match dom.dead_reason with
		| None        -> raise (Domain_not_dead domid)
		| Some reason -> reason
	else (
		try
			let dom = Xc.domain_getinfo ctx.xc domid in
			dead_reason_of_xc dom
		with Xc.Error err ->
			debug "Xc.domain_getinfo %d threw exception: %s; assuming domain has Vanished" domid err;
			Vanished
	)

(** return the latest domid register for a specific uuid *)
let domain_get_domid ctx uuid =
	let found_domid = ref None in
	Hashtbl.iter (fun domid dom ->
		if dom.uuid = uuid then
			match !found_domid with
			| None    -> found_domid := Some domid
			| Some id -> if id > domid then found_domid := Some id
		) ctx.tbl;
	!found_domid

(** return the uuid given a domid *)
let uuid_of_domid ctx domid =
  let state = Hashtbl.find ctx.tbl domid in
    state.uuid

(* hopefully in xen 4, we'll have signals about paused domains.
   in xen 3, make sure you test domain_is_dead after domain_is_paused *)
let domain_is_paused ctx domid =
	if Hashtbl.mem ctx.tbl domid then (
		let domstate = Hashtbl.find ctx.tbl domid in
		if domstate.dead_reason = None then
			try
				let dom = Xc.domain_getinfo ctx.xc domid in
				if is_running dom then
					dom.Xc.paused
				else (
					domstate.dead_reason <- Some (dead_reason_of_xc dom);
					false
				)
			with
			Xc.Error _ ->
				domstate.dead_reason <- Some Vanished;
				false
		else
			false
	) else
		false

let domains_running ctx =
	List.map (fun (domid, state) -> domid)
	         (List.filter (fun (domid, state) -> state = Running) ctx.currents)

let device_is_connected ctx domid ty devid =
	if not ctx.monitor_devices then
		raise Device_not_monitored;
	let devstate = get_devstate ctx domid ty devid in
	devstate.frontstate = Connected && devstate.backstate = Connected

let device_get_hotplug ctx domid ty devid =
	if not ctx.monitor_devices then
		raise Device_not_monitored;
	let devstate = get_devstate ctx domid ty devid in
	devstate.hotplug

(* do a diff against the list of running domain we know and
   create events from that. *)
let domain_update ctx =
	let doms = get_all_doms ctx.xc in
	let olddoms = ctx.currents in
	let news = ref [] and deads = ref [] in

	let paths_to_watch domid =
		[ sprintf "/xapi/%d" domid;
		  sprintf "/local/domain/%d/data/updated" domid; (* guest agent *)
		  sprintf "/local/domain/%d/messages" domid; (* messages *)
		  sprintf "/local/domain/%d/memory/target" domid;
		  sprintf "/local/domain/%d/memory/uncooperative" domid;
		  sprintf "/local/domain/%d/console" domid;
		  sprintf "/local/domain/%d/console/vnc-port" domid;
		] @
		if ctx.monitor_devices then [
			sprintf "/local/domain/%d/device" domid;
			sprintf "/local/domain/%d/error/device" domid;
		] else [] in

	let add_domain_watch uuid domid =
		List.iter (fun p -> ctx.xs.Xs.watch p ("xal-" ^ string_of_int domid))
		          (paths_to_watch domid)
	and del_domain_watch domid =
		List.iter (fun p -> try ctx.xs.Xs.unwatch p ("xal-" ^ string_of_int domid) with _ -> ())
		          (paths_to_watch domid)
		in

	let domain_create domid uuid =
		let uuid = Uuid.to_string (Uuid.uuid_of_int_array uuid) in
		(* NB the domstate might already be in the table because of a previous device event.
		   In this case it won't have a valid uuid: we must update it. *)
		let ds =
			if Hashtbl.mem ctx.tbl domid
			then Hashtbl.find ctx.tbl domid
			else domstate_init uuid in
		Hashtbl.replace ctx.tbl domid { ds with uuid = uuid };
		add_domain_watch uuid domid;
		news := (domid, uuid) :: !news
		in
	let domain_set_dead domid reason =
		let domev = get_domstate ctx domid in
		domev.dead_reason <- Some reason;
		if reason <> Suspended then begin
			del_domain_watch domid;
			deads := (domid, domev.uuid) :: !deads;
		end in
	let domain_resume domid =
		let domev = get_domstate ctx domid in
		domev.dead_reason <- None
		in
		
	(* search for resumed domains *)
	List.iter (fun (domid, state, dom) ->
				if state = Running && List.mem_assoc domid olddoms && List.assoc domid olddoms = Dead
				then domain_resume domid)
		doms;

	(* search for new domains first *)
	List.iter (fun (domid, state, dom) ->
		(* if domid is not found in old list, create the domain *)
		if state = Running && not (List.mem_assoc domid olddoms) then
			domain_create domid dom.Xc.handle
	) doms;

	(* search for domains that disappeared from the list *)
	List.iter (fun (domid, state) ->
		if state = Running then
			try ignore (List.find (fun (ndomid,_,_) -> ndomid = domid) doms)
			with Not_found -> domain_set_dead domid Vanished
	) olddoms;

	(* search for dying|shutdown domains *)
	List.iter (fun (domid, state, dom) ->
		let already_died domid =
			try List.assoc domid olddoms = Dead with Not_found -> false in
		if is_not_running dom && (not (already_died domid)) then (
			domain_set_dead domid (dead_reason_of_xc dom)
		)
	) doms;

	(* We delete the records for domains which have vanished, we assume that someone has already
	   seen them 'shutdown' and taken appropriate action. *)
	let domids_and_states = get_doms doms in
	(* any domains which aren't in the 'doms' list will be deleted from the tbl *)
	let to_be_deleted = Hashtbl.fold (fun domid _ acc -> if not(List.mem_assoc domid domids_and_states) then domid::acc else acc) ctx.tbl [] in
	List.iter (fun domid ->
		     try Hashtbl.remove ctx.tbl domid with _ -> ()) to_be_deleted;

	ctx.currents <- domids_and_states;
	!news, !deads

(** Decode the watch path into an device description.
 * The valid watch event have the following format :
 *    1      2       3       4       5       6       7      8
 * /local/domain /0       /backend/<type> /<domid>/<devid>/state
 * /local/domain /0       /backend/vbd    /<domid>/<devid>/kthread-pid
 * /local/domain /0       /backend/tap    /<domid>/<devid>/tapdisk-pid
 * /local/domain /0       /backend/<type> /<domid>/<devid>/shutdown-done
 * /local/domain /0       /backend/<type> /<domid>/<devid>/type
 * /local/domain /<domid> /device /<type> /<devid>/state
 * /local/domain /<domid> /error  /device /<type> /<devid>/error
 * /xapi /<domid>/hotplug /<type> /<devid>/
 * /xapi /<domid>/frontend/<type> /<devid>/
 * /vm   /<uuid> /rtc     / timeoffset
 * /local/domain /<domid> /messages/<uid> /name   /prio   /body
 * /local/domain /<domid> /console/
 *)
let other_watch xs w v =
	let read_state path =
		try
			let s = xs.Xs.read path in
			xal_devstate_of_string s
		with _ -> Closed
		in
	let lpath = String.split '/' w in
	match lpath with
	| "" :: "local" :: "domain" :: "0" :: "backend" :: ty :: domid :: devid :: [ "state" ] ->
		let xsds = read_state w in
		Some (int_of_string domid, Backend xsds, ty, devid)
	| "" :: "local" :: "domain" :: "0" :: "backend" :: "vbd" :: domid :: devid :: [ "kthread-pid" ] ->
		begin try
			let kthread_pid = int_of_string (xs.Xs.read w) in
			Some (int_of_string domid, BackThread kthread_pid, "", devid)
		with _ ->
			None
		end
	| "" :: "local" :: "domain" :: "0" :: "backend" :: "tap" :: domid :: devid :: [ "tapdisk-pid" ] ->
		begin try
			let tapdisk_pid = int_of_string (xs.Xs.read w) in
			Some (int_of_string domid, BackThread tapdisk_pid, "", devid)
		with _ ->
			None
		end
	| "" :: "local" :: "domain" :: "0" :: "backend" :: ty :: domid :: devid :: [ "shutdown-done" ] ->
		Some (int_of_string domid, BackShutdown, ty, devid)
	| "" :: "local" :: "domain" :: "0" :: "backend" :: ( "vbd" | "tap" ) :: domid :: devid :: [ "params" ] ->
		begin try
			if xs.Xs.read w = "" then
				Some (int_of_string domid, BackEject, "", devid)
			else
				None
		with _ ->
			None
		end
	| "" :: "local" :: "domain" :: "0" :: "backend" :: "pci" :: domid :: [] ->
		debug "pci devices for domain %s have disappeared" domid;
		Some (int_of_string domid, PciBackend (Closed, ""), "", "")
	| "" :: "local" :: "domain" :: "0" :: "backend" :: "pci" :: domid :: "0" :: [dev] ->
		let pciid = xs.Xs.read w in
		let devid = String.sub_to_end dev 4 in
		debug "pci device %s/%s passed through to domain %s" devid pciid domid;
		Some (int_of_string domid, PciBackend (Connected, devid), "", "")
	| "" :: "local" :: "domain" :: domid :: "device" :: ty :: devid :: [ "state" ] ->
		let xsds = read_state w in
		Some (int_of_string domid, Frontend xsds, ty, devid)
	| "" :: "local" :: "domain" :: domid :: "console" :: [ "vnc-port" ] ->
		let port = int_of_string (xs.Xs.read w) in
		Some (int_of_string domid, ConsolePort(VNC, port), "", "")
	| "" :: "local" :: "domain" :: domid :: "console" :: [ "tc-port" ] ->
		let port = int_of_string (xs.Xs.read w) in
		Some (int_of_string domid, ConsolePort(Text, port), "", "")
	| "" :: "xapi" :: domid :: "hotplug" :: "vif" :: devid :: [ "hotplug" ] ->
		let extra = try Some (xs.Xs.read w) with _ -> None in
		Some (int_of_string domid, (HotplugBackend extra), "", devid)
	| "" :: "vm" :: uuid :: "rtc" :: [ "timeoffset" ] ->
		let data = xs.Xs.read w in
		Some (-1, (Rtc (uuid, data)), "", "")
	| "" :: "local" :: "domain" :: domid :: "memory" :: [ "uncooperative" ] ->
		let uncooperative = try ignore (xs.Xs.read w); true with Xb.Noent -> false in
		Some (int_of_string domid, Uncooperative uncooperative, "", "")
(* disabled for CA-22306
	| "" :: "local" :: "domain" :: domid :: "messages" :: id :: name :: priority :: [ "body" ] ->
	    begin
	      try
		debug "Found message! (name=%s)" name;
		let body = xs.Xs.read w in
		xs.Xs.rm (Printf.sprintf "/local/domain/%s/messages/%s" domid id);
		let vm = xs.Xs.read (Printf.sprintf "/local/domain/%s/vm" domid) in
		debug "vm=%s" vm;
		match String.split '/' vm with 
		  | "" :: "vm" :: uuid :: _ ->
		      Some (-1, IntMessage (uuid, name, (Int64.of_string priority), body), "", "") 
		  | _ -> None
	      with _ -> None
	    end
*)
	| _ -> None

let device_state_init ctx domid =
	let xs = ctx.xs in
	let frontend_path = sprintf "/local/domain/%u/device" domid in
	let frontend_tys = try xs.Xs.directory frontend_path with Xb.Noent -> [] in
	let frontend_tys = List.filter (fun ty -> ty <> "") frontend_tys in

	let iter_device ty devid =
		let path = sprintf "%s/%s/%s" frontend_path ty devid in
		let s = xs.Xs.read (path ^ "/" ^ "state") in
		let devstate = get_devstate ctx domid ty devid in

		(* read frontstate *)
		let frontstate = xal_devstate_of_string s in
		devstate.frontstate <- frontstate;

		(* try to read backstate *)
		let backpath = xs.Xs.read (path ^ "/" ^ "backend") in
		let backstate = xal_devstate_of_string
			    (xs.Xs.read (backpath ^ "/" ^ "state")) in
		devstate.backstate <- backstate;

		(* read error if any *)
		let epath = sprintf "/local/domain/%u/error/device/%s/%s/error"
				    domid ty devid in
		let error = try Some (xs.Xs.read epath) with _ -> None in
		devstate.error <- error

		in

	List.iter (fun ty ->
		let devids = xs.Xs.directory (frontend_path ^ "/" ^ ty) in
		List.iter (fun devid ->
			try iter_device ty devid with exn -> ()) devids
		) frontend_tys;
	()

let callback_dom_null ctx domid = ()
let callback_dom_uuid_null ctx domid uuid = ()
let callback_dev_null ctx domid dev_event = ()

let init ?(callback_introduce = callback_dom_uuid_null)
         ?(callback_release = callback_dom_uuid_null)
         ?(callback_devices = callback_dev_null)
	 ?(callback_guest_agent = callback_dom_null)
	 ?(callback_memory_target = callback_dom_null)
         ?(monitor_devices = false) () =
	(* Make sure if this function fails to close any opened descriptors *)
	let xs = Xs.daemon_open () in	
	try
		let xc = Xc.interface_open () in
		(try
			let ctx = {
				xs = xs;
				xc = xc;
				callback_introduce = callback_introduce;
				callback_release = callback_release;
				callback_devices = callback_devices;
				callback_guest_agent = callback_guest_agent;
				callback_memory_target = callback_memory_target;
				monitor_devices = monitor_devices;
				currents = [];
				tbl = Hashtbl.create 20;
			} in

			ctx.xs.Xs.watch "@introduceDomain" "xallist";
			ctx.xs.Xs.watch "@releaseDomain" "xallist";

			if ctx.monitor_devices then (
				ctx.xs.Xs.watch "/local/domain/0/backend" "xaldevs";
				ctx.xs.Xs.watch "/vm" "xaldevs"
			);

			let newdomains, deaddomains = domain_update ctx in
			if ctx.monitor_devices then
				List.iter (fun (domid, _) -> device_state_init ctx domid) newdomains;
			List.iter (fun (domid, uuid) -> callback_introduce ctx domid uuid) newdomains;
			List.iter (fun (domid, uuid) -> callback_release ctx domid uuid) deaddomains;
			ctx
		with e -> Xc.interface_close xc; raise e)
	with e -> Xs.close xs; raise e

let close ctx =
	(try Xs.close ctx.xs 
	 with e -> error "Caught exception: %s while closing xenstore connection" (Printexc.to_string e));
  	(try Xc.interface_close ctx.xc 
	 with e -> error "Caught exception: %s while closing xc" (Printexc.to_string e))

let domain_device_event ctx w v =
	match other_watch ctx.xs w v with
	| None                        -> ()
	| Some (_, Rtc (uuid, value), _, _) ->
		ctx.callback_devices ctx (-1) (ChangeRtc (uuid, value))
	| Some (domid, Uncooperative x, _, _) ->
		ctx.callback_devices ctx domid (ChangeUncooperative x)
	| Some (_, IntMessage (uuid, name, priority, body), _, _) ->
	        ctx.callback_devices ctx (-1) (Message (uuid, name, priority, body))
	| Some (domid, BackThread (pid), _, devid) ->
		ctx.callback_devices ctx domid (DevThread (devid, pid))
	| Some (domid, BackEject, _, devid) ->
		ctx.callback_devices ctx domid (DevEject (devid))
	| Some (domid, ev, ty, devid) -> (
		let devstate = get_devstate ctx domid ty devid in

		match ev with
		| Rtc _ | IntMessage _ | BackThread _ | BackEject | Uncooperative _ -> assert false
		| Backend state  ->
			devstate.backstate <- state;
		| Frontend state ->
			devstate.frontstate <- state;
		| BackShutdown ->
			ctx.callback_devices ctx domid (DevShutdownDone (ty, devid))
		| HotplugBackend extra  ->
			let old = devstate.hotplug in
			devstate.hotplug <- extra;
			ctx.callback_devices ctx domid
			               (HotplugChanged (devid, old, extra))
		| PciBackend (state, devid) ->
			ctx.callback_devices ctx domid (PciChanged devid)
		| ConsolePort (ty, port) ->
			ctx.callback_devices ctx domid (Console(ty, port))
	)

(** Internal helper function which wraps the Xs.read_watchevent with
    an optional timeout (via select) *)
let _read_watchevent ?timeout xs = match timeout with
	| None -> Xs.read_watchevent xs
	| Some timeout ->
		if timeout < 0. then
			raise Timeout;
		let r,_,_ = Unix.select [ Xs.get_fd xs ] [] [] timeout in
		if r = [] then
			raise Timeout
		else
			Xs.read_watchevent xs

let process ?timeout ctx =
	let w,v =
		if Xs.has_watchevents ctx.xs then
			Xs.get_watchevent ctx.xs
		else
			_read_watchevent ?timeout ctx.xs in
	match w with
	| "@introduceDomain" | "@releaseDomain" ->
		let newdomains, deaddomains = domain_update ctx in
		List.iter (fun (domid, uuid) -> ctx.callback_introduce ctx domid uuid) newdomains;
		List.iter (fun (domid, uuid) -> ctx.callback_release ctx domid uuid) deaddomains;
	| x when String.endswith "/data/updated" x ->
		(* guest agent has updated state in xenstore *)
		let domid = int_of_string (String.sub v 4 (String.length v - 4)) in
		ctx.callback_guest_agent ctx domid
	| x when String.endswith "/memory/target" x ->
		(* someone has given the guest a new balloon target *)
		let domid = int_of_string (String.sub v 4 (String.length v - 4)) in
		ctx.callback_memory_target ctx domid
	| _ ->
		(* we should not come here if we don't monitor devices, just be careful *)
		if ctx.monitor_devices then
			domain_device_event ctx w v

let wait ctx timeout =
	try
		let timeout = Some timeout and finished = ref false in

		let start = Unix.gettimeofday () in
		let timeout' = ref timeout in
		while not (!finished)
		do
			process ?timeout:(!timeout') ctx;
			let spent = Unix.gettimeofday () -. start in
			timeout' := may (fun x -> x -. spent) timeout;
		done;
	with
	| Timeout -> ()

let wait_release ctx ?timeout domid =
	if domain_is_dead ctx domid then
		domain_get_dead ctx domid
	else (
		let finished = ref false in
		let cb ctx domid2 _ = (finished := domid = domid2) in
		let old_callback = ctx.callback_release in
		ctx.callback_release <- cb;

		let start = Unix.gettimeofday () in
		let timeout' = ref timeout in
		try
			while not (!finished)
			do
				process ?timeout:(!timeout') ctx;
				let spent = Unix.gettimeofday () -. start in
				timeout' := may (fun x -> x -. spent) timeout;
			done;
			ctx.callback_release <- old_callback;
			domain_get_dead ctx domid
		with Timeout ->
			(* last resort, ask xen directly just in case we didn't saw the @releaseDomain *)
			ctx.callback_release <- old_callback;
			let newdomains, deaddomains = domain_update ctx in
			List.iter (fun (domid, uuid) -> ctx.callback_introduce ctx domid uuid) newdomains;
			List.iter (fun (domid, uuid) -> ctx.callback_release ctx domid uuid) deaddomains;
			if deaddomains <> [] && domain_is_dead ctx domid then (
				(* we missed the event but it's dead anyway so we recover from timeout *)
				domain_get_dead ctx domid
			) else
				raise Timeout
	)

let loop ?callback_introduce ?callback_release ?callback_devices ?callback_guest_agent ?callback_memory_target () =
	let ctx = init ?callback_introduce ?callback_release
	               ?callback_devices ?callback_guest_agent ?callback_memory_target ~monitor_devices:true () in

	try
		while true
		do
			process ctx
		done
	with e ->
		close ctx;
		raise e
