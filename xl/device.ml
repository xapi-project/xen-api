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

open Printf

open Xenops_utils

open Device_common
open Xenstore
open Cancel_utils
open Xenops_task

exception Device_shutdown
exception Device_not_found

module D = Debug.Make(struct let name = "xenops" end)
open D

(****************************************************************************************)

module Generic = struct

let vnc_port_path domid = sprintf "/local/domain/%d/console/vnc-port" domid

let tc_port_path domid = sprintf "/local/domain/%d/console/tc-port" domid

(* Oxenstored's transaction conflict algorithm will cause parallel but separate device
   creation transactions to abort and retry, leading to livelock while starting lots of
   VMs. Work around this by serialising these transactions for now. *)
let device_serialise_m = Mutex.create ()
let add_device ~xs device backend_list frontend_list private_list =
	Mutex.execute device_serialise_m (fun () ->

	let frontend_path = frontend_path_of_device ~xs device
	and backend_path = backend_path_of_device ~xs device
	and hotplug_path = Hotplug.get_hotplug_path device
	and private_data_path = Hotplug.get_private_data_path_of_device device in

	debug "adding device  B%d[%s]  F%d[%s]  H[%s]" device.backend.domid backend_path device.frontend.domid frontend_path hotplug_path;
	Xs.transaction xs (fun t ->
		begin try
			ignore (t.Xst.read frontend_path);
			raise (Device_frontend_already_connected device)
		with Xs_protocol.Enoent _ -> () end;

		t.Xst.rm frontend_path;
		t.Xst.rm backend_path;
		(* CA-16259: don't clear the 'hotplug_path' because this is where we
		   record our own use of /dev/loop devices. Clearing this causes us to leak
		   one per PV .iso *)

		t.Xst.mkdir frontend_path;
		t.Xst.setperms frontend_path (Xenbus_utils.device_frontend device);

		t.Xst.mkdir backend_path;
		t.Xst.setperms backend_path (Xenbus_utils.device_backend device);

		t.Xst.mkdir hotplug_path;
		t.Xst.setperms hotplug_path (Xenbus_utils.hotplug device);

		t.Xst.writev frontend_path
		             (("backend", backend_path) :: frontend_list);
		t.Xst.writev backend_path
		             (("frontend", frontend_path) :: backend_list);

		t.Xst.mkdir private_data_path;
		t.Xst.setperms private_data_path (Xenbus_utils.hotplug device);
		t.Xst.writev private_data_path private_list;
	)
	)

let safe_rm ~xs path =
  try 
    debug "xenstore-rm %s" path;
    xs.Xs.rm path
  with _ -> debug "Failed to xenstore-rm %s; continuing" path 

(* Helper function to delete the frontend, backend and error trees for a device.
   This must only be done after synchronising with the hotplug scripts.
   Cleaning up is best-effort; some of it might fail but as much will be
   done as possible. *)
let rm_device_state ~xs (x: device) =
	debug "Device.rm_device_state %s" (string_of_device x);
	safe_rm ~xs (frontend_path_of_device ~xs x);
	safe_rm ~xs (backend_path_of_device ~xs x);
	(* Cleanup the directory containing the error node *)
	safe_rm ~xs (backend_error_path_of_device ~xs x);
	safe_rm ~xs (Filename.dirname (error_path_of_device ~xs x))

let can_surprise_remove ~xs (x: device) =
  (* "(info key in xenstore) && 2" tells us whether a vbd can be surprised removed *)
        let key = backend_path_of_device ~xs x ^ "/info" in
	try
	  let info = Int64.of_string (xs.Xs.read key) in
	  (Int64.logand info 2L) <> 0L
	with _ -> false

(** Checks whether the supplied device still exists (ie hasn't been deleted) *)
let exists ~xs (x: device) = 
  let backend_stub = backend_path_of_device ~xs x in
  try
    ignore_string(xs.Xs.read backend_stub);
    true
  with Xs_protocol.Enoent _ -> false

let assert_exists_t ~xs t (x: device) =
  let backend_stub = backend_path_of_device ~xs x in
  try
    ignore_string(t.Xst.read backend_stub)
  with Xs_protocol.Enoent _ -> raise Device_not_found

(** When hot-unplugging a device we ask nicely *)
let clean_shutdown_async ~xs (x: device) =
	let backend_path = backend_path_of_device ~xs x in
	let state_path = backend_path ^ "/state" in
	Xs.transaction xs (fun t ->
		let online_path = backend_path ^ "/online" in
		debug "xenstore-write %s = 0" online_path;
		t.Xst.write online_path "0";
		let state = try Xenbus_utils.of_string (t.Xst.read state_path) with _ -> Xenbus_utils.Closed in
		if state <> Xenbus_utils.Closed then (
			debug "Device.del_device setting backend to Closing";
			t.Xst.write state_path (Xenbus_utils.string_of Xenbus_utils.Closing);
		)
	)

let unplug_watch ~xs (x: device) = Hotplug.path_written_by_hotplug_scripts x |> Watch.key_to_disappear
let error_watch ~xs (x: device) = Watch.value_to_appear (error_path_of_device ~xs x)
let frontend_closed ~xs (x: device) = Watch.map (fun () -> "") (Watch.value_to_become (frontend_path_of_device ~xs x ^ "/state") (Xenbus_utils.string_of Xenbus_utils.Closed))
let backend_closed ~xs (x: device) = Watch.map (fun () -> "") (Watch.value_to_become (backend_path_of_device ~xs x ^ "/state") (Xenbus_utils.string_of Xenbus_utils.Closed))

let clean_shutdown_wait (task: Xenops_task.t) ~xs ~ignore_transients (x: device) =
	debug "Device.Generic.clean_shutdown_wait %s" (string_of_device x);

	let on_error () =
		let error_path = error_path_of_device ~xs x in
		let error = try xs.Xs.read error_path with _ -> "" in
	    debug "Device.Generic.shutdown_common: read an error: %s" error;
	    (* After CA-14804 we deleted the error node *)
		(* After CA-73099 we stopped doing that *)
		(* ... but in the case of a "managed" domain,
		   this transient should be ignored anyway *)
	    raise (Device_error (x, error)) in

	let cancel = Device x in
	let frontend_closed = Watch.map (fun _ -> ()) (frontend_closed ~xs x) in
	let backend_closed = Watch.map (fun _ -> ()) (backend_closed ~xs x) in
	let error = Watch.map (fun _ -> ()) (error_watch ~xs x) in
	debug "waiting for backend to close";
	if not(cancellable_watch cancel [ backend_closed ] (if ignore_transients then [] else [ error ]) task ~xs ~timeout:!Xenopsd.hotplug_timeout ())
	then on_error ();
	debug "waiting for frontend to close";
	if not(cancellable_watch cancel [ frontend_closed ] (if ignore_transients then [] else [ error ]) task ~xs ~timeout:!Xenopsd.hotplug_timeout ())
	then on_error ();
	safe_rm ~xs (frontend_path_of_device ~xs x);
	rm_device_state ~xs x

let clean_shutdown (task: Xenops_task.t) ~xs (x: device) =
	debug "Device.Generic.clean_shutdown %s" (string_of_device x);
	clean_shutdown_async ~xs x;
	clean_shutdown_wait task ~xs ~ignore_transients:false x

let hard_shutdown_request ~xs (x: device) =
	debug "Device.Generic.hard_shutdown_request %s" (string_of_device x);

	let backend_path = backend_path_of_device ~xs x in
	let online_path = backend_path ^ "/online" in
	debug "xenstore-write %s = 0" online_path;
	xs.Xs.write online_path "0";

	debug "Device.Generic.hard_shutdown about to blow away frontend";
	let frontend_path = frontend_path_of_device ~xs x in
	safe_rm xs frontend_path

let hard_shutdown_complete ~xs (x: device) = unplug_watch ~xs x

let hard_shutdown (task: Xenops_task.t) ~xs (x: device) = 
	hard_shutdown_request ~xs x;

	let (_: bool) = cancellable_watch (Device x) [ hard_shutdown_complete ~xs x ] [ ] task ~xs ~timeout:!Xenopsd.hotplug_timeout () in
	(* blow away the backend and error paths *)
	debug "Device.Generic.hard_shutdown about to blow away backend and error paths";
	rm_device_state ~xs x

(*
(* Assume we've told the backend to close. Watch both the error node and one other path.
   When the watch fires, call a predicate function and look for an error node.
   If an error node appears, throw Device_error. If the predicate returns true then
   return unit. If the timeout expires throw Device_disconnect_timeout. *)
let wait_for_error_or ~xs ?(timeout=Hotplug.hotplug_timeout) doc predicate otherpath domid kind devid = 
	let doc' = Printf.sprintf "%s (timeout = %f; %s)" doc timeout (print_device domid kind devid) in
  	let errorpath = error_node domid kind devid in
	debug "Device.wait_for_error_or %s (watching [ %s; %s ])" doc' otherpath errorpath;

	let finished = ref false and error = ref None in
	let callback watch =
		finished := predicate ();
		error := (try Some (xs.Xs.read errorpath) with Xs_protocol.Enoent _ -> None);
		(* We return if the predicate is true of an error node has appeared *)
		!finished || !error <> None in
	begin try
		Xs.monitor_paths xs [ otherpath, "X";
				      errorpath, "X" ] timeout callback;
	with
		Xs.Timeout ->
			warn "Device.wait_for_error_or %s: timeout" doc';
			raise (Device_disconnect_timeout (domid, kind, devid))
	end;
	begin match !error with
	| Some error ->
		warn "Device.wait_for_error_or %s: failed: %s" doc' error;
		raise (Device_error (domid, kind, devid, error))
	| None ->
		debug "Device.wait_for_error_or %s: succeeded" doc'
	end

(** When destroying a whole domain, we blow away the frontend tree of individual devices.
    NB we only ever blow away the frontend (blowing away the backend risks resource leaks)
    NB we only ever blow away frontends of domUs which are being destroyed - we don't
    expect them to recover from this! *)
let destroy ~xs domid kind devid =
	let frontend_path = get_frontend_path ~xs domid kind devid in
	xs.Xs.rm frontend_path
*)

	  let really_kill pid =
		  try
			  Unixext.kill_and_wait pid
		  with Unixext.Process_still_alive ->
			  debug "%d: failed to respond to SIGTERM, sending SIGKILL" pid;
			  Unixext.kill_and_wait ~signal:Sys.sigkill pid
	  let best_effort txt f =
		  try
			  f ()
		  with e ->
			  info "%s: ignoring exception %s" txt (Printexc.to_string e)

end

(****************************************************************************************)
(** Disks:                                                                              *)

module Vbd = struct

type shutdown_mode =
	| Classic (** no signal that backend has flushed, rely on (eg) SM vdi_deactivate for safety *)
    | ShutdownRequest (** explicit signal that backend has flushed via "shutdown-done" *)

let read_feature_flag ~xs (x: device) flag =
	let feature_flag_path = Printf.sprintf "/local/domain/%d/control/%s" x.backend.domid flag in
	try ignore(xs.Xs.read feature_flag_path); true with _ -> false

let shutdown_mode_of_device ~xs (x: device) =
	if read_feature_flag ~xs x "feature-shutdown-request"
	then ShutdownRequest
	else Classic

type mode = ReadOnly | ReadWrite

let string_of_mode = function
	| ReadOnly -> "r"
	| ReadWrite -> "w"

let mode_of_string = function
	| "r" -> ReadOnly
	| "w" -> ReadWrite
	| s   -> invalid_arg "mode_of_string"

type lock = string

(** The format understood by blocktap *)
let string_of_lock lock mode = lock ^ ":" ^ (string_of_mode mode)

type physty = File | Phys | Qcow | Vhd | Aio

let backendty_of_physty = function
	| File -> "file"
	| Phys -> "phy"
	| Qcow | Vhd | Aio -> "phy"

let string_of_physty = function
	| Qcow -> "qcow"
	| Vhd  -> "vhd"
	| Aio  -> "aio"
	| File -> "file"
	| Phys -> "phys"

let physty_of_string s =
	match s with
	| "qcow" -> Qcow
	| "vhd"  -> Vhd
	| "aio"  -> Aio
	| "phy"  -> Phys
	| "file" -> File
	| _      -> invalid_arg "physty_of_string"

type devty = CDROM | Disk

let string_of_devty = function
	| CDROM -> "cdrom"
	| Disk  -> "disk"

let devty_of_string = function
	| "cdrom" -> CDROM
	| "disk"  -> Disk
	| _       -> invalid_arg "devty_of_string"

let kind_of_physty physty =
	match physty with
	| Qcow -> Tap
	| Vhd  -> Tap
	| Aio  -> Tap
	| Phys -> Vbd
	| File -> Vbd

let add_backend_keys ~xs (x: device) subdir keys =
	let backend_stub = backend_path_of_device ~xs x in
	let backend = backend_stub ^ "/" ^ subdir in
	debug "About to write data %s to path %s" (String.concat ";" (List.map (fun (a,b) -> "("^a^","^b^")") keys)) backend;
	Xs.transaction xs (fun t ->
		ignore(t.Xst.read backend_stub);
		t.Xst.writev backend keys
	)

let remove_backend_keys ~xs (x: device) subdir keys =
	let backend_stub = backend_path_of_device ~xs x in
	let backend = backend_stub ^ "/" ^ subdir in
	Xs.transaction xs (fun t ->
		List.iter (fun key -> t.Xst.rm (backend ^ "/" ^ key)) keys
	)


let uses_blktap ~phystype = List.mem phystype [ Qcow; Vhd; Aio ]

(** Request either a clean or hard shutdown *)
let request_shutdown ~xs (x: device) (force: bool) =
    let request = if force then "force" else "normal" in

    debug "Device.Vbd.request_shutdown %s %s" (string_of_device x) request;

    let backend_path = backend_path_of_device ~xs x in
    let request_path = backend_shutdown_request_path_of_device ~xs x in
    let online_path = backend_path ^ "/online" in

    (* Prevent spurious errors appearing by not writing online=0 if force *)
    if not(force) then begin
        debug "xenstore-write %s = 0" online_path;
        xs.Xs.write online_path "0";
    end;
    debug "xenstore-write %s = %s" request_path request;
    xs.Xs.write request_path request

(** Return the event to wait for when the shutdown has completed *)
let shutdown_done ~xs (x: device): unit Watch.t =
	Watch.value_to_appear (backend_shutdown_done_path_of_device ~xs x) |> Watch.map (fun _ -> ())

let shutdown_request_clean_shutdown_wait (task: Xenops_task.t) ~xs ~ignore_transients (x: device) =
    debug "Device.Vbd.clean_shutdown_wait %s" (string_of_device x);

    (* Allow the domain to reject the request by writing to the error node *)
    let shutdown_done = shutdown_done ~xs x in
    let error = Watch.value_to_appear (error_path_of_device ~xs x) |> Watch.map (fun _ -> ())  in

	if cancellable_watch (Device x) [ shutdown_done ] (if ignore_transients then [] else [ error ]) task ~xs ~timeout:!Xenopsd.hotplug_timeout ()
	then begin
		debug "Device.Vbd.shutdown_common: shutdown-done appeared";
		(* Delete the trees (otherwise attempting to plug the device in again doesn't
           work.) This also clears any stale error nodes. *)
		Generic.rm_device_state ~xs x
	end else begin
		let error_path = error_path_of_device ~xs x in
		let error = try xs.Xs.read error_path with _ -> "" in
		(* CA-14804: Delete the error node contents *)
		(* After CA-73099 we stopped doing that *)
		debug "Device.Vbd.shutdown_common: read an error: %s" error;
		raise (Device_error (x, error))
	end

let shutdown_request_hard_shutdown (task: Xenops_task.t) ~xs (x: device) = 
	debug "Device.Vbd.hard_shutdown %s" (string_of_device x);
	request_shutdown ~xs x true; (* force *)

	(* We don't watch for error nodes *)
	let (_: bool) = cancellable_watch (Device x) [ shutdown_done ~xs x ] [] task ~xs ~timeout:!Xenopsd.hotplug_timeout () in
	Generic.rm_device_state ~xs x;

	debug "Device.Vbd.hard_shutdown complete"

let clean_shutdown_async ~xs x = match shutdown_mode_of_device ~xs x with
	| Classic -> Generic.clean_shutdown_async ~xs x
	| ShutdownRequest -> request_shutdown ~xs x false (* normal *)

let clean_shutdown_wait (task: Xenops_task.t) ~xs ~ignore_transients x = match shutdown_mode_of_device ~xs x with
	| Classic -> Generic.clean_shutdown_wait task ~xs ~ignore_transients x
	| ShutdownRequest -> shutdown_request_clean_shutdown_wait task ~xs ~ignore_transients x

let clean_shutdown (task: Xenops_task.t) ~xs x =
	clean_shutdown_async ~xs x;
	clean_shutdown_wait task ~xs ~ignore_transients:false x

let hard_shutdown (task: Xenops_task.t) ~xs x = match shutdown_mode_of_device ~xs x with
	| Classic -> Generic.hard_shutdown task ~xs x
	| ShutdownRequest -> shutdown_request_hard_shutdown task ~xs x

let hard_shutdown_request ~xs x = match shutdown_mode_of_device ~xs x with
	| Classic -> Generic.hard_shutdown_request ~xs x
	| ShutdownRequest -> request_shutdown ~xs x true

let hard_shutdown_complete ~xs x = match shutdown_mode_of_device ~xs x with
	| Classic -> Generic.hard_shutdown_complete ~xs x
	| ShutdownRequest -> shutdown_done ~xs x

let hard_shutdown_wait (task: Xenops_task.t) ~xs ~timeout x =
	let (_: bool) = cancellable_watch (Device x) [ Watch.map (fun _ -> ()) (hard_shutdown_complete ~xs x) ] [] task ~xs ~timeout () in
	()

let free_device ~xs bus_type domid =
	let disks = List.map
		(fun x -> x.frontend.devid
		|> Device_number.of_xenstore_key
		|> Device_number.spec
		|> (fun (_, disk, _) -> disk))
		(Device_common.list_frontends ~xs domid) in
	let next = List.fold_left max 0 disks + 1 in
	bus_type, next, 0

type t = {
	mode:mode;
	device_number: Device_number.t option;
	phystype: physty;
	params: string;
	dev_type: devty;
	unpluggable: bool;
	protocol: protocol option;
	extra_backend_keys: (string * string) list;
	extra_private_keys: (string * string) list;
	backend_domid: int;
}

let add_async ~xs ~hvm ?(backend_kind=Vbd) x domid =
	let back_tbl = Hashtbl.create 16 and front_tbl = Hashtbl.create 16 in
	let open Device_number in
	(* If no device number is provided then autodetect a free one *)
	let device_number = match x.device_number with
		| Some x -> x
		| None ->
			make (free_device ~xs (if hvm then Ide else Xen) domid) in
	let devid = to_xenstore_key device_number in
	let device = 
	  let backend = { domid = x.backend_domid; kind = backend_kind; devid = devid } 
	  in  device_of_backend backend domid
	in

	debug "Device.Vbd.add (device_number=%s | params=%s | phystype=%s)"
	  (to_debug_string device_number) x.params (string_of_physty x.phystype);
	(* Notes:
	   1. qemu accesses devices images itself and so needs the path of the original
              file (in params)
           2. when windows PV drivers initialise, the new blockfront connects to the
              up-til-now idle blockback.
           3. when the VM is fully PV, Ioemu devices do not work; all devices must be PV
	   4. in the future an HVM guest might support a mixture of both
	*)

	List.iter (fun (k, v) -> Hashtbl.add back_tbl k v) x.extra_backend_keys;

	List.iter (fun (k, v) -> Hashtbl.replace front_tbl k v) [
		"backend-id", string_of_int x.backend_domid;
		"state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
		"virtual-device", string_of_int devid;
		"device-type", if x.dev_type = CDROM then "cdrom" else "disk";
	];
	List.iter (fun (k, v) -> Hashtbl.replace back_tbl k v) [
		"frontend-id", sprintf "%u" domid;
		(* Prevents the backend hotplug scripts from running if the frontend disconnects.
		   This allows the xenbus connection to re-establish itself *)
		"online", "1";
		"removable", if x.unpluggable then "1" else "0";
		"state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
		"dev", to_linux_device device_number;
		"type", if backend_kind = Qdisk then "qdisk" else backendty_of_physty x.phystype;
		"mode", string_of_mode x.mode;
		"params", if backend_kind = Qdisk then "aio:" ^ x.params else x.params;
	];
    (* We don't have PV drivers for HVM guests for CDROMs. We prevent
       blkback from successfully opening the device since this can
       prevent qemu CD eject (and subsequent vdi_deactivate) *)
	if hvm && (x.dev_type = CDROM) then
		Hashtbl.add back_tbl "no-physical-device" "";

	Opt.iter
		(fun protocol ->
			Hashtbl.add front_tbl "protocol" (string_of_protocol protocol)
		) x.protocol;

	let back = Hashtbl.fold (fun k v acc -> (k, v) :: acc) back_tbl [] in
	let front = Hashtbl.fold (fun k v acc -> (k, v) :: acc) front_tbl [] in

	Generic.add_device ~xs device back front x.extra_private_keys;
	device

let add_wait (task: Xenops_task.t) ~xs device =
	if !Xenopsd.run_hotplug_scripts
	then Hotplug.run_hotplug_script device [ "add" ];

	Hotplug.wait_for_plug task ~xs device;
	debug "Device.Vbd successfully added; device_is_online = %b" (Hotplug.device_is_online ~xs device);

	(* 'Normally' we connect devices to other domains, and cannot know whether the
	   device is 'available' from their userspace (or even if they have a userspace).
	   The best we can do is just to wait for the backend hotplug scripts to run,
	   indicating that the backend has locked the resource.
	   In the case of domain 0 we can do better: we have custom hotplug scripts
	   which call us back when the device is actually available to userspace. We need
	   to wait for this condition to make the template installers work.
	   NB if the custom hotplug script fires this implies that the xenbus state
	   reached "connected", so we don't have to check for that first. *)
	if device.frontend.domid = 0 then begin
		let path = device.frontend.devid |> Device_number.of_xenstore_key |> Device_number.to_linux_device |> (fun x -> "/dev/" ^ x) in
		debug "Waiting for %s to appear" path;
		let start = Unix.gettimeofday () in
		while not (Sys.file_exists path) && (Unix.gettimeofday () -. start < 30.) do
			Thread.delay 0.2
		done
	end;
	device

(* Add the VBD to the domain, When this command returns, the device is ready. (This isn't as
   concurrent as xend-- xend allocates loopdevices via hotplug in parallel and then
   performs a 'waitForDevices') *)
let add (task: Xenops_task.t) ~xs ~hvm ?(backend_kind=Vbd) x domid =
	let device =
		let result = ref None in
		while !result = None do
			try
				result := Some (add_async ~xs ~hvm ~backend_kind x domid);
			with Device_frontend_already_connected _ as e ->
				if x.device_number = None then begin
					debug "Temporary failure to allocte a device number; retrying";
					Thread.delay 0.1
				end else raise e (* permanent failure *)
		done; Opt.unbox !result in
	add_wait task ~xs device

end

module PV_Vnc = struct

let vnc_pid_path domid = sprintf "/local/domain/%d/vncterm-pid" domid
let vnc_console_path domid = sprintf "/local/domain/%d/console" domid

let pid ~xs domid =
	try
		let pid = xs.Xs.read (vnc_pid_path domid) in
		Some (int_of_string pid)
	with _ ->
		None

(* Look up the commandline args for the vncterm pid; *)
(* Check that they include the vncterm binary path and the xenstore console path for the supplied domid. *)
let is_cmdline_valid domid pid =
	let null = Re_str.regexp "[\000]" in
	let cmdline =
		Printf.sprintf "/proc/%d/cmdline" pid
		|> Unixext.string_of_file
		|> Re_str.split null
	in
	if (List.mem !Xl_path.vncterm cmdline) && (List.mem (vnc_console_path domid) cmdline)
	then true
	else false

let is_vncterm_running ~xs domid =
	match pid ~xs domid with
		| None -> false
		| Some p ->
			try
				Unix.kill p 0;
				is_cmdline_valid domid p
			with _ -> false

let get_vnc_port ~xs domid =
	if not (is_vncterm_running ~xs domid)
	then None
	else (try Some(int_of_string (xs.Xs.read (Generic.vnc_port_path domid))) with _ -> None)

let get_tc_port ~xs domid =
	if not (is_vncterm_running ~xs domid)
	then None
	else (try Some(int_of_string (xs.Xs.read (Generic.tc_port_path domid))) with _ -> None)


let load_args = function
	| None -> []
	| Some filename ->
		if Sys.file_exists filename
		then ["-l"; filename]
		else []

exception Failed_to_start

let vncterm_statefile pid = sprintf "/var/xen/vncterm/%d/vncterm.statefile" pid

let get_statefile ~xs domid =
	match pid ~xs domid with
	| None -> None
	| Some pid -> 
		  let filename = vncterm_statefile pid in
		  if Sys.file_exists filename then
			  Some filename
		  else
			  None

let save ~xs domid =
	match pid ~xs domid with
	| Some pid -> 
		  Unix.kill pid Sys.sigusr1;
		  let filename = vncterm_statefile pid in
		  let delay = 10. in
		  let start_time = Unix.time () in
		  (* wait at most ten seconds *)
		  while not (Sys.file_exists filename) || Unix.time () -. start_time > delay do
			  debug "Device.PV_Vnc.save: waiting for %s to appear" filename;
			  Thread.delay 1.
		  done;
		  if Unix.time () -. start_time > delay then
			  debug "Device.PV_Vnc.save: timeout while waiting for %s to appear" filename
		  else
			  debug "Device.PV_Vnc.save: %s has appeared" filename
	| None     -> ()

let start ?statefile ~xs ?ip domid =
        debug "In PV_Vnc.start";
	let ip = Opt.default "127.0.0.1" ip in
	let l = [ "-x"; sprintf "/local/domain/%d/console" domid;
		  "-T"; (* listen for raw connections *)
		  "-v"; ip ^ ":1";
		] @ load_args statefile in
	(* Now add the close fds wrapper *)
	let pid = Forkhelpers.safe_close_and_exec None None None [] !Xl_path.vncterm l in
	let path = vnc_pid_path domid in
	xs.Xs.write path (string_of_int (Forkhelpers.getpid pid));
	Forkhelpers.dontwaitpid pid

let stop ~xs domid =
	let open Generic in
	match pid ~xs domid with
		| Some pid ->
			best_effort "killing vncterm"
				(fun () -> Unix.kill (-pid) Sys.sigterm);
			best_effort "removing vncterm-pid from xenstore"
				(fun () -> xs.Xs.rm (vnc_pid_path domid))
		| None -> ()

end

module Qemu = struct
	(* Where qemu-dm-wrapper writes its pid *)
	let qemu_pid_path domid = sprintf "/local/domain/%d/qemu-pid" domid

	let pid ~xs domid =
		try
			let pid = xs.Xs.read (qemu_pid_path domid) in
			Some (int_of_string pid)
		with _ ->
			None

	let is_running ~xs domid =
		match pid ~xs domid with
			| None -> false
			| Some p ->
				try
					Unix.kill p 0;
					true
				with _ -> false
end

let can_surprise_remove ~xs (x: device) = Generic.can_surprise_remove ~xs x

module Dm = struct
	let get_vnc_port ~xs domid =
		if false (*not (Qemu.is_running ~xs domid)*)
		then None
		else (try Some(int_of_string (xs.Xs.read (Generic.vnc_port_path domid))) with _ -> None)

	let get_tc_port ~xs domid =
		if not (Qemu.is_running ~xs domid)
		then None
		else (try Some(int_of_string (xs.Xs.read (Generic.tc_port_path domid))) with _ -> None)
end

let get_vnc_port ~xs domid = 
	(* Check whether a qemu exists for this domain *)
(*	let qemu_exists = Qemu.is_running ~xs domid in *)
	if true (* qemu_exists *)
	then Dm.get_vnc_port ~xs domid
	else PV_Vnc.get_vnc_port ~xs domid

let get_tc_port ~xs domid = 
	(* Check whether a qemu exists for this domain *)
	let qemu_exists = Qemu.is_running ~xs domid in
	if qemu_exists
	then Dm.get_tc_port ~xs domid
	else PV_Vnc.get_tc_port ~xs domid


