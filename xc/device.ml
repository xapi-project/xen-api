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
open Xenops_interface

open Device_common
open Xenstore
open Cancel_utils
open Xenops_task

exception Ioemu_failed of (string * string)

exception Device_shutdown
exception Device_not_found

exception Cdrom

module D = Debug.Make(struct let name = "xenops" end)
open D

(** Definition of available qemu profiles, used by the qemu backend implementations *)
module Profile = struct
  type t = Qemu_trad | Qemu_upstream_compat | Qemu_upstream [@@deriving rpc]
  let fallback = Qemu_trad
  let all = [ Qemu_trad; Qemu_upstream_compat; Qemu_upstream ]
  module Name = struct
    let qemu_trad            = "qemu-trad"
    let qemu_upstream_compat = "qemu-upstream-compat"
    let qemu_upstream        = "qemu-upstream"
  end
  let wrapper_of = function
    | Qemu_trad            -> !Resources.qemu_dm_wrapper
    | Qemu_upstream_compat -> !Resources.upstream_compat_qemu_dm_wrapper
    | Qemu_upstream        -> !Resources.upstream_compat_qemu_dm_wrapper
  let string_of  = function
    | Qemu_trad              -> Name.qemu_trad
    | Qemu_upstream_compat   -> Name.qemu_upstream_compat
    | Qemu_upstream          -> Name.qemu_upstream
  let of_string  = function
    | x when x = Name.qemu_trad            -> Qemu_trad
    | x when x = Name.qemu_upstream_compat -> Qemu_upstream_compat
    | x when x = Name.qemu_upstream        ->
       sprintf "unsupported device-model profile %s: use %s" x Name.qemu_upstream_compat
       |> fun s -> Internal_error s
       |> raise
    | x -> debug "unknown device-model profile %s: defaulting to fallback: %s" x (string_of fallback);
      fallback

  (* XXX remove again *)
  let of_domid x = if is_upstream_qemu x then Qemu_upstream else Qemu_trad
end

(** Represent an IPC endpoint *)
module Socket = struct
  type t = Unix of string | Port of int
  module Unix = struct
    let path x = "unix:" ^ x
    let rm   x =
      let dbg = debug "error cleaning unix socket %s: %s" x in
      try Unix.unlink x
      with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      | Unix.Unix_error (e, _, _)           -> dbg (Unix.error_message e)
  end
end

(* keys read by vif udev script (keep in sync with api:scripts/vif) *)
let vif_udev_keys = "promiscuous" :: (List.map (fun x -> "ethtool-" ^ x) [ "rx"; "tx"; "sg"; "tso"; "ufo"; "gso" ])

(****************************************************************************************)

module Generic = struct

  let vnc_port_path domid = sprintf "/local/domain/%d/console/vnc-port" domid

  let tc_port_path domid = sprintf "/local/domain/%d/console/tc-port" domid

  (* Oxenstored's transaction conflict algorithm will cause parallel but separate device
     creation transactions to abort and retry, leading to livelock while starting lots of
     VMs. Work around this by serialising these transactions for now. *)
  let device_serialise_m = Mutex.create ()
  let add_device ~xs device backend_list frontend_list private_list xenserver_list =
    Mutex.execute device_serialise_m (fun () ->

        let frontend_ro_path = frontend_ro_path_of_device ~xs device
        and frontend_rw_path = frontend_rw_path_of_device ~xs device
        and backend_path = backend_path_of_device ~xs device
        and hotplug_path = Hotplug.get_hotplug_path device
        and private_data_path = Device_common.get_private_data_path_of_device device
        and extra_xenserver_path = Device_common.extra_xenserver_path_of_device ~xs device in

        debug "adding device  B%d[%s]  F%d[%s]  H[%s]" device.backend.domid backend_path device.frontend.domid frontend_rw_path hotplug_path;
        Xs.transaction xs (fun t ->
            begin try
                (* Use the ro one because a bad guest could delete the rw node. *)
                ignore (t.Xst.read frontend_ro_path);
                raise (Device_frontend_already_connected device)
              with Xs_protocol.Enoent _ -> () end;

            t.Xst.rm frontend_rw_path;
            t.Xst.rm frontend_ro_path;
            t.Xst.rm backend_path;
            (* CA-16259: don't clear the 'hotplug_path' because this is where we
               		   record our own use of /dev/loop devices. Clearing this causes us to leak
               		   one per PV .iso *)

            t.Xst.mkdirperms frontend_rw_path (Xenbus_utils.device_frontend device);
            t.Xst.mkdirperms frontend_ro_path (Xenbus_utils.rwperm_for_guest 0);

            t.Xst.mkdirperms backend_path (Xenbus_utils.device_backend device);

            t.Xst.mkdirperms hotplug_path (Xenbus_utils.hotplug device);

            t.Xst.writev frontend_rw_path
              (("backend", backend_path) :: frontend_list);
            t.Xst.writev frontend_ro_path
              (("backend", backend_path) :: []);
            t.Xst.writev backend_path
              (("frontend", frontend_rw_path) :: backend_list);

            t.Xst.mkdirperms private_data_path (Xenbus_utils.hotplug device);
            t.Xst.writev private_data_path
              (("backend-kind", string_of_kind device.backend.kind) ::
               ("backend-id", string_of_int device.backend.domid) :: private_list);

            t.Xst.mkdirperms extra_xenserver_path (Xenbus_utils.rwperm_for_guest device.frontend.domid);
            t.Xst.writev extra_xenserver_path xenserver_list;
          )
      )

  let get_private_key ~xs device x =
    let private_data_path = Device_common.get_private_data_path_of_device device in
    let key = private_data_path ^ "/" ^x in
    try
      xs.Xs.read key
    with e ->
      error "read %s: Noent" key;
      raise e

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
    safe_rm ~xs (frontend_ro_path_of_device ~xs x);
    safe_rm ~xs (frontend_rw_path_of_device ~xs x);
    safe_rm ~xs (backend_path_of_device ~xs x);
    (* Cleanup the directory containing the error node *)
    safe_rm ~xs (backend_error_path_of_device ~xs x);
    safe_rm ~xs (Filename.dirname (error_path_of_device ~xs x))

  (* The surprise-remove flag is now ignored: a vbd-unplug --force will
     	unplug regardless of surprise-remove. Leave this code here for now,
     	to warn the user in the logs. *)
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

  let unplug_watch ~xs (x: device) =
    let path = Hotplug.path_written_by_hotplug_scripts x in
    let qdisk = Astring.String.is_infix ~affix:"backend/qdisk/" path in
    if not qdisk then begin
      Watch.key_to_disappear path
    end else begin
      debug "unplug_watch: not waiting for qdisk %s; returning dummy watch" path;
      Watch.{ evaluate = fun xs -> try xs.Xs.rm path with _ -> debug "dummy unplug_watch for qdisk: %s already removed" path }
    end

  let error_watch ~xs (x: device) = Watch.value_to_appear (error_path_of_device ~xs x)
  let frontend_closed ~xs (x: device) = Watch.map (fun () -> "") (Watch.value_to_become (frontend_rw_path_of_device ~xs x ^ "/state") (Xenbus_utils.string_of Xenbus_utils.Closed))
  let backend_closed ~xs (x: device) = Watch.value_to_become (backend_path_of_device ~xs x ^ "/state") (Xenbus_utils.string_of Xenbus_utils.Closed)

  let clean_shutdown_wait (task: Xenops_task.task_handle) ~xs ~ignore_transients (x: device) =
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
    let unplug = Watch.map (fun _ -> ()) (unplug_watch ~xs x) in
    let error = Watch.map (fun _ -> ()) (error_watch ~xs x) in
    if cancellable_watch cancel [ frontend_closed; unplug ] (if ignore_transients then [] else [ error ]) task ~xs ~timeout:!Xenopsd.hotplug_timeout ()
    then begin
      safe_rm ~xs (frontend_rw_path_of_device ~xs x);
      safe_rm ~xs (frontend_ro_path_of_device ~xs x);
      if cancellable_watch cancel [ unplug ] (if ignore_transients then [] else [ error ]) task ~xs ~timeout:!Xenopsd.hotplug_timeout ()
      then rm_device_state ~xs x
      else on_error ()
    end else on_error ()

  let clean_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
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
    safe_rm xs (frontend_rw_path_of_device ~xs x);
    safe_rm xs (frontend_ro_path_of_device ~xs x)

  let hard_shutdown_complete ~xs (x: device) =
    if !Xenopsd.run_hotplug_scripts
    then backend_closed ~xs x
    else unplug_watch ~xs x

  let hard_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
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

(** Vbd_Common contains the private Vbd functions that are common between the qemu profile backends *)
module Vbd_Common = struct

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

  type devty = CDROM | Disk | Floppy

  let string_of_devty = function
    | CDROM -> "cdrom"
    | Disk  -> "disk"
    | Floppy -> "floppy"

  let devty_of_string = function
    | "cdrom" -> CDROM
    | "disk"  -> Disk
    | "floppy" -> Floppy
    | _       -> invalid_arg "devty_of_string"

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

  let shutdown_request_clean_shutdown_wait (task: Xenops_task.task_handle) ~xs ~ignore_transients (x: device) =
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

  let shutdown_request_hard_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Vbd.hard_shutdown %s" (string_of_device x);
    request_shutdown ~xs x true; (* force *)

    (* We don't watch for error nodes *)
    let (_: bool) = cancellable_watch (Device x) [ shutdown_done ~xs x ] [] task ~xs ~timeout:!Xenopsd.hotplug_timeout () in
    Generic.rm_device_state ~xs x;

    debug "Device.Vbd.hard_shutdown complete"

  let clean_shutdown_async ~xs x = match shutdown_mode_of_device ~xs x with
    | Classic -> Generic.clean_shutdown_async ~xs x
    | ShutdownRequest -> request_shutdown ~xs x false (* normal *)

  let clean_shutdown_wait (task: Xenops_task.task_handle) ~xs ~ignore_transients x = match shutdown_mode_of_device ~xs x with
    | Classic -> Generic.clean_shutdown_wait task ~xs ~ignore_transients x
    | ShutdownRequest -> shutdown_request_clean_shutdown_wait task ~xs ~ignore_transients x

  let clean_shutdown (task: Xenops_task.task_handle) ~xs x =
    clean_shutdown_async ~xs x;
    clean_shutdown_wait task ~xs ~ignore_transients:false x

  let hard_shutdown (task: Xenops_task.task_handle) ~xs x = match shutdown_mode_of_device ~xs x with
    | Classic -> Generic.hard_shutdown task ~xs x
    | ShutdownRequest -> shutdown_request_hard_shutdown task ~xs x

  let hard_shutdown_request ~xs x = match shutdown_mode_of_device ~xs x with
    | Classic -> Generic.hard_shutdown_request ~xs x
    | ShutdownRequest -> request_shutdown ~xs x true

  let hard_shutdown_complete ~xs x = match shutdown_mode_of_device ~xs x with
    | Classic -> Generic.hard_shutdown_complete ~xs x
    | ShutdownRequest -> shutdown_done ~xs x

  let hard_shutdown_wait (task: Xenops_task.task_handle) ~xs ~timeout x =
    let (_: bool) = cancellable_watch (Device x) [ Watch.map (fun _ -> ()) (hard_shutdown_complete ~xs x) ] [] task ~xs ~timeout () in
    ()

  let release (task: Xenops_task.task_handle) ~xc ~xs (x: device) =
    debug "Device.Vbd.release %s" (string_of_device x);
    (* Make sure blktap/blkback fire the udev remove event by deleting the
       	   backend now *)
    Generic.safe_rm ~xs (backend_path_of_device ~xs x);
    Hotplug.release task ~xc ~xs x;

    if !Xenopsd.run_hotplug_scripts
    then Hotplug.run_hotplug_script x [ "remove" ];

    (* As for add above, if the frontend is in dom0, we can wait for the frontend 
       	 * to unplug as well as the backend. CA-13506 *)
    if x.frontend.domid = 0 then Hotplug.wait_for_frontend_unplug task ~xs x

  let free_device ~xs hvm domid =
    let disks = List.map
        (fun x -> x.frontend.devid
                  |> Device_number.of_xenstore_key
                  |> Device_number.spec
                  |> (fun (_, disk, _) -> disk))
        (Device_common.list_frontends ~xs domid) in
    let next = List.fold_left max 0 disks + 1 in
    let open Device_number in
    let bus_type = if (hvm && next < 4) then Ide else Xen in
    bus_type, next, 0

  type t = {
    mode:mode;
    device_number: Device_number.t option;
    phystype: physty;
    params: string;
    dev_type: devty;
    unpluggable: bool;
    protocol: protocol option;
    kind: Device_common.kind;
    extra_backend_keys: (string * string) list;
    extra_private_keys: (string * string) list;
    backend_domid: int;
  }

  let add_async ~xs ~hvm x domid =
    let back_tbl = Hashtbl.create 16 and front_tbl = Hashtbl.create 16 in
    let open Device_number in
    (* If no device number is provided then autodetect a free one *)
    let device_number = match x.device_number with
      | Some x -> x
      | None ->
        make (free_device ~xs hvm domid) in
    let devid = to_xenstore_key device_number in
    let device = 
      let backend = { domid = x.backend_domid; kind = x.kind; devid = devid }
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
      "device-type", match x.dev_type with
      | CDROM -> "cdrom"
      | Disk -> "disk"
      | Floppy -> "floppy";
    ];
    (* Hack: this should be returned separately from SMAPIv3 attach call *)
    let (params, qemu_params) = if String.startswith "hack|" x.params then begin
        match String.split_on_char '|' x.params with
        | [_; params; qemu_params] -> (params, qemu_params)
        | _ -> (x.params, "")
      end else (x.params, "") in
    List.iter (fun (k, v) -> Hashtbl.replace back_tbl k v) [
      "frontend-id", sprintf "%u" domid;
      (* Prevents the backend hotplug scripts from running if the frontend disconnects.
         		   This allows the xenbus connection to re-establish itself *)
      "online", "1";
      "removable", if x.unpluggable then "1" else "0";
      "state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
      "dev", to_linux_device device_number;
      "type", backendty_of_physty x.phystype;
      "mode", string_of_mode x.mode;
      "params", params;
      "qemu-params", qemu_params;
      "direct-io-safe", "1";
    ];
    (* We don't have PV drivers for HVM guests for CDROMs. We prevent
       blkback from successfully opening the device since this can
       prevent qemu CD eject (and subsequent vdi_deactivate) *)
    let no_phys_device =
      if hvm && (x.dev_type = CDROM) then ["no-physical-device", ""] else [] in

    Opt.iter
      (fun protocol ->
         Hashtbl.add front_tbl "protocol" (string_of_protocol protocol)
      ) x.protocol;

    let back = Hashtbl.fold (fun k v acc -> (k, v) :: acc) back_tbl [] in
    let front = Hashtbl.fold (fun k v acc -> (k, v) :: acc) front_tbl [] in
    let priv = no_phys_device @ x.extra_private_keys in

    Generic.add_device ~xs device back front priv [];
    device

  let add_wait (task: Xenops_task.task_handle) ~xc ~xs device =
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
      try
        (* CA-15605: clean up on dom0 block-attach failure *)
        Hotplug.wait_for_frontend_plug task ~xs device;
      with Hotplug.Frontend_device_error _ as e ->
        debug "Caught Frontend_device_error: assuming it is safe to shutdown the backend";
        clean_shutdown task ~xs device; (* assumes double-failure isn't possible *)
        release task ~xc ~xs device;
        raise e
    end;
    device

  (* Add the VBD to the domain, When this command returns, the device is ready. (This isn't as
     concurrent as xend-- xend allocates loopdevices via hotplug in parallel and then
     performs a 'waitForDevices') *)
  let add (task: Xenops_task.task_handle) ~xc ~xs ~hvm x domid =
    let device =
      let result = ref None in
      while !result = None do
        try
          result := Some (add_async ~xs ~hvm x domid);
        with Device_frontend_already_connected _ as e ->
          if x.device_number = None then begin
            debug "Temporary failure to allocte a device number; retrying";
            Thread.delay 0.1
          end else raise e (* permanent failure *)
      done; Opt.unbox !result in
    add_wait task ~xc ~xs device

  let qemu_media_change ~xs device _type params =
    let backend_path  = (backend_path_of_device ~xs device) in
    let params_path = backend_path ^ "/params" in

    (* unfortunately qemu filter the request if on the same string it has,
       	   so we trick it by having a different string, but the same path, adding a
       	   spurious '/' character at the beggining of the string.  *)
    let oldval = try xs.Xs.read params_path with _ -> "" in
    let pathtowrite =
      if oldval = params then (
        "/" ^ params
      ) else
        params in

    let back_delta = [
      "type",           _type;
      "params",         pathtowrite;
    ] in
    Xs.transaction xs (fun t -> t.Xst.writev backend_path back_delta);
    debug "Media changed: params = %s" pathtowrite

  let media_is_ejected ~xs device =
    let path = (backend_path_of_device ~xs device) ^ "/params" in
    try xs.Xs.read path = "" with _ -> raise Device_not_found

end

(****************************************************************************************)
(** VIFs:                                                                               *)

(**
   Generate a random MAC address, using OUI (Organizationally Unique
   Identifier) 00-16-3E, allocated to Xensource, Inc.

   The remaining 3 fields are random, with the first bit of the first random
   field set 0.
*)

module Vif = struct

  let add ~xs ~devid ~mac ?mtu ?(rate=None) ?(backend_domid=0) ?(other_config=[]) 
      ~netty ~carrier ?(protocol=Protocol_Native) ?(extra_private_keys=[]) ?(extra_xenserver_keys=[]) (task: Xenops_task.task_handle) domid =
    debug "Device.Vif.add domid=%d devid=%d mac=%s carrier=%b rate=%s other_config=[%s] extra_private_keys=[%s] extra_xenserver_keys=[%s]" domid devid mac carrier
      (match rate with None -> "none" | Some (a, b) -> sprintf "(%Ld,%Ld)" a b)
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) other_config))
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) extra_private_keys))
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) extra_xenserver_keys));
    (* Filter the other_config keys using vif_udev_keys as a whitelist *)
    let other_config = List.filter (fun (x, _) -> List.mem x vif_udev_keys) other_config in
    let frontend = { domid = domid; kind = Vif; devid = devid } in
    let backend = { domid = backend_domid; kind = Vif; devid = devid } in
    let device = { backend = backend; frontend = frontend } in


    let back_options =
      match rate with
      | None                              -> []
      | Some (kbytes_per_s, timeslice_us) ->
        let ( ^* ) = Int64.mul and ( ^/ ) = Int64.div in
        let timeslice_us =
          if timeslice_us > 0L then
            timeslice_us
          else
            50000L (* 50ms by default *) in
        let bytes_per_interval = ((kbytes_per_s ^* 1024L) ^* timeslice_us)
                                 ^/ 1000000L in
        if bytes_per_interval > 0L && bytes_per_interval < 0xffffffffL then
          [ "rate", sprintf "%Lu,%Lu" bytes_per_interval timeslice_us ]
        else (
          debug "VIF qos: invalid value for byte/interval: %Lu" bytes_per_interval;
          []
        )
    in

    let back = [
      "frontend-id", sprintf "%u" domid;
      "online", "1";
      "state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
      "script", !Xc_resources.vif_script;
      "mac", mac;
      "handle", string_of_int devid
    ] @ back_options in

    let front_options =
      if protocol <> Protocol_Native then
        [ "protocol", string_of_protocol protocol; ]
      else
        [] in

    let front = [
      "backend-id", string_of_int backend_domid;
      "state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
      "handle", string_of_int devid;
      "mac", mac;
      "disconnect", if carrier then "0" else "1";
    ] @ front_options in

    let extra_private_keys = List.map (fun (k, v) -> "other-config/" ^ k, v) other_config @ extra_private_keys in
    (* Add the rest of the important configuration to the private bit of xenstore so we can access it later *)
    let extra_private_keys = extra_private_keys @
                             ("mac", mac) ::
                             (match mtu with | Some mtu when mtu > 0 -> [ "MTU", string_of_int mtu ] | _ -> []) @
                             (match netty with
                              | Netman.Bridge b -> [ "bridge", b; "bridge-MAC", "fe:ff:ff:ff:ff:ff"; ]
                              | Netman.Vswitch b -> [ "bridge", b; "bridge-MAC", "fe:ff:ff:ff:ff:ff"; ]
                              | Netman.DriverDomain -> []
                              | Netman.Nat -> []) @
                             (match rate with | None -> [] | Some(rate, timeslice) -> [ "rate", Int64.to_string rate; "timeslice", Int64.to_string timeslice ]) in

    Generic.add_device ~xs device back front extra_private_keys extra_xenserver_keys;

    if !Xenopsd.run_hotplug_scripts then begin
      (* The VIF device won't be created until the backend is
         		   in state InitWait: *)
      Hotplug.wait_for_connect task ~xs device;
      let tap = { device with backend = { device.backend with kind = Tap } } in
      Hotplug.run_hotplug_script device [ "add" ];
      Hotplug.run_hotplug_script device [ "online" ];
      Hotplug.run_hotplug_script tap [ "add" ];
      Hotplug.run_hotplug_script tap [ "online" ];
    end;

    Hotplug.wait_for_plug task ~xs device;
    device

  let clean_shutdown = Generic.clean_shutdown

  let hard_shutdown = Generic.hard_shutdown

  let set_carrier ~xs (x: device) carrier = 
    debug "Device.Vif.set_carrier %s <- %b" (string_of_device x) carrier;
    let disconnect_path = disconnect_path_of_device ~xs x in
    xs.Xs.write disconnect_path (if carrier then "0" else "1")

  let release (task: Xenops_task.task_handle) ~xc ~xs (x: device) =
    debug "Device.Vif.release %s" (string_of_device x);

    if !Xenopsd.run_hotplug_scripts then begin
      let tap = { x with backend = { x.backend with kind = Tap } } in
      Hotplug.run_hotplug_script x [ "remove" ];
      Hotplug.run_hotplug_script tap [ "remove" ];
    end;
    Hotplug.release task ~xc ~xs x

  let move ~xs (x: device) bridge =
    let xs_bridge_path = Device_common.get_private_data_path_of_device x ^ "/bridge" in
    xs.Xs.write xs_bridge_path bridge;
    Hotplug.run_hotplug_script x ["move"; "type_if=vif"];
    (* Maybe there's a tap, too *)
    try Hotplug.run_hotplug_script {x with backend={x.backend with kind=Tap}}  ["move"; "type_if=tap"] with _ -> ()

end

(****************************************************************************************)
(** Network SR-IOV VFs:                                                                 *)
module NetSriovVf = struct

  let add  ~xs ~devid ~mac ?mtu ?(rate=None) ?(backend_domid=0) ?(other_config=[]) 
      ~pci ~vlan ~carrier ?(extra_private_keys=[]) ?(extra_xenserver_keys=[])
      (task: Xenops_task.task_handle) domid =
    let vlan_str = match vlan with None -> "none" | Some vlan -> sprintf "%Ld" vlan in
    let rate_str = match rate with None -> "none" | Some (a, b) -> sprintf "(%Ld,%Ld)" a b in
    debug "Device.NetSriovVf.add domid=%d devid=%d pci=%s vlan=%s mac=%s carrier=%b \
           rate=%s other_config=[%s] extra_private_keys=[%s] extra_xenserver_keys=[%s]" 
      domid devid (Xenops_interface.Pci.string_of_address pci)  vlan_str mac carrier rate_str
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) other_config))
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) extra_private_keys))
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) extra_xenserver_keys));

    let frontend = { domid = domid; kind = NetSriovVf; devid = devid } in
    let backend = { domid = backend_domid; kind = NetSriovVf; devid = devid } in
    let device = { backend = backend; frontend = frontend } in
    Generic.add_device ~xs device [] [] extra_private_keys (extra_xenserver_keys @ other_config);
    let rate_Mbps = match rate with
      | Some (0L, _) | None -> None
      | Some (rate, _) -> (match Int64.div rate 1024L with
        | 0L -> Some 1L
        | rate -> Some rate)
    in
    let net_sriov_vf_config = Network_client.Client.Sriov.{
      mac=Some mac;
      vlan=vlan;
      rate=rate_Mbps;}
    in
    begin
      let ret = Network_client.Client.Sriov.make_vf_config
        (Xenops_task.get_dbg task) pci net_sriov_vf_config
      in
      let open Network_client.Client.Sriov in
      match ret with
      | Ok -> ()
      | Error Config_vf_rate_not_supported ->
        error "It is not supported to configure rate on this network SR-IOV VF (pci:%s)"
          (Pci.string_of_address pci)
      | Error (Unknown s) ->
        failwith (Printf.sprintf "Failed to configure network SR-IOV VF \
            (pci:%s) with mac=%s vlan=%s rate=%s: %s"
            (Pci.string_of_address pci) mac vlan_str rate_str s)
    end;
    device

  let hard_shutdown ~xs (x: device) =
    debug "Device.NetSriovVf.hard_shutdown about to blow away backend and frontend paths";
    Generic.safe_rm ~xs (frontend_ro_path_of_device ~xs x);
    Generic.safe_rm ~xs (frontend_rw_path_of_device ~xs x);
    Generic.safe_rm ~xs (backend_path_of_device ~xs x)

end

(*****************************************************************************)
(** Vcpus:                                                                   *)
module Vcpu_Common = struct

  let add ~xs ~devid domid online =
    let path = sprintf "/local/domain/%d/cpu/%d/availability" domid devid in
    xs.Xs.write path (if online then "online" else "offline")

  let set = add

  let del ~xs ~devid domid =
    let path = sprintf "/local/domain/%d/cpu/%d" domid devid in
    xs.Xs.rm path

  let status ~xs ~devid domid =
    let path = sprintf "/local/domain/%d/cpu/%d/availability" domid devid in
    try match xs.Xs.read path with
      | "online"  -> true
      | "offline" -> false
      | _         -> (* garbage, assuming false *) false
    with Xs_protocol.Enoent _ -> false

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
    let cmdline =
      Printf.sprintf "/proc/%d/cmdline" pid
      |> Unixext.string_of_file
      |> Stdext.Xstringext.String.split '\000'
    in
    if (List.mem !Xc_resources.vncterm cmdline) && (List.mem (vnc_console_path domid) cmdline)
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
    else (try Some(Socket.Port (int_of_string (xs.Xs.read (Generic.vnc_port_path domid)))) with _ -> None)

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
    let pid = Forkhelpers.safe_close_and_exec None None None [] !Xc_resources.vncterm l in
    let path = vnc_pid_path domid in
    xs.Xs.write path (string_of_int (Forkhelpers.getpid pid));
    Forkhelpers.dontwaitpid pid

  let stop ~xs domid =
    let open Generic in
    match pid ~xs domid with
    | Some pid ->
      best_effort "killing vncterm"
        (fun () -> Unix.kill pid Sys.sigterm);
      best_effort "removing vncterm-pid from xenstore"
        (fun () -> xs.Xs.rm (vnc_pid_path domid))
    | None -> ()

end

module type DAEMONPIDPATH = sig
  val pid_path : int -> string
end

module DaemonMgmt (D : DAEMONPIDPATH) = struct
  module SignalMask = struct
    module H = Hashtbl
    type t = (int, bool) H.t
    let create () = H.create 16
    let set tbl key = H.replace tbl key true
    let unset tbl key = H.remove tbl key
    let has tbl key = H.mem tbl key
  end
  let signal_mask = SignalMask.create ()

  let pid_path = D.pid_path
  let pid_path_signal domid = (pid_path domid) ^ "-signal"
  let pid ~xs domid =
    try
      let pid = xs.Xs.read (pid_path domid) in
      Some (int_of_string pid)
    with _ -> None
  let is_running ~xs domid =
    match pid ~xs domid with
    | None -> false
    | Some p ->
      try
        Unix.kill p 0; (* This checks the existence of pid p *)
        true
      with _ -> false
end

module Qemu = DaemonMgmt(struct let pid_path domid = sprintf "/local/domain/%d/qemu-pid" domid end)
module Vgpu = DaemonMgmt(struct let pid_path domid = sprintf "/local/domain/%d/vgpu-pid" domid end)

module PCI = struct

  type t = {
    address: Xenops_interface.Pci.address;
    irq: int;
    resources: (int64 * int64 * int64) list;
    driver: string;
  }

  exception Cannot_add of Xenops_interface.Pci.address list * exn (* devices, reason *)
  exception Cannot_use_pci_with_no_pciback of t list

  (* same as libxl_internal: PROC_PCI_NUM_RESOURCES *)
  let _proc_pci_num_resources = 7
  (* same as libxl_internal: PCI_BAR_IO *)
  let _pci_bar_io = 0x01L

  (* XXX: we don't want to use the 'xl' command here because the "interface"
     isn't considered as stable as the C API *)
  let xl_pci cmd pcidevs domid =
    List.iter
      (fun dev ->
         try
           let (_, _) = Forkhelpers.execute_command_get_output
               "/usr/sbin/xl"
               [
                 cmd;
                 string_of_int domid;
                 Xenops_interface.Pci.string_of_address dev
               ] in
           ()
         with e ->
           debug "xl %s: %s" cmd (Printexc.to_string e);
           raise e
      ) pcidevs

  let add_xl = xl_pci "pci-attach"

  let release_xl = xl_pci "pci-detach"

  let device_model_pci_device_path xs be_domid fe_domid =
    let be_path = xs.Xs.getdomainpath be_domid in
    Printf.sprintf "%s/backend/pci/%d/0" be_path fe_domid

  (* Given a domid, return a list of [ X, (domain, bus, dev, func) ] where X indicates the order in
     which the device was plugged. *)
  let read_pcidir ~xs domid =
    let path = device_model_pci_device_path xs 0 domid in
    let prefix = "dev-" in
    let all = List.filter (String.startswith prefix) (try xs.Xs.directory path with Xs_protocol.Enoent _ -> []) in
    (* The values are the PCI device (domain, bus, dev, func) strings *)
    let device_number_of_string x =
      (* remove the silly prefix *)
      int_of_string (String.sub x (String.length prefix) (String.length x - (String.length prefix))) in
    let pairs = List.map
        (fun x ->
           device_number_of_string x,
           Xenops_interface.Pci.address_of_string (xs.Xs.read (path ^ "/" ^ x)))
        all
    in
    (* Sort into the order the devices were plugged *)
    List.sort (fun a b -> compare (fst a) (fst b)) pairs

  let add ~xs pcidevs domid =
    try
      let current = read_pcidir ~xs domid in
      let next_idx = List.fold_left max (-1) (List.map fst current) + 1 in
      add_xl pcidevs domid;
      List.iteri
        (fun count address ->
           xs.Xs.write
             (device_model_pci_device_path xs 0 domid ^ "/dev-" ^ string_of_int (next_idx + count))
             (Xenops_interface.Pci.string_of_address address))
        pcidevs
    with exn -> raise (Cannot_add (pcidevs, exn))

  let release pcidevs domid =
    release_xl pcidevs domid

  let write_string_to_file file s =
    let fn_write_string fd = Unixext.really_write fd s 0 (String.length s) in
    Unixext.with_file file [ Unix.O_WRONLY ] 0o640 fn_write_string

  let do_flr device =
    debug "Doing FLR on pci device: %s" device;
    let doflr = "/sys/bus/pci/drivers/pciback/do_flr" in
    let device_reset_file = Printf.sprintf "/sys/bus/pci/devices/%s/reset" device in
    let callscript s devstr =
      if Sys.file_exists !Xc_resources.pci_flr_script then begin
        try ignore (Forkhelpers.execute_command_get_output !Xc_resources.pci_flr_script [ s; devstr; ])
        with _ -> ()
      end
    in
    callscript "flr-pre" device;
    (
      if Sys.file_exists device_reset_file then
        try write_string_to_file device_reset_file "1" with _ -> ()
      else
        try write_string_to_file doflr device with _ -> ()
    );
    callscript "flr-post" device

  type supported_driver =
    | I915
    | Nvidia
    | Pciback

  type driver =
    | Supported of supported_driver
    | Unsupported of string

  let string_of_driver = function
    | Supported I915 -> "i915"
    | Supported Nvidia -> "nvidia"
    | Supported Pciback -> "pciback"
    | Unsupported driver -> driver

  let driver_of_string = function
    | "i915" -> Supported I915
    | "nvidia" -> Supported Nvidia
    | "pciback" -> Supported Pciback
    | driver -> Unsupported driver

  let sysfs_devices = "/sys/bus/pci/devices"
  let sysfs_drivers = "/sys/bus/pci/drivers"
  let sysfs_i915 = Filename.concat sysfs_drivers "i915"
  let sysfs_nvidia = Filename.concat sysfs_drivers "nvidia"
  let sysfs_pciback = Filename.concat sysfs_drivers "pciback"

  let get_driver devstr =
    try
      let sysfs_device = Filename.concat sysfs_devices devstr in
      Some (Filename.concat sysfs_device "driver"
            |> Unix.readlink
            |> Filename.basename
            |> driver_of_string)
    with _ -> None

  let bind_to_pciback devstr =
    debug "pci: binding device %s to pciback" devstr;
    let new_slot = Filename.concat sysfs_pciback "new_slot" in
    let bind = Filename.concat sysfs_pciback "bind" in
    write_string_to_file new_slot devstr;
    write_string_to_file bind devstr

  let bind_to_i915 devstr =
    debug "pci: binding device %s to i915" devstr;
    let is_loaded = Unixext.file_lines_fold (
        fun loaded line ->
          loaded || match Stdext.Xstringext.String.split ' ' line with "i915" :: _ -> true | _ -> false
      ) false "/proc/modules" in
    if not is_loaded then ignore (Forkhelpers.execute_command_get_output !Resources.modprobe ["i915"]);
    match get_driver devstr	with
    | None -> write_string_to_file (Filename.concat sysfs_i915 "bind") devstr
    | Some (Supported I915) -> ()
    | Some drv -> raise (Internal_error (Printf.sprintf "Fail to bind to i915, device is bound to %s" (string_of_driver drv)))

  let bind_to_nvidia devstr =
    debug "pci: binding device %s to nvidia" devstr;
    let bind = Filename.concat sysfs_nvidia "bind" in
    write_string_to_file bind devstr

  let unbind devstr driver =
    let driverstr = string_of_driver driver in
    debug "pci: unbinding device %s from %s" devstr driverstr;
    let sysfs_driver = Filename.concat sysfs_drivers driverstr in
    let unbind = Filename.concat sysfs_driver "unbind" in
    write_string_to_file unbind devstr

  let unbind_from_i915 devstr =
    unbind devstr (Supported I915);
    let (_:string * string) =
      Forkhelpers.execute_command_get_output !Resources.rmmod ["i915"] in ()

  let procfs_nvidia = "/proc/driver/nvidia/gpus"

  let nvidia_smi = "/usr/bin/nvidia-smi"

  let unbind_from_nvidia devstr =
    debug "pci: attempting to lock device %s before unbinding from nvidia" devstr;
    let gpus = Sys.readdir procfs_nvidia in
    (* Find the GPU with this device ID. *)
    let rec find_gpu = function
      | [] ->
        failwith (Printf.sprintf "Couldn't find GPU with device ID %s" devstr)
      | gpu :: rest ->
        let gpu_path = Filename.concat procfs_nvidia gpu in
        let gpu_info_file = Filename.concat gpu_path "information" in
        let gpu_info = Unixext.string_of_file gpu_info_file in
        (* Work around due to PCI ID formatting inconsistency. *)
        let devstr2 = String.copy devstr in
        devstr2.[7] <- '.';
        if false
        || (Stdext.Xstringext.String.has_substr gpu_info devstr2)
        || (Stdext.Xstringext.String.has_substr gpu_info devstr)
        then gpu_path
        else find_gpu rest
    in
    (* Disable persistence mode on the device before unbinding it. In future it
       	 * might be worth augmenting gpumon so that it can do this, and to enable
       	 * xapi and/or xenopsd to tell it to do so. *)
    let (_: string * string) =
      Forkhelpers.execute_command_get_output
        nvidia_smi
        ["--id="^devstr; "--persistence-mode=0"]
    in
    let unbind_lock_path =
      Filename.concat (find_gpu (Array.to_list gpus)) "unbindLock"
    in
    (* Grab the unbind lock. *)
    write_string_to_file unbind_lock_path "1\n";
    (* Unbind if we grabbed the lock; fail otherwise. *)
    if Unixext.string_of_file unbind_lock_path = "1\n"
    then unbind devstr (Supported Nvidia)
    else failwith (Printf.sprintf "Couldn't lock GPU with device ID %s" devstr)

  let bind_lock = Mutex.create ()

  let bind devices new_driver =
    let bind_to devstr = function
      | I915 -> bind_to_i915 devstr
      | Nvidia -> bind_to_nvidia devstr
      | Pciback -> begin
          bind_to_pciback devstr;
          do_flr devstr
        end
    in
    let unbind_from devstr = function
      | Supported I915 -> unbind_from_i915 devstr
      | Supported Nvidia -> unbind_from_nvidia devstr
      | driver -> unbind devstr driver
    in
    Mutex.execute bind_lock (fun () ->
        List.iter
          (fun device ->
             let devstr = Xenops_interface.Pci.string_of_address device in
             let old_driver = get_driver devstr in
             match old_driver, new_driver with
             (* We want the driver which is already bound. *)
             | Some (Supported I915), I915 ->
               debug "pci: device %s already bound to i915; doing nothing" devstr
             | Some (Supported Nvidia), Nvidia ->
               debug "pci: device %s already bound to nvidia; doing nothing" devstr
             | Some (Supported Pciback), Pciback ->
               debug "pci: device %s already bound to pciback; doing flr" devstr;
               do_flr devstr
             (* No driver is bound, so just bind the one we want. *)
             | None, new_driver ->
               debug "pci: device %s not bound" devstr;
               bind_to devstr new_driver
             (* Unbinding from one driver and binding to another driver. *)
             | Some (old_driver), new_driver ->
               unbind_from devstr old_driver;
               bind_to devstr new_driver)
          devices)

  let enumerate_devs ~xs (x: device) =
    let backend_path = backend_path_of_device ~xs x in
    let num =
      try int_of_string (xs.Xs.read (backend_path ^ "/num_devs"))
      with _ -> 0
    in
    let devs = Array.make num None in
    for i = 0 to num
    do
      try
        let devstr = xs.Xs.read (backend_path ^ "/dev-" ^ (string_of_int i)) in
        let dev = Xenops_interface.Pci.address_of_string devstr in
        devs.(i) <- Some dev
      with _ ->
        ()
    done;
    List.rev (List.fold_left (fun acc dev ->
        match dev with
        | None -> acc
        | Some dev -> dev :: acc
      ) [] (Array.to_list devs))

  let reset ~xs address =
    let devstr = Xenops_interface.Pci.string_of_address address in
    debug "Device.Pci.reset %s" devstr;
    do_flr devstr

  let clean_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Pci.clean_shutdown %s" (string_of_device x);
    let devs = enumerate_devs ~xs x in
    Xenctrl.with_intf (fun xc ->
        try release devs x.frontend.domid
        with _ -> ());
    ()

  let hard_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Pci.hard_shutdown %s" (string_of_device x);
    clean_shutdown task ~xs x

  (* This is the global location where PCI device add/remove status is put. We should aim to use
     a per-device location to support parallel requests in future *)
  let device_model_state_path xs be_domid fe_domid =
    Printf.sprintf "%s/device-model/%d/state" (xs.Xs.getdomainpath be_domid) fe_domid

  let signal_device_model ~xs domid cmd parameter = 
    debug "Device.Pci.signal_device_model domid=%d cmd=%s param=%s" domid cmd parameter;
    let be_domid = 0 in (* XXX: assume device model is in domain 0 *)
    let be_path = xs.Xs.getdomainpath be_domid in 
    (* Currently responses go in this global place. Blank it to prevent request/response/request confusion *)
    xs.Xs.rm (device_model_state_path xs be_domid domid);

    Xs.transaction xs (fun t ->
        t.Xst.writev be_path [ Printf.sprintf "device-model/%d/command" domid, cmd;
                               Printf.sprintf "device-model/%d/parameter" domid, parameter ];
      )

  let wait_device_model (task: Xenops_task.task_handle) ~xs domid =
    let be_domid = 0 in
    let path = device_model_state_path xs be_domid domid in
    let watch = Watch.value_to_appear path |> Watch.map (fun _ -> ()) in
    let shutdown = Watch.key_to_disappear (Qemu.pid_path domid) in
    let cancel = Domain domid in
    let (_: bool) = cancellable_watch cancel [ watch; shutdown ] [] task ~xs ~timeout:!Xenopsd.hotplug_timeout () in
    if Qemu.is_running ~xs domid then begin
      let answer = try xs.Xs.read path with _ -> "" in
      xs.Xs.rm path;
      Some answer
    end else begin
      info "wait_device_model: qemu has shutdown";
      None
    end

  (* Return a list of PCI devices *)
  let list ~xs domid = 
    (* replace the sort index with the default '0' -- XXX must figure out whether this matters to anyone *)
    List.map (fun (_, y) -> (0, y)) (read_pcidir ~xs domid)

  (* We explicitly add a device frontend in the hotplug case so the device watch code
     can find the backend and monitor it. *)
  let ensure_device_frontend_exists ~xs backend_domid frontend_domid =
    let frontend_path = Printf.sprintf "/local/domain/%d/device/pci/0" frontend_domid in
    let backend_path = Printf.sprintf "/local/domain/%d/backend/pci/%d/0" backend_domid frontend_domid in

    debug "adding PCI frontend: frontend_domid = %d; backend_domid = %d" frontend_domid backend_domid;
    Xs.transaction xs (fun t ->
        (* If the frontend already exists, no work to do *)
        if try ignore(t.Xst.read (frontend_path ^ "/backend")); true with _ -> false
        then debug "PCI frontend already exists: no work to do"
        else begin
          t.Xst.mkdirperms frontend_path (Xs_protocol.ACL.({owner = frontend_domid; other = NONE; acl = [ (backend_domid, READ) ]}));
          t.Xst.writev frontend_path [
            "backend", backend_path;
            "backend-id", string_of_int backend_domid;
            "state", "1"
          ]
        end
      )
end

module Vfs = struct

  let add ~xc ~xs ?(backend_domid=0) domid =
    debug "Device.Vfs.add domid=%d" domid;
    let frontend = { domid = domid; kind = Vfs; devid = 0 } in
    let backend = { domid = backend_domid; kind = Vfs; devid = 0 } in
    let _ = { backend = backend; frontend = frontend } in

    let frontend_path = Printf.sprintf "/local/domain/%d/device/vfs/%d" domid 0 in
    let backend_path = Printf.sprintf "/local/domain/%d/backend/vfs/%d" backend_domid domid in
    let request_path = Printf.sprintf "/local/domain/%d/backend/vfs/exports/requests/%d" backend_domid domid in

    (* TODO also have a backend?! *)
    let front = [
      "state", "ready"; (* definitely needs to be "ready" *)
      "backend", backend_path;
    ] in
    Xs.transaction xs (fun t ->
        (* Add the frontend *)
        let perms = Xs_protocol.ACL.({owner = domid; other = NONE; acl =[(0, READ)]}) in
        t.Xst.mkdirperms frontend_path perms;
        t.Xst.writev frontend_path front;

        (* Now make the request *)
        let perms = Xs_protocol.ACL.({owner = domid; other = NONE; acl = []}) in
        let request_path = Printf.sprintf "%s/%d" request_path 0 in
        t.Xst.mkdirperms request_path perms;
        t.Xst.write (request_path ^ "/frontend") frontend_path;
      );
    ()

  let hard_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Vfs.hard_shutdown %s" (string_of_device x);
    ()

  let clean_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Vfs.clean_shutdown %s" (string_of_device x);
    ()
end


module Vfb = struct

  let add ~xc ~xs ?(backend_domid=0) ?(protocol=Protocol_Native) domid =
    debug "Device.Vfb.add %d" domid;

    let frontend = { domid = domid; kind = Vfb; devid = 0 } in
    let backend = { domid = backend_domid; kind = Vfb; devid = 0 } in
    let device = { backend = backend; frontend = frontend } in

    let back = [
      "frontend-id", sprintf "%u" domid;
      "online", "1";
      "state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
    ] in
    let front = [
      "backend-id", string_of_int backend_domid;
      "protocol", (string_of_protocol protocol);
      "state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
    ] in
    Generic.add_device ~xs device back front [] [];
    ()

  let hard_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Vfb.hard_shutdown %s" (string_of_device x);
    ()

  let clean_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Vfb.clean_shutdown %s" (string_of_device x);
    ()

end

module Vkbd = struct

  let add ~xc ~xs ?(backend_domid=0) ?(protocol=Protocol_Native) domid =
    debug "Device.Vkbd.add %d" domid;

    let frontend = { domid = domid; kind = Vkbd; devid = 0 } in
    let backend = { domid = backend_domid; kind = Vkbd; devid = 0 } in
    let device = { backend = backend; frontend = frontend } in

    let back = [
      "frontend-id", sprintf "%u" domid;
      "online", "1";
      "state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
    ] in
    let front = [
      "backend-id", string_of_int backend_domid;
      "protocol", (string_of_protocol protocol);
      "state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising);
    ] in
    Generic.add_device ~xs device back front [] [];
    ()

  let hard_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Vkbd.hard_shutdown %s" (string_of_device x);
    ()

  let clean_shutdown (task: Xenops_task.task_handle) ~xs (x: device) =
    debug "Device.Vkbd.clean_shutdown %s" (string_of_device x);
    ()

end

module Vusb = struct
  let exec_usb_reset_script argv =
    try
      let stdout, stderr = Forkhelpers.execute_command_get_output !Xc_resources.usb_reset_script argv in
      debug "usb_reset script %s returned stdout=%s stderr=%s" (String.concat " " argv) stdout stderr
    with err ->
      error "Failed to call usb_reset script with arguments %s" (String.concat " " argv);
      raise (Internal_error (Printf.sprintf "Call to usb reset failed: %s" (Printexc.to_string err)))

  let cleanup domid =
    try
      let cmd = ["cleanup"; "-d"; string_of_int domid] in
      exec_usb_reset_script cmd
    with err ->
      warn "Failed to clean up VM %s: %s" (string_of_int domid) (Printexc.to_string err)

  let usb_reset_attach ~hostbus ~hostport ~domid ~pid ~privileged =
    let argv =  List.concat [
        [ "attach"
        ; sprintf "%s-%s" hostbus hostport
        ; "-d"
        ; string_of_int domid
        ; "-p"
        ; string_of_int pid
        ]
      ; if privileged then ["-r"] else []
      ]
    in
    exec_usb_reset_script argv

  let usb_reset_detach ~hostbus ~hostport ~domid ~privileged =
    if not privileged then
      let argv = [ "detach"
                 ; hostbus ^ "-" ^ hostport
                 ; "-d"
                 ; string_of_int domid
                 ]
      in
      exec_usb_reset_script argv

  let qom_list ~xs ~domid =
    (*. 1. The QEMU Object Model(qom) provides a framework for registering user
         creatable types and instantiating objects from those types.
        2. Qom types can be instantiated and configured directly from the QEMU
         monitor or command-line (eg,-device, device_add).
        3. Command example:
         {"execute":"qom-list","arguments":{"path":"/machine/peripheral"}}
         result:
         {"return": [{"name": "usb1", "type": "child<usb-host>"}, {"name":"type", "type": "string"}}
         The usb1 is added.
    *)
    if Qemu.is_running ~xs domid then
      let path = "/machine/peripheral" in
      try
        qmp_send_cmd domid Qmp.(Qom_list path)
        |> ( function
            | Qmp.(Qom usbs) -> List.map (fun p -> p.Qmp.name) usbs
            | other ->
              debug "%s unexpected QMP result for domid %d Qom_list"
                __LOC__ domid;
              []
          )
      with QMP_connection_error(_, _) ->
        raise Device_not_connected
    else
      []

  let vusb_controller_plug ~xs ~domid ~driver ~driver_id =
    if (Qemu.is_running ~xs domid)
    && not (List.mem driver_id (qom_list ~xs ~domid))then
      qmp_send_cmd domid Qmp.(Device_add Device.({driver; device=USB { USB.id=driver_id; params=None }})) |> ignore

  let vusb_plug ~xs ~privileged ~domid ~id ~hostbus ~hostport ~version =
    let device_model = Profile.of_domid domid in
    if device_model = Profile.Qemu_trad then
      raise (Internal_error
               (Printf.sprintf
                  "Failed to plug VUSB %s because domain %d uses device-model profile %s."
                  id domid (Profile.string_of device_model)));
    debug "vusb_plug: plug VUSB device %s" id;
    let get_bus v =
      if String.startswith "1" v then
        "usb-bus.0"
      else begin
        (* Here plug usb controller according to the usb version*)
        let usb_controller_driver = "usb-ehci" in
        let driver_id = "ehci" in
        vusb_controller_plug ~xs ~domid ~driver:usb_controller_driver ~driver_id;
        Printf.sprintf "%s.0" driver_id;
      end
    in
    if Qemu.is_running ~xs domid then
      begin
        (* Need to reset USB device before passthrough to vm according to CP-24616.
           Also need to do deprivileged work in usb_reset script if QEMU is deprivileged.
        *)
        begin match Qemu.pid ~xs domid with
          | Some pid -> usb_reset_attach ~hostbus ~hostport ~domid ~pid ~privileged
          | _ -> raise (Internal_error (Printf.sprintf "qemu pid does not exist for vm %d" domid))
        end;
        let cmd = Qmp.(Device_add Device.(
                       { driver = "usb-host";
                         device = USB {
                           USB.id = id;
                           params = Some USB.({bus = get_bus version; hostbus; hostport})
                         }
                       }
                    ))
        in qmp_send_cmd domid cmd |> ignore
      end

  let vusb_unplug ~xs ~privileged ~domid ~id ~hostbus ~hostport=
    debug "vusb_unplug: unplug VUSB device %s" id;
    finally
      (fun () ->
         if Qemu.is_running ~xs domid then
           try
             qmp_send_cmd domid Qmp.(Device_del id) |> ignore
           with QMP_connection_error(_, _) ->
             raise Device_not_connected
      )
      (fun () ->
         usb_reset_detach ~hostbus ~hostport ~domid ~privileged
      )

end

let can_surprise_remove ~xs (x: device) = Generic.can_surprise_remove ~xs x

(** Dm_Common contains the private Dm functions that are common between the qemu profile backends. *)
module Dm_Common = struct

  (* An example one:
     /usr/lib/xen/bin/qemu-dm -d 39 -m 256 -boot cd -serial pty -usb -usbdevice tablet -domain-name bee94ac1-8f97-42e0-bf77-5cb7a6b664ee -net nic,vlan=1,macaddr=00:16:3E:76:CE:44,model=rtl8139 -net tap,vlan=1,bridge=xenbr0 -vnc 39 -k en-us -vnclisten 127.0.0.1
  *)

  let max_emulated_nics = 8 (** Should be <= the hardcoded maximum number of emulated NICs *)

  type usb_opt =
    | Enabled of (string * int) list
    | Disabled

  (* How the display appears to the guest *)
  type disp_intf_opt =
    | Std_vga
    | Cirrus
    | Vgpu of Xenops_interface.Vgpu.t list
    | GVT_d

  (* Display output / keyboard input *)
  type disp_opt =
    | NONE
    | VNC of disp_intf_opt * string option * bool * int * string option (* IP address, auto-allocate, port if previous false, keymap *)
    | SDL of disp_intf_opt * string (* X11 display *)

  type media = Disk | Cdrom
  let string_of_media = function Disk -> "disk" | Cdrom -> "cdrom"

  type info = {
    memory: int64;
    boot: string;
    serial: string option;
    monitor: string option;
    vcpus: int; (* vcpus max *)
    vcpus_current: int;
    usb: usb_opt;
    parallel: string option;
    nics: (string * string * int) list;
    disks: (int * string * media) list;
    acpi: bool;
    disp: disp_opt;
    pci_emulations: string list;
    pci_passthrough: bool;
    video_mib : int;
    extras: (string * string option) list;
  }

  type qemu_args =
    { argv:     string list                     (** command line args *)
    ; fd_map:   (string * Unix.file_descr) list (** open files *)
    }

  let vnc_socket_path = (sprintf "%s/vnc-%d") Device_common.var_run_xen_path

  let get_vnc_port ~xs domid ~f =
    match Qemu.is_running ~xs domid with
    | true  -> f ()
    | false -> None

  let get_tc_port ~xs domid =
    if not (Qemu.is_running ~xs domid)
    then None
    else (try Some(int_of_string (xs.Xs.read (Generic.tc_port_path domid))) with _ -> None)


  let signal (task: Xenops_task.task_handle) ~xs ~qemu_domid ~domid ?wait_for ?param cmd =
    let cmdpath = device_model_path ~qemu_domid domid in
    Xs.transaction xs (fun t ->
        t.Xst.write (cmdpath ^ "/command") cmd;
        match param with
        | None -> ()
        | Some param -> t.Xst.write (cmdpath ^ "/parameter") param
      );
    match wait_for with
    | Some state ->
      let pw = cmdpath ^ "/state" in
      (* MTC: The default timeout for this operation was 20mins, which is
       * way too long for our software to recover successfully.
       * Talk to Citrix about this
      *)
      let cancel = Qemu (qemu_domid, domid) in
      let (_: bool) = cancellable_watch cancel [ Watch.value_to_become pw state ] [] task ~xs ~timeout:30. () in
      ()
    | None -> ()

  let get_state ~xs ~qemu_domid domid =
    let cmdpath = device_model_path ~qemu_domid domid in
    let statepath = cmdpath ^ "/state" in
    try Some (xs.Xs.read statepath)
    with _ -> None

  let cmdline_of_disp ?domid info =
    let vga_type_opts x =
      let open Xenops_interface.Vgpu in
      match x with
      | Vgpu [{implementation = Nvidia _}] -> ["-vgpu"]
      | Vgpu [{implementation = GVT_g gvt_g}] ->
        let base_opts = [
          "-xengt";
          "-vgt_low_gm_sz"; Int64.to_string gvt_g.low_gm_sz;
          "-vgt_high_gm_sz"; Int64.to_string gvt_g.high_gm_sz;
          "-vgt_fence_sz"; Int64.to_string gvt_g.fence_sz;
        ]
        and config_file_opt = match gvt_g.monitor_config_file with
          | Some path -> ["-vgt_monitor_config_file"; path]
          | None -> []
        and priv_opt = ["-priv"] in
        List.flatten [base_opts; config_file_opt; priv_opt]
      | Vgpu [{implementation = MxGPU mxgpu}] -> []
      | Vgpu _ -> failwith "Unsupported vGPU configuration"
      | Std_vga -> ["-std-vga"]
      | Cirrus -> []
      | GVT_d -> ["-std-vga"] (* relies on pci-passthrough *)
    in
    let videoram_opt = ["-videoram"; string_of_int info.video_mib] in
    let vnc_opts_of ip_addr_opt auto port keymap ~domid =
      let ip_addr = Opt.default "127.0.0.1" ip_addr_opt in
      let unused_opt, vnc_arg = match domid with
        | None when auto -> ["-vncunused"], Printf.sprintf "%s:1"  ip_addr
        | None           -> [], Printf.sprintf "%s:%d" ip_addr port
          (*
              Disable lock-key-sync
              #  lock-key-sync expects vnclient to send different keysym for
              #  alphabet keys (different for lowercase and uppercase). XC
              #  can't do it at the moment, so disable lock-key-sync
          *)
        | Some domid     -> [], Printf.sprintf "%s,lock-key-sync=off" (Socket.Unix.path (vnc_socket_path domid))
      in
      let vnc_opt = ["-vnc"; vnc_arg] in
      let keymap_opt = match keymap with Some k -> ["-k"; k] | None -> [] in
      List.flatten [unused_opt; vnc_opt; keymap_opt]
    in
    let disp_options, wait_for_port =
      match info.disp with
      | NONE -> 
        ([], false)
      | SDL (opts, x11name) ->
        ([], false)
      | VNC (disp_intf, ip_addr_opt, auto, port, keymap) ->
        let vga_type_opts = vga_type_opts disp_intf in
        let vnc_opts = vnc_opts_of ip_addr_opt auto port keymap ~domid in
        (vga_type_opts @ videoram_opt @ vnc_opts), true
    in
    disp_options, wait_for_port

  let qemu_args ~xs ~dm info restore ?(domid_for_vnc=false) domid =
    let disks' = List.map (fun (index, file, media) -> [
          "-drive"; sprintf "file=%s,if=ide,index=%d,media=%s" file index (string_of_media media)
        ]) info.disks in

    let restorefile = sprintf qemu_restore_path domid in
    let disp_options, wait_for_port = if domid_for_vnc
      then cmdline_of_disp info ~domid
      else cmdline_of_disp info
    in
    let argv =
      List.concat
        [ disp_options
        ; List.concat disks'
        ; ( info.acpi |> function false -> [] | true -> [ "-acpi" ])
        ; ( restore   |> function false -> [] | true -> [ "-loadvm"; restorefile ])
        ; ( info.pci_emulations
            |> List.map (fun pci -> ["-pciemulation"; pci])
            |> List.concat
          )
        ; ( info.pci_passthrough |> function false -> [] | true -> ["-priv"])
        ; ( (List.rev info.extras)
            |> List.map (function (k,None) -> ["-"^k] | (k,Some v) -> ["-"^k; v])
            |> List.concat
          )
        ; ( info.monitor  |> function None -> [] | Some x -> [ "-monitor";  x])
        ]
    in
    { argv   = argv
    ; fd_map = []
    }

  let vnconly_cmdline ~info ?(extras=[]) domid =
    let disp_options, _ = cmdline_of_disp info in
    [
      "-d"; string_of_int domid;
      "-M"; "xenpv"; ] (* the stubdom is a PV guest *)
    @ disp_options
    @ (List.fold_left (fun l (k, v) -> ("-" ^ k) :: (match v with None -> l | Some v -> v :: l)) [] extras)

  let vgpu_args_of_nvidia domid vcpus (vgpu:Xenops_interface.Vgpu.nvidia) pci restore =
    let open Xenops_interface.Vgpu in
    let suspend_file = sprintf demu_save_path domid in
    let base_args = [
      "--domain=" ^ (string_of_int domid);
      "--vcpus=" ^ (string_of_int vcpus);
      "--gpu=" ^ (Xenops_interface.Pci.string_of_address pci);
      "--config=" ^ vgpu.config_file;
      "--suspend=" ^ suspend_file;
    ] in
    let fd_arg = if restore then ["--resume"] else [] in
    List.concat [base_args; fd_arg]

  let write_vgpu_data ~xs domid devid keys =
    let path = xenops_vgpu_path domid devid in
    xs.Xs.writev path keys

  let mxgpu_device_in_use ~xs physical_function =
    (* Check if there is a /xenops/domain/<x>/device/vgpu/<y>/pf xenstore node
       	 * that is equal to the given physical_function. *)
    let root = Device_common.xenops_domain_path in
    try
      (* NB: The response size of this directory call may exceed the default payload
         		 * size limit. However, we have an exception that allows oversized packets. *)
      xs.Xs.directory root
      |> List.map (fun domid ->
          let path = Printf.sprintf "%s/%s/device/vgpu" root domid in
          try
            List.map (fun x -> path ^ "/" ^ x) (xs.Xs.directory path)
          with Xs_protocol.Enoent _ -> []
        )
      |> List.concat
      |> List.exists (fun vgpu ->
          try
            let path = Printf.sprintf "%s/pf" vgpu in
            let pf = xs.Xs.read path in
            pf = physical_function
          with Xs_protocol.Enoent _ -> false
        )
    with Xs_protocol.Enoent _ -> false

  let call_gimtool args =
    try
      info "Initialising MxGPU PF: %s %s" !Xc_resources.gimtool (String.concat " " args);
      ignore (Forkhelpers.execute_command_get_output !Xc_resources.gimtool args)
    with _ ->
      error "Failed to initialise MxGPU PF (%s %s)" !Xc_resources.gimtool (String.concat " " args);
      failwith "Call to gimtool failed"

  let configure_gim ~xs physical_function vgpus_per_gpu framebufferbytes =
    let pf = Xenops_interface.Pci.string_of_address physical_function in
    (* gimtool must (only) be called when no VM is using the PF yet *)
    if not (mxgpu_device_in_use ~xs pf) then
      let vgpus = Int64.to_string vgpus_per_gpu in
      let fb = Int64.to_string (Int64.div framebufferbytes Memory.bytes_per_mib) in
      call_gimtool ["--bdf"; pf; "--num_vfs"; vgpus; "--vf_fb_size"; fb]
    else
      info "MxGPU PF %s already initialised" pf

  let prepend_wrapper_args domid args =
    (string_of_int domid) :: "--syslog" :: args

  (* Forks a daemon and then returns the pid. *)
  let start_daemon ~path ~args ~name ~domid ?(fds=[]) _ =
    debug "Starting daemon: %s with args [%s]" path (String.concat "; " args);
    let syslog_key = (Printf.sprintf "%s-%d" name domid) in
    let syslog_stdout = Forkhelpers.Syslog_WithKey syslog_key in
    let redirect_stderr_to_stdout = true in
    let pid = Forkhelpers.safe_close_and_exec None None None fds ~syslog_stdout ~redirect_stderr_to_stdout path args in
    debug
      "%s: should be running in the background (stdout -> syslog); (fd,pid) = %s"
      name (Forkhelpers.string_of_pidty pid);
    debug "Daemon started: %s" syslog_key;
    pid

  (* Waits for a daemon to signal startup by writing to a xenstore path
   * (optionally with a given value) If this doesn't happen in the timeout then
   * an exception is raised *)
  let wait_path ~pid ~task ~name ~domid ~xs ~ready_path ?ready_val ~timeout
      ~cancel _ =
    let syslog_key = Printf.sprintf "%s-%d" name domid in
      let finished = ref false in
      let watch = Watch.value_to_appear ready_path |> Watch.map (fun _ -> ()) in
    let timeout_ns = Int64.of_float (timeout *. Mtime.s_to_ns) in
    let target =
      match Mtime.add_span (Mtime_clock.now ()) (Mtime.Span.of_uint64_ns timeout_ns) with
      | None -> raise (Ioemu_failed (name, "Timeout overflow"))
      | Some x -> x in
    while Mtime.is_earlier (Mtime_clock.now ()) ~than:target && not !finished do
        Xenops_task.check_cancelling task;
        try
          let (_: bool) = cancellable_watch cancel [ watch ] [] task ~xs ~timeout () in
          let state = try xs.Xs.read ready_path with _ -> "" in
          match ready_val with
        | Some value when value = state -> finished := true
        | Some _ -> raise (Ioemu_failed (name, (Printf.sprintf "Daemon state not running (%s)" state)))
          | None -> finished := true
        with Watch.Timeout _ ->
          begin match Forkhelpers.waitpid_nohang pid with
            | 0, Unix.WEXITED 0 -> () (* still running => keep waiting *)
            | _, Unix.WEXITED n ->
              error "%s: unexpected exit with code: %d" name n;
              raise (Ioemu_failed (name, "Daemon exited unexpectedly"))
            | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) ->
              error "%s: unexpected signal: %d" name n;
              raise (Ioemu_failed (name, "Daemon exited unexpectedly"))
          end
      done;
      if not !finished then
      raise (Ioemu_failed (name, "Timeout reached while starting daemon"));
    debug "Daemon initialised: %s" syslog_key

  let gimtool_m = Mutex.create ()

  let resume (task: Xenops_task.task_handle) ~xs ~qemu_domid domid =
    signal task ~xs ~qemu_domid ~domid "continue" ~wait_for:"running"

  (* Called by every domain destroy, even non-HVM *)
  let stop ~xs ~qemu_domid domid  =
    let qemu_pid_path = Qemu.pid_path domid
    in
    let stop_qemu () = (match (Qemu.pid ~xs domid) with
        | None -> () (* nothing to do *)
        | Some qemu_pid ->
          debug "qemu-dm: stopping qemu-dm with SIGTERM (domid = %d)" domid;
          let open Generic in
          best_effort "signalling that qemu is ending as expected, mask further signals"
            (fun () -> Qemu.SignalMask.set Qemu.signal_mask domid);
          best_effort "killing qemu-dm"
            (fun () -> really_kill qemu_pid);
          best_effort "removing qemu-pid from xenstore"
            (fun () -> xs.Xs.rm qemu_pid_path);
          best_effort "unmasking signals, qemu-pid is already gone from xenstore"
            (fun () -> Qemu.SignalMask.unset Qemu.signal_mask domid);
          best_effort "removing device model path from xenstore"
            (fun () -> xs.Xs.rm (device_model_path ~qemu_domid domid)))
    in
    let stop_vgpu () = match (Vgpu.pid ~xs domid) with
      | None -> ()
      | Some vgpu_pid ->
        debug "vgpu: stopping vgpu with SIGTERM (domid = %d pid = %d)" domid vgpu_pid;
        let open Generic in
        best_effort "killing vgpu"
          (fun () -> really_kill vgpu_pid)
    in
    stop_vgpu ();
    stop_qemu ()

end (* End of module Dm_Common *)


(** Implementation of the qemu profile backends *)
module Backend = struct

  (** Common signature for all the profile backends *)
  module type Intf = sig

    (** Vbd functions that use the dispatcher to choose between different profile backends *)
    module Vbd: sig
      val qemu_media_change : xs:Xenstore.Xs.xsh -> device -> string -> string -> unit
    end

    (** Vcpu functions that use the dispatcher to choose between different profile backends *)
    module Vcpu: sig
      val add : xs:Xenstore.Xs.xsh -> devid:int -> int -> bool -> unit
      val set : xs:Xenstore.Xs.xsh -> devid:int -> int -> bool -> unit
      val del : xs:Xenstore.Xs.xsh -> devid:int -> int -> unit
      val status : xs:Xenstore.Xs.xsh -> devid:int -> int -> bool
    end

    (** Dm functions that use the dispatcher to choose between different profile backends *)
    module Dm: sig

      module Event: sig
        val init : unit -> unit
      end

      (** [get_vnc_port xenstore domid] returns the dom0 tcp port in which the vnc server for [domid] can be found *)
      val get_vnc_port : xs:Xenstore.Xs.xsh -> int -> Socket.t option

      (** [suspend task xenstore qemu_domid xc] suspends a domain *)
      val suspend: Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> qemu_domid:int -> Xenctrl.domid -> unit

      (** [init_daemon task path args name domid xenstore ready_path ready_val timeout cancel] returns a forkhelper pid after starting the qemu daemon in dom0 *)
      val init_daemon: task:Xenops_task.task_handle -> path:string -> args:string list -> name:string -> domid:int -> xs:Xenstore.Xs.xsh -> ready_path:Watch.path -> ?ready_val:string -> timeout:float -> cancel:Cancel_utils.key -> ?fds:(string * Unix.file_descr) list -> 'a -> Forkhelpers.pidty

      (** [stop xenstore qemu_domid domid] stops a domain *)
      val stop: xs:Xenstore.Xs.xsh -> qemu_domid:int -> int -> unit

      (** [with_dirty_log domid f] executes f in a context where the dirty log is enabled *)
      val with_dirty_log: int -> f:(unit -> 'a) -> 'a

      (** [cmdline_of_info xenstore info restore domid] creates the command line arguments to pass to the qemu wrapper script *)
      val qemu_args: xs:Xenstore.Xs.xsh -> dm:Profile.t -> Dm_Common.info
        -> bool -> int -> Dm_Common.qemu_args

      (** [after_suspend_image xs qemu_domid domid] hook to execute actions after the suspend image has been created *)
      val after_suspend_image: xs:Xenstore.Xs.xsh -> qemu_domid:int -> int -> unit
    end
  end

  (** Implementation of the backend common signature for the qemu-trad backend *)
  module Qemu_trad : Intf = struct

    (** Implementation of the Vbd functions that use the dispatcher for the qemu-trad backend *)
    module Vbd = struct
      let qemu_media_change = Vbd_Common.qemu_media_change
    end

    (** Implementation of the Vcpu functions that use the dispatcher for the qemu-trad backend *)
    module Vcpu = struct
      let add = Vcpu_Common.add
      let set = Vcpu_Common.set
      let del = Vcpu_Common.del
      let status = Vcpu_Common.status
    end

    (** Implementation of the Dm functions that use the dispatcher for the qemu-trad backend *)
    module Dm = struct

      module Event = struct
        let init () = ()
      end

      let get_vnc_port ~xs domid =
        Dm_Common.get_vnc_port ~xs domid ~f:(fun () ->
            (try Some(Socket.Port (int_of_string (xs.Xs.read (Generic.vnc_port_path domid)))) with _ -> None)
          )

      let suspend (task: Xenops_task.task_handle) ~xs ~qemu_domid domid =
        Dm_Common.signal task ~xs ~qemu_domid ~domid "save" ~wait_for:"paused"

      let init_daemon ~task ~path ~args ~name ~domid ~xs ~ready_path ?ready_val
          ~timeout ~cancel ?(fds=[]) _ =
        let pid = Dm_Common.start_daemon ~path ~args ~name ~domid ~fds () in
        Dm_Common.wait_path ~pid ~task ~name ~domid ~xs ~ready_path ?ready_val
          ~timeout ~cancel ();
        pid

      let stop ~xs ~qemu_domid domid =
        let pid = Qemu.pid ~xs domid in
        Dm_Common.stop ~xs ~qemu_domid domid;
        match pid with
        | None -> () (* nothing to do *)
        | Some qemu_pid ->
          Generic.best_effort "removing core files from /var/xen/qemu"
            (fun () -> Unix.rmdir ("/var/xen/qemu/"^(string_of_int qemu_pid)))

      let with_dirty_log domid ~f = f()

      let qemu_args ~xs ~dm info restore domid =
        let common = Dm_Common.qemu_args ~xs ~dm info restore domid in

        let usb =
          match info.Dm_Common.usb with
          | Dm_Common.Disabled -> []
          | Dm_Common.Enabled devices ->
            let devs = devices |> List.map (fun (x,_) -> ["-usbdevice"; x]) |> List.concat in
            "-usb" :: devs
        in

        let misc = List.concat
            [ [ "-d"; string_of_int domid
              ; "-m"; Int64.to_string (Int64.div info.Dm_Common.memory 1024L)
              ; "-boot"; info.Dm_Common.boot
              ]
            ; usb
            ; [ "-vcpus"; string_of_int info.Dm_Common.vcpus]
            ; (info.Dm_Common.serial |> function None -> [] | Some x -> [ "-serial"; x ])
            ; (info.Dm_Common.parallel |> function None -> [] | Some x -> [ "-parallel"; x])
            ] in

        (* Sort the VIF devices by devid *)
        let nics = List.stable_sort (fun (_,_,a) (_,_,b) -> compare a b) info.Dm_Common.nics in
        if List.length nics > Dm_Common.max_emulated_nics then debug "Limiting the number of emulated NICs to %d" Dm_Common.max_emulated_nics;
        (* Take the first 'max_emulated_nics' elements from the list. *)
        let nics = Xapi_stdext_std.Listext.List.take Dm_Common.max_emulated_nics nics in

        (* qemu need a different id for every vlan, or things get very bad *)
        let nics' =
          if nics <> [] then
            List.mapi (fun vlan_id (mac, bridge, devid) ->
                [
                  "-net"; sprintf "nic,vlan=%d,macaddr=%s,model=rtl8139" vlan_id mac;
                  "-net"; sprintf "tap,vlan=%d,bridge=%s,ifname=tap%d.%d" vlan_id bridge domid devid
                ]
              ) nics
          else [["-net"; "none"]]
        in
        Dm_Common.{ common with argv = common.argv @ misc @ (List.concat nics') }

      let after_suspend_image ~xs ~qemu_domid domid = ()

    end (* Backend.Qemu_trad.Dm *)
  end (* Backend.Qemu_trad *)

  (** Implementation of the backend common signature for the qemu-upstream-compat backend *)
  module Qemu_upstream_compat : Intf  = struct

    (** Implementation of the Vbd functions that use the dispatcher for the qemu-upstream-compat backend *)
    module Vbd = struct

      let qemu_media_change ~xs device _type params =
        Vbd_Common.qemu_media_change ~xs device _type params;
        let cd    = "ide1-cd1" in
        let domid = device.frontend.domid in
        if params = "" then
          qmp_send_cmd domid Qmp.(Eject(cd, Some true)) |> ignore
        else
          try
            let as_msg cmd = Qmp.(Success(Some __LOC__, cmd)) in
            let fd_cd      = Unix.openfile params [ Unix.O_RDONLY ] 0o640 in
            finally
              (fun () ->
                 let cmd     = Qmp.(Add_fd None) in
                 let fd_info = match qmp_send_cmd ~send_fd:fd_cd domid cmd with
                   | Qmp.Fd_info x -> x
                   | other ->
                     raise (Internal_error
                              (sprintf
                                 "Unexpected result for QMP command: %s"
                                 Qmp.(other |> as_msg |> string_of_message))) in
                 finally
                   (fun () ->
                      let path = sprintf "/dev/fdset/%d" fd_info.Qmp.fdset_id in
                      let cmd  = Qmp.(Blockdev_change_medium (cd, path)) in
                      qmp_send_cmd domid cmd |> ignore)
                   (fun () ->
                      let cmd = Qmp.(Remove_fd fd_info.fdset_id) in
                      qmp_send_cmd domid cmd |> ignore))
              (fun () ->
                 Unix.close fd_cd)
          with
          | Unix.Unix_error(Unix.ECONNREFUSED, "connect", p) -> raise(Internal_error (Printf.sprintf "Failed to connnect QMP socket: %s" p))
          | Unix.Unix_error(Unix.ENOENT, "open", p) -> raise(Internal_error (Printf.sprintf "Failed to open CD Image: %s" p))
          | Internal_error(_) as e -> raise e
          | e -> raise(Internal_error (Printf.sprintf "Get unexpected error trying to change CD: %s" (Printexc.to_string e)))
    end (* Backend.Qemu_upstream_compat.Vbd *)

    (** Implementation of the Vcpu functions that use the dispatcher for the qemu-upstream-compat backend *)
    module Vcpu = struct
      let add = Vcpu_Common.add
      let del = Vcpu_Common.del
      let status = Vcpu_Common.status

      (* hot(un)plug vcpu using QMP, keeping backwards-compatible xenstored mechanism *)
      let set ~xs ~devid domid online =
        Vcpu_Common.set ~xs ~devid domid online;
        match online with
        | true  -> ( (* hotplug *)
          let socket_id, core_id, thread_id = (devid, 0, 0) in
          let id = Qmp.Device.VCPU.id_of ~socket_id ~core_id ~thread_id in
          qmp_send_cmd domid Qmp.(Device_add Device.(
            { driver = VCPU.Driver.(string_of QEMU32_I386_CPU);
              device = VCPU { VCPU.id; socket_id; core_id; thread_id }
            })) |> ignore
          )
        | false -> ( (* hotunplug *)
          let err msg = raise (Internal_error msg) in
          let qom_path = qmp_send_cmd domid Qmp.Query_hotpluggable_cpus
          |> function
          | Qmp.Hotpluggable_cpus x -> ( x
            |> List.filter (fun Qmp.Device.VCPU.{ qom_path; props={ socket_id } } -> socket_id = devid)
            |> function
            | [] -> err (sprintf "No QEMU CPU found with devid %d" devid)
            | Qmp.Device.VCPU.{ qom_path = None   } :: _ -> err (sprintf "No qom_path for QEMU CPU devid %d" devid)
            | Qmp.Device.VCPU.{ qom_path = Some p } :: _ -> p
            )
          | other ->
            let as_msg cmd = Qmp.(Success(Some __LOC__, cmd)) in
            err (sprintf "Unexpected result for QMP command: %s" Qmp.(other |> as_msg |> string_of_message))
          in
          qom_path |> fun id -> qmp_send_cmd domid Qmp.(Device_del id) |> ignore
          )

    end

    (** Implementation of the Dm functions that use the dispatcher for the qemu-upstream-compat backend *)
    module Dm = struct

      (** Handler for the QMP events in upstream qemu *)
      module QMP_Event = struct
        open Qmp

        let (>>=) m f = match m with | Some x -> f x | None -> ()
        let (>>|) m f = match m with | Some _ -> () | None -> f ()

        (** Efficient lookup table between file descriptors, channels and domain ids *)
        module Lookup = struct
          let ftod, dtoc = Hashtbl.create 16, Hashtbl.create 16
          let add c domid =
            Hashtbl.replace ftod (Qmp_protocol.to_fd c) domid;
            Hashtbl.replace dtoc domid c
          let remove c domid =
            Hashtbl.remove ftod (Qmp_protocol.to_fd c);
            Hashtbl.remove dtoc domid
          let domid_of fd = try Some (Hashtbl.find ftod fd) with Not_found -> None
          let channel_of domid = try Some (Hashtbl.find dtoc domid) with Not_found -> None
        end

        (** File-descriptor event monitor implementation for the epoll library *)
        module Monitor = struct
          module Epoll = Core.Linux_ext.Epoll
          module Flags = Core.Linux_ext.Epoll.Flags
          let create () = (Core.Std.Or_error.ok_exn Epoll.create) ~num_file_descrs:1001 ~max_ready_events:1
          let add m fd = Epoll.set m fd Flags.in_
          let remove m fd = Epoll.remove m fd
          let wait m = Epoll.wait m ~timeout:`Never
          let with_event m fn = function
            | `Ok -> Epoll.iter_ready m ~f:(fun fd flags -> fn fd (flags = Flags.in_))
            | `Timeout -> debug "Shouldn't receive epoll timeout event in qmp_event_thread"
        end
        let m = Monitor.create ()

        let monitor_path domid = qmp_event_path domid
        let debug_exn msg e = debug "%s: %s" msg (Printexc.to_string e)

        let remove domid =
          Lookup.channel_of domid >>= fun c ->
          try 
            finally
              (fun () ->
                 Lookup.remove c domid;
                 Monitor.remove m (Qmp_protocol.to_fd c);
                 debug "Removed QMP Event fd for domain %d" domid)
              (fun () -> Qmp_protocol.close c)
          with e -> debug_exn (Printf.sprintf "Got exception trying to remove QMP on domain-%d" domid) e

        let add domid =
          try
            Lookup.channel_of domid >>| fun () ->
            let c = Qmp_protocol.connect (monitor_path domid) in
            Qmp_protocol.negotiate c;
            Qmp_protocol.write c (Command (None, Cont));
            Lookup.add c domid;
            Monitor.add m (Qmp_protocol.to_fd c);
            debug "Added QMP Event fd for domain %d" domid
          with e ->
            debug_exn (Printf.sprintf "QMP domain-%d: negotiation failed: removing socket" domid) e;
            remove domid

        let qmp_event_handle domid qmp_event =
          (* This function will be extended to handle qmp events *)
          debug "Got QMP event, domain-%d: %s" domid qmp_event.event;

          let rtc_change timeoffset =
            with_xs (fun xs ->
                let timeoffset_key = sprintf "/vm/%s/rtc/timeoffset" (Uuidm.to_string (Xenops_helpers.uuid_of_domid ~xs domid)) in
                try
                  let rtc = xs.Xs.read timeoffset_key in
                  xs.Xs.write timeoffset_key Int64.(add timeoffset (of_string rtc) |> to_string)
                with e -> error "Failed to process RTC_CHANGE for domain %d: %s" domid (Printexc.to_string e)
              )
          in

          let xen_platform_pv_driver_info pv_info =
            with_xs (fun xs ->
                let is_hvm_linux { product_num; build_num } =
                  let _XEN_IOPORT_LINUX_PRODNUM = 3 in (* from Linux include/xen/platform_pci.h *)
                  (product_num = _XEN_IOPORT_LINUX_PRODNUM) && (build_num <= 0xff)
                in
                if is_hvm_linux pv_info then
                  begin
                    let write_local_domain prefix x = xs.Xs.write (Printf.sprintf "/local/domain/%d/%s%s" domid prefix x) "1" in
                    List.iter (write_local_domain "control/feature-") ["suspend"; "poweroff"; "reboot"; "vcpu-hotplug"];
                    List.iter (write_local_domain "data/") ["updated"]
                  end
              )
          in

          qmp_event.data |> function
          | Some (RTC_CHANGE timeoffset)         -> rtc_change timeoffset
          | Some (XEN_PLATFORM_PV_DRIVER_INFO x) -> xen_platform_pv_driver_info x
          | _ -> () (* unhandled QMP events *)

        let qmp_event_thread () =
          debug "Starting QMP_Event thread";
          (* Add the existing qmp sockets first *)
          Sys.readdir var_run_xen_path
          |> Array.to_list
          |> List.iter (fun x -> try Scanf.sscanf x "qmp-event-%d" add with _ -> ());

          while true do
            try
              Monitor.wait m |> Monitor.with_event m (fun fd is_flag_in ->
                  Lookup.domid_of fd >>= fun domid ->
                  if is_flag_in then
                    Lookup.channel_of domid >>= fun c ->
                    try
                      match Qmp_protocol.read c with
                      | Event e -> qmp_event_handle domid e
                      | msg -> debug "Got non-event message, domain-%d: %s" domid (string_of_message msg)
                    with End_of_file ->
                      debug "domain-%d: end of file, close QMP socket" domid;
                      remove domid
                       | e ->
                         debug_exn (Printf.sprintf "domain-%d: close QMP socket" domid) e;
                         remove domid
                  else begin
                    debug "EPOLL error on domain-%d, close QMP socket" domid;
                    remove domid
                  end
                )
            with e ->
              debug_exn "Exception in qmp_event_thread: %s" e;
          done

      end (* Qemu_upstream_compat.Dm.QMP_Event *)

      module Event = struct
        let init () = ignore(Thread.create QMP_Event.qmp_event_thread ())
      end

      let get_vnc_port ~xs domid =
        Dm_Common.get_vnc_port ~xs domid ~f:(fun () ->
            Some (Socket.Unix (Dm_Common.vnc_socket_path domid))
          )

      let suspend (task: Xenops_task.task_handle) ~xs ~qemu_domid domid =
        let as_msg cmd = Qmp.(Success(Some __LOC__, cmd)) in
        let perms      = [ Unix.O_WRONLY; Unix.O_CREAT ] in
        let save_file  = sprintf qemu_save_path domid in
        let save_fd    = Unix.openfile save_file perms 0o660 in
        finally
          (fun () ->
             let fd = qmp_send_cmd ~send_fd:save_fd domid Qmp.(Add_fd None)
                      |> function
                      | Qmp.(Fd_info fd) -> fd
                      | other ->
                        raise (Internal_error (sprintf
                                                 "Unexpected result for QMP command: %s"
                                                 Qmp.(other |> as_msg |> string_of_message)))
             in
             finally
               (fun () ->
                  let path = sprintf "/dev/fdset/%d" fd.Qmp.fdset_id in
                  qmp_send_cmd domid Qmp.Stop |> ignore;
                  qmp_send_cmd domid Qmp.(Xen_save_devices_state path) |> ignore)
               (fun () ->
                  qmp_send_cmd domid Qmp.(Remove_fd fd.fdset_id) |> ignore))
          (fun () ->
             Unix.close save_fd)


      (* Wait for QEMU's event socket to appear. *)
      let wait_event_socket ~task ~name ~domid ~timeout =
        let finished = ref false in
        let timeout_ns = Int64.of_float (timeout *. Mtime.s_to_ns) in
        let target =
          match Mtime.add_span (Mtime_clock.now ()) (Mtime.Span.of_uint64_ns timeout_ns) with
          | None -> raise (Ioemu_failed (name, "Timeout overflow"))
          | Some x -> x in
        while Mtime.is_earlier (Mtime_clock.now ()) ~than:target && not !finished do
          Xenops_task.check_cancelling task;
          if Sys.file_exists (qmp_event_path domid) then
            finished := true
          else
            ignore (Unix.select [] [] [] 0.05)
        done;
        if not !finished then
          raise (Ioemu_failed (name, "Timeout reached while starting daemon"))

      let init_daemon ~task ~path ~args ~name ~domid ~xs ~ready_path ?ready_val ~timeout ~cancel ?(fds=[]) _ =
        let pid = Dm_Common.start_daemon ~path ~args ~name ~domid ~fds () in
        wait_event_socket ~task ~name ~domid ~timeout;
        QMP_Event.add domid;
        pid

      let stop ~xs ~qemu_domid domid =
        Dm_Common.stop ~xs ~qemu_domid domid;
        QMP_Event.remove domid;
        xs.Xs.rm (sprintf "/libxl/%d" domid);
        let rm path =
          let msg = Printf.sprintf "removing %s" path in
          Generic.best_effort msg (fun () -> Socket.Unix.rm path) in
        [ (* clean up QEMU socket leftover files *)
          Dm_Common.vnc_socket_path domid;
          (qmp_event_path domid);
          (qmp_libxl_path domid);
        ] |> List.iter rm;
        Vusb.cleanup domid; (* unmounts devices in /var/xen/qemu/root-* *)
        let path = Printf.sprintf "/var/xen/qemu/root-%d" domid in
        Generic.best_effort (Printf.sprintf "removing %s" path)
          (fun () -> Xenops_utils.FileFS.rmtree path)

      let with_dirty_log domid ~f =
        finally
          (fun() ->
             qmp_send_cmd domid Qmp.(Xen_set_global_dirty_log true) |> ignore;
             f()
          )
          (fun() ->
             qmp_send_cmd domid Qmp.(Xen_set_global_dirty_log false) |> ignore
          )

      let tap_open ifname =
        let uuid = Uuidm.to_string (Uuidm.create `V4) in
        let fd   = Tuntap.tap_open ifname in
        (uuid, fd)

      let qemu_args ~xs ~dm info restore domid =
        let common = Dm_Common.qemu_args ~xs ~dm info restore domid ~domid_for_vnc:true in

        let usb =
          match info.Dm_Common.usb with
          | Dm_Common.Disabled -> []
          | Dm_Common.Enabled devices ->
            let devs = devices |>
                       List.map (fun (x,y) -> ["-device"; sprintf "usb-%s,port=%d" x y]) |> List.concat in
            "-usb" :: devs
        in

        let serial_device =
          try
            let xs_path = xs.Xs.read "/local/logconsole/@" in
            let file =
              if Stdext.Xstringext.String.has_substr xs_path "%d" then
                Stdext.Xstringext.String.replace "%d" (string_of_int domid) xs_path
              else
                xs_path
            in Some ("file:" ^ file)
          with _ -> info.Dm_Common.serial
        in

        let mult xs ys =
          List.map (fun x -> List.map (fun y -> x^"."^y) ys) xs |>
          List.concat in
        let global =
          mult
            ["piix3-ide-xen"; "piix3-usb-uhci"; "rtl8139"]
            ["subvendor_id=0x5853"; "subsystem_id=0x0001"]
        in

        let qmp = ["libxl"; "event"] |>
                  List.map (fun x -> ["-qmp"; sprintf "unix:/var/run/xen/qmp-%s-%d,server,nowait" x domid]) |>
                  List.concat in

        let has_platform_device =
          try
            int_of_string (xs.Xs.read (sprintf "/local/domain/%d/vm-data/disable_pf" domid)) <> 1
          with _ -> true
        in
        let xen_platform_device =
          if has_platform_device then begin
            let device_id =
              try xs.Xs.read (sprintf "/local/domain/%d/platform/device_id" domid)
              with _ -> "0001"
            in
            [ "-device"; String.concat "," [
                  "xen-platform"
                ; "addr=3"
                ; sprintf "device-id=0x%s" device_id
                ; "revision=0x2"
                ; "class-id=0x0100"
                ; "subvendor_id=0x5853"
                ; sprintf"subsystem_id=0x%s" device_id
                ]
            ]
          end else []
        in

        let pv_device addr =
          try
            let path = sprintf "/local/domain/%d/control/has-vendor-device" domid in
            let has_device = xs.Xs.read path in
            if int_of_string has_device = 1 then
              ["-device"
              ; sprintf "xen-pvdevice,device-id=0xc000,addr=%x" addr
              ]
            else []
          with _ -> []
        in

        let misc = List.concat
            [ [ "-xen-domid"; string_of_int domid
              ; "-m"; "size=" ^ (Int64.to_string (Int64.div info.Dm_Common.memory 1024L))
              ; "-boot"; "order=" ^ info.Dm_Common.boot
              ]
            ; usb
            ; [ "-smp"; sprintf "%d,maxcpus=%d" info.Dm_Common.vcpus_current info.Dm_Common.vcpus]
            ; (serial_device |> function None -> [] | Some x -> [ "-serial"; x ])
            ; [ "-display"; "none"; "-nodefaults"]
            ; [ "-trace"; "enable=xen_platform_log"]
            ; [ "-sandbox"; "on,obsolete=deny,elevateprivileges=allow,spawn=deny,resourcecontrol=deny"]
            ; [ "-S"]
            ; [ "-global"; "PIIX4_PM.revision_id=0x1"]
            ; [ "-global"; "ide-hd.ver=0.10.2"]
            ; (global |> List.map (fun x -> ["-global"; x]) |> List.concat)
            ; (info.Dm_Common.parallel |> function None -> [ "-parallel"; "null"] | Some x -> [ "-parallel"; x])
            ; qmp
            ; xen_platform_device
            ] in

        (* Sort the VIF devices by devid *)
        let nics = List.stable_sort
          (fun (_,_,a) (_,_,b) -> compare a b) info.Dm_Common.nics in
        let nic_count = List.length nics in
        let nic_max   = Dm_Common.max_emulated_nics in
        if nic_count > nic_max then
          debug "Limiting the number of emulated NICs to %d" nic_max;
        (* Take the first 'max_emulated_nics' elements from the list. *)
        let nics = Xapi_stdext_std.Listext.List.take nic_max nics in

        (* add_nic is used in a fold: it adds fd and command line args
         * for a nic to the existing fds and arguments (fds, argv)
        *)
        let none = ["-net"; "none"] in
        let add_nic (fds, argv) (mac, bridge, devid) =
          let ifname          = sprintf "tap%d.%d" domid devid in
          let uuid, _  as tap = tap_open ifname in
          let args =
            [ "-device"; sprintf "rtl8139,netdev=tapnet%d,mac=%s,addr=%x" devid mac (devid + 4)
            ; "-netdev"; sprintf "tap,id=tapnet%d,fd=%s" devid uuid
            ] in
          (tap::fds, args@argv) in

        (** [first_gap n xs] expects an ascending list of integers [xs].
         * It looks for a gap in sequence [xs] and returns the first it
         * finds at position n or higher:
         * first_gap 4 []      = 4
         * first_gap 4 [5;6]   = 4
         * first_gap 4 [1;3]   = 4
         * first_gap 4 [5;6;8] = 4
         * first_gap 4 [4;5;7] = 6
         * first_gap 4 [4;5;6] = 7
         *)
        let rec first_gap n = function
          | []             -> n
          | x::xs when x<n -> first_gap n xs
          | x::xs when x=n -> first_gap (n+1) xs
          | x::xs          -> n
        in

        let has_nvidia_vgpu =
          let open Xenops_interface.Vgpu in
          let open Dm_Common in
          match info.disp with
          | VNC(Vgpu [{implementation = Nvidia _}],_,_,_,_) -> true
          | SDL(Vgpu [{implementation = Nvidia _}],_)       -> true
          | _                                               -> false
        in

        let pv_device_addr =
          if has_nvidia_vgpu            then 2 else
          if not @@ has_platform_device then 3 else
            nics
            |> List.map (fun (_, _, devid) -> devid+4)
            |> first_gap 4
        in

        (* Go over all nics and collect file descriptors and command
         * line arguments. Add these to the already existing command
         * line arguments in common
        *)
        List.fold_left add_nic ([],[]) nics
        |> function
        |  _, []    ->
          Dm_Common.
            { argv   = common.argv   @ misc @ pv_device pv_device_addr @ none
            ; fd_map = common.fd_map
            }
        | fds, argv ->
          Dm_Common.
            { argv   = common.argv   @ misc @ pv_device pv_device_addr @ argv
            ; fd_map = common.fd_map @ fds
            }

      let after_suspend_image ~xs ~qemu_domid domid =
        (* device model not needed anymore after suspend image has been created *)
        stop ~xs ~qemu_domid domid

    end (* Backend.Qemu_upstream_compat.Dm *)
  end (* Backend.Qemu_upstream *)

  (** Implementation of the backend common signature for the qemu-upstream backend *)
  module Qemu_upstream  = Qemu_upstream_compat
  (** Until the stage 4 defined in the qemu upstream design is implemented, qemu_upstream behaves as qemu_upstream_compat *)

  let of_profile p = match p with
    | Profile.Qemu_trad            -> (module Qemu_trad            : Intf)
    | Profile.Qemu_upstream_compat -> (module Qemu_upstream_compat : Intf)
    | Profile.Qemu_upstream        -> (module Qemu_upstream        : Intf)

  let init() =
    Qemu_upstream.Dm.Event.init()
end

(*
 *  Functions using the backend dispatcher
 *)

(** Vbd module conforming to the corresponding public mli interface *)
module Vbd = struct
  include Vbd_Common

  let media_eject ~xs ~dm device =
    let module Q = (val Backend.of_profile dm) in
    Q.Vbd.qemu_media_change ~xs device "" ""

  let media_insert ~xs ~dm ~phystype ~params device =
    let _type = backendty_of_physty phystype in
    let module Q = (val Backend.of_profile dm) in
    Q.Vbd.qemu_media_change ~xs device _type params

end (* Vbd *)

(** Vcpu module conforming to the corresponding public mli interface *)
module Vcpu = struct
  include Vcpu_Common

  let add ~xs ~dm ~devid domid online =
    let module Q = (val Backend.of_profile dm) in
    Q.Vcpu.add ~xs ~devid domid online

  let set ~xs ~dm ~devid domid online =
    let module Q = (val Backend.of_profile dm) in
    Q.Vcpu.set ~xs ~devid domid online

  let del ~xs ~dm ~devid domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Vcpu.del ~xs ~devid domid

  let status ~xs ~dm ~devid domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Vcpu.status ~xs ~devid domid

end (* Vcpu *)

(** Dm module conforming to the corresponding public mli interface *)
module Dm = struct
  include Dm_Common

  let init_daemon ~task ~path ~args ~name ~domid ~xs ~ready_path ?ready_val ~timeout ~cancel ?(fds=[]) profile =
    let module Q = (val Backend.of_profile profile) in
    Q.Dm.init_daemon ~task ~path ~args ~name ~domid ~xs ~ready_path ?ready_val ~timeout ~cancel ~fds ()

  let get_vnc_port ~xs ~dm domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.get_vnc_port ~xs domid

  let suspend (task: Xenops_task.task_handle) ~xs ~qemu_domid ~dm domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.suspend task ~xs ~qemu_domid domid

  let stop ~xs ~qemu_domid ~dm domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.stop ~xs ~qemu_domid domid

  let with_dirty_log dm domid ~f =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.with_dirty_log domid ~f

  let qemu_args ~xs ~dm info restore domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.qemu_args ~xs ~dm info restore domid

  let after_suspend_image ~xs ~dm ~qemu_domid domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.after_suspend_image ~xs ~qemu_domid domid

  (* the following functions depend on the functions above that use the qemu backend Q *)

  let start_vgpu ~xs task ?(restore=false) domid vgpus vcpus profile =
    let open Xenops_interface.Vgpu in
    match vgpus with
    | [{physical_pci_address = pci; implementation = Nvidia vgpu}] ->
      (* Start DEMU and wait until it has reached the desired state *)
      let state_path = Printf.sprintf "/local/domain/%d/vgpu/state" domid in
      let cancel = Cancel_utils.Vgpu domid in
      if not (Vgpu.is_running ~xs domid) then begin
        (* The below line does nothing if the device is already bound to the
           			 * nvidia driver. We rely on xapi to refrain from attempting to run
           			 * a vGPU on a device which is passed through to a guest. *)
        debug "start_vgpu: got VGPU with physical pci address %s"
          (Xenops_interface.Pci.string_of_address pci);
        PCI.bind [pci] PCI.Nvidia;
        let args = vgpu_args_of_nvidia domid vcpus vgpu pci restore in
        let vgpu_pid = start_daemon ~path:!Xc_resources.vgpu ~args ~name:"vgpu"
            ~domid ~fds:[] profile in
        wait_path ~pid:vgpu_pid ~task ~name:"vgpu" ~domid ~xs
          ~ready_path:state_path ~timeout:!Xenopsd.vgpu_ready_timeout
          ~cancel ();
        Forkhelpers.dontwaitpid vgpu_pid
      end else
        info "Daemon %s is already running for domain %d" !Xc_resources.vgpu domid;

      (* Keep waiting until DEMU's state becomes "initialising" or "running", or an error occurred. *)
      let good_watches = [
        Watch.value_to_become state_path "initialising";
        Watch.value_to_become state_path "running";
      ] in
      let error_watch = Watch.value_to_become state_path "error" in
      if cancellable_watch cancel good_watches [ error_watch ] task ~xs ~timeout:3600. () then
        info "Daemon vgpu is ready"
      else begin
        let error_code_path = Printf.sprintf "/local/domain/%d/vgpu/error-code" domid in
        let error_code = xs.Xs.read error_code_path in
        error "Daemon vgpu returned error: %s" error_code;
        raise (Ioemu_failed ("vgpu", Printf.sprintf "Daemon vgpu returned error: %s" error_code))
      end
    | [{physical_pci_address = pci; implementation = GVT_g vgpu}] ->
      PCI.bind [pci] PCI.I915
    | [{physical_pci_address = pci; implementation = MxGPU vgpu}] ->
      Mutex.execute gimtool_m (fun () ->
          configure_gim ~xs pci vgpu.vgpus_per_pgpu vgpu.framebufferbytes;
          let keys = [
            "pf", Xenops_interface.Pci.string_of_address pci;
          ] in
          write_vgpu_data ~xs domid 0 keys
        )
    | _ -> failwith "Unsupported vGPU configuration"


  type action = Start | Restore | StartVNC

  let __start (task: Xenops_task.task_handle)
      ~xs ~dm ?(timeout = !Xenopsd.qemu_dm_ready_timeout) action info domid =

    let args = match action with
      | Start    -> qemu_args ~xs ~dm info false domid
      | Restore  -> qemu_args ~xs ~dm info true  domid
      | StartVNC -> Dm_Common.{argv = vnconly_cmdline ~info domid; fd_map = []}
    in

    debug "Device.Dm.start domid=%d args: [%s]"
      domid (String.concat " " args.argv);

    (* start vgpu emulation if appropriate *)
    let () = match info.disp with
      | VNC (Vgpu vgpus, _, _, _, _)
      | SDL (Vgpu vgpus, _) ->
        start_vgpu ~xs task domid vgpus info.vcpus dm
      | _ -> ()
    in

    (* Execute qemu-dm-wrapper, forwarding stdout to the syslog, with the key "qemu-dm-<domid>" *)

    let argv = (prepend_wrapper_args domid args.argv) in
    let qemu_domid = 0 in (* See stubdom.ml for the corresponding kernel code *)
    let ready_path =
      Printf.sprintf "/local/domain/%d/device-model/%d/state" qemu_domid domid in
    let cancel = Cancel_utils.Qemu (qemu_domid, domid) in
    let qemu_pid = init_daemon ~task ~path:(Profile.wrapper_of dm) ~args:argv
        ~name:"qemu-dm" ~domid ~xs ~ready_path ~ready_val:"running"
        ~timeout ~cancel ~fds:args.fd_map dm in
    let close (uuid,fd) = try Unix.close fd with
      | e -> error "Closing fd for %s failed: %s (%s)"
               uuid (Printexc.to_string e) __LOC__ in
    List.iter close args.fd_map;
    match !Xenopsd.action_after_qemu_crash with
    | None ->
      (* At this point we expect qemu to outlive us; we will never call waitpid *)
      Forkhelpers.dontwaitpid qemu_pid
    | Some _ ->
      (* We register a callback to be run asynchronously in case qemu fails/crashes or is killed *)
      let waitpid_async x ~callback =
        ignore(Thread.create (fun x->callback (try Forkhelpers.waitpid_fail_if_bad_exit x; None with e -> Some e)) x)
      in
      waitpid_async qemu_pid ~callback:(fun qemu_crash -> Forkhelpers.(
          debug "Finished waiting qemu pid=%d for domid=%d" (getpid qemu_pid) domid;
          let crash_reason = match qemu_crash with
            | None -> debug "domid=%d qemu-pid=%d: detected exit=0" domid (getpid qemu_pid);
              "exit:0"
            | Some e -> begin match e with
                | Subprocess_failed x -> (* exit n *)
                  debug "domid=%d qemu-pid=%d: detected exit=%d" domid (getpid qemu_pid) x;
                  Printf.sprintf "exit:%d" x
                | Subprocess_killed x -> (* qemu received signal/crashed *)
                  debug "domid=%d qemu-pid=%d: detected signal=%d" domid (getpid qemu_pid) x;
                  Printf.sprintf "signal:%d" x
                | e -> debug "domid=%d qemu-pid=%d: detected unknown exception %s" domid (getpid qemu_pid) (Printexc.to_string e);
                  Printf.sprintf "unknown"
              end
          in
          if not (Qemu.SignalMask.has Qemu.signal_mask domid) then
            match (Qemu.pid ~xs domid) with
            | None -> (* after expected qemu stop or domain xs tree destroyed: this event arrived too late, nothing to do *)
              debug "domid=%d qemu-pid=%d: already removed from xenstore during domain destroy" domid (getpid qemu_pid);
            | Some _ ->
              (* before expected qemu stop: qemu-pid is available in domain xs tree: signal action to take *)
              xs.Xs.write (Qemu.pid_path_signal domid) crash_reason
        ))

  let start (task: Xenops_task.task_handle) ~xs ~dm ?timeout info domid =
    __start task ~xs ~dm ?timeout Start info domid

  let restore (task: Xenops_task.task_handle) ~xs ~dm ?timeout info domid =
    __start task ~xs ~dm ?timeout Restore info domid

  let start_vnconly (task: Xenops_task.task_handle) ~xs ~dm ?timeout info domid =
    __start task ~xs ~dm ?timeout StartVNC info domid

  let restore_vgpu (task: Xenops_task.task_handle) ~xs domid vgpu vcpus =
    debug "Called Dm.restore_vgpu";
    start_vgpu ~xs task ~restore:true domid [vgpu] vcpus Profile.Qemu_trad

end (* Dm *)

(* functions depending on modules Vif, Vbd, Pci, Vfs, Vfb, Vkbd, Dm *)

let hard_shutdown (task: Xenops_task.task_handle) ~xs (x: device) = match x.backend.kind with
  | Vif -> Vif.hard_shutdown task ~xs x
  | NetSriovVf -> NetSriovVf.hard_shutdown ~xs x
  | Vbd _ | Tap -> Vbd.hard_shutdown task ~xs x
  | Pci -> PCI.hard_shutdown task ~xs x
  | Vfs -> Vfs.hard_shutdown task ~xs x
  | Vfb -> Vfb.hard_shutdown task ~xs x
  | Vkbd -> Vkbd.hard_shutdown task ~xs x

let clean_shutdown (task: Xenops_task.task_handle) ~xs (x: device) = match x.backend.kind with
  | Vif -> Vif.clean_shutdown task ~xs x
  | NetSriovVf -> raise (Unimplemented("network sr-iov"))
  | Vbd _ | Tap -> Vbd.clean_shutdown task ~xs x
  | Pci -> PCI.clean_shutdown task ~xs x
  | Vfs -> Vfs.clean_shutdown task ~xs x
  | Vfb -> Vfb.clean_shutdown task ~xs x
  | Vkbd -> Vkbd.clean_shutdown task ~xs x

let get_vnc_port ~xs ~dm domid =
  (* Check whether a qemu exists for this domain *)
  let qemu_exists = Qemu.is_running ~xs domid in
  if qemu_exists
  then Dm.get_vnc_port ~xs ~dm domid
  else PV_Vnc.get_vnc_port ~xs domid

let get_tc_port ~xs domid = 
  (* Check whether a qemu exists for this domain *)
  let qemu_exists = Qemu.is_running ~xs domid in
  if qemu_exists
  then Dm.get_tc_port ~xs domid
  else PV_Vnc.get_tc_port ~xs domid
