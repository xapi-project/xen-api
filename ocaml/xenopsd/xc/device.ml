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
module Unixext = Xapi_stdext_unix.Unixext

exception Ioemu_failed of (string * string)

exception Device_shutdown

exception Device_not_found

exception Cdrom

module D = Debug.Make (struct let name = "device" end)

open D

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let internal_error fmt =
  Printf.kprintf
    (fun str ->
      error "%s" str ;
      raise (Xenopsd_error (Internal_error str))
    )
    fmt

(** Definition of available qemu profiles, used by the qemu backend
    implementations *)
module Profile = struct
  (* Qemu_trad cannot be removed from here, we need to be able to unmarshal it,
     it must not be used as a fallback anywhere though *)
  type t =
    | Qemu_trad
    | Qemu_none
    | Qemu_upstream_compat
    | Qemu_upstream
    | Qemu_upstream_uefi
  [@@deriving rpcty]

  let fallback = Qemu_upstream_compat

  module Name = struct
    let qemu_none = "qemu-none"

    let qemu_trad = "qemu-trad"

    let qemu_upstream_compat = "qemu-upstream-compat"

    let qemu_upstream = "qemu-upstream"

    let qemu_upstream_uefi = "qemu-upstream-uefi"
  end

  let wrapper_of = function
    | Qemu_none | Qemu_trad ->
        "/bin/false"
    | Qemu_upstream_compat | Qemu_upstream | Qemu_upstream_uefi ->
        !Resources.upstream_compat_qemu_dm_wrapper

  let of_string = function
    | x when x = Name.qemu_trad ->
        sprintf "unsupported device-model profile %s: use %s" x
          Name.qemu_upstream_compat
        |> fun s -> Xenopsd_error (Internal_error s) |> raise
    | x when x = Name.qemu_upstream_compat ->
        Qemu_upstream_compat
    | x when x = Name.qemu_upstream ->
        sprintf "unsupported device-model profile %s: use %s" x
          Name.qemu_upstream_compat
        |> fun s -> Xenopsd_error (Internal_error s) |> raise
    | x when x = Name.qemu_upstream_uefi ->
        Qemu_upstream_uefi
    | x when x = Name.qemu_none ->
        Qemu_none
    | x ->
        debug "unknown device-model profile %s: defaulting to %s" x
          Name.qemu_upstream_compat ;
        Qemu_upstream_compat
end

(* keys read by vif udev script (keep in sync with api:scripts/vif) *)
let vif_udev_keys =
  "promiscuous"
  :: List.map (fun x -> "ethtool-" ^ x) ["rx"; "tx"; "sg"; "tso"; "ufo"; "gso"]

(****************************************************************************************)

module Generic = struct
  (* Oxenstored's transaction conflict algorithm will cause parallel but
     separate device creation transactions to abort and retry, leading to
     livelock while starting lots of VMs. Work around this by serialising these
     transactions for now. *)
  let device_serialise_m = Mutex.create ()

  let add_device ~xs device backend_list frontend_list private_list
      xenserver_list =
    with_lock device_serialise_m (fun () ->
        let frontend_ro_path = frontend_ro_path_of_device ~xs device
        and frontend_rw_path = frontend_rw_path_of_device ~xs device
        and backend_path = backend_path_of_device ~xs device
        and hotplug_path = Hotplug.get_hotplug_path device
        and private_data_path =
          Device_common.get_private_data_path_of_device device
        and extra_xenserver_path =
          Device_common.extra_xenserver_path_of_device ~xs device
        in
        debug "adding device  B%d[%s]  F%d[%s]  H[%s]" device.backend.domid
          backend_path device.frontend.domid frontend_rw_path hotplug_path ;
        Xs.transaction xs (fun t ->
            ( try
                (* Use the ro one because a bad guest could delete the rw node. *)
                ignore (t.Xst.read frontend_ro_path) ;
                raise (Device_frontend_already_connected device)
              with Xs_protocol.Enoent _ -> ()
            ) ;
            t.Xst.rm frontend_rw_path ;
            t.Xst.rm frontend_ro_path ;
            t.Xst.rm backend_path ;
            (* CA-16259: don't clear the 'hotplug_path' because this is where we
               record our own use of /dev/loop devices. Clearing this causes us
               to leak one per PV .iso *)
            t.Xst.mkdirperms frontend_rw_path
              (Xenbus_utils.device_frontend device) ;
            t.Xst.mkdirperms frontend_ro_path (Xenbus_utils.rwperm_for_guest 0) ;
            t.Xst.mkdirperms backend_path (Xenbus_utils.device_backend device) ;
            t.Xst.mkdirperms hotplug_path (Xenbus_utils.hotplug device) ;
            t.Xst.writev frontend_rw_path
              (("backend", backend_path) :: frontend_list) ;
            t.Xst.writev frontend_ro_path [("backend", backend_path)] ;
            t.Xst.writev backend_path
              (("frontend", frontend_rw_path) :: backend_list) ;
            t.Xst.mkdirperms private_data_path (Xenbus_utils.hotplug device) ;
            t.Xst.writev private_data_path
              (("backend-kind", string_of_kind device.backend.kind)
              :: ("backend-id", string_of_int device.backend.domid)
              :: private_list
              ) ;
            t.Xst.mkdirperms extra_xenserver_path
              (Xenbus_utils.rwperm_for_guest device.frontend.domid) ;
            t.Xst.writev extra_xenserver_path xenserver_list
        )
    )

  let get_private_key ~xs device x =
    let private_data_path =
      Device_common.get_private_data_path_of_device device
    in
    let key = private_data_path ^ "/" ^ x in
    try xs.Xs.read key with e -> error "read %s: Noent" key ; raise e

  let safe_rm ~xs path =
    try
      debug "xenstore-rm %s" path ;
      xs.Xs.rm path
    with _ -> debug "Failed to xenstore-rm %s; continuing" path

  (* Helper function to delete the frontend, backend and error trees for a
     device. This must only be done after synchronising with the hotplug
     scripts. Cleaning up is best-effort; some of it might fail but as much will
     be done as possible. *)
  let rm_device_state ~xs (x : device) =
    debug "Device.rm_device_state %s" (string_of_device x) ;
    safe_rm ~xs (frontend_ro_path_of_device ~xs x) ;
    safe_rm ~xs (frontend_rw_path_of_device ~xs x) ;
    safe_rm ~xs (backend_path_of_device ~xs x) ;
    (* Cleanup the directory containing the error node *)
    safe_rm ~xs (backend_error_path_of_device ~xs x) ;
    safe_rm ~xs (Filename.dirname (error_path_of_device ~xs x))

  (* The surprise-remove flag is now ignored: a vbd-unplug --force will unplug
     regardless of surprise-remove. Leave this code here for now, to warn the
     user in the logs. *)
  let can_surprise_remove ~xs (x : device) =
    (* "(info key in xenstore) && 2" tells us whether a vbd can be surprised
       removed *)
    let key = backend_path_of_device ~xs x ^ "/info" in
    try
      let info = Int64.of_string (xs.Xs.read key) in
      Int64.logand info 2L <> 0L
    with _ -> false

  (** Checks whether the supplied device still exists (ie hasn't been deleted) *)
  let exists ~xs (x : device) =
    let backend_stub = backend_path_of_device ~xs x in
    try
      ignore_string (xs.Xs.read backend_stub) ;
      true
    with Xs_protocol.Enoent _ -> false

  (** When hot-unplugging a device we ask nicely *)
  let clean_shutdown_async ~xs (x : device) =
    let backend_path = backend_path_of_device ~xs x in
    let state_path = backend_path ^ "/state" in
    Xs.transaction xs (fun t ->
        let online_path = backend_path ^ "/online" in
        debug "xenstore-write %s = 0" online_path ;
        t.Xst.write online_path "0" ;
        let state =
          try Xenbus_utils.of_string (t.Xst.read state_path)
          with _ -> Xenbus_utils.Closed
        in
        if state <> Xenbus_utils.Closed then (
          debug "Device.del_device setting backend to Closing" ;
          t.Xst.write state_path (Xenbus_utils.string_of Xenbus_utils.Closing)
        )
    )

  let unplug_watch ~xs:_ (x : device) =
    Hotplug.path_written_by_hotplug_scripts x |> Watch.key_to_disappear

  let error_watch ~xs (x : device) =
    Watch.value_to_appear (error_path_of_device ~xs x)

  let frontend_closed ~xs (x : device) =
    Watch.map
      (fun () -> "")
      (Watch.value_to_become
         (frontend_rw_path_of_device ~xs x ^ "/state")
         (Xenbus_utils.string_of Xenbus_utils.Closed)
      )

  let backend_closed ~xs (x : device) =
    Watch.value_to_become
      (backend_path_of_device ~xs x ^ "/state")
      (Xenbus_utils.string_of Xenbus_utils.Closed)

  let is_backend backend_type path =
    let affix = Printf.sprintf "backend/%s/" backend_type in
    Astring.String.is_infix ~affix path

  let is_qdisk_or_9pfs x =
    let path = Hotplug.path_written_by_hotplug_scripts x in
    is_backend "qdisk" path || is_backend "9pfs" path

  let on_backend_closed_unplug ~xs x =
    debug "Device.on_backend_closed_unplug for %s" (string_of_device x) ;
    (* qemu-dp does not delete the hotplug status key *)
    backend_closed ~xs x
    |> Watch.map (fun _ ->
           debug "Backend closed for %s, deleting hotplug-status"
             (string_of_device x) ;
           (* deleting this key causes the udev rule to fire *)
           safe_rm ~xs (Hotplug.path_written_by_hotplug_scripts x)
       )

  let clean_shutdown_wait (task : Xenops_task.task_handle) ~xs
      ~ignore_transients (x : device) =
    debug "Device.Generic.clean_shutdown_wait %s" (string_of_device x) ;
    let on_error () =
      let error_path = error_path_of_device ~xs x in
      let error = try xs.Xs.read error_path with _ -> "" in
      debug "Device.Generic.shutdown_common: read an error: %s" error ;
      (* After CA-14804 we deleted the error node *)
      (* After CA-73099 we stopped doing that *)
      (* ... but in the case of a "managed" domain, this transient should be
         ignored anyway *)
      raise (Device_error (x, error))
    in
    let cancel = Device x in
    let frontend_closed = Watch.map (fun _ -> ()) (frontend_closed ~xs x) in
    let unplug =
      let qdisk_or_9pfs = is_qdisk_or_9pfs x in
      debug "Device.unplug_watch %s, disk=%b" (string_of_device x) qdisk_or_9pfs ;
      let backend_watch =
        if qdisk_or_9pfs then [((), on_backend_closed_unplug ~xs x)] else []
      in
      let frontend_gone =
        ( ()
        , frontend_rw_path_of_device ~xs x ^ "/state" |> Watch.key_to_disappear
        )
      in
      let unplugged_watch = ((), unplug_watch ~xs x) in
      (* we need to evaluate all watches, so use any_of *)
      Watch.any_of (unplugged_watch :: frontend_gone :: backend_watch)
      |> Watch.map (fun _ -> ())
    in
    let error = Watch.map (fun _ -> ()) (error_watch ~xs x) in
    if
      cancellable_watch cancel [frontend_closed; unplug]
        (if ignore_transients then [] else [error])
        task ~xs ~timeout:!Xenopsd.hotplug_timeout ()
    then (
      safe_rm ~xs (frontend_rw_path_of_device ~xs x) ;
      safe_rm ~xs (frontend_ro_path_of_device ~xs x) ;
      if
        cancellable_watch cancel [unplug]
          (if ignore_transients then [] else [error])
          task ~xs ~timeout:!Xenopsd.hotplug_timeout ()
      then
        rm_device_state ~xs x
      else
        on_error ()
    ) else
      on_error ()

  let clean_shutdown (task : Xenops_task.task_handle) ~xs (x : device) =
    debug "Device.Generic.clean_shutdown %s" (string_of_device x) ;
    clean_shutdown_async ~xs x ;
    clean_shutdown_wait task ~xs ~ignore_transients:false x

  let hard_shutdown_request ~xs (x : device) =
    debug "Device.Generic.hard_shutdown_request %s" (string_of_device x) ;
    let backend_path = backend_path_of_device ~xs x in
    let online_path = backend_path ^ "/online" in
    debug "xenstore-write %s = 0" online_path ;
    xs.Xs.write online_path "0" ;
    debug "Device.Generic.hard_shutdown about to blow away frontend" ;
    safe_rm ~xs (frontend_rw_path_of_device ~xs x) ;
    safe_rm ~xs (frontend_ro_path_of_device ~xs x)

  let run_hotplug_scripts (x : device) =
    !Xenopsd.run_hotplug_scripts || x.backend.domid > 0

  let hard_shutdown_complete ~xs (x : device) =
    if is_qdisk_or_9pfs x then
      safe_rm ~xs (Hotplug.path_written_by_hotplug_scripts x) ;
    if run_hotplug_scripts x then
      backend_closed ~xs x
    else
      unplug_watch ~xs x

  let hard_shutdown (task : Xenops_task.task_handle) ~xs (x : device) =
    hard_shutdown_request ~xs x ;
    let (_ : bool) =
      cancellable_watch (Device x)
        [hard_shutdown_complete ~xs x]
        [] task ~xs ~timeout:!Xenopsd.hotplug_timeout ()
    in
    (* blow away the backend and error paths *)
    debug
      "Device.Generic.hard_shutdown about to blow away backend and error paths" ;
    rm_device_state ~xs x

  let best_effort = Xenops_utils.best_effort
end

(****************************************************************************************)
(** Disks: *)

(** Vbd_Common contains the private Vbd functions that are common between the
    qemu profile backends *)
module Vbd_Common = struct
  type shutdown_mode =
    | Classic
        (** no signal that backend has flushed, rely on (eg) SM vdi_deactivate
            for safety *)
    | ShutdownRequest
        (** explicit signal that backend has flushed via "shutdown-done" *)

  let read_feature_flag ~xs (x : device) flag =
    let feature_flag_path =
      Printf.sprintf "/local/domain/%d/control/%s" x.backend.domid flag
    in
    try
      ignore (xs.Xs.read feature_flag_path) ;
      true
    with _ -> false

  let shutdown_mode_of_device ~xs (x : device) =
    if read_feature_flag ~xs x "feature-shutdown-request" then
      ShutdownRequest
    else
      Classic

  type mode = ReadOnly | ReadWrite

  let string_of_mode = function ReadOnly -> "r" | ReadWrite -> "w"

  let mode_of_string = function
    | "r" ->
        ReadOnly
    | "w" ->
        ReadWrite
    | _ ->
        invalid_arg "mode_of_string"

  type physty = File | Phys | Qcow | Vhd | Aio

  let backendty_of_physty = function
    | File ->
        "file"
    | Phys ->
        "phy"
    | Qcow | Vhd | Aio ->
        "phy"

  let string_of_physty = function
    | Qcow ->
        "qcow"
    | Vhd ->
        "vhd"
    | Aio ->
        "aio"
    | File ->
        "file"
    | Phys ->
        "phys"

  let physty_of_string s =
    match s with
    | "qcow" ->
        Qcow
    | "vhd" ->
        Vhd
    | "aio" ->
        Aio
    | "phy" ->
        Phys
    | "file" ->
        File
    | _ ->
        invalid_arg "physty_of_string"

  type devty = CDROM | Disk | Floppy

  let string_of_devty = function
    | CDROM ->
        "cdrom"
    | Disk ->
        "disk"
    | Floppy ->
        "floppy"

  let devty_of_string = function
    | "cdrom" ->
        CDROM
    | "disk" ->
        Disk
    | "floppy" ->
        Floppy
    | _ ->
        invalid_arg "devty_of_string"

  let uses_blktap ~phystype = List.mem phystype [Qcow; Vhd; Aio]

  (** Request either a clean or hard shutdown *)
  let request_shutdown ~xs (x : device) (force : bool) =
    let request = if force then "force" else "normal" in
    debug "Device.Vbd.request_shutdown %s %s" (string_of_device x) request ;
    let backend_path = backend_path_of_device ~xs x in
    let request_path = backend_shutdown_request_path_of_device ~xs x in
    let online_path = backend_path ^ "/online" in
    (* Prevent spurious errors appearing by not writing online=0 if force *)
    if not force then (
      debug "xenstore-write %s = 0" online_path ;
      xs.Xs.write online_path "0"
    ) ;
    debug "xenstore-write %s = %s" request_path request ;
    xs.Xs.write request_path request

  (** Return the event to wait for when the shutdown has completed *)
  let shutdown_done ~xs (x : device) : unit Watch.t =
    Watch.value_to_appear (backend_shutdown_done_path_of_device ~xs x)
    |> Watch.map (fun _ -> ())

  let shutdown_request_clean_shutdown_wait (task : Xenops_task.task_handle) ~xs
      ~ignore_transients (x : device) =
    debug "Device.Vbd.clean_shutdown_wait %s" (string_of_device x) ;
    (* Allow the domain to reject the request by writing to the error node *)
    let shutdown_done = shutdown_done ~xs x in
    let error =
      Watch.value_to_appear (error_path_of_device ~xs x)
      |> Watch.map (fun _ -> ())
    in
    if
      cancellable_watch (Device x) [shutdown_done]
        (if ignore_transients then [] else [error])
        task ~xs ~timeout:!Xenopsd.hotplug_timeout ()
    then (
      debug "Device.Vbd.shutdown_common: shutdown-done appeared" ;
      (* Delete the trees (otherwise attempting to plug the device in again
         doesn't work.) This also clears any stale error nodes. *)
      Generic.rm_device_state ~xs x
    ) else
      let error_path = error_path_of_device ~xs x in
      let error = try xs.Xs.read error_path with _ -> "" in
      (* CA-14804: Delete the error node contents *)
      (* After CA-73099 we stopped doing that *)
      debug "Device.Vbd.shutdown_common: read an error: %s" error ;
      raise (Device_error (x, error))

  let shutdown_request_hard_shutdown (task : Xenops_task.task_handle) ~xs
      (x : device) =
    debug "Device.Vbd.hard_shutdown %s" (string_of_device x) ;
    request_shutdown ~xs x true ;

    (* force *)

    (* We don't watch for error nodes *)
    let (_ : bool) =
      cancellable_watch (Device x)
        [shutdown_done ~xs x]
        [] task ~xs ~timeout:!Xenopsd.hotplug_timeout ()
    in
    Generic.rm_device_state ~xs x ;
    debug "Device.Vbd.hard_shutdown complete"

  let clean_shutdown_async ~xs x =
    match shutdown_mode_of_device ~xs x with
    | Classic ->
        Generic.clean_shutdown_async ~xs x
    | ShutdownRequest ->
        request_shutdown ~xs x false

  (* normal *)

  let clean_shutdown_wait (task : Xenops_task.task_handle) ~xs
      ~ignore_transients x =
    match shutdown_mode_of_device ~xs x with
    | Classic ->
        Generic.clean_shutdown_wait task ~xs ~ignore_transients x
    | ShutdownRequest ->
        shutdown_request_clean_shutdown_wait task ~xs ~ignore_transients x

  let clean_shutdown (task : Xenops_task.task_handle) ~xs x =
    clean_shutdown_async ~xs x ;
    clean_shutdown_wait task ~xs ~ignore_transients:false x

  let hard_shutdown (task : Xenops_task.task_handle) ~xs x =
    match shutdown_mode_of_device ~xs x with
    | Classic ->
        Generic.hard_shutdown task ~xs x
    | ShutdownRequest ->
        shutdown_request_hard_shutdown task ~xs x

  let hard_shutdown_request ~xs x =
    match shutdown_mode_of_device ~xs x with
    | Classic ->
        Generic.hard_shutdown_request ~xs x
    | ShutdownRequest ->
        request_shutdown ~xs x true

  let hard_shutdown_complete ~xs x =
    match shutdown_mode_of_device ~xs x with
    | Classic ->
        Generic.hard_shutdown_complete ~xs x
    | ShutdownRequest ->
        shutdown_done ~xs x

  let hard_shutdown_wait (task : Xenops_task.task_handle) ~xs ~timeout x =
    let (_ : bool) =
      cancellable_watch (Device x)
        [Watch.map (fun _ -> ()) (hard_shutdown_complete ~xs x)]
        [] task ~xs ~timeout ()
    in
    ()

  let release (task : Xenops_task.task_handle) ~xc ~xs (x : device) =
    debug "Device.Vbd.release %s" (string_of_device x) ;
    (* Make sure blktap/blkback fire the udev remove event by deleting the
       backend now *)
    Generic.safe_rm ~xs (backend_path_of_device ~xs x) ;
    Hotplug.release task ~xc ~xs x ;
    if Generic.run_hotplug_scripts x then
      Hotplug.run_hotplug_script x ["remove"] ;
    (* As for add above, if the frontend is in dom0, we can wait for the
       frontend to unplug as well as the backend. CA-13506 *)
    if x.frontend.domid = 0 && x.backend.domid = 0 then
      Hotplug.wait_for_frontend_unplug task ~xs x

  let free_device ~xs hvm domid =
    let disks =
      List.map
        (fun x ->
          x.frontend.devid
          |> Device_number.of_xenstore_key
          |> Device_number.spec
          |> function
          | _, disk, _ ->
              disk
        )
        (Device_common.list_frontends ~xs domid)
    in
    let next = List.fold_left max 0 disks + 1 in
    let open Device_number in
    let bus_type = if hvm && next < 4 then Ide else Xen in
    (bus_type, next, 0)

  type t = {
      mode: mode
    ; device_number: Device_number.t option
    ; phystype: physty
    ; params: string
    ; dev_type: devty
    ; unpluggable: bool
    ; protocol: protocol option
    ; kind: Device_common.kind
    ; extra_backend_keys: (string * string) list
    ; extra_private_keys: (string * string) list
    ; backend_domid: int
  }

  let add_async ~xs ~hvm x domid =
    let back_tbl = Hashtbl.create 16 and front_tbl = Hashtbl.create 16 in
    let open Device_number in
    (* If no device number is provided then autodetect a free one *)
    let device_number =
      match x.device_number with
      | Some x ->
          x
      | None ->
          make (free_device ~xs hvm domid)
    in
    let devid = to_xenstore_key device_number in
    let device =
      let backend = {domid= x.backend_domid; kind= x.kind; devid} in
      device_of_backend backend domid
    in
    debug "Device.Vbd.add (device_number=%s | params=%s | phystype=%s)"
      (to_debug_string device_number)
      x.params
      (string_of_physty x.phystype) ;
    (* Notes:

       1. qemu accesses devices images itself and so needs the path of the
       original file (in params)

       2. when windows PV drivers initialise, the new blockfront connects to the
       up-til-now idle blockback.

       3. when the VM is fully PV, Ioemu devices do not work; all devices must
       be PV

       4. in the future an HVM guest might support a mixture of both *)
    List.iter (fun (k, v) -> Hashtbl.add back_tbl k v) x.extra_backend_keys ;
    List.iter
      (fun (k, v) -> Hashtbl.replace front_tbl k v)
      [
        ("backend-id", string_of_int x.backend_domid)
      ; ("state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising))
      ; ("virtual-device", string_of_int devid)
      ; ( "device-type"
        , match x.dev_type with
          | CDROM ->
              "cdrom"
          | Disk ->
              "disk"
          | Floppy ->
              "floppy"
        )
      ] ;
    List.iter
      (fun (k, v) -> Hashtbl.replace back_tbl k v)
      [
        ("frontend-id", sprintf "%u" domid)
      ; (* Prevents the backend hotplug scripts from running if the frontend
           disconnects. This allows the xenbus connection to re-establish itself *)
        ("online", "1")
      ; ("removable", if x.unpluggable then "1" else "0")
      ; ("state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising))
      ; ("dev", to_linux_device device_number)
      ; ("type", backendty_of_physty x.phystype)
      ; ("mode", string_of_mode x.mode)
      ; ("params", x.params)
      ] ;
    let qemu_params =
      x.extra_backend_keys
      |> List.assoc_opt "qemu-params"
      |> Option.value ~default:""
    in
    ( match String.split_on_char ' ' qemu_params with
    | [physical_device_path; physical_device] ->
        List.iter
          (fun (k, v) -> Hashtbl.replace back_tbl k v)
          [
            ("physical-device", physical_device)
          ; ("physical-device-path", physical_device_path)
          ]
    | [_vdi; tag; security_model; path] ->
        List.iter
          (fun (k, v) -> Hashtbl.replace back_tbl k v)
          [("security_model", security_model); ("path", path)] ;

        List.iter (fun (k, v) -> Hashtbl.replace front_tbl k v) [("tag", tag)]
    | _ ->
        ()
    ) ;
    Option.iter
      (fun protocol ->
        Hashtbl.add front_tbl "protocol" (string_of_protocol protocol)
      )
      x.protocol ;
    let back = Hashtbl.fold (fun k v acc -> (k, v) :: acc) back_tbl [] in
    let front = Hashtbl.fold (fun k v acc -> (k, v) :: acc) front_tbl [] in
    Generic.add_device ~xs device back front x.extra_private_keys [] ;
    device

  let add_wait (task : Xenops_task.task_handle) ~xc ~xs device =
    if Generic.run_hotplug_scripts device then
      Hotplug.run_hotplug_script device ["add"] ;
    Hotplug.wait_for_plug task ~xs device ;
    debug "Device.Vbd successfully added; device_is_online = %b"
      (Hotplug.device_is_online ~xs device) ;
    (* 'Normally' we connect devices to other domains, and cannot know whether
       the device is 'available' from their userspace (or even if they have a
       userspace). The best we can do is just to wait for the backend hotplug
       scripts to run, indicating that the backend has locked the resource. In
       the case of domain 0 we can do better: we have custom hotplug scripts
       which call us back when the device is actually available to userspace. We
       need to wait for this condition to make the template installers work. NB
       if the custom hotplug script fires this implies that the xenbus state
       reached "connected", so we don't have to check for that first. *)
    if device.frontend.domid = 0 && device.backend.domid = 0 then (
      try
        (* CA-15605: clean up on dom0 block-attach failure *)
        Hotplug.wait_for_frontend_plug task ~xs device
      with Hotplug.Frontend_device_error _ as e ->
        debug
          "Caught Frontend_device_error: assuming it is safe to shutdown the \
           backend" ;
        clean_shutdown task ~xs device ;
        (* assumes double-failure isn't possible *)
        release task ~xc ~xs device ;
        raise e
    ) ;
    device

  (* Add the VBD to the domain, When this command returns, the device is ready.
     (This isn't as concurrent as xend-- xend allocates loopdevices via hotplug
     in parallel and then performs a 'waitForDevices') *)
  let add (task : Xenops_task.task_handle) ~xc ~xs ~hvm x domid =
    let device =
      let result = ref None in
      while !result = None do
        try result := Some (add_async ~xs ~hvm x domid)
        with Device_frontend_already_connected _ as e ->
          if x.device_number = None then (
            debug "Temporary failure to allocte a device number; retrying" ;
            Thread.delay 0.1
          ) else
            raise e
        (* permanent failure *)
      done ;
      Option.get !result
    in
    add_wait task ~xc ~xs device

  let qemu_media_change ~xs device _type params =
    let backend_path = backend_path_of_device ~xs device in
    let params_path = backend_path ^ "/params" in
    (* unfortunately qemu filter the request if on the same string it has, so we
       trick it by having a different string, but the same path, adding a
       spurious '/' character at the beggining of the string. *)
    let oldval = try xs.Xs.read params_path with _ -> "" in
    let pathtowrite =
      if oldval = params then
        "/" ^ params
      else
        params
    in
    let back_delta = [("type", _type); ("params", pathtowrite)] in
    Xs.transaction xs (fun t -> t.Xst.writev backend_path back_delta) ;
    debug "Media changed: params = %s" pathtowrite

  let media_is_ejected ~xs device =
    let path = backend_path_of_device ~xs device ^ "/params" in
    try xs.Xs.read path = "" with _ -> raise Device_not_found
end

(****************************************************************************************)
(** VIFs: *)

(** Generate a random MAC address, using OUI (Organizationally Unique
    Identifier) 00-16-3E, allocated to Xensource, Inc.

    The remaining 3 fields are random, with the first bit of the first random
    field set 0. *)

module Vif = struct
  let add ~xs ~devid ~mac ?mtu ?(rate = None) ?(backend_domid = 0)
      ?(other_config = []) ~netty ~carrier ?(protocol = Protocol_Native)
      ?(extra_private_keys = []) ?(extra_xenserver_keys = [])
      (task : Xenops_task.task_handle) domid =
    debug
      "Device.Vif.add domid=%d devid=%d mac=%s carrier=%b rate=%s \
       other_config=[%s] extra_private_keys=[%s] extra_xenserver_keys=[%s]"
      domid devid mac carrier
      (match rate with None -> "none" | Some (a, b) -> sprintf "(%Ld,%Ld)" a b)
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) other_config))
      (String.concat "; "
         (List.map (fun (k, v) -> k ^ "=" ^ v) extra_private_keys)
      )
      (String.concat "; "
         (List.map (fun (k, v) -> k ^ "=" ^ v) extra_xenserver_keys)
      ) ;
    (* Filter the other_config keys using vif_udev_keys as a whitelist *)
    let other_config =
      List.filter (fun (x, _) -> List.mem x vif_udev_keys) other_config
    in
    let frontend = {domid; kind= Vif; devid} in
    let backend = {domid= backend_domid; kind= Vif; devid} in
    let device = {backend; frontend} in
    let back_options =
      match rate with
      | None ->
          []
      | Some (kbytes_per_s, timeslice_us) ->
          let ( ^* ) = Int64.mul and ( ^/ ) = Int64.div in
          let timeslice_us =
            if timeslice_us > 0L then
              timeslice_us
            else
              50000L
            (* 50ms by default *)
          in
          let bytes_per_interval =
            ((kbytes_per_s ^* 1024L) ^* timeslice_us) ^/ 1000000L
          in
          if bytes_per_interval > 0L && bytes_per_interval < 0xffffffffL then
            [("rate", sprintf "%Lu,%Lu" bytes_per_interval timeslice_us)]
          else (
            debug "VIF qos: invalid value for byte/interval: %Lu"
              bytes_per_interval ;
            []
          )
    in
    let back =
      [
        ("frontend-id", sprintf "%u" domid)
      ; ("online", "1")
      ; ("state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising))
      ; ("script", !Xc_resources.vif_script)
      ; ("mac", mac)
      ; ("handle", string_of_int devid)
      ]
      @ back_options
    in
    let front_options =
      if protocol <> Protocol_Native then
        [("protocol", string_of_protocol protocol)]
      else
        []
    in
    let front_mtu =
      match mtu with
      | Some mtu when mtu > 0 ->
          [("mtu", string_of_int mtu)]
      | _ ->
          []
    in
    let front =
      [
        ("backend-id", string_of_int backend_domid)
      ; ("state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising))
      ; ("handle", string_of_int devid)
      ; ("mac", mac)
      ; ("disconnect", if carrier then "0" else "1")
      ]
      @ front_options
      @ front_mtu
    in
    let extra_private_keys =
      List.map (fun (k, v) -> ("other-config/" ^ k, v)) other_config
      @ extra_private_keys
    in
    (* Add the rest of the important configuration to the private bit of
       xenstore so we can access it later *)
    let extra_private_keys =
      extra_private_keys
      @ ("mac", mac)
        ::
        ( match mtu with
        | Some mtu when mtu > 0 ->
            [("MTU", string_of_int mtu)]
        | _ ->
            []
        )
      @ ( match netty with
        | Netman.Bridge b ->
            [("bridge", b); ("bridge-MAC", "fe:ff:ff:ff:ff:ff")]
        | Netman.Vswitch b ->
            [("bridge", b); ("bridge-MAC", "fe:ff:ff:ff:ff:ff")]
        | Netman.DriverDomain ->
            []
        | Netman.Nat ->
            []
        )
      @
      match rate with
      | None ->
          []
      | Some (rate, timeslice) ->
          [
            ("rate", Int64.to_string rate)
          ; ("timeslice", Int64.to_string timeslice)
          ]
    in
    Generic.add_device ~xs device back front extra_private_keys
      extra_xenserver_keys ;
    if Generic.run_hotplug_scripts device then (
      (* The VIF device won't be created until the backend is in state InitWait: *)
      Hotplug.wait_for_connect task ~xs device ;
      let tap = {device with backend= {device.backend with kind= Tap}} in
      Hotplug.run_hotplug_script device ["add"] ;
      Hotplug.run_hotplug_script device ["online"] ;
      Hotplug.run_hotplug_script tap ["add"] ;
      Hotplug.run_hotplug_script tap ["online"]
    ) ;
    Hotplug.wait_for_plug task ~xs device ;
    device

  let clean_shutdown = Generic.clean_shutdown

  let hard_shutdown = Generic.hard_shutdown

  let set_carrier ~xs (x : device) carrier =
    debug "Device.Vif.set_carrier %s <- %b" (string_of_device x) carrier ;
    let disconnect_path = disconnect_path_of_device ~xs x in
    xs.Xs.write disconnect_path (if carrier then "0" else "1")

  let release (task : Xenops_task.task_handle) ~xc ~xs (x : device) =
    debug "Device.Vif.release %s" (string_of_device x) ;
    if Generic.run_hotplug_scripts x then (
      let tap = {x with backend= {x.backend with kind= Tap}} in
      Hotplug.run_hotplug_script x ["remove"] ;
      Hotplug.run_hotplug_script tap ["remove"]
    ) ;
    Hotplug.release task ~xc ~xs x

  let move ~xs (x : device) bridge =
    let xs_bridge_path =
      Device_common.get_private_data_path_of_device x ^ "/bridge"
    in
    xs.Xs.write xs_bridge_path bridge ;
    Hotplug.run_hotplug_script x ["move"; "type_if=vif"] ;
    (* Maybe there's a tap, too *)
    try
      Hotplug.run_hotplug_script
        {x with backend= {x.backend with kind= Tap}}
        ["move"; "type_if=tap"]
    with _ -> ()
end

(****************************************************************************************)

(** Network SR-IOV VFs: *)
module NetSriovVf = struct
  let add ~xs ~devid ~mac ?mtu:_ ?(rate = None) ?(backend_domid = 0)
      ?(other_config = []) ~pci ~vlan ~carrier ?(extra_private_keys = [])
      ?(extra_xenserver_keys = []) (task : Xenops_task.task_handle) domid =
    let vlan_str =
      match vlan with None -> "none" | Some vlan -> sprintf "%Ld" vlan
    in
    let rate_str =
      match rate with None -> "none" | Some (a, b) -> sprintf "(%Ld,%Ld)" a b
    in
    debug
      "Device.NetSriovVf.add domid=%d devid=%d pci=%s vlan=%s mac=%s \
       carrier=%b rate=%s other_config=[%s] extra_private_keys=[%s] \
       extra_xenserver_keys=[%s]"
      domid devid
      (Xenops_interface.Pci.string_of_address pci)
      vlan_str mac carrier rate_str
      (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) other_config))
      (String.concat "; "
         (List.map (fun (k, v) -> k ^ "=" ^ v) extra_private_keys)
      )
      (String.concat "; "
         (List.map (fun (k, v) -> k ^ "=" ^ v) extra_xenserver_keys)
      ) ;
    let frontend = {domid; kind= NetSriovVf; devid} in
    let backend = {domid= backend_domid; kind= NetSriovVf; devid} in
    let device = {backend; frontend} in
    Generic.add_device ~xs device [] [] extra_private_keys
      (extra_xenserver_keys @ other_config) ;
    let rate_Mbps =
      match rate with
      | Some (0L, _) | None ->
          None
      | Some (rate, _) -> (
        match Int64.div rate 1024L with 0L -> Some 1L | rate -> Some rate
      )
    in
    let net_sriov_vf_config =
      Network_client.Client.Sriov.{mac= Some mac; vlan; rate= rate_Mbps}
    in
    (let ret =
       Network_client.Client.Sriov.make_vf_config (Xenops_task.get_dbg task) pci
         net_sriov_vf_config
     in
     let open Network_client.Client.Sriov in
     match ret with
     | Ok ->
         ()
     | Error Config_vf_rate_not_supported ->
         error
           "It is not supported to configure rate on this network SR-IOV VF \
            (pci:%s)"
           (Pci.string_of_address pci)
     | Error (Unknown s) ->
         failwith
           (Printf.sprintf
              "Failed to configure network SR-IOV VF (pci:%s) with mac=%s \
               vlan=%s rate=%s: %s"
              (Pci.string_of_address pci)
              mac vlan_str rate_str s
           )
    ) ;
    device

  let hard_shutdown ~xs (x : device) =
    debug
      "Device.NetSriovVf.hard_shutdown about to blow away backend and frontend \
       paths" ;
    Generic.safe_rm ~xs (frontend_ro_path_of_device ~xs x) ;
    Generic.safe_rm ~xs (frontend_rw_path_of_device ~xs x) ;
    Generic.safe_rm ~xs (backend_path_of_device ~xs x)
end

(*****************************************************************************)

(** Vcpus: *)
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
    try
      match xs.Xs.read path with
      | "online" ->
          true
      | "offline" ->
          false
      | _ ->
          (* garbage, assuming false *) false
    with Xs_protocol.Enoent _ -> false
end

module PCI = struct
  type t = {
      address: Xenops_interface.Pci.address
    ; irq: int
    ; resources: (int64 * int64 * int64) list
    ; driver: string
  }

  type index = int

  type device' = {
      host: Xenops_interface.Pci.address
    ; guest: index * Xenops_interface.Pci.address option
    ; qmp_add: bool  (** false: don't issue Device_add command *)
  }

  exception Domain_not_running of Xenops_interface.Pci.address * int

  exception Cannot_add of Xenops_interface.Pci.address list * exn

  (* devices, reason *)

  let () =
    Printexc.register_printer (function
      | Domain_not_running (addr, domid) ->
          Some
            (Printf.sprintf "Domain_not_running(inserting pci=%s,domid=%d)"
               (Xenops_interface.Pci.string_of_address addr)
               domid
            )
      | Cannot_add (devices, e) ->
          let addrs =
            devices
            |> List.map Xenops_interface.Pci.string_of_address
            |> String.concat ";"
          in
          Some (Printf.sprintf "Cannot_add(%s, %s)" addrs (Printexc.to_string e))
      | Ioemu_failed (name, msg) ->
          Some (Printf.sprintf "Ioemu_failed(%s, %s)" name msg)
      | _ ->
          None
      )

  (* From
     https://github.com/torvalds/linux/blob/v4.19/include/linux/pci.h#L76-L102 *)
  (* same as libxl_internal: PROC_PCI_NUM_RESOURCES *)
  let _proc_pci_num_resources = 7

  (* same as libxl_internal: PCI_BAR_IO *)
  let _pci_bar_io = 0x01n

  let _page_size = 4096n

  let _xen_domctl_dev_rdm_relaxed = 1

  (* XXX: we don't want to use the 'xl' command here because the "interface"
     isn't considered as stable as the C API *)
  let xl_pci cmd pcidevs domid =
    List.iter
      (fun dev ->
        try
          let _, _ =
            Forkhelpers.execute_command_get_output "/usr/sbin/xl"
              [
                cmd
              ; string_of_int domid
              ; Xenops_interface.Pci.string_of_address dev
              ]
          in
          ()
        with e ->
          debug "xl %s: %s" cmd (Printexc.to_string e) ;
          raise e
      )
      pcidevs

  let add_xl = xl_pci "pci-attach"

  let release_xl = xl_pci "pci-detach"

  let device_model_pci_device_path xs be_domid fe_domid =
    let be_path = xs.Xs.getdomainpath be_domid in
    Printf.sprintf "%s/backend/pci/%d/0" be_path fe_domid

  (* Given a domid, return a list of [ X, (domain, bus, dev, func) ] where X
     indicates the order in which the device was plugged. *)
  let read_pcidir ~xs domid =
    let path = device_model_pci_device_path xs 0 domid in
    let prefix = "dev-" in
    let is_device = Astring.String.is_prefix ~affix:prefix in
    let all =
      List.filter is_device
        (try xs.Xs.directory path with Xs_protocol.Enoent _ -> [])
    in
    (* The values are the PCI device (domain, bus, dev, func) strings *)
    let device_number_of_string x =
      (* remove the silly prefix *)
      int_of_string
        (String.sub x (String.length prefix)
           (String.length x - String.length prefix)
        )
    in
    let pairs =
      List.map
        (fun x ->
          ( device_number_of_string x
          , Xenops_interface.Pci.address_of_string (xs.Xs.read (path ^ "/" ^ x))
          )
        )
        all
    in
    (* Sort into the order the devices were plugged *)
    List.sort (fun a b -> compare (fst a) (fst b)) pairs

  let encode_bdf pci =
    (pci.Xenops_interface.Pci.domain lsl 16)
    lor ((pci.bus land 0xff) lsl 8)
    lor ((pci.dev land 0x1f) lsl 3)
    lor (pci.fn land 0x7)

  let _quarantine pci quarantine =
    if !Xenopsd.pci_quarantine then
      let pci_bdf = encode_bdf pci in
      let domid = Xenctrlext.domid_quarantine () in
      let xcext = Xenctrlext.get_handle () in
      try
        match quarantine with
        | true ->
            Xenctrlext.assign_device xcext domid pci_bdf 0 ;
            true
        | false ->
            Xenctrlext.deassign_device xcext domid pci_bdf ;
            true
      with
      | Xenctrlext.Unix_error (Unix.ESRCH, _) ->
          false
      | Xenctrlext.Unix_error (Unix.ENODEV, _) ->
          true
      | e ->
          raise e
    else
      true

  let quarantine pci = _quarantine pci true

  let dequarantine pci = _quarantine pci false

  let _pci_add ~xc ~xs ~hvm domid {host; guest= _, guest_addr; qmp_add} =
    let open Xenops_interface.Pci in
    let sysfs_pci_dev = "/sys/bus/pci/devices/" in
    let devfn =
      match guest_addr with None -> None | Some g -> Some (g.dev, g.fn)
    in
    let irq =
      sysfs_pci_dev ^ Pci.string_of_address host ^ "/irq"
      |> Unixext.string_of_file
      |> String.trim
      |> int_of_string
    in
    if hvm && qmp_add then
      if Service.Qemu.is_running ~xs domid then
        let id =
          Printf.sprintf "pci-pt-%02x_%02x.%01x" host.bus host.dev host.fn
        in
        let _qmp_result =
          qmp_send_cmd domid
            (Qmp.Device_add
               {
                 driver= "xen-pci-passthrough"
               ; device=
                   Qmp.Device.PCI
                     {
                       id
                     ; devfn
                     ; hostaddr= string_of_address host
                     ; permissive= false
                     }
               }
            )
        in
        ()
      else
        raise (Domain_not_running (host, domid)) ;
    let addresses =
      sysfs_pci_dev ^ string_of_address host ^ "/resource"
      |> Unixext.string_of_file
      |> String.split_on_char '\n'
    in
    let apply_io_permission i addr =
      if i < _proc_pci_num_resources then
        Scanf.sscanf addr "0x%nx 0x%nx 0x%nx"
        @@ fun scan_start scan_end scan_flags ->
        if scan_start <> 0n then
          let scan_size = Nativeint.(sub scan_end scan_start |> succ) in
          if Nativeint.(logand scan_flags _pci_bar_io > 0n) then
            Xenctrl.domain_ioport_permission xc domid
              (Nativeint.to_int scan_start)
              (Nativeint.to_int scan_size)
              true
          else
            let scan_start = Nativeint.(shift_right_logical scan_start 12) in
            let scan_size =
              Nativeint.(
                shift_right_logical (add _page_size scan_size |> pred) 12
              )
            in
            Xenctrl.domain_iomem_permission xc domid scan_start scan_size true
    in
    List.iteri apply_io_permission addresses ;
    let xcext = Xenctrlext.get_handle () in
    ( if irq > 0 then
        Xenctrlext.physdev_map_pirq xcext domid irq |> fun x ->
        Xenctrl.domain_irq_permission xc domid x true
    ) ;
    ignore (quarantine host) ;
    Xenctrlext.assign_device xcext domid (encode_bdf host)
      _xen_domctl_dev_rdm_relaxed

  let add ~xc ~xs ~hvm pcidevs domid =
    let host_addr {host; guest= _; _} = host in
    try
      if !Xenopsd.use_old_pci_add || not hvm then (
        List.iter (fun x -> ignore (quarantine (host_addr x))) pcidevs ;
        add_xl (List.map host_addr pcidevs) domid
      ) else
        List.iter (_pci_add ~xc ~xs ~hvm domid) pcidevs ;
      List.iter
        (fun {host= pcidev; guest= dev, _; _} ->
          xs.Xs.write
            (Printf.sprintf "%s/dev-%d"
               (device_model_pci_device_path xs 0 domid)
               dev
            )
            (Pci.string_of_address pcidev)
        )
        pcidevs
    with exn ->
      Backtrace.is_important exn ;
      Debug.log_backtrace exn (Backtrace.get exn) ;
      Backtrace.reraise exn (Cannot_add (List.map host_addr pcidevs, exn))

  let release pcidevs domid = release_xl pcidevs domid

  let write_string_to_file file s =
    let fn_write_string fd = Unixext.really_write fd s 0 (String.length s) in
    Unixext.with_file file [Unix.O_WRONLY] 0o640 fn_write_string

  let do_flr device =
    debug "Doing FLR on pci device: %s" device ;
    let doflr = "/sys/bus/pci/drivers/pciback/do_flr" in
    let device_reset_file =
      Printf.sprintf "/sys/bus/pci/devices/%s/reset" device
    in
    let callscript s devstr =
      if Sys.file_exists !Xc_resources.pci_flr_script then
        try
          ignore
            (Forkhelpers.execute_command_get_output
               !Xc_resources.pci_flr_script
               [s; devstr]
            )
        with _ -> ()
    in
    callscript "flr-pre" device ;
    ( if Sys.file_exists device_reset_file then
        try write_string_to_file device_reset_file "1" with _ -> ()
      else
        try write_string_to_file doflr device with _ -> ()
    ) ;
    callscript "flr-post" device

  type supported_driver = I915 | Nvidia | Pciback

  type driver = Supported of supported_driver | Unsupported of string

  let string_of_driver = function
    | Supported I915 ->
        "i915"
    | Supported Nvidia ->
        "nvidia"
    | Supported Pciback ->
        "pciback"
    | Unsupported driver ->
        driver

  let driver_of_string = function
    | "i915" ->
        Supported I915
    | "nvidia" ->
        Supported Nvidia
    | "pciback" ->
        Supported Pciback
    | driver ->
        Unsupported driver

  let sysfs_devices = "/sys/bus/pci/devices"

  let sysfs_drivers = "/sys/bus/pci/drivers"

  let sysfs_i915 = Filename.concat sysfs_drivers "i915"

  let sysfs_nvidia = Filename.concat sysfs_drivers "nvidia"

  let sysfs_pciback = Filename.concat sysfs_drivers "pciback"

  let ( // ) = Filename.concat

  let get_driver devstr =
    try
      let sysfs_device = Filename.concat sysfs_devices devstr in
      Some
        (Filename.concat sysfs_device "driver"
        |> Unix.readlink
        |> Filename.basename
        |> driver_of_string
        )
    with _ -> None

  let bind_to_pciback devstr =
    debug "pci: binding device %s to pciback" devstr ;
    let new_slot = Filename.concat sysfs_pciback "new_slot" in
    let bind = Filename.concat sysfs_pciback "bind" in
    write_string_to_file new_slot devstr ;
    write_string_to_file bind devstr

  let unbind_from_pciback devstr =
    let rm_slot = sysfs_pciback // "remove_slot" in
    let unbind = sysfs_pciback // "unbind" in
    debug "pci: unbinding device %s from pciback" devstr ;
    write_string_to_file unbind devstr ;
    write_string_to_file rm_slot devstr

  let bind_to_i915 devstr =
    debug "pci: binding device %s to i915" devstr ;
    let is_loaded =
      Unixext.file_lines_fold
        (fun loaded line ->
          loaded
          ||
          match Astring.String.cut ~sep:" " line with
          | Some ("i915", _) ->
              true
          | _ ->
              false
        )
        false "/proc/modules"
    in
    if not is_loaded then
      ignore
        (Forkhelpers.execute_command_get_output !Resources.modprobe ["i915"]) ;
    match get_driver devstr with
    | None ->
        write_string_to_file (Filename.concat sysfs_i915 "bind") devstr
    | Some (Supported I915) ->
        ()
    | Some drv ->
        internal_error "Fail to bind to i915, device is bound to %s"
          (string_of_driver drv)

  let unbind devstr driver =
    let driverstr = string_of_driver driver in
    debug "pci: unbinding device %s from %s" devstr driverstr ;
    let sysfs_driver = Filename.concat sysfs_drivers driverstr in
    let unbind = Filename.concat sysfs_driver "unbind" in
    write_string_to_file unbind devstr

  let unbind_from_i915 devstr =
    unbind devstr (Supported I915) ;
    let (_ : string * string) =
      Forkhelpers.execute_command_get_output !Resources.rmmod ["i915"]
    in
    ()

  let procfs_nvidia = "/proc/driver/nvidia/gpus"

  let nvidia_smi = "/usr/bin/nvidia-smi"

  let nvidia_manage = "/usr/lib/nvidia/sriov-manage"

  (** [num_vfs devstr] returns the number of PCI VFs of [devstr] or 0 if
      [devstr] is not an SRIOV device *)
  let num_vfs devstr =
    let path = sysfs_devices // devstr // "sriov_numvfs" in
    try Some (Unixext.string_of_file path |> String.trim |> int_of_string) with
    | Unix.(Unix_error (ENOENT, _, _)) ->
        debug "File %s does not exist - assuming no SRIOV devices in use" path ;
        None
    | exn ->
        internal_error "Can't read %s to reset Nvidia GPU %s: %s" path devstr
          (Printexc.to_string exn)

  (** [vfs_of device] returns the PCI addresses of the virtual functions of PCI
      [device]. We find each virtual function by looking at the virtfnX symlink
      in [device]. *)
  let vfs_of devstr =
    let virtfn n =
      let path = sysfs_devices // devstr // Printf.sprintf "virtfn%d" n in
      try Some (path |> Unix.readlink |> Filename.basename)
      with exn ->
        debug "Can't read %s of PCI %s: %s" path devstr (Printexc.to_string exn) ;
        None
    in
    let seq n = List.init n (fun x -> n - 1 - x) in
    match num_vfs devstr with
    | Some n when n > 0 ->
        seq n |> List.filter_map virtfn
    | _ ->
        []

  (** [deactivate_nvidia_sriov devstr] deactivates SRIOV PCI VFs of [devstr] if
      necessary. This needs to be called for NVidia GPUs before using [devstr]
      as a pass-through GPU. *)
  let deactivate_nvidia_sriov devstr =
    let cmd = nvidia_manage in
    let args = ["-d"; devstr] in
    let has_pciback devstr = get_driver devstr = Some (Supported Pciback) in
    match vfs_of devstr with
    | [] ->
        debug "No need to deactivate NVidia vGPUs %s (it has 0 VFs)" devstr
    | vfs when Sys.file_exists cmd ->
        vfs |> List.filter has_pciback |> List.iter unbind_from_pciback ;
        debug "About to deactivate NVidia vGPUs %s by calling %s" devstr cmd ;
        let out, _ = Forkhelpers.execute_command_get_output cmd args in
        debug "Deactivating NVidia vGPUs %s yielded: '%s'" devstr
          (String.escaped out)
    | _ ->
        internal_error "Can't find %s to reset NVidia GPU %s" cmd devstr

  let bind_to_nvidia devstr =
    debug "pci: binding device %s to nvidia" devstr ;
    let bind = Filename.concat sysfs_nvidia "bind" in
    write_string_to_file bind devstr

  let unbind_from_nvidia devstr =
    debug "pci: attempting to lock device %s before unbinding from nvidia"
      devstr ;
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
          let devstr2 =
            String.mapi (fun i c -> if i = 7 then '.' else c) devstr
          in
          if
            false
            || Astring.String.is_infix ~affix:devstr2 gpu_info
            || Astring.String.is_infix ~affix:devstr gpu_info
          then
            gpu_path
          else
            find_gpu rest
    in
    deactivate_nvidia_sriov devstr ;
    (* Disable persistence mode on the device before unbinding it. In future it
       might be worth augmenting gpumon so that it can do this, and to enable
       xapi and/or xenopsd to tell it to do so. *)
    let (_ : string * string) =
      Forkhelpers.execute_command_get_output nvidia_smi
        ["--id=" ^ devstr; "--persistence-mode=0"]
    in
    let unbind_lock_path =
      Filename.concat (find_gpu (Array.to_list gpus)) "unbindLock"
    in
    (* Grab the unbind lock. *)
    write_string_to_file unbind_lock_path "1\n" ;
    (* Unbind if we grabbed the lock; fail otherwise. *)
    if Unixext.string_of_file unbind_lock_path = "1\n" then
      unbind devstr (Supported Nvidia)
    else
      failwith (Printf.sprintf "Couldn't lock GPU with device ID %s" devstr)

  let bind_lock = Mutex.create ()

  let bind devices new_driver =
    let bind_to devstr = function
      | I915 ->
          bind_to_i915 devstr
      | Nvidia ->
          bind_to_nvidia devstr
      | Pciback ->
          bind_to_pciback devstr ; do_flr devstr
    in
    let unbind_from devstr = function
      | Supported I915 ->
          unbind_from_i915 devstr
      | Supported Nvidia ->
          unbind_from_nvidia devstr
      | Supported Pciback ->
          unbind_from_pciback devstr
      | driver ->
          unbind devstr driver
    in
    with_lock bind_lock (fun () ->
        List.iter
          (fun device ->
            let devstr = Xenops_interface.Pci.string_of_address device in
            let old_driver = get_driver devstr in
            match (old_driver, new_driver) with
            (* We want the driver which is already bound. *)
            | Some (Supported I915), I915 ->
                debug "pci: device %s already bound to i915; doing nothing"
                  devstr
            | Some (Supported Nvidia), Nvidia ->
                debug "pci: device %s already bound to nvidia; doing nothing"
                  devstr
            | Some (Supported Pciback), Pciback ->
                debug "pci: device %s already bound to pciback; doing flr"
                  devstr ;
                do_flr devstr
            (* No driver is bound, so just bind the one we want. *)
            | None, new_driver ->
                debug "pci: device %s not bound" devstr ;
                bind_to devstr new_driver
            (* Unbinding from one driver and binding to another driver. *)
            | Some old_driver, new_driver ->
                unbind_from devstr old_driver ;
                bind_to devstr new_driver
          )
          devices
    )

  let enumerate_devs ~xs (x : device) =
    let backend_path = backend_path_of_device ~xs x in
    let num =
      try int_of_string (xs.Xs.read (backend_path ^ "/num_devs")) with _ -> 0
    in
    let devs = Array.make num None in
    for i = 0 to num do
      try
        let devstr = xs.Xs.read (backend_path ^ "/dev-" ^ string_of_int i) in
        let dev = Xenops_interface.Pci.address_of_string devstr in
        devs.(i) <- Some dev
      with _ -> ()
    done ;
    List.rev
      (List.fold_left
         (fun acc dev -> match dev with None -> acc | Some dev -> dev :: acc)
         [] (Array.to_list devs)
      )

  let reset ~xs:_ address =
    let devstr = Xenops_interface.Pci.string_of_address address in
    debug "Device.Pci.reset %s" devstr ;
    do_flr devstr

  let clean_shutdown (_ : Xenops_task.task_handle) ~xs (x : device) =
    debug "Device.Pci.clean_shutdown %s" (string_of_device x) ;
    let devs = enumerate_devs ~xs x in
    try release devs x.frontend.domid with _ -> ()

  let hard_shutdown (task : Xenops_task.task_handle) ~xs (x : device) =
    debug "Device.Pci.hard_shutdown %s" (string_of_device x) ;
    clean_shutdown task ~xs x

  (* Return a list of PCI devices *)
  let list = read_pcidir
end

module Vfs = struct
  let add ~xc:_ ~xs ?(backend_domid = 0) domid =
    debug "Device.Vfs.add domid=%d" domid ;
    let frontend = {domid; kind= Vfs; devid= 0} in
    let backend = {domid= backend_domid; kind= Vfs; devid= 0} in
    let _ = {backend; frontend} in
    let frontend_path =
      Printf.sprintf "/local/domain/%d/device/vfs/%d" domid 0
    in
    let backend_path =
      Printf.sprintf "/local/domain/%d/backend/vfs/%d" backend_domid domid
    in
    let request_path =
      Printf.sprintf "/local/domain/%d/backend/vfs/exports/requests/%d"
        backend_domid domid
    in
    (* TODO also have a backend?! *)
    let front =
      [
        ("state", "ready")
      ; (* definitely needs to be "ready" *)
        ("backend", backend_path)
      ]
    in
    Xs.transaction xs (fun t ->
        (* Add the frontend *)
        let perms =
          Xs_protocol.ACL.{owner= domid; other= NONE; acl= [(0, READ)]}
        in
        t.Xst.mkdirperms frontend_path perms ;
        t.Xst.writev frontend_path front ;
        (* Now make the request *)
        let perms = Xs_protocol.ACL.{owner= domid; other= NONE; acl= []} in
        let request_path = Printf.sprintf "%s/%d" request_path 0 in
        t.Xst.mkdirperms request_path perms ;
        t.Xst.write (request_path ^ "/frontend") frontend_path
    ) ;
    ()

  let hard_shutdown (_ : Xenops_task.task_handle) ~xs:_ (x : device) =
    debug "Device.Vfs.hard_shutdown %s" (string_of_device x) ;
    ()

  let clean_shutdown (_ : Xenops_task.task_handle) ~xs:_ (x : device) =
    debug "Device.Vfs.clean_shutdown %s" (string_of_device x) ;
    ()
end

module Vfb = struct
  let add ~xc:_ ~xs ?(backend_domid = 0) ?(protocol = Protocol_Native) domid =
    debug "Device.Vfb.add %d" domid ;
    let frontend = {domid; kind= Vfb; devid= 0} in
    let backend = {domid= backend_domid; kind= Vfb; devid= 0} in
    let device = {backend; frontend} in
    let back =
      [
        ("frontend-id", sprintf "%u" domid)
      ; ("online", "1")
      ; ("state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising))
      ]
    in
    let front =
      [
        ("backend-id", string_of_int backend_domid)
      ; ("protocol", string_of_protocol protocol)
      ; ("state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising))
      ]
    in
    Generic.add_device ~xs device back front [] [] ;
    ()

  let hard_shutdown (_ : Xenops_task.task_handle) ~xs:_ (x : device) =
    debug "Device.Vfb.hard_shutdown %s" (string_of_device x) ;
    ()

  let clean_shutdown (_ : Xenops_task.task_handle) ~xs:_ (x : device) =
    debug "Device.Vfb.clean_shutdown %s" (string_of_device x) ;
    ()
end

module Vkbd = struct
  let add ~xc:_ ~xs ?(backend_domid = 0) ?(protocol = Protocol_Native) domid =
    debug "Device.Vkbd.add %d" domid ;
    let frontend = {domid; kind= Vkbd; devid= 0} in
    let backend = {domid= backend_domid; kind= Vkbd; devid= 0} in
    let device = {backend; frontend} in
    let back =
      [
        ("frontend-id", sprintf "%u" domid)
      ; ("online", "1")
      ; ("state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising))
      ]
    in
    let front =
      [
        ("backend-id", string_of_int backend_domid)
      ; ("protocol", string_of_protocol protocol)
      ; ("state", string_of_int (Xenbus_utils.int_of Xenbus_utils.Initialising))
      ]
    in
    Generic.add_device ~xs device back front [] [] ;
    ()

  let hard_shutdown (_ : Xenops_task.task_handle) ~xs:_ (x : device) =
    debug "Device.Vkbd.hard_shutdown %s" (string_of_device x) ;
    ()

  let clean_shutdown (_ : Xenops_task.task_handle) ~xs:_ (x : device) =
    debug "Device.Vkbd.clean_shutdown %s" (string_of_device x) ;
    ()
end

module Vusb = struct
  let exec_usb_reset_script argv =
    try
      let stdout, stderr =
        Forkhelpers.execute_command_get_output
          !Xc_resources.usb_reset_script
          argv
      in
      debug "usb_reset script %s returned stdout=%s stderr=%s"
        (String.concat " " argv) stdout stderr
    with err ->
      error "Failed to call usb_reset script with arguments %s"
        (String.concat " " argv) ;
      internal_error "Call to usb reset failed: %s" (Printexc.to_string err)

  let cleanup domid =
    try
      let cmd = ["cleanup"; "-d"; string_of_int domid] in
      exec_usb_reset_script cmd
    with err ->
      warn "Failed to clean up VM %s: %s" (string_of_int domid)
        (Printexc.to_string err)

  let usb_reset_attach ~hostbus ~hostport ~domid ~pid ~privileged =
    let argv =
      List.concat
        [
          [
            "attach"
          ; sprintf "%s-%s" hostbus hostport
          ; "-d"
          ; string_of_int domid
          ; "-p"
          ; string_of_int pid
          ]
        ; (if privileged then ["-r"] else [])
        ]
    in
    exec_usb_reset_script argv

  let usb_reset_detach ~hostbus ~hostport ~domid ~privileged =
    if not privileged then
      let argv =
        ["detach"; hostbus ^ "-" ^ hostport; "-d"; string_of_int domid]
      in
      exec_usb_reset_script argv

  let qom_list ~xs ~domid =
    (*. 1. The QEMU Object Model(qom) provides a framework for registering user
      creatable types and instantiating objects from those types. 2. Qom types
      can be instantiated and configured directly from the QEMU monitor or
      command-line (eg,-device, device_add). 3. Command example:
      {"execute":"qom-list","arguments":{"path":"/machine/peripheral"}} result:
      {"return": [{"name": "usb1", "type": "child<usb-host>"}, {"name":"type",
      "type": "string"}} The usb1 is added. *)
    if Service.Qemu.is_running ~xs domid then
      let path = "/machine/peripheral" in
      match qmp_send_cmd domid Qmp.(Qom_list path) with
      | Qmp.(Qom usbs) ->
          List.map (fun p -> p.Qmp.name) usbs
      | _other ->
          debug "%s unexpected QMP result for domid %d Qom_list" __LOC__ domid ;
          []
      | exception QMP_connection_error _ ->
          raise (Xenopsd_error Device_not_connected)
    else
      []

  let vusb_plug ~xs ~privileged ~domid ~id ~hostbus ~hostport ~version ~speed =
    debug "vusb_plug: plug VUSB device %s" id ;
    let get_bus () =
      let vusb_controller_plug ~driver ~driver_id =
        (* requires Qemu.is_running *)
        if not (List.mem driver_id (qom_list ~xs ~domid)) then
          qmp_send_cmd domid
            Qmp.(
              Device_add
                Device.{driver; device= USB {USB.id= driver_id; params= None}}
            )
          |> ignore
      in
      let usb_bus0 = ("usb-bus.0", fun () -> ()) in
      let ehci0 =
        ( "ehci.0"
        , fun () -> vusb_controller_plug ~driver:"usb-ehci" ~driver_id:"ehci"
        )
      in
      let speed_of_float x =
        if x <= 0. then
          `Unknown
        else if x <= 1.5 then
          `Low (* v1.0 *)
        else if x <= 12. then
          `Full (* v1.1 *)
        else if x <= 480. then
          `High (* v2.0 *)
        else if x <= 5000. then
          `Super (* v3.0 *)
        else
          `Unknown
      in
      let bus_from_speed =
        match speed_of_float speed with
        | `Unknown ->
            None
        | `Low | `Full ->
            Some usb_bus0
        | `High | `Super ->
            Some ehci0
      in
      let get_bus_from_version () =
        let major_version =
          Scanf.sscanf version "%d.%d" (fun major _minor -> major)
        in
        match major_version with 1 -> usb_bus0 | _ -> ehci0
      in
      match bus_from_speed with
      | Some x ->
          x
      | None ->
          D.warn
            "vusb_plug: failed to get bus from usb speed: %f, for VUSB device \
             %s. getting bus from version"
            speed id ;
          get_bus_from_version ()
    in
    match Service.Qemu.pid ~xs domid with
    | Some pid ->
        (* Need to reset USB device before passthrough to vm according to
           CP-24616. Also need to do deprivileged work in usb_reset script if QEMU
           is deprivileged. *)
        let bus, prepare_bus = get_bus () in
        prepare_bus () ;
        usb_reset_attach ~hostbus ~hostport ~domid ~pid ~privileged ;

        let cmd =
          Qmp.(
            Device_add
              Device.
                {
                  driver= "usb-host"
                ; device= USB {USB.id; params= Some {bus; hostbus; hostport}}
                }
          )
        in
        qmp_send_cmd domid cmd |> ignore
    | None ->
        ()

  let vusb_unplug ~xs ~privileged ~domid ~id ~hostbus ~hostport =
    debug "vusb_unplug: unplug VUSB device %s" id ;
    finally
      (fun () ->
        if Service.Qemu.is_running ~xs domid then
          try qmp_send_cmd domid Qmp.(Device_del id) |> ignore
          with QMP_connection_error _ ->
            raise (Xenopsd_error Device_not_connected)
      )
      (fun () -> usb_reset_detach ~hostbus ~hostport ~domid ~privileged)
end

module Serial : sig
  val update_xenstore : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit
end = struct
  let tty_prefix = "pty:"

  let tty_path domid = Printf.sprintf "/local/domain/%d/serial/0/tty" domid

  let strip n str = String.sub str n (String.length str - n)

  let is_serial0 device =
    device.Qmp.label = "serial0"
    && Astring.String.is_prefix ~affix:tty_prefix device.Qmp.filename

  let find_serial0 domid =
    match qmp_send_cmd domid Qmp.Query_chardev with
    | Qmp.(Char_devices devices) ->
        List.find_opt is_serial0 devices
    | _other ->
        warn "Unexpected QMP result for domid %d query-chardev" domid ;
        None

  (** query qemu for the serial console and write it to xenstore. Only write
      path for a real console, not a file or socket path. CA-318579 *)
  let update_xenstore ~xs domid =
    if not @@ Service.Qemu.is_running ~xs domid then
      internal_error "Qemu not running for domain %d (%s)" domid __LOC__ ;
    match find_serial0 domid with
    | Some device ->
        let path = strip (String.length tty_prefix) device.Qmp.filename in
        xs.Xs.write (tty_path domid) path
    | None ->
        debug "no serial device for domain %d found" domid
    | exception e ->
        debug "Can't probe serial0 device for domid %d: %s" domid
          (Printexc.to_string e)
end

let can_surprise_remove ~xs (x : device) = Generic.can_surprise_remove ~xs x

(** Dm_Common contains the private Dm functions that are common between the qemu
    profile backends. *)
module Dm_Common = struct
  (* An example one: [/usr/lib/xen/bin/qemu-dm -d 39 -m 256 -boot cd -serial pty
     -usb -usbdevice tablet -domain-name bee94ac1-8f97-42e0-bf77-5cb7a6b664ee
     -net nic,vlan=1,macaddr=00:16:3E:76:CE:44,model=rtl8139 -net
     tap,vlan=1,bridge=xenbr0 -vnc 39 -k en-us -vnclisten 127.0.0.1] *)

  type usb_opt = Enabled of (string * int) list | Disabled

  (* How the display appears to the guest *)
  type disp_intf_opt =
    | Std_vga
    | Cirrus
    | Vgpu of Xenops_interface.Vgpu.t list
    | GVT_d

  (* Display output / keyboard input *)
  type disp_opt =
    | NONE
    | VNC of disp_intf_opt * string option * bool * int * string option
    (* IP address, auto-allocate, port if previous false, keymap *)
    | SDL of disp_intf_opt * string

  (* X11 display *)

  module Media = struct
    type t = Disk | Cdrom | Floppy

    let to_string = function
      | Disk ->
          "disk"
      | Cdrom ->
          "cdrom"
      | Floppy ->
          "floppy"

    let format_of media file =
      match (media, file) with
      | Disk, _ ->
          ["format=raw"]
      | Cdrom, "" ->
          []
      | Cdrom, _ ->
          ["format=raw"]
      | Floppy, _ ->
          ["format=raw"]

    let lba_of = function
      | Disk ->
          ["bios-chs-trans=forcelba"]
      | Cdrom ->
          []
      | Floppy ->
          []

    let device_driver_of = function
      | Disk ->
          "ide-hd"
      | Cdrom ->
          "ide-cd"
      | Floppy ->
          ""

    let readonly_of = function
      | Disk ->
          []
      | Cdrom ->
          ["read-only=on"]
      | Floppy ->
          []
  end

  type info = {
      memory: int64
    ; boot: string
    ; firmware: Xenops_types.Vm.firmware_type
    ; serial: string option
    ; monitor: string option
    ; vcpus: int
    ; (* vcpus max *)
      vcpus_current: int
    ; usb: usb_opt
    ; parallel: string option
    ; nics: (string * string * int) list
    ; disks: (int * string * Media.t) list
    ; acpi: bool
    ; disp: disp_opt
    ; pci_emulations: string list
    ; pci_passthrough: bool
    ; video_mib: int
    ; tpm: Xenops_types.Vm.tpm option
    ; xen_platform: (int * int) option
    ; extras: (string * string option) list
  }

  type qemu_args = {
      argv: string list  (** command line args *)
    ; fd_map: (string * Unix.file_descr) list  (** open files *)
  }

  let vnc_socket_path = (sprintf "%s/vnc-%d") Device_common.var_run_xen_path

  let efivars_resume_path = Service.Varstored.efivars_resume_path

  let efivars_save_path = Service.Varstored.efivars_save_path

  let get_vnc_port ~xs domid ~f =
    match Service.Qemu.is_running ~xs domid with true -> f () | false -> None

  let get_tc_port ~xs domid =
    if not (Service.Qemu.is_running ~xs domid) then
      None
    else
      try Some (int_of_string (xs.Xs.read (Service.PV_Vnc.tc_port_path domid)))
      with _ -> None

  let signal (task : Xenops_task.task_handle) ~xs ~qemu_domid ~domid ?wait_for
      ?param cmd =
    let cmdpath = device_model_path ~qemu_domid domid in
    Xs.transaction xs (fun t ->
        t.Xst.write (cmdpath ^ "/command") cmd ;
        match param with
        | None ->
            ()
        | Some param ->
            t.Xst.write (cmdpath ^ "/parameter") param
    ) ;
    match wait_for with
    | Some state ->
        let pw = cmdpath ^ "/state" in
        (* MTC: The default timeout for this operation was 20mins, which is way
           too long for our software to recover successfully. Talk to Citrix
           about this *)
        let cancel = Qemu (qemu_domid, domid) in
        let (_ : bool) =
          cancellable_watch cancel
            [Watch.value_to_become pw state]
            [] task ~xs ~timeout:30. ()
        in
        ()
    | None ->
        ()

  let cmdline_of_disp ?domid info =
    let vga_type_opts x =
      let open Xenops_interface.Vgpu in
      (* We can match on the implementation details to detect the VCS
         case. Don't pass -vgpu for a compute vGPU. *)
      match x with
      | Vgpu ({implementation= Nvidia {vclass= Some "Compute"; _}; _} :: _) ->
          ["-std-vga"]
      | Vgpu ({implementation= Nvidia _; _} :: _) ->
          ["-vgpu"]
      | Vgpu [{implementation= GVT_g gvt_g; _}] ->
          let base_opts =
            [
              "-xengt"
            ; "-vgt_low_gm_sz"
            ; Int64.to_string gvt_g.low_gm_sz
            ; "-vgt_high_gm_sz"
            ; Int64.to_string gvt_g.high_gm_sz
            ; "-vgt_fence_sz"
            ; Int64.to_string gvt_g.fence_sz
            ]
          and priv_opt = ["-priv"] in
          List.flatten [base_opts; priv_opt]
      | Vgpu [{implementation= MxGPU _; _}] ->
          []
      | Vgpu _ ->
          failwith "Unsupported vGPU configuration"
      | Std_vga ->
          ["-std-vga"]
      | Cirrus ->
          []
      | GVT_d ->
          ["-std-vga"]
      (* relies on pci-passthrough *)
    in
    let videoram_opt = ["-videoram"; string_of_int info.video_mib] in
    let vnc_opts_of ip_addr_opt auto port keymap ~domid =
      let ip_addr = Option.value ~default:"127.0.0.1" ip_addr_opt in
      let unused_opt, vnc_arg =
        match domid with
        | None when auto ->
            (["-vncunused"], Printf.sprintf "%s:1" ip_addr)
        | None ->
            ([], Printf.sprintf "%s:%d" ip_addr port)
            (* Disable lock-key-sync

               # lock-key-sync expects vnclient to send different keysym for

               # alphabet keys (different for lowercase and uppercase). XC

               # can't do it at the moment, so disable lock-key-sync *)
        | Some domid ->
            ( []
            , Printf.sprintf "%s,lock-key-sync=off"
                (Socket.Unix.path (vnc_socket_path domid))
            )
      in
      let vnc_opt = ["-vnc"; vnc_arg] in
      let keymap_opt = match keymap with Some k -> ["-k"; k] | None -> [] in
      List.flatten [unused_opt; vnc_opt; keymap_opt]
    in
    let disp_options, wait_for_port =
      match info.disp with
      | NONE ->
          ([], false)
      | SDL (_, _) ->
          ([], false)
      | VNC (disp_intf, ip_addr_opt, auto, port, keymap) ->
          let vga_type_opts = vga_type_opts disp_intf in
          let vnc_opts = vnc_opts_of ip_addr_opt auto port keymap ~domid in
          (vga_type_opts @ videoram_opt @ vnc_opts, true)
    in
    (disp_options, wait_for_port)

  let qemu_args ~xs:_ ~dm:_ info restore ?(domid_for_vnc = false) domid =
    let restorefile = sprintf qemu_restore_path domid in
    let disp_options, _ =
      if domid_for_vnc then
        cmdline_of_disp info ~domid
      else
        cmdline_of_disp info
    in
    let argv =
      List.concat
        [
          disp_options
        ; (info.acpi |> function false -> [] | true -> ["-acpi"])
        ; (restore |> function false -> [] | true -> ["-loadvm"; restorefile])
        ; info.pci_emulations
          |> List.map (fun pci -> ["-pciemulation"; pci])
          |> List.concat
        ; (info.pci_passthrough |> function false -> [] | true -> ["-priv"])
        ; List.rev info.extras
          |> List.map (function
               | k, None ->
                   ["-" ^ k]
               | k, Some v ->
                   ["-" ^ k; v]
               )
          |> List.concat
        ; (info.monitor |> function None -> [] | Some x -> ["-monitor"; x])
        ; ["-pidfile"; Service.Qemu.pidfile_path domid]
        ]
    in
    {argv; fd_map= []}

  let write_vgpu_data ~xs domid devid keys =
    let path = xenops_vgpu_path domid devid in
    xs.Xs.writev path keys

  let mxgpu_device_in_use ~xs physical_function =
    (* Check if there is a /xenops/domain/<x>/device/vgpu/<y>/pf xenstore node
       that is equal to the given physical_function. *)
    let root = Device_common.xenops_domain_path in
    try
      (* NB: The response size of this directory call may exceed the default
         payload size limit. However, we have an exception that allows oversized
         packets. *)
      xs.Xs.directory root
      |> List.map (fun domid ->
             let path = Printf.sprintf "%s/%s/device/vgpu" root domid in
             try List.map (fun x -> path ^ "/" ^ x) (xs.Xs.directory path)
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
      info "Initialising MxGPU PF: %s %s" !Xc_resources.gimtool
        (String.concat " " args) ;
      ignore (Forkhelpers.execute_command_get_output !Xc_resources.gimtool args)
    with _ ->
      error "Failed to initialise MxGPU PF (%s %s)" !Xc_resources.gimtool
        (String.concat " " args) ;
      failwith "Call to gimtool failed"

  let configure_gim ~xs physical_function vgpus_per_gpu framebufferbytes =
    let pf = Xenops_interface.Pci.string_of_address physical_function in
    (* gimtool must (only) be called when no VM is using the PF yet *)
    if not (mxgpu_device_in_use ~xs pf) then
      let vgpus = Int64.to_string vgpus_per_gpu in
      let fb =
        Int64.to_string (Int64.div framebufferbytes Memory.bytes_per_mib)
      in
      call_gimtool ["--bdf"; pf; "--num_vfs"; vgpus; "--vf_fb_size"; fb]
    else
      info "MxGPU PF %s already initialised" pf

  let prepend_wrapper_args domid args =
    string_of_int domid :: "--syslog" :: args

  let gimtool_m = Mutex.create ()

  let resume (task : Xenops_task.task_handle) ~xs ~qemu_domid domid =
    signal task ~xs ~qemu_domid ~domid "continue" ~wait_for:"running"

  (* Called by every domain destroy, even non-HVM *)
  let stop ~xs ~qemu_domid ~vtpm domid =
    let vm_uuid = Xenops_helpers.uuid_of_domid ~xs domid |> Uuidx.to_string in
    let dbg = Printf.sprintf "stop domid %d" domid in
    let stop_qemu () = Service.Qemu.stop ~xs ~qemu_domid domid in
    let stop_swptm () =
      Option.iter
        (fun (Xenops_interface.Vm.Vtpm vtpm_uuid) ->
          Service.Swtpm.stop dbg ~xs ~domid ~vm_uuid ~vtpm_uuid
        )
        vtpm ;
      Xenops_sandbox.Swtpm_guard.stop dbg ~domid ~vm_uuid
    in
    let stop_vgpu () = Service.Vgpu.stop ~xs domid in
    let stop_varstored () =
      debug "About to stop varstored for domain %d (%s)" domid vm_uuid ;
      Service.Varstored.stop ~xs domid ;
      Xenops_sandbox.Varstore_guard.stop dbg ~domid ~vm_uuid
    in
    stop_vgpu () ; stop_varstored () ; stop_qemu () ; stop_swptm ()

  type disk_type_args = int * string * Media.t -> string list

  let ide = "ide"

  let ide_device_of ~trad_compat (index, file, media) =
    let id =
      sprintf "ide%d-%s%d" (index / 2)
        (match media with Media.Cdrom -> "cd" | _ -> "hd")
        (index mod 2)
    in
    [
      "-drive"
    ; String.concat ","
        (List.concat
           [
             [sprintf "file=%s" file; "if=none"; sprintf "id=%s" id]
           ; (if file <> "" then ["auto-read-only=off"] else [])
           ; Media.readonly_of media
           ; Media.format_of media file
           ]
        )
    ; "-device"
    ; String.concat ","
        (List.concat
           [
             [
               Media.device_driver_of media
             ; sprintf "drive=%s" id
             ; sprintf "bus=ide.%d" (index / 2)
             ; sprintf "unit=%d" (index mod 2)
             ]
           ; (if trad_compat then Media.lba_of media else [])
           ]
        )
    ]

  let nvme = "nvme"

  let nvme_device_of (index, file, media) =
    let id = sprintf "disk%d" index in
    [
      "-drive"
    ; String.concat ","
        ([
           sprintf "id=%s" id
         ; "if=none"
         ; sprintf "file=%s" file
         ; sprintf "media=%s" (Media.to_string media)
         ]
        @ (if file <> "" then ["auto-read-only=off"] else [])
        @ Media.format_of media file
        )
    ; "-device"
    ; String.concat ","
        [
          "nvme-ns"
        ; sprintf "drive=%s" id
        ; "bus=nvme0"
        ; sprintf "nsid=%d" (index + 1)
        ]
    ]

  let xen_platform ~trad_compat ~xs:_ ~domid:_ ~info =
    [
      "-device"
    ; String.concat ","
        (List.concat
           [
             ["xen-platform"; "addr=3"]
           ; ( if trad_compat then
                 match info.xen_platform with
                 | Some (device_id, revision) ->
                     [
                       sprintf "device-id=0x%04x" device_id
                     ; sprintf "revision=0x%x" revision
                     ; "class-id=0x0100"
                     ; "subvendor_id=0x5853"
                     ; sprintf "subsystem_id=0x%04x" device_id
                     ]
                 | None ->
                     []
               else
                 match info.xen_platform with
                 | Some (device_id, _) ->
                     [sprintf "device-id=0x%04x" device_id]
                 | None ->
                     []
             )
           ]
        )
    ]

  let cant_suspend_reason_path domid =
    sprintf "/local/domain/%d/data/cant_suspend_reason" domid
end

(* End of module Dm_Common *)

(** Implementation of the qemu profile backends *)
module Backend = struct
  (** Common signature for all the profile backends *)
  module type Intf = sig
    (** Vgpu functions that use the dispatcher to choose between different
        profile and device-model backends *)
    module Vgpu : sig
      val device : index:int -> int option
    end

    (** Vbd functions that use the dispatcher to choose between different
        profile backends *)
    module Vbd : sig
      val qemu_media_change :
        xs:Xenstore.Xs.xsh -> device -> string -> string -> unit
    end

    (** Vcpu functions that use the dispatcher to choose between different
        profile backends *)
    module Vcpu : sig
      val add : xs:Xenstore.Xs.xsh -> devid:int -> int -> bool -> unit

      val set : xs:Xenstore.Xs.xsh -> devid:int -> int -> bool -> unit

      val del : xs:Xenstore.Xs.xsh -> devid:int -> int -> unit

      val status : xs:Xenstore.Xs.xsh -> devid:int -> int -> bool
    end

    (** Dm functions that use the dispatcher to choose between different profile
        backends *)
    module Dm : sig
      val get_vnc_port : xs:Xenstore.Xs.xsh -> int -> Socket.t option
      (** [get_vnc_port xenstore domid] returns the dom0 tcp port in which the
          vnc server for [domid] can be found *)

      val assert_can_suspend : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit
      (** [assert_can_suspend xenstore xc] checks whether suspending is
          prevented by QEMU *)

      val suspend :
           Xenops_task.task_handle
        -> xs:Xenstore.Xs.xsh
        -> qemu_domid:int
        -> Xenctrl.domid
        -> unit
      (** [suspend task xenstore qemu_domid xc] suspends a domain *)

      val init_daemon :
           task:Xenops_task.task_handle
        -> path:string
        -> args:string list
        -> domid:int
        -> xs:Xenstore.Xs.xsh
        -> ready_path:Watch.path
        -> timeout:float
        -> cancel:Cancel_utils.key
        -> ?fds:(string * Unix.file_descr) list
        -> 'a
        -> Forkhelpers.pidty
      (** [init_daemon task path args domid xenstore ready_path timeout cancel]
          returns a forkhelper pid after starting the qemu daemon in dom0 *)

      val stop :
           xs:Xenstore.Xs.xsh
        -> qemu_domid:int
        -> vtpm:Xenops_interface.Vm.tpm option
        -> int
        -> unit
      (** [stop xenstore qemu_domid domid] stops a domain *)

      val qemu_args :
           xs:Xenstore.Xs.xsh
        -> dm:Profile.t
        -> Dm_Common.info
        -> bool
        -> int
        -> Dm_Common.qemu_args
      (** [cmdline_of_info xenstore info restore domid] creates the command line
          arguments to pass to the qemu wrapper script *)

      val after_suspend_image :
           xs:Xenstore.Xs.xsh
        -> qemu_domid:int
        -> vtpm:Xenops_interface.Vm.tpm option
        -> int
        -> unit
      (** [after_suspend_image xs qemu_domid domid] hook to execute actions
          after the suspend image has been created *)

      val pci_assign_guest :
           xs:Xenstore.Xs.xsh
        -> index:int
        -> host:Pci.address
        -> Pci.address option
    end
  end

  (** Implementation of the backend common signature for the qemu-none (PV)
      backend *)
  module Qemu_none : Intf = struct
    module Vgpu = struct let device ~index:_ = None end

    (** Implementation of the Vbd functions that use the dispatcher for the
        qemu-none backend *)
    module Vbd = struct
      let qemu_media_change = Vbd_Common.qemu_media_change
    end

    (** Implementation of the Vcpu functions that use the dispatcher for the
        qemu-none backend *)
    module Vcpu = struct
      let add = Vcpu_Common.add

      let set = Vcpu_Common.set

      let del = Vcpu_Common.del

      let status = Vcpu_Common.status
    end

    (** Implementation of the Dm functions that use the dispatcher for the
        qemu-none backend *)
    module Dm = struct
      let get_vnc_port ~xs domid =
        Dm_Common.get_vnc_port ~xs domid ~f:(fun () ->
            try
              Some
                (Socket.Port
                   (int_of_string
                      (xs.Xs.read (Service.PV_Vnc.vnc_port_path domid))
                   )
                )
            with _ -> None
        )

      let assert_can_suspend ~xs:_ _ = ()

      let suspend (task : Xenops_task.task_handle) ~xs ~qemu_domid domid =
        Dm_Common.signal task ~xs ~qemu_domid ~domid "save" ~wait_for:"paused"

      let stop ~xs:_ ~qemu_domid:_ ~vtpm:_ _ = ()

      let init_daemon ~task:_ ~path:_ ~args:_ ~domid:_ ~xs:_ ~ready_path:_
          ~timeout:_ ~cancel:_ ?fds:_ _ =
        raise (Ioemu_failed (Service.Qemu.name, "PV guests have no IO emulator"))

      let qemu_args ~xs:_ ~dm:_ _ _ _ = {Dm_Common.argv= []; fd_map= []}

      let after_suspend_image ~xs:_ ~qemu_domid:_ ~vtpm:_ _ = ()

      let pci_assign_guest ~xs:_ ~index:_ ~host:_ = None
    end

    (* Backend.Qemu_none.Dm *)
  end

  (* Backend.Qemu_none *)

  (** Implementation of the backend common signature for the
      qemu-upstream-compat backend *)
  module type Qemu_upstream_config = sig
    module NIC : sig
      val max_emulated : int
      (** Should be <= the hardcoded maximum number of emulated NICs *)

      val default : string

      val addr : devid:int -> index:int -> int

      val extra_flags : string list
    end

    module DISK : sig
      val max_emulated : int option
      (** None = just the qemu imposed 4 IDE device limit *)

      val default : string

      val types : (string * Dm_Common.disk_type_args) list

      val extra_args : string list
    end

    module Firmware : sig
      val supported : Xenops_types.Vm.firmware_type -> bool
    end

    module XenPV : sig
      val addr :
           xs:Xenstore.Xs.xsh
        -> domid:int
        -> Dm_Common.info
        -> nics:(string * string * int) list
        -> int
    end

    module XenPlatform : sig
      val device :
        xs:Xenstore.Xs.xsh -> domid:int -> info:Dm_Common.info -> string list
    end

    module VGPU : sig
      val device : index:int -> int option
    end

    module PCI : sig
      val assign_guest :
           xs:Xenstore.Xs.xsh
        -> index:int
        -> host:Pci.address
        -> Pci.address option
    end

    val extra_qemu_args : nic_type:string -> string list

    val name : string
  end

  module Config_qemu_upstream_compat = struct
    module NIC = struct
      let max_emulated = 8

      let default = "rtl8139"

      let base_addr = 4

      let addr ~devid ~index:_ = devid + base_addr

      let extra_flags = []
    end

    module DISK = struct
      let max_emulated = None

      let default = Dm_Common.ide

      let types = [(Dm_Common.ide, Dm_Common.ide_device_of ~trad_compat:true)]

      let extra_args = []
    end

    module XenPlatform = struct
      let device ~xs ~domid ~info =
        let has_platform_device =
          try
            int_of_string
              (xs.Xs.read (sprintf "/local/domain/%d/vm-data/disable_pf" domid))
            <> 1
          with _ -> true
        in
        if has_platform_device then
          Dm_Common.xen_platform ~trad_compat:true ~xs ~domid ~info
        else
          []
    end

    module VGPU = struct let device ~index:_ = None end

    module XenPV = struct
      let addr ~xs ~domid info ~nics =
        (* [first_gap n xs] expects an ascending list of integers [xs]. It looks
           for a gap in sequence [xs] and returns the first it finds at position
           n or higher:

           first_gap 4 [] = 4

           first_gap 4 [5;6] = 4

           first_gap 4 [1;3] = 4

           first_gap 4 [5;6;8] = 4

           first_gap 4 [4;5;7] = 6

           first_gap 4 [4;5;6] = 7 *)
        let rec first_gap n = function
          | [] ->
              n
          | x :: xs when x < n ->
              first_gap n xs
          | x :: xs when x = n ->
              first_gap (n + 1) xs
          | _ ->
              n
        in
        let has_nvidia_vgpu =
          let open Xenops_interface.Vgpu in
          let open Dm_Common in
          match info.disp with
          | VNC (Vgpu [{implementation= Nvidia _; _}], _, _, _, _) ->
              true
          | SDL (Vgpu [{implementation= Nvidia _; _}], _) ->
              true
          | _ ->
              false
        in
        if has_nvidia_vgpu then
          2
        else if XenPlatform.device ~xs ~domid ~info = [] then
          3
        else
          nics
          |> List.map (fun (_, _, devid) -> devid + NIC.base_addr)
          |> first_gap NIC.base_addr
    end

    module Firmware = struct let supported _ = true end

    module PCI = struct
      (* compat: let qemu deal with it as before *)
      let assign_guest ~xs:_ ~index:_ ~host:_ = None
    end

    let name = Profile.Name.qemu_upstream_compat

    let extra_qemu_args ~nic_type =
      let mult xs ys =
        List.map (fun x -> List.map (fun y -> x ^ "." ^ y) ys) xs |> List.concat
      in
      List.concat
        [
          ["-trad-compat"]
        ; ["-global"; "PIIX4_PM.revision_id=0x1"]
        ; ["-global"; "ide-hd.ver=0.10.2"]
        ; mult
            ["piix3-ide-xen"; "piix3-usb-uhci"; nic_type]
            ["subvendor_id=0x5853"; "subsystem_id=0x0001"]
          |> List.map (fun x -> ["-global"; x])
          |> List.concat
        ]
  end

  module Config_qemu_upstream_uefi = struct
    (* 0: i440FX

       1: PIIX3

       2: VGA or empty

       3: Xen platform or empty

       4 - 5: NIC (limited to first 2)

       6: Xen PV

       7: NVME

       8+: vGPU and other pass-through devices *)
    module NIC = struct
      let max_emulated = 2

      let default = "e1000"

      let addr ~devid:_ ~index = 4 + index

      let extra_flags = ["rombar=0"]
    end

    module DISK = struct
      let max_emulated = Some 4

      let default = Dm_Common.nvme

      let types =
        [
          (Dm_Common.ide, Dm_Common.ide_device_of ~trad_compat:false)
        ; (Dm_Common.nvme, Dm_Common.nvme_device_of)
        ]

      (* 4 and 5 are NICs, and we can only have two, 6 is platform *)
      let extra_args = ["-device"; "nvme,serial=nvme0,id=nvme0,addr=7"]
    end

    module XenPV = struct let addr ~xs:_ ~domid:_ _ ~nics:_ = 6 end

    module VGPU = struct let device ~index = Some (8 + index) end

    module XenPlatform = struct
      let device = Dm_Common.xen_platform ~trad_compat:false
    end

    module Firmware = struct
      let supported =
        let open Xenops_types.Vm in
        function Bios -> false | Uefi _ -> true
    end

    module PCI = struct
      let assign_guest ~xs:_ ~index ~host:_ =
        (* domain here refers to PCI segment from SBDF, and not a Xen domain *)
        Some {Pci.domain= 0; bus= 0; dev= 8 + index; fn= 0}
    end

    let name = Profile.Name.qemu_upstream_uefi

    let extra_qemu_args ~nic_type:_ = []
  end

  (** Handler for the QMP events in upstream qemu *)
  module QMP_Event = struct
    open Qmp

    let ( >>= ) m f = match m with Some x -> f x | None -> ()

    let ( >>| ) m f = match m with Some _ -> () | None -> f ()

    (** Efficient lookup table between file descriptors, channels and domain ids *)
    module Lookup = struct
      let ftod, dtoc = (Hashtbl.create 16, Hashtbl.create 16)

      let add c domid =
        Hashtbl.replace ftod (Qmp_protocol.to_fd c) domid ;
        Hashtbl.replace dtoc domid c

      let remove c domid =
        Hashtbl.remove ftod (Qmp_protocol.to_fd c) ;
        Hashtbl.remove dtoc domid

      let domid_of fd = try Some (Hashtbl.find ftod fd) with Not_found -> None

      let channel_of domid =
        try Some (Hashtbl.find dtoc domid) with Not_found -> None
    end

    (** File-descriptor event monitor implementation for the epoll library *)
    module Monitor = struct
      let create () = Polly.create ()

      let add m fd = Polly.add m fd Polly.Events.inp

      let remove m fd = Polly.del m fd
    end

    let m = Monitor.create ()

    let monitor_path domid = qmp_event_path domid

    let debug_exn msg e = debug "%s: %s" msg (Printexc.to_string e)

    let remove domid =
      Lookup.channel_of domid >>= fun c ->
      try
        finally
          (fun () ->
            Lookup.remove c domid ;
            Monitor.remove m (Qmp_protocol.to_fd c) ;
            debug "Removed QMP Event fd for domain %d" domid
          )
          (fun () -> Qmp_protocol.close c)
      with e ->
        debug_exn
          (Printf.sprintf "Got exception trying to remove QMP on domain-%d"
             domid
          )
          e

    let add domid =
      try
        Lookup.channel_of domid >>| fun () ->
        let c = Qmp_protocol.connect (monitor_path domid) in
        Lookup.add c domid ;
        Qmp_protocol.negotiate c ;
        Qmp_protocol.write c (Command (None, Cont)) ;
        Monitor.add m (Qmp_protocol.to_fd c) ;
        debug "Added QMP Event fd for domain %d" domid
      with e ->
        debug_exn
          (Printf.sprintf "QMP domain-%d: negotiation failed: removing socket"
             domid
          )
          e ;
        remove domid ;
        raise
        @@ Ioemu_failed (sprintf "domid %d" domid, "QMP failure at " ^ __LOC__)

    let update_cant_suspend domid xs =
      let as_msg cmd = Qmp.(Success (Some __LOC__, cmd)) in
      (* changing this will cause fire_event_on_vm to get called, which will do
         a VM.check_state, which will trigger a VM.stat from XAPI to update
         migratable state *)
      let path = Dm_Common.cant_suspend_reason_path domid in
      (* This will raise QMP_Error if it can't do it, we catch it and update
         xenstore. *)
      match qmp_send_cmd ~may_fail:true domid Qmp.Query_migratable with
      | Qmp.Unit ->
          debug "query-migratable precheck passed (domid=%d)" domid ;
          Generic.safe_rm ~xs path
      | other ->
          internal_error "Unexpected result for QMP command: %s"
            Qmp.(other |> as_msg |> string_of_message)
      | exception QMP_Error (_, msg) -> (
        match Astring.String.find_sub ~sub:"CommandNotFound" msg with
        | None ->
            xs.Xs.write path msg
        | Some _ ->
            debug "query-migratable ignoring precheck, qemu too old (domid=%d)"
              domid ;
            Generic.safe_rm ~xs path
      )

    let qmp_event_handle domid qmp_event =
      (* This function will be extended to handle qmp events *)
      debug "Got QMP event, domain-%d: %s" domid qmp_event.event ;
      let rtc_change timeoffset =
        with_xs (fun xs ->
            let timeoffset_key =
              sprintf "/vm/%s/rtc/timeoffset"
                (Uuidx.to_string (Xenops_helpers.uuid_of_domid ~xs domid))
            in
            try
              let rtc = xs.Xs.read timeoffset_key in
              xs.Xs.write timeoffset_key
                Int64.(add timeoffset (of_string rtc) |> to_string)
            with e ->
              error "Failed to process RTC_CHANGE for domain %d: %s" domid
                (Printexc.to_string e)
        )
      in
      let xen_platform_pv_driver_info pv_info =
        with_xs (fun xs ->
            let is_hvm_linux {product_num; build_num} =
              let _XEN_IOPORT_LINUX_PRODNUM = 3 in
              (* from Linux include/xen/platform_pci.h *)
              product_num = _XEN_IOPORT_LINUX_PRODNUM && build_num <= 0xff
            in
            if is_hvm_linux pv_info then (
              let write_local_domain prefix x =
                xs.Xs.write
                  (Printf.sprintf "/local/domain/%d/%s%s" domid prefix x)
                  "1"
              in
              List.iter
                (write_local_domain "control/feature-")
                ["suspend"; "poweroff"; "reboot"; "vcpu-hotplug"] ;
              List.iter (write_local_domain "data/") ["updated"]
            )
        )
      in
      qmp_event.data |> function
      | Some (RTC_CHANGE timeoffset) ->
          rtc_change timeoffset
      | Some (XEN_PLATFORM_PV_DRIVER_INFO x) ->
          xen_platform_pv_driver_info x
      | _ ->
          with_xs (update_cant_suspend domid)

    (* unhandled QMP events, including RESUME, and DEVICE_DELETED *)

    let process domid line =
      match Qmp.message_of_string line with
      | Event e ->
          qmp_event_handle domid e
      | msg ->
          debug "Got non-event message, domain-%d: %s" domid
            (string_of_message msg)

    let qmp_event_thread () =
      let qmp_domid socket =
        try Some (Scanf.sscanf socket "qmp-event-%d" (fun d -> d))
        with _ -> None
      in
      let add_domain id =
        try add id with _ -> error "Adding QMP socket for domain %d failed" id
      in
      ( try
          debug "Starting QMP_Event thread using Polly" ;
          (* Add the existing qmp sockets first *)
          Sys.readdir var_run_xen_path
          |> Array.to_list
          |> List.filter_map qmp_domid
          |> List.iter add_domain
        with e ->
          error "Connecting to existing QMP sockets failed: %s (%s)"
            (Printexc.to_string e) __LOC__
      ) ;
      let forever = -1 in
      while true do
        try
          ignore
          @@ Polly.wait m 10 forever (fun _ fd events ->
                 Lookup.domid_of fd >>= fun domid ->
                 Lookup.channel_of domid >>= fun c ->
                 let qmp = Qmp_protocol.to_fd c in
                 if Polly.Events.(test events inp) then (
                   match Readln.read qmp with
                   | Readln.Ok msgs ->
                       List.iter (process domid) msgs
                   | Readln.Error msg ->
                       error "domain-%d: %s, close QMP socket" domid msg ;
                       Readln.free qmp ;
                       remove domid
                   | Readln.EOF ->
                       debug "domain-%d: end of file, close QMP socket" domid ;
                       Readln.free qmp ;
                       remove domid
                 ) else (
                   debug "EPOLL error on domain-%d, close QMP socket" domid ;
                   Readln.free qmp ;
                   remove domid
                 )
             )
        with e -> debug_exn "Exception in QMP_Event_thread: %s" e
      done
  end

  (* Qemu_upstream_compat.Dm.QMP_Event *)

  module Event = struct
    let init () = ignore (Thread.create QMP_Event.qmp_event_thread ())
  end

  module Make_qemu_upstream (DefaultConfig : Qemu_upstream_config) : Intf =
  struct
    module Vgpu = struct let device = DefaultConfig.VGPU.device end

    (** Implementation of the Vbd functions that use the dispatcher for the
        qemu-upstream-compat backend *)
    module Vbd = struct
      let cd_of devid =
        devid |> Device_number.of_xenstore_key |> Device_number.spec |> function
        | Ide, 0, _ ->
            "ide0-cd0"
        | Ide, 1, _ ->
            "ide0-cd1"
        | Ide, 2, _ ->
            "ide1-cd0"
        | Ide, 3, _ ->
            "ide1-cd1"
        | _ ->
            internal_error "unexpected disk for devid %d" devid

      (* parse NBD URI. We are not using the URI module because the
         format is not compliant but used by qemu. Using sscanf instead
         to recognise and parse the specific URI *)
      let is_nbd str =
        try Scanf.sscanf str "nbd:unix:%s@:exportname=%s" (fun _ _ -> true)
        with _ -> false

      let nbd str =
        try Scanf.sscanf str "nbd:unix:%s@:exportname=%s" (fun x y -> (x, y))
        with _ -> internal_error "%s: failed to parse '%s'" __FUNCTION__ str

      let with_socket path f =
        let addr = Unix.ADDR_UNIX path in
        let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        finally (fun () -> Unix.connect fd addr ; f fd) (fun () -> Unix.close fd)

      let qemu_media_change ~xs device _type params =
        debug "%s: params='%s'" __FUNCTION__ params ;
        Vbd_Common.qemu_media_change ~xs device _type params ;
        let as_msg cmd = Qmp.(Success (Some __LOC__, cmd)) in
        try
          let cd = cd_of device.backend.devid in
          let domid = device.frontend.domid in
          match params with
          | "" ->
              qmp_send_cmd domid Qmp.(Eject (cd, Some true)) |> ignore
          | params when is_nbd params ->
              let path, exportname = nbd params in
              info "%s: domain=%d NBD socket=%s" __FUNCTION__ domid path ;
              with_socket path @@ fun fd ->
              let cmd = Qmp.(Add_fd None) in
              let fd_info =
                match qmp_send_cmd ~send_fd:fd domid cmd with
                | Qmp.Fd_info x ->
                    x
                | other ->
                    internal_error "Unexpected result for QMP command: %s"
                      Qmp.(other |> as_msg |> string_of_message)
              in
              let filename =
                Printf.sprintf "nbd:fd:%d:exportname=%s" fd_info.Qmp.fdset_id
                  exportname
              in
              let medium =
                Qmp.
                  {
                    medium_device= cd
                  ; medium_filename= filename
                  ; medium_format= Some "raw"
                  }
              in
              let cmd = Qmp.(Blockdev_change_medium medium) in
              qmp_send_cmd domid cmd |> ignore
          | params ->
              Unixext.with_file params [Unix.O_RDONLY] 0o640 @@ fun fd_cd ->
              let cmd = Qmp.(Add_fd None) in
              let fd_info =
                match qmp_send_cmd ~send_fd:fd_cd domid cmd with
                | Qmp.Fd_info x ->
                    x
                | other ->
                    internal_error "Unexpected result for QMP command: %s"
                      Qmp.(other |> as_msg |> string_of_message)
              in

              finally
                (fun () ->
                  let path = sprintf "/dev/fdset/%d" fd_info.Qmp.fdset_id in
                  let medium =
                    Qmp.
                      {
                        medium_device= cd
                      ; medium_filename= path
                      ; medium_format= Some "raw"
                      }
                  in
                  let cmd = Qmp.(Blockdev_change_medium medium) in
                  qmp_send_cmd domid cmd |> ignore
                )
                (fun () ->
                  let cmd = Qmp.(Remove_fd fd_info.fdset_id) in
                  qmp_send_cmd domid cmd |> ignore
                )
        with
        | Unix.Unix_error (Unix.ECONNREFUSED, "connect", p) ->
            internal_error "Failed to connnect QMP socket: %s" p
        | Unix.Unix_error (Unix.ENOENT, "open", p) ->
            internal_error "Failed to open CD Image: %s" p
        | Xenopsd_error (Internal_error _) as e ->
            raise e
        | e ->
            internal_error "Get unexpected error trying to change CD: %s"
              (Printexc.to_string e)
    end

    (* Backend.Qemu_upstream_compat.Vbd *)

    (** Implementation of the Vcpu functions that use the dispatcher for the
        qemu-upstream-compat backend *)
    module Vcpu = struct
      let add = Vcpu_Common.add

      let del = Vcpu_Common.del

      let status = Vcpu_Common.status

      (* hot(un)plug vcpu using QMP, keeping backwards-compatible xenstored
         mechanism *)
      let set ~xs ~devid domid online =
        Vcpu_Common.set ~xs ~devid domid online ;
        match online with
        | true ->
            (* hotplug *)
            let socket_id, core_id, thread_id = (devid, 0, 0) in
            let id = Qmp.Device.VCPU.id_of ~socket_id ~core_id ~thread_id in
            qmp_send_cmd domid
              Qmp.(
                Device_add
                  Device.
                    {
                      driver= VCPU.Driver.(string_of QEMU32_I386_CPU)
                    ; device= VCPU {VCPU.id; socket_id; core_id; thread_id}
                    }
              )
            |> ignore
        | false ->
            (* hotunplug *)
            let qom_path =
              qmp_send_cmd domid Qmp.Query_hotpluggable_cpus |> function
              | Qmp.Hotpluggable_cpus x -> (
                  x
                  |> List.filter
                       (fun Qmp.Device.VCPU.{props= {socket_id; _}; _} ->
                         socket_id = devid
                     )
                  |> function
                  | [] ->
                      internal_error "No QEMU CPU found with devid %d" devid
                  | Qmp.Device.VCPU.{qom_path= None; _} :: _ ->
                      internal_error "No qom_path for QEMU CPU devid %d" devid
                  | Qmp.Device.VCPU.{qom_path= Some p; _} :: _ ->
                      p
                )
              | other ->
                  let as_msg cmd = Qmp.(Success (Some __LOC__, cmd)) in
                  internal_error "Unexpected result for QMP command: %s"
                    Qmp.(other |> as_msg |> string_of_message)
            in
            qom_path |> fun id ->
            qmp_send_cmd domid Qmp.(Device_del id) |> ignore
    end

    (** Implementation of the Dm functions that use the dispatcher for the
        qemu-upstream-compat backend *)
    module Dm = struct
      let get_vnc_port ~xs domid =
        Dm_Common.get_vnc_port ~xs domid ~f:(fun () ->
            Some (Socket.Unix (Dm_Common.vnc_socket_path domid))
        )

      let assert_can_suspend ~xs domid =
        QMP_Event.update_cant_suspend domid xs ;
        match xs.Xs.read (Dm_Common.cant_suspend_reason_path domid) with
        | msg ->
            debug "assert_can_suspend: rejecting (domid=%d)" domid ;
            raise
            @@ Xenopsd_error
                 (Device_detach_rejected
                    ( "VM"
                    , domid
                      |> Xenops_helpers.uuid_of_domid ~xs
                      |> Uuidx.to_string
                    , msg
                    )
                 )
        | exception _ ->
            debug "assert_can_suspend: OK (domid=%d)" domid ;
            ()

      (* key not present *)

      let suspend (_ : Xenops_task.task_handle) ~xs:_ ~qemu_domid:_ domid =
        let as_msg cmd = Qmp.(Success (Some __LOC__, cmd)) in
        let perms = [Unix.O_WRONLY; Unix.O_CREAT] in
        let save_file = sprintf qemu_save_path domid in
        let save_fd = Unix.openfile save_file perms 0o660 in
        finally
          (fun () ->
            let fd =
              qmp_send_cmd ~send_fd:save_fd domid Qmp.(Add_fd None) |> function
              | Qmp.(Fd_info fd) ->
                  fd
              | other ->
                  internal_error "Unexpected result for QMP command: %s"
                    Qmp.(other |> as_msg |> string_of_message)
            in
            finally
              (fun () ->
                let path = sprintf "/dev/fdset/%d" fd.Qmp.fdset_id in
                qmp_send_cmd domid Qmp.Stop |> ignore ;
                qmp_send_cmd domid Qmp.(Xen_save_devices_state path) |> ignore
              )
              (fun () ->
                qmp_send_cmd domid Qmp.(Remove_fd fd.fdset_id) |> ignore
              )
          )
          (fun () -> Unix.close save_fd)

      (* Wait for QEMU's event socket to appear. Connect to it to make sure it
         is ready. *)
      let wait_event_socket ~task ~name ~domid ~timeout =
        let finished = ref false in
        let timeout_ns = Int64.of_float (timeout *. 1e9) in
        let now = Mtime_clock.now () in
        let target =
          match Mtime.(add_span now (Span.of_uint64_ns timeout_ns)) with
          | None ->
              raise (Ioemu_failed (name, "Timeout overflow"))
          | Some x ->
              x
        in
        let path = qmp_event_path domid in
        let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
        finally (* make sure we don't leak socket *)
          (fun () ->
            while
              Mtime.is_earlier (Mtime_clock.now ()) ~than:target
              && not !finished
            do
              Xenops_task.check_cancelling task ;
              (* might raise exn *)
              if Sys.file_exists path then (
                try
                  Unix.connect socket Unix.(ADDR_UNIX path) ;
                  finished := true
                with e ->
                  debug "QMP event socket for domid %d not yet ready: %s" domid
                    (Printexc.to_string e) ;
                  Thread.delay 0.1
              ) else
                Thread.delay 0.05
            done
          )
          (fun () -> Unix.close socket) ;
        if not !finished then
          raise (Ioemu_failed (name, "Timeout reached while starting daemon"))

      let init_daemon ~task ~path ~args ~domid ~xs:_ ~ready_path:_ ~timeout
          ~cancel:_ ?(fds = []) _ =
        let pid = Service.Qemu.start_daemon ~path ~args ~domid ~fds () in
        wait_event_socket ~task ~name:Service.Qemu.name ~domid ~timeout ;
        QMP_Event.add domid ;
        pid

      let stop ~xs ~qemu_domid ~vtpm domid =
        Dm_Common.stop ~xs ~qemu_domid ~vtpm domid ;
        QMP_Event.remove domid ;
        let rm path =
          let msg = Printf.sprintf "removing %s" path in
          Generic.best_effort msg (fun () -> Socket.Unix.rm path)
        in
        [
          (* clean up QEMU socket leftover files *)
          Dm_Common.vnc_socket_path domid
        ; qmp_event_path domid
        ; qmp_libxl_path domid
        ]
        |> List.iter rm ;
        Vusb.cleanup domid ;
        (* unmounts devices in /var/xen/qemu/root-* *)
        let path = Printf.sprintf "/var/xen/qemu/root-%d" domid in
        Generic.best_effort (Printf.sprintf "removing %s" path) (fun () ->
            Xapi_stdext_unix.Unixext.rm_rec path
        )

      let tap_open ifname =
        let uuid = Uuidx.(to_string (make ())) in
        let fd = Tuntap.tap_open ifname in
        (uuid, fd)

      let qemu_args ~xs ~dm info restore domid =
        let module Config = DefaultConfig in
        debug "Using device-model=%s for domid=%d" Config.name domid ;
        let common =
          Dm_Common.qemu_args ~xs ~dm info restore domid ~domid_for_vnc:true
        in
        let usb =
          match info.Dm_Common.usb with
          | Dm_Common.Disabled ->
              []
          | Dm_Common.Enabled devices ->
              let devs =
                devices
                |> List.map (fun (x, y) ->
                       ["-device"; sprintf "usb-%s,port=%d" x y]
                   )
                |> List.concat
              in
              "-usb" :: devs
        in
        let serial_device =
          try
            let xs_path = xs.Xs.read "/local/logconsole/@" in
            let domid_placeholder = Re.(compile @@ str "%d") in
            let file =
              Re.replace_string domid_placeholder ~by:(string_of_int domid)
                xs_path
            in
            [
              "-chardev"
            ; "file,id=serial0,append=on,path=" ^ file
            ; "-serial"
            ; "chardev:serial0"
            ]
          with _ -> (
            match info.Dm_Common.serial with
            | None ->
                []
            | Some x ->
                ["-serial"; x]
          )
        in
        let nic_type =
          try xs.Xs.read (sprintf "/local/domain/%d/platform/nic_type" domid)
          with _ -> Config.NIC.default
        in
        let disk_type =
          try xs.Xs.read (sprintf "/local/domain/%d/platform/disk_type" domid)
          with _ -> Config.DISK.default
        in
        let disk_interface =
          match List.assoc_opt disk_type Config.DISK.types with
          | Some interface ->
              interface
          | None ->
              raise
                (Ioemu_failed
                   ( sprintf "domid %d" domid
                   , sprintf "Unknown platform:disk_type=%s in device-model=%s"
                       disk_type Config.name
                   )
                )
        in
        if not (Config.Firmware.supported info.firmware) then
          (* XAPI itself should've already prevented this, but lets double check *)
          raise
            (Ioemu_failed
               ( sprintf "domid %d" domid
               , sprintf "The firmware doesn't support device-model=%s"
                   Config.name
               )
            ) ;
        let qmp =
          ["libxl"; "event"]
          |> List.map (fun x ->
                 [
                   "-qmp"
                 ; sprintf "unix:/var/run/xen/qmp-%s-%d,server,nowait" x domid
                 ]
             )
          |> List.concat
        in
        let pv_device addr =
          try
            let path =
              sprintf "/local/domain/%d/control/has-vendor-device" domid
            in
            let has_device = xs.Xs.read path in
            if int_of_string has_device = 1 then
              ["-device"; sprintf "xen-pvdevice,device-id=0xc000,addr=%x" addr]
            else
              []
          with _ -> []
        in
        let misc =
          List.concat
            [
              [
                "-xen-domid"
              ; string_of_int domid
              ; "-m"
              ; "size=" ^ Int64.to_string (Int64.div info.Dm_Common.memory 1024L)
              ; "-boot"
              ; "order=" ^ info.Dm_Common.boot
              ]
            ; usb
            ; [
                "-smp"
              ; sprintf "%d,maxcpus=%d" info.Dm_Common.vcpus_current
                  info.Dm_Common.vcpus
              ]
            ; serial_device
            ; ["-display"; "none"; "-nodefaults"]
            ; ["-trace"; "enable=xen_platform_log"]
            ; [
                "-sandbox"
              ; "on,obsolete=deny,elevateprivileges=allow,spawn=deny,resourcecontrol=deny"
              ]
            ; ["-S"]
            ; Config.extra_qemu_args ~nic_type
            ; (info.Dm_Common.parallel |> function
               | None ->
                   ["-parallel"; "null"]
               | Some x ->
                   ["-parallel"; x]
              )
            ; qmp
            ; Config.XenPlatform.device ~xs ~domid ~info
            ]
        in
        let disks_cdrom, disks_floppy, disks_other =
          let partition (cdroms, floppies, other) ((_, _, media) as next) =
            match media with
            | Dm_Common.Media.Cdrom ->
                (next :: cdroms, floppies, other)
            | Floppy ->
                (cdroms, next :: floppies, other)
            | Disk ->
                (cdroms, floppies, next :: other)
          in
          List.fold_left partition ([], [], []) info.Dm_Common.disks
        in
        let limit_emulated_disks disks =
          let disks =
            List.stable_sort (fun (a, _, _) (b, _, _) -> compare a b) disks
          in
          match Config.DISK.max_emulated with
          | Some limit when List.length disks > limit ->
              debug "Limiting the number of emulated disks to %d" limit ;
              Xapi_stdext_std.Listext.List.take limit disks
          | _ ->
              disks
        in
        let disks' =
          let qemu_floppy_args (index, path, media) =
            [
              "-drive"
            ; String.concat ","
                ([sprintf "file=%s" path; "if=floppy"; sprintf "index=%i" index]
                @ Dm_Common.Media.format_of media path
                )
            ]
          in
          [
            List.map (List.assoc Dm_Common.ide Config.DISK.types) disks_cdrom
          ; [Config.DISK.extra_args]
          ; List.map qemu_floppy_args disks_floppy
          ; disks_other |> limit_emulated_disks |> List.map disk_interface
          ]
          |> List.concat
          |> List.concat
        in
        (* Sort the VIF devices by devid *)
        let nics =
          List.stable_sort
            (fun (_, _, a) (_, _, b) -> compare a b)
            info.Dm_Common.nics
        in
        let nic_count = List.length nics in
        let nic_max = Config.NIC.max_emulated in
        if nic_count > nic_max then
          debug "Limiting the number of emulated NICs to %d" nic_max ;
        (* Take the first 'max_emulated_nics' elements from the list. *)
        let nics = Xapi_stdext_std.Listext.List.take nic_max nics in
        (* add_nic is used in a fold: it adds fd and command line args for a nic
           to the existing fds and arguments (fds, argv) *)
        let none = ["-net"; "none"] in
        let add_nic (index, fds, argv) (mac, _bridge, devid) =
          let ifname = sprintf "tap%d.%d" domid devid in
          let ((uuid, _) as tap) = tap_open ifname in
          let args =
            [
              "-device"
            ; String.concat ","
                ([
                   nic_type
                 ; sprintf "netdev=tapnet%d" devid
                 ; sprintf "mac=%s" mac
                 ; sprintf "addr=%x" (Config.NIC.addr ~devid ~index)
                 ]
                @ Config.NIC.extra_flags
                )
            ; "-netdev"
            ; sprintf "tap,id=tapnet%d,fd=%s" devid uuid
            ]
          in
          (index + 1, tap :: fds, args @ argv)
        in
        let pv_device_addr = Config.XenPV.addr ~xs ~domid info ~nics in
        (* Go over all nics and collect file descriptors and command line
           arguments. Add these to the already existing command line arguments
           in common *)
        List.fold_left add_nic (0, [], []) nics |> function
        | _, _, [] ->
            Dm_Common.
              {
                argv=
                  common.argv @ misc @ disks' @ pv_device pv_device_addr @ none
              ; fd_map= common.fd_map
              }
        | _, fds, argv ->
            Dm_Common.
              {
                argv=
                  common.argv @ misc @ disks' @ pv_device pv_device_addr @ argv
              ; fd_map= common.fd_map @ fds
              }

      let after_suspend_image ~xs ~qemu_domid ~vtpm domid =
        (* device model not needed anymore after suspend image has been created *)
        stop ~xs ~qemu_domid ~vtpm domid

      let pci_assign_guest ~xs ~index ~host =
        DefaultConfig.PCI.assign_guest ~xs ~index ~host
    end

    (* Backend.Qemu_upstream_compat.Dm *)
  end

  (* Backend.Qemu_upstream *)

  (** Implementation of the backend common signature for the qemu-upstream
      backend *)
  module Qemu_upstream_compat = Make_qemu_upstream (Config_qemu_upstream_compat)

  (** Until the stage 4 defined in the qemu upstream design is implemented,
      qemu_upstream behaves as qemu_upstream_compat *)
  module Qemu_upstream = Qemu_upstream_compat

  module Qemu_upstream_uefi = Make_qemu_upstream (Config_qemu_upstream_uefi)

  let of_profile p =
    match p with
    | Profile.Qemu_trad ->
        (* checks elsewhere should've blocked or transparently upgraded
           qemu-trad, if we reach this place there is a bug elsewhere *)
        assert false
    | Profile.Qemu_none ->
        (module Qemu_none : Intf)
    | Profile.Qemu_upstream_compat ->
        (module Qemu_upstream_compat : Intf)
    | Profile.Qemu_upstream ->
        (module Qemu_upstream : Intf)
    | Profile.Qemu_upstream_uefi ->
        (module Qemu_upstream_uefi : Intf)

  let init () = Event.init ()
end

(* Functions using the backend dispatcher *)

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
end

(* Vbd *)

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
end

(* Vcpu *)

(** Dm module conforming to the corresponding public mli interface *)
module Dm = struct
  include Dm_Common

  let init_daemon ~task ~path ~args ~domid ~xs ~ready_path ~timeout ~cancel
      ?(fds = []) profile =
    let module Q = (val Backend.of_profile profile) in
    Q.Dm.init_daemon ~task ~path ~args ~domid ~xs ~ready_path ~timeout ~cancel
      ~fds ()

  let get_vnc_port ~xs ~dm domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.get_vnc_port ~xs domid

  let assert_can_suspend ~xs ~dm domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.assert_can_suspend ~xs domid

  let suspend (task : Xenops_task.task_handle) ~xs ~qemu_domid ~dm domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.suspend task ~xs ~qemu_domid domid

  let stop ~xs ~qemu_domid ~vtpm ~dm domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.stop ~xs ~vtpm ~qemu_domid domid

  let qemu_args ~xs ~dm info restore domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.qemu_args ~xs ~dm info restore domid

  let after_suspend_image ~xs ~dm ~qemu_domid ~vtpm domid =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.after_suspend_image ~xs ~qemu_domid ~vtpm domid

  let pci_assign_guest ~xs ~dm ~index ~host =
    let module Q = (val Backend.of_profile dm) in
    Q.Dm.pci_assign_guest ~xs ~index ~host

  let ioemu_failed emu fmt =
    Printf.kprintf (fun msg -> raise (Ioemu_failed (emu, msg))) fmt

  let wait_for_vgpu_state states ~timeout ~xs ~domid ~task vgpus =
    let open Xenops_interface.Vgpu in
    match vgpus with
    | {implementation= Nvidia {vclass= _; _}; _} :: _ -> (
        let error_path =
          Printf.sprintf "/local/domain/%d/vgpu/error-code" domid
        in
        let state_path = Service.Vgpu.state_path domid in
        let watch_for state = Watch.value_to_become state_path state in
        let error_watches = List.map watch_for ["error"] in
        let good_watches = List.map watch_for states in
        match
          cancellable_watch
            (Service.Vgpu.cancel_key domid)
            good_watches error_watches task ~xs ~timeout ()
        with
        | true ->
            info "%s: daemon vgpu for domain %d is ready: %s" __FUNCTION__ domid
              (xs.Xs.read state_path)
        | false ->
            let error_code = xs.Xs.read error_path in
            error "%s: daemon vgpu for domain %d returned error: %s"
              __FUNCTION__ domid error_code ;
            ioemu_failed "vgpu" "Daemon vgpu returned error: %s" error_code
      )
    | _ ->
        ()

  (* the following functions depend on the functions above that use the qemu
     backend Q *)

  let start_vgpu ~xc:_ ~xs task ?(restore = false) domid vgpus vcpus profile =
    let open Xenops_interface.Vgpu in
    match vgpus with
    | {implementation= Nvidia {vclass; _}; _} :: _ ->
        let vclass = Option.value ~default:"unknown" vclass in
        info "NVidia vgpu vclass=%s" vclass ;
        (* Start DEMU and wait until it has reached the desired state *)
        if not (Service.Vgpu.is_running ~xs domid) then (
          let pcis = List.map (fun x -> x.physical_pci_address) vgpus in
          PCI.bind pcis PCI.Nvidia ;
          let module Q = (val Backend.of_profile profile) in
          try Service.Vgpu.start ~xs ~vcpus ~vgpus ~restore task domid
          with e ->
            error "%s: vgpu start failed: %s" __FUNCTION__ (Printexc.to_string e) ;
            ioemu_failed "vgpu" "%s: emulator failed to start for domain %d"
              __FUNCTION__ domid
        ) else
          info "Daemon %s is already running for domain %d" !Xc_resources.vgpu
            domid ;
        (* Keep waiting until DEMU's state becomes "initialising" or "running",
           or an error occurred. *)
        wait_for_vgpu_state
          ["running"; "resuming"; "initialising"]
          ~timeout:60.0 ~xs ~domid ~task vgpus
    | [{physical_pci_address= pci; implementation= GVT_g _; _}] ->
        PCI.bind [pci] PCI.I915
    | [{physical_pci_address= pci; implementation= MxGPU vgpu; _}] ->
        with_lock gimtool_m (fun () ->
            configure_gim ~xs pci vgpu.vgpus_per_pgpu vgpu.framebufferbytes ;
            let keys = [("pf", Xenops_interface.Pci.string_of_address pci)] in
            write_vgpu_data ~xs domid 0 keys
        )
    | _ ->
        failwith "Unsupported vGPU configuration"

  type action = Start | Restore

  let action_to_string = function Start -> "Start" | Restore -> "Restore"

  let __start (task : Xenops_task.task_handle) ~xc ~xs ~dm
      ?(timeout = !Xenopsd.qemu_dm_ready_timeout) action info domid =
    let args =
      match action with
      | Start ->
          qemu_args ~xs ~dm info false domid
      | Restore ->
          qemu_args ~xs ~dm info true domid
    in
    debug "Device.Dm.start domid=%d action=%s qemu args: [%s]" domid
      (action_to_string action)
      (String.concat " " args.argv) ;
    (* start vgpu emulation if appropriate *)
    let () =
      match info.disp with
      | VNC (Vgpu vgpus, _, _, _, _) | SDL (Vgpu vgpus, _) ->
          start_vgpu ~xc ~xs task domid vgpus info.vcpus dm
      | _ ->
          ()
    in
    (* start varstored if appropriate *)
    ( match info.firmware with
    | Uefi nvram_uefi ->
        Service.Varstored.start ~restore:(action = Restore) ~xs
          ~nvram:nvram_uefi task domid
    | Bios ->
        ()
    ) ;

    (* start swtpm-wrapper if appropriate and modify QEMU arguments as needed *)
    let tpmargs =
      let mk_args vtpm_uuid =
        let tpm_socket_path =
          Service.Swtpm.start ~xs task domid ~vtpm_uuid ~index:0
        in
        [
          "-chardev"
        ; Printf.sprintf "socket,id=chrtpm,path=%s" tpm_socket_path
        ; "-tpmdev"
        ; "emulator,id=tpm0,chardev=chrtpm"
        ; "-device"
        ; "tpm-crb,tpmdev=tpm0"
        ]
      in
      match info.tpm with
      | Some (Vtpm vtpm_uuid) ->
          mk_args vtpm_uuid
      | None ->
          D.debug "VM domid %d has no vTPM" domid ;
          []
    in

    (* Execute qemu-wrapper, forwarding stdout to the syslog, with the key
       "qemu-dm-<domid>" *)
    let argv = prepend_wrapper_args domid (List.concat [tpmargs; args.argv]) in
    let qemu_domid = 0 in
    let ready_path =
      Printf.sprintf "/local/domain/%d/device-model/%d/state" qemu_domid domid
    in
    let cancel = Cancel_utils.Qemu (qemu_domid, domid) in
    let close (uuid, fd) =
      try Unix.close fd
      with e ->
        error "Closing fd for %s failed: %s (%s)" uuid (Printexc.to_string e)
          __LOC__
    in
    let qemu_pid =
      finally
        (fun () ->
          init_daemon ~task ~path:(Profile.wrapper_of dm) ~args:argv ~domid ~xs
            ~ready_path ~timeout ~cancel ~fds:args.fd_map dm
        )
        (fun () -> List.iter close args.fd_map)
    in
    ( match !Xenopsd.action_after_qemu_crash with
    | None ->
        (* At this point we expect qemu to outlive us; we will never call
           waitpid *)
        Forkhelpers.dontwaitpid qemu_pid
    | Some _ ->
        (* We register a callback to be run asynchronously in case qemu
           fails/crashes or is killed *)
        let waitpid_async x ~callback =
          ignore
            (Thread.create
               (fun x ->
                 callback
                   ( try
                       Forkhelpers.waitpid_fail_if_bad_exit x ;
                       None
                     with e -> Some e
                   )
               )
               x
            )
        in
        waitpid_async qemu_pid ~callback:(fun qemu_crash ->
            Forkhelpers.(
              debug "Finished waiting qemu pid=%d for domid=%d"
                (getpid qemu_pid) domid ;
              let crash_reason =
                match qemu_crash with
                | None ->
                    debug "domid=%d qemu-pid=%d: detected exit=0" domid
                      (getpid qemu_pid) ;
                    "exit:0"
                | Some e -> (
                  match e with
                  | Subprocess_failed x ->
                      (* exit n *)
                      debug "domid=%d qemu-pid=%d: detected exit=%d" domid
                        (getpid qemu_pid) x ;
                      Printf.sprintf "exit:%d" x
                  | Subprocess_killed x ->
                      (* qemu received signal/crashed *)
                      debug "domid=%d qemu-pid=%d: detected signal=%d" domid
                        (getpid qemu_pid) x ;
                      Printf.sprintf "signal:%d" x
                  | e ->
                      debug
                        "domid=%d qemu-pid=%d: detected unknown exception %s"
                        domid (getpid qemu_pid) (Printexc.to_string e) ;
                      Printf.sprintf "unknown"
                )
              in
              if not Service.Qemu.(SignalMask.has signal_mask domid) then
                match Service.Qemu.pid ~xs domid with
                | None ->
                    (* after expected qemu stop or domain xs tree destroyed:
                       this event arrived too late, nothing to do *)
                    debug
                      "domid=%d qemu-pid=%d: already removed from xenstore \
                       during domain destroy"
                      domid (getpid qemu_pid)
                | Some _ ->
                    (* before expected qemu stop: qemu-pid is available in
                       domain xs tree: signal action to take *)
                    xs.Xs.write
                      (Service.Qemu.pidxenstore_path_signal domid)
                      crash_reason
            )
        )
    ) ;
    (* CP-46917 Wait for vgpu/demu to be running. Timeout needs to be
       long enough for migrations to complete *)
    match info.disp with
    | VNC (Vgpu vgpus, _, _, _, _) | SDL (Vgpu vgpus, _) ->
        wait_for_vgpu_state ["running"] ~timeout:3600.0 ~xs ~domid ~task vgpus
    | _ ->
        ()

  let start (task : Xenops_task.task_handle) ~xc ~xs ~dm ?timeout info domid =
    __start task ~xc ~xs ~dm ?timeout Start info domid

  let restore (task : Xenops_task.task_handle) ~xc ~xs ~dm ?timeout info domid =
    __start task ~xc ~xs ~dm ?timeout Restore info domid

  let restore_vgpu (task : Xenops_task.task_handle) ~xc ~xs domid vgpus vcpus
      profile =
    debug "Called Dm.restore_vgpu" ;
    start_vgpu ~xc ~xs task ~restore:true domid vgpus vcpus profile

  let suspend_varstored (_ : Xenops_task.task_handle) ~xs domid ~vm_uuid =
    debug "Called Dm.suspend_varstored (domid=%d)" domid ;
    Service.Varstored.stop ~xs domid ;
    Xenops_sandbox.Varstore_guard.read ~domid efivars_save_path ~vm_uuid

  let restore_varstored (_ : Xenops_task.task_handle) ~xs ~efivars domid =
    debug "Called Dm.restore_varstored (domid=%d)" domid ;
    let path =
      Xenops_sandbox.Varstore_guard.create ~domid
        ~vm_uuid:(Uuidx.to_string (Xenops_helpers.uuid_of_domid ~xs domid))
        efivars_resume_path
    in
    debug "Writing EFI variables to %s (domid=%d)" path domid ;
    Unixext.write_string_to_file path efivars ;
    debug "Wrote EFI variables to %s (domid=%d)" path domid

  let suspend_vtpm (task : Xenops_task.task_handle) ~xs domid ~vtpm =
    debug "Called Dm.suspend_vtpm (domid=%d)" domid ;
    let dbg = Xenops_task.get_dbg task in
    Option.map
      (fun (Xenops_interface.Vm.Vtpm vtpm_uuid) ->
        Service.Swtpm.suspend dbg ~xs ~domid ~vtpm_uuid
      )
      vtpm
    |> Option.to_list

  let restore_vtpm (task : Xenops_task.task_handle) ~xs:_ ~contents ~vtpm domid
      =
    debug "Called Dm.restore_vtpm (domid=%d)" domid ;
    let dbg = Xenops_task.get_dbg task in
    Option.iter
      (fun (Xenops_interface.Vm.Vtpm vtpm_uuid) ->
        Service.Swtpm.restore dbg ~domid ~vtpm_uuid contents
      )
      vtpm
end

(* Dm *)

(* functions depending on modules Vif, Vbd, Pci, Vfs, Vfb, Vkbd, Dm *)

let hard_shutdown (task : Xenops_task.task_handle) ~xs (x : device) =
  match x.backend.kind with
  | Vif ->
      Vif.hard_shutdown task ~xs x
  | NetSriovVf ->
      NetSriovVf.hard_shutdown ~xs x
  | Vbd _ | Tap ->
      Vbd.hard_shutdown task ~xs x
  | Pci ->
      PCI.hard_shutdown task ~xs x
  | Vfs ->
      Vfs.hard_shutdown task ~xs x
  | Vfb ->
      Vfb.hard_shutdown task ~xs x
  | Vkbd ->
      Vkbd.hard_shutdown task ~xs x

let clean_shutdown (task : Xenops_task.task_handle) ~xs (x : device) =
  match x.backend.kind with
  | Vif ->
      Vif.clean_shutdown task ~xs x
  | NetSriovVf ->
      raise (Xenopsd_error (Unimplemented "network sr-iov"))
  | Vbd _ | Tap ->
      Vbd.clean_shutdown task ~xs x
  | Pci ->
      PCI.clean_shutdown task ~xs x
  | Vfs ->
      Vfs.clean_shutdown task ~xs x
  | Vfb ->
      Vfb.clean_shutdown task ~xs x
  | Vkbd ->
      Vkbd.clean_shutdown task ~xs x

let get_vnc_port ~xs ~dm domid =
  (* Check whether a qemu exists for this domain *)
  let qemu_exists = Service.Qemu.is_running ~xs domid in
  if qemu_exists then
    Dm.get_vnc_port ~xs ~dm domid
  else
    Service.PV_Vnc.get_vnc_port ~xs domid

let get_tc_port ~xs domid =
  (* Check whether a qemu exists for this domain *)
  let qemu_exists = Service.Qemu.is_running ~xs domid in
  if qemu_exists then
    Dm.get_tc_port ~xs domid
  else
    Service.PV_Vnc.get_tc_port ~xs domid
