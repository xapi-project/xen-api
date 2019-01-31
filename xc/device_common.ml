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
open Xenstore
open Xenops_utils

type kind = Vif | Tap | Pci | Vfs | Vfb | Vkbd | Vbd of string | NetSriovVf
  [@@deriving rpcty]

type devid = int
(** Represents one end of a device *)
type endpoint = { domid: int; kind: kind; devid: int }
[@@deriving rpcty]

(** Represent a device as a pair of endpoints *)
type device = { 
  frontend: endpoint;
  backend: endpoint
}
[@@deriving rpcty]

exception Device_frontend_already_connected of device
exception Device_disconnect_timeout of device
exception Device_error of device * string
exception Device_unrecognized of string
exception Hotplug_script_expecting_field of device * string
exception Unknown_device_type of string
exception Unknown_device_protocol of string
exception QMP_Error of int * string
exception QMP_connection_error of int * string

module D = Debug.Make(struct let name = "xenops" end)
open D

open Printf

let supported_vbd_backends = [ "vbd"; "vbd3"; "qdisk" ] (* TODO: get from xenopsd config *)
let default_vbd_frontend_kind = Vbd "vbd"
let vbd_kind_of_string backend_kind =
  if List.mem backend_kind supported_vbd_backends then Vbd backend_kind
  else Vbd "unsupported"

let string_of_kind = function
  | Vif -> "vif" | Tap -> "tap" | Pci -> "pci" | Vfs -> "vfs" | Vfb -> "vfb" | Vkbd -> "vkbd" | NetSriovVf -> "net-sriov-vf"
  | Vbd x -> x
let kind_of_string = function
  | "vif" -> Vif | "tap" -> Tap | "pci" -> Pci | "vfs" -> Vfs | "vfb" -> Vfb | "vkbd" -> Vkbd | "net-sriov-vf" -> NetSriovVf
  | b when List.mem b supported_vbd_backends -> Vbd b
  | x -> raise (Unknown_device_type x)

let string_of_endpoint (x: endpoint) =
  sprintf "(domid=%d | kind=%s | devid=%d)" x.domid (string_of_kind x.kind) x.devid  
let block_device_of_device device =
  device.frontend.devid |> Device_number.of_xenstore_key |> Device_number.to_linux_device |> (fun x -> "/dev/" ^ x)

let backend_path ~xs (backend: endpoint) (domu: Xenctrl.domid) = 
  let p = match backend.kind with
    | NetSriovVf -> "xenserver/backend"
    | _ -> "backend"
  in
  sprintf "%s/%s/%s/%u/%d" 
    (xs.Xs.getdomainpath backend.domid) 
    p
    (string_of_kind backend.kind)
    domu backend.devid

(** Location of the backend in xenstore *)
let backend_path_of_device ~xs (x: device) = backend_path ~xs x.backend x.frontend.domid

(** Location of the frontend in xenstore: this is owned by the guest. *)
let frontend_rw_path_of_device ~xs (x: device) = 
  let p = match x.frontend.kind with
    | NetSriovVf -> "xenserver/device"
    | _ -> "device"
  in
  sprintf "%s/%s/%s/%d"
    (xs.Xs.getdomainpath x.frontend.domid)
    p
    (string_of_kind x.frontend.kind)
    x.frontend.devid

(** Location of the frontend read-only path (owned by dom0 not guest) in xenstore *)
let frontend_ro_path_of_device ~xs (x: device) = 
  sprintf "/xenops/domain/%d/device/%s/%d"
    x.frontend.domid
    (string_of_kind x.frontend.kind)
    x.frontend.devid

(** Location of the frontend error node *)
let error_path_of_device ~xs (x: device) = 
  sprintf "%s/error/device/%s/%d/error"
    (xs.Xs.getdomainpath x.frontend.domid)
    (string_of_kind x.frontend.kind)
    x.frontend.devid

(** Location of the frontend node where carrier status is inflicted *)
let disconnect_path_of_device ~xs (x: device) = 
  sprintf "%s/device/%s/%d/disconnect"
    (xs.Xs.getdomainpath x.frontend.domid)
    (string_of_kind x.frontend.kind)
    x.frontend.devid

(** Where linux blkback writes its thread id. NB this won't work in a driver domain *)
let kthread_pid_path_of_device ~xs (x: device) =
  sprintf "%s/kthread-pid" (backend_path_of_device ~xs x)

let backend_queue_regexp = Re.Pcre.regexp "^queue-\\d+$"

(** Where linux blkback may write its thread ids if multiple queues/rings are in use *)
let kthread_pid_paths_of_device ~xs (x: device) =
  let backend_path = backend_path_of_device ~xs x in
  let paths = xs.Xs.directory backend_path in
  if List.mem "kthread-pid" paths then
    [sprintf "%s/kthread-pid" backend_path]
  else
    List.filter (Re.execp backend_queue_regexp) paths
    |> List.map (fun queue -> sprintf "%s/%s/kthread-pid" backend_path queue)

(** Location of the backend error path *)
let backend_error_path_of_device ~xs (x : device) =
  sprintf "%s/error/backend/%s/%d"
    (xs.Xs.getdomainpath x.backend.domid)
    (string_of_kind x.backend.kind)
    x.frontend.domid

(** Written to by blkback/blktap when they have shutdown a device *)
let backend_shutdown_done_path_of_device ~xs (x: device) = 
  sprintf "%s/shutdown-done" (backend_path_of_device ~xs x)

(** Path to write blkback/blktap shutdown requests to *)
let backend_shutdown_request_path_of_device ~xs (x: device) = 
  sprintf "%s/shutdown-request" (backend_path_of_device ~xs x)

(** Path to write blkback/blktap pause requests to *)
let backend_pause_request_path_of_device ~xs (x: device) = 
  sprintf "%s/pause" (backend_path_of_device ~xs x)

(** Path to write blkback/blktap pause tokens to *)
let backend_pause_token_path_of_device ~xs (x: device) = 
  sprintf "%s/pause-token" (backend_path_of_device ~xs x)

(** Path to write blkback/blktap pause done responses to *)
let backend_pause_done_path_of_device ~xs (x: device) = 
  sprintf "%s/pause-done" (backend_path_of_device ~xs x)

let backend_state_path_of_device ~xs (x: device) =
  sprintf "%s/state" (backend_path_of_device ~xs x)

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

let string_of_device (x: device) = 
  sprintf "frontend %s; backend %s" (string_of_endpoint x.frontend) (string_of_endpoint x.backend)

(* We use this function below to switch from domid- to UUID-based private
 * paths. It can be made a little more efficient by changing the functions below
 * to take the UUID as an argument (and change the callers as well...) *)
let uuid_of_domid domid =
  try
    with_xs (fun xs ->
        Uuidm.to_string (Xenops_helpers.uuid_of_domid ~xs domid)
      )
  with Xenops_helpers.Domain_not_found ->
    error "uuid_of_domid failed for domid %d" domid;
    (* Returning a random string on error is not very neat, but we must avoid
       		 * exceptions here. This patch must be followed soon by a patch that changes the
       		 * callers of get_private_path{_of_device} to call by UUID, so that this code
       		 * can go away. *)
    Printf.sprintf "unknown-domid-%d" domid

(* We store some transient data elsewhere in xenstore to avoid it getting
   deleted by accident when a domain shuts down. We should always zap this
   tree on boot. *)
let private_path = "/xapi"

(* The private data path is only used by xapi and ignored by frontend and backend *)
let get_private_path domid =
  sprintf "%s/%s" private_path (uuid_of_domid domid)

let get_private_path_by_uuid uuid =
  sprintf "%s/%s" private_path (Uuidm.to_string uuid)

let get_private_data_path_of_device (x: device) =
  sprintf "%s/private/%s/%d" (get_private_path x.frontend.domid) (string_of_kind x.backend.kind) x.backend.devid

(** Location of the device node's extra xenserver xenstore keys *)
let extra_xenserver_path_of_device ~xs (x: device) =
  sprintf "%s/xenserver/device/%s/%d"
    (xs.Xs.getdomainpath x.frontend.domid)
    (string_of_kind x.backend.kind)
    x.frontend.devid

let device_of_backend (backend: endpoint) (domu: Xenctrl.domid) = 
  let frontend = { domid = domu;
                   kind = (match backend.kind with
                       | Tap | Vbd _ -> default_vbd_frontend_kind
                       | _ -> backend.kind);
                   devid = backend.devid } in
  { backend = backend; frontend = frontend }

let parse_kind k =
  try
    Some (kind_of_string k)
  with Unknown_device_type _ -> None

let parse_int i = 
  try
    Some (int_of_string i)
  with _ -> None

let parse_frontend_link x =
  match Stdext.Xstringext.String.split '/' x with
  | [ ""; "local"; "domain"; domid; "device"; kind; devid ] ->
    begin
      match parse_int domid, parse_kind kind, parse_int devid with
      | Some domid, Some kind, Some devid ->
        Some { domid = domid; kind = kind; devid = devid }
      | _, _, _ -> None
    end
  | _ -> None

let parse_backend_link x = 
  match Stdext.Xstringext.String.split '/' x with 
  | [ ""; "local"; "domain"; domid; "xenserver"; "backend"; kind; _; devid ]
  | [ ""; "local"; "domain"; domid; "backend"; kind; _; devid ] ->
    begin
      match parse_int domid, parse_kind kind, parse_int devid with
      | Some domid, Some kind, Some devid ->
        Some { domid = domid; kind = kind; devid = devid }
      | _, _, _ -> None
    end
  | _ -> None

let readdir ~xs d = try xs.Xs.directory d with Xs_protocol.Enoent _ -> []
let to_list ys = List.concat (List.map Opt.to_list ys)
let list_kinds ~xs dir = to_list (List.map parse_kind (readdir ~xs dir))

(* NB: we only read data from the frontend directory. Therefore this gives
   the "frontend's point of view", using data that we wrote into a subtree
   that is writable dom0 only (not by the frontend domain). *)
let list_frontends ~xs ?for_devids domid =
  let frontend_dir = sprintf "/xenops/domain/%d/device" domid in
  let kinds = list_kinds ~xs frontend_dir in
  List.concat (List.map
                 (fun k ->
                    let dir = sprintf "%s/%s" frontend_dir (string_of_kind k) in
                    let devids = match for_devids with
                      | None -> to_list (List.map parse_int (readdir ~xs dir))
                      | Some devids ->(* check that any specified devids are present in frontend_dir *)
                        List.filter (fun devid->try ignore(xs.Xs.read (sprintf "%s/%d" dir devid)); true with _->false) devids;
                    in
                    to_list (List.map
                               (fun devid ->
                                  (* domain [domid] believes it has a frontend for
                                     					   device [devid] *)
                                  let frontend = { domid = domid; kind = k; devid = devid } in
                                  try
                                    let link = xs.Xs.read (sprintf "%s/%d/backend" dir devid) in
                                    match parse_backend_link link with
                                    | Some b -> Some { backend = b; frontend = frontend }
                                    | None -> None
                                  with _ -> None
                               ) devids)
                 ) kinds)

(* NB: we only read data from the backend directory. Therefore this gives
   the "backend's point of view". *)
let list_backends ~xs domid =
  let backend_dir = xs.Xs.getdomainpath domid ^ "/backend" in
  let kinds = list_kinds ~xs backend_dir in

  List.concat (List.map
                 (fun k ->
                    let dir = sprintf "%s/%s" backend_dir (string_of_kind k) in
                    let domids = to_list (List.map parse_int (readdir ~xs dir)) in
                    List.concat (List.map
                                   (fun frontend_domid ->
                                      let dir = sprintf "%s/%s/%d" backend_dir (string_of_kind k) frontend_domid in
                                      let devids = to_list (List.map parse_int (readdir ~xs dir)) in
                                      to_list (List.map
                                                 (fun devid ->
                                                    (* domain [domid] believes it has a backend for
                                                       							   [frontend_domid] of type [k] with devid [devid] *)
                                                    let backend = { domid = domid; kind = k; devid = devid } in
                                                    try
                                                      let link = xs.Xs.read (sprintf "%s/%d/frontend" dir devid) in
                                                      match parse_frontend_link link with
                                                      | Some f -> Some { backend = backend; frontend = f }
                                                      | None -> None
                                                    with _ -> None
                                                 ) devids)
                                   ) domids)
                 ) kinds)

(** Return a list of devices connecting two domains. Ignore those whose kind 
    we don't recognise *)
let list_devices_between ~xs driver_domid user_domid = 
  List.filter
    (fun d -> d.frontend.domid = user_domid) 
    (list_backends ~xs driver_domid)


let print_device domid kind devid =
  sprintf "(domid=%d | kind=%s | devid=%s)" domid kind devid


type protocol = Protocol_Native | Protocol_X86_32 | Protocol_X86_64

let string_of_protocol = function
  | Protocol_Native -> "native"
  | Protocol_X86_32 -> "x86_32-abi"
  | Protocol_X86_64 -> "x86_64-abi"

let protocol_of_string = function
  | "native" -> Protocol_Native
  | "x86_32-abi" -> Protocol_X86_32
  | "x86_64-abi" -> Protocol_X86_64
  | s            -> raise (Unknown_device_protocol s)


let qemu_save_path : (_, _, _) format = "/var/lib/xen/qemu-save.%d"
let qemu_restore_path : (_, _, _) format = "/var/lib/xen/qemu-resume.%d"

let demu_save_path : (_, _, _) format = "/var/lib/xen/demu-save.%d"
let demu_restore_path : (_, _, _) format = "/var/lib/xen/demu-resume.%d"

let var_run_xen_path = "/var/run/xen"
let qmp_libxl_path = (sprintf "%s/qmp-libxl-%d") var_run_xen_path
let qmp_event_path = (sprintf "%s/qmp-event-%d") var_run_xen_path

(* Where qemu writes its state and is signalled *)
let device_model_path ~qemu_domid domid = sprintf "/local/domain/%d/device-model/%d" qemu_domid domid

let xenops_domain_path = "/xenops/domain"
let xenops_path_of_domain domid = sprintf "%s/%d" xenops_domain_path domid
let xenops_vgpu_path domid devid =
  sprintf "%s/device/vgpu/%d" (xenops_path_of_domain domid) devid

let is_upstream_qemu domid =
  try
    with_xs (fun xs -> xs.Xs.read (sprintf "/libxl/%d/dm-version" domid)) = "qemu_xen"
  with _ -> false

(** [make_id_generator] returns a function that creates unique IDs using
 * an internal counter *)
let make_id_generator name =
  let count = ref 0 in
  fun domid -> incr count; Printf.sprintf "%s-%06d-%d" name !count domid

let make_qmp_id = make_id_generator "qmp"

(** [qmp_send_cmd_internal connection domid cmd] sends [cmd] to QEMU, waits
 * for the *matching* response, and returns the result. It uses an
 * already open and negotiated [connection]. Commands are tagged with an
 * identifier and the function waits for the matching response, skipping
 * all other responses it receives while waiting.
*)
let qmp_send_cmd_internal connection domid cmd =
  let id      = make_qmp_id domid in
  let msg     = Qmp.Command(Some id, cmd) in
  let msg'    = Qmp.string_of_message msg in
  debug "QMP command for domid %d: %s" domid msg';
  let rec wait_for_result id =
    match Qmp_protocol.read connection with
    (* no ID *)
    | Qmp.Greeting(_)
    | Qmp.Command(_, _)
    | Qmp.Error(None, _)
    | Qmp.Success(None, _)
    | Qmp.Event(_) as resp ->
      let resp' = Qmp.string_of_message resp in
      debug "skipping QMP message while waiting for %s: %s (%s)"
        id resp' __LOC__;
      wait_for_result id

    (* wrong ID *)
    | Qmp.Success(Some id',_)
    | Qmp.Error(Some id',_) as resp when id <> id' ->
      let resp' = Qmp.string_of_message resp in
      debug "skipping QMP message while waiting for %s: %s (%s)"
        id resp' __LOC__;
      wait_for_result id

    (* correct ID *)
    | Qmp.Success(Some id, _)
    | Qmp.Error(Some id,_) as message ->
        debug "received QMP response %s (%s)" id __LOC__;
        message
  in
  Qmp_protocol.write connection msg;
  wait_for_result id

let with_qmp_connection domid f =
  let exec f =
    try f ()
    with e ->
      warn "QMP connection error for domain %d" domid;
      raise (QMP_connection_error (domid, Printexc.to_string e))
  in
  let connection = exec (fun () ->
      Qmp_protocol.connect (qmp_libxl_path domid)
    )
  in
  finally
    (fun () -> f connection)
    (fun () -> exec (fun () ->
         Qmp_protocol.close connection
       )
    )

(* [qmp_send_cmd domid cmd] sends [cmd] to [domid] and checks that the
 * result it returns is Success. Otherwise it will raise [QMP_Error].
 *
 * [qmp_send_cmd] can send optionally a file descriptor [send_fd]
 * over the connection before it sends the command. This is required for
 * some commands.
*)
let qmp_send_cmd ?send_fd domid cmd =
  with_qmp_connection domid (fun connection ->
      let result =
        try
          (* no mutex required: QMP never sends unrelated messages *)
          Qmp_protocol.negotiate connection;
          begin match send_fd with
            | Some fd ->
              let connection' = Qmp_protocol.to_fd connection in
              Fd_send_recv.send_fd_substring connection' " " 0 1 [] fd |> ignore
            | None    -> ()
          end;
          qmp_send_cmd_internal connection domid cmd
        with e ->
          let cmd' = Qmp.(string_of_message (Command(None,cmd))) in
          error "sending QMP command '%s' to domain %d: %s (%s)"
            cmd' domid (Printexc.to_string e) __LOC__;
          raise (QMP_connection_error(domid, Printexc.to_string e))
      in
      match result with
      | Qmp.(Success (_, result)) -> result
      | message ->
        let msg' = Qmp.string_of_message message in
        error "QMP result for domid %d: %s (%s)" domid msg' __LOC__;
        raise (QMP_Error(domid, msg'))
    )
