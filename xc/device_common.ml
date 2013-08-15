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

type kind = Vif | Vbd | Tap | Pci | Vfs | Vfb | Vkbd
with rpc

type devid = int
(** Represents one end of a device *)
type endpoint = { domid: int; kind: kind; devid: int }
with rpc

(** Represent a device as a pair of endpoints *)
type device = { 
  frontend: endpoint;
  backend: endpoint
}
with rpc

exception Device_frontend_already_connected of device
exception Device_disconnect_timeout of device
exception Device_error of device * string
exception Device_unrecognized of string
exception Hotplug_script_expecting_field of device * string
exception Unknown_device_type of string
exception Unknown_device_protocol of string

module D = Debug.Make(struct let name = "xenops" end)
open D

open Printf

let string_of_kind = function
  | Vif -> "vif" | Vbd -> "vbd" | Tap -> "tap" | Pci -> "pci" | Vfs -> "vfs" | Vfb -> "vfb" | Vkbd -> "vkbd"
let kind_of_string = function
  | "vif" -> Vif | "vbd" -> Vbd | "tap" -> Tap | "pci" -> Pci | "vfs" -> Vfs | "vfb" -> Vfb | "vkbd" -> Vkbd
  | x -> raise (Unknown_device_type x)

let string_of_endpoint (x: endpoint) =
  sprintf "(domid=%d | kind=%s | devid=%d)" x.domid (string_of_kind x.kind) x.devid  

let backend_path ~xs (backend: endpoint) (domu: Xenctrl.domid) = 
  sprintf "%s/backend/%s/%u/%d" 
    (xs.Xs.getdomainpath backend.domid) 
    (string_of_kind backend.kind)
    domu backend.devid

(** Location of the backend in xenstore *)
let backend_path_of_device ~xs (x: device) = backend_path ~xs x.backend x.frontend.domid

(** Location of the frontend in xenstore *)
let frontend_path_of_device ~xs (x: device) = 
  sprintf "%s/device/%s/%d"
    (xs.Xs.getdomainpath x.frontend.domid)
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
	sprintf "%s/device/%s/%d/kthread-pid"
		(xs.Xs.getdomainpath x.backend.domid)
		(string_of_kind x.backend.kind)
		x.frontend.devid

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

let string_of_device (x: device) = 
  sprintf "frontend %s; backend %s" (string_of_endpoint x.frontend) (string_of_endpoint x.backend)

let device_of_backend (backend: endpoint) (domu: Xenctrl.domid) = 
  let frontend = { domid = domu;
		   kind = (match backend.kind with
			   | Vbd | Tap -> Vbd
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

let slash = Re_str.regexp_string "/"

let parse_frontend_link x =
	match Re_str.split slash x with
		| [ "local"; "domain"; domid; "device"; kind; devid ] ->
			begin
				match parse_int domid, parse_kind kind, parse_int devid with
					| Some domid, Some kind, Some devid ->
						Some { domid = domid; kind = kind; devid = devid }
					| _, _, _ -> None
			end
		| _ -> None

let parse_backend_link x = 
	match Re_str.split slash x with
		| [ "local"; "domain"; domid; "backend"; kind; _; devid ] ->
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
   the "frontend's point of view". *)
let list_frontends ~xs domid = 
	let frontend_dir = xs.Xs.getdomainpath domid ^ "/device" in
	let kinds = list_kinds ~xs frontend_dir in
	List.concat (List.map
		(fun k ->
			let dir = sprintf "%s/%s" frontend_dir (string_of_kind k) in
			let devids = to_list (List.map parse_int (readdir ~xs dir)) in
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

(* Where qemu writes its state and is signalled *)
let device_model_path ~qemu_domid domid = sprintf "/local/domain/%d/device-model/%d" qemu_domid domid

