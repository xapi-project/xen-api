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
open Xstringext
open Hashtblext
open Pervasiveext

type kind = Vif | Vbd | Tap | Pci | Vfb | Vkbd

type devid = int
(** Represents one end of a device *)
type endpoint = { domid: Xc.domid; kind: kind; devid: int }

(** Represent a device as a pair of endpoints *)
type device = { 
  frontend: endpoint;
  backend: endpoint
}

exception Device_frontend_already_connected of device
exception Device_backend_vanished of device
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
  | Vif -> "vif" | Vbd -> "vbd" | Tap -> "tap" | Pci -> "pci" | Vfb -> "vfb" | Vkbd -> "vkbd"
let kind_of_string = function
  | "vif" -> Vif | "vbd" -> Vbd | "tap" -> Tap | "pci" -> Pci | "vfb" -> Vfb | "vkbd" -> Vkbd
  | x -> raise (Unknown_device_type x)

let string_of_endpoint (x: endpoint) =
  sprintf "(domid=%d | kind=%s | devid=%d)" x.domid (string_of_kind x.kind) x.devid  

let backend_path ~xs (backend: endpoint) (domu: Xc.domid) = 
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

let string_of_device (x: device) = 
  sprintf "frontend %s; backend %s" (string_of_endpoint x.frontend) (string_of_endpoint x.backend)

let device_of_backend (backend: endpoint) (domu: Xc.domid) = 
  let frontend = { domid = domu;
		   kind = (match backend.kind with
			   | Vbd | Tap -> Vbd
			   | _ -> backend.kind);
		   devid = backend.devid } in
  { backend = backend; frontend = frontend }

(** Return a list of devices connecting two domains. Ignore those whose kind 
    we don't recognise *)
let list_devices_between ~xs driver_domid user_domid = 
  let backend_dir = xs.Xs.getdomainpath driver_domid ^ "/backend" in
  let kinds = try xs.Xs.directory backend_dir with Xb.Noent -> [] in
  let kinds = List.concat 
    (List.map 
       (fun k -> 
	try [ kind_of_string k ]
	with Unknown_device_type _ ->
	  debug "Ignoring unknown backend device type: %s" k;
	  []) kinds) in
  List.concat
    (List.map 
       (fun k ->
	  let dir = sprintf "%s/%s/%d" backend_dir (string_of_kind k) user_domid in
	  let devids = try xs.Xs.directory dir with _ -> [] in
	  List.concat
	    (List.map 
	       (fun devid ->
		  try
		    let backend = { domid = driver_domid; kind = k; devid = int_of_string devid } in
		    (* Ignore if we fail to parse the frontend link *)
		    let fe = xs.Xs.read (dir ^ "/" ^ devid ^ "/frontend") in
		    let devid = int_of_string devid in
		    match String.split '/' fe with
		    | [ ""; "local"; "domain"; domid'; "device"; k'; devid' ] ->
			let domid' = int_of_string domid'
			and k' = kind_of_string k' 
			and devid' = int_of_string devid' in
			if domid' <> user_domid then begin
			  debug "Malformed frontend link %s: domid should be %d" fe user_domid;
			  []
			end else if devid' <> devid then begin
			  debug "Malformed frontend link %s: devid should be %d" fe devid;
			  []
			end else begin
			  let frontend = { domid = user_domid; kind = k'; devid = devid' } in
			  [ { frontend = frontend; backend = backend } ]
			end
		    | _ -> 
			debug "Malformed frontend link %s" fe;
			[]
		  with _ -> []) devids)
       ) kinds
    )

  

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
