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

val backend_path : xs:Xs.xsh -> endpoint -> Xc.domid -> string
val backend_path_of_device : xs:Xs.xsh -> device -> string
val frontend_path_of_device : xs:Xs.xsh -> device -> string
val disconnect_path_of_device : xs:Xs.xsh -> device -> string
val error_path_of_device : xs:Xs.xsh -> device -> string
val backend_error_path_of_device : xs:Xs.xsh -> device -> string

val backend_shutdown_request_path_of_device : xs:Xs.xsh -> device -> string
val backend_shutdown_done_path_of_device : xs:Xs.xsh -> device -> string

val backend_pause_request_path_of_device : xs:Xs.xsh -> device -> string
val backend_pause_token_path_of_device : xs:Xs.xsh -> device -> string
val backend_pause_done_path_of_device : xs:Xs.xsh -> device -> string

val string_of_endpoint : endpoint -> string
val string_of_device : device -> string
val string_of_kind : kind -> string
val kind_of_string : string -> kind

(** Return a list of devices connecting two domains. Ignore those whose kind 
    we don't recognise *)
val list_devices_between : xs:Xs.xsh -> Xc.domid -> Xc.domid -> device list

val device_of_backend : endpoint -> Xc.domid -> device

type protocol = Protocol_Native | Protocol_X86_32 | Protocol_X86_64
val string_of_protocol : protocol -> string
val protocol_of_string : string -> protocol

val qemu_save_path: (int -> 'a, 'b, 'a) format
val qemu_restore_path: (int -> 'a, 'b, 'a) format
