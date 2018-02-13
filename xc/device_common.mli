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

type kind = Vif | Tap | Pci | Vfs | Vfb | Vkbd | Vbd of string
val vbd_kind_of_string : string -> kind
val default_vbd_frontend_kind : kind

type devid = int

(** Represents one end of a device *)
type endpoint = { domid: int; kind: kind; devid: int }

(** Represent a device as a pair of endpoints *)
type device = { 
  frontend: endpoint;
  backend: endpoint
}
val rpc_of_device: device -> Rpc.t
val device_of_rpc: Rpc.t -> device

exception Device_frontend_already_connected of device
exception Device_disconnect_timeout of device
exception Device_error of device * string
exception Device_unrecognized of string
exception Hotplug_script_expecting_field of device * string
exception QMP_Error of int * string (** domid, message *)

val backend_path : xs:Xenstore.Xs.xsh -> endpoint -> Xenctrl.domid -> string
val backend_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val frontend_rw_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val frontend_ro_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val disconnect_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val kthread_pid_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val error_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val backend_error_path_of_device : xs:Xenstore.Xs.xsh -> device -> string

val backend_shutdown_request_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val backend_shutdown_done_path_of_device : xs:Xenstore.Xs.xsh -> device -> string

val backend_pause_request_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val backend_pause_token_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val backend_pause_done_path_of_device : xs:Xenstore.Xs.xsh -> device -> string
val backend_state_path_of_device : xs:Xenstore.Xs.xsh -> device -> string

val get_private_path : Xenctrl.domid -> string
val get_private_path_by_uuid : Uuidm.t -> string
val get_private_data_path_of_device : device -> string

val extra_xenserver_path_of_device: xs:Xenstore.Xs.xsh -> device -> string

val string_of_endpoint : endpoint -> string
val string_of_device : device -> string
val string_of_kind : kind -> string
val kind_of_string : string -> kind

(** [list_backends xs domid] returns a list of devices where there is a
    	backend in [domid]. This function only reads data stored in the backend
    directory.*)
val list_backends : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> device list

(** [list_frontends xs domid] returns a list of devices where there is a
    	frontend in [domid]. This function only reads data stored in the frontend
    directory.*)
val list_frontends : xs:Xenstore.Xs.xsh -> ?for_devids:int list -> Xenctrl.domid -> device list

(** Return a list of devices connecting two domains. Ignore those whose kind 
    we don't recognise *)
val list_devices_between : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> Xenctrl.domid -> device list

val device_of_backend : endpoint -> Xenctrl.domid -> device

val add_backend_keys : xs:Xenstore.Xs.xsh -> device -> string -> (string * string) list -> unit
val remove_backend_keys: xs:Xenstore.Xs.xsh -> device -> string -> string list -> unit

type protocol = Protocol_Native | Protocol_X86_32 | Protocol_X86_64
val string_of_protocol : protocol -> string
val protocol_of_string : string -> protocol

val qemu_save_path: (int -> 'a, 'b, 'a) format
val qemu_restore_path: (int -> 'a, 'b, 'a) format

val demu_save_path: (int -> 'a, 'b, 'a) format
val demu_restore_path: (int -> 'a, 'b, 'a) format

val var_run_xen_path: string
val qmp_libxl_path: int -> string
val qmp_event_path: int -> string

(** Directory in xenstore where qemu writes its state *)
val device_model_path: qemu_domid:int -> int -> string

val xenops_domain_path: string
val xenops_path_of_domain: Xenctrl.domid -> string
val xenops_vgpu_path: Xenctrl.domid -> devid -> string
val is_upstream_qemu: Xenctrl.domid -> bool
val qmp_send_cmd
  : ?send_fd:Unix.file_descr (* send this fd ahead of command *)
  -> Xenctrl.domid
  -> Qmp.command
  -> Qmp.result (** may raise QMP_Error *)
