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
open Device_common
open Xenops_task

exception Ioemu_failed of (string * string)

exception Device_shutdown
exception Device_not_found

exception Cdrom

(** Definition of available qemu profiles, used by the qemu backend implementations *)
module Profile: sig
  type t = Qemu_trad | Qemu_none | Qemu_upstream_compat | Qemu_upstream | Qemu_upstream_uefi
  (** available qemu profiles *)

  val typ_of: t Rpc.Types.typ
  val t: t Rpc.Types.def

  (** the fallback profile in case an invalid profile string is provided to [of_string] *)
  val fallback : t

  (** all available profiles *)
  val all: t list

  (** Valid names for a profile, used to define valid values for VM.platform.device-model *)
  module Name: sig
    val qemu_trad: string
    val qemu_upstream_compat: string
    val qemu_upstream: string
  end

  (** [wrapper_of profile] returns the qemu wrapper script path of a profile *)
  val wrapper_of: t -> string

  (** [of_string  profile_name] returns the profile of a profile name, and [fallback] if an invalid name is provided. *)
  val of_string : string -> t
end

(** Represent an IPC endpoint *)
module Socket :
sig
	type t = Unix of string | Port of int
end

module Generic :
sig
  val rm_device_state : xs:Xenstore.Xs.xsh -> device -> unit
  val exists : xs:Xenstore.Xs.xsh -> device -> bool
  val get_private_key: xs:Xenstore.Xs.xsh -> device -> string -> string
end

module Vbd :
sig
  type mode = ReadOnly | ReadWrite
  val string_of_mode : mode -> string
  val mode_of_string : string -> mode

  type physty = File | Phys | Qcow | Vhd | Aio
  val string_of_physty : physty -> string
  val physty_of_string : string -> physty
  val uses_blktap : phystype:physty -> bool

  type devty = CDROM | Disk | Floppy
  val string_of_devty : devty -> string
  val devty_of_string : string -> devty

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

  val add : Xenops_task.task_handle -> xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> hvm:bool -> t -> Xenctrl.domid -> device

  val release : Xenops_task.task_handle -> xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> device -> unit
  val media_eject : xs:Xenstore.Xs.xsh -> dm:Profile.t -> device -> unit
  val media_insert : xs:Xenstore.Xs.xsh -> dm:Profile.t -> phystype:physty -> params:string -> device -> unit
  val media_is_ejected : xs:Xenstore.Xs.xsh -> device -> bool

  val clean_shutdown_async : xs:Xenstore.Xs.xsh -> device -> unit
  val clean_shutdown_wait : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> ignore_transients:bool -> device -> unit

  (* For migration: *)
  val hard_shutdown_request : xs:Xenstore.Xs.xsh -> device -> unit
  val hard_shutdown_complete : xs:Xenstore.Xs.xsh -> device -> unit Watch.t
  val hard_shutdown_wait : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> timeout:float -> device -> unit
end

module Vif :
sig
  val add : xs:Xenstore.Xs.xsh -> devid:int 
    -> mac:string -> ?mtu:int -> ?rate:(int64 * int64) option
    -> ?backend_domid:Xenctrl.domid
    -> ?other_config:((string * string) list)
    -> netty:Netman.netty -> carrier:bool -> ?protocol:protocol 
    -> ?extra_private_keys:(string * string) list
    -> ?extra_xenserver_keys:(string * string) list
    -> Xenops_task.task_handle -> Xenctrl.domid
    -> device
  val set_carrier : xs:Xenstore.Xs.xsh -> device -> bool -> unit
  val release : Xenops_task.task_handle -> xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> device -> unit
  val move : xs:Xenstore.Xs.xsh -> device -> string -> unit
end

module NetSriovVf :
sig
  val add : xs:Xenstore.Xs.xsh -> devid:int 
    -> mac:string -> ?mtu:int -> ?rate:(int64 * int64) option
    -> ?backend_domid:Xenctrl.domid
    -> ?other_config:((string * string) list)
    -> pci:Xenops_interface.Pci.address -> vlan:int64 option -> carrier:bool
    -> ?extra_private_keys:(string * string) list
    -> ?extra_xenserver_keys:(string * string) list
    -> Xenops_task.task_handle ->  Xenctrl.domid
    -> device
end

val clean_shutdown : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> device -> unit
val hard_shutdown  : Xenops_task.task_handle  -> xs:Xenstore.Xs.xsh -> device -> unit

val can_surprise_remove : xs:Xenstore.Xs.xsh -> device -> bool

module Vcpu :
sig
  val add : xs:Xenstore.Xs.xsh -> dm:Profile.t -> devid:int -> int -> bool -> unit
  val del : xs:Xenstore.Xs.xsh -> dm:Profile.t -> devid:int -> int -> unit
  val set : xs:Xenstore.Xs.xsh -> dm:Profile.t -> devid:int -> int -> bool -> unit
  val status : xs:Xenstore.Xs.xsh -> dm:Profile.t -> devid:int -> int -> bool
end

module PV_Vnc :
sig
  exception Failed_to_start
  val save : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit
  val get_statefile : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> string option
  val start : ?statefile:string -> xs:Xenstore.Xs.xsh -> ?ip:string -> Xenctrl.domid -> unit
  val stop : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit

  val get_vnc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> Socket.t option
  val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
end

module Qemu :
sig
  module SignalMask: sig
    type t
    val create: unit -> t
    val set: t -> int -> unit
    val unset: t -> int -> unit
    val has: t -> int -> bool
  end
  val signal_mask : SignalMask.t
  val pid_path_signal : Xenctrl.domid -> string
  val pid : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
  val is_running : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> bool
end

module Vgpu :
sig
  val pid : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
  val is_running : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> bool
end

module PCI :
sig
  open Xenops_interface.Pci

  type t = {
    address: address;
    irq: int;
    resources: (int64 * int64 * int64) list;
    driver: string;
  }

  type supported_driver =
    | I915
    | Nvidia
    | Pciback

  exception Cannot_use_pci_with_no_pciback of t list

  val add : xs:Xenstore.Xs.xsh -> address list -> Xenctrl.domid -> unit
  val release : address list -> Xenctrl.domid -> unit
  val reset : xs:Xenstore.Xs.xsh -> address -> unit
  val bind : address list -> supported_driver -> unit
  val list : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> (int * address) list
end

module Vfs :
sig
  val add : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> ?backend_domid:int -> Xenctrl.domid -> unit
end

module Vfb :
sig
  val add : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> ?backend_domid:int -> ?protocol:protocol -> Xenctrl.domid -> unit
end

module Vkbd :
sig
  val add : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> ?backend_domid:int -> ?protocol:protocol -> Xenctrl.domid -> unit
end

module Dm :
sig
  type usb_opt =
    | Enabled of (string * int) list
    | Disabled
  type disp_intf_opt =
    | Std_vga
    | Cirrus
    | Vgpu of Xenops_interface.Vgpu.t list
    | GVT_d

  type disp_opt =
    | NONE
    | VNC of disp_intf_opt * string option * bool * int * string option (* IP address, auto-allocate, port if previous false, keymap *)
    | SDL of disp_intf_opt * string (* X11 display *)

  type media = Disk | Cdrom

  type info = {
    memory: int64;
    boot: string;
    firmware: Xenops_types.Vm.firmware_type;
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
    video_mib: int;
    xen_platform: (int * int) option;
    extras: (string * string option) list;
  }

  type qemu_args =
    { argv:     string list                     (** command line args *)
    ; fd_map:   (string * Unix.file_descr) list (** open files *)
    }

  val get_vnc_port : xs:Xenstore.Xs.xsh -> dm:Profile.t -> Xenctrl.domid -> Socket.t option
  val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option

  val signal : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> qemu_domid:int -> domid:Xenctrl.domid -> ?wait_for:string -> ?param:string
    -> string -> unit

  (** computes files and command line arguments to be passed to qemu *)
  val qemu_args
    :  xs:Xenstore.Xs.xsh
    -> dm:Profile.t
    -> info
    -> bool (** true = restore *)
    -> int  (** domid *)
    -> qemu_args

  val start : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> dm:Profile.t -> ?timeout:float -> info -> Xenctrl.domid -> unit
  val restore : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> dm:Profile.t -> ?timeout:float -> info -> Xenctrl.domid -> unit
  val assert_can_suspend : xs:Xenstore.Xs.xsh -> dm:Profile.t -> Xenctrl.domid -> unit
  val suspend : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> qemu_domid:int -> dm:Profile.t -> Xenctrl.domid -> unit
  val resume : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> qemu_domid:int -> Xenctrl.domid -> unit
  val stop : xs:Xenstore.Xs.xsh -> qemu_domid:int -> dm:Profile.t -> Xenctrl.domid -> unit
  val restore_vgpu : Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> Xenctrl.domid  -> Xenops_interface.Vgpu.t -> int -> Profile.t  -> unit
  val suspend_varstored: Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> Xenctrl.domid -> vm_uuid:string -> string
  val restore_varstored: Xenops_task.task_handle -> xs:Xenstore.Xs.xsh -> efivars:string -> Xenctrl.domid -> unit

  val after_suspend_image: xs:Xenstore.Xs.xsh -> dm:Profile.t -> qemu_domid:int -> int -> unit
end

module Backend: sig
  val init : unit -> unit
end

module Vusb :
sig
  val vusb_plug : xs:Xenstore.Xs.xsh -> privileged:bool -> domid:Xenctrl.domid -> id:string -> hostbus:string -> hostport: string -> version: string-> unit
  val vusb_unplug : xs:Xenstore.Xs.xsh -> privileged:bool -> domid:Xenctrl.domid -> id:string -> hostbus:string -> hostport:string -> unit
  val qom_list : xs:Xenstore.Xs.xsh -> domid:Xenctrl.domid -> string list
end

val get_vnc_port : xs:Xenstore.Xs.xsh -> dm:Profile.t -> Xenctrl.domid -> Socket.t option
val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
