(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

type enabled = {
  enabled : bool; (** feature is present and turned on *)
  present : bool; (** feature is present (but may not be turned on) *)
}

type vnc = {
  enabled : bool;
  auth    : string;
  family  : string;
  service : int;
  host    : string;
}

type xen_platform_pv_driver_info = {
  product_num : int;
  build_num   : int;
}

type fd_info = {
  fd       : int;
  fdset_id : int;
}

(** QOM properties - https://wiki.qemu.org/index.php/Documentation
                     https://qemu.weilnetz.de/doc/qemu-qmp-ref.html*)
type qom = {
  name : string;
  ty   : string;
}

module Device : sig
  module USB : sig
    module Driver : sig
      type t = USB_EHCI | USB_HOST
      val string_of : t -> string
    end
    type params_t = { bus: string; hostbus: string; hostport: string; }
    type t = { id: string; params: params_t option }
  end
  module VCPU : sig
    module Driver : sig
      type t = QEMU32_I386_CPU
      val string_of : t -> string
    end
    type t = { id: string; socket_id: int; core_id: int; thread_id: int; }
    val id_of : socket_id: int -> core_id: int -> thread_id: int -> string
    type hotpluggable_t = { driver_type: string; vcpus_count: int; props: t; qom_path: string option; }
  end
  type t = USB of USB.t | VCPU of VCPU.t
end

(* according to qapi schema at https://github.com/qemu/qemu/blob/master/qapi-schema.json#L1478 *)
type device_add_t = {
  driver : string; (* only required field is driver *)
  device : Device.t;
}

type result =
    Name_list of string list
  | Enabled of enabled
  | Status of string
  | Vnc of vnc
  | Xen_platform_pv_driver_info of xen_platform_pv_driver_info
  | Hotpluggable_cpus of Device.VCPU.hotpluggable_t list
  | Fd_info of fd_info
  | Unit
  | Qom of qom list
(** A successful RPC result *)

type greeting = {
  major : int;      (** qemu major version *)
  minor : int;      (** qemu minor version *)
  micro : int;      (** qemu micro version *)
  package : string; (** some information about the (binary?) package *)
}

type event_data =

    (** emitted when the guest changes the RTC time *)
  | RTC_CHANGE of int64

    (** emitted when the XEN PV driver writes build number to io-port 0x10, marking the end of the preamble *)
  | XEN_PLATFORM_PV_DRIVER_INFO of xen_platform_pv_driver_info

   (* extend this to support other qmp events data*)

type event = {
  timestamp : int * int; (** time the event occurred in (seconds, microseconds) *)
  event : string;    (** type of event *)
  data: event_data option
}

type error = { cls : string; descr : string; }

type id = string
(** identifier used to match responses with original requests *)


type medium =
  { medium_device:   string
  ; medium_filename: string
  ; medium_format:   string option
  }

type command =
    Qmp_capabilities
  | Query_commands
  | Query_kvm
  | Query_status
  | Query_vnc
  | Query_xen_platform_pv_driver_info
  | Query_hotpluggable_cpus
  | Stop
  | Cont
  | Eject of string * bool option
  | Change of string * string * string option
  | System_powerdown
  | Xen_save_devices_state of string
  | Xen_load_devices_state of string
  | Xen_set_global_dirty_log of bool
  | Add_fd of int option
  | Remove_fd of int
  | Blockdev_change_medium of medium
  | Device_add of device_add_t
  | Device_del of string
  | Qom_list of string
(** commands that may be sent to qemu *)

type message =
    Greeting of greeting
  | Command of (id option * command)
  | Error of (id option * error)
  | Success of (id option * result)
  | Event of event
(** an individual message sent or received to or from qemu *)

val _JSONParsing : string

val message_of_string : string -> message
val string_of_message : message -> string

val json_of_message : message -> Yojson.Safe.json
