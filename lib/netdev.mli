(*
 * Copyright (C) 2009 Citrix Systems Inc.
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
(** Interface to the domain 0 network stack. *)

(** Enumerates possible network backend types. *)
type kind = 
    Bridge  (** Linux Bridge based networking *)
  | Vswitch (** Vswitch based networking *)

(** Possible operations on each network backend type. *)
type network_ops = {
  kind : kind;                              (** The type of network backend. *)
  add : string -> ?uuid:string -> unit;     (** Add a bridge. *)
  del : string -> unit;                     (** Remove a bridge. *)
  list : unit -> string list;               (** List all bridges. *)
  exists : string -> bool;                  (** Query the existance of a bridge. *)
  intf_add : string -> string -> unit;      (** Add a network device as a port on a bridge. *)
  intf_del : string -> string -> unit;      (** Remove a network device from a bridge. *)
  intf_list : string -> string list;        (** List all network devices currently attached as a port on a bridge. *)
  get_bridge : string -> string;            (** Return the bridge to which a network device is currently attached. *)
  is_on_bridge : string -> bool;            (** Query whether a network device is currently attached to a bridge. *)
  set_forward_delay : string -> int -> unit;(** Set the forwarding delay for a device on a bridge. *)
}

(** Raised when an invalid network backend is detected.  *)
exception Unknown_network_backend of string

(** Raised when an operation in network_ops is not valid for a particular kind *)
exception Invalid_network_backend_operation of string * kind

(** Returns string name of a network backend type. *)
val string_of_kind : kind -> string

(** Converts a string to a valid network backend type, or raises Unknown_network_backend. *)
val kind_of_string : string -> kind

(** Module dealing with network device link characteristics *)
module Link :
  sig
    (** Link speed in megabits. *)
    type speed

    (** Convert speed to a string. *)
    val int_of_speed : speed -> int

    (** Create speed from a string. *)
    val speed_of_int : int -> speed

    (** Magic speed value representing Unknown. *)
    val speed_unknown : speed

    (** Device duplex. *)
    type duplex = 
      Duplex_unknown (** Device duplex is unknown. *)
    | Duplex_half    (** Device is running half-duplex. *)
    | Duplex_full    (** Device is running full-duplex. *)

    (** Convert duplex setting to string. *)
    val string_of_duplex : duplex -> string

    (** Create duplex from a string *)
    val duplex_of_string : string -> duplex

    (** Bring up a network device. *)
    val up : string -> unit

    (** Determine if a network device is up. *)
    val is_up : string -> bool

    (** Bring down a network device. *)
    val down : string -> unit

    (** Configure a device to allow or disallow multicast. *)
    val multicast : string -> bool -> unit

    (** Configure a device to respond to or ignore ARP requests. *)
    val arp : string -> bool -> unit

    (** Change the name of a network device. *)
    val change_name : string -> string -> unit

    (** Set MAC address of a device. *)
    val set_addr : string -> string -> unit

    (** Get current speed a duplex settings for a device. *)
    val get_status : string -> speed * duplex
  end

(** Module dealing with IP addresses on network devices. *)
module Addr :
  sig
    (** Flush all the addresses configured on a device. *)
    val flush : string -> unit

    (** Get all IPV4 addresses associated with a device. *)
    val get : string -> (Unix.inet_addr * Unix.inet_addr) list
  end

(** List all the interfaces on the system. *)
val list : unit -> string list

(** Return MAC address for a network device. *)
val get_address : string -> string

(** Get device MTU. *)
val get_mtu : string -> string

(** Set device MTU. *)
val set_mtu : string -> int -> unit

(** Returns the list of device names (eg physical + VLAN) which a particular MAC address. *)
val get_by_address : string -> string list

(** Returns the PCI bus path of a device. *)
val get_pcibuspath : string -> string

(** Returns the carrier status for a device. *)
val get_carrier : string -> bool

(** Returns PCI vendor and device ID for network device. *)
val get_ids : string -> string * string

(** Indicates whether the given interface is a physical interface. *)
val is_physical : string -> bool

(** Returns the device name of the given interface according to [biosdevname]. This is a name
 *  that is based on the BIOS name and should not be affected by changes in the Linux kernel or
 *  manual device renaming. *)
val get_bios_name : string -> string

(** Dispatch operation to correct backend device. *)
val network : network_ops
