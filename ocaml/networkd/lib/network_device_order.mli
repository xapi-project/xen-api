(*
 * Copyright (c) Cloud Software Group, Inc.
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

(** Generate an order for host network devices and keep the order as stable as
    possible. *)

type error =
  | Pci_addr_parse_error of string
  | Mac_addr_parse_error of string
  | Rule_parse_error of string
  | Missing_biosdevname_key of string
  | Duplicate_mac_address
  | Duplicate_position
  | Invalid_biosdevname_key_value of (string * string)

val string_of_error : error -> string
(** [string_of_error e] returns a string representation of the error [e]. *)

(** PCI address in format SBDF: domain:bus:device:function *)
module Pciaddr : sig
  (** Type of the PCI address *)
  type t = Xcp_pci.address

  val of_string : string -> (t, error) result
  (** [of_string s] returns [Ok pci] where [pci] is the PCI address converted
      from [s]. Otherwise, it returns [Error error] whenever [s] can't be
      parsed. [error] is for the parsing failure. *)

  val compare : t -> t -> int
  (** [compare x y] return 0 if [x] is equal to [y]; a negative integer if [x]
      is less than [y], and a positive integer if [x] is greater than [y]. *)
end

module Macaddr : sig
  type t = Macaddr.t

  val of_string : string -> (t, error) result
  (** [of_string s] returns [Ok pci] where [pci] is the PCI address converted
      from [s]. Otherwise, it returns [Error error] whenever [s] can't be
      parsed. [error] is for the parsing failure. *)
end

(** A rule specifies a position for a network device which can be identified by
    MAC address, PCI address, or name label. *)
module Rule : sig
  type index =
    | Mac_addr of Macaddr.t
    | Pci_addr of Pciaddr.t
    | Label of string  (** Type of mapping *)

  (** Type of one mapping configuration. *)
  type t = {position: int; index: index}

  val read : path:string -> (t list, error) result
  (** [read ~path] returns either [Ok rules], where [rules] are parsed from the
      content of the file at [path], or [Error error], where [error] is the
      reason for the parsing failure. The file at [path] contains lines in the
      following format:
        <N>:<label|mac|pci>="<value>", where
        label: means the <value> is the name label of the device,
        mac: means the <value> is the MAC address of the device like
             00:02:C9:ED:FD:F0,
        pci: means the <value> is the PCI address (in SBDF format) of the device
             locates at, like 0000:05:00.0. *)

  val matches : mac:Macaddr.t -> pci:Pciaddr.t -> label:string -> t -> bool
  (** [true] if any of the [mac], [pci], or [label] meets the rule [t]. *)
end

(** A network device recognized by biosdevname *)
module Dev : sig
  (** Type of an network deivce parsed from the output of biosdevname. *)
  type t = {
      name: Network_interface.iface
    ; mac: Network_interface.mac_address
    ; pci: Xcp_pci.address
    ; bios_eth_order: int
          (** The <N> in eth<N> which is the value of "BIOS device" from output
              of [biosdevname --policy all_ethN], is greater than or equal to 0.
            *)
    ; multi_nic: bool
          (** [true] if there are other devices locate at the same PCI address.
              Otherwise [false]. *)
  }

  val get_all : unit -> (t list, error) result
  (** [get_all ()] returns [Ok l], where l is a list of network devices parsed
      from the output of biosdevname. Otherwise, it returns [Error error], where
      [error] is the reason for the parsing failure. *)
end

module IntMap : Map.S with type key = int

(** A network device which has been assigned a postion in the order  by sorting *)
module OrderedDev : sig
  (** Type of an ordered network device. *)
  type t = Network_interface.ordered_iface

  val map_by_position : t list -> (t IntMap.t, error) result
  (** [map_by_position lst] returns [Ok map], where [map] is a map with values
      from [lst] and their keys are positions. It returns
      [Error Duplicate_position] if more than one value in [lst] has the same
      position. *)

  val validate_order : t list -> (t list, error) result
  (** [validate_order devs] returns [Ok lst], where [lst] is a list of devices
      without duplicate MAC addresses or duplicate positions. Otherwise,
      [Error error] is returned, where [error] is either Duplicate_position or
      Duplicate_mac_address. *)

  val assign_position : Dev.t -> int -> Network_interface.ordered_iface
  (** [assign_position dev pos] returns a device with [pos] assigned. *)
end

val sort :
     OrderedDev.t list
  -> (OrderedDev.t list * (string * string) list, error) result
(** [sort last_order] sorts and generates an order based on [last_order]. It
    returns [Ok (order, changes)], where [order] is a list of devices each
    assigned unique positions, and [changes] is a list of pairs like
    [(old, new)]. In these pairs, [old] is the name of the device from the
    previous call to [sort], and [new] is the current name of the device.
    It returns [Error error] when it fails to generate an order.[error] is the
    reason for the failure. *)

(* Below is exposed only for unit tests *)

val sort' :
     currents:Dev.t list
  -> rules:Rule.t list
  -> last_order:OrderedDev.t list
  -> (OrderedDev.t list, error) result
