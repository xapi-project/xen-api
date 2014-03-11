(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
(** Interface to the Inventory file *)

(** The inventory file is a file containing key-value pair of information
 *  related to the host. It is stored at /etc/xensource-inventory. *)

(** Thrown when trying to retrieve a non-existing key. *)
exception Missing_inventory_key of string

(** Reads the inventory file from disk. *)
val read_inventory: unit -> unit

(** Clears the copy of the inventory file in memory and reads the file from disk. *)
val reread_inventory: unit -> unit

(** Return the value of key [key] in the inventory file. If the key does not exist,
 *  returns the default if supplied, or throws {!Missing_inventory_key} otherwise. *)
val lookup: ?default:string -> string -> string

(** Remove the key with the given name from the inventory file, if it exists. *)
val remove: string -> unit

(** Change the contents of key [key] in the inventory file to [value]. The key
 *  is added if it does not yet exist. *)
val update: string -> string -> unit

(** Parses a line [line] from the inventory file, and returns a key-value pair if successful. *)
val parse_inventory_entry: string -> (string * string) option

(* Keys defined in Geneva *)
(** UUID of the Host object in the xapi database *)
val _installation_uuid : string
(*
(** Brand name, such as "XenServer" *)
val _product_brand : string

(** Product name, such as "xenenterprise" *)
val _product_name : string

(** Product version *)
val _product_version : string

(** Build number *)
val _build_number : string

(** Dom0 kernel version *)
val _kernel_version : string

(** Xen version *)
val _xen_version : string

(** Date on which the host was installed *)
val _installation_date : string

(** UUID of the default SR (?) *)
val _default_sr : string

(** Device path of primary disk *)
val _primary_disk : string

(** Device path of backup partition *)
val _backup_partition : string

(** Device path of the default SR used for local storage *)
val _default_sr_physdevs : string

(** Memory size of dom0 (?) *)
val _dom0_mem : string
*)

(* Keys defined in Rio *)
(** UUID of the control domain (dom0) *)
val _control_domain_uuid : string

(** Interface name of the management PIF *)
val _management_interface : string

(** Primary address type of the management PIF *)
val _management_address_type : string

(* Keys defined in Miami *)
(** OEM manufacturer name *)
val _oem_manufacturer : string

(** OEM model name *)
val _oem_model : string

(** OEM edition build number *)
val _oem_build_number : string

(** Machine serial number *)
val _machine_serial_number : string

(** Machine serial name *)
val _machine_serial_name : string

(** Stunnel timeout *)
val _stunnel_idle_timeout : string

(* Keys defined in Orlando, redefined in MNR *)
(** List of bridges that are automatically brought up when the host starts up *)
val _current_interfaces : string
