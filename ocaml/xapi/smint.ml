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
(**
 * @group Storage
*)

module D=Debug.Make(struct let name="smint" end)
open D

type vdi_info = {
  vdi_info_uuid: string option;
  vdi_info_location: string;
}

(** Very primitive first attempt at a set of backend features *)
type capability =
  | Sr_create | Sr_delete | Sr_attach | Sr_detach | Sr_scan | Sr_probe | Sr_update
  | Sr_supports_local_caching
  | Sr_stats
  | Sr_metadata
  | Sr_trim
  | Sr_multipath
  | Vdi_create | Vdi_delete | Vdi_attach | Vdi_detach | Vdi_mirror
  | Vdi_clone | Vdi_snapshot | Vdi_resize | Vdi_activate | Vdi_deactivate
  | Vdi_update | Vdi_introduce
  | Vdi_resize_online
  | Vdi_generate_config
  | Vdi_attach_offline
  | Vdi_reset_on_boot
  | Vdi_configure_cbt
  | Large_vdi (** Supports >2TB VDIs *)
  | Thin_provisioning

type feature = capability * int64

let string_to_capability_table = [
  "SR_PROBE",       Sr_probe;
  "SR_UPDATE",      Sr_update;
  "SR_SUPPORTS_LOCAL_CACHING", Sr_supports_local_caching;
  "SR_METADATA",    Sr_metadata;
  "SR_TRIM",        Sr_trim;
  "SR_MULTIPATH",   Sr_multipath;
  "VDI_CREATE",     Vdi_create;
  "VDI_DELETE",     Vdi_delete;
  "VDI_ATTACH",     Vdi_attach;
  "VDI_DETACH",     Vdi_detach;
  "VDI_MIRROR",     Vdi_mirror;
  "VDI_RESIZE",     Vdi_resize;
  "VDI_RESIZE_ONLINE",Vdi_resize_online;
  "VDI_CLONE",      Vdi_clone;
  "VDI_SNAPSHOT",   Vdi_snapshot;
  "VDI_ACTIVATE",   Vdi_activate;
  "VDI_DEACTIVATE", Vdi_deactivate;
  "VDI_UPDATE",     Vdi_update;
  "VDI_INTRODUCE",  Vdi_introduce;
  "VDI_GENERATE_CONFIG", Vdi_generate_config;
  "VDI_ATTACH_OFFLINE", Vdi_attach_offline;
  "VDI_RESET_ON_BOOT", Vdi_reset_on_boot;
  "VDI_CONFIG_CBT", Vdi_configure_cbt;
  "SR_STATS", Sr_stats;
  "LARGE_VDI", Large_vdi;
  "THIN_PROVISIONING", Thin_provisioning;
]
let capability_to_string_table = List.map (fun (k, v) -> v, k) string_to_capability_table

let string_of_capability c = List.assoc c capability_to_string_table

let string_of_feature (c,v) =
  Printf.sprintf "%s/%Ld" (string_of_capability c) v

let has_capability (c : capability) fl = List.mem_assoc c fl

let capability_of_feature : feature -> capability = fst

let parse_string_int64_features strings =
  let text_features =
    List.filter
      (fun s ->
         let s = List.hd (Stdext.Xstringext.String.split '/' s) in
         let p = List.mem s (List.map fst string_to_capability_table) in
         if not p then debug "SM.feature: unknown feature %s" s;
         p)
      strings in
  List.map
    (fun c ->
       match Stdext.Xstringext.String.split '/' c with
       | [] -> failwith "parse_feature" (* not possible *)
       | [cs] -> (cs, 1L) (* default version *)
       | [cs; vs]
       | cs :: vs :: _ ->
         try
           let v = int_of_string vs in
           (cs, if v < 1 then 1L else Int64.of_int v)
         with _ ->
           debug "SM.feature %s has bad version %s, defaulting to 1" cs vs;
           (cs, 1L))
    text_features

let parse_capability_int64_features strings =
  List.map
    (function c,v ->
       ((List.assoc c string_to_capability_table), v))
    (parse_string_int64_features strings)

type sr_driver_info = {
  sr_driver_filename: string;
  sr_driver_name: string;
  sr_driver_description: string;
  sr_driver_vendor: string;
  sr_driver_copyright: string;
  sr_driver_version: string;
  sr_driver_required_api_version: string;
  sr_driver_features: feature list;
  sr_driver_text_features: string list;
  sr_driver_configuration: (string * string) list;
  sr_driver_required_cluster_stack: string list;
}

let query_result_of_sr_driver_info x = {
  Storage_interface.driver = x.sr_driver_filename;
  name = x.sr_driver_name;
  description = x.sr_driver_description;
  vendor = x.sr_driver_vendor;
  copyright = x.sr_driver_copyright;
  version = x.sr_driver_version;
  required_api_version = x.sr_driver_required_api_version;
  features = x.sr_driver_text_features;
  configuration = x.sr_driver_configuration;
  required_cluster_stack = x.sr_driver_required_cluster_stack;
}

type attach_info = {
  params : string;
  o_direct : bool;
  o_direct_reason : string;
  xenstore_data : (string * string) list;
}


exception Backend_missing_field of string
exception Backend_report_error of (int * string)
exception Command_failed of (int * string * string (*stdout*) * string (*stderr*))
exception Command_killed of (int * string * string (*stdout*) * string (*stderr*))
exception Missing_field of string

exception Not_implemented_in_backend (* Raised by clone at least *)

exception Sr_not_empty
exception Vdi_in_use
exception Device_in_use

(** Identifies where a request should go to (i.e. a URI) or None if unknown *)
type request = string option
