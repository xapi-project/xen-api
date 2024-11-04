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

module D = Debug.Make (struct let name = "smint" end)

open D

type vdi_info = {vdi_info_uuid: string option; vdi_info_location: string}

module Feature = struct
  (** Very primitive first attempt at a set of backend features *)
  type capability =
    | Sr_create
    | Sr_delete
    | Sr_attach
    | Sr_detach
    | Sr_scan
    | Sr_probe
    | Sr_update
    | Sr_supports_local_caching
    | Sr_stats
    | Sr_metadata
    | Sr_trim
    | Sr_multipath
    | Vdi_create
    | Vdi_delete
    | Vdi_attach
    | Vdi_detach
    | Vdi_mirror
    | Vdi_mirror_in
    | Vdi_clone
    | Vdi_snapshot
    | Vdi_resize
    | Vdi_activate
    | Vdi_activate_readonly
    | Vdi_deactivate
    | Vdi_update
    | Vdi_introduce
    | Vdi_resize_online
    | Vdi_generate_config
    | Vdi_attach_offline
    | Vdi_reset_on_boot
    | Vdi_configure_cbt
    | Vdi_compose
    | Large_vdi  (** Supports >2TB VDIs *)
    | Thin_provisioning
    | Vdi_read_caching

  type t = capability * int64

  let string_to_capability_table =
    [
      ("SR_CREATE", Sr_create)
    ; ("SR_DELETE", Sr_delete)
    ; ("SR_ATTACH", Sr_attach)
    ; ("SR_DETACH", Sr_detach)
    ; ("SR_SCAN", Sr_scan)
    ; ("SR_PROBE", Sr_probe)
    ; ("SR_UPDATE", Sr_update)
    ; ("SR_SUPPORTS_LOCAL_CACHING", Sr_supports_local_caching)
    ; ("SR_METADATA", Sr_metadata)
    ; ("SR_TRIM", Sr_trim)
    ; ("SR_MULTIPATH", Sr_multipath)
    ; ("SR_STATS", Sr_stats)
    ; ("VDI_CREATE", Vdi_create)
    ; ("VDI_DELETE", Vdi_delete)
    ; ("VDI_ATTACH", Vdi_attach)
    ; ("VDI_DETACH", Vdi_detach)
    ; ("VDI_MIRROR", Vdi_mirror)
    ; ("VDI_MIRROR_IN", Vdi_mirror_in)
    ; ("VDI_RESIZE", Vdi_resize)
    ; ("VDI_RESIZE_ONLINE", Vdi_resize_online)
    ; ("VDI_CLONE", Vdi_clone)
    ; ("VDI_SNAPSHOT", Vdi_snapshot)
    ; ("VDI_ACTIVATE", Vdi_activate)
    ; ("VDI_ACTIVATE_READONLY", Vdi_activate_readonly)
    ; ("VDI_DEACTIVATE", Vdi_deactivate)
    ; ("VDI_UPDATE", Vdi_update)
    ; ("VDI_INTRODUCE", Vdi_introduce)
    ; ("VDI_GENERATE_CONFIG", Vdi_generate_config)
    ; ("VDI_ATTACH_OFFLINE", Vdi_attach_offline)
    ; ("VDI_RESET_ON_BOOT", Vdi_reset_on_boot)
    ; ("VDI_CONFIG_CBT", Vdi_configure_cbt)
    ; ("VDI_COMPOSE", Vdi_compose)
    ; ("LARGE_VDI", Large_vdi)
    ; ("THIN_PROVISIONING", Thin_provisioning)
    ; ("VDI_READ_CACHING", Vdi_read_caching)
    ]

  let capability_to_string_table =
    List.map (fun (k, v) -> (v, k)) string_to_capability_table

  let known_features = List.map fst string_to_capability_table

  let capability_to_string c = List.assoc c capability_to_string_table

  let to_string (c, v) = Printf.sprintf "%s/%Ld" (capability_to_string c) v

  let capability_of : t -> capability = fst

  let unparse (f, v) = f ^ "/" ^ Int64.to_string v

  let string_int64_of_string_opt feature =
    match String.split_on_char '/' feature with
    | [] ->
        None
    | [feature] when List.mem feature known_features ->
        Some (feature, 1L)
    | feature :: version :: _ when List.mem feature known_features -> (
      try
        let v = Int64.(max 1L (of_string version)) in
        Some (feature, v)
      with _ ->
        debug "SM.feature: %s has bad version %s, defaulting to 1" feature
          version ;
        Some (feature, 1L)
    )
    | feature :: _ ->
        error "SM.feature: unknown feature %s" feature ;
        None

  (** [compat_features features1 features2] finds the compatible features in the input
  features lists. We assume features backwards compatible, i.e. if there are FOO/1 and
  FOO/2 are present, then we assume they can both do FOO/1*)
  let compat_features features1 features2 =
    let features2 = List.to_seq features2 |> Hashtbl.of_seq in
    List.filter_map
      (fun (f1, v1) ->
        match Hashtbl.find_opt features2 f1 with
        | Some v2 ->
            Some (f1, Int64.min v1 v2)
        | None ->
            None
      )
      features1

  let of_string_int64_opt (c, v) =
    List.assoc_opt c string_to_capability_table |> Option.map (fun c -> (c, v))

  (** [has_capability c fl] will test weather the required capability [c] is present 
  in the feature list [fl]. Callers should use this function to test if a feature
  is available rather than directly using membership functions on a feature list
  as this function might have special logic for some features. *)
  let has_capability (c : capability) (fl : t list) =
    List.exists
      (fun (c', _v) ->
        match (c, c') with
        | Vdi_mirror_in, Vdi_mirror ->
            true
        | c, c' when c = c' ->
            true
        | _ ->
            false
      )
      fl

  (** [parse_string_int64 features] takes a [features] list in its plain string
  forms such as "VDI_MIRROR/2" and parses them into the form of (VDI_MIRROR, 2).
  If the number is malformated, default to (VDI_MIRROR, 1). It will also deduplicate
  based on the capability ONLY, and randomly choose a verion, based on the order
  it appears in the input list.
  *)
  let parse_string_int64 features =
    List.filter_map string_int64_of_string_opt features
    |> List.sort_uniq (fun (x, _) (y, _) -> compare x y)

  (** [parse_capability_int64 features] is similar to [parse_string_int64_features features]
  but parses the input list into a [t list] *)
  let parse_capability_int64 features =
    parse_string_int64 features |> List.filter_map of_string_int64_opt
end

type sr_driver_info = {
    sr_driver_filename: string
  ; sr_driver_name: string
  ; sr_driver_description: string
  ; sr_driver_vendor: string
  ; sr_driver_copyright: string
  ; sr_driver_version: string
  ; sr_driver_required_api_version: string
  ; sr_driver_features: Feature.t list
  ; sr_driver_text_features: string list
  ; sr_driver_configuration: (string * string) list
  ; sr_driver_required_cluster_stack: string list
}

let query_result_of_sr_driver_info x =
  {
    Storage_interface.driver= x.sr_driver_filename
  ; name= x.sr_driver_name
  ; description= x.sr_driver_description
  ; vendor= x.sr_driver_vendor
  ; copyright= x.sr_driver_copyright
  ; version= x.sr_driver_version
  ; required_api_version= x.sr_driver_required_api_version
  ; features= x.sr_driver_text_features
  ; configuration= x.sr_driver_configuration
  ; required_cluster_stack= x.sr_driver_required_cluster_stack
  }

type attach_info = {
    params: string option
  ; params_nbd: string
  ; o_direct: bool
  ; o_direct_reason: string
  ; xenstore_data: (string * string) list
}

exception Backend_missing_field of string

exception Backend_report_error of (int * string)

exception Command_failed of (int * string * string (*stdout*) * string)

(*stderr*)

exception Command_killed of (int * string * string (*stdout*) * string)

(*stderr*)

exception Missing_field of string

exception Not_implemented_in_backend (* Raised by clone at least *)

exception Sr_not_empty

exception Vdi_in_use

exception Device_in_use

(** Identifies where a request should go to (i.e. a URI) or None if unknown *)
type request = string option
