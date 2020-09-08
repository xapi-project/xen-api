(*
 * Copyright (C) Citrix Systems Inc.
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

module D = Debug.Make (struct let name = "xapi" end)

open D
open Stdext
open Xstringext
open Listext
open Pervasiveext

let ( *** ) = Int64.mul

let ( /// ) = Int64.div

let ( +++ ) = Int64.add

let ( --- ) = Int64.sub

let mib x = List.fold_left Int64.mul x [1024L; 1024L]

exception Parse_error of exn

module Identifier = struct
  let version = 1

  type nvidia_id = {
      pdev_id: int
    ; psubdev_id: int option
    ; vdev_id: int
    ; vsubdev_id: int
    ; sriov: bool  (** true if SRIOV mode to be used *)
  }

  type gvt_g_id = {
      pdev_id: int
    ; low_gm_sz: int64
    ; high_gm_sz: int64
    ; fence_sz: int64
  }

  type mxgpu_id = {
      pdev_id: int
    ; (* Device id of the PCI PF, not the VFs *)
      framebufferbytes: int64
  }

  type t =
    | Passthrough
    | Nvidia of nvidia_id
    | GVT_g of gvt_g_id
    | MxGPU of mxgpu_id

  (* Create a unique string for each possible value of type t. This value
     	 * functions as a primary key for the VGPU_type object, so we can use it
     	 * to decide whether a relevant VGPU_type already exists in the database. *)
  let to_string id =
    let data =
      match id with
      | Passthrough ->
          "passthrough"
      | Nvidia nvidia_id ->
          Printf.sprintf "nvidia,%04x,%s,%04x,%04x" nvidia_id.pdev_id
            ( match nvidia_id.psubdev_id with
            | Some id ->
                Printf.sprintf "%04x" id
            | None ->
                ""
            )
            nvidia_id.vdev_id nvidia_id.vsubdev_id
      | GVT_g gvt_g_id ->
          Printf.sprintf "gvt-g,%04x,%Lx,%Lx,%Lx" gvt_g_id.pdev_id
            gvt_g_id.low_gm_sz gvt_g_id.high_gm_sz gvt_g_id.fence_sz
      | MxGPU mxgpu_id ->
          Printf.sprintf "mxgpu,%04x,%Lx" mxgpu_id.pdev_id
            mxgpu_id.framebufferbytes
    in
    Printf.sprintf "%04d:%s" version data

  let to_implementation : t -> API.vgpu_type_implementation = function
    | Passthrough ->
        `passthrough
    | Nvidia {sriov= false; _} ->
        `nvidia
    | Nvidia {sriov= true; _} ->
        `nvidia_sriov
    | GVT_g _ ->
        `gvt_g
    | MxGPU _ ->
        `mxgpu
end

type vgpu_type = {
    vendor_name: string
  ; model_name: string
  ; framebuffer_size: int64
  ; max_heads: int64
  ; max_resolution_x: int64
  ; max_resolution_y: int64
  ; size: int64
  ; internal_config: (string * string) list
  ; identifier: Identifier.t
  ; experimental: bool
  ; compatible_model_names_in_vm: string list
  ; compatible_model_names_on_pgpu: string list
}

let passthrough_gpu =
  {
    vendor_name= ""
  ; model_name= "passthrough"
  ; framebuffer_size= 0L
  ; max_heads= 0L
  ; max_resolution_x= 0L
  ; max_resolution_y= 0L
  ; size= 0L
  ; internal_config= []
  ; identifier= Identifier.Passthrough
  ; experimental= false
  ; compatible_model_names_in_vm= []
  ; compatible_model_names_on_pgpu= []
  }

let create ~__context ~vendor_name ~model_name ~framebuffer_size ~max_heads
    ~max_resolution_x ~max_resolution_y ~size ~internal_config ~implementation
    ~identifier ~experimental ~compatible_model_names_in_vm
    ~compatible_model_names_on_pgpu =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  (* Currently Nvidia has only one type of vGPU in the VM and on pGPU
   * We just check the compatilbe list, if it is not empty, then it just
   * compatible with self.
   * We need to convert the compatible model names to the comptible Refs
   * once Nvidia enable multiple Types, probably by two rounds to get
   * valid Ref *)
  let compatible_types_in_vm =
    if compatible_model_names_in_vm = [] then [] else [ref]
  in
  let compatible_types_on_pgpu =
    if compatible_model_names_on_pgpu = [] then [] else [ref]
  in
  Db.VGPU_type.create ~__context ~ref ~uuid ~vendor_name ~model_name
    ~framebuffer_size ~max_heads ~max_resolution_x ~max_resolution_y ~size
    ~internal_config ~implementation ~identifier ~experimental
    ~compatible_types_in_vm ~compatible_types_on_pgpu ;
  debug "VGPU_type ref='%s' created (vendor_name = '%s'; model_name = '%s')"
    (Ref.string_of ref) vendor_name model_name ;
  ref

let find_and_update ~__context vgpu_type =
  let identifier_string = Identifier.to_string vgpu_type.identifier in
  let fail () =
    failwith "Error: Multiple vGPU types exist with the same configuration."
  in
  let open Db_filter_types in
  let new_expr = Eq (Field "identifier", Literal identifier_string) in
  let old_expr =
    And
      ( Eq (Field "vendor_name", Literal vgpu_type.vendor_name)
      , Eq (Field "model_name", Literal vgpu_type.model_name) )
  in
  (* First try to look up by identifier. *)
  match Db.VGPU_type.get_internal_records_where ~__context ~expr:new_expr with
  | [(vgpu_type_ref, rc)] ->
      (* If looking up by identifier succeeds, update that VGPU_type's vendor_name
         		 * and model_name. *)
      if vgpu_type.vendor_name <> rc.Db_actions.vGPU_type_vendor_name then
        Db.VGPU_type.set_vendor_name ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.vendor_name ;
      if vgpu_type.model_name <> rc.Db_actions.vGPU_type_model_name then
        Db.VGPU_type.set_model_name ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.model_name ;
      let new_rc =
        Db_actions.
          {
            rc with
            vGPU_type_vendor_name= vgpu_type.vendor_name
          ; vGPU_type_model_name= vgpu_type.model_name
          }
      in
      Some (vgpu_type_ref, new_rc)
  | [] -> (
    (* If looking up by identifier fails, try to the old method (vendor name
       		 * and model name). If this finds a VGPU_type, update its identifier
       		 * field. *)
    match Db.VGPU_type.get_internal_records_where ~__context ~expr:old_expr with
    | [(vgpu_type_ref, rc)] ->
        Db.VGPU_type.set_identifier ~__context ~self:vgpu_type_ref
          ~value:identifier_string ;
        let new_rc =
          {rc with Db_actions.vGPU_type_identifier= identifier_string}
        in
        Some (vgpu_type_ref, new_rc)
    | [] ->
        None
    | _ ->
        fail ()
  )
  | _ ->
      fail ()

let find_or_create ~__context vgpu_type =
  let implementation = Identifier.to_implementation vgpu_type.identifier in
  match find_and_update ~__context vgpu_type with
  | Some (vgpu_type_ref, rc) ->
      (* Update anything about the VGPU type which might have changed since we
         		 * last read the config file. *)
      if vgpu_type.framebuffer_size <> rc.Db_actions.vGPU_type_framebuffer_size
      then
        Db.VGPU_type.set_framebuffer_size ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.framebuffer_size ;
      if vgpu_type.max_heads <> rc.Db_actions.vGPU_type_max_heads then
        Db.VGPU_type.set_max_heads ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.max_heads ;
      if vgpu_type.max_resolution_x <> rc.Db_actions.vGPU_type_max_resolution_x
      then
        Db.VGPU_type.set_max_resolution_x ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.max_resolution_x ;
      if vgpu_type.max_resolution_y <> rc.Db_actions.vGPU_type_max_resolution_x
      then
        Db.VGPU_type.set_max_resolution_y ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.max_resolution_y ;
      if vgpu_type.size <> rc.Db_actions.vGPU_type_size then
        Db.VGPU_type.set_size ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.size ;
      if vgpu_type.internal_config <> rc.Db_actions.vGPU_type_internal_config
      then
        Db.VGPU_type.set_internal_config ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.internal_config ;
      if implementation <> rc.Db_actions.vGPU_type_implementation then
        Db.VGPU_type.set_implementation ~__context ~self:vgpu_type_ref
          ~value:implementation ;
      if vgpu_type.experimental <> rc.Db_actions.vGPU_type_experimental then
        Db.VGPU_type.set_experimental ~__context ~self:vgpu_type_ref
          ~value:vgpu_type.experimental ;
      let compatible_types_in_vm =
        if vgpu_type.compatible_model_names_in_vm = [] then
          []
        else
          [vgpu_type_ref]
      in
      if
        compatible_types_in_vm <> rc.Db_actions.vGPU_type_compatible_types_in_vm
      then
        Db.VGPU_type.set_compatible_types_in_vm ~__context ~self:vgpu_type_ref
          ~value:compatible_types_in_vm ;
      let compatible_types_on_pgpu =
        if vgpu_type.compatible_model_names_on_pgpu = [] then
          []
        else
          [vgpu_type_ref]
      in
      if
        compatible_types_on_pgpu
        <> rc.Db_actions.vGPU_type_compatible_types_on_pgpu
      then
        Db.VGPU_type.set_compatible_types_on_pgpu ~__context ~self:vgpu_type_ref
          ~value:compatible_types_on_pgpu ;
      vgpu_type_ref
  | None ->
      create ~__context ~vendor_name:vgpu_type.vendor_name
        ~model_name:vgpu_type.model_name
        ~framebuffer_size:vgpu_type.framebuffer_size
        ~max_heads:vgpu_type.max_heads
        ~max_resolution_x:vgpu_type.max_resolution_x
        ~max_resolution_y:vgpu_type.max_resolution_y ~size:vgpu_type.size
        ~internal_config:vgpu_type.internal_config ~implementation
        ~identifier:(Identifier.to_string vgpu_type.identifier)
        ~experimental:vgpu_type.experimental
        ~compatible_model_names_in_vm:vgpu_type.compatible_model_names_in_vm
        ~compatible_model_names_on_pgpu:vgpu_type.compatible_model_names_on_pgpu

module Passthrough = struct
  let find_or_create_supported_types ~__context ~pci ~is_system_display_device
      ~is_host_display_enabled ~is_pci_hidden =
    if is_system_display_device then
      []
    else
      [find_or_create ~__context passthrough_gpu]
end

module type VENDOR = sig
  type vgpu_conf

  val vendor_id : int

  val pt_when_vgpu : bool

  val whitelist_file : unit -> string

  val read_whitelist : whitelist:string -> device_id:int -> vgpu_conf list

  val vgpu_type_of_conf :
    Pci.Pci_access.t -> string -> Pci.Pci_dev.t -> vgpu_conf -> vgpu_type option
end

module Vendor =
functor
  (V : VENDOR)
  ->
  struct
    let vendor_id = V.vendor_id

    let make_vgpu_types ~__context ~pci =
      let open Xenops_interface.Pci in
      let device_id =
        Db.PCI.get_device_id ~__context ~self:pci |> Xapi_pci.int_of_id
      in
      let address =
        Db.PCI.get_pci_id ~__context ~self:pci |> address_of_string
      in
      let whitelist =
        V.read_whitelist ~whitelist:(V.whitelist_file ()) ~device_id
      in
      let default ~msg v =
        match v with
        | Some v ->
            v
        | None ->
            debug "make_vgpu_types empty %s" msg ;
            ""
      in
      Pci.(
        with_access (fun access ->
            let vendor_name, device =
              let vendor_name =
                lookup_vendor_name access V.vendor_id
                |> default ~msg:(Printf.sprintf "vendor %04x name" V.vendor_id)
              in
              let device =
                List.find
                  (fun device ->
                    device.Pci_dev.domain = address.domain
                    && device.Pci_dev.bus = address.bus
                    && device.Pci_dev.dev = address.dev
                    && device.Pci_dev.func = address.fn)
                  (get_devices access)
              in
              (vendor_name, device)
            in
            List.filter_map
              (V.vgpu_type_of_conf access vendor_name device)
              whitelist))

    let find_or_create_supported_types ~__context ~pci ~is_system_display_device
        ~is_host_display_enabled ~is_pci_hidden =
      let vgpu_types = make_vgpu_types ~__context ~pci in
      let passthrough_types =
        if
          is_system_display_device
          && (is_host_display_enabled || not is_pci_hidden)
        then
          []
        else
          [passthrough_gpu]
      in
      let types =
        match (V.pt_when_vgpu, passthrough_types, vgpu_types) with
        | false, passthrough_types, [] ->
            passthrough_types
        | false, _, vgpu_types ->
            vgpu_types
        | true, passthrough_types, vgpu_types ->
            passthrough_types @ vgpu_types
      in
      List.map (find_or_create ~__context) types
  end

let read_whitelist_line_by_line ~whitelist ~device_id ~parse_line
    ~device_id_of_conf =
  if Sys.file_exists whitelist then
    Stdext.Unixext.file_lines_fold
      Identifier.(
        fun acc line ->
          match parse_line ~line with
          | Some conf when device_id_of_conf conf = device_id ->
              conf :: acc
          | _ ->
              acc)
      [] whitelist
  else
    []

module Vendor_nvidia = struct
  type vgpu_conf = {
      identifier: Identifier.nvidia_id
    ; framebufferlength: int64
    ; num_heads: int64
    ; max_instance: int64
    ; max_x: int64
    ; max_y: int64
    ; file_path: string
    ; type_id: string
    ; compatible_model_names_in_vm: string list
    ; compatible_model_names_on_pgpu: string list
  }

  let vendor_id = 0x10de

  let pt_when_vgpu = true

  let whitelist_file () = !Xapi_globs.nvidia_whitelist

  let device_id_of_conf conf = conf.identifier.Identifier.pdev_id

  let get_host_driver_version () =
    try
      Scanf.sscanf (Unix.readlink !Xapi_globs.nvidia_host_driver_file)
        "libnvidia-vgpu.so.%s" (fun x -> x)
    with _ ->
      info "Can not get the file version of %s, use the default value"
        !Xapi_globs.nvidia_host_driver_file ;
      Xapi_globs.nvidia_default_host_driver_version

  (** Turn an Nvidia version string into a list on numbers *)
  let nvidia_version version =
    try
      Astring.String.filter (( <> ) '+') version
      |> Astring.String.cuts ~empty:false ~sep:"."
      |> List.map int_of_string
    with _ ->
      error "Not a proper NVidia host driver version: '%s'" version ;
      failwith (Printf.sprintf "%s is not a version string" version)

  (** [compare_versions] compares two host driver
  versions represented as int list of possibly different length. The
  shorter list is conceptually extended with zero entries *)
  let compare_version v1 v2 =
    let rec compare v1 v2 =
      match (v1, v2) with
      | [], [] ->
          0
      | [], 0 :: ys ->
          compare v1 ys
      | [], _ ->
          -1
      | 0 :: xs, [] ->
          compare xs v2
      | _, [] ->
          1
      | x :: xs, y :: ys when x < y ->
          -1
      | x :: xs, y :: ys when x > y ->
          1
      | _ :: xs, _ :: ys ->
          compare xs ys
    in
    compare v1 v2

  let host_driver_supports_multi_vgpu ~host_driver_version ~supported_versions =
    try
      let driver = nvidia_version host_driver_version in
      let supported = List.map nvidia_version supported_versions in
      let latest =
        Stdlib.List.find_opt (fun v -> String.contains v '+') supported_versions
      in
      let is_equal v1 v2 = compare_version v1 v2 = 0 in
      let gt_latest v1 =
        match latest with
        | None ->
            false
        | Some v2 ->
            compare_version v1 (nvidia_version v2) > 0
      in
      List.exists (is_equal driver) supported || gt_latest driver
    with _ -> false

  (* A type to capture the XML tree with functions for use in Xmlm.input_doc_tree *)
  type tree = E of string * (string * string) list * tree list | D of string

  let el ((_, name), attr) children =
    E (name, List.map (fun ((_, k), v) -> (k, v)) attr, children)

  let data d = D d

  (* Helper functions for searching through the XML tree *)
  let rec find_by_name name = function
    | E (n, _, _) as t when n = name ->
        [t]
    | E (_, _, ch) ->
        List.map (find_by_name name) ch |> List.concat
    | D _ ->
        []

  let find_one_by_name name t =
    match find_by_name name t with
    | x :: _ ->
        x
    | [] ->
        failwith (Printf.sprintf "find_one_by_name(%s): not found" name)

  let get_attr name = function
    | E (_, attr, _) ->
        List.assoc name attr
    | D _ ->
        failwith (Printf.sprintf "get_attr(%s): not an element" name)

  let get_data = function
    | E (_, _, [D d]) ->
        d
    | _ ->
        failwith "get_data: no data node"

  type vgpu_type = {max: int64; psubdev_id: int option; sriov: bool}

  let find_supported_vgpu_types device_id pgpus =
    (*
      Input example:
        <pgpu hostVgpuMode="sriov">
          <devId vendorId="0x10de" deviceId="0x1BB4" subsystemVendorId="0x10de" subsystemId="0x0"></devId>
          <supportedVgpu vgpuId="72">
            <maxVgpus>16</maxVgpus>
            <digest type="signature">(...)</digest>
          </supportedVgpu>
          <supportedVgpu vgpuId="73">
            (...)
      Output: 
      - Find the pgpus with deviceId = device_id (there's likely just one).
      - List the vgpuIds of all supportedVgpus of these pgpus with their
        maxVgpus value and the pgpu's subsystemId.
    *)
    List.filter_map
      (fun pgpu ->
        let devid = find_one_by_name "devId" pgpu in
        if int_of_string (get_attr "deviceId" devid) = device_id then
          let psubdev_id =
            let id = int_of_string (get_attr "subsystemId" devid) in
            if id = 0 then None else Some id
          in
          let vgpus = find_by_name "supportedVgpu" pgpu in
          let sriov =
            match get_attr "hostVgpuMode" pgpu with
            | "sriov" ->
                true
            | other ->
                warn "NVidia VGPU hostVgpuMode='%s' unrecognized" other ;
                false
            | exception Not_found ->
                false
            | exception e ->
                warn "find_supported_vgpu_types %s (%s)" (Printexc.to_string e)
                  __LOC__ ;
                false
          in
          Some
            (List.map
               (fun vgpu ->
                 let id = get_attr "vgpuId" vgpu in
                 let max =
                   Int64.of_string (get_data (find_one_by_name "maxVgpus" vgpu))
                 in
                 (id, {max; psubdev_id; sriov}))
               vgpus)
        else
          None)
      pgpus
    |> List.concat

  let extract_conf whitelist device_id vgpu_types vgpu_ids =
    (*
      Input example:
        <vgpuType id="11" name="GRID M60-0B" class="NVS">
          <devId vendorId="0x10de" deviceId="0x13F2" subsystemVendorId="0x10de" subsystemId="0x1176"></devId>
          <framebuffer>0x1A000000</framebuffer>
          <numHeads>2</numHeads>
          <display width="2560" height="1600"></display>
          (...)
        </vgpuType>
      Output:
      - Find the vgpuType elements that match the vgpu type ids found above,
        and construct vgpu_conf records.
    *)
    let host_driver_version = get_host_driver_version () in
    let driver_supports_multiple_vgpus =
      host_driver_supports_multi_vgpu ~host_driver_version
        ~supported_versions:
          !Xapi_globs.nvidia_multi_vgpu_enabled_driver_versions
    in
    List.filter_map
      (fun vgpu_type ->
        let is_sriov id =
          try
            (* IDs of T4 cards *)
            List.mem (int_of_string id)
              [
                230
              ; 231
              ; 232
              ; 233
              ; 234
              ; 225
              ; 226
              ; 227
              ; 228
              ; 229
              ; 222
              ; 252
              ; 223
              ; 224
              ]
          with Failure _ -> false
        in
        let id = get_attr "id" vgpu_type in
        if List.mem_assoc id vgpu_ids then (
          let {max= max_instance; psubdev_id; sriov} = List.assoc id vgpu_ids in
          let framebufferlength =
            Int64.of_string
              (get_data (find_one_by_name "framebuffer" vgpu_type))
          in
          let num_heads =
            Int64.of_string (get_data (find_one_by_name "numHeads" vgpu_type))
          in
          let max_x, max_y =
            let display = find_one_by_name "display" vgpu_type in
            ( Int64.of_string (get_attr "width" display)
            , Int64.of_string (get_attr "height" display) )
          in
          let devid = find_one_by_name "devId" vgpu_type in
          let identifier =
            Identifier.
              {
                pdev_id= device_id
              ; psubdev_id
              ; vdev_id= int_of_string (get_attr "deviceId" devid)
              ; vsubdev_id= int_of_string (get_attr "subsystemId" devid)
              ; sriov=
                  ( match !Xapi_globs.nvidia_t4_sriov with
                  | Xapi_globs.Nvidia_DEFAULT ->
                      sriov (* from XML *)
                  | Xapi_globs.Nvidia_T4_SRIOV ->
                      is_sriov id (* true if T4 card *)
                  | Xapi_globs.Nvidia_LEGACY ->
                      false
                  )
                  (* don't use SRIOV *)
              }
          in
          let file_path = whitelist in
          let type_id = id in
          (* Multiple vgpu support:
             - Read  'multiVgpuSupported' from config, 1L indicates this type support multiple
             - Currently always initialize 'compatible_types_on_pgpu' to itself only since we
               don't support yet *)
          let multi_vgpu_support =
            Int64.of_string
              (get_data (find_one_by_name "multiVgpuSupported" vgpu_type))
          in
          let name = get_attr "name" vgpu_type in
          info
            "Getting multiple vGPU supported from config file: %Ld, model: %s"
            multi_vgpu_support name ;
          info "NVIDIA GPU name=%s id=%s sriov=%b" name id sriov ;
          let compatible_model_names_in_vm, compatible_model_names_on_pgpu =
            match (multi_vgpu_support, driver_supports_multiple_vgpus) with
            | 1L, true ->
                ([name], [name])
            | _ ->
                ([], [name])
          in
          Some
            {
              identifier
            ; framebufferlength
            ; num_heads
            ; max_instance
            ; max_x
            ; max_y
            ; file_path
            ; type_id
            ; compatible_model_names_in_vm
            ; compatible_model_names_on_pgpu
            }
        ) else
          None)
      vgpu_types

  let read_whitelist ~whitelist ~device_id =
    try
      let ch = open_in whitelist in
      let t =
        finally
          (fun () ->
            let i = Xmlm.make_input ~strip:true (`Channel ch) in
            let _, t = Xmlm.input_doc_tree ~el ~data i in
            t)
          (fun () -> close_in ch)
      in
      let pgpus = find_by_name "pgpu" t in
      let vgpu_types = find_by_name "vgpuType" t in
      find_supported_vgpu_types device_id pgpus
      |> extract_conf whitelist device_id vgpu_types
    with e ->
      error "Ignoring error parsing %s: %s\n%s\n" whitelist
        (Printexc.to_string e)
        (Printexc.get_backtrace ()) ;
      []

  let vgpu_type_of_conf pci_access vendor_name _ conf =
    let open Identifier in
    debug
      "Pci.lookup_subsystem_device_name: vendor=%04x device=%04x subdev=%04x"
      vendor_id conf.identifier.vdev_id conf.identifier.vsubdev_id ;
    let default v =
      match v with
      | Some v ->
          v
      | None ->
          debug "Pci.lookup_subsystem_device_name: empty string" ;
          ""
    in
    let model_name =
      Pci.lookup_subsystem_device_name pci_access vendor_id
        conf.identifier.vdev_id vendor_id conf.identifier.vsubdev_id
      |> default
    in
    Some
      {
        vendor_name
      ; model_name
      ; framebuffer_size= conf.framebufferlength
      ; max_heads= conf.num_heads
      ; max_resolution_x= conf.max_x
      ; max_resolution_y= conf.max_y
      ; size= Int64.div Constants.pgpu_default_size conf.max_instance
      ; internal_config= [(Xapi_globs.vgpu_type_id, conf.type_id)]
      ; identifier= Nvidia conf.identifier
      ; experimental= false
      ; compatible_model_names_in_vm= conf.compatible_model_names_in_vm
      ; compatible_model_names_on_pgpu= conf.compatible_model_names_on_pgpu
      }
end

module Vendor_intel = struct
  type vgpu_conf = {
      identifier: Identifier.gvt_g_id
    ; experimental: bool
    ; model_name: string
    ; framebufferlength: int64
    ; (* in MiB *)
      num_heads: int64
    ; max_x: int64
    ; max_y: int64
  }

  let vendor_id = 0x8086

  let pt_when_vgpu = true

  let whitelist_file () = !Xapi_globs.gvt_g_whitelist

  let device_id_of_conf conf = conf.identifier.Identifier.pdev_id

  let read_whitelist_line ~line =
    try
      Some
        (Scanf.sscanf line
           "%04x experimental=%c name='%s@' low_gm_sz=%Ld high_gm_sz=%Ld \
            fence_sz=%Ld framebuffer_sz=%Ld max_heads=%Ld resolution=%Ldx%Ld"
           (fun pdev_id
                experimental
                model_name
                low_gm_sz
                high_gm_sz
                fence_sz
                framebuffer_sz
                num_heads
                max_x
                max_y
                ->
             {
               identifier= Identifier.{pdev_id; low_gm_sz; high_gm_sz; fence_sz}
             ; experimental= experimental <> '0'
             ; model_name
             ; framebufferlength= mib framebuffer_sz
             ; num_heads
             ; max_x
             ; max_y
             }))
    with e ->
      error "Failed to read whitelist line: '%s' %s" line (Printexc.to_string e) ;
      None

  let read_whitelist ~whitelist ~device_id =
    read_whitelist_line_by_line ~whitelist ~device_id
      ~parse_line:read_whitelist_line ~device_id_of_conf

  let vgpu_type_of_conf _ vendor_name device conf =
    let open Identifier in
    let bar_size = List.nth device.Pci.Pci_dev.size 2 |> Int64.of_nativeint in
    let vgpus_per_pgpu =
      (bar_size /// 1024L /// 1024L /// conf.identifier.low_gm_sz) --- 1L
    in
    if vgpus_per_pgpu <= 0L then (
      warn
        "Not enough memory for Intel VGPUs. If you intend to use them, \
         increase the GPU BAR size in the BIOS settings." ;
      None
    ) else
      let vgpu_size = Constants.pgpu_default_size /// vgpus_per_pgpu in
      let internal_config =
        let open Xapi_globs in
        List.concat
          [
            [
              (vgt_low_gm_sz, Int64.to_string conf.identifier.low_gm_sz)
            ; (vgt_high_gm_sz, Int64.to_string conf.identifier.high_gm_sz)
            ; (vgt_fence_sz, Int64.to_string conf.identifier.fence_sz)
            ]
          ]
      in
      Some
        {
          vendor_name
        ; model_name= conf.model_name
        ; framebuffer_size= conf.framebufferlength
        ; max_heads= conf.num_heads
        ; max_resolution_x= conf.max_x
        ; max_resolution_y= conf.max_y
        ; size= vgpu_size
        ; internal_config
        ; identifier= GVT_g conf.identifier
        ; experimental= conf.experimental
        ; compatible_model_names_in_vm= []
        ; compatible_model_names_on_pgpu= [conf.model_name]
        }
end

module Vendor_amd = struct
  type vgpu_conf = {
      (* The identifier has a field for the framebuffer size. *)
      identifier: Identifier.mxgpu_id
    ; experimental: bool
    ; model_name: string
    ; vgpus_per_pgpu: int64
  }

  let vendor_id = 0x1002

  let pt_when_vgpu = false

  let whitelist_file () = !Xapi_globs.mxgpu_whitelist

  let device_id_of_conf conf = conf.identifier.Identifier.pdev_id

  let read_whitelist_line ~line =
    try
      Some
        (Scanf.sscanf line
           "%04x experimental=%c name='%s@' framebuffer_sz=%Ld \
            vgpus_per_pgpu=%Ld"
           (fun pdev_id (* e.g. "FirePro S7150" has 6929 (PF), 692f (VF) *)
                experimental
                model_name (* e.g. PF "FirePro S7150" or VF "FirePro S7150V" *)
                framebuffer_sz
                vgpus_per_pgpu
                ->
             {
               identifier=
                 Identifier.{pdev_id; framebufferbytes= mib framebuffer_sz}
             ; experimental= experimental <> '0'
             ; model_name
             ; vgpus_per_pgpu
             }))
    with e ->
      error "Failed to read whitelist line: '%s' %s" line (Printexc.to_string e) ;
      None

  let read_whitelist ~whitelist ~device_id =
    read_whitelist_line_by_line ~whitelist ~device_id
      ~parse_line:read_whitelist_line ~device_id_of_conf

  let vgpu_type_of_conf _ vendor_name _ conf =
    let open Identifier in
    let size_of_pgpu =
      (* counterpart of bar_size in gvt-g *)
      Constants.pgpu_default_size
    in
    if conf.vgpus_per_pgpu <= 0L then (
      warn "Ignoring a config line that specifies 0 vgpus per pgpu." ;
      None
    ) else
      let vgpu_size = size_of_pgpu /// conf.vgpus_per_pgpu in
      let internal_config =
        let open Xapi_globs in
        [(mxgpu_vgpus_per_pgpu, Int64.to_string conf.vgpus_per_pgpu)]
      in
      Some
        {
          vendor_name
        ; model_name= conf.model_name
        ; framebuffer_size= conf.identifier.framebufferbytes
        ; max_heads= 0L
        ; max_resolution_x= 0L
        ; max_resolution_y= 0L
        ; size= vgpu_size
        ; internal_config
        ; identifier= MxGPU conf.identifier
        ; experimental= conf.experimental
        ; compatible_model_names_in_vm= []
        ; compatible_model_names_on_pgpu= [conf.model_name]
        }
end

module Nvidia = Vendor (Vendor_nvidia)
module Intel = Vendor (Vendor_intel)
module AMD = Vendor (Vendor_amd)

module Nvidia_compat = struct
  let conf_dir = Xapi_globs.nvidia_compat_conf_dir

  let of_conf_file file_path =
    try
      let conf = Stdext.Unixext.read_lines file_path in
      let args =
        List.filter (fun s -> not (String.startswith "#" s || s = "")) conf
      in
      let args = List.map (String.strip String.isspace) args in
      (* Expecting space separated key value entries *)
      let args =
        List.map
          (fun s ->
            match String.split ' ' s ~limit:2 with
            | [k; v] ->
                (k, v)
            | _ ->
                ("", ""))
          args
      in
      (* plugin0.pdev_id will either be just the physical device id, or of the
       * form "device_id:subdevice_id" *)
      let pdev_id, psubdev_id =
        let pdev_id_data = List.assoc "plugin0.pdev_id" args in
        try
          Scanf.sscanf pdev_id_data {|"0x%x:0x%x"|} (fun pdev_id psubdev_id ->
              (pdev_id, Some psubdev_id))
        with Scanf.Scan_failure _ ->
          Scanf.sscanf pdev_id_data {|"0x%x"|} (fun pdev_id -> (pdev_id, None))
      in
      (* NVIDIA key is "device_id:subdevice_id", N.B. not subvendor id *)
      (* Since old config file doesn't include multiple or compatible list, define empty list here *)
      Scanf.sscanf (List.assoc "plugin0.vdev_id" args) {|"0x%x:0x%x"|}
        (fun vdev_id vsubdev_id ->
          Identifier.(
            Nvidia {pdev_id; psubdev_id; vdev_id; vsubdev_id; sriov= false}))
    with e -> raise (Parse_error e)

  let read_config_dir conf_dir =
    let rec read_configs ac = function
      | [] ->
          ac
      | conf_file :: tl -> (
        try read_configs ((conf_file, of_conf_file conf_file) :: ac) tl
        with Parse_error e ->
          error "Ignoring error parsing %s: %s\n%s\n" conf_file
            (Printexc.to_string e)
            (Printexc.get_backtrace ()) ;
          read_configs ac tl
      )
    in
    let conf_files = Array.to_list (Sys.readdir conf_dir) in
    debug "Reading NVIDIA vGPU config files %s/{%s}" conf_dir
      (String.concat ", " conf_files) ;
    read_configs []
      (List.map (fun conf -> Filename.concat conf_dir conf) conf_files)

  let create_compat_config_file __context =
    try
      let open Db_filter_types in
      let host_driver_version = Vendor_nvidia.get_host_driver_version () in
      let host_driver_supports_multiple =
        Vendor_nvidia.host_driver_supports_multi_vgpu ~host_driver_version
          ~supported_versions:
            !Xapi_globs.nvidia_multi_vgpu_enabled_driver_versions
      in
      if not host_driver_supports_multiple then
        read_config_dir conf_dir
        (* Nvidia host driver does not support multiple vGPU, create compat config file *)
        |> List.iter (fun (conf_file, identifier) ->
               let identifier_string = Identifier.to_string identifier in
               let expr = Eq (Field "identifier", Literal identifier_string) in
               match
                 Db.VGPU_type.get_internal_records_where ~__context ~expr
               with
               | [(vgpu_type_ref, rc)] ->
                   let updated_config =
                     rc.Db_actions.vGPU_type_internal_config
                     |> List.remove_assoc
                          Xapi_globs.nvidia_compat_config_file_key
                     |> fun x ->
                     (Xapi_globs.nvidia_compat_config_file_key, conf_file) :: x
                   in
                   Db.VGPU_type.set_internal_config ~__context
                     ~self:vgpu_type_ref ~value:updated_config
               | _ ->
                   ()
               (* Type is not relevant: ignore *))
    with e ->
      error "Failed to create NVidia compat config_file: %s\n%s\n"
        (Printexc.to_string e)
        (Printexc.get_backtrace ())
end

let find_or_create_supported_types ~__context ~pci =
  let vendor_id =
    Db.PCI.get_vendor_id ~__context ~self:pci |> Xapi_pci.int_of_id
  in
  let find_or_create_supported_types =
    match vendor_id with
    | x when x = Nvidia.vendor_id ->
        Nvidia.find_or_create_supported_types
    | x when x = Intel.vendor_id ->
        Intel.find_or_create_supported_types
    | x when x = AMD.vendor_id ->
        AMD.find_or_create_supported_types
    | _ ->
        Passthrough.find_or_create_supported_types
  in
  find_or_create_supported_types ~__context ~pci

let requires_passthrough ~__context ~self =
  match Db.VGPU_type.get_implementation ~__context ~self with
  | `passthrough ->
      Some `PF (* Requires passthough of a physical function (entire device) *)
  | `mxgpu ->
      Some `VF (* Requires passthough of a virtual function (SR-IOV) *)
  | `nvidia_sriov ->
      Some `VF (* Requires passthough of a virtual function (SR-IOV) *)
  | `nvidia | `gvt_g ->
      None

(* Does not require any passthrough *)
