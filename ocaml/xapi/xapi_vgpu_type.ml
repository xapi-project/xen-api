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

module D = Debug.Make(struct let name = "xapi" end)
open D

open Stdext
open Xstringext
open Listext

let ( *** ) = Int64.mul
let ( /// ) = Int64.div
let ( +++ ) = Int64.add
let ( --- ) = Int64.sub
let mib x = List.fold_left Int64.mul x [1024L; 1024L]

exception Parse_error of exn

module Identifier = struct
  let version = 1

  type nvidia_id = {
    pdev_id : int;
    psubdev_id : int option;
    vdev_id : int;
    vsubdev_id : int;
  }

  type gvt_g_id = {
    pdev_id : int;
    low_gm_sz : int64;
    high_gm_sz : int64;
    fence_sz : int64;
    monitor_config_file : string option;
  }

  type mxgpu_id = {
    pdev_id : int; (* Device id of the PCI PF, not the VFs *)
    sched: int;
    framebufferbytes: int64;
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
      | Passthrough -> "passthrough"
      | Nvidia nvidia_id ->
        Printf.sprintf "nvidia,%04x,%s,%04x,%04x"
          nvidia_id.pdev_id
          (match nvidia_id.psubdev_id with
           | Some id -> Printf.sprintf "%04x" id
           | None -> "")
          nvidia_id.vdev_id
          nvidia_id.vsubdev_id
      | GVT_g gvt_g_id ->
        Printf.sprintf "gvt-g,%04x,%Lx,%Lx,%Lx,%s"
          gvt_g_id.pdev_id
          gvt_g_id.low_gm_sz
          gvt_g_id.high_gm_sz
          gvt_g_id.fence_sz
          (match gvt_g_id.monitor_config_file with
           | Some path -> path
           | None -> "")
      | MxGPU mxgpu_id ->
        Printf.sprintf "mxgpu,%04x,%x,%Lx"
          mxgpu_id.pdev_id
          mxgpu_id.sched
          mxgpu_id.framebufferbytes
    in
    Printf.sprintf "%04d:%s" version data

  let to_implementation : (t -> API.vgpu_type_implementation) = function
    | Passthrough -> `passthrough
    | Nvidia _ -> `nvidia
    | GVT_g _ -> `gvt_g
    | MxGPU _ -> `mxgpu
end

type vgpu_type = {
  vendor_name : string;
  model_name : string;
  framebuffer_size : int64;
  max_heads : int64;
  max_resolution_x : int64;
  max_resolution_y : int64;
  size : int64;
  internal_config : (string * string) list;
  identifier : Identifier.t;
  experimental : bool;
}

let passthrough_gpu = {
  vendor_name = "";
  model_name = "passthrough";
  framebuffer_size = 0L;
  max_heads = 0L;
  max_resolution_x = 0L;
  max_resolution_y = 0L;
  size = 0L;
  internal_config = [];
  identifier = Identifier.Passthrough;
  experimental = false;
}

let create ~__context ~vendor_name ~model_name ~framebuffer_size ~max_heads
    ~max_resolution_x ~max_resolution_y ~size ~internal_config ~implementation
    ~identifier ~experimental =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.VGPU_type.create ~__context ~ref ~uuid ~vendor_name ~model_name
    ~framebuffer_size ~max_heads ~max_resolution_x ~max_resolution_y
    ~size ~internal_config ~implementation ~identifier ~experimental;
  debug "VGPU_type ref='%s' created (vendor_name = '%s'; model_name = '%s')"
    (Ref.string_of ref) vendor_name model_name;
  ref

let find_and_update ~__context vgpu_type =
  let identifier_string = Identifier.to_string vgpu_type.identifier in
  let fail () =
    failwith "Error: Multiple vGPU types exist with the same configuration." in
  let open Db_filter_types in
  let new_expr = Eq (Field "identifier", Literal identifier_string) in
  let old_expr = And
      ((Eq (Field "vendor_name", Literal vgpu_type.vendor_name),
        (Eq (Field "model_name", Literal vgpu_type.model_name))))
  in
  (* First try to look up by identifier. *)
  match Db.VGPU_type.get_internal_records_where ~__context ~expr:new_expr with
  | [vgpu_type_ref, rc] -> begin
      (* If looking up by identifier succeeds, update that VGPU_type's vendor_name
         		 * and model_name. *)
      if vgpu_type.vendor_name <> rc.Db_actions.vGPU_type_vendor_name then
        Db.VGPU_type.set_vendor_name ~__context
          ~self:vgpu_type_ref
          ~value:vgpu_type.vendor_name;
      if vgpu_type.model_name <> rc.Db_actions.vGPU_type_model_name then
        Db.VGPU_type.set_model_name ~__context
          ~self:vgpu_type_ref
          ~value:vgpu_type.model_name;
      let new_rc = Db_actions.({rc with
                                vGPU_type_vendor_name = vgpu_type.vendor_name;
                                vGPU_type_model_name = vgpu_type.model_name;
                               }) in
      Some (vgpu_type_ref, new_rc)
    end
  | [] -> begin
      (* If looking up by identifier fails, try to the old method (vendor name
         		 * and model name). If this finds a VGPU_type, update its identifier
         		 * field. *)
      match Db.VGPU_type.get_internal_records_where ~__context ~expr:old_expr with
      | [vgpu_type_ref, rc] -> begin
          Db.VGPU_type.set_identifier ~__context
            ~self:vgpu_type_ref
            ~value:identifier_string;
          let new_rc = {rc with
                        Db_actions.vGPU_type_identifier = identifier_string;
                       } in
          Some (vgpu_type_ref, new_rc)
        end
      | [] -> None
      | _ -> fail ()
    end
  | _ -> fail ()

let find_or_create ~__context vgpu_type =
  let implementation = Identifier.to_implementation vgpu_type.identifier in
  match (find_and_update ~__context vgpu_type) with
  | Some (vgpu_type_ref, rc) ->
    (* Update anything about the VGPU type which might have changed since we
       		 * last read the config file. *)
    if vgpu_type.framebuffer_size <> rc.Db_actions.vGPU_type_framebuffer_size then
      Db.VGPU_type.set_framebuffer_size ~__context
        ~self:vgpu_type_ref
        ~value:vgpu_type.framebuffer_size;
    if vgpu_type.max_heads <> rc.Db_actions.vGPU_type_max_heads then
      Db.VGPU_type.set_max_heads ~__context
        ~self:vgpu_type_ref
        ~value:vgpu_type.max_heads;
    if vgpu_type.max_resolution_x <> rc.Db_actions.vGPU_type_max_resolution_x then
      Db.VGPU_type.set_max_resolution_x ~__context
        ~self:vgpu_type_ref
        ~value:vgpu_type.max_resolution_x;
    if vgpu_type.max_resolution_y <> rc.Db_actions.vGPU_type_max_resolution_x then
      Db.VGPU_type.set_max_resolution_y ~__context
        ~self:vgpu_type_ref
        ~value:vgpu_type.max_resolution_y;
    if vgpu_type.size <> rc.Db_actions.vGPU_type_size then
      Db.VGPU_type.set_size ~__context
        ~self:vgpu_type_ref
        ~value:vgpu_type.size;
    if vgpu_type.internal_config <> rc.Db_actions.vGPU_type_internal_config then
      Db.VGPU_type.set_internal_config ~__context
        ~self:vgpu_type_ref
        ~value:vgpu_type.internal_config;
    if implementation <> rc.Db_actions.vGPU_type_implementation then
      Db.VGPU_type.set_implementation ~__context
        ~self:vgpu_type_ref
        ~value:implementation;
    if vgpu_type.experimental <> rc.Db_actions.vGPU_type_experimental then
      Db.VGPU_type.set_experimental ~__context
        ~self:vgpu_type_ref
        ~value:vgpu_type.experimental;
    vgpu_type_ref
  | None ->
    create ~__context ~vendor_name:vgpu_type.vendor_name
      ~model_name:vgpu_type.model_name
      ~framebuffer_size:vgpu_type.framebuffer_size
      ~max_heads:vgpu_type.max_heads
      ~max_resolution_x:vgpu_type.max_resolution_x
      ~max_resolution_y:vgpu_type.max_resolution_y
      ~size:vgpu_type.size
      ~internal_config:vgpu_type.internal_config
      ~implementation
      ~identifier:(Identifier.to_string vgpu_type.identifier)
      ~experimental:vgpu_type.experimental

module Nvidia = struct
  let nvidia_conf_dir = "/usr/share/nvidia/vgx"
  let nvidia_vendor_id = 0x10de

  type vgpu_conf = {
    identifier : Identifier.nvidia_id;
    framebufferlength : int64;
    num_heads : int64;
    max_instance : int64;
    max_x : int64;
    max_y : int64;
    file_path : string;
  }

  let of_conf_file file_path =
    try
      let conf = Stdext.Unixext.read_lines file_path in
      let args = List.filter
          (fun s -> not (String.startswith "#" s || s = "")) conf in
      let args = List.map (String.strip String.isspace) args in
      (* Expecting space separated key value entries *)
      let args = List.map
          (fun s ->
             match (String.split ' ' s ~limit:2) with
             | k :: [v] -> (k, v)
             | _ -> ("", "")
          ) args in
      (* plugin0.pdev_id will either be just the physical device id, or of the
         			 * form "device_id:subdevice_id" *)
      let pdev_id, psubdev_id =
        let pdev_id_data = (List.assoc "plugin0.pdev_id" args) in
        try
          Scanf.sscanf pdev_id_data "\"0x%x:0x%x\""
            (fun pdev_id psubdev_id -> pdev_id, Some psubdev_id)
        with Scanf.Scan_failure _ ->
          Scanf.sscanf pdev_id_data "\"0x%x\""
            (fun pdev_id -> pdev_id, None)
      in
      (* NVIDIA key is "device_id:subdevice_id", N.B. not subvendor id *)
      Scanf.sscanf (List.assoc "plugin0.vdev_id" args) "\"0x%x:0x%x\"" (fun vdev_id vsubdev_id ->
          Scanf.sscanf (List.assoc "plugin0.max_resolution" args) "%Ldx%Ld" (fun max_x max_y ->
              let framebufferlength = Int64.of_string
                  (List.assoc "plugin0.framebufferlength" args) in
              let num_heads = Int64.of_string
                  (List.assoc "plugin0.num_heads" args) in
              let max_instance = Int64.of_string
                  (List.assoc "plugin0.max_instance" args) in
              let identifier = Identifier.({
                  pdev_id;
                  psubdev_id;
                  vdev_id;
                  vsubdev_id;
                }) in
              {identifier; framebufferlength;
               num_heads; max_instance; max_x; max_y; file_path}
            )
        )
    with e ->
      raise (Parse_error e)

  let read_config_dir conf_dir =
    let rec read_configs ac = function
      | [] -> ac
      | conf_file::tl ->
        try
          read_configs (of_conf_file conf_file :: ac) tl
        with Parse_error e ->
          error "Ignoring error parsing %s: %s\n%s\n" conf_file
            (Printexc.to_string e) (Printexc.get_backtrace ());
          read_configs ac tl
    in
    let conf_files = Array.to_list (Sys.readdir conf_dir) in
    debug "Reading NVIDIA vGPU config files %s/{%s}"
      conf_dir (String.concat ", " conf_files);
    read_configs []
      (List.map (fun conf -> String.concat "/" [conf_dir; conf]) conf_files)

  let relevant_vgpu_types pci_dev_id subsystem_device_id =
    let open Identifier in
    let vgpu_confs = try read_config_dir nvidia_conf_dir with _ -> [] in
    let relevant_vgpu_confs =
      List.filter
        (fun c ->
           let device_id_matches = (c.identifier.pdev_id = pci_dev_id) in
           let subsystem_device_id_matches =
             (* If the config file doesn't specify a physical subdevice ID, then
                						 * the config file is valid for this device no matter the device's
                						 * subsystem device ID.
                						 *
                						 * If the config file does specify a physical subdevice ID, then the
                						 * corresponding ID of the card must match. *)
             match subsystem_device_id, c.identifier.psubdev_id with
             | _, None -> true
             | None, Some _ -> false
             | Some device_id, Some conf_id -> device_id = conf_id
           in
           device_id_matches && subsystem_device_id_matches)
        vgpu_confs
    in
    debug "Relevant confs = [ %s ]"
      (String.concat "; " (List.map (fun c ->
           Printf.sprintf
             "{pdev_id:%04x; psubdev_id:%s; vdev_id:%04x; vsubdev_id:%04x; framebufferlength:0x%Lx}"
             c.identifier.pdev_id
             (match c.identifier.psubdev_id with
              | None -> "Any"
              | Some id -> Printf.sprintf "%04x" id)
             c.identifier.vdev_id
             c.identifier.vsubdev_id
             c.framebufferlength)
           relevant_vgpu_confs));
    let rec build_vgpu_types pci_access ac = function
      | [] -> ac
      | conf::tl ->
        debug "Pci.lookup_subsystem_device_name: vendor=%04x device=%04x subdev=%04x"
          nvidia_vendor_id conf.identifier.vdev_id conf.identifier.vsubdev_id;
        let vendor_name = Pci.lookup_vendor_name pci_access nvidia_vendor_id
        and model_name =
          Pci.lookup_subsystem_device_name pci_access nvidia_vendor_id
            conf.identifier.vdev_id nvidia_vendor_id conf.identifier.vsubdev_id
        and framebuffer_size = conf.framebufferlength
        and max_heads = conf.num_heads
        and max_resolution_x = conf.max_x
        and max_resolution_y = conf.max_y
        and size = Int64.div Constants.pgpu_default_size conf.max_instance
        and internal_config = [Xapi_globs.vgpu_config_key, conf.file_path]
        and identifier = Nvidia conf.identifier
        and experimental = false in
        let vgpu_type = {
          vendor_name; model_name; framebuffer_size; max_heads;
          max_resolution_x; max_resolution_y; size; internal_config;
          identifier; experimental}
        in
        build_vgpu_types pci_access (vgpu_type :: ac) tl
    in
    Pci.with_access (fun a -> build_vgpu_types a [] relevant_vgpu_confs)

  let find_or_create_supported_types ~__context ~pci =
    let dev_id = Xapi_pci.int_of_id (Db.PCI.get_device_id ~__context ~self:pci) in
    let subsystem_dev_id =
      match Db.PCI.get_subsystem_device_id ~__context ~self:pci with
      | "" -> None
      | id_string -> Some (Xapi_pci.int_of_id id_string)
    in
    debug "dev_id = %s" (Printf.sprintf "%04x" dev_id);
    let relevant_types = relevant_vgpu_types dev_id subsystem_dev_id in
    debug "Relevant vGPU configurations for pgpu = [ %s ]"
      (String.concat "; "
         (List.map (fun vt -> vt.model_name) relevant_types));
    let vgpu_types = List.map
        (fun v -> find_or_create ~__context v) relevant_types in
    let passthrough_gpu_type = find_or_create ~__context passthrough_gpu in
    passthrough_gpu_type :: vgpu_types
end

module type VENDOR = sig
  type vgpu_conf
  val vendor_id : int
  val whitelist_file : unit -> string
  val device_id_of_conf : vgpu_conf -> int
  val read_whitelist_line : line:string -> vgpu_conf option
  val vgpu_type_of_conf : string -> Pci.Pci_dev.t -> vgpu_conf -> vgpu_type option
end

module Vendor = functor (V : VENDOR) -> struct
  let vendor_id = V.vendor_id

  let read_whitelist ~whitelist ~device_id =
    if Sys.file_exists whitelist then begin
      Stdext.Unixext.file_lines_fold
        Identifier.(fun acc line ->
            match V.read_whitelist_line ~line with
            | Some conf when V.device_id_of_conf conf = device_id -> conf :: acc
            | _ -> acc)
        []
        whitelist
    end else []

  let make_vgpu_types ~__context ~pci ~whitelist =
    let open Xenops_interface.Pci in
    let device_id =
      Db.PCI.get_device_id ~__context ~self:pci
      |> Xapi_pci.int_of_id
    in
    let address =
      Db.PCI.get_pci_id ~__context ~self:pci
      |> address_of_string
    in
    let whitelist = read_whitelist ~whitelist ~device_id in
    let vendor_name, device =
      Pci.(with_access (fun access ->
          let vendor_name = lookup_vendor_name access V.vendor_id in
          let device =
            List.find
              (fun device ->
                 (device.Pci_dev.domain = address.domain) &&
                 (device.Pci_dev.bus = address.bus) &&
                 (device.Pci_dev.dev = address.dev) &&
                 (device.Pci_dev.func = address.fn))
              (get_devices access)
          in
          vendor_name, device))
    in
    List.filter_map (V.vgpu_type_of_conf vendor_name device) whitelist

  let find_or_create_supported_types ~__context ~pci
      ~is_system_display_device
      ~is_host_display_enabled
      ~is_pci_hidden =
    let types =
      let passthrough_types =
        if is_system_display_device && (is_host_display_enabled || not is_pci_hidden)
        then []
        else [passthrough_gpu]
      in
      passthrough_types @
      (make_vgpu_types ~__context ~pci ~whitelist:(V.whitelist_file ()))
    in
    List.map (find_or_create ~__context) types
end

module Vendor_intel = struct
  type vgpu_conf = {
    identifier : Identifier.gvt_g_id;
    experimental : bool;
    model_name : string;
    framebufferlength : int64;
    num_heads : int64;
    max_x : int64;
    max_y : int64;
  }

  let vendor_id = 0x8086
  let whitelist_file () = !Xapi_globs.gvt_g_whitelist
  let device_id_of_conf conf = conf.identifier.Identifier.pdev_id

  let read_whitelist_line ~line =
    try
      Some (Scanf.sscanf
              line
              "%04x experimental=%c name='%s@' low_gm_sz=%Ld high_gm_sz=%Ld fence_sz=%Ld framebuffer_sz=%Ld max_heads=%Ld resolution=%Ldx%Ld monitor_config_file=%s"
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
                monitor_config_file ->
                {
                  identifier = Identifier.({
                      pdev_id;
                      low_gm_sz;
                      high_gm_sz;
                      fence_sz;
                      monitor_config_file = Some monitor_config_file;
                    });
                  experimental =
                    (match experimental with
                     | '0' -> false
                     | _ -> true);
                  model_name;
                  framebufferlength = mib framebuffer_sz;
                  num_heads;
                  max_x;
                  max_y;
                }))
    with e ->
      error "Failed to read whitelist line: '%s' %s"
        line (Printexc.to_string e);
      None
  
  let vgpu_type_of_conf vendor_name device conf =
    let open Identifier in
    let bar_size =
      List.nth device.Pci.Pci_dev.size 2
      |> Int64.of_nativeint
    in
    let vgpus_per_pgpu =
      bar_size /// 1024L /// 1024L
      /// conf.identifier.low_gm_sz
      --- 1L
    in
    if vgpus_per_pgpu <= 0L then 
      begin
        warn "Not enough memory for Intel VGPUs. \
              If you intend to use them, increase the GPU \
              BAR size in the BIOS settings.";
        None
      end
    else
      let vgpu_size =
        Constants.pgpu_default_size /// vgpus_per_pgpu
      in
      let internal_config = let open Xapi_globs in
        List.concat [
          [ vgt_low_gm_sz, Int64.to_string conf.identifier.low_gm_sz
          ; vgt_high_gm_sz, Int64.to_string conf.identifier.high_gm_sz
          ; vgt_fence_sz, Int64.to_string conf.identifier.fence_sz
          ]
        ; match conf.identifier.monitor_config_file with
          | Some monitor_config_file ->
            [vgt_monitor_config_file, monitor_config_file]
          | None -> []
        ]
      in
      Some {
        vendor_name;
        model_name = conf.model_name;
        framebuffer_size = conf.framebufferlength;
        max_heads = conf.num_heads;
        max_resolution_x = conf.max_x;
        max_resolution_y = conf.max_y;
        size = vgpu_size;
        internal_config = internal_config;
        identifier = GVT_g conf.identifier;
        experimental = conf.experimental;
      }
end

module Vendor_amd = struct
  type vgpu_conf = {
    (* The identifier has fields for framebuffer size and scheduling slice. *)
    identifier : Identifier.mxgpu_id;
    model_name : string;
    vgpus_per_pgpu : int64;
  }

  let vendor_id = 0x1002
  let whitelist_file () = !Xapi_globs.mxgpu_whitelist
  let device_id_of_conf conf = conf.identifier.Identifier.pdev_id

  let read_whitelist_line ~line =
    try
      Some (Scanf.sscanf
              line
              "%04x name='%s@' framebuffer_sz=%Ld sched=%d vgpus_per_pgpu=%Ld"
              (fun pdev_id (* e.g. "FirePro S7150" has 6929 (PF), 692f (VF) *)
                model_name (* e.g. PF "FirePro S7150" or VF "FirePro S7150V" *)
                framebufferbytes
                sched
                vgpus_per_pgpu ->
                {
                  identifier = Identifier.({
                      pdev_id;
                      sched;
                      framebufferbytes;
                    });
                  model_name;
                  vgpus_per_pgpu;
                }))
    with e ->
      error "Failed to read whitelist line: '%s' %s"
        line (Printexc.to_string e);
      None
  
  let vgpu_type_of_conf vendor_name _ conf =
    let open Identifier in
    let size_of_pgpu = (* counterpart of bar_size in gvt-g *)
      Constants.pgpu_default_size
    in
    if conf.vgpus_per_pgpu <= 0L then 
      begin
        warn "Ignoring a config line that specifies 0 vgpus per pgpu.";
        None
      end
    else
      let vgpu_size =
        size_of_pgpu /// conf.vgpus_per_pgpu
      in
      let internal_config = let open Xapi_globs in
        [ mxgpu_sched, string_of_int conf.identifier.sched ]
      in
      Some {
        vendor_name;
        model_name = conf.model_name;
        framebuffer_size = conf.identifier.framebufferbytes;
        max_heads = 0L;
        max_resolution_x = 0L;
        max_resolution_y = 0L;
        size = vgpu_size;
        internal_config = internal_config;
        identifier = MxGPU conf.identifier;
        experimental = false;
      }
end

module Intel = Vendor(Vendor_intel)
module AMD = Vendor(Vendor_amd)

let find_or_create_supported_types ~__context ~pci
    ~is_system_display_device
    ~is_host_display_enabled
    ~is_pci_hidden =
  let vendor_id =
    Db.PCI.get_vendor_id ~__context ~self:pci
    |> Xapi_pci.int_of_id
  in
  match vendor_id with
  | x when x = Nvidia.nvidia_vendor_id -> begin
      if is_system_display_device then []
      else Nvidia.find_or_create_supported_types ~__context ~pci
    end
  | x when x = Intel.vendor_id -> begin
      Intel.find_or_create_supported_types ~__context ~pci
        ~is_system_display_device
        ~is_host_display_enabled
        ~is_pci_hidden
    end
  | x when x = AMD.vendor_id -> begin
      AMD.find_or_create_supported_types ~__context ~pci
        ~is_system_display_device
        ~is_host_display_enabled
        ~is_pci_hidden
    end
  | _ -> begin
      if is_system_display_device then []
      else [find_or_create ~__context passthrough_gpu]
    end

let requires_passthrough ~__context ~self =
  Db.VGPU_type.get_implementation ~__context ~self = `passthrough
