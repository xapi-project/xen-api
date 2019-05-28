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

open Xapi_vgpu_type

let k100 = {
  vendor_name = "NVIDIA Corporation";
  model_name = "GRID K100";
  framebuffer_size = 268435456L;
  max_heads = 2L;
  max_resolution_x = 1920L;
  max_resolution_y = 1200L;
  size = Int64.div Constants.pgpu_default_size 8L;
  internal_config = [
    Xapi_globs.vgpu_type_id, "type_id_1"
  ];
  identifier = Identifier.(Nvidia {
      pdev_id = 0x0ff2;
      psubdev_id = None;
      vdev_id = 0x0fe7;
      vsubdev_id = 0x101e;
    });
  experimental = false;
  compatible_model_names_in_vm = [];
  compatible_model_names_on_pgpu = ["GRID K100"];
}

let k140q = {
  vendor_name = "NVIDIA Corporation";
  model_name = "GRID K140Q";
  framebuffer_size = 1006632960L;
  max_heads = 2L;
  max_resolution_x = 2560L;
  max_resolution_y = 1600L;
  size = Int64.div Constants.pgpu_default_size 4L;
  internal_config = [
    Xapi_globs.vgpu_type_id, "type_id_2"
  ];
  identifier = Identifier.(Nvidia {
      pdev_id = 0x0ff2;
      psubdev_id = None;
      vdev_id = 0x0ff7;
      vsubdev_id = 0x1037;
    });
  experimental = false;
  compatible_model_names_in_vm = [];
  compatible_model_names_on_pgpu = ["GRID K140Q"];
}

let k200 = {
  vendor_name = "NVIDIA Corporation";
  model_name = "GRID K200";
  framebuffer_size = 268435456L;
  max_heads = 2L;
  max_resolution_x = 1920L;
  max_resolution_y = 1200L;
  size = Int64.div Constants.pgpu_default_size 8L;
  internal_config = [
    Xapi_globs.vgpu_type_id, "type_id_3"
  ];
  identifier = Identifier.(Nvidia {
      pdev_id = 0x11bf;
      psubdev_id = None;
      vdev_id = 0x118d;
      vsubdev_id = 0x101d;
    });
  experimental = false;
  compatible_model_names_in_vm = [];
  compatible_model_names_on_pgpu = ["GRID K200"];
}

let k240q = {
  vendor_name = "NVIDIA Corporation";
  model_name = "GRID K240Q";
  framebuffer_size = 1006632960L;
  max_heads = 2L;
  max_resolution_x = 2560L;
  max_resolution_y = 1600L;
  size = Int64.div Constants.pgpu_default_size 4L;
  internal_config = [
    Xapi_globs.vgpu_type_id, "type_id_4"
  ];
  identifier = Identifier.(Nvidia {
      pdev_id = 0x11bf;
      psubdev_id = None;
      vdev_id = 0x11b0;
      vsubdev_id = 0x101a;
    });
  experimental = false;
  compatible_model_names_in_vm = [];
  compatible_model_names_on_pgpu = ["GRID K240Q"];
}

let k260q = {
  vendor_name = "NVIDIA Corporation";
  model_name = "GRID K260Q";
  framebuffer_size = 2013265920L;
  max_heads = 4L;
  max_resolution_x = 2560L;
  max_resolution_y = 1600L;
  size = Int64.div Constants.pgpu_default_size 2L;
  internal_config = [
    Xapi_globs.vgpu_type_id, "type_id_5"
  ];
  identifier = Identifier.(Nvidia {
      pdev_id = 0x11bf;
      psubdev_id = None;
      vdev_id = 0x11b0;
      vsubdev_id = 0x101b;
    });
  experimental = false;
  compatible_model_names_in_vm = [];
  compatible_model_names_on_pgpu = ["GRID K260Q"];
}

let k1_vgpu_types = [
  k100;
  k140q;
  passthrough_gpu;
]

let k2_vgpu_types = [
  k200;
  k240q;
  k260q;
  passthrough_gpu;
]

let gvt_g_041a = {
  vendor_name = "Intel Corporation";
  model_name = "Intel GVT-g";
  framebuffer_size = 134217728L;
  max_heads = 1L;
  max_resolution_x = 1920L;
  max_resolution_y = 1080L;
  size = Int64.div Constants.pgpu_default_size 7L;
  internal_config = [
    Xapi_globs.vgt_low_gm_sz, "128";
    Xapi_globs.vgt_high_gm_sz, "384";
    Xapi_globs.vgt_fence_sz, "4";
  ];
  identifier = Identifier.(GVT_g {
      pdev_id = 0x041a;
      low_gm_sz = 128L;
      high_gm_sz = 384L;
      fence_sz = 4L;
    });
  experimental = false;
  compatible_model_names_in_vm = [];
  compatible_model_names_on_pgpu = [];
}

let intel_041a_vgpu_types = [
  gvt_g_041a;
  passthrough_gpu;
]

(* Represents the state of a PGPU, its supported and enabled VGPU types, and
 * the types of the VGPUs running and scheduled to run on it. *)
type pgpu_state = {
  supported_VGPU_types: vgpu_type list;
  enabled_VGPU_types: vgpu_type list;
  resident_VGPU_types: vgpu_type list;
  scheduled_VGPU_types: vgpu_type list;
}

let default_k1 = {
  supported_VGPU_types = k1_vgpu_types;
  enabled_VGPU_types = k1_vgpu_types;
  resident_VGPU_types = [];
  scheduled_VGPU_types = [];
}

let default_k2 = {
  supported_VGPU_types = k2_vgpu_types;
  enabled_VGPU_types = k2_vgpu_types;
  resident_VGPU_types = [];
  scheduled_VGPU_types = [];
}

let default_intel_041a = {
  supported_VGPU_types = intel_041a_vgpu_types;
  enabled_VGPU_types = intel_041a_vgpu_types;
  resident_VGPU_types = [];
  scheduled_VGPU_types = [];
}

let string_of_vgpu_type vgpu_type =
  vgpu_type.model_name

let string_of_pgpu_state pgpu =
  Printf.sprintf "{supported: %s; enabled: %s; resident: %s; scheduled: %s}"
    (Test_printers.(list string_of_vgpu_type) pgpu.supported_VGPU_types)
    (Test_printers.(list string_of_vgpu_type) pgpu.enabled_VGPU_types)
    (Test_printers.(list string_of_vgpu_type) pgpu.resident_VGPU_types)
    (Test_printers.(list string_of_vgpu_type) pgpu.scheduled_VGPU_types)

let make_vgpu ~__context
    ?(vm_ref=Ref.null)
    ?(gPU_group=Ref.null)
    ?(resident_on=Ref.null)
    ?(scheduled_to_be_resident_on=Ref.null)
    ?(uuid=Test_common.make_uuid())
    ?(extra_args="")
    vgpu_type =
  let vgpu_type_ref = find_or_create ~__context vgpu_type in
  (* For the passthrough VGPU type, create a VM and mark it as attached to the
     	 * PGPU's PCI device. *)
  let vm_ref =
    if Db.is_valid_ref __context vm_ref
    then vm_ref
    else Test_common.make_vm ~__context ()
  in
  if (Xapi_vgpu_type.requires_passthrough ~__context ~self:vgpu_type_ref = Some `PF)
  && (Db.is_valid_ref __context resident_on)
  then begin
    let pci_ref = Db.PGPU.get_PCI ~__context ~self:resident_on in
    Db.PCI.add_attached_VMs ~__context ~self:pci_ref ~value:vm_ref
  end;
  Test_common.make_vgpu ~__context
    ~vM:vm_ref
    ~_type:vgpu_type_ref
    ~uuid
    ~gPU_group
    ~resident_on
    ~scheduled_to_be_resident_on
    ~extra_args ()

let make_pgpu ~__context ?address ?(host=Ref.null) ?(gPU_group=Ref.null) pgpu =
  let pCI = Test_common.make_pci ~__context ?pci_id:address ~host ~functions:1L () in
  let supported_VGPU_types =
    List.map (find_or_create ~__context) pgpu.supported_VGPU_types
  in
  let enabled_VGPU_types =
    List.map (find_or_create ~__context) pgpu.supported_VGPU_types
  in
  let pgpu_ref = Test_common.make_pgpu ~__context
      ~pCI
      ~host
      ~gPU_group
      ~supported_VGPU_types
      ~enabled_VGPU_types () in
  List.iter
    (fun vgpu_type ->
       let (_: API.ref_VGPU) =
         (make_vgpu ~__context ~resident_on:pgpu_ref vgpu_type) in ())
    pgpu.resident_VGPU_types;
  List.iter
    (fun vgpu_type ->
       let (_: API.ref_VGPU) =
         (make_vgpu ~__context ~scheduled_to_be_resident_on:pgpu_ref vgpu_type)
       in ())
    pgpu.scheduled_VGPU_types;
  pgpu_ref
