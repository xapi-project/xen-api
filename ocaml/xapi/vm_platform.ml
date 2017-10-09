(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

open Stdext
open Xstringext
open Listext

(* Keys we push through to xenstore. *)
let acpi = "acpi"
let apic = "apic"
let nx = "nx"
let pae = "pae"
let viridian = "viridian"
let acpi_s3 = "acpi_s3"
let acpi_s4 = "acpi_s4"
let mmio_size_mib = "mmio_size_mib"
let revision = "revision"
let device_id = "device_id"
let tsc_mode = "tsc_mode"
let device_model = "device-model"
let xenguest = "xenguest"
let pv_kernel_max_size = "pv-kernel-max-size"
let pv_ramdisk_max_size = "pv-ramdisk-max-size"
let pv_postinstall_kernel_max_size = "pv-postinstall-kernel-max-size"
let pv_postinstall_ramdisk_max_size = "pv-postinstall-ramdisk-max-size"
let usb = "usb"
let usb_tablet = "usb_tablet"
let parallel = "parallel"
let vga = "vga"
let vgpu_pci_id = Xapi_globs.vgpu_pci_key
let vgpu_config = Xapi_globs.vgpu_config_key
let igd_passthru_key = Xapi_globs.igd_passthru_key
let featureset = "featureset"
let nested_virt = "nested-virt"

(* The default value of device model should be set as
   'qemu-trad', 'qemu-upstream-compat', 'qemu-upstream' according to QEMU-upstream feature release stages *)
let fallback_device_model_stage_1      = "qemu-trad"
let fallback_device_model_stage_2      = fallback_device_model_stage_1
let fallback_device_model_stage_3      = "qemu-upstream-compat"
let fallback_device_model_stage_4      = fallback_device_model_stage_3
let default_device_model_default_value = fallback_device_model_stage_1

(* This is only used to block the 'present multiple physical cores as one big hyperthreaded core' feature *)
let filtered_flags = [
  acpi;
  apic;
  nx;
  pae;
  viridian;
  acpi_s3;
  acpi_s4;
  mmio_size_mib;
  revision;
  device_id;
  tsc_mode;
  device_model;
  xenguest;
  pv_kernel_max_size;
  pv_ramdisk_max_size;
  pv_postinstall_kernel_max_size;
  pv_postinstall_ramdisk_max_size;
  usb;
  usb_tablet;
  parallel;
  vga;
  vgpu_pci_id;
  vgpu_config;
  featureset;
  nested_virt;
]

(* Other keys we might want to write to the platform map. *)
let timeoffset = "timeoffset"
let generation_id = "generation-id"

(* Helper functions. *)
(* [is_valid key platformdata] returns true if:
   1. The key is _not_ in platformdata (absence of key is valid) or
   2. The key is in platformdata, associated with a booleanish value *)
let is_valid ~key ~platformdata =
  (not (List.mem_assoc key platformdata)) ||
  (match List.assoc key platformdata |> String.lowercase_ascii with
   | "true" | "1" | "false" | "0" -> true
   | v -> false)

let is_true ~key ~platformdata ~default =
  try
    match List.assoc key platformdata |> String.lowercase_ascii with
    | "true" | "1" -> true
    | "false" | "0" -> false
    | _ -> default (* Check for validity using is_valid if required *)
  with Not_found ->
    default

let is_valid_device_model ~key ~platformdata =
  try
    match List.assoc key platformdata with
    | "qemu-upstream-compat" -> true
    | _ -> false
  with Not_found ->
    false

let sanity_check ~platformdata ~vcpu_max ~vcpu_at_startup ~hvm ~filter_out_unknowns =
  (* Filter out unknown flags, if applicable *)
  let platformdata =
    if filter_out_unknowns
    then List.filter (fun (k, v) -> List.mem k filtered_flags) platformdata
    else platformdata
  in
  (* Filter out invalid TSC modes. *)
  let platformdata =
    List.filter
      (fun (k, v) -> k <> tsc_mode || List.mem v ["0"; "1"; "2"; "3"])
      platformdata
  in
  (* Sanity check for HVM domains with invalid VCPU configuration*)
  if hvm && (List.mem_assoc "cores-per-socket" platformdata) then
    begin
      try
        let cores_per_socket = int_of_string(List.assoc "cores-per-socket" platformdata) in
        (* VCPUs_max has to be a multiple of cores per socket *)
        if ((Int64.to_int(vcpu_max) mod cores_per_socket) <> 0) then
          raise (Api_errors.Server_error(Api_errors.invalid_value,
                                         ["platform:cores-per-socket";
                                          "VCPUs_max must be a multiple of this field"]))
      with Failure msg ->
        raise (Api_errors.Server_error(Api_errors.invalid_value, ["platform:cores-per-socket";
                                                                  Printf.sprintf "value = %s is not a valid int" (List.assoc "cores-per-socket" platformdata)]))
    end;
  (* Add usb emulation flags.
     Make sure we don't send usb=false and usb_tablet=true,
     as that wouldn't make sense. *)
  let usb_enabled =
    is_true ~key:usb ~platformdata ~default:true in
  let usb_tablet_enabled =
    if usb_enabled
    then is_true ~key:usb_tablet ~platformdata ~default:true
    else false
  in
  let platformdata =
    List.update_assoc
      [(usb, string_of_bool usb_enabled);
       (usb_tablet, string_of_bool usb_tablet_enabled)]
      platformdata
  in
  (* Filter out invalid values for the "parallel" key. We don't want to give
   * guests access to anything other than a real parallel port. *)
  let platformdata =
    let is_valid_parallel_flag = function
      | "none" -> true
      | dev -> String.startswith "/dev/parport" dev
    in
    List.filter
      (fun (k, v) -> k <> parallel || is_valid_parallel_flag v)
      platformdata
  in
  platformdata


let check_restricted_flags ~__context platform =
  if not (is_valid nested_virt platform) then
    raise (Api_errors.Server_error
             (Api_errors.invalid_value,
              [Printf.sprintf "platform:%s" nested_virt;
               List.assoc nested_virt platform]));

  if is_true nested_virt platform false
  then Pool_features.assert_enabled ~__context ~f:Features.Nested_virt

let check_restricted_device_model ~__context platform =
  if not (is_valid_device_model device_model platform) then
    raise (Api_errors.Server_error(Api_errors.invalid_value,
            [Printf.sprintf "platform:%s when vm has VUSBs" device_model
            ; try List.assoc device_model platform with _ -> "undefined"])
          )
