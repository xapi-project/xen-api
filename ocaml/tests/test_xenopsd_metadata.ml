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

open OUnit
open Test_common
open Test_highlevel
open Xenops_interface
open Xcp_pci

let test_vm_name = "__test_vm"

let rpc_of t x = Rpcmarshal.marshal t.Rpc.Types.ty x

type vm_config = {
  oc: (string * string) list;
  platform: (string * string) list;
}

let string_of_vm_config conf =
  Printf.sprintf "other_config = %s, platform = %s"
    (Test_printers.(assoc_list string string) conf.oc)
    (Test_printers.(assoc_list string string) conf.platform)

let string_of_vgpu_type {Xapi_vgpu_type.vendor_name; model_name} =
  Printf.sprintf "vendor_name = %s, model_name = %s" vendor_name model_name

let load_vm_config __context conf =
  let (self: API.ref_VM) = make_vm ~__context
      ~name_label:test_vm_name
      ~domain_type:`hvm
      ~other_config:conf.oc
      ~platform:conf.platform
      ()
  in
  let flags = [
    Xapi_globs.cpu_info_vendor_key, "AuthenticAMD";
    Xapi_globs.cpu_info_features_key, "deadbeef-deadbeef";
  ] in
  Db.VM.set_last_boot_CPU_flags ~__context ~self ~value:flags;
  self

let run_create_metadata ~__context =
  let vms = Db.VM.get_by_name_label ~__context ~label:test_vm_name in
  let vm = List.nth vms 0 in
  Xapi_xenops.create_metadata ~__context ~self:vm

(* Test the behaviour of the "hvm_serial" other_config/platform key. *)
module HVMSerial =
  Generic.Make(
      Generic.EncapsulateState(struct
          module Io = struct
            type input_t = vm_config
            type output_t = string option

            let string_of_input_t = string_of_vm_config
            let string_of_output_t = Test_printers.(option string)
          end

          module State = Test_state.XapiDb

          let load_input __context conf =
            let (_ : API.ref_VM) = load_vm_config __context conf in ()

          let extract_output __context _ =
            let metadata = run_create_metadata ~__context in
            match metadata.Metadata.vm.Vm.ty with
            | Vm.HVM {Vm.serial = serial} -> serial
            | _ -> failwith "expected HVM metadata"

          let tests =
            [
              (* Should default to "pty" if nothing is set. *)
              (
                {oc=[]; platform=[]},
                Some "pty"
              );
              (* other_config value should override default if no platform value. *)
              (
                {oc=["hvm_serial", "none"]; platform=[]},
                Some "none"
              );
              (* Should be able to disable serial emulation via the platform key. *)
              (
                {oc=[]; platform=["hvm_serial", "none"]},
                Some "none"
              );
              (* platform value should override other_config value. *)
              (
                {oc=["hvm_serial", "none"]; platform=["hvm_serial", "pty"]},
                Some "pty"
              );
              (* platform value should override other_config value. *)
              (
                {oc=["hvm_serial", "pty"]; platform=["hvm_serial", "none"]},
                Some "none"
              );
              (* Windows debugger redirects the serial port to tcp - this should be
               * configurable via the other_config key. *)
              (
                {oc=["hvm_serial", "tcp:1.2.3.4:7001"]; platform=[]},
                Some "tcp:1.2.3.4:7001"
              );
              (* Windows debugger should be configurable via the platform key too. *)
              (
                {oc=[]; platform=["hvm_serial", "tcp:1.2.3.4:7001"]},
                Some "tcp:1.2.3.4:7001"
              );
              (* Windows debugger setting via the platform key should override anything
               * set in other_config. *)
              (
                {oc=["hvm_serial", "none"]; platform=["hvm_serial", "tcp:1.2.3.4:7001"]},
                Some "tcp:1.2.3.4:7001"
              );
            ]
        end))
  
module VideoMode =
  Generic.Make(
      Generic.EncapsulateState(struct
          module Io = struct
            type input_t = vm_config
            type output_t = Vm.video_card

            let string_of_input_t = string_of_vm_config
            let string_of_output_t = function
              | Vm.Cirrus -> "Cirrus"
              | Vm.Standard_VGA -> "Standard_VGA"
              | Vm.Vgpu -> "Vgpu"
              | Vm.IGD_passthrough Vm.GVT_d -> "IGD_passthrough"
          end

          module State = Test_state.XapiDb

          let load_input __context conf =
            let (_ : API.ref_VM) = load_vm_config __context conf in ()

          let extract_output __context _ =
            let metadata = run_create_metadata ~__context in
            match metadata.Metadata.vm.Vm.ty with
            | Vm.HVM {Vm.video = video_mode} -> video_mode
            | _ -> failwith "expected HVM metadata"

          let tests = [
              (* Default video mode should be Cirrus. *)
              {oc=[]; platform=[]}, Vm.Cirrus;
              (* Unrecognised video mode should default to Cirrus. *)
              {oc=[]; platform=["vga", "foo"]}, Vm.Cirrus;
              (* Video modes set in the platform map should be respected. *)
              {oc=[]; platform=["vga", "cirrus"]}, Vm.Cirrus;
              {oc=[]; platform=["vga", "std"]}, Vm.Standard_VGA;
              (* The IGD passthrough key should be respected. *)
              {oc=[]; platform=["igd_passthrough", "true"]}, Vm.(IGD_passthrough GVT_d);
              (* The IGD passthrough should override the "vga" key. *)
              {
                oc=[];
                platform=["igd_passthrough", "true"; "vga", "cirrus"]
              }, Vm.(IGD_passthrough GVT_d);
              {
                oc=[];
                platform=["igd_passthrough", "true"; "vga", "std"]
              }, Vm.(IGD_passthrough GVT_d);
            ]
        end))

module VideoRam =
  Generic.Make(
      Generic.EncapsulateState(struct
          module Io = struct
            type input_t = vm_config
            type output_t = int

            let string_of_input_t = string_of_vm_config
            let string_of_output_t = Test_printers.int
          end

          module State = Test_state.XapiDb

          let load_input __context conf =
            let (_ : API.ref_VM) = load_vm_config __context conf in ()

          let extract_output __context _ =
            let metadata = run_create_metadata ~__context in
            match metadata.Metadata.vm.Vm.ty with
            | Vm.HVM {Vm.video_mib = video_mib} -> video_mib
            | _ -> failwith "expected HVM metadata"

          let tests = [
              (* Video ram defaults to 4MiB. *)
              {oc=[]; platform=[]}, 4;
              (* Specifying a different amount of videoram works. *)
              {oc=[]; platform=["videoram", "8"]}, 8;
              (* Other VGA options shouldn't affect the videoram setting. *)
              {oc = []; platform=["vga", "cirrus"]}, 4;
              {oc = []; platform=["vga", "cirrus"; "videoram", "8"]}, 8;
            ]
        end))
  
let uuid_with_index index =
 Printf.sprintf "00000000-0000-0000-0000-00000000%04d" index

module GenerateVGPUMetadata =
  Generic.Make(
      Generic.EncapsulateState(struct
          open Test_vgpu_common

          module Io = struct
            type input_t =
              vm_config * (Test_vgpu_common.pgpu_state * Xapi_vgpu_type.vgpu_type) list
            type output_t = Xenops_interface.Vgpu.implementation list

            let string_of_input_t =
              Test_printers.(pair
                               string_of_vm_config
                               (list (pair Test_vgpu_common.string_of_pgpu_state string_of_vgpu_type)))
            let string_of_output_t =
              Test_printers.list (fun vgpu ->
                  rpc_of Xenops_interface.Vgpu.implementation vgpu |> Rpc.to_string)
          end

          module State = Test_state.XapiDb

          let load_input __context (vm_config, pgpus_and_vgpu_types) =
            let vm_ref = load_vm_config __context vm_config in
            List.iteri
              (fun index (pgpu, vgpu_type) ->
                let pgpu_ref = make_pgpu ~__context
                                 ~address:(Printf.sprintf "0000:%02d:00.0" index) pgpu
                in
                let (_ : API.ref_VGPU) =
                  make_vgpu ~__context
                    ~vm_ref ~scheduled_to_be_resident_on:pgpu_ref ~uuid:(uuid_with_index 0) vgpu_type in
                ())
              pgpus_and_vgpu_types

          let extract_output __context _ =
            let metadata = run_create_metadata ~__context in
            List.map
              (fun vgpu -> vgpu.Xenops_interface.Vgpu.implementation)
              metadata.Metadata.vgpus

          let tests = [
              (* No vGPUs. *)
              (
                {oc = []; platform = []},
                []
              ),
              [];
              (* One passthrough GPU. *)
              (
                {oc = []; platform = []},
                [default_k1, Xapi_vgpu_type.passthrough_gpu]
              ),
              [];
              (* One NVIDIA vGPU. *)
              (
                {oc = []; platform = []},
                [default_k1, k100]
              ),
              [
                Xenops_interface.Vgpu.(Nvidia {
                                           physical_pci_address = None;
                                           virtual_pci_address = Some {domain=0; bus = 0; dev = 0; fn = 0;};
                                           config_file = None;
                                           type_id = "type_id_1";
                                           uuid = uuid_with_index 0;
                })
              ];
              (* One Intel vGPU. *)
              (
                {oc = []; platform = []},
                [default_intel_041a, gvt_g_041a]
              ),
              [
                Xenops_interface.Vgpu.(GVT_g {
                                           physical_pci_address = None;
                                           low_gm_sz = 128L;
                                           high_gm_sz = 384L;
                                           fence_sz = 4L;
                                           monitor_config_file = None;
                })
              ];
            ]
        end))
  
module GenerateMultiVGPUMetadata =
  Generic.Make(
      Generic.EncapsulateState(struct
          open Test_vgpu_common
          module Io = struct
            type input_t =
              vm_config * (Test_vgpu_common.pgpu_state * Xapi_vgpu_type.vgpu_type) list
            type output_t = Xenops_interface.Vgpu.implementation list

            let string_of_input_t =
              Test_printers.(pair
                               string_of_vm_config
                               (list (pair Test_vgpu_common.string_of_pgpu_state string_of_vgpu_type)))
            let string_of_output_t =
              Test_printers.list (fun vgpu ->
                  rpc_of Xenops_interface.Vgpu.implementation vgpu |> Rpc.to_string)
          end

          module State = Test_state.XapiDb

          let load_input __context (vm_config, pgpus_and_vgpu_types) =
            let vm_ref = load_vm_config __context vm_config in
            List.iteri
              (fun index (pgpu, vgpu_type) ->
                let pgpu_ref = make_pgpu ~__context
                                 ~address:(Printf.sprintf "0000:%02d:00.0" index) pgpu
                in
                let test_uuid = uuid_with_index index in
                let (_ : API.ref_VGPU) =
                  make_vgpu ~__context
                    ~vm_ref ~scheduled_to_be_resident_on:pgpu_ref ~uuid:test_uuid vgpu_type in
                ())
              pgpus_and_vgpu_types

          let extract_output __context _ =
            let metadata = run_create_metadata ~__context in
            List.map
              (fun vgpu -> vgpu.Xenops_interface.Vgpu.implementation)
              metadata.Metadata.vgpus

          let tests = [
              (* 2 NVIDIA vGPUs. *)
              (
                {oc = []; platform = []},
                [default_k1, k100; default_k1, k100]
              ),
              [
                Xenops_interface.Vgpu.(Nvidia {
                                           physical_pci_address = None;
                                           virtual_pci_address = Some {domain=0; bus = 0; dev = 0; fn = 0;};
                                           config_file = None;
                                           type_id = "type_id_1";
                                           uuid = uuid_with_index 0;
                });
                Xenops_interface.Vgpu.(Nvidia {
                                           physical_pci_address = None;
                                           virtual_pci_address = Some {domain=0; bus = 0; dev = 0; fn = 0;};
                                           config_file = None;
                                           type_id = "type_id_1";
                                           uuid = uuid_with_index 1;
                })
              ];
              (* 4 NVIDIA vGPUs. *)
              (
                {oc = []; platform = []},
                [default_k1, k100; default_k1, k100; default_k1, k100; default_k1, k100]
              ),
              [
                Xenops_interface.Vgpu.(Nvidia {
                                           physical_pci_address = None;
                                           virtual_pci_address = Some {domain=0; bus = 0; dev = 0; fn = 0;};
                                           config_file = None;
                                           type_id = "type_id_1";
                                           uuid = uuid_with_index 0;
                });
                Xenops_interface.Vgpu.(Nvidia {
                                           physical_pci_address = None;
                                           virtual_pci_address = Some {domain=0; bus = 0; dev = 0; fn = 0;};
                                           config_file = None;
                                           type_id = "type_id_1";
                                           uuid = uuid_with_index 1;
                });
                Xenops_interface.Vgpu.(Nvidia {
                                           physical_pci_address = None;
                                           virtual_pci_address = Some {domain=0; bus = 0; dev = 0; fn = 0;};
                                           config_file = None;
                                           type_id = "type_id_1";
                                           uuid = uuid_with_index 2;
                });
                Xenops_interface.Vgpu.(Nvidia {
                                           physical_pci_address = None;
                                           virtual_pci_address = Some {domain=0; bus = 0; dev = 0; fn = 0;};
                                           config_file = None;
                                           type_id = "type_id_1";
                                           uuid = uuid_with_index 3;
                })                
              ];
            ]
        end))

let test =
  "test_xenopsd_metadata" >:::
  [
    "test_hvm_serial" >::: HVMSerial.tests;
    "test_videomode" >::: VideoMode.tests;
    "test_videoram" >::: VideoRam.tests;
    "test_generate_vgpu_metadata" >::: GenerateVGPUMetadata.tests;
    "test_generate_multi_vgpu_metadata" >::: GenerateMultiVGPUMetadata.tests;
  ]
