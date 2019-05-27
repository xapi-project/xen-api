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
open Test_vgpu_common
open Xapi_vgpu_type

let mib x = List.fold_left Int64.mul x [1024L; 1024L]

module NvidiaTest = struct
  (*
  let string_of_vgpu_conf conf =
    let open Identifier in
    let open Nvidia_old in
    Printf.sprintf "%04x %s %04x %04x %Ld"
      conf.identifier.pdev_id
      (match conf.identifier.psubdev_id with
       | Some id -> Printf.sprintf "Some %04x" id
       | None -> "None")
      conf.identifier.vdev_id
      conf.identifier.vsubdev_id
      conf.framebufferlength

  let print_vgpu_conf conf =
    Printf.printf "%s\n" (string_of_vgpu_conf conf)

  module OfConfFile = Generic.Make(struct
      module Io = struct
        type input_t = string
        type output_t = Nvidia_old.vgpu_conf

        let string_of_input_t x = x
        let string_of_output_t = string_of_vgpu_conf
      end

      let transform = Nvidia_old.of_conf_file

      let tests = [
        "test_data/test_vgpu_subdevid.conf",
        Nvidia_old.({
            identifier = Identifier.({
                pdev_id = 0x3333;
                psubdev_id = Some 0x4444;
                vdev_id = 0x1111;
                vsubdev_id = 0x2222;
              });
            framebufferlength = 0x10000000L;
            num_heads = 2L;
            max_instance = 8L;
            max_x = 1920L;
            max_y = 1200L;
            file_path = "test_data/test_vgpu_subdevid.conf";
            compatible_model_names_in_vm = [];
            compatible_model_names_on_pgpu = [];
          });
        "test_data/test_vgpu_nosubdevid.conf",
        Nvidia_old.({
            identifier = Identifier.({
                pdev_id = 0x3333;
                psubdev_id = None;
                vdev_id = 0x1111;
                vsubdev_id = 0x2222;
              });
            framebufferlength = 0x10000000L;
            num_heads = 2L;
            max_instance = 8L;
            max_x = 1920L;
            max_y = 1200L;
            file_path = "test_data/test_vgpu_nosubdevid.conf";
            compatible_model_names_in_vm = [];
            compatible_model_names_on_pgpu = [];
          });
      ]
    end)
*)
  let string_of_vgpu_conf conf =
    let open Identifier in
    let open Vendor_nvidia in
    Printf.sprintf "%04x %s %04x %04x %Ld %Ld %Ld %Ldx%Ld"
      conf.identifier.pdev_id
      (match conf.identifier.psubdev_id with
       | Some id -> Printf.sprintf "Some %04x" id
       | None -> "None")
      conf.identifier.vdev_id
      conf.identifier.vsubdev_id
      conf.framebufferlength
      conf.num_heads
      conf.max_instance
      conf.max_x
      conf.max_y

  module ReadWhitelist = Generic.Make(struct
      module Io = struct
        type input_t = (string * int) (* whitelist * device_id *)
        type output_t = Vendor_nvidia.vgpu_conf list

        let string_of_input_t (whitelist, device_id) =
          Printf.sprintf "(%s, %04x)" whitelist device_id
        let string_of_output_t =
          Test_printers.list string_of_vgpu_conf
      end

      let transform (whitelist, device_id) =
        Vendor_nvidia.read_whitelist ~whitelist ~device_id

      let tests = [
        ("test_data/this-file-is-not-there.xml", 0x3333),
        [];
        ("test_data/nvidia-whitelist.xml", 0x4444),
        [];
        ("test_data/nvidia-whitelist.xml", 0x3333),
        [
          Vendor_nvidia.({
              identifier = Identifier.({
                  pdev_id = 0x3333;
                  psubdev_id = Some 0x4444;
                  vdev_id = 0x1111;
                  vsubdev_id = 0x2222;
                });
              framebufferlength = 0x10000000L;
              num_heads = 2L;
              max_instance = 8L;
              max_x = 1920L;
              max_y = 1200L;
              file_path = "test_data/nvidia-whitelist.xml";
              type_id = "11";
              compatible_model_names_in_vm = [];
              compatible_model_names_on_pgpu = ["TYPE FOO1"];
            })
        ];
        ("test_data/nvidia-whitelist.xml", 0x3334),
        [
          Vendor_nvidia.({
              identifier = Identifier.({
                  pdev_id = 0x3334;
                  psubdev_id = None;
                  vdev_id = 0x1111;
                  vsubdev_id = 0x2222;
                });
              framebufferlength = 0x10000000L;
              num_heads = 2L;
              max_instance = 8L;
              max_x = 1920L;
              max_y = 1200L;
              file_path = "test_data/nvidia-whitelist.xml";
              type_id = "11";
              compatible_model_names_in_vm = [];
              compatible_model_names_on_pgpu = ["TYPE FOO1"];
            })
        ];
        ("test_data/nvidia-whitelist.xml", 0x3335),
        [
          Vendor_nvidia.({
              identifier = Identifier.({
                  pdev_id = 0x3335;
                  psubdev_id = Some 0x4445;
                  vdev_id = 0x1112;
                  vsubdev_id = 0x2223;
                });
              framebufferlength = 0x20000000L;
              num_heads = 4L;
              max_instance = 8L;
              max_x = 2400L;
              max_y = 1600L;
              file_path = "test_data/nvidia-whitelist.xml";
              type_id = "20";
              compatible_model_names_in_vm = [];
              compatible_model_names_on_pgpu = ["TYPE FOO2"];
            });
          Vendor_nvidia.({
              identifier = Identifier.({
                  pdev_id = 0x3335;
                  psubdev_id = Some 0x4445;
                  vdev_id = 0x1111;
                  vsubdev_id = 0x2222;
                });
              framebufferlength = 0x10000000L;
              num_heads = 2L;
              max_instance = 16L;
              max_x = 1920L;
              max_y = 1200L;
              file_path = "test_data/nvidia-whitelist.xml";
              type_id = "21";
              compatible_model_names_in_vm = [];
              compatible_model_names_on_pgpu = ["TYPE FOO3"];
            })
        ];
        
      ]
    end)

  (* This test generates a lot of print --- set skip to false to enable *)
  let skip = true

  let print_nv_types () =
    skip_if skip "Generates print...";
    
end

module IntelTest = struct
  let string_of_vgpu_conf conf =
    let open Identifier in
    let open Vendor_intel in
    Printf.sprintf "%04x %Ld %Ld %Ld %b %s"
      conf.identifier.pdev_id
      conf.identifier.low_gm_sz
      conf.identifier.high_gm_sz
      conf.identifier.fence_sz
      conf.experimental
      conf.model_name

  module ReadWhitelistLine = Generic.Make(struct
      module Io = struct
        type input_t = string
        type output_t = Vendor_intel.vgpu_conf option

        let string_of_input_t x = x
        let string_of_output_t = Test_printers.option string_of_vgpu_conf
      end

      let transform line = Vendor_intel.read_whitelist_line ~line

      let tests = [
        (* Test some failure cases. *)
        "", None;
        "nonsense123", None;
        (* Test some success cases. *)
        "1234 experimental=0 name='myvgpu' low_gm_sz=128 high_gm_sz=384 fence_sz=4 framebuffer_sz=128 max_heads=1 resolution=1920x1080",
        Some {
          Vendor_intel.identifier = Identifier.({
              pdev_id = 0x1234;
              low_gm_sz = 128L;
              high_gm_sz = 384L;
              fence_sz = 4L;
            });
          experimental = false;
          model_name = "myvgpu";
          framebufferlength = mib 128L;
          num_heads = 1L;
          max_x = 1920L;
          max_y = 1080L;
        };
        "1234 experimental=1 name='myvgpu' low_gm_sz=128 high_gm_sz=384 fence_sz=4 framebuffer_sz=128 max_heads=1 resolution=1920x1080",
        Some {
          Vendor_intel.identifier = Identifier.({
              pdev_id = 0x1234;
              low_gm_sz = 128L;
              high_gm_sz = 384L;
              fence_sz = 4L;
            });
          experimental = true;
          model_name = "myvgpu";
          framebufferlength = mib 128L;
          num_heads = 1L;
          max_x = 1920L;
          max_y = 1080L;
        };
      ]
    end)

  module ReadWhitelist = Generic.Make(struct
      module Io = struct
        type input_t = (string * int) (* whitelist * device_id *)
        type output_t = Vendor_intel.vgpu_conf list

        let string_of_input_t (whitelist, device_id) =
          Printf.sprintf "(%s, %04x)" whitelist device_id
        let string_of_output_t =
          Test_printers.list string_of_vgpu_conf
      end

      let transform (whitelist, device_id) =
        Vendor_intel.read_whitelist ~whitelist ~device_id |> List.rev

      let tests = [
        ("test_data/gvt-g-whitelist-empty", 0x1234), [];
        ("test_data/gvt-g-whitelist-missing", 0x1234), [];
        ("test_data/gvt-g-whitelist-1234", 0x1234),
        [
          Vendor_intel.({
              identifier = Identifier.({
                  pdev_id = 0x1234;
                  low_gm_sz = 128L;
                  high_gm_sz = 384L;
                  fence_sz = 4L;
                });
              experimental = false;
              model_name = "GVT-g on 1234";
              framebufferlength = mib 128L;
              num_heads = 1L;
              max_x = 1920L;
              max_y = 1080L;
            });
          Vendor_intel.({
              identifier = Identifier.({
                  pdev_id = 0x1234;
                  low_gm_sz = 128L;
                  high_gm_sz = 384L;
                  fence_sz = 4L;
                });
              experimental = true;
              model_name = "GVT-g on 1234 (experimental)";
              framebufferlength = mib 128L;
              num_heads = 1L;
              max_x = 1920L;
              max_y = 1080L;
            });
        ];
        ("test_data/gvt-g-whitelist-1234", 0x5678), [];
        ("test_data/gvt-g-whitelist-mixed", 0x1234),
        [
          Vendor_intel.({
              identifier = Identifier.({
                  pdev_id = 0x1234;
                  low_gm_sz = 128L;
                  high_gm_sz = 384L;
                  fence_sz = 4L;
                });
              experimental = false;
              model_name = "GVT-g on 1234";
              framebufferlength = mib 128L;
              num_heads = 1L;
              max_x = 1920L;
              max_y = 1080L;
            });
        ];
      ]
    end)
end

module AMDTest = struct
  let string_of_vgpu_conf conf =
    let open Identifier in
    let open Vendor_amd in
    Printf.sprintf "%04x %Ld %b %s %Ld"
      conf.identifier.pdev_id
      conf.identifier.framebufferbytes
      conf.experimental
      conf.model_name
      conf.vgpus_per_pgpu

  module ReadWhitelistLine = Generic.Make(struct
      module Io = struct
        type input_t = string
        type output_t = Vendor_amd.vgpu_conf option

        let string_of_input_t x = x
        let string_of_output_t = Test_printers.option string_of_vgpu_conf
      end

      let transform line = Vendor_amd.read_whitelist_line ~line

      let tests = [
        (* Test some failure cases. *)
        "", None;
        "nonsense123", None;
        (* Test some success cases. *)
        "1234 experimental=0 name='mymxgpu' framebuffer_sz=256 vgpus_per_pgpu=5",
        Some {
          Vendor_amd.identifier = Identifier.({
              pdev_id = 0x1234;
              framebufferbytes = mib 256L;
            });
          experimental = false;
          model_name = "mymxgpu";
          vgpus_per_pgpu = 5L;
        };
        "2345 experimental=1 name='yourmxgpu' framebuffer_sz=512 vgpus_per_pgpu=8",
        Some {
          Vendor_amd.identifier = Identifier.({
              pdev_id = 0x2345;
              framebufferbytes = mib 512L;
            });
          experimental = true;
          model_name = "yourmxgpu";
          vgpus_per_pgpu = 8L;
        };
      ]
    end)

  module ReadWhitelist = Generic.Make(struct
      module Io = struct
        type input_t = (string * int) (* whitelist * device_id *)
        type output_t = Vendor_amd.vgpu_conf list

        let string_of_input_t (whitelist, device_id) =
          Printf.sprintf "(%s, %04x)" whitelist device_id
        let string_of_output_t =
          Test_printers.list string_of_vgpu_conf
      end

      let transform (whitelist, device_id) =
        Vendor_amd.read_whitelist ~whitelist ~device_id |> List.rev

      let tests = [
        ("test_data/mxgpu-whitelist-empty", 0x1234), [];
        ("test_data/mxgpu-whitelist-missing", 0x1234), [];
        ("test_data/mxgpu-whitelist-1234", 0x1234),
        [
          Vendor_amd.({
              identifier = Identifier.({
                  pdev_id = 0x1234;
                  framebufferbytes = mib 128L;
                });
                experimental = false;
              model_name = "Small AMD MxGPU on 1234";
              vgpus_per_pgpu = 4L;
            });
          Vendor_amd.({
              identifier = Identifier.({
                  pdev_id = 0x1234;
                  framebufferbytes = mib 256L;
                });
              experimental = true;
              model_name = "Big AMD MxGPU on 1234";
              vgpus_per_pgpu = 2L;
            });
        ];
        ("test_data/mxgpu-whitelist-1234", 0x5678), [];
        ("test_data/mxgpu-whitelist-mixed", 0x1234),
        [
          Vendor_amd.({
              identifier = Identifier.({
                  pdev_id = 0x1234;
                  framebufferbytes = mib 128L;
                });
              experimental = false;
              model_name = "Small AMD MxGPU on 1234";
              vgpus_per_pgpu = 4L;
            });
        ];
      ]
    end)
end

let test_find_or_create () =
  let __context = make_test_database () in
  let k100_ref_1 = find_or_create ~__context k100 in
  (* Check the VGPU type created in the DB has the expected fields. *)
  assert_equal
    ~msg:"k100 framebuffer_size is incorrect"
    k100.framebuffer_size
    (Db.VGPU_type.get_framebuffer_size ~__context ~self:k100_ref_1);
  assert_equal
    ~msg:"k100 max_heads is incorrect"
    k100.max_heads
    (Db.VGPU_type.get_max_heads ~__context ~self:k100_ref_1);
  assert_equal
    ~msg:"k100 size is incorrect"
    k100.size
    (Db.VGPU_type.get_size ~__context ~self:k100_ref_1);
  assert_equal
    ~msg:"k100 experimental flag is incorrect"
    k100.experimental
    (Db.VGPU_type.get_experimental ~__context ~self:k100_ref_1);
  (* Simulate an update of framebuffer_size, max_heads, size and the
     	 * experimental flag, as if the config file had been updated. *)
  let new_k100 = {
    k100 with
    framebuffer_size = (Int64.mul k100.framebuffer_size 2L);
    max_heads = (Int64.mul k100.max_heads 2L);
    size = (Int64.mul k100.size 2L);
    experimental = not k100.experimental;
  } in
  (* We can ignore the result as it should be the same as the VGPU_type ref
     	 * obtained earlier. *)
  let k100_ref_2 = find_or_create ~__context new_k100 in
  (* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
     	 * been created. *)
  assert_equal
    ~msg:"New k100 type was created erroneously"
    k100_ref_1 k100_ref_2;
  (* Make sure the existing VGPU type object in the database
     	 * has been updated. *)
  assert_equal
    ~msg:"k100 framebuffer_size was not updated"
    new_k100.framebuffer_size
    (Db.VGPU_type.get_framebuffer_size ~__context ~self:k100_ref_1);
  assert_equal
    ~msg:"k100 max_heads was not updated"
    new_k100.max_heads
    (Db.VGPU_type.get_max_heads ~__context ~self:k100_ref_1);
  assert_equal
    ~msg:"k100 size was not updated"
    new_k100.size
    (Db.VGPU_type.get_size ~__context ~self:k100_ref_1);
  assert_equal
    ~msg:"k100 was not marked experimental"
    new_k100.experimental
    (Db.VGPU_type.get_experimental ~__context ~self:k100_ref_1)

let test_identifier_lookup () =
  let test_vendor_name = "test_vendor_name" in
  let test_model_name = "test_model_name" in
  let __context = make_test_database () in
  let k100_ref_1 = find_or_create ~__context k100 in
  let k100_ref_2 = find_or_create ~__context
      {k100 with vendor_name = test_vendor_name; model_name = test_model_name} in
  (* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
     	 * been created. *)
  assert_equal
    ~msg:"New k100 type was created erroneously"
    k100_ref_1 k100_ref_2;
  (* Make sure the VGPU_type's vendor and model names have been updated. *)
  assert_equal
    ~msg:"k100 vendor_name was not updated"
    test_vendor_name
    (Db.VGPU_type.get_vendor_name ~__context ~self:k100_ref_1);
  assert_equal
    ~msg:"k100 model_name was not updated"
    test_model_name
    (Db.VGPU_type.get_model_name ~__context ~self:k100_ref_1)

let test_vendor_model_lookup () =
  let __context = make_test_database () in
  let k100_ref_1 = find_or_create ~__context k100 in
  (* Set the identifier to the empty string, as if we have upgraded from an old
     	 * version that did not have the identifier field. *)
  Db.VGPU_type.set_identifier ~__context ~self:k100_ref_1 ~value:"";
  let k100_ref_2 = find_or_create ~__context k100 in
  (* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
     	 * been created. *)
  assert_equal
    ~msg:"New k100 type was created erroneously"
    k100_ref_1 k100_ref_2;
  (* Make sure the identifier field has been updated. *)
  assert_equal
    ~msg:"k100 identifier was not updated."
    (Identifier.to_string k100.identifier)
    (Db.VGPU_type.get_identifier ~__context ~self:k100_ref_1)

let test =
  "test_vgpu_type" >:::
  [
    "nvidia_read_whitelist" >::: NvidiaTest.ReadWhitelist.tests;
    "nvidia_print_nv_types" >:: NvidiaTest.print_nv_types;
    "intel_read_whitelist_line" >::: IntelTest.ReadWhitelistLine.tests;
    "intel_read_whitelist" >::: IntelTest.ReadWhitelist.tests;
    "mxgpu_read_whitelist_line" >::: AMDTest.ReadWhitelistLine.tests;
    "mxgpu_read_whitelist" >::: AMDTest.ReadWhitelist.tests;
    "test_find_or_create" >:: test_find_or_create;
    "test_identifier_lookup" >:: test_identifier_lookup;
    "test_vendor_model_lookup" >:: test_vendor_model_lookup;
  ]
