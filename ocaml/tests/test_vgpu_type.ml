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

open Test_common
open Test_highlevel
open Test_vgpu_common
open Xapi_vgpu_type

let mib x = List.fold_left Int64.mul x [1024L; 1024L]

module NvidiaTest = struct
  let string_of_vgpu_conf conf =
    let open Identifier in
    let open Vendor_nvidia in
    let opt = function
      | Some n ->
          Printf.sprintf "Some %Ld" n
      | None ->
          "None"
    in
    Printf.sprintf
      "%04x %s %04x %04x %Ld %Ld %Ld %Ldx%Ld slices=%s max_vgpus=%s"
      conf.identifier.pdev_id
      ( match conf.identifier.psubdev_id with
      | Some id ->
          Printf.sprintf "Some %04x" id
      | None ->
          "None"
      )
      conf.identifier.vdev_id conf.identifier.vsubdev_id conf.framebufferlength
      conf.num_heads conf.max_instance conf.max_x conf.max_y
      (opt conf.partition_slices)
      (opt conf.partition_max_vgpus)

  (* A type supported by the partitioned test card 3336:4446 *)
  let partitioned_conf ~vsubdev_id ~type_id ~name ~max_instance
      ~partition_slices ~partition_max_vgpus =
    Vendor_nvidia.
      {
        identifier=
          Identifier.
            {
              pdev_id= 0x3336
            ; psubdev_id= Some 0x4446
            ; vdev_id= 0x1113
            ; vsubdev_id
            ; sriov= false
            ; vclass= "Quadro"
            }
      ; framebufferlength= 0x10000000L
      ; num_heads= 4L
      ; max_instance
      ; max_x= 7680L
      ; max_y= 4320L
      ; file_path= "test_data/nvidia-whitelist.xml"
      ; type_id
      ; compatible_model_names_in_vm= []
      ; compatible_model_names_on_pgpu= [name]
      ; partition_slices
      ; partition_max_vgpus
      }

  module ReadWhitelist = Generic.MakeStateless (struct
    module Io = struct
      type input_t = string * int * int (* whitelist * device_id * subsys_id *)

      type output_t = Vendor_nvidia.vgpu_conf list

      let string_of_input_t (whitelist, device_id, subsys_id) =
        Printf.sprintf "(%s, %04x, %04x)" whitelist device_id subsys_id

      let string_of_output_t = Test_printers.list string_of_vgpu_conf
    end

    let transform (whitelist, device_id, subsys_id) =
      let id = Vendor_nvidia.{device_id; subsys_id} in
      Vendor_nvidia.read_whitelist ~whitelist ~id

    let tests =
      `QuickAndAutoDocumented
        [
          (("test_data/this-file-is-not-there.xml", 0x3333, 0x4444), [])
        ; (("test_data/nvidia-whitelist.xml", 0x4444, 0x4444), [])
        ; ( ("test_data/nvidia-whitelist.xml", 0x3333, 0x4444)
          , [
              Vendor_nvidia.
                {
                  identifier=
                    Identifier.
                      {
                        pdev_id= 0x3333
                      ; psubdev_id= Some 0x4444
                      ; vdev_id= 0x1111
                      ; vsubdev_id= 0x2222
                      ; sriov= false
                      ; vclass= "NVS"
                      }
                ; framebufferlength= 0x10000000L
                ; num_heads= 2L
                ; max_instance= 8L
                ; max_x= 1920L
                ; max_y= 1200L
                ; file_path= "test_data/nvidia-whitelist.xml"
                ; type_id= "11"
                ; compatible_model_names_in_vm= []
                ; compatible_model_names_on_pgpu= ["TYPE FOO1"]
                ; partition_slices= None
                ; partition_max_vgpus= None
                }
            ]
          )
        ; ( ("test_data/nvidia-whitelist.xml", 0x3334, 0x0)
          , [
              Vendor_nvidia.
                {
                  identifier=
                    Identifier.
                      {
                        pdev_id= 0x3334
                      ; psubdev_id= None
                      ; vdev_id= 0x1111
                      ; vsubdev_id= 0x2222
                      ; sriov= false
                      ; vclass= "NVS"
                      }
                ; framebufferlength= 0x10000000L
                ; num_heads= 2L
                ; max_instance= 8L
                ; max_x= 1920L
                ; max_y= 1200L
                ; file_path= "test_data/nvidia-whitelist.xml"
                ; type_id= "11"
                ; compatible_model_names_in_vm= []
                ; compatible_model_names_on_pgpu= ["TYPE FOO1"]
                ; partition_slices= None
                ; partition_max_vgpus= None
                }
            ]
          )
        ; ( ("test_data/nvidia-whitelist.xml", 0x3335, 0x4445)
          , [
              Vendor_nvidia.
                {
                  identifier=
                    Identifier.
                      {
                        pdev_id= 0x3335
                      ; psubdev_id= Some 0x4445
                      ; vdev_id= 0x1112
                      ; vsubdev_id= 0x2223
                      ; sriov= false
                      ; vclass= "NVS"
                      }
                ; framebufferlength= 0x20000000L
                ; num_heads= 4L
                ; max_instance= 8L
                ; max_x= 2400L
                ; max_y= 1600L
                ; file_path= "test_data/nvidia-whitelist.xml"
                ; type_id= "20"
                ; compatible_model_names_in_vm= []
                ; compatible_model_names_on_pgpu= ["TYPE FOO2"]
                ; partition_slices= None
                ; partition_max_vgpus= None
                }
            ; Vendor_nvidia.
                {
                  identifier=
                    Identifier.
                      {
                        pdev_id= 0x3335
                      ; psubdev_id= Some 0x4445
                      ; vdev_id= 0x1111
                      ; vsubdev_id= 0x2222
                      ; sriov= false
                      ; vclass= "NVS"
                      }
                ; framebufferlength= 0x10000000L
                ; num_heads= 2L
                ; max_instance= 16L
                ; max_x= 1920L
                ; max_y= 1200L
                ; file_path= "test_data/nvidia-whitelist.xml"
                ; type_id= "21"
                ; compatible_model_names_in_vm= []
                ; compatible_model_names_on_pgpu= ["TYPE FOO3"]
                ; partition_slices= None
                ; partition_max_vgpus= None
                }
            ]
          )
        ; (* The partition fields are read only when they hold a positive
             integer. Every type is still returned, so a malformed value
             neither raises nor hides the card's other types. *)
          ( ("test_data/nvidia-whitelist.xml", 0x3336, 0x4446)
          , [
              (* both given *)
              partitioned_conf ~vsubdev_id:0x2230 ~type_id:"30"
                ~name:"TYPE PART2" ~max_instance:24L ~partition_slices:(Some 2L)
                ~partition_max_vgpus:(Some 3L)
            ; (* neither given: a whole-card type *)
              partitioned_conf ~vsubdev_id:0x2231 ~type_id:"31"
                ~name:"TYPE WHOLE" ~max_instance:4L ~partition_slices:None
                ~partition_max_vgpus:None
            ; (* not a number, and empty *)
              partitioned_conf ~vsubdev_id:0x2232 ~type_id:"32"
                ~name:"TYPE GARBAGE" ~max_instance:4L ~partition_slices:None
                ~partition_max_vgpus:None
            ; (* zero, and negative *)
              partitioned_conf ~vsubdev_id:0x2233 ~type_id:"33"
                ~name:"TYPE ZERO" ~max_instance:4L ~partition_slices:None
                ~partition_max_vgpus:None
            ; (* a self-closing element, and the other field alone *)
              partitioned_conf ~vsubdev_id:0x2234 ~type_id:"34"
                ~name:"TYPE EMPTY" ~max_instance:4L ~partition_slices:None
                ~partition_max_vgpus:(Some 12L)
            ]
          )
        ]
  end)

  module HostDriverMultipleVgpuSupport = Generic.MakeStateless (struct
    module Io = struct
      type input_t = string * string list

      type output_t = bool

      let string_of_input_t (host_driver_version, supported_driver_versions) =
        Printf.sprintf "(%s, %s)" host_driver_version
          (String.concat "," supported_driver_versions)

      let string_of_output_t x = string_of_bool x
    end

    let transform (host_driver_version, supported_versions) =
      Xapi_vgpu_type.Vendor_nvidia.host_driver_supports_multi_vgpu
        ~host_driver_version ~supported_versions

    let tests =
      `QuickAndAutoDocumented
        [
          (("430.19", ["430.19"]), true)
        ; (* Test specific version should match *)
          (("430.19", ["430.89"; "430.19"]), true)
        ; (* Test specific version should match *)
          (("430.19", ["430.19"; "typo_error"]), false)
        ; (* Any typo means we don't support it *)
          (("430.19", ["430.19typo"]), false)
        ; (* Any typo means we don't support it *)
          (("430.19", ["430.18"]), false)
        ; (* Test specific version should not match *)
          (("430.18", ["430.19"]), false)
        ; (* Test specific version should not match *)
          (("430.400", ["430.200+"]), true)
        ; (* Higher version should match *)
          (("430.100", ["430.200+"]), false)
        ; (* Higher version should match *)
          (("100.800", ["430.200+"]), false)
        ; (* Lower version should not match *)
          (("530.400", ["430.200+"; "typo_error"]), false)
          (* Any typo means we don't support it *)
        ]
  end)
end

module IntelTest = struct
  let string_of_vgpu_conf conf =
    let open Identifier in
    let open Vendor_intel in
    Printf.sprintf "%04x %Ld %Ld %Ld %b %s" conf.identifier.pdev_id
      conf.identifier.low_gm_sz conf.identifier.high_gm_sz
      conf.identifier.fence_sz conf.experimental conf.model_name

  module ReadWhitelistLine = Generic.MakeStateless (struct
    module Io = struct
      type input_t = string

      type output_t = Vendor_intel.vgpu_conf option

      let string_of_input_t x = x

      let string_of_output_t = Test_printers.option string_of_vgpu_conf
    end

    let transform line = Vendor_intel.read_whitelist_line ~line

    let tests =
      `QuickAndAutoDocumented
        [
          (* Test some failure cases. *)
          ("", None)
        ; ("nonsense123", None)
        ; (* Test some success cases. *)
          ( "1234 experimental=0 name='myvgpu' low_gm_sz=128 high_gm_sz=384 \
             fence_sz=4 framebuffer_sz=128 max_heads=1 resolution=1920x1080"
          , Some
              {
                Vendor_intel.identifier=
                  Identifier.
                    {
                      pdev_id= 0x1234
                    ; low_gm_sz= 128L
                    ; high_gm_sz= 384L
                    ; fence_sz= 4L
                    }
              ; experimental= false
              ; model_name= "myvgpu"
              ; framebufferlength= mib 128L
              ; num_heads= 1L
              ; max_x= 1920L
              ; max_y= 1080L
              }
          )
        ; ( "1234 experimental=1 name='myvgpu' low_gm_sz=128 high_gm_sz=384 \
             fence_sz=4 framebuffer_sz=128 max_heads=1 resolution=1920x1080"
          , Some
              {
                Vendor_intel.identifier=
                  Identifier.
                    {
                      pdev_id= 0x1234
                    ; low_gm_sz= 128L
                    ; high_gm_sz= 384L
                    ; fence_sz= 4L
                    }
              ; experimental= true
              ; model_name= "myvgpu"
              ; framebufferlength= mib 128L
              ; num_heads= 1L
              ; max_x= 1920L
              ; max_y= 1080L
              }
          )
        ]
  end)

  module ReadWhitelist = Generic.MakeStateless (struct
    module Io = struct
      type input_t = string * int (* whitelist * device_id *)

      type output_t = Vendor_intel.vgpu_conf list

      let string_of_input_t (whitelist, device_id) =
        Printf.sprintf "(%s, %04x)" whitelist device_id

      let string_of_output_t = Test_printers.list string_of_vgpu_conf
    end

    let transform (whitelist, id) =
      Vendor_intel.read_whitelist ~whitelist ~id |> List.rev

    let tests =
      `QuickAndAutoDocumented
        [
          (("test_data/gvt-g-whitelist-empty", 0x1234), [])
        ; (("test_data/gvt-g-whitelist-missing", 0x1234), [])
        ; ( ("test_data/gvt-g-whitelist-1234", 0x1234)
          , [
              Vendor_intel.
                {
                  identifier=
                    Identifier.
                      {
                        pdev_id= 0x1234
                      ; low_gm_sz= 128L
                      ; high_gm_sz= 384L
                      ; fence_sz= 4L
                      }
                ; experimental= false
                ; model_name= "GVT-g on 1234"
                ; framebufferlength= mib 128L
                ; num_heads= 1L
                ; max_x= 1920L
                ; max_y= 1080L
                }
            ; Vendor_intel.
                {
                  identifier=
                    Identifier.
                      {
                        pdev_id= 0x1234
                      ; low_gm_sz= 128L
                      ; high_gm_sz= 384L
                      ; fence_sz= 4L
                      }
                ; experimental= true
                ; model_name= "GVT-g on 1234 (experimental)"
                ; framebufferlength= mib 128L
                ; num_heads= 1L
                ; max_x= 1920L
                ; max_y= 1080L
                }
            ]
          )
        ; (("test_data/gvt-g-whitelist-1234", 0x5678), [])
        ; ( ("test_data/gvt-g-whitelist-mixed", 0x1234)
          , [
              Vendor_intel.
                {
                  identifier=
                    Identifier.
                      {
                        pdev_id= 0x1234
                      ; low_gm_sz= 128L
                      ; high_gm_sz= 384L
                      ; fence_sz= 4L
                      }
                ; experimental= false
                ; model_name= "GVT-g on 1234"
                ; framebufferlength= mib 128L
                ; num_heads= 1L
                ; max_x= 1920L
                ; max_y= 1080L
                }
            ]
          )
        ]
  end)
end

module AMDTest = struct
  let string_of_vgpu_conf conf =
    let open Identifier in
    let open Vendor_amd in
    Printf.sprintf "%04x %Ld %b %s %Ld" conf.identifier.pdev_id
      conf.identifier.framebufferbytes conf.experimental conf.model_name
      conf.vgpus_per_pgpu

  module ReadWhitelistLine = Generic.MakeStateless (struct
    module Io = struct
      type input_t = string

      type output_t = Vendor_amd.vgpu_conf option

      let string_of_input_t x = x

      let string_of_output_t = Test_printers.option string_of_vgpu_conf
    end

    let transform line = Vendor_amd.read_whitelist_line ~line

    let tests =
      `QuickAndAutoDocumented
        [
          (* Test some failure cases. *)
          ("", None)
        ; ("nonsense123", None)
        ; (* Test some success cases. *)
          ( "1234 experimental=0 name='mymxgpu' framebuffer_sz=256 \
             vgpus_per_pgpu=5"
          , Some
              {
                Vendor_amd.identifier=
                  Identifier.{pdev_id= 0x1234; framebufferbytes= mib 256L}
              ; experimental= false
              ; model_name= "mymxgpu"
              ; vgpus_per_pgpu= 5L
              }
          )
        ; ( "2345 experimental=1 name='yourmxgpu' framebuffer_sz=512 \
             vgpus_per_pgpu=8"
          , Some
              {
                Vendor_amd.identifier=
                  Identifier.{pdev_id= 0x2345; framebufferbytes= mib 512L}
              ; experimental= true
              ; model_name= "yourmxgpu"
              ; vgpus_per_pgpu= 8L
              }
          )
        ]
  end)

  module ReadWhitelist = Generic.MakeStateless (struct
    module Io = struct
      type input_t = string * int (* whitelist * device_id *)

      type output_t = Vendor_amd.vgpu_conf list

      let string_of_input_t (whitelist, device_id) =
        Printf.sprintf "(%s, %04x)" whitelist device_id

      let string_of_output_t = Test_printers.list string_of_vgpu_conf
    end

    let transform (whitelist, id) =
      Vendor_amd.read_whitelist ~whitelist ~id |> List.rev

    let tests =
      `QuickAndAutoDocumented
        [
          (("test_data/mxgpu-whitelist-empty", 0x1234), [])
        ; (("test_data/mxgpu-whitelist-missing", 0x1234), [])
        ; ( ("test_data/mxgpu-whitelist-1234", 0x1234)
          , [
              Vendor_amd.
                {
                  identifier=
                    Identifier.{pdev_id= 0x1234; framebufferbytes= mib 128L}
                ; experimental= false
                ; model_name= "Small AMD MxGPU on 1234"
                ; vgpus_per_pgpu= 4L
                }
            ; Vendor_amd.
                {
                  identifier=
                    Identifier.{pdev_id= 0x1234; framebufferbytes= mib 256L}
                ; experimental= true
                ; model_name= "Big AMD MxGPU on 1234"
                ; vgpus_per_pgpu= 2L
                }
            ]
          )
        ; (("test_data/mxgpu-whitelist-1234", 0x5678), [])
        ; ( ("test_data/mxgpu-whitelist-mixed", 0x1234)
          , [
              Vendor_amd.
                {
                  identifier=
                    Identifier.{pdev_id= 0x1234; framebufferbytes= mib 128L}
                ; experimental= false
                ; model_name= "Small AMD MxGPU on 1234"
                ; vgpus_per_pgpu= 4L
                }
            ]
          )
        ]
  end)
end

let test_find_or_create () =
  let __context = make_test_database () in
  let k100_ref_1 = find_or_create ~__context k100 in
  (* Check the VGPU type created in the DB has the expected fields. *)
  Alcotest.check Alcotest.int64 "k100 framebuffer_size is incorrect"
    k100.framebuffer_size
    (Db.VGPU_type.get_framebuffer_size ~__context ~self:k100_ref_1) ;
  Alcotest.check Alcotest.int64 "k100 max_heads is incorrect" k100.max_heads
    (Db.VGPU_type.get_max_heads ~__context ~self:k100_ref_1) ;
  Alcotest.check Alcotest.int64 "k100 size is incorrect" k100.size
    (Db.VGPU_type.get_size ~__context ~self:k100_ref_1) ;
  Alcotest.check Alcotest.bool "k100 experimental flag is incorrect"
    k100.experimental
    (Db.VGPU_type.get_experimental ~__context ~self:k100_ref_1) ;
  (* Simulate an update of framebuffer_size, max_heads, size and the
     	 * experimental flag, as if the config file had been updated. *)
  let new_k100 =
    {
      k100 with
      framebuffer_size= Int64.mul k100.framebuffer_size 2L
    ; max_heads= Int64.mul k100.max_heads 2L
    ; size= Int64.mul k100.size 2L
    ; experimental= not k100.experimental
    }
  in
  (* We can ignore the result as it should be the same as the VGPU_type ref
     	 * obtained earlier. *)
  let k100_ref_2 = find_or_create ~__context new_k100 in
  (* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
     	 * been created. *)
  Alcotest.check
    (Alcotest_comparators.ref ())
    "New k100 type was created erroneously" k100_ref_1 k100_ref_2 ;
  (* Make sure the existing VGPU type object in the database
     	 * has been updated. *)
  Alcotest.check Alcotest.int64 "k100 framebuffer_size was not updated"
    new_k100.framebuffer_size
    (Db.VGPU_type.get_framebuffer_size ~__context ~self:k100_ref_1) ;
  Alcotest.check Alcotest.int64 "k100 max_heads was not updated"
    new_k100.max_heads
    (Db.VGPU_type.get_max_heads ~__context ~self:k100_ref_1) ;
  Alcotest.check Alcotest.int64 "k100 size was not updated" new_k100.size
    (Db.VGPU_type.get_size ~__context ~self:k100_ref_1) ;
  Alcotest.check Alcotest.bool "k100 was not marked experimental"
    new_k100.experimental
    (Db.VGPU_type.get_experimental ~__context ~self:k100_ref_1)

let test_identifier_lookup () =
  let test_vendor_name = "test_vendor_name" in
  let test_model_name = "test_model_name" in
  let __context = make_test_database () in
  let k100_ref_1 = find_or_create ~__context k100 in
  let k100_ref_2 =
    find_or_create ~__context
      {k100 with vendor_name= test_vendor_name; model_name= test_model_name}
  in
  (* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
     	 * been created. *)
  Alcotest.check
    (Alcotest_comparators.ref ())
    "New k100 type was created erroneously" k100_ref_1 k100_ref_2 ;
  (* Make sure the VGPU_type's vendor and model names have been updated. *)
  Alcotest.check Alcotest.string "k100 vendor_name was not updated"
    test_vendor_name
    (Db.VGPU_type.get_vendor_name ~__context ~self:k100_ref_1) ;
  Alcotest.check Alcotest.string "k100 model_name was not updated"
    test_model_name
    (Db.VGPU_type.get_model_name ~__context ~self:k100_ref_1)

let test_vendor_model_lookup () =
  let __context = make_test_database () in
  let k100_ref_1 = find_or_create ~__context k100 in
  (* Set the identifier to the empty string, as if we have upgraded from an old
     	 * version that did not have the identifier field. *)
  Db.VGPU_type.set_identifier ~__context ~self:k100_ref_1 ~value:"" ;
  let k100_ref_2 = find_or_create ~__context k100 in
  (* Make sure the new ref is the same as the old ref, i.e. no new VGPU_type has
     	 * been created. *)
  Alcotest.check
    (Alcotest_comparators.ref ())
    "New k100 type was created erroneously" k100_ref_1 k100_ref_2 ;
  (* Make sure the identifier field has been updated. *)
  Alcotest.check Alcotest.string "k100 identifier was not updated."
    (Identifier.to_string k100.identifier)
    (Db.VGPU_type.get_identifier ~__context ~self:k100_ref_1)

(* Every host's sync writes the same row of a type: the last data synced
   wins, even when it is older than the data already stored *)
let test_last_sync_wins () =
  let __context = make_test_database () in
  let newer_k100 =
    {
      k100 with
      max_resolution_x= Int64.mul k100.max_resolution_x 2L
    ; internal_config= [(Xapi_globs.vgpu_type_id, "type_id_newer")]
    ; compatible_model_names_on_pgpu= []
    }
  in
  let k100_ref = find_or_create ~__context newer_k100 in
  Alcotest.check
    (Alcotest_comparators.ref ())
    "New k100 type was created erroneously" k100_ref
    (find_or_create ~__context k100) ;
  Alcotest.check Alcotest.int64 "k100 max_resolution_x is the last sync's"
    k100.max_resolution_x
    (Db.VGPU_type.get_max_resolution_x ~__context ~self:k100_ref) ;
  Alcotest.(check (list (pair string string)))
    "k100 internal_config is the last sync's" k100.internal_config
    (Db.VGPU_type.get_internal_config ~__context ~self:k100_ref) ;
  Alcotest.check
    (Alcotest.list (Alcotest_comparators.ref ()))
    "k100 compatible_types_on_pgpu is the last sync's" [k100_ref]
    (Db.VGPU_type.get_compatible_types_on_pgpu ~__context ~self:k100_ref)

let int64_option = Alcotest.(option int64)

(* Check what the database holds for a type's partition fields, both as
   stored and as read through the accessors *)
let check_partition ~__context ~self ~what ~stored:(slices, max_vgpus)
    ~expected:(expected_slices, expected_max_vgpus) =
  Alcotest.check Alcotest.int64
    (what ^ ": stored partition_slices")
    slices
    (Db.VGPU_type.get_partition_slices ~__context ~self) ;
  Alcotest.check Alcotest.int64
    (what ^ ": stored partition_max_vgpus")
    max_vgpus
    (Db.VGPU_type.get_partition_max_vgpus ~__context ~self) ;
  Alcotest.check int64_option
    (what ^ ": partition_slices")
    expected_slices
    (partition_slices ~__context ~self) ;
  Alcotest.check int64_option
    (what ^ ": partition_max_vgpus")
    expected_max_vgpus
    (partition_max_vgpus ~__context ~self) ;
  Alcotest.check Alcotest.bool (what ^ ": is_mig_backed")
    (Option.is_some expected_slices)
    (is_mig_backed ~__context ~self)

let test_partition_fields_created () =
  let __context = make_test_database () in
  let whole = find_or_create ~__context k100 in
  check_partition ~__context ~self:whole ~what:"whole-card type"
    ~stored:(0L, 0L) ~expected:(None, None) ;
  let partitioned =
    find_or_create ~__context
      {k140q with partition_slices= Some 2L; partition_max_vgpus= Some 3L}
  in
  check_partition ~__context ~self:partitioned ~what:"partition-backed type"
    ~stored:(2L, 3L) ~expected:(Some 2L, Some 3L)

let test_partition_fields_updated () =
  let __context = make_test_database () in
  (* As if the type was first read from a manifest that did not give the
     values, then from one that does, then from one that no longer does *)
  let self = find_or_create ~__context k100 in
  let self' =
    find_or_create ~__context
      {k100 with partition_slices= Some 4L; partition_max_vgpus= Some 6L}
  in
  Alcotest.check
    (Alcotest_comparators.ref ())
    "New k100 type was created erroneously" self self' ;
  check_partition ~__context ~self ~what:"after the manifest gives them"
    ~stored:(4L, 6L) ~expected:(Some 4L, Some 6L) ;
  let (_ : API.ref_VGPU_type) = find_or_create ~__context k100 in
  check_partition ~__context ~self ~what:"after the manifest stops giving them"
    ~stored:(0L, 0L) ~expected:(None, None)

let test_partition_fields_default () =
  let __context = make_test_database () in
  (* A row written by a xapi that does not know the fields has the default 0 *)
  let self = make_vgpu_type ~__context ~implementation:`nvidia () in
  check_partition ~__context ~self ~what:"default" ~stored:(0L, 0L)
    ~expected:(None, None) ;
  (* A value that is not positive is never written, but is still read as
     not given *)
  let self =
    make_vgpu_type ~__context ~implementation:`nvidia ~partition_slices:(-1L)
      ~partition_max_vgpus:(-2L) ()
  in
  check_partition ~__context ~self ~what:"negative" ~stored:(-1L, -2L)
    ~expected:(None, None)

let test =
  [
    ("find_or_create", `Quick, test_find_or_create)
  ; ("identifier_lookup", `Quick, test_identifier_lookup)
  ; ("vendor_model_lookup", `Quick, test_vendor_model_lookup)
  ; ("last_sync_wins", `Quick, test_last_sync_wins)
  ; ("partition_fields_created", `Quick, test_partition_fields_created)
  ; ("partition_fields_updated", `Quick, test_partition_fields_updated)
  ; ("partition_fields_default", `Quick, test_partition_fields_default)
  ]

let tests =
  make_suite "vgpu_type"
    [
      ("_nvidia_read_whitelist", NvidiaTest.ReadWhitelist.tests)
    ; ( "_nvidia_host_driver_support_multiple_vgpu"
      , NvidiaTest.HostDriverMultipleVgpuSupport.tests
      )
    ; ("_intel_read_whitelist_line", IntelTest.ReadWhitelistLine.tests)
    ; ("_intel_read_whitelist", IntelTest.ReadWhitelist.tests)
    ; ("_mxgpu_read_whitelist_line", AMDTest.ReadWhitelistLine.tests)
    ; ("_mxgpu_read_whitelist", AMDTest.ReadWhitelist.tests)
    ]
