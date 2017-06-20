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

open Stdext
open Fun
open OUnit
open Test_common
open Test_highlevel

type sm_object = {
  capabilities: string list;
  features: (string * int64) list;
}

type sm_data_sequence = {
  (* Text feature list we get back as part of sr_get_driver_info. *)
  raw: string list;
  (* SMAPIv1 driver info. *)
  smapiv1_features: Smint.feature list;
  (* SMAPIv2 driver info. *)
  smapiv2_features: string list;
  (* SM object created in the database. *)
  sm: sm_object;
}

let string_of_sm_object sm =
  Printf.sprintf "{capabilities = %s; features = %s}"
    (Test_printers.(list string) sm.capabilities)
    (Test_printers.(list string)
       (List.map
          (fun (capability, version) -> Printf.sprintf "%s/%Ld" capability version)
          sm.features))

let test_sequences =
  let open Smint in
  [
    (* Test NFS driver features as of Clearwater. *)
    {
      raw = [
        "SR_PROBE";
        "SR_UPDATE";
        "SR_CACHING"; (* xapi ignores this. *)
        "VDI_CREATE";
        "VDI_DELETE";
        "VDI_ATTACH";
        "VDI_DETACH";
        "VDI_UPDATE";
        "VDI_CLONE";
        "VDI_SNAPSHOT";
        "VDI_RESIZE";
        "VDI_GENERATE_CONFIG";
        "VDI_RESET_ON_BOOT/2";
        "VDI_CONFIG_CBT";
        "ATOMIC_PAUSE"; (* xapi ignores this *)
      ];
      smapiv1_features = [
        Sr_probe, 1L;
        Sr_update, 1L;
        Vdi_create, 1L;
        Vdi_delete, 1L;
        Vdi_attach, 1L;
        Vdi_detach, 1L;
        Vdi_update, 1L;
        Vdi_clone, 1L;
        Vdi_snapshot, 1L;
        Vdi_resize, 1L;
        Vdi_generate_config, 1L;
        Vdi_reset_on_boot, 2L;
        Vdi_configure_cbt, 1L;
      ];
      smapiv2_features = [
        "SR_PROBE/1";
        "SR_UPDATE/1";
        "VDI_CREATE/1";
        "VDI_DELETE/1";
        "VDI_ATTACH/1";
        "VDI_DETACH/1";
        "VDI_UPDATE/1";
        "VDI_CLONE/1";
        "VDI_SNAPSHOT/1";
        "VDI_RESIZE/1";
        "VDI_GENERATE_CONFIG/1";
        "VDI_RESET_ON_BOOT/2";
        "VDI_CONFIG_CBT/1";
      ];
      sm = {
        capabilities = [
          "SR_PROBE";
          "SR_UPDATE";
          "VDI_CREATE";
          "VDI_DELETE";
          "VDI_ATTACH";
          "VDI_DETACH";
          "VDI_UPDATE";
          "VDI_CLONE";
          "VDI_SNAPSHOT";
          "VDI_RESIZE";
          "VDI_GENERATE_CONFIG";
          "VDI_RESET_ON_BOOT";
          "VDI_CONFIG_CBT";
        ];
        features = [
          "SR_PROBE", 1L;
          "SR_UPDATE", 1L;
          "VDI_CREATE", 1L;
          "VDI_DELETE", 1L;
          "VDI_ATTACH", 1L;
          "VDI_DETACH", 1L;
          "VDI_UPDATE", 1L;
          "VDI_CLONE", 1L;
          "VDI_SNAPSHOT", 1L;
          "VDI_RESIZE", 1L;
          "VDI_GENERATE_CONFIG", 1L;
          "VDI_RESET_ON_BOOT", 2L;
          "VDI_CONFIG_CBT", 1L;
        ];
      };
    };
    (* Test that unknown features are discarded. *)
    {
      raw = ["UNKNOWN_FEATURE"; "UNKNOWN_VERSIONED_FEATURE/3"];
      smapiv1_features = [];
      smapiv2_features = [];
      sm = {
        capabilities = [];
        features = [];
      };
    };
    (* Test that versioned features are parsed as expected. *)
    {
      raw = ["SR_PROBE/5"];
      smapiv1_features = [Sr_probe, 5L];
      smapiv2_features = ["SR_PROBE/5"];
      sm = {
        capabilities = ["SR_PROBE"];
        features = ["SR_PROBE", 5L];
      };
    };
    (* Test that unversioned features are implicitly parsed as version 1. *)
    {
      raw = ["VDI_RESIZE"];
      smapiv1_features = [Vdi_resize, 1L];
      smapiv2_features = ["VDI_RESIZE/1"];
      sm = {
        capabilities = ["VDI_RESIZE"];
        features = ["VDI_RESIZE", 1L];
      };
    };
  ]

module ParseSMAPIv1Features = Generic.Make(struct
    module Io = struct
      type input_t = string list
      type output_t = Smint.feature list

      let string_of_input_t = Test_printers.(list string)
      let string_of_output_t = Test_printers.(list Smint.string_of_feature)
    end

    let transform = Smint.parse_capability_int64_features

    let tests =
      List.map
        (fun sequence -> (sequence.raw, sequence.smapiv1_features))
        test_sequences
  end)

module CreateSMAPIv2Features = Generic.Make(struct
    module Io = struct
      type input_t = Smint.feature list
      type output_t = string list

      let string_of_input_t = Test_printers.(list Smint.string_of_feature)
      let string_of_output_t = Test_printers.(list string)
    end

    let transform = List.map Smint.string_of_feature

    let tests =
      List.map
        (fun sequence -> (sequence.smapiv1_features, sequence.smapiv2_features))
        test_sequences
  end)

let test_sm_name_label = "__test_sm"

module CreateSMObject = Generic.Make(Generic.EncapsulateState(struct
                                       module Io = struct
                                         type input_t = string list
                                         type output_t = sm_object

                                         let string_of_input_t = Test_printers.(list string)
                                         let string_of_output_t = string_of_sm_object
                                       end

                                       module State = Test_state.XapiDb

                                       let load_input __context features =
                                         Xapi_sm.create_from_query_result ~__context {
                                           Storage_interface.driver = "";
                                           name = test_sm_name_label;
                                           description = "";
                                           vendor = "";
                                           copyright = "";
                                           version = "";
                                           required_api_version = "";
                                           features = features;
                                           configuration = [];
                                           required_cluster_stack = [];
                                         }

                                       let extract_output __context _ =
                                         let sm =
                                           List.nth (Db.SM.get_by_name_label ~__context ~label:test_sm_name_label) 0
                                         in
                                         {
                                           capabilities = Db.SM.get_capabilities ~__context ~self:sm;
                                           features = Db.SM.get_features ~__context ~self:sm;
                                         }

                                       let tests =
                                         List.map
                                           (fun sequence -> (sequence.smapiv2_features, sequence.sm))
                                           test_sequences
                                     end))

let test =
  "test_sm_features" >:::
  [
    "test_parse_smapiv1_features" >::: ParseSMAPIv1Features.tests;
    "test_create_smapiv2_features" >::: CreateSMAPIv2Features.tests;
    "test_create_sm_object" >::: CreateSMObject.tests;
  ]
