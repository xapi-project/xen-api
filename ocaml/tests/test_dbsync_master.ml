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

open Test_highlevel
open Dbsync_master

module CreateToolsSR = Generic.MakeStateful (struct
  module Io = struct
    type input_t = (string * string * (string * string) list * bool) list

    type output_t = (string * string * (string * string) list) list

    let string_of_input_t =
      Test_printers.(
        list (tuple4 string string (assoc_list string string) bool)
      )

    let string_of_output_t =
      Test_printers.(list (tuple3 string string (assoc_list string string)))
  end

  module State = Test_state.XapiDb

  let name = "Tools"

  let description = "Tools ISOs"

  let other_config =
    [
      (Xapi_globs.xensource_internal, "true")
    ; (Xapi_globs.tools_sr_tag, "true")
    ; (Xapi_globs.i18n_key, "xenserver-tools")
    ; (Xapi_globs.i18n_original_value_prefix ^ "name_label", name)
    ; (Xapi_globs.i18n_original_value_prefix ^ "name_description", description)
    ]

  let load_input __context srs =
    let sr_introduce ~uuid ~name_label ~name_description ~_type ~content_type
        ~shared ~sm_config =
      Test_common.make_sr ~__context ~uuid ~name_label ~name_description ~_type
        ~content_type ~shared ~sm_config ()
    in
    let maybe_create_pbd sR device_config host =
      Test_common.make_pbd ~__context ~sR ~device_config ~host ()
    in
    Test_common.make_localhost ~__context () ;
    List.iter
      (fun (name_label, name_description, other_config, is_tools_sr) ->
        ignore
          (Test_common.make_sr ~__context ~name_label ~name_description
             ~other_config ~is_tools_sr ()
          )
        )
      srs ;
    Dbsync_master.create_tools_sr __context name description sr_introduce
      maybe_create_pbd

  let extract_output __context vms =
    List.fold_left
      (fun acc self ->
        if Db.SR.get_is_tools_sr ~__context ~self then
          ( Db.SR.get_name_label ~__context ~self
          , Db.SR.get_name_description ~__context ~self
          , Db.SR.get_other_config ~__context ~self
          )
          :: acc
        else
          acc
        )
      [] (Db.SR.get_all ~__context)

  (* And other_config key/value pair we use to prove that and existing
   * Tools SR is reused rather than destroyed and recreated. *)
  let extra_oc = ("toolz", "true")

  (* All tests expect the outcome to be one and only one Tools SR with
   * the pre-specified name, description and other_config keys *)
  let tests =
    `QuickAndAutoDocumented
      [
        (* No Tools SR yet *)
        ([], [(name, description, other_config)])
      ; (* An existing Tools SR *)
        ( [("Toolz", "Toolz ISOs", [extra_oc], true)]
        , [(name, description, extra_oc :: other_config)]
        )
      ; (* Two existing Tools SRs (bad state!) *)
        ( [
            ("Toolz", "Toolz ISOs", [extra_oc], true)
          ; ("Toolz2", "Toolz ISOs2", [extra_oc], true)
          ]
        , [(name, description, extra_oc :: other_config)]
        )
      ; (* An existing Tools SR with an old tag *)
        ( [
            ( "Toolz"
            , "Toolz ISOs"
            , [(Xapi_globs.tools_sr_tag, "true"); extra_oc]
            , false
            )
          ]
        , [(name, description, extra_oc :: other_config)]
        )
      ; (* An existing Tools SR with another old tag *)
        ( [
            ( "Toolz"
            , "Toolz ISOs"
            , [(Xapi_globs.xensource_internal, "true"); extra_oc]
            , false
            )
          ]
        , [(name, description, extra_oc :: other_config)]
        )
      ; (* Two existing Tools SRs with different tags; expect to keep the
           one with is_tools_iso=true *)
        ( [
            ("Other", "Other SR", [extra_oc], true)
          ; ( "Toolz"
            , "Toolz ISOs"
            , [(Xapi_globs.xensource_internal, "true")]
            , false
            )
          ]
        , [(name, description, extra_oc :: other_config)]
        )
      ; (* Two existing Tools SRs with different tags; expect to keep the
           one with is_tools_iso=true *)
        ( [
            ("Toolz", "Toolz ISOs", [(Xapi_globs.tools_sr_tag, "true")], false)
          ; ("Other", "Other SR", [extra_oc], true)
          ]
        , [(name, description, extra_oc :: other_config)]
        )
      ; (* Two old SRs - one gets promoted, the other removed *)
        ( [
            ("Toolz", "Toolz ISOs", [(Xapi_globs.tools_sr_tag, "true")], false)
          ; ( "Toolz 2"
            , "Toolz ISOs 2"
            , [(Xapi_globs.tools_sr_tag, "true")]
            , false
            )
          ]
        , [(name, description, other_config)]
        )
      ; (* two new SRs, two old SRs - pick first new, remove the others *)
        ( [
            ("Toolz", "Toolz ISOs", [(Xapi_globs.tools_sr_tag, "true")], true)
          ; ( "Toolz 2"
            , "Toolz ISOs 2"
            , [(Xapi_globs.tools_sr_tag, "true")]
            , true
            )
          ; ( "Toolz 3"
            , "Toolz ISOs 3"
            , [(Xapi_globs.tools_sr_tag, "true")]
            , false
            )
          ; ( "Toolz 4"
            , "Toolz ISOs 4"
            , [(Xapi_globs.tools_sr_tag, "true")]
            , false
            )
          ]
        , [(name, description, other_config)]
        )
      ]
end)

let tests = [("dbsync_master_create_tools_sr", CreateToolsSR.tests)]
