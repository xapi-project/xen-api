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
open Test_highlevel
open Features

module OfAssocList = Generic.Make(struct
    module Io = struct
      type input_t = (string * string) list
      type output_t = Features.feature list

      let string_of_input_t = Test_printers.(assoc_list string string)
      let string_of_output_t =
        Test_printers.(fun features -> String.concat "," (List.map name_of_feature features))
    end

    let transform = of_assoc_list

    (* Xen_motion and AD are enabled unless explicitly disabled. All other features
       	   are disabled unless explitly enabled. *)
    let tests = [
      [],
      [Xen_motion; AD];

      ["restrict_xen_motion", "true";
       "restrict_ad", "true"],
      [];

      ["restrict_xen_motion", "true"],
      [AD];

      ["restrict_xen_motion", "false"],
      [Xen_motion; AD];

      ["restrict_xen_motion", "false";
       "restrict_dmc", "false"],
      [DMC; Xen_motion; AD];

      ["restrict_xen_motion", "false";
       "restrict_ad", "true";
       "restrict_dmc", "false"],
      [DMC; Xen_motion];

      ["enable_xha", "true";
       "restrict_xen_motion", "true"],
      [HA; AD];
    ]
  end)


let test =
  "pool_license" >:::
  [
    "test_of_assoc_list" >::: OfAssocList.tests;
  ]
