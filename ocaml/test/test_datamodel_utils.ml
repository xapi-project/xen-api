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
open Test_highlevel

module HasBeenRemoved = Generic.Make(struct
    module Io = struct
      type input_t = Datamodel_types.lifecycle_transition list
      type output_t = bool

      let string_of_input_t input =
        List.map
          (fun (lifecycle_change, _, _) ->
             Datamodel_types.rpc_of_lifecycle_change lifecycle_change
             |> Rpc.to_string)
          input
        |> String.concat "; "

      let string_of_output_t = string_of_bool
    end

    let transform = Datamodel_utils.has_been_removed

    let tests = Datamodel_types.([
        [], false;
        [Published, "release1", ""], false;
        [Removed, "release1", ""], true;
        [
          Published, "release1", "";
          Deprecated, "release2", "";
          Removed, "release3", "";
        ], true;
        [
          Published, "release1", "";
          Deprecated, "release2", "";
          Removed, "release3", "";
          Published, "release4", "";
        ], false;
      ])
  end)

let test =
  "datamodel_utils" >:::
  [
    "test_has_been_removed" >::: HasBeenRemoved.tests;
  ]
