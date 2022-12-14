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
module Lifecycle = Datamodel_types.Lifecycle

module SuccessfulLifecycleCreation = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Lifecycle.transition list

    type output_t = Lifecycle.state

    let string_of_input_t input =
      List.map
        (fun (lifecycle_change, _, _) ->
          Lifecycle.string_of_change lifecycle_change
        )
        input
      |> String.concat "; "

    let string_of_output_t t = Lifecycle.string_of_state t
  end

  let transform changes =
    let life = Lifecycle.from changes in
    life.state

  let tests =
    `QuickAndAutoDocumented
      Lifecycle.
        [
          ([], Lifecycle.Unreleased_s)
        ; ([(Published, "release1", "")], Published_s)
        ; ( [
              (Published, "release1", "")
            ; (Deprecated, "release2", "")
            ; (Removed, "release3", "")
            ]
          , Removed_s
          )
        ]
      
end)

module FailingLifecycleCreation = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Lifecycle.transition list

    type output_t = exn

    let string_of_input_t input =
      List.map
        (fun (lifecycle_change, _, _) ->
          Lifecycle.string_of_change lifecycle_change
        )
        input
      |> String.concat "; "

    let string_of_output_t = ExnHelper.string_of_exn
  end

  let transform changes =
    try
      let _ = Lifecycle.from changes in
      Invalid_argument "Lifecycle.from did not raise an exception"
    with e -> e

  let tests =
    `QuickAndAutoDocumented
      Lifecycle.
        [
          ( [(Removed, "release1", "")]
          , Invalid "Invalid transition Removed from Unreleased_s"
          )
        ; ( [
              (Published, "release1", "")
            ; (Deprecated, "release2", "")
            ; (Removed, "release3", "")
            ; (Published, "release4", "")
            ]
          , Invalid "Invalid transition Published from Removed_s"
          )
        ]
      
end)

let tests =
  [
    ( "Datamodel's lifecycle creation, success"
    , SuccessfulLifecycleCreation.tests
    )
  ; ("Datamodel's lifecycle creation, failures", FailingLifecycleCreation.tests)
  ]
