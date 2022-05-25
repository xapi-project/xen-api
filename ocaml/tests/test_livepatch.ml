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

module XenGetRunningLivepatch = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (string * string * string * string) option

    let string_of_input_t s = s

    let string_of_output_t =
      Test_printers.(option (tuple4 string string string string))
  end

  let transform s = Livepatch.XenLivePatch.get_running_livepatch' s

  let tests =
    `QuickAndAutoDocumented
      [
        ( {|
 ID                                     | status
----------------------------------------+------------
lp_4.13.4-10.22.xs8_4.13.4-10.23.xs8    | CHECKED
lp_4.13.4-10.22.xs8_4.13.4-10.23.xs8    | APPLIED
          |}
        , Some ("4.13.4", "10.22.xs8", "4.13.4", "10.23.xs8")
        )
      ; ( {|
 ID                                     | status
----------------------------------------+------------
lp_4.13.4-10.22.xs8_4.13.4-10.23.xs8    | CHECKED
          |}
        , None
        )
      ; ( {|
 ID                                     | status
----------------------------------------+------------
          |}
        , None
        )
      ]
end)

module KernelGetRunningLivepatch = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (string * string * string * string) option

    let string_of_input_t s = s

    let string_of_output_t =
      Test_printers.(option (tuple4 string string string string))
  end

  let transform s = Livepatch.KernelLivePatch.get_running_livepatch' s

  let tests =
    `QuickAndAutoDocumented
      [
        ( {|
Loaded patch modules:
livepatch_4_19_19__8_0_20_xs8__4_19_19__8_0_21_xs8 [enabled]
Installed patch modules:
          |}
        , Some ("4.19.19", "8.0.20.xs8", "4.19.19", "8.0.21.xs8")
        )
      ; ( {|
Loaded patch modules:
livepatch_4_19_19__8_0_20_xs8__4_19_19__8_0_21_xs8 [enabled]
livepatch_4_19_19__8_0_20_xs8__4_19_19__8_0_22_xs8 [enabled]
Installed patch modules:
          |}
        , Some ("4.19.19", "8.0.20.xs8", "4.19.19", "8.0.22.xs8")
        )
      ; ( {|
Loaded patch modules:
livepatch_4_19_19__8_0_20_xs8__4_19_19__8_0_21_xs8 [enabled]
livepatch_4_19_19__8_0_20_xs8__4_19_19__8_0_22_xs8 [enabled]
livepatch_4_19_19__8_0_20_xs8__4_19_19__8_1_21_xs8 [enabled]
Installed patch modules:
          |}
        , Some ("4.19.19", "8.0.20.xs8", "4.19.19", "8.1.21.xs8")
        )
      ; ({|
Loaded patch modules:

Installed patch modules:
          |}, None)
      ]
end)

let tests =
  make_suite "livepatch_"
    [
      ("xen_get_running_livepatch", XenGetRunningLivepatch.tests)
    ; ("kernel_get_running_livepatch", KernelGetRunningLivepatch.tests)
    ]

let () = Alcotest.run "Livepatch" tests
