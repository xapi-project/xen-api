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

module ExtractOuConfig = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (string * string) list

    type output_t = (string * string) list * string list

    let string_of_input_t = Test_printers.(assoc_list string string)

    let string_of_output_t =
      Test_printers.(pair (assoc_list string string) (list string))
  end

  let transform x = Extauth_plugin_ADwinbind.extract_ou_config x

  let tests =
    `QuickAndAutoDocumented
      [
        ([("auth-type", "AD"); ("service-name", "conappada.local")], ([], []))
      ; ( [
            ("auth-type", "AD")
          ; ("service-name", "conappada.local")
          ; ("ou", "TOU")
          ]
        , ([("ou", "TOU")], ["createcomputer=TOU"]) )
      ]
end)

let tests =
  make_suite "xapi_cmd_result_" [("extract_ou_config", ExtractOuConfig.tests)]
