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

module XapiCmdResult = Generic.MakeStateless (struct
  module Io = struct
    type input_t = char * string * string

    type output_t = string option

    let string_of_input_t = Test_printers.(tuple3 char string string)

    let string_of_output_t = Test_printers.(option string)
  end

  let transform (sep, key, lines) = Xapi_cmd_result.of_output_opt sep key lines

  let tests =
    `QuickAndAutoDocumented
      [
        ( (':', "Pre-Win2k Domain", "Pre-Win2k Domain: CONNAPP\nsome:other")
        , Some "CONNAPP" )
      ; ((':', "Pre-Win2k Domain", "Not import msg"), None)
      ]
end)

let tests = make_suite "xapi_cmd_result_" [("of_output", XapiCmdResult.tests)]
