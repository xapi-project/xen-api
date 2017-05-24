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

module PbisExtractSid = Generic.Make(struct
    module Io = struct
      type input_t = (string * string) list
      type output_t = string list

      let string_of_input_t = Test_printers.(list (pair string string))
      let string_of_output_t = Test_printers.(list string)
    end

    let transform = Extauth_plugin_ADpbis.extract_sid_from_group_list

    let tests = [
      [(" ", " ")], [];

      [("Exception","Remote connection shutdown!")], [];

      [("Number of groups found for user 'testAD@BLE'", "0");
       ("Error", "No record found")],
      [];

      [("Number of groups found for user 'admin@NVC'", "1");
       ("", "Group[1 of 1] name = NVC\\testg(ab) (gid = 564135020, sid = S-1-5-21-1171552557-368733809-2946345504-1132)")],
      ["S-1-5-21-1171552557-368733809-2946345504-1132"];

      [("Number of groups found for user 'cnk3@UN'", "1");
       ("", "Group[1 of 1] name = UN\\KnmOJ (gid = 492513842, sid = S-1-5-31-5921451325-154521381-3135732118-4527)")],
      ["S-1-5-31-5921451325-154521381-3135732118-4527"];

      [("Number of groups found for user 'test@testdomain'", "2");
       ("", "Group[1 of 2] name = testdomain\\dnsadmins (gid = 580912206, sid = S-1-5-21-791009147-1041474540-2433379237-1102)");
       ("", "Group[2 of 2] name = testdomain\\domain+users (gid = 580911617, sid = S-1-5-21-791009147-1041474540-2433379237-513)")],
      ["S-1-5-21-791009147-1041474540-2433379237-1102"; "S-1-5-21-791009147-1041474540-2433379237-513"];
    ]
  end)

let test =
  "test_extauth_ADpbis" >:::
  [
    "test_pbis_extract_sid" >::: PbisExtractSid.tests;
  ]

