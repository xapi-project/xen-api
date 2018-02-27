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

module FixCookie = Generic.Make(struct
    module Io = struct
      type input_t = (string * string) list
      type output_t = (string * string) list

      let string_of_input_t = Test_printers.(assoc_list string string)
      let string_of_output_t = Test_printers.(assoc_list string string)
    end

    let transform = Xapi_services.fix_cookie

    let tests = [
      (* These cookies should be unchanged. *)
      [], [];
      ["foo", "bar"], ["foo", "bar"];
      (* Any pairs where the key starts with '$' should be filtered out. *)
      ["$PATH", "baz"], [];
      ["$PATH", "baz"; "foo", "bar"], ["foo", "bar"];
      (* These cookies have got a bit mangled, and should get unmangled. *)
      ["foo=x, bar", "y"],  ["foo", "x"; "bar", "y"];
      ["foo=x,\tbar", "y"], ["foo", "x"; "bar", "y"];
      ["foo=x; bar", "y"],  ["foo", "x"; "bar", "y"];
      ["foo=x;\tbar", "y"], ["foo", "x"; "bar", "y"];
      ["foo", "x, bar=y"],  ["foo", "x"; "bar", "y"];
      ["foo", "x,\tbar=y"], ["foo", "x"; "bar", "y"];
      ["foo", "x; bar=y"],  ["foo", "x"; "bar", "y"];
      ["foo", "x;\tbar=y"], ["foo", "x"; "bar", "y"];
      (* These cookies need unmangling and filtering. *)
      ["foo=x,\tbar", "y"; "$Stuff", "whatever"], ["foo", "x"; "bar", "y"];
      ["$Stuff", "whatever"; "foo=x,\tbar", "y"], ["foo", "x"; "bar", "y"];
    ]
  end)

let test =
  "test_http" >:::
  [
    "test_fix_cookie" >::: FixCookie.tests;
  ]
