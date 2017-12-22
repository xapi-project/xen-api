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
open Daily_license_check

module Tests = Generic.Make (struct
    module Io = struct
      type input_t = (string * string) list * (string * (string * string) list) list
      type output_t = result
      let string_of_input_t = Test_printers.(pair (assoc_list string string) (assoc_list string (assoc_list string string)))
      let string_of_output_t = function
        | Good -> "Not expired"
        | Expiring hosts -> "Expiring soon on hosts: " ^ (Test_printers.(list string) hosts)
        | Expired hosts -> "Expired on hosts: " ^ (Test_printers.(list string) hosts)
    end

    let now = Xapi_stdext_date.Date.(to_float (of_string "20160601T04:00:00Z"))

    let transform = fun (pool_license_state, all_license_params) ->
      check_license now pool_license_state all_license_params

    let tests = [
      (["expiry", "20170101T00:00:00Z"], []),
      Good;

      (["expiry", "20160701T04:01:00Z"], []),
      Good;

      (["expiry", "20160701T04:00:00Z"], []),
      Expiring [];

      (["expiry", "20160616T00:00:00Z"], []),
      Expiring [];

      (["expiry", "20160601T04:00:01Z"], []),
      Expiring [];

      (["expiry", "20160601T04:00:00Z"], []),
      Expired [];

      (["expiry", "20160101T00:00:00Z"], []),
      Expired [];

      (["expiry", "20160615T00:00:00Z"],
       ["host0", ["expiry", "20160615T00:00:00Z"];
        "host1", ["expiry", "20160615T00:00:00Z"]]),
      Expiring ["host1"; "host0"];

      (["expiry", "20160615T00:00:00Z"],
       ["host0", ["expiry", "20160615T00:00:00Z"];
        "host1", ["expiry", "20160715T00:00:00Z"]]),
      Expiring ["host0"];

      (["expiry", "20160101T00:00:00Z"],
       ["host0", ["expiry", "20160601T00:00:00Z"];
        "host1", ["expiry", "20150601T00:00:00Z"]]),
      Expired ["host1"; "host0"];

      (["expiry", "20160101T00:00:00Z"],
       ["host0", ["expiry", "20170601T00:00:00Z"];
        "host1", ["expiry", "20150601T00:00:00Z"]]),
      Expired ["host1"];
    ]
  end)

let base_suite =
  "base_suite" >:::
  [ "test_daily_license_check" >::: Tests.tests ]

let () =
  Printexc.record_backtrace true;
  ounit2_of_ounit1 base_suite |> OUnit2.run_test_tt_main
