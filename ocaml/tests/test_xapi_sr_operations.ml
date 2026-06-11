(*
  Copyright (C) 2026 Vates.
 
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation; version 2.1 only. with the special
  exception on linking described in file LICENSE.
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.
 *)

module Ops = Xapi_sr_operations

let contained_tables =
  [
    (Ops.all_rpu_ops, "all_rpu_ops")
  ; (Ops.disallowed_during_rpu, "disallowed_during_rpu")
  ; (Ops.sm_cap_table |> List.map fst, "sm_cap_table")
  ]

let not_contained element =
  if not (List.mem element Ops.all_ops) then
    Some (Record_util.storage_operations_to_string element)
  else
    None

let test_tables =
  List.map
    (fun (input, name) ->
      let test () =
        let not_contained = List.filter_map not_contained input in
        Alcotest.(check @@ list string)
          "There cannot be operations missing from all_ops" [] not_contained
      in
      (Printf.sprintf {|%s is a subset of all_ops|} name, `Quick, test)
    )
    contained_tables

let () =
  Alcotest.run "Test XAPI Helpers suite"
    [("SR operation tables all are complete ", test_tables)]
