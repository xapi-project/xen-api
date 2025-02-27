(*
 * Copyright (C) Cloud Software Group.
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

module Testlimit = Generic.MakeStateless (struct
  module Io = struct
    type input_t =
      (string * string) list * (int option * int option * int option)

    type output_t = bool * (string * string) list

    let string_of_input_t =
      Test_printers.(
        pair (assoc_list string string)
          (tuple3 (option int) (option int) (option int))
      )

    let string_of_output_t = Test_printers.(pair bool (assoc_list string string))
  end

  let walk_through_mock (limit, acc) (k, v) =
    let open Xenops_server_xen.VM in
    if
      Limit.valid_amount limit
      && Limit.valid_key_length k limit
      && Limit.valid_value_length v limit
    then
      (Limit.decrease_amount limit, (k, v) :: acc)
    else
      (Limit.mark_reached limit, acc)

  let transform input =
    let open Xenops_server_xen.VM in
    let data, (amount, key_len, value_len) = input in
    let limit = Limit.make ~amount ~key_len ~value_len () in
    let limit, result = List.fold_left walk_through_mock (limit, []) data in
    (Limit.is_reached limit, List.rev result)

  let tests =
    (* input: (datasource, (limit_amount, limit_key_len, limit_value_len))
       output: (is_limit_reached, walkthrough result)
    *)
    `QuickAndAutoDocumented
      [
        (* no limit *)
        (([], (None, None, None)), (false, []))
      ; ( ([("k1", "v1"); ("k2", "v2")], (None, None, None))
        , (false, [("k1", "v1"); ("k2", "v2")])
        )
      ; (* limit amount *)
        ( ([("k1", "v1"); ("k2", "v2")], (Some 1, None, None))
        , (true, [("k1", "v1")])
        )
      ; ( ([("k1", "v1"); ("k2", "v2")], (Some 2, None, None))
        , (false, [("k1", "v1"); ("k2", "v2")])
        )
      ; (* limit key string length *)
        ( ([("kk1", "v1"); ("k2", "v2")], (None, Some 2, None))
        , (true, [("k2", "v2")])
        )
      ; (* limit value string length *)
        ( ([("k1", "vv1"); ("k2", "v2")], (None, None, Some 2))
        , (true, [("k2", "v2")])
        )
      ; (* multiple limits are set *)
        ( ( [
              ("kk1", "v1")
            ; ("k2", "v2")
            ; ("k3", "vv3")
            ; ("k4", "v4")
            ; ("k5", "v5")
            ]
          , (Some 2, Some 2, Some 2)
          )
        , (true, [("k2", "v2"); ("k4", "v4")])
        )
      ]
end)

let tests = make_suite "xc_" [("test_limit", Testlimit.tests)]

let () = Alcotest.run "xc" tests
