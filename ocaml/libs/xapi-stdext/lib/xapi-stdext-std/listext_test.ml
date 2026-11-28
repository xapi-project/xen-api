(* Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module Listext = Xapi_stdext_std.Listext.List

let test_last_list tested_f (name, case, expected) =
  let check () = Alcotest.(check @@ option int) name expected (tested_f case) in
  (name, `Quick, check)

let test_list tested_f (name, case, expected) =
  let check () = Alcotest.(check @@ list int) name expected (tested_f case) in
  (name, `Quick, check)

let test_split_at_list tested_f (name, case, expected) =
  let check () =
    Alcotest.(check @@ pair (list int) (list int)) name expected (tested_f case)
  in
  (name, `Quick, check)

let test_try_map tested_f (name, case, expected) =
  let check () =
    Alcotest.(check @@ result (list int) int) name expected (tested_f case)
  in
  (name, `Quick, check)

let test_take =
  let specs =
    [
      ([], -1, [])
    ; ([], 0, [])
    ; ([], 1, [])
    ; ([1; 2; 3], -1, [])
    ; ([1; 2; 3], 0, [])
    ; ([1; 2; 3], 1, [1])
    ; ([1; 2; 3], 2, [1; 2])
    ; ([1; 2; 3], 3, [1; 2; 3])
    ; ([1; 2; 3], 4, [1; 2; 3])
    ; ([1; 2; 3], 5, [1; 2; 3])
    ]
  in
  let test (whole, number, expected) =
    let name =
      Printf.sprintf "take %i from [%s]" number
        (String.concat "; " (List.map string_of_int whole))
    in
    test_list (Listext.take number) (name, whole, expected)
  in
  let tests = List.map test specs in
  ("take", tests)

let test_drop =
  let specs =
    [
      ([], -1, [])
    ; ([], 0, [])
    ; ([], 1, [])
    ; ([1; 2; 3], -1, [1; 2; 3])
    ; ([1; 2; 3], 0, [1; 2; 3])
    ; ([1; 2; 3], 1, [2; 3])
    ; ([1; 2; 3], 2, [3])
    ; ([1; 2; 3], 3, [])
    ; ([1; 2; 3], 4, [])
    ; ([1; 2; 3], 5, [])
    ]
  in
  let test (whole, number, expected) =
    let name =
      Printf.sprintf "drop %i from [%s]" number
        (String.concat "; " (List.map string_of_int whole))
    in
    test_list (Listext.drop number) (name, whole, expected)
  in
  let tests = List.map test specs in
  ("drop", tests)

let test_last =
  let specs = [([1], 0, Some 1); ([1; 2; 3], 1, Some 3); ([], -1, None)] in
  let test_good (whole, number, expected) =
    let name =
      Printf.sprintf "get last %i from [%s]" number
        (String.concat "; " (List.map string_of_int whole))
    in
    test_last_list Listext.last (name, whole, expected)
  in
  let tests = List.map test_good specs in
  ("last", tests)

let test_split_at =
  let specs =
    [
      ([], 0, ([], []))
    ; ([0], 0, ([], [0]))
    ; ([0], 1, ([0], []))
    ; ([0; 1], 0, ([], [0; 1]))
    ; ([0; 1], 1, ([0], [1]))
    ; ([0; 1], 2, ([0; 1], []))
    (* test invalid arguments *) [@ocamlformat "disable"]
    ; ([0], -1, ([], [0]))
    ; ([0], 2, ([0], []))
    ]
  in
  let test (whole, number, expected) =
    let name =
      Printf.sprintf "split_at [%s] with %i"
        (String.concat "; " (List.map string_of_int whole))
        number
    in
    test_split_at_list (Listext.split_at number) (name, whole, expected)
  in
  let tests = List.map test specs in
  ("split_at", tests)

let test_try_map =
  let only_positive = function i when i >= 0 -> Ok i | i -> Error i in
  let specs =
    [
      ([], Ok [])
    ; ([0; 1], Ok [0; 1])
    ; ([-1], Error (-1))
    ; ([-2; 0], Error (-2))
    ; ([0; -3], Error (-3))
    ; ([-4; -3], Error (-4))
    ]
  in
  let test (lst, expected) =
    let name =
      Printf.sprintf "try_map only_positive [%s]"
        (String.concat "; " (List.map string_of_int lst))
    in
    test_try_map (Listext.try_map only_positive) (name, lst, expected)
  in

  let tests = List.map test specs in
  ("try_map", tests)

let () =
  Alcotest.run "Listext"
    [test_take; test_drop; test_last; test_split_at; test_try_map]
