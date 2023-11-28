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

let test_list tested_f (name, case, expected) =
  let check () = Alcotest.(check @@ list int) name expected (tested_f case) in
  (name, `Quick, check)

let test_option tested_f (name, case, expected) =
  let check () = Alcotest.(check @@ option int) name expected (tested_f case) in
  (name, `Quick, check)

let test_chopped_list tested_f (name, case, expected) =
  let check () =
    Alcotest.(check @@ pair (list int) (list int)) name expected (tested_f case)
  in
  (name, `Quick, check)

let test_error tested_f (name, case, expected) =
  let check () = Alcotest.check_raises name expected (tested_f case) in
  (name, `Quick, check)

let test_iteri_right =
  let specs =
    [
      ([], [])
    ; ([0], [(0, 0)])
    ; ([2; 4], [(0, 4); (1, 2)])
    ; ([2; 4; 8], [(0, 8); (1, 4); (2, 2)])
    ]
  in
  let test (list, expected) =
    let name =
      Printf.sprintf "iteri over from [%s]"
        (String.concat "; " (List.map string_of_int list))
    in
    let accum = ref [] in
    let tested_f = Listext.iteri_right (fun i x -> accum := (i, x) :: !accum) in
    let check () =
      tested_f list ;
      (* reverse the list so the lists in the specs reflect the order of
         processing *)
      let result = List.rev !accum in
      Alcotest.(check @@ list @@ pair int int) name expected result
    in
    (name, `Quick, check)
  in
  let tests = List.map test specs in
  ("iteri_right", tests)

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

let test_chop =
  let specs =
    [
      ([], 0, ([], []))
    ; ([0], 0, ([], [0]))
    ; ([0], 1, ([0], []))
    ; ([0; 1], 0, ([], [0; 1]))
    ; ([0; 1], 1, ([0], [1]))
    ; ([0; 1], 2, ([0; 1], []))
    ]
  in
  let error_specs =
    [
      ([0], -1, Invalid_argument "chop: index cannot be negative")
    ; ([0], 2, Invalid_argument "chop: index not in list")
    ]
  in
  let test (whole, number, expected) =
    let name =
      Printf.sprintf "chop [%s] with %i"
        (String.concat "; " (List.map string_of_int whole))
        number
    in
    test_chopped_list (Listext.chop number) (name, whole, expected)
  in
  let tests = List.map test specs in
  let error_test (whole, number, error) =
    let name =
      Printf.sprintf "chop [%s] with %i fails"
        (String.concat "; " (List.map string_of_int whole))
        number
    in
    test_error
      (fun ls () -> ignore (Listext.chop number ls))
      (name, whole, error)
  in
  let error_tests = List.map error_test error_specs in
  ("chop", tests @ error_tests)

let test_sub =
  let specs =
    [
      ([], 0, 0, [])
    ; ([], 0, 1, [])
    ; ([0], 0, 0, [])
    ; ([0], 0, 1, [0])
    ; ([0], 1, 1, [])
    ; ([0], 0, 2, [0])
    ; ([0; 1], 0, 0, [])
    ; ([0; 1], 0, 1, [0])
    ; ([0; 1], 0, 2, [0; 1])
    ; ([0; 1], 1, 1, [])
    ; ([0; 1], 1, 2, [1])
    ; ([0; 1], 2, 2, [])
      (* test_cases below used to fail *) [@ocamlformat "disable"]
    ; ([0], -1, 0, [])
    ; ([0], 0, -1, [])
    ; ([0; 1], 1, 0, [])
    ]
  in
  let test (whole, from, until, expected) =
    let name =
      Printf.sprintf "sub [%s] from %i to %i"
        (String.concat "; " (List.map string_of_int whole))
        from until
    in
    test_list (Listext.sub from until) (name, whole, expected)
  in
  let tests = List.map test specs in
  ("sub", tests)

let () =
  Alcotest.run "Listext"
    [test_iteri_right; test_take; test_drop; test_chop; test_sub]
