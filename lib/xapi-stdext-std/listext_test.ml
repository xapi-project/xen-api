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

let test_option typ tested_f (name, case, expected) =
  let check () = Alcotest.(check @@ option typ) name expected (tested_f case) in
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

let test_find_minimum (name, pp, typ, specs) =
  let test ((cmp, cmp_name), input, expected) =
    let name = Printf.sprintf "%s of [%s]" cmp_name (pp input) in
    test_option typ (Listext.find_minimum cmp) (name, input, expected)
  in
  let tests = List.map test specs in
  (Printf.sprintf "find_minimum (%s)" name, tests)

let test_find_minimum_int =
  let ascending = (Int.compare, "ascending") in
  let descending = ((fun a b -> Int.compare b a), "descending") in
  let specs_int =
    ( "int"
    , (fun a -> String.concat "; " (List.map string_of_int a))
    , Alcotest.int
    , [
        (ascending, [], None)
      ; (ascending, [1; 2; 3; 4; 5], Some 1)
      ; (ascending, [2; 3; 1; 5; 4], Some 1)
      ; (descending, [], None)
      ; (descending, [1; 2; 3; 4; 5], Some 5)
      ; (descending, [2; 3; 1; 5; 4], Some 5)
      ]
    )
  in
  test_find_minimum specs_int

let test_find_minimum_tuple =
  let ascending = ((fun (a, _) (b, _) -> Int.compare a b), "ascending") in
  let descending = ((fun (a, _) (b, _) -> Int.compare b a), "descending") in
  let specs_tuple =
    ( "tuple"
    , (fun a ->
        String.concat "; "
          (List.map (fun (a, b) -> "(" ^ string_of_int a ^ ", " ^ b ^ ")") a)
      )
    , Alcotest.(pair int string)
    , [
        (ascending, [(1, "fst"); (1, "snd")], Some (1, "fst"))
      ; (descending, [(1, "fst"); (1, "snd")], Some (1, "fst"))
      ; (ascending, [(1, "fst"); (1, "snd"); (2, "nil")], Some (1, "fst"))
      ; (descending, [(1, "nil"); (2, "fst"); (2, "snd")], Some (2, "fst"))
      ]
    )
  in
  test_find_minimum specs_tuple

let () =
  Alcotest.run "Listext"
    [
      test_iteri_right
    ; test_take
    ; test_drop
    ; test_chop
    ; test_sub
    ; test_find_minimum_int
    ; test_find_minimum_tuple
    ]
