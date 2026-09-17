(*
 * Copyright (C) 2026 Cloud Software Group
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

module LL = Rate_limit_lib.Linked_list

(* Generators *)
let chars =
  let open QCheck in
  list_of_size Gen.(int_range 0 50) char

let ints =
  let open QCheck in
  list_of_size Gen.(int_range 0 50) int

let count = 1000

let test_ll_from_to_list =
  QCheck.Test.make ~name:"LL from_list/to_list roundtrip" chars ~count
  @@ fun chars ->
  let t = LL.from_list chars in
  LL.to_list t = chars

let test_ll_append_drop =
  QCheck.Test.make ~name:"LL append and drop" chars ~count @@ fun chars ->
  let open LL in
  let t = from_list chars in
  let x = node 'x' in
  let y = node 'y' in
  let z = node 'z' in
  List.iter (append t) [x; y; z] ;
  assert (match last t with Some z -> value z = 'z' | None -> false) ;
  List.iter (drop t) [y; z; x] ;
  to_list t = chars

let test_ll_fold =
  QCheck.Test.make ~name:"LL foldl/foldr consistency" ints ~count @@ fun ints ->
  let total = List.fold_left ( + ) 0 ints in
  let open LL in
  let t = from_list ints in
  List.for_all (( = ) total) [foldl ( + ) 0 t; foldr ( + ) t 0]

let test_ll_compare_matches_list_compare =
  QCheck.Test.make ~name:"LL compare matches List.compare"
    QCheck.(pair ints ints)
    ~count
  @@ fun (xs, ys) ->
  let got = LL.compare Int.compare (LL.from_list xs) (LL.from_list ys) in
  let expected = List.compare Int.compare xs ys in
  Int.compare got 0 = Int.compare expected 0

let test_ll_equal_same =
  QCheck.Test.make ~name:"LL equal: same contents" ints ~count @@ fun xs ->
  LL.equal (LL.from_list xs) (LL.from_list xs)

let test_ll_equal_matches_value_equality =
  QCheck.Test.make ~name:"LL equal matches structural equality on values"
    QCheck.(pair ints ints)
    ~count
  @@ fun (xs, ys) -> LL.equal (LL.from_list xs) (LL.from_list ys) = (xs = ys)

(* Wire last.next back to first, manufacturing a cycle.
   Uses Obj because the node type is abstract; field layout is
   { value=0; prev=1; next=2 }. *)
let make_cyclic xs =
  let t = LL.from_list xs in
  ( match (LL.first t, LL.last t) with
  | Some _, Some last ->
      Obj.set_field (Obj.repr last) 2 (Obj.repr (LL.first t))
  | _ ->
      ()
  ) ;
  t

let cyclic_tests =
  [
    Alcotest.test_case "compare terminates on cyclic list" `Quick (fun () ->
        let t1 = make_cyclic [1; 2; 3] in
        let t2 = make_cyclic [1; 2; 3] in
        ignore (LL.compare Int.compare t1 t2)
    )
  ; Alcotest.test_case "equal terminates on cyclic list" `Quick (fun () ->
        let t1 = make_cyclic [1; 2; 3] in
        let t2 = make_cyclic [1; 2; 3] in
        ignore (LL.equal t1 t2)
    )
  ]

let property_tests =
  List.map QCheck_alcotest.to_alcotest
    [
      test_ll_from_to_list
    ; test_ll_append_drop
    ; test_ll_fold
    ; test_ll_compare_matches_list_compare
    ; test_ll_equal_same
    ; test_ll_equal_matches_value_equality
    ]

let () =
  Alcotest.run "Linked list library"
    [("Linked list tests", property_tests); ("Cycle termination", cyclic_tests)]
