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

(* A list whose [next] chain is [prefix] followed by [cycle] repeated for
   ever, built with the public interface: appending the first node of
   [cycle] a second time links the last node back to it. *)
let make_lasso prefix cycle =
  let t = LL.create () in
  List.iter (fun x -> LL.append t (LL.node x)) prefix ;
  let nodes = List.map LL.node cycle in
  List.iter (LL.append t) nodes ;
  (match nodes with n :: _ -> LL.append t n | [] -> ()) ;
  t

(* The first [n] values of [prefix] followed by [cycle] repeated *)
let unroll prefix cycle n =
  let rec go acc k rest =
    if k = n then
      List.rev acc
    else
      match rest with
      | x :: rest ->
          go (x :: acc) (k + 1) rest
      | [] ->
          if cycle = [] then
            List.rev acc
          else
            go acc k cycle
  in
  go [] 0 prefix

let sign x = Int.compare x 0

let cyclic_value_tests =
  [
    Alcotest.test_case "cyclic list against a finite one" `Quick (fun () ->
        (* 1, 1, 1, 1, ... is lexicographically before 1, 1, 1, 2 *)
        let a = make_lasso [] [1] and b = LL.from_list [1; 1; 1; 2] in
        Alcotest.(check int)
          "compare a b" (-1)
          (sign (LL.compare Int.compare a b)) ;
        Alcotest.(check int) "compare b a" 1 (sign (LL.compare Int.compare b a)) ;
        Alcotest.(check bool) "equal a b" false (LL.equal a b)
    )
  ; Alcotest.test_case "cyclic lists that differ late" `Quick (fun () ->
        (* 1, 2, 3, 2, 3, ... and 1, 2, 3, 2, 4, 2, 4, ... *)
        let a = make_lasso [1] [2; 3] and b = make_lasso [1; 2; 3] [2; 4] in
        Alcotest.(check int)
          "compare a b" (-1)
          (sign (LL.compare Int.compare a b)) ;
        Alcotest.(check bool) "equal a b" false (LL.equal a b)
    )
  ; Alcotest.test_case "equal cyclic lists of different shapes" `Quick
      (fun () ->
        (* both are 1, 2, 3, 2, 3, ... *)
        let a = make_lasso [1] [2; 3]
        and b = make_lasso [1; 2; 3] [2; 3; 2; 3] in
        Alcotest.(check int) "compare a b" 0 (LL.compare Int.compare a b) ;
        Alcotest.(check bool) "equal a b" true (LL.equal a b)
    )
  ]

let test_ll_compare_lassos =
  let values = QCheck.Gen.int_range 0 2 in
  let lasso =
    QCheck.Gen.(
      pair (list_size (int_range 0 4) values) (list_size (int_range 0 4) values)
    )
  in
  QCheck.Test.make ~name:"LL compare matches List.compare on unrolled lists"
    ~count
    (QCheck.make QCheck.Gen.(pair lasso lasso))
  @@ fun ((p1, c1), (p2, c2)) ->
  (* Two cyclic lists that agree on this many values agree on all of them;
     with a finite list, one more value than it has decides. *)
  let n =
    List.length p1
    + List.length p2
    + List.length c1
    + List.length c2
    + (max 1 (List.length c1) * max 1 (List.length c2))
    + 1
  in
  let expected = List.compare Int.compare (unroll p1 c1 n) (unroll p2 c2 n) in
  sign (LL.compare Int.compare (make_lasso p1 c1) (make_lasso p2 c2))
  = sign expected

let property_tests =
  List.map QCheck_alcotest.to_alcotest
    [
      test_ll_from_to_list
    ; test_ll_append_drop
    ; test_ll_fold
    ; test_ll_compare_matches_list_compare
    ; test_ll_equal_same
    ; test_ll_equal_matches_value_equality
    ; test_ll_compare_lassos
    ]

let () =
  Alcotest.run "Linked list library"
    [
      ("Linked list tests", property_tests)
    ; ("Cycle termination", cyclic_tests)
    ; ("Cyclic values", cyclic_value_tests)
    ]
