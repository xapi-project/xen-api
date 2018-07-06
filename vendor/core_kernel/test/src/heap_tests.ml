open! Core_kernel
open  Expect_test_helpers_kernel

let%expect_test "Heap.sexp_of_t" =
  let test list =
    let heap = Heap.of_list list ~cmp:Int.compare in
    print_s [%sexp (heap : int Heap.t)];
    (* test for stability of element order in sexps, and make sure [sexp_of_t] does not
       accidentally mutate [t] *)
    while not (Heap.is_empty heap) do
      ignore (Heap.pop_exn heap : int);
      print_s [%sexp (heap : int Heap.t)];
    done
  in
  test [];
  [%expect {| () |}];
  test [3];
  [%expect {|
    (3)
    () |}];
  test [3;1;4];
  [%expect {|
    (1 3 4)
    (3 4)
    (4)
    () |}];
  test [3;1;4;1;5;9;2;6;5];
  [%expect {|
    (1 1 2 3 4 5 5 6 9)
    (1 2 3 4 5 5 6 9)
    (2 3 4 5 5 6 9)
    (3 4 5 5 6 9)
    (4 5 5 6 9)
    (5 5 6 9)
    (5 6 9)
    (6 9)
    (9)
    () |}];
;;

let%expect_test "Heap.sexp_of_t with removes" =
  let test list =
    let heap = Heap.create ~cmp:Int.compare () in
    let elts = List.map list ~f:(Heap.add_removable heap) in
    print_s [%sexp (heap : int Heap.t)];
    (* test for stability of element order in sexps, and make sure [sexp_of_t] does not
       accidentally mutate [t] *)
    List.iter elts ~f:(fun elt ->
      Heap.remove heap elt;
      print_s [%sexp (heap : int Heap.t)]);
  in
  test [];
  [%expect {|
    () |}];
  test [3];
  [%expect {|
    (3)
    () |}];
  test [3;1;4];
  [%expect {|
    (1 3 4)
    (1 4)
    (4)
    () |}];
  test [3;1;4;1;5;9;2;6;5];
  [%expect {|
    (1 1 2 3 4 5 5 6 9)
    (1 1 2 4 5 5 6 9)
    (1 2 4 5 5 6 9)
    (1 2 5 5 6 9)
    (2 5 5 6 9)
    (2 5 6 9)
    (2 5 6)
    (5 6)
    (5)
    () |}];
;;
