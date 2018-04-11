(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module Utility = struct

  let assert_invalid_argument = Alcotest_comparators.assert_raises_match
      (function Invalid_argument _ -> true | _ -> false)

  (** Tests the drop function with valid arguments. *)
  let test_drop_valid () =
    List.iter
      (fun (n, xs, xs') ->
         Alcotest.(check (list int))
           "drop function with valid arguments"
           (Vm_placement.drop n xs) xs')
      [
        0, [       ], [       ];
        0, [0      ], [0      ];
        0, [0; 1   ], [0; 1   ];
        0, [0; 1; 2], [0; 1; 2];
        1, [0      ], [       ];
        1, [0; 1   ], [   1   ];
        1, [0; 1; 2], [   1; 2];
        2, [0; 1   ], [       ];
        2, [0; 1; 2], [      2];
        3, [0; 1; 2], [       ];
      ]

  (** Tests the drop function with invalid arguments. *)
  let test_drop_invalid () =
    List.iter
      (fun (n, xs) ->
         assert_invalid_argument (fun () -> ignore (Vm_placement.drop n xs)))
      [
        -1, [       ];
        -1, [0      ];
        -1, [0; 1   ];
        -1, [0; 1; 2];
        1, [       ];
        2, [0      ];
        3, [0; 1   ];
        4, [0; 1; 2];
      ]

  (** Tests the take function with valid arguments. *)
  let test_take_valid () =
    List.iter
      (fun (n, xs, xs') ->
         Alcotest.(check (list int))
           "same lists"
           (Vm_placement.take n xs) xs')
      [
        0, [       ], [       ];
        0, [0      ], [       ];
        0, [0; 1   ], [       ];
        0, [0; 1; 2], [       ];
        1, [0      ], [0;     ];
        1, [0; 1   ], [0;     ];
        1, [0; 1; 2], [0;     ];
        2, [0; 1   ], [0; 1   ];
        2, [0; 1; 2], [0; 1   ];
        3, [0; 1; 2], [0; 1; 2];
      ]

  (** Tests the take function with invalid arguments. *)
  let test_take_invalid () =
    List.iter
      (fun (n, xs) ->
         assert_invalid_argument (fun () -> ignore (Vm_placement.take n xs)))
      [
        -1, [       ];
        -1, [0      ];
        -1, [0; 1   ];
        -1, [0; 1; 2];
        1, [       ];
        2, [0      ];
        3, [0; 1   ];
        4, [0; 1; 2];
      ]

  (** Tests the take_nth function with valid arguments. *)
  let test_take_nth_valid () =
    List.iter
      (fun (n, xs, x, xs') ->
         Alcotest.(check (pair int (list int))) "same lists"
           (Vm_placement.take_nth n xs)
           (x, xs'))
      [
        0, [0      ], 0, [       ];
        0, [0; 1   ], 0, [   1   ];
        0, [0; 1; 2], 0, [   1; 2];
        1, [0; 1   ], 1, [0;     ];
        1, [0; 1; 2], 1, [0;    2];
        2, [0; 1; 2], 2, [0; 1   ];
      ]

  (** Tests the take_nth function with invalid arguments. *)
  let test_take_nth_invalid () =
    List.iter
      (fun (n, xs) ->
         assert_invalid_argument (fun () -> ignore (Vm_placement.take_nth n xs)))
      [
        -1, [       ];
        -1, [0      ];
        -1, [0; 1   ];
        -1, [0; 1; 2];
        1, [       ];
        2, [0      ];
        3, [0; 1   ];
        4, [0; 1; 2];
      ]

  (** Tests the generate_list_index function with valid arguments. *)
  let test_generate_list_index_valid () =
    List.iter
      (fun (v, xs, i) ->
         Alcotest.(check int) "same index" (Vm_placement.generate_list_index (fun () -> v) xs) i)
      [
        0.00, [0         ], 0;
        0.00, [0; 1      ], 0;
        0.49, [0; 1      ], 0;
        0.50, [0; 1      ], 1;
        0.99, [0; 1      ], 1;
        0.00, [0; 1; 2; 3], 0;
        0.24, [0; 1; 2; 3], 0;
        0.25, [0; 1; 2; 3], 1;
        0.49, [0; 1; 2; 3], 1;
        0.50, [0; 1; 2; 3], 2;
        0.74, [0; 1; 2; 3], 2;
        0.75, [0; 1; 2; 3], 3;
        0.99, [0; 1; 2; 3], 3;
      ]

  (** Tests the generate_list_index function with invalid arguments. *)
  let test_generate_list_index_invalid () =
    List.iter
      (fun (v, xs) ->
         assert_invalid_argument
           (fun () ->
              ignore (Vm_placement.generate_list_index (fun () -> v) xs)))
      [
        0.50, [          ];
        -0.01, [0; 1; 2; 3];
        1.00, [0; 1; 2; 3];
      ]

  (** Tests the take_random_element_from_list function with valid arguments. *)
  let test_take_random_element_from_list_valid () =
    List.iter
      (fun (v, xs, x, xs') ->
         Alcotest.(check (pair int (list int))) "correct element taken"
           (x, xs')
           (Vm_placement.take_random_element_from_list (fun () -> v) xs))
      [
        0.00, [0         ], 0, [          ];
        0.00, [0; 1      ], 0, [   1      ];
        0.49, [0; 1      ], 0, [   1      ];
        0.50, [0; 1      ], 1, [0         ];
        0.99, [0; 1      ], 1, [0         ];
        0.00, [0; 1; 2; 3], 0, [   1; 2; 3];
        0.24, [0; 1; 2; 3], 0, [   1; 2; 3];
        0.25, [0; 1; 2; 3], 1, [0;    2; 3];
        0.49, [0; 1; 2; 3], 1, [0;    2; 3];
        0.50, [0; 1; 2; 3], 2, [0; 1;    3];
        0.74, [0; 1; 2; 3], 2, [0; 1;    3];
        0.75, [0; 1; 2; 3], 3, [0; 1; 2;  ];
        0.99, [0; 1; 2; 3], 3, [0; 1; 2;  ];
      ]

  (** Tests the take_random_element_from_list function with invalid arguments. *)
  let test_take_random_element_from_list_invalid () =
    List.iter
      (fun (v, xs) ->
         assert_invalid_argument
           (fun () ->
              ignore (Vm_placement.take_random_element_from_list
                        (fun () -> v) xs)))
      [
        0.50, [          ];
        -0.01, [0; 1; 2; 3];
        1.00, [0; 1; 2; 3];
      ]

  type dummy = N2 | N1 | Z | P1 | P2

  let evaluate_dummy = function
    | N2 -> -2 | N1 -> -1 | Z -> 0 | P1 -> 1 | P2 -> 2

  let dummy_testable =
    let fmt = Fmt.using evaluate_dummy Fmt.int in
    Alcotest.testable fmt (=)

  let test_evaluate_sort_partition () =
    (* Comparator for ascending order. *)
    let forward = compare in
    (* Comparator for descending order. *)
    let reverse = (fun x y -> compare y x) in
    (* Filter for positive values. *)
    let positive = ((<) 0) in
    (* Filter for negative values. *)
    let negative = ((>) 0) in
    List.iter
      (fun (input, sort, partition, out_selected, out_unselected) ->
         Alcotest.(check (pair (list dummy_testable) (list dummy_testable)))
           "same lists"
           (Vm_placement.evaluate_sort_partition
              evaluate_dummy sort partition input)
           (out_selected, out_unselected))
      [
        [             ], forward, positive, [     ], [       ];
        [      Z      ], forward, positive, [     ], [      Z];
        [   P1;Z      ], forward, positive, [P1   ], [      Z];
        [P2;P1;Z      ], forward, positive, [P1;P2], [      Z];
        [      Z;N1   ], forward, positive, [     ], [   N1;Z];
        [      Z;N1;N2], forward, positive, [     ], [N2;N1;Z];
        [P2;P1;Z;N1;N2], forward, positive, [P1;P2], [N2;N1;Z];

        [             ], forward, negative, [     ], [       ];
        [      Z      ], forward, negative, [     ], [Z      ];
        [   P1;Z      ], forward, negative, [     ], [Z;P1   ];
        [P2;P1;Z      ], forward, negative, [     ], [Z;P1;P2];
        [      Z;N1   ], forward, negative, [   N1], [Z      ];
        [      Z;N1;N2], forward, negative, [N2;N1], [Z      ];
        [P2;P1;Z;N1;N2], forward, negative, [N2;N1], [Z;P1;P2];

        [             ], reverse, positive, [     ], [       ];
        [      Z      ], reverse, positive, [     ], [Z      ];
        [   P1;Z      ], reverse, positive, [   P1], [Z      ];
        [P2;P1;Z      ], reverse, positive, [P2;P1], [Z      ];
        [      Z;N1   ], reverse, positive, [     ], [Z;N1   ];
        [      Z;N1;N2], reverse, positive, [     ], [Z;N1;N2];
        [P2;P1;Z;N1;N2], reverse, positive, [P2;P1], [Z;N1;N2];

        [             ], reverse, negative, [     ], [       ];
        [      Z      ], reverse, negative, [     ], [      Z];
        [   P1;Z      ], reverse, negative, [     ], [   P1;Z];
        [P2;P1;Z      ], reverse, negative, [     ], [P2;P1;Z];
        [      Z;N1   ], reverse, negative, [N1   ], [      Z];
        [      Z;N1;N2], reverse, negative, [N1;N2], [Z      ];
        [P2;P1;Z;N1;N2], reverse, negative, [N1;N2], [P2;P1;Z];
      ]

  let tests =
    [ "test_drop_valid", `Quick, test_drop_valid
    ; "test_drop_invalid", `Quick, test_drop_invalid
    ; "test_take_valid", `Quick, test_take_valid
    ; "test_take_invalid", `Quick, test_take_invalid
    ; "test_take_nth_valid", `Quick, test_take_nth_valid
    ; "test_take_nth_invalid", `Quick, test_take_nth_invalid
    ; "test_generate_list_index_valid", `Quick, test_generate_list_index_valid
    ; "test_generate_list_index_invalid", `Quick, test_generate_list_index_invalid
    ; "test_take_random_element_from_list_valid", `Quick, test_take_random_element_from_list_valid
    ; "test_take_random_element_from_list_invalid", `Quick, test_take_random_element_from_list_invalid
    ; "test_evaluate_sort_partition", `Quick, test_evaluate_sort_partition
    ]

end

module Construction = struct

  let guest_snapshot id
      memory_overhead
      memory_static_min
      memory_dynamic_min
      memory_dynamic_max
      memory_static_max
    =
    { Vm_placement.GS.id = id
    ; memory_overhead    = Int64.of_int memory_overhead
    ; memory_static_min  = Int64.of_int memory_static_min
    ; memory_dynamic_min = Int64.of_int memory_dynamic_min
    ; memory_dynamic_max = Int64.of_int memory_dynamic_max
    ; memory_static_max  = Int64.of_int memory_static_max
    }

  let host_snapshot id
      is_pool_master
      guests_resident
      guests_scheduled
      memory_overhead
      memory_total
    =
    { Vm_placement.HS.id = id
    ; is_pool_master   = is_pool_master
    ; guests_resident  = guests_resident
    ; guests_scheduled = guests_scheduled
    ; memory_overhead  = Int64.of_int memory_overhead
    ; memory_total     = Int64.of_int memory_total
    }

  let host_snapshot_summary id
      is_pool_master
      memory_available_sum
      memory_static_min_sum
      memory_dynamic_min_sum
      memory_dynamic_max_sum
      memory_static_max_sum
    =
    { Vm_placement.HSS.id = id
    ; is_pool_master         = is_pool_master
    ; memory_available_sum   = Int64.of_int memory_available_sum
    ; memory_static_min_sum  = Int64.of_int memory_static_min_sum
    ; memory_dynamic_min_sum = Int64.of_int memory_dynamic_min_sum
    ; memory_dynamic_max_sum = Int64.of_int memory_dynamic_max_sum
    ; memory_static_max_sum  = Int64.of_int memory_static_max_sum
    }

end

module Summarisation = struct

  open Construction

  (** Raw input and output data for the summarise_host_snapshot function. *)
  let rec summarise_host_snapshot_input_output_data = [  (*
	(---------------------------------------------------), (-------------)
	(                     INPUT:                        ), (   OUTPUT:   )
	(---------------------------------------------------), (-------------)
	(                 host snapshot                     ), (   host      )
	([-------------],[-------------],[-------------]    ), (   snapshot  )
	([  guests     ],[  guests     ],[  guests     ]    ), (   summary   )
	([  resident   ],[  scheduled  ],[  extra      ]    ), (             )
	([-------------],[-------------],[-------------]    ), (-------------)
	([ xpqrs; xpqrs],[ xpqrs; xpqrs],[ xpqrs; xpqrs],x,t), (A, P, Q, R, S)*)
    ([             ],[             ],[             ],0,0), (0, 0, 0, 0, 0) ;
    ([             ],[             ],[             ],0,8), (8, 0, 0, 0, 0) ;
    ([             ],[             ],[             ],1,8), (7, 0, 0, 0, 0) ;
    ([             ],[             ],[       _11234],1,8), (6, 1, 2, 3, 4) ;
    ([             ],[       _11234],[             ],1,8), (6, 1, 2, 3, 4) ;
    ([       _11234],[             ],[             ],1,8), (6, 1, 2, 3, 4) ;
    ([             ],[             ],[_11234;_11234],1,8), (5, 2, 4, 6, 8) ;
    ([             ],[_11234;_11234],[             ],1,8), (5, 2, 4, 6, 8) ;
    ([_11234;_11234],[             ],[             ],1,8), (5, 2, 4, 6, 8) ;
    ([             ],[       _11234],[       _11234],1,8), (5, 2, 4, 6, 8) ;
    ([       _11234],[             ],[       _11234],1,8), (5, 2, 4, 6, 8) ;
    ([       _11234],[       _11234],[             ],1,8), (5, 2, 4, 6, 8) ;
    ([             ],[       _11234],[_11234;_11234],1,8), (4, 3, 6, 9,12) ;
    ([       _11234],[_11234;_11234],[             ],1,8), (4, 3, 6, 9,12) ;
    ([_11234;_11234],[             ],[       _11234],1,8), (4, 3, 6, 9,12) ;
    ([       _11234],[       _11234],[       _11234],1,8), (4, 3, 6, 9,12) ;
    ([             ],[_11234;_11234],[_11234;_11234],1,8), (3, 4, 8,12,16) ;
    ([_11234;_11234],[_11234;_11234],[             ],1,8), (3, 4, 8,12,16) ;
    ([_11234;_11234],[             ],[_11234;_11234],1,8), (3, 4, 8,12,16) ;
    ([       _11234],[       _11234],[_11234;_11234],1,8), (3, 4, 8,12,16) ;
    ([       _11234],[_11234;_11234],[       _11234],1,8), (3, 4, 8,12,16) ;
    ([_11234;_11234],[       _11234],[       _11234],1,8), (3, 4, 8,12,16) ;
    ([       _11234],[_11234;_11234],[_11234;_11234],1,8), (2, 5,10,15,20) ;
    ([_11234;_11234],[_11234;_11234],[       _11234],1,8), (2, 5,10,15,20) ;
    ([_11234;_11234],[       _11234],[_11234;_11234],1,8), (2, 5,10,15,20) ;
    ([_11234;_11234],[_11234;_11234],[_11234;_11234],1,8), (1, 6,12,18,24) ]
  (*-------------------------+----------------------------*)
  (*  INPUT KEY:             |  OUTPUT KEY:               *)
  (*-------------------------+----------------------------*)
  (*  t = memory_total       |  A = memory_total          *)
  (*  x = memory_overhead    |  - Σ memory_overhead       *)
  (*-------------------------+----------------------------*)
  (*  p = memory_static_min  |  P = Σ memory_static_min   *)
  (*  q = memory_dynamic_min |  Q = Σ memory_dynamic_min  *)
  (*  r = memory_dynamic_max |  R = Σ memory_dynamic_max  *)
  (*  s = memory_static_max  |  S = Σ memory_static_max   *)
  (*-------------------------+----------------------------*)
  and _11234 = (1, 1, 2, 3, 4)

  (** A list of (input, output) for the summarise_host_snapshot function. *)
  let summarise_host_snapshot_input_output_list =
    let make_guest (x, p, q, r, s) =
      guest_snapshot "" x p q r s in
    let make_guests guests =
      List.map make_guest guests in
    let make_input (resident, scheduled, extra, x, t) =
      (host_snapshot "" false
         (make_guests resident)
         (make_guests scheduled) x t)
      ,
      (make_guests extra) in
    let make_output (a, p, q, r, s) =
      host_snapshot_summary "" false a p q r s in
    List.map
      (fun (input, output) -> (make_input input, make_output output))
      summarise_host_snapshot_input_output_data

  let test_summarise_host_snapshot () =
    List.iter
      (fun ((host_snapshot, extra_guests), host_snapshot_summary) ->
         Alcotest.check
           (Alcotest_comparators.only_compare ())
           "same host snapshot summary"
           (Vm_placement.summarise_host_snapshot extra_guests host_snapshot)
           (host_snapshot_summary))
      summarise_host_snapshot_input_output_list

  (** Tests relating to pool, host and guest snapshot summarisation. *)
  let tests =
      [ "test_summarise_host_snapshot", `Quick, test_summarise_host_snapshot ]

end

module Categorisation = struct

  open Construction

  let mock_slave  = host_snapshot_summary "id" false
  let mock_master = host_snapshot_summary "id" true

  (* Wildcard value to aid readability. *)
  let x = 0

  (** Tests the definite_host_category function with slaves. *)
  let test_definite_host_category_slave () =
    List.iter
      (fun ((a, s_min, d_min, d_max, s_max), expected_result) ->
         Alcotest.(check int64) "same ints"
           (Int64.of_int expected_result)
           (Vm_placement.definite_host_category
              (mock_slave a s_min d_min d_max s_max)))
      [
        (* Varying these parameters SHOULD vary the result: *)
        (* Σ available, Σ static_max                        *)
        (0, x, x, x, 0),  0;
        (0, x, x, x, 1), -1;
        (0, x, x, x, 4), -4;
        (1, x, x, x, 0),  1;
        (1, x, x, x, 1),  0;
        (1, x, x, x, 4), -3;
        (4, x, x, x, 0),  4;
        (4, x, x, x, 1),  3;
        (4, x, x, x, 4),  0;

        (* Varying these parameters should NOT vary the result: *)
        (* Σ static_min, Σ dynamic_min, Σ dynamic_max           *)
        (x, 0, 0, 0, x),  0;
        (x, 1, 0, 0, x),  0;
        (x, 0, 1, 0, x),  0;
        (x, 0, 0, 1, x),  0;
      ]

  (** Tests the definite_host_category function with masters. *)
  let test_definite_host_category_master () =
    List.iter
      (fun ((a, s_min, d_min, d_max, s_max), expected_result) ->
         Alcotest.(check int64) "same ints"
           (Int64.of_int expected_result)
           (Vm_placement.definite_host_category
              (mock_master a s_min d_min d_max s_max)))
      [
        (* Varying these parameters SHOULD vary the result: *)
        (* Σ available, Σ static_max                        *)
        (0, x, x, x, 0),  0;
        (0, x, x, x, 1), -1;
        (0, x, x, x, 4), -2;
        (1, x, x, x, 0),  0;
        (1, x, x, x, 1),  0;
        (1, x, x, x, 4), -2;
        (4, x, x, x, 0),  1;
        (4, x, x, x, 1),  1;
        (4, x, x, x, 4),  0;

        (* Varying these parameters should NOT vary the result: *)
        (* Σ static_min, Σ dynamic_min, Σ dynamic_max           *)
        (x, 0, 0, 0, x),  0;
        (x, 1, 0, 0, x),  0;
        (x, 0, 1, 0, x),  0;
        (x, 0, 0, 1, x),  0;
      ]

  (** Tests the probable_host_category function with slaves. *)
  let test_probable_host_category_slave () =
    List.iter
      (fun ((a, s_min, d_min, d_max, s_max), expected_result) ->
         Alcotest.(check int64) "same ints"
           (Int64.of_int expected_result)
           (Vm_placement.probable_host_category
              (mock_slave a s_min d_min d_max s_max)))
      [
        (* Varying these parameters SHOULD vary the result: *)
        (* Σ available, Σ dynamic_max                       *)
        (0, x, x, 0, x),  0;
        (0, x, x, 1, x), -1;
        (0, x, x, 4, x), -4;
        (1, x, x, 0, x),  1;
        (1, x, x, 1, x),  0;
        (1, x, x, 4, x), -3;
        (4, x, x, 0, x),  4;
        (4, x, x, 1, x),  3;
        (4, x, x, 4, x),  0;

        (* Varying these parameters should NOT vary the result: *)
        (* Σ static_min, Σ dynamic_min, Σ static_max            *)
        (x, 0, 0, x, 0),  0;
        (x, 1, 0, x, 0),  0;
        (x, 0, 1, x, 0),  0;
        (x, 0, 0, x, 1),  0;
      ]

  (** Tests the probable_host_category function with masters. *)
  let test_probable_host_category_master () =
    List.iter
      (fun ((a, s_min, d_min, d_max, s_max), expected_result) ->
         Alcotest.(check int64) "same ints"
           (Int64.of_int expected_result)
           (Vm_placement.probable_host_category
              (mock_master a s_min d_min d_max s_max)))
      [
        (* Varying these parameters SHOULD vary the result: *)
        (* Σ available, Σ dynamic_max                       *)
        (0, x, x, 0, x),  0;
        (0, x, x, 1, x), -1;
        (0, x, x, 4, x), -2;
        (1, x, x, 0, x),  0;
        (1, x, x, 1, x),  0;
        (1, x, x, 4, x), -2;
        (4, x, x, 0, x),  1;
        (4, x, x, 1, x),  1;
        (4, x, x, 4, x),  0;

        (* Varying these parameters should NOT vary the result: *)
        (* Σ static_max, Σ static_min, Σ dynamic_min            *)
        (x, 0, 0, x, 0),  0;
        (x, 1, 0, x, 0),  0;
        (x, 0, 1, x, 0),  0;
        (x, 0, 0, x, 1),  0;
      ]

  let test_compression_host_category (category_fn : Vm_placement.host_category) mock_host =
    let ( ** ), ( // ) = Vm_placement.( ( ** ) , ( // ) ) in
    let ceiling = Vm_placement.compression_ratio_resolution in
    List.iter
      (fun ((a, s_min, d_min, d_max, s_max), expected_result) ->
         Alcotest.(check int64) "same ints"
           (expected_result)
           (category_fn
              (mock_host a s_min d_min d_max s_max)))
      [
        (* Varying these parameters SHOULD vary the result: *)
        (* Σ available, Σ dynamic_mix, Σ dynamic_max        *)

        (* Vary (Σ available) while (Σ dynamic_min = Σ dynamic_max) *)
        (0, x, 1, 1, x), -1L;
        (1, x, 1, 1, x), ceiling;
        (2, x, 1, 1, x), ceiling;

        (* Vary (Σ available) while (Σ dynamic_min ≠ Σ dynamic_max) *)
        (-1, x, 0, 4, x), -1L;
        ( 0, x, 0, 4, x),  0L;
        ( 1, x, 0, 4, x), ceiling ** 1L // 4L;
        ( 2, x, 0, 4, x), ceiling ** 2L // 4L;
        ( 3, x, 0, 4, x), ceiling ** 3L // 4L;
        ( 4, x, 0, 4, x), ceiling;
        ( 5, x, 0, 4, x), ceiling;

        (* Varying these parameters should NOT vary the result: *)
        (* Σ static_min, Σ static_max                           *)
        (x, 0, x, x, 0), ceiling;
        (x, 1, x, x, 0), ceiling;
        (x, 0, x, x, 1), ceiling;
      ]

  let test_possible_host_category () =
    test_compression_host_category Vm_placement.possible_host_category mock_master;
    test_compression_host_category Vm_placement.possible_host_category mock_slave

  let test_affinity_host_category () =
    (* The affinity host category excludes all non-affinity hosts. *)
    let non_matching_category = Vm_placement.affinity_host_category ["??"] in
    Alcotest.(check int64) "same ints" (-1L) (non_matching_category (mock_master 0 0 0 0 0));
    Alcotest.(check int64) "same ints" (-1L) (non_matching_category (mock_slave  0 0 0 0 0));

    (* The affinity-host-category function values affinity hosts *)
    (* identically to the possible-host-category function.       *)
    let matching_category = Vm_placement.affinity_host_category ["id"] in
    test_compression_host_category matching_category mock_master;
    test_compression_host_category matching_category mock_slave

  (** Tests functions relating to host categorisation. *)
  let tests =
    [ "test_definite_host_category_slave", `Quick, test_definite_host_category_slave
    ; "test_definite_host_category_master", `Quick, test_definite_host_category_master
    ; "test_probable_host_category_slave", `Quick, test_probable_host_category_slave
    ; "test_probable_host_category_master", `Quick, test_probable_host_category_master
    ; "test_possible_host_category", `Quick, test_possible_host_category
    ; "test_affinity_host_category", `Quick, test_affinity_host_category
    ]

end

module Selection = struct

  open Construction

  let match_no_hosts = fun host -> -1L
  let match_all_hosts = fun host -> 1L
  let validate_all_hosts = fun host -> true
  let select_first_host () = 0.0

  (** Tests the select_host_from_category function. *)
  let test_select_host_from_category () =
    let check = Alcotest.(check (pair (option string) (list (Alcotest_comparators.only_compare ())))) "same lists" in
    check
      (Vm_placement.select_host_from_category
         match_no_hosts [] validate_all_hosts select_first_host)
      (None, []);
    check
      (Vm_placement.select_host_from_category
         match_all_hosts [] validate_all_hosts select_first_host)
      (None, [])

  (** Tests functions relating to host selection. *)
  let tests =
    [ "test_select_host_from_category", `Quick, test_select_host_from_category ]

end

let test =
  Utility.tests @
  Summarisation.tests @
  Categorisation.tests @
  Selection.tests

