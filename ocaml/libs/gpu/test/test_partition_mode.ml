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

open Gpu.Partition_mode

let check_mode msg expected actual =
  Alcotest.(check string) msg (string_of_mode expected) (string_of_mode actual)

let test_pending_is_absolute_not_delta () =
  (* One axis will disable at the next reboot while another will enable: the two
     cancel and the composite is simply enabled, with nothing pending. This is why
     an axis records its absolute after-state rather than "a change is pending". *)
  check_mode "disable_on_reboot + enable_on_reboot -> enabled" Enabled
    (combine Disable_on_reboot Enable_on_reboot)

let test_disabled_distinct_from_not_supported () =
  check_mode "combining disabled axes stays disabled" Disabled
    (combine Disabled Disabled) ;
  check_mode "not_supported is the identity" Disabled
    (combine Not_supported Disabled)

let test_enabled_dominates () =
  check_mode "an enabled axis makes the whole card enabled" Enabled
    (combine Enabled Disabled)

(* There are only five modes, so the laws below are checked exhaustively *)

let all =
  [Not_supported; Disabled; Enable_on_reboot; Enabled; Disable_on_reboot]

let all_axes = List.map axis_of_mode all

(* Adding a mode breaks this match, and test_domain_complete then checks that
   [all] holds it *)
let index = function
  | Not_supported ->
      0
  | Disabled ->
      1
  | Enable_on_reboot ->
      2
  | Enabled ->
      3
  | Disable_on_reboot ->
      4

let test_domain_complete () =
  Alcotest.(check (list int))
    "every mode is checked" [0; 1; 2; 3; 4]
    (List.sort_uniq compare (List.map index all))

let pairs xs = List.concat_map (fun x -> List.map (fun y -> (x, y)) xs) xs

let triples xs =
  List.concat_map (fun (x, y) -> List.map (fun z -> (x, y, z)) xs) (pairs xs)

let string_of_axis a =
  Printf.sprintf "{capable=%b; enabled=%b; after=%b}" a.capable a.enabled
    a.after

let check_axis msg expected actual =
  Alcotest.(check string) msg (string_of_axis expected) (string_of_axis actual)

let for_all_modes name f () =
  List.iter (fun m -> f (Printf.sprintf "%s: %s" name (string_of_mode m)) m) all

let for_all_axes name f () =
  List.iter
    (fun a -> f (Printf.sprintf "%s: %s" name (string_of_axis a)) a)
    all_axes

let for_all_axis_pairs name f () =
  List.iter
    (fun (a, b) ->
      f
        (Printf.sprintf "%s: %s, %s" name (string_of_axis a) (string_of_axis b))
        a b
    )
    (pairs all_axes)

let test_round_trip =
  for_all_modes "round-trip" (fun msg m ->
      check_mode msg m (axis_to_mode (axis_of_mode m))
  )

(* The per-axis combination is a join *)

let test_commutative =
  for_all_axis_pairs "axis order cannot matter" (fun msg a b ->
      check_axis msg (combine_axis a b) (combine_axis b a)
  )

let test_associative () =
  List.iter
    (fun (a, b, c) ->
      check_axis "grouping cannot matter"
        (combine_axis (combine_axis a b) c)
        (combine_axis a (combine_axis b c))
    )
    (triples all_axes)

let test_idempotent =
  for_all_axes "reporting an axis twice is harmless" (fun msg a ->
      check_axis msg a (combine_axis a a)
  )

let test_identity =
  for_all_axes "a missing axis changes nothing" (fun msg a ->
      check_axis msg a (combine_axis a absent) ;
      check_axis msg a (combine_axis absent a)
  )

(* The composite mode can be computed from the per-axis modes alone *)

let test_homomorphism () =
  List.iter
    (fun (a, b) ->
      check_mode
        (Printf.sprintf "to_mode (%s + %s)" (string_of_axis a) (string_of_axis b)
        )
        (axis_to_mode (combine_axis a b))
        (combine (axis_to_mode a) (axis_to_mode b))
    )
    (pairs all_axes)

(* An axis that could not be read *)

let check_observed msg expected actual =
  Alcotest.(check (option string))
    msg
    (Option.map string_of_mode expected)
    (Option.map string_of_mode actual)

let test_unread_poisons () =
  List.iter
    (fun m ->
      List.iter
        (fun axes ->
          check_observed "an unread axis makes the card unread" None
            (of_observations axes)
        )
        [[None]; [Some m; None]; [None; Some m]; [Some m; None; Some m]]
    )
    all

let test_all_read () =
  List.iter
    (fun (m, n) ->
      check_observed "every axis read -> their composite"
        (Some (combine m n))
        (of_observations [Some m; Some n])
    )
    (pairs all) ;
  check_observed "no axes -> not_supported" (Some Not_supported)
    (of_observations [])

let test_to_api () =
  let api = List.map to_api (None :: List.map Option.some all) in
  Alcotest.(check bool) "unread is unknown" true (to_api None = `unknown) ;
  Alcotest.(check int)
    "distinct values stay distinct" 6
    (List.length (List.sort_uniq compare api))

let tests =
  [
    ( "pending state is absolute, not a delta"
    , `Quick
    , test_pending_is_absolute_not_delta
    )
  ; ( "disabled vs not_supported"
    , `Quick
    , test_disabled_distinct_from_not_supported
    )
  ; ("enabled dominates", `Quick, test_enabled_dominates)
  ; ("every mode is checked", `Quick, test_domain_complete)
  ; ("round-trip", `Quick, test_round_trip)
  ; ("join is commutative", `Quick, test_commutative)
  ; ("join is associative", `Quick, test_associative)
  ; ("join is idempotent", `Quick, test_idempotent)
  ; ("absent is the identity", `Quick, test_identity)
  ; ("enum view is a homomorphism", `Quick, test_homomorphism)
  ; ("an unread axis poisons the composite", `Quick, test_unread_poisons)
  ; ("all axes read", `Quick, test_all_read)
  ; ("wire values", `Quick, test_to_api)
  ]

let () = Alcotest.run "partition_mode" [("partition_mode", tests)]
