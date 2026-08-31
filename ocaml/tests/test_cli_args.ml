(*
 * Copyright (C) 2026 Vates.
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

open Cli_args

let strings = Alcotest.(list string)

let t_of = from_pairs

let check_unused msg expected t = Alcotest.check strings msg expected (unused t)

let test_map_contents_preserves_order_and_dups () =
  let t = t_of [("a", "1"); ("b", "2"); ("a", "3")] in
  Alcotest.check strings "map_contents keeps command-line order and duplicates"
    ["1"; "2"; "3"]
    (List.map snd (map_contents t)) ;
  check_unused "and consumes every entry" [] t

let test_assoc_default_ci () =
  let t = t_of [("IP", "1.2.3.4"); ("x", "1")] in
  Alcotest.check Alcotest.string "case-insensitive hit" "1.2.3.4"
    (assoc_default_ci "ip" t) ;
  Alcotest.check Alcotest.string "absent falls back to empty" ""
    (assoc_default_ci "dns" t) ;
  check_unused "marks only the matched key" ["x"] t

let test_get_marks () =
  let t = t_of [("a", "1"); ("b", "2"); ("c", "3")] in
  check_unused "nothing read yet" ["a"; "b"; "c"] t ;
  Alcotest.check Alcotest.string "get value" "1" (get "a" t) ;
  ignore (get_opt "b" t) ;
  check_unused "a and b now read" ["c"] t

let test_absent_lookup_marks_nothing () =
  let t = t_of [("a", "1")] in
  ignore (get_opt "zzz" t) ;
  Alcotest.check Alcotest.string "get_default falls back" "d"
    (get_default "zzz" t "d") ;
  check_unused "absent lookups mark nothing" ["a"] t

let test_exists_marks () =
  let t = t_of [("force", "true"); ("x", "1")] in
  Alcotest.check Alcotest.bool "exists" true (exists "force" t) ;
  Alcotest.check Alcotest.bool "exists absent" false (exists "nope" t) ;
  check_unused "exists marks the present key only" ["x"] t

let test_get_all_marks_every_occurrence () =
  let t = t_of [("vm-uuid", "a"); ("other", "z"); ("vm-uuid", "b")] in
  Alcotest.check strings "get_all in command-line order" ["a"; "b"]
    (get_all "vm-uuid" t) ;
  check_unused "both occurrences marked" ["other"] t

let test_view_strips_prefix_and_marks_root () =
  let t =
    t_of [("other-config:a", "1"); ("other-config:b", "2"); ("scalar", "s")]
  in
  let oc = view "other-config" t in
  Alcotest.check strings "view keys are prefix-stripped" ["a"; "b"] (keys oc) ;
  ignore (get "a" oc) ;
  check_unused "reading through the view marks the root entry"
    ["other-config:b"; "scalar"]
    t

let test_view_accepts_legacy_dash_separator () =
  let t = t_of [("device-config-target", "/dev/x"); ("z", "1")] in
  let dc = view "device-config" t in
  Alcotest.check
    (Alcotest.option Alcotest.string)
    "dash-separated key is visible through the view" (Some "/dev/x")
    (get_opt "target" dc) ;
  check_unused "and marked on the root" ["z"] t

let test_consume_marks_all_visible () =
  let t = t_of [("a", "1"); ("b", "2")] in
  ignore (consume t) ;
  check_unused "consume marks everything" [] t

let test_consume_on_view_respects_scope () =
  let t = t_of [("m:a", "1"); ("m:b", "2"); ("outside", "x")] in
  ignore (consume (view "m" t)) ;
  check_unused "consume on a view leaves entries outside its scope" ["outside"]
    t

let test_mark_used_without_reading () =
  let t = t_of [("a", "1"); ("b", "2")] in
  mark_used "a" t ;
  mark_used "absent" t ;
  check_unused "mark_used marks a, absent is a no-op" ["b"] t

let test_add_entry_starts_unused () =
  let t = add "b" "2" (t_of [("a", "1")]) in
  check_unused "the added entry is not marked" ["b"; "a"] t

let test_remove_first_occurrence_only () =
  let t = remove "k" (t_of [("k", "1"); ("k", "2"); ("k", "3")]) in
  Alcotest.check strings "remove drops only the first" ["2"; "3"] (get_all "k" t)

let test_rename_key_in_place () =
  let t = t_of [("host-uuid", "H"); ("x", "1")] in
  rename_key ~from_:"host-uuid" ~to_:"host" t ;
  check_unused "the key is renamed, still unread" ["host"; "x"] t ;
  Alcotest.check Alcotest.string "value survives the rename" "H" (get "host" t) ;
  check_unused "reading the renamed key marks it" ["x"] t

let test_rename_key_seen_through_shared_records () =
  let t = t_of [("a", "1"); ("b", "2")] in
  let derived = filter (fun _ -> true) t in
  rename_key ~from_:"a" ~to_:"z" t ;
  Alcotest.check strings "a structure derived before the rename sees it"
    ["z"; "b"] (keys derived) ;
  ignore (get "z" derived) ;
  check_unused "and marking through it reaches the original" ["b"] t

let tests =
  [
    ( "Cli_args"
    , [
        ( "map_contents keeps order and duplicates"
        , `Quick
        , test_map_contents_preserves_order_and_dups
        )
      ; ("assoc_default_ci", `Quick, test_assoc_default_ci)
      ; ("get / get_opt mark", `Quick, test_get_marks)
      ; ("absent lookups mark nothing", `Quick, test_absent_lookup_marks_nothing)
      ; ("exists marks", `Quick, test_exists_marks)
      ; ( "get_all marks every occurrence"
        , `Quick
        , test_get_all_marks_every_occurrence
        )
      ; ( "view strips prefix, marks root"
        , `Quick
        , test_view_strips_prefix_and_marks_root
        )
      ; ( "view accepts legacy dash"
        , `Quick
        , test_view_accepts_legacy_dash_separator
        )
      ; ("consume marks all visible", `Quick, test_consume_marks_all_visible)
      ; ( "consume on view respects scope"
        , `Quick
        , test_consume_on_view_respects_scope
        )
      ; ("mark_used without reading", `Quick, test_mark_used_without_reading)
      ; ("add entry starts unused", `Quick, test_add_entry_starts_unused)
      ; ( "remove first occurrence only"
        , `Quick
        , test_remove_first_occurrence_only
        )
      ; ("rename_key in place", `Quick, test_rename_key_in_place)
      ; ( "rename_key via shared records"
        , `Quick
        , test_rename_key_seen_through_shared_records
        )
      ]
    )
  ]
