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
  ]

let () = Alcotest.run "partition_mode" [("partition_mode", tests)]
