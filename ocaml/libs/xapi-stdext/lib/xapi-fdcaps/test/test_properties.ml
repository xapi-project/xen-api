(*
 * Copyright (C) 2023 Cloud Software Group
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

open Xapi_fdcaps.Properties

(* compilation tests, failed ones are in [properties.t] *)
let () =
  let (_ : (_, _) t) = as_readable (make `rdonly `reg) in
  let (_ : (_, _) t) = as_writable (make `wronly `reg) in
  let (_ : (_, _) t) = as_readable (make `rdwr `reg) in
  let (_ : (_, _) t) = as_writable (make `rdwr `reg) in
  let #espipe = `fifo in
  let #espipe = `sock in
  let #seekable = `reg in
  let #seekable = `blk in
  let #truncatable = `reg in
  ()

(* test that unification works *)
let _any_file = function
  | 0 ->
      make `rdonly `reg
  | 1 ->
      make `rdonly `blk
  | 2 ->
      make `rdonly `chr
  | 3 ->
      make `rdonly `dir
  | 4 ->
      make `rdonly `sock
  | _ ->
      make `rdonly `fifo

let all_rw = [`rdonly; `wronly; `rdwr]

let test_as_rw_opt f expected_set =
  let t = Alcotest.testable pp ( = ) in
  all_rw
  |> List.map @@ fun rw ->
     let test () =
       let prop = make rw `reg in
       let expected = if List.mem rw expected_set then Some prop else None in
       let msg = Fmt.str "as_%a_opt" pp_rw rw in
       Alcotest.(check' @@ option t) ~msg ~expected ~actual:(f prop)
     in
     Alcotest.test_case (Fmt.to_to_string pp_rw rw) `Quick test

let _test_pp prop = Alcotest.test_case (Fmt.to_to_string pp prop) `Quick ignore

let all_kinds = [`reg; `blk; `chr; `dir; `lnk; `sock; `fifo]

let test_to_unix_kind () =
  let all_unix_kinds =
    List.sort_uniq compare @@ all_kinds |> List.map to_unix_kind
  in
  Alcotest.(check' int)
    ~msg:"to_unix_kind mapping is unique" ~expected:(List.length all_kinds)
    ~actual:(List.length all_unix_kinds)

let test_as_kind =
  let t = Alcotest.testable pp ( = ) in
  all_kinds
  |> List.map @@ fun k1 ->
     ( Fmt.str "as_kind_opt %a" pp_kind k1
     , all_kinds
       |> List.map @@ fun k2 ->
          let test () =
            let prop = make `rdonly k2 in
            let actual = as_kind_opt k1 prop in
            let expected = if k1 = k2 then Some prop else None in
            Alcotest.(check' @@ option t) ~msg:"as_kind_opt" ~expected ~actual
          in
          Alcotest.test_case (Fmt.to_to_string pp_kind k2) `Quick test
     )

let tests =
  let open Alcotest in
  ("to_unix_kind", [test_case "to_unix_kind" `Quick test_to_unix_kind])
  :: ("as_readable_opt", test_as_rw_opt as_readable_opt [`rdonly; `rdwr])
  :: ("as_writable_opt", test_as_rw_opt as_writable_opt [`wronly; `rdwr])
  :: test_as_kind

let () = Alcotest.run ~show_errors:true "test_capabilities" tests
