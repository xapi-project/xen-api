(*
 * Copyright (c) Cloud Software Group, Inc
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

let test_basic () =
  let open Ezxenstore_core.Xenstore in
  let result = with_xs (fun xs -> xs.write "/foo" "bar" ; xs.read "/foo") in
  if result <> "bar" then
    Alcotest.failf "Bad xenstore reply: %S" result

let test_watch_within_timeout () =
  let open Ezxenstore_core in
  Xenstore.with_xs @@ fun xs ->
  let key = "/towatch" in
  xs.rm key ;
  let expected = "valuewritten" in
  let t = Thread.create (fun (xs : Xenstore.xsh) -> xs.write key expected) xs in
  let finally () = Thread.join t in
  Fun.protect ~finally @@ fun () ->
  let watch = Watch.value_to_appear key in
  let actual = Ezxenstore_core.Watch.wait_for ~xs ~timeout:5.0 watch in
  Alcotest.check' Alcotest.string ~expected ~actual
    ~msg:"xenstore value matches"

let test_watch_exceed_timeout () =
  let open Ezxenstore_core in
  Xenstore.with_xs @@ fun xs ->
  let key = "/towatch2" in
  xs.rm key ;
  let expected = "valuewritten" in
  let timeout = 0.3 in
  let t =
    Thread.create
      (fun (xs : Xenstore.xsh) ->
        Unix.sleepf (2. *. timeout) ;
        xs.write key expected
      )
      xs
  in
  let finally () = Thread.join t in
  Fun.protect ~finally @@ fun () ->
  let watch = Watch.value_to_appear key in
  Alcotest.check_raises "timeout" (Ezxenstore_core.Watch.Timeout timeout)
  @@ fun () ->
  let (_actual : string) = Ezxenstore_core.Watch.wait_for ~xs ~timeout watch in
  ()

let tests () =
  [
    ("basic", `Quick, test_basic)
  ; ("watch_within_timeout", `Quick, test_watch_within_timeout)
  ; ("watch_exceed_timeout", `Quick, test_watch_exceed_timeout)
  ]
