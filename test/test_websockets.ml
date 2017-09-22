(*
 * Copyright (C) Citrix Systems Inc.
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

open OUnit
open Wslib

module TestWsIteratee = Websockets.Wsprotocol(Test.StringMonad)
module I = Iteratees.Iteratee(Test.StringMonad)
open TestWsIteratee
open I

(* Unmasked text message frame *)
let test_strings =
  [ "\x81\x05\x48\x65\x6c\x6c\x6f"; (* contains "Hello" *)
    "\x01\x03\x48\x65\x6c";         (* contains "Hel" *)
    "\x80\x02\x6c\x6f";             (* contains "lo" *)
    "\x82\x7F\x0000000000010000";   (* contains 256 bytes of binary data *)
    "\x82\x7F\x0000000000010000"    (* contains 65536 bytes of binary data *)
  ]
let test_old_str = "\x00Hello\xff\x00There\xff"
(* A single-frame masked text message *)
let test_mask_str = "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58" (* contains "Hello" *)

let test_wsframe () =
  let frame = wsframe (writer Test.StringMonad.strwr "foo") in
  let unframe = wsunframe (writer Test.StringMonad.strwr "bar") in
  test_strings |> List.iter (fun str ->
    (* Test with enum_1chunk *)
    let x = enum_1chunk (Test.StringMonad.getstr (enum_1chunk str frame)) unframe in
    assert_equal str (Test.StringMonad.getstr x);
    (* Test with enum_nchunk *)
    for i = 1 to 10 do
      let z = enum_nchunk (Test.StringMonad.getstr (enum_nchunk str i frame)) i unframe in
      assert_equal str (Test.StringMonad.getstr z)
    done)

let test_wsframe_old () =
  let frame = wsframe_old (writer Test.StringMonad.strwr "foo") in
  let unframe = wsunframe_old (writer Test.StringMonad.strwr "bar") in
  test_strings |> List.iter (fun str ->
    (* Test with enum_1chunk *)
    let x = enum_1chunk (Test.StringMonad.getstr (enum_1chunk str frame)) unframe in
    assert_equal str (Test.StringMonad.getstr x);
    (* Test with enum_nchunk *)
    for i = 1 to 10 do
      let z = enum_nchunk (Test.StringMonad.getstr (enum_nchunk str i frame)) i unframe in
      assert_equal str (Test.StringMonad.getstr z)
    done)

let test_wsunframe () =
  let unframe = wsunframe (writer Test.StringMonad.strwr "foo") in
  (* Test with enum_1chunk *)
  assert_equal "Hello" (Test.StringMonad.getstr (enum_1chunk test_mask_str unframe));
  (* Test with enum_nchunk *)
  for i = 1 to 10 do
    assert_equal "Hello" (Test.StringMonad.getstr (enum_nchunk test_mask_str i unframe))
  done

let test_wsunframe_old () =
  let unframe = wsunframe_old (writer Test.StringMonad.strwr "foo") in
  (* Test with enum_1chunk *)
  assert_equal "HelloThere" (Test.StringMonad.getstr (enum_1chunk test_old_str unframe));
  (* Test with enum_nchunk *)
  for i = 1 to 10 do
    assert_equal "HelloThere" (Test.StringMonad.getstr (enum_nchunk test_old_str i unframe))
  done

let test =
  "test_websockets" >:::
  [
    "test_wsframe" >:: test_wsframe;
    "test_wsunframe" >:: test_wsunframe;
    "test_wsframe_old" >:: test_wsframe_old;
    "test_wsunframe_old" >:: test_wsunframe_old;
  ]
