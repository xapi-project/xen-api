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

open Wslib

let gen_nums n generator = QCheck.Gen.generate ~n generator

let test_split () =
  let helper = Helpers.split "helper" 2 in
  Alcotest.(check @@ pair string string) __LOC__ ("he", "lper") helper

let test_break () =
  let pred = function 'x' -> true | _ -> false in
  let break = Helpers.break pred "helper" in
  Alcotest.(check @@ pair string string) __LOC__ ("helper", "") break ;
  let break = Helpers.break pred "helxper" in
  Alcotest.(check @@ pair string string) __LOC__ ("hel", "xper") break

let test_str_drop_while () =
  let pred = function 'x' -> true | _ -> false in
  let a = Helpers.str_drop_while pred "helper" in
  Alcotest.(check string) __LOC__ "helper" a ;
  let b = Helpers.str_drop_while pred "xhelper" in
  Alcotest.(check string) __LOC__ "helper" b

let test_marshal_unmarshal_int () =
  let generator = QCheck.Gen.ui64 in
  let nums = gen_nums 10 generator in
  List.iter
    (fun i ->
      Alcotest.(check int64)
        __LOC__ i
        (Helpers.unmarshal_int 8 (Helpers.marshal_int 8 i))
    )
    nums

let test_marshal_unmarshal_int8 () =
  let generator = QCheck.Gen.int_bound 255 in
  let nums = gen_nums 10 generator in
  List.iter
    (fun i ->
      Alcotest.(check int)
        __LOC__ i
        (Helpers.unmarshal_int8 (Helpers.marshal_int8 i))
    )
    nums

let test_marshal_unmarshal_int16 () =
  let generator = QCheck.Gen.int_bound 65535 in
  let nums = gen_nums 10 generator in
  List.iter
    (fun i ->
      Alcotest.(check int)
        __LOC__ i
        (Helpers.unmarshal_int16 (Helpers.marshal_int16 i))
    )
    nums

let test_marshal_unmarshal_int32 () =
  let generator = QCheck.Gen.ui32 in
  let nums = gen_nums 10 generator in
  List.iter
    (fun i ->
      Alcotest.(check int32)
        __LOC__ i
        (Helpers.unmarshal_int32 (Helpers.marshal_int32 i))
    )
    nums

let test_marshal_unmarshal_int64 () =
  let generator = QCheck.Gen.ui64 in
  let nums = gen_nums 10 generator in
  List.iter
    (fun i ->
      Alcotest.(check int64)
        __LOC__ i
        (Helpers.unmarshal_int64 (Helpers.marshal_int64 i))
    )
    nums

let test_unmask () =
  let a = Helpers.unmask "01010101" "\x01\x01\x01\x01\x01\x01\x01\x01" in
  Alcotest.(check string) "Unmasks match" "10101010" a

let tests =
  ( "helpers"
  , [
      ("split", `Quick, test_split)
    ; ("break", `Quick, test_break)
    ; ("str_drop_while", `Quick, test_str_drop_while)
    ; ("marshal_unmarshal_int", `Quick, test_marshal_unmarshal_int)
    ; ("marshal_unmarshal_int8", `Quick, test_marshal_unmarshal_int8)
    ; ("marshal_unmarshal_int16", `Quick, test_marshal_unmarshal_int16)
    ; ("marshal_unmarshal_int32", `Quick, test_marshal_unmarshal_int32)
    ; ("marshal_unmarshal_int64", `Quick, test_marshal_unmarshal_int64)
    ; ("unmask", `Quick, test_unmask)
    ]
  )
