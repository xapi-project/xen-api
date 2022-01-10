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
module I = Iteratees.Iteratee (Test.StringMonad)
module TestWsIteratee = Websockets.Wsprotocol (Test.StringMonad)
open TestWsIteratee
open I

let get_data = function IE_done x -> Some x | IE_cont _ -> None

let test_heads () =
  let res str =
    match
      get_data (Test.StringMonad.getdata (enum_1chunk "test" (heads str)))
    with
    | Some x ->
        x
    | None ->
        1234
  in
  assert_equal (res "t") 1 ;
  assert_equal (res "te") 2 ;
  assert_equal (res "x") 0

let test_drop () =
  assert_equal
    (Test.StringMonad.getdata (enum_1chunk "test" (drop 1)))
    (IE_done ())

let test_readn () =
  let res i =
    match
      get_data (Test.StringMonad.getdata (enum_1chunk "test" (readn i)))
    with
    | Some x ->
        x
    | None ->
        "xxxxx"
  in
  assert_equal (res 0) "" ;
  assert_equal (res 1) "t" ;
  assert_equal (res 2) "te" ;
  assert_equal (res 4) "test"

let test_read_int8 () =
  let res str =
    match get_data (Test.StringMonad.getdata (enum_1chunk str read_int8)) with
    | Some x ->
        x
    | None ->
        0
  in
  assert_equal 97 (res "a") ;
  assert_equal 65 (res "A") ;
  assert_equal 125 (res "}")

let test_peek () =
  let res str =
    match get_data (Test.StringMonad.getdata (enum_1chunk str peek)) with
    | Some x -> (
      match x with Some c -> c | None -> 'g'
    )
    | None ->
        'g'
  in
  assert_equal 'a' (res "abc") ;
  assert_equal 'x' (res "xyz")

let test_head () =
  let res str =
    match get_data (Test.StringMonad.getdata (enum_1chunk str head)) with
    | Some x -> (
      match x with Some c -> c | None -> 'g'
    )
    | None ->
        'g'
  in
  assert_equal 'a' (res "abc") ;
  assert_equal 'x' (res "xyz")

let test_break () =
  let alter = function '\n' -> true | _ -> false in
  let res str =
    match
      get_data (Test.StringMonad.getdata (enum_1chunk str (break alter)))
    with
    | Some x ->
        x
    | None ->
        "xxxxx"
  in
  assert_equal "" (res "\ntest") ;
  assert_equal "test" (res "test\nabc") ;
  assert_equal "abcxyz" (res "abcxyz\n")

let test =
  "test_iteratees"
  >::: [
         "test_heads" >:: test_heads
       ; "test_drop" >:: test_drop
       ; "test_readn" >:: test_readn
       ; "test_read_int8" >:: test_read_int8
       ; "test_peek" >:: test_peek
       ; "test_head" >:: test_head
       ; "test_break" >:: test_break
       ]
