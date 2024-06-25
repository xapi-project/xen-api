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
module I = Iteratees.Iteratee (Test.StringMonad)
module TestWsIteratee = Websockets.Wsprotocol (Test.StringMonad)
open I

let get_data = function IE_done x -> Some x | IE_cont _ -> None

let test_heads () =
  let res str =
    get_data (Test.StringMonad.getdata (enum_1chunk "test" (heads str)))
  in
  Alcotest.(check @@ option int) __LOC__ (Some 1) (res "t") ;
  Alcotest.(check @@ option int) __LOC__ (Some 2) (res "te") ;
  Alcotest.(check @@ option int) __LOC__ (Some 0) (res "x")

let test_drop () =
  let res i =
    get_data (Test.StringMonad.getdata (enum_1chunk "test" (drop i)))
  in
  Alcotest.(check @@ option unit) __LOC__ (Some ()) (res 1)

let test_readn () =
  let res i =
    get_data (Test.StringMonad.getdata (enum_1chunk "test" (readn i)))
  in
  Alcotest.(check @@ option string) __LOC__ (Some "") (res 0) ;
  Alcotest.(check @@ option string) __LOC__ (Some "t") (res 1) ;
  Alcotest.(check @@ option string) __LOC__ (Some "te") (res 2) ;
  Alcotest.(check @@ option string) __LOC__ (Some "test") (res 4)

let test_read_int8 () =
  let res str =
    get_data (Test.StringMonad.getdata (enum_1chunk str read_int8))
  in
  Alcotest.(check @@ option int) __LOC__ (Some 97) (res "a") ;
  Alcotest.(check @@ option int) __LOC__ (Some 65) (res "A") ;
  Alcotest.(check @@ option int) __LOC__ (Some 125) (res "}")

let test_peek () =
  let res str = get_data (Test.StringMonad.getdata (enum_1chunk str peek)) in
  Alcotest.(check @@ option @@ option char) __LOC__ (Some (Some 'a')) (res "abc") ;
  Alcotest.(check @@ option @@ option char) __LOC__ (Some (Some 'x')) (res "xyz")

let test_head () =
  let res str = get_data (Test.StringMonad.getdata (enum_1chunk str head)) in
  Alcotest.(check @@ option @@ option char) __LOC__ (Some (Some 'a')) (res "abc") ;
  Alcotest.(check @@ option @@ option char) __LOC__ (Some (Some 'x')) (res "xyz")

let test_break () =
  let alter = function '\n' -> true | _ -> false in
  let res str =
    get_data (Test.StringMonad.getdata (enum_1chunk str (break alter)))
  in
  Alcotest.(check @@ option string) __LOC__ (Some "") (res "\ntest") ;
  Alcotest.(check @@ option string) __LOC__ (Some "test") (res "test\nabc") ;
  Alcotest.(check @@ option string) __LOC__ (Some "abcxyz") (res "abcxyz\n")

let tests =
  ( "iteratees"
  , [
      ("heads", `Quick, test_heads)
    ; ("drop", `Quick, test_drop)
    ; ("readn", `Quick, test_readn)
    ; ("read_int8", `Quick, test_read_int8)
    ; ("peek", `Quick, test_peek)
    ; ("head", `Quick, test_head)
    ; ("break", `Quick, test_break)
    ]
  )
