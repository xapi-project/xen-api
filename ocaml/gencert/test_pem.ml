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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

module G = Gencertlib

let passing file =
  match G.Pem.parse_file file with
  | Ok _ ->
      ()
  | Error e ->
      Alcotest.failf "expected %s to parse correctly: %s" file e

let failing file =
  match G.Pem.parse_file file with
  | Ok _ ->
      Alcotest.failf "expected %s to fail parsing" file
  | Error _ ->
      ()

(* Pems in this folder get tested depending on their names, "pass*" are
   expected to be parsed successfully, "fail*" are not expected to be parsed *)
let data_dir = "test_data/pems"

let ( // ) = Filename.concat

let case_of name test = (name, `Quick, fun () -> test (data_dir // name))

let tests =
  Array.to_seq (Sys.readdir data_dir)
  |> Seq.filter_map (fun name ->
         if String.starts_with ~prefix:"pass" name then
           Some (case_of name passing)
         else if String.starts_with ~prefix:"fail" name then
           Some (case_of name failing)
         else
           Alcotest.failf "Found file in %s with unexpected name: %s" data_dir
             name
     )

let all = List.of_seq tests
