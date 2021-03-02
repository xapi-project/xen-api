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
  | Error _ ->
      Alcotest.failf "expected %s to parse correctly" file

let failing file =
  match G.Pem.parse_file file with
  | Ok _ ->
      Alcotest.failf "expected %s to fail parsing" file
  | Error _ ->
      ()

let positive =
  ["pass-01.pem"; "pass-02.pem"; "pass-03.pem"]
  |> List.map (fun pem -> Filename.concat "test_data/pems" pem)
  |> List.map (fun path -> ("positive", `Quick, fun () -> passing path))

let negative =
  ["fail-01.pem"; "fail-02.pem"; "fail-03.pem"; "fail-04.pem"]
  |> List.map (fun pem -> Filename.concat "test_data/pems" pem)
  |> List.map (fun path -> ("negative", `Quick, fun () -> failing path))

let all = List.concat [positive; negative]
