(* Copyright (C) 2025 Vates

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module Zerocheck = Xapi_stdext_zerocheck.Zerocheck

module Str = struct
  let big_non_zero =
    let spec = [("2 GiBs", 2_147_483_647); ("2 GiBs + 1", 2147483647 + 1)] in
    let test size () =
      let non_zeroes = Bytes.make size '\x00' in
      Bytes.set non_zeroes (size - 1) '\x01' ;
      let non_zeroes = Bytes.unsafe_to_string non_zeroes in
      let expected = true in
      let actual = not (Zerocheck.is_all_zeros non_zeroes) in
      Alcotest.(check bool) "The last byte is not zero" expected actual
    in
    List.map (fun (name, size) -> (name, `Quick, test size)) spec

  let big_zero =
    let test () =
      let size = 2147483647 + 1 in
      let zeroes = String.make size '\x00' in
      let expected = true in
      let actual = Zerocheck.is_all_zeros zeroes in
      Alcotest.(check bool) "All bytes are zero" expected actual
    in
    [("2 GiBs + 1", `Quick, test)]

  let tests =
    [("String: Not all zeroes", big_non_zero); ("String: all zeroes", big_zero)]
end

let () = Alcotest.run "Zerocheck" Str.tests
