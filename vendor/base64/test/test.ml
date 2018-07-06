(*
 * Copyright (c) 2016 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Printf
open Rresult

(* Test vectors from RFC4648
   BASE64("") = ""
   BASE64("f") = "Zg=="
   BASE64("fo") = "Zm8="
   BASE64("foo") = "Zm9v"
   BASE64("foob") = "Zm9vYg=="
   BASE64("fooba") = "Zm9vYmE="
   BASE64("foobar") = "Zm9vYmFy"
*)

let rfc4648_tests = [
  "", "";
  "f", "Zg==";
  "fo", "Zm8=";
  "foo", "Zm9v";
  "foob", "Zm9vYg==";
  "fooba", "Zm9vYmE=";
  "foobar", "Zm9vYmFy";
]

let alphabet_size () =
  List.iter (fun (name,alphabet) ->
    Alcotest.(check int) (sprintf "Alphabet size %s = 64" name)
     64 (String.length alphabet))
     ["default",B64.default_alphabet; "uri_safe",B64.uri_safe_alphabet]

(* Encode using OpenSSL `base64` utility *)
let openssl_encode buf =
  Bos.(OS.Cmd.in_string buf |> OS.Cmd.run_io (Cmd.v "base64") |> OS.Cmd.to_string ~trim:true) |>
  function | Ok r -> prerr_endline r; r | Error (`Msg e) -> raise (Failure (sprintf "OpenSSL decode: %s" e))

(* Encode using this library *)
let lib_encode buf =
  B64.encode ~pad:true buf

let test_rfc4648 () =
  List.iter (fun (c,r) ->
    (* B64 vs openssl *)
    Alcotest.(check string) (sprintf "encode %s" c) (openssl_encode c) (lib_encode c);
    (* B64 vs test cases above *)
    Alcotest.(check string) (sprintf "encode rfc4648 %s" c) r (lib_encode c);
    (* B64 decode vs library *)
    Alcotest.(check string) (sprintf "decode %s" r) c (B64.decode r);
  ) rfc4648_tests

let test_invariants = [ "Alphabet size", `Quick, alphabet_size ]
let test_codec = [ "RFC4648 test vectors", `Quick, test_rfc4648 ]

let () =
  Alcotest.run "Base64" [
    "invariants", test_invariants;
    "codec", test_codec;
  ]

