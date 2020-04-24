(*
 * Copyright (C) 2011-2013 Citrix Inc
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

let unbuffered_headers () =
  (* A miscalculation in the Cohttp_posix_io module can cause the HTTP
     status line to be parsed ok but the headers mangled. This can be
     surprisingly hard to spot! *)
  let open Cohttp_posix_io.Unbuffered_IO in
  let ic = {
    header_buffer = Some "HTTP/200 OK\r\nHeader1: Val1\r\nHeader2: Val2\r\n\r\n";
    header_buffer_idx = 0;
    (* unused *)
    fd = Unix.stdin;
  } in
  Alcotest.(check (option string)) "header line 1" (Some "HTTP/200 OK") (read_line ic);
  Alcotest.(check (option string)) "header line 2" (Some "Header1: Val1") (read_line ic);
  Alcotest.(check (option string)) "header line 3" (Some "Header2: Val2") (read_line ic)

let tests =
  [
    "unbuffered_headers", `Quick, unbuffered_headers;
  ]
