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

open Lwt

let main filename =
  Disk.of_file filename >|=
  Disk.print_ocaml stdout

let _ =
  if Array.length Sys.argv <> 2 then begin
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr "  %s <disk filename>\n" Sys.argv.(0);
    exit 1;
  end;
  let filename = Sys.argv.(1) in
  Lwt_main.run(main filename)

