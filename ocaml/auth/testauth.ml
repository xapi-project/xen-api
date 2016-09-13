(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

let usage() =
  print_endline "Usage:";
  Printf.printf "%s auth <username> <password>\n" Sys.argv.(0);
  Printf.printf "%s chpasswd <username> <new password>\n" Sys.argv.(0);
  exit 1

let _ =
  if Array.length Sys.argv <> 4 then usage ();
  let username = Sys.argv.(2)
  and password = Sys.argv.(3) in
  match Sys.argv.(1) with
  | "auth" ->
    Pam.authenticate username password
  | "chpasswd" ->
    Pam.change_password username password
  | _ -> usage()
