(*
 * Copyright (C) 2013 Citrix Inc
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

let vhd_util = "vhd-util"

let create ~name ~size ?(reserve=false) ?max_size () =
  let args = [ "-n"; name; "-s"; string_of_int size ] @
    (if reserve then [ "-r" ] else []) @
    (match max_size with None -> [] | Some s -> [ "-S"; string_of_int s ]) in
  let cmdline = vhd_util ^ " " ^ (String.concat " " args) in
  let result = Sys.command cmdline in
  if result <> 0
  then failwith (Printf.sprintf "%s: exitted with %d" cmdline result)
