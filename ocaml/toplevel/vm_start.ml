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
open Toplevelhelper

let _ =
  host := "mindanao";
  port := 8086;
  let s = init_session "root" "xenroot" in
  let vm = List.nth (Remote.VM.get_by_name_label s Sys.argv.(1)) 0 in
  Remote.VM.start s vm false
