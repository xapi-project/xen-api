(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Qmp
open Qmp_protocol

let monitor_events c =
  while true do
    let m = read c in
    Printf.fprintf stderr "%s\n%!" (string_of_message m);
  done

let watch copts =
  let c = connect copts.Common.socket in
  negotiate c;
  monitor_events c

let system_powerdown copts =
  let c = connect copts.Common.socket in
  negotiate c;
  write c (Command(None, System_powerdown));
  monitor_events c

let stop copts =
  let c = connect copts.Common.socket in
  negotiate c;
  write c (Command(None, Stop));
  monitor_events c

let cont copts =
  let c = connect copts.Common.socket in
  negotiate c;
  write c (Command(None, Cont));
  monitor_events c
