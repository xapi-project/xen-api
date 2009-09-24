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
type message = 
    (* Guest to Dom0 messages *)
  | CmdResult of string  

    (* Dom0 to guest messages *) 
  | CheckCD of string list  (* Check we've got connected cds *)
  | CheckVIF of string (* Check one device exists *)
  | CheckDisks of string list
  | CheckMountDisks of string list
  | SetupTestDisk of string
  | Shutdown of int
  | Reboot of int
  | Crash
  | Test
  | CheckCDFail of string list

