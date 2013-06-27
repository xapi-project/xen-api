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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Storage_interface
open Storage_client

(* Principles:
   1. we don't delete or manipulate VDIs we didn't create
   2. we create VDIs with non-clashing names
   3. we always clean up (as best we can) after every test.
*)

(* For each VDI we check that:
   1. it shows up in a SR.scan
   2. attach RO, activate, deactivate, detach works
   3. attach RW, activate, deactivate, detach works
*)

(* Create a VDI with name "" *)

(* Create a VDI with name ".." *)

(* Create a VDI with name "/" *)

(* Create a VDI with a very long name *)

(* Create two VDIs with the same name *)



let _ = ()
