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
(** XC, XS and XAL interface helpers.
 * @group Virtual-Machine Management
 *)

open Pervasiveext

module D = Debug.Debugger(struct let name="xapi" end)
open D

open Xenops_helpers

let with_xc              = with_xc
let with_xs              = with_xs
let with_xc_and_xs       = with_xc_and_xs
let with_xc_and_xs_final = with_xc_and_xs_final


