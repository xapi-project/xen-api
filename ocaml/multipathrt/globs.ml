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

(* Global values *)

let meg = Int64.mul 1024L 1024L
let two_megs = Int64.mul meg 2L
let four_megs = Int64.mul meg 4L
let four_gigs = Int64.mul four_megs 1024L
let eight_megs = Int64.mul meg 8L
let sixteen_megs = Int64.mul meg 16L
let thirtytwo_megs = Int64.mul meg 32L

let binary_name = "multipathrt"
let helper_plugin = "multipathrt-helper"

let hostname = ref ""
let username = ref "root"
let password = ref ""
let tc = ref 0


