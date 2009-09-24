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
(* Coalesce fist points *)

let finding_suitable_pair = "LVHDRT_finding_a_suitable_pair"
let inflating_parent = "LVHDRT_inflating_the_parent"
let resizing_while_paused = "LVHDRT_resizing_while_vdis_are_paused"
let coalescing_data = "LVHDRT_coalescing_VHD_data"
let relinking_granchildren = "LVHDRT_relinking_grandchildren"
let before_create_relink_journal = "LVHDRT_before_create_relink_journal"
let before_inflate_grandparent = "LVHDRT_coalescing_before_inflate_grandparent"

let die = "LVHDRT_exit"
