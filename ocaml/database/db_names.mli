(*
 * Copyright (C) Cloud Software Group
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

val uuid : string

val ref : string

val suspend_VDI : string

val vm : string

val console : string

val name_label : string

val power_state : string

val allowed_operations : string

val current_operations : string

val memory_dynamic_max : string

val memory_dynamic_min : string

val memory_static_max : string

val memory_static_min : string

val memory_target : string

val is_a_template : string

val is_default_template : string

val is_a_snapshot : string

val is_control_domain : string

val platform : string

val other_config : string

val metrics : string

val guest_metrics : string

val parent : string

val snapshot_of : string

val snapshot_time : string

val transportable_snapshot_id : string

val resident_on : string

val scheduled_to_be_resident_on : string

val domid : string

val ha_always_run : string

val host : string

val pool : string

val master : string

val bios_strings : string

val protection_policy : string

val snapshot_schedule : string
