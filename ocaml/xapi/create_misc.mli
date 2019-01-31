(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

type host_info = {
  name_label : string;
  xen_verstring : string;
  linux_verstring : string;
  hostname : string;
  uuid : string;
  dom0_uuid : string;
  oem_manufacturer : string option;
  oem_model : string option;
  oem_build_number : string option;
  machine_serial_number : string option;
  machine_serial_name : string option;
  total_memory_mib : int64;
  dom0_static_max : int64;
  ssl_legacy : bool;
  cpu_info : Xenops_interface.Host.cpu_info;
  chipset_info : Xenops_interface.Host.chipset_info;
  hypervisor : Xenops_interface.Host.hypervisor;
}

val read_dom0_memory_usage : unit -> int64 option
val read_localhost_info : __context:Context.t -> host_info

val ensure_domain_zero_records : __context:Context.t -> host:[`host] Ref.t -> host_info -> unit

val create_root_user : __context:Context.t -> unit

val create_software_version : __context:Context.t -> host_info -> unit

val create_host_cpu : __context:Context.t -> host_info -> unit
val create_pool_cpuinfo : __context:Context.t -> unit
val create_chipset_info : __context:Context.t -> host_info -> unit
val create_updates_requiring_reboot_info : __context:Context.t -> host:[`host] Ref.t -> unit
