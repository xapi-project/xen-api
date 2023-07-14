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
(* constants which are global across all the tools *)

let ( // ) = Filename.concat

let get_vm_rrd = "vm_rrd"

let get_vm_rrd_uri = "/" ^ get_vm_rrd

let get_host_rrd = "host_rrd"

let get_host_rrd_uri = "/" ^ get_host_rrd

let get_sr_rrd = "sr_rrd"

let get_sr_rrd_uri = "/" ^ get_sr_rrd

let get_rrd_updates = "rrd_updates"

let get_rrd_updates_uri = "/" ^ get_rrd_updates

let put_rrd = "rrd"

let put_rrd_uri = "/" ^ put_rrd

let rrd_unarchive = "rrd_unarchive"

let rrd_unarchive_uri = "/" ^ rrd_unarchive

(* Path to the pool secret file. *)
let pool_secret_path = Filename.concat "/etc/xensource" "ptoken"

(* Path to the pool configuration file. *)
let pool_config_file = Filename.concat "/etc/xensource" "pool.conf"

(* RRD storage location. *)
let rrd_location = Filename.concat "/var/lib/xcp" "blobs/rrds"

(* Blob storage location. *)
let blob_location = Filename.concat "/var/lib/xcp" "blobs"

(* Location of json dump for NRPE to read them *)
let datasource_dump_file = "/dev/shm" // "host-metrics.json"

let rrdd_user_agent = "rrdd" // Xapi_version.version
