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

type without_secret =
  [ `auth
  | `blob
  | `Bond
  | `Certificate
  | `Cluster
  | `Cluster_host
  | `console
  | `crashdump
  | `data_source
  | `Diagnostics
  | `DR_task
  | `event
  | `Feature
  | `generation
  | `Generic
  | `GPU_group
  | `host
  | `host_cpu
  | `host_crashdump
  | `host_metrics
  | `host_patch
  | `LVHD
  | `message
  | `network
  | `network_sriov
  | `Observer
  | `PBD
  | `PCI
  | `PGPU
  | `PIF
  | `PIF_metrics
  | `pool
  | `pool_patch
  | `pool_update
  | `probe_result
  | `PUSB
  | `PVS_cache_storage
  | `PVS_proxy
  | `PVS_server
  | `PVS_site
  | `Repository
  | `role
  | `SDN_controller
  | `secret
  | `SM
  | `SR
  | `sr_stat
  | `subject
  | `task
  | `tunnel
  | `USB_group
  | `user
  | `VBD
  | `VBD_metrics
  | `VDI
  | `vdi_nbd_server_info
  | `VGPU
  | `VGPU_type
  | `VIF
  | `VIF_metrics
  | `VLAN
  | `VM
  | `VM_appliance
  | `VM_group
  | `VM_guest_metrics
  | `VM_metrics
  | `VMPP
  | `VMSS
  | `VTPM
  | `VUSB ]

type secret = [`session]

type not_secret = [without_secret | `session of [`use_make_uuid_rnd_instead]]

type all = [without_secret | secret]

type 'a t = Uuidm.t constraint 'a = [< all]

let null = Uuidm.nil

let pp = Uuidm.pp

let equal = Uuidm.equal

let of_bytes u = Uuidm.of_bytes ~pos:0 u

let to_bytes = Uuidm.to_bytes

let of_int_array arr =
  arr |> Array.to_seq |> Seq.map char_of_int |> String.of_seq |> of_bytes

let to_int_array u =
  Uuidm.to_bytes u |> String.to_seq |> Seq.map int_of_char |> Array.of_seq

let of_string = Uuidm.of_string ~pos:0

let to_string = Uuidm.to_string ~upper:false

let is_uuid str = match of_string str with None -> false | Some _ -> true

let dev_urandom = "/dev/urandom"

let dev_urandom_fd = Unix.openfile dev_urandom [Unix.O_RDONLY] 0o640
(* we can't close this in at_exit, because Crowbar runs at_exit, and
   it'll fail because this FD will then be closed
*)

let read_bytes dev n =
  let buf = Bytes.create n in
  let read = Unix.read dev buf 0 n in
  if read <> n then
    raise End_of_file
  else
    Bytes.to_string buf

let make_uuid_urnd () = of_bytes (read_bytes dev_urandom_fd 16) |> Option.get

(* Use the CSPRNG-backed urandom *)
let make = make_uuid_urnd

type cookie = string

let make_cookie () =
  read_bytes dev_urandom_fd 64
  |> String.to_seq
  |> Seq.map (fun c -> Printf.sprintf "%1x" (int_of_char c))
  |> List.of_seq
  |> String.concat ""

let string_of_cookie s = s

let cookie_of_string s = s

(* deprecated: we don't need to duplicate the uuid prefix/suffix *)
let uuid_of_string = of_string

let string_of_uuid = to_string

let uuid_of_int_array = of_int_array

let int_array_of_uuid = to_int_array

module Hash = struct
  (** Derive a deterministic UUID from a string: the same
      string maps to the same UUID. We are using our own namespace; the
      namespace is not a secret *)

  let namespace =
    let ns = "e93e0639-2bdb-4a59-8b46-352b3f408c19" in
    Uuidm.(of_string ns |> Option.get)

  let string str = Uuidm.v5 namespace str
end
