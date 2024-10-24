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

let of_bytes u = Uuidm.of_binary_string ~pos:0 u

let to_bytes = Uuidm.to_binary_string

let of_int_array arr =
  arr |> Array.to_seq |> Seq.map char_of_int |> String.of_seq |> of_bytes

let to_int_array u =
  to_bytes u |> String.to_seq |> Seq.map int_of_char |> Array.of_seq

let of_string = Uuidm.of_string ~pos:0

let to_string = Uuidm.to_string ~upper:false

let is_uuid str = match of_string str with None -> false | Some _ -> true

let dev_urandom = "/dev/urandom"

let read_bytes dev n =
  let fd = Unix.openfile dev [Unix.O_RDONLY] 0o640 in
  let finally body_f clean_f =
    try
      let ret = body_f () in
      clean_f () ; ret
    with e -> clean_f () ; raise e
  in
  finally
    (fun () ->
      let buf = Bytes.create n in
      let read = Unix.read fd buf 0 n in
      if read <> n then
        raise End_of_file
      else
        Bytes.to_string buf
    )
    (fun () -> Unix.close fd)

let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let csprng_urandom = read_bytes dev_urandom

let csprng_fortuna n = Mirage_crypto_rng.generate n |> Cstruct.to_string

let make_uuid csprng = of_bytes (csprng 16) |> Option.get

let make_uuid_urnd () = make_uuid csprng_urandom

let make_uuid_fortuna () = make_uuid csprng_fortuna

let make_uuid_fast = make_uuid_fortuna

let make_default = ref make_uuid_urnd

let make () = !make_default ()

let make_v7_uuid_from_parts time_ns rand_b = Uuidm.v7_ns ~time_ns ~rand_b

let rand64 () =
  let b = csprng_fortuna 8 in
  String.get_int64_ne b 0

let now_ns =
  let start = Mtime_clock.counter () in
  let t0 =
    let d, ps = Ptime_clock.now () |> Ptime.to_span |> Ptime.Span.to_d_ps in
    Int64.(add (mul (of_int d) 86_400_000_000_000L) (div ps 1000L))
  in
  fun () ->
    let since_t0 = Mtime_clock.count start |> Mtime.Span.to_uint64_ns in
    Int64.add t0 since_t0

let make_v7_uuid () = make_v7_uuid_from_parts (now_ns ()) (rand64 ())

type cookie = string

let make_cookie () =
  read_bytes dev_urandom 64
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
