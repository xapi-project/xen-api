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
(** Functions relating to memory requirements of Xen domains *)

open Printf
open Xenops_utils

module D = Debug.Make(struct let name = "xenops" end)
open D

let ( +++ ) = Int64.add
let ( --- ) = Int64.sub
let ( *** ) = Int64.mul
let ( /// ) = Int64.div

(* === Memory conversion factors ============================================ *)

let bytes_per_kib  = 1024L
let bytes_per_mib  = 1048576L
let bytes_per_page = 4096L
let kib_per_page   = bytes_per_page /// bytes_per_kib
let kib_per_mib    = 1024L
let pages_per_mib  = bytes_per_mib /// bytes_per_page

(* === Arithmetic functions ================================================= *)

(** Returns true if (and only if) the specified argument is a power of 2. *)
let is_power_of_2 n =
  (n > 0) && (n land (0 - n) = n)

let round_down_to_multiple_of x y =
  (x /// y) *** y

let round_up_to_multiple_of x y =
  ((x +++ y --- 1L) /// y) *** y

(* === Memory rounding functions ============================================ *)

let round_up = round_up_to_multiple_of
let round_down = round_down_to_multiple_of

let round_bytes_down_to_nearest_page_boundary v = round_down v bytes_per_page
let round_bytes_down_to_nearest_mib           v = round_down v bytes_per_mib
let round_kib_down_to_nearest_page_boundary   v = round_down v kib_per_page
let round_kib_up_to_nearest_page_boundary     v = round_up   v kib_per_page
let round_kib_up_to_nearest_mib               v = round_up   v kib_per_mib
let round_pages_up_to_nearest_mib             v = round_up   v pages_per_mib

(* === Division functions =================================================== *)

let divide_rounding_down numerator denominator =
  numerator /// denominator

let divide_rounding_up numerator denominator =
  (numerator +++ denominator --- 1L) /// denominator

(* === Memory unit conversion functions ===================================== *)

let bytes_of_kib   value = value *** bytes_per_kib
let bytes_of_pages value = value *** bytes_per_page
let bytes_of_mib   value = value *** bytes_per_mib
let kib_of_mib     value = value *** kib_per_mib
let kib_of_pages   value = value *** kib_per_page
let pages_of_mib   value = value *** pages_per_mib

let kib_of_bytes_free   value = divide_rounding_down value bytes_per_kib
let pages_of_bytes_free value = divide_rounding_down value bytes_per_page
let pages_of_kib_free   value = divide_rounding_down value kib_per_page
let mib_of_bytes_free   value = divide_rounding_down value bytes_per_mib
let mib_of_kib_free     value = divide_rounding_down value kib_per_mib
let mib_of_pages_free   value = divide_rounding_down value pages_per_mib

let kib_of_bytes_used   value = divide_rounding_up value bytes_per_kib
let pages_of_bytes_used value = divide_rounding_up value bytes_per_page
let pages_of_kib_used   value = divide_rounding_up value kib_per_page
let mib_of_bytes_used   value = divide_rounding_up value bytes_per_mib
let mib_of_kib_used     value = divide_rounding_up value kib_per_mib
let mib_of_pages_used   value = divide_rounding_up value pages_per_mib

(* === Domain memory breakdown ============================================== *)

(*           ╤  ╔══════════╗                                     ╤            *)
(*           │  ║ shadow   ║                                     │            *)
(*           │  ╠══════════╣                                     │            *)
(*  overhead │  ║ extra    ║                                     │            *)
(*           │  ║ external ║                                     │            *)
(*           │  ╠══════════╣                          ╤          │            *)
(*           │  ║ extra    ║                          │          │            *)
(*           │  ║ internal ║                          │          │            *)
(*           ╪  ╠══════════╣                ╤         │          │ footprint  *)
(*           │  ║ video    ║                │         │          │            *)
(*           │  ╠══════════╣  ╤    ╤        │ actual  │ xen      │            *)
(*           │  ║          ║  │    │        │ /       │ maximum  │            *)
(*           │  ║          ║  │    │        │ target  │          │            *)
(*           │  ║ guest    ║  │    │ build  │ /       │          │            *)
(*           │  ║          ║  │    │ start  │ total   │          │            *)
(*    static │  ║          ║  │    │        │         │          │            *)
(*   maximum │  ╟──────────╢  │    ╧        ╧         ╧          ╧            *)
(*           │  ║          ║  │                                               *)
(*           │  ║          ║  │                                               *)
(*           │  ║ balloon  ║  │ build                                         *)
(*           │  ║          ║  │ maximum                                       *)
(*           │  ║          ║  │                                               *)
(*           ╧  ╚══════════╝  ╧                                               *)

(* === Domain memory breakdown: HVM guests ================================== *)

module type MEMORY_MODEL_DATA = sig
  val extra_internal_mib : int64
  val extra_external_mib : int64
end

module HVM_memory_model_data : MEMORY_MODEL_DATA = struct
  let extra_internal_mib = 1L
  let extra_external_mib = 1L
end

module Linux_memory_model_data : MEMORY_MODEL_DATA = struct
  let extra_internal_mib = 0L
  let extra_external_mib = 1L
end

module Memory_model (D : MEMORY_MODEL_DATA) = struct

  let build_max_mib static_max_mib video_mib = static_max_mib --- (Int64.of_int video_mib)

  let build_start_mib target_mib video_mib = target_mib --- (Int64.of_int video_mib)

  let xen_max_offset_mib = D.extra_internal_mib

  let xen_max_mib target_mib = target_mib +++ xen_max_offset_mib

  let shadow_mib static_max_mib vcpu_count multiplier =
    let vcpu_pages = 256L *** (Int64.of_int vcpu_count) in
    let p2m_map_pages = static_max_mib in
    let shadow_resident_pages = static_max_mib in
    let total_mib = mib_of_pages_used
        (vcpu_pages +++ p2m_map_pages +++ shadow_resident_pages) in
    let total_mib_multiplied =
      Int64.of_float ((Int64.to_float total_mib) *. multiplier) in
    max 1L total_mib_multiplied

  let overhead_mib static_max_mib vcpu_count multiplier =
    D.extra_internal_mib +++
    D.extra_external_mib +++
    (shadow_mib static_max_mib vcpu_count multiplier)

  let footprint_mib target_mib static_max_mib vcpu_count multiplier =
    target_mib +++ (overhead_mib static_max_mib vcpu_count multiplier)

  let shadow_multiplier_default = 1.0

end

module HVM = Memory_model (HVM_memory_model_data)
module Linux = Memory_model (Linux_memory_model_data)


