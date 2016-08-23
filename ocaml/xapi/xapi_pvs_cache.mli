(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

exception No_cache_sr_available
exception No_cache_vdi_present

(** Check the pool for PVS cache SRs which are
    a) visible to the specified host
    b) listed as cache storage for the specified PVS site.

    Returns
      Some (sr, Some vdi)
      - if there is an SR available with a cache VDI
      Some (sr, None)
      - if there is an SR available with no cache VDI
      None
      - if there is no SR available

    If multiple SRs are available, the algorithm will firstly prefer SRs which
    already contain cache VDIs, and secondly will prefer SRs with
    lexicographically lower UUIDs. *)
val check_cache_availability :
  __context:Context.t ->
  host:API.ref_host ->
  site:API.ref_PVS_site ->
  (API.ref_SR * int64 * API.ref_VDI option) option

(** Ensure a cache VDI exists in an SR suitable for this host and PVS site.
    If there is no suitable SR, raise [No_cache_sr_available]. *)
val find_or_create_cache_vdi :
  __context:Context.t -> host:API.ref_host -> site:API.ref_PVS_site -> API.ref_SR * API.ref_VDI

(** Find a cache VDI on the given SR.
    If there is none, raise [No_cache_vdi_present]. *)
val find_cache_vdi :
  __context:Context.t -> sr:API.ref_SR -> API.ref_VDI

(** Destroy all PVS cache VDIs in the specified SR. *)
val on_sr_remove : __context:Context.t -> sr:API.ref_SR -> unit
