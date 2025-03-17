(*
 * Copyright (C) 2017 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open Qcow_types

val on_duplicate_reference: Qcow_metadata.t -> Qcow_cluster_map.t -> cluster_bits:int ->
   (int64 * int) -> (int64 * int) -> int64 ->
   (unit, [> `Disconnected | `Is_read_only | `Msg of string | `Unimplemented ]) result Lwt.t

val check_references: Qcow_metadata.t -> Qcow_cluster_map.t -> cluster_bits:int -> Cluster.t ->
  (unit, [> `Disconnected | `Is_read_only | `Msg of string | `Unimplemented ]) result Lwt.t
(** [check_references metadata map cluster_bits target] follows the back references
    from physical offset [target], verifying the references on disk as it goes *)
