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

let src =
  let src = Logs.Src.create "qcow" ~doc:"qcow2-formatted BLOCK device" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Error = Qcow_error
module Physical = Qcow_physical
module Metadata = Qcow_metadata
open Qcow_types

let check_on_disk_reference metadata ~cluster_bits (c, w) target =
  Metadata.read metadata c
    (fun contents ->
      let p = Metadata.Physical.of_contents contents in
      let target' = Metadata.Physical.get p w in
      let target_cluster = Physical.cluster ~cluster_bits target in
      let target'_cluster = Physical.cluster ~cluster_bits target' in
      let descr = Printf.sprintf "Physical.get %s:%d = %s (%s %s)"
        (Cluster.to_string c) w (Cluster.to_string target'_cluster)
        (if target = target' then "=" else "<>")
        (Cluster.to_string target_cluster) in
      if target <> target'
      then Log.err (fun f -> f "%s" descr)
      else Log.info (fun f -> f "%s" descr);
      Lwt.return (Ok ())
    )

let rec check_references metadata cluster_map ~cluster_bits (cluster: Cluster.t) =
  let open Error.Lwt_write_error.Infix in
  match Qcow_cluster_map.find cluster_map cluster with
  | exception Not_found ->
    if Qcow_cluster_map.is_immovable cluster_map cluster
    then Log.info (fun f -> f "Cluster %s is an L1 cluster" (Cluster.to_string cluster))
    else Log.err (fun f -> f "No reference to cluster %s" (Cluster.to_string cluster));
    Lwt.return (Ok ())
  | c', w' ->
    let target = Physical.make ~is_mutable:true ~is_compressed:false (Cluster.to_int cluster lsl cluster_bits) in
    check_on_disk_reference metadata ~cluster_bits (c', w') target
    >>= fun () ->
    check_references metadata cluster_map ~cluster_bits c'

let on_duplicate_reference metadata cluster_map ~cluster_bits (c, w) (c', w') cluster =
  let open Error.Lwt_write_error.Infix in
  let cluster = Cluster.of_int64 cluster in
  let rec follow (c, w) (cluster: Cluster.t) =
    let target = Physical.make ~is_mutable:true ~is_compressed:true (Cluster.to_int cluster lsl cluster_bits) in
    check_on_disk_reference metadata ~cluster_bits (c, w) target
    >>= fun () ->
    match Qcow_cluster_map.find cluster_map c with
    | exception Not_found ->
      Log.err (fun f -> f "No reference to cluster %s" (Cluster.to_string c));
      Lwt.return (Ok ())
    | c', w' -> follow (c', w') c in
  follow (Cluster.of_int64 c', w') cluster
  >>= fun () ->
  follow (Cluster.of_int64 c, w) cluster
