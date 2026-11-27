(*
 * Copyright (c) Cloud Software Group, Inc.
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

(* Host-free unit tests for the vhd-tool allocation-map parsers
   (Vhd_qcow_parsing.parse_header / parse_header_interval) and the XVA chunk
   arithmetic in Stream_vdi. The send/recv round-trip (needs a live host) is in
   ocaml/quicktest; the qemu-img parser parse_header_qemu_img is covered separately. *)

(* Feed [json] to [f] through a temp-file fd. The parsers close the fd
   themselves (In_channel.close), so we don't. *)
let with_json_fd json f =
  let path = Filename.temp_file "test_stream_vdi" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      let oc = open_out_bin path in
      output_string oc json ;
      close_out oc ;
      let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
      f fd
    )

(* ---------------------------------------------------------------------------
   Vhd_qcow_parsing: the vhd-tool header shapes
   --------------------------------------------------------------------------- *)

(* vhd-tool emits {"cluster_bits": n, "data_clusters": [...]}; cluster_size is
   derived as 1 lsl cluster_bits. 21 bits -> 2 MiB, the VHD block size. *)
let test_parse_header_vhd_tool () =
  let json = {|{"cluster_bits": 21, "data_clusters": [0, 1, 5]}|} in
  let cluster_size, clusters =
    with_json_fd json Vhd_qcow_parsing.parse_header
  in
  Alcotest.(check int)
    "cluster_size derived from cluster_bits" 2097152 cluster_size ;
  Alcotest.(check (list int)) "data cluster indices" [0; 1; 5] clusters

(* Same header, but data_clusters are [lo, hi] interval pairs. *)
let test_parse_header_interval_vhd_tool () =
  let json = {|{"cluster_bits": 21, "data_clusters": [[0, 2], [5, 5]]}|} in
  let cluster_size, intervals =
    with_json_fd json Vhd_qcow_parsing.parse_header_interval
  in
  Alcotest.(check int)
    "cluster_size derived from cluster_bits" 2097152 cluster_size ;
  Alcotest.(check (list (pair int int)))
    "data cluster intervals"
    [(0, 2); (5, 5)]
    intervals

(* The vhd-tool parser must reject the qemu-img `map` array shape, so the two
   JSON shapes can't be silently swapped (cf. the Qcow_tool_wrapper defect). *)
let test_parse_header_rejects_qemu_img_shape () =
  let qemu_map = {|[{"start": 0, "length": 1048576, "data": true}]|} in
  let raises =
    try
      ignore (with_json_fd qemu_map Vhd_qcow_parsing.parse_header) ;
      false
    with Yojson.Basic.Util.Type_error _ -> true
  in
  Alcotest.(check bool) "parse_header rejects a qemu-img map array" true raises

(* ---------------------------------------------------------------------------
   Stream_vdi.get_allocated_chunks_from_clusters: clusters -> 1 MiB chunk set
   --------------------------------------------------------------------------- *)

let elements = Stream_vdi.ChunkSet.elements

(* 512 KiB clusters in 1 MiB chunks: clusters 0..3 collapse to chunks {0, 1}. *)
let test_allocated_chunks_subchunk_cluster () =
  let set = Stream_vdi.get_allocated_chunks_from_clusters 524288 [0; 1; 2; 3] in
  Alcotest.(check (list int))
    "sub-chunk clusters fold into chunks" [0; 1] (elements set)

(* 2 MiB clusters in 1 MiB chunks: clusters {0, 1} cover chunks {0, 1, 2, 3}. *)
let test_allocated_chunks_multichunk_cluster () =
  let set = Stream_vdi.get_allocated_chunks_from_clusters 2097152 [0; 1] in
  Alcotest.(check (list int))
    "clusters larger than a chunk span several chunks" [0; 1; 2; 3]
    (elements set)

(* cluster_size equal to chunk_size: cluster n maps exactly to chunk n. *)
let test_allocated_chunks_boundary () =
  let set = Stream_vdi.get_allocated_chunks_from_clusters 1048576 [0; 3] in
  Alcotest.(check (list int))
    "cluster equal to chunk maps one-to-one" [0; 3] (elements set)

(* ---------------------------------------------------------------------------
   Stream_vdi.get_chunk_numbers_in_increasing_order: NBD extents -> chunk list
   --------------------------------------------------------------------------- *)

let data ?(flags = 0l) length = Stream_vdi.{flags; length= Int64.of_int length}

(* An extent ending on a chunk boundary covers one chunk, not the next. *)
let test_chunks_single_extent_on_boundary () =
  let result =
    Stream_vdi.get_chunk_numbers_in_increasing_order [data 1048576] 0L
  in
  Alcotest.(check (list int64))
    "extent ending on a boundary covers one chunk" [0L] result

(* One byte past the boundary spills into the next chunk. *)
let test_chunks_extent_spilling_into_next () =
  let result =
    Stream_vdi.get_chunk_numbers_in_increasing_order [data 1048577] 0L
  in
  Alcotest.(check (list int64))
    "one extra byte spills into the next chunk" [0L; 1L] result

(* A hole is skipped but still advances the offset, so the next data extent
   lands in the right chunk. *)
let test_chunks_hole_is_skipped () =
  let result =
    Stream_vdi.get_chunk_numbers_in_increasing_order
      [data ~flags:Stream_vdi.flag_hole 1048576; data 1048576]
      0L
  in
  Alcotest.(check (list int64))
    "a hole is skipped but still advances the offset" [1L] result

(* A zero extent is treated like a hole and skipped. *)
let test_chunks_zero_is_skipped () =
  let result =
    Stream_vdi.get_chunk_numbers_in_increasing_order
      [data ~flags:Stream_vdi.flag_zero 1048576; data 1048576]
      0L
  in
  Alcotest.(check (list int64)) "a zero extent is skipped" [1L] result

(* Two adjacent half-chunk extents both land in chunk 0; emitted only once. *)
let test_chunks_dedup_within_chunk () =
  let result =
    Stream_vdi.get_chunk_numbers_in_increasing_order
      [data 524288; data 524288]
      0L
  in
  Alcotest.(check (list int64))
    "adjacent extents in one chunk are not duplicated" [0L] result

(* A non-zero starting offset shifts the chunk numbers. *)
let test_chunks_respects_offset () =
  let result =
    Stream_vdi.get_chunk_numbers_in_increasing_order [data 1048576] 1048576L
  in
  Alcotest.(check (list int64))
    "a non-zero starting offset shifts the chunk number" [1L] result

let test =
  [
    ("parse_header vhd-tool shape", `Quick, test_parse_header_vhd_tool)
  ; ( "parse_header_interval vhd-tool shape"
    , `Quick
    , test_parse_header_interval_vhd_tool
    )
  ; ( "parse_header rejects qemu-img map shape"
    , `Quick
    , test_parse_header_rejects_qemu_img_shape
    )
  ; ( "allocated chunks: sub-chunk clusters"
    , `Quick
    , test_allocated_chunks_subchunk_cluster
    )
  ; ( "allocated chunks: multi-chunk clusters"
    , `Quick
    , test_allocated_chunks_multichunk_cluster
    )
  ; ( "allocated chunks: cluster on boundary"
    , `Quick
    , test_allocated_chunks_boundary
    )
  ; ( "chunk numbers: single extent on boundary"
    , `Quick
    , test_chunks_single_extent_on_boundary
    )
  ; ( "chunk numbers: extent spilling into next"
    , `Quick
    , test_chunks_extent_spilling_into_next
    )
  ; ("chunk numbers: hole skipped", `Quick, test_chunks_hole_is_skipped)
  ; ("chunk numbers: zero skipped", `Quick, test_chunks_zero_is_skipped)
  ; ("chunk numbers: dedup within chunk", `Quick, test_chunks_dedup_within_chunk)
  ; ("chunk numbers: respects offset", `Quick, test_chunks_respects_offset)
  ]
