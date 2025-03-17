(*
 * Copyright (C) 2013 Citrix Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

let mib = Int64.mul 1024L 1024L
let gib = Int64.mul mib 1024L
let tib = Int64.mul gib 1024L
let pib = Int64.mul tib 1024L

let boundaries cluster_bits =
  let cluster_size = Int64.shift_left 1L cluster_bits in
  let pointers_in_cluster = Int64.(div cluster_size 8L) in [
    "0", 0L;
    Printf.sprintf "one %Ld byte cluster" cluster_size, cluster_size;
    Printf.sprintf "one L2 table (containing %Ld 8-byte pointers to cluster)"
      pointers_in_cluster,
    Int64.(mul cluster_size pointers_in_cluster);
    Printf.sprintf "one L1 table (containing %Ld 8-byte pointers to L2 tables)"
      pointers_in_cluster,
    Int64.(mul (mul cluster_size pointers_in_cluster) pointers_in_cluster)
  ]

let sizes sector_size cluster_bits = [
  "one sector", Int64.of_int sector_size;
  "one page", 4096L;
  "one cluster", Int64.shift_left 1L cluster_bits;
]

let off_by ((label', offset'), (label, offset)) = [
  label, offset;
  label ^ " + " ^ label', Int64.add offset offset';
  label ^ " - " ^ label', Int64.sub offset offset';
  label ^ " + 2 * " ^ label', Int64.(add offset (mul 2L offset'));
]

let rec cross xs ys = match xs, ys with
  | [], _ -> []
  | x :: xs, ys -> List.map (fun y -> x, y) ys @ (cross xs ys)

(* Parameterise over sector, page, cluster, more *)
let interesting_ranges sector_size size_sectors cluster_bits =
  let size_bytes = Int64.(mul size_sectors (of_int sector_size)) in
  let starts = List.concat (List.map off_by (cross (sizes sector_size cluster_bits) (boundaries cluster_bits))) in
  let all = starts @ (List.map (fun (label, offset) -> label ^ " from the end", Int64.sub size_bytes offset) starts) in
  (* add lengths *)
  let all = List.map (fun ((label', length'), (label, offset)) ->
      label' ^ " @ " ^ label, offset, length'
    ) (cross (sizes sector_size cluster_bits) all) in
  List.filter
    (fun (_label, offset, length) ->
       offset >= 0L && (Int64.add offset length <= size_bytes)
    ) all
