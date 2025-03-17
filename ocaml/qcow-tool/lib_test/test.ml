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
module Lwt_error = Error.Lwt_error
module Lwt_write_error = Error.Lwt_write_error
module FromResult = Error.FromResult

open Qcow
open Lwt
open OUnit
open Utils
open Sizes

module Block = UnsafeBlock

let repair_refcounts path =
  let module B = Qcow.Make(Block)(Time) in
  let t =
    Block.connect path
    >>= fun raw ->
    B.connect raw
    >>= fun qcow ->
    let open Lwt_write_error.Infix in
    B.rebuild_refcount_table qcow
    >>= fun () ->
    let open Lwt.Infix in
    B.disconnect qcow
    >>= fun () ->
    Block.disconnect raw
    >>= fun () ->
    Lwt.return (Ok ()) in
  t >>= function
  | Ok () -> Lwt.return ()
  | Error (`Msg x) -> failwith x

(* qemu-img will set version = `Three and leave an extra cluster
   presumably for extension headers *)

let read_write_header name size =
  let module B = Qcow.Make(Block)(Time) in
  let path = Filename.concat test_dir (Printf.sprintf "read_write_header.%s.%Ld" name size) in

  let t =
    truncate path
    >>= fun () ->
    Block.connect path
    >>= fun raw ->
    B.create raw ~size ()
    >>= fun _b ->
    let open Lwt.Infix in
    repair_refcounts path
    >>= fun () ->
    Qemu.Img.check path;

    let page = Io_page.(to_cstruct (get 1)) in
    let open Lwt_error.Infix in
    Block.read raw 0L [ page ]
    >>= fun () ->
    let open FromResult in
    Qcow.Header.read page
    >>= fun (hdr, _) ->
    Lwt.return (Ok hdr) in
  match Lwt_main.run t with
  | Ok x -> x
  | Error _ -> failwith "read_write_header"

let additional = Some {
  Qcow.Header.dirty = true;
  corrupt = false;
  lazy_refcounts = true;
  autoclear_features = 0L;
  refcount_order = 4l;
}

let create_1K () =
  let hdr = read_write_header "1K" 1024L in
  let expected = {
    Qcow.Header.version = `Three; backing_file_offset = 0L;
    backing_file_size = 0l; cluster_bits = 16l; size = 1024L;
    crypt_method = `None; l1_size = 1l; l1_table_offset = Qcow.Physical.make ~is_mutable:false 131072;
    refcount_table_offset = Qcow.Physical.make ~is_mutable:false 65536; refcount_table_clusters = 1l;
    nb_snapshots = 0l; snapshots_offset = 0L; additional;
    extensions = [ `Feature_name_table Qcow.Header.Feature.understood ];
  } in
  let cmp a b = Qcow.Header.compare a b = 0 in
  let printer = Qcow.Header.to_string in
  assert_equal ~printer ~cmp expected hdr

let create_1M () =
  let hdr = read_write_header "1M" 1048576L in
  let expected = {
    Qcow.Header.version = `Three; backing_file_offset = 0L;
    backing_file_size = 0l; cluster_bits = 16l; size = 1048576L;
    crypt_method = `None; l1_size = 1l; l1_table_offset = Qcow.Physical.make ~is_mutable:false 131072;
    refcount_table_offset = Qcow.Physical.make ~is_mutable:false 65536; refcount_table_clusters = 1l;
    nb_snapshots = 0l; snapshots_offset = 0L; additional;
    extensions = [ `Feature_name_table Qcow.Header.Feature.understood ];
  } in
  let cmp a b = Qcow.Header.compare a b = 0 in
  let printer = Qcow.Header.to_string in
  assert_equal ~printer ~cmp expected hdr

let create_1P () =
  let hdr = read_write_header "1P" pib in
  let expected = {
    Qcow.Header.version = `Three; backing_file_offset = 0L;
    backing_file_size = 0l; cluster_bits = 16l; size = pib;
    crypt_method = `None; l1_size = 2097152l; l1_table_offset = Qcow.Physical.make ~is_mutable:false 131072;
    refcount_table_offset = Qcow.Physical.make ~is_mutable:false 65536; refcount_table_clusters = 1l;
    nb_snapshots = 0l; snapshots_offset = 0L; additional;
    extensions = [ `Feature_name_table Qcow.Header.Feature.understood ];
  } in
  let cmp a b = Qcow.Header.compare a b = 0 in
  let printer = Qcow.Header.to_string in
  assert_equal ~printer ~cmp expected hdr

let get_id =
  let next = ref 1 in
  fun () ->
    let this = !next in
    incr next;
    this

let rec fragment into remaining =
  if into >= Cstruct.len remaining
  then [ remaining ]
  else
    let this = Cstruct.sub remaining 0 into in
    let rest = Cstruct.shift remaining into in
    this :: (fragment into rest)

let check_file_contents path id _sector_size _size_sectors (start, length) () =
  let module RawReader = Block in
  let module Reader = Qcow.Make(RawReader)(Time) in
  let sector = Int64.div start 512L in
  (* This is the range that we expect to see written *)
  RawReader.connect path
  >>= fun raw ->
  Reader.connect raw
  >>= fun b ->
  let expected = { Extent.start = sector; length = Int64.(div (of_int length) 512L) } in
  let open Lwt_error.Infix in
  let module F = Mirage_block_combinators.Fast_fold(Reader) in
  F.mapped_s
    ~f:(fun bytes_seen ofs data ->
        let actual = { Extent.start = ofs; length = Int64.of_int (Cstruct.len data / 512) } in
        (* Any data we read now which wasn't expected must be full of zeroes *)
        let extra = Extent.difference actual expected in
        List.iter
          (fun { Extent.start; length } ->
             let buf = Cstruct.sub data (512 * Int64.(to_int (sub start ofs))) (Int64.to_int length * 512) in
             for i = 0 to Cstruct.len buf - 1 do
               assert_equal ~printer:string_of_int ~cmp:(fun a b -> a = b) 0 (Cstruct.get_uint8 buf i);
             done;
          ) extra;
        let common = Extent.intersect actual expected in
        List.iter
          (fun { Extent.start; length } ->
             let buf = Cstruct.sub data (512 * Int64.(to_int (sub start ofs))) (Int64.to_int length * 512) in
             for i = 0 to Cstruct.len buf - 1 do
               assert_equal ~printer:string_of_int ~cmp:(fun a b -> a = b) (id mod 256) (Cstruct.get_uint8 buf i)
             done;
          ) common;
        let seen_this_time = 512 * List.(fold_left (+) 0 (map (fun e -> Int64.to_int e.Extent.length) common)) in
        return (bytes_seen + seen_this_time)
      ) 0  b
  >>= fun total_bytes_seen ->
  assert_equal ~printer:string_of_int length total_bytes_seen;
  Reader.Debug.check_no_overlaps b
  >>= fun () ->
  let open Lwt.Infix in
  Reader.disconnect b
  >>= fun () ->
  RawReader.disconnect raw
  >>= fun () ->
  Lwt.return (Ok ())

let write_read_native sector_size size_sectors (start, length) () =
  let module RawWriter = Block in
  let module Writer = Qcow.Make(RawWriter)(Time) in
  let path = Filename.concat test_dir (Printf.sprintf "write_read_native.%Ld.%Ld.%d" size_sectors start length) in

  let t =
    truncate path
    >>= fun () ->
    RawWriter.connect path
    >>= fun raw ->
    let open Lwt_write_error.Infix in
    Writer.create raw ~size:Int64.(mul size_sectors (of_int sector_size)) ()
    >>= fun b ->

    let sector = Int64.div start 512L in
    let id = get_id () in
    let buf = malloc length in
    Cstruct.memset buf (id mod 256);
    Writer.write b sector (fragment 4096 buf)
    >>= fun () ->
    let buf' = malloc length in
    let open Lwt_error.Infix in
    Writer.read b sector (fragment 4096 buf')
    >>= fun () ->
    let cmp a b = Cstruct.compare a b = 0 in
    assert_equal ~printer:(fun x -> String.escaped (Cstruct.to_string x)) ~cmp buf buf';
    let open Lwt.Infix in
    Writer.disconnect b
    >>= fun () ->
    RawWriter.disconnect raw
    >>= fun () ->
    repair_refcounts path
    >>= fun () ->
    Qemu.Img.check path;
    check_file_contents path id sector_size size_sectors (start, length) () in
  or_failwith @@ Lwt_main.run t

let write_discard_read_native sector_size size_sectors (start, length) () =
  let module RawWriter = Block in
  let module Writer = Qcow.Make(RawWriter)(Time) in
  let path = Filename.concat test_dir (Printf.sprintf "write_discard_read_native.%Ld.%Ld.%d" size_sectors start length) in
  let t =
    truncate path
    >>= fun () ->
    let open Lwt.Infix in
    RawWriter.connect path
    >>= fun raw ->
    let config = Writer.Config.create ~discard:true ~runtime_asserts:true ~id:"id" () in
    let open Lwt_write_error.Infix in
    Writer.create raw ~size:Int64.(mul size_sectors (of_int sector_size)) ~config ()
    >>= fun b ->

    let sector = Int64.div start 512L in
    let id = get_id () in
    let buf = malloc length in
    Cstruct.memset buf (id mod 256);
    Writer.write b sector (fragment 4096 buf)
    >>= fun () ->
    Writer.discard b ~sector ~n:(Int64.of_int (length / 512)) ()
    >>= fun () ->
    let buf' = malloc length in
    let open Lwt_error.Infix in
    Writer.read b sector (fragment 4096 buf')
    >>= fun () ->
    (* Data has been discarded, so assume the implementation now guarantees
       zero (cf ATA RZAT) *)
    for i = 0 to Cstruct.len buf' - 1 do
      if Cstruct.get_uint8 buf' i <> 0 then failwith "I did not Read Zero After TRIM"
    done;
    let open Lwt.Infix in
    Writer.Debug.assert_cluster_map_in_sync b
    >>= fun () ->
    Writer.disconnect b
    >>= fun () ->
    RawWriter.disconnect raw
    >>= fun () ->
    repair_refcounts path
    >>= fun () ->
    Qemu.Img.check path;
    check_file_contents path id sector_size size_sectors (0L, 0) () in

  or_failwith @@ Lwt_main.run t

let check_refcount_table_allocation () =
  let module B = Qcow.Make(Ramdisk)(Time) in
  let t =
    Ramdisk.destroy ~name:"test";
    Ramdisk.connect ~name:"test"
    >>= fun ramdisk ->
    let open Lwt_write_error.Infix in
    B.create ramdisk ~size:pib ()
    >>= fun b ->

    let h = B.header b in
    (* let max_cluster = Int64.shift_right h.Header.size (Int32.to_int h.Header.cluster_bits) in
    B.Debug.set_next_cluster b (Int64.pred max_cluster); *)
    let length = 1 lsl (Int32.to_int h.Header.cluster_bits) in
    let sector = 0L in

    let buf = malloc length in
    B.write b sector (fragment 4096 buf)
    >>= fun () ->
    Lwt.return (Ok ()) in
  or_failwith @@ Lwt_main.run t

let check_full_disk () =
  let module B = Qcow.Make(Ramdisk)(Time) in
  let t =
    Ramdisk.destroy ~name:"test";
    Ramdisk.connect ~name:"test"
    >>= fun ramdisk ->
    let open Lwt_write_error.Infix in
    let config = B.Config.create ~runtime_asserts:true ~id:"id" () in
    B.create ramdisk ~size:gib ~config ()
    >>= fun b ->

    let open Lwt.Infix in
    B.get_info b
    >>= fun info ->

    let buf = malloc 512 in
    let h = B.header b in
    let sectors_per_cluster = Int64.(div (shift_left 1L (Int32.to_int h.Header.cluster_bits)) 512L) in
    let rec loop sector =
      if sector >= info.Mirage_block.size_sectors
      then Lwt.return (Ok ())
      else begin
        let open Lwt_write_error.Infix in
        B.write b sector [ buf ]
        >>= fun () ->
        loop Int64.(add sector sectors_per_cluster)
      end in
    loop 0L in
  or_failwith @@ Lwt_main.run t

(* Compare the output of this code against qemu *)
let virtual_sizes = [
  mib;
  gib;
  tib;
]

let check_file path size =
  let open Lwt.Infix in
  let info = Qemu.Img.info path in
  assert_equal ~printer:Int64.to_string size info.Qemu.Img.virtual_size;
  let module M = Qcow.Make(Block)(Time) in
  repair_refcounts path
  >>= fun () ->
  Qemu.Img.check path;
  Block.connect path
  >>= fun b ->
  M.connect b
  >>= fun qcow ->
  let h = M.header qcow in
  assert_equal ~printer:Int64.to_string size h.Qcow.Header.size;
  (* Unfortunately qemu-img info doesn't query the dirty flag:
     https://github.com/djs55/qemu/commit/9ac8f24fde855c66b1378cee30791a4aef5c33ba
  assert_equal ~printer:string_of_bool dirty info.Qemu.Img.dirty_flag;
  *)
  M.disconnect qcow
  >>= fun () ->
  Block.disconnect b
  >>= fun () ->
  Lwt.return (Ok ())

let qemu_img size =
  let path = Filename.concat test_dir (Int64.to_string size) in
  Qemu.Img.create path size;
  or_failwith @@ Lwt_main.run @@ check_file path size

let qemu_img_suite =
  List.map (fun size ->
      Printf.sprintf "check that qemu-img creates files and we can read the metadata, size = %Ld bytes" size >:: (fun () -> qemu_img size)
    ) virtual_sizes


let qcow_tool size =
  let open Lwt.Infix in
  let module B = Qcow.Make(Block)(Time) in
  let path = Filename.concat test_dir (Int64.to_string size) in

  let t =
    truncate path
    >>= fun () ->
    Block.connect path
    >>= fun block ->
    let open Lwt_write_error.Infix in
    let config = B.Config.create ~runtime_asserts:true ~id:"id" () in
    B.create block ~size ~config ()
    >>= fun qcow ->
    let open Lwt.Infix in
    B.disconnect qcow
    >>= fun () ->
    Block.disconnect block
    >>= fun () ->
    check_file path size in
  or_failwith @@ Lwt_main.run t

let qcow_tool_resize ?ignore_data_loss size_from size_to =
  let open Lwt.Infix in
  let module B = Qcow.Make(Block)(Time) in
  let path = Filename.concat test_dir (Int64.to_string size_from) in

  let t =
    truncate path
    >>= fun () ->
    Block.connect path
    >>= fun block ->
    let open Lwt_write_error.Infix in
    let config = B.Config.create ~runtime_asserts:true ~id:"id" () in
    B.create block ~size:size_from ~config ()
    >>= fun qcow ->
    B.resize qcow ~new_size:size_to ?ignore_data_loss ()
    >>= fun () ->
    let open Lwt.Infix in
    B.disconnect qcow
    >>= fun () ->
    Block.disconnect block
    >>= fun () ->
    check_file path size_to in
  or_failwith @@ Lwt_main.run t

let qcow_tool_bad_resize size_from size_to =
  let open Lwt.Infix in
  let module B = Qcow.Make(Block)(Time) in
  let path = Filename.concat test_dir (Int64.to_string size_from) in

  let t =
    truncate path
    >>= fun () ->
    Block.connect path
    >>= fun block ->
    let open Lwt_write_error.Infix in
    let config = B.Config.create ~runtime_asserts:true ~id:"id" () in
    B.create block ~size:size_from ~config ()
    >>= fun qcow ->
    let open Lwt.Infix in
    B.resize qcow ~new_size:size_to ()
    >>= fun result ->
    B.disconnect qcow
    >>= fun () ->
    Block.disconnect block
    >>= fun () ->
    match result with
    | Ok () -> failwith (Printf.sprintf "Resize succeeded when it shouldn't: size_from = %Ld; size_to = %Ld" size_from size_to)
    | Error _ -> Lwt.return (Ok ()) in
  or_failwith @@ Lwt_main.run t

let create_resize_equals_create size_from size_to =
  let open Lwt.Infix in
  let module B = Qcow.Make(Block)(Time) in
  let path1 = Filename.concat test_dir (Int64.to_string size_from) in
  let path2 = path1 ^ ".resized" in
  let t =
    truncate path2
    >>= fun () ->
    Block.connect path2
    >>= fun block ->
    let open Lwt_write_error.Infix in
    let config = B.Config.create ~runtime_asserts:true ~id:"id" () in
    B.create block ~size:size_from ~config ()
    >>= fun qcow ->
    B.resize qcow ~new_size:size_to ()
    >>= fun () ->
    let open Lwt.Infix in
    B.disconnect qcow
    >>= fun () ->
    Block.disconnect block
    >>= fun () ->
    truncate path1
    >>= fun () ->
    Block.connect path1
    >>= fun block ->
    let open Lwt_write_error.Infix in
    let config = B.Config.create ~runtime_asserts:true ~id:"id" () in
    B.create block ~size:size_to ~config ()
    >>= fun qcow ->
    let open Lwt.Infix in
    B.disconnect qcow
    >>= fun () ->
    Block.disconnect block
    >>= fun () ->
    ignore(Utils.run "diff" [ path1; path2 ]);
    Lwt.return (Ok ()) in
  or_failwith @@ Lwt_main.run t

let range from upto =
  let rec loop acc n = if n = upto then acc else loop (n :: acc) (Int64.succ n) in
  loop [] from

let create_write_discard_all_compact clusters () =
  (* create a large disk *)
  let open Lwt.Infix in
  let module B = Qcow.Make(Block)(Time) in
  let size = gib in
  let path = Filename.concat test_dir (Int64.to_string size) ^ ".compact" in
  let t =
    truncate path
    >>= fun () ->
    Block.connect path
    >>= fun block ->
    let config = B.Config.create ~discard:true ~runtime_asserts:true ~id:"id" () in
    let open Lwt_write_error.Infix in
    B.create block ~size ~config ()
    >>= fun qcow ->
    let h = B.header qcow in
    let cluster_size = 1 lsl (Int32.to_int h.Qcow.Header.cluster_bits) in
    let open Lwt.Infix in
    B.get_info qcow
    >>= fun info ->
    let sectors_per_cluster = cluster_size / info.Mirage_block.sector_size in
    (* write a bunch of clusters at the beginning *)
    let write_cluster idx =
      let cluster = malloc cluster_size in (* don't care about the contents *)
      B.write qcow Int64.(mul idx (of_int sectors_per_cluster)) [ cluster ]
      >>= function
      | Error _ -> failwith "write"
      | Ok () ->
        Lwt.return_unit in
    Lwt_list.iter_s write_cluster (range 0L clusters)
    >>= fun () ->
    (* discard everything *)
    ( B.discard qcow ~sector:0L ~n:info.Mirage_block.size_sectors ()
      >>= function
      | Error _ -> failwith "discard"
      | Ok () -> Lwt.return_unit )
    >>= fun () ->
    (* compact *)
    let open Lwt_write_error.Infix in
    B.compact qcow ()
    >>= fun _report ->
    let open Lwt.Infix in
    B.Debug.assert_cluster_map_in_sync qcow
    >>= fun () ->
    B.disconnect qcow
    >>= fun () ->
    Block.disconnect block
    >>= fun () ->
    Lwt.return (Ok ()) in
  or_failwith @@ Lwt_main.run t

let create_write_discard_compact () =
  (* create a large disk *)
  let open Lwt.Infix in
  let module B = Qcow.Make(Block)(Time) in
  let size = gib in
  let path = Filename.concat test_dir (Int64.to_string size) ^ ".compact" in
  let t =
    truncate path
    >>= fun () ->
    Block.connect path
    >>= fun block ->
    let config = B.Config.create ~discard:true ~runtime_asserts:true ~id:"id" () in
    let open Lwt_write_error.Infix in
    B.create block ~size ~config ()
    >>= fun qcow ->
    (* write a bunch of clusters at the beginning *)
    let h = B.header qcow in
    let cluster_size = 1 lsl (Int32.to_int h.Qcow.Header.cluster_bits) in
    let open Lwt.Infix in
    B.get_info qcow
    >>= fun info ->
    let sectors_per_cluster = cluster_size / info.Mirage_block.sector_size in
    let make_cluster idx =
      let cluster = malloc cluster_size in
      for i = 0 to cluster_size / 8 - 1 do
        Cstruct.BE.set_uint64 cluster (i * 8) idx
      done;
      cluster in
    let write_cluster idx =
      let cluster = make_cluster idx in
      B.write qcow Int64.(mul idx (of_int sectors_per_cluster)) [ cluster ]
      >>= function
      | Error _ -> failwith "write"
      | Ok () ->
        Lwt.return_unit in
    let discard_cluster idx =
      B.discard qcow ~sector:Int64.(mul idx (of_int sectors_per_cluster)) ~n:(Int64.of_int sectors_per_cluster) ()
      >>= function
      | Error _ -> failwith "discard"
      | Ok () ->
        Lwt.return_unit in
    let read_cluster idx =
      let cluster = malloc cluster_size in
      B.read qcow Int64.(mul idx (of_int sectors_per_cluster)) [ cluster ]
      >>= function
      | Error _ -> failwith "read"
      | Ok () ->
        Lwt.return cluster in
    let check_contents cluster expected =
      for i = 0 to cluster_size / 8 - 1 do
        let actual = Cstruct.BE.get_uint64 cluster (i * 8) in
        assert (actual = expected)
      done in
    (* write a bunch of clusters at the beginning *)
    let first = [ 0L; 1L; 2L; 3L; 4L; 5L; 6L; 7L ] in
    Lwt_list.iter_s write_cluster first
    >>= fun () ->
    Lwt_list.iter_s
      (fun idx ->
        read_cluster idx
        >>= fun data ->
        check_contents data idx;
        Lwt.return_unit
      ) first
    >>= fun () ->
    (* write a bunch of clusters near the end. Note we write one fewer cluster
    than we discard because we expect one of the block allocations to be a
    metadata block and we want to test the rewriting. *)
    let second = List.tl @@ List.map Int64.(add (div (div gib (of_int cluster_size)) 2L)) first in
    Lwt_list.iter_s write_cluster second
    >>= fun () ->
    Lwt_list.iter_s
      (fun idx ->
        read_cluster idx
        >>= fun data ->
        check_contents data idx;
        Lwt.return_unit
      ) second
    >>= fun () ->
    (* discard the clusters at the beginning *)
    Lwt_list.iter_s discard_cluster first
    >>= fun () ->
    (* check all the values are as expected *)
    Lwt_list.iter_s
      (fun idx ->
        read_cluster idx
        >>= fun data ->
        check_contents data 0L;
        Lwt.return_unit
      ) first
    >>= fun () ->
    Lwt_list.iter_s
      (fun idx ->
        read_cluster idx
        >>= fun data ->
        check_contents data idx;
        Lwt.return_unit
      ) second
    >>= fun () ->
    (* compact *)
    let open Lwt_write_error.Infix in
    B.compact qcow ()
    >>= fun _report ->
    let open Lwt.Infix in
    (* check all the values are as expected *)
    Lwt_list.iter_s
      (fun idx ->
        read_cluster idx
        >>= fun data ->
        check_contents data 0L;
        Lwt.return_unit
      ) first
    >>= fun () ->
    Lwt_list.iter_s
      (fun idx ->
        read_cluster idx
        >>= fun data ->
        check_contents data idx;
        Lwt.return_unit
      ) second
    >>= fun () ->
    B.Debug.assert_cluster_map_in_sync qcow
    >>= fun () ->
    B.disconnect qcow
    >>= fun () ->
    Block.disconnect block
    >>= fun () ->
    Lwt.return (Ok ()) in
  or_failwith @@ Lwt_main.run t

let qcow_tool_suite =
  let create =
    List.map (fun size ->
        Printf.sprintf "check that qcow-tool creates files and we can read the metadata, size = %Ld bytes" size >:: (fun () -> qcow_tool size)
      ) virtual_sizes in
  let ok_resize =
    let ok = List.filter (fun (a, b) -> a < b) (cross virtual_sizes virtual_sizes) in
    List.map (fun (size_from, size_to) ->
      Printf.sprintf "check that qcow-tool can make files bigger and we can read the metadata, from = %Ld bytes to = %Ld bytes" size_from size_to >:: (fun () -> qcow_tool_resize size_from size_to)
    ) ok in
  let bad_resize =
    let bad = List.filter (fun (a, b) -> a > b) (cross virtual_sizes virtual_sizes) in
    List.map (fun (size_from, size_to) ->
      Printf.sprintf "check that qcow-tool refuses to make files smaller and we can read the metadata, from = %Ld bytes to = %Ld bytes" size_from size_to >:: (fun () -> qcow_tool_bad_resize size_from size_to)
    ) bad in
  let ignore_data_loss_resize =
    let bad = List.filter (fun (a, b) -> a > b) (cross virtual_sizes virtual_sizes) in
    List.map (fun (size_from, size_to) ->
      Printf.sprintf "check that qcow-tool can be forced to make files smaller and we can read the metadata, from = %Ld bytes to = %Ld bytes" size_from size_to >:: (fun () -> qcow_tool_resize ~ignore_data_loss:true size_from size_to)
    ) bad in
  let create_resize_equals_create =
    let good = List.filter (fun (a, b) -> a < b) (cross virtual_sizes virtual_sizes) in
    List.map (fun (size_from, size_to) ->
      Printf.sprintf "check that create then resize creates the same result as create, from = %Ld bytes to = %Ld bytes" size_from size_to >:: (fun () -> create_resize_equals_create size_from size_to)
    ) good in
  create @ ok_resize @ bad_resize @ ignore_data_loss_resize @ create_resize_equals_create

let _ =
  Logs.set_reporter (Logs_fmt.reporter ());
  let sector_size = 512 in
  (* Test with a 1 PiB disk, bigger than we'll need for a while. *)
  let size_sectors = Int64.div pib 512L in
  let cluster_bits = 16 in
  let interesting_native_reads = List.map
      (fun (label, start, length) -> label >:: write_read_native sector_size size_sectors (start, Int64.to_int length))
      (interesting_ranges sector_size size_sectors cluster_bits) in
  let interesting_native_discards = List.map
      (fun (label, start, length) -> label >:: write_discard_read_native sector_size size_sectors (start, Int64.to_int length))
      (interesting_ranges sector_size size_sectors cluster_bits) in
  let diet_tests = List.map (fun (name, fn) -> name >:: fn) Qcow_diet.Test.all in
  let bitmap_tests = List.map (fun (name, fn) -> name >:: fn) Qcow_bitmap.Test.all in
  let suite = "qcow2" >::: (diet_tests @ bitmap_tests @ [
      "check we can fill the disk" >:: check_full_disk;
      "check we can reallocate the refcount table" >:: check_refcount_table_allocation;
      "create 1K" >:: create_1K;
      "create 1M" >:: create_1M;
      "create 1P" >:: create_1P;
      "compact" >:: create_write_discard_compact;
      "discard all then compact 0L" >:: create_write_discard_all_compact 0L;
      "discard all then compact 1L" >:: create_write_discard_all_compact 1L;
      "discard all then compact 2L" >:: create_write_discard_all_compact 2L;
      "discard all then compact 16384L" >:: create_write_discard_all_compact 16384L;
    ] @ interesting_native_reads @ interesting_native_discards @ qemu_img_suite @ qcow_tool_suite) in
  OUnit2.run_test_tt_main (ounit2_of_ounit1 suite);
  (* If no error, delete the directory *)
  ignore(run "rm" [ "-rf"; test_dir ])
