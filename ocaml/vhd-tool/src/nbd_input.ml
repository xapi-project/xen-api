module F = Vhd_format.F.From_file (Vhd_format_lwt.IO)
open Lwt.Infix

type extent = {flags: int32; length: int64} [@@deriving rpc]

(* The flags returned for the base:allocation NBD metadata context are defined here:
   https://github.com/NetworkBlockDevice/nbd/blob/extension-blockstatus/doc/proto.md#baseallocation-metadata-context *)
let flag_hole = 1l

let flag_zero = 2l

type extent_list = extent list [@@deriving rpc]

(** We query the block status for an area of up to 1GiB at a time, to avoid
    excessive memory usage when marshalling/unmarshalling the JSON containing
    the extent list. *)
let max_query_length = Int64.(mul 1024L (mul 1024L 1024L))

let min a b = if Int64.compare a b < 0 then a else b

(** The extents returned by this Python script must be consecutive,
    non-overlapping, in the correct order starting from the specified offset,
    and must exactly cover the requested area. *)
let get_extents_json ~extent_reader ~server ~export_name ~offset ~length =
  Lwt_process.pread
    ( ""
    , [|
         extent_reader
       ; "--path"
       ; server
       ; "--exportname"
       ; export_name
       ; "--offset"
       ; Int64.to_string offset
       ; "--length"
       ; Int64.to_string length
      |]
    )

let is_empty e =
  let has_flag flag = Int32.logand e.flags flag = flag in
  (* We assume the destination is prezeroed, so we do not have to copy zeroed extents *)
  has_flag flag_hole || has_flag flag_zero

let assert_integer_sectors b =
  if Int64.rem b 512L <> 0L then failwith "Expecting sector aligned extents"

let raw ?(extent_reader = "/opt/xensource/libexec/get_nbd_extents.py") raw
    server export_name size =
  let to_sectors b = Int64.div b 512L in
  let rec operations extents offset acc =
    match extents with
    | e :: es ->
        assert_integer_sectors e.length ;
        let op =
          if is_empty e then
            `Empty (to_sectors e.length)
          else
            `Copy (raw, to_sectors offset, to_sectors e.length)
        in
        operations es (Int64.add offset e.length) (op :: acc)
    | [] ->
        (List.rev acc, offset)
  in
  let operations ~offset ~length =
    get_extents_json ~extent_reader ~server ~export_name ~offset ~length
    >>= fun extents_json ->
    let extents = extent_list_of_rpc (Jsonrpc.of_string extents_json) in
    let ops, final_offset = operations extents offset [] in
    ( if final_offset <> Int64.add offset length then
        Lwt.fail_with
          (Printf.sprintf
             "Nbd_input.raw: extents returned for offset=%Ld & length=%Ld \
              finished at incorrect offset %Ld,"
             offset length final_offset
          )
    else
      Lwt.return_unit
    )
    >|= fun () -> ops
  in

  let rec block ops offset =
    match (ops, offset) with
    | [], offset when offset >= size ->
        ( if offset <> size then
            Lwt.fail_with
              (Printf.sprintf
                 "Nbd_input.raw finished with offset=%Ld <> size=%Ld" offset
                 size
              )
        else
          Lwt.return_unit
        )
        >>= fun () -> Lwt.return F.End
    | [], _ ->
        let length = min (Int64.sub size offset) max_query_length in
        operations ~offset ~length >>= fun ops ->
        block ops (Int64.add offset length)
    | op :: ops, _ ->
        Lwt.return (F.Cons (op, fun () -> block ops offset))
  in
  block [] 0L >>= fun elements ->
  let size = Vhd_format.F.{total= size; metadata= 0L; empty= 0L; copy= 0L} in
  Lwt.return F.{elements; size}

let vhd ?(extent_reader = "/opt/xensource/libexec/get_nbd_extents.py") raw
    server export_name size =
  (* All units are bytes *)
  let open Int64 in
  (* Get extents directly from the NBD server through a helper program.
     Each extent has a length and flags that indicate whether or not there is data. *)
  let rec read_extents extents offset =
    if offset > size then
      Lwt.fail_with
        (Printf.sprintf "Nbd_input.vhd finished with offset=%Ld <> size=%Ld"
           offset size
        )
    else if offset = size then
      Lwt.return (List.rev extents)
    else
      let length = min (sub size offset) max_query_length in
      get_extents_json ~extent_reader ~server ~export_name ~offset ~length
      >>= fun extents_json ->
      let new_extents = extent_list_of_rpc (Jsonrpc.of_string extents_json) in
      read_extents (List.rev_append new_extents extents) (add offset length)
  in
  read_extents [] 0L >>= fun extents ->
  let find_data_blocks ~blocks:_ ~block_size =
    (* Given a list of extents and a VHD block size, return a list of block indices
       for all blocks that contain data. *)
    let add_new i l =
      (* Add i to list l, unless the list's head is already equal to i *)
      match l with [] -> [i] | j :: _ when i = j -> l | _ -> i :: l
    in
    let next_offset offset e =
      assert_integer_sectors e.length ;
      add offset e.length
    in
    let rec loop offset acc = function
      | [] ->
          List.rev acc
      | e :: es when is_empty e ->
          (* Empty extent: move on to the next *)
          loop (next_offset offset e) acc es
      | e :: es ->
          (* If the extent is non-empty, then determine which blocks this extent falls
             into and add those to the list. *)
          let offset' = next_offset offset e in
          let acc' =
            let first_block = div offset block_size |> to_int in
            let last_block = div (sub offset' 1L) block_size |> to_int in
            let rec add_blocks i acc' =
              if i <= last_block then
                add_blocks (i + 1) (add_new i acc')
              else
                acc'
            in
            add_blocks first_block acc
          in
          loop offset' acc' es
    in
    Lwt.return (loop 0L [] extents)
  in
  (* Use ocaml-vhd to create a sparse VHD stream from the raw input and
     data block info. *)
  F.Hybrid_raw_input.vhd raw find_data_blocks
