module F = Vhd_format.F.From_file(Vhd_format_lwt.IO)

open Lwt.Infix

type extent = {
  flags : int32;
  length : int64;
} [@@deriving rpc]

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
    ("", [|extent_reader; "--path"; server; "--exportname"; export_name; "--offset"; Int64.to_string offset; "--length"; Int64.to_string length|])

let raw ?(extent_reader="/opt/xensource/libexec/get_nbd_extents.py") raw server export_name size =
  let to_sectors b = Int64.div b 512L in
  let is_empty e =
    let has_flag flag =
      Int32.logand e.flags flag = flag
    in
    (* We assume the destination is prezeroed, so we do not have to copy zeroed extents *)
    (has_flag flag_hole) || (has_flag flag_zero)
  in
  let assert_integer_sectors b =
    if Int64.rem b 512L <> 0L then failwith "Expecting sector aligned extents"
  in
  let rec operations extents offset acc =
    match extents with
    | e::es ->
      assert_integer_sectors e.length;
      let op =
        if is_empty e
        then `Empty (to_sectors e.length)
        else `Copy (raw, to_sectors offset, to_sectors e.length)
      in
      operations es (Int64.add offset e.length) (op::acc)
    | [] -> (List.rev acc, offset)
  in
  let operations ~offset ~length =
    get_extents_json ~extent_reader ~server ~export_name ~offset ~length >>= fun extents_json ->
    let extents = extent_list_of_rpc (Jsonrpc.of_string extents_json) in
    let ops, final_offset = operations extents offset [] in
    (if final_offset <> (Int64.add offset length)
     then Lwt.fail_with (Printf.sprintf "Nbd_input.raw: extents returned for offset=%Ld & length=%Ld finished at incorrect offset %Ld," offset length final_offset)
     else Lwt.return_unit) >|= fun () ->
    ops
  in

  let rec block ops offset =
    match ops, offset with
    | [], offset when offset >= size ->
      (if offset <> size then Lwt.fail_with (Printf.sprintf "Nbd_input.raw finished with offset=%Ld <> size=%Ld" offset size)
       else Lwt.return_unit) >>= fun () ->
      Lwt.return F.End
    | [], _ ->
      let length = min (Int64.sub size offset) max_query_length in
      operations ~offset ~length >>= fun ops ->
      block ops (Int64.add offset length)
    | op::ops, _ -> Lwt.return (F.Cons (op, fun () -> block ops offset))
  in
  block [] 0L >>= fun elements ->
  let size = Vhd_format.F.{ total = size; metadata = 0L; empty = 0L; copy = 0L } in
  Lwt.return F.{elements; size}
