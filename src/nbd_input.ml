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

let get_extents_json ~extent_reader ~server ~export_name =
  Lwt_process.pread
    ("", [|extent_reader; "--path"; server; "--exportname"; export_name|])

let raw ?(extent_reader="/opt/xensource/libexec/get_nbd_extents.py") raw server export_name =
  get_extents_json ~extent_reader ~server ~export_name >>= fun extents_json ->
  let extents = extent_list_of_rpc (Jsonrpc.of_string extents_json) in
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
    | [] -> List.rev acc
  in
  let ops = operations extents 0L [] in
  let total = List.fold_left (fun acc e -> Int64.add acc e.length) 0L extents in

  let rec block ops =
    match ops with
    | [] -> Lwt.return F.End
    | op::ops -> Lwt.return (F.Cons (op, fun () -> block ops))
  in
  block ops >>= fun elements ->
  let size = Vhd_format.F.{ total; metadata = 0L; empty = 0L; copy = 0L } in
  Lwt.return F.{elements; size}
