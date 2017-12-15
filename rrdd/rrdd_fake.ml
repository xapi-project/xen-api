open Ds

open Rrdd_fake_common

module D = Debug.Make(struct let name="rrdd_fake" end)
open D

let read_fakestats fname ty =
  try
    let file =  Xapi_stdext_unix.Unixext.string_of_file fname in
    let ds_list = fake_ds_list_of_rpc (Jsonrpc.of_string file) in
    debug "Got %d fake data source(s) from filename %s" (List.length ds_list) fname;
    List.map (fun fake_ds ->
        (ty, ds_make ~name:fake_ds.f_name ~units:"" ~description:""
           ~value:(Rrd.VT_Float fake_ds.f_val) ~ty:fake_ds.f_ty ~default:true ())
      ) (fake_ds_list_of_rpc (Jsonrpc.of_string file))
  with _ ->
    []

let collect_fake_stats uuids =
  let host_stats =
    read_fakestats (Printf.sprintf "%s/host.fakestats" fake_dir) Rrd.Host in
  host_stats :: (List.map (fun uuid -> read_fakestats (Printf.sprintf "%s/%s.fakestats" fake_dir uuid) (Rrd.VM uuid)) uuids)

let get_fake_stats uuids =
  let fake_dir_exists =
    try
      let st = Unix.stat fake_dir in
      st.Unix.st_kind = Unix.S_DIR
    with _ -> false
  in
  if fake_dir_exists then List.concat (collect_fake_stats uuids) else []

let combine_stats real fake =
  (* Remove all of the real DSs that are shadowed by fake dss *)
  let real2 = List.fold_left (fun acc (fake_ds_x, fake_ds_ds) ->
      List.filter (fun (x, ds) -> x = fake_ds_x && ds.ds_name <> fake_ds_ds.ds_name) acc
    ) real fake in
  real2 @ fake
