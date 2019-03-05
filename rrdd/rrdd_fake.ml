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


(** [override_stats test overridable overrider] is the tuple of [overridable]
    list with items replaced from [overrider] that pass the [test]; and the
    list of items from [overrider] that failed the [test].
    Each list produced maintains the order present in their list of origin *)
let override_stats test overridable overrider =
  List.fold_left
    (fun (filtered, all_unused) (owner, ds) ->
      let replacements, unused = List.partition (test (owner, ds)) all_unused in
      if List.length replacements = 0 then
        filtered @ [(owner, ds)], unused
      else
        filtered @ replacements, unused
    ) ([], overrider) overridable

(* Returns the list real with all the elements from fake added; without the
 * elements of real that have a matching element in fake with the same owner,
 * but a different ds name.
 *)
let combine_stats real fake =
  let test (owner, ds) =
    fun (over_o, over_ds) -> owner = over_o && ds.ds_name <> over_ds.ds_name in
  let combined, extra = override_stats test real fake in
  combined @ extra
