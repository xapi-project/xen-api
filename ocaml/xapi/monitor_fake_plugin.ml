open Monitor_fake_common

exception UnknownRpc


let add_fake_ds fname ds_name ds_type value =
  let value = float_of_string value in
  let ty = match ds_type with
    | "absolute" -> Rrd.Absolute
    | "gauge" -> Rrd.Gauge
    | "derive" -> Rrd.Derive
    | _ -> failwith "Unknown ds type"
  in
  Unixext.mkdir_rec fake_dir 0o755;
  let orig_dss =
    try
      fake_ds_list_of_rpc (Jsonrpc.of_string (Unixext.string_of_file fname))
    with _ -> []
  in
  let new_ds = { f_name=ds_name; f_ty=ty; f_val=value } in
  let new_dss = new_ds::(List.filter (fun ds -> ds.f_name <> ds_name) orig_dss) in
  Unixext.write_string_to_file fname (Jsonrpc.to_string (rpc_of_fake_ds_list new_dss))

let add_fake_ds_vm uuid =
  let fname = Printf.sprintf "%s/%s.fakestats" fake_dir uuid in
  add_fake_ds fname

let add_fake_ds_host =
  let fname = Printf.sprintf "%s/host.fakestats" fake_dir in
  add_fake_ds fname

let _ =
  try
    let oc = open_out "/tmp/foo" in
    Printf.fprintf oc "%s" Sys.argv.(1);
    let call = Xmlrpc.call_of_string Sys.argv.(1) in
    let host = Rpc.string_of_rpc (List.hd call.Rpc.params) in
    let oc = open_out "/tmp/foo2" in
    Printf.fprintf oc "%s" host;
    let args = match (List.hd (List.tl call.Rpc.params)) with
      | Rpc.Dict args ->
        args
      | _ ->
        failwith "Can't parse args"
    in
    let oc = open_out "/tmp/foo3" in
    List.iter (fun (a,b) -> Printf.fprintf oc "%s: %s" a (Rpc.string_of_rpc b)) args;
    let contents =
      match call.Rpc.name with
      | "add_fake_ds" ->
        let uuid = Rpc.string_of_rpc (List.assoc "uuid" args) in
        let ds_name = Rpc.string_of_rpc (List.assoc "ds_name" args) in
        let ds_type = Rpc.string_of_rpc (List.assoc "ds_type" args) in
        let value = Rpc.string_of_rpc (List.assoc "value" args) in
        add_fake_ds_vm uuid ds_name ds_type value;
        Rpc.rpc_of_string "OK"
      | "add_fake_ds_host" ->
        let ds_name = Rpc.string_of_rpc (List.assoc "ds_name" args) in
        let ds_type = Rpc.string_of_rpc (List.assoc "ds_type" args) in
        let value = Rpc.string_of_rpc (List.assoc "value" args) in
        add_fake_ds_host ds_name ds_type value;
        Rpc.rpc_of_string "OK"
      | _ ->
        raise UnknownRpc
    in
    Printf.printf "%s" (Xmlrpc.string_of_response {Rpc.success=true; contents=contents});
    exit 0
  with
  | UnknownRpc ->
    Printf.printf "%s" (Xmlrpc.string_of_response {Rpc.success=false; contents=Rpc.rpc_of_string "Unknown RPC"});
    exit 1
  | _ ->
    Printf.printf "%s" (Xmlrpc.string_of_response {Rpc.success=false; contents=Rpc.rpc_of_string "Internal error"});
    exit 1
