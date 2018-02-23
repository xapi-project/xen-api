(* Dump API request/response *)

module Impl : Rrd_interface.Server_impl with type context = unit = struct
  type context = unit
  let has_vm_rrd () ~vm_uuid:_ = true
  let push_rrd_local () ~vm_uuid:_ ~domid:_ = ()
  let push_rrd_remote () ~vm_uuid:_ ~remote_address:_ = ()
  let remove_rrd () ~uuid:_ = ()
  let migrate_rrd () ?session_id:_ ~remote_address:_ ~vm_uuid:_ ~host_uuid:_ = ()
  let send_host_rrd_to_master () ~master_address:_ = ()
  let backup_rrds () ?remote_address:_ () = ()
  let archive_rrd () ~vm_uuid:_ ~remote_address:_ = ()
  let archive_sr_rrd () ~sr_uuid:_ = ""
  let push_sr_rrd () ~sr_uuid:_ ~path:_ = ()
  let add_host_ds () ~ds_name:_ = ()
  let forget_host_ds () ~ds_name:_ = ()
  let query_possible_host_dss () () = [Data_source.{name="name"; description=""; enabled=true; standard=true; min=0.0; max=1.0; units="units"}]
  let query_host_ds () ~ds_name:_ = 0.0
  let add_vm_ds () ~vm_uuid:_ ~domid:_ ~ds_name:_ = ()
  let forget_vm_ds () ~vm_uuid:_ ~ds_name:_ = ()
  let query_possible_vm_dss () ~vm_uuid:_ = []
  let query_vm_ds () ~vm_uuid:_ ~ds_name:_ = 0.0
  let add_sr_ds () ~sr_uuid:_ ~ds_name:_ = ()
  let forget_sr_ds () ~sr_uuid:_ ~ds_name:_ = ()
  let query_possible_sr_dss () ~sr_uuid:_ = []
  let query_sr_ds () ~sr_uuid:_ ~ds_name:_ = 0.0
  let update_use_min_max () ~value:_ = ()
  let update_vm_memory_target () ~domid:_ ~target:_ = ()
  let set_cache_sr () ~sr_uuid:_ = ()
  let unset_cache_sr () () = ()

  module Plugin = struct
    let get_header () () = ""
    let get_path () ~uid:_ = ""

    module Local = struct
      let register () ~uid:_ ~info:_ ~protocol:_ = 0.0
      let deregister () ~uid:_ = ()
      let next_reading () ~uid:_ = 0.0
    end

    module Interdomain = struct
      let register () ~uid:_ ~info:_ ~protocol:_ = 0.0
      let deregister () ~uid:_ = ()
      let next_reading () ~uid:_ = 0.0
    end

    let register () ~uid:_ ~frequency:_ = 0.0
    let deregister () ~uid:_ = ()
    let next_reading () ~uid:_ = 0.0
  end

  module HA = struct
    let enable_and_update () ~statefile_latencies:_ ~heartbeat_latency:_ ~xapi_latency:_ = ()
    let disable () () = ()
  end

  module Deprecated = struct
    let load_rrd () ~uuid:_ ~timescale:_ ~master_address:_ = ()
  end
end

module S = Rrd_interface.Server(Impl)

let write_str filename str =
  let oc = open_out filename in
  Printf.fprintf oc "%s" str;
  close_out oc

let read_str filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let dumping_rpc call =
  let request_str = Xmlrpc.string_of_call call in
  let response = S.process () call in
  let response_str = Xmlrpc.string_of_response response in
  write_str (Printf.sprintf "rpc_data/%s.request" call.Rpc.name) request_str;
  write_str (Printf.sprintf "rpc_data/%s.response" call.Rpc.name) response_str;
  response

let run_all rpc =
  let module C = Rrd_interface.Client(struct let rpc = rpc end) in
  begin try Unix.mkdir "rpc_data" 0o755 with Unix.(Unix_error (EEXIST, _, _)) -> () end;
  ignore(C.has_vm_rrd ~vm_uuid:"abcde");
  ignore(C.push_rrd_local ~vm_uuid:"abcde" ~domid:1);
  ignore(C.push_rrd_remote ~vm_uuid:"abcde" ~remote_address:"127.0.0.1");
  ignore(C.remove_rrd ~uuid:"abcde");
  ignore(C.migrate_rrd ~session_id:"session" ~remote_address:"127.0.0.1" ~vm_uuid:"abcde" ~host_uuid:"12345");
  ignore(C.send_host_rrd_to_master ~master_address:"127.0.0.2");
  ignore(C.backup_rrds ~remote_address:(Some "127.0.0.1") ());
  ignore(C.archive_rrd ~vm_uuid:"abcde" ~remote_address:(Some "127.0.0.1"));
  ignore(C.archive_sr_rrd ~sr_uuid:"abcde");
  ignore(C.push_sr_rrd ~sr_uuid:"abcde" ~path:"/foo");
  ignore(C.add_host_ds ~ds_name:"ds");
  ignore(C.forget_host_ds ~ds_name:"ds");
  ignore(C.query_possible_host_dss ());
  ignore(C.query_host_ds ~ds_name:"ds");
  ignore(C.add_vm_ds ~vm_uuid:"abcde" ~domid:1 ~ds_name:"ds");
  ignore(C.forget_vm_ds ~vm_uuid:"abcde" ~ds_name:"ds");
  ignore(C.query_possible_vm_dss ~vm_uuid:"abcde");
  ignore(C.query_vm_ds ~vm_uuid:"abcde" ~ds_name:"ds");
  ignore(C.add_sr_ds ~sr_uuid:"abcde" ~ds_name:"ds");
  ignore(C.forget_sr_ds ~sr_uuid:"abcde" ~ds_name:"ds");
  ignore(C.query_possible_sr_dss ~sr_uuid:"abcde");
  ignore(C.query_sr_ds ~sr_uuid:"abcde" ~ds_name:"ds");
  ignore(C.update_use_min_max ~value:true);
  ignore(C.update_vm_memory_target ~domid:1 ~target:1L);
  ignore(C.set_cache_sr ~sr_uuid:"abcde");
  ignore(C.unset_cache_sr ());
  ignore(C.Plugin.get_header ());
  ignore(C.Plugin.get_path ~uid:"uid");
  ignore(C.Plugin.Local.register ~uid:"uid" ~info:Rrd.Five_Seconds ~protocol:Rrd_interface.V1);
  ignore(C.Plugin.Local.deregister ~uid:"uid");
  ignore(C.Plugin.Local.next_reading ~uid:"uid");
  ignore(C.Plugin.Interdomain.register ~uid:(Rrd_interface.{name="uid"; frontend_domid=1}) ~info:(Rrd_interface.{frequency=Rrd.Five_Seconds; shared_page_refs=[1]}) ~protocol:Rrd_interface.V1);
  ignore(C.Plugin.Interdomain.deregister ~uid:(Rrd_interface.{name="uid"; frontend_domid=1}));
  ignore(C.Plugin.Interdomain.next_reading ~uid:(Rrd_interface.{name="uid"; frontend_domid=1}));
  ignore(C.Plugin.register ~uid:"uid" ~frequency:Rrd.Five_Seconds);
  ignore(C.Plugin.deregister ~uid:"uid");
  ignore(C.Plugin.next_reading ~uid:"uid");
  ignore(C.HA.enable_and_update ~statefile_latencies:[{id="statefile_latency"; latency=Some 1.0}] ~heartbeat_latency:1.0 ~xapi_latency:1.0);
  ignore(C.HA.disable ());
  ignore(C.Deprecated.load_rrd ~uuid:"abcde" ~timescale:1 ~master_address:(Some "127.0.0.1"))

let dump_all () =
  run_all dumping_rpc

let test_old_requests () =
  let all_requests =
    let dir = "test_data/rrd/requests" in
    Sys.readdir dir |>
    Array.to_list |>
    List.map (fun f -> read_str ("test_data/rrd/requests/" ^ f))
  in
  List.iter (fun request ->
    let call = Xmlrpc.call_of_string request in
    let response = S.process () call in
    OUnit.assert_bool (Printf.sprintf "Result of '%s' successful" call.Rpc.name) (response.Rpc.success)) all_requests

let test_old_responses () =
  let new_rpc call =
    let name = call.Rpc.name in
    Xmlrpc.response_of_string (read_str (Printf.sprintf "test_data/rrd/responses/%s.response" name))
  in
  run_all new_rpc

open OUnit

let tests =
  "rrd_dump" >:::
    [
      "Check interface works" >:: dump_all;
      "Test old requests" >:: test_old_requests;
      "Test old responses" >:: test_old_responses;
    ]
