open OUnit2

let assert_equal_int = assert_equal ~printer:string_of_int

let ds_a = Ds.ds_make ~name:"ds_a" ~units:"(fraction)"
    ~description:"datasource a"
    ~value:(Rrd.VT_Float 1.0)
    ~ty:Rrd.Gauge ~default:true ()

let ds_b = Ds.ds_make ~name:"ds_b" ~units:"(fraction)"
    ~description:"datasource b"
    ~value:(Rrd.VT_Float 2.0)
    ~ty:Rrd.Gauge ~default:true ()

let reset_rrdd_shared_state _ctxt =
  Hashtbl.clear Rrdd_shared.vm_rrds;
  Rrdd_shared.host_rrd := None

let dump_dss = List.map (fun ds -> ds.Ds.ds_name)

let dump_rrd_hash hash =
  Hashtbl.fold
    (fun k v acc -> (k, dump_dss v.Rrdd_shared.dss) :: acc)
    hash []

let string_of_rrd_dump dump =
  let rrds = List.map
      (fun (k, v) ->  Printf.sprintf "(%s, [%s])" k (String.concat "; " v))
      dump
  in
  Printf.sprintf "[%s]" (String.concat "; " rrds)

let check_rrd_hash actual_rrds expected_rrds =
  assert_equal ~printer:string_of_rrd_dump
    (List.sort compare expected_rrds)
    (List.sort compare (dump_rrd_hash actual_rrds))

let check_host_dss expected_dss =
  match !Rrdd_shared.host_rrd with
  | None -> assert_failure "host_rrd should have been created"
  | Some info ->
    assert_equal ~printer:(String.concat "; ")
      (List.sort compare expected_dss)
      (List.sort compare (dump_dss info.Rrdd_shared.dss))

let update_rrds_test ~dss ~uuid_domids ~paused_vms
    ~expected_vm_rrds ~expected_sr_rrds ~expected_host_dss = fun ctxt ->
  OUnit2.bracket reset_rrdd_shared_state (fun () -> ignore) ctxt;
  Rrdd_monitor.update_rrds 12345.0 dss uuid_domids paused_vms;
  check_rrd_hash Rrdd_shared.vm_rrds expected_vm_rrds;
  check_rrd_hash Rrdd_shared.sr_rrds expected_sr_rrds;
  check_host_dss expected_host_dss

let update_rrds = "update_rrds" >::: let open Rrd in [
    "Null update" >:: update_rrds_test
      ~dss:[]
      ~uuid_domids:[]
      ~paused_vms:[]
      ~expected_vm_rrds:[]
      ~expected_sr_rrds:[]
      ~expected_host_dss:[];

    "Single host update" >:: update_rrds_test
      ~dss:[(Host, ds_a)]
      ~uuid_domids:[]
      ~paused_vms:[]
      ~expected_vm_rrds:[]
      ~expected_sr_rrds:[]
      ~expected_host_dss:["ds_a"];

    "Multiple host updates" >:: update_rrds_test
      ~dss:[(Host, ds_a); (Host, ds_a)]
      ~uuid_domids:[]
      ~paused_vms:[]
      ~expected_vm_rrds:[]
      ~expected_sr_rrds:[]
      ~expected_host_dss:["ds_a"; "ds_a"];

    "Single non-resident VM update" >:: update_rrds_test
      ~dss:[(VM "a", ds_a)]
      ~uuid_domids:[]
      ~paused_vms:[]
      ~expected_vm_rrds:[]
      ~expected_sr_rrds:[]
      ~expected_host_dss:[];

    "Multiple non-resident VM updates" >:: update_rrds_test
      ~dss:[(VM "a", ds_a); (VM "b", ds_a)]
      ~uuid_domids:[]
      ~paused_vms:[]
      ~expected_vm_rrds:[]
      ~expected_sr_rrds:[]
      ~expected_host_dss:[];

    "Single resident VM update" >:: update_rrds_test
      ~dss:[(VM "a", ds_a)]
      ~uuid_domids:[("a", 1)]
      ~paused_vms:[]
      ~expected_vm_rrds: ["a", ["ds_a"]]
      ~expected_sr_rrds:[]
      ~expected_host_dss:[];

    "Multiple resident VM updates" >:: update_rrds_test
      ~dss:[(VM "a", ds_a); (VM "b", ds_a); (VM "b", ds_b)]
      ~uuid_domids:[("a", 1); ("b", 1)]
      ~paused_vms:[]
      ~expected_vm_rrds:["a", ["ds_a"]; "b", ["ds_a"; "ds_b"]]
      ~expected_sr_rrds:[]
      ~expected_host_dss:[];

    "Multiple resident and non-resident VM updates" >:: update_rrds_test
      ~dss:[(VM "a", ds_a); (VM "b", ds_a); (VM "c", ds_a)]
      ~uuid_domids:[("a", 1); ("b", 1)]
      ~paused_vms:[]
      ~expected_vm_rrds:["a", ["ds_a"]; "b", ["ds_a"]]
      ~expected_sr_rrds:[]
      ~expected_host_dss:[];

    "Multiple SR updates" >:: update_rrds_test
      ~dss:[(SR "a", ds_a); (SR "b", ds_a); (SR "b", ds_b)]
      ~uuid_domids:[]
      ~paused_vms:[]
      ~expected_vm_rrds:[]
      ~expected_sr_rrds:["a", ["ds_a"]; "b", ["ds_a"; "ds_b"]]
      ~expected_host_dss:[];
  ]

let suite = "rrdd monitor test" >:::
            [
              update_rrds
            ]

let () =
  run_test_tt_main suite
