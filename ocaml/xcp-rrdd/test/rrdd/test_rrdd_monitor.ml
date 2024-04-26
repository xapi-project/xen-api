let ds_a =
  Ds.ds_make ~name:"ds_a" ~units:"(fraction)" ~description:"datasource a"
    ~value:(Rrd.VT_Float 1.0) ~ty:Rrd.Gauge ~default:true ()

let ds_b =
  Ds.ds_make ~name:"ds_b" ~units:"(fraction)" ~description:"datasource b"
    ~value:(Rrd.VT_Float 2.0) ~ty:Rrd.Gauge ~default:true ()

let reset_rrdd_shared_state () =
  Hashtbl.clear Rrdd_shared.vm_rrds ;
  Rrdd_shared.host_rrd := None

let pp_ds =
  Fmt.(
    Dump.record
      [
        Dump.field "ds_name" (fun t -> t.Ds.ds_name) string
      ; Dump.field "ds_description" (fun t -> t.Ds.ds_description) string
      ; Dump.field "ds_default" (fun t -> t.Ds.ds_default) bool
      ; Dump.field "ds_min" (fun t -> t.Ds.ds_min) float
      ; Dump.field "ds_max" (fun t -> t.Ds.ds_max) float
      ; Dump.field "ds_units" (fun t -> t.Ds.ds_units) string
      ]
  )

let ds = Alcotest.testable pp_ds (fun a b -> String.equal a.ds_name b.ds_name)

let dss_of_rrds rrds =
  Hashtbl.fold (fun k v acc -> (k, v.Rrdd_shared.dss) :: acc) rrds []
  |> List.fast_sort Stdlib.compare

let check_datasources kind rdds expected_dss =
  match rdds with
  | None when expected_dss <> [] ->
      Alcotest.fail (Printf.sprintf "%s RRD must be created" kind)
  | None ->
      ()
  | Some actual_rdds ->
      let actual_dss = dss_of_rrds actual_rdds in
      let expected_dss = List.fast_sort Stdlib.compare expected_dss in
      Alcotest.(check @@ list @@ pair string (list ds))
        (Printf.sprintf "%s rrds are not expected" kind)
        actual_dss expected_dss

let host_rrds rrd_info =
  Option.bind rrd_info @@ fun rrd_info ->
  let h = Hashtbl.create 1 in
  if rrd_info.Rrdd_shared.dss <> [] then
    Hashtbl.add h "host" rrd_info ;
  Some h

let update_rrds_test ~dss ~uuid_domids ~paused_vms ~expected_vm_rrds
    ~expected_sr_rrds ~expected_host_dss =
  let test () =
    reset_rrdd_shared_state () ;
    Rrdd_monitor.update_rrds 12345.0 (List.to_seq dss) uuid_domids paused_vms ;
    check_datasources "VM" (Some Rrdd_shared.vm_rrds) expected_vm_rrds ;
    check_datasources "SR" (Some Rrdd_shared.sr_rrds) expected_sr_rrds ;
    check_datasources "Host" (host_rrds !Rrdd_shared.host_rrd) expected_host_dss
  in
  [("", `Quick, test)]

let update_rrds =
  let open Rrd in
  [
    ( "Null update"
    , update_rrds_test ~dss:[] ~uuid_domids:[] ~paused_vms:[]
        ~expected_vm_rrds:[] ~expected_sr_rrds:[] ~expected_host_dss:[]
    )
  ; ( "Single host update"
    , update_rrds_test
        ~dss:[(Host, ds_a)]
        ~uuid_domids:[] ~paused_vms:[] ~expected_vm_rrds:[] ~expected_sr_rrds:[]
        ~expected_host_dss:[("host", [ds_a])]
    )
  ; ( "Multiple host updates"
    , update_rrds_test
        ~dss:[(Host, ds_a); (Host, ds_b)]
        ~uuid_domids:[] ~paused_vms:[] ~expected_vm_rrds:[] ~expected_sr_rrds:[]
        ~expected_host_dss:[("host", [ds_a; ds_b])]
    )
  ; ( "Single non-resident VM update"
    , update_rrds_test
        ~dss:[(VM "a", ds_a)]
        ~uuid_domids:[] ~paused_vms:[] ~expected_vm_rrds:[] ~expected_sr_rrds:[]
        ~expected_host_dss:[]
    )
  ; ( "Multiple non-resident VM updates"
    , update_rrds_test
        ~dss:[(VM "a", ds_a); (VM "b", ds_a)]
        ~uuid_domids:[] ~paused_vms:[] ~expected_vm_rrds:[] ~expected_sr_rrds:[]
        ~expected_host_dss:[]
    )
  ; ( "Single resident VM update"
    , update_rrds_test
        ~dss:[(VM "a", ds_a)]
        ~uuid_domids:[("a", 1)]
        ~paused_vms:[]
        ~expected_vm_rrds:[("a", [ds_a])]
        ~expected_sr_rrds:[] ~expected_host_dss:[]
    )
  ; ( "Multiple resident VM updates"
    , update_rrds_test
        ~dss:[(VM "a", ds_a); (VM "b", ds_a); (VM "b", ds_b)]
        ~uuid_domids:[("a", 1); ("b", 1)]
        ~paused_vms:[]
        ~expected_vm_rrds:[("a", [ds_a]); ("b", [ds_a; ds_b])]
        ~expected_sr_rrds:[] ~expected_host_dss:[]
    )
  ; ( "Multiple resident and non-resident VM updates"
    , update_rrds_test
        ~dss:[(VM "a", ds_a); (VM "b", ds_a); (VM "c", ds_a)]
        ~uuid_domids:[("a", 1); ("b", 1)]
        ~paused_vms:[]
        ~expected_vm_rrds:[("a", [ds_a]); ("b", [ds_a])]
        ~expected_sr_rrds:[] ~expected_host_dss:[]
    )
  ; ( "Multiple SR updates"
    , update_rrds_test
        ~dss:[(SR "a", ds_a); (SR "b", ds_a); (SR "b", ds_b)]
        ~uuid_domids:[] ~paused_vms:[] ~expected_vm_rrds:[]
        ~expected_sr_rrds:[("a", [ds_a]); ("b", [ds_a; ds_b])]
        ~expected_host_dss:[]
    )
  ]

let () = Alcotest.run "RRD daemon monitor test" update_rrds
