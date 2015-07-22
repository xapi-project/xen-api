open OUnit2

let ds_a = Ds.ds_make ~name:"datasource_a" ~units:"(fraction)"
	~description:"datasource_a"
	~value:(Rrd.VT_Float 1.0)
	~ty:Rrd.Gauge ~default:true ()

let reset_rrdd_shared_state ctxt =
        Hashtbl.clear Rrdd_shared.vm_rrds;
        Rrdd_shared.host_rrd := None

let update_rrds_test dss uuid_domids paused_vms
	num_vm_rrds num_host_dss =
	fun ctxt ->
	OUnit2.bracket reset_rrdd_shared_state (fun () -> ignore) ctxt;
	Rrdd_monitor.update_rrds 12345.0 dss uuid_domids paused_vms;
	assert_equal num_vm_rrds (Hashtbl.length Rrdd_shared.vm_rrds);
	match !Rrdd_shared.host_rrd with
	| None -> assert_failure "host_rrd should have been created"
	| Some info ->
		assert_equal num_host_dss (List.length Rrdd_shared.(info.dss))

let update_rrds = "update_rrds" >::: let open Rrd in [
	("Null update" >::
		update_rrds_test [] [] [] 0 0);

	("Single host update" >::
		update_rrds_test [(Host, ds_a)] [] [] 0 1);

	("Multiple host updates" >::
		update_rrds_test [(Host, ds_a); (Host, ds_a)] [] [] 0 2);

	("Single non-resident VM update" >::
		update_rrds_test [(VM "a", ds_a)] [] [] 0 0);

	("Multiple non-resident VM updates" >::
		update_rrds_test [(VM "a", ds_a); (VM "b", ds_a)] [] [] 0 0);

	("Single resident VM update" >::
		update_rrds_test [(VM "a", ds_a)] [("a", 1)] [] 1 0);

	("Multiple resident VM updates" >::
		update_rrds_test [(VM "a", ds_a); (VM "b", ds_a)] [("a", 1); ("b", 1)] [] 2 0);
]

let suite = "rrdd monitor test" >:::
	[
		update_rrds
	]

let () =
	run_test_tt_main suite
