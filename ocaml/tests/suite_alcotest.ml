
let () =
  Suite_init.harness_init ();
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Traivs *)
  Debug.log_to_stdout ();

  Alcotest.run "Base suite"
    [ "Test_valid_ref_list", Test_valid_ref_list.test
    ; "Test_vdi_allowed_operations", Test_vdi_allowed_operations.test
    ; "Test_vm_migrate", Test_vm_migrate.test
    ; "Test_no_migrate", Test_no_migrate.test
    ; "Test_vm_check_operation_error", Test_vm_check_operation_error.test
    ; "Test_host", Test_host.test
    ; "Test_vdi_cbt", Test_vdi_cbt.test
    ; "Test_xapi_db_upgrade", Test_xapi_db_upgrade.test
    ; "Test_db_lowlevel", Test_db_lowlevel.test
    ; "Test_vlan", Test_vlan.test
    ; "Test_network", Test_network.test
    ; "Test_agility", Test_agility.test
    ; "Test_daemon_manager", Test_daemon_manager.test
    ; "Test_cluster", Test_cluster.test
    ; "Test_cluster_host", Test_cluster_host.test
    ; "Test_client", Test_client.test
    ; "Test_ca91480", Test_ca91480.test
    ; "Test_pgpu", Test_pgpu.test
    ; "Test_gpu_group", Test_gpu_group.test
    ; "Test_pool_apply_edition", Test_pool_apply_edition.test
    ; "Test_pool_update", Test_pool_update.test
    ; "Test_pusb", Test_pusb.test
    ; "Test_pvs_site", Test_pvs_site.test
    ; "Test_pvs_proxy", Test_pvs_proxy.test
    ; "Test_clustering", Test_clustering.test
    ; "Test_clustering_allowed_operations", Test_clustering_allowed_operations.test
    ; "Test_event", Test_event.test
    ; "Test_vm_placement", Test_vm_placement.test
    ; "Test_vm_memory_constraints", Test_vm_memory_constraints.test
    ; "Test_network_event_loop", Test_network_event_loop.test
    ]

