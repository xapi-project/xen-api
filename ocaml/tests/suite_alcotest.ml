
let () =
  Suite_init.harness_init ();
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Travis *)
  Debug.log_to_stdout ();

  Alcotest.run "Base suite"
    [ "Test_valid_ref_list", Test_valid_ref_list.test
    ; "Test_sdn_controller", Test_sdn_controller.test
    ; "Test_pci_helpers", Test_pci_helpers.test
    ; "Test_vdi_allowed_operations", Test_vdi_allowed_operations.test
    ; "Test_sr_allowed_operations", Test_sr_allowed_operations.test
    ; "Test_vm_migrate", Test_vm_migrate.test
    ; "Test_no_migrate", Test_no_migrate.test
    ; "Test_vm_check_operation_error", Test_vm_check_operation_error.test
    ; "Test_vm_helpers", Test_vm_helpers.test
    ; "Test_xapi_vbd_helpers", Test_xapi_vbd_helpers.test
    ; "Test_host", Test_host.test
    ; "Test_host_helpers", Test_host_helpers.test
    ; "Test_vdi_cbt", Test_vdi_cbt.test
    ; "Test_sr_update_vdis", Test_sr_update_vdis.test
    ; "Test_xapi_db_upgrade", Test_xapi_db_upgrade.test
    ; "Test_db_lowlevel", Test_db_lowlevel.test
    ; "Test_vlan", Test_vlan.test
    ; "Test_network", Test_network.test
    ; "Test_network_sriov", Test_network_sriov.test
    ; "Test_bond", Test_bond.test
    ; "Test_tunnel", Test_tunnel.test
    ; "Test_agility", Test_agility.test
    ; "Test_daemon_manager", Test_daemon_manager.test
    ; "Test_cluster", Test_cluster.test
    ; "Test_cluster_host", Test_cluster_host.test
    ; "Test_clustering", Test_clustering.test
    ; "Test_clustering_allowed_operations", Test_clustering_allowed_operations.test
    ; "Test_client", Test_client.test
    ; "Test_ca91480", Test_ca91480.test
    ; "Test_pgpu", Test_pgpu.test
    ; "Test_gpu_group", Test_gpu_group.test
    ; "Test_pool_apply_edition", Test_pool_apply_edition.test
    ; "Test_pool_update", Test_pool_update.test
    ; "Test_pool_db_backup", Test_pool_db_backup.test
    ; "Test_pool_restore_database", Test_pool_restore_database.test
    ; "Test_workload_balancing", Test_workload_balancing.test
    ; "Test_pusb", Test_pusb.test
    ; "Test_pvs_site", Test_pvs_site.test
    ; "Test_pvs_proxy", Test_pvs_proxy.test
    ; "Test_pvs_server", Test_pvs_server.test
    ; "Test_event", Test_event.test
    ; "Test_vm_placement", Test_vm_placement.test
    ; "Test_vm_memory_constraints", Test_vm_memory_constraints.test
    ; "Test_xapi_xenops", Test_xapi_xenops.test
    ; "Test_network_event_loop", Test_network_event_loop.test
    ]

