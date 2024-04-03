let () =
  Suite_init.harness_init () ;
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Travis *)
  Debug.log_to_stdout () ;
  Alcotest.run "Base suite"
    ([
       ("Test_valid_ref_list", Test_valid_ref_list.test)
     ; ("Test_sdn_controller", Test_sdn_controller.test)
     ; ("Test_pci_helpers", Test_pci_helpers.test)
     ; ("Test_vdi_allowed_operations", Test_vdi_allowed_operations.test)
     ; ("Test_sr_allowed_operations", Test_sr_allowed_operations.test)
     ; ("Test_vm_migrate", Test_vm_migrate.test)
     ; ("Test_no_migrate", Test_no_migrate.test)
     ; ("Test_vm_check_operation_error", Test_vm_check_operation_error.test)
     ; ("Test_xapi_vbd_helpers", Test_xapi_vbd_helpers.test)
     ; ("Test_host", Test_host.test)
     ; ("Test_host_helpers", Test_host_helpers.test)
     ; ("Test_sr_update_vdis", Test_sr_update_vdis.test)
     ; ("Test_xapi_db_upgrade", Test_xapi_db_upgrade.test)
     ; ("Test_db_lowlevel", Test_db_lowlevel.test)
     ; ("Test_vlan", Test_vlan.test)
     ; ("Test_network", Test_network.test)
     ; ("Test_bond", Test_bond.test)
     ; ("Test_tunnel", Test_tunnel.test)
     ; ("Test_agility", Test_agility.test)
     ; ( "Test_clustering_allowed_operations"
       , Test_clustering_allowed_operations.test
       )
     ; ("Test_client", Test_client.test)
     ; ("Test_ca91480", Test_ca91480.test)
     ; ("Test_pgpu", Test_pgpu.test)
     ; ("Test_gpu_group", Test_gpu_group.test)
     ; ("Test_pool_apply_edition", Test_pool_apply_edition.test)
     ; ("Test_pool_update", Test_pool_update.test)
     ; ("Test_pool_db_backup", Test_pool_db_backup.test)
     ; ("Test_pool_restore_database", Test_pool_restore_database.test)
     ; ("Test_workload_balancing", Test_workload_balancing.test)
     ; ("Test_pvs_site", Test_pvs_site.test)
     ; ("Test_pvs_proxy", Test_pvs_proxy.test)
     ; ("Test_pvs_server", Test_pvs_server.test)
     ; ("Test_vm_memory_constraints", Test_vm_memory_constraints.test)
     ; ("Test_xapi_xenops", Test_xapi_xenops.test)
     ; ("Test_network_event_loop", Test_network_event_loop.test)
     ; ("Test_vgpu_type", Test_vgpu_type.test)
     ; ("Test_storage_migrate_state", Test_storage_migrate_state.test)
     ; ("Test_bios_strings", Test_bios_strings.test)
     ; ("Test_certificates", Test_certificates.test)
     ]
    @ Test_guest_agent.tests
    @ Test_nm.tests
    @ Test_xenopsd_metadata.tests
    @ Test_http.tests
    @ Test_ha_vm_failover.tests
    @ Test_map_check.tests
    @ Test_pool_license.tests
    @ Test_features.tests
    @ Test_platformdata.tests
    @ Test_sm_features.tests
    @ Test_vgpu_type.tests
    @ Test_pgpu_helpers.tests
    @ Test_storage_migrate_state.tests
    @ Test_vm.tests
    @ Test_dbsync_master.tests
    @ Test_pvs_cache_storage.tests
    @ Test_extauth_plugin_ADpbis.tests
    @ Test_helpers.tests
    @ Test_datamodel_lifecycle.tests
    @ Test_psr.tests
    @ Test_context.tests
    @ Test_session.tests
    @ Test_xapi_cmd_result.tests
    @ Test_extauth_plugin_ADwinbind.tests
    )
