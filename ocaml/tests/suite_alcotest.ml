
let () =
  Suite_init.harness_init ();
  (* Alcotest hides the standard output of successful tests,
     so we will probably not exceed the 4MB limit in Traivs *)
  Debug.log_to_stdout ();

  Alcotest.run "Base suite"
    [ "Test_valid_ref_list", Test_valid_ref_list.test
    ; "Test_vdi_allowed_operations", Test_vdi_allowed_operations.test
    ; "Test_vm_check_operation_error", Test_vm_check_operation_error.test
    ; "Test_host", Test_host.test
    ; "Test_vdi_cbt", Test_vdi_cbt.test
    ; "Test_db_lowlevel", Test_db_lowlevel.test
    ; "Test_vlan", Test_vlan.test
    ; "Test_agility", Test_agility.test
    ; "Test_daemon_manager", Test_daemon_manager.test
    ; "Test_cluster", Test_cluster.test
    ; "Test_cluster_host", Test_cluster_host.test
    ; "Test_client", Test_client.test
    ; "Test_clustering", Test_clustering.test
    ; "Test_clustering_allowed_operations", Test_clustering_allowed_operations.test
    ]

