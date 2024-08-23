module O = Old_record_util
module N = Record_util
open Old_enum_all
open Printf
open Alcotest

let test_to_string ~name all_enum (old_to_string, new_to_string) =
  ( name ^ "to_string"
  , all_enum
    |> List.map @@ fun enum ->
       let expected = old_to_string enum in
       V1.test_case expected `Quick @@ fun () ->
       let actual = new_to_string enum in
       V1.(check' ~msg:"compatible" ~expected ~actual string)
  )

(* If record_util raises on of_string of a valid enum, it should raise the same exception.
   Currently this only happens on 'unspecified' VM domain type.
*)
let wrap f x = try Ok (f x) with e -> Error e

let drop_module_prefix s =
  match Astring.String.cut ~sep:"." s with
  | Some (_module, rest) ->
      rest
  | None ->
      s

let drop_exn_arguments s =
  match Astring.String.cut ~sep:"(" s with
  | Some (typ, _args) ->
      typ
  | None ->
      s

let exn_to_string_strip e =
  (* Drop the module prefix: that is expected to be different.
     We'll only look at the exception type and not its string arguments,
     to allow improving the error message in the future.
  *)
  e |> Printexc.to_string |> drop_module_prefix |> drop_exn_arguments

let test_of_string ~name all_enum old_to_string of_string_opt =
  let exn_equal_strip a b =
    String.equal (exn_to_string_strip a) (exn_to_string_strip b)
  in
  (* New function is allowed to be more lenient and accept more cases *)
  let custom_eq expected actual =
    match (expected, actual) with
    | Error _, Ok _ ->
        true
    | Error a, Error b ->
        exn_equal_strip a b
    | a, b ->
        a = b
  in
  of_string_opt
  |> Option.map (fun (old_of_string, new_of_string) ->
         let make input =
           V1.test_case input `Quick @@ fun () ->
           let expected = wrap old_of_string input in
           let actual = wrap new_of_string input in
           let pp_enum_result =
             Fmt.of_to_string (function
               | Ok a ->
                   old_to_string a
               | Error b ->
                   exn_to_string_strip b
               )
           in
           V1.(
             check' ~msg:"compatible" ~expected ~actual
             @@ testable pp_enum_result custom_eq
           )
         in
         ( name ^ "of_string"
         , make "bad-BaD-BAD"
           :: (all_enum
              |> List.concat_map @@ fun enum ->
                 let input = old_to_string enum in
                 [
                   make input
                 ; make (String.capitalize_ascii input)
                 ; make (String.uppercase_ascii input)
                 ]
              )
         )
     )
  |> Option.to_list

let mk line of_string_opt all_enum (old_to_string, new_to_string) =
  let name = sprintf "line%d:" line in
  test_to_string ~name all_enum (old_to_string, new_to_string)
  :: test_of_string ~name all_enum old_to_string of_string_opt

(*
Created by:
```
grep 'let.*to_string' old_record_util.ml | \
sed -re 's/^let ([^ ]+)_to_string.*/\1/' | \
while read ENUM; do
  if grep "${ENUM}_of_string" old_record_util.ml >/dev/null; then
    echo "; mk __LINE__ (Some (O.${ENUM}_of_string, N.${ENUM}_of_string)) all_${ENUM} (O.${ENUM}_to_string, N.${ENUM}_to_string)";
  else
    echo "; mk __LINE__ None all_${ENUM} (O.${ENUM}_to_string, N.${ENUM}_to_string)";
  fi;
done
```
and then tweaked to compile using LSP hints  where the names were not consistent (e.g. singular vs plural, etc.)
*)
let tests =
  [
    mk __LINE__ None all_certificate_type
      (O.certificate_type_to_string, N.certificate_type_to_string)
  ; mk __LINE__ None all_cls (O.class_to_string, N.class_to_string)
  ; mk __LINE__ None all_vm_power_state
      (O.power_state_to_string, N.vm_power_state_to_string)
  ; mk __LINE__ None all_vm_power_state
      (O.power_to_string, N.vm_power_state_to_lowercase_string)
  ; mk __LINE__ None all_vm_operations
      (O.vm_operation_to_string, N.vm_operation_to_string)
  ; mk __LINE__ None all_vm_secureboot_readiness
      (O.vm_secureboot_readiness_to_string, N.vm_secureboot_readiness_to_string)
  ; mk __LINE__ None all_pool_guest_secureboot_readiness
      ( O.pool_guest_secureboot_readiness_to_string
      , N.pool_guest_secureboot_readiness_to_string
      )
  ; mk __LINE__ None all_pool_allowed_operations
      (O.pool_operation_to_string, N.pool_allowed_operations_to_string)
  ; mk __LINE__ None all_host_allowed_operations
      (O.host_operation_to_string, N.host_operation_to_string)
  ; mk __LINE__ None all_update_guidances
      (O.update_guidance_to_string, N.update_guidances_to_string)
  ; mk __LINE__ None all_latest_synced_updates_applied_state
      ( O.latest_synced_updates_applied_state_to_string
      , N.latest_synced_updates_applied_state_to_string
      )
  ; mk __LINE__ None all_vdi_operations
      (O.vdi_operation_to_string, N.vdi_operations_to_string)
  ; mk __LINE__ None all_storage_operations
      (O.sr_operation_to_string, N.sr_operation_to_string)
  ; mk __LINE__ None all_vbd_operations
      (O.vbd_operation_to_string, N.vbd_operations_to_string)
  ; mk __LINE__ None all_vif_operations
      (O.vif_operation_to_string, N.vif_operations_to_string)
  ; mk __LINE__ None all_vif_locking_mode
      (O.vif_locking_mode_to_string, N.vif_locking_mode_to_string)
  ; mk __LINE__ None all_vmss_type (O.vmss_type_to_string, N.vmss_type_to_string)
  ; mk __LINE__ None all_vmss_frequency
      (O.vmss_frequency_to_string, N.vmss_frequency_to_string)
  ; mk __LINE__ None all_network_default_locking_mode
      ( O.network_default_locking_mode_to_string
      , N.network_default_locking_mode_to_string
      )
  ; mk __LINE__ None all_network_purpose
      (O.network_purpose_to_string, N.network_purpose_to_string)
  ; mk __LINE__ None all_vm_appliance_operation
      (O.vm_appliance_operation_to_string, N.vm_appliance_operation_to_string)
    (*; mk __LINE__ None all_cpu_feature (O.cpu_feature_to_string, N.cpu_feature_to_string)*)
  ; mk __LINE__ None all_task_status_type
      (O.task_status_type_to_string, N.task_status_type_to_string)
  ; mk __LINE__ None all_console_protocol
      (O.protocol_to_string, N.protocol_to_string)
  ; mk __LINE__ None all_telemetry_frequency
      (O.telemetry_frequency_to_string, N.telemetry_frequency_to_string)
  ; mk __LINE__ None all_task_allowed_operations
      (O.task_allowed_operations_to_string, N.task_allowed_operations_to_string)
    (*; mk __LINE__ None all_alert_level (O.alert_level_to_string, N.alert_level_to_string)*)
  ; mk __LINE__ None all_on_normal_exit
      (O.on_normal_exit_to_string, N.on_normal_exit_to_string)
  ; mk __LINE__ None all_on_crash_behaviour
      (O.on_crash_behaviour_to_string, N.on_crash_behaviour_to_string)
  ; mk __LINE__ None all_on_softreboot_behavior
      (O.on_softreboot_behaviour_to_string, N.on_softreboot_behaviour_to_string)
  ; mk __LINE__ None all_host_display
      (O.host_display_to_string, N.host_display_to_string)
  ; mk __LINE__
      (Some (O.host_sched_gran_of_string, N.host_sched_gran_of_string))
      all_host_sched_gran
      (O.host_sched_gran_to_string, N.host_sched_gran_to_string)
  ; mk __LINE__
      (Some
         ( O.host_numa_affinity_policy_of_string
         , N.host_numa_affinity_policy_of_string
         )
      )
      all_host_numa_affinity_policy
      ( O.host_numa_affinity_policy_to_string
      , N.host_numa_affinity_policy_to_string
      )
  ; mk __LINE__ None all_pci_dom0_access
      (O.pci_dom0_access_to_string, N.pci_dom0_access_to_string)
  ; mk __LINE__
      (Some (O.string_to_vbd_mode, N.vbd_mode_of_string))
      all_vbd_mode
      (O.vbd_mode_to_string, N.vbd_mode_to_string)
  ; mk __LINE__
      (Some (O.string_to_vbd_type, N.vbd_type_of_string))
      all_vbd_type
      (N.vbd_type_to_string, N.vbd_type_to_string)
    (*; mk __LINE__ None all_power (O.power_to_string, N.power_to_string)*)
  ; mk __LINE__ None all_vdi_type (O.vdi_type_to_string, N.vdi_type_to_string)
  ; mk __LINE__
      (Some
         (O.ip_configuration_mode_of_string, N.ip_configuration_mode_of_string)
      )
      all_ip_configuration_mode
      (O.ip_configuration_mode_to_string, N.ip_configuration_mode_to_string)
  ; mk __LINE__
      (Some
         ( O.vif_ipv4_configuration_mode_of_string
         , N.vif_ipv4_configuration_mode_of_string
         )
      )
      all_vif_ipv4_configuration_mode
      ( O.vif_ipv4_configuration_mode_to_string
      , N.vif_ipv4_configuration_mode_to_string
      )
  ; mk __LINE__
      (Some
         ( O.ipv6_configuration_mode_of_string
         , N.ipv6_configuration_mode_of_string
         )
      )
      all_ipv6_configuration_mode
      (O.ipv6_configuration_mode_to_string, N.ipv6_configuration_mode_to_string)
  ; mk __LINE__
      (Some
         ( O.vif_ipv6_configuration_mode_of_string
         , N.vif_ipv6_configuration_mode_of_string
         )
      )
      all_vif_ipv6_configuration_mode
      ( O.vif_ipv6_configuration_mode_to_string
      , N.vif_ipv6_configuration_mode_to_string
      )
  ; mk __LINE__
      (Some (O.primary_address_type_of_string, N.primary_address_type_of_string))
      all_primary_address_type
      (O.primary_address_type_to_string, N.primary_address_type_to_string)
  ; mk __LINE__
      (Some (O.bond_mode_of_string, N.bond_mode_of_string))
      all_bond_mode
      (O.bond_mode_to_string, N.bond_mode_to_string)
  ; mk __LINE__
      (Some (O.allocation_algorithm_of_string, N.allocation_algorithm_of_string))
      all_allocation_algorithm
      (O.allocation_algorithm_to_string, N.allocation_algorithm_to_string)
  ; mk __LINE__ None all_pvs_proxy_status
      (O.pvs_proxy_status_to_string, N.pvs_proxy_status_to_string)
  ; mk __LINE__ None all_cluster_operation
      (O.cluster_operation_to_string, N.cluster_operation_to_string)
  ; mk __LINE__ None all_cluster_host_operation
      (O.cluster_host_operation_to_string, N.cluster_host_operation_to_string)
  ; mk __LINE__
      (Some (O.sdn_protocol_of_string, N.sdn_controller_protocol_of_string))
      all_sdn_controller_protocol
      (O.sdn_protocol_to_string, N.sdn_controller_protocol_to_string)
  ; mk __LINE__
      (Some (O.tunnel_protocol_of_string, N.tunnel_protocol_of_string))
      all_tunnel_protocol
      (O.tunnel_protocol_to_string, N.tunnel_protocol_to_string)
  ; mk __LINE__ None all_pif_igmp_status
      (O.pif_igmp_status_to_string, N.pif_igmp_status_to_string)
  ; mk __LINE__ None all_vusb_operations
      (O.vusb_operation_to_string, N.vusb_operations_to_string)
  ; mk __LINE__ None all_sriov_configuration_mode
      ( O.network_sriov_configuration_mode_to_string
      , N.sriov_configuration_mode_to_string
      )
  ; mk __LINE__ None all_on_boot (O.on_boot_to_string, N.on_boot_to_string)
  ; mk __LINE__ None all_tristate_type
      (O.tristate_to_string, N.tristate_to_string)
  ; mk __LINE__
      (Some (O.domain_type_of_string, N.domain_type_of_string))
      all_domain_type
      (O.domain_type_to_string, N.domain_type_to_string)
  ; mk __LINE__ None all_vtpm_operations
      (O.vtpm_operation_to_string, N.vtpm_operations_to_string)
  ; mk __LINE__
      (Some
         (O.update_sync_frequency_of_string, N.update_sync_frequency_of_string)
      )
      all_update_sync_frequency
      (O.update_sync_frequency_to_string, N.update_sync_frequency_to_string)
  ; mk __LINE__
      (Some (O.vm_placement_policy_of_string, N.vm_placement_policy_of_string))
      all_placement_policy
      (O.vm_placement_policy_to_string, N.vm_placement_policy_to_string)
  ; mk __LINE__ None all_origin (O.repo_origin_to_string, N.origin_to_string)
  ]
  |> List.concat

let () = V1.run "record_util" tests
