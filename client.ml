open Xml
open XMLRPC
open API
module type RPC = sig val rpc: xml -> xml end
module type IO = sig type 'a t val bind : 'a t -> ('a -> 'b t) -> 'b t val return : 'a -> 'a t end
let server_failure code args = raise (Api_errors.Server_error (code, args))

module ClientF = functor(X : IO) ->struct

  let (>>=) = X.bind
  let return = X.return
  let rpc_wrapper rpc name args = 
    rpc(To.methodCall name args) >>= fun x ->
    match From.methodResponse(x) with
    | Fault _ -> return (invalid_arg "Client.rpc (Fault _)")
    | Success [] -> return (XMLRPC.To.structure []) (* dummy value *)
    | Success [x] -> return x
    | Success _ -> return (invalid_arg "more than one result from an RPC")
    | Failure(code, strings) -> return (server_failure code strings)
    | _ -> return (invalid_arg "unexpected result from an RPC")
  module Async = struct
    module Session = struct
      (**  *)
      let get_all_subject_identifiers ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.session.get_all_subject_identifiers" [ session_id ] >>= fun x -> return (From.ref_task "return value of session.get_all_subject_identifiers"  x)
      (**  *)
      let logout_subject_identifier ~rpc ~session_id ~subject_identifier =
        let session_id = To.ref_session session_id in
        let subject_identifier = To.string subject_identifier in
        
        rpc_wrapper rpc "Async.session.logout_subject_identifier" [ session_id; subject_identifier ] >>= fun x -> return (From.ref_task "return value of session.logout_subject_identifier"  x)
    end
    module Subject = struct
      (**  *)
      let create ~rpc ~session_id ~subject_identifier ~other_config =
        let session_id = To.ref_session session_id in
        let subject_identifier = To.string subject_identifier in
        let other_config = To.string_to_string_map other_config in
        let __structure = To.structure [ "subject_identifier", subject_identifier; "other_config", other_config] in
        rpc_wrapper rpc "Async.subject.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of subject.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_subject self in
        
        rpc_wrapper rpc "Async.subject.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of subject.destroy"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~subject_identifier:value.subject_subject_identifier
        ~other_config:value.subject_other_config
    end
    module Role = struct
    end
    module Task = struct
      (**  *)
      let cancel ~rpc ~session_id ~task =
        let session_id = To.ref_session session_id in
        let task = To.ref_task task in
        
        rpc_wrapper rpc "Async.task.cancel" [ session_id; task ] >>= fun x -> return (From.ref_task "return value of task.cancel"  x)
    end
    module Event = struct
      (**  *)
      let register ~rpc ~session_id ~classes =
        let session_id = To.ref_session session_id in
        let classes = To.string_set classes in
        
        rpc_wrapper rpc "Async.event.register" [ session_id; classes ] >>= fun x -> return (From.ref_task "return value of event.register"  x)
      (**  *)
      let unregister ~rpc ~session_id ~classes =
        let session_id = To.ref_session session_id in
        let classes = To.string_set classes in
        
        rpc_wrapper rpc "Async.event.unregister" [ session_id; classes ] >>= fun x -> return (From.ref_task "return value of event.unregister"  x)
    end
    module Pool = struct
      (**  *)
      let join ~rpc ~session_id ~master_address ~master_username ~master_password =
        let session_id = To.ref_session session_id in
        let master_address = To.string master_address in
        let master_username = To.string master_username in
        let master_password = To.string master_password in
        
        rpc_wrapper rpc "Async.pool.join" [ session_id; master_address; master_username; master_password ] >>= fun x -> return (From.ref_task "return value of pool.join"  x)
      (**  *)
      let join_force ~rpc ~session_id ~master_address ~master_username ~master_password =
        let session_id = To.ref_session session_id in
        let master_address = To.string master_address in
        let master_username = To.string master_username in
        let master_password = To.string master_password in
        
        rpc_wrapper rpc "Async.pool.join_force" [ session_id; master_address; master_username; master_password ] >>= fun x -> return (From.ref_task "return value of pool.join_force"  x)
      (**  *)
      let eject ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.pool.eject" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of pool.eject"  x)
      (**  *)
      let initial_auth ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.initial_auth" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.initial_auth"  x)
      (**  *)
      let recover_slaves ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.recover_slaves" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.recover_slaves"  x)
      (**  *)
      let hello ~rpc ~session_id ~host_uuid ~host_address =
        let session_id = To.ref_session session_id in
        let host_uuid = To.string host_uuid in
        let host_address = To.string host_address in
        
        rpc_wrapper rpc "Async.pool.hello" [ session_id; host_uuid; host_address ] >>= fun x -> return (From.ref_task "return value of pool.hello"  x)
      (**  *)
      let create_VLAN ~rpc ~session_id ~device ~network ~vLAN =
        let session_id = To.ref_session session_id in
        let device = To.string device in
        let network = To.ref_network network in
        let vLAN = To.int64 vLAN in
        
        rpc_wrapper rpc "Async.pool.create_VLAN" [ session_id; device; network; vLAN ] >>= fun x -> return (From.ref_task "return value of pool.create_VLAN"  x)
      (**  *)
      let create_VLAN_from_PIF ~rpc ~session_id ~pif ~network ~vLAN =
        let session_id = To.ref_session session_id in
        let pif = To.ref_PIF pif in
        let network = To.ref_network network in
        let vLAN = To.int64 vLAN in
        
        rpc_wrapper rpc "Async.pool.create_VLAN_from_PIF" [ session_id; pif; network; vLAN ] >>= fun x -> return (From.ref_task "return value of pool.create_VLAN_from_PIF"  x)
      (**  *)
      let slave_network_report ~rpc ~session_id ~phydevs ~dev_to_mac ~dev_to_mtu ~slave_host =
        let session_id = To.ref_session session_id in
        let phydevs = To.string_to_string_map phydevs in
        let dev_to_mac = To.string_to_string_map dev_to_mac in
        let dev_to_mtu = To.string_to_int64_map dev_to_mtu in
        let slave_host = To.ref_host slave_host in
        
        rpc_wrapper rpc "Async.pool.slave_network_report" [ session_id; phydevs; dev_to_mac; dev_to_mtu; slave_host ] >>= fun x -> return (From.ref_task "return value of pool.slave_network_report"  x)
      (**  *)
      let enable_ha ~rpc ~session_id ~heartbeat_srs ~configuration =
        let session_id = To.ref_session session_id in
        let heartbeat_srs = To.ref_SR_set heartbeat_srs in
        let configuration = To.string_to_string_map configuration in
        
        rpc_wrapper rpc "Async.pool.enable_ha" [ session_id; heartbeat_srs; configuration ] >>= fun x -> return (From.ref_task "return value of pool.enable_ha"  x)
      (**  *)
      let disable_ha ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.disable_ha" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.disable_ha"  x)
      (**  *)
      let sync_database ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.sync_database" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.sync_database"  x)
      (**  *)
      let designate_new_master ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.pool.designate_new_master" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of pool.designate_new_master"  x)
      (**  *)
      let set_ha_host_failures_to_tolerate ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool self in
        let value = To.int64 value in
        
        rpc_wrapper rpc "Async.pool.set_ha_host_failures_to_tolerate" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of pool.set_ha_host_failures_to_tolerate"  x)
      (**  *)
      let create_new_blob ~rpc ~session_id ~pool ~name ~mime_type ~public =
        let session_id = To.ref_session session_id in
        let pool = To.ref_pool pool in
        let name = To.string name in
        let mime_type = To.string mime_type in
        let public = To.bool public in
        
        rpc_wrapper rpc "Async.pool.create_new_blob" [ session_id; pool; name; mime_type; public ] >>= fun x -> return (From.ref_task "return value of pool.create_new_blob"  x)
      (**  *)
      let ha_schedule_plan_recomputation ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.ha_schedule_plan_recomputation" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.ha_schedule_plan_recomputation"  x)
      (**  *)
      let enable_binary_storage ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.enable_binary_storage" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.enable_binary_storage"  x)
      (**  *)
      let disable_binary_storage ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.disable_binary_storage" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.disable_binary_storage"  x)
      (**  *)
      let initialize_wlb ~rpc ~session_id ~wlb_url ~wlb_username ~wlb_password ~xenserver_username ~xenserver_password =
        let session_id = To.ref_session session_id in
        let wlb_url = To.string wlb_url in
        let wlb_username = To.string wlb_username in
        let wlb_password = To.string wlb_password in
        let xenserver_username = To.string xenserver_username in
        let xenserver_password = To.string xenserver_password in
        
        rpc_wrapper rpc "Async.pool.initialize_wlb" [ session_id; wlb_url; wlb_username; wlb_password; xenserver_username; xenserver_password ] >>= fun x -> return (From.ref_task "return value of pool.initialize_wlb"  x)
      (**  *)
      let deconfigure_wlb ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.deconfigure_wlb" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.deconfigure_wlb"  x)
      (**  *)
      let send_wlb_configuration ~rpc ~session_id ~config =
        let session_id = To.ref_session session_id in
        let config = To.string_to_string_map config in
        
        rpc_wrapper rpc "Async.pool.send_wlb_configuration" [ session_id; config ] >>= fun x -> return (From.ref_task "return value of pool.send_wlb_configuration"  x)
      (**  *)
      let retrieve_wlb_configuration ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.retrieve_wlb_configuration" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.retrieve_wlb_configuration"  x)
      (**  *)
      let retrieve_wlb_recommendations ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.retrieve_wlb_recommendations" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.retrieve_wlb_recommendations"  x)
      (**  *)
      let send_test_post ~rpc ~session_id ~host ~port ~body =
        let session_id = To.ref_session session_id in
        let host = To.string host in
        let port = To.int64 port in
        let body = To.string body in
        
        rpc_wrapper rpc "Async.pool.send_test_post" [ session_id; host; port; body ] >>= fun x -> return (From.ref_task "return value of pool.send_test_post"  x)
      (**  *)
      let certificate_install ~rpc ~session_id ~name ~cert =
        let session_id = To.ref_session session_id in
        let name = To.string name in
        let cert = To.string cert in
        
        rpc_wrapper rpc "Async.pool.certificate_install" [ session_id; name; cert ] >>= fun x -> return (From.ref_task "return value of pool.certificate_install"  x)
      (**  *)
      let certificate_uninstall ~rpc ~session_id ~name =
        let session_id = To.ref_session session_id in
        let name = To.string name in
        
        rpc_wrapper rpc "Async.pool.certificate_uninstall" [ session_id; name ] >>= fun x -> return (From.ref_task "return value of pool.certificate_uninstall"  x)
      (**  *)
      let certificate_list ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.certificate_list" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.certificate_list"  x)
      (**  *)
      let crl_install ~rpc ~session_id ~name ~cert =
        let session_id = To.ref_session session_id in
        let name = To.string name in
        let cert = To.string cert in
        
        rpc_wrapper rpc "Async.pool.crl_install" [ session_id; name; cert ] >>= fun x -> return (From.ref_task "return value of pool.crl_install"  x)
      (**  *)
      let crl_uninstall ~rpc ~session_id ~name =
        let session_id = To.ref_session session_id in
        let name = To.string name in
        
        rpc_wrapper rpc "Async.pool.crl_uninstall" [ session_id; name ] >>= fun x -> return (From.ref_task "return value of pool.crl_uninstall"  x)
      (**  *)
      let crl_list ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.crl_list" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.crl_list"  x)
      (**  *)
      let certificate_sync ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.certificate_sync" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.certificate_sync"  x)
      (**  *)
      let enable_redo_log ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.pool.enable_redo_log" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of pool.enable_redo_log"  x)
      (**  *)
      let disable_redo_log ~rpc ~session_id =
        let session_id = To.ref_session session_id in
        
        rpc_wrapper rpc "Async.pool.disable_redo_log" [ session_id ] >>= fun x -> return (From.ref_task "return value of pool.disable_redo_log"  x)
      (**  *)
      let audit_log_append ~rpc ~session_id ~line =
        let session_id = To.ref_session session_id in
        let line = To.string line in
        
        rpc_wrapper rpc "Async.pool.audit_log_append" [ session_id; line ] >>= fun x -> return (From.ref_task "return value of pool.audit_log_append"  x)
      (**  *)
      let set_vswitch_controller ~rpc ~session_id ~address =
        let session_id = To.ref_session session_id in
        let address = To.string address in
        
        rpc_wrapper rpc "Async.pool.set_vswitch_controller" [ session_id; address ] >>= fun x -> return (From.ref_task "return value of pool.set_vswitch_controller"  x)
      (**  *)
      let enable_local_storage_caching ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool self in
        
        rpc_wrapper rpc "Async.pool.enable_local_storage_caching" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of pool.enable_local_storage_caching"  x)
      (**  *)
      let disable_local_storage_caching ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool self in
        
        rpc_wrapper rpc "Async.pool.disable_local_storage_caching" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of pool.disable_local_storage_caching"  x)
    end
    module Pool_patch = struct
      (**  *)
      let apply ~rpc ~session_id ~self ~host =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool_patch self in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.pool_patch.apply" [ session_id; self; host ] >>= fun x -> return (From.ref_task "return value of pool_patch.apply"  x)
      (**  *)
      let pool_apply ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool_patch self in
        
        rpc_wrapper rpc "Async.pool_patch.pool_apply" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of pool_patch.pool_apply"  x)
      (**  *)
      let precheck ~rpc ~session_id ~self ~host =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool_patch self in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.pool_patch.precheck" [ session_id; self; host ] >>= fun x -> return (From.ref_task "return value of pool_patch.precheck"  x)
      (**  *)
      let clean ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool_patch self in
        
        rpc_wrapper rpc "Async.pool_patch.clean" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of pool_patch.clean"  x)
      (**  *)
      let pool_clean ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool_patch self in
        
        rpc_wrapper rpc "Async.pool_patch.pool_clean" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of pool_patch.pool_clean"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool_patch self in
        
        rpc_wrapper rpc "Async.pool_patch.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of pool_patch.destroy"  x)
      (**  *)
      let clean_on_host ~rpc ~session_id ~self ~host =
        let session_id = To.ref_session session_id in
        let self = To.ref_pool_patch self in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.pool_patch.clean_on_host" [ session_id; self; host ] >>= fun x -> return (From.ref_task "return value of pool_patch.clean_on_host"  x)
    end
    module VM = struct
      (**  *)
      let create ~rpc ~session_id ~name_label ~name_description ~user_version ~is_a_template ~affinity ~memory_target ~memory_static_max ~memory_dynamic_max ~memory_dynamic_min ~memory_static_min ~vCPUs_params ~vCPUs_max ~vCPUs_at_startup ~actions_after_shutdown ~actions_after_reboot ~actions_after_crash ~pV_bootloader ~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader_args ~pV_legacy_args ~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier ~platform ~pCI_bus ~other_config ~recommendations ~xenstore_data ~ha_always_run ~ha_restart_priority ~tags ~blocked_operations ~protection_policy ~is_snapshot_from_vmpp ~appliance ~start_delay ~shutdown_delay ~order ~suspend_SR ~version =
        let session_id = To.ref_session session_id in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let user_version = To.int64 user_version in
        let is_a_template = To.bool is_a_template in
        let affinity = To.ref_host affinity in
        let memory_target = To.int64 memory_target in
        let memory_static_max = To.int64 memory_static_max in
        let memory_dynamic_max = To.int64 memory_dynamic_max in
        let memory_dynamic_min = To.int64 memory_dynamic_min in
        let memory_static_min = To.int64 memory_static_min in
        let vCPUs_params = To.string_to_string_map vCPUs_params in
        let vCPUs_max = To.int64 vCPUs_max in
        let vCPUs_at_startup = To.int64 vCPUs_at_startup in
        let actions_after_shutdown = To.on_normal_exit actions_after_shutdown in
        let actions_after_reboot = To.on_normal_exit actions_after_reboot in
        let actions_after_crash = To.on_crash_behaviour actions_after_crash in
        let pV_bootloader = To.string pV_bootloader in
        let pV_kernel = To.string pV_kernel in
        let pV_ramdisk = To.string pV_ramdisk in
        let pV_args = To.string pV_args in
        let pV_bootloader_args = To.string pV_bootloader_args in
        let pV_legacy_args = To.string pV_legacy_args in
        let hVM_boot_policy = To.string hVM_boot_policy in
        let hVM_boot_params = To.string_to_string_map hVM_boot_params in
        let hVM_shadow_multiplier = To.float hVM_shadow_multiplier in
        let platform = To.string_to_string_map platform in
        let pCI_bus = To.string pCI_bus in
        let other_config = To.string_to_string_map other_config in
        let recommendations = To.string recommendations in
        let xenstore_data = To.string_to_string_map xenstore_data in
        let ha_always_run = To.bool ha_always_run in
        let ha_restart_priority = To.string ha_restart_priority in
        let tags = To.string_set tags in
        let blocked_operations = To.vm_operations_to_string_map blocked_operations in
        let protection_policy = To.ref_VMPP protection_policy in
        let is_snapshot_from_vmpp = To.bool is_snapshot_from_vmpp in
        let appliance = To.ref_VM_appliance appliance in
        let start_delay = To.int64 start_delay in
        let shutdown_delay = To.int64 shutdown_delay in
        let order = To.int64 order in
        let suspend_SR = To.ref_SR suspend_SR in
        let version = To.int64 version in
        let __structure = To.structure [ "name_label", name_label; "name_description", name_description; "user_version", user_version; "is_a_template", is_a_template; "affinity", affinity; "memory_target", memory_target; "memory_static_max", memory_static_max; "memory_dynamic_max", memory_dynamic_max; "memory_dynamic_min", memory_dynamic_min; "memory_static_min", memory_static_min; "VCPUs_params", vCPUs_params; "VCPUs_max", vCPUs_max; "VCPUs_at_startup", vCPUs_at_startup; "actions_after_shutdown", actions_after_shutdown; "actions_after_reboot", actions_after_reboot; "actions_after_crash", actions_after_crash; "PV_bootloader", pV_bootloader; "PV_kernel", pV_kernel; "PV_ramdisk", pV_ramdisk; "PV_args", pV_args; "PV_bootloader_args", pV_bootloader_args; "PV_legacy_args", pV_legacy_args; "HVM_boot_policy", hVM_boot_policy; "HVM_boot_params", hVM_boot_params; "HVM_shadow_multiplier", hVM_shadow_multiplier; "platform", platform; "PCI_bus", pCI_bus; "other_config", other_config; "recommendations", recommendations; "xenstore_data", xenstore_data; "ha_always_run", ha_always_run; "ha_restart_priority", ha_restart_priority; "tags", tags; "blocked_operations", blocked_operations; "protection_policy", protection_policy; "is_snapshot_from_vmpp", is_snapshot_from_vmpp; "appliance", appliance; "start_delay", start_delay; "shutdown_delay", shutdown_delay; "order", order; "suspend_SR", suspend_SR; "version", version] in
        rpc_wrapper rpc "Async.VM.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of VM.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        
        rpc_wrapper rpc "Async.VM.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM.destroy"  x)
      (**  *)
      let snapshot ~rpc ~session_id ~vm ~new_name =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let new_name = To.string new_name in
        
        rpc_wrapper rpc "Async.VM.snapshot" [ session_id; vm; new_name ] >>= fun x -> return (From.ref_task "return value of VM.snapshot"  x)
      (**  *)
      let snapshot_with_quiesce ~rpc ~session_id ~vm ~new_name =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let new_name = To.string new_name in
        
        rpc_wrapper rpc "Async.VM.snapshot_with_quiesce" [ session_id; vm; new_name ] >>= fun x -> return (From.ref_task "return value of VM.snapshot_with_quiesce"  x)
      (**  *)
      let clone ~rpc ~session_id ~vm ~new_name =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let new_name = To.string new_name in
        
        rpc_wrapper rpc "Async.VM.clone" [ session_id; vm; new_name ] >>= fun x -> return (From.ref_task "return value of VM.clone"  x)
      (**  *)
      let copy ~rpc ~session_id ~vm ~new_name ~sr =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let new_name = To.string new_name in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.VM.copy" [ session_id; vm; new_name; sr ] >>= fun x -> return (From.ref_task "return value of VM.copy"  x)
      (**  *)
      let revert ~rpc ~session_id ~snapshot =
        let session_id = To.ref_session session_id in
        let snapshot = To.ref_VM snapshot in
        
        rpc_wrapper rpc "Async.VM.revert" [ session_id; snapshot ] >>= fun x -> return (From.ref_task "return value of VM.revert"  x)
      (**  *)
      let checkpoint ~rpc ~session_id ~vm ~new_name =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let new_name = To.string new_name in
        
        rpc_wrapper rpc "Async.VM.checkpoint" [ session_id; vm; new_name ] >>= fun x -> return (From.ref_task "return value of VM.checkpoint"  x)
      (**  *)
      let provision ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.provision" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.provision"  x)
      (**  *)
      let start ~rpc ~session_id ~vm ~start_paused ~force =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let start_paused = To.bool start_paused in
        let force = To.bool force in
        
        rpc_wrapper rpc "Async.VM.start" [ session_id; vm; start_paused; force ] >>= fun x -> return (From.ref_task "return value of VM.start"  x)
      (**  *)
      let start_on ~rpc ~session_id ~vm ~host ~start_paused ~force =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let host = To.ref_host host in
        let start_paused = To.bool start_paused in
        let force = To.bool force in
        
        rpc_wrapper rpc "Async.VM.start_on" [ session_id; vm; host; start_paused; force ] >>= fun x -> return (From.ref_task "return value of VM.start_on"  x)
      (**  *)
      let pause ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.pause" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.pause"  x)
      (**  *)
      let unpause ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.unpause" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.unpause"  x)
      (**  *)
      let clean_shutdown ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.clean_shutdown" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.clean_shutdown"  x)
      (**  *)
      let clean_reboot ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.clean_reboot" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.clean_reboot"  x)
      (**  *)
      let hard_shutdown ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.hard_shutdown" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.hard_shutdown"  x)
      (**  *)
      let power_state_reset ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.power_state_reset" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.power_state_reset"  x)
      (**  *)
      let hard_reboot ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.hard_reboot" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.hard_reboot"  x)
      (**  *)
      let suspend ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.suspend" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.suspend"  x)
      (**  *)
      let csvm ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.csvm" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.csvm"  x)
      (**  *)
      let resume ~rpc ~session_id ~vm ~start_paused ~force =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let start_paused = To.bool start_paused in
        let force = To.bool force in
        
        rpc_wrapper rpc "Async.VM.resume" [ session_id; vm; start_paused; force ] >>= fun x -> return (From.ref_task "return value of VM.resume"  x)
      (**  *)
      let hard_reboot_internal ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.hard_reboot_internal" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.hard_reboot_internal"  x)
      (**  *)
      let resume_on ~rpc ~session_id ~vm ~host ~start_paused ~force =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let host = To.ref_host host in
        let start_paused = To.bool start_paused in
        let force = To.bool force in
        
        rpc_wrapper rpc "Async.VM.resume_on" [ session_id; vm; host; start_paused; force ] >>= fun x -> return (From.ref_task "return value of VM.resume_on"  x)
      (**  *)
      let pool_migrate ~rpc ~session_id ~vm ~host ~options =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let host = To.ref_host host in
        let options = To.string_to_string_map options in
        
        rpc_wrapper rpc "Async.VM.pool_migrate" [ session_id; vm; host; options ] >>= fun x -> return (From.ref_task "return value of VM.pool_migrate"  x)
      (**  *)
      let pool_migrate_complete ~rpc ~session_id ~vm ~host =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.VM.pool_migrate_complete" [ session_id; vm; host ] >>= fun x -> return (From.ref_task "return value of VM.pool_migrate_complete"  x)
      (**  *)
      let set_VCPUs_number_live ~rpc ~session_id ~self ~nvcpu =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let nvcpu = To.int64 nvcpu in
        
        rpc_wrapper rpc "Async.VM.set_VCPUs_number_live" [ session_id; self; nvcpu ] >>= fun x -> return (From.ref_task "return value of VM.set_VCPUs_number_live"  x)
      (**  *)
      let add_to_VCPUs_params_live ~rpc ~session_id ~self ~key ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let key = To.string key in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.VM.add_to_VCPUs_params_live" [ session_id; self; key; value ] >>= fun x -> return (From.ref_task "return value of VM.add_to_VCPUs_params_live"  x)
      (**  *)
      let compute_memory_overhead ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.compute_memory_overhead" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.compute_memory_overhead"  x)
      (**  *)
      let set_memory_dynamic_range ~rpc ~session_id ~self ~min ~max =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let min = To.int64 min in
        let max = To.int64 max in
        
        rpc_wrapper rpc "Async.VM.set_memory_dynamic_range" [ session_id; self; min; max ] >>= fun x -> return (From.ref_task "return value of VM.set_memory_dynamic_range"  x)
      (**  *)
      let set_memory_static_range ~rpc ~session_id ~self ~min ~max =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let min = To.int64 min in
        let max = To.int64 max in
        
        rpc_wrapper rpc "Async.VM.set_memory_static_range" [ session_id; self; min; max ] >>= fun x -> return (From.ref_task "return value of VM.set_memory_static_range"  x)
      (**  *)
      let set_memory_limits ~rpc ~session_id ~self ~static_min ~static_max ~dynamic_min ~dynamic_max =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let static_min = To.int64 static_min in
        let static_max = To.int64 static_max in
        let dynamic_min = To.int64 dynamic_min in
        let dynamic_max = To.int64 dynamic_max in
        
        rpc_wrapper rpc "Async.VM.set_memory_limits" [ session_id; self; static_min; static_max; dynamic_min; dynamic_max ] >>= fun x -> return (From.ref_task "return value of VM.set_memory_limits"  x)
      (**  *)
      let set_memory_target_live ~rpc ~session_id ~self ~target =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let target = To.int64 target in
        
        rpc_wrapper rpc "Async.VM.set_memory_target_live" [ session_id; self; target ] >>= fun x -> return (From.ref_task "return value of VM.set_memory_target_live"  x)
      (**  *)
      let wait_memory_target_live ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        
        rpc_wrapper rpc "Async.VM.wait_memory_target_live" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM.wait_memory_target_live"  x)
      (**  *)
      let get_cooperative ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        
        rpc_wrapper rpc "Async.VM.get_cooperative" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM.get_cooperative"  x)
      (**  *)
      let set_shadow_multiplier_live ~rpc ~session_id ~self ~multiplier =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let multiplier = To.float multiplier in
        
        rpc_wrapper rpc "Async.VM.set_shadow_multiplier_live" [ session_id; self; multiplier ] >>= fun x -> return (From.ref_task "return value of VM.set_shadow_multiplier_live"  x)
      (**  *)
      let send_sysrq ~rpc ~session_id ~vm ~key =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let key = To.string key in
        
        rpc_wrapper rpc "Async.VM.send_sysrq" [ session_id; vm; key ] >>= fun x -> return (From.ref_task "return value of VM.send_sysrq"  x)
      (**  *)
      let send_trigger ~rpc ~session_id ~vm ~trigger =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let trigger = To.string trigger in
        
        rpc_wrapper rpc "Async.VM.send_trigger" [ session_id; vm; trigger ] >>= fun x -> return (From.ref_task "return value of VM.send_trigger"  x)
      (**  *)
      let maximise_memory ~rpc ~session_id ~self ~total ~approximate =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let total = To.int64 total in
        let approximate = To.bool approximate in
        
        rpc_wrapper rpc "Async.VM.maximise_memory" [ session_id; self; total; approximate ] >>= fun x -> return (From.ref_task "return value of VM.maximise_memory"  x)
      (**  *)
      let migrate_send ~rpc ~session_id ~vm ~dest ~live ~vdi_map ~vif_map ~options =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let dest = To.string_to_string_map dest in
        let live = To.bool live in
        let vdi_map = To.ref_VDI_to_ref_SR_map vdi_map in
        let vif_map = To.ref_VIF_to_ref_network_map vif_map in
        let options = To.string_to_string_map options in
        
        rpc_wrapper rpc "Async.VM.migrate_send" [ session_id; vm; dest; live; vdi_map; vif_map; options ] >>= fun x -> return (From.ref_task "return value of VM.migrate_send"  x)
      (**  *)
      let assert_can_migrate ~rpc ~session_id ~vm ~dest ~live ~vdi_map ~vif_map ~options =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let dest = To.string_to_string_map dest in
        let live = To.bool live in
        let vdi_map = To.ref_VDI_to_ref_SR_map vdi_map in
        let vif_map = To.ref_VIF_to_ref_network_map vif_map in
        let options = To.string_to_string_map options in
        
        rpc_wrapper rpc "Async.VM.assert_can_migrate" [ session_id; vm; dest; live; vdi_map; vif_map; options ] >>= fun x -> return (From.ref_task "return value of VM.assert_can_migrate"  x)
      (**  *)
      let assert_operation_valid ~rpc ~session_id ~self ~op =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let op = To.vm_operations op in
        
        rpc_wrapper rpc "Async.VM.assert_operation_valid" [ session_id; self; op ] >>= fun x -> return (From.ref_task "return value of VM.assert_operation_valid"  x)
      (**  *)
      let update_allowed_operations ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        
        rpc_wrapper rpc "Async.VM.update_allowed_operations" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM.update_allowed_operations"  x)
      (**  *)
      let get_possible_hosts ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.get_possible_hosts" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.get_possible_hosts"  x)
      (**  *)
      let assert_can_boot_here ~rpc ~session_id ~self ~host =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.VM.assert_can_boot_here" [ session_id; self; host ] >>= fun x -> return (From.ref_task "return value of VM.assert_can_boot_here"  x)
      (**  *)
      let atomic_set_resident_on ~rpc ~session_id ~vm ~host =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.VM.atomic_set_resident_on" [ session_id; vm; host ] >>= fun x -> return (From.ref_task "return value of VM.atomic_set_resident_on"  x)
      (**  *)
      let create_new_blob ~rpc ~session_id ~vm ~name ~mime_type ~public =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let name = To.string name in
        let mime_type = To.string mime_type in
        let public = To.bool public in
        
        rpc_wrapper rpc "Async.VM.create_new_blob" [ session_id; vm; name; mime_type; public ] >>= fun x -> return (From.ref_task "return value of VM.create_new_blob"  x)
      (**  *)
      let s3_suspend ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.s3_suspend" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.s3_suspend"  x)
      (**  *)
      let s3_resume ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.s3_resume" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.s3_resume"  x)
      (**  *)
      let assert_agile ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        
        rpc_wrapper rpc "Async.VM.assert_agile" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM.assert_agile"  x)
      (**  *)
      let update_snapshot_metadata ~rpc ~session_id ~vm ~snapshot_of ~snapshot_time ~transportable_snapshot_id =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let snapshot_of = To.ref_VM snapshot_of in
        let snapshot_time = To.datetime snapshot_time in
        let transportable_snapshot_id = To.string transportable_snapshot_id in
        
        rpc_wrapper rpc "Async.VM.update_snapshot_metadata" [ session_id; vm; snapshot_of; snapshot_time; transportable_snapshot_id ] >>= fun x -> return (From.ref_task "return value of VM.update_snapshot_metadata"  x)
      (**  *)
      let retrieve_wlb_recommendations ~rpc ~session_id ~vm =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.VM.retrieve_wlb_recommendations" [ session_id; vm ] >>= fun x -> return (From.ref_task "return value of VM.retrieve_wlb_recommendations"  x)
      (**  *)
      let copy_bios_strings ~rpc ~session_id ~vm ~host =
        let session_id = To.ref_session session_id in
        let vm = To.ref_VM vm in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.VM.copy_bios_strings" [ session_id; vm; host ] >>= fun x -> return (From.ref_task "return value of VM.copy_bios_strings"  x)
      (**  *)
      let set_start_delay ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let value = To.int64 value in
        
        rpc_wrapper rpc "Async.VM.set_start_delay" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VM.set_start_delay"  x)
      (**  *)
      let set_shutdown_delay ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let value = To.int64 value in
        
        rpc_wrapper rpc "Async.VM.set_shutdown_delay" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VM.set_shutdown_delay"  x)
      (**  *)
      let set_order ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let value = To.int64 value in
        
        rpc_wrapper rpc "Async.VM.set_order" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VM.set_order"  x)
      (**  *)
      let set_suspend_VDI ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let value = To.ref_VDI value in
        
        rpc_wrapper rpc "Async.VM.set_suspend_VDI" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VM.set_suspend_VDI"  x)
      (**  *)
      let assert_can_be_recovered ~rpc ~session_id ~self ~session_to =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let session_to = To.ref_session session_to in
        
        rpc_wrapper rpc "Async.VM.assert_can_be_recovered" [ session_id; self; session_to ] >>= fun x -> return (From.ref_task "return value of VM.assert_can_be_recovered"  x)
      (**  *)
      let recover ~rpc ~session_id ~self ~session_to ~force =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let session_to = To.ref_session session_to in
        let force = To.bool force in
        
        rpc_wrapper rpc "Async.VM.recover" [ session_id; self; session_to; force ] >>= fun x -> return (From.ref_task "return value of VM.recover"  x)
      (**  *)
      let import_convert ~rpc ~session_id ~_type ~username ~password ~sr ~remote_config =
        let session_id = To.ref_session session_id in
        let _type = To.string _type in
        let username = To.string username in
        let password = To.string password in
        let sr = To.ref_SR sr in
        let remote_config = To.string_to_string_map remote_config in
        
        rpc_wrapper rpc "Async.VM.import_convert" [ session_id; _type; username; password; sr; remote_config ] >>= fun x -> return (From.ref_task "return value of VM.import_convert"  x)
      (**  *)
      let set_appliance ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        let value = To.ref_VM_appliance value in
        
        rpc_wrapper rpc "Async.VM.set_appliance" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VM.set_appliance"  x)
      (**  *)
      let query_services ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM self in
        
        rpc_wrapper rpc "Async.VM.query_services" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM.query_services"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~name_label:value.vM_name_label
        ~name_description:value.vM_name_description
        ~user_version:value.vM_user_version
        ~is_a_template:value.vM_is_a_template
        ~affinity:value.vM_affinity
        ~memory_target:value.vM_memory_target
        ~memory_static_max:value.vM_memory_static_max
        ~memory_dynamic_max:value.vM_memory_dynamic_max
        ~memory_dynamic_min:value.vM_memory_dynamic_min
        ~memory_static_min:value.vM_memory_static_min
        ~vCPUs_params:value.vM_VCPUs_params
        ~vCPUs_max:value.vM_VCPUs_max
        ~vCPUs_at_startup:value.vM_VCPUs_at_startup
        ~actions_after_shutdown:value.vM_actions_after_shutdown
        ~actions_after_reboot:value.vM_actions_after_reboot
        ~actions_after_crash:value.vM_actions_after_crash
        ~pV_bootloader:value.vM_PV_bootloader
        ~pV_kernel:value.vM_PV_kernel
        ~pV_ramdisk:value.vM_PV_ramdisk
        ~pV_args:value.vM_PV_args
        ~pV_bootloader_args:value.vM_PV_bootloader_args
        ~pV_legacy_args:value.vM_PV_legacy_args
        ~hVM_boot_policy:value.vM_HVM_boot_policy
        ~hVM_boot_params:value.vM_HVM_boot_params
        ~hVM_shadow_multiplier:value.vM_HVM_shadow_multiplier
        ~platform:value.vM_platform
        ~pCI_bus:value.vM_PCI_bus
        ~other_config:value.vM_other_config
        ~recommendations:value.vM_recommendations
        ~xenstore_data:value.vM_xenstore_data
        ~ha_always_run:value.vM_ha_always_run
        ~ha_restart_priority:value.vM_ha_restart_priority
        ~tags:value.vM_tags
        ~blocked_operations:value.vM_blocked_operations
        ~protection_policy:value.vM_protection_policy
        ~is_snapshot_from_vmpp:value.vM_is_snapshot_from_vmpp
        ~appliance:value.vM_appliance
        ~start_delay:value.vM_start_delay
        ~shutdown_delay:value.vM_shutdown_delay
        ~order:value.vM_order
        ~suspend_SR:value.vM_suspend_SR
        ~version:value.vM_version
    end
    module VM_metrics = struct
    end
    module VM_guest_metrics = struct
    end
    module VMPP = struct
      (**  *)
      let create ~rpc ~session_id ~name_label ~name_description ~is_policy_enabled ~backup_type ~backup_retention_value ~backup_frequency ~backup_schedule ~archive_target_type ~archive_target_config ~archive_frequency ~archive_schedule ~is_alarm_enabled ~alarm_config =
        let session_id = To.ref_session session_id in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let is_policy_enabled = To.bool is_policy_enabled in
        let backup_type = To.vmpp_backup_type backup_type in
        let backup_retention_value = To.int64 backup_retention_value in
        let backup_frequency = To.vmpp_backup_frequency backup_frequency in
        let backup_schedule = To.string_to_string_map backup_schedule in
        let archive_target_type = To.vmpp_archive_target_type archive_target_type in
        let archive_target_config = To.string_to_string_map archive_target_config in
        let archive_frequency = To.vmpp_archive_frequency archive_frequency in
        let archive_schedule = To.string_to_string_map archive_schedule in
        let is_alarm_enabled = To.bool is_alarm_enabled in
        let alarm_config = To.string_to_string_map alarm_config in
        let __structure = To.structure [ "name_label", name_label; "name_description", name_description; "is_policy_enabled", is_policy_enabled; "backup_type", backup_type; "backup_retention_value", backup_retention_value; "backup_frequency", backup_frequency; "backup_schedule", backup_schedule; "archive_target_type", archive_target_type; "archive_target_config", archive_target_config; "archive_frequency", archive_frequency; "archive_schedule", archive_schedule; "is_alarm_enabled", is_alarm_enabled; "alarm_config", alarm_config] in
        rpc_wrapper rpc "Async.VMPP.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of VMPP.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VMPP self in
        
        rpc_wrapper rpc "Async.VMPP.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VMPP.destroy"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~name_label:value.vMPP_name_label
        ~name_description:value.vMPP_name_description
        ~is_policy_enabled:value.vMPP_is_policy_enabled
        ~backup_type:value.vMPP_backup_type
        ~backup_retention_value:value.vMPP_backup_retention_value
        ~backup_frequency:value.vMPP_backup_frequency
        ~backup_schedule:value.vMPP_backup_schedule
        ~archive_target_type:value.vMPP_archive_target_type
        ~archive_target_config:value.vMPP_archive_target_config
        ~archive_frequency:value.vMPP_archive_frequency
        ~archive_schedule:value.vMPP_archive_schedule
        ~is_alarm_enabled:value.vMPP_is_alarm_enabled
        ~alarm_config:value.vMPP_alarm_config
    end
    module VM_appliance = struct
      (**  *)
      let create ~rpc ~session_id ~name_label ~name_description =
        let session_id = To.ref_session session_id in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let __structure = To.structure [ "name_label", name_label; "name_description", name_description] in
        rpc_wrapper rpc "Async.VM_appliance.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of VM_appliance.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM_appliance self in
        
        rpc_wrapper rpc "Async.VM_appliance.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM_appliance.destroy"  x)
      (**  *)
      let start ~rpc ~session_id ~self ~paused =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM_appliance self in
        let paused = To.bool paused in
        
        rpc_wrapper rpc "Async.VM_appliance.start" [ session_id; self; paused ] >>= fun x -> return (From.ref_task "return value of VM_appliance.start"  x)
      (**  *)
      let clean_shutdown ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM_appliance self in
        
        rpc_wrapper rpc "Async.VM_appliance.clean_shutdown" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM_appliance.clean_shutdown"  x)
      (**  *)
      let hard_shutdown ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM_appliance self in
        
        rpc_wrapper rpc "Async.VM_appliance.hard_shutdown" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM_appliance.hard_shutdown"  x)
      (**  *)
      let shutdown ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM_appliance self in
        
        rpc_wrapper rpc "Async.VM_appliance.shutdown" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VM_appliance.shutdown"  x)
      (**  *)
      let assert_can_be_recovered ~rpc ~session_id ~self ~session_to =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM_appliance self in
        let session_to = To.ref_session session_to in
        
        rpc_wrapper rpc "Async.VM_appliance.assert_can_be_recovered" [ session_id; self; session_to ] >>= fun x -> return (From.ref_task "return value of VM_appliance.assert_can_be_recovered"  x)
      (**  *)
      let recover ~rpc ~session_id ~self ~session_to ~force =
        let session_id = To.ref_session session_id in
        let self = To.ref_VM_appliance self in
        let session_to = To.ref_session session_to in
        let force = To.bool force in
        
        rpc_wrapper rpc "Async.VM_appliance.recover" [ session_id; self; session_to; force ] >>= fun x -> return (From.ref_task "return value of VM_appliance.recover"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~name_label:value.vM_appliance_name_label
        ~name_description:value.vM_appliance_name_description
    end
    module DR_task = struct
      (**  *)
      let create ~rpc ~session_id ~_type ~device_config ~whitelist =
        let session_id = To.ref_session session_id in
        let _type = To.string _type in
        let device_config = To.string_to_string_map device_config in
        let whitelist = To.string_set whitelist in
        
        rpc_wrapper rpc "Async.DR_task.create" [ session_id; _type; device_config; whitelist ] >>= fun x -> return (From.ref_task "return value of DR_task.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_DR_task self in
        
        rpc_wrapper rpc "Async.DR_task.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of DR_task.destroy"  x)
    end
    module Host = struct
      (**  *)
      let disable ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.disable" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.disable"  x)
      (**  *)
      let enable ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.enable" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.enable"  x)
      (**  *)
      let shutdown ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.shutdown" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.shutdown"  x)
      (**  *)
      let reboot ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.reboot" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.reboot"  x)
      (**  *)
      let dmesg ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.dmesg" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.dmesg"  x)
      (**  *)
      let dmesg_clear ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.dmesg_clear" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.dmesg_clear"  x)
      (**  *)
      let get_log ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.get_log" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.get_log"  x)
      (**  *)
      let send_debug_keys ~rpc ~session_id ~host ~keys =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let keys = To.string keys in
        
        rpc_wrapper rpc "Async.host.send_debug_keys" [ session_id; host; keys ] >>= fun x -> return (From.ref_task "return value of host.send_debug_keys"  x)
      (**  *)
      let bugreport_upload ~rpc ~session_id ~host ~url ~options =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let url = To.string url in
        let options = To.string_to_string_map options in
        
        rpc_wrapper rpc "Async.host.bugreport_upload" [ session_id; host; url; options ] >>= fun x -> return (From.ref_task "return value of host.bugreport_upload"  x)
      (**  *)
      let license_apply ~rpc ~session_id ~host ~contents =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let contents = To.string contents in
        
        rpc_wrapper rpc "Async.host.license_apply" [ session_id; host; contents ] >>= fun x -> return (From.ref_task "return value of host.license_apply"  x)
      (**  *)
      let create ~rpc ~session_id ~uuid ~name_label ~name_description ~hostname ~address ~external_auth_type ~external_auth_service_name ~external_auth_configuration ~license_params ~edition ~license_server ~local_cache_sr ~chipset_info =
        let session_id = To.ref_session session_id in
        let uuid = To.string uuid in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let hostname = To.string hostname in
        let address = To.string address in
        let external_auth_type = To.string external_auth_type in
        let external_auth_service_name = To.string external_auth_service_name in
        let external_auth_configuration = To.string_to_string_map external_auth_configuration in
        let license_params = To.string_to_string_map license_params in
        let edition = To.string edition in
        let license_server = To.string_to_string_map license_server in
        let local_cache_sr = To.ref_SR local_cache_sr in
        let chipset_info = To.string_to_string_map chipset_info in
        
        rpc_wrapper rpc "Async.host.create" [ session_id; uuid; name_label; name_description; hostname; address; external_auth_type; external_auth_service_name; external_auth_configuration; license_params; edition; license_server; local_cache_sr; chipset_info ] >>= fun x -> return (From.ref_task "return value of host.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_host self in
        
        rpc_wrapper rpc "Async.host.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of host.destroy"  x)
      (**  *)
      let power_on ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.power_on" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.power_on"  x)
      (**  *)
      let set_license_params ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_host self in
        let value = To.string_to_string_map value in
        
        rpc_wrapper rpc "Async.host.set_license_params" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of host.set_license_params"  x)
      (**  *)
      let ha_disarm_fencing ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.ha_disarm_fencing" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.ha_disarm_fencing"  x)
      (**  *)
      let preconfigure_ha ~rpc ~session_id ~host ~statefiles ~metadata_vdi ~generation =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let statefiles = To.ref_VDI_set statefiles in
        let metadata_vdi = To.ref_VDI metadata_vdi in
        let generation = To.string generation in
        
        rpc_wrapper rpc "Async.host.preconfigure_ha" [ session_id; host; statefiles; metadata_vdi; generation ] >>= fun x -> return (From.ref_task "return value of host.preconfigure_ha"  x)
      (**  *)
      let ha_join_liveset ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.ha_join_liveset" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.ha_join_liveset"  x)
      (**  *)
      let ha_disable_failover_decisions ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.ha_disable_failover_decisions" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.ha_disable_failover_decisions"  x)
      (**  *)
      let ha_wait_for_shutdown_via_statefile ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.ha_wait_for_shutdown_via_statefile" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.ha_wait_for_shutdown_via_statefile"  x)
      (**  *)
      let ha_stop_daemon ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.ha_stop_daemon" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.ha_stop_daemon"  x)
      (**  *)
      let ha_release_resources ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.ha_release_resources" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.ha_release_resources"  x)
      (**  *)
      let assert_can_evacuate ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.assert_can_evacuate" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.assert_can_evacuate"  x)
      (**  *)
      let get_vms_which_prevent_evacuation ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_host self in
        
        rpc_wrapper rpc "Async.host.get_vms_which_prevent_evacuation" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of host.get_vms_which_prevent_evacuation"  x)
      (**  *)
      let get_uncooperative_resident_VMs ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_host self in
        
        rpc_wrapper rpc "Async.host.get_uncooperative_resident_VMs" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of host.get_uncooperative_resident_VMs"  x)
      (**  *)
      let get_uncooperative_domains ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_host self in
        
        rpc_wrapper rpc "Async.host.get_uncooperative_domains" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of host.get_uncooperative_domains"  x)
      (**  *)
      let evacuate ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.evacuate" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.evacuate"  x)
      (**  *)
      let notify ~rpc ~session_id ~ty ~params =
        let session_id = To.ref_session session_id in
        let ty = To.string ty in
        let params = To.string params in
        
        rpc_wrapper rpc "Async.host.notify" [ session_id; ty; params ] >>= fun x -> return (From.ref_task "return value of host.notify"  x)
      (**  *)
      let syslog_reconfigure ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.syslog_reconfigure" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.syslog_reconfigure"  x)
      (**  *)
      let management_reconfigure ~rpc ~session_id ~pif =
        let session_id = To.ref_session session_id in
        let pif = To.ref_PIF pif in
        
        rpc_wrapper rpc "Async.host.management_reconfigure" [ session_id; pif ] >>= fun x -> return (From.ref_task "return value of host.management_reconfigure"  x)
      (**  *)
      let get_management_interface ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.get_management_interface" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.get_management_interface"  x)
      (**  *)
      let restart_agent ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.restart_agent" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.restart_agent"  x)
      (**  *)
      let compute_free_memory ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.compute_free_memory" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.compute_free_memory"  x)
      (**  *)
      let compute_memory_overhead ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.compute_memory_overhead" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.compute_memory_overhead"  x)
      (**  *)
      let create_new_blob ~rpc ~session_id ~host ~name ~mime_type ~public =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let name = To.string name in
        let mime_type = To.string mime_type in
        let public = To.bool public in
        
        rpc_wrapper rpc "Async.host.create_new_blob" [ session_id; host; name; mime_type; public ] >>= fun x -> return (From.ref_task "return value of host.create_new_blob"  x)
      (**  *)
      let call_plugin ~rpc ~session_id ~host ~plugin ~fn ~args =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let plugin = To.string plugin in
        let fn = To.string fn in
        let args = To.string_to_string_map args in
        
        rpc_wrapper rpc "Async.host.call_plugin" [ session_id; host; plugin; fn; args ] >>= fun x -> return (From.ref_task "return value of host.call_plugin"  x)
      (**  *)
      let enable_binary_storage ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.enable_binary_storage" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.enable_binary_storage"  x)
      (**  *)
      let disable_binary_storage ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.disable_binary_storage" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.disable_binary_storage"  x)
      (**  *)
      let retrieve_wlb_evacuate_recommendations ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_host self in
        
        rpc_wrapper rpc "Async.host.retrieve_wlb_evacuate_recommendations" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of host.retrieve_wlb_evacuate_recommendations"  x)
      (**  *)
      let certificate_install ~rpc ~session_id ~host ~name ~cert =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let name = To.string name in
        let cert = To.string cert in
        
        rpc_wrapper rpc "Async.host.certificate_install" [ session_id; host; name; cert ] >>= fun x -> return (From.ref_task "return value of host.certificate_install"  x)
      (**  *)
      let certificate_uninstall ~rpc ~session_id ~host ~name =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let name = To.string name in
        
        rpc_wrapper rpc "Async.host.certificate_uninstall" [ session_id; host; name ] >>= fun x -> return (From.ref_task "return value of host.certificate_uninstall"  x)
      (**  *)
      let certificate_list ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.certificate_list" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.certificate_list"  x)
      (**  *)
      let crl_install ~rpc ~session_id ~host ~name ~crl =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let name = To.string name in
        let crl = To.string crl in
        
        rpc_wrapper rpc "Async.host.crl_install" [ session_id; host; name; crl ] >>= fun x -> return (From.ref_task "return value of host.crl_install"  x)
      (**  *)
      let crl_uninstall ~rpc ~session_id ~host ~name =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let name = To.string name in
        
        rpc_wrapper rpc "Async.host.crl_uninstall" [ session_id; host; name ] >>= fun x -> return (From.ref_task "return value of host.crl_uninstall"  x)
      (**  *)
      let crl_list ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.crl_list" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.crl_list"  x)
      (**  *)
      let certificate_sync ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.certificate_sync" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.certificate_sync"  x)
      (**  *)
      let get_server_certificate ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.get_server_certificate" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.get_server_certificate"  x)
      (**  *)
      let update_pool_secret ~rpc ~session_id ~host ~pool_secret =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let pool_secret = To.string pool_secret in
        
        rpc_wrapper rpc "Async.host.update_pool_secret" [ session_id; host; pool_secret ] >>= fun x -> return (From.ref_task "return value of host.update_pool_secret"  x)
      (**  *)
      let update_master ~rpc ~session_id ~host ~master_address =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let master_address = To.string master_address in
        
        rpc_wrapper rpc "Async.host.update_master" [ session_id; host; master_address ] >>= fun x -> return (From.ref_task "return value of host.update_master"  x)
      (**  *)
      let attach_static_vdis ~rpc ~session_id ~host ~vdi_reason_map =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let vdi_reason_map = To.ref_VDI_to_string_map vdi_reason_map in
        
        rpc_wrapper rpc "Async.host.attach_static_vdis" [ session_id; host; vdi_reason_map ] >>= fun x -> return (From.ref_task "return value of host.attach_static_vdis"  x)
      (**  *)
      let detach_static_vdis ~rpc ~session_id ~host ~vdis =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let vdis = To.ref_VDI_set vdis in
        
        rpc_wrapper rpc "Async.host.detach_static_vdis" [ session_id; host; vdis ] >>= fun x -> return (From.ref_task "return value of host.detach_static_vdis"  x)
      (**  *)
      let set_localdb_key ~rpc ~session_id ~host ~key ~value =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let key = To.string key in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.host.set_localdb_key" [ session_id; host; key; value ] >>= fun x -> return (From.ref_task "return value of host.set_localdb_key"  x)
      (**  *)
      let refresh_pack_info ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.refresh_pack_info" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.refresh_pack_info"  x)
      (**  *)
      let set_power_on_mode ~rpc ~session_id ~self ~power_on_mode ~power_on_config =
        let session_id = To.ref_session session_id in
        let self = To.ref_host self in
        let power_on_mode = To.string power_on_mode in
        let power_on_config = To.string_to_string_map power_on_config in
        
        rpc_wrapper rpc "Async.host.set_power_on_mode" [ session_id; self; power_on_mode; power_on_config ] >>= fun x -> return (From.ref_task "return value of host.set_power_on_mode"  x)
      (**  *)
      let reset_networking ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.host.reset_networking" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of host.reset_networking"  x)
      (**  *)
      let migrate_receive ~rpc ~session_id ~host ~network ~options =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let network = To.ref_network network in
        let options = To.string_to_string_map options in
        
        rpc_wrapper rpc "Async.host.migrate_receive" [ session_id; host; network; options ] >>= fun x -> return (From.ref_task "return value of host.migrate_receive"  x)
    end
    module Host_crashdump = struct
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_host_crashdump self in
        
        rpc_wrapper rpc "Async.host_crashdump.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of host_crashdump.destroy"  x)
      (**  *)
      let upload ~rpc ~session_id ~self ~url ~options =
        let session_id = To.ref_session session_id in
        let self = To.ref_host_crashdump self in
        let url = To.string url in
        let options = To.string_to_string_map options in
        
        rpc_wrapper rpc "Async.host_crashdump.upload" [ session_id; self; url; options ] >>= fun x -> return (From.ref_task "return value of host_crashdump.upload"  x)
    end
    module Host_patch = struct
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_host_patch self in
        
        rpc_wrapper rpc "Async.host_patch.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of host_patch.destroy"  x)
      (**  *)
      let apply ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_host_patch self in
        
        rpc_wrapper rpc "Async.host_patch.apply" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of host_patch.apply"  x)
    end
    module Host_metrics = struct
    end
    module Host_cpu = struct
    end
    module Network = struct
      (**  *)
      let create ~rpc ~session_id ~name_label ~name_description ~mTU ~other_config ~tags =
        let session_id = To.ref_session session_id in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let mTU = To.int64 mTU in
        let other_config = To.string_to_string_map other_config in
        let tags = To.string_set tags in
        let __structure = To.structure [ "name_label", name_label; "name_description", name_description; "MTU", mTU; "other_config", other_config; "tags", tags] in
        rpc_wrapper rpc "Async.network.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of network.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_network self in
        
        rpc_wrapper rpc "Async.network.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of network.destroy"  x)
      (**  *)
      let attach ~rpc ~session_id ~network ~host =
        let session_id = To.ref_session session_id in
        let network = To.ref_network network in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.network.attach" [ session_id; network; host ] >>= fun x -> return (From.ref_task "return value of network.attach"  x)
      (**  *)
      let pool_introduce ~rpc ~session_id ~name_label ~name_description ~mTU ~other_config ~bridge =
        let session_id = To.ref_session session_id in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let mTU = To.int64 mTU in
        let other_config = To.string_to_string_map other_config in
        let bridge = To.string bridge in
        
        rpc_wrapper rpc "Async.network.pool_introduce" [ session_id; name_label; name_description; mTU; other_config; bridge ] >>= fun x -> return (From.ref_task "return value of network.pool_introduce"  x)
      (**  *)
      let create_new_blob ~rpc ~session_id ~network ~name ~mime_type ~public =
        let session_id = To.ref_session session_id in
        let network = To.ref_network network in
        let name = To.string name in
        let mime_type = To.string mime_type in
        let public = To.bool public in
        
        rpc_wrapper rpc "Async.network.create_new_blob" [ session_id; network; name; mime_type; public ] >>= fun x -> return (From.ref_task "return value of network.create_new_blob"  x)
      (**  *)
      let set_default_locking_mode ~rpc ~session_id ~network ~value =
        let session_id = To.ref_session session_id in
        let network = To.ref_network network in
        let value = To.network_default_locking_mode value in
        
        rpc_wrapper rpc "Async.network.set_default_locking_mode" [ session_id; network; value ] >>= fun x -> return (From.ref_task "return value of network.set_default_locking_mode"  x)
      (**  *)
      let attach_for_vm ~rpc ~session_id ~host ~vm =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.network.attach_for_vm" [ session_id; host; vm ] >>= fun x -> return (From.ref_task "return value of network.attach_for_vm"  x)
      (**  *)
      let detach_for_vm ~rpc ~session_id ~host ~vm =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let vm = To.ref_VM vm in
        
        rpc_wrapper rpc "Async.network.detach_for_vm" [ session_id; host; vm ] >>= fun x -> return (From.ref_task "return value of network.detach_for_vm"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~name_label:value.network_name_label
        ~name_description:value.network_name_description
        ~mTU:value.network_MTU
        ~other_config:value.network_other_config
        ~tags:value.network_tags
    end
    module VIF = struct
      (**  *)
      let create ~rpc ~session_id ~device ~network ~vM ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params =
        let session_id = To.ref_session session_id in
        let device = To.string device in
        let network = To.ref_network network in
        let vM = To.ref_VM vM in
        let mAC = To.string mAC in
        let mTU = To.int64 mTU in
        let other_config = To.string_to_string_map other_config in
        let qos_algorithm_type = To.string qos_algorithm_type in
        let qos_algorithm_params = To.string_to_string_map qos_algorithm_params in
        let __structure = To.structure [ "device", device; "network", network; "VM", vM; "MAC", mAC; "MTU", mTU; "other_config", other_config; "qos_algorithm_type", qos_algorithm_type; "qos_algorithm_params", qos_algorithm_params] in
        rpc_wrapper rpc "Async.VIF.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of VIF.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        
        rpc_wrapper rpc "Async.VIF.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VIF.destroy"  x)
      (**  *)
      let plug ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        
        rpc_wrapper rpc "Async.VIF.plug" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VIF.plug"  x)
      (**  *)
      let unplug ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        
        rpc_wrapper rpc "Async.VIF.unplug" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VIF.unplug"  x)
      (**  *)
      let unplug_force ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        
        rpc_wrapper rpc "Async.VIF.unplug_force" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VIF.unplug_force"  x)
      (**  *)
      let set_locking_mode ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        let value = To.vif_locking_mode value in
        
        rpc_wrapper rpc "Async.VIF.set_locking_mode" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VIF.set_locking_mode"  x)
      (**  *)
      let set_ipv4_allowed ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        let value = To.string_set value in
        
        rpc_wrapper rpc "Async.VIF.set_ipv4_allowed" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VIF.set_ipv4_allowed"  x)
      (**  *)
      let add_ipv4_allowed ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.VIF.add_ipv4_allowed" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VIF.add_ipv4_allowed"  x)
      (**  *)
      let remove_ipv4_allowed ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.VIF.remove_ipv4_allowed" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VIF.remove_ipv4_allowed"  x)
      (**  *)
      let set_ipv6_allowed ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        let value = To.string_set value in
        
        rpc_wrapper rpc "Async.VIF.set_ipv6_allowed" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VIF.set_ipv6_allowed"  x)
      (**  *)
      let add_ipv6_allowed ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.VIF.add_ipv6_allowed" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VIF.add_ipv6_allowed"  x)
      (**  *)
      let remove_ipv6_allowed ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VIF self in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.VIF.remove_ipv6_allowed" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VIF.remove_ipv6_allowed"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~device:value.vIF_device
        ~network:value.vIF_network
        ~vM:value.vIF_VM
        ~mAC:value.vIF_MAC
        ~mTU:value.vIF_MTU
        ~other_config:value.vIF_other_config
        ~qos_algorithm_type:value.vIF_qos_algorithm_type
        ~qos_algorithm_params:value.vIF_qos_algorithm_params
    end
    module VIF_metrics = struct
    end
    module PIF = struct
      (**  *)
      let create_VLAN ~rpc ~session_id ~device ~network ~host ~vLAN =
        let session_id = To.ref_session session_id in
        let device = To.string device in
        let network = To.ref_network network in
        let host = To.ref_host host in
        let vLAN = To.int64 vLAN in
        
        rpc_wrapper rpc "Async.PIF.create_VLAN" [ session_id; device; network; host; vLAN ] >>= fun x -> return (From.ref_task "return value of PIF.create_VLAN"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_PIF self in
        
        rpc_wrapper rpc "Async.PIF.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of PIF.destroy"  x)
      (**  *)
      let reconfigure_ip ~rpc ~session_id ~self ~mode ~iP ~netmask ~gateway ~dNS =
        let session_id = To.ref_session session_id in
        let self = To.ref_PIF self in
        let mode = To.ip_configuration_mode mode in
        let iP = To.string iP in
        let netmask = To.string netmask in
        let gateway = To.string gateway in
        let dNS = To.string dNS in
        
        rpc_wrapper rpc "Async.PIF.reconfigure_ip" [ session_id; self; mode; iP; netmask; gateway; dNS ] >>= fun x -> return (From.ref_task "return value of PIF.reconfigure_ip"  x)
      (**  *)
      let reconfigure_ipv6 ~rpc ~session_id ~self ~mode ~iPv6 ~gateway ~dNS =
        let session_id = To.ref_session session_id in
        let self = To.ref_PIF self in
        let mode = To.ipv6_configuration_mode mode in
        let iPv6 = To.string iPv6 in
        let gateway = To.string gateway in
        let dNS = To.string dNS in
        
        rpc_wrapper rpc "Async.PIF.reconfigure_ipv6" [ session_id; self; mode; iPv6; gateway; dNS ] >>= fun x -> return (From.ref_task "return value of PIF.reconfigure_ipv6"  x)
      (**  *)
      let set_primary_address_type ~rpc ~session_id ~self ~primary_address_type =
        let session_id = To.ref_session session_id in
        let self = To.ref_PIF self in
        let primary_address_type = To.primary_address_type primary_address_type in
        
        rpc_wrapper rpc "Async.PIF.set_primary_address_type" [ session_id; self; primary_address_type ] >>= fun x -> return (From.ref_task "return value of PIF.set_primary_address_type"  x)
      (**  *)
      let scan ~rpc ~session_id ~host =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        
        rpc_wrapper rpc "Async.PIF.scan" [ session_id; host ] >>= fun x -> return (From.ref_task "return value of PIF.scan"  x)
      (**  *)
      let introduce ~rpc ~session_id ~host ~mAC ~device =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let mAC = To.string mAC in
        let device = To.string device in
        
        rpc_wrapper rpc "Async.PIF.introduce" [ session_id; host; mAC; device ] >>= fun x -> return (From.ref_task "return value of PIF.introduce"  x)
      (**  *)
      let forget ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_PIF self in
        
        rpc_wrapper rpc "Async.PIF.forget" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of PIF.forget"  x)
      (**  *)
      let unplug ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_PIF self in
        
        rpc_wrapper rpc "Async.PIF.unplug" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of PIF.unplug"  x)
      (**  *)
      let plug ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_PIF self in
        
        rpc_wrapper rpc "Async.PIF.plug" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of PIF.plug"  x)
      (**  *)
      let pool_introduce ~rpc ~session_id ~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode ~iP ~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug ~ipv6_configuration_mode ~iPv6 ~ipv6_gateway ~primary_address_type =
        let session_id = To.ref_session session_id in
        let device = To.string device in
        let network = To.ref_network network in
        let host = To.ref_host host in
        let mAC = To.string mAC in
        let mTU = To.int64 mTU in
        let vLAN = To.int64 vLAN in
        let physical = To.bool physical in
        let ip_configuration_mode = To.ip_configuration_mode ip_configuration_mode in
        let iP = To.string iP in
        let netmask = To.string netmask in
        let gateway = To.string gateway in
        let dNS = To.string dNS in
        let bond_slave_of = To.ref_Bond bond_slave_of in
        let vLAN_master_of = To.ref_VLAN vLAN_master_of in
        let management = To.bool management in
        let other_config = To.string_to_string_map other_config in
        let disallow_unplug = To.bool disallow_unplug in
        let ipv6_configuration_mode = To.ipv6_configuration_mode ipv6_configuration_mode in
        let iPv6 = To.string_set iPv6 in
        let ipv6_gateway = To.string ipv6_gateway in
        let primary_address_type = To.primary_address_type primary_address_type in
        
        rpc_wrapper rpc "Async.PIF.pool_introduce" [ session_id; device; network; host; mAC; mTU; vLAN; physical; ip_configuration_mode; iP; netmask; gateway; dNS; bond_slave_of; vLAN_master_of; management; other_config; disallow_unplug; ipv6_configuration_mode; iPv6; ipv6_gateway; primary_address_type ] >>= fun x -> return (From.ref_task "return value of PIF.pool_introduce"  x)
      (**  *)
      let db_introduce ~rpc ~session_id ~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode ~iP ~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug ~ipv6_configuration_mode ~iPv6 ~ipv6_gateway ~primary_address_type =
        let session_id = To.ref_session session_id in
        let device = To.string device in
        let network = To.ref_network network in
        let host = To.ref_host host in
        let mAC = To.string mAC in
        let mTU = To.int64 mTU in
        let vLAN = To.int64 vLAN in
        let physical = To.bool physical in
        let ip_configuration_mode = To.ip_configuration_mode ip_configuration_mode in
        let iP = To.string iP in
        let netmask = To.string netmask in
        let gateway = To.string gateway in
        let dNS = To.string dNS in
        let bond_slave_of = To.ref_Bond bond_slave_of in
        let vLAN_master_of = To.ref_VLAN vLAN_master_of in
        let management = To.bool management in
        let other_config = To.string_to_string_map other_config in
        let disallow_unplug = To.bool disallow_unplug in
        let ipv6_configuration_mode = To.ipv6_configuration_mode ipv6_configuration_mode in
        let iPv6 = To.string_set iPv6 in
        let ipv6_gateway = To.string ipv6_gateway in
        let primary_address_type = To.primary_address_type primary_address_type in
        
        rpc_wrapper rpc "Async.PIF.db_introduce" [ session_id; device; network; host; mAC; mTU; vLAN; physical; ip_configuration_mode; iP; netmask; gateway; dNS; bond_slave_of; vLAN_master_of; management; other_config; disallow_unplug; ipv6_configuration_mode; iPv6; ipv6_gateway; primary_address_type ] >>= fun x -> return (From.ref_task "return value of PIF.db_introduce"  x)
      (**  *)
      let db_forget ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_PIF self in
        
        rpc_wrapper rpc "Async.PIF.db_forget" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of PIF.db_forget"  x)
    end
    module PIF_metrics = struct
    end
    module Bond = struct
      (**  *)
      let create ~rpc ~session_id ~network ~members ~mAC ~mode ~properties =
        let session_id = To.ref_session session_id in
        let network = To.ref_network network in
        let members = To.ref_PIF_set members in
        let mAC = To.string mAC in
        let mode = To.bond_mode mode in
        let properties = To.string_to_string_map properties in
        
        rpc_wrapper rpc "Async.Bond.create" [ session_id; network; members; mAC; mode; properties ] >>= fun x -> return (From.ref_task "return value of Bond.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_Bond self in
        
        rpc_wrapper rpc "Async.Bond.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of Bond.destroy"  x)
      (**  *)
      let set_mode ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_Bond self in
        let value = To.bond_mode value in
        
        rpc_wrapper rpc "Async.Bond.set_mode" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of Bond.set_mode"  x)
      (**  *)
      let set_property ~rpc ~session_id ~self ~name ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_Bond self in
        let name = To.string name in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.Bond.set_property" [ session_id; self; name; value ] >>= fun x -> return (From.ref_task "return value of Bond.set_property"  x)
    end
    module VLAN = struct
      (**  *)
      let create ~rpc ~session_id ~tagged_PIF ~tag ~network =
        let session_id = To.ref_session session_id in
        let tagged_PIF = To.ref_PIF tagged_PIF in
        let tag = To.int64 tag in
        let network = To.ref_network network in
        
        rpc_wrapper rpc "Async.VLAN.create" [ session_id; tagged_PIF; tag; network ] >>= fun x -> return (From.ref_task "return value of VLAN.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VLAN self in
        
        rpc_wrapper rpc "Async.VLAN.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VLAN.destroy"  x)
    end
    module SM = struct
    end
    module SR = struct
      (**  *)
      let create ~rpc ~session_id ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~shared ~sm_config =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let device_config = To.string_to_string_map device_config in
        let physical_size = To.int64 physical_size in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let _type = To.string _type in
        let content_type = To.string content_type in
        let shared = To.bool shared in
        let sm_config = To.string_to_string_map sm_config in
        
        rpc_wrapper rpc "Async.SR.create" [ session_id; host; device_config; physical_size; name_label; name_description; _type; content_type; shared; sm_config ] >>= fun x -> return (From.ref_task "return value of SR.create"  x)
      (**  *)
      let introduce ~rpc ~session_id ~uuid ~name_label ~name_description ~_type ~content_type ~shared ~sm_config =
        let session_id = To.ref_session session_id in
        let uuid = To.string uuid in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let _type = To.string _type in
        let content_type = To.string content_type in
        let shared = To.bool shared in
        let sm_config = To.string_to_string_map sm_config in
        
        rpc_wrapper rpc "Async.SR.introduce" [ session_id; uuid; name_label; name_description; _type; content_type; shared; sm_config ] >>= fun x -> return (From.ref_task "return value of SR.introduce"  x)
      (**  *)
      let make ~rpc ~session_id ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~sm_config =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let device_config = To.string_to_string_map device_config in
        let physical_size = To.int64 physical_size in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let _type = To.string _type in
        let content_type = To.string content_type in
        let sm_config = To.string_to_string_map sm_config in
        
        rpc_wrapper rpc "Async.SR.make" [ session_id; host; device_config; physical_size; name_label; name_description; _type; content_type; sm_config ] >>= fun x -> return (From.ref_task "return value of SR.make"  x)
      (**  *)
      let destroy ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.SR.destroy" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of SR.destroy"  x)
      (**  *)
      let forget ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.SR.forget" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of SR.forget"  x)
      (**  *)
      let update ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.SR.update" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of SR.update"  x)
      (**  *)
      let scan ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.SR.scan" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of SR.scan"  x)
      (**  *)
      let probe ~rpc ~session_id ~host ~device_config ~_type ~sm_config =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let device_config = To.string_to_string_map device_config in
        let _type = To.string _type in
        let sm_config = To.string_to_string_map sm_config in
        
        rpc_wrapper rpc "Async.SR.probe" [ session_id; host; device_config; _type; sm_config ] >>= fun x -> return (From.ref_task "return value of SR.probe"  x)
      (**  *)
      let set_shared ~rpc ~session_id ~sr ~value =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        let value = To.bool value in
        
        rpc_wrapper rpc "Async.SR.set_shared" [ session_id; sr; value ] >>= fun x -> return (From.ref_task "return value of SR.set_shared"  x)
      (**  *)
      let set_name_label ~rpc ~session_id ~sr ~value =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.SR.set_name_label" [ session_id; sr; value ] >>= fun x -> return (From.ref_task "return value of SR.set_name_label"  x)
      (**  *)
      let set_name_description ~rpc ~session_id ~sr ~value =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.SR.set_name_description" [ session_id; sr; value ] >>= fun x -> return (From.ref_task "return value of SR.set_name_description"  x)
      (**  *)
      let create_new_blob ~rpc ~session_id ~sr ~name ~mime_type ~public =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        let name = To.string name in
        let mime_type = To.string mime_type in
        let public = To.bool public in
        
        rpc_wrapper rpc "Async.SR.create_new_blob" [ session_id; sr; name; mime_type; public ] >>= fun x -> return (From.ref_task "return value of SR.create_new_blob"  x)
      (**  *)
      let assert_can_host_ha_statefile ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.SR.assert_can_host_ha_statefile" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of SR.assert_can_host_ha_statefile"  x)
      (**  *)
      let assert_supports_database_replication ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.SR.assert_supports_database_replication" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of SR.assert_supports_database_replication"  x)
      (**  *)
      let enable_database_replication ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.SR.enable_database_replication" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of SR.enable_database_replication"  x)
      (**  *)
      let disable_database_replication ~rpc ~session_id ~sr =
        let session_id = To.ref_session session_id in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.SR.disable_database_replication" [ session_id; sr ] >>= fun x -> return (From.ref_task "return value of SR.disable_database_replication"  x)
    end
    module VDI = struct
      (**  *)
      let create ~rpc ~session_id ~name_label ~name_description ~sR ~virtual_size ~_type ~sharable ~read_only ~other_config ~xenstore_data ~sm_config ~tags =
        let session_id = To.ref_session session_id in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let sR = To.ref_SR sR in
        let virtual_size = To.int64 virtual_size in
        let _type = To.vdi_type _type in
        let sharable = To.bool sharable in
        let read_only = To.bool read_only in
        let other_config = To.string_to_string_map other_config in
        let xenstore_data = To.string_to_string_map xenstore_data in
        let sm_config = To.string_to_string_map sm_config in
        let tags = To.string_set tags in
        let __structure = To.structure [ "name_label", name_label; "name_description", name_description; "SR", sR; "virtual_size", virtual_size; "type", _type; "sharable", sharable; "read_only", read_only; "other_config", other_config; "xenstore_data", xenstore_data; "sm_config", sm_config; "tags", tags] in
        rpc_wrapper rpc "Async.VDI.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of VDI.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VDI self in
        
        rpc_wrapper rpc "Async.VDI.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VDI.destroy"  x)
      (**  *)
      let snapshot ~rpc ~session_id ~vdi ~driver_params =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        let driver_params = To.string_to_string_map driver_params in
        
        rpc_wrapper rpc "Async.VDI.snapshot" [ session_id; vdi; driver_params ] >>= fun x -> return (From.ref_task "return value of VDI.snapshot"  x)
      (**  *)
      let clone ~rpc ~session_id ~vdi ~driver_params =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        let driver_params = To.string_to_string_map driver_params in
        
        rpc_wrapper rpc "Async.VDI.clone" [ session_id; vdi; driver_params ] >>= fun x -> return (From.ref_task "return value of VDI.clone"  x)
      (**  *)
      let resize ~rpc ~session_id ~vdi ~size =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        let size = To.int64 size in
        
        rpc_wrapper rpc "Async.VDI.resize" [ session_id; vdi; size ] >>= fun x -> return (From.ref_task "return value of VDI.resize"  x)
      (**  *)
      let resize_online ~rpc ~session_id ~vdi ~size =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        let size = To.int64 size in
        
        rpc_wrapper rpc "Async.VDI.resize_online" [ session_id; vdi; size ] >>= fun x -> return (From.ref_task "return value of VDI.resize_online"  x)
      (**  *)
      let introduce ~rpc ~session_id ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
        let session_id = To.ref_session session_id in
        let uuid = To.string uuid in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let sR = To.ref_SR sR in
        let _type = To.vdi_type _type in
        let sharable = To.bool sharable in
        let read_only = To.bool read_only in
        let other_config = To.string_to_string_map other_config in
        let location = To.string location in
        let xenstore_data = To.string_to_string_map xenstore_data in
        let sm_config = To.string_to_string_map sm_config in
        let managed = To.bool managed in
        let virtual_size = To.int64 virtual_size in
        let physical_utilisation = To.int64 physical_utilisation in
        let metadata_of_pool = To.ref_pool metadata_of_pool in
        let is_a_snapshot = To.bool is_a_snapshot in
        let snapshot_time = To.datetime snapshot_time in
        let snapshot_of = To.ref_VDI snapshot_of in
        
        rpc_wrapper rpc "Async.VDI.introduce" [ session_id; uuid; name_label; name_description; sR; _type; sharable; read_only; other_config; location; xenstore_data; sm_config; managed; virtual_size; physical_utilisation; metadata_of_pool; is_a_snapshot; snapshot_time; snapshot_of ] >>= fun x -> return (From.ref_task "return value of VDI.introduce"  x)
      (**  *)
      let pool_introduce ~rpc ~session_id ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
        let session_id = To.ref_session session_id in
        let uuid = To.string uuid in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let sR = To.ref_SR sR in
        let _type = To.vdi_type _type in
        let sharable = To.bool sharable in
        let read_only = To.bool read_only in
        let other_config = To.string_to_string_map other_config in
        let location = To.string location in
        let xenstore_data = To.string_to_string_map xenstore_data in
        let sm_config = To.string_to_string_map sm_config in
        let managed = To.bool managed in
        let virtual_size = To.int64 virtual_size in
        let physical_utilisation = To.int64 physical_utilisation in
        let metadata_of_pool = To.ref_pool metadata_of_pool in
        let is_a_snapshot = To.bool is_a_snapshot in
        let snapshot_time = To.datetime snapshot_time in
        let snapshot_of = To.ref_VDI snapshot_of in
        
        rpc_wrapper rpc "Async.VDI.pool_introduce" [ session_id; uuid; name_label; name_description; sR; _type; sharable; read_only; other_config; location; xenstore_data; sm_config; managed; virtual_size; physical_utilisation; metadata_of_pool; is_a_snapshot; snapshot_time; snapshot_of ] >>= fun x -> return (From.ref_task "return value of VDI.pool_introduce"  x)
      (**  *)
      let db_introduce ~rpc ~session_id ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
        let session_id = To.ref_session session_id in
        let uuid = To.string uuid in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let sR = To.ref_SR sR in
        let _type = To.vdi_type _type in
        let sharable = To.bool sharable in
        let read_only = To.bool read_only in
        let other_config = To.string_to_string_map other_config in
        let location = To.string location in
        let xenstore_data = To.string_to_string_map xenstore_data in
        let sm_config = To.string_to_string_map sm_config in
        let managed = To.bool managed in
        let virtual_size = To.int64 virtual_size in
        let physical_utilisation = To.int64 physical_utilisation in
        let metadata_of_pool = To.ref_pool metadata_of_pool in
        let is_a_snapshot = To.bool is_a_snapshot in
        let snapshot_time = To.datetime snapshot_time in
        let snapshot_of = To.ref_VDI snapshot_of in
        
        rpc_wrapper rpc "Async.VDI.db_introduce" [ session_id; uuid; name_label; name_description; sR; _type; sharable; read_only; other_config; location; xenstore_data; sm_config; managed; virtual_size; physical_utilisation; metadata_of_pool; is_a_snapshot; snapshot_time; snapshot_of ] >>= fun x -> return (From.ref_task "return value of VDI.db_introduce"  x)
      (**  *)
      let db_forget ~rpc ~session_id ~vdi =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        
        rpc_wrapper rpc "Async.VDI.db_forget" [ session_id; vdi ] >>= fun x -> return (From.ref_task "return value of VDI.db_forget"  x)
      (**  *)
      let update ~rpc ~session_id ~vdi =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        
        rpc_wrapper rpc "Async.VDI.update" [ session_id; vdi ] >>= fun x -> return (From.ref_task "return value of VDI.update"  x)
      (**  *)
      let copy ~rpc ~session_id ~vdi ~sr =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        let sr = To.ref_SR sr in
        
        rpc_wrapper rpc "Async.VDI.copy" [ session_id; vdi; sr ] >>= fun x -> return (From.ref_task "return value of VDI.copy"  x)
      (**  *)
      let force_unlock ~rpc ~session_id ~vdi =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        
        rpc_wrapper rpc "Async.VDI.force_unlock" [ session_id; vdi ] >>= fun x -> return (From.ref_task "return value of VDI.force_unlock"  x)
      (**  *)
      let forget ~rpc ~session_id ~vdi =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        
        rpc_wrapper rpc "Async.VDI.forget" [ session_id; vdi ] >>= fun x -> return (From.ref_task "return value of VDI.forget"  x)
      (**  *)
      let set_name_label ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VDI self in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.VDI.set_name_label" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VDI.set_name_label"  x)
      (**  *)
      let set_name_description ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VDI self in
        let value = To.string value in
        
        rpc_wrapper rpc "Async.VDI.set_name_description" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VDI.set_name_description"  x)
      (**  *)
      let generate_config ~rpc ~session_id ~host ~vdi =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let vdi = To.ref_VDI vdi in
        
        rpc_wrapper rpc "Async.VDI.generate_config" [ session_id; host; vdi ] >>= fun x -> return (From.ref_task "return value of VDI.generate_config"  x)
      (**  *)
      let set_on_boot ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VDI self in
        let value = To.on_boot value in
        
        rpc_wrapper rpc "Async.VDI.set_on_boot" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VDI.set_on_boot"  x)
      (**  *)
      let set_allow_caching ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_VDI self in
        let value = To.bool value in
        
        rpc_wrapper rpc "Async.VDI.set_allow_caching" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of VDI.set_allow_caching"  x)
      (**  *)
      let open_database ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VDI self in
        
        rpc_wrapper rpc "Async.VDI.open_database" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VDI.open_database"  x)
      (**  *)
      let checksum ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VDI self in
        
        rpc_wrapper rpc "Async.VDI.checksum" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VDI.checksum"  x)
      (**  *)
      let read_database_pool_uuid ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VDI self in
        
        rpc_wrapper rpc "Async.VDI.read_database_pool_uuid" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VDI.read_database_pool_uuid"  x)
      (**  *)
      let pool_migrate ~rpc ~session_id ~vdi ~sr ~options =
        let session_id = To.ref_session session_id in
        let vdi = To.ref_VDI vdi in
        let sr = To.ref_SR sr in
        let options = To.string_to_string_map options in
        
        rpc_wrapper rpc "Async.VDI.pool_migrate" [ session_id; vdi; sr; options ] >>= fun x -> return (From.ref_task "return value of VDI.pool_migrate"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~name_label:value.vDI_name_label
        ~name_description:value.vDI_name_description
        ~sR:value.vDI_SR
        ~virtual_size:value.vDI_virtual_size
        ~_type:value.vDI_type
        ~sharable:value.vDI_sharable
        ~read_only:value.vDI_read_only
        ~other_config:value.vDI_other_config
        ~xenstore_data:value.vDI_xenstore_data
        ~sm_config:value.vDI_sm_config
        ~tags:value.vDI_tags
    end
    module VBD = struct
      (**  *)
      let create ~rpc ~session_id ~vM ~vDI ~userdevice ~bootable ~mode ~_type ~unpluggable ~empty ~other_config ~qos_algorithm_type ~qos_algorithm_params =
        let session_id = To.ref_session session_id in
        let vM = To.ref_VM vM in
        let vDI = To.ref_VDI vDI in
        let userdevice = To.string userdevice in
        let bootable = To.bool bootable in
        let mode = To.vbd_mode mode in
        let _type = To.vbd_type _type in
        let unpluggable = To.bool unpluggable in
        let empty = To.bool empty in
        let other_config = To.string_to_string_map other_config in
        let qos_algorithm_type = To.string qos_algorithm_type in
        let qos_algorithm_params = To.string_to_string_map qos_algorithm_params in
        let __structure = To.structure [ "VM", vM; "VDI", vDI; "userdevice", userdevice; "bootable", bootable; "mode", mode; "type", _type; "unpluggable", unpluggable; "empty", empty; "other_config", other_config; "qos_algorithm_type", qos_algorithm_type; "qos_algorithm_params", qos_algorithm_params] in
        rpc_wrapper rpc "Async.VBD.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of VBD.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VBD self in
        
        rpc_wrapper rpc "Async.VBD.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VBD.destroy"  x)
      (**  *)
      let eject ~rpc ~session_id ~vbd =
        let session_id = To.ref_session session_id in
        let vbd = To.ref_VBD vbd in
        
        rpc_wrapper rpc "Async.VBD.eject" [ session_id; vbd ] >>= fun x -> return (From.ref_task "return value of VBD.eject"  x)
      (**  *)
      let insert ~rpc ~session_id ~vbd ~vdi =
        let session_id = To.ref_session session_id in
        let vbd = To.ref_VBD vbd in
        let vdi = To.ref_VDI vdi in
        
        rpc_wrapper rpc "Async.VBD.insert" [ session_id; vbd; vdi ] >>= fun x -> return (From.ref_task "return value of VBD.insert"  x)
      (**  *)
      let plug ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VBD self in
        
        rpc_wrapper rpc "Async.VBD.plug" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VBD.plug"  x)
      (**  *)
      let unplug ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VBD self in
        
        rpc_wrapper rpc "Async.VBD.unplug" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VBD.unplug"  x)
      (**  *)
      let unplug_force ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VBD self in
        
        rpc_wrapper rpc "Async.VBD.unplug_force" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VBD.unplug_force"  x)
      (**  *)
      let unplug_force_no_safety_check ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VBD self in
        
        rpc_wrapper rpc "Async.VBD.unplug_force_no_safety_check" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VBD.unplug_force_no_safety_check"  x)
      (**  *)
      let assert_attachable ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VBD self in
        
        rpc_wrapper rpc "Async.VBD.assert_attachable" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VBD.assert_attachable"  x)
      (**  *)
      let pause ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VBD self in
        
        rpc_wrapper rpc "Async.VBD.pause" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VBD.pause"  x)
      (**  *)
      let unpause ~rpc ~session_id ~self ~token =
        let session_id = To.ref_session session_id in
        let self = To.ref_VBD self in
        let token = To.string token in
        
        rpc_wrapper rpc "Async.VBD.unpause" [ session_id; self; token ] >>= fun x -> return (From.ref_task "return value of VBD.unpause"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~vM:value.vBD_VM
        ~vDI:value.vBD_VDI
        ~userdevice:value.vBD_userdevice
        ~bootable:value.vBD_bootable
        ~mode:value.vBD_mode
        ~_type:value.vBD_type
        ~unpluggable:value.vBD_unpluggable
        ~empty:value.vBD_empty
        ~other_config:value.vBD_other_config
        ~qos_algorithm_type:value.vBD_qos_algorithm_type
        ~qos_algorithm_params:value.vBD_qos_algorithm_params
    end
    module VBD_metrics = struct
    end
    module PBD = struct
      (**  *)
      let create ~rpc ~session_id ~host ~sR ~device_config ~other_config =
        let session_id = To.ref_session session_id in
        let host = To.ref_host host in
        let sR = To.ref_SR sR in
        let device_config = To.string_to_string_map device_config in
        let other_config = To.string_to_string_map other_config in
        let __structure = To.structure [ "host", host; "SR", sR; "device_config", device_config; "other_config", other_config] in
        rpc_wrapper rpc "Async.PBD.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of PBD.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_PBD self in
        
        rpc_wrapper rpc "Async.PBD.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of PBD.destroy"  x)
      (**  *)
      let plug ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_PBD self in
        
        rpc_wrapper rpc "Async.PBD.plug" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of PBD.plug"  x)
      (**  *)
      let unplug ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_PBD self in
        
        rpc_wrapper rpc "Async.PBD.unplug" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of PBD.unplug"  x)
      (**  *)
      let set_device_config ~rpc ~session_id ~self ~value =
        let session_id = To.ref_session session_id in
        let self = To.ref_PBD self in
        let value = To.string_to_string_map value in
        
        rpc_wrapper rpc "Async.PBD.set_device_config" [ session_id; self; value ] >>= fun x -> return (From.ref_task "return value of PBD.set_device_config"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~host:value.pBD_host
        ~sR:value.pBD_SR
        ~device_config:value.pBD_device_config
        ~other_config:value.pBD_other_config
    end
    module Crashdump = struct
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_crashdump self in
        
        rpc_wrapper rpc "Async.crashdump.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of crashdump.destroy"  x)
    end
    module VTPM = struct
      (**  *)
      let create ~rpc ~session_id ~vM ~backend =
        let session_id = To.ref_session session_id in
        let vM = To.ref_VM vM in
        let backend = To.ref_VM backend in
        let __structure = To.structure [ "VM", vM; "backend", backend] in
        rpc_wrapper rpc "Async.VTPM.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of VTPM.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VTPM self in
        
        rpc_wrapper rpc "Async.VTPM.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VTPM.destroy"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~vM:value.vTPM_VM
        ~backend:value.vTPM_backend
    end
    module Console = struct
      (**  *)
      let create ~rpc ~session_id ~other_config =
        let session_id = To.ref_session session_id in
        let other_config = To.string_to_string_map other_config in
        let __structure = To.structure [ "other_config", other_config] in
        rpc_wrapper rpc "Async.console.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of console.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_console self in
        
        rpc_wrapper rpc "Async.console.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of console.destroy"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~other_config:value.console_other_config
    end
    module User = struct
      (**  *)
      let create ~rpc ~session_id ~short_name ~fullname ~other_config =
        let session_id = To.ref_session session_id in
        let short_name = To.string short_name in
        let fullname = To.string fullname in
        let other_config = To.string_to_string_map other_config in
        let __structure = To.structure [ "short_name", short_name; "fullname", fullname; "other_config", other_config] in
        rpc_wrapper rpc "Async.user.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of user.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_user self in
        
        rpc_wrapper rpc "Async.user.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of user.destroy"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~short_name:value.user_short_name
        ~fullname:value.user_fullname
        ~other_config:value.user_other_config
    end
    module Data_source = struct
    end
    module Blob = struct
    end
    module Message = struct
    end
    module Secret = struct
      (**  *)
      let create ~rpc ~session_id ~value ~other_config =
        let session_id = To.ref_session session_id in
        let value = To.string value in
        let other_config = To.string_to_string_map other_config in
        let __structure = To.structure [ "value", value; "other_config", other_config] in
        rpc_wrapper rpc "Async.secret.create" [ session_id; __structure ] >>= fun x -> return (From.ref_task "return value of secret.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_secret self in
        
        rpc_wrapper rpc "Async.secret.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of secret.destroy"  x)
      (**  *)
      let create_from_record ~rpc ~session_id ~value =
        create
        ~rpc
        ~session_id
        ~value:value.secret_value
        ~other_config:value.secret_other_config
    end
    module Tunnel = struct
      (**  *)
      let create ~rpc ~session_id ~transport_PIF ~network =
        let session_id = To.ref_session session_id in
        let transport_PIF = To.ref_PIF transport_PIF in
        let network = To.ref_network network in
        
        rpc_wrapper rpc "Async.tunnel.create" [ session_id; transport_PIF; network ] >>= fun x -> return (From.ref_task "return value of tunnel.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_tunnel self in
        
        rpc_wrapper rpc "Async.tunnel.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of tunnel.destroy"  x)
    end
    module PCI = struct
    end
    module PGPU = struct
    end
    module GPU_group = struct
      (**  *)
      let create ~rpc ~session_id ~name_label ~name_description ~other_config =
        let session_id = To.ref_session session_id in
        let name_label = To.string name_label in
        let name_description = To.string name_description in
        let other_config = To.string_to_string_map other_config in
        
        rpc_wrapper rpc "Async.GPU_group.create" [ session_id; name_label; name_description; other_config ] >>= fun x -> return (From.ref_task "return value of GPU_group.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_GPU_group self in
        
        rpc_wrapper rpc "Async.GPU_group.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of GPU_group.destroy"  x)
    end
    module VGPU = struct
      (**  *)
      let create ~rpc ~session_id ~vM ~gPU_group ~device ~other_config =
        let session_id = To.ref_session session_id in
        let vM = To.ref_VM vM in
        let gPU_group = To.ref_GPU_group gPU_group in
        let device = To.string device in
        let other_config = To.string_to_string_map other_config in
        
        rpc_wrapper rpc "Async.VGPU.create" [ session_id; vM; gPU_group; device; other_config ] >>= fun x -> return (From.ref_task "return value of VGPU.create"  x)
      (**  *)
      let destroy ~rpc ~session_id ~self =
        let session_id = To.ref_session session_id in
        let self = To.ref_VGPU self in
        
        rpc_wrapper rpc "Async.VGPU.destroy" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of VGPU.destroy"  x)
    end
  end
  module Session = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_record" [ session_id; self ] >>= fun x -> return (From.session_t "return value of session.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "session.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_session "return value of session.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of session.get_uuid"  x)
    (**  *)
    let get_this_host ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_this_host" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of session.get_this_host"  x)
    (**  *)
    let get_this_user ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_this_user" [ session_id; self ] >>= fun x -> return (From.ref_user "return value of session.get_this_user"  x)
    (**  *)
    let get_last_active ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_last_active" [ session_id; self ] >>= fun x -> return (From.datetime "return value of session.get_last_active"  x)
    (**  *)
    let get_pool ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_pool" [ session_id; self ] >>= fun x -> return (From.bool "return value of session.get_pool"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of session.get_other_config"  x)
    (**  *)
    let get_is_local_superuser ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_is_local_superuser" [ session_id; self ] >>= fun x -> return (From.bool "return value of session.get_is_local_superuser"  x)
    (**  *)
    let get_subject ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_subject" [ session_id; self ] >>= fun x -> return (From.ref_subject "return value of session.get_subject"  x)
    (**  *)
    let get_validation_time ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_validation_time" [ session_id; self ] >>= fun x -> return (From.datetime "return value of session.get_validation_time"  x)
    (**  *)
    let get_auth_user_sid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_auth_user_sid" [ session_id; self ] >>= fun x -> return (From.string "return value of session.get_auth_user_sid"  x)
    (**  *)
    let get_auth_user_name ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_auth_user_name" [ session_id; self ] >>= fun x -> return (From.string "return value of session.get_auth_user_name"  x)
    (**  *)
    let get_rbac_permissions ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_rbac_permissions" [ session_id; self ] >>= fun x -> return (From.string_set "return value of session.get_rbac_permissions"  x)
    (**  *)
    let get_tasks ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_tasks" [ session_id; self ] >>= fun x -> return (From.ref_task_set "return value of session.get_tasks"  x)
    (**  *)
    let get_parent ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      
      rpc_wrapper rpc "session.get_parent" [ session_id; self ] >>= fun x -> return (From.ref_session "return value of session.get_parent"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "session.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "session.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_session self in
      let key = To.string key in
      
      rpc_wrapper rpc "session.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let login_with_password ~rpc ~uname ~pwd ~version =
      let uname = To.string uname in
      let pwd = To.string pwd in
      let version = To.string version in
      
      rpc_wrapper rpc "session.login_with_password" [ uname; pwd; version ] >>= fun x -> return (From.ref_session "return value of session.login_with_password"  x)
    (**  *)
    let logout ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "session.logout" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let change_password ~rpc ~session_id ~old_pwd ~new_pwd =
      let session_id = To.ref_session session_id in
      let old_pwd = To.string old_pwd in
      let new_pwd = To.string new_pwd in
      
      rpc_wrapper rpc "session.change_password" [ session_id; old_pwd; new_pwd ] >>= fun x -> return (ignore x)
    (**  *)
    let slave_login ~rpc ~host ~psecret =
      let host = To.ref_host host in
      let psecret = To.string psecret in
      
      rpc_wrapper rpc "session.slave_login" [ host; psecret ] >>= fun x -> return (From.ref_session "return value of session.slave_login"  x)
    (**  *)
    let slave_local_login ~rpc ~psecret =
      let psecret = To.string psecret in
      
      rpc_wrapper rpc "session.slave_local_login" [ psecret ] >>= fun x -> return (From.ref_session "return value of session.slave_local_login"  x)
    (**  *)
    let slave_local_login_with_password ~rpc ~uname ~pwd =
      let uname = To.string uname in
      let pwd = To.string pwd in
      
      rpc_wrapper rpc "session.slave_local_login_with_password" [ uname; pwd ] >>= fun x -> return (From.ref_session "return value of session.slave_local_login_with_password"  x)
    (**  *)
    let local_logout ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "session.local_logout" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all_subject_identifiers ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "session.get_all_subject_identifiers" [ session_id ] >>= fun x -> return (From.string_set "return value of session.get_all_subject_identifiers"  x)
    (**  *)
    let logout_subject_identifier ~rpc ~session_id ~subject_identifier =
      let session_id = To.ref_session session_id in
      let subject_identifier = To.string subject_identifier in
      
      rpc_wrapper rpc "session.logout_subject_identifier" [ session_id; subject_identifier ] >>= fun x -> return (ignore x)
  end
  module Auth = struct
    (**  *)
    let get_subject_identifier ~rpc ~session_id ~subject_name =
      let session_id = To.ref_session session_id in
      let subject_name = To.string subject_name in
      
      rpc_wrapper rpc "auth.get_subject_identifier" [ session_id; subject_name ] >>= fun x -> return (From.string "return value of auth.get_subject_identifier"  x)
    (**  *)
    let get_subject_information_from_identifier ~rpc ~session_id ~subject_identifier =
      let session_id = To.ref_session session_id in
      let subject_identifier = To.string subject_identifier in
      
      rpc_wrapper rpc "auth.get_subject_information_from_identifier" [ session_id; subject_identifier ] >>= fun x -> return (From.string_to_string_map "return value of auth.get_subject_information_from_identifier"  x)
    (**  *)
    let get_group_membership ~rpc ~session_id ~subject_identifier =
      let session_id = To.ref_session session_id in
      let subject_identifier = To.string subject_identifier in
      
      rpc_wrapper rpc "auth.get_group_membership" [ session_id; subject_identifier ] >>= fun x -> return (From.string_set "return value of auth.get_group_membership"  x)
  end
  module Subject = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      
      rpc_wrapper rpc "subject.get_record" [ session_id; self ] >>= fun x -> return (From.subject_t "return value of subject.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "subject.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_subject "return value of subject.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~subject_identifier ~other_config =
      let session_id = To.ref_session session_id in
      let subject_identifier = To.string subject_identifier in
      let other_config = To.string_to_string_map other_config in
      let __structure = To.structure [ "subject_identifier", subject_identifier; "other_config", other_config] in
      rpc_wrapper rpc "subject.create" [ session_id; __structure ] >>= fun x -> return (From.ref_subject "return value of subject.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      
      rpc_wrapper rpc "subject.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      
      rpc_wrapper rpc "subject.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of subject.get_uuid"  x)
    (**  *)
    let get_subject_identifier ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      
      rpc_wrapper rpc "subject.get_subject_identifier" [ session_id; self ] >>= fun x -> return (From.string "return value of subject.get_subject_identifier"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      
      rpc_wrapper rpc "subject.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of subject.get_other_config"  x)
    (**  *)
    let get_roles ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      
      rpc_wrapper rpc "subject.get_roles" [ session_id; self ] >>= fun x -> return (From.ref_role_set "return value of subject.get_roles"  x)
    (**  *)
    let add_to_roles ~rpc ~session_id ~self ~role =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      let role = To.ref_role role in
      
      rpc_wrapper rpc "subject.add_to_roles" [ session_id; self; role ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_roles ~rpc ~session_id ~self ~role =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      let role = To.ref_role role in
      
      rpc_wrapper rpc "subject.remove_from_roles" [ session_id; self; role ] >>= fun x -> return (ignore x)
    (**  *)
    let get_permissions_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_subject self in
      
      rpc_wrapper rpc "subject.get_permissions_name_label" [ session_id; self ] >>= fun x -> return (From.string_set "return value of subject.get_permissions_name_label"  x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "subject.get_all" [ session_id ] >>= fun x -> return (From.ref_subject_set "return value of subject.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "subject.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_subject_to_subject_t_map "return value of subject.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "subject.get_all_records" [ session_id ] >>= fun x -> return (From.ref_subject_to_subject_t_map "return value of subject.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~subject_identifier:value.subject_subject_identifier
      ~other_config:value.subject_other_config
  end
  module Role = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_role self in
      
      rpc_wrapper rpc "role.get_record" [ session_id; self ] >>= fun x -> return (From.role_t "return value of role.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "role.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_role "return value of role.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "role.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_role_set "return value of role.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_role self in
      
      rpc_wrapper rpc "role.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of role.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_role self in
      
      rpc_wrapper rpc "role.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of role.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_role self in
      
      rpc_wrapper rpc "role.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of role.get_name_description"  x)
    (**  *)
    let get_subroles ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_role self in
      
      rpc_wrapper rpc "role.get_subroles" [ session_id; self ] >>= fun x -> return (From.ref_role_set "return value of role.get_subroles"  x)
    (**  *)
    let get_permissions ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_role self in
      
      rpc_wrapper rpc "role.get_permissions" [ session_id; self ] >>= fun x -> return (From.ref_role_set "return value of role.get_permissions"  x)
    (**  *)
    let get_permissions_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_role self in
      
      rpc_wrapper rpc "role.get_permissions_name_label" [ session_id; self ] >>= fun x -> return (From.string_set "return value of role.get_permissions_name_label"  x)
    (**  *)
    let get_by_permission ~rpc ~session_id ~permission =
      let session_id = To.ref_session session_id in
      let permission = To.ref_role permission in
      
      rpc_wrapper rpc "role.get_by_permission" [ session_id; permission ] >>= fun x -> return (From.ref_role_set "return value of role.get_by_permission"  x)
    (**  *)
    let get_by_permission_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "role.get_by_permission_name_label" [ session_id; label ] >>= fun x -> return (From.ref_role_set "return value of role.get_by_permission_name_label"  x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "role.get_all" [ session_id ] >>= fun x -> return (From.ref_role_set "return value of role.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "role.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_role_to_role_t_map "return value of role.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "role.get_all_records" [ session_id ] >>= fun x -> return (From.ref_role_to_role_t_map "return value of role.get_all_records"  x)
  end
  module Task = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_record" [ session_id; self ] >>= fun x -> return (From.task_t "return value of task.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "task.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_task "return value of task.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "task.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_task_set "return value of task.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of task.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of task.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of task.get_name_description"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.task_allowed_operations_set "return value of task.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_task_allowed_operations_map "return value of task.get_current_operations"  x)
    (**  *)
    let get_created ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_created" [ session_id; self ] >>= fun x -> return (From.datetime "return value of task.get_created"  x)
    (**  *)
    let get_finished ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_finished" [ session_id; self ] >>= fun x -> return (From.datetime "return value of task.get_finished"  x)
    (**  *)
    let get_status ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_status" [ session_id; self ] >>= fun x -> return (From.task_status_type "return value of task.get_status"  x)
    (**  *)
    let get_resident_on ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_resident_on" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of task.get_resident_on"  x)
    (**  *)
    let get_progress ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_progress" [ session_id; self ] >>= fun x -> return (From.float "return value of task.get_progress"  x)
    (**  *)
    let get_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_type" [ session_id; self ] >>= fun x -> return (From.string "return value of task.get_type"  x)
    (**  *)
    let get_result ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_result" [ session_id; self ] >>= fun x -> return (From.string "return value of task.get_result"  x)
    (**  *)
    let get_error_info ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_error_info" [ session_id; self ] >>= fun x -> return (From.string_set "return value of task.get_error_info"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of task.get_other_config"  x)
    (**  *)
    let get_subtask_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_subtask_of" [ session_id; self ] >>= fun x -> return (From.ref_task "return value of task.get_subtask_of"  x)
    (**  *)
    let get_subtasks ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.get_subtasks" [ session_id; self ] >>= fun x -> return (From.ref_task_set "return value of task.get_subtasks"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "task.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "task.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      let key = To.string key in
      
      rpc_wrapper rpc "task.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~label ~description =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      let description = To.string description in
      
      rpc_wrapper rpc "task.create" [ session_id; label; description ] >>= fun x -> return (From.ref_task "return value of task.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_task self in
      
      rpc_wrapper rpc "task.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let cancel ~rpc ~session_id ~task =
      let session_id = To.ref_session session_id in
      let task = To.ref_task task in
      
      rpc_wrapper rpc "task.cancel" [ session_id; task ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "task.get_all" [ session_id ] >>= fun x -> return (From.ref_task_set "return value of task.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "task.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_task_to_task_t_map "return value of task.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "task.get_all_records" [ session_id ] >>= fun x -> return (From.ref_task_to_task_t_map "return value of task.get_all_records"  x)
  end
  module Event = struct
    (**  *)
    let register ~rpc ~session_id ~classes =
      let session_id = To.ref_session session_id in
      let classes = To.string_set classes in
      
      rpc_wrapper rpc "event.register" [ session_id; classes ] >>= fun x -> return (ignore x)
    (**  *)
    let unregister ~rpc ~session_id ~classes =
      let session_id = To.ref_session session_id in
      let classes = To.string_set classes in
      
      rpc_wrapper rpc "event.unregister" [ session_id; classes ] >>= fun x -> return (ignore x)
    (**  *)
    let next ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "event.next" [ session_id ] >>= fun x -> return ( x)
    (**  *)
    let from ~rpc ~session_id ~classes ~token ~timeout =
      let session_id = To.ref_session session_id in
      let classes = To.string_set classes in
      let token = To.string token in
      let timeout = To.float timeout in
      
      rpc_wrapper rpc "event.from" [ session_id; classes; token; timeout ] >>= fun x -> return ( x)
    (**  *)
    let get_current_id ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "event.get_current_id" [ session_id ] >>= fun x -> return (From.int64 "return value of event.get_current_id"  x)
    (**  *)
    let inject ~rpc ~session_id ~_class ~ref =
      let session_id = To.ref_session session_id in
      let _class = To.string _class in
      let ref = To.string ref in
      
      rpc_wrapper rpc "event.inject" [ session_id; _class; ref ] >>= fun x -> return (From.string "return value of event.inject"  x)
  end
  module Pool = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_record" [ session_id; self ] >>= fun x -> return (From.pool_t "return value of pool.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "pool.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_pool "return value of pool.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of pool.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of pool.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of pool.get_name_description"  x)
    (**  *)
    let get_master ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_master" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of pool.get_master"  x)
    (**  *)
    let get_default_SR ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_default_SR" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of pool.get_default_SR"  x)
    (**  *)
    let get_suspend_image_SR ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_suspend_image_SR" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of pool.get_suspend_image_SR"  x)
    (**  *)
    let get_crash_dump_SR ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_crash_dump_SR" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of pool.get_crash_dump_SR"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of pool.get_other_config"  x)
    (**  *)
    let get_ha_enabled ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_ha_enabled" [ session_id; self ] >>= fun x -> return (From.bool "return value of pool.get_ha_enabled"  x)
    (**  *)
    let get_ha_configuration ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_ha_configuration" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of pool.get_ha_configuration"  x)
    (**  *)
    let get_ha_statefiles ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_ha_statefiles" [ session_id; self ] >>= fun x -> return (From.string_set "return value of pool.get_ha_statefiles"  x)
    (**  *)
    let get_ha_host_failures_to_tolerate ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_ha_host_failures_to_tolerate" [ session_id; self ] >>= fun x -> return (From.int64 "return value of pool.get_ha_host_failures_to_tolerate"  x)
    (**  *)
    let get_ha_plan_exists_for ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_ha_plan_exists_for" [ session_id; self ] >>= fun x -> return (From.int64 "return value of pool.get_ha_plan_exists_for"  x)
    (**  *)
    let get_ha_allow_overcommit ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_ha_allow_overcommit" [ session_id; self ] >>= fun x -> return (From.bool "return value of pool.get_ha_allow_overcommit"  x)
    (**  *)
    let get_ha_overcommitted ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_ha_overcommitted" [ session_id; self ] >>= fun x -> return (From.bool "return value of pool.get_ha_overcommitted"  x)
    (**  *)
    let get_blobs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_blobs" [ session_id; self ] >>= fun x -> return (From.string_to_ref_blob_map "return value of pool.get_blobs"  x)
    (**  *)
    let get_tags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_tags" [ session_id; self ] >>= fun x -> return (From.string_set "return value of pool.get_tags"  x)
    (**  *)
    let get_gui_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_gui_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of pool.get_gui_config"  x)
    (**  *)
    let get_wlb_url ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_wlb_url" [ session_id; self ] >>= fun x -> return (From.string "return value of pool.get_wlb_url"  x)
    (**  *)
    let get_wlb_username ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_wlb_username" [ session_id; self ] >>= fun x -> return (From.string "return value of pool.get_wlb_username"  x)
    (**  *)
    let get_wlb_enabled ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_wlb_enabled" [ session_id; self ] >>= fun x -> return (From.bool "return value of pool.get_wlb_enabled"  x)
    (**  *)
    let get_wlb_verify_cert ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_wlb_verify_cert" [ session_id; self ] >>= fun x -> return (From.bool "return value of pool.get_wlb_verify_cert"  x)
    (**  *)
    let get_redo_log_enabled ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_redo_log_enabled" [ session_id; self ] >>= fun x -> return (From.bool "return value of pool.get_redo_log_enabled"  x)
    (**  *)
    let get_redo_log_vdi ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_redo_log_vdi" [ session_id; self ] >>= fun x -> return (From.ref_VDI "return value of pool.get_redo_log_vdi"  x)
    (**  *)
    let get_vswitch_controller ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_vswitch_controller" [ session_id; self ] >>= fun x -> return (From.string "return value of pool.get_vswitch_controller"  x)
    (**  *)
    let get_restrictions ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_restrictions" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of pool.get_restrictions"  x)
    (**  *)
    let get_metadata_VDIs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.get_metadata_VDIs" [ session_id; self ] >>= fun x -> return (From.ref_VDI_set "return value of pool.get_metadata_VDIs"  x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.string value in
      
      rpc_wrapper rpc "pool.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.string value in
      
      rpc_wrapper rpc "pool.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_default_SR ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.ref_SR value in
      
      rpc_wrapper rpc "pool.set_default_SR" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_suspend_image_SR ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.ref_SR value in
      
      rpc_wrapper rpc "pool.set_suspend_image_SR" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_crash_dump_SR ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.ref_SR value in
      
      rpc_wrapper rpc "pool.set_crash_dump_SR" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "pool.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "pool.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let key = To.string key in
      
      rpc_wrapper rpc "pool.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_ha_allow_overcommit ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.bool value in
      
      rpc_wrapper rpc "pool.set_ha_allow_overcommit" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.string_set value in
      
      rpc_wrapper rpc "pool.set_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.string value in
      
      rpc_wrapper rpc "pool.add_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.string value in
      
      rpc_wrapper rpc "pool.remove_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_gui_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "pool.set_gui_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_gui_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "pool.add_to_gui_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_gui_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let key = To.string key in
      
      rpc_wrapper rpc "pool.remove_from_gui_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_wlb_enabled ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.bool value in
      
      rpc_wrapper rpc "pool.set_wlb_enabled" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_wlb_verify_cert ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.bool value in
      
      rpc_wrapper rpc "pool.set_wlb_verify_cert" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let join ~rpc ~session_id ~master_address ~master_username ~master_password =
      let session_id = To.ref_session session_id in
      let master_address = To.string master_address in
      let master_username = To.string master_username in
      let master_password = To.string master_password in
      
      rpc_wrapper rpc "pool.join" [ session_id; master_address; master_username; master_password ] >>= fun x -> return (ignore x)
    (**  *)
    let join_force ~rpc ~session_id ~master_address ~master_username ~master_password =
      let session_id = To.ref_session session_id in
      let master_address = To.string master_address in
      let master_username = To.string master_username in
      let master_password = To.string master_password in
      
      rpc_wrapper rpc "pool.join_force" [ session_id; master_address; master_username; master_password ] >>= fun x -> return (ignore x)
    (**  *)
    let eject ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "pool.eject" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let initial_auth ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.initial_auth" [ session_id ] >>= fun x -> return (From.string "return value of pool.initial_auth"  x)
    (**  *)
    let emergency_transition_to_master ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.emergency_transition_to_master" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let emergency_reset_master ~rpc ~session_id ~master_address =
      let session_id = To.ref_session session_id in
      let master_address = To.string master_address in
      
      rpc_wrapper rpc "pool.emergency_reset_master" [ session_id; master_address ] >>= fun x -> return (ignore x)
    (**  *)
    let recover_slaves ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.recover_slaves" [ session_id ] >>= fun x -> return (From.ref_host_set "return value of pool.recover_slaves"  x)
    (**  *)
    let hello ~rpc ~session_id ~host_uuid ~host_address =
      let session_id = To.ref_session session_id in
      let host_uuid = To.string host_uuid in
      let host_address = To.string host_address in
      
      rpc_wrapper rpc "pool.hello" [ session_id; host_uuid; host_address ] >>= fun x -> return (From.hello_return "return value of pool.hello"  x)
    (**  *)
    let is_slave ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "pool.is_slave" [ session_id; host ] >>= fun x -> return (From.bool "return value of pool.is_slave"  x)
    (**  *)
    let create_VLAN ~rpc ~session_id ~device ~network ~vLAN =
      let session_id = To.ref_session session_id in
      let device = To.string device in
      let network = To.ref_network network in
      let vLAN = To.int64 vLAN in
      
      rpc_wrapper rpc "pool.create_VLAN" [ session_id; device; network; vLAN ] >>= fun x -> return (From.ref_PIF_set "return value of pool.create_VLAN"  x)
    (**  *)
    let create_VLAN_from_PIF ~rpc ~session_id ~pif ~network ~vLAN =
      let session_id = To.ref_session session_id in
      let pif = To.ref_PIF pif in
      let network = To.ref_network network in
      let vLAN = To.int64 vLAN in
      
      rpc_wrapper rpc "pool.create_VLAN_from_PIF" [ session_id; pif; network; vLAN ] >>= fun x -> return (From.ref_PIF_set "return value of pool.create_VLAN_from_PIF"  x)
    (**  *)
    let slave_network_report ~rpc ~session_id ~phydevs ~dev_to_mac ~dev_to_mtu ~slave_host =
      let session_id = To.ref_session session_id in
      let phydevs = To.string_to_string_map phydevs in
      let dev_to_mac = To.string_to_string_map dev_to_mac in
      let dev_to_mtu = To.string_to_int64_map dev_to_mtu in
      let slave_host = To.ref_host slave_host in
      
      rpc_wrapper rpc "pool.slave_network_report" [ session_id; phydevs; dev_to_mac; dev_to_mtu; slave_host ] >>= fun x -> return (From.ref_PIF_set "return value of pool.slave_network_report"  x)
    (**  *)
    let enable_ha ~rpc ~session_id ~heartbeat_srs ~configuration =
      let session_id = To.ref_session session_id in
      let heartbeat_srs = To.ref_SR_set heartbeat_srs in
      let configuration = To.string_to_string_map configuration in
      
      rpc_wrapper rpc "pool.enable_ha" [ session_id; heartbeat_srs; configuration ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_ha ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.disable_ha" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let sync_database ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.sync_database" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let designate_new_master ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "pool.designate_new_master" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_prevent_restarts_for ~rpc ~session_id ~seconds =
      let session_id = To.ref_session session_id in
      let seconds = To.int64 seconds in
      
      rpc_wrapper rpc "pool.ha_prevent_restarts_for" [ session_id; seconds ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_failover_plan_exists ~rpc ~session_id ~n =
      let session_id = To.ref_session session_id in
      let n = To.int64 n in
      
      rpc_wrapper rpc "pool.ha_failover_plan_exists" [ session_id; n ] >>= fun x -> return (From.bool "return value of pool.ha_failover_plan_exists"  x)
    (**  *)
    let ha_compute_max_host_failures_to_tolerate ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.ha_compute_max_host_failures_to_tolerate" [ session_id ] >>= fun x -> return (From.int64 "return value of pool.ha_compute_max_host_failures_to_tolerate"  x)
    (**  *)
    let ha_compute_hypothetical_max_host_failures_to_tolerate ~rpc ~session_id ~configuration =
      let session_id = To.ref_session session_id in
      let configuration = To.ref_VM_to_string_map configuration in
      
      rpc_wrapper rpc "pool.ha_compute_hypothetical_max_host_failures_to_tolerate" [ session_id; configuration ] >>= fun x -> return (From.int64 "return value of pool.ha_compute_hypothetical_max_host_failures_to_tolerate"  x)
    (**  *)
    let ha_compute_vm_failover_plan ~rpc ~session_id ~failed_hosts ~failed_vms =
      let session_id = To.ref_session session_id in
      let failed_hosts = To.ref_host_set failed_hosts in
      let failed_vms = To.ref_VM_set failed_vms in
      
      rpc_wrapper rpc "pool.ha_compute_vm_failover_plan" [ session_id; failed_hosts; failed_vms ] >>= fun x -> return (From.ref_VM_to_string_to_string_map_map "return value of pool.ha_compute_vm_failover_plan"  x)
    (**  *)
    let set_ha_host_failures_to_tolerate ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "pool.set_ha_host_failures_to_tolerate" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let create_new_blob ~rpc ~session_id ~pool ~name ~mime_type ~public =
      let session_id = To.ref_session session_id in
      let pool = To.ref_pool pool in
      let name = To.string name in
      let mime_type = To.string mime_type in
      let public = To.bool public in
      
      rpc_wrapper rpc "pool.create_new_blob" [ session_id; pool; name; mime_type; public ] >>= fun x -> return (From.ref_blob "return value of pool.create_new_blob"  x)
    (**  *)
    let ha_schedule_plan_recomputation ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.ha_schedule_plan_recomputation" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let enable_binary_storage ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.enable_binary_storage" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_binary_storage ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.disable_binary_storage" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let enable_external_auth ~rpc ~session_id ~pool ~config ~service_name ~auth_type =
      let session_id = To.ref_session session_id in
      let pool = To.ref_pool pool in
      let config = To.string_to_string_map config in
      let service_name = To.string service_name in
      let auth_type = To.string auth_type in
      
      rpc_wrapper rpc "pool.enable_external_auth" [ session_id; pool; config; service_name; auth_type ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_external_auth ~rpc ~session_id ~pool ~config =
      let session_id = To.ref_session session_id in
      let pool = To.ref_pool pool in
      let config = To.string_to_string_map config in
      
      rpc_wrapper rpc "pool.disable_external_auth" [ session_id; pool; config ] >>= fun x -> return (ignore x)
    (**  *)
    let detect_nonhomogeneous_external_auth ~rpc ~session_id ~pool =
      let session_id = To.ref_session session_id in
      let pool = To.ref_pool pool in
      
      rpc_wrapper rpc "pool.detect_nonhomogeneous_external_auth" [ session_id; pool ] >>= fun x -> return (ignore x)
    (**  *)
    let initialize_wlb ~rpc ~session_id ~wlb_url ~wlb_username ~wlb_password ~xenserver_username ~xenserver_password =
      let session_id = To.ref_session session_id in
      let wlb_url = To.string wlb_url in
      let wlb_username = To.string wlb_username in
      let wlb_password = To.string wlb_password in
      let xenserver_username = To.string xenserver_username in
      let xenserver_password = To.string xenserver_password in
      
      rpc_wrapper rpc "pool.initialize_wlb" [ session_id; wlb_url; wlb_username; wlb_password; xenserver_username; xenserver_password ] >>= fun x -> return (ignore x)
    (**  *)
    let deconfigure_wlb ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.deconfigure_wlb" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let send_wlb_configuration ~rpc ~session_id ~config =
      let session_id = To.ref_session session_id in
      let config = To.string_to_string_map config in
      
      rpc_wrapper rpc "pool.send_wlb_configuration" [ session_id; config ] >>= fun x -> return (ignore x)
    (**  *)
    let retrieve_wlb_configuration ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.retrieve_wlb_configuration" [ session_id ] >>= fun x -> return (From.string_to_string_map "return value of pool.retrieve_wlb_configuration"  x)
    (**  *)
    let retrieve_wlb_recommendations ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.retrieve_wlb_recommendations" [ session_id ] >>= fun x -> return (From.ref_VM_to_string_set_map "return value of pool.retrieve_wlb_recommendations"  x)
    (**  *)
    let send_test_post ~rpc ~session_id ~host ~port ~body =
      let session_id = To.ref_session session_id in
      let host = To.string host in
      let port = To.int64 port in
      let body = To.string body in
      
      rpc_wrapper rpc "pool.send_test_post" [ session_id; host; port; body ] >>= fun x -> return (From.string "return value of pool.send_test_post"  x)
    (**  *)
    let certificate_install ~rpc ~session_id ~name ~cert =
      let session_id = To.ref_session session_id in
      let name = To.string name in
      let cert = To.string cert in
      
      rpc_wrapper rpc "pool.certificate_install" [ session_id; name; cert ] >>= fun x -> return (ignore x)
    (**  *)
    let certificate_uninstall ~rpc ~session_id ~name =
      let session_id = To.ref_session session_id in
      let name = To.string name in
      
      rpc_wrapper rpc "pool.certificate_uninstall" [ session_id; name ] >>= fun x -> return (ignore x)
    (**  *)
    let certificate_list ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.certificate_list" [ session_id ] >>= fun x -> return (From.string_set "return value of pool.certificate_list"  x)
    (**  *)
    let crl_install ~rpc ~session_id ~name ~cert =
      let session_id = To.ref_session session_id in
      let name = To.string name in
      let cert = To.string cert in
      
      rpc_wrapper rpc "pool.crl_install" [ session_id; name; cert ] >>= fun x -> return (ignore x)
    (**  *)
    let crl_uninstall ~rpc ~session_id ~name =
      let session_id = To.ref_session session_id in
      let name = To.string name in
      
      rpc_wrapper rpc "pool.crl_uninstall" [ session_id; name ] >>= fun x -> return (ignore x)
    (**  *)
    let crl_list ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.crl_list" [ session_id ] >>= fun x -> return (From.string_set "return value of pool.crl_list"  x)
    (**  *)
    let certificate_sync ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.certificate_sync" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let enable_redo_log ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "pool.enable_redo_log" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_redo_log ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.disable_redo_log" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let audit_log_append ~rpc ~session_id ~line =
      let session_id = To.ref_session session_id in
      let line = To.string line in
      
      rpc_wrapper rpc "pool.audit_log_append" [ session_id; line ] >>= fun x -> return (ignore x)
    (**  *)
    let set_vswitch_controller ~rpc ~session_id ~address =
      let session_id = To.ref_session session_id in
      let address = To.string address in
      
      rpc_wrapper rpc "pool.set_vswitch_controller" [ session_id; address ] >>= fun x -> return (ignore x)
    (**  *)
    let test_archive_target ~rpc ~session_id ~self ~config =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      let config = To.string_to_string_map config in
      
      rpc_wrapper rpc "pool.test_archive_target" [ session_id; self; config ] >>= fun x -> return (From.string "return value of pool.test_archive_target"  x)
    (**  *)
    let enable_local_storage_caching ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.enable_local_storage_caching" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_local_storage_caching ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool self in
      
      rpc_wrapper rpc "pool.disable_local_storage_caching" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.get_all" [ session_id ] >>= fun x -> return (From.ref_pool_set "return value of pool.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "pool.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_pool_to_pool_t_map "return value of pool.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool.get_all_records" [ session_id ] >>= fun x -> return (From.ref_pool_to_pool_t_map "return value of pool.get_all_records"  x)
  end
  module Pool_patch = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_record" [ session_id; self ] >>= fun x -> return (From.pool_patch_t "return value of pool_patch.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "pool_patch.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_pool_patch "return value of pool_patch.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "pool_patch.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_pool_patch_set "return value of pool_patch.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of pool_patch.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of pool_patch.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of pool_patch.get_name_description"  x)
    (**  *)
    let get_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_version" [ session_id; self ] >>= fun x -> return (From.string "return value of pool_patch.get_version"  x)
    (**  *)
    let get_size ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_size" [ session_id; self ] >>= fun x -> return (From.int64 "return value of pool_patch.get_size"  x)
    (**  *)
    let get_pool_applied ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_pool_applied" [ session_id; self ] >>= fun x -> return (From.bool "return value of pool_patch.get_pool_applied"  x)
    (**  *)
    let get_host_patches ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_host_patches" [ session_id; self ] >>= fun x -> return (From.ref_host_patch_set "return value of pool_patch.get_host_patches"  x)
    (**  *)
    let get_after_apply_guidance ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_after_apply_guidance" [ session_id; self ] >>= fun x -> return (From.after_apply_guidance_set "return value of pool_patch.get_after_apply_guidance"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of pool_patch.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "pool_patch.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "pool_patch.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      let key = To.string key in
      
      rpc_wrapper rpc "pool_patch.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let apply ~rpc ~session_id ~self ~host =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "pool_patch.apply" [ session_id; self; host ] >>= fun x -> return (From.string "return value of pool_patch.apply"  x)
    (**  *)
    let pool_apply ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.pool_apply" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let precheck ~rpc ~session_id ~self ~host =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "pool_patch.precheck" [ session_id; self; host ] >>= fun x -> return (From.string "return value of pool_patch.precheck"  x)
    (**  *)
    let clean ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.clean" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let pool_clean ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.pool_clean" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      
      rpc_wrapper rpc "pool_patch.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let clean_on_host ~rpc ~session_id ~self ~host =
      let session_id = To.ref_session session_id in
      let self = To.ref_pool_patch self in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "pool_patch.clean_on_host" [ session_id; self; host ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool_patch.get_all" [ session_id ] >>= fun x -> return (From.ref_pool_patch_set "return value of pool_patch.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "pool_patch.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_pool_patch_to_pool_patch_t_map "return value of pool_patch.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "pool_patch.get_all_records" [ session_id ] >>= fun x -> return (From.ref_pool_patch_to_pool_patch_t_map "return value of pool_patch.get_all_records"  x)
  end
  module VM = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_record" [ session_id; self ] >>= fun x -> return (From.vM_t "return value of VM.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VM.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VM "return value of VM.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~name_label ~name_description ~user_version ~is_a_template ~affinity ~memory_target ~memory_static_max ~memory_dynamic_max ~memory_dynamic_min ~memory_static_min ~vCPUs_params ~vCPUs_max ~vCPUs_at_startup ~actions_after_shutdown ~actions_after_reboot ~actions_after_crash ~pV_bootloader ~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader_args ~pV_legacy_args ~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier ~platform ~pCI_bus ~other_config ~recommendations ~xenstore_data ~ha_always_run ~ha_restart_priority ~tags ~blocked_operations ~protection_policy ~is_snapshot_from_vmpp ~appliance ~start_delay ~shutdown_delay ~order ~suspend_SR ~version =
      let session_id = To.ref_session session_id in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let user_version = To.int64 user_version in
      let is_a_template = To.bool is_a_template in
      let affinity = To.ref_host affinity in
      let memory_target = To.int64 memory_target in
      let memory_static_max = To.int64 memory_static_max in
      let memory_dynamic_max = To.int64 memory_dynamic_max in
      let memory_dynamic_min = To.int64 memory_dynamic_min in
      let memory_static_min = To.int64 memory_static_min in
      let vCPUs_params = To.string_to_string_map vCPUs_params in
      let vCPUs_max = To.int64 vCPUs_max in
      let vCPUs_at_startup = To.int64 vCPUs_at_startup in
      let actions_after_shutdown = To.on_normal_exit actions_after_shutdown in
      let actions_after_reboot = To.on_normal_exit actions_after_reboot in
      let actions_after_crash = To.on_crash_behaviour actions_after_crash in
      let pV_bootloader = To.string pV_bootloader in
      let pV_kernel = To.string pV_kernel in
      let pV_ramdisk = To.string pV_ramdisk in
      let pV_args = To.string pV_args in
      let pV_bootloader_args = To.string pV_bootloader_args in
      let pV_legacy_args = To.string pV_legacy_args in
      let hVM_boot_policy = To.string hVM_boot_policy in
      let hVM_boot_params = To.string_to_string_map hVM_boot_params in
      let hVM_shadow_multiplier = To.float hVM_shadow_multiplier in
      let platform = To.string_to_string_map platform in
      let pCI_bus = To.string pCI_bus in
      let other_config = To.string_to_string_map other_config in
      let recommendations = To.string recommendations in
      let xenstore_data = To.string_to_string_map xenstore_data in
      let ha_always_run = To.bool ha_always_run in
      let ha_restart_priority = To.string ha_restart_priority in
      let tags = To.string_set tags in
      let blocked_operations = To.vm_operations_to_string_map blocked_operations in
      let protection_policy = To.ref_VMPP protection_policy in
      let is_snapshot_from_vmpp = To.bool is_snapshot_from_vmpp in
      let appliance = To.ref_VM_appliance appliance in
      let start_delay = To.int64 start_delay in
      let shutdown_delay = To.int64 shutdown_delay in
      let order = To.int64 order in
      let suspend_SR = To.ref_SR suspend_SR in
      let version = To.int64 version in
      let __structure = To.structure [ "name_label", name_label; "name_description", name_description; "user_version", user_version; "is_a_template", is_a_template; "affinity", affinity; "memory_target", memory_target; "memory_static_max", memory_static_max; "memory_dynamic_max", memory_dynamic_max; "memory_dynamic_min", memory_dynamic_min; "memory_static_min", memory_static_min; "VCPUs_params", vCPUs_params; "VCPUs_max", vCPUs_max; "VCPUs_at_startup", vCPUs_at_startup; "actions_after_shutdown", actions_after_shutdown; "actions_after_reboot", actions_after_reboot; "actions_after_crash", actions_after_crash; "PV_bootloader", pV_bootloader; "PV_kernel", pV_kernel; "PV_ramdisk", pV_ramdisk; "PV_args", pV_args; "PV_bootloader_args", pV_bootloader_args; "PV_legacy_args", pV_legacy_args; "HVM_boot_policy", hVM_boot_policy; "HVM_boot_params", hVM_boot_params; "HVM_shadow_multiplier", hVM_shadow_multiplier; "platform", platform; "PCI_bus", pCI_bus; "other_config", other_config; "recommendations", recommendations; "xenstore_data", xenstore_data; "ha_always_run", ha_always_run; "ha_restart_priority", ha_restart_priority; "tags", tags; "blocked_operations", blocked_operations; "protection_policy", protection_policy; "is_snapshot_from_vmpp", is_snapshot_from_vmpp; "appliance", appliance; "start_delay", start_delay; "shutdown_delay", shutdown_delay; "order", order; "suspend_SR", suspend_SR; "version", version] in
      rpc_wrapper rpc "VM.create" [ session_id; __structure ] >>= fun x -> return (From.ref_VM "return value of VM.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "VM.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_VM_set "return value of VM.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_uuid"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.vm_operations_set "return value of VM.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_vm_operations_map "return value of VM.get_current_operations"  x)
    (**  *)
    let get_power_state ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_power_state" [ session_id; self ] >>= fun x -> return (From.vm_power_state "return value of VM.get_power_state"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_name_description"  x)
    (**  *)
    let get_user_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_user_version" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_user_version"  x)
    (**  *)
    let get_is_a_template ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_is_a_template" [ session_id; self ] >>= fun x -> return (From.bool "return value of VM.get_is_a_template"  x)
    (**  *)
    let get_suspend_VDI ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_suspend_VDI" [ session_id; self ] >>= fun x -> return (From.ref_VDI "return value of VM.get_suspend_VDI"  x)
    (**  *)
    let get_resident_on ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_resident_on" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of VM.get_resident_on"  x)
    (**  *)
    let get_affinity ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_affinity" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of VM.get_affinity"  x)
    (**  *)
    let get_memory_overhead ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_memory_overhead" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_memory_overhead"  x)
    (**  *)
    let get_memory_target ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_memory_target" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_memory_target"  x)
    (**  *)
    let get_memory_static_max ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_memory_static_max" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_memory_static_max"  x)
    (**  *)
    let get_memory_dynamic_max ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_memory_dynamic_max" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_memory_dynamic_max"  x)
    (**  *)
    let get_memory_dynamic_min ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_memory_dynamic_min" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_memory_dynamic_min"  x)
    (**  *)
    let get_memory_static_min ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_memory_static_min" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_memory_static_min"  x)
    (**  *)
    let get_VCPUs_params ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_VCPUs_params" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.get_VCPUs_params"  x)
    (**  *)
    let get_VCPUs_max ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_VCPUs_max" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_VCPUs_max"  x)
    (**  *)
    let get_VCPUs_at_startup ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_VCPUs_at_startup" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_VCPUs_at_startup"  x)
    (**  *)
    let get_actions_after_shutdown ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_actions_after_shutdown" [ session_id; self ] >>= fun x -> return (From.on_normal_exit "return value of VM.get_actions_after_shutdown"  x)
    (**  *)
    let get_actions_after_reboot ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_actions_after_reboot" [ session_id; self ] >>= fun x -> return (From.on_normal_exit "return value of VM.get_actions_after_reboot"  x)
    (**  *)
    let get_actions_after_crash ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_actions_after_crash" [ session_id; self ] >>= fun x -> return (From.on_crash_behaviour "return value of VM.get_actions_after_crash"  x)
    (**  *)
    let get_consoles ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_consoles" [ session_id; self ] >>= fun x -> return (From.ref_console_set "return value of VM.get_consoles"  x)
    (**  *)
    let get_VIFs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_VIFs" [ session_id; self ] >>= fun x -> return (From.ref_VIF_set "return value of VM.get_VIFs"  x)
    (**  *)
    let get_VBDs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_VBDs" [ session_id; self ] >>= fun x -> return (From.ref_VBD_set "return value of VM.get_VBDs"  x)
    (**  *)
    let get_crash_dumps ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_crash_dumps" [ session_id; self ] >>= fun x -> return (From.ref_crashdump_set "return value of VM.get_crash_dumps"  x)
    (**  *)
    let get_VTPMs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_VTPMs" [ session_id; self ] >>= fun x -> return (From.ref_VTPM_set "return value of VM.get_VTPMs"  x)
    (**  *)
    let get_PV_bootloader ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_PV_bootloader" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_PV_bootloader"  x)
    (**  *)
    let get_PV_kernel ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_PV_kernel" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_PV_kernel"  x)
    (**  *)
    let get_PV_ramdisk ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_PV_ramdisk" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_PV_ramdisk"  x)
    (**  *)
    let get_PV_args ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_PV_args" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_PV_args"  x)
    (**  *)
    let get_PV_bootloader_args ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_PV_bootloader_args" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_PV_bootloader_args"  x)
    (**  *)
    let get_PV_legacy_args ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_PV_legacy_args" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_PV_legacy_args"  x)
    (**  *)
    let get_HVM_boot_policy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_HVM_boot_policy" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_HVM_boot_policy"  x)
    (**  *)
    let get_HVM_boot_params ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_HVM_boot_params" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.get_HVM_boot_params"  x)
    (**  *)
    let get_HVM_shadow_multiplier ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_HVM_shadow_multiplier" [ session_id; self ] >>= fun x -> return (From.float "return value of VM.get_HVM_shadow_multiplier"  x)
    (**  *)
    let get_platform ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_platform" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.get_platform"  x)
    (**  *)
    let get_PCI_bus ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_PCI_bus" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_PCI_bus"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.get_other_config"  x)
    (**  *)
    let get_domid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_domid" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_domid"  x)
    (**  *)
    let get_domarch ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_domarch" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_domarch"  x)
    (**  *)
    let get_last_boot_CPU_flags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_last_boot_CPU_flags" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.get_last_boot_CPU_flags"  x)
    (**  *)
    let get_is_control_domain ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_is_control_domain" [ session_id; self ] >>= fun x -> return (From.bool "return value of VM.get_is_control_domain"  x)
    (**  *)
    let get_metrics ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_metrics" [ session_id; self ] >>= fun x -> return (From.ref_VM_metrics "return value of VM.get_metrics"  x)
    (**  *)
    let get_guest_metrics ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_guest_metrics" [ session_id; self ] >>= fun x -> return (From.ref_VM_guest_metrics "return value of VM.get_guest_metrics"  x)
    (**  *)
    let get_last_booted_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_last_booted_record" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_last_booted_record"  x)
    (**  *)
    let get_recommendations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_recommendations" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_recommendations"  x)
    (**  *)
    let get_xenstore_data ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_xenstore_data" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.get_xenstore_data"  x)
    (**  *)
    let get_ha_always_run ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_ha_always_run" [ session_id; self ] >>= fun x -> return (From.bool "return value of VM.get_ha_always_run"  x)
    (**  *)
    let get_ha_restart_priority ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_ha_restart_priority" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_ha_restart_priority"  x)
    (**  *)
    let get_is_a_snapshot ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_is_a_snapshot" [ session_id; self ] >>= fun x -> return (From.bool "return value of VM.get_is_a_snapshot"  x)
    (**  *)
    let get_snapshot_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_snapshot_of" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of VM.get_snapshot_of"  x)
    (**  *)
    let get_snapshots ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_snapshots" [ session_id; self ] >>= fun x -> return (From.ref_VM_set "return value of VM.get_snapshots"  x)
    (**  *)
    let get_snapshot_time ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_snapshot_time" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VM.get_snapshot_time"  x)
    (**  *)
    let get_transportable_snapshot_id ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_transportable_snapshot_id" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_transportable_snapshot_id"  x)
    (**  *)
    let get_blobs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_blobs" [ session_id; self ] >>= fun x -> return (From.string_to_ref_blob_map "return value of VM.get_blobs"  x)
    (**  *)
    let get_tags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_tags" [ session_id; self ] >>= fun x -> return (From.string_set "return value of VM.get_tags"  x)
    (**  *)
    let get_blocked_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_blocked_operations" [ session_id; self ] >>= fun x -> return (From.vm_operations_to_string_map "return value of VM.get_blocked_operations"  x)
    (**  *)
    let get_snapshot_info ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_snapshot_info" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.get_snapshot_info"  x)
    (**  *)
    let get_snapshot_metadata ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_snapshot_metadata" [ session_id; self ] >>= fun x -> return (From.string "return value of VM.get_snapshot_metadata"  x)
    (**  *)
    let get_parent ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_parent" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of VM.get_parent"  x)
    (**  *)
    let get_children ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_children" [ session_id; self ] >>= fun x -> return (From.ref_VM_set "return value of VM.get_children"  x)
    (**  *)
    let get_bios_strings ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_bios_strings" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.get_bios_strings"  x)
    (**  *)
    let get_protection_policy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_protection_policy" [ session_id; self ] >>= fun x -> return (From.ref_VMPP "return value of VM.get_protection_policy"  x)
    (**  *)
    let get_is_snapshot_from_vmpp ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_is_snapshot_from_vmpp" [ session_id; self ] >>= fun x -> return (From.bool "return value of VM.get_is_snapshot_from_vmpp"  x)
    (**  *)
    let get_appliance ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_appliance" [ session_id; self ] >>= fun x -> return (From.ref_VM_appliance "return value of VM.get_appliance"  x)
    (**  *)
    let get_start_delay ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_start_delay" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_start_delay"  x)
    (**  *)
    let get_shutdown_delay ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_shutdown_delay" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_shutdown_delay"  x)
    (**  *)
    let get_order ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_order" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_order"  x)
    (**  *)
    let get_VGPUs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_VGPUs" [ session_id; self ] >>= fun x -> return (From.ref_VGPU_set "return value of VM.get_VGPUs"  x)
    (**  *)
    let get_attached_PCIs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_attached_PCIs" [ session_id; self ] >>= fun x -> return (From.ref_PCI_set "return value of VM.get_attached_PCIs"  x)
    (**  *)
    let get_suspend_SR ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_suspend_SR" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of VM.get_suspend_SR"  x)
    (**  *)
    let get_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_version" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM.get_version"  x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_user_version ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_user_version" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_is_a_template ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VM.set_is_a_template" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_affinity ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.ref_host value in
      
      rpc_wrapper rpc "VM.set_affinity" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_VCPUs_params ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VM.set_VCPUs_params" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_VCPUs_params ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.add_to_VCPUs_params" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_VCPUs_params ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      
      rpc_wrapper rpc "VM.remove_from_VCPUs_params" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_actions_after_shutdown ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.on_normal_exit value in
      
      rpc_wrapper rpc "VM.set_actions_after_shutdown" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_actions_after_reboot ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.on_normal_exit value in
      
      rpc_wrapper rpc "VM.set_actions_after_reboot" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_actions_after_crash ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.on_crash_behaviour value in
      
      rpc_wrapper rpc "VM.set_actions_after_crash" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_PV_bootloader ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_PV_bootloader" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_PV_kernel ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_PV_kernel" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_PV_ramdisk ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_PV_ramdisk" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_PV_args ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_PV_args" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_PV_bootloader_args ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_PV_bootloader_args" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_PV_legacy_args ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_PV_legacy_args" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_HVM_boot_policy ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_HVM_boot_policy" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_HVM_boot_params ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VM.set_HVM_boot_params" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_HVM_boot_params ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.add_to_HVM_boot_params" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_HVM_boot_params ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      
      rpc_wrapper rpc "VM.remove_from_HVM_boot_params" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_platform ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VM.set_platform" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_platform ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.add_to_platform" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_platform ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      
      rpc_wrapper rpc "VM.remove_from_platform" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_PCI_bus ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_PCI_bus" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VM.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      
      rpc_wrapper rpc "VM.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_recommendations ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_recommendations" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_xenstore_data ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VM.set_xenstore_data" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_xenstore_data ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.add_to_xenstore_data" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_xenstore_data ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      
      rpc_wrapper rpc "VM.remove_from_xenstore_data" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string_set value in
      
      rpc_wrapper rpc "VM.set_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.add_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.remove_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_blocked_operations ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.vm_operations_to_string_map value in
      
      rpc_wrapper rpc "VM.set_blocked_operations" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_blocked_operations ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.vm_operations key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.add_to_blocked_operations" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_blocked_operations ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.vm_operations key in
      
      rpc_wrapper rpc "VM.remove_from_blocked_operations" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_suspend_SR ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.ref_SR value in
      
      rpc_wrapper rpc "VM.set_suspend_SR" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let snapshot ~rpc ~session_id ~vm ~new_name =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let new_name = To.string new_name in
      
      rpc_wrapper rpc "VM.snapshot" [ session_id; vm; new_name ] >>= fun x -> return (From.ref_VM "return value of VM.snapshot"  x)
    (**  *)
    let snapshot_with_quiesce ~rpc ~session_id ~vm ~new_name =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let new_name = To.string new_name in
      
      rpc_wrapper rpc "VM.snapshot_with_quiesce" [ session_id; vm; new_name ] >>= fun x -> return (From.ref_VM "return value of VM.snapshot_with_quiesce"  x)
    (**  *)
    let clone ~rpc ~session_id ~vm ~new_name =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let new_name = To.string new_name in
      
      rpc_wrapper rpc "VM.clone" [ session_id; vm; new_name ] >>= fun x -> return (From.ref_VM "return value of VM.clone"  x)
    (**  *)
    let copy ~rpc ~session_id ~vm ~new_name ~sr =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let new_name = To.string new_name in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "VM.copy" [ session_id; vm; new_name; sr ] >>= fun x -> return (From.ref_VM "return value of VM.copy"  x)
    (**  *)
    let revert ~rpc ~session_id ~snapshot =
      let session_id = To.ref_session session_id in
      let snapshot = To.ref_VM snapshot in
      
      rpc_wrapper rpc "VM.revert" [ session_id; snapshot ] >>= fun x -> return (ignore x)
    (**  *)
    let checkpoint ~rpc ~session_id ~vm ~new_name =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let new_name = To.string new_name in
      
      rpc_wrapper rpc "VM.checkpoint" [ session_id; vm; new_name ] >>= fun x -> return (From.ref_VM "return value of VM.checkpoint"  x)
    (**  *)
    let provision ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.provision" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let start ~rpc ~session_id ~vm ~start_paused ~force =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let start_paused = To.bool start_paused in
      let force = To.bool force in
      
      rpc_wrapper rpc "VM.start" [ session_id; vm; start_paused; force ] >>= fun x -> return (ignore x)
    (**  *)
    let start_on ~rpc ~session_id ~vm ~host ~start_paused ~force =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let host = To.ref_host host in
      let start_paused = To.bool start_paused in
      let force = To.bool force in
      
      rpc_wrapper rpc "VM.start_on" [ session_id; vm; host; start_paused; force ] >>= fun x -> return (ignore x)
    (**  *)
    let pause ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.pause" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let unpause ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.unpause" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let clean_shutdown ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.clean_shutdown" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let clean_reboot ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.clean_reboot" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let hard_shutdown ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.hard_shutdown" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let power_state_reset ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.power_state_reset" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let hard_reboot ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.hard_reboot" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let suspend ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.suspend" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let csvm ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.csvm" [ session_id; vm ] >>= fun x -> return (From.ref_VM "return value of VM.csvm"  x)
    (**  *)
    let resume ~rpc ~session_id ~vm ~start_paused ~force =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let start_paused = To.bool start_paused in
      let force = To.bool force in
      
      rpc_wrapper rpc "VM.resume" [ session_id; vm; start_paused; force ] >>= fun x -> return (ignore x)
    (**  *)
    let hard_reboot_internal ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.hard_reboot_internal" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let resume_on ~rpc ~session_id ~vm ~host ~start_paused ~force =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let host = To.ref_host host in
      let start_paused = To.bool start_paused in
      let force = To.bool force in
      
      rpc_wrapper rpc "VM.resume_on" [ session_id; vm; host; start_paused; force ] >>= fun x -> return (ignore x)
    (**  *)
    let pool_migrate ~rpc ~session_id ~vm ~host ~options =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let host = To.ref_host host in
      let options = To.string_to_string_map options in
      
      rpc_wrapper rpc "VM.pool_migrate" [ session_id; vm; host; options ] >>= fun x -> return (ignore x)
    (**  *)
    let pool_migrate_complete ~rpc ~session_id ~vm ~host =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "VM.pool_migrate_complete" [ session_id; vm; host ] >>= fun x -> return (ignore x)
    (**  *)
    let set_VCPUs_number_live ~rpc ~session_id ~self ~nvcpu =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let nvcpu = To.int64 nvcpu in
      
      rpc_wrapper rpc "VM.set_VCPUs_number_live" [ session_id; self; nvcpu ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_VCPUs_params_live ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.add_to_VCPUs_params_live" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_ha_restart_priority ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM.set_ha_restart_priority" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_ha_always_run ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VM.set_ha_always_run" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let compute_memory_overhead ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.compute_memory_overhead" [ session_id; vm ] >>= fun x -> return (From.int64 "return value of VM.compute_memory_overhead"  x)
    (**  *)
    let set_memory_dynamic_max ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_memory_dynamic_max" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_memory_dynamic_min ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_memory_dynamic_min" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_memory_dynamic_range ~rpc ~session_id ~self ~min ~max =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let min = To.int64 min in
      let max = To.int64 max in
      
      rpc_wrapper rpc "VM.set_memory_dynamic_range" [ session_id; self; min; max ] >>= fun x -> return (ignore x)
    (**  *)
    let set_memory_static_max ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_memory_static_max" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_memory_static_min ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_memory_static_min" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_memory_static_range ~rpc ~session_id ~self ~min ~max =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let min = To.int64 min in
      let max = To.int64 max in
      
      rpc_wrapper rpc "VM.set_memory_static_range" [ session_id; self; min; max ] >>= fun x -> return (ignore x)
    (**  *)
    let set_memory_limits ~rpc ~session_id ~self ~static_min ~static_max ~dynamic_min ~dynamic_max =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let static_min = To.int64 static_min in
      let static_max = To.int64 static_max in
      let dynamic_min = To.int64 dynamic_min in
      let dynamic_max = To.int64 dynamic_max in
      
      rpc_wrapper rpc "VM.set_memory_limits" [ session_id; self; static_min; static_max; dynamic_min; dynamic_max ] >>= fun x -> return (ignore x)
    (**  *)
    let set_memory_target_live ~rpc ~session_id ~self ~target =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let target = To.int64 target in
      
      rpc_wrapper rpc "VM.set_memory_target_live" [ session_id; self; target ] >>= fun x -> return (ignore x)
    (**  *)
    let wait_memory_target_live ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.wait_memory_target_live" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_cooperative ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_cooperative" [ session_id; self ] >>= fun x -> return (From.bool "return value of VM.get_cooperative"  x)
    (**  *)
    let set_HVM_shadow_multiplier ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.float value in
      
      rpc_wrapper rpc "VM.set_HVM_shadow_multiplier" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_shadow_multiplier_live ~rpc ~session_id ~self ~multiplier =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let multiplier = To.float multiplier in
      
      rpc_wrapper rpc "VM.set_shadow_multiplier_live" [ session_id; self; multiplier ] >>= fun x -> return (ignore x)
    (**  *)
    let set_VCPUs_max ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_VCPUs_max" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_VCPUs_at_startup ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_VCPUs_at_startup" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let send_sysrq ~rpc ~session_id ~vm ~key =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let key = To.string key in
      
      rpc_wrapper rpc "VM.send_sysrq" [ session_id; vm; key ] >>= fun x -> return (ignore x)
    (**  *)
    let send_trigger ~rpc ~session_id ~vm ~trigger =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let trigger = To.string trigger in
      
      rpc_wrapper rpc "VM.send_trigger" [ session_id; vm; trigger ] >>= fun x -> return (ignore x)
    (**  *)
    let maximise_memory ~rpc ~session_id ~self ~total ~approximate =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let total = To.int64 total in
      let approximate = To.bool approximate in
      
      rpc_wrapper rpc "VM.maximise_memory" [ session_id; self; total; approximate ] >>= fun x -> return (From.int64 "return value of VM.maximise_memory"  x)
    (**  *)
    let migrate_send ~rpc ~session_id ~vm ~dest ~live ~vdi_map ~vif_map ~options =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let dest = To.string_to_string_map dest in
      let live = To.bool live in
      let vdi_map = To.ref_VDI_to_ref_SR_map vdi_map in
      let vif_map = To.ref_VIF_to_ref_network_map vif_map in
      let options = To.string_to_string_map options in
      
      rpc_wrapper rpc "VM.migrate_send" [ session_id; vm; dest; live; vdi_map; vif_map; options ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_can_migrate ~rpc ~session_id ~vm ~dest ~live ~vdi_map ~vif_map ~options =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let dest = To.string_to_string_map dest in
      let live = To.bool live in
      let vdi_map = To.ref_VDI_to_ref_SR_map vdi_map in
      let vif_map = To.ref_VIF_to_ref_network_map vif_map in
      let options = To.string_to_string_map options in
      
      rpc_wrapper rpc "VM.assert_can_migrate" [ session_id; vm; dest; live; vdi_map; vif_map; options ] >>= fun x -> return (ignore x)
    (**  *)
    let get_boot_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_boot_record" [ session_id; self ] >>= fun x -> return (From.vM_t "return value of VM.get_boot_record"  x)
    (**  *)
    let get_data_sources ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.get_data_sources" [ session_id; self ] >>= fun x -> return (From.data_source_t_set "return value of VM.get_data_sources"  x)
    (**  *)
    let record_data_source ~rpc ~session_id ~self ~data_source =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let data_source = To.string data_source in
      
      rpc_wrapper rpc "VM.record_data_source" [ session_id; self; data_source ] >>= fun x -> return (ignore x)
    (**  *)
    let query_data_source ~rpc ~session_id ~self ~data_source =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let data_source = To.string data_source in
      
      rpc_wrapper rpc "VM.query_data_source" [ session_id; self; data_source ] >>= fun x -> return (From.float "return value of VM.query_data_source"  x)
    (**  *)
    let forget_data_source_archives ~rpc ~session_id ~self ~data_source =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let data_source = To.string data_source in
      
      rpc_wrapper rpc "VM.forget_data_source_archives" [ session_id; self; data_source ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_operation_valid ~rpc ~session_id ~self ~op =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let op = To.vm_operations op in
      
      rpc_wrapper rpc "VM.assert_operation_valid" [ session_id; self; op ] >>= fun x -> return (ignore x)
    (**  *)
    let update_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.update_allowed_operations" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_allowed_VBD_devices ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.get_allowed_VBD_devices" [ session_id; vm ] >>= fun x -> return (From.string_set "return value of VM.get_allowed_VBD_devices"  x)
    (**  *)
    let get_allowed_VIF_devices ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.get_allowed_VIF_devices" [ session_id; vm ] >>= fun x -> return (From.string_set "return value of VM.get_allowed_VIF_devices"  x)
    (**  *)
    let get_possible_hosts ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.get_possible_hosts" [ session_id; vm ] >>= fun x -> return (From.ref_host_set "return value of VM.get_possible_hosts"  x)
    (**  *)
    let assert_can_boot_here ~rpc ~session_id ~self ~host =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "VM.assert_can_boot_here" [ session_id; self; host ] >>= fun x -> return (ignore x)
    (**  *)
    let atomic_set_resident_on ~rpc ~session_id ~vm ~host =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "VM.atomic_set_resident_on" [ session_id; vm; host ] >>= fun x -> return (ignore x)
    (**  *)
    let create_new_blob ~rpc ~session_id ~vm ~name ~mime_type ~public =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let name = To.string name in
      let mime_type = To.string mime_type in
      let public = To.bool public in
      
      rpc_wrapper rpc "VM.create_new_blob" [ session_id; vm; name; mime_type; public ] >>= fun x -> return (From.ref_blob "return value of VM.create_new_blob"  x)
    (**  *)
    let s3_suspend ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.s3_suspend" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let s3_resume ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.s3_resume" [ session_id; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_agile ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.assert_agile" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let update_snapshot_metadata ~rpc ~session_id ~vm ~snapshot_of ~snapshot_time ~transportable_snapshot_id =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let snapshot_of = To.ref_VM snapshot_of in
      let snapshot_time = To.datetime snapshot_time in
      let transportable_snapshot_id = To.string transportable_snapshot_id in
      
      rpc_wrapper rpc "VM.update_snapshot_metadata" [ session_id; vm; snapshot_of; snapshot_time; transportable_snapshot_id ] >>= fun x -> return (ignore x)
    (**  *)
    let retrieve_wlb_recommendations ~rpc ~session_id ~vm =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "VM.retrieve_wlb_recommendations" [ session_id; vm ] >>= fun x -> return (From.ref_host_to_string_set_map "return value of VM.retrieve_wlb_recommendations"  x)
    (**  *)
    let copy_bios_strings ~rpc ~session_id ~vm ~host =
      let session_id = To.ref_session session_id in
      let vm = To.ref_VM vm in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "VM.copy_bios_strings" [ session_id; vm; host ] >>= fun x -> return (ignore x)
    (**  *)
    let set_protection_policy ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.ref_VMPP value in
      
      rpc_wrapper rpc "VM.set_protection_policy" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_start_delay ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_start_delay" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_shutdown_delay ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_shutdown_delay" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_order ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VM.set_order" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_suspend_VDI ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.ref_VDI value in
      
      rpc_wrapper rpc "VM.set_suspend_VDI" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_can_be_recovered ~rpc ~session_id ~self ~session_to =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let session_to = To.ref_session session_to in
      
      rpc_wrapper rpc "VM.assert_can_be_recovered" [ session_id; self; session_to ] >>= fun x -> return (ignore x)
    (**  *)
    let recover ~rpc ~session_id ~self ~session_to ~force =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let session_to = To.ref_session session_to in
      let force = To.bool force in
      
      rpc_wrapper rpc "VM.recover" [ session_id; self; session_to; force ] >>= fun x -> return (ignore x)
    (**  *)
    let import_convert ~rpc ~session_id ~_type ~username ~password ~sr ~remote_config =
      let session_id = To.ref_session session_id in
      let _type = To.string _type in
      let username = To.string username in
      let password = To.string password in
      let sr = To.ref_SR sr in
      let remote_config = To.string_to_string_map remote_config in
      
      rpc_wrapper rpc "VM.import_convert" [ session_id; _type; username; password; sr; remote_config ] >>= fun x -> return (ignore x)
    (**  *)
    let set_appliance ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      let value = To.ref_VM_appliance value in
      
      rpc_wrapper rpc "VM.set_appliance" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let query_services ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM self in
      
      rpc_wrapper rpc "VM.query_services" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM.query_services"  x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VM.get_all" [ session_id ] >>= fun x -> return (From.ref_VM_set "return value of VM.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VM.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VM_to_vM_t_map "return value of VM.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VM.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VM_to_vM_t_map "return value of VM.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~name_label:value.vM_name_label
      ~name_description:value.vM_name_description
      ~user_version:value.vM_user_version
      ~is_a_template:value.vM_is_a_template
      ~affinity:value.vM_affinity
      ~memory_target:value.vM_memory_target
      ~memory_static_max:value.vM_memory_static_max
      ~memory_dynamic_max:value.vM_memory_dynamic_max
      ~memory_dynamic_min:value.vM_memory_dynamic_min
      ~memory_static_min:value.vM_memory_static_min
      ~vCPUs_params:value.vM_VCPUs_params
      ~vCPUs_max:value.vM_VCPUs_max
      ~vCPUs_at_startup:value.vM_VCPUs_at_startup
      ~actions_after_shutdown:value.vM_actions_after_shutdown
      ~actions_after_reboot:value.vM_actions_after_reboot
      ~actions_after_crash:value.vM_actions_after_crash
      ~pV_bootloader:value.vM_PV_bootloader
      ~pV_kernel:value.vM_PV_kernel
      ~pV_ramdisk:value.vM_PV_ramdisk
      ~pV_args:value.vM_PV_args
      ~pV_bootloader_args:value.vM_PV_bootloader_args
      ~pV_legacy_args:value.vM_PV_legacy_args
      ~hVM_boot_policy:value.vM_HVM_boot_policy
      ~hVM_boot_params:value.vM_HVM_boot_params
      ~hVM_shadow_multiplier:value.vM_HVM_shadow_multiplier
      ~platform:value.vM_platform
      ~pCI_bus:value.vM_PCI_bus
      ~other_config:value.vM_other_config
      ~recommendations:value.vM_recommendations
      ~xenstore_data:value.vM_xenstore_data
      ~ha_always_run:value.vM_ha_always_run
      ~ha_restart_priority:value.vM_ha_restart_priority
      ~tags:value.vM_tags
      ~blocked_operations:value.vM_blocked_operations
      ~protection_policy:value.vM_protection_policy
      ~is_snapshot_from_vmpp:value.vM_is_snapshot_from_vmpp
      ~appliance:value.vM_appliance
      ~start_delay:value.vM_start_delay
      ~shutdown_delay:value.vM_shutdown_delay
      ~order:value.vM_order
      ~suspend_SR:value.vM_suspend_SR
      ~version:value.vM_version
  end
  module VM_metrics = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_record" [ session_id; self ] >>= fun x -> return (From.vM_metrics_t "return value of VM_metrics.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VM_metrics.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VM_metrics "return value of VM_metrics.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VM_metrics.get_uuid"  x)
    (**  *)
    let get_memory_actual ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_memory_actual" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM_metrics.get_memory_actual"  x)
    (**  *)
    let get_VCPUs_number ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_VCPUs_number" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VM_metrics.get_VCPUs_number"  x)
    (**  *)
    let get_VCPUs_utilisation ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_VCPUs_utilisation" [ session_id; self ] >>= fun x -> return (From.int64_to_float_map "return value of VM_metrics.get_VCPUs_utilisation"  x)
    (**  *)
    let get_VCPUs_CPU ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_VCPUs_CPU" [ session_id; self ] >>= fun x -> return (From.int64_to_int64_map "return value of VM_metrics.get_VCPUs_CPU"  x)
    (**  *)
    let get_VCPUs_params ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_VCPUs_params" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_metrics.get_VCPUs_params"  x)
    (**  *)
    let get_VCPUs_flags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_VCPUs_flags" [ session_id; self ] >>= fun x -> return (From.int64_to_string_set_map "return value of VM_metrics.get_VCPUs_flags"  x)
    (**  *)
    let get_state ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_state" [ session_id; self ] >>= fun x -> return (From.string_set "return value of VM_metrics.get_state"  x)
    (**  *)
    let get_start_time ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_start_time" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VM_metrics.get_start_time"  x)
    (**  *)
    let get_install_time ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_install_time" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VM_metrics.get_install_time"  x)
    (**  *)
    let get_last_updated ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_last_updated" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VM_metrics.get_last_updated"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      
      rpc_wrapper rpc "VM_metrics.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_metrics.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VM_metrics.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM_metrics.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_metrics self in
      let key = To.string key in
      
      rpc_wrapper rpc "VM_metrics.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VM_metrics.get_all" [ session_id ] >>= fun x -> return (From.ref_VM_metrics_set "return value of VM_metrics.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VM_metrics.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VM_metrics_to_vM_metrics_t_map "return value of VM_metrics.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VM_metrics.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VM_metrics_to_vM_metrics_t_map "return value of VM_metrics.get_all_records"  x)
  end
  module VM_guest_metrics = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_record" [ session_id; self ] >>= fun x -> return (From.vM_guest_metrics_t "return value of VM_guest_metrics.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VM_guest_metrics.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VM_guest_metrics "return value of VM_guest_metrics.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VM_guest_metrics.get_uuid"  x)
    (**  *)
    let get_os_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_os_version" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_guest_metrics.get_os_version"  x)
    (**  *)
    let get_PV_drivers_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_PV_drivers_version" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_guest_metrics.get_PV_drivers_version"  x)
    (**  *)
    let get_PV_drivers_up_to_date ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_PV_drivers_up_to_date" [ session_id; self ] >>= fun x -> return (From.bool "return value of VM_guest_metrics.get_PV_drivers_up_to_date"  x)
    (**  *)
    let get_memory ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_memory" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_guest_metrics.get_memory"  x)
    (**  *)
    let get_disks ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_disks" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_guest_metrics.get_disks"  x)
    (**  *)
    let get_networks ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_networks" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_guest_metrics.get_networks"  x)
    (**  *)
    let get_other ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_other" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_guest_metrics.get_other"  x)
    (**  *)
    let get_last_updated ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_last_updated" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VM_guest_metrics.get_last_updated"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VM_guest_metrics.get_other_config"  x)
    (**  *)
    let get_live ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      
      rpc_wrapper rpc "VM_guest_metrics.get_live" [ session_id; self ] >>= fun x -> return (From.bool "return value of VM_guest_metrics.get_live"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VM_guest_metrics.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VM_guest_metrics.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_guest_metrics self in
      let key = To.string key in
      
      rpc_wrapper rpc "VM_guest_metrics.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VM_guest_metrics.get_all" [ session_id ] >>= fun x -> return (From.ref_VM_guest_metrics_set "return value of VM_guest_metrics.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VM_guest_metrics.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VM_guest_metrics_to_vM_guest_metrics_t_map "return value of VM_guest_metrics.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VM_guest_metrics.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VM_guest_metrics_to_vM_guest_metrics_t_map "return value of VM_guest_metrics.get_all_records"  x)
  end
  module VMPP = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_record" [ session_id; self ] >>= fun x -> return (From.vMPP_t "return value of VMPP.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VMPP.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VMPP "return value of VMPP.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~name_label ~name_description ~is_policy_enabled ~backup_type ~backup_retention_value ~backup_frequency ~backup_schedule ~archive_target_type ~archive_target_config ~archive_frequency ~archive_schedule ~is_alarm_enabled ~alarm_config =
      let session_id = To.ref_session session_id in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let is_policy_enabled = To.bool is_policy_enabled in
      let backup_type = To.vmpp_backup_type backup_type in
      let backup_retention_value = To.int64 backup_retention_value in
      let backup_frequency = To.vmpp_backup_frequency backup_frequency in
      let backup_schedule = To.string_to_string_map backup_schedule in
      let archive_target_type = To.vmpp_archive_target_type archive_target_type in
      let archive_target_config = To.string_to_string_map archive_target_config in
      let archive_frequency = To.vmpp_archive_frequency archive_frequency in
      let archive_schedule = To.string_to_string_map archive_schedule in
      let is_alarm_enabled = To.bool is_alarm_enabled in
      let alarm_config = To.string_to_string_map alarm_config in
      let __structure = To.structure [ "name_label", name_label; "name_description", name_description; "is_policy_enabled", is_policy_enabled; "backup_type", backup_type; "backup_retention_value", backup_retention_value; "backup_frequency", backup_frequency; "backup_schedule", backup_schedule; "archive_target_type", archive_target_type; "archive_target_config", archive_target_config; "archive_frequency", archive_frequency; "archive_schedule", archive_schedule; "is_alarm_enabled", is_alarm_enabled; "alarm_config", alarm_config] in
      rpc_wrapper rpc "VMPP.create" [ session_id; __structure ] >>= fun x -> return (From.ref_VMPP "return value of VMPP.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "VMPP.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_VMPP_set "return value of VMPP.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VMPP.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of VMPP.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of VMPP.get_name_description"  x)
    (**  *)
    let get_is_policy_enabled ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_is_policy_enabled" [ session_id; self ] >>= fun x -> return (From.bool "return value of VMPP.get_is_policy_enabled"  x)
    (**  *)
    let get_backup_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_backup_type" [ session_id; self ] >>= fun x -> return (From.vmpp_backup_type "return value of VMPP.get_backup_type"  x)
    (**  *)
    let get_backup_retention_value ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_backup_retention_value" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VMPP.get_backup_retention_value"  x)
    (**  *)
    let get_backup_frequency ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_backup_frequency" [ session_id; self ] >>= fun x -> return (From.vmpp_backup_frequency "return value of VMPP.get_backup_frequency"  x)
    (**  *)
    let get_backup_schedule ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_backup_schedule" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VMPP.get_backup_schedule"  x)
    (**  *)
    let get_is_backup_running ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_is_backup_running" [ session_id; self ] >>= fun x -> return (From.bool "return value of VMPP.get_is_backup_running"  x)
    (**  *)
    let get_backup_last_run_time ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_backup_last_run_time" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VMPP.get_backup_last_run_time"  x)
    (**  *)
    let get_archive_target_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_archive_target_type" [ session_id; self ] >>= fun x -> return (From.vmpp_archive_target_type "return value of VMPP.get_archive_target_type"  x)
    (**  *)
    let get_archive_target_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_archive_target_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VMPP.get_archive_target_config"  x)
    (**  *)
    let get_archive_frequency ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_archive_frequency" [ session_id; self ] >>= fun x -> return (From.vmpp_archive_frequency "return value of VMPP.get_archive_frequency"  x)
    (**  *)
    let get_archive_schedule ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_archive_schedule" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VMPP.get_archive_schedule"  x)
    (**  *)
    let get_is_archive_running ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_is_archive_running" [ session_id; self ] >>= fun x -> return (From.bool "return value of VMPP.get_is_archive_running"  x)
    (**  *)
    let get_archive_last_run_time ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_archive_last_run_time" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VMPP.get_archive_last_run_time"  x)
    (**  *)
    let get_VMs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_VMs" [ session_id; self ] >>= fun x -> return (From.ref_VM_set "return value of VMPP.get_VMs"  x)
    (**  *)
    let get_is_alarm_enabled ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_is_alarm_enabled" [ session_id; self ] >>= fun x -> return (From.bool "return value of VMPP.get_is_alarm_enabled"  x)
    (**  *)
    let get_alarm_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_alarm_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VMPP.get_alarm_config"  x)
    (**  *)
    let get_recent_alerts ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      
      rpc_wrapper rpc "VMPP.get_recent_alerts" [ session_id; self ] >>= fun x -> return (From.string_set "return value of VMPP.get_recent_alerts"  x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.string value in
      
      rpc_wrapper rpc "VMPP.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.string value in
      
      rpc_wrapper rpc "VMPP.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_is_policy_enabled ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VMPP.set_is_policy_enabled" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_backup_type ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.vmpp_backup_type value in
      
      rpc_wrapper rpc "VMPP.set_backup_type" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let protect_now ~rpc ~session_id ~vmpp =
      let session_id = To.ref_session session_id in
      let vmpp = To.ref_VMPP vmpp in
      
      rpc_wrapper rpc "VMPP.protect_now" [ session_id; vmpp ] >>= fun x -> return (From.string "return value of VMPP.protect_now"  x)
    (**  *)
    let archive_now ~rpc ~session_id ~snapshot =
      let session_id = To.ref_session session_id in
      let snapshot = To.ref_VM snapshot in
      
      rpc_wrapper rpc "VMPP.archive_now" [ session_id; snapshot ] >>= fun x -> return (From.string "return value of VMPP.archive_now"  x)
    (**  *)
    let create_alert ~rpc ~session_id ~vmpp ~name ~priority ~body ~data =
      let session_id = To.ref_session session_id in
      let vmpp = To.ref_VMPP vmpp in
      let name = To.string name in
      let priority = To.int64 priority in
      let body = To.string body in
      let data = To.string data in
      
      rpc_wrapper rpc "VMPP.create_alert" [ session_id; vmpp; name; priority; body; data ] >>= fun x -> return (ignore x)
    (**  *)
    let get_alerts ~rpc ~session_id ~vmpp ~hours_from_now =
      let session_id = To.ref_session session_id in
      let vmpp = To.ref_VMPP vmpp in
      let hours_from_now = To.int64 hours_from_now in
      
      rpc_wrapper rpc "VMPP.get_alerts" [ session_id; vmpp; hours_from_now ] >>= fun x -> return (From.string_set "return value of VMPP.get_alerts"  x)
    (**  *)
    let set_backup_retention_value ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VMPP.set_backup_retention_value" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_is_backup_running ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VMPP.set_is_backup_running" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_is_archive_running ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VMPP.set_is_archive_running" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_backup_frequency ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.vmpp_backup_frequency value in
      
      rpc_wrapper rpc "VMPP.set_backup_frequency" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_backup_schedule ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VMPP.set_backup_schedule" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_archive_frequency ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.vmpp_archive_frequency value in
      
      rpc_wrapper rpc "VMPP.set_archive_frequency" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_archive_schedule ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VMPP.set_archive_schedule" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_archive_target_type ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.vmpp_archive_target_type value in
      
      rpc_wrapper rpc "VMPP.set_archive_target_type" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_archive_target_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VMPP.set_archive_target_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_is_alarm_enabled ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VMPP.set_is_alarm_enabled" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_alarm_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VMPP.set_alarm_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_backup_schedule ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VMPP.add_to_backup_schedule" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_archive_target_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VMPP.add_to_archive_target_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_archive_schedule ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VMPP.add_to_archive_schedule" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_alarm_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VMPP.add_to_alarm_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_backup_schedule ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let key = To.string key in
      
      rpc_wrapper rpc "VMPP.remove_from_backup_schedule" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_archive_target_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let key = To.string key in
      
      rpc_wrapper rpc "VMPP.remove_from_archive_target_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_archive_schedule ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let key = To.string key in
      
      rpc_wrapper rpc "VMPP.remove_from_archive_schedule" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_alarm_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let key = To.string key in
      
      rpc_wrapper rpc "VMPP.remove_from_alarm_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_backup_last_run_time ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.datetime value in
      
      rpc_wrapper rpc "VMPP.set_backup_last_run_time" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_archive_last_run_time ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VMPP self in
      let value = To.datetime value in
      
      rpc_wrapper rpc "VMPP.set_archive_last_run_time" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VMPP.get_all" [ session_id ] >>= fun x -> return (From.ref_VMPP_set "return value of VMPP.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VMPP.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VMPP_to_vMPP_t_map "return value of VMPP.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VMPP.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VMPP_to_vMPP_t_map "return value of VMPP.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~name_label:value.vMPP_name_label
      ~name_description:value.vMPP_name_description
      ~is_policy_enabled:value.vMPP_is_policy_enabled
      ~backup_type:value.vMPP_backup_type
      ~backup_retention_value:value.vMPP_backup_retention_value
      ~backup_frequency:value.vMPP_backup_frequency
      ~backup_schedule:value.vMPP_backup_schedule
      ~archive_target_type:value.vMPP_archive_target_type
      ~archive_target_config:value.vMPP_archive_target_config
      ~archive_frequency:value.vMPP_archive_frequency
      ~archive_schedule:value.vMPP_archive_schedule
      ~is_alarm_enabled:value.vMPP_is_alarm_enabled
      ~alarm_config:value.vMPP_alarm_config
  end
  module VM_appliance = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.get_record" [ session_id; self ] >>= fun x -> return (From.vM_appliance_t "return value of VM_appliance.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VM_appliance.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VM_appliance "return value of VM_appliance.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~name_label ~name_description =
      let session_id = To.ref_session session_id in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let __structure = To.structure [ "name_label", name_label; "name_description", name_description] in
      rpc_wrapper rpc "VM_appliance.create" [ session_id; __structure ] >>= fun x -> return (From.ref_VM_appliance "return value of VM_appliance.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "VM_appliance.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_VM_appliance_set "return value of VM_appliance.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VM_appliance.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of VM_appliance.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of VM_appliance.get_name_description"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.vm_appliance_operation_set "return value of VM_appliance.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_vm_appliance_operation_map "return value of VM_appliance.get_current_operations"  x)
    (**  *)
    let get_VMs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.get_VMs" [ session_id; self ] >>= fun x -> return (From.ref_VM_set "return value of VM_appliance.get_VMs"  x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM_appliance.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      let value = To.string value in
      
      rpc_wrapper rpc "VM_appliance.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let start ~rpc ~session_id ~self ~paused =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      let paused = To.bool paused in
      
      rpc_wrapper rpc "VM_appliance.start" [ session_id; self; paused ] >>= fun x -> return (ignore x)
    (**  *)
    let clean_shutdown ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.clean_shutdown" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let hard_shutdown ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.hard_shutdown" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let shutdown ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      
      rpc_wrapper rpc "VM_appliance.shutdown" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_can_be_recovered ~rpc ~session_id ~self ~session_to =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      let session_to = To.ref_session session_to in
      
      rpc_wrapper rpc "VM_appliance.assert_can_be_recovered" [ session_id; self; session_to ] >>= fun x -> return (ignore x)
    (**  *)
    let recover ~rpc ~session_id ~self ~session_to ~force =
      let session_id = To.ref_session session_id in
      let self = To.ref_VM_appliance self in
      let session_to = To.ref_session session_to in
      let force = To.bool force in
      
      rpc_wrapper rpc "VM_appliance.recover" [ session_id; self; session_to; force ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VM_appliance.get_all" [ session_id ] >>= fun x -> return (From.ref_VM_appliance_set "return value of VM_appliance.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VM_appliance.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VM_appliance_to_vM_appliance_t_map "return value of VM_appliance.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VM_appliance.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VM_appliance_to_vM_appliance_t_map "return value of VM_appliance.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~name_label:value.vM_appliance_name_label
      ~name_description:value.vM_appliance_name_description
  end
  module DR_task = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_DR_task self in
      
      rpc_wrapper rpc "DR_task.get_record" [ session_id; self ] >>= fun x -> return (From.dR_task_t "return value of DR_task.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "DR_task.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_DR_task "return value of DR_task.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_DR_task self in
      
      rpc_wrapper rpc "DR_task.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of DR_task.get_uuid"  x)
    (**  *)
    let get_introduced_SRs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_DR_task self in
      
      rpc_wrapper rpc "DR_task.get_introduced_SRs" [ session_id; self ] >>= fun x -> return (From.ref_SR_set "return value of DR_task.get_introduced_SRs"  x)
    (**  *)
    let create ~rpc ~session_id ~_type ~device_config ~whitelist =
      let session_id = To.ref_session session_id in
      let _type = To.string _type in
      let device_config = To.string_to_string_map device_config in
      let whitelist = To.string_set whitelist in
      
      rpc_wrapper rpc "DR_task.create" [ session_id; _type; device_config; whitelist ] >>= fun x -> return (From.ref_DR_task "return value of DR_task.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_DR_task self in
      
      rpc_wrapper rpc "DR_task.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "DR_task.get_all" [ session_id ] >>= fun x -> return (From.ref_DR_task_set "return value of DR_task.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "DR_task.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_DR_task_to_dR_task_t_map "return value of DR_task.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "DR_task.get_all_records" [ session_id ] >>= fun x -> return (From.ref_DR_task_to_dR_task_t_map "return value of DR_task.get_all_records"  x)
  end
  module Host = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_record" [ session_id; self ] >>= fun x -> return (From.host_t "return value of host.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "host.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_host "return value of host.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "host.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_host_set "return value of host.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_name_description"  x)
    (**  *)
    let get_memory_overhead ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_memory_overhead" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host.get_memory_overhead"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.host_allowed_operations_set "return value of host.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_host_allowed_operations_map "return value of host.get_current_operations"  x)
    (**  *)
    let get_API_version_major ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_API_version_major" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host.get_API_version_major"  x)
    (**  *)
    let get_API_version_minor ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_API_version_minor" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host.get_API_version_minor"  x)
    (**  *)
    let get_API_version_vendor ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_API_version_vendor" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_API_version_vendor"  x)
    (**  *)
    let get_API_version_vendor_implementation ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_API_version_vendor_implementation" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_API_version_vendor_implementation"  x)
    (**  *)
    let get_enabled ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_enabled" [ session_id; self ] >>= fun x -> return (From.bool "return value of host.get_enabled"  x)
    (**  *)
    let get_software_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_software_version" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_software_version"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_other_config"  x)
    (**  *)
    let get_capabilities ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_capabilities" [ session_id; self ] >>= fun x -> return (From.string_set "return value of host.get_capabilities"  x)
    (**  *)
    let get_cpu_configuration ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_cpu_configuration" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_cpu_configuration"  x)
    (**  *)
    let get_sched_policy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_sched_policy" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_sched_policy"  x)
    (**  *)
    let get_supported_bootloaders ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_supported_bootloaders" [ session_id; self ] >>= fun x -> return (From.string_set "return value of host.get_supported_bootloaders"  x)
    (**  *)
    let get_resident_VMs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_resident_VMs" [ session_id; self ] >>= fun x -> return (From.ref_VM_set "return value of host.get_resident_VMs"  x)
    (**  *)
    let get_logging ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_logging" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_logging"  x)
    (**  *)
    let get_PIFs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_PIFs" [ session_id; self ] >>= fun x -> return (From.ref_PIF_set "return value of host.get_PIFs"  x)
    (**  *)
    let get_suspend_image_sr ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_suspend_image_sr" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of host.get_suspend_image_sr"  x)
    (**  *)
    let get_crash_dump_sr ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_crash_dump_sr" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of host.get_crash_dump_sr"  x)
    (**  *)
    let get_crashdumps ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_crashdumps" [ session_id; self ] >>= fun x -> return (From.ref_host_crashdump_set "return value of host.get_crashdumps"  x)
    (**  *)
    let get_patches ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_patches" [ session_id; self ] >>= fun x -> return (From.ref_host_patch_set "return value of host.get_patches"  x)
    (**  *)
    let get_PBDs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_PBDs" [ session_id; self ] >>= fun x -> return (From.ref_PBD_set "return value of host.get_PBDs"  x)
    (**  *)
    let get_host_CPUs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_host_CPUs" [ session_id; self ] >>= fun x -> return (From.ref_host_cpu_set "return value of host.get_host_CPUs"  x)
    (**  *)
    let get_cpu_info ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_cpu_info" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_cpu_info"  x)
    (**  *)
    let get_hostname ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_hostname" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_hostname"  x)
    (**  *)
    let get_address ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_address" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_address"  x)
    (**  *)
    let get_metrics ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_metrics" [ session_id; self ] >>= fun x -> return (From.ref_host_metrics "return value of host.get_metrics"  x)
    (**  *)
    let get_license_params ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_license_params" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_license_params"  x)
    (**  *)
    let get_ha_statefiles ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_ha_statefiles" [ session_id; self ] >>= fun x -> return (From.string_set "return value of host.get_ha_statefiles"  x)
    (**  *)
    let get_ha_network_peers ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_ha_network_peers" [ session_id; self ] >>= fun x -> return (From.string_set "return value of host.get_ha_network_peers"  x)
    (**  *)
    let get_blobs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_blobs" [ session_id; self ] >>= fun x -> return (From.string_to_ref_blob_map "return value of host.get_blobs"  x)
    (**  *)
    let get_tags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_tags" [ session_id; self ] >>= fun x -> return (From.string_set "return value of host.get_tags"  x)
    (**  *)
    let get_external_auth_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_external_auth_type" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_external_auth_type"  x)
    (**  *)
    let get_external_auth_service_name ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_external_auth_service_name" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_external_auth_service_name"  x)
    (**  *)
    let get_external_auth_configuration ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_external_auth_configuration" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_external_auth_configuration"  x)
    (**  *)
    let get_edition ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_edition" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_edition"  x)
    (**  *)
    let get_license_server ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_license_server" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_license_server"  x)
    (**  *)
    let get_bios_strings ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_bios_strings" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_bios_strings"  x)
    (**  *)
    let get_power_on_mode ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_power_on_mode" [ session_id; self ] >>= fun x -> return (From.string "return value of host.get_power_on_mode"  x)
    (**  *)
    let get_power_on_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_power_on_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_power_on_config"  x)
    (**  *)
    let get_local_cache_sr ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_local_cache_sr" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of host.get_local_cache_sr"  x)
    (**  *)
    let get_chipset_info ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_chipset_info" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host.get_chipset_info"  x)
    (**  *)
    let get_PCIs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_PCIs" [ session_id; self ] >>= fun x -> return (From.ref_PCI_set "return value of host.get_PCIs"  x)
    (**  *)
    let get_PGPUs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_PGPUs" [ session_id; self ] >>= fun x -> return (From.ref_PGPU_set "return value of host.get_PGPUs"  x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string value in
      
      rpc_wrapper rpc "host.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string value in
      
      rpc_wrapper rpc "host.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "host.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "host.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let key = To.string key in
      
      rpc_wrapper rpc "host.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_logging ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "host.set_logging" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_logging ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "host.add_to_logging" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_logging ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let key = To.string key in
      
      rpc_wrapper rpc "host.remove_from_logging" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_suspend_image_sr ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.ref_SR value in
      
      rpc_wrapper rpc "host.set_suspend_image_sr" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_crash_dump_sr ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.ref_SR value in
      
      rpc_wrapper rpc "host.set_crash_dump_sr" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_hostname ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string value in
      
      rpc_wrapper rpc "host.set_hostname" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_address ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string value in
      
      rpc_wrapper rpc "host.set_address" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string_set value in
      
      rpc_wrapper rpc "host.set_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string value in
      
      rpc_wrapper rpc "host.add_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string value in
      
      rpc_wrapper rpc "host.remove_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_license_server ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "host.set_license_server" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_license_server ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "host.add_to_license_server" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_license_server ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let key = To.string key in
      
      rpc_wrapper rpc "host.remove_from_license_server" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let disable ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.disable" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let enable ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.enable" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let shutdown ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.shutdown" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let reboot ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.reboot" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let dmesg ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.dmesg" [ session_id; host ] >>= fun x -> return (From.string "return value of host.dmesg"  x)
    (**  *)
    let dmesg_clear ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.dmesg_clear" [ session_id; host ] >>= fun x -> return (From.string "return value of host.dmesg_clear"  x)
    (**  *)
    let get_log ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_log" [ session_id; host ] >>= fun x -> return (From.string "return value of host.get_log"  x)
    (**  *)
    let send_debug_keys ~rpc ~session_id ~host ~keys =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let keys = To.string keys in
      
      rpc_wrapper rpc "host.send_debug_keys" [ session_id; host; keys ] >>= fun x -> return (ignore x)
    (**  *)
    let bugreport_upload ~rpc ~session_id ~host ~url ~options =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let url = To.string url in
      let options = To.string_to_string_map options in
      
      rpc_wrapper rpc "host.bugreport_upload" [ session_id; host; url; options ] >>= fun x -> return (ignore x)
    (**  *)
    let list_methods ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.list_methods" [ session_id ] >>= fun x -> return (From.string_set "return value of host.list_methods"  x)
    (**  *)
    let license_apply ~rpc ~session_id ~host ~contents =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let contents = To.string contents in
      
      rpc_wrapper rpc "host.license_apply" [ session_id; host; contents ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~uuid ~name_label ~name_description ~hostname ~address ~external_auth_type ~external_auth_service_name ~external_auth_configuration ~license_params ~edition ~license_server ~local_cache_sr ~chipset_info =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let hostname = To.string hostname in
      let address = To.string address in
      let external_auth_type = To.string external_auth_type in
      let external_auth_service_name = To.string external_auth_service_name in
      let external_auth_configuration = To.string_to_string_map external_auth_configuration in
      let license_params = To.string_to_string_map license_params in
      let edition = To.string edition in
      let license_server = To.string_to_string_map license_server in
      let local_cache_sr = To.ref_SR local_cache_sr in
      let chipset_info = To.string_to_string_map chipset_info in
      
      rpc_wrapper rpc "host.create" [ session_id; uuid; name_label; name_description; hostname; address; external_auth_type; external_auth_service_name; external_auth_configuration; license_params; edition; license_server; local_cache_sr; chipset_info ] >>= fun x -> return (From.ref_host "return value of host.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let power_on ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.power_on" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let set_license_params ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "host.set_license_params" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let emergency_ha_disable ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.emergency_ha_disable" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_disarm_fencing ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.ha_disarm_fencing" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let preconfigure_ha ~rpc ~session_id ~host ~statefiles ~metadata_vdi ~generation =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let statefiles = To.ref_VDI_set statefiles in
      let metadata_vdi = To.ref_VDI metadata_vdi in
      let generation = To.string generation in
      
      rpc_wrapper rpc "host.preconfigure_ha" [ session_id; host; statefiles; metadata_vdi; generation ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_join_liveset ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.ha_join_liveset" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_disable_failover_decisions ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.ha_disable_failover_decisions" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_wait_for_shutdown_via_statefile ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.ha_wait_for_shutdown_via_statefile" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_stop_daemon ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.ha_stop_daemon" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_release_resources ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.ha_release_resources" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let ha_xapi_healthcheck ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.ha_xapi_healthcheck" [ session_id ] >>= fun x -> return (From.bool "return value of host.ha_xapi_healthcheck"  x)
    (**  *)
    let local_assert_healthy ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.local_assert_healthy" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let request_backup ~rpc ~session_id ~host ~generation ~force =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let generation = To.int64 generation in
      let force = To.bool force in
      
      rpc_wrapper rpc "host.request_backup" [ session_id; host; generation; force ] >>= fun x -> return (ignore x)
    (**  *)
    let request_config_file_sync ~rpc ~session_id ~host ~hash =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let hash = To.string hash in
      
      rpc_wrapper rpc "host.request_config_file_sync" [ session_id; host; hash ] >>= fun x -> return (ignore x)
    (**  *)
    let propose_new_master ~rpc ~session_id ~address ~manual =
      let session_id = To.ref_session session_id in
      let address = To.string address in
      let manual = To.bool manual in
      
      rpc_wrapper rpc "host.propose_new_master" [ session_id; address; manual ] >>= fun x -> return (ignore x)
    (**  *)
    let commit_new_master ~rpc ~session_id ~address =
      let session_id = To.ref_session session_id in
      let address = To.string address in
      
      rpc_wrapper rpc "host.commit_new_master" [ session_id; address ] >>= fun x -> return (ignore x)
    (**  *)
    let abort_new_master ~rpc ~session_id ~address =
      let session_id = To.ref_session session_id in
      let address = To.string address in
      
      rpc_wrapper rpc "host.abort_new_master" [ session_id; address ] >>= fun x -> return (ignore x)
    (**  *)
    let get_data_sources ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_data_sources" [ session_id; host ] >>= fun x -> return (From.data_source_t_set "return value of host.get_data_sources"  x)
    (**  *)
    let record_data_source ~rpc ~session_id ~host ~data_source =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let data_source = To.string data_source in
      
      rpc_wrapper rpc "host.record_data_source" [ session_id; host; data_source ] >>= fun x -> return (ignore x)
    (**  *)
    let query_data_source ~rpc ~session_id ~host ~data_source =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let data_source = To.string data_source in
      
      rpc_wrapper rpc "host.query_data_source" [ session_id; host; data_source ] >>= fun x -> return (From.float "return value of host.query_data_source"  x)
    (**  *)
    let forget_data_source_archives ~rpc ~session_id ~host ~data_source =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let data_source = To.string data_source in
      
      rpc_wrapper rpc "host.forget_data_source_archives" [ session_id; host; data_source ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_can_evacuate ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.assert_can_evacuate" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let get_vms_which_prevent_evacuation ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_vms_which_prevent_evacuation" [ session_id; self ] >>= fun x -> return (From.ref_VM_to_string_set_map "return value of host.get_vms_which_prevent_evacuation"  x)
    (**  *)
    let get_uncooperative_resident_VMs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_uncooperative_resident_VMs" [ session_id; self ] >>= fun x -> return (From.ref_VM_set "return value of host.get_uncooperative_resident_VMs"  x)
    (**  *)
    let get_uncooperative_domains ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.get_uncooperative_domains" [ session_id; self ] >>= fun x -> return (From.string_set "return value of host.get_uncooperative_domains"  x)
    (**  *)
    let evacuate ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.evacuate" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let signal_networking_change ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.signal_networking_change" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let notify ~rpc ~session_id ~ty ~params =
      let session_id = To.ref_session session_id in
      let ty = To.string ty in
      let params = To.string params in
      
      rpc_wrapper rpc "host.notify" [ session_id; ty; params ] >>= fun x -> return (ignore x)
    (**  *)
    let syslog_reconfigure ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.syslog_reconfigure" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let management_reconfigure ~rpc ~session_id ~pif =
      let session_id = To.ref_session session_id in
      let pif = To.ref_PIF pif in
      
      rpc_wrapper rpc "host.management_reconfigure" [ session_id; pif ] >>= fun x -> return (ignore x)
    (**  *)
    let local_management_reconfigure ~rpc ~session_id ~interface =
      let session_id = To.ref_session session_id in
      let interface = To.string interface in
      
      rpc_wrapper rpc "host.local_management_reconfigure" [ session_id; interface ] >>= fun x -> return (ignore x)
    (**  *)
    let management_disable ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.management_disable" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let get_management_interface ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_management_interface" [ session_id; host ] >>= fun x -> return (From.ref_PIF "return value of host.get_management_interface"  x)
    (**  *)
    let get_system_status_capabilities ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_system_status_capabilities" [ session_id; host ] >>= fun x -> return (From.string "return value of host.get_system_status_capabilities"  x)
    (**  *)
    let get_diagnostic_timing_stats ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_diagnostic_timing_stats" [ session_id; host ] >>= fun x -> return (From.string_to_string_map "return value of host.get_diagnostic_timing_stats"  x)
    (**  *)
    let restart_agent ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.restart_agent" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let shutdown_agent ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.shutdown_agent" [ session_id ] >>= fun x -> return (ignore x)
    (**  *)
    let set_hostname_live ~rpc ~session_id ~host ~hostname =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let hostname = To.string hostname in
      
      rpc_wrapper rpc "host.set_hostname_live" [ session_id; host; hostname ] >>= fun x -> return (ignore x)
    (**  *)
    let is_in_emergency_mode ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.is_in_emergency_mode" [ session_id ] >>= fun x -> return (From.bool "return value of host.is_in_emergency_mode"  x)
    (**  *)
    let compute_free_memory ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.compute_free_memory" [ session_id; host ] >>= fun x -> return (From.int64 "return value of host.compute_free_memory"  x)
    (**  *)
    let compute_memory_overhead ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.compute_memory_overhead" [ session_id; host ] >>= fun x -> return (From.int64 "return value of host.compute_memory_overhead"  x)
    (**  *)
    let tickle_heartbeat ~rpc ~session_id ~host ~stuff =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let stuff = To.string_to_string_map stuff in
      
      rpc_wrapper rpc "host.tickle_heartbeat" [ session_id; host; stuff ] >>= fun x -> return (From.string_to_string_map "return value of host.tickle_heartbeat"  x)
    (**  *)
    let sync_data ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.sync_data" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let backup_rrds ~rpc ~session_id ~host ~delay =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let delay = To.float delay in
      
      rpc_wrapper rpc "host.backup_rrds" [ session_id; host; delay ] >>= fun x -> return (ignore x)
    (**  *)
    let create_new_blob ~rpc ~session_id ~host ~name ~mime_type ~public =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let name = To.string name in
      let mime_type = To.string mime_type in
      let public = To.bool public in
      
      rpc_wrapper rpc "host.create_new_blob" [ session_id; host; name; mime_type; public ] >>= fun x -> return (From.ref_blob "return value of host.create_new_blob"  x)
    (**  *)
    let call_plugin ~rpc ~session_id ~host ~plugin ~fn ~args =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let plugin = To.string plugin in
      let fn = To.string fn in
      let args = To.string_to_string_map args in
      
      rpc_wrapper rpc "host.call_plugin" [ session_id; host; plugin; fn; args ] >>= fun x -> return (From.string "return value of host.call_plugin"  x)
    (**  *)
    let get_servertime ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_servertime" [ session_id; host ] >>= fun x -> return (From.datetime "return value of host.get_servertime"  x)
    (**  *)
    let get_server_localtime ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_server_localtime" [ session_id; host ] >>= fun x -> return (From.datetime "return value of host.get_server_localtime"  x)
    (**  *)
    let enable_binary_storage ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.enable_binary_storage" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_binary_storage ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.disable_binary_storage" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let enable_external_auth ~rpc ~session_id ~host ~config ~service_name ~auth_type =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let config = To.string_to_string_map config in
      let service_name = To.string service_name in
      let auth_type = To.string auth_type in
      
      rpc_wrapper rpc "host.enable_external_auth" [ session_id; host; config; service_name; auth_type ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_external_auth ~rpc ~session_id ~host ~config =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let config = To.string_to_string_map config in
      
      rpc_wrapper rpc "host.disable_external_auth" [ session_id; host; config ] >>= fun x -> return (ignore x)
    (**  *)
    let retrieve_wlb_evacuate_recommendations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      
      rpc_wrapper rpc "host.retrieve_wlb_evacuate_recommendations" [ session_id; self ] >>= fun x -> return (From.ref_VM_to_string_set_map "return value of host.retrieve_wlb_evacuate_recommendations"  x)
    (**  *)
    let certificate_install ~rpc ~session_id ~host ~name ~cert =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let name = To.string name in
      let cert = To.string cert in
      
      rpc_wrapper rpc "host.certificate_install" [ session_id; host; name; cert ] >>= fun x -> return (ignore x)
    (**  *)
    let certificate_uninstall ~rpc ~session_id ~host ~name =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let name = To.string name in
      
      rpc_wrapper rpc "host.certificate_uninstall" [ session_id; host; name ] >>= fun x -> return (ignore x)
    (**  *)
    let certificate_list ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.certificate_list" [ session_id; host ] >>= fun x -> return (From.string_set "return value of host.certificate_list"  x)
    (**  *)
    let crl_install ~rpc ~session_id ~host ~name ~crl =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let name = To.string name in
      let crl = To.string crl in
      
      rpc_wrapper rpc "host.crl_install" [ session_id; host; name; crl ] >>= fun x -> return (ignore x)
    (**  *)
    let crl_uninstall ~rpc ~session_id ~host ~name =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let name = To.string name in
      
      rpc_wrapper rpc "host.crl_uninstall" [ session_id; host; name ] >>= fun x -> return (ignore x)
    (**  *)
    let crl_list ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.crl_list" [ session_id; host ] >>= fun x -> return (From.string_set "return value of host.crl_list"  x)
    (**  *)
    let certificate_sync ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.certificate_sync" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let get_server_certificate ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_server_certificate" [ session_id; host ] >>= fun x -> return (From.string "return value of host.get_server_certificate"  x)
    (**  *)
    let update_pool_secret ~rpc ~session_id ~host ~pool_secret =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let pool_secret = To.string pool_secret in
      
      rpc_wrapper rpc "host.update_pool_secret" [ session_id; host; pool_secret ] >>= fun x -> return (ignore x)
    (**  *)
    let update_master ~rpc ~session_id ~host ~master_address =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let master_address = To.string master_address in
      
      rpc_wrapper rpc "host.update_master" [ session_id; host; master_address ] >>= fun x -> return (ignore x)
    (**  *)
    let attach_static_vdis ~rpc ~session_id ~host ~vdi_reason_map =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let vdi_reason_map = To.ref_VDI_to_string_map vdi_reason_map in
      
      rpc_wrapper rpc "host.attach_static_vdis" [ session_id; host; vdi_reason_map ] >>= fun x -> return (ignore x)
    (**  *)
    let detach_static_vdis ~rpc ~session_id ~host ~vdis =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let vdis = To.ref_VDI_set vdis in
      
      rpc_wrapper rpc "host.detach_static_vdis" [ session_id; host; vdis ] >>= fun x -> return (ignore x)
    (**  *)
    let set_localdb_key ~rpc ~session_id ~host ~key ~value =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "host.set_localdb_key" [ session_id; host; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let apply_edition ~rpc ~session_id ~host ~edition =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let edition = To.string edition in
      
      rpc_wrapper rpc "host.apply_edition" [ session_id; host; edition ] >>= fun x -> return (ignore x)
    (**  *)
    let refresh_pack_info ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.refresh_pack_info" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let set_power_on_mode ~rpc ~session_id ~self ~power_on_mode ~power_on_config =
      let session_id = To.ref_session session_id in
      let self = To.ref_host self in
      let power_on_mode = To.string power_on_mode in
      let power_on_config = To.string_to_string_map power_on_config in
      
      rpc_wrapper rpc "host.set_power_on_mode" [ session_id; self; power_on_mode; power_on_config ] >>= fun x -> return (ignore x)
    (**  *)
    let set_cpu_features ~rpc ~session_id ~host ~features =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let features = To.string features in
      
      rpc_wrapper rpc "host.set_cpu_features" [ session_id; host; features ] >>= fun x -> return (ignore x)
    (**  *)
    let reset_cpu_features ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.reset_cpu_features" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let reset_networking ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.reset_networking" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let enable_local_storage_caching ~rpc ~session_id ~host ~sr =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "host.enable_local_storage_caching" [ session_id; host; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_local_storage_caching ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.disable_local_storage_caching" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let get_sm_diagnostics ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_sm_diagnostics" [ session_id; host ] >>= fun x -> return (From.string "return value of host.get_sm_diagnostics"  x)
    (**  *)
    let get_thread_diagnostics ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.get_thread_diagnostics" [ session_id; host ] >>= fun x -> return (From.string "return value of host.get_thread_diagnostics"  x)
    (**  *)
    let sm_dp_destroy ~rpc ~session_id ~host ~dp ~allow_leak =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let dp = To.string dp in
      let allow_leak = To.bool allow_leak in
      
      rpc_wrapper rpc "host.sm_dp_destroy" [ session_id; host; dp; allow_leak ] >>= fun x -> return (ignore x)
    (**  *)
    let sync_vlans ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.sync_vlans" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let sync_tunnels ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "host.sync_tunnels" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let sync_pif_currently_attached ~rpc ~session_id ~host ~bridges =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let bridges = To.string_set bridges in
      
      rpc_wrapper rpc "host.sync_pif_currently_attached" [ session_id; host; bridges ] >>= fun x -> return (ignore x)
    (**  *)
    let migrate_receive ~rpc ~session_id ~host ~network ~options =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let network = To.ref_network network in
      let options = To.string_to_string_map options in
      
      rpc_wrapper rpc "host.migrate_receive" [ session_id; host; network; options ] >>= fun x -> return (From.string_to_string_map "return value of host.migrate_receive"  x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.get_all" [ session_id ] >>= fun x -> return (From.ref_host_set "return value of host.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "host.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_host_to_host_t_map "return value of host.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host.get_all_records" [ session_id ] >>= fun x -> return (From.ref_host_to_host_t_map "return value of host.get_all_records"  x)
  end
  module Host_crashdump = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      
      rpc_wrapper rpc "host_crashdump.get_record" [ session_id; self ] >>= fun x -> return (From.host_crashdump_t "return value of host_crashdump.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "host_crashdump.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_host_crashdump "return value of host_crashdump.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      
      rpc_wrapper rpc "host_crashdump.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of host_crashdump.get_uuid"  x)
    (**  *)
    let get_host ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      
      rpc_wrapper rpc "host_crashdump.get_host" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of host_crashdump.get_host"  x)
    (**  *)
    let get_timestamp ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      
      rpc_wrapper rpc "host_crashdump.get_timestamp" [ session_id; self ] >>= fun x -> return (From.datetime "return value of host_crashdump.get_timestamp"  x)
    (**  *)
    let get_size ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      
      rpc_wrapper rpc "host_crashdump.get_size" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host_crashdump.get_size"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      
      rpc_wrapper rpc "host_crashdump.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host_crashdump.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "host_crashdump.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "host_crashdump.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      let key = To.string key in
      
      rpc_wrapper rpc "host_crashdump.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      
      rpc_wrapper rpc "host_crashdump.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let upload ~rpc ~session_id ~self ~url ~options =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_crashdump self in
      let url = To.string url in
      let options = To.string_to_string_map options in
      
      rpc_wrapper rpc "host_crashdump.upload" [ session_id; self; url; options ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host_crashdump.get_all" [ session_id ] >>= fun x -> return (From.ref_host_crashdump_set "return value of host_crashdump.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "host_crashdump.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_host_crashdump_to_host_crashdump_t_map "return value of host_crashdump.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host_crashdump.get_all_records" [ session_id ] >>= fun x -> return (From.ref_host_crashdump_to_host_crashdump_t_map "return value of host_crashdump.get_all_records"  x)
  end
  module Host_patch = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_record" [ session_id; self ] >>= fun x -> return (From.host_patch_t "return value of host_patch.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "host_patch.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_host_patch "return value of host_patch.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "host_patch.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_host_patch_set "return value of host_patch.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of host_patch.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of host_patch.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of host_patch.get_name_description"  x)
    (**  *)
    let get_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_version" [ session_id; self ] >>= fun x -> return (From.string "return value of host_patch.get_version"  x)
    (**  *)
    let get_host ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_host" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of host_patch.get_host"  x)
    (**  *)
    let get_applied ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_applied" [ session_id; self ] >>= fun x -> return (From.bool "return value of host_patch.get_applied"  x)
    (**  *)
    let get_timestamp_applied ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_timestamp_applied" [ session_id; self ] >>= fun x -> return (From.datetime "return value of host_patch.get_timestamp_applied"  x)
    (**  *)
    let get_size ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_size" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host_patch.get_size"  x)
    (**  *)
    let get_pool_patch ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_pool_patch" [ session_id; self ] >>= fun x -> return (From.ref_pool_patch "return value of host_patch.get_pool_patch"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host_patch.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "host_patch.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "host_patch.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      let key = To.string key in
      
      rpc_wrapper rpc "host_patch.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let apply ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_patch self in
      
      rpc_wrapper rpc "host_patch.apply" [ session_id; self ] >>= fun x -> return (From.string "return value of host_patch.apply"  x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host_patch.get_all" [ session_id ] >>= fun x -> return (From.ref_host_patch_set "return value of host_patch.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "host_patch.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_host_patch_to_host_patch_t_map "return value of host_patch.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host_patch.get_all_records" [ session_id ] >>= fun x -> return (From.ref_host_patch_to_host_patch_t_map "return value of host_patch.get_all_records"  x)
  end
  module Host_metrics = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      
      rpc_wrapper rpc "host_metrics.get_record" [ session_id; self ] >>= fun x -> return (From.host_metrics_t "return value of host_metrics.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "host_metrics.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_host_metrics "return value of host_metrics.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      
      rpc_wrapper rpc "host_metrics.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of host_metrics.get_uuid"  x)
    (**  *)
    let get_memory_total ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      
      rpc_wrapper rpc "host_metrics.get_memory_total" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host_metrics.get_memory_total"  x)
    (**  *)
    let get_memory_free ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      
      rpc_wrapper rpc "host_metrics.get_memory_free" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host_metrics.get_memory_free"  x)
    (**  *)
    let get_live ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      
      rpc_wrapper rpc "host_metrics.get_live" [ session_id; self ] >>= fun x -> return (From.bool "return value of host_metrics.get_live"  x)
    (**  *)
    let get_last_updated ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      
      rpc_wrapper rpc "host_metrics.get_last_updated" [ session_id; self ] >>= fun x -> return (From.datetime "return value of host_metrics.get_last_updated"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      
      rpc_wrapper rpc "host_metrics.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host_metrics.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "host_metrics.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "host_metrics.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_metrics self in
      let key = To.string key in
      
      rpc_wrapper rpc "host_metrics.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host_metrics.get_all" [ session_id ] >>= fun x -> return (From.ref_host_metrics_set "return value of host_metrics.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "host_metrics.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_host_metrics_to_host_metrics_t_map "return value of host_metrics.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host_metrics.get_all_records" [ session_id ] >>= fun x -> return (From.ref_host_metrics_to_host_metrics_t_map "return value of host_metrics.get_all_records"  x)
  end
  module Host_cpu = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_record" [ session_id; self ] >>= fun x -> return (From.host_cpu_t "return value of host_cpu.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "host_cpu.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_host_cpu "return value of host_cpu.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of host_cpu.get_uuid"  x)
    (**  *)
    let get_host ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_host" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of host_cpu.get_host"  x)
    (**  *)
    let get_number ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_number" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host_cpu.get_number"  x)
    (**  *)
    let get_vendor ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_vendor" [ session_id; self ] >>= fun x -> return (From.string "return value of host_cpu.get_vendor"  x)
    (**  *)
    let get_speed ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_speed" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host_cpu.get_speed"  x)
    (**  *)
    let get_modelname ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_modelname" [ session_id; self ] >>= fun x -> return (From.string "return value of host_cpu.get_modelname"  x)
    (**  *)
    let get_family ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_family" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host_cpu.get_family"  x)
    (**  *)
    let get_model ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_model" [ session_id; self ] >>= fun x -> return (From.int64 "return value of host_cpu.get_model"  x)
    (**  *)
    let get_stepping ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_stepping" [ session_id; self ] >>= fun x -> return (From.string "return value of host_cpu.get_stepping"  x)
    (**  *)
    let get_flags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_flags" [ session_id; self ] >>= fun x -> return (From.string "return value of host_cpu.get_flags"  x)
    (**  *)
    let get_features ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_features" [ session_id; self ] >>= fun x -> return (From.string "return value of host_cpu.get_features"  x)
    (**  *)
    let get_utilisation ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_utilisation" [ session_id; self ] >>= fun x -> return (From.float "return value of host_cpu.get_utilisation"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      
      rpc_wrapper rpc "host_cpu.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of host_cpu.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "host_cpu.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "host_cpu.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_host_cpu self in
      let key = To.string key in
      
      rpc_wrapper rpc "host_cpu.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host_cpu.get_all" [ session_id ] >>= fun x -> return (From.ref_host_cpu_set "return value of host_cpu.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "host_cpu.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_host_cpu_to_host_cpu_t_map "return value of host_cpu.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "host_cpu.get_all_records" [ session_id ] >>= fun x -> return (From.ref_host_cpu_to_host_cpu_t_map "return value of host_cpu.get_all_records"  x)
  end
  module Network = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_record" [ session_id; self ] >>= fun x -> return (From.network_t "return value of network.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "network.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_network "return value of network.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~name_label ~name_description ~mTU ~other_config ~tags =
      let session_id = To.ref_session session_id in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let mTU = To.int64 mTU in
      let other_config = To.string_to_string_map other_config in
      let tags = To.string_set tags in
      let __structure = To.structure [ "name_label", name_label; "name_description", name_description; "MTU", mTU; "other_config", other_config; "tags", tags] in
      rpc_wrapper rpc "network.create" [ session_id; __structure ] >>= fun x -> return (From.ref_network "return value of network.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "network.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_network_set "return value of network.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of network.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of network.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of network.get_name_description"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.network_operations_set "return value of network.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_network_operations_map "return value of network.get_current_operations"  x)
    (**  *)
    let get_VIFs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_VIFs" [ session_id; self ] >>= fun x -> return (From.ref_VIF_set "return value of network.get_VIFs"  x)
    (**  *)
    let get_PIFs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_PIFs" [ session_id; self ] >>= fun x -> return (From.ref_PIF_set "return value of network.get_PIFs"  x)
    (**  *)
    let get_MTU ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_MTU" [ session_id; self ] >>= fun x -> return (From.int64 "return value of network.get_MTU"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of network.get_other_config"  x)
    (**  *)
    let get_bridge ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_bridge" [ session_id; self ] >>= fun x -> return (From.string "return value of network.get_bridge"  x)
    (**  *)
    let get_blobs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_blobs" [ session_id; self ] >>= fun x -> return (From.string_to_ref_blob_map "return value of network.get_blobs"  x)
    (**  *)
    let get_tags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_tags" [ session_id; self ] >>= fun x -> return (From.string_set "return value of network.get_tags"  x)
    (**  *)
    let get_default_locking_mode ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      
      rpc_wrapper rpc "network.get_default_locking_mode" [ session_id; self ] >>= fun x -> return (From.network_default_locking_mode "return value of network.get_default_locking_mode"  x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let value = To.string value in
      
      rpc_wrapper rpc "network.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let value = To.string value in
      
      rpc_wrapper rpc "network.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_MTU ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "network.set_MTU" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "network.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "network.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let key = To.string key in
      
      rpc_wrapper rpc "network.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let value = To.string_set value in
      
      rpc_wrapper rpc "network.set_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let value = To.string value in
      
      rpc_wrapper rpc "network.add_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_network self in
      let value = To.string value in
      
      rpc_wrapper rpc "network.remove_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let attach ~rpc ~session_id ~network ~host =
      let session_id = To.ref_session session_id in
      let network = To.ref_network network in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "network.attach" [ session_id; network; host ] >>= fun x -> return (ignore x)
    (**  *)
    let pool_introduce ~rpc ~session_id ~name_label ~name_description ~mTU ~other_config ~bridge =
      let session_id = To.ref_session session_id in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let mTU = To.int64 mTU in
      let other_config = To.string_to_string_map other_config in
      let bridge = To.string bridge in
      
      rpc_wrapper rpc "network.pool_introduce" [ session_id; name_label; name_description; mTU; other_config; bridge ] >>= fun x -> return (From.ref_network "return value of network.pool_introduce"  x)
    (**  *)
    let create_new_blob ~rpc ~session_id ~network ~name ~mime_type ~public =
      let session_id = To.ref_session session_id in
      let network = To.ref_network network in
      let name = To.string name in
      let mime_type = To.string mime_type in
      let public = To.bool public in
      
      rpc_wrapper rpc "network.create_new_blob" [ session_id; network; name; mime_type; public ] >>= fun x -> return (From.ref_blob "return value of network.create_new_blob"  x)
    (**  *)
    let set_default_locking_mode ~rpc ~session_id ~network ~value =
      let session_id = To.ref_session session_id in
      let network = To.ref_network network in
      let value = To.network_default_locking_mode value in
      
      rpc_wrapper rpc "network.set_default_locking_mode" [ session_id; network; value ] >>= fun x -> return (ignore x)
    (**  *)
    let attach_for_vm ~rpc ~session_id ~host ~vm =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "network.attach_for_vm" [ session_id; host; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let detach_for_vm ~rpc ~session_id ~host ~vm =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let vm = To.ref_VM vm in
      
      rpc_wrapper rpc "network.detach_for_vm" [ session_id; host; vm ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "network.get_all" [ session_id ] >>= fun x -> return (From.ref_network_set "return value of network.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "network.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_network_to_network_t_map "return value of network.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "network.get_all_records" [ session_id ] >>= fun x -> return (From.ref_network_to_network_t_map "return value of network.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~name_label:value.network_name_label
      ~name_description:value.network_name_description
      ~mTU:value.network_MTU
      ~other_config:value.network_other_config
      ~tags:value.network_tags
  end
  module VIF = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_record" [ session_id; self ] >>= fun x -> return (From.vIF_t "return value of VIF.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VIF.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VIF "return value of VIF.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~device ~network ~vM ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params =
      let session_id = To.ref_session session_id in
      let device = To.string device in
      let network = To.ref_network network in
      let vM = To.ref_VM vM in
      let mAC = To.string mAC in
      let mTU = To.int64 mTU in
      let other_config = To.string_to_string_map other_config in
      let qos_algorithm_type = To.string qos_algorithm_type in
      let qos_algorithm_params = To.string_to_string_map qos_algorithm_params in
      let __structure = To.structure [ "device", device; "network", network; "VM", vM; "MAC", mAC; "MTU", mTU; "other_config", other_config; "qos_algorithm_type", qos_algorithm_type; "qos_algorithm_params", qos_algorithm_params] in
      rpc_wrapper rpc "VIF.create" [ session_id; __structure ] >>= fun x -> return (From.ref_VIF "return value of VIF.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VIF.get_uuid"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.vif_operations_set "return value of VIF.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_vif_operations_map "return value of VIF.get_current_operations"  x)
    (**  *)
    let get_device ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_device" [ session_id; self ] >>= fun x -> return (From.string "return value of VIF.get_device"  x)
    (**  *)
    let get_network ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_network" [ session_id; self ] >>= fun x -> return (From.ref_network "return value of VIF.get_network"  x)
    (**  *)
    let get_VM ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_VM" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of VIF.get_VM"  x)
    (**  *)
    let get_MAC ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_MAC" [ session_id; self ] >>= fun x -> return (From.string "return value of VIF.get_MAC"  x)
    (**  *)
    let get_MTU ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_MTU" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VIF.get_MTU"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VIF.get_other_config"  x)
    (**  *)
    let get_currently_attached ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_currently_attached" [ session_id; self ] >>= fun x -> return (From.bool "return value of VIF.get_currently_attached"  x)
    (**  *)
    let get_status_code ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_status_code" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VIF.get_status_code"  x)
    (**  *)
    let get_status_detail ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_status_detail" [ session_id; self ] >>= fun x -> return (From.string "return value of VIF.get_status_detail"  x)
    (**  *)
    let get_runtime_properties ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_runtime_properties" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VIF.get_runtime_properties"  x)
    (**  *)
    let get_qos_algorithm_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_qos_algorithm_type" [ session_id; self ] >>= fun x -> return (From.string "return value of VIF.get_qos_algorithm_type"  x)
    (**  *)
    let get_qos_algorithm_params ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_qos_algorithm_params" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VIF.get_qos_algorithm_params"  x)
    (**  *)
    let get_qos_supported_algorithms ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_qos_supported_algorithms" [ session_id; self ] >>= fun x -> return (From.string_set "return value of VIF.get_qos_supported_algorithms"  x)
    (**  *)
    let get_metrics ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_metrics" [ session_id; self ] >>= fun x -> return (From.ref_VIF_metrics "return value of VIF.get_metrics"  x)
    (**  *)
    let get_MAC_autogenerated ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_MAC_autogenerated" [ session_id; self ] >>= fun x -> return (From.bool "return value of VIF.get_MAC_autogenerated"  x)
    (**  *)
    let get_locking_mode ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_locking_mode" [ session_id; self ] >>= fun x -> return (From.vif_locking_mode "return value of VIF.get_locking_mode"  x)
    (**  *)
    let get_ipv4_allowed ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_ipv4_allowed" [ session_id; self ] >>= fun x -> return (From.string_set "return value of VIF.get_ipv4_allowed"  x)
    (**  *)
    let get_ipv6_allowed ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.get_ipv6_allowed" [ session_id; self ] >>= fun x -> return (From.string_set "return value of VIF.get_ipv6_allowed"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VIF.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VIF.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let key = To.string key in
      
      rpc_wrapper rpc "VIF.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_qos_algorithm_type ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string value in
      
      rpc_wrapper rpc "VIF.set_qos_algorithm_type" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_qos_algorithm_params ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VIF.set_qos_algorithm_params" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_qos_algorithm_params ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VIF.add_to_qos_algorithm_params" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_qos_algorithm_params ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let key = To.string key in
      
      rpc_wrapper rpc "VIF.remove_from_qos_algorithm_params" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let plug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.plug" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let unplug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.unplug" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let unplug_force ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      
      rpc_wrapper rpc "VIF.unplug_force" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let set_locking_mode ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.vif_locking_mode value in
      
      rpc_wrapper rpc "VIF.set_locking_mode" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_ipv4_allowed ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string_set value in
      
      rpc_wrapper rpc "VIF.set_ipv4_allowed" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_ipv4_allowed ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string value in
      
      rpc_wrapper rpc "VIF.add_ipv4_allowed" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_ipv4_allowed ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string value in
      
      rpc_wrapper rpc "VIF.remove_ipv4_allowed" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_ipv6_allowed ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string_set value in
      
      rpc_wrapper rpc "VIF.set_ipv6_allowed" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_ipv6_allowed ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string value in
      
      rpc_wrapper rpc "VIF.add_ipv6_allowed" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_ipv6_allowed ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF self in
      let value = To.string value in
      
      rpc_wrapper rpc "VIF.remove_ipv6_allowed" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VIF.get_all" [ session_id ] >>= fun x -> return (From.ref_VIF_set "return value of VIF.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VIF.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VIF_to_vIF_t_map "return value of VIF.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VIF.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VIF_to_vIF_t_map "return value of VIF.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~device:value.vIF_device
      ~network:value.vIF_network
      ~vM:value.vIF_VM
      ~mAC:value.vIF_MAC
      ~mTU:value.vIF_MTU
      ~other_config:value.vIF_other_config
      ~qos_algorithm_type:value.vIF_qos_algorithm_type
      ~qos_algorithm_params:value.vIF_qos_algorithm_params
  end
  module VIF_metrics = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      
      rpc_wrapper rpc "VIF_metrics.get_record" [ session_id; self ] >>= fun x -> return (From.vIF_metrics_t "return value of VIF_metrics.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VIF_metrics.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VIF_metrics "return value of VIF_metrics.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      
      rpc_wrapper rpc "VIF_metrics.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VIF_metrics.get_uuid"  x)
    (**  *)
    let get_io_read_kbs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      
      rpc_wrapper rpc "VIF_metrics.get_io_read_kbs" [ session_id; self ] >>= fun x -> return (From.float "return value of VIF_metrics.get_io_read_kbs"  x)
    (**  *)
    let get_io_write_kbs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      
      rpc_wrapper rpc "VIF_metrics.get_io_write_kbs" [ session_id; self ] >>= fun x -> return (From.float "return value of VIF_metrics.get_io_write_kbs"  x)
    (**  *)
    let get_last_updated ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      
      rpc_wrapper rpc "VIF_metrics.get_last_updated" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VIF_metrics.get_last_updated"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      
      rpc_wrapper rpc "VIF_metrics.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VIF_metrics.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VIF_metrics.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VIF_metrics.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VIF_metrics self in
      let key = To.string key in
      
      rpc_wrapper rpc "VIF_metrics.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VIF_metrics.get_all" [ session_id ] >>= fun x -> return (From.ref_VIF_metrics_set "return value of VIF_metrics.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VIF_metrics.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VIF_metrics_to_vIF_metrics_t_map "return value of VIF_metrics.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VIF_metrics.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VIF_metrics_to_vIF_metrics_t_map "return value of VIF_metrics.get_all_records"  x)
  end
  module PIF = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_record" [ session_id; self ] >>= fun x -> return (From.pIF_t "return value of PIF.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "PIF.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_PIF "return value of PIF.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF.get_uuid"  x)
    (**  *)
    let get_device ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_device" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF.get_device"  x)
    (**  *)
    let get_network ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_network" [ session_id; self ] >>= fun x -> return (From.ref_network "return value of PIF.get_network"  x)
    (**  *)
    let get_host ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_host" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of PIF.get_host"  x)
    (**  *)
    let get_MAC ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_MAC" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF.get_MAC"  x)
    (**  *)
    let get_MTU ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_MTU" [ session_id; self ] >>= fun x -> return (From.int64 "return value of PIF.get_MTU"  x)
    (**  *)
    let get_VLAN ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_VLAN" [ session_id; self ] >>= fun x -> return (From.int64 "return value of PIF.get_VLAN"  x)
    (**  *)
    let get_metrics ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_metrics" [ session_id; self ] >>= fun x -> return (From.ref_PIF_metrics "return value of PIF.get_metrics"  x)
    (**  *)
    let get_physical ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_physical" [ session_id; self ] >>= fun x -> return (From.bool "return value of PIF.get_physical"  x)
    (**  *)
    let get_currently_attached ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_currently_attached" [ session_id; self ] >>= fun x -> return (From.bool "return value of PIF.get_currently_attached"  x)
    (**  *)
    let get_ip_configuration_mode ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_ip_configuration_mode" [ session_id; self ] >>= fun x -> return (From.ip_configuration_mode "return value of PIF.get_ip_configuration_mode"  x)
    (**  *)
    let get_IP ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_IP" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF.get_IP"  x)
    (**  *)
    let get_netmask ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_netmask" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF.get_netmask"  x)
    (**  *)
    let get_gateway ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_gateway" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF.get_gateway"  x)
    (**  *)
    let get_DNS ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_DNS" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF.get_DNS"  x)
    (**  *)
    let get_bond_slave_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_bond_slave_of" [ session_id; self ] >>= fun x -> return (From.ref_Bond "return value of PIF.get_bond_slave_of"  x)
    (**  *)
    let get_bond_master_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_bond_master_of" [ session_id; self ] >>= fun x -> return (From.ref_Bond_set "return value of PIF.get_bond_master_of"  x)
    (**  *)
    let get_VLAN_master_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_VLAN_master_of" [ session_id; self ] >>= fun x -> return (From.ref_VLAN "return value of PIF.get_VLAN_master_of"  x)
    (**  *)
    let get_VLAN_slave_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_VLAN_slave_of" [ session_id; self ] >>= fun x -> return (From.ref_VLAN_set "return value of PIF.get_VLAN_slave_of"  x)
    (**  *)
    let get_management ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_management" [ session_id; self ] >>= fun x -> return (From.bool "return value of PIF.get_management"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of PIF.get_other_config"  x)
    (**  *)
    let get_disallow_unplug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_disallow_unplug" [ session_id; self ] >>= fun x -> return (From.bool "return value of PIF.get_disallow_unplug"  x)
    (**  *)
    let get_tunnel_access_PIF_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_tunnel_access_PIF_of" [ session_id; self ] >>= fun x -> return (From.ref_tunnel_set "return value of PIF.get_tunnel_access_PIF_of"  x)
    (**  *)
    let get_tunnel_transport_PIF_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_tunnel_transport_PIF_of" [ session_id; self ] >>= fun x -> return (From.ref_tunnel_set "return value of PIF.get_tunnel_transport_PIF_of"  x)
    (**  *)
    let get_ipv6_configuration_mode ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_ipv6_configuration_mode" [ session_id; self ] >>= fun x -> return (From.ipv6_configuration_mode "return value of PIF.get_ipv6_configuration_mode"  x)
    (**  *)
    let get_IPv6 ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_IPv6" [ session_id; self ] >>= fun x -> return (From.string_set "return value of PIF.get_IPv6"  x)
    (**  *)
    let get_ipv6_gateway ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_ipv6_gateway" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF.get_ipv6_gateway"  x)
    (**  *)
    let get_primary_address_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.get_primary_address_type" [ session_id; self ] >>= fun x -> return (From.primary_address_type "return value of PIF.get_primary_address_type"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "PIF.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "PIF.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      let key = To.string key in
      
      rpc_wrapper rpc "PIF.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_disallow_unplug ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      let value = To.bool value in
      
      rpc_wrapper rpc "PIF.set_disallow_unplug" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let create_VLAN ~rpc ~session_id ~device ~network ~host ~vLAN =
      let session_id = To.ref_session session_id in
      let device = To.string device in
      let network = To.ref_network network in
      let host = To.ref_host host in
      let vLAN = To.int64 vLAN in
      
      rpc_wrapper rpc "PIF.create_VLAN" [ session_id; device; network; host; vLAN ] >>= fun x -> return (From.ref_PIF "return value of PIF.create_VLAN"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let reconfigure_ip ~rpc ~session_id ~self ~mode ~iP ~netmask ~gateway ~dNS =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      let mode = To.ip_configuration_mode mode in
      let iP = To.string iP in
      let netmask = To.string netmask in
      let gateway = To.string gateway in
      let dNS = To.string dNS in
      
      rpc_wrapper rpc "PIF.reconfigure_ip" [ session_id; self; mode; iP; netmask; gateway; dNS ] >>= fun x -> return (ignore x)
    (**  *)
    let reconfigure_ipv6 ~rpc ~session_id ~self ~mode ~iPv6 ~gateway ~dNS =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      let mode = To.ipv6_configuration_mode mode in
      let iPv6 = To.string iPv6 in
      let gateway = To.string gateway in
      let dNS = To.string dNS in
      
      rpc_wrapper rpc "PIF.reconfigure_ipv6" [ session_id; self; mode; iPv6; gateway; dNS ] >>= fun x -> return (ignore x)
    (**  *)
    let set_primary_address_type ~rpc ~session_id ~self ~primary_address_type =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      let primary_address_type = To.primary_address_type primary_address_type in
      
      rpc_wrapper rpc "PIF.set_primary_address_type" [ session_id; self; primary_address_type ] >>= fun x -> return (ignore x)
    (**  *)
    let scan ~rpc ~session_id ~host =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      
      rpc_wrapper rpc "PIF.scan" [ session_id; host ] >>= fun x -> return (ignore x)
    (**  *)
    let introduce ~rpc ~session_id ~host ~mAC ~device =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let mAC = To.string mAC in
      let device = To.string device in
      
      rpc_wrapper rpc "PIF.introduce" [ session_id; host; mAC; device ] >>= fun x -> return (From.ref_PIF "return value of PIF.introduce"  x)
    (**  *)
    let forget ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.forget" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let unplug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.unplug" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let plug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.plug" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let pool_introduce ~rpc ~session_id ~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode ~iP ~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug ~ipv6_configuration_mode ~iPv6 ~ipv6_gateway ~primary_address_type =
      let session_id = To.ref_session session_id in
      let device = To.string device in
      let network = To.ref_network network in
      let host = To.ref_host host in
      let mAC = To.string mAC in
      let mTU = To.int64 mTU in
      let vLAN = To.int64 vLAN in
      let physical = To.bool physical in
      let ip_configuration_mode = To.ip_configuration_mode ip_configuration_mode in
      let iP = To.string iP in
      let netmask = To.string netmask in
      let gateway = To.string gateway in
      let dNS = To.string dNS in
      let bond_slave_of = To.ref_Bond bond_slave_of in
      let vLAN_master_of = To.ref_VLAN vLAN_master_of in
      let management = To.bool management in
      let other_config = To.string_to_string_map other_config in
      let disallow_unplug = To.bool disallow_unplug in
      let ipv6_configuration_mode = To.ipv6_configuration_mode ipv6_configuration_mode in
      let iPv6 = To.string_set iPv6 in
      let ipv6_gateway = To.string ipv6_gateway in
      let primary_address_type = To.primary_address_type primary_address_type in
      
      rpc_wrapper rpc "PIF.pool_introduce" [ session_id; device; network; host; mAC; mTU; vLAN; physical; ip_configuration_mode; iP; netmask; gateway; dNS; bond_slave_of; vLAN_master_of; management; other_config; disallow_unplug; ipv6_configuration_mode; iPv6; ipv6_gateway; primary_address_type ] >>= fun x -> return (From.ref_PIF "return value of PIF.pool_introduce"  x)
    (**  *)
    let db_introduce ~rpc ~session_id ~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode ~iP ~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug ~ipv6_configuration_mode ~iPv6 ~ipv6_gateway ~primary_address_type =
      let session_id = To.ref_session session_id in
      let device = To.string device in
      let network = To.ref_network network in
      let host = To.ref_host host in
      let mAC = To.string mAC in
      let mTU = To.int64 mTU in
      let vLAN = To.int64 vLAN in
      let physical = To.bool physical in
      let ip_configuration_mode = To.ip_configuration_mode ip_configuration_mode in
      let iP = To.string iP in
      let netmask = To.string netmask in
      let gateway = To.string gateway in
      let dNS = To.string dNS in
      let bond_slave_of = To.ref_Bond bond_slave_of in
      let vLAN_master_of = To.ref_VLAN vLAN_master_of in
      let management = To.bool management in
      let other_config = To.string_to_string_map other_config in
      let disallow_unplug = To.bool disallow_unplug in
      let ipv6_configuration_mode = To.ipv6_configuration_mode ipv6_configuration_mode in
      let iPv6 = To.string_set iPv6 in
      let ipv6_gateway = To.string ipv6_gateway in
      let primary_address_type = To.primary_address_type primary_address_type in
      
      rpc_wrapper rpc "PIF.db_introduce" [ session_id; device; network; host; mAC; mTU; vLAN; physical; ip_configuration_mode; iP; netmask; gateway; dNS; bond_slave_of; vLAN_master_of; management; other_config; disallow_unplug; ipv6_configuration_mode; iPv6; ipv6_gateway; primary_address_type ] >>= fun x -> return (From.ref_PIF "return value of PIF.db_introduce"  x)
    (**  *)
    let db_forget ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF self in
      
      rpc_wrapper rpc "PIF.db_forget" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PIF.get_all" [ session_id ] >>= fun x -> return (From.ref_PIF_set "return value of PIF.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "PIF.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_PIF_to_pIF_t_map "return value of PIF.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PIF.get_all_records" [ session_id ] >>= fun x -> return (From.ref_PIF_to_pIF_t_map "return value of PIF.get_all_records"  x)
  end
  module PIF_metrics = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_record" [ session_id; self ] >>= fun x -> return (From.pIF_metrics_t "return value of PIF_metrics.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "PIF_metrics.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_PIF_metrics "return value of PIF_metrics.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF_metrics.get_uuid"  x)
    (**  *)
    let get_io_read_kbs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_io_read_kbs" [ session_id; self ] >>= fun x -> return (From.float "return value of PIF_metrics.get_io_read_kbs"  x)
    (**  *)
    let get_io_write_kbs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_io_write_kbs" [ session_id; self ] >>= fun x -> return (From.float "return value of PIF_metrics.get_io_write_kbs"  x)
    (**  *)
    let get_carrier ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_carrier" [ session_id; self ] >>= fun x -> return (From.bool "return value of PIF_metrics.get_carrier"  x)
    (**  *)
    let get_vendor_id ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_vendor_id" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF_metrics.get_vendor_id"  x)
    (**  *)
    let get_vendor_name ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_vendor_name" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF_metrics.get_vendor_name"  x)
    (**  *)
    let get_device_id ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_device_id" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF_metrics.get_device_id"  x)
    (**  *)
    let get_device_name ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_device_name" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF_metrics.get_device_name"  x)
    (**  *)
    let get_speed ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_speed" [ session_id; self ] >>= fun x -> return (From.int64 "return value of PIF_metrics.get_speed"  x)
    (**  *)
    let get_duplex ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_duplex" [ session_id; self ] >>= fun x -> return (From.bool "return value of PIF_metrics.get_duplex"  x)
    (**  *)
    let get_pci_bus_path ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_pci_bus_path" [ session_id; self ] >>= fun x -> return (From.string "return value of PIF_metrics.get_pci_bus_path"  x)
    (**  *)
    let get_last_updated ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_last_updated" [ session_id; self ] >>= fun x -> return (From.datetime "return value of PIF_metrics.get_last_updated"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      
      rpc_wrapper rpc "PIF_metrics.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of PIF_metrics.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "PIF_metrics.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "PIF_metrics.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_PIF_metrics self in
      let key = To.string key in
      
      rpc_wrapper rpc "PIF_metrics.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PIF_metrics.get_all" [ session_id ] >>= fun x -> return (From.ref_PIF_metrics_set "return value of PIF_metrics.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "PIF_metrics.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_PIF_metrics_to_pIF_metrics_t_map "return value of PIF_metrics.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PIF_metrics.get_all_records" [ session_id ] >>= fun x -> return (From.ref_PIF_metrics_to_pIF_metrics_t_map "return value of PIF_metrics.get_all_records"  x)
  end
  module Bond = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_record" [ session_id; self ] >>= fun x -> return (From.bond_t "return value of Bond.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "Bond.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_Bond "return value of Bond.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of Bond.get_uuid"  x)
    (**  *)
    let get_master ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_master" [ session_id; self ] >>= fun x -> return (From.ref_PIF "return value of Bond.get_master"  x)
    (**  *)
    let get_slaves ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_slaves" [ session_id; self ] >>= fun x -> return (From.ref_PIF_set "return value of Bond.get_slaves"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of Bond.get_other_config"  x)
    (**  *)
    let get_primary_slave ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_primary_slave" [ session_id; self ] >>= fun x -> return (From.ref_PIF "return value of Bond.get_primary_slave"  x)
    (**  *)
    let get_mode ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_mode" [ session_id; self ] >>= fun x -> return (From.bond_mode "return value of Bond.get_mode"  x)
    (**  *)
    let get_properties ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_properties" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of Bond.get_properties"  x)
    (**  *)
    let get_links_up ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.get_links_up" [ session_id; self ] >>= fun x -> return (From.int64 "return value of Bond.get_links_up"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "Bond.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "Bond.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      let key = To.string key in
      
      rpc_wrapper rpc "Bond.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~network ~members ~mAC ~mode ~properties =
      let session_id = To.ref_session session_id in
      let network = To.ref_network network in
      let members = To.ref_PIF_set members in
      let mAC = To.string mAC in
      let mode = To.bond_mode mode in
      let properties = To.string_to_string_map properties in
      
      rpc_wrapper rpc "Bond.create" [ session_id; network; members; mAC; mode; properties ] >>= fun x -> return (From.ref_Bond "return value of Bond.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      
      rpc_wrapper rpc "Bond.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let set_mode ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      let value = To.bond_mode value in
      
      rpc_wrapper rpc "Bond.set_mode" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_property ~rpc ~session_id ~self ~name ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_Bond self in
      let name = To.string name in
      let value = To.string value in
      
      rpc_wrapper rpc "Bond.set_property" [ session_id; self; name; value ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "Bond.get_all" [ session_id ] >>= fun x -> return (From.ref_Bond_set "return value of Bond.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "Bond.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_Bond_to_bond_t_map "return value of Bond.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "Bond.get_all_records" [ session_id ] >>= fun x -> return (From.ref_Bond_to_bond_t_map "return value of Bond.get_all_records"  x)
  end
  module VLAN = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      
      rpc_wrapper rpc "VLAN.get_record" [ session_id; self ] >>= fun x -> return (From.vLAN_t "return value of VLAN.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VLAN.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VLAN "return value of VLAN.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      
      rpc_wrapper rpc "VLAN.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VLAN.get_uuid"  x)
    (**  *)
    let get_tagged_PIF ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      
      rpc_wrapper rpc "VLAN.get_tagged_PIF" [ session_id; self ] >>= fun x -> return (From.ref_PIF "return value of VLAN.get_tagged_PIF"  x)
    (**  *)
    let get_untagged_PIF ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      
      rpc_wrapper rpc "VLAN.get_untagged_PIF" [ session_id; self ] >>= fun x -> return (From.ref_PIF "return value of VLAN.get_untagged_PIF"  x)
    (**  *)
    let get_tag ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      
      rpc_wrapper rpc "VLAN.get_tag" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VLAN.get_tag"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      
      rpc_wrapper rpc "VLAN.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VLAN.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VLAN.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VLAN.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      let key = To.string key in
      
      rpc_wrapper rpc "VLAN.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~tagged_PIF ~tag ~network =
      let session_id = To.ref_session session_id in
      let tagged_PIF = To.ref_PIF tagged_PIF in
      let tag = To.int64 tag in
      let network = To.ref_network network in
      
      rpc_wrapper rpc "VLAN.create" [ session_id; tagged_PIF; tag; network ] >>= fun x -> return (From.ref_VLAN "return value of VLAN.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VLAN self in
      
      rpc_wrapper rpc "VLAN.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VLAN.get_all" [ session_id ] >>= fun x -> return (From.ref_VLAN_set "return value of VLAN.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VLAN.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VLAN_to_vLAN_t_map "return value of VLAN.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VLAN.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VLAN_to_vLAN_t_map "return value of VLAN.get_all_records"  x)
  end
  module SM = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_record" [ session_id; self ] >>= fun x -> return (From.sM_t "return value of SM.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "SM.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_SM "return value of SM.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "SM.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_SM_set "return value of SM.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_name_description"  x)
    (**  *)
    let get_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_type" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_type"  x)
    (**  *)
    let get_vendor ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_vendor" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_vendor"  x)
    (**  *)
    let get_copyright ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_copyright" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_copyright"  x)
    (**  *)
    let get_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_version" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_version"  x)
    (**  *)
    let get_required_api_version ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_required_api_version" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_required_api_version"  x)
    (**  *)
    let get_configuration ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_configuration" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of SM.get_configuration"  x)
    (**  *)
    let get_capabilities ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_capabilities" [ session_id; self ] >>= fun x -> return (From.string_set "return value of SM.get_capabilities"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of SM.get_other_config"  x)
    (**  *)
    let get_driver_filename ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      
      rpc_wrapper rpc "SM.get_driver_filename" [ session_id; self ] >>= fun x -> return (From.string "return value of SM.get_driver_filename"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "SM.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "SM.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_SM self in
      let key = To.string key in
      
      rpc_wrapper rpc "SM.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "SM.get_all" [ session_id ] >>= fun x -> return (From.ref_SM_set "return value of SM.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "SM.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_SM_to_sM_t_map "return value of SM.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "SM.get_all_records" [ session_id ] >>= fun x -> return (From.ref_SM_to_sM_t_map "return value of SM.get_all_records"  x)
  end
  module SR = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_record" [ session_id; self ] >>= fun x -> return (From.sR_t "return value of SR.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "SR.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_SR "return value of SR.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "SR.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_SR_set "return value of SR.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of SR.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of SR.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of SR.get_name_description"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.storage_operations_set "return value of SR.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_storage_operations_map "return value of SR.get_current_operations"  x)
    (**  *)
    let get_VDIs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_VDIs" [ session_id; self ] >>= fun x -> return (From.ref_VDI_set "return value of SR.get_VDIs"  x)
    (**  *)
    let get_PBDs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_PBDs" [ session_id; self ] >>= fun x -> return (From.ref_PBD_set "return value of SR.get_PBDs"  x)
    (**  *)
    let get_virtual_allocation ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_virtual_allocation" [ session_id; self ] >>= fun x -> return (From.int64 "return value of SR.get_virtual_allocation"  x)
    (**  *)
    let get_physical_utilisation ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_physical_utilisation" [ session_id; self ] >>= fun x -> return (From.int64 "return value of SR.get_physical_utilisation"  x)
    (**  *)
    let get_physical_size ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_physical_size" [ session_id; self ] >>= fun x -> return (From.int64 "return value of SR.get_physical_size"  x)
    (**  *)
    let get_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_type" [ session_id; self ] >>= fun x -> return (From.string "return value of SR.get_type"  x)
    (**  *)
    let get_content_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_content_type" [ session_id; self ] >>= fun x -> return (From.string "return value of SR.get_content_type"  x)
    (**  *)
    let get_shared ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_shared" [ session_id; self ] >>= fun x -> return (From.bool "return value of SR.get_shared"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of SR.get_other_config"  x)
    (**  *)
    let get_tags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_tags" [ session_id; self ] >>= fun x -> return (From.string_set "return value of SR.get_tags"  x)
    (**  *)
    let get_sm_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_sm_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of SR.get_sm_config"  x)
    (**  *)
    let get_blobs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_blobs" [ session_id; self ] >>= fun x -> return (From.string_to_ref_blob_map "return value of SR.get_blobs"  x)
    (**  *)
    let get_local_cache_enabled ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_local_cache_enabled" [ session_id; self ] >>= fun x -> return (From.bool "return value of SR.get_local_cache_enabled"  x)
    (**  *)
    let get_introduced_by ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      
      rpc_wrapper rpc "SR.get_introduced_by" [ session_id; self ] >>= fun x -> return (From.ref_DR_task "return value of SR.get_introduced_by"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "SR.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "SR.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let key = To.string key in
      
      rpc_wrapper rpc "SR.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let value = To.string_set value in
      
      rpc_wrapper rpc "SR.set_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let value = To.string value in
      
      rpc_wrapper rpc "SR.add_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let value = To.string value in
      
      rpc_wrapper rpc "SR.remove_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_sm_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "SR.set_sm_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_sm_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "SR.add_to_sm_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_sm_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let key = To.string key in
      
      rpc_wrapper rpc "SR.remove_from_sm_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~shared ~sm_config =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let device_config = To.string_to_string_map device_config in
      let physical_size = To.int64 physical_size in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let _type = To.string _type in
      let content_type = To.string content_type in
      let shared = To.bool shared in
      let sm_config = To.string_to_string_map sm_config in
      
      rpc_wrapper rpc "SR.create" [ session_id; host; device_config; physical_size; name_label; name_description; _type; content_type; shared; sm_config ] >>= fun x -> return (From.ref_SR "return value of SR.create"  x)
    (**  *)
    let introduce ~rpc ~session_id ~uuid ~name_label ~name_description ~_type ~content_type ~shared ~sm_config =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let _type = To.string _type in
      let content_type = To.string content_type in
      let shared = To.bool shared in
      let sm_config = To.string_to_string_map sm_config in
      
      rpc_wrapper rpc "SR.introduce" [ session_id; uuid; name_label; name_description; _type; content_type; shared; sm_config ] >>= fun x -> return (From.ref_SR "return value of SR.introduce"  x)
    (**  *)
    let make ~rpc ~session_id ~host ~device_config ~physical_size ~name_label ~name_description ~_type ~content_type ~sm_config =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let device_config = To.string_to_string_map device_config in
      let physical_size = To.int64 physical_size in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let _type = To.string _type in
      let content_type = To.string content_type in
      let sm_config = To.string_to_string_map sm_config in
      
      rpc_wrapper rpc "SR.make" [ session_id; host; device_config; physical_size; name_label; name_description; _type; content_type; sm_config ] >>= fun x -> return (From.string "return value of SR.make"  x)
    (**  *)
    let destroy ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "SR.destroy" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let forget ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "SR.forget" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let update ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "SR.update" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let get_supported_types ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "SR.get_supported_types" [ session_id ] >>= fun x -> return (From.string_set "return value of SR.get_supported_types"  x)
    (**  *)
    let scan ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "SR.scan" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let probe ~rpc ~session_id ~host ~device_config ~_type ~sm_config =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let device_config = To.string_to_string_map device_config in
      let _type = To.string _type in
      let sm_config = To.string_to_string_map sm_config in
      
      rpc_wrapper rpc "SR.probe" [ session_id; host; device_config; _type; sm_config ] >>= fun x -> return (From.string "return value of SR.probe"  x)
    (**  *)
    let set_shared ~rpc ~session_id ~sr ~value =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      let value = To.bool value in
      
      rpc_wrapper rpc "SR.set_shared" [ session_id; sr; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_label ~rpc ~session_id ~sr ~value =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      let value = To.string value in
      
      rpc_wrapper rpc "SR.set_name_label" [ session_id; sr; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~sr ~value =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      let value = To.string value in
      
      rpc_wrapper rpc "SR.set_name_description" [ session_id; sr; value ] >>= fun x -> return (ignore x)
    (**  *)
    let create_new_blob ~rpc ~session_id ~sr ~name ~mime_type ~public =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      let name = To.string name in
      let mime_type = To.string mime_type in
      let public = To.bool public in
      
      rpc_wrapper rpc "SR.create_new_blob" [ session_id; sr; name; mime_type; public ] >>= fun x -> return (From.ref_blob "return value of SR.create_new_blob"  x)
    (**  *)
    let set_physical_size ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "SR.set_physical_size" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_virtual_allocation ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "SR.set_virtual_allocation" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_physical_utilisation ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_SR self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "SR.set_physical_utilisation" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_can_host_ha_statefile ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "SR.assert_can_host_ha_statefile" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_supports_database_replication ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "SR.assert_supports_database_replication" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let enable_database_replication ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "SR.enable_database_replication" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let disable_database_replication ~rpc ~session_id ~sr =
      let session_id = To.ref_session session_id in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "SR.disable_database_replication" [ session_id; sr ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "SR.get_all" [ session_id ] >>= fun x -> return (From.ref_SR_set "return value of SR.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "SR.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_SR_to_sR_t_map "return value of SR.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "SR.get_all_records" [ session_id ] >>= fun x -> return (From.ref_SR_to_sR_t_map "return value of SR.get_all_records"  x)
  end
  module VDI = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_record" [ session_id; self ] >>= fun x -> return (From.vDI_t "return value of VDI.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VDI.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VDI "return value of VDI.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~name_label ~name_description ~sR ~virtual_size ~_type ~sharable ~read_only ~other_config ~xenstore_data ~sm_config ~tags =
      let session_id = To.ref_session session_id in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let sR = To.ref_SR sR in
      let virtual_size = To.int64 virtual_size in
      let _type = To.vdi_type _type in
      let sharable = To.bool sharable in
      let read_only = To.bool read_only in
      let other_config = To.string_to_string_map other_config in
      let xenstore_data = To.string_to_string_map xenstore_data in
      let sm_config = To.string_to_string_map sm_config in
      let tags = To.string_set tags in
      let __structure = To.structure [ "name_label", name_label; "name_description", name_description; "SR", sR; "virtual_size", virtual_size; "type", _type; "sharable", sharable; "read_only", read_only; "other_config", other_config; "xenstore_data", xenstore_data; "sm_config", sm_config; "tags", tags] in
      rpc_wrapper rpc "VDI.create" [ session_id; __structure ] >>= fun x -> return (From.ref_VDI "return value of VDI.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "VDI.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_VDI_set "return value of VDI.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VDI.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of VDI.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of VDI.get_name_description"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.vdi_operations_set "return value of VDI.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_vdi_operations_map "return value of VDI.get_current_operations"  x)
    (**  *)
    let get_SR ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_SR" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of VDI.get_SR"  x)
    (**  *)
    let get_VBDs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_VBDs" [ session_id; self ] >>= fun x -> return (From.ref_VBD_set "return value of VDI.get_VBDs"  x)
    (**  *)
    let get_crash_dumps ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_crash_dumps" [ session_id; self ] >>= fun x -> return (From.ref_crashdump_set "return value of VDI.get_crash_dumps"  x)
    (**  *)
    let get_virtual_size ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_virtual_size" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VDI.get_virtual_size"  x)
    (**  *)
    let get_physical_utilisation ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_physical_utilisation" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VDI.get_physical_utilisation"  x)
    (**  *)
    let get_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_type" [ session_id; self ] >>= fun x -> return (From.vdi_type "return value of VDI.get_type"  x)
    (**  *)
    let get_sharable ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_sharable" [ session_id; self ] >>= fun x -> return (From.bool "return value of VDI.get_sharable"  x)
    (**  *)
    let get_read_only ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_read_only" [ session_id; self ] >>= fun x -> return (From.bool "return value of VDI.get_read_only"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VDI.get_other_config"  x)
    (**  *)
    let get_storage_lock ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_storage_lock" [ session_id; self ] >>= fun x -> return (From.bool "return value of VDI.get_storage_lock"  x)
    (**  *)
    let get_location ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_location" [ session_id; self ] >>= fun x -> return (From.string "return value of VDI.get_location"  x)
    (**  *)
    let get_managed ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_managed" [ session_id; self ] >>= fun x -> return (From.bool "return value of VDI.get_managed"  x)
    (**  *)
    let get_missing ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_missing" [ session_id; self ] >>= fun x -> return (From.bool "return value of VDI.get_missing"  x)
    (**  *)
    let get_parent ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_parent" [ session_id; self ] >>= fun x -> return (From.ref_VDI "return value of VDI.get_parent"  x)
    (**  *)
    let get_xenstore_data ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_xenstore_data" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VDI.get_xenstore_data"  x)
    (**  *)
    let get_sm_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_sm_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VDI.get_sm_config"  x)
    (**  *)
    let get_is_a_snapshot ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_is_a_snapshot" [ session_id; self ] >>= fun x -> return (From.bool "return value of VDI.get_is_a_snapshot"  x)
    (**  *)
    let get_snapshot_of ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_snapshot_of" [ session_id; self ] >>= fun x -> return (From.ref_VDI "return value of VDI.get_snapshot_of"  x)
    (**  *)
    let get_snapshots ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_snapshots" [ session_id; self ] >>= fun x -> return (From.ref_VDI_set "return value of VDI.get_snapshots"  x)
    (**  *)
    let get_snapshot_time ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_snapshot_time" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VDI.get_snapshot_time"  x)
    (**  *)
    let get_tags ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_tags" [ session_id; self ] >>= fun x -> return (From.string_set "return value of VDI.get_tags"  x)
    (**  *)
    let get_allow_caching ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_allow_caching" [ session_id; self ] >>= fun x -> return (From.bool "return value of VDI.get_allow_caching"  x)
    (**  *)
    let get_on_boot ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_on_boot" [ session_id; self ] >>= fun x -> return (From.on_boot "return value of VDI.get_on_boot"  x)
    (**  *)
    let get_metadata_of_pool ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_metadata_of_pool" [ session_id; self ] >>= fun x -> return (From.ref_pool "return value of VDI.get_metadata_of_pool"  x)
    (**  *)
    let get_metadata_latest ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.get_metadata_latest" [ session_id; self ] >>= fun x -> return (From.bool "return value of VDI.get_metadata_latest"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VDI.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VDI.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let key = To.string key in
      
      rpc_wrapper rpc "VDI.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_xenstore_data ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VDI.set_xenstore_data" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_xenstore_data ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VDI.add_to_xenstore_data" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_xenstore_data ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let key = To.string key in
      
      rpc_wrapper rpc "VDI.remove_from_xenstore_data" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_sm_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VDI.set_sm_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_sm_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VDI.add_to_sm_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_sm_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let key = To.string key in
      
      rpc_wrapper rpc "VDI.remove_from_sm_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.string_set value in
      
      rpc_wrapper rpc "VDI.set_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.string value in
      
      rpc_wrapper rpc "VDI.add_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_tags ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.string value in
      
      rpc_wrapper rpc "VDI.remove_tags" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let snapshot ~rpc ~session_id ~vdi ~driver_params =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      let driver_params = To.string_to_string_map driver_params in
      
      rpc_wrapper rpc "VDI.snapshot" [ session_id; vdi; driver_params ] >>= fun x -> return (From.ref_VDI "return value of VDI.snapshot"  x)
    (**  *)
    let clone ~rpc ~session_id ~vdi ~driver_params =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      let driver_params = To.string_to_string_map driver_params in
      
      rpc_wrapper rpc "VDI.clone" [ session_id; vdi; driver_params ] >>= fun x -> return (From.ref_VDI "return value of VDI.clone"  x)
    (**  *)
    let resize ~rpc ~session_id ~vdi ~size =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      let size = To.int64 size in
      
      rpc_wrapper rpc "VDI.resize" [ session_id; vdi; size ] >>= fun x -> return (ignore x)
    (**  *)
    let resize_online ~rpc ~session_id ~vdi ~size =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      let size = To.int64 size in
      
      rpc_wrapper rpc "VDI.resize_online" [ session_id; vdi; size ] >>= fun x -> return (ignore x)
    (**  *)
    let introduce ~rpc ~session_id ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let sR = To.ref_SR sR in
      let _type = To.vdi_type _type in
      let sharable = To.bool sharable in
      let read_only = To.bool read_only in
      let other_config = To.string_to_string_map other_config in
      let location = To.string location in
      let xenstore_data = To.string_to_string_map xenstore_data in
      let sm_config = To.string_to_string_map sm_config in
      let managed = To.bool managed in
      let virtual_size = To.int64 virtual_size in
      let physical_utilisation = To.int64 physical_utilisation in
      let metadata_of_pool = To.ref_pool metadata_of_pool in
      let is_a_snapshot = To.bool is_a_snapshot in
      let snapshot_time = To.datetime snapshot_time in
      let snapshot_of = To.ref_VDI snapshot_of in
      
      rpc_wrapper rpc "VDI.introduce" [ session_id; uuid; name_label; name_description; sR; _type; sharable; read_only; other_config; location; xenstore_data; sm_config; managed; virtual_size; physical_utilisation; metadata_of_pool; is_a_snapshot; snapshot_time; snapshot_of ] >>= fun x -> return (From.ref_VDI "return value of VDI.introduce"  x)
    (**  *)
    let pool_introduce ~rpc ~session_id ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let sR = To.ref_SR sR in
      let _type = To.vdi_type _type in
      let sharable = To.bool sharable in
      let read_only = To.bool read_only in
      let other_config = To.string_to_string_map other_config in
      let location = To.string location in
      let xenstore_data = To.string_to_string_map xenstore_data in
      let sm_config = To.string_to_string_map sm_config in
      let managed = To.bool managed in
      let virtual_size = To.int64 virtual_size in
      let physical_utilisation = To.int64 physical_utilisation in
      let metadata_of_pool = To.ref_pool metadata_of_pool in
      let is_a_snapshot = To.bool is_a_snapshot in
      let snapshot_time = To.datetime snapshot_time in
      let snapshot_of = To.ref_VDI snapshot_of in
      
      rpc_wrapper rpc "VDI.pool_introduce" [ session_id; uuid; name_label; name_description; sR; _type; sharable; read_only; other_config; location; xenstore_data; sm_config; managed; virtual_size; physical_utilisation; metadata_of_pool; is_a_snapshot; snapshot_time; snapshot_of ] >>= fun x -> return (From.ref_VDI "return value of VDI.pool_introduce"  x)
    (**  *)
    let db_introduce ~rpc ~session_id ~uuid ~name_label ~name_description ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot ~snapshot_time ~snapshot_of =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let sR = To.ref_SR sR in
      let _type = To.vdi_type _type in
      let sharable = To.bool sharable in
      let read_only = To.bool read_only in
      let other_config = To.string_to_string_map other_config in
      let location = To.string location in
      let xenstore_data = To.string_to_string_map xenstore_data in
      let sm_config = To.string_to_string_map sm_config in
      let managed = To.bool managed in
      let virtual_size = To.int64 virtual_size in
      let physical_utilisation = To.int64 physical_utilisation in
      let metadata_of_pool = To.ref_pool metadata_of_pool in
      let is_a_snapshot = To.bool is_a_snapshot in
      let snapshot_time = To.datetime snapshot_time in
      let snapshot_of = To.ref_VDI snapshot_of in
      
      rpc_wrapper rpc "VDI.db_introduce" [ session_id; uuid; name_label; name_description; sR; _type; sharable; read_only; other_config; location; xenstore_data; sm_config; managed; virtual_size; physical_utilisation; metadata_of_pool; is_a_snapshot; snapshot_time; snapshot_of ] >>= fun x -> return (From.ref_VDI "return value of VDI.db_introduce"  x)
    (**  *)
    let db_forget ~rpc ~session_id ~vdi =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      
      rpc_wrapper rpc "VDI.db_forget" [ session_id; vdi ] >>= fun x -> return (ignore x)
    (**  *)
    let update ~rpc ~session_id ~vdi =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      
      rpc_wrapper rpc "VDI.update" [ session_id; vdi ] >>= fun x -> return (ignore x)
    (**  *)
    let copy ~rpc ~session_id ~vdi ~sr =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      let sr = To.ref_SR sr in
      
      rpc_wrapper rpc "VDI.copy" [ session_id; vdi; sr ] >>= fun x -> return (From.ref_VDI "return value of VDI.copy"  x)
    (**  *)
    let force_unlock ~rpc ~session_id ~vdi =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      
      rpc_wrapper rpc "VDI.force_unlock" [ session_id; vdi ] >>= fun x -> return (ignore x)
    (**  *)
    let set_managed ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VDI.set_managed" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let forget ~rpc ~session_id ~vdi =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      
      rpc_wrapper rpc "VDI.forget" [ session_id; vdi ] >>= fun x -> return (ignore x)
    (**  *)
    let set_sharable ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VDI.set_sharable" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_read_only ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VDI.set_read_only" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_missing ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VDI.set_missing" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_virtual_size ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VDI.set_virtual_size" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_physical_utilisation ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.int64 value in
      
      rpc_wrapper rpc "VDI.set_physical_utilisation" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_is_a_snapshot ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VDI.set_is_a_snapshot" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_snapshot_of ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.ref_VDI value in
      
      rpc_wrapper rpc "VDI.set_snapshot_of" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_snapshot_time ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.datetime value in
      
      rpc_wrapper rpc "VDI.set_snapshot_time" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_metadata_of_pool ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.ref_pool value in
      
      rpc_wrapper rpc "VDI.set_metadata_of_pool" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.string value in
      
      rpc_wrapper rpc "VDI.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.string value in
      
      rpc_wrapper rpc "VDI.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let generate_config ~rpc ~session_id ~host ~vdi =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let vdi = To.ref_VDI vdi in
      
      rpc_wrapper rpc "VDI.generate_config" [ session_id; host; vdi ] >>= fun x -> return (From.string "return value of VDI.generate_config"  x)
    (**  *)
    let set_on_boot ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.on_boot value in
      
      rpc_wrapper rpc "VDI.set_on_boot" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_allow_caching ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VDI.set_allow_caching" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let open_database ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.open_database" [ session_id; self ] >>= fun x -> return (From.ref_session "return value of VDI.open_database"  x)
    (**  *)
    let checksum ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.checksum" [ session_id; self ] >>= fun x -> return (From.string "return value of VDI.checksum"  x)
    (**  *)
    let read_database_pool_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VDI self in
      
      rpc_wrapper rpc "VDI.read_database_pool_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VDI.read_database_pool_uuid"  x)
    (**  *)
    let pool_migrate ~rpc ~session_id ~vdi ~sr ~options =
      let session_id = To.ref_session session_id in
      let vdi = To.ref_VDI vdi in
      let sr = To.ref_SR sr in
      let options = To.string_to_string_map options in
      
      rpc_wrapper rpc "VDI.pool_migrate" [ session_id; vdi; sr; options ] >>= fun x -> return (From.ref_VDI "return value of VDI.pool_migrate"  x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VDI.get_all" [ session_id ] >>= fun x -> return (From.ref_VDI_set "return value of VDI.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VDI.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VDI_to_vDI_t_map "return value of VDI.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VDI.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VDI_to_vDI_t_map "return value of VDI.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~name_label:value.vDI_name_label
      ~name_description:value.vDI_name_description
      ~sR:value.vDI_SR
      ~virtual_size:value.vDI_virtual_size
      ~_type:value.vDI_type
      ~sharable:value.vDI_sharable
      ~read_only:value.vDI_read_only
      ~other_config:value.vDI_other_config
      ~xenstore_data:value.vDI_xenstore_data
      ~sm_config:value.vDI_sm_config
      ~tags:value.vDI_tags
  end
  module VBD = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_record" [ session_id; self ] >>= fun x -> return (From.vBD_t "return value of VBD.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VBD.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VBD "return value of VBD.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~vM ~vDI ~userdevice ~bootable ~mode ~_type ~unpluggable ~empty ~other_config ~qos_algorithm_type ~qos_algorithm_params =
      let session_id = To.ref_session session_id in
      let vM = To.ref_VM vM in
      let vDI = To.ref_VDI vDI in
      let userdevice = To.string userdevice in
      let bootable = To.bool bootable in
      let mode = To.vbd_mode mode in
      let _type = To.vbd_type _type in
      let unpluggable = To.bool unpluggable in
      let empty = To.bool empty in
      let other_config = To.string_to_string_map other_config in
      let qos_algorithm_type = To.string qos_algorithm_type in
      let qos_algorithm_params = To.string_to_string_map qos_algorithm_params in
      let __structure = To.structure [ "VM", vM; "VDI", vDI; "userdevice", userdevice; "bootable", bootable; "mode", mode; "type", _type; "unpluggable", unpluggable; "empty", empty; "other_config", other_config; "qos_algorithm_type", qos_algorithm_type; "qos_algorithm_params", qos_algorithm_params] in
      rpc_wrapper rpc "VBD.create" [ session_id; __structure ] >>= fun x -> return (From.ref_VBD "return value of VBD.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VBD.get_uuid"  x)
    (**  *)
    let get_allowed_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_allowed_operations" [ session_id; self ] >>= fun x -> return (From.vbd_operations_set "return value of VBD.get_allowed_operations"  x)
    (**  *)
    let get_current_operations ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_current_operations" [ session_id; self ] >>= fun x -> return (From.string_to_vbd_operations_map "return value of VBD.get_current_operations"  x)
    (**  *)
    let get_VM ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_VM" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of VBD.get_VM"  x)
    (**  *)
    let get_VDI ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_VDI" [ session_id; self ] >>= fun x -> return (From.ref_VDI "return value of VBD.get_VDI"  x)
    (**  *)
    let get_device ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_device" [ session_id; self ] >>= fun x -> return (From.string "return value of VBD.get_device"  x)
    (**  *)
    let get_userdevice ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_userdevice" [ session_id; self ] >>= fun x -> return (From.string "return value of VBD.get_userdevice"  x)
    (**  *)
    let get_bootable ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_bootable" [ session_id; self ] >>= fun x -> return (From.bool "return value of VBD.get_bootable"  x)
    (**  *)
    let get_mode ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_mode" [ session_id; self ] >>= fun x -> return (From.vbd_mode "return value of VBD.get_mode"  x)
    (**  *)
    let get_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_type" [ session_id; self ] >>= fun x -> return (From.vbd_type "return value of VBD.get_type"  x)
    (**  *)
    let get_unpluggable ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_unpluggable" [ session_id; self ] >>= fun x -> return (From.bool "return value of VBD.get_unpluggable"  x)
    (**  *)
    let get_storage_lock ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_storage_lock" [ session_id; self ] >>= fun x -> return (From.bool "return value of VBD.get_storage_lock"  x)
    (**  *)
    let get_empty ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_empty" [ session_id; self ] >>= fun x -> return (From.bool "return value of VBD.get_empty"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VBD.get_other_config"  x)
    (**  *)
    let get_currently_attached ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_currently_attached" [ session_id; self ] >>= fun x -> return (From.bool "return value of VBD.get_currently_attached"  x)
    (**  *)
    let get_status_code ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_status_code" [ session_id; self ] >>= fun x -> return (From.int64 "return value of VBD.get_status_code"  x)
    (**  *)
    let get_status_detail ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_status_detail" [ session_id; self ] >>= fun x -> return (From.string "return value of VBD.get_status_detail"  x)
    (**  *)
    let get_runtime_properties ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_runtime_properties" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VBD.get_runtime_properties"  x)
    (**  *)
    let get_qos_algorithm_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_qos_algorithm_type" [ session_id; self ] >>= fun x -> return (From.string "return value of VBD.get_qos_algorithm_type"  x)
    (**  *)
    let get_qos_algorithm_params ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_qos_algorithm_params" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VBD.get_qos_algorithm_params"  x)
    (**  *)
    let get_qos_supported_algorithms ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_qos_supported_algorithms" [ session_id; self ] >>= fun x -> return (From.string_set "return value of VBD.get_qos_supported_algorithms"  x)
    (**  *)
    let get_metrics ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.get_metrics" [ session_id; self ] >>= fun x -> return (From.ref_VBD_metrics "return value of VBD.get_metrics"  x)
    (**  *)
    let set_userdevice ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let value = To.string value in
      
      rpc_wrapper rpc "VBD.set_userdevice" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_bootable ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VBD.set_bootable" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_mode ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let value = To.vbd_mode value in
      
      rpc_wrapper rpc "VBD.set_mode" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_type ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let value = To.vbd_type value in
      
      rpc_wrapper rpc "VBD.set_type" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_unpluggable ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let value = To.bool value in
      
      rpc_wrapper rpc "VBD.set_unpluggable" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VBD.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VBD.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let key = To.string key in
      
      rpc_wrapper rpc "VBD.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_qos_algorithm_type ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let value = To.string value in
      
      rpc_wrapper rpc "VBD.set_qos_algorithm_type" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_qos_algorithm_params ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VBD.set_qos_algorithm_params" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_qos_algorithm_params ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VBD.add_to_qos_algorithm_params" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_qos_algorithm_params ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let key = To.string key in
      
      rpc_wrapper rpc "VBD.remove_from_qos_algorithm_params" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let eject ~rpc ~session_id ~vbd =
      let session_id = To.ref_session session_id in
      let vbd = To.ref_VBD vbd in
      
      rpc_wrapper rpc "VBD.eject" [ session_id; vbd ] >>= fun x -> return (ignore x)
    (**  *)
    let insert ~rpc ~session_id ~vbd ~vdi =
      let session_id = To.ref_session session_id in
      let vbd = To.ref_VBD vbd in
      let vdi = To.ref_VDI vdi in
      
      rpc_wrapper rpc "VBD.insert" [ session_id; vbd; vdi ] >>= fun x -> return (ignore x)
    (**  *)
    let plug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.plug" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let unplug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.unplug" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let unplug_force ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.unplug_force" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let unplug_force_no_safety_check ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.unplug_force_no_safety_check" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let assert_attachable ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.assert_attachable" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let pause ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      
      rpc_wrapper rpc "VBD.pause" [ session_id; self ] >>= fun x -> return (From.string "return value of VBD.pause"  x)
    (**  *)
    let unpause ~rpc ~session_id ~self ~token =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD self in
      let token = To.string token in
      
      rpc_wrapper rpc "VBD.unpause" [ session_id; self; token ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VBD.get_all" [ session_id ] >>= fun x -> return (From.ref_VBD_set "return value of VBD.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VBD.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VBD_to_vBD_t_map "return value of VBD.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VBD.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VBD_to_vBD_t_map "return value of VBD.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~vM:value.vBD_VM
      ~vDI:value.vBD_VDI
      ~userdevice:value.vBD_userdevice
      ~bootable:value.vBD_bootable
      ~mode:value.vBD_mode
      ~_type:value.vBD_type
      ~unpluggable:value.vBD_unpluggable
      ~empty:value.vBD_empty
      ~other_config:value.vBD_other_config
      ~qos_algorithm_type:value.vBD_qos_algorithm_type
      ~qos_algorithm_params:value.vBD_qos_algorithm_params
  end
  module VBD_metrics = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      
      rpc_wrapper rpc "VBD_metrics.get_record" [ session_id; self ] >>= fun x -> return (From.vBD_metrics_t "return value of VBD_metrics.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VBD_metrics.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VBD_metrics "return value of VBD_metrics.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      
      rpc_wrapper rpc "VBD_metrics.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VBD_metrics.get_uuid"  x)
    (**  *)
    let get_io_read_kbs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      
      rpc_wrapper rpc "VBD_metrics.get_io_read_kbs" [ session_id; self ] >>= fun x -> return (From.float "return value of VBD_metrics.get_io_read_kbs"  x)
    (**  *)
    let get_io_write_kbs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      
      rpc_wrapper rpc "VBD_metrics.get_io_write_kbs" [ session_id; self ] >>= fun x -> return (From.float "return value of VBD_metrics.get_io_write_kbs"  x)
    (**  *)
    let get_last_updated ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      
      rpc_wrapper rpc "VBD_metrics.get_last_updated" [ session_id; self ] >>= fun x -> return (From.datetime "return value of VBD_metrics.get_last_updated"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      
      rpc_wrapper rpc "VBD_metrics.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VBD_metrics.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VBD_metrics.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VBD_metrics.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VBD_metrics self in
      let key = To.string key in
      
      rpc_wrapper rpc "VBD_metrics.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VBD_metrics.get_all" [ session_id ] >>= fun x -> return (From.ref_VBD_metrics_set "return value of VBD_metrics.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VBD_metrics.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VBD_metrics_to_vBD_metrics_t_map "return value of VBD_metrics.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VBD_metrics.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VBD_metrics_to_vBD_metrics_t_map "return value of VBD_metrics.get_all_records"  x)
  end
  module PBD = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.get_record" [ session_id; self ] >>= fun x -> return (From.pBD_t "return value of PBD.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "PBD.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_PBD "return value of PBD.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~host ~sR ~device_config ~other_config =
      let session_id = To.ref_session session_id in
      let host = To.ref_host host in
      let sR = To.ref_SR sR in
      let device_config = To.string_to_string_map device_config in
      let other_config = To.string_to_string_map other_config in
      let __structure = To.structure [ "host", host; "SR", sR; "device_config", device_config; "other_config", other_config] in
      rpc_wrapper rpc "PBD.create" [ session_id; __structure ] >>= fun x -> return (From.ref_PBD "return value of PBD.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of PBD.get_uuid"  x)
    (**  *)
    let get_host ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.get_host" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of PBD.get_host"  x)
    (**  *)
    let get_SR ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.get_SR" [ session_id; self ] >>= fun x -> return (From.ref_SR "return value of PBD.get_SR"  x)
    (**  *)
    let get_device_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.get_device_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of PBD.get_device_config"  x)
    (**  *)
    let get_currently_attached ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.get_currently_attached" [ session_id; self ] >>= fun x -> return (From.bool "return value of PBD.get_currently_attached"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of PBD.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "PBD.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "PBD.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      let key = To.string key in
      
      rpc_wrapper rpc "PBD.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let plug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.plug" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let unplug ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      
      rpc_wrapper rpc "PBD.unplug" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let set_device_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PBD self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "PBD.set_device_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PBD.get_all" [ session_id ] >>= fun x -> return (From.ref_PBD_set "return value of PBD.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "PBD.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_PBD_to_pBD_t_map "return value of PBD.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PBD.get_all_records" [ session_id ] >>= fun x -> return (From.ref_PBD_to_pBD_t_map "return value of PBD.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~host:value.pBD_host
      ~sR:value.pBD_SR
      ~device_config:value.pBD_device_config
      ~other_config:value.pBD_other_config
  end
  module Crashdump = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      
      rpc_wrapper rpc "crashdump.get_record" [ session_id; self ] >>= fun x -> return (From.crashdump_t "return value of crashdump.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "crashdump.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_crashdump "return value of crashdump.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      
      rpc_wrapper rpc "crashdump.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of crashdump.get_uuid"  x)
    (**  *)
    let get_VM ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      
      rpc_wrapper rpc "crashdump.get_VM" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of crashdump.get_VM"  x)
    (**  *)
    let get_VDI ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      
      rpc_wrapper rpc "crashdump.get_VDI" [ session_id; self ] >>= fun x -> return (From.ref_VDI "return value of crashdump.get_VDI"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      
      rpc_wrapper rpc "crashdump.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of crashdump.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "crashdump.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "crashdump.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      let key = To.string key in
      
      rpc_wrapper rpc "crashdump.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_crashdump self in
      
      rpc_wrapper rpc "crashdump.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "crashdump.get_all" [ session_id ] >>= fun x -> return (From.ref_crashdump_set "return value of crashdump.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "crashdump.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_crashdump_to_crashdump_t_map "return value of crashdump.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "crashdump.get_all_records" [ session_id ] >>= fun x -> return (From.ref_crashdump_to_crashdump_t_map "return value of crashdump.get_all_records"  x)
  end
  module VTPM = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VTPM self in
      
      rpc_wrapper rpc "VTPM.get_record" [ session_id; self ] >>= fun x -> return (From.vTPM_t "return value of VTPM.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VTPM.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VTPM "return value of VTPM.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~vM ~backend =
      let session_id = To.ref_session session_id in
      let vM = To.ref_VM vM in
      let backend = To.ref_VM backend in
      let __structure = To.structure [ "VM", vM; "backend", backend] in
      rpc_wrapper rpc "VTPM.create" [ session_id; __structure ] >>= fun x -> return (From.ref_VTPM "return value of VTPM.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VTPM self in
      
      rpc_wrapper rpc "VTPM.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VTPM self in
      
      rpc_wrapper rpc "VTPM.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VTPM.get_uuid"  x)
    (**  *)
    let get_VM ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VTPM self in
      
      rpc_wrapper rpc "VTPM.get_VM" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of VTPM.get_VM"  x)
    (**  *)
    let get_backend ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VTPM self in
      
      rpc_wrapper rpc "VTPM.get_backend" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of VTPM.get_backend"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~vM:value.vTPM_VM
      ~backend:value.vTPM_backend
  end
  module Console = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      
      rpc_wrapper rpc "console.get_record" [ session_id; self ] >>= fun x -> return (From.console_t "return value of console.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "console.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_console "return value of console.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~other_config =
      let session_id = To.ref_session session_id in
      let other_config = To.string_to_string_map other_config in
      let __structure = To.structure [ "other_config", other_config] in
      rpc_wrapper rpc "console.create" [ session_id; __structure ] >>= fun x -> return (From.ref_console "return value of console.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      
      rpc_wrapper rpc "console.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      
      rpc_wrapper rpc "console.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of console.get_uuid"  x)
    (**  *)
    let get_protocol ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      
      rpc_wrapper rpc "console.get_protocol" [ session_id; self ] >>= fun x -> return (From.console_protocol "return value of console.get_protocol"  x)
    (**  *)
    let get_location ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      
      rpc_wrapper rpc "console.get_location" [ session_id; self ] >>= fun x -> return (From.string "return value of console.get_location"  x)
    (**  *)
    let get_VM ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      
      rpc_wrapper rpc "console.get_VM" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of console.get_VM"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      
      rpc_wrapper rpc "console.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of console.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "console.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "console.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_console self in
      let key = To.string key in
      
      rpc_wrapper rpc "console.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "console.get_all" [ session_id ] >>= fun x -> return (From.ref_console_set "return value of console.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "console.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_console_to_console_t_map "return value of console.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "console.get_all_records" [ session_id ] >>= fun x -> return (From.ref_console_to_console_t_map "return value of console.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~other_config:value.console_other_config
  end
  module User = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      
      rpc_wrapper rpc "user.get_record" [ session_id; self ] >>= fun x -> return (From.user_t "return value of user.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "user.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_user "return value of user.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~short_name ~fullname ~other_config =
      let session_id = To.ref_session session_id in
      let short_name = To.string short_name in
      let fullname = To.string fullname in
      let other_config = To.string_to_string_map other_config in
      let __structure = To.structure [ "short_name", short_name; "fullname", fullname; "other_config", other_config] in
      rpc_wrapper rpc "user.create" [ session_id; __structure ] >>= fun x -> return (From.ref_user "return value of user.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      
      rpc_wrapper rpc "user.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      
      rpc_wrapper rpc "user.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of user.get_uuid"  x)
    (**  *)
    let get_short_name ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      
      rpc_wrapper rpc "user.get_short_name" [ session_id; self ] >>= fun x -> return (From.string "return value of user.get_short_name"  x)
    (**  *)
    let get_fullname ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      
      rpc_wrapper rpc "user.get_fullname" [ session_id; self ] >>= fun x -> return (From.string "return value of user.get_fullname"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      
      rpc_wrapper rpc "user.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of user.get_other_config"  x)
    (**  *)
    let set_fullname ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      let value = To.string value in
      
      rpc_wrapper rpc "user.set_fullname" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "user.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "user.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_user self in
      let key = To.string key in
      
      rpc_wrapper rpc "user.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~short_name:value.user_short_name
      ~fullname:value.user_fullname
      ~other_config:value.user_other_config
  end
  module Data_source = struct
  end
  module Blob = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.get_record" [ session_id; self ] >>= fun x -> return (From.blob_t "return value of blob.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "blob.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_blob "return value of blob.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "blob.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_blob_set "return value of blob.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of blob.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of blob.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of blob.get_name_description"  x)
    (**  *)
    let get_size ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.get_size" [ session_id; self ] >>= fun x -> return (From.int64 "return value of blob.get_size"  x)
    (**  *)
    let get_public ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.get_public" [ session_id; self ] >>= fun x -> return (From.bool "return value of blob.get_public"  x)
    (**  *)
    let get_last_updated ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.get_last_updated" [ session_id; self ] >>= fun x -> return (From.datetime "return value of blob.get_last_updated"  x)
    (**  *)
    let get_mime_type ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.get_mime_type" [ session_id; self ] >>= fun x -> return (From.string "return value of blob.get_mime_type"  x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      let value = To.string value in
      
      rpc_wrapper rpc "blob.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      let value = To.string value in
      
      rpc_wrapper rpc "blob.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_public ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      let value = To.bool value in
      
      rpc_wrapper rpc "blob.set_public" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~mime_type ~public =
      let session_id = To.ref_session session_id in
      let mime_type = To.string mime_type in
      let public = To.bool public in
      
      rpc_wrapper rpc "blob.create" [ session_id; mime_type; public ] >>= fun x -> return (From.ref_blob "return value of blob.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_blob self in
      
      rpc_wrapper rpc "blob.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "blob.get_all" [ session_id ] >>= fun x -> return (From.ref_blob_set "return value of blob.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "blob.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_blob_to_blob_t_map "return value of blob.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "blob.get_all_records" [ session_id ] >>= fun x -> return (From.ref_blob_to_blob_t_map "return value of blob.get_all_records"  x)
  end
  module Message = struct
    (**  *)
    let create ~rpc ~session_id ~name ~priority ~cls ~obj_uuid ~body =
      let session_id = To.ref_session session_id in
      let name = To.string name in
      let priority = To.int64 priority in
      let cls = To.cls cls in
      let obj_uuid = To.string obj_uuid in
      let body = To.string body in
      
      rpc_wrapper rpc "message.create" [ session_id; name; priority; cls; obj_uuid; body ] >>= fun x -> return (From.ref_message "return value of message.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_message self in
      
      rpc_wrapper rpc "message.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get ~rpc ~session_id ~cls ~obj_uuid ~since =
      let session_id = To.ref_session session_id in
      let cls = To.cls cls in
      let obj_uuid = To.string obj_uuid in
      let since = To.datetime since in
      
      rpc_wrapper rpc "message.get" [ session_id; cls; obj_uuid; since ] >>= fun x -> return (From.ref_message_to_message_t_map "return value of message.get"  x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "message.get_all" [ session_id ] >>= fun x -> return (From.ref_message_set "return value of message.get_all"  x)
    (**  *)
    let get_since ~rpc ~session_id ~since =
      let session_id = To.ref_session session_id in
      let since = To.datetime since in
      
      rpc_wrapper rpc "message.get_since" [ session_id; since ] >>= fun x -> return (From.ref_message_to_message_t_map "return value of message.get_since"  x)
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_message self in
      
      rpc_wrapper rpc "message.get_record" [ session_id; self ] >>= fun x -> return (From.message_t "return value of message.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "message.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_message "return value of message.get_by_uuid"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "message.get_all_records" [ session_id ] >>= fun x -> return (From.ref_message_to_message_t_map "return value of message.get_all_records"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "message.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_message_to_message_t_map "return value of message.get_all_records_where"  x)
  end
  module Secret = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      
      rpc_wrapper rpc "secret.get_record" [ session_id; self ] >>= fun x -> return (From.secret_t "return value of secret.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "secret.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_secret "return value of secret.get_by_uuid"  x)
    (**  *)
    let create ~rpc ~session_id ~value ~other_config =
      let session_id = To.ref_session session_id in
      let value = To.string value in
      let other_config = To.string_to_string_map other_config in
      let __structure = To.structure [ "value", value; "other_config", other_config] in
      rpc_wrapper rpc "secret.create" [ session_id; __structure ] >>= fun x -> return (From.ref_secret "return value of secret.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      
      rpc_wrapper rpc "secret.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      
      rpc_wrapper rpc "secret.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of secret.get_uuid"  x)
    (**  *)
    let get_value ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      
      rpc_wrapper rpc "secret.get_value" [ session_id; self ] >>= fun x -> return (From.string "return value of secret.get_value"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      
      rpc_wrapper rpc "secret.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of secret.get_other_config"  x)
    (**  *)
    let set_value ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      let value = To.string value in
      
      rpc_wrapper rpc "secret.set_value" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "secret.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "secret.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_secret self in
      let key = To.string key in
      
      rpc_wrapper rpc "secret.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let introduce ~rpc ~session_id ~uuid ~value ~other_config =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      let value = To.string value in
      let other_config = To.string_to_string_map other_config in
      
      rpc_wrapper rpc "secret.introduce" [ session_id; uuid; value; other_config ] >>= fun x -> return (From.ref_secret "return value of secret.introduce"  x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "secret.get_all" [ session_id ] >>= fun x -> return (From.ref_secret_set "return value of secret.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "secret.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_secret_to_secret_t_map "return value of secret.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "secret.get_all_records" [ session_id ] >>= fun x -> return (From.ref_secret_to_secret_t_map "return value of secret.get_all_records"  x)
    (**  *)
    let create_from_record ~rpc ~session_id ~value =
      create
      ~rpc
      ~session_id
      ~value:value.secret_value
      ~other_config:value.secret_other_config
  end
  module Tunnel = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      
      rpc_wrapper rpc "tunnel.get_record" [ session_id; self ] >>= fun x -> return (From.tunnel_t "return value of tunnel.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "tunnel.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_tunnel "return value of tunnel.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      
      rpc_wrapper rpc "tunnel.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of tunnel.get_uuid"  x)
    (**  *)
    let get_access_PIF ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      
      rpc_wrapper rpc "tunnel.get_access_PIF" [ session_id; self ] >>= fun x -> return (From.ref_PIF "return value of tunnel.get_access_PIF"  x)
    (**  *)
    let get_transport_PIF ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      
      rpc_wrapper rpc "tunnel.get_transport_PIF" [ session_id; self ] >>= fun x -> return (From.ref_PIF "return value of tunnel.get_transport_PIF"  x)
    (**  *)
    let get_status ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      
      rpc_wrapper rpc "tunnel.get_status" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of tunnel.get_status"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      
      rpc_wrapper rpc "tunnel.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of tunnel.get_other_config"  x)
    (**  *)
    let set_status ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "tunnel.set_status" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_status ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "tunnel.add_to_status" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_status ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      let key = To.string key in
      
      rpc_wrapper rpc "tunnel.remove_from_status" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "tunnel.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "tunnel.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      let key = To.string key in
      
      rpc_wrapper rpc "tunnel.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~transport_PIF ~network =
      let session_id = To.ref_session session_id in
      let transport_PIF = To.ref_PIF transport_PIF in
      let network = To.ref_network network in
      
      rpc_wrapper rpc "tunnel.create" [ session_id; transport_PIF; network ] >>= fun x -> return (From.ref_tunnel "return value of tunnel.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_tunnel self in
      
      rpc_wrapper rpc "tunnel.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "tunnel.get_all" [ session_id ] >>= fun x -> return (From.ref_tunnel_set "return value of tunnel.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "tunnel.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_tunnel_to_tunnel_t_map "return value of tunnel.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "tunnel.get_all_records" [ session_id ] >>= fun x -> return (From.ref_tunnel_to_tunnel_t_map "return value of tunnel.get_all_records"  x)
  end
  module PCI = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_record" [ session_id; self ] >>= fun x -> return (From.pCI_t "return value of PCI.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "PCI.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_PCI "return value of PCI.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of PCI.get_uuid"  x)
    (**  *)
    let get_class_name ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_class_name" [ session_id; self ] >>= fun x -> return (From.string "return value of PCI.get_class_name"  x)
    (**  *)
    let get_vendor_name ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_vendor_name" [ session_id; self ] >>= fun x -> return (From.string "return value of PCI.get_vendor_name"  x)
    (**  *)
    let get_device_name ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_device_name" [ session_id; self ] >>= fun x -> return (From.string "return value of PCI.get_device_name"  x)
    (**  *)
    let get_host ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_host" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of PCI.get_host"  x)
    (**  *)
    let get_pci_id ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_pci_id" [ session_id; self ] >>= fun x -> return (From.string "return value of PCI.get_pci_id"  x)
    (**  *)
    let get_dependencies ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_dependencies" [ session_id; self ] >>= fun x -> return (From.ref_PCI_set "return value of PCI.get_dependencies"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      
      rpc_wrapper rpc "PCI.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of PCI.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "PCI.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "PCI.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_PCI self in
      let key = To.string key in
      
      rpc_wrapper rpc "PCI.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PCI.get_all" [ session_id ] >>= fun x -> return (From.ref_PCI_set "return value of PCI.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "PCI.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_PCI_to_pCI_t_map "return value of PCI.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PCI.get_all_records" [ session_id ] >>= fun x -> return (From.ref_PCI_to_pCI_t_map "return value of PCI.get_all_records"  x)
  end
  module PGPU = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      
      rpc_wrapper rpc "PGPU.get_record" [ session_id; self ] >>= fun x -> return (From.pGPU_t "return value of PGPU.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "PGPU.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_PGPU "return value of PGPU.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      
      rpc_wrapper rpc "PGPU.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of PGPU.get_uuid"  x)
    (**  *)
    let get_PCI ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      
      rpc_wrapper rpc "PGPU.get_PCI" [ session_id; self ] >>= fun x -> return (From.ref_PCI "return value of PGPU.get_PCI"  x)
    (**  *)
    let get_GPU_group ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      
      rpc_wrapper rpc "PGPU.get_GPU_group" [ session_id; self ] >>= fun x -> return (From.ref_GPU_group "return value of PGPU.get_GPU_group"  x)
    (**  *)
    let get_host ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      
      rpc_wrapper rpc "PGPU.get_host" [ session_id; self ] >>= fun x -> return (From.ref_host "return value of PGPU.get_host"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      
      rpc_wrapper rpc "PGPU.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of PGPU.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "PGPU.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "PGPU.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_PGPU self in
      let key = To.string key in
      
      rpc_wrapper rpc "PGPU.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PGPU.get_all" [ session_id ] >>= fun x -> return (From.ref_PGPU_set "return value of PGPU.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "PGPU.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_PGPU_to_pGPU_t_map "return value of PGPU.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "PGPU.get_all_records" [ session_id ] >>= fun x -> return (From.ref_PGPU_to_pGPU_t_map "return value of PGPU.get_all_records"  x)
  end
  module GPU_group = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.get_record" [ session_id; self ] >>= fun x -> return (From.gPU_group_t "return value of GPU_group.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "GPU_group.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_GPU_group "return value of GPU_group.get_by_uuid"  x)
    (**  *)
    let get_by_name_label ~rpc ~session_id ~label =
      let session_id = To.ref_session session_id in
      let label = To.string label in
      
      rpc_wrapper rpc "GPU_group.get_by_name_label" [ session_id; label ] >>= fun x -> return (From.ref_GPU_group_set "return value of GPU_group.get_by_name_label"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of GPU_group.get_uuid"  x)
    (**  *)
    let get_name_label ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.get_name_label" [ session_id; self ] >>= fun x -> return (From.string "return value of GPU_group.get_name_label"  x)
    (**  *)
    let get_name_description ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.get_name_description" [ session_id; self ] >>= fun x -> return (From.string "return value of GPU_group.get_name_description"  x)
    (**  *)
    let get_PGPUs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.get_PGPUs" [ session_id; self ] >>= fun x -> return (From.ref_PGPU_set "return value of GPU_group.get_PGPUs"  x)
    (**  *)
    let get_VGPUs ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.get_VGPUs" [ session_id; self ] >>= fun x -> return (From.ref_VGPU_set "return value of GPU_group.get_VGPUs"  x)
    (**  *)
    let get_GPU_types ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.get_GPU_types" [ session_id; self ] >>= fun x -> return (From.string_set "return value of GPU_group.get_GPU_types"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of GPU_group.get_other_config"  x)
    (**  *)
    let set_name_label ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      let value = To.string value in
      
      rpc_wrapper rpc "GPU_group.set_name_label" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_name_description ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      let value = To.string value in
      
      rpc_wrapper rpc "GPU_group.set_name_description" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "GPU_group.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "GPU_group.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      let key = To.string key in
      
      rpc_wrapper rpc "GPU_group.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~name_label ~name_description ~other_config =
      let session_id = To.ref_session session_id in
      let name_label = To.string name_label in
      let name_description = To.string name_description in
      let other_config = To.string_to_string_map other_config in
      
      rpc_wrapper rpc "GPU_group.create" [ session_id; name_label; name_description; other_config ] >>= fun x -> return (From.ref_GPU_group "return value of GPU_group.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_GPU_group self in
      
      rpc_wrapper rpc "GPU_group.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "GPU_group.get_all" [ session_id ] >>= fun x -> return (From.ref_GPU_group_set "return value of GPU_group.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "GPU_group.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_GPU_group_to_gPU_group_t_map "return value of GPU_group.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "GPU_group.get_all_records" [ session_id ] >>= fun x -> return (From.ref_GPU_group_to_gPU_group_t_map "return value of GPU_group.get_all_records"  x)
  end
  module VGPU = struct
    (**  *)
    let get_record ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      
      rpc_wrapper rpc "VGPU.get_record" [ session_id; self ] >>= fun x -> return (From.vGPU_t "return value of VGPU.get_record"  x)
    (**  *)
    let get_by_uuid ~rpc ~session_id ~uuid =
      let session_id = To.ref_session session_id in
      let uuid = To.string uuid in
      
      rpc_wrapper rpc "VGPU.get_by_uuid" [ session_id; uuid ] >>= fun x -> return (From.ref_VGPU "return value of VGPU.get_by_uuid"  x)
    (**  *)
    let get_uuid ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      
      rpc_wrapper rpc "VGPU.get_uuid" [ session_id; self ] >>= fun x -> return (From.string "return value of VGPU.get_uuid"  x)
    (**  *)
    let get_VM ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      
      rpc_wrapper rpc "VGPU.get_VM" [ session_id; self ] >>= fun x -> return (From.ref_VM "return value of VGPU.get_VM"  x)
    (**  *)
    let get_GPU_group ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      
      rpc_wrapper rpc "VGPU.get_GPU_group" [ session_id; self ] >>= fun x -> return (From.ref_GPU_group "return value of VGPU.get_GPU_group"  x)
    (**  *)
    let get_device ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      
      rpc_wrapper rpc "VGPU.get_device" [ session_id; self ] >>= fun x -> return (From.string "return value of VGPU.get_device"  x)
    (**  *)
    let get_currently_attached ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      
      rpc_wrapper rpc "VGPU.get_currently_attached" [ session_id; self ] >>= fun x -> return (From.bool "return value of VGPU.get_currently_attached"  x)
    (**  *)
    let get_other_config ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      
      rpc_wrapper rpc "VGPU.get_other_config" [ session_id; self ] >>= fun x -> return (From.string_to_string_map "return value of VGPU.get_other_config"  x)
    (**  *)
    let set_other_config ~rpc ~session_id ~self ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      let value = To.string_to_string_map value in
      
      rpc_wrapper rpc "VGPU.set_other_config" [ session_id; self; value ] >>= fun x -> return (ignore x)
    (**  *)
    let add_to_other_config ~rpc ~session_id ~self ~key ~value =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      let key = To.string key in
      let value = To.string value in
      
      rpc_wrapper rpc "VGPU.add_to_other_config" [ session_id; self; key; value ] >>= fun x -> return (ignore x)
    (**  *)
    let remove_from_other_config ~rpc ~session_id ~self ~key =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      let key = To.string key in
      
      rpc_wrapper rpc "VGPU.remove_from_other_config" [ session_id; self; key ] >>= fun x -> return (ignore x)
    (**  *)
    let create ~rpc ~session_id ~vM ~gPU_group ~device ~other_config =
      let session_id = To.ref_session session_id in
      let vM = To.ref_VM vM in
      let gPU_group = To.ref_GPU_group gPU_group in
      let device = To.string device in
      let other_config = To.string_to_string_map other_config in
      
      rpc_wrapper rpc "VGPU.create" [ session_id; vM; gPU_group; device; other_config ] >>= fun x -> return (From.ref_VGPU "return value of VGPU.create"  x)
    (**  *)
    let destroy ~rpc ~session_id ~self =
      let session_id = To.ref_session session_id in
      let self = To.ref_VGPU self in
      
      rpc_wrapper rpc "VGPU.destroy" [ session_id; self ] >>= fun x -> return (ignore x)
    (**  *)
    let get_all ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VGPU.get_all" [ session_id ] >>= fun x -> return (From.ref_VGPU_set "return value of VGPU.get_all"  x)
    (**  *)
    let get_all_records_where ~rpc ~session_id ~expr =
      let session_id = To.ref_session session_id in
      let expr = To.string expr in
      
      rpc_wrapper rpc "VGPU.get_all_records_where" [ session_id; expr ] >>= fun x -> return (From.ref_VGPU_to_vGPU_t_map "return value of VGPU.get_all_records_where"  x)
    (**  *)
    let get_all_records ~rpc ~session_id =
      let session_id = To.ref_session session_id in
      
      rpc_wrapper rpc "VGPU.get_all_records" [ session_id ] >>= fun x -> return (From.ref_VGPU_to_vGPU_t_map "return value of VGPU.get_all_records"  x)
  end

end


module Id = struct type 'a t = 'a let bind x f = f x let return x = x end
module Client = ClientF(Id)
