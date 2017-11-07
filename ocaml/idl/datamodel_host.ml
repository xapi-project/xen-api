(* datamodel_host *)


open Datamodel_common
open Datamodel_roles
open Datamodel_types

let host_memory =
  let field = field ~ty:Int in
  [
    field ~qualifier:DynamicRO "overhead" "Virtualization memory overhead (bytes)." ~default_value:(Some (VInt 0L)) ~doc_tags:[Memory];
  ]

let api_version =
  let field' = field ~qualifier:DynamicRO in
  [
    field' ~ty:Int "major" "major version number";
    field' ~ty:Int "minor" "minor version number";
    field' ~ty:String "vendor" "identification of vendor";
    field' ~ty:(Map(String,String)) "vendor_implementation" "details of vendor implementation";
  ]

  let migrate_receive = call
      ~in_oss_since:None
      ~in_product_since:rel_tampa
      ~name:"migrate_receive"
      ~doc:"Prepare to receive a VM, returning a token which can be passed to VM.migrate."
      ~params:[Ref _host, "host", "The target host";
               Ref _network, "network", "The network through which migration traffic should be received.";
               Map(String, String), "options", "Extra configuration operations" ]
      ~result:(Map(String,String), "A value which should be passed to VM.migrate")
      ~allowed_roles:_R_VM_POWER_ADMIN
      ()

  let ha_disable_failover_decisions = call
      ~in_product_since:rel_orlando
      ~name:"ha_disable_failover_decisions"
      ~doc:"Prevents future failover decisions happening on this node. This function should only be used as part of a controlled shutdown of the HA system."
      ~params:[Ref _host, "host", "The Host to disable failover decisions for"]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let ha_disarm_fencing = call
      ~in_product_since:rel_orlando
      ~name:"ha_disarm_fencing"
      ~doc:"Disarms the fencing function of the HA subsystem. This function is extremely dangerous and should only be used as part of a controlled shutdown of the HA system."
      ~params:[Ref _host, "host", "The Host to disarm"]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let ha_stop_daemon = call
      ~in_product_since:rel_orlando
      ~name:"ha_stop_daemon"
      ~doc:"Stops the HA daemon. This function is extremely dangerous and should only be used as part of a controlled shutdown of the HA system."
      ~params:[Ref _host, "host", "The Host whose daemon should be stopped"]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let ha_release_resources = call
      ~in_product_since:rel_orlando
      ~name:"ha_release_resources"
      ~doc:"Cleans up any resources on the host associated with this HA instance."
      ~params:[Ref _host, "host", "The Host whose resources should be cleaned up"]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let local_assert_healthy = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"local_assert_healthy"
      ~doc:"Returns nothing if this host is healthy, otherwise it throws an error explaining why the host is unhealthy"
      ~params:[]
      ~pool_internal:true
      ~hide_from_docs:true
      ~errs:[ Api_errors.host_still_booting;
              Api_errors.host_has_no_management_ip;
              Api_errors.host_master_cannot_talk_back;
              Api_errors.host_unknown_to_master;
              Api_errors.host_broken;
              Api_errors.license_restriction;
              Api_errors.license_does_not_support_pooling;
              Api_errors.ha_should_be_fenced;
            ]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let preconfigure_ha = call
      ~in_product_since:rel_miami
      ~name:"preconfigure_ha"
      ~doc:"Attach statefiles, generate config files but do not start the xHA daemon."
      ~params:[Ref _host, "host", "The Host to modify";
               Set(Ref _vdi), "statefiles", "Set of statefile VDIs to use";
               Ref _vdi, "metadata_vdi", "VDI to use for Pool metadata";
               String, "generation", "UUID identifying this HA instance";
              ]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let ha_join_liveset = call
      ~in_product_since:rel_orlando
      ~name:"ha_join_liveset"
      ~doc:"Block until this host joins the liveset."
      ~params:[Ref _host, "host", "The Host whose HA daemon to start"]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let ha_wait_for_shutdown_via_statefile = call
      ~in_product_since:rel_orlando
      ~name:"ha_wait_for_shutdown_via_statefile"
      ~doc:"Block until this host xHA daemon exits after having seen the invalid statefile. If the host loses statefile access then throw an exception"
      ~params:[Ref _host, "host", "The Host whose HA subsystem to query"]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()
(*
let host_query_ha = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"query_ha"
  ~doc:"Return the local HA configuration as seen by this host"
  ~params:[]
  ~custom_marshaller:true
  ~pool_internal:true
  ~hide_from_docs:true
  ()
*)
  let request_backup = call ~flags:[`Session]
      ~name:"request_backup"
      ~in_product_since:rel_rio
      ~doc:"Request this host performs a database backup"
      ~params:[Ref _host, "host", "The Host to send the request to";
               Int, "generation", "The generation count of the master's database";
               Bool, "force", "If this is true then the client _has_ to take a backup, otherwise it's just an 'offer'"
              ]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let request_config_file_sync = call ~flags:[`Session]
      ~name:"request_config_file_sync"
      ~in_product_since:rel_rio
      ~doc:"Request this host syncs dom0 config files"
      ~params:[Ref _host, "host", "The Host to send the request to";
               String, "hash", "The hash of the master's dom0 config files package"
              ]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()


  (* Since there are no async versions, no tasks are generated (!) this is important
     otherwise the call would block doing a Db.Task.create *)
  let propose_new_master = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"propose_new_master"
      ~doc:"First phase of a two-phase commit protocol to set the new master. If the host has already committed to another configuration or if the proposed new master is not in this node's membership set then the call will return an exception."
      ~params:[String, "address", "The address of the Host which is proposed as the new master";
               Bool, "manual", "True if this call is being invoked by the user manually, false if automatic";
              ]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let abort_new_master = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"abort_new_master"
      ~doc:"Causes the new master transaction to abort"
      ~params:[String, "address", "The address of the Host which is proposed as the new master"]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let commit_new_master = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"commit_new_master"
      ~doc:"Second phase of a two-phase commit protocol to set the new master."
      ~params:[String, "address", "The address of the Host which should be committed as the new master"]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let compute_free_memory = call
      ~in_product_since:rel_orlando
      ~name:"compute_free_memory"
      ~doc:"Computes the amount of free memory on the host."
      ~params:[Ref _host, "host", "The host to send the request to"]
      ~pool_internal:false
      ~hide_from_docs:false
      ~result:(Int, "the amount of free memory on the host.")
      ~allowed_roles:_R_READ_ONLY
      ~doc_tags:[Memory]
      ()

  let compute_memory_overhead = call
      ~in_product_since:rel_midnight_ride
      ~name:"compute_memory_overhead"
      ~doc:"Computes the virtualization memory overhead of a host."
      ~params:[Ref _host, "host", "The host for which to compute the memory overhead"]
      ~pool_internal:false
      ~hide_from_docs:false
      ~result:(Int, "the virtualization memory overhead of the host.")
      ~allowed_roles:_R_READ_ONLY
      ~doc_tags:[Memory]
      ()

  (* Diagnostics see if host is in emergency mode *)
  let is_in_emergency_mode = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"is_in_emergency_mode"
      ~doc:"Diagnostics call to discover if host is in emergency mode"
      ~params:[]
      ~pool_internal:false
      ~hide_from_docs:true
      ~result:(Bool, "true if host is in emergency mode")
      ~allowed_roles:_R_READ_ONLY
      ()

  (* Signal that the management IP address or hostname has been changed beneath us. *)
  let signal_networking_change = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"signal_networking_change"
      ~doc:"Signals that the management IP address or hostname has been changed beneath us."
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~doc_tags:[Networking]
      ()

  let notify = call
      ~in_product_since:rel_miami
      ~name:"notify"
      ~doc:"Notify an event"
      ~params:[String, "ty", "type of the notification";
               String, "params", "arguments of the notification (can be empty)"; ]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let syslog_reconfigure = call
      ~in_product_since:rel_miami
      ~name:"syslog_reconfigure"
      ~doc:"Re-configure syslog logging"
      ~params:[Ref _host, "host", "Tell the host to reread its Host.logging parameters and reconfigure itself accordingly"]
      ~allowed_roles:_R_POOL_OP
      ()

  let management_reconfigure = call
      ~in_product_since:rel_miami
      ~name:"management_reconfigure"
      ~doc:"Reconfigure the management network interface"
      ~params:[
        Ref _pif, "pif", "reference to a PIF object corresponding to the management interface";
      ]
      ~allowed_roles:_R_POOL_OP
      ~doc_tags:[Networking]
      ()

  let local_management_reconfigure = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"local_management_reconfigure"
      ~doc:"Reconfigure the management network interface. Should only be used if Host.management_reconfigure is impossible because the network configuration is broken."
      ~params:[
        String, "interface", "name of the interface to use as a management interface";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let ha_xapi_healthcheck = call ~flags:[`Session]
      ~in_product_since:rel_orlando
      ~name:"ha_xapi_healthcheck"
      ~doc:"Returns true if xapi appears to be functioning normally."
      ~result:(Bool, "true if xapi is functioning normally.")
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let management_disable = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"management_disable"
      ~doc:"Disable the management network interface"
      ~params:[]
      ~allowed_roles:_R_POOL_OP
      ~doc_tags:[Networking]
      ()

  let get_management_interface = call
      ~lifecycle:[Prototyped, rel_tampa, ""]
      ~name:"get_management_interface"
      ~doc:"Returns the management interface for the specified host"
      ~params:[Ref _host, "host", "Which host's management interface is required"]
      ~result:(Ref _pif, "The management interface for the host")
      ~allowed_roles:_R_POOL_OP
      ~doc_tags:[Networking]
      ()

  (* Simple host evacuate message for Miami.
     Not intended for HA *)

  let assert_can_evacuate = call
      ~in_product_since:rel_miami
      ~name:"assert_can_evacuate"
      ~doc:"Check this host can be evacuated."
      ~params:[Ref _host, "host", "The host to evacuate"]
      ~allowed_roles:_R_POOL_OP
      ()

  (* New Orlando message which aims to make the GUI less brittle (unexpected errors will trigger a VM suspend)
     and sensitive to HA planning constraints *)
  let get_vms_which_prevent_evacuation = call
      ~in_product_since:rel_orlando
      ~name:"get_vms_which_prevent_evacuation"
      ~doc:"Return a set of VMs which prevent the host being evacuated, with per-VM error codes"
      ~params:[Ref _host, "self", "The host to query"]
      ~result:(Map(Ref _vm, Set(String)), "VMs which block evacuation together with reasons")
      ~allowed_roles:_R_READ_ONLY
      ()

  let evacuate = call
      ~in_product_since:rel_miami
      ~name:"evacuate"
      ~doc:"Migrate all VMs off of this host, where possible."
      ~params:[Ref _host, "host", "The host to evacuate"]
      ~allowed_roles:_R_POOL_OP
      ()

  let get_uncooperative_resident_VMs = call
      ~in_product_since:rel_midnight_ride
      ~internal_deprecated_since:rel_tampa
      ~name:"get_uncooperative_resident_VMs"
      ~doc:"Return a set of VMs which are not co-operating with the host's memory control system"
      ~params:[Ref _host, "self", "The host to query"]
      ~result:((Set(Ref _vm)), "VMs which are not co-operating")
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_uncooperative_domains = call
      ~in_product_since:rel_midnight_ride
      ~internal_deprecated_since:rel_tampa
      ~name:"get_uncooperative_domains"
      ~doc:"Return the set of domain uuids which are not co-operating with the host's memory control system"
      ~params:[Ref _host, "self", "The host to query"]
      ~result:((Set(String)), "UUIDs of domains which are not co-operating")
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let retrieve_wlb_evacuate_recommendations = call
      ~name:"retrieve_wlb_evacuate_recommendations"
      ~in_product_since:rel_george
      ~doc:"Retrieves recommended host migrations to perform when evacuating the host from the wlb server. If a VM cannot be migrated from the host the reason is listed instead of a recommendation."
      ~params:[Ref _host, "self", "The host to query"]
      ~result:(Map(Ref _vm, Set(String)), "VMs and the reasons why they would block evacuation, or their target host recommended by the wlb server")
      ~allowed_roles:_R_READ_ONLY
      ()

  (* Host.Disable *)

  let disable = call
      ~in_product_since:rel_rio
      ~name:"disable"
      ~doc:"Puts the host into a state in which no new VMs can be started. Currently active VMs on the host continue to execute."
      ~params:[Ref _host, "host", "The Host to disable"]
      ~allowed_roles:_R_POOL_OP
      ()

  (* Host.Enable *)

  let enable = call
      ~name:"enable"
      ~in_product_since:rel_rio
      ~doc:"Puts the host into a state in which new VMs can be started."
      ~params:[Ref _host, "host", "The Host to enable"]
      ~allowed_roles:_R_POOL_OP
      ()

  (* Host.Shutdown *)

  let shutdown = call
      ~name:"shutdown"
      ~in_product_since:rel_rio
      ~doc:"Shutdown the host. (This function can only be called if there are no currently running VMs on the host and it is disabled.)"
      ~params:[Ref _host, "host", "The Host to shutdown"]
      ~allowed_roles:_R_POOL_OP
      ()

  (* Host.reboot *)

  let reboot = call
      ~name:"reboot"
      ~in_product_since:rel_rio
      ~doc:"Reboot the host. (This function can only be called if there are no currently running VMs on the host and it is disabled.)"
      ~params:[Ref _host, "host", "The Host to reboot"]
      ~allowed_roles:_R_POOL_OP
      ()

  (* Host.power_on *)

  let power_on = call
      ~name:"power_on"
      ~in_product_since:rel_orlando
      ~doc:"Attempt to power-on the host (if the capability exists)."
      ~params:[Ref _host, "host", "The Host to power on"]
      ~allowed_roles:_R_POOL_OP
      ()

  let restart_agent = call
      ~name:"restart_agent"
      ~in_product_since:rel_rio
      ~doc:"Restarts the agent after a 10 second pause. WARNING: this is a dangerous operation. Any operations in progress will be aborted, and unrecoverable data loss may occur. The caller is responsible for ensuring that there are no operations in progress when this method is called."
      ~params:[Ref _host, "host", "The Host on which you want to restart the agent"]
      ~allowed_roles:_R_POOL_OP
      ()

  let shutdown_agent = call
      ~name:"shutdown_agent"
      ~in_product_since:rel_orlando
      ~doc:"Shuts the agent down after a 10 second pause. WARNING: this is a dangerous operation. Any operations in progress will be aborted, and unrecoverable data loss may occur. The caller is responsible for ensuring that there are no operations in progress when this method is called."
      ~params:[]
      ~flags:[`Session] (* no async *)
      ~allowed_roles:_R_POOL_OP
      ()

  let dmesg = call
      ~name:"dmesg"
      ~in_product_since:rel_rio
      ~doc:"Get the host xen dmesg."
      ~params:[Ref _host, "host", "The Host to query"]
      ~result:(String, "dmesg string")
      ~allowed_roles:_R_POOL_OP
      ()

  let dmesg_clear = call
      ~name:"dmesg_clear"
      ~in_product_since:rel_rio
      ~doc:"Get the host xen dmesg, and clear the buffer."
      ~params:[Ref _host, "host", "The Host to query"]
      ~result:(String, "dmesg string")
      ~allowed_roles:_R_POOL_OP
      ()

  let get_log = call
      ~name:"get_log"
      ~in_product_since:rel_rio
      ~doc:"Get the host's log file"
      ~params:[Ref _host, "host", "The Host to query"]
      ~result:(String, "The contents of the host's primary log file")
      ~allowed_roles:_R_READ_ONLY
      ()

  let send_debug_keys = call
      ~name:"send_debug_keys"
      ~in_product_since:rel_rio
      ~doc:"Inject the given string as debugging keys into Xen"
      ~params:[Ref _host, "host", "The host";
               String, "keys", "The keys to send"]
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let get_data_sources = call
      ~name:"get_data_sources"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~doc:""
      ~result:(Set (Record _data_source), "A set of data sources")
      ~params:[Ref _host, "host", "The host to interrogate"]
      ~errs:[]
      ~flags:[`Session]
      ~allowed_roles:_R_READ_ONLY
      ()

  let record_data_source = call
      ~name:"record_data_source"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~doc:"Start recording the specified data source"
      ~params:[Ref _host, "host", "The host";
               String, "data_source", "The data source to record"]
      ~errs:[]
      ~flags:[`Session]
      ~allowed_roles:_R_POOL_OP
      ()

  let query_data_source = call
      ~name:"query_data_source"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~doc:"Query the latest value of the specified data source"
      ~params:[Ref _host, "host", "The host";
               String, "data_source", "The data source to query"]
      ~result:(Float,"The latest value, averaged over the last 5 seconds")
      ~errs:[]
      ~flags:[`Session]
      ~allowed_roles:_R_READ_ONLY
      ()

  let attach_static_vdis = call
      ~name:"attach_static_vdis"
      ~in_product_since:rel_midnight_ride
      ~doc:"Statically attach VDIs on a host."
      ~params:[Ref _host, "host", "The Host to modify";
               Map(Ref _vdi, String), "vdi_reason_map", "List of VDI+reason pairs to attach"
              ]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let detach_static_vdis = call
      ~name:"detach_static_vdis"
      ~in_product_since:rel_midnight_ride
      ~doc:"Detach static VDIs from a host."
      ~params:[Ref _host, "host", "The Host to modify";
               Set(Ref _vdi), "vdis", "Set of VDIs to detach";
              ]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let declare_dead = call
      ~name:"declare_dead"
      ~in_product_since:rel_clearwater
      ~doc:"Declare that a host is dead. This is a dangerous operation, and should only be called if the administrator is absolutely sure the host is definitely dead"
      ~params:[Ref _host, "host", "The Host to declare is dead"]
      ~allowed_roles:_R_POOL_OP
      ()

  let forget_data_source_archives = call
      ~name:"forget_data_source_archives"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~doc:"Forget the recorded statistics related to the specified data source"
      ~params:[Ref _host, "host", "The host";
               String, "data_source", "The data source whose archives are to be forgotten"]
      ~flags:[`Session]
      ~allowed_roles:_R_POOL_OP
      ()

  let get_diagnostic_timing_stats = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"get_diagnostic_timing_stats"
      ~doc:"Return timing statistics for diagnostic purposes"
      ~params:[Ref _host, "host", "The host to interrogate"]
      ~result:(Map(String, String), "population name to summary map")
      ~hide_from_docs:true
      ~allowed_roles:_R_READ_ONLY
      ()

  let create_new_blob = call
      ~name: "create_new_blob"
      ~in_product_since:rel_orlando
      ~doc:"Create a placeholder for a named binary blob of data that is associated with this host"
      ~versioned_params:
        [{param_type=Ref _host; param_name="host"; param_doc="The host"; param_release=orlando_release; param_default=None};
         {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
         {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
         {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}]
      ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
      ~allowed_roles:_R_POOL_OP
      ()

  let call_plugin = call
      ~name:"call_plugin"
      ~in_product_since:rel_orlando
      ~doc:"Call a XenAPI plugin on this host"
      ~params:[Ref _host, "host", "The host";
               String, "plugin", "The name of the plugin";
               String, "fn", "The name of the function within the plugin";
               Map(String, String), "args", "Arguments for the function";]
      ~result:(String, "Result from the plugin")
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let has_extension = call
      ~name:"has_extension"
      ~in_product_since:rel_ely
      ~doc:"Return true if the extension is available on the host"
      ~params:[Ref _host, "host", "The host";
               String, "name", "The name of the API call";]
      ~result:(Bool, "True if the extension exists, false otherwise")
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let call_extension = call
      ~name:"call_extension"
      ~in_product_since:rel_ely
      ~custom_marshaller:true
      ~doc:"Call a XenAPI extension on this host"
      ~params:[Ref _host, "host", "The host";
               String, "call", "Rpc call for the extension";]
      ~result:(String, "Result from the extension")
      ~allowed_roles:_R_POOL_ADMIN
      ~flags:[`Session] (* no async *)
      ()

  let enable_binary_storage = call
      ~name:"enable_binary_storage"
      ~in_product_since:rel_orlando
      ~hide_from_docs:true
      ~pool_internal:true
      ~doc:"Enable binary storage on a particular host, for storing RRDs, messages and blobs"
      ~params:[Ref _host, "host", "The host"]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let disable_binary_storage = call
      ~name:"disable_binary_storage"
      ~in_product_since:rel_orlando
      ~hide_from_docs:true
      ~pool_internal:true
      ~doc:"Disable binary storage on a particular host, deleting stored RRDs, messages and blobs"
      ~params:[Ref _host, "host", "The host"]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let update_pool_secret = call
      ~name:"update_pool_secret"
      ~in_product_since:rel_midnight_ride
      ~hide_from_docs:true
      ~pool_internal:true
      ~doc:""
      ~params:[
        Ref _host, "host", "The host";
        String, "pool_secret", "The new pool secret" ]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let update_master = call
      ~name:"update_master"
      ~in_product_since:rel_midnight_ride
      ~hide_from_docs:true
      ~pool_internal:true
      ~doc:""
      ~params:[
        Ref _host, "host", "The host";
        String, "master_address", "The new master address" ]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let set_localdb_key = call
      ~name:"set_localdb_key"
      ~in_product_since:rel_midnight_ride
      ~doc:"Set a key in the local DB of the host."
      ~params:[Ref _host, "host", "The Host to modify";
               String, "key", "Key to change";
               String, "value", "Value to set"
              ]
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let refresh_pack_info = call
      ~name:"refresh_pack_info"
      ~doc:"Refresh the list of installed Supplemental Packs."
      ~params:[Ref _host, "host", "The Host to modify"]
      ~allowed_roles:_R_POOL_OP
      ~lifecycle:[Published, rel_midnight_ride, "";
                  Deprecated, rel_ely, "Use Pool_update.resync_host instead"]
      ()

  let bugreport_upload = call
      ~name:"bugreport_upload"
      ~doc:"Run xen-bugtool --yestoall and upload the output to support"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[ Ref _host, "host", "The host on which to run xen-bugtool";
                String, "url", "The URL to upload to";
                Map(String, String), "options", "Extra configuration operations" ]
      ~allowed_roles:_R_POOL_OP
      ()

  let list_methods = call
      ~name:"list_methods"
      ~in_product_since:rel_rio
      ~flags: [`Session]
      ~doc:"List all supported methods"
      ~params:[]
      ~result:(Set(String), "The name of every supported method.")
      ~allowed_roles:_R_READ_ONLY
      ()

  let license_apply = call
      ~name:"license_apply"
      ~in_oss_since:None
      ~lifecycle:[
        Published, rel_rio, "Apply a new license to a host";
        Removed, rel_clearwater, "Free licenses no longer handled by xapi";
      ]
      ~params:[Ref _host, "host", "The host to upload the license to";
               String, "contents", "The contents of the license file, base64 encoded"]
      ~doc:"Apply a new license to a host"
      ~errs: [Api_errors.license_processing_error]
      ~allowed_roles:_R_POOL_OP
      ()

  let license_add = call
      ~name:"license_add"
      ~in_oss_since:None
      ~lifecycle:[
        Published, rel_indigo, "Functionality for parsing license files re-added";
      ]
      ~params:[Ref _host, "host", "The host to upload the license to";
               String, "contents", "The contents of the license file, base64 encoded"]
      ~doc:"Apply a new license to a host"
      ~errs: [Api_errors.license_processing_error]
      ~allowed_roles:_R_POOL_OP
      ()

  let license_remove = call
      ~name:"license_remove"
      ~in_oss_since:None
      ~lifecycle:[
        Published, rel_indigo, "";
      ]
      ~params:[
        Ref _host, "host", "The host from which any license will be removed"
      ]
      ~doc:"Remove any license file from the specified host, and switch that host to the unlicensed edition"
      ~allowed_roles:_R_POOL_OP
      ()

  let create_params =
    [
      {param_type=String; param_name="uuid"; param_doc="unique identifier/object reference"; param_release=rio_release; param_default=None};
      {param_type=String; param_name="name_label"; param_doc="The name of the new storage repository"; param_release=rio_release; param_default=None};
      {param_type=String; param_name="name_description"; param_doc="The description of the new storage repository"; param_release=rio_release; param_default=None};
      {param_type=String; param_name="hostname"; param_doc="Hostname"; param_release=rio_release; param_default=None};
      {param_type=String; param_name="address"; param_doc="An address by which this host can be contacted by other members in its pool"; param_release=rio_release; param_default=None};
      {param_type=String; param_name="external_auth_type"; param_doc="type of external authentication service configured; empty if none configured"; param_release=george_release; param_default=Some(VString "")};
      {param_type=String; param_name="external_auth_service_name"; param_doc="name of external authentication service configured; empty if none configured"; param_release=george_release; param_default=Some(VString "")};
      {param_type=Map(String,String); param_name="external_auth_configuration"; param_doc="configuration specific to external authentication service"; param_release=george_release; param_default=Some(VMap [])};
      {param_type=Map(String,String); param_name="license_params"; param_doc="State of the current license"; param_release=midnight_ride_release; param_default=Some(VMap [])};
      {param_type=String; param_name="edition"; param_doc="Product edition"; param_release=midnight_ride_release; param_default=Some(VString "")};
      {param_type=Map(String,String); param_name="license_server"; param_doc="Contact information of the license server"; param_release=midnight_ride_release; param_default=Some(VMap [VString "address", VString "localhost"; VString "port", VString "27000"])};
      {param_type=Ref _sr; param_name="local_cache_sr"; param_doc="The SR that is used as a local cache"; param_release=cowley_release; param_default=(Some (VRef null_ref))};
      {param_type=Map(String,String); param_name="chipset_info"; param_doc="Information about chipset features"; param_release=boston_release; param_default=Some(VMap [])};
      {param_type=Bool; param_name="ssl_legacy"; param_doc="Allow SSLv3 protocol and ciphersuites as used by older XenServers. This controls both incoming and outgoing connections."; param_release=dundee_release; param_default=Some (VBool true)};
    ]

  let create = call
      ~name:"create"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~versioned_params:create_params
      ~doc:"Create a new host record"
      ~result:(Ref _host, "Reference to the newly created host object.")
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~doc:"Destroy specified host record in database"
      ~params:[(Ref _host, "self", "The host record to remove")]
      ~allowed_roles:_R_POOL_OP
      ()

  let get_system_status_capabilities = call ~flags:[`Session]
      ~name:"get_system_status_capabilities"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[Ref _host, "host", "The host to interrogate"]
      ~doc:""
      ~result:(String, "An XML fragment containing the system status capabilities.")
      ~allowed_roles:_R_READ_ONLY
      ()

  let set_hostname_live = call ~flags:[`Session]
      ~name:"set_hostname_live"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[Ref _host, "host", "The host whose host name to set";
               String, "hostname", "The new host name"]
      ~errs:[Api_errors.host_name_invalid]
      ~doc:"Sets the host name to the specified string.  Both the API and lower-level system hostname are changed immediately."
      ~allowed_roles:_R_POOL_OP
      ()

  let tickle_heartbeat = call ~flags:[`Session]
      ~name:"tickle_heartbeat"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~params:[Ref _host, "host", "The host calling the function, and whose heartbeat to tickle";
               Map(String, String), "stuff", "Anything else we want to let the master know";
              ]
      ~result:(Map(String, String), "Anything the master wants to tell the slave")
      ~doc:"Needs to be called every 30 seconds for the master to believe the host is alive"
      ~pool_internal:true
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let sync_data = call ~flags:[`Session]
      ~name:"sync_data"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~params:[Ref _host, "host", "The host to whom the data should be sent"]
      ~doc:"This causes the synchronisation of the non-database data (messages, RRDs and so on) stored on the master to be synchronised with the host"
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let backup_rrds = call ~flags:[`Session]
      ~name:"backup_rrds"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~params:[Ref _host, "host", "Schedule a backup of the RRDs of this host";
               Float, "delay", "Delay in seconds from when the call is received to perform the backup"]
      ~doc:"This causes the RRDs to be backed up to the master"
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let get_servertime = call ~flags:[`Session]
      ~name:"get_servertime"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~params:[Ref _host, "host", "The host whose clock should be queried"]
      ~doc:"This call queries the host's clock for the current time"
      ~result:(DateTime, "The current time")
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_server_localtime = call ~flags:[`Session]
      ~name:"get_server_localtime"
      ~in_oss_since:None
      ~in_product_since:rel_cowley
      ~params:[Ref _host, "host", "The host whose clock should be queried"]
      ~doc:"This call queries the host's clock for the current time in the host's local timezone"
      ~result:(DateTime, "The current local time")
      ~allowed_roles:_R_READ_ONLY
      ()

  let emergency_ha_disable = call ~flags:[`Session]
      ~name:"emergency_ha_disable"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~versioned_params:
        [{param_type=Bool; param_name="soft"; param_doc="Disable HA temporarily, revert upon host reboot or further changes, idempotent"; param_release=ely_release; param_default=Some(VBool false)};
        ]
      ~doc:"This call disables HA on the local host. This should only be used with extreme care."
      ~allowed_roles:_R_POOL_OP
      ()

  let certificate_install = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~pool_internal:true
      ~hide_from_docs:true
      ~name:"certificate_install"
      ~doc:"Install an SSL certificate to this host."
      ~params:[Ref _host, "host", "The host";
               String, "name", "A name to give the certificate";
               String, "cert", "The certificate"]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let certificate_uninstall = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~pool_internal:true
      ~hide_from_docs:true
      ~name:"certificate_uninstall"
      ~doc:"Remove an SSL certificate from this host."
      ~params:[Ref _host, "host", "The host";
               String, "name", "The certificate name"]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let certificate_list = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~pool_internal:true
      ~hide_from_docs:true
      ~name:"certificate_list"
      ~doc:"List all installed SSL certificates."
      ~params:[Ref _host, "host", "The host"]
      ~result:(Set(String),"All installed certificates")
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let crl_install = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~pool_internal:true
      ~hide_from_docs:true
      ~name:"crl_install"
      ~doc:"Install an SSL certificate revocation list to this host."
      ~params:[Ref _host, "host", "The host";
               String, "name", "A name to give the CRL";
               String, "crl", "The CRL"]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let crl_uninstall = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~pool_internal:true
      ~hide_from_docs:true
      ~name:"crl_uninstall"
      ~doc:"Remove an SSL certificate revocation list from this host."
      ~params:[Ref _host, "host", "The host";
               String, "name", "The CRL name"]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let crl_list = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~pool_internal:true
      ~hide_from_docs:true
      ~name:"crl_list"
      ~doc:"List all installed SSL certificate revocation lists."
      ~params:[Ref _host, "host", "The host"]
      ~result:(Set(String),"All installed CRLs")
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let certificate_sync = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~pool_internal:true
      ~hide_from_docs:true
      ~name:"certificate_sync"
      ~doc:"Resync installed SSL certificates and CRLs."
      ~params:[Ref _host, "host", "The host"]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let get_server_certificate = call
      ~in_oss_since:None
      ~lifecycle:[Published, rel_george, ""; Changed, rel_inverness, "Now available to all RBAC roles."]
      ~name:"get_server_certificate"
      ~doc:"Get the installed server public TLS certificate."
      ~params:[Ref _host, "host", "The host"]
      ~result:(String,"The installed server public TLS certificate, in PEM form.")
      ~allowed_roles:_R_READ_ONLY
      ()

  let display =
    Enum ("host_display", [
        "enabled", "This host is outputting its console to a physical display device";
        "disable_on_reboot", "The host will stop outputting its console to a physical display device on next boot";
        "disabled", "This host is not outputting its console to a physical display device";
        "enable_on_reboot", "The host will start outputting its console to a physical display device on next boot";
      ])

  let operations =
    Enum ("host_allowed_operations",
          [ "provision", "Indicates this host is able to provision another VM";
            "evacuate", "Indicates this host is evacuating";
            "shutdown", "Indicates this host is in the process of shutting itself down";
            "reboot", "Indicates this host is in the process of rebooting";
            "power_on", "Indicates this host is in the process of being powered on";
            "vm_start", "This host is starting a VM";
            "vm_resume", "This host is resuming a VM";
            "vm_migrate", "This host is the migration target of a VM";
          ])

  let enable_external_auth = call ~flags:[`Session]
      ~name:"enable_external_auth"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~params:[
        Ref _host, "host", "The host whose external authentication should be enabled";
        Map (String,String), "config", "A list of key-values containing the configuration data" ;
        String, "service_name", "The name of the service" ;
        String, "auth_type", "The type of authentication (e.g. AD for Active Directory)"
      ]
      ~doc:"This call enables external authentication on a host"
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let disable_external_auth = call ~flags:[`Session]
      ~name:"disable_external_auth"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~versioned_params:[
        {param_type=Ref _host; param_name="host"; param_doc="The host whose external authentication should be disabled"; param_release=george_release; param_default=None};
        {param_type=Map (String, String); param_name="config"; param_doc="Optional parameters as a list of key-values containing the configuration data"; param_release=george_release; param_default=Some (VMap [])}
      ]
      ~doc:"This call disables external authentication on the local host"
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let set_license_params = call
      ~name:"set_license_params"
      ~in_product_since:rel_orlando (* actually update 3 aka floodgate *)
      ~doc:"Set the new license details in the database, trigger a recomputation of the pool SKU"
      ~params:[
        Ref _host, "self", "The host";
        Map(String, String), "value", "The license_params"
      ]
      ~hide_from_docs:true
      ~pool_internal:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let apply_edition = call ~flags:[`Session]
      ~name:"apply_edition"
      ~in_product_since:rel_midnight_ride
      ~doc:"Change to another edition, or reactivate the current edition after a license has expired. This may be subject to the successful checkout of an appropriate license."
      ~versioned_params:[
        {param_type=Ref _host; param_name="host"; param_doc="The host"; param_release=midnight_ride_release; param_default=None};
        {param_type=String; param_name="edition"; param_doc="The requested edition"; param_release=midnight_ride_release; param_default=None};
        {param_type=Bool; param_name="force"; param_doc="Update the license params even if the apply call fails"; param_release=clearwater_release; param_default=Some (VBool false)};
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_power_on_mode = call
      ~name:"set_power_on_mode"
      ~in_product_since:rel_midnight_ride
      ~doc:"Set the power-on-mode, host, user and password "
      ~params:[
        Ref _host, "self", "The host";
        String, "power_on_mode", "power-on-mode can be empty,iLO,wake-on-lan, DRAC or other";
        Map(String, String), "power_on_config", "Power on config";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_ssl_legacy = call
      ~name:"set_ssl_legacy"
      ~lifecycle:[Published, rel_dundee, ""]
      ~doc:"Enable/disable SSLv3 for interoperability with older versions of XenServer. When this is set to a different value, the host immediately restarts its SSL/TLS listening service; typically this takes less than a second but existing connections to it will be broken. XenAPI login sessions will remain valid."
      ~params:[
        Ref _host, "self", "The host";
        Bool, "value", "True to allow SSLv3 and ciphersuites as used in old XenServer versions";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_cpu_features = call ~flags:[`Session]
      ~name:"set_cpu_features"
      ~in_product_since:rel_midnight_ride
      ~doc:"Set the CPU features to be used after a reboot, if the given features string is valid."
      ~params:[
        Ref _host, "host", "The host";
        String, "features", "The features string (32 hexadecimal digits)"
      ]
      ~allowed_roles:_R_POOL_OP
      ~lifecycle:[Published, rel_midnight_ride, ""; Removed, rel_dundee, "Manual CPU feature setting was removed"]
      ()

  let reset_cpu_features = call ~flags:[`Session]
      ~name:"reset_cpu_features"
      ~in_product_since:rel_midnight_ride
      ~doc:"Remove the feature mask, such that after a reboot all features of the CPU are enabled."
      ~params:[
        Ref _host, "host", "The host"
      ]
      ~allowed_roles:_R_POOL_OP
      ~lifecycle:[Published, rel_midnight_ride, ""; Removed, rel_dundee, "Manual CPU feature setting was removed"]
      ()

  let reset_networking = call
      ~name:"reset_networking"
      ~lifecycle:[]
      ~doc:"Purge all network-related metadata associated with the given host."
      ~params:[Ref _host, "host", "The Host to modify"]
      ~allowed_roles:_R_POOL_OP
      ~hide_from_docs:true
      ()

  let enable_local_storage_caching = call ~flags:[`Session]
      ~name:"enable_local_storage_caching"
      ~in_product_since:rel_cowley
      ~doc:"Enable the use of a local SR for caching purposes"
      ~params:[
        Ref _host, "host", "The host";
        Ref _sr, "sr", "The SR to use as a local cache"
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_local_storage_caching = call ~flags:[`Session]
      ~name:"disable_local_storage_caching"
      ~in_product_since:rel_cowley
      ~doc:"Disable the use of a local SR for caching purposes"
      ~params:[
        Ref _host, "host", "The host"
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let get_sm_diagnostics = call ~flags:[`Session]
      ~name:"get_sm_diagnostics"
      ~in_product_since:rel_boston
      ~doc:"Return live SM diagnostics"
      ~params:[
        Ref _host, "host", "The host"
      ]
      ~result:(String, "Printable diagnostic data")
      ~allowed_roles:_R_POOL_OP
      ~hide_from_docs:true
      ()

  let get_thread_diagnostics = call ~flags:[`Session]
      ~name:"get_thread_diagnostics"
      ~in_product_since:rel_boston
      ~doc:"Return live thread diagnostics"
      ~params:[
        Ref _host, "host", "The host"
      ]
      ~result:(String, "Printable diagnostic data")
      ~allowed_roles:_R_POOL_OP
      ~hide_from_docs:true
      ()

  let sm_dp_destroy = call ~flags:[`Session]
      ~name:"sm_dp_destroy"
      ~in_product_since:rel_boston
      ~doc:"Attempt to cleanup and destroy a named SM datapath"
      ~params:[
        Ref _host, "host", "The host";
        String, "dp", "The datapath";
        Bool, "allow_leak", "If true, all records of the datapath will be removed even if the datapath could not be destroyed cleanly.";
      ]
      ~allowed_roles:_R_POOL_OP
      ~hide_from_docs:true
      ()

  let sync_vlans = call ~flags:[`Session]
      ~name:"sync_vlans"
      ~lifecycle:[]
      ~doc:"Synchronise VLANs on given host with the master's VLANs"
      ~params:[
        Ref _host, "host", "The host";
      ]
      ~hide_from_docs:true
      ~pool_internal:true
      ~allowed_roles:_R_POOL_OP
      ()

  let sync_tunnels = call ~flags:[`Session]
      ~name:"sync_tunnels"
      ~lifecycle:[]
      ~doc:"Synchronise tunnels on given host with the master's tunnels"
      ~params:[
        Ref _host, "host", "The host";
      ]
      ~hide_from_docs:true
      ~pool_internal:true
      ~allowed_roles:_R_POOL_OP
      ()

  let sync_pif_currently_attached = call ~flags:[`Session]
      ~name:"sync_pif_currently_attached"
      ~lifecycle:[]
      ~doc:"Synchronise tunnels on given host with the master's tunnels"
      ~params:[
        Ref _host, "host", "The host";
        Set String, "bridges", "A list of bridges that are currently up";
      ]
      ~hide_from_docs:true
      ~pool_internal:true
      ~allowed_roles:_R_POOL_OP
      ()

  let enable_display = call
      ~name:"enable_display"
      ~lifecycle:[Published, rel_cream, ""]
      ~doc:"Enable console output to the physical display device next time this host boots"
      ~params:[
        Ref _host, "host", "The host";
      ]
      ~result:(display, "This host's physical display usage")
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_display = call
      ~name:"disable_display"
      ~lifecycle:[Published, rel_cream, ""]
      ~doc:"Disable console output to the physical display device next time this host boots"
      ~params:[
        Ref _host, "host", "The host";
      ]
      ~result:(display, "This host's physical display usage")
      ~allowed_roles:_R_POOL_OP
      ()

  let apply_guest_agent_config = call
      ~name:"apply_guest_agent_config"
      ~lifecycle:[Published, rel_dundee, ""]
      ~doc:"Signal to the host that the pool-wide guest agent config has changed"
      ~params:[
        Ref _host, "host", "The host";
      ]
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let mxgpu_vf_setup = call
      ~name:"mxgpu_vf_setup"
      ~lifecycle:[Published, rel_falcon, ""]
      ~doc:"Ensure the driver (kernel module) for MxGPU is loaded on the host, and create PCI objects for any new PCI devices (virtual functions) that the module makes visible."
      ~params:[
        Ref _host, "host", "The host";
      ]
      ~hide_from_docs:true
      ~pool_internal:true
      ~allowed_roles:_R_VM_OP
      ()

  let allocate_resources_for_vm = call
      ~name:"allocate_resources_for_vm"
      ~lifecycle:[Published, rel_inverness, ""]
      ~doc:"Reserves the resources for a VM by setting the 'scheduled_to_be_resident_on' fields"
      ~params:[
        Ref _host, "self", "The host";
        Ref _vm, "vm", "The VM";
        Bool, "live", "Is this part of a live migration?"
      ]
      ~hide_from_docs:true
      ~allowed_roles:_R_VM_OP
      ()

  let set_iscsi_iqn = call
    ~name:"set_iscsi_iqn"
    ~lifecycle:[Published, rel_kolkata, ""]
    ~doc:"Sets the initiator IQN for the host"
    ~params:[
      Ref _host, "host", "The host";
      String, "value", "The value to which the IQN should be set"
    ]
    ~allowed_roles:_R_POOL_OP
    ()

  (** Hosts *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host ~descr:"A physical host" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages: [
        disable;
        enable;
        shutdown;
        reboot;
        dmesg;
        dmesg_clear;
        get_log;
        send_debug_keys;
        bugreport_upload;
        list_methods;
        license_apply;
        license_add;
        license_remove;
        create;
        destroy;
        power_on;
        set_license_params;
        emergency_ha_disable;
        ha_disarm_fencing;
        preconfigure_ha;
        ha_join_liveset;
        ha_disable_failover_decisions;
        ha_wait_for_shutdown_via_statefile;
        ha_stop_daemon;
        ha_release_resources;
        ha_xapi_healthcheck;
        local_assert_healthy;
        request_backup;
        request_config_file_sync;
        propose_new_master;
        commit_new_master;
        abort_new_master;
        get_data_sources;
        record_data_source;
        query_data_source;
        forget_data_source_archives;
        assert_can_evacuate;
        get_vms_which_prevent_evacuation;
        get_uncooperative_resident_VMs;
        get_uncooperative_domains;
        evacuate;
        signal_networking_change;
        notify;
        syslog_reconfigure;
        management_reconfigure;
        local_management_reconfigure;
        management_disable;
        get_management_interface;
        get_system_status_capabilities;
        get_diagnostic_timing_stats;
        restart_agent;
        shutdown_agent;
        set_hostname_live;
        is_in_emergency_mode;
        compute_free_memory;
        compute_memory_overhead;
        tickle_heartbeat;
        sync_data;
        backup_rrds;
        create_new_blob;
        call_plugin;
        has_extension;
        call_extension;
        get_servertime;
        get_server_localtime;
        enable_binary_storage;
        disable_binary_storage;
        enable_external_auth;
        disable_external_auth;
        retrieve_wlb_evacuate_recommendations;
        certificate_install;
        certificate_uninstall;
        certificate_list;
        crl_install;
        crl_uninstall;
        crl_list;
        certificate_sync;
        get_server_certificate;
        update_pool_secret;
        update_master;
        attach_static_vdis;
        detach_static_vdis;
        set_localdb_key;
        apply_edition;
        refresh_pack_info;
        set_power_on_mode;
        set_cpu_features;
        reset_cpu_features;
        reset_networking;
        enable_local_storage_caching;
        disable_local_storage_caching;
        get_sm_diagnostics;
        get_thread_diagnostics;
        sm_dp_destroy;
        sync_vlans;
        sync_tunnels;
        sync_pif_currently_attached;
        migrate_receive;
        declare_dead;
        enable_display;
        disable_display;
        set_ssl_legacy;
        apply_guest_agent_config;
        mxgpu_vf_setup;
        allocate_resources_for_vm;
        set_iscsi_iqn;
      ]
      ~contents:
        ([ uid _host;
           namespace ~name:"name" ~contents:(names None RW) ();
           namespace ~name:"memory" ~contents:host_memory ();
         ] @ (allowed_and_current_operations operations) @ [
           namespace ~name:"API_version" ~contents:api_version ();
           field ~qualifier:DynamicRO ~ty:Bool "enabled" "True if the host is currently enabled";
           field ~qualifier:StaticRO ~ty:(Map(String, String)) "software_version" "version strings";
           field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
           field ~qualifier:StaticRO ~ty:(Set(String)) "capabilities" "Xen capabilities";
           field ~qualifier:DynamicRO ~ty:(Map(String, String)) "cpu_configuration" "The CPU configuration on this host.  May contain keys such as \"nr_nodes\", \"sockets_per_node\", \"cores_per_socket\", or \"threads_per_core\"";
           field ~qualifier:DynamicRO ~ty:String "sched_policy" "Scheduler policy currently in force on this host";
           field ~qualifier:DynamicRO ~ty:(Set String) "supported_bootloaders" "a list of the bootloaders installed on the machine";
           field ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) "resident_VMs" "list of VMs currently resident on host";
           field ~qualifier:RW ~ty:(Map(String, String)) "logging" "logging configuration";
           field ~qualifier:DynamicRO ~ty:(Set (Ref _pif)) ~doc_tags:[Networking] "PIFs" "physical network interfaces";
           field ~qualifier:RW ~ty:(Ref _sr) "suspend_image_sr" "The SR in which VDIs for suspend images are created";
           field ~qualifier:RW ~ty:(Ref _sr) "crash_dump_sr" "The SR in which VDIs for crash dumps are created";
           field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host_crashdump)) "crashdumps" "Set of host crash dumps";
           field ~in_oss_since:None ~internal_deprecated_since:rel_ely ~qualifier:DynamicRO ~ty:(Set (Ref _host_patch)) "patches" "Set of host patches";
           field ~in_oss_since:None ~in_product_since:rel_ely ~qualifier:DynamicRO ~ty:(Set (Ref _pool_update)) "updates" "Set of updates";
           field ~qualifier:DynamicRO ~ty:(Set (Ref _pbd)) "PBDs" "physical blockdevices";
           field ~qualifier:DynamicRO ~ty:(Set (Ref _hostcpu)) "host_CPUs" "The physical CPUs on this host";
           field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "cpu_info" "Details about the physical CPUs on this host";
           field ~in_oss_since:None ~qualifier:RW ~ty:String ~doc_tags:[Networking] "hostname" "The hostname of this host";
           field ~in_oss_since:None ~qualifier:RW ~ty:String ~doc_tags:[Networking] "address" "The address by which this host can be contacted from any other host in the pool";
           field ~qualifier:DynamicRO ~ty:(Ref _host_metrics) "metrics" "metrics associated with this host";
           field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map (String,String)) "license_params" "State of the current license";
           field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Int "boot_free_mem" "Free memory on host at boot time";
           field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_statefiles" "The set of statefiles accessible from this host";
           field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_network_peers" "The set of hosts visible via the network from this host";
           field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String,Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this host";
           field ~writer_roles:_R_VM_OP ~qualifier:RW ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
           field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VString "")) ~ty:String "external_auth_type" "type of external authentication service configured; empty if none configured.";
           field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VString "")) ~ty:String "external_auth_service_name" "name of external authentication service configured; empty if none configured.";
           field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VMap [])) ~ty:(Map (String,String)) "external_auth_configuration" "configuration specific to external authentication service";
           field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~ty:String "edition" "Product edition";
           field ~qualifier:RW ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [VString "address", VString "localhost"; VString "port", VString "27000"])) ~ty:(Map (String, String)) "license_server" "Contact information of the license server";
           field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map (String,String)) "bios_strings" "BIOS strings";
           field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~ty:String "power_on_mode" "The power on mode";
           field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "power_on_config" "The power on config";
           field ~qualifier:StaticRO ~in_product_since:rel_cowley ~default_value:(Some (VRef null_ref)) ~ty:(Ref _sr) "local_cache_sr" "The SR that is used as a local cache";
           field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Map (String, String)) ~default_value:(Some (VMap []))
             "chipset_info" "Information about chipset features";
           field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _pci)) "PCIs" "List of PCI devices in the host";
           field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _pgpu)) "PGPUs" "List of physical GPUs in the host";
           field ~qualifier:DynamicRO ~lifecycle:[Published, rel_inverness, ""] ~ty:(Set (Ref _pusb)) "PUSBs" "List of physical USBs in the host";
           field ~qualifier:StaticRO ~lifecycle:[Published, rel_dundee, ""] ~ty:Bool ~default_value:(Some (VBool true)) "ssl_legacy" "Allow SSLv3 protocol and ciphersuites as used by older XenServers. This controls both incoming and outgoing connections. When this is set to a different value, the host immediately restarts its SSL/TLS listening service; typically this takes less than a second but existing connections to it will be broken. XenAPI login sessions will remain valid.";
           field ~qualifier:RW ~in_product_since:rel_tampa ~default_value:(Some (VMap [])) ~ty:(Map (String, String)) "guest_VCPUs_params" "VCPUs params to apply to all resident guests";
           field ~qualifier:RW ~in_product_since:rel_cream ~default_value:(Some (VEnum "enabled")) ~ty:display "display" "indicates whether the host is configured to output its console to a physical display device";
           field ~qualifier:DynamicRO ~in_product_since:rel_cream ~default_value:(Some (VSet [VInt 0L])) ~ty:(Set (Int)) "virtual_hardware_platform_versions" "The set of versions of the virtual hardware platform that the host can offer to its guests";
           field ~qualifier:DynamicRO ~default_value:(Some (VRef null_ref)) ~in_product_since:rel_ely ~ty:(Ref _vm) "control_domain" "The control domain (domain 0)";
           field ~qualifier:DynamicRO ~lifecycle:[Published, rel_ely, ""] ~ty:(Set (Ref _pool_update)) ~ignore_foreign_key:true "updates_requiring_reboot" "List of updates which require reboot";
           field ~qualifier:DynamicRO ~lifecycle:[Published, rel_falcon, ""] ~ty:(Set (Ref _feature)) "features" "List of features available on this host";
           field ~qualifier:StaticRO ~lifecycle:[Published, rel_kolkata, ""] ~default_value:(Some (VString "")) ~ty:String "iscsi_iqn" "The initiator IQN for the host";
         ])
      ()
