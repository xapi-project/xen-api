(* datamodel_host *)

open Datamodel_common
open Datamodel_roles
open Datamodel_types

let host_memory =
  let field = field ~ty:Int in
  [
    field ~qualifier:DynamicRO "overhead"
      ~lifecycle:
        [(Published, rel_rio, "Virtualization memory overhead (bytes).")]
      "Virtualization memory overhead (bytes)." ~default_value:(Some (VInt 0L))
      ~doc_tags:[Memory]
  ]

let api_version =
  let field' = field ~qualifier:DynamicRO in
  [
    field' ~ty:Int
      ~lifecycle:[(Published, rel_rio, "major version number")]
      "major" "major version number"
  ; field' ~ty:Int
      ~lifecycle:[(Published, rel_rio, "minor version number")]
      "minor" "minor version number"
  ; field' ~ty:String
      ~lifecycle:[(Published, rel_rio, "identification of vendor")]
      "vendor" "identification of vendor"
  ; field'
      ~ty:(Map (String, String))
      ~lifecycle:[(Published, rel_rio, "details of vendor implementation")]
      "vendor_implementation" "details of vendor implementation"
  ]

let migrate_receive =
  call ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_tampa
        , "Prepare to receive a VM, returning a token which can be passed to \
           VM.migrate."
        )
      ]
    ~name:"migrate_receive"
    ~doc:
      "Prepare to receive a VM, returning a token which can be passed to \
       VM.migrate."
    ~params:
      [
        (Ref _host, "host", "The target host")
      ; ( Ref _network
        , "network"
        , "The network through which migration traffic should be received."
        )
      ; (Map (String, String), "options", "Extra configuration operations")
      ]
    ~result:
      (Map (String, String), "A value which should be passed to VM.migrate")
    ~allowed_roles:_R_VM_POWER_ADMIN ()

let ha_disable_failover_decisions =
  call
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Prevents future failover decisions happening on this node. This \
           function should only be used as part of a controlled shutdown of \
           the HA system."
        )
      ]
    ~name:"ha_disable_failover_decisions"
    ~doc:
      "Prevents future failover decisions happening on this node. This \
       function should only be used as part of a controlled shutdown of the HA \
       system."
    ~params:[(Ref _host, "host", "The Host to disable failover decisions for")]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let ha_disarm_fencing =
  call
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Disarms the fencing function of the HA subsystem. This function is \
           extremely dangerous and should only be used as part of a controlled \
           shutdown of the HA system."
        )
      ]
    ~name:"ha_disarm_fencing"
    ~doc:
      "Disarms the fencing function of the HA subsystem. This function is \
       extremely dangerous and should only be used as part of a controlled \
       shutdown of the HA system."
    ~params:[(Ref _host, "host", "The Host to disarm")]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let ha_stop_daemon =
  call
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Stops the HA daemon. This function is extremely dangerous and \
           should only be used as part of a controlled shutdown of the HA \
           system."
        )
      ]
    ~name:"ha_stop_daemon"
    ~doc:
      "Stops the HA daemon. This function is extremely dangerous and should \
       only be used as part of a controlled shutdown of the HA system."
    ~params:[(Ref _host, "host", "The Host whose daemon should be stopped")]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let ha_release_resources =
  call
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Cleans up any resources on the host associated with this HA \
           instance."
        )
      ]
    ~name:"ha_release_resources"
    ~doc:"Cleans up any resources on the host associated with this HA instance."
    ~params:
      [(Ref _host, "host", "The Host whose resources should be cleaned up")]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let local_assert_healthy =
  call ~flags:[`Session]
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "Returns nothing if this host is healthy, otherwise it throws an \
           error explaining why the host is unhealthy"
        )
      ]
    ~name:"local_assert_healthy"
    ~doc:
      "Returns nothing if this host is healthy, otherwise it throws an error \
       explaining why the host is unhealthy"
    ~params:[] ~pool_internal:true ~hide_from_docs:true
    ~errs:
      [
        Api_errors.host_still_booting
      ; Api_errors.host_has_no_management_ip
      ; Api_errors.host_master_cannot_talk_back
      ; Api_errors.host_unknown_to_master
      ; Api_errors.host_broken
      ; Api_errors.license_restriction
      ; Api_errors.license_does_not_support_pooling
      ; Api_errors.ha_should_be_fenced
      ; Api_errors.host_xapi_version_higher_than_coordinator
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let preconfigure_ha =
  call
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "Attach statefiles, generate config files but do not start the xHA \
           daemon."
        )
      ]
    ~name:"preconfigure_ha"
    ~doc:
      "Attach statefiles, generate config files but do not start the xHA \
       daemon."
    ~params:
      [
        (Ref _host, "host", "The Host to modify")
      ; (Set (Ref _vdi), "statefiles", "Set of statefile VDIs to use")
      ; (Ref _vdi, "metadata_vdi", "VDI to use for Pool metadata")
      ; (String, "generation", "UUID identifying this HA instance")
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let ha_join_liveset =
  call
    ~lifecycle:
      [(Published, rel_orlando, "Block until this host joins the liveset.")]
    ~name:"ha_join_liveset" ~doc:"Block until this host joins the liveset."
    ~params:[(Ref _host, "host", "The Host whose HA daemon to start")]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let ha_wait_for_shutdown_via_statefile =
  call
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Block until this host xHA daemon exits after having seen the \
           invalid statefile. If the host loses statefile access then throw an \
           exception"
        )
      ]
    ~name:"ha_wait_for_shutdown_via_statefile"
    ~doc:
      "Block until this host xHA daemon exits after having seen the invalid \
       statefile. If the host loses statefile access then throw an exception"
    ~params:[(Ref _host, "host", "The Host whose HA subsystem to query")]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let request_backup =
  call ~flags:[`Session] ~name:"request_backup"
    ~lifecycle:
      [(Published, rel_rio, "Request this host performs a database backup")]
    ~doc:"Request this host performs a database backup"
    ~params:
      [
        (Ref _host, "host", "The Host to send the request to")
      ; (Int, "generation", "The generation count of the master's database")
      ; ( Bool
        , "force"
        , "If this is true then the client _has_ to take a backup, otherwise \
           it's just an 'offer'"
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let request_config_file_sync =
  call ~flags:[`Session] ~name:"request_config_file_sync"
    ~lifecycle:
      [(Published, rel_rio, "Request this host syncs dom0 config files")]
    ~doc:"Request this host syncs dom0 config files"
    ~params:
      [
        (Ref _host, "host", "The Host to send the request to")
      ; (String, "hash", "The hash of the master's dom0 config files package")
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

(* Since there are no async versions, no tasks are generated (!) this is important
   otherwise the call would block doing a Db.Task.create *)
let propose_new_master =
  call ~flags:[`Session]
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "First phase of a two-phase commit protocol to set the new master. \
           If the host has already committed to another configuration or if \
           the proposed new master is not in this node's membership set then \
           the call will return an exception."
        )
      ]
    ~name:"propose_new_master"
    ~doc:
      "First phase of a two-phase commit protocol to set the new master. If \
       the host has already committed to another configuration or if the \
       proposed new master is not in this node's membership set then the call \
       will return an exception."
    ~params:
      [
        ( String
        , "address"
        , "The address of the Host which is proposed as the new master"
        )
      ; ( Bool
        , "manual"
        , "True if this call is being invoked by the user manually, false if \
           automatic"
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let abort_new_master =
  call ~flags:[`Session]
    ~lifecycle:
      [(Published, rel_miami, "Causes the new master transaction to abort")]
    ~name:"abort_new_master" ~doc:"Causes the new master transaction to abort"
    ~params:
      [
        ( String
        , "address"
        , "The address of the Host which is proposed as the new master"
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let commit_new_master =
  call ~flags:[`Session]
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "Second phase of a two-phase commit protocol to set the new master."
        )
      ]
    ~name:"commit_new_master"
    ~doc:"Second phase of a two-phase commit protocol to set the new master."
    ~params:
      [
        ( String
        , "address"
        , "The address of the Host which should be committed as the new master"
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let compute_free_memory =
  call
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Computes the amount of free memory on the host."
        )
      ]
    ~name:"compute_free_memory"
    ~doc:"Computes the amount of free memory on the host."
    ~params:[(Ref _host, "host", "The host to send the request to")]
    ~pool_internal:false ~hide_from_docs:false
    ~result:(Int, "the amount of free memory on the host.")
    ~allowed_roles:_R_READ_ONLY ~doc_tags:[Memory] ()

let compute_memory_overhead =
  call
    ~lifecycle:
      [
        ( Published
        , rel_midnight_ride
        , "Computes the virtualization memory overhead of a host."
        )
      ]
    ~name:"compute_memory_overhead"
    ~doc:"Computes the virtualization memory overhead of a host."
    ~params:
      [(Ref _host, "host", "The host for which to compute the memory overhead")]
    ~pool_internal:false ~hide_from_docs:false
    ~result:(Int, "the virtualization memory overhead of the host.")
    ~allowed_roles:_R_READ_ONLY ~doc_tags:[Memory] ()

(* Diagnostics see if host is in emergency mode *)
let is_in_emergency_mode =
  call ~flags:[`Session]
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "Diagnostics call to discover if host is in emergency mode"
        )
      ]
    ~name:"is_in_emergency_mode"
    ~doc:"Diagnostics call to discover if host is in emergency mode" ~params:[]
    ~pool_internal:false ~hide_from_docs:true
    ~result:(Bool, "true if host is in emergency mode")
    ~allowed_roles:_R_READ_ONLY ()

(* Signal that the management IP address or hostname has been changed beneath us. *)
let signal_networking_change =
  call ~flags:[`Session]
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "Signals that the management IP address or hostname has been changed \
           beneath us."
        )
      ]
    ~name:"signal_networking_change"
    ~doc:
      "Signals that the management IP address or hostname has been changed \
       beneath us."
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ~doc_tags:[Networking] ()

let notify =
  call
    ~lifecycle:[(Published, rel_miami, "Notify an event")]
    ~name:"notify" ~doc:"Notify an event"
    ~params:
      [
        (String, "ty", "type of the notification")
      ; (String, "params", "arguments of the notification (can be empty)")
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let syslog_reconfigure =
  call
    ~lifecycle:[(Published, rel_miami, "Re-configure syslog logging")]
    ~name:"syslog_reconfigure" ~doc:"Re-configure syslog logging"
    ~params:
      [
        ( Ref _host
        , "host"
        , "Tell the host to reread its Host.logging parameters and reconfigure \
           itself accordingly"
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

let management_reconfigure =
  call
    ~lifecycle:
      [(Published, rel_miami, "Reconfigure the management network interface")]
    ~name:"management_reconfigure"
    ~doc:"Reconfigure the management network interface"
    ~params:
      [
        ( Ref _pif
        , "pif"
        , "reference to a PIF object corresponding to the management interface"
        )
      ]
    ~allowed_roles:_R_POOL_OP ~doc_tags:[Networking] ()

let local_management_reconfigure =
  call ~flags:[`Session]
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "Reconfigure the management network interface. Should only be used \
           if Host.management_reconfigure is impossible because the network \
           configuration is broken."
        )
      ]
    ~name:"local_management_reconfigure"
    ~doc:
      "Reconfigure the management network interface. Should only be used if \
       Host.management_reconfigure is impossible because the network \
       configuration is broken."
    ~params:
      [
        ( String
        , "interface"
        , "name of the interface to use as a management interface"
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

let ha_xapi_healthcheck =
  call ~flags:[`Session]
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Returns true if xapi appears to be functioning normally."
        )
      ]
    ~name:"ha_xapi_healthcheck"
    ~doc:"Returns true if xapi appears to be functioning normally."
    ~result:(Bool, "true if xapi is functioning normally.")
    ~hide_from_docs:true ~allowed_roles:_R_POOL_ADMIN ()

let management_disable =
  call ~flags:[`Session]
    ~lifecycle:
      [(Published, rel_miami, "Disable the management network interface")]
    ~name:"management_disable" ~doc:"Disable the management network interface"
    ~params:[] ~allowed_roles:_R_POOL_OP ~doc_tags:[Networking] ()

let get_management_interface =
  call
    ~lifecycle:[(Published, rel_tampa, "")]
    ~name:"get_management_interface"
    ~doc:"Returns the management interface for the specified host"
    ~params:
      [(Ref _host, "host", "Which host's management interface is required")]
    ~result:(Ref _pif, "The management interface for the host")
    ~allowed_roles:_R_POOL_OP ~doc_tags:[Networking] ()

(* Simple host evacuate message for Miami.
   Not intended for HA *)

let assert_can_evacuate =
  call
    ~lifecycle:[(Published, rel_miami, "Check this host can be evacuated.")]
    ~name:"assert_can_evacuate" ~doc:"Check this host can be evacuated."
    ~params:[(Ref _host, "host", "The host to evacuate")]
    ~allowed_roles:_R_POOL_OP ()

(* New Orlando message which aims to make the GUI less brittle (unexpected errors will trigger a VM suspend)
   and sensitive to HA planning constraints *)
let get_vms_which_prevent_evacuation =
  call
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Return a set of VMs which prevent the host being evacuated, with \
           per-VM error codes"
        )
      ]
    ~name:"get_vms_which_prevent_evacuation"
    ~doc:
      "Return a set of VMs which prevent the host being evacuated, with per-VM \
       error codes"
    ~params:[(Ref _host, "self", "The host to query")]
    ~result:
      ( Map (Ref _vm, Set String)
      , "VMs which block evacuation together with reasons"
      )
    ~allowed_roles:_R_READ_ONLY ()

let evacuate =
  call ~name:"evacuate" ~doc:"Migrate all VMs off of this host, where possible."
    ~lifecycle:
      [
        (Published, rel_miami, "")
      ; (Extended, "1.297.0", "Enable migration network selection.")
      ; (Extended, "23.27.0", "Choose batch size of VM evacuation.")
      ]
    ~versioned_params:
      [
        {
          param_type= Ref _host
        ; param_name= "host"
        ; param_doc= "The host to evacuate"
        ; param_release= miami_release
        ; param_default= None
        }
      ; {
          param_type= Ref _network
        ; param_name= "network"
        ; param_doc= "Optional preferred network for migration"
        ; param_release= numbered_release "1.297.0"
        ; param_default= Some (VRef null_ref)
        }
      ; {
          param_type= Int
        ; param_name= "evacuate_batch_size"
        ; param_doc=
            "The maximum number of VMs to be migrated per batch 0 will use the \
             value `evacuation-batch-size` defined in xapi.conf"
        ; param_release= numbered_release "23.27.0"
        ; param_default= Some (VInt 0L)
        }
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let get_uncooperative_resident_VMs =
  call
    ~lifecycle:
      [
        ( Published
        , rel_midnight_ride
        , "Return a set of VMs which are not co-operating with the host's \
           memory control system"
        )
      ; (Deprecated, rel_tampa, "")
      ]
    ~name:"get_uncooperative_resident_VMs"
    ~doc:
      "Return a set of VMs which are not co-operating with the host's memory \
       control system"
    ~params:[(Ref _host, "self", "The host to query")]
    ~result:(Set (Ref _vm), "VMs which are not co-operating")
    ~allowed_roles:_R_READ_ONLY ()

let get_uncooperative_domains =
  call
    ~lifecycle:
      [
        ( Published
        , rel_midnight_ride
        , "Return the set of domain uuids which are not co-operating with the \
           host's memory control system"
        )
      ; (Deprecated, rel_tampa, "")
      ]
    ~name:"get_uncooperative_domains"
    ~doc:
      "Return the set of domain uuids which are not co-operating with the \
       host's memory control system"
    ~params:[(Ref _host, "self", "The host to query")]
    ~result:(Set String, "UUIDs of domains which are not co-operating")
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let retrieve_wlb_evacuate_recommendations =
  call ~name:"retrieve_wlb_evacuate_recommendations"
    ~lifecycle:
      [
        ( Published
        , rel_george
        , "Retrieves recommended host migrations to perform when evacuating \
           the host from the wlb server. If a VM cannot be migrated from the \
           host the reason is listed instead of a recommendation."
        )
      ]
    ~doc:
      "Retrieves recommended host migrations to perform when evacuating the \
       host from the wlb server. If a VM cannot be migrated from the host the \
       reason is listed instead of a recommendation."
    ~params:[(Ref _host, "self", "The host to query")]
    ~result:
      ( Map (Ref _vm, Set String)
      , "VMs and the reasons why they would block evacuation, or their target \
         host recommended by the wlb server"
      )
    ~allowed_roles:_R_READ_ONLY ()

(* Host.Disable *)

let disable =
  call
    ~lifecycle:
      [
        ( Published
        , rel_rio
        , "Puts the host into a state in which no new VMs can be started. \
           Currently active VMs on the host continue to execute."
        )
      ]
    ~name:"disable"
    ~doc:
      "Puts the host into a state in which no new VMs can be started. \
       Currently active VMs on the host continue to execute."
    ~params:[(Ref _host, "host", "The Host to disable")]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

(* Host.Enable *)

let enable =
  call ~name:"enable"
    ~lifecycle:
      [
        ( Published
        , rel_rio
        , "Puts the host into a state in which new VMs can be started."
        )
      ]
    ~doc:"Puts the host into a state in which new VMs can be started."
    ~params:[(Ref _host, "host", "The Host to enable")]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

(* Host.Shutdown *)

let shutdown =
  call ~name:"shutdown"
    ~lifecycle:
      [
        ( Published
        , rel_rio
        , "Shutdown the host. (This function can only be called if there are \
           no currently running VMs on the host and it is disabled.)"
        )
      ]
    ~doc:
      "Shutdown the host. (This function can only be called if there are no \
       currently running VMs on the host and it is disabled.)"
    ~params:[(Ref _host, "host", "The Host to shutdown")]
    ~allowed_roles:_R_POOL_OP ()

(* Host.reboot *)

let reboot =
  call ~name:"reboot"
    ~lifecycle:
      [
        ( Published
        , rel_rio
        , "Reboot the host. (This function can only be called if there are no \
           currently running VMs on the host and it is disabled.)"
        )
      ]
    ~doc:
      "Reboot the host. (This function can only be called if there are no \
       currently running VMs on the host and it is disabled.)"
    ~params:[(Ref _host, "host", "The Host to reboot")]
    ~allowed_roles:_R_POOL_OP ()

(* Host.prepare_for_poweroff *)

let prepare_for_poweroff =
  call ~name:"prepare_for_poweroff"
    ~lifecycle:
      [
        ( Published
        , rel_kolkata
        , "Performs the necessary actions before host shutdown or reboot."
        )
      ]
    ~doc:"Performs the necessary actions before host shutdown or reboot."
    ~params:
      [(Ref _host, "host", "The Host that is about to reboot or shutdown")]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ~hide_from_docs:true ()

(* Host.power_on *)

let power_on =
  call ~name:"power_on"
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Attempt to power-on the host (if the capability exists)."
        )
      ]
    ~doc:"Attempt to power-on the host (if the capability exists)."
    ~params:[(Ref _host, "host", "The Host to power on")]
    ~allowed_roles:_R_POOL_OP ()

let restart_agent =
  call ~name:"restart_agent"
    ~lifecycle:
      [
        ( Published
        , rel_rio
        , "Restarts the agent after a 10 second pause. WARNING: this is a \
           dangerous operation. Any operations in progress will be aborted, \
           and unrecoverable data loss may occur. The caller is responsible \
           for ensuring that there are no operations in progress when this \
           method is called."
        )
      ]
    ~doc:
      "Restarts the agent after a 10 second pause. WARNING: this is a \
       dangerous operation. Any operations in progress will be aborted, and \
       unrecoverable data loss may occur. The caller is responsible for \
       ensuring that there are no operations in progress when this method is \
       called."
    ~params:
      [(Ref _host, "host", "The Host on which you want to restart the agent")]
    ~allowed_roles:_R_POOL_OP ()

let shutdown_agent =
  call ~name:"shutdown_agent"
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Shuts the agent down after a 10 second pause. WARNING: this is a \
           dangerous operation. Any operations in progress will be aborted, \
           and unrecoverable data loss may occur. The caller is responsible \
           for ensuring that there are no operations in progress when this \
           method is called."
        )
      ]
    ~doc:
      "Shuts the agent down after a 10 second pause. WARNING: this is a \
       dangerous operation. Any operations in progress will be aborted, and \
       unrecoverable data loss may occur. The caller is responsible for \
       ensuring that there are no operations in progress when this method is \
       called."
    ~params:[] ~flags:[`Session] (* no async *)
    ~allowed_roles:_R_POOL_OP ()

let dmesg =
  call ~name:"dmesg"
    ~lifecycle:[(Published, rel_rio, "Get the host xen dmesg.")]
    ~doc:"Get the host xen dmesg."
    ~params:[(Ref _host, "host", "The Host to query")]
    ~result:(String, "dmesg string") ~allowed_roles:_R_POOL_OP ()

let dmesg_clear =
  call ~name:"dmesg_clear"
    ~lifecycle:
      [(Published, rel_rio, "Get the host xen dmesg, and clear the buffer.")]
    ~doc:"Get the host xen dmesg, and clear the buffer."
    ~params:[(Ref _host, "host", "The Host to query")]
    ~result:(String, "dmesg string") ~allowed_roles:_R_POOL_OP ()

let get_log =
  call ~name:"get_log"
    ~lifecycle:[(Published, rel_rio, "Get the host's log file")]
    ~doc:"Get the host's log file"
    ~params:[(Ref _host, "host", "The Host to query")]
    ~result:(String, "The contents of the host's primary log file")
    ~allowed_roles:_R_READ_ONLY ()

let send_debug_keys =
  call ~name:"send_debug_keys"
    ~lifecycle:
      [
        ( Published
        , rel_rio
        , "Inject the given string as debugging keys into Xen"
        )
      ]
    ~doc:"Inject the given string as debugging keys into Xen"
    ~params:
      [(Ref _host, "host", "The host"); (String, "keys", "The keys to send")]
    ~allowed_roles:_R_POOL_ADMIN ()

let get_data_sources =
  call ~name:"get_data_sources" ~in_oss_since:None
    ~lifecycle:[(Published, rel_orlando, "")]
    ~doc:""
    ~result:(Set (Record _data_source), "A set of data sources")
    ~params:[(Ref _host, "host", "The host to interrogate")]
    ~errs:[] ~flags:[`Session] ~allowed_roles:_R_READ_ONLY ()

let record_data_source =
  call ~name:"record_data_source" ~in_oss_since:None
    ~lifecycle:
      [(Published, rel_orlando, "Start recording the specified data source")]
    ~doc:"Start recording the specified data source"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "data_source", "The data source to record")
      ]
    ~errs:[] ~flags:[`Session] ~allowed_roles:_R_POOL_OP ()

let query_data_source =
  call ~name:"query_data_source" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Query the latest value of the specified data source"
        )
      ]
    ~doc:"Query the latest value of the specified data source"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "data_source", "The data source to query")
      ]
    ~result:(Float, "The latest value, averaged over the last 5 seconds")
    ~errs:[] ~flags:[`Session] ~allowed_roles:_R_READ_ONLY ()

let attach_static_vdis =
  call ~name:"attach_static_vdis"
    ~lifecycle:
      [(Published, rel_midnight_ride, "Statically attach VDIs on a host.")]
    ~doc:"Statically attach VDIs on a host."
    ~params:
      [
        (Ref _host, "host", "The Host to modify")
      ; ( Map (Ref _vdi, String)
        , "vdi_reason_map"
        , "List of VDI+reason pairs to attach"
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let detach_static_vdis =
  call ~name:"detach_static_vdis"
    ~lifecycle:
      [(Published, rel_midnight_ride, "Detach static VDIs from a host.")]
    ~doc:"Detach static VDIs from a host."
    ~params:
      [
        (Ref _host, "host", "The Host to modify")
      ; (Set (Ref _vdi), "vdis", "Set of VDIs to detach")
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let declare_dead =
  call ~name:"declare_dead"
    ~lifecycle:
      [
        ( Published
        , rel_clearwater
        , "Declare that a host is dead. This is a dangerous operation, and \
           should only be called if the administrator is absolutely sure the \
           host is definitely dead"
        )
      ]
    ~doc:
      "Declare that a host is dead. This is a dangerous operation, and should \
       only be called if the administrator is absolutely sure the host is \
       definitely dead"
    ~params:[(Ref _host, "host", "The Host to declare is dead")]
    ~allowed_roles:_R_POOL_OP ()

let forget_data_source_archives =
  call ~name:"forget_data_source_archives" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Forget the recorded statistics related to the specified data source"
        )
      ]
    ~doc:"Forget the recorded statistics related to the specified data source"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; ( String
        , "data_source"
        , "The data source whose archives are to be forgotten"
        )
      ]
    ~flags:[`Session] ~allowed_roles:_R_POOL_OP ()

let get_diagnostic_timing_stats =
  call ~flags:[`Session]
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "Return timing statistics for diagnostic purposes"
        )
      ]
    ~name:"get_diagnostic_timing_stats"
    ~doc:"Return timing statistics for diagnostic purposes"
    ~params:[(Ref _host, "host", "The host to interrogate")]
    ~result:(Map (String, String), "population name to summary map")
    ~hide_from_docs:true ~allowed_roles:_R_READ_ONLY ()

let create_new_blob =
  call ~name:"create_new_blob"
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Create a placeholder for a named binary blob of data that is \
           associated with this host"
        )
      ]
    ~doc:
      "Create a placeholder for a named binary blob of data that is associated \
       with this host"
    ~versioned_params:
      [
        {
          param_type= Ref _host
        ; param_name= "host"
        ; param_doc= "The host"
        ; param_release= orlando_release
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "name"
        ; param_doc= "The name associated with the blob"
        ; param_release= orlando_release
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "mime_type"
        ; param_doc=
            "The mime type for the data. Empty string translates to \
             application/octet-stream"
        ; param_release= orlando_release
        ; param_default= None
        }
      ; {
          param_type= Bool
        ; param_name= "public"
        ; param_doc= "True if the blob should be publicly available"
        ; param_release= tampa_release
        ; param_default= Some (VBool false)
        }
      ]
    ~result:
      (Ref _blob, "The reference of the blob, needed for populating its data")
    ~allowed_roles:_R_POOL_OP ()

let call_plugin =
  call ~name:"call_plugin"
    ~lifecycle:[(Published, rel_orlando, "Call an API plugin on this host")]
    ~doc:"Call an API plugin on this host"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "plugin", "The name of the plugin")
      ; (String, "fn", "The name of the function within the plugin")
      ; (Map (String, String), "args", "Arguments for the function")
      ]
    ~result:(String, "Result from the plugin")
    ~allowed_roles:_R_POOL_ADMIN ()

let has_extension =
  call ~name:"has_extension"
    ~lifecycle:
      [
        ( Published
        , rel_ely
        , "Return true if the extension is available on the host"
        )
      ]
    ~doc:"Return true if the extension is available on the host"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "name", "The name of the API call")
      ]
    ~result:(Bool, "True if the extension exists, false otherwise")
    ~allowed_roles:_R_POOL_ADMIN ()

let call_extension =
  call ~name:"call_extension"
    ~lifecycle:[(Published, rel_ely, "Call an API extension on this host")]
    ~custom_marshaller:true ~doc:"Call an API extension on this host"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "call", "Rpc call for the extension")
      ]
    ~result:(String, "Result from the extension")
    ~allowed_roles:_R_POOL_ADMIN ~flags:[`Session] (* no async *)
    ()

let enable_binary_storage =
  call ~name:"enable_binary_storage"
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Enable binary storage on a particular host, for storing RRDs, \
           messages and blobs"
        )
      ]
    ~hide_from_docs:true ~pool_internal:true
    ~doc:
      "Enable binary storage on a particular host, for storing RRDs, messages \
       and blobs"
    ~params:[(Ref _host, "host", "The host")]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let disable_binary_storage =
  call ~name:"disable_binary_storage"
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Disable binary storage on a particular host, deleting stored RRDs, \
           messages and blobs"
        )
      ]
    ~hide_from_docs:true ~pool_internal:true
    ~doc:
      "Disable binary storage on a particular host, deleting stored RRDs, \
       messages and blobs"
    ~params:[(Ref _host, "host", "The host")]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let update_pool_secret =
  call ~name:"update_pool_secret"
    ~lifecycle:[(Published, rel_midnight_ride, "")]
    ~hide_from_docs:true ~pool_internal:true ~doc:""
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (SecretString, "pool_secret", "The new pool secret")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let update_master =
  call ~name:"update_master"
    ~lifecycle:[(Published, rel_midnight_ride, "")]
    ~hide_from_docs:true ~pool_internal:true ~doc:""
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "master_address", "The new master address")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let set_localdb_key =
  call ~name:"set_localdb_key"
    ~lifecycle:
      [(Published, rel_midnight_ride, "Set a key in the local DB of the host.")]
    ~doc:"Set a key in the local DB of the host."
    ~params:
      [
        (Ref _host, "host", "The Host to modify")
      ; (String, "key", "Key to change")
      ; (String, "value", "Value to set")
      ]
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let refresh_pack_info =
  call ~name:"refresh_pack_info"
    ~doc:"Refresh the list of installed Supplemental Packs."
    ~params:[(Ref _host, "host", "The Host to modify")]
    ~allowed_roles:_R_POOL_OP
    ~lifecycle:
      [
        (Published, rel_midnight_ride, "")
      ; (Deprecated, rel_ely, "Use Pool_update.resync_host instead")
      ]
    ()

let bugreport_upload =
  call ~name:"bugreport_upload"
    ~doc:"Run xen-bugtool --yestoall and upload the output to support"
    ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_rio
        , "Run xen-bugtool --yestoall and upload the output to support"
        )
      ]
    ~params:
      [
        (Ref _host, "host", "The host on which to run xen-bugtool")
      ; (String, "url", "The URL to upload to")
      ; (Map (String, String), "options", "Extra configuration operations")
      ]
    ~allowed_roles:_R_POOL_OP ()

let list_methods =
  call ~name:"list_methods"
    ~lifecycle:[(Published, rel_rio, "List all supported methods")]
    ~flags:[`Session] ~doc:"List all supported methods" ~params:[]
    ~result:(Set String, "The name of every supported method.")
    ~allowed_roles:_R_READ_ONLY ()

let license_apply =
  call ~name:"license_apply" ~in_oss_since:None
    ~lifecycle:
      [
        (Published, rel_rio, "Apply a new license to a host")
      ; (Deprecated, rel_clearwater, "Dummy transition")
      ; (Removed, rel_clearwater, "Free licenses no longer handled by xapi")
      ]
    ~params:
      [
        (Ref _host, "host", "The host to upload the license to")
      ; (String, "contents", "The contents of the license file, base64 encoded")
      ]
    ~doc:"Apply a new license to a host"
    ~errs:[Api_errors.license_processing_error]
    ~allowed_roles:_R_POOL_OP ()

let license_add =
  call ~name:"license_add" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_indigo
        , "Functionality for parsing license files re-added"
        )
      ]
    ~params:
      [
        (Ref _host, "host", "The host to upload the license to")
      ; (String, "contents", "The contents of the license file, base64 encoded")
      ]
    ~doc:"Apply a new license to a host"
    ~errs:[Api_errors.license_processing_error]
    ~allowed_roles:_R_POOL_OP ()

let license_remove =
  call ~name:"license_remove" ~in_oss_since:None
    ~lifecycle:[(Published, rel_indigo, "")]
    ~params:
      [(Ref _host, "host", "The host from which any license will be removed")]
    ~doc:
      "Remove any license file from the specified host, and switch that host \
       to the unlicensed edition"
    ~allowed_roles:_R_POOL_OP ()

let create_params =
  [
    {
      param_type= String
    ; param_name= "uuid"
    ; param_doc= "unique identifier/object reference"
    ; param_release= rio_release
    ; param_default= None
    }
  ; {
      param_type= String
    ; param_name= "name_label"
    ; param_doc= "The name of the new storage repository"
    ; param_release= rio_release
    ; param_default= None
    }
  ; {
      param_type= String
    ; param_name= "name_description"
    ; param_doc= "The description of the new storage repository"
    ; param_release= rio_release
    ; param_default= None
    }
  ; {
      param_type= String
    ; param_name= "hostname"
    ; param_doc= "Hostname"
    ; param_release= rio_release
    ; param_default= None
    }
  ; {
      param_type= String
    ; param_name= "address"
    ; param_doc=
        "An address by which this host can be contacted by other members in \
         its pool"
    ; param_release= rio_release
    ; param_default= None
    }
  ; {
      param_type= String
    ; param_name= "external_auth_type"
    ; param_doc=
        "type of external authentication service configured; empty if none \
         configured"
    ; param_release= george_release
    ; param_default= Some (VString "")
    }
  ; {
      param_type= String
    ; param_name= "external_auth_service_name"
    ; param_doc=
        "name of external authentication service configured; empty if none \
         configured"
    ; param_release= george_release
    ; param_default= Some (VString "")
    }
  ; {
      param_type= Map (String, String)
    ; param_name= "external_auth_configuration"
    ; param_doc= "configuration specific to external authentication service"
    ; param_release= george_release
    ; param_default= Some (VMap [])
    }
  ; {
      param_type= Map (String, String)
    ; param_name= "license_params"
    ; param_doc= "State of the current license"
    ; param_release= midnight_ride_release
    ; param_default= Some (VMap [])
    }
  ; {
      param_type= String
    ; param_name= "edition"
    ; param_doc= "Product edition"
    ; param_release= midnight_ride_release
    ; param_default= Some (VString "")
    }
  ; {
      param_type= Map (String, String)
    ; param_name= "license_server"
    ; param_doc= "Contact information of the license server"
    ; param_release= midnight_ride_release
    ; param_default=
        Some
          (VMap
             [
               (VString "address", VString "localhost")
             ; (VString "port", VString "27000")
             ]
          )
    }
  ; {
      param_type= Ref _sr
    ; param_name= "local_cache_sr"
    ; param_doc= "The SR that is used as a local cache"
    ; param_release= cowley_release
    ; param_default= Some (VRef null_ref)
    }
  ; {
      param_type= Map (String, String)
    ; param_name= "chipset_info"
    ; param_doc= "Information about chipset features"
    ; param_release= boston_release
    ; param_default= Some (VMap [])
    }
  ; {
      param_type= Bool
    ; param_name= "ssl_legacy"
    ; param_doc=
        "Allow SSLv3 protocol and ciphersuites as used by older XenServers. \
         This controls both incoming and outgoing connections."
    ; param_release= dundee_release
    ; param_default= Some (VBool true)
    }
  ; {
      param_type= DateTime
    ; param_name= "last_software_update"
    ; param_doc=
        "Date and time when the last software update was applied. When the \
         timezone is missing, UTC is assumed"
    ; param_release= dundee_release
    ; param_default= Some (VDateTime Date.epoch)
    }
  ; {
      param_type= String
    ; param_name= "last_update_hash"
    ; param_doc=
        "The SHA256 checksum of updateinfo of the most recently applied update \
         on the host"
    ; param_release= numbered_release "24.40.0"
    ; param_default= Some (VString "")
    }
  ; {
      param_type= Bool
    ; param_name= "ssh_enabled"
    ; param_doc= "True if SSH access is enabled for the host"
    ; param_release= numbered_release "25.20.0-next"
    ; param_default= Some (VBool Constants.default_ssh_enabled)
    }
  ; {
      param_type= Int
    ; param_name= "ssh_enabled_timeout"
    ; param_doc=
        "The timeout in seconds after which SSH access will be automatically \
         disabled (0 means never), this setting will be applied every time the \
         SSH is enabled by XAPI"
    ; param_release= numbered_release "25.20.0-next"
    ; param_default= Some (VInt Constants.default_ssh_enabled_timeout)
    }
  ; {
      param_type= DateTime
    ; param_name= "ssh_expiry"
    ; param_doc=
        "The time in UTC after which the SSH access will be automatically \
         disabled"
    ; param_release= numbered_release "25.20.0-next"
    ; param_default= Some (VDateTime Date.epoch)
    }
  ; {
      param_type= Int
    ; param_name= "console_idle_timeout"
    ; param_doc=
        "The timeout in seconds after which idle console will be automatically \
         terminated (0 means never)"
    ; param_release= numbered_release "25.20.0-next"
    ; param_default= Some (VInt Constants.default_console_idle_timeout)
    }
  ]

let create =
  call ~name:"create" ~in_oss_since:None
    ~lifecycle:
      [
        (Published, rel_rio, "Create a new host record")
      ; ( Changed
        , "24.40.0"
        , "Added --last_update_hash option to allow last_update_hash to be \
           kept for host joined a pool"
        )
      ; ( Changed
        , "25.20.0-next"
        , "Added --ssh_enabled --ssh_enabled_timeout --ssh_expiry \
           --console_idle_timeout options to allow them to be configured for \
           new host"
        )
      ]
    ~versioned_params:create_params ~doc:"Create a new host record"
    ~result:(Ref _host, "Reference to the newly created host object.")
    ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

let destroy =
  call ~name:"destroy" ~in_oss_since:None
    ~lifecycle:
      [(Published, rel_rio, "Destroy specified host record in database")]
    ~doc:"Destroy specified host record in database"
    ~params:[(Ref _host, "self", "The host record to remove")]
    ~allowed_roles:_R_POOL_OP ()

let get_system_status_capabilities =
  call ~flags:[`Session] ~name:"get_system_status_capabilities"
    ~in_oss_since:None
    ~lifecycle:[(Published, rel_miami, "")]
    ~params:[(Ref _host, "host", "The host to interrogate")]
    ~doc:""
    ~result:
      (String, "An XML fragment containing the system status capabilities.")
    ~allowed_roles:_R_READ_ONLY ()

let set_hostname_live =
  call ~flags:[`Session] ~name:"set_hostname_live" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_miami
        , "Sets the host name to the specified string.  Both the API and \
           lower-level system hostname are changed immediately."
        )
      ]
    ~params:
      [
        (Ref _host, "host", "The host whose host name to set")
      ; (String, "hostname", "The new host name")
      ]
    ~errs:[Api_errors.host_name_invalid]
    ~doc:
      "Sets the host name to the specified string.  Both the API and \
       lower-level system hostname are changed immediately."
    ~allowed_roles:_R_POOL_OP ()

let tickle_heartbeat =
  call ~flags:[`Session] ~name:"tickle_heartbeat" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Needs to be called every 30 seconds for the master to believe the \
           host is alive"
        )
      ]
    ~params:
      [
        ( Ref _host
        , "host"
        , "The host calling the function, and whose heartbeat to tickle"
        )
      ; ( Map (String, String)
        , "stuff"
        , "Anything else we want to let the master know"
        )
      ]
    ~result:(Map (String, String), "Anything the master wants to tell the slave")
    ~doc:
      "Needs to be called every 30 seconds for the master to believe the host \
       is alive"
    ~pool_internal:true ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let sync_data =
  call ~flags:[`Session] ~name:"sync_data" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "This causes the synchronisation of the non-database data (messages, \
           RRDs and so on) stored on the master to be synchronised with the \
           host"
        )
      ]
    ~params:[(Ref _host, "host", "The host to whom the data should be sent")]
    ~doc:
      "This causes the synchronisation of the non-database data (messages, \
       RRDs and so on) stored on the master to be synchronised with the host"
    ~allowed_roles:_R_POOL_ADMIN ()

let backup_rrds =
  call ~flags:[`Session] ~name:"backup_rrds" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "This causes the RRDs to be backed up to the master"
        )
      ]
    ~params:
      [
        (Ref _host, "host", "Schedule a backup of the RRDs of this host")
      ; ( Float
        , "delay"
        , "Delay in seconds from when the call is received to perform the \
           backup"
        )
      ]
    ~doc:"This causes the RRDs to be backed up to the master"
    ~allowed_roles:_R_POOL_ADMIN ()

let get_servertime =
  call ~flags:[`Session] ~name:"get_servertime" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "This call queries the host's clock for the current time"
        )
      ]
    ~params:[(Ref _host, "host", "The host whose clock should be queried")]
    ~doc:"This call queries the host's clock for the current time"
    ~result:(DateTime, "The current time")
    ~allowed_roles:_R_READ_ONLY ()

let get_server_localtime =
  call ~flags:[`Session] ~name:"get_server_localtime" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_cowley
        , "This call queries the host's clock for the current time in the \
           host's local timezone"
        )
      ]
    ~params:[(Ref _host, "host", "The host whose clock should be queried")]
    ~doc:
      "This call queries the host's clock for the current time in the host's \
       local timezone"
    ~result:(DateTime, "The current local time")
    ~allowed_roles:_R_READ_ONLY ()

let emergency_ha_disable =
  call ~flags:[`Session] ~name:"emergency_ha_disable" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "This call disables HA on the local host. This should only be used \
           with extreme care."
        )
      ]
    ~versioned_params:
      [
        {
          param_type= Bool
        ; param_name= "soft"
        ; param_doc=
            "Disable HA temporarily, revert upon host reboot or further \
             changes, idempotent"
        ; param_release= ely_release
        ; param_default= Some (VBool false)
        }
      ]
    ~doc:
      "This call disables HA on the local host. This should only be used with \
       extreme care."
    ~allowed_roles:_R_POOL_OP ()

let install_ca_certificate =
  call ~pool_internal:true ~hide_from_docs:true ~name:"install_ca_certificate"
    ~doc:"Install a TLS CA certificate on this host."
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "name", "A name to give the certificate")
      ; (String, "cert", "The certificate")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ~lifecycle:[(Published, "1.290.0", "Install TLS CA certificate")]
    ()

let uninstall_ca_certificate =
  call ~pool_internal:true ~hide_from_docs:true ~name:"uninstall_ca_certificate"
    ~doc:"Remove a TLS CA certificate from this host."
    ~versioned_params:
      [
        {
          param_type= Ref _host
        ; param_name= "host"
        ; param_doc= "The host"
        ; param_release= numbered_release "1.290.0"
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "name"
        ; param_doc= "The certificate name"
        ; param_release= numbered_release "1.290.0"
        ; param_default= None
        }
      ; {
          param_type= Bool
        ; param_name= "force"
        ; param_doc= "Remove the DB entry even if the file is non-existent"
        ; param_release= numbered_release "24.35.0"
        ; param_default= Some (VBool false)
        }
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ~lifecycle:
      [
        (Published, "1.290.0", "Uninstall TLS CA certificate")
      ; ( Changed
        , "24.35.0"
        , "Added --force option to allow DB entries to be removed for \
           non-existent files"
        )
      ]
    ()

let certificate_list =
  call ~pool_internal:true ~hide_from_docs:true ~name:"certificate_list"
    ~doc:"List the filenames of all installed TLS CA certificates."
    ~params:[(Ref _host, "host", "The host")]
    ~result:(Set String, "All installed certificates")
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ~lifecycle:
      [
        (Published, rel_george, "List installed TLS CA certificate")
      ; (Deprecated, "1.290.0", "Use openssl to inspect certificate?")
      ]
    ()

let crl_install =
  call ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_george
        , "Install a TLS CA-issued Certificate Revocation List to this host."
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~name:"crl_install"
    ~doc:"Install a TLS CA-issued Certificate Revocation List to this host."
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "name", "A name to give the CRL")
      ; (String, "crl", "The CRL")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let crl_uninstall =
  call ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_george
        , "Uninstall a TLS CA-issued certificate revocation list from this \
           host."
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~name:"crl_uninstall"
    ~doc:"Uninstall a TLS CA-issued certificate revocation list from this host."
    ~params:[(Ref _host, "host", "The host"); (String, "name", "The CRL name")]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let crl_list =
  call ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_george
        , "List the filenames of all installed TLS CA-issued Certificate \
           Revocation Lists."
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~name:"crl_list"
    ~doc:
      "List the filenames of all installed TLS CA-issued Certificate \
       Revocation Lists."
    ~params:[(Ref _host, "host", "The host")]
    ~result:(Set String, "All installed CRLs")
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let certificate_sync =
  call ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_george
        , "Make installed TLS CA certificates and CRLs available to all \
           programs using OpenSSL."
        )
      ]
    ~pool_internal:true ~hide_from_docs:true ~name:"certificate_sync"
    ~doc:
      "Make installed TLS CA certificates and CRLs available to all programs \
       using OpenSSL."
    ~params:[(Ref _host, "host", "The host")]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let install_server_certificate =
  call
    ~lifecycle:[(Published, rel_stockholm, "")]
    ~name:"install_server_certificate"
    ~doc:"Install the TLS server certificate."
    ~versioned_params:
      [
        {
          param_type= Ref _host
        ; param_name= "host"
        ; param_doc= "The host"
        ; param_release= stockholm_release
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "certificate"
        ; param_doc= "The server certificate, in PEM form"
        ; param_release= stockholm_release
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "private_key"
        ; param_doc=
            "The unencrypted private key used to sign the certificate, in \
             PKCS#8 form"
        ; param_release= stockholm_release
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "certificate_chain"
        ; param_doc= "The certificate chain, in PEM form"
        ; param_release= stockholm_release
        ; param_default= Some (VString "")
        }
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let reset_server_certificate =
  call
    ~lifecycle:[(Published, "1.290.0", "")]
    ~name:"reset_server_certificate"
    ~doc:
      "Delete the current TLS server certificate and replace by a new, \
       self-signed one. This should only be used with extreme care."
    ~params:[(Ref _host, "host", "The host")]
    ~allowed_roles:_R_POOL_ADMIN ()

let emergency_reset_server_certificate =
  call ~flags:[`Session]
    ~lifecycle:[(Published, rel_stockholm, "")]
    ~name:"emergency_reset_server_certificate"
    ~doc:
      "Delete the current TLS server certificate and replace by a new, \
       self-signed one. This should only be used with extreme care."
    ~versioned_params:[] ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

(* this internal call is used to process values of type [Cert_distrib.Wire.command] *)
let cert_distrib_atom =
  call ~pool_internal:true ~hide_from_docs:true
    ~lifecycle:[(Published, "1.295.0", "")]
    ~name:"cert_distrib_atom" ~doc:""
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "command", "The string encoded command")
      ]
    ~result:(String, "The string encoded result")
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let get_server_certificate =
  call ~in_oss_since:None
    ~lifecycle:
      [
        (Published, rel_george, "")
      ; (Changed, rel_inverness, "Now available to all RBAC roles.")
      ]
    ~name:"get_server_certificate"
    ~doc:"Get the installed server public TLS certificate."
    ~params:[(Ref _host, "host", "The host")]
    ~result:(String, "The installed server public TLS certificate, in PEM form.")
    ~allowed_roles:_R_READ_ONLY ()

let refresh_server_certificate =
  call
    ~lifecycle:[(Published, "1.307.0", "")]
    ~name:"refresh_server_certificate"
    ~doc:"Replace the internal self-signed host certficate with a new one."
    ~params:[(Ref _host, "host", "The host")]
    ~allowed_roles:_R_POOL_ADMIN ()

let display =
  Enum
    ( "host_display"
    , [
        ( "enabled"
        , "This host is outputting its console to a physical display device"
        )
      ; ( "disable_on_reboot"
        , "The host will stop outputting its console to a physical display \
           device on next boot"
        )
      ; ( "disabled"
        , "This host is not outputting its console to a physical display device"
        )
      ; ( "enable_on_reboot"
        , "The host will start outputting its console to a physical display \
           device on next boot"
        )
      ]
    )

let operations =
  Enum
    ( "host_allowed_operations"
    , [
        ("provision", "Indicates this host is able to provision another VM")
      ; ("evacuate", "Indicates this host is evacuating")
      ; ( "shutdown"
        , "Indicates this host is in the process of shutting itself down"
        )
      ; ("reboot", "Indicates this host is in the process of rebooting")
      ; ("power_on", "Indicates this host is in the process of being powered on")
      ; ("vm_start", "This host is starting a VM")
      ; ("vm_resume", "This host is resuming a VM")
      ; ("vm_migrate", "This host is the migration target of a VM")
      ; ("apply_updates", "Indicates this host is being updated")
      ; ("enable", "Indicates this host is in the process of enabling")
      ]
    )

let enable_external_auth =
  call ~flags:[`Session] ~name:"enable_external_auth" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_george
        , "This call enables external authentication on a host"
        )
      ]
    ~params:
      [
        ( Ref _host
        , "host"
        , "The host whose external authentication should be enabled"
        )
      ; ( Map (String, String)
        , "config"
        , "A list of key-values containing the configuration data"
        )
      ; (String, "service_name", "The name of the service")
      ; ( String
        , "auth_type"
        , "The type of authentication (e.g. AD for Active Directory)"
        )
      ]
    ~doc:"This call enables external authentication on a host"
    ~allowed_roles:_R_POOL_ADMIN ()

let disable_external_auth =
  call ~flags:[`Session] ~name:"disable_external_auth" ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_george
        , "This call disables external authentication on the local host"
        )
      ]
    ~versioned_params:
      [
        {
          param_type= Ref _host
        ; param_name= "host"
        ; param_doc= "The host whose external authentication should be disabled"
        ; param_release= george_release
        ; param_default= None
        }
      ; {
          param_type= Map (String, String)
        ; param_name= "config"
        ; param_doc=
            "Optional parameters as a list of key-values containing the \
             configuration data"
        ; param_release= george_release
        ; param_default= Some (VMap [])
        }
      ]
    ~doc:"This call disables external authentication on the local host"
    ~allowed_roles:_R_POOL_ADMIN ()

let set_license_params =
  call ~name:"set_license_params"
    ~lifecycle:
      [
        ( Published
        , rel_orlando
        , "Set the new license details in the database, trigger a \
           recomputation of the pool SKU"
        )
      ]
      (* actually update 3 aka floodgate *)
    ~doc:
      "Set the new license details in the database, trigger a recomputation of \
       the pool SKU"
    ~params:
      [
        (Ref _host, "self", "The host")
      ; (Map (String, String), "value", "The license_params")
      ]
    ~hide_from_docs:true ~pool_internal:true ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let apply_edition =
  call ~flags:[`Session] ~name:"apply_edition"
    ~lifecycle:
      [
        ( Published
        , rel_midnight_ride
        , "Change to another edition, or reactivate the current edition after \
           a license has expired. This may be subject to the successful \
           checkout of an appropriate license."
        )
      ]
    ~doc:
      "Change to another edition, or reactivate the current edition after a \
       license has expired. This may be subject to the successful checkout of \
       an appropriate license."
    ~versioned_params:
      [
        {
          param_type= Ref _host
        ; param_name= "host"
        ; param_doc= "The host"
        ; param_release= midnight_ride_release
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "edition"
        ; param_doc= "The requested edition"
        ; param_release= midnight_ride_release
        ; param_default= None
        }
      ; {
          param_type= Bool
        ; param_name= "force"
        ; param_doc= "Update the license params even if the apply call fails"
        ; param_release= clearwater_release
        ; param_default= Some (VBool false)
        }
      ]
    ~allowed_roles:_R_POOL_OP ()

let set_power_on_mode =
  call ~name:"set_power_on_mode"
    ~lifecycle:
      [
        (Published, rel_cowley, "")
      ; (Changed, rel_stockholm, "Removed iLO script")
      ; (Changed, "24.19.0", "Replaced DRAC mode with IPMI")
      ]
    ~doc:"Set the power-on-mode, host, user and password"
    ~params:
      [
        (Ref _host, "self", "The host")
      ; ( String
        , "power_on_mode"
        , "power-on-mode can be empty, wake-on-lan, IPMI or other"
        )
      ; (Map (String, String), "power_on_config", "Power on config")
      ]
    ~allowed_roles:_R_POOL_OP ()

let set_ssl_legacy =
  call ~name:"set_ssl_legacy"
    ~lifecycle:
      [
        (Published, rel_dundee, "")
      ; (Changed, rel_stockholm, "Legacy SSL no longer supported")
      ]
    ~doc:
      "Enable/disable SSLv3 for interoperability with older server versions. \
       When this is set to a different value, the host immediately restarts \
       its SSL/TLS listening service; typically this takes less than a second \
       but existing connections to it will be broken. API login sessions will \
       remain valid."
    ~params:
      [
        (Ref _host, "self", "The host")
      ; ( Bool
        , "value"
        , "True to allow SSLv3 and ciphersuites as used in old XenServer \
           versions"
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

let set_cpu_features =
  call ~flags:[`Session] ~name:"set_cpu_features"
    ~doc:
      "Set the CPU features to be used after a reboot, if the given features \
       string is valid."
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "features", "The features string (32 hexadecimal digits)")
      ]
    ~allowed_roles:_R_POOL_OP
    ~lifecycle:
      [
        (Published, rel_midnight_ride, "")
      ; (Deprecated, rel_dundee, "Dummy transition")
      ; (Removed, rel_dundee, "Manual CPU feature setting was removed")
      ]
    ()

let reset_cpu_features =
  call ~flags:[`Session] ~name:"reset_cpu_features"
    ~doc:
      "Remove the feature mask, such that after a reboot all features of the \
       CPU are enabled."
    ~params:[(Ref _host, "host", "The host")]
    ~allowed_roles:_R_POOL_OP
    ~lifecycle:
      [
        (Published, rel_midnight_ride, "")
      ; (Deprecated, rel_dundee, "Dummy transition")
      ; (Removed, rel_dundee, "Manual CPU feature setting was removed")
      ]
    ()

let reset_networking =
  call ~name:"reset_networking"
    ~lifecycle:[(Published, rel_boston, "")]
    ~doc:"Purge all network-related metadata associated with the given host."
    ~params:[(Ref _host, "host", "The Host to modify")]
    ~allowed_roles:_R_POOL_OP ~hide_from_docs:true ()

let enable_local_storage_caching =
  call ~flags:[`Session] ~name:"enable_local_storage_caching"
    ~lifecycle:
      [
        ( Published
        , rel_cowley
        , "Enable the use of a local SR for caching purposes"
        )
      ]
    ~doc:"Enable the use of a local SR for caching purposes"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (Ref _sr, "sr", "The SR to use as a local cache")
      ]
    ~allowed_roles:_R_POOL_OP ()

let disable_local_storage_caching =
  call ~flags:[`Session] ~name:"disable_local_storage_caching"
    ~lifecycle:
      [
        ( Published
        , rel_cowley
        , "Disable the use of a local SR for caching purposes"
        )
      ]
    ~doc:"Disable the use of a local SR for caching purposes"
    ~params:[(Ref _host, "host", "The host")]
    ~allowed_roles:_R_POOL_OP ()

let get_sm_diagnostics =
  call ~flags:[`Session] ~name:"get_sm_diagnostics"
    ~lifecycle:[(Published, rel_boston, "Return live SM diagnostics")]
    ~doc:"Return live SM diagnostics"
    ~params:[(Ref _host, "host", "The host")]
    ~result:(String, "Printable diagnostic data")
    ~allowed_roles:_R_POOL_OP ~hide_from_docs:true ()

let get_thread_diagnostics =
  call ~flags:[`Session] ~name:"get_thread_diagnostics"
    ~lifecycle:[(Published, rel_boston, "Return live thread diagnostics")]
    ~doc:"Return live thread diagnostics"
    ~params:[(Ref _host, "host", "The host")]
    ~result:(String, "Printable diagnostic data")
    ~allowed_roles:_R_POOL_OP ~hide_from_docs:true ()

let sm_dp_destroy =
  call ~flags:[`Session] ~name:"sm_dp_destroy"
    ~lifecycle:
      [
        ( Published
        , rel_boston
        , "Attempt to cleanup and destroy a named SM datapath"
        )
      ]
    ~doc:"Attempt to cleanup and destroy a named SM datapath"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "dp", "The datapath")
      ; ( Bool
        , "allow_leak"
        , "If true, all records of the datapath will be removed even if the \
           datapath could not be destroyed cleanly."
        )
      ]
    ~allowed_roles:_R_POOL_OP ~hide_from_docs:true ()

let sync_vlans =
  call ~flags:[`Session] ~name:"sync_vlans"
    ~lifecycle:[(Published, rel_boston, "")]
    ~doc:"Synchronise VLANs on given host with the master's VLANs"
    ~params:[(Ref _host, "host", "The host")]
    ~hide_from_docs:true ~pool_internal:true ~allowed_roles:_R_POOL_OP ()

let sync_tunnels =
  call ~flags:[`Session] ~name:"sync_tunnels"
    ~lifecycle:[(Published, rel_boston, "")]
    ~doc:"Synchronise tunnels on given host with the master's tunnels"
    ~params:[(Ref _host, "host", "The host")]
    ~hide_from_docs:true ~pool_internal:true ~allowed_roles:_R_POOL_OP ()

let sync_pif_currently_attached =
  call ~flags:[`Session] ~name:"sync_pif_currently_attached"
    ~lifecycle:[(Published, rel_boston, "")]
    ~doc:"Synchronise tunnels on given host with the master's tunnels"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (Set String, "bridges", "A list of bridges that are currently up")
      ]
    ~hide_from_docs:true ~pool_internal:true ~allowed_roles:_R_POOL_OP ()

let enable_display =
  call ~name:"enable_display"
    ~lifecycle:[(Published, rel_cream, "")]
    ~doc:
      "Enable console output to the physical display device next time this \
       host boots"
    ~params:[(Ref _host, "host", "The host")]
    ~result:(display, "This host's physical display usage")
    ~allowed_roles:_R_POOL_OP ()

let disable_display =
  call ~name:"disable_display"
    ~lifecycle:[(Published, rel_cream, "")]
    ~doc:
      "Disable console output to the physical display device next time this \
       host boots"
    ~params:[(Ref _host, "host", "The host")]
    ~result:(display, "This host's physical display usage")
    ~allowed_roles:_R_POOL_OP ()

let apply_guest_agent_config =
  call ~name:"apply_guest_agent_config"
    ~lifecycle:[(Published, rel_dundee, "")]
    ~doc:"Signal to the host that the pool-wide guest agent config has changed"
    ~params:[(Ref _host, "host", "The host")]
    ~hide_from_docs:true ~allowed_roles:_R_POOL_ADMIN ()

let mxgpu_vf_setup =
  call ~name:"mxgpu_vf_setup"
    ~lifecycle:[(Published, rel_falcon, "")]
    ~doc:
      "Ensure the driver (kernel module) for MxGPU is loaded on the host, and \
       create PCI objects for any new PCI devices (virtual functions) that the \
       module makes visible."
    ~params:[(Ref _host, "host", "The host")]
    ~hide_from_docs:true ~pool_internal:true ~allowed_roles:_R_VM_OP ()

let nvidia_vf_setup =
  call ~name:"nvidia_vf_setup"
    ~lifecycle:[(Published, rel_quebec, "")]
    ~doc:
      "Ensure the nvidia VFs are created, and create PCI objects for\n\
      \      any new PCI devices (virtual functions) that the module makes \
       visible."
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (Ref _pci, "pf", "The physical function (PF), may be Null")
      ; (Bool, "enable", "true enables virtual functions")
      ]
    ~hide_from_docs:true ~pool_internal:true ~allowed_roles:_R_VM_OP ()

let allocate_resources_for_vm =
  call ~name:"allocate_resources_for_vm"
    ~lifecycle:[(Published, rel_inverness, "")]
    ~doc:
      "Reserves the resources for a VM by setting the \
       'scheduled_to_be_resident_on' fields"
    ~params:
      [
        (Ref _host, "self", "The host")
      ; (Ref _vm, "vm", "The VM")
      ; (Bool, "live", "Is this part of a live migration?")
      ]
    ~hide_from_docs:true ~allowed_roles:_R_VM_OP ()

let set_iscsi_iqn =
  call ~name:"set_iscsi_iqn"
    ~lifecycle:[(Published, rel_kolkata, "")]
    ~doc:"Sets the initiator IQN for the host"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "value", "The value to which the IQN should be set")
      ]
    ~allowed_roles:_R_POOL_OP ()

let set_multipathing =
  call ~name:"set_multipathing"
    ~lifecycle:[(Published, rel_kolkata, "")]
    ~doc:"Specifies whether multipathing is enabled"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (Bool, "value", "Whether multipathing should be enabled")
      ]
    ~allowed_roles:_R_POOL_OP ()

let write_uefi_certificates_to_disk =
  call ~name:"write_uefi_certificates_to_disk"
    ~lifecycle:[(Published, "22.16.0", "")]
    ~doc:"Writes the UEFI certificates to a host disk"
    ~params:[(Ref _host, "host", "The host")]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ~pool_internal:true ~hide_from_docs:true
    ()

let set_uefi_certificates =
  call ~name:"set_uefi_certificates"
    ~lifecycle:
      [
        (Published, rel_quebec, "")
      ; (Deprecated, "22.16.0", "Use Pool.set_uefi_certificates instead")
      ]
    ~doc:"Sets the UEFI certificates on a host"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (String, "value", "The certificates to apply to a host")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let notify_accept_new_pool_secret =
  call ~name:"notify_accept_new_pool_secret"
    ~lifecycle:[(Published, rel_stockholm_psr, "")]
    ~doc:
      "Notifies host that they must begin accepting requests containing the \
       new pool secret"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (SecretString, "old_ps", "Old pool secret")
      ; (SecretString, "new_ps", "New pool secret")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ~hide_from_docs:true ()

let notify_send_new_pool_secret =
  call ~name:"notify_send_new_pool_secret"
    ~lifecycle:[(Published, rel_stockholm_psr, "")]
    ~doc:
      "Notifies host that they must begin sending requests with the new pool \
       secret"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (SecretString, "old_ps", "Old pool secret")
      ; (SecretString, "new_ps", "New pool secret")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ~hide_from_docs:true ()

let cleanup_pool_secret =
  call ~name:"cleanup_pool_secret"
    ~lifecycle:[(Published, rel_stockholm_psr, "")]
    ~doc:"Cleanup old pool secret on recipient host"
    ~params:
      [
        (Ref _host, "host", "The host")
      ; (SecretString, "old_ps", "Old pool secret")
      ; (SecretString, "new_ps", "New pool secret")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ~hide_from_docs:true ()

let host_numa_affinity_policy =
  Enum
    ( "host_numa_affinity_policy"
    , [
        ("any", "VMs are spread across all available NUMA nodes")
      ; ( "best_effort"
        , "VMs are placed on the smallest number of NUMA nodes that they fit \
           using soft-pinning, but the policy doesn't guarantee a balanced \
           placement, falling back to the 'any' policy."
        )
      ; ( "default_policy"
        , "Use the NUMA affinity policy that is the default for the current \
           version"
        )
      ]
    )

let set_numa_affinity_policy =
  call ~name:"set_numa_affinity_policy" ~lifecycle:[]
    ~doc:"Set VM placement NUMA affinity policy"
    ~params:
      [
        (Ref _host, "self", "The host")
      ; ( host_numa_affinity_policy
        , "value"
        , "The NUMA affinity policy to apply to a host"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let host_sched_gran =
  Enum
    ( "host_sched_gran"
    , [
        ("core", "core scheduling")
      ; ("cpu", "CPU scheduling")
      ; ("socket", "socket scheduling")
      ]
    )

let set_sched_gran =
  call ~name:"set_sched_gran"
    ~lifecycle:[(Published, "1.271.0", "")]
    ~doc:
      "Sets xen's sched-gran on a host. See: \
       https://xenbits.xen.org/docs/unstable/misc/xen-command-line.html#sched-gran-x86"
    ~params:
      [
        (Ref _host, "self", "The host")
      ; (host_sched_gran, "value", "The sched-gran to apply to a host")
      ]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let get_sched_gran =
  call ~name:"get_sched_gran"
    ~lifecycle:[(Published, "1.271.0", "")]
    ~doc:"Gets xen's sched-gran on a host"
    ~params:[(Ref _host, "self", "The host")]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ~result:(host_sched_gran, "The host's sched-gran")
    ()

let emergency_disable_tls_verification =
  call ~flags:[`Session] ~name:"emergency_disable_tls_verification"
    ~lifecycle:[(Published, "1.290.0", "")]
    ~in_oss_since:None ~params:[]
    ~doc:"Disable TLS verification for this host only"
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let emergency_reenable_tls_verification =
  call ~flags:[`Session] ~name:"emergency_reenable_tls_verification"
    ~lifecycle:[(Published, "1.298.0", "")]
    ~in_oss_since:None ~params:[]
    ~doc:
      "Reenable TLS verification for this host only, and only after it was \
       emergency disabled"
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let apply_updates =
  call ~name:"apply_updates" ~in_oss_since:None
    ~lifecycle:[(Published, "1.301.0", "")]
    ~doc:"apply updates from current enabled repository on a host"
    ~params:
      [
        (Ref _host, "self", "The host where updates will be applied")
      ; ( String
        , "hash"
        , "The hash of updateinfo to be applied which is returned by previous \
           pool.sync_udpates"
        )
      ]
    ~result:
      ( Set (Set String)
      , "The list of results after applying updates, including livepatch apply \
         failures and recommended guidances"
      )
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let rescan_drivers =
  call ~name:"rescan_drivers" ~in_oss_since:None ~lifecycle:[]
    ~doc:"Scan the host and update its driver information."
    ~params:[(Ref _host, "self", "The host to be rescanned")]
    ~allowed_roles:_R_POOL_ADMIN ()

let copy_primary_host_certs =
  call ~name:"copy_primary_host_certs" ~in_oss_since:None
    ~lifecycle:[(Published, "1.307.0", "")]
    ~doc:"useful for secondary hosts that are missing some certs"
    ~params:
      [
        ( Ref _host
        , "host"
        , "this host receives a copy of the primary host's trusted certificates"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ~hide_from_docs:true ~pool_internal:true ()

let set_https_only =
  call ~name:"set_https_only"
    ~doc:
      "updates the host firewall to open or close port 80 depending on the \
       value"
    ~lifecycle:[]
    ~params:
      [
        (Ref _host, "self", "The Host")
      ; ( Bool
        , "value"
        , "true - http port 80 will be blocked, false - http port 80 will be \
           open"
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

let apply_recommended_guidances =
  call ~name:"apply_recommended_guidances"
    ~doc:
      "apply all recommended guidances both on the host and on all HVM VMs on \
       the host after updates are applied on the host"
    ~lifecycle:[(Prototyped, "23.18.0", ""); (Removed, "23.25.0", "")]
    ~params:
      [
        ( Ref _host
        , "self"
        , "The host whose recommended guidances will be applied"
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

let emergency_clear_mandatory_guidance =
  call ~flags:[`Session] ~name:"emergency_clear_mandatory_guidance"
    ~lifecycle:[] ~in_oss_since:None ~params:[]
    ~doc:"Clear the pending mandatory guidance on this host"
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let enable_ssh =
  call ~name:"enable_ssh"
    ~doc:
      "Enable SSH access on the host. It will start the service sshd only if \
       it is not running. It will also enable the service sshd only if it is \
       not enabled. A newly joined host in the pool or an ejected host from \
       the pool would keep the original status."
    ~lifecycle:[]
    ~params:[(Ref _host, "self", "The host")]
    ~allowed_roles:_R_POOL_ADMIN ()

let disable_ssh =
  call ~name:"disable_ssh"
    ~doc:
      "Disable SSH access on the host. It will stop the service sshd only if \
       it is running. It will also disable the service sshd only if it is \
       enabled. A newly joined host in the pool or an ejected host from the \
       pool would keep the original status."
    ~lifecycle:[]
    ~params:[(Ref _host, "self", "The host")]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_ssh_enabled_timeout =
  call ~name:"set_ssh_enabled_timeout" ~lifecycle:[]
    ~doc:"Set the SSH service enabled timeout for the host"
    ~params:
      [
        (Ref _host, "self", "The host")
      ; ( Int
        , "value"
        , "The SSH enabled timeout in seconds (0 means no timeout, max 2 days)"
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_console_idle_timeout =
  call ~name:"set_console_idle_timeout" ~lifecycle:[]
    ~doc:"Set the console idle timeout for the host"
    ~params:
      [
        (Ref _host, "self", "The host")
      ; (Int, "value", "The console idle timeout in seconds")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let latest_synced_updates_applied_state =
  Enum
    ( "latest_synced_updates_applied_state"
    , [
        ( "yes"
        , "The host is up to date with the latest updates synced from remote \
           CDN"
        )
      ; ( "no"
        , "The host is outdated with the latest updates synced from remote CDN"
        )
      ; ( "unknown"
        , "If the host is up to date with the latest updates synced from \
           remote CDN is unknown"
        )
      ]
    )

(** Hosts *)
let t =
  create_obj ~in_db:true
    ~lifecycle:[(Published, rel_rio, "A physical host")]
    ~in_oss_since:oss_since_303 ~persist:PersistEverything
    ~gen_constructor_destructor:false ~name:_host ~descr:"A physical host"
    ~gen_events:true ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:
      [
        disable
      ; enable
      ; shutdown
      ; reboot
      ; prepare_for_poweroff
      ; dmesg
      ; dmesg_clear
      ; get_log
      ; send_debug_keys
      ; bugreport_upload
      ; list_methods
      ; license_apply
      ; license_add
      ; license_remove
      ; create
      ; destroy
      ; power_on
      ; set_license_params
      ; emergency_ha_disable
      ; ha_disarm_fencing
      ; preconfigure_ha
      ; ha_join_liveset
      ; ha_disable_failover_decisions
      ; ha_wait_for_shutdown_via_statefile
      ; ha_stop_daemon
      ; ha_release_resources
      ; ha_xapi_healthcheck
      ; local_assert_healthy
      ; request_backup
      ; request_config_file_sync
      ; propose_new_master
      ; commit_new_master
      ; abort_new_master
      ; get_data_sources
      ; record_data_source
      ; query_data_source
      ; forget_data_source_archives
      ; assert_can_evacuate
      ; get_vms_which_prevent_evacuation
      ; get_uncooperative_resident_VMs
      ; get_uncooperative_domains
      ; evacuate
      ; signal_networking_change
      ; notify
      ; syslog_reconfigure
      ; management_reconfigure
      ; local_management_reconfigure
      ; management_disable
      ; get_management_interface
      ; get_system_status_capabilities
      ; get_diagnostic_timing_stats
      ; restart_agent
      ; shutdown_agent
      ; set_hostname_live
      ; is_in_emergency_mode
      ; compute_free_memory
      ; compute_memory_overhead
      ; tickle_heartbeat
      ; sync_data
      ; backup_rrds
      ; create_new_blob
      ; call_plugin
      ; has_extension
      ; call_extension
      ; get_servertime
      ; get_server_localtime
      ; enable_binary_storage
      ; disable_binary_storage
      ; enable_external_auth
      ; disable_external_auth
      ; retrieve_wlb_evacuate_recommendations
      ; install_ca_certificate
      ; uninstall_ca_certificate
      ; certificate_list
      ; crl_install
      ; crl_uninstall
      ; crl_list
      ; certificate_sync
      ; get_server_certificate
      ; refresh_server_certificate
      ; install_server_certificate
      ; emergency_reset_server_certificate
      ; reset_server_certificate
      ; update_pool_secret
      ; update_master
      ; attach_static_vdis
      ; detach_static_vdis
      ; set_localdb_key
      ; apply_edition
      ; refresh_pack_info
      ; set_power_on_mode
      ; set_cpu_features
      ; reset_cpu_features
      ; reset_networking
      ; enable_local_storage_caching
      ; disable_local_storage_caching
      ; get_sm_diagnostics
      ; get_thread_diagnostics
      ; sm_dp_destroy
      ; sync_vlans
      ; sync_tunnels
      ; sync_pif_currently_attached
      ; migrate_receive
      ; declare_dead
      ; enable_display
      ; disable_display
      ; set_ssl_legacy
      ; apply_guest_agent_config
      ; mxgpu_vf_setup
      ; nvidia_vf_setup
      ; allocate_resources_for_vm
      ; set_iscsi_iqn
      ; set_multipathing
      ; write_uefi_certificates_to_disk
      ; set_uefi_certificates
      ; notify_accept_new_pool_secret
      ; notify_send_new_pool_secret
      ; cleanup_pool_secret
      ; set_sched_gran
      ; get_sched_gran
      ; set_numa_affinity_policy
      ; emergency_disable_tls_verification
      ; emergency_reenable_tls_verification
      ; cert_distrib_atom
      ; apply_updates
      ; rescan_drivers
      ; copy_primary_host_certs
      ; set_https_only
      ; apply_recommended_guidances
      ; emergency_clear_mandatory_guidance
      ; enable_ssh
      ; disable_ssh
      ; set_ssh_enabled_timeout
      ; set_console_idle_timeout
      ]
    ~contents:
      ([
         uid _host
           ~lifecycle:
             [(Published, rel_rio, "Unique identifier/object reference")]
       ; namespace ~name:"name"
           ~contents:(names None RW ~lifecycle:[(Published, rel_rio, "")])
           ()
       ; namespace ~name:"memory" ~contents:host_memory ()
       ]
      @ allowed_and_current_operations operations
      @ [
          namespace ~name:"API_version" ~contents:api_version ()
        ; field ~qualifier:DynamicRO ~ty:Bool "enabled"
            ~lifecycle:
              [(Published, rel_rio, "True if the host is currently enabled")]
            "True if the host is currently enabled"
        ; field ~qualifier:StaticRO
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "version strings")]
            "software_version" "version strings"
        ; field
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "additional configuration")]
            "other_config" "additional configuration"
            ~map_keys_roles:
              [("folder", _R_VM_OP); ("XenCenter.CustomFields.*", _R_VM_OP)]
        ; field ~qualifier:StaticRO ~ty:(Set String)
            ~lifecycle:[(Published, rel_rio, "Xen capabilities")]
            "capabilities" "Xen capabilities"
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The CPU configuration on this host.  May contain keys such \
                   as \"nr_nodes\", \"sockets_per_node\", \
                   \"cores_per_socket\", or \"threads_per_core\""
                )
              ]
            "cpu_configuration"
            "The CPU configuration on this host.  May contain keys such as \
             \"nr_nodes\", \"sockets_per_node\", \"cores_per_socket\", or \
             \"threads_per_core\""
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Scheduler policy currently in force on this host"
                )
              ]
            "sched_policy" "Scheduler policy currently in force on this host"
        ; field ~qualifier:DynamicRO ~ty:(Set String)
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "a list of the bootloaders installed on the machine"
                )
              ]
            "supported_bootloaders"
            "a list of the bootloaders installed on the machine"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vm))
            ~lifecycle:
              [(Published, rel_rio, "list of VMs currently resident on host")]
            "resident_VMs" "list of VMs currently resident on host"
        ; field ~qualifier:RW
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "logging configuration")]
            "logging" "logging configuration"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pif)) ~doc_tags:[Networking]
            ~lifecycle:[(Published, rel_rio, "physical network interfaces")]
            "PIFs" "physical network interfaces"
        ; field ~qualifier:RW ~ty:(Ref _sr)
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The SR in which VDIs for suspend images are created"
                )
              ]
            "suspend_image_sr"
            "The SR in which VDIs for suspend images are created"
        ; field ~qualifier:RW ~ty:(Ref _sr)
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The SR in which VDIs for crash dumps are created"
                )
              ]
            "crash_dump_sr" "The SR in which VDIs for crash dumps are created"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Set of host crash dumps")]
            ~qualifier:DynamicRO ~ty:(Set (Ref _host_crashdump)) "crashdumps"
            "Set of host crash dumps"
        ; field ~in_oss_since:None
            ~lifecycle:
              [
                (Published, rel_rio, "Set of host patches")
              ; (Deprecated, rel_ely, "")
              ]
            ~qualifier:DynamicRO ~ty:(Set (Ref _host_patch)) "patches"
            "Set of host patches"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_ely, "Set of updates")]
            ~qualifier:DynamicRO ~ty:(Set (Ref _pool_update)) "updates"
            "Set of updates"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pbd))
            ~lifecycle:[(Published, rel_rio, "physical blockdevices")]
            "PBDs" "physical blockdevices"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _hostcpu))
            ~lifecycle:[(Published, rel_rio, "The physical CPUs on this host")]
            "host_CPUs" "The physical CPUs on this host"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "Details about the physical CPUs on this host"
                )
              ]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "cpu_info" "Details about the physical CPUs on this host"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "The hostname of this host")]
            ~qualifier:RW ~ty:String ~doc_tags:[Networking] "hostname"
            "The hostname of this host"
        ; field ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The address by which this host can be contacted from any \
                   other host in the pool"
                )
              ]
            ~qualifier:RW ~ty:String ~doc_tags:[Networking] "address"
            "The address by which this host can be contacted from any other \
             host in the pool"
        ; field ~qualifier:DynamicRO ~ty:(Ref _host_metrics)
            ~lifecycle:
              [(Published, rel_rio, "metrics associated with this host")]
            "metrics" "metrics associated with this host"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "State of the current license")]
            ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            "license_params" "State of the current license"
        ; field ~in_oss_since:None
            ~lifecycle:
              [(Published, rel_rio, "Free memory on host at boot time")]
            ~internal_only:true ~qualifier:DynamicRO ~ty:Int "boot_free_mem"
            "Free memory on host at boot time"
        ; field ~in_oss_since:None ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_orlando
                , "The set of statefiles accessible from this host"
                )
              ]
            ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_statefiles"
            "The set of statefiles accessible from this host"
        ; field ~in_oss_since:None ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_orlando
                , "The set of hosts visible via the network from this host"
                )
              ]
            ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_network_peers"
            "The set of hosts visible via the network from this host"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_orlando
                , "Binary blobs associated with this host"
                )
              ]
            ~ty:(Map (String, Ref _blob))
            ~default_value:(Some (VMap [])) "blobs"
            "Binary blobs associated with this host"
        ; field ~writer_roles:_R_VM_OP ~qualifier:RW
            ~lifecycle:
              [
                ( Published
                , rel_orlando
                , "user-specified tags for categorization purposes"
                )
              ]
            ~default_value:(Some (VSet [])) ~ty:(Set String) "tags"
            "user-specified tags for categorization purposes"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_george
                , "type of external authentication service configured; empty \
                   if none configured."
                )
              ]
            ~default_value:(Some (VString "")) ~ty:String "external_auth_type"
            "type of external authentication service configured; empty if none \
             configured."
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_george
                , "name of external authentication service configured; empty \
                   if none configured."
                )
              ]
            ~default_value:(Some (VString "")) ~ty:String
            "external_auth_service_name"
            "name of external authentication service configured; empty if none \
             configured."
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_george
                , "configuration specific to external authentication service"
                )
              ]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "external_auth_configuration"
            "configuration specific to external authentication service"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_midnight_ride, "Product edition")]
            ~default_value:(Some (VString "")) ~ty:String "edition"
            "Product edition"
        ; field ~qualifier:RW
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "Contact information of the license server"
                )
              ]
            ~default_value:
              (Some
                 (VMap
                    [
                      (VString "address", VString "localhost")
                    ; (VString "port", VString "27000")
                    ]
                 )
              )
            ~ty:(Map (String, String))
            "license_server" "Contact information of the license server"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_midnight_ride, "BIOS strings")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "bios_strings" "BIOS strings"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_midnight_ride, "The power on mode")]
            ~default_value:(Some (VString "")) ~ty:String "power_on_mode"
            "The power on mode"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_midnight_ride, "The power on config")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "power_on_config" "The power on config"
        ; field ~qualifier:StaticRO
            ~lifecycle:
              [(Published, rel_cowley, "The SR that is used as a local cache")]
            ~default_value:(Some (VRef null_ref)) ~ty:(Ref _sr) "local_cache_sr"
            "The SR that is used as a local cache"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_boston, "")]
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap [])) "chipset_info"
            "Information about chipset features"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_boston, "")]
            ~ty:(Set (Ref _pci)) "PCIs" "List of PCI devices in the host"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_boston, "")]
            ~ty:(Set (Ref _pgpu)) "PGPUs" "List of physical GPUs in the host"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_inverness, "")]
            ~ty:(Set (Ref _pusb)) "PUSBs" "List of physical USBs in the host"
        ; field ~qualifier:StaticRO
            ~lifecycle:
              [
                (Published, rel_dundee, "")
              ; (Deprecated, rel_stockholm, "Legacy SSL no longer supported")
              ]
            ~ty:Bool ~default_value:(Some (VBool true)) "ssl_legacy"
            "Allow SSLv3 protocol and ciphersuites as used by older server \
             versions. This controls both incoming and outgoing connections. \
             When this is set to a different value, the host immediately \
             restarts its SSL/TLS listening service; typically this takes less \
             than a second but existing connections to it will be broken. API \
             login sessions will remain valid."
        ; field ~qualifier:RW
            ~lifecycle:
              [
                ( Published
                , rel_tampa
                , "VCPUs params to apply to all resident guests"
                )
              ]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "guest_VCPUs_params" "VCPUs params to apply to all resident guests"
        ; field ~qualifier:RW
            ~lifecycle:
              [
                ( Published
                , rel_cream
                , "indicates whether the host is configured to output its \
                   console to a physical display device"
                )
              ]
            ~default_value:(Some (VEnum "enabled")) ~ty:display "display"
            "indicates whether the host is configured to output its console to \
             a physical display device"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_cream
                , "The set of versions of the virtual hardware platform that \
                   the host can offer to its guests"
                )
              ]
            ~default_value:(Some (VSet [VInt 0L])) ~ty:(Set Int)
            "virtual_hardware_platform_versions"
            "The set of versions of the virtual hardware platform that the \
             host can offer to its guests"
        ; field ~qualifier:DynamicRO ~default_value:(Some (VRef null_ref))
            ~lifecycle:[(Published, rel_ely, "The control domain (domain 0)")]
            ~ty:(Ref _vm) "control_domain" "The control domain (domain 0)"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_ely, "")]
            ~ty:(Set (Ref _pool_update)) ~ignore_foreign_key:true
            "updates_requiring_reboot" "List of updates which require reboot"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_falcon, "")]
            ~ty:(Set (Ref _feature)) "features"
            "List of features available on this host"
        ; field ~qualifier:StaticRO
            ~lifecycle:[(Published, rel_kolkata, "")]
            ~default_value:(Some (VString "")) ~ty:String "iscsi_iqn"
            "The initiator IQN for the host"
        ; field ~qualifier:StaticRO
            ~lifecycle:[(Published, rel_kolkata, "")]
            ~default_value:(Some (VBool false)) ~ty:Bool "multipathing"
            "Specifies whether multipathing is enabled"
        ; field ~qualifier:StaticRO
            ~lifecycle:
              [
                (Published, rel_quebec, "")
              ; (Deprecated, "22.16.0", "Use Pool.uefi_certificates instead")
              ]
            ~default_value:(Some (VString "")) ~ty:String "uefi_certificates"
            "The UEFI certificates allowing Secure Boot"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_stockholm, "")]
            ~ty:(Set (Ref _certificate)) "certificates"
            "List of certificates installed in the host"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_stockholm, "")]
            ~default_value:(Some (VSet [])) ~ty:(Set String) "editions"
            "List of all available product editions"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , "1.303.0"
                , "The set of pending mandatory guidances after applying \
                   updates, which must be applied, as otherwise there may be \
                   e.g. VM failures"
                )
              ]
            ~ty:(Set update_guidances) "pending_guidances"
            ~default_value:(Some (VSet []))
            "The set of pending mandatory guidances after applying updates, \
             which must be applied, as otherwise there may be e.g. VM failures"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , "1.313.0"
                , "True if this host has TLS verifcation enabled"
                )
              ]
            ~ty:Bool "tls_verification_enabled"
            ~default_value:(Some (VBool false))
            "True if this host has TLS verifcation enabled"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:DateTime
            "last_software_update" ~default_value:(Some (VDateTime Date.epoch))
            "Date and time when the last software update was applied"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:Bool
            ~default_value:(Some (VBool false)) "https_only"
            "Reflects whether port 80 is open (false) or not (true)"
        ; field ~qualifier:DynamicRO ~internal_only:true
            ~lifecycle:[(Prototyped, "23.18.0", ""); (Removed, "23.24.0", "")]
            ~ty:(Set update_guidances) "recommended_guidances"
            ~default_value:(Some (VSet []))
            "The set of recommended guidances after applying updates"
        ; field ~qualifier:DynamicRO ~lifecycle:[]
            ~ty:latest_synced_updates_applied_state
            "latest_synced_updates_applied"
            ~default_value:(Some (VEnum "unknown"))
            "Default as 'unknown', 'yes' if the host is up to date with \
             updates synced from remote CDN, otherwise 'no'"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:host_numa_affinity_policy
            "numa_affinity_policy"
            ~default_value:(Some (VEnum "default_policy"))
            "NUMA-aware VM memory and vCPU placement policy"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:(Set update_guidances)
            "pending_guidances_recommended" ~default_value:(Some (VSet []))
            "The set of pending recommended guidances after applying updates, \
             which most users should follow to make the updates effective, but \
             if not followed, will not cause a failure"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:(Set update_guidances)
            "pending_guidances_full" ~default_value:(Some (VSet []))
            "The set of pending full guidances after applying updates, which a \
             user should follow to make some updates, e.g. specific hardware \
             drivers or CPU features, fully effective, but the 'average user' \
             doesn't need to"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:String
            ~default_value:(Some (VString "")) "last_update_hash"
            "The SHA256 checksum of updateinfo of the most recently applied \
             update on the host"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:Bool
            ~default_value:(Some (VBool Constants.default_ssh_enabled))
            "ssh_enabled" "True if SSH access is enabled for the host"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:Int
            ~default_value:(Some (VInt Constants.default_ssh_enabled_timeout))
            "ssh_enabled_timeout"
            "The timeout in seconds after which SSH access will be \
             automatically disabled (0 means never), this setting will be \
             applied every time the SSH is enabled by XAPI"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:DateTime
            ~default_value:(Some (VDateTime Date.epoch)) "ssh_expiry"
            "The time in UTC after which the SSH access will be automatically \
             disabled"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:Int
            ~default_value:(Some (VInt Constants.default_console_idle_timeout))
            "console_idle_timeout"
            "The timeout in seconds after which idle console will be \
             automatically terminated (0 means never)"
        ]
      )
    ()
