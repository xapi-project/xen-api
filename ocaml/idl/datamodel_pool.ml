open Datamodel_common
open Datamodel_roles
open Datamodel_types

  let operations =
    Enum ("pool_allowed_operations", (* FIXME: This should really be called `pool_operations`, to avoid confusion with the Pool.allowed_operations field *)
          [ "ha_enable", "Indicates this pool is in the process of enabling HA";
            "ha_disable", "Indicates this pool is in the process of disabling HA";
	    "cluster_create", "Indicates this pool is in the process of creating a cluster";
          ])

  let enable_ha = call
      ~in_product_since:rel_miami
      ~name:"enable_ha"
      ~in_oss_since:None
      ~versioned_params:
        [{param_type=Set(Ref _sr); param_name="heartbeat_srs"; param_doc="Set of SRs to use for storage heartbeating"; param_release=miami_release; param_default=None };
         {param_type=Map(String, String); param_name="configuration"; param_doc="Detailed HA configuration to apply"; param_release=miami_release; param_default=None };
        ]
      ~doc:"Turn on High Availability mode"
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_ha = call
      ~in_product_since:rel_miami
      ~name:"disable_ha"
      ~in_oss_since:None
      ~params:[]
      ~doc:"Turn off High Availability mode"
      ~allowed_roles:_R_POOL_OP
      ()

  let sync_database = call
      ~name:"sync_database"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[]
      ~doc:"Forcibly synchronise the database now"
      ~allowed_roles:_R_POOL_OP
      ()

  let designate_new_master = call
      ~in_product_since:rel_miami
      ~name:"designate_new_master"
      ~in_oss_since:None
      ~params:[Ref _host, "host", "The host who should become the new master"]
      ~doc:"Perform an orderly handover of the role of master to the referenced host."
      ~allowed_roles:_R_POOL_OP
      ()

  let join = call
      ~name:"join"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[String, "master_address", "The hostname of the master of the pool to join";
               String, "master_username", "The username of the master (for initial authentication)";
               String, "master_password", "The password for the master (for initial authentication)";
              ]
      ~errs:[Api_errors.pool_joining_host_cannot_contain_shared_SRs]
      ~doc:"Instruct host to join a new pool"
      ~allowed_roles:_R_POOL_OP
      ()

  let join_force = call
      ~name:"join_force"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[String, "master_address", "The hostname of the master of the pool to join";
               String, "master_username", "The username of the master (for initial authentication)";
               String, "master_password", "The password for the master (for initial authentication)";
              ]
      ~doc:"Instruct host to join a new pool"
      ~allowed_roles:_R_POOL_OP
      ()


  let slave_reset_master = call ~flags:[`Session]
      ~name:"emergency_reset_master"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[
        String, "master_address", "The hostname of the master";
      ]
      ~doc:"Instruct a slave already in a pool that the master has changed"
      ~allowed_roles:_R_POOL_OP
      ()

  let transition_to_master = call ~flags:[`Session]
      ~name:"emergency_transition_to_master"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[]
      ~doc:"Instruct host that's currently a slave to transition to being master"
      ~allowed_roles:_R_POOL_OP
      ()

  let recover_slaves = call
      ~name:"recover_slaves"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[]
      ~result:(Set (Ref _host), "list of hosts whose master address were successfully reset")
      ~doc:"Instruct a pool master, M, to try and contact its slaves and, if slaves are in emergency mode, reset their master address to M."
      ~allowed_roles:_R_POOL_OP
      ()

  let eject = call
      ~name:"eject"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[Ref _host, "host", "The host to eject"]
      ~doc:"Instruct a pool master to eject a host from the pool"
      ~allowed_roles:_R_POOL_OP
      ()

  let initial_auth = call
      ~name:"initial_auth"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[]
      ~result:(String, "")
      ~doc:"Internal use only"
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_OP
      ()

  let create_VLAN_from_PIF = call
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~name:"create_VLAN_from_PIF"
      ~doc:"Create a pool-wide VLAN by taking the PIF."
      ~params:[Ref _pif, "pif", "physical interface on any particular host, that identifies the PIF on which to create the (pool-wide) VLAN interface";
               Ref _network, "network", "network to which this interface should be connected";
               Int, "VLAN", "VLAN tag for the new interface"]
      ~result:(Set (Ref _pif), "The references of the created PIF objects")
      ~errs:[Api_errors.vlan_tag_invalid]
      ~allowed_roles:_R_POOL_OP
      ()

  (* !! THIS IS BROKEN; it takes a device name which in the case of a bond is not homogeneous across all pool hosts.
        See CA-22613. !! *)
  let create_VLAN = call
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~name:"create_VLAN"
      ~doc:"Create PIFs, mapping a network to the same physical interface/VLAN on each host. This call is deprecated: use Pool.create_VLAN_from_PIF instead."
      ~params:[String, "device", "physical interface on which to create the VLAN interface";
               Ref _network, "network", "network to which this interface should be connected";
               Int, "VLAN", "VLAN tag for the new interface"]
      ~result:(Set (Ref _pif), "The references of the created PIF objects")
      ~errs:[Api_errors.vlan_tag_invalid]
      ~allowed_roles:_R_POOL_OP
      ()

  let management_reconfigure = call
      ~name:"management_reconfigure"
      ~in_oss_since:None
      ~in_product_since:rel_inverness
      ~params:[
        Ref _network, "network", "The network";
      ]
      ~doc:"Reconfigure the management network interface for all Hosts in the Pool"
      ~errs:[ Api_errors.ha_is_enabled;
              Api_errors.pif_not_present;
              Api_errors.cannot_plug_bond_slave;
              Api_errors.pif_incompatible_primary_address_type;
              Api_errors.pif_has_no_network_configuration;
              Api_errors.pif_has_no_v6_network_configuration
            ]
      ~allowed_roles:_R_POOL_OP
      ()

  let hello_return = Enum("hello_return", [
      "ok", "";
      "unknown_host", "";
      "cannot_talk_back", ""
    ])

  let hello = call
      ~name:"hello"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[String, "host_uuid", "";
               String, "host_address", ""
              ]
      ~result:(hello_return, "")
      ~doc:"Internal use only"
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_OP
      ()

  let slave_network_report = call
      ~name:"slave_network_report"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~doc:"Internal use only"
      ~params:[Map (String, String), "phydevs", "(device,bridge) pairs of physical NICs on slave";
               Map (String, String), "dev_to_mac", "(device,mac) pairs of physical NICs on slave";
               Map (String, Int), "dev_to_mtu", "(device,mtu) pairs of physical NICs on slave";
               Ref _host, "slave_host", "the host that the PIFs will be attached to when created"
              ]
      ~result:(Set(Ref _pif), "refs for pifs corresponding to device list")
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let ping_slave = call ~flags:[`Session]
      ~name:"is_slave"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[Ref _host, "host", ""]
      ~doc:"Internal use only"
      ~result:(Bool, "returns false if pinged host is master [indicating critical error condition]; true if pinged host is slave")
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let ha_prevent_restarts_for = call ~flags:[`Session]
      ~name:"ha_prevent_restarts_for"
      ~in_product_since:rel_orlando_update_1
      ~doc:"When this call returns the VM restart logic will not run for the requested number of seconds. If the argument is zero then the restart thread is immediately unblocked"
      ~params:[Int, "seconds", "The number of seconds to block the restart thread for"]
      ~allowed_roles:_R_POOL_OP
      ()

  let ha_failover_plan_exists = call ~flags:[`Session]
      ~name:"ha_failover_plan_exists"
      ~in_product_since:rel_orlando
      ~doc:"Returns true if a VM failover plan exists for up to 'n' host failures"
      ~params:[Int, "n", "The number of host failures to plan for" ]
      ~result:(Bool, "true if a failover plan exists for the supplied number of host failures")
      ~allowed_roles:_R_POOL_OP
      ()

  let ha_compute_max_host_failures_to_tolerate = call ~flags:[`Session]
      ~name:"ha_compute_max_host_failures_to_tolerate"
      ~in_product_since:rel_orlando
      ~doc:"Returns the maximum number of host failures we could tolerate before we would be unable to restart configured VMs"
      ~params:[]
      ~result:(Int, "maximum value for ha_host_failures_to_tolerate given current configuration")
      ~allowed_roles:_R_POOL_OP
      ()

  let ha_compute_hypothetical_max_host_failures_to_tolerate = call ~flags:[`Session]
      ~name:"ha_compute_hypothetical_max_host_failures_to_tolerate"
      ~in_product_since:rel_orlando
      ~doc:"Returns the maximum number of host failures we could tolerate before we would be unable to restart the provided VMs"
      ~params:[ Map(Ref _vm, String), "configuration", "Map of protected VM reference to restart priority" ]
      ~result:(Int, "maximum value for ha_host_failures_to_tolerate given provided configuration")
      ~allowed_roles:_R_READ_ONLY
      ()

  let ha_compute_vm_failover_plan = call ~flags:[`Session]
      ~name:"ha_compute_vm_failover_plan"
      ~in_product_since:rel_orlando
      ~doc:"Return a VM failover plan assuming a given subset of hosts fail"
      ~params:[Set(Ref _host), "failed_hosts", "The set of hosts to assume have failed";
               Set(Ref _vm), "failed_vms", "The set of VMs to restart" ]
      ~result:(Map(Ref _vm, Map(String, String)), "VM failover plan: a map of VM to host to restart the host on")
      ~allowed_roles:_R_POOL_OP
      ()

  let create_new_blob = call
      ~name: "create_new_blob"
      ~in_product_since:rel_orlando
      ~doc:"Create a placeholder for a named binary blob of data that is associated with this pool"
      ~versioned_params:
        [{param_type=Ref _pool; param_name="pool"; param_doc="The pool"; param_release=orlando_release; param_default=None};
         {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
         {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
         {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
        ]
      ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
      ~allowed_roles:_R_POOL_OP
      ()

  let set_ha_host_failures_to_tolerate = call
      ~name:"set_ha_host_failures_to_tolerate"
      ~in_product_since:rel_orlando
      ~doc:"Set the maximum number of host failures to consider in the HA VM restart planner"
      ~params:[Ref _pool, "self", "The pool";
               Int, "value", "New number of host failures to consider"]
      ~allowed_roles:_R_POOL_OP
      ()

  let ha_schedule_plan_recomputation = call
      ~name:"ha_schedule_plan_recomputation"
      ~in_product_since:rel_orlando
      ~doc:"Signal that the plan should be recomputed (eg a host has come online)"
      ~params:[]
      ~hide_from_docs:true
      ~pool_internal:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let enable_binary_storage = call
      ~name:"enable_binary_storage"
      ~in_product_since:rel_orlando
      ~hide_from_docs:true
      ~doc:"Enable the storage of larger objects, such as RRDs, messages and binary blobs across all hosts in the pool"
      ~params:[]
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_binary_storage = call
      ~name:"disable_binary_storage"
      ~in_product_since:rel_orlando
      ~hide_from_docs:true
      ~doc:"Disable the storage of larger objects, such as RRDs, messages and binary blobs across all hosts in the pool. This will destroy all of these objects where they exist."
      ~params:[]
      ~allowed_roles:_R_POOL_OP
      ()

  let enable_external_auth = call ~flags:[`Session]
      ~name:"enable_external_auth"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~params:[
        Ref _pool, "pool", "The pool whose external authentication should be enabled";
        Map (String,String), "config", "A list of key-values containing the configuration data" ;
        String, "service_name", "The name of the service" ;
        String, "auth_type", "The type of authentication (e.g. AD for Active Directory)"
      ]
      ~doc:"This call enables external authentication on all the hosts of the pool"
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let disable_external_auth = call ~flags:[`Session]
      ~name:"disable_external_auth"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~versioned_params:[
        {param_type=Ref _pool; param_name="pool"; param_doc="The pool whose external authentication should be disabled"; param_release=george_release; param_default=None};
        {param_type=Map (String, String); param_name="config"; param_doc="Optional parameters as a list of key-values containing the configuration data"; param_release=george_release; param_default=Some (VMap [])}
      ]
      ~doc:"This call disables external authentication on all the hosts of the pool"
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let detect_nonhomogeneous_external_auth = call ~flags:[`Session]
      ~name:"detect_nonhomogeneous_external_auth"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~params:[
        Ref _pool, "pool", "The pool where to detect non-homogeneous external authentication configuration";
      ]
      ~doc:"This call asynchronously detects if the external authentication configuration in any slave is different from that in the master and raises appropriate alerts"
      ~allowed_roles:_R_POOL_OP
      ()

  let initialize_wlb = call
      ~name:"initialize_wlb"
      ~in_product_since:rel_george
      ~doc:"Initializes workload balancing monitoring on this pool with the specified wlb server"
      ~params:[String, "wlb_url", "The ip address and port to use when accessing the wlb server";
               String, "wlb_username", "The username used to authenticate with the wlb server";
               String, "wlb_password", "The password used to authenticate with the wlb server";
               String, "xenserver_username", "The username used by the wlb server to authenticate with the xenserver";
               String, "xenserver_password", "The password used by the wlb server to authenticate with the xenserver"]
      ~allowed_roles:_R_POOL_OP
      ()

  let deconfigure_wlb = call
      ~name:"deconfigure_wlb"
      ~in_product_since:rel_george
      ~doc:"Permanently deconfigures workload balancing monitoring on this pool"
      ~params:[]
      ~allowed_roles:_R_POOL_OP
      ()

  let send_wlb_configuration = call
      ~name:"send_wlb_configuration"
      ~in_product_since:rel_george
      ~doc:"Sets the pool optimization criteria for the workload balancing server"
      ~params:[Map(String, String), "config", "The configuration to use in optimizing this pool"]
      ~allowed_roles:_R_POOL_OP
      ()

  let retrieve_wlb_configuration = call
      ~name:"retrieve_wlb_configuration"
      ~in_product_since:rel_george
      ~doc:"Retrieves the pool optimization criteria from the workload balancing server"
      ~params:[]
      ~result:(Map(String,String), "The configuration used in optimizing this pool")
      ~allowed_roles:_R_READ_ONLY
      ()

  let retrieve_wlb_recommendations = call
      ~name:"retrieve_wlb_recommendations"
      ~in_product_since:rel_george
      ~doc:"Retrieves vm migrate recommendations for the pool from the workload balancing server"
      ~params:[]
      ~result:(Map(Ref _vm,Set(String)), "The list of vm migration recommendations")
      ~allowed_roles:_R_READ_ONLY
      ()

  let send_test_post = call
      ~name:"send_test_post"
      ~in_product_since:rel_george
      ~doc:"Send the given body to the given host and port, using HTTPS, and print the response.  This is used for debugging the SSL layer."
      ~params:[(String, "host", ""); (Int, "port", ""); (String, "body", "")]
      ~result:(String, "The response")
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let certificate_install = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~name:"certificate_install"
      ~doc:"Install an SSL certificate pool-wide."
      ~params:[String, "name", "A name to give the certificate";
               String, "cert", "The certificate"]
      ~allowed_roles:_R_POOL_OP
      ()

  let certificate_uninstall = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~name:"certificate_uninstall"
      ~doc:"Remove an SSL certificate."
      ~params:[String, "name", "The certificate name"]
      ~allowed_roles:_R_POOL_OP
      ()

  let certificate_list = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~name:"certificate_list"
      ~doc:"List all installed SSL certificates."
      ~result:(Set(String),"All installed certificates")
      ~allowed_roles:_R_POOL_OP
      ()

  let crl_install = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~name:"crl_install"
      ~doc:"Install an SSL certificate revocation list, pool-wide."
      ~params:[String, "name", "A name to give the CRL";
               String, "cert", "The CRL"]
      ~allowed_roles:_R_POOL_OP
      ()

  let crl_uninstall = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~name:"crl_uninstall"
      ~doc:"Remove an SSL certificate revocation list."
      ~params:[String, "name", "The CRL name"]
      ~allowed_roles:_R_POOL_OP
      ()

  let crl_list = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~name:"crl_list"
      ~doc:"List all installed SSL certificate revocation lists."
      ~result:(Set(String), "All installed CRLs")
      ~allowed_roles:_R_POOL_OP
      ()

  let certificate_sync = call
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~name:"certificate_sync"
      ~doc:"Sync SSL certificates from master to slaves."
      ~allowed_roles:_R_POOL_OP
      ()

  let enable_redo_log = call
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~name:"enable_redo_log"
      ~params:[Ref _sr, "sr", "SR to hold the redo log."]
      ~doc:"Enable the redo log on the given SR and start using it, unless HA is enabled."
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_redo_log = call
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~name:"disable_redo_log"
      ~doc:"Disable the redo log if in use, unless HA is enabled."
      ~allowed_roles:_R_POOL_OP
      ()

  let audit_log_append = call
      ~in_oss_since:None
      ~pool_internal:true
      ~hide_from_docs:true
      ~in_product_since:rel_midnight_ride
      ~name:"audit_log_append"
      ~params:[String, "line", "line to be appended to the audit log"]
      ~doc:"Append a line to the audit log on the master."
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let set_vswitch_controller = call
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~lifecycle:[
        Published, rel_midnight_ride, "Set the IP address of the vswitch controller.";
        Extended, rel_cowley, "Allow to be set to the empty string (no controller is used).";
        Deprecated, rel_falcon, "Deprecated: use 'SDN_controller.introduce' and 'SDN_controller.forget' instead."]
      ~name:"set_vswitch_controller"
      ~params:[String, "address", "IP address of the vswitch controller."]
      ~doc:"Set the IP address of the vswitch controller."
      ~allowed_roles:_R_POOL_OP
      ()

  let test_archive_target = call ~flags:[`Session]
      ~name:"test_archive_target"
      ~in_oss_since:None
      ~in_product_since:rel_cowley
      ~params:[Ref _pool, "self", "Reference to the pool";
               Map(String,String), "config", "Location config settings to test";
              ]
      ~doc:"This call tests if a location is valid"
      ~allowed_roles:_R_POOL_OP
      ~result:(String, "An XMLRPC result")
      ()

  let enable_local_storage_caching = call
      ~name:"enable_local_storage_caching"
      ~in_oss_since:None
      ~in_product_since:rel_cowley
      ~params:[Ref _pool, "self", "Reference to the pool"]
      ~doc:"This call attempts to enable pool-wide local storage caching"
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_local_storage_caching = call
      ~name:"disable_local_storage_caching"
      ~in_oss_since:None
      ~in_product_since:rel_cowley
      ~params:[Ref _pool, "self", "Reference to the pool"]
      ~doc:"This call disables pool-wide local storage caching"
      ~allowed_roles:_R_POOL_OP
      ()

  let get_license_state = call
      ~name:"get_license_state"
      ~in_oss_since:None
      ~in_product_since:rel_clearwater
      ~params:[Ref _pool, "self", "Reference to the pool"]
      ~doc:"This call returns the license state for the pool"
      ~allowed_roles:_R_READ_ONLY
      ~result:(Map(String,String), "The pool's license state")
      ()

  let apply_edition = call
      ~name:"apply_edition"
      ~in_oss_since:None
      ~in_product_since:rel_clearwater
      ~params:[
        Ref _pool, "self", "Reference to the pool";
        String, "edition", "The requested edition";
      ]
      ~doc:"Apply an edition to all hosts in the pool"
      ~allowed_roles:_R_POOL_OP
      ()

  let enable_ssl_legacy = call
      ~name:"enable_ssl_legacy"
      ~in_oss_since:None
      ~lifecycle:[
        Published, rel_dundee, "";
      ]
      ~params:[Ref _pool, "self", "(ignored)";]
      ~doc:"Sets ssl_legacy true on each host, pool-master last. See Host.ssl_legacy and Host.set_ssl_legacy."
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_ssl_legacy = call
      ~name:"disable_ssl_legacy"
      ~in_oss_since:None
      ~lifecycle:[
        Published, rel_dundee, "";
      ]
      ~params:[Ref _pool, "self", "(ignored)";]
      ~doc:"Sets ssl_legacy true on each host, pool-master last. See Host.ssl_legacy and Host.set_ssl_legacy."
      ~allowed_roles:_R_POOL_OP
      ()

  let set_igmp_snooping_enabled = call
      ~in_oss_since:None
      ~lifecycle:[
        Published, rel_inverness, "Enable or disable IGMP Snooping on the pool.";
      ]
      ~name:"set_igmp_snooping_enabled"
      ~params:[
        Ref _pool, "self", "The pool";
        Bool, "value", "Enable or disable IGMP Snooping on the pool"
      ]
      ~doc:"Enable or disable IGMP Snooping on the pool."
      ~allowed_roles:_R_POOL_OP
      ()

  let has_extension = call
      ~name:"has_extension"
      ~in_product_since:rel_dundee
      ~doc:"Return true if the extension is available on the pool"
      ~params:[
        Ref _pool, "self", "The pool";
        String, "name", "The name of the API call"
      ]
      ~result:(Bool, "True if the extension exists, false otherwise")
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let add_to_guest_agent_config = call
      ~name:"add_to_guest_agent_config"
      ~in_product_since:rel_dundee
      ~doc:"Add a key-value pair to the pool-wide guest agent configuration"
      ~params:[
        Ref _pool, "self", "The pool";
        String, "key", "The key to add";
        String, "value", "The value to add";
      ]
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let remove_from_guest_agent_config = call
      ~name:"remove_from_guest_agent_config"
      ~in_product_since:rel_dundee
      ~doc:"Remove a key-value pair from the pool-wide guest agent configuration"
      ~params:[
        Ref _pool, "self", "The pool";
        String, "key", "The key to remove";
      ]
      ~allowed_roles:_R_POOL_ADMIN
      ()

  (** A pool class *)
  let t =
    create_obj
      ~in_db:true
      ~in_product_since:rel_rio
      ~in_oss_since:None
      ~internal_deprecated_since:None
      ~persist:PersistEverything
      ~gen_constructor_destructor:false
      ~name:_pool
      ~descr:"Pool-wide information"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [ join
        ; join_force
        ; eject
        ; initial_auth
        ; transition_to_master
        ; slave_reset_master
        ; recover_slaves
        ; hello
        ; ping_slave
        ; create_VLAN
        ; management_reconfigure
        ; create_VLAN_from_PIF
        ; slave_network_report
        ; enable_ha
        ; disable_ha
        ; sync_database
        ; designate_new_master
        ; ha_prevent_restarts_for
        ; ha_failover_plan_exists
        ; ha_compute_max_host_failures_to_tolerate
        ; ha_compute_hypothetical_max_host_failures_to_tolerate
        ; ha_compute_vm_failover_plan
        ; set_ha_host_failures_to_tolerate
        ; create_new_blob
        ; ha_schedule_plan_recomputation
        ; enable_binary_storage
        ; disable_binary_storage
        ; enable_external_auth
        ; disable_external_auth
        ; detect_nonhomogeneous_external_auth
        ; initialize_wlb
        ; deconfigure_wlb
        ; send_wlb_configuration
        ; retrieve_wlb_configuration
        ; retrieve_wlb_recommendations
        ; send_test_post
        ; certificate_install
        ; certificate_uninstall
        ; certificate_list
        ; crl_install
        ; crl_uninstall
        ; crl_list
        ; certificate_sync
        ; enable_redo_log
        ; disable_redo_log
        ; audit_log_append
        ; set_vswitch_controller
        ; test_archive_target
        ; enable_local_storage_caching
        ; disable_local_storage_caching
        ; get_license_state
        ; apply_edition
        ; enable_ssl_legacy
        ; disable_ssl_legacy
        ; set_igmp_snooping_enabled
        ; has_extension
        ; add_to_guest_agent_config
        ; remove_from_guest_agent_config
        ]
      ~contents:
        ([uid ~in_oss_since:None _pool] @
         [ field ~in_oss_since:None ~qualifier:RW ~ty:String "name_label" "Short name"
         ; field ~in_oss_since:None ~qualifier:RW ~ty:String "name_description" "Description"
         ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Ref _host) "master" "The host that is pool master"
         ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "default_SR" "Default SR for VDIs"
         ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "suspend_image_SR" "The SR in which VDIs for suspend images are created"
         ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "crash_dump_SR" "The SR in which VDIs for crash dumps are created"
         ; field ~in_oss_since:None ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP));("EMPTY_FOLDERS",(_R_VM_OP))]
         ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "ha_enabled" "true if HA is enabled on the pool, false otherwise"
         ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "ha_configuration" "The current HA configuration"
         ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_statefiles" "HA statefile VDIs in use"
         ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "ha_host_failures_to_tolerate" "Number of host failures to tolerate before the Pool is declared to be overcommitted"
         ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "ha_plan_exists_for" "Number of future host failures we have managed to find a plan for. Once this reaches zero any future host failures will cause the failure of protected VMs."
         ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "ha_allow_overcommit" "If set to false then operations which would cause the Pool to become overcommitted will be blocked."
         ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "ha_overcommitted" "True if the Pool is considered to be overcommitted i.e. if there exist insufficient physical resources to tolerate the configured number of host failures"
         ; field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this pool"
         ; field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes"
         ; field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "gui_config" "gui-specific configuration for pool"
         ; field ~writer_roles:_R_POOL_OP ~in_product_since:rel_dundee ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "health_check_config" "Configuration for the automatic health check feature"
         ; field ~in_product_since:rel_george ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "wlb_url" "Url for the configured workload balancing host"
         ; field ~in_product_since:rel_george ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "wlb_username" "Username for accessing the workload balancing host"
         ; field ~in_product_since:rel_george ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _secret) "wlb_password" "Password for accessing the workload balancing host"
         ; field ~in_product_since:rel_george ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "wlb_enabled" "true if workload balancing is enabled on the pool, false otherwise"
         ; field ~in_product_since:rel_george ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "wlb_verify_cert" "true if communication with the WLB server should enforce SSL certificate verification."
         ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "redo_log_enabled" "true a redo-log is to be used other than when HA is enabled, false otherwise"
         ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:(Ref _vdi) ~default_value:(Some (VRef null_ref)) "redo_log_vdi" "indicates the VDI to use for the redo-log other than when HA is enabled"
         ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "vswitch_controller" "address of the vswitch controller"
             ~lifecycle:[
               Published, rel_midnight_ride, "the IP address of the vswitch controller.";
               Deprecated, rel_falcon, "Deprecated: set the IP address of the vswitch controller in SDN_controller instead."]
         ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "restrictions" "Pool-wide restrictions currently in effect"
         ; field ~in_oss_since:None ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:(Set (Ref _vdi)) "metadata_VDIs" "The set of currently known metadata VDIs for this pool"
         ; field ~in_oss_since:None ~in_product_since:rel_dundee ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String "ha_cluster_stack" "The HA cluster stack that is currently in use. Only valid when HA is enabled."
         ] @ (allowed_and_current_operations operations) @
         [ field ~in_oss_since:None ~in_product_since:rel_dundee ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "guest_agent_config" "Pool-wide guest agent configuration information"
         ; field ~qualifier:DynamicRO ~in_product_since:rel_dundee ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "cpu_info" "Details about the physical CPUs on the pool"
         ; field ~qualifier:RW ~in_product_since:rel_dundee ~default_value:(Some (VBool false)) ~ty:Bool "policy_no_vendor_device" "The pool-wide policy for clients on whether to use the vendor device or not on newly created VMs. This field will also be consulted if the 'has_vendor_device' field is not specified in the VM.create call."
         ; field ~qualifier:RW ~in_product_since:rel_ely ~default_value:(Some (VBool false)) ~ty:Bool "live_patching_disabled" "The pool-wide flag to show if the live patching feauture is disabled or not."
         ; field ~in_product_since:rel_inverness ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "igmp_snooping_enabled" "true if IGMP snooping is enabled in the pool, false otherwise."
         ; field ~in_product_since:rel_oslo ~qualifier:RW ~ty:String ~default_value:(Some (VString "")) "uefi_certificates" "The UEFI certificates allowing Secure Boot"
         ])
      ()
