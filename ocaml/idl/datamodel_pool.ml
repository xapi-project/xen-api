open Datamodel_common
open Datamodel_roles
open Datamodel_types

let operations =
  Enum
    ( "pool_allowed_operations"
    , (* FIXME: This should really be called `pool_operations`, to avoid confusion with the Pool.allowed_operations field *)
      [
        ("ha_enable", "Indicates this pool is in the process of enabling HA")
      ; ("ha_disable", "Indicates this pool is in the process of disabling HA")
      ; ( "cluster_create"
        , "Indicates this pool is in the process of creating a cluster"
        )
      ; ( "designate_new_master"
        , "Indicates this pool is in the process of changing master"
        )
      ; ( "configure_repositories"
        , "Indicates this pool is in the process of configuring repositories"
        )
      ; ( "sync_updates"
        , "Indicates this pool is in the process of syncing updates"
        )
      ; ( "get_updates"
        , "Indicates this pool is in the process of getting updates"
        )
      ; ( "apply_updates"
        , "Indicates this pool is in the process of applying updates"
        )
      ; (* ops involving cert distribution; these do not necessarily correspond to 'Pool.x' commands,
         * but they do require a pool-wide lock *)
        ( "tls_verification_enable"
        , "Indicates this pool is in the process of enabling TLS verification"
        )
      ; ("cert_refresh", "A certificate refresh and distribution is in progress")
      ; ( "exchange_certificates_on_join"
        , "Indicates this pool is exchanging internal certificates with a new \
           joiner"
        )
      ; ( "exchange_ca_certificates_on_join"
        , "Indicates this pool is exchanging ca certificates with a new joiner"
        )
      ; ( "copy_primary_host_certs"
        , "Indicates the primary host is sending its certificates to another \
           host"
        )
      ]
    )

let telemetry_frequency =
  Enum
    ( "telemetry_frequency"
    , [
        ("daily", "Run telemetry task daily")
      ; ("weekly", "Run telemetry task weekly")
      ; ("monthly", "Run telemetry task monthly")
      ]
    )

let enable_ha =
  call ~in_product_since:rel_miami ~name:"enable_ha" ~in_oss_since:None
    ~versioned_params:
      [
        {
          param_type= Set (Ref _sr)
        ; param_name= "heartbeat_srs"
        ; param_doc= "Set of SRs to use for storage heartbeating"
        ; param_release= miami_release
        ; param_default= None
        }
      ; {
          param_type= Map (String, String)
        ; param_name= "configuration"
        ; param_doc= "Detailed HA configuration to apply"
        ; param_release= miami_release
        ; param_default= None
        }
      ]
    ~doc:"Turn on High Availability mode"
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let disable_ha =
  call ~in_product_since:rel_miami ~name:"disable_ha" ~in_oss_since:None
    ~params:[] ~doc:"Turn off High Availability mode"
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let sync_database =
  call ~name:"sync_database" ~in_oss_since:None ~in_product_since:rel_rio
    ~params:[] ~doc:"Forcibly synchronise the database now"
    ~allowed_roles:_R_POOL_OP ()

let designate_new_master =
  call ~in_product_since:rel_miami ~name:"designate_new_master"
    ~in_oss_since:None
    ~params:[(Ref _host, "host", "The host who should become the new master")]
    ~doc:
      "Perform an orderly handover of the role of master to the referenced \
       host."
    ~allowed_roles:_R_POOL_OP ()

let join =
  call ~name:"join" ~in_oss_since:None ~in_product_since:rel_rio
    ~params:
      [
        ( String
        , "master_address"
        , "The hostname of the master of the pool to join"
        )
      ; ( String
        , "master_username"
        , "The username of the master (for initial authentication)"
        )
      ; ( String
        , "master_password"
        , "The password for the master (for initial authentication)"
        )
      ]
    ~errs:[Api_errors.pool_joining_host_cannot_contain_shared_SRs]
    ~doc:"Instruct host to join a new pool" ~allowed_roles:_R_POOL_OP ()

let join_force =
  call ~name:"join_force" ~in_oss_since:None ~in_product_since:rel_rio
    ~params:
      [
        ( String
        , "master_address"
        , "The hostname of the master of the pool to join"
        )
      ; ( String
        , "master_username"
        , "The username of the master (for initial authentication)"
        )
      ; ( String
        , "master_password"
        , "The password for the master (for initial authentication)"
        )
      ]
    ~doc:"Instruct host to join a new pool" ~allowed_roles:_R_POOL_OP ()

(* This is a map of uuid -> cert_blob *)
let certs = Map (String, String)

let exchange_certificates_on_join =
  call ~name:"exchange_certificates_on_join" ~in_oss_since:None
    ~in_product_since:"1.298.0"
    ~params:
      [
        (String, "uuid", "The uuid of the joining host")
      ; (String, "certificate", "The contents of the joiner's certificate")
      ]
    ~result:(certs, "The contents of the pool's certificates")
    ~doc:
      "Install the pool certificate of a joiner and return the pool's \
       certificates"
    ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

let exchange_ca_certificates_on_join =
  call ~name:"exchange_ca_certificates_on_join" ~in_oss_since:None
    ~in_product_since:"1.320.0"
    ~params:
      [
        (certs, "import", "The CA certificates that are to be installed")
      ; ( Set (Ref _certificate)
        , "export"
        , "The CA certificates that will be returned, ready to be installed"
        )
      ]
    ~result:(certs, "The contents of the CA certificates requested")
    ~doc:
      "Install the CA certificates of a joiner and return the requested CA \
       certificates"
    ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

let slave_reset_master =
  call ~flags:[`Session] ~name:"emergency_reset_master" ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[(String, "master_address", "The hostname of the master")]
    ~doc:"Instruct a slave already in a pool that the master has changed"
    ~allowed_roles:_R_POOL_OP ()

let transition_to_master =
  call ~flags:[`Session] ~name:"emergency_transition_to_master"
    ~in_oss_since:None ~in_product_since:rel_rio ~params:[]
    ~doc:"Instruct host that's currently a slave to transition to being master"
    ~allowed_roles:_R_POOL_OP ()

let recover_slaves =
  call ~name:"recover_slaves" ~in_oss_since:None ~in_product_since:rel_rio
    ~params:[]
    ~result:
      ( Set (Ref _host)
      , "list of hosts whose master address were successfully reset"
      )
    ~doc:
      "Instruct a pool master, M, to try and contact its slaves and, if slaves \
       are in emergency mode, reset their master address to M."
    ~allowed_roles:_R_POOL_OP ()

let eject =
  call ~name:"eject" ~in_oss_since:None ~in_product_since:rel_rio
    ~params:[(Ref _host, "host", "The host to eject")]
    ~doc:"Instruct a pool master to eject a host from the pool"
    ~allowed_roles:_R_POOL_OP ()

let initial_auth =
  call ~name:"initial_auth" ~in_oss_since:None ~in_product_since:rel_rio
    ~params:[] ~result:(SecretString, "") ~doc:"Internal use only"
    ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

let create_VLAN_from_PIF =
  call ~in_oss_since:None ~in_product_since:rel_rio ~name:"create_VLAN_from_PIF"
    ~doc:"Create a pool-wide VLAN by taking the PIF."
    ~params:
      [
        ( Ref _pif
        , "pif"
        , "physical interface on any particular host, that identifies the PIF \
           on which to create the (pool-wide) VLAN interface"
        )
      ; ( Ref _network
        , "network"
        , "network to which this interface should be connected"
        )
      ; (Int, "VLAN", "VLAN tag for the new interface")
      ]
    ~result:(Set (Ref _pif), "The references of the created PIF objects")
    ~errs:[Api_errors.vlan_tag_invalid]
    ~allowed_roles:_R_POOL_OP ()

(* !! THIS IS BROKEN; it takes a device name which in the case of a bond is not homogeneous across all pool hosts.
      See CA-22613. !! *)
let create_VLAN =
  call ~in_oss_since:None ~in_product_since:rel_rio ~name:"create_VLAN"
    ~doc:
      "Create PIFs, mapping a network to the same physical interface/VLAN on \
       each host. This call is deprecated: use Pool.create_VLAN_from_PIF \
       instead."
    ~params:
      [
        ( String
        , "device"
        , "physical interface on which to create the VLAN interface"
        )
      ; ( Ref _network
        , "network"
        , "network to which this interface should be connected"
        )
      ; (Int, "VLAN", "VLAN tag for the new interface")
      ]
    ~result:(Set (Ref _pif), "The references of the created PIF objects")
    ~errs:[Api_errors.vlan_tag_invalid]
    ~allowed_roles:_R_POOL_OP ()

let management_reconfigure =
  call ~name:"management_reconfigure" ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~params:[(Ref _network, "network", "The network")]
    ~doc:
      "Reconfigure the management network interface for all Hosts in the Pool"
    ~errs:
      [
        Api_errors.ha_is_enabled
      ; Api_errors.pif_not_present
      ; Api_errors.cannot_plug_bond_slave
      ; Api_errors.pif_incompatible_primary_address_type
      ; Api_errors.pif_has_no_network_configuration
      ; Api_errors.pif_has_no_v6_network_configuration
      ]
    ~allowed_roles:_R_POOL_OP ()

let hello_return =
  Enum
    ( "hello_return"
    , [("ok", ""); ("unknown_host", ""); ("cannot_talk_back", "")]
    )

let hello =
  call ~name:"hello" ~in_oss_since:None ~in_product_since:rel_rio
    ~params:[(String, "host_uuid", ""); (String, "host_address", "")]
    ~result:(hello_return, "") ~doc:"Internal use only" ~hide_from_docs:true
    ~allowed_roles:_R_POOL_OP ()

let ping_slave =
  call ~flags:[`Session] ~name:"is_slave" ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[(Ref _host, "host", "")]
    ~doc:"Internal use only"
    ~result:
      ( Bool
      , "returns false if pinged host is master [indicating critical error \
         condition]; true if pinged host is slave"
      )
    ~hide_from_docs:true ~allowed_roles:_R_POOL_ADMIN ()

let ha_prevent_restarts_for =
  call ~flags:[`Session] ~name:"ha_prevent_restarts_for"
    ~in_product_since:rel_orlando_update_1
    ~doc:
      "When this call returns the VM restart logic will not run for the \
       requested number of seconds. If the argument is zero then the restart \
       thread is immediately unblocked"
    ~params:
      [
        (Int, "seconds", "The number of seconds to block the restart thread for")
      ]
    ~allowed_roles:_R_POOL_OP ()

let ha_failover_plan_exists =
  call ~flags:[`Session] ~name:"ha_failover_plan_exists"
    ~in_product_since:rel_orlando
    ~doc:"Returns true if a VM failover plan exists for up to 'n' host failures"
    ~params:[(Int, "n", "The number of host failures to plan for")]
    ~result:
      ( Bool
      , "true if a failover plan exists for the supplied number of host \
         failures"
      )
    ~allowed_roles:_R_POOL_OP ()

let ha_compute_max_host_failures_to_tolerate =
  call ~flags:[`Session] ~name:"ha_compute_max_host_failures_to_tolerate"
    ~in_product_since:rel_orlando
    ~doc:
      "Returns the maximum number of host failures we could tolerate before we \
       would be unable to restart configured VMs"
    ~params:[]
    ~result:
      ( Int
      , "maximum value for ha_host_failures_to_tolerate given current \
         configuration"
      )
    ~allowed_roles:_R_POOL_OP ()

let ha_compute_hypothetical_max_host_failures_to_tolerate =
  call ~flags:[`Session]
    ~name:"ha_compute_hypothetical_max_host_failures_to_tolerate"
    ~in_product_since:rel_orlando
    ~doc:
      "Returns the maximum number of host failures we could tolerate before we \
       would be unable to restart the provided VMs"
    ~params:
      [
        ( Map (Ref _vm, String)
        , "configuration"
        , "Map of protected VM reference to restart priority"
        )
      ]
    ~result:
      ( Int
      , "maximum value for ha_host_failures_to_tolerate given provided \
         configuration"
      )
    ~allowed_roles:_R_READ_ONLY ()

let ha_compute_vm_failover_plan =
  call ~flags:[`Session] ~name:"ha_compute_vm_failover_plan"
    ~in_product_since:rel_orlando
    ~doc:"Return a VM failover plan assuming a given subset of hosts fail"
    ~params:
      [
        ( Set (Ref _host)
        , "failed_hosts"
        , "The set of hosts to assume have failed"
        )
      ; (Set (Ref _vm), "failed_vms", "The set of VMs to restart")
      ]
    ~result:
      ( Map (Ref _vm, Map (String, String))
      , "VM failover plan: a map of VM to host to restart the host on"
      )
    ~allowed_roles:_R_POOL_OP ()

let create_new_blob =
  call ~name:"create_new_blob" ~in_product_since:rel_orlando
    ~doc:
      "Create a placeholder for a named binary blob of data that is associated \
       with this pool"
    ~versioned_params:
      [
        {
          param_type= Ref _pool
        ; param_name= "pool"
        ; param_doc= "The pool"
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

let set_ha_host_failures_to_tolerate =
  call ~name:"set_ha_host_failures_to_tolerate" ~in_product_since:rel_orlando
    ~doc:
      "Set the maximum number of host failures to consider in the HA VM \
       restart planner"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; (Int, "value", "New number of host failures to consider")
      ]
    ~allowed_roles:_R_POOL_OP ()

let ha_schedule_plan_recomputation =
  call ~name:"ha_schedule_plan_recomputation" ~in_product_since:rel_orlando
    ~doc:"Signal that the plan should be recomputed (eg a host has come online)"
    ~params:[] ~hide_from_docs:true ~pool_internal:true
    ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

let enable_binary_storage =
  call ~name:"enable_binary_storage" ~in_product_since:rel_orlando
    ~hide_from_docs:true
    ~doc:
      "Enable the storage of larger objects, such as RRDs, messages and binary \
       blobs across all hosts in the pool"
    ~params:[] ~allowed_roles:_R_POOL_OP ()

let disable_binary_storage =
  call ~name:"disable_binary_storage" ~in_product_since:rel_orlando
    ~hide_from_docs:true
    ~doc:
      "Disable the storage of larger objects, such as RRDs, messages and \
       binary blobs across all hosts in the pool. This will destroy all of \
       these objects where they exist."
    ~params:[] ~allowed_roles:_R_POOL_OP ()

let enable_external_auth =
  call ~flags:[`Session] ~name:"enable_external_auth" ~in_oss_since:None
    ~in_product_since:rel_george
    ~params:
      [
        ( Ref _pool
        , "pool"
        , "The pool whose external authentication should be enabled"
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
    ~doc:
      "This call enables external authentication on all the hosts of the pool"
    ~allowed_roles:_R_POOL_ADMIN ()

let disable_external_auth =
  call ~flags:[`Session] ~name:"disable_external_auth" ~in_oss_since:None
    ~in_product_since:rel_george
    ~versioned_params:
      [
        {
          param_type= Ref _pool
        ; param_name= "pool"
        ; param_doc= "The pool whose external authentication should be disabled"
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
    ~doc:
      "This call disables external authentication on all the hosts of the pool"
    ~allowed_roles:_R_POOL_ADMIN ()

let detect_nonhomogeneous_external_auth =
  call ~flags:[`Session] ~name:"detect_nonhomogeneous_external_auth"
    ~in_oss_since:None ~in_product_since:rel_george
    ~params:
      [
        ( Ref _pool
        , "pool"
        , "The pool where to detect non-homogeneous external authentication \
           configuration"
        )
      ]
    ~doc:
      "This call asynchronously detects if the external authentication \
       configuration in any slave is different from that in the master and \
       raises appropriate alerts"
    ~allowed_roles:_R_POOL_OP ()

let initialize_wlb =
  call ~name:"initialize_wlb" ~in_product_since:rel_george
    ~doc:
      "Initializes workload balancing monitoring on this pool with the \
       specified wlb server"
    ~params:
      [
        ( String
        , "wlb_url"
        , "The ip address and port to use when accessing the wlb server"
        )
      ; ( String
        , "wlb_username"
        , "The username used to authenticate with the wlb server"
        )
      ; ( String
        , "wlb_password"
        , "The password used to authenticate with the wlb server"
        )
      ; ( String
        , "xenserver_username"
        , "The username used by the wlb server to authenticate with the \
           xenserver"
        )
      ; ( String
        , "xenserver_password"
        , "The password used by the wlb server to authenticate with the \
           xenserver"
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

let deconfigure_wlb =
  call ~name:"deconfigure_wlb" ~in_product_since:rel_george
    ~doc:"Permanently deconfigures workload balancing monitoring on this pool"
    ~params:[] ~allowed_roles:_R_POOL_OP ()

let send_wlb_configuration =
  call ~name:"send_wlb_configuration" ~in_product_since:rel_george
    ~doc:"Sets the pool optimization criteria for the workload balancing server"
    ~params:
      [
        ( Map (String, String)
        , "config"
        , "The configuration to use in optimizing this pool"
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

let retrieve_wlb_configuration =
  call ~name:"retrieve_wlb_configuration" ~in_product_since:rel_george
    ~doc:
      "Retrieves the pool optimization criteria from the workload balancing \
       server"
    ~params:[]
    ~result:
      (Map (String, String), "The configuration used in optimizing this pool")
    ~allowed_roles:_R_READ_ONLY ()

let retrieve_wlb_recommendations =
  call ~name:"retrieve_wlb_recommendations" ~in_product_since:rel_george
    ~doc:
      "Retrieves vm migrate recommendations for the pool from the workload \
       balancing server"
    ~params:[]
    ~result:
      (Map (Ref _vm, Set String), "The list of vm migration recommendations")
    ~allowed_roles:_R_READ_ONLY ()

let send_test_post =
  call ~name:"send_test_post" ~in_product_since:rel_george
    ~doc:
      "Send the given body to the given host and port, using HTTPS, and print \
       the response.  This is used for debugging the SSL layer."
    ~params:[(String, "host", ""); (Int, "port", ""); (String, "body", "")]
    ~result:(String, "The response") ~allowed_roles:_R_POOL_ADMIN ()

let certificate_install =
  call ~name:"certificate_install"
    ~doc:"Install a TLS CA certificate, pool-wide."
    ~params:
      [
        (String, "name", "A name to give the certificate")
      ; (String, "cert", "The certificate in PEM format")
      ]
    ~allowed_roles:_R_POOL_OP
    ~lifecycle:
      [
        (Published, rel_george, "Install TLS CA certificate")
      ; (Deprecated, "1.290.0", "Use Pool.install_ca_certificate instead")
      ]
    ()

let install_ca_certificate =
  call ~name:"install_ca_certificate"
    ~doc:"Install a TLS CA certificate, pool-wide."
    ~params:
      [
        (String, "name", "A name to give the certificate")
      ; (String, "cert", "The certificate in PEM format")
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ~lifecycle:[(Published, "1.290.0", "Install TLS CA certificate")]
    ()

let certificate_uninstall =
  call ~name:"certificate_uninstall"
    ~doc:"Remove a pool-wide TLS CA certificate."
    ~params:[(String, "name", "The certificate name")]
    ~allowed_roles:_R_POOL_OP
    ~lifecycle:
      [
        (Published, rel_george, "Install TLS CA certificate")
      ; (Deprecated, "1.290.0", "Use Pool.uninstall_ca_certificate instead")
      ]
    ()

let uninstall_ca_certificate =
  call ~name:"uninstall_ca_certificate"
    ~doc:"Remove a pool-wide TLS CA certificate."
    ~params:[(String, "name", "The certificate name")]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ~lifecycle:[(Published, "1.290.0", "Uninstall TLS CA certificate")]
    ()

let certificate_list =
  call ~name:"certificate_list"
    ~doc:"List the names of all installed TLS CA certificates."
    ~result:(Set String, "All installed certificates")
    ~allowed_roles:_R_POOL_OP
    ~lifecycle:
      [
        (Published, rel_george, "List installed TLS CA certificate")
      ; (Deprecated, "1.290.0", "Use openssl to inspect certificate")
      ]
    ()

let crl_install =
  call ~in_oss_since:None ~in_product_since:rel_george ~name:"crl_install"
    ~doc:"Install a TLS CA-issued Certificate Revocation List, pool-wide."
    ~params:
      [(String, "name", "A name to give the CRL"); (String, "cert", "The CRL")]
    ~allowed_roles:_R_POOL_OP ()

let crl_uninstall =
  call ~in_oss_since:None ~in_product_since:rel_george ~name:"crl_uninstall"
    ~doc:"Remove a pool-wide TLS CA-issued Certificate Revocation List."
    ~params:[(String, "name", "The CRL name")]
    ~allowed_roles:_R_POOL_OP ()

let crl_list =
  call ~in_oss_since:None ~in_product_since:rel_george ~name:"crl_list"
    ~doc:
      "List the names of all installed TLS CA-issued Certificate Revocation \
       Lists."
    ~result:(Set String, "The names of all installed CRLs")
    ~allowed_roles:_R_POOL_OP ()

let certificate_sync =
  call ~in_oss_since:None ~in_product_since:rel_george ~name:"certificate_sync"
    ~doc:"Copy the TLS CA certificates and CRLs of the master to all slaves."
    ~allowed_roles:_R_POOL_OP ()

let enable_tls_verification =
  call ~flags:[`Session]
    ~lifecycle:[(Published, "1.290.0", "")]
    ~name:"enable_tls_verification"
    ~doc:"Enable TLS server certificate verification"
    ~allowed_roles:_R_POOL_ADMIN ()

let enable_redo_log =
  call ~in_oss_since:None ~in_product_since:rel_midnight_ride
    ~name:"enable_redo_log"
    ~params:[(Ref _sr, "sr", "SR to hold the redo log.")]
    ~doc:
      "Enable the redo log on the given SR and start using it, unless HA is \
       enabled."
    ~allowed_roles:_R_POOL_OP ()

let disable_redo_log =
  call ~in_oss_since:None ~in_product_since:rel_midnight_ride
    ~name:"disable_redo_log"
    ~doc:"Disable the redo log if in use, unless HA is enabled."
    ~allowed_roles:_R_POOL_OP ()

let audit_log_append =
  call ~in_oss_since:None ~pool_internal:true ~hide_from_docs:true
    ~in_product_since:rel_midnight_ride ~name:"audit_log_append"
    ~params:[(String, "line", "line to be appended to the audit log")]
    ~doc:"Append a line to the audit log on the master."
    ~allowed_roles:_R_POOL_ADMIN ()

let set_vswitch_controller =
  call ~in_oss_since:None ~in_product_since:rel_midnight_ride
    ~lifecycle:
      [
        ( Published
        , rel_midnight_ride
        , "Set the IP address of the vswitch controller."
        )
      ; ( Extended
        , rel_cowley
        , "Allow to be set to the empty string (no controller is used)."
        )
      ; ( Deprecated
        , rel_falcon
        , "Deprecated: use 'SDN_controller.introduce' and \
           'SDN_controller.forget' instead."
        )
      ]
    ~name:"set_vswitch_controller"
    ~params:[(String, "address", "IP address of the vswitch controller.")]
    ~doc:"Set the IP address of the vswitch controller."
    ~allowed_roles:_R_POOL_OP ()

let test_archive_target =
  call ~flags:[`Session] ~name:"test_archive_target" ~in_oss_since:None
    ~in_product_since:rel_cowley
    ~params:
      [
        (Ref _pool, "self", "Reference to the pool")
      ; (Map (String, String), "config", "Location config settings to test")
      ]
    ~doc:"This call tests if a location is valid" ~allowed_roles:_R_POOL_OP
    ~result:(String, "An XMLRPC result")
    ()

let enable_local_storage_caching =
  call ~name:"enable_local_storage_caching" ~in_oss_since:None
    ~in_product_since:rel_cowley
    ~params:[(Ref _pool, "self", "Reference to the pool")]
    ~doc:"This call attempts to enable pool-wide local storage caching"
    ~allowed_roles:_R_POOL_OP ()

let disable_local_storage_caching =
  call ~name:"disable_local_storage_caching" ~in_oss_since:None
    ~in_product_since:rel_cowley
    ~params:[(Ref _pool, "self", "Reference to the pool")]
    ~doc:"This call disables pool-wide local storage caching"
    ~allowed_roles:_R_POOL_OP ()

let get_license_state =
  call ~name:"get_license_state" ~in_oss_since:None
    ~in_product_since:rel_clearwater
    ~params:[(Ref _pool, "self", "Reference to the pool")]
    ~doc:"This call returns the license state for the pool"
    ~allowed_roles:_R_READ_ONLY
    ~result:(Map (String, String), "The pool's license state")
    ()

let apply_edition =
  call ~name:"apply_edition" ~in_oss_since:None ~in_product_since:rel_clearwater
    ~params:
      [
        (Ref _pool, "self", "Reference to the pool")
      ; (String, "edition", "The requested edition")
      ]
    ~doc:"Apply an edition to all hosts in the pool" ~allowed_roles:_R_POOL_OP
    ()

let enable_ssl_legacy =
  call ~name:"enable_ssl_legacy" ~in_oss_since:None
    ~lifecycle:
      [
        (Published, rel_dundee, "")
      ; (Deprecated, rel_dundee, "Legacy SSL will soon cease to be supported")
      ; (Removed, rel_stockholm, "Legacy SSL no longer supported")
      ]
    ~params:[(Ref _pool, "self", "(ignored)")]
    ~doc:
      "Sets ssl_legacy true on each host, pool-master last. See \
       Host.ssl_legacy and Host.set_ssl_legacy."
    ~allowed_roles:_R_POOL_OP ()

let disable_ssl_legacy =
  call ~name:"disable_ssl_legacy" ~in_oss_since:None
    ~lifecycle:
      [
        (Published, rel_dundee, "")
      ; (Deprecated, rel_stockholm, "Legacy SSL no longer supported")
      ]
    ~params:[(Ref _pool, "self", "(ignored)")]
    ~doc:
      "Sets ssl_legacy false on each host, pool-master last. See \
       Host.ssl_legacy and Host.set_ssl_legacy."
    ~allowed_roles:_R_POOL_OP ()

let set_igmp_snooping_enabled =
  call ~in_oss_since:None
    ~lifecycle:
      [
        ( Published
        , rel_inverness
        , "Enable or disable IGMP Snooping on the pool."
        )
      ]
    ~name:"set_igmp_snooping_enabled"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; (Bool, "value", "Enable or disable IGMP Snooping on the pool")
      ]
    ~doc:"Enable or disable IGMP Snooping on the pool."
    ~allowed_roles:_R_POOL_OP ()

let has_extension =
  call ~name:"has_extension" ~in_product_since:rel_dundee
    ~doc:"Return true if the extension is available on the pool"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; (String, "name", "The name of the API call")
      ]
    ~result:(Bool, "True if the extension exists, false otherwise")
    ~allowed_roles:_R_POOL_ADMIN ()

let add_to_guest_agent_config =
  call ~name:"add_to_guest_agent_config" ~in_product_since:rel_dundee
    ~doc:"Add a key-value pair to the pool-wide guest agent configuration"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; (String, "key", "The key to add")
      ; (String, "value", "The value to add")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let remove_from_guest_agent_config =
  call ~name:"remove_from_guest_agent_config" ~in_product_since:rel_dundee
    ~doc:"Remove a key-value pair from the pool-wide guest agent configuration"
    ~params:
      [(Ref _pool, "self", "The pool"); (String, "key", "The key to remove")]
    ~allowed_roles:_R_POOL_ADMIN ()

let rotate_secret =
  call ~in_product_since:rel_stockholm_psr ~name:"rotate_secret" ~params:[]
    ~errs:
      [
        Api_errors.internal_error
      ; Api_errors.host_is_slave
      ; Api_errors.cannot_contact_host
      ; Api_errors.ha_is_enabled
      ; Api_errors.not_supported_during_upgrade
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_repositories =
  call ~name:"set_repositories" ~in_product_since:"1.301.0"
    ~doc:"Set enabled set of repositories"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; (Set (Ref _repository), "value", "The set of repositories to be enabled")
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let add_repository =
  call ~name:"add_repository" ~in_product_since:"1.301.0"
    ~doc:"Add a repository to the enabled set"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; ( Ref _repository
        , "value"
        , "The repository to be added to the enabled set"
        )
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let remove_repository =
  call ~name:"remove_repository" ~in_product_since:"1.301.0"
    ~doc:"Remove a repository from the enabled set"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; (Ref _repository, "value", "The repository to be removed")
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let sync_updates =
  call ~name:"sync_updates" ~in_product_since:"1.329.0"
    ~doc:"Sync with the enabled repository"
    ~versioned_params:
      [
        {
          param_type= Ref _pool
        ; param_name= "self"
        ; param_doc= "The pool"
        ; param_release= next_release
        ; param_default= None
        }
      ; {
          param_type= Bool
        ; param_name= "force"
        ; param_doc=
            "If true local mirroring repo will be removed before syncing"
        ; param_release= next_release
        ; param_default= None
        }
      ; {
          param_type= String
        ; param_name= "token"
        ; param_doc= "The token for repository client authentication"
        ; param_release= next_release
        ; param_default= Some (VString "")
        }
      ; {
          param_type= String
        ; param_name= "token_id"
        ; param_doc= "The ID of the token"
        ; param_release= next_release
        ; param_default= Some (VString "")
        }
      ]
    ~result:(String, "The SHA256 hash of updateinfo.xml.gz")
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let check_update_readiness =
  call ~name:"check_update_readiness"
    ~lifecycle:[(Published, "1.304.0", "")]
    ~doc:"Check if the pool is ready to be updated. If not, report the reasons."
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; ( Bool
        , "requires_reboot"
        , "Assume that the update will require host reboots"
        )
      ]
    ~result:
      ( Set (Set String)
      , "A set of error codes with arguments, if the pool is\n\
        \        not ready to update. An empty list means the pool can be \
         updated."
      )
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let enable_client_certificate_auth =
  call ~name:"enable_client_certificate_auth"
    ~lifecycle:[(Published, "1.318.0", "")]
    ~doc:"Enable client certificate authentication on the pool"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; ( String
        , "name"
        , "The name (CN/SAN) that an incoming client certificate must have to \
           allow authentication"
        )
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let set_https_only =
  call ~name:"set_https_only"
    ~doc:
      "updates all the host firewalls in the pool to open or close port 80 \
       depending on the value"
    ~lifecycle:[]
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; ( Bool
        , "value"
        , "true - http port 80 will be blocked, false - http port 80 will be \
           open for the hosts in the pool"
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

let disable_client_certificate_auth =
  call ~name:"disable_client_certificate_auth"
    ~lifecycle:[(Published, "1.318.0", "")]
    ~doc:"Disable client certificate authentication on the pool"
    ~params:[(Ref _pool, "self", "The pool")]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let configure_repository_proxy =
  call ~name:"configure_repository_proxy" ~in_product_since:"21.3.0"
    ~doc:"Configure proxy for RPM package repositories."
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; (String, "url", "The URL of the proxy server")
      ; ( String
        , "username"
        , "The username used to authenticate with the proxy server"
        )
      ; ( String
        , "password"
        , "The password used to authenticate with the proxy server"
        )
      ]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let disable_repository_proxy =
  call ~name:"disable_repository_proxy" ~in_product_since:"21.4.0"
    ~doc:"Disable the proxy for RPM package repositories."
    ~params:[(Ref _pool, "self", "The pool")]
    ~allowed_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
    ()

let set_uefi_certificates =
  call ~name:"set_uefi_certificates"
    ~lifecycle:[(Published, "22.16.0", "")]
    ~doc:"Sets the UEFI certificates for a pool and all its hosts"
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; (String, "value", "The certificates to apply to the pool and its hosts")
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let set_telemetry_next_collection =
  call ~name:"set_telemetry_next_collection" ~lifecycle:[]
    ~doc:"Set the timestamp for the next telemetry data collection."
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; ( DateTime
        , "value"
        , "The earliest timestamp (in UTC) when the next round of telemetry \
           collection can be carried out."
        )
      ]
    ~allowed_roles:_R_POOL_ADMIN ()

let reset_telemetry_uuid =
  call ~name:"reset_telemetry_uuid" ~lifecycle:[]
    ~doc:"Assign a new UUID to telemetry data."
    ~params:[(Ref _pool, "self", "The pool")]
    ~allowed_roles:_R_POOL_ADMIN ()

let update_sync_frequency =
  Enum
    ( "update_sync_frequency"
    , [
        ("daily", "Indicates the pool update synchronization is scheduled daily")
      ; ( "weekly"
        , "Indicates the pool update synchronization is scheduled weekly"
        )
      ]
    )

let configure_update_sync =
  call ~name:"configure_update_sync"
    ~doc:"Config periodic update synchronizaiton from remote CDN" ~lifecycle:[]
    ~params:
      [
        (Ref _pool, "self", "The pool")
      ; ( update_sync_frequency
        , "update_sync_frequency"
        , "The frequency at which updates are synced from remote CDN: daily or \
           weekly."
        )
      ; ( Int
        , "update_sync_day"
        , "Which day of one period the update sychronization is scheduled. For \
           'daily' schedule, it should be 0. For 'weekly' schedule, 0..6, \
           where 0 is Sunday."
        )
      ]
    ~allowed_roles:_R_POOL_OP ()

(** A pool class *)
let t =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None
    ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_pool
    ~descr:"Pool-wide information" ~gen_events:true ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:
      [
        join
      ; join_force
      ; eject
      ; initial_auth
      ; exchange_certificates_on_join
      ; exchange_ca_certificates_on_join
      ; transition_to_master
      ; slave_reset_master
      ; recover_slaves
      ; hello
      ; ping_slave
      ; create_VLAN
      ; management_reconfigure
      ; create_VLAN_from_PIF
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
      ; install_ca_certificate
      ; uninstall_ca_certificate
      ; crl_install
      ; crl_uninstall
      ; crl_list
      ; certificate_sync
      ; enable_tls_verification
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
      ; rotate_secret
      ; set_repositories
      ; add_repository
      ; remove_repository
      ; sync_updates
      ; check_update_readiness
      ; enable_client_certificate_auth
      ; disable_client_certificate_auth
      ; configure_repository_proxy
      ; disable_repository_proxy
      ; set_uefi_certificates
      ; set_https_only
      ; set_telemetry_next_collection
      ; reset_telemetry_uuid
      ; configure_update_sync
      ]
    ~contents:
      ([uid ~in_oss_since:None _pool]
      @ [
          field ~in_oss_since:None ~qualifier:RW ~ty:String "name_label"
            "Short name"
        ; field ~in_oss_since:None ~qualifier:RW ~ty:String "name_description"
            "Description"
        ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Ref _host) "master"
            "The host that is pool master"
        ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "default_SR"
            "Default SR for VDIs"
        ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr)
            "suspend_image_SR"
            "The SR in which VDIs for suspend images are created"
        ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "crash_dump_SR"
            "The SR in which VDIs for crash dumps are created"
        ; field ~in_oss_since:None
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
            ~map_keys_roles:
              [
                ("folder", _R_VM_OP)
              ; ("XenCenter.CustomFields.*", _R_VM_OP)
              ; ("EMPTY_FOLDERS", _R_VM_OP)
              ]
        ; field ~in_oss_since:None ~in_product_since:rel_orlando
            ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false))
            "ha_enabled" "true if HA is enabled on the pool, false otherwise"
        ; field ~in_oss_since:None ~in_product_since:rel_orlando
            ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap [])) "ha_configuration"
            "The current HA configuration"
        ; field ~in_oss_since:None ~in_product_since:rel_orlando
            ~qualifier:DynamicRO ~ty:(Set String)
            ~default_value:(Some (VSet [])) "ha_statefiles"
            "HA statefile VDIs in use"
        ; field ~in_oss_since:None ~in_product_since:rel_orlando
            ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L))
            "ha_host_failures_to_tolerate"
            "Number of host failures to tolerate before the Pool is declared \
             to be overcommitted"
        ; field ~in_oss_since:None ~in_product_since:rel_orlando
            ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L))
            "ha_plan_exists_for"
            "Number of future host failures we have managed to find a plan \
             for. Once this reaches zero any future host failures will cause \
             the failure of protected VMs."
        ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:RW
            ~ty:Bool ~default_value:(Some (VBool false)) "ha_allow_overcommit"
            "If set to false then operations which would cause the Pool to \
             become overcommitted will be blocked."
        ; field ~in_oss_since:None ~in_product_since:rel_orlando
            ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false))
            "ha_overcommitted"
            "True if the Pool is considered to be overcommitted i.e. if there \
             exist insufficient physical resources to tolerate the configured \
             number of host failures"
        ; field ~qualifier:DynamicRO ~in_product_since:rel_orlando
            ~ty:(Map (String, Ref _blob))
            ~default_value:(Some (VMap [])) "blobs"
            "Binary blobs associated with this pool"
        ; field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando
            ~default_value:(Some (VSet [])) ~ty:(Set String) "tags"
            "user-specified tags for categorization purposes"
        ; field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "gui_config" "gui-specific configuration for pool"
        ; field ~writer_roles:_R_POOL_OP ~in_product_since:rel_dundee
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "health_check_config"
            "Configuration for the automatic health check feature"
        ; field ~in_product_since:rel_george ~qualifier:DynamicRO ~ty:String
            ~default_value:(Some (VString "")) "wlb_url"
            "Url for the configured workload balancing host"
        ; field ~in_product_since:rel_george ~qualifier:DynamicRO ~ty:String
            ~default_value:(Some (VString "")) "wlb_username"
            "Username for accessing the workload balancing host"
        ; field ~in_product_since:rel_george ~internal_only:true
            ~qualifier:DynamicRO ~ty:(Ref _secret) "wlb_password"
            "Password for accessing the workload balancing host"
        ; field
            ~writer_roles:(_R_POOL_OP ++ _R_CLIENT_CERT)
            ~in_product_since:rel_george ~qualifier:RW ~ty:Bool
            ~default_value:(Some (VBool false)) "wlb_enabled"
            "true if workload balancing is enabled on the pool, false otherwise"
        ; field ~in_product_since:rel_george ~qualifier:RW ~ty:Bool
            ~default_value:(Some (VBool false)) "wlb_verify_cert"
            "true if communication with the WLB server should enforce TLS \
             certificate verification."
            ~lifecycle:
              [
                (Published, rel_george, "")
              ; ( Deprecated
                , "1.290.0"
                , "Deprecated: to enable TLS verification use \
                   Pool.enable_tls_verification instead"
                )
              ]
        ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride
            ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false))
            "redo_log_enabled"
            "true a redo-log is to be used other than when HA is enabled, \
             false otherwise"
        ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride
            ~qualifier:DynamicRO ~ty:(Ref _vdi)
            ~default_value:(Some (VRef null_ref)) "redo_log_vdi"
            "indicates the VDI to use for the redo-log other than when HA is \
             enabled"
        ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:String
            ~default_value:(Some (VString "")) "vswitch_controller"
            "address of the vswitch controller"
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "the IP address of the vswitch controller."
                )
              ; ( Deprecated
                , rel_falcon
                , "Deprecated: set the IP address of the vswitch controller in \
                   SDN_controller instead."
                )
              ]
        ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride
            ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap [])) "restrictions"
            "Pool-wide restrictions currently in effect"
        ; field ~in_oss_since:None ~in_product_since:rel_boston
            ~qualifier:DynamicRO ~ty:(Set (Ref _vdi)) "metadata_VDIs"
            "The set of currently known metadata VDIs for this pool"
        ; field ~in_oss_since:None ~in_product_since:rel_dundee
            ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String
            "ha_cluster_stack"
            "The HA cluster stack that is currently in use. Only valid when HA \
             is enabled."
        ]
      @ allowed_and_current_operations operations
      @ [
          field ~in_oss_since:None ~in_product_since:rel_dundee
            ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap [])) "guest_agent_config"
            "Pool-wide guest agent configuration information"
        ; field ~qualifier:DynamicRO ~in_product_since:rel_dundee
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "cpu_info" "Details about the physical CPUs on the pool"
        ; field ~qualifier:RW ~in_product_since:rel_dundee
            ~default_value:(Some (VBool false)) ~ty:Bool
            "policy_no_vendor_device"
            "The pool-wide policy for clients on whether to use the vendor \
             device or not on newly created VMs. This field will also be \
             consulted if the 'has_vendor_device' field is not specified in \
             the VM.create call."
        ; field ~qualifier:RW ~in_product_since:rel_ely
            ~default_value:(Some (VBool false)) ~ty:Bool
            "live_patching_disabled"
            "The pool-wide flag to show if the live patching feauture is \
             disabled or not."
        ; field ~in_product_since:rel_inverness ~qualifier:DynamicRO ~ty:Bool
            ~default_value:(Some (VBool false)) "igmp_snooping_enabled"
            "true if IGMP snooping is enabled in the pool, false otherwise."
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:
              [
                ( Published
                , rel_quebec
                , "The UEFI certificates allowing Secure Boot"
                )
              ; ( Changed
                , "22.16.0"
                , "Became StaticRO to be editable through new method"
                )
              ]
            ~default_value:(Some (VString "")) "uefi_certificates"
            "The UEFI certificates allowing Secure Boot"
        ; field ~in_product_since:rel_stockholm_psr ~qualifier:RW ~ty:Bool
            ~default_value:(Some (VBool false)) "is_psr_pending"
            "True if either a PSR is running or we are waiting for a PSR to be \
             re-run"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, "1.290.0", "")]
            ~ty:Bool ~default_value:(Some (VBool false))
            "tls_verification_enabled"
            "True iff TLS certificate verification is enabled"
        ; field ~in_product_since:"1.301.0" ~qualifier:DynamicRO
            ~ty:(Set (Ref _repository)) ~ignore_foreign_key:true "repositories"
            ~default_value:(Some (VSet []))
            "The set of currently enabled repositories"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, "1.318.0", "")]
            ~ty:Bool ~default_value:(Some (VBool false))
            "client_certificate_auth_enabled"
            "True if authentication by TLS client certificates is enabled"
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, "1.318.0", "")]
            ~ty:String ~default_value:(Some (VString ""))
            "client_certificate_auth_name"
            "The name (CN/SAN) that an incoming client certificate must have \
             to allow authentication"
        ; field ~in_product_since:"21.3.0" ~qualifier:DynamicRO ~ty:String
            ~default_value:(Some (VString "")) "repository_proxy_url"
            "Url of the proxy used in syncing with the enabled repositories"
        ; field ~in_product_since:"21.3.0" ~qualifier:DynamicRO ~ty:String
            ~default_value:(Some (VString "")) "repository_proxy_username"
            "Username for the authentication of the proxy used in syncing with \
             the enabled repositories"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                (Published, "21.3.0", "")
              ; (Changed, rel_next, "Changed internal_only to false")
              ]
            ~ty:(Ref _secret) ~default_value:(Some (VRef null_ref))
            "repository_proxy_password"
            "Password for the authentication of the proxy used in syncing with \
             the enabled repositories"
        ; field ~qualifier:RW ~lifecycle:[] ~ty:Bool
            ~default_value:(Some (VBool false)) "migration_compression"
            "Default behaviour during migration, True if stream compression \
             should be used"
        ; field ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool true))
            "coordinator_bias"
            "true if bias against pool master when scheduling vms is enabled, \
             false otherwise"
        ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:(Ref _secret)
            ~default_value:(Some (VRef null_ref)) "telemetry_uuid"
            "The UUID of the pool for identification of telemetry data"
        ; field ~lifecycle:[] ~qualifier:DynamicRO ~ty:telemetry_frequency
            ~default_value:(Some (VEnum "weekly")) "telemetry_frequency"
            "How often the telemetry collection will be carried out"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:DateTime
            ~default_value:(Some (VDateTime Date.epoch))
            "telemetry_next_collection"
            "The earliest timestamp (in UTC) when the next round of telemetry \
             collection can be carried out"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:DateTime
            ~default_value:(Some (VDateTime Date.epoch)) "last_update_sync"
            "time of the last update sychronization"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:update_sync_frequency
            ~default_value:(Some (VEnum "weekly")) "update_sync_frequency"
            "The frequency at which updates are synced from remote CDN: daily \
             or weekly."
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:Int "update_sync_day"
            ~default_value:(Some (VInt 0L))
            "Which day of one period the update sychronization is scheduled"
        ; field ~qualifier:DynamicRO ~lifecycle:[] ~ty:Bool
            ~default_value:(Some (VBool false)) "update_sync_enabled"
            "If periodic update sychronization is enabled or not"
        ]
      )
    ()
