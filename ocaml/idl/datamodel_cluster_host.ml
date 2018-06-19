(* datamodel_cluster_host *)

open Datamodel_common
open Datamodel_roles
open Datamodel_types

let cluster_host_operation =
  Enum ("cluster_host_operation",
        [ "enable",  "enabling cluster membership on a particular host";
          "disable", "disabling cluster membership on a particular host";
          "destroy", "completely destroying a cluster host";
        ])

let lifecycle = [Prototyped, rel_kolkata, ""]

let create = call
    ~name:"create"
    ~doc:"Add a new host to an existing cluster."
    ~result:(Ref _cluster_host, "the newly created cluster_host object")
    ~params:
      [ Ref _cluster, "cluster", "Cluster to join"
      ; Ref _host,    "host",    "new cluster member"
      ; Ref _pif,     "pif",     "Network interface to use for communication"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ~errs:Api_errors.([ pif_not_attached_to_host
                      ; no_cluster_hosts_reachable
                      ])
    ()

let destroy = call
    ~name:"destroy"
    ~doc:"Remove a host from an existing cluster."
    ~params:
      [ Ref _cluster_host, "self", "the cluster_host to remove from the cluster"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ~errs:Api_errors.([ cluster_stack_in_use
                      ; clustering_disabled
                      ; cluster_host_is_last
                      ])
    ()

let force_destroy = call
    ~name:"force_destroy"
    ~doc:"Remove a host from an existing cluster forcefully."
    ~params:
      [ Ref _cluster_host, "self", "the cluster_host to remove from the cluster"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ~errs:Api_errors.([ cluster_stack_in_use
                      ])
    ()

let enable = call
    ~name:"enable"
    ~doc:"Enable cluster membership for a disabled cluster host."
    ~params:
      [ Ref _cluster_host, "self", "the cluster_host to enable"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ~errs:Api_errors.([ pif_allows_unplug
                      ; required_pif_is_unplugged
                      ])
    ()

let disable = call
    ~name:"disable"
    ~doc:"Disable cluster membership for an enabled cluster host."
    ~params:
      [ Ref _cluster_host, "self", "the cluster_host to disable"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ~errs:Api_errors.([ cluster_stack_in_use
                      ])
    ()

let forget = call
  ~name:"forget"
  ~doc:"Permanently remove a dead host from the cluster. This host must never rejoin the cluster."
  ~params:
      [ Ref _cluster_host, "self", "the cluster_host to declare permanently dead and forget"
      ]
  ~lifecycle:[Prototyped, rel_lima, ""]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ~hide_from_docs:true
  ()

let t =
  create_obj
    ~name: _cluster_host
    ~descr:"Cluster member metadata"
    ~doccomments:[]
    ~gen_constructor_destructor:false
    ~gen_events:true
    ~in_db:true
    ~lifecycle
    ~persist:PersistEverything
    ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:(
      [ uid     _cluster_host ~lifecycle

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:(Ref _cluster) "cluster" ~default_value:(Some (VRef null_ref))
          "Reference to the Cluster object"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:(Ref _host) "host" ~default_value:(Some (VRef null_ref))
          "Reference to the Host object"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:Bool "enabled" ~default_value:(Some (VBool false))
          "Whether the cluster host believes that clustering should be enabled on this host"

      ; field  ~qualifier:StaticRO ~lifecycle
          ~ty:(Ref _pif) "PIF" ~default_value:(Some (VRef null_ref))
          "Reference to the PIF object"

      ; field  ~qualifier:StaticRO ~lifecycle
          ~ty:Bool "joined" ~default_value:(Some (VBool true))
          "Whether the cluster host has joined the cluster"

      (* TODO: add `live` member to represent whether corosync believes that this
               cluster host actually is enabled *)

      ] @ (allowed_and_current_operations cluster_host_operation) @ [

        field   ~qualifier:StaticRO ~lifecycle
          ~ty:(Map(String, String)) "other_config" ~default_value:(Some (VMap []))
          "Additional configuration"
      ])
    ~messages:
      [ create
      ; destroy
      ; enable
      ; force_destroy
      ; forget
      ; disable
      ]
    ()
