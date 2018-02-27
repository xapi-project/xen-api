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

let lifecycle = [Published, rel_jura, ""]

let create = call
    ~name:"create"
    ~doc:"Add a new host to an existing cluster."
    ~result:(Ref _cluster_host, "the newly created cluster_host object")
    ~params:
      [ Ref _cluster, "cluster", "Cluster to join"
      ; Ref _host,    "host",    "new cluster member"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let destroy = call
    ~name:"destroy"
    ~doc:"Remove a host from an existing cluster."
    ~params:
      [ Ref _cluster_host, "self", "the cluster_host to remove from the cluster"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let enable = call
    ~name:"enable"
    ~doc:"Enable a host for an existing cluster."
    ~params:
      [ Ref _cluster_host, "self", "the cluster_host to enable"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let disable = call
    ~name:"disable"
    ~doc:"Disable a host in existing cluster."
    ~params:
      [ Ref _cluster_host, "self", "the cluster_host to disable"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
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
    ~contents:
      [ uid     _cluster_host ~lifecycle

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:(Ref _cluster) "cluster" ~default_value:(Some (VRef null_ref))
          "Reference to the Cluster object"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:(Ref _host) "host" ~default_value:(Some (VRef null_ref))
          "Reference to the Host object"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:Bool "enabled" ~default_value:(Some (VBool false))
          "Whether clustering should be enabled on this host"

      ; field   ~qualifier:DynamicRO ~lifecycle
          ~ty:(Map(Ref _task, cluster_host_operation)) "current_operations" ~default_value:(Some (VMap []))
          "Links each of the running tasks using this object (by reference) to a current_operation enum which describes the nature of the task"

      ; field   ~qualifier:DynamicRO ~lifecycle
          ~ty:(Set cluster_host_operation) "allowed_operations" ~default_value:(Some (VSet []))
          "List of the operations allowed in this state. This list is advisory only and the server state may have changed by the time this field is read by a client"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:(Map(String, String)) "other_config" ~default_value:(Some (VMap []))
          "Additional configuration"
      ]
    ~messages:
      [ create
      ; destroy
      ; enable
      ; disable
      ]
    ()
