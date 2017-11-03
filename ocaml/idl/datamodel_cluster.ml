(* datamodel_cluster *)

open Datamodel_common
open Datamodel_roles
open Datamodel_types

(** Corosync-based clustering *)

let cluster_operation =
  Enum ("cluster_operation",
        [ "add",     "adding a new member to the cluster";
          "remove",  "removing a member from the cluster";
          "enable",  "enabling any cluster member";
          "disable", "disabling any cluster member";
          "destroy", "completely destroying a cluster";
        ])

let lifecycle = [Published, rel_jura, ""]

let create = call
    ~name:"create"
    ~doc:"Creates a Cluster object and one Cluster_host object as its first member"
    ~result:(Ref _cluster, "the new Cluster")
    ~params:
      [ Ref _network, "network",        "the single network on which corosync carries out its inter-host communications"
      ; String,       "cluster_stack",  "simply the string 'corosync'. No other cluster stacks are currently supported"
      ; Bool,         "pool_auto_join", "true if xapi is automatically joining new pool members to the cluster"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let destroy = call
    ~name:"destroy"
    ~doc:"Destroys a Cluster object and the one remaining Cluster_host member"
    ~params:
      [ Ref _cluster, "self", "the Cluster to destroy"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_create = call
    ~name:"pool_create"
    ~doc:"Attempt to create a Cluster from the entire pool"
    ~result:(Ref _cluster, "the new Cluster")
    ~params:
      [ Ref _pool,    "pool",          "The pool to create a Cluster from"
      ; String,       "cluster_stack", "simply the string 'corosync'. No other cluster stacks are currently supported"
      ; Ref _network, "network",       "the single network on which corosync carries out its inter-host communications"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_resync = call
  ~name:"pool_resync"
  ~doc:"Resynchronise the cluster_host objects across the pool. Creates them where they need creating and then plugs them"
  ~params:[ Ref _cluster, "self", "The cluster to resync"]
  ~lifecycle
  ~allowed_roles:_R_POOL_ADMIN
  ()

let t =
  create_obj
    ~name: _cluster
    ~descr:"Cluster-wide Cluster metadata"
    ~doccomments:[]
    ~gen_constructor_destructor:false
    ~gen_events:true
    ~in_db:true
    ~lifecycle
    ~persist:PersistEverything
    ~in_oss_since:None
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~contents:(
      [ uid     _cluster ~lifecycle

      ; field   ~qualifier:DynamicRO ~lifecycle
          ~ty:(Set (Ref _cluster_host)) "cluster_hosts"
          "A list of the cluster_host objects associated with the Cluster"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:(Ref _network) "network" ~default_value:(Some (VRef null_ref))
          "Reference to the single network on which corosync carries out its inter-host communications"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:String "cluster_token" ~default_value:(Some (VString ""))
          "The secret key used by xapi-clusterd when it talks to itself on other hosts"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:String "cluster_stack" ~default_value:(Some (VString "corosync"))
          "Simply the string 'corosync'. No other cluster stacks are currently supported"

      ] @ (allowed_and_current_operations cluster_operation) @ [

       field   ~qualifier:StaticRO ~lifecycle
          ~ty:Bool "pool_auto_join" ~default_value:(Some (VBool true))
          "True if xapi is automatically joining new pool members to the cluster. This will be `true` in the first release"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:(Map(String, String)) "cluster_config" ~default_value:(Some (VMap []))
          "Contains read-only settings for the cluster, such as timeouts and other options. It can only be set at cluster create time"

      ; field   ~qualifier:RW ~lifecycle
          ~ty:(Map(String, String)) "other_config" ~default_value:(Some (VMap []))
          "Additional configuration"
      ])
    ~messages:
      [ create
      ; destroy
      ; pool_create
      ; pool_resync
      ]
    ()
