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

let lifecycle = [Prototyped, rel_kolkata, ""]

let timeout_params =
  [ {param_type=Float;
     param_name="token_timeout";
     param_doc="Corosync token timeout in seconds";
     param_release=kolkata_release;
     param_default=Some(VFloat Constants.default_token_timeout_s)};

    {param_type=Float;
     param_name="token_timeout_coefficient";
     param_doc="Corosync token timeout coefficient in seconds";
     param_release=kolkata_release;
     param_default=Some(VFloat Constants.default_token_timeout_coefficient_s)};

  ]


let create = call
    ~name:"create"
    ~doc:"Creates a Cluster object and one Cluster_host object as its first member"
    ~result:(Ref _cluster, "the new Cluster")
    ~versioned_params:
      ([{param_type=(Ref _pif);
         param_name="PIF";
         param_doc="The PIF to connect the cluster's first cluster_host to";
         param_release=kolkata_release;
         param_default=None};

        {param_type=String;
         param_name="cluster_stack";
         param_doc="simply the string 'corosync'. No other cluster stacks are currently supported";
         param_release=kolkata_release;
         param_default=None};

        {param_type=Bool;
         param_name="pool_auto_join";
         param_doc="true if xapi is automatically joining new pool members to the cluster";
         param_release=kolkata_release;
         param_default=None};

       ] @timeout_params)
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

let get_network = call
    ~name:"get_network"
    ~doc:("Returns the network used by the cluster for inter-host communication, " ^
          "i.e. the network shared by all cluster host PIFs")
    ~result:(Ref _network, "network of cluster")
    ~params:
      [ Ref _cluster, "self", "the Cluster with the network"
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_create = call
    ~name:"pool_create"
    ~doc:"Attempt to create a Cluster from the entire pool"
    ~result:(Ref _cluster, "the new Cluster")
    ~versioned_params:
      ([{param_type=Ref _network;
         param_name="network";
         param_doc="the single network on which corosync carries out its inter-host communications";
         param_release=kolkata_release;
         param_default=None};

        {param_type=String;
         param_name="cluster_stack";
         param_doc="simply the string 'corosync'. No other cluster stacks are currently supported";
         param_release=kolkata_release;
         param_default=None};

       ] @ timeout_params)
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_force_destroy = call
    ~name:"pool_force_destroy"
    ~doc:"Attempt to force destroy the Cluster_host objects, and then destroy the Cluster."
    ~params:
      [ Ref _cluster, "self", "The cluster to force destroy."
      ]
    ~lifecycle
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_destroy = call
    ~name:"pool_destroy"
    ~doc:"Attempt to destroy the Cluster_host objects for all hosts in the pool and then destroy the Cluster."
    ~params:
      [ Ref _cluster, "self", "The cluster to destroy."
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
          ~ty:String "cluster_token" ~default_value:(Some (VString ""))
          "The secret key used by xapi-clusterd when it talks to itself on other hosts"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:String "cluster_stack" ~default_value:(Some (VString Constants.default_smapiv3_cluster_stack))
          "Simply the string 'corosync'. No other cluster stacks are currently supported"

      ] @ (allowed_and_current_operations cluster_operation) @ [

        field   ~qualifier:StaticRO ~lifecycle
          ~ty:Bool "pool_auto_join" ~default_value:(Some (VBool true))
          "True if automatically joining new pool members to the cluster. This will be `true` in the first release"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:Int "token_timeout" ~default_value:(Some (VInt 20000L))
          "The corosync token timeout in ms"

      ; field   ~qualifier:StaticRO ~lifecycle
          ~ty:Int "token_timeout_coefficient" ~default_value:(Some (VInt 1000L))
          "The corosync token timeout coefficient in ms"


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
      ; get_network
      ; pool_create
      ; pool_force_destroy
      ; pool_destroy
      ; pool_resync
      ]
    ()
