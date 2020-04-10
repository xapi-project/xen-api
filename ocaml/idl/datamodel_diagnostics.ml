open Datamodel_common

let gc_compact = call
    ~name:"gc_compact"
    ~in_product_since:Datamodel_types.rel_stockholm
    ~doc:""
    ~params:[Ref _session, "session", "The session to do gc_compact";
             Ref _host, "host", "The host to do gc_compact."]
    ~errs:[]
    ~allowed_roles:Datamodel_roles._R_POOL_OP
    ()

let gc_stats = call
    ~name:"gc_stats"
    ~in_product_since:Datamodel_types.rel_stockholm
    ~doc:""
    ~params:[Ref _session, "session", "The session to do gc_stats";
             Ref _host, "host", "The host to do gc_stats."]
    ~errs:[]
    ~allowed_roles:Datamodel_roles._R_POOL_OP
    ~result:(Map(String, String), "")
    ()

let db_stats = call
    ~name:"db_stats"
    ~in_product_since:Datamodel_types.rel_stockholm
    ~doc:""
    ~params:[Ref _session, "session", "The session to do db_stats";
             Ref _host, "host", "The host to do db_stats."]
    ~errs:[]
    ~allowed_roles:Datamodel_roles._R_POOL_OP
    ~result:(Map(String, String), "")
    ()

let network_stats = call
    ~name:"network_stats"
    ~in_product_since:Datamodel_types.rel_stockholm
    ~doc:""
    ~params:[Ref _session, "session", "The session to do netowrk_stats";
             Ref _host, "host", "The host to do network_stats."]
    ~errs:[]
    ~allowed_roles:Datamodel_roles._R_POOL_OP
    ~result:(Map(String, String), "")
    ()

let license_stats = call
    ~name:"license_stats"
    ~in_product_since:Datamodel_types.rel_stockholm
    ~doc:""
    ~params:[Ref _session, "session", "The session to do license_stats";
             Ref _host, "host", "The host to do license_stats."]
    ~errs:[]
    ~allowed_roles:Datamodel_roles._R_POOL_OP
    ~result:(Map(String, String), "")
    ()

let t =
  create_obj ~in_db:false ~in_product_since:Datamodel_types.rel_stockholm ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_diagnostics ~descr:"A set of functions for diagnostic purpose" ~gen_events:false
    ~doccomments:[]
    ~messages_default_allowed_roles:Datamodel_roles._R_POOL_OP
    ~messages:[
      gc_compact;
      gc_stats;
      db_stats;
      network_stats;
      license_stats
    ]
    ~contents:[ uid _diagnostics]()
