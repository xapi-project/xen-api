open Datamodel_common

let gc_compact =
  call ~name:"gc_compact"
    ~lifecycle:
      [
        ( Published
        , Datamodel_types.rel_stockholm
        , "Perform a full major collection and compact the heap on a host"
        )
      ]
    ~doc:"Perform a full major collection and compact the heap on a host"
    ~hide_from_docs:true
    ~params:[(Ref _host, "host", "The host to perform GC")]
    ~errs:[] ~allowed_roles:Datamodel_roles._R_POOL_OP ()

let gc_stats =
  call ~name:"gc_stats"
    ~lifecycle:
      [(Published, Datamodel_types.rel_stockholm, "Get GC stats of a host")]
    ~doc:"Get GC stats of a host" ~hide_from_docs:true
    ~params:[(Ref _host, "host", "The host from which to obtain GC stats")]
    ~errs:[] ~allowed_roles:Datamodel_roles._R_POOL_OP
    ~result:(Map (String, String), "Collection of GC stats")
    ()

let db_stats =
  call ~name:"db_stats"
    ~lifecycle:
      [
        ( Published
        , Datamodel_types.rel_stockholm
        , "Get the database stats of the pool"
        )
      ]
    ~doc:"Get the database stats of the pool" ~hide_from_docs:true ~params:[]
    ~errs:[] ~allowed_roles:Datamodel_roles._R_POOL_OP
    ~result:(Map (String, String), "Collection of database stats")
    ()

let network_stats =
  call ~name:"network_stats"
    ~lifecycle:
      [
        (Published, Datamodel_types.rel_stockholm, "Get network stats of a host")
      ]
    ~doc:"Get network stats of a host" ~hide_from_docs:true
    ~params:
      [
        (Ref _host, "host", "The host from which to obtain network stats")
      ; ( Map (String, String)
        , "params"
        , "The params to filter and format the output"
        )
      ]
    ~errs:[] ~allowed_roles:Datamodel_roles._R_POOL_OP
    ~result:(Set (Set String), "Collection of network stats")
    ()

let t =
  create_obj ~in_db:false
    ~lifecycle:
      [
        ( Published
        , Datamodel_types.rel_stockholm
        , "A set of functions for diagnostic purpose"
        )
      ]
    ~in_oss_since:None ~persist:PersistNothing ~gen_constructor_destructor:false
    ~name:_diagnostics ~descr:"A set of functions for diagnostic purpose"
    ~gen_events:false ~doccomments:[]
    ~messages_default_allowed_roles:Datamodel_roles._R_POOL_OP
    ~messages:[gc_compact; gc_stats; db_stats; network_stats]
    ~contents:[] ()
