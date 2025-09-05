  $ ./cluster_cli.exe --help=plain
  NAME
         cluster_cli - A CLI for the cluster API. This tool is not intended to
         be used as an end user tool
  
  SYNOPSIS
         cluster_cli [COMMAND] …
  
  COMMANDS
         Observer.create [OPTION]… dbg uuid name_label dict endpoints bool
  
         Observer.destroy [OPTION]… dbg uuid
  
         Observer.init [OPTION]… dbg
  
         Observer.set_attributes [OPTION]… dbg uuid dict
  
         Observer.set_compress_tracing_files [OPTION]… dbg bool
  
         Observer.set_enabled [OPTION]… dbg uuid bool
  
         Observer.set_endpoints [OPTION]… dbg uuid endpoints
  
         Observer.set_export_chunk_size [OPTION]… dbg int
  
         Observer.set_export_interval [OPTION]… dbg float
  
         Observer.set_host_id [OPTION]… dbg string
  
         Observer.set_max_depth [OPTION]… dbg int
  
         Observer.set_max_file_size [OPTION]… dbg int
  
         Observer.set_max_spans [OPTION]… dbg int
  
         Observer.set_max_traces [OPTION]… dbg int
  
         Observer.set_trace_log_dir [OPTION]… dbg string
  
         UPDATES.get [OPTION]… dbg timeout
             Get updates from corosync-notifyd, this blocking call will return
             when there is an update from corosync-notifyd or it is timed out
             after timeout_p seconds
  
         create [OPTION]… dbg init_config
             Creates the cluster. The call takes the initial config of the
             initial host to add to the cluster. This will be the address on
             which the rings will be created.
  
         declare-changed-addrs [OPTION]… dbg changed_members
             Declare that one or more hosts in the cluster have changed
             address. Only use this command if unable to rejoin the cluster
             using `enable` because the IPv4 addresses of all nodes this node
             previously saw are now invalid. If any one of these addresses
             remains valid on an enabled node then this action is unnecessary.
  
         declare-dead [OPTION]… dbg dead_members
             Declare that some hosts in the cluster are permanently dead.
             Removes the hosts from the cluster. If the hosts do attempt to
             rejoin the cluster in future, this may lead to fencing of other
             hosts and/or data loss or data corruption.
  
         destroy [OPTION]… dbg
             Destroys a created cluster
  
         diagnostics [OPTION]… dbg
             Returns diagnostic information about the cluster
  
         disable [OPTION]… dbg
             Stop the cluster on this host; leave the rest of the cluster
             enabled. The cluster can be reenabled either by restarting the
             host, or by calling the `enable` API call.
  
         enable [OPTION]… dbg init_config
             Rejoins the cluster following a call to `disable`. The parameter
             passed is the cluster config to use (optional fields set to None
             unless updated) in case it changed while the host was disabled.
             (Note that changing optional fields isn't yet supported, TODO)
  
         join [OPTION]… dbg token new_member tls_config existing_members
             Adds a node to an initialised cluster. Takes the IPv4 address of
             the new member and a list of the addresses of all the existing
             members.
  
         leave [OPTION]… dbg
             Causes this host to permanently leave the cluster, but leaves the
             rest of the cluster enabled. This is not a temporary removal - if
             the admin wants the hosts to rejoin the cluster again, he will
             have to call `join` rather than `enable`.
  
         set-tls-verification [OPTION]… dbg server_pem_path
         trusted_bundle_path cn enabled
             Enable or disable TLS verification for xapi/clusterd
             communication. The trusted_bundle_path is ignored when
             verification is disabled and can be empty
  
         switch-cluster-stack [OPTION]… dbg cluster_stack
             Switch cluster stack version to the target
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         cluster_cli exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
