  $ ./rrd_cli.exe --help=plain
  NAME
         rrd-cli - A CLI for the Db monitoring API. This allows scripting of
         the Rrd daemon for testing and debugging. This tool is not intended to
         be used as an end user tool
  
  SYNOPSIS
         rrd-cli [COMMAND] …
  
  COMMANDS
         Deprecated.load_rrd [OPTION]… uuid timescale
             Deprecated call.
  
         HA.disable [OPTION]…
             Disables the HA metrics.
  
         HA.enable_and_update [OPTION]… statefile_latencies heartbeat_latency
         xapi_latency
             Enables the gathering of HA metrics, a built-in function of
             xcp-rrdd.
  
         Plugin.Local.deregister [OPTION]… uid
             Deregisters a plugin by uid
  
         Plugin.Local.next_reading [OPTION]… uid
             Returns the number of seconds until the next reading will be
             taken.
  
         Plugin.Local.register [OPTION]… uid info protocol
             [Plugin.Local.register uid info protocol] registers a plugin as a
             source of a set of data-sources. [uid] is a unique identifier for
             the plugin, often the name of the plugin. [info] is the RRD
             frequency, and [protocol] specifies whether the plugin will be
             using V1 or V2 of the protocol.
  
         Plugin.deregister [OPTION]… uid
             Preserved for backwards compatibility. Deregesters a local plugin.
  
         Plugin.get_header [OPTION]…
             Returns header string. This string should be copied exactly to the
             start of the shared memory containing the data source
  
         Plugin.get_path [OPTION]… uid
             Returns path in the local filesystem to place the data source file
  
         Plugin.next_reading [OPTION]… uid
             Returns the time until the next reading.
  
         Plugin.register [OPTION]… uid frequency
             Preserved for backwards compatibility. Equivalent to a Local
             plugin registration with V1 protocol.
  
         add_host_ds [OPTION]… ds_name
             Adds a host data source to the host RRD. This causes the data
             source to be recorded if it wasn't a default data source.
  
         add_sr_ds [OPTION]… sr_uuid ds_name
             Adds an SR data source to the SR RRD. This causes the data source
             to be recorded if it wasn't a default data source.
  
         add_vm_ds [OPTION]… vm_uuid domid ds_name
             Adds a VM data source to the VM RRD. This causes the data source
             to be recorded if it wasn't a default data source.
  
         archive_rrd [OPTION]… vm_uuid
             Sends the VM RRD either to local disk or the remote address if
             specified, and removes it from memory. Called on VM
             shutdown/suspend.
  
         archive_sr_rrd [OPTION]… sr_uuid
             Saves the SR RRD to the local disk. Returns the path to the saved
             RRD so it can be copied onto the SR before it is detached.
  
         backup_rrds [OPTION]…
             Backs up RRD data to disk. This should be done periodically to
             ensure that if the host crashes we don't lose too much data.
  
         forget_host_ds [OPTION]… ds_name
             Forgets the recorded archives for the named data source. Note that
             if the data source is marked as default, new data coming in will
             cause the archive to be recreated.
  
         forget_sr_ds [OPTION]… sr_uuid ds_name
             Forgets the recorded archives for the named SR data source. Note
             that if the data source is marked as default, new data coming in
             will cause the archive to be recreated.
  
         forget_vm_ds [OPTION]… vm_uuid ds_name
             Forgets the recorded archives for the named VM data source. Note
             that if the data source is marked as default, new data coming in
             will cause the archive to be recreated.
  
         has_vm_rrd [OPTION]… vm_uuid
             Returns `true` if xcp-rrdd has an RRD for the specified VM in
             memory
  
         migrate_rrd [OPTION]… remote_address vm_uuid host_uuid
             Migrate_push - used by the migrate code to push an RRD directly to
             a remote host without going via the master. If the host is on a
             different pool, you must pass both the remote_address and
             session_id parameters.
  
         push_rrd_local [OPTION]… vm_uuid domid
             Loads a VM RRD from local storage, associates it with the
             specified domid, and starts recording all data sources related to
             the VM to that RRD
  
         push_rrd_remote [OPTION]… vm_uuid remote_address
             Loads a VM RRD from local storage and pushes it to a remote host
  
         push_sr_rrd [OPTION]… sr_uuid path
             Loads the RRD from the path specified on the local disk.
             Overwrites any RRD already in memory for the SR. Data sources will
             subsequently be recorded to this RRD.
  
         query_host_ds [OPTION]… ds_name
             Returns the current value of the named host data source. Note this
             returns the raw data source value, not the smoothed last value of
             the RRA.
  
         query_possible_host_dss [OPTION]…
             Returns list of possible host DSs. This will include data sources
             not currently being recorded into archives.
  
         query_possible_sr_dss [OPTION]… sr_uuid
             Returns list of possible SR DSs. This will include data sources
             not currently being recorded into archives.
  
         query_possible_vm_dss [OPTION]… vm_uuid
             Returns list of possible VM DSs. This will include data sources
             not currently being recorded into archives.
  
         query_sr_ds [OPTION]… sr_uuid ds_name
             Returns the current value of the named VM data source. Note this
             returns the raw data source value, not the smoothed last value of
             the RRA.
  
         query_vm_ds [OPTION]… vm_uuid ds_name
             Returns the current value of the named VM data source. Note this
             returns the raw data source value, not the smoothed last value of
             the RRA.
  
         remove_rrd [OPTION]… uuid
             Removes a VM RRD from the local filesystem, if it exists.
  
         save_rrds [OPTION]…
             Backs up RRD data to disk on localhost. This should be done
             periodically to ensure that if the host crashes we don't lose too
             much data.
  
         send_host_rrd_to_master [OPTION]… master_address
             Called on host shutdown/reboot to send the Host RRD to the master
             for backup.
  
         set_cache_sr [OPTION]… sr_uuid
             Sets the uuid of the cache SR. If this is set, statistics about
             the usage of the cache will be recorded into the host SR.
  
         unset_cache_sr [OPTION]…
             Unsets the cache_sr. No futher data will be gathered about cache
             usage, but existing archive data will not be deleted.
  
         update_use_min_max [OPTION]… value
             Set the value of the `use_min_max` variable. If this is `true`,
             when creating a new RRD, archives for the minimum and maximum
             observed values will be created alongside the standard archive of
             average values
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         rrd-cli exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
