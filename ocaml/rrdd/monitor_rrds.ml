(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(** RRD maintainence code
 * @group Performance Monitoring
 *)

(**
 * This module is primarily concerned with the lifecycle of RRDs. They
 * are created here, stored to disk, retrieved from disk, and sent
 * amongst the pool.
 *
 * The entry point in terms of the monitor thread is function
 * 'update_rrds' which is called directly from monitor.ml. This updates
 * the VMs and the hosts RRDs. It also removes any RRDs for VMs that
 * have disappeared (unless they're marked as rebooting) and copies
 * these back to to the master. It's important that as far as possible
 * we don't access the database in this thread - not only because it's
 * potentially expensive as we run every 5 seconds, but also because we
 * want to carry on recording data if the master goes offline.  For the
 * purposes of doing database write (for example, for host memory), we
 * have another thread run in the module Monitor_dbcalls.
 *
 * These two threads need access to the same data, so the shared stuff
 * is all in rrd_shared.ml, and all protected by 1 big mutex for
 * simplicity.
 *
 * If the PIFs have changed, or the memory of any VM, or the memory of
 * the host has changed, they are marked as dirty by the update_rrds
 * function via the keys in rrd_shared. If any one of these is true, we
 * signal the monitor_dbcalls thread to wake up via the condition
 * variable in rrd_shared.ml
 *
 *
 * RRD lifecycle:
 *
 * Host RRDs are pulled from the master on boot by dbsync_slave
 * (calling 'pull_rrd' in this module), and pushed out to the master by
 * xapi_fuse which calls 'backup' in this module.
 *
 * VM RRDs are created afresh whenever a VM is resident that doesn't
 * already have an RRD in the cache (defined in rrd_shared). When a VM
 * is started/resumed/migrated then the RRD is pushed along with it,
 * and replaces whatever is currently in the cache. When the VM
 * disappears, the RRD is streamed back to the master for writing to
 * disk.
 *
 *
 * Legacy metrics:
 *
 * The legacy metrics are updated using the mechanism from Miami/Rio.
 * A large XML blob is constructed and streamed across to the master
 * (or updated locally). This is triggered by a hook on one of the RRAs
 * which happens when the archive gets updated with a new CDP. The hook
 * is on the host rrd, and is added either when the host rrd is received
 * by the http handler, or when the rrd is first created. The trigger
 * essentially marks another boolean in the rrd_shared module, which
 * causes the monitor_dbcalls thread to wake up and do the marshalling.
 *
 *
 * Http handlers:
 *
 * There are 3 handlers defined: /host_rrd /vm_rrd and /rrd_updates. The
 * first 2 simply retrieve the rrds (a uuid must be supplied to the vm_rrd
 * handler) and the last returns the CDPs in a specified archive between
 * a specified time and now. The first vm_rrd handler accepts both PUT
 * and GET requests (the functions receive_handler and handler below) and
 * is the PUT is used to upload RRDs from the master to the slave when
 * VMs are started/stopped
 *)

open Threadext
open Hashtblext
open Monitor_types
open Stringext
open Listext
open Ds
(*open Rrd_shared*) (* Nb this contains the mutex *)

(** Cleanup functions *)

(* Load an RRD from the local filesystem. Will return an RRD or throw an exception. *)
let load_rrd_from_local_filesystem ~__context uuid =
  debug "Loading RRD from local filesystem for object uuid=%s" uuid;
  let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
  rrd_of_gzip path
