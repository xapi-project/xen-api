(*
 * Copyright (C) Citrix Systems Inc.
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

(** Functions for implementing the Cluster_host objects.
    @group Clustering *)

(******************************************************************************)
(** {2 Internal helper functions} *)

val fix_pif_prerequisites : __context:Context.t -> API.ref_PIF -> unit

(* [fix_pif_prerequisites ~__context (pif_ref,pif_rec)] will fix those
   prerequisites that are fixable automatically. It won't be able to fix a
   missing IP address, but it will plug the PIF if it's not attached and it
   will set disallow_unplug once the PIF is plugged *)

val sync_required :
  __context:Context.t -> host:API.ref_host -> API.ref_Cluster option
(** [sync_required ~__context ~host] returns an option type indicating whether
    any action is required to sync the cluster. This will only be the case if
    the cluster object has [pool_auto_join] set and no corresponding
    Cluster_host object exists for the host. If one does not exist, the call
    will return the reference to the Cluster object wrapped in 'Some' *)

val create_as_necessary : __context:Context.t -> host:API.ref_host -> unit
(** [create_as_necessary ~__context ~host] calls [sync_required], and if any
    Cluster_host objects are required it will create them in the database *)

(******************************************************************************)
(** {2 External API calls} *)

val create :
     __context:Context.t
  -> cluster:API.ref_Cluster
  -> host:API.ref_host
  -> pif:API.ref_PIF
  -> API.ref_Cluster_host
(** [create ~__context ~cluster ~host] is implementation of the XenAPI call
    'Cluster_host.create'. It is the Cluster_host object constructor, and creates
    a cluster_host in the DB before calling [resync_host ~__context ~host], which
    either joins the host to the cluster or enables the cluster host *)

val force_destroy : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [force_destroy ~__context ~self] is the implementation of the XenAPI call
    'Cluster_host.force_destroy'. It forcefully removes the DB object and
     destroys the cluster on specified host. *)

val destroy : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [destroy ~__context ~self] is the implementation of the XenAPI call
    'Cluster_host.destroy'. It is the Cluster_host destructor
    Note that this is the only Cluster_host call that is still valid if the
    clustering daemon is disabled, all others require it enabled *)

val enable : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [enable ~__context ~self] is the implementation of the XenAPI call
    'Cluster_host.enable'. It attempts to enable clustering on the host referred
    to by the Cluster_host object. This will cause xapi to ask xapi_clusterd to
    join the cluster. It requires all other cluster members to be online. *)

val disable : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [disable ~__context ~self] is the implementation of the XenAPI call
    'Cluster_host.disable'. It will call xapi-clusterd and ask it to leave the
    cluster. This requires all cluster members to be online. *)

val disable_clustering : __context:Context.t -> unit
(** [disable_clustering ~__context] is a wrapper for Xapi_cluster_host.disable
    which finds the local cluster_host [self], calls [disable ~__context self]
    and logs its actions. *)

val resync_host : __context:Context.t -> host:API.ref_host -> unit
(** [resync_host ~__context ~host] checks for the existence of a cluster_host.
    If one exists but hasn't joined the cluster, xapi asks xapi-clusterd to add
    the host to the cluster, otherwise it enables the cluster host.
    If no cluster_host is found, nothing happens.
    If a failure occurs, Xapi sends an alert to XenCenter *)

val forget : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [forget ~__context ~self] marks the cluster host as permanently removed
    from the cluster. This will only succeed if the rest of the hosts are online,
    so in the case of failure the cluster's pending_forget list will be updated.
    If you declare all your dead hosts as dead one by one the last one should succeed *)
