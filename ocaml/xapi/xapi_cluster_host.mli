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

val fix_pif_prerequisites : __context:Context.t -> (API.ref_PIF * API.pIF_t) ->
  unit
(* [fix_pif_prerequisites ~__context (pif_ref,pif_rec)] will fix those
   prerequisites that are fixable automatically. It won't be able to fix a
   missing IP address, but it will plug the PIF if it's not attached and it
   will set disallow_unplug once the PIF is plugged *)

val sync_required : __context:Context.t -> host:API.ref_host ->
  API.ref_Cluster option
(** [sync_required ~__context ~host] returns an option type indicating whether
    any action is required to sync the cluster. This will only be the case if
    the cluster object has [pool_auto_join] set and no corresponding
    Cluster_host object exists for the host. If one does not exist, the call
    will return the reference to the Cluster object wrapped in 'Some' *)

val create_as_necessary : __context:Context.t -> host:API.ref_host -> unit
(** [create_as_necessary ~__context ~host] calls [sync_required], and if any
    Cluster_host objects are required it will create them *)

(******************************************************************************)
(** {2 External API calls} *)

val create : __context:Context.t -> cluster:API.ref_Cluster -> host:API.ref_host
  -> API.ref_Cluster_host
(** [create ~__context ~cluster ~host] is implementation of the XenAPI call
    'Cluster_host.create'. It is the Cluster_host object constructor *)

val force_destroy : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [force_destroy ~__context ~self] is the implementation of the XenAPI call
    'Cluster_host.force_destroy'. It forcefully removes the DB object and
     destroys the cluster on specified host. *)

val destroy : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [destroy ~__context ~self] is the implementation of the XenAPI call
    'Cluster_host.destroy'. It is the Cluster_host destructor *)

val enable : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [enable ~__context ~self] is the implementation of the XenAPI call
    'Cluster_host.enable'. It attempts to enable clustering on the host referred
    to by the Cluster_host object. This will cause xapi to ask xapi_clusterd to
    join the cluster. It requires all other cluster members to be online. *)

val disable : __context:Context.t -> self:API.ref_Cluster_host -> unit
(** [disable ~__context ~self] is the implementation of the XenAPI call
    'Cluster_host.disable'. It will call xapi-clusterd and ask it to leave the
    cluster. This requires all cluster members to be online. *)
