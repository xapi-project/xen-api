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

(** Functions for implementing the Cluster objects.
    @group Clustering *)

(******************************************************************************)
(** {2 External API calls} *)
val create : __context:Context.t -> network:API.ref_network ->
  cluster_stack:string -> pool_auto_join:bool -> 
  token_timeout:float -> token_timeout_coefficient:float -> 
  API.ref_Cluster
(** [create ~__context ~network ~cluster_stack ~pool_auto_join ~token_timeout 
 *   ~token_timeout_coefficient] is the implementation of the XenAPI method 
 *   'Cluster.create'. It is the constructor of the Cluster object. *)

val destroy : __context:Context.t -> self:API.ref_Cluster -> unit
(** [destroy ~__context ~self] is the implementation of the XenAPI method
    'Cluster.destroy'. It is the destructor of the Cluster object *)

val pool_create : __context:Context.t -> network:API.ref_network ->
  cluster_stack:string -> token_timeout:float -> 
  token_timeout_coefficient:float -> API.ref_Cluster
(** [pool_create ~__context ~network ~cluster_stack ~token_timeout 
    ~token_timeout_coefficient] is the implementation of the XenAPI 
    method 'Cluster.pool_create'. This is a convenience function
    that creates the Cluster object and then creates Cluster_host objects for
    all hosts in the pool. *)

val pool_force_destroy : __context:Context.t -> self:API.ref_Cluster -> unit
(** [pool_force_destroy ~__context ~pool ~self] is the implementation of the XenAPI
    method 'Cluster.pool_force_create'. This is a convenience function that force 
    destroys the Cluster_host objects for all hosts in the pool and then destroys the
    Cluster object. *)

val pool_destroy : __context:Context.t -> self:API.ref_Cluster -> unit
(** [pool_destroy ~__context ~pool ~self] is the implementation of the XenAPI
    method 'Cluster.pool_create'. This is a convenience function that destroys
    the Cluster_host objects for all hosts in the pool and then destroys the
    Cluster object. *)

val pool_resync : __context:Context.t -> self:API.ref_Cluster -> unit
(** [pool_resync ~__context ~self] is the implementation of the XenAPI method
    'Cluster.pool_resync'. The purpose of this function is to help after the
    failure to create Cluster_host objects. It should create all necessary
    Cluster_host objects (ie., one for each host in the pool if the Cluster
    has [pool_auto_join] set. If there is a failure, this function must return
    an error that enables the administrator to fix the problem. *)
