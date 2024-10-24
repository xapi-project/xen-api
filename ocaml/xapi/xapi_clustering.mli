(* Copyright (C) Cloud Software Group Inc.
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

val set_ha_cluster_stack : __context:Context.t -> unit

val with_clustering_lock : string -> (unit -> 'a) -> 'a

val pif_of_host :
     __context:Context.t
  -> API.ref_network
  -> API.ref_host
  -> API.ref_PIF * API.pIF_t

val ip_of_pif : API.ref_PIF * API.pIF_t -> Cluster_interface.address

val assert_pif_prerequisites : API.ref_PIF * API.pIF_t -> unit

val assert_pif_attached_to :
  __context:Context.t -> host:[`host] Ref.t -> pIF:[`PIF] Ref.t -> unit

val handle_error : Cluster_interface.error -> 'a

val assert_cluster_host_can_be_created :
  __context:Context.t -> host:API.ref_host -> unit

val get_required_cluster_stacks :
  __context:Context.t -> sr_sm_type:string -> string list

val assert_cluster_stack_valid : cluster_stack:string -> unit

val with_clustering_lock_if_needed :
  __context:Context.t -> sr_sm_type:string -> string -> (unit -> 'a) -> 'a

val with_clustering_lock_if_cluster_exists :
  __context:Context.t -> string -> (unit -> 'a) -> 'a

val find_cluster_host :
  __context:Context.t -> host:[`host] Ref.t -> API.ref_Cluster_host option

val get_network_internal :
  __context:Context.t -> self:[`Cluster] Ref.t -> [`network] Ref.t

val assert_cluster_host_enabled :
  __context:Context.t -> self:[`Cluster_host] Ref.t -> expected:bool -> unit

val assert_operation_host_target_is_localhost :
  __context:Context.t -> host:[`host] Ref.t -> unit

val assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack :
  __context:Context.t -> self:[`Cluster_host] Ref.t -> unit

module Daemon : sig
  val is_enabled : unit -> bool

  val enable : __context:Context.t -> unit

  val disable : __context:Context.t -> unit

  val restart : __context:Context.t -> unit
end

val rpc : __context:Context.t -> Rpc.call -> Rpc.response Idl.IdM.t

val maybe_switch_cluster_stack_version :
     __context:Context.t
  -> self:API.ref_Cluster_host
  -> cluster_stack:Cluster_interface.Cluster_stack.t
  -> unit

val assert_cluster_host_is_enabled_for_matching_sms :
  __context:Context.t -> host:[`host] Ref.t -> sr_sm_type:string -> unit

val is_clustering_disabled_on_host :
  __context:Context.t -> [`host] Ref.t -> bool

val compute_corosync_max_host_failures : __context:Context.t -> int

module Watcher : sig
  val on_corosync_update :
    __context:Context.t -> cluster:[`Cluster] Ref.t -> string list -> unit

  val signal_exit : unit -> unit

  val create_as_necessary : __context:Context.t -> host:[`host] Ref.t -> unit
end
