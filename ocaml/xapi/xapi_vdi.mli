(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

(** Module that defines API functions for VDI objects
 * @group XenAPI functions
*)

(** Checks to see if an operation is valid in this state. Returns Some exception
    if not and None if everything is ok. *)
(* Exposed here only for use by Test_vdi_allowed_operations, this
   declaration also serves to annotate the (op:API.vdi_operations)
   parameter. *)
val check_operation_error :
  __context:Context.t ->
  ?sr_records:'a list ->
  ?pbd_records:('b API.Ref.t * API.pBD_t) list ->
  ?vbd_records:('c  API.Ref.t * Db_actions.vBD_t) list ->
  bool ->
  Db_actions.vDI_t ->
  API.ref_VDI ->
  API.vdi_operations ->
  (string * string list) option

val assert_operation_valid :
  __context:Context.t ->
  self:[ `VDI ] API.Ref.t -> op:API.vdi_operations -> unit

val update_allowed_operations_internal :
  __context:Context.t ->
  self:[ `VDI ] API.Ref.t ->
  sr_records:'a list ->
  pbd_records:('b API.Ref.t * API.pBD_t) list ->
  vbd_records:('c API.Ref.t * Db_actions.vBD_t) list -> unit

val update_allowed_operations :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> unit

val cancel_tasks :
  __context:Context.t ->
  self:[ `VDI ] API.Ref.t ->
  all_tasks_in_db:'a Ref.t list -> task_ids:string list -> unit

val create :
  __context:Context.t ->
  name_label:string ->
  name_description:string ->
  sR:[ `SR ] API.Ref.t ->
  virtual_size:int64 ->
  _type:API.vdi_type ->
  sharable:bool ->
  read_only:bool ->
  other_config:(string * string) list ->
  xenstore_data:(string * string) list ->
  sm_config:(string * string) list -> tags:string list -> [ `VDI ] API.Ref.t

val pool_introduce :
  __context:Context.t ->
  uuid:string ->
  name_label:string ->
  name_description:string ->
  sR:[ `SR ] API.Ref.t ->
  _type:API.vdi_type ->
  sharable:bool ->
  read_only:bool ->
  other_config:(string * string) list ->
  location:string ->
  xenstore_data:(string * string) list ->
  sm_config:(string * string) list ->
  managed:bool ->
  virtual_size:int64 ->
  physical_utilisation:int64 ->
  metadata_of_pool:[ `pool ] API.Ref.t ->
  is_a_snapshot:bool ->
  snapshot_time:API.Date.iso8601 ->
  snapshot_of:[ `VDI ] API.Ref.t ->
  cbt_enabled:bool -> [ `VDI ] Ref.t

val db_introduce :
  __context:Context.t ->
  uuid:string ->
  name_label:string ->
  name_description:string ->
  sR:[ `SR ] API.Ref.t ->
  _type:API.vdi_type ->
  sharable:bool ->
  read_only:bool ->
  other_config:(string * string) list ->
  location:string ->
  xenstore_data:(string * string) list ->
  sm_config:(string * string) list ->
  managed:bool ->
  virtual_size:int64 ->
  physical_utilisation:int64 ->
  metadata_of_pool:[ `pool ] API.Ref.t ->
  is_a_snapshot:bool ->
  snapshot_time:API.Date.iso8601 ->
  snapshot_of:[ `VDI ] API.Ref.t ->
  cbt_enabled:bool -> [ `VDI ] Ref.t

val db_forget : __context:Context.t -> vdi:[ `VDI ] API.Ref.t -> unit

val introduce :
  __context:Context.t ->
  uuid:string ->
  name_label:string ->
  name_description:string ->
  sR:[ `SR ] API.Ref.t ->
  _type:API.vdi_type ->
  sharable:bool ->
  read_only:'a ->
  other_config:(string * string) list ->
  location:string ->
  xenstore_data:(string * string) list ->
  sm_config:(string * string) list ->
  managed:'b ->
  virtual_size:'c ->
  physical_utilisation:'d ->
  metadata_of_pool:'e ->
  is_a_snapshot:'f ->
  snapshot_time:'g -> snapshot_of:'h -> [ `VDI ] API.Ref.t
val update : __context:Context.t -> vdi:[ `VDI ] API.Ref.t -> unit
val forget : __context:Context.t -> vdi:[ `VDI ] API.Ref.t -> unit

(** driver_params is the storage-backend-specific parameters that are used to drive the
    snapshot operation (e.g. vmhint for NetAPP)
*)
val snapshot_and_clone :
  (dbg:string ->
   sr:string ->
   vdi_info:Storage_interface.vdi_info -> Storage_interface.vdi_info) ->
  __context:Context.t ->
  vdi:[ `VDI ] API.Ref.t ->
  driver_params:(string * string) list -> [ `VDI ] API.Ref.t

val snapshot :
  __context:Context.t ->
  vdi:[ `VDI ] API.Ref.t ->
  driver_params:(string * string) list -> [ `VDI ] API.Ref.t
val destroy : __context:Context.t -> self:[ `VDI ] API.Ref.t -> unit
val data_destroy : __context:Context.t -> self:[ `VDI ] API.Ref.t -> unit
val _data_destroy : __context:Context.t -> self:[ `VDI ] API.Ref.t -> timeout:float -> unit
(** This version of {!data_destroy} is for unit testing purposes: the timeout
    for waiting for the VDI's VBDs to disappear is configurable to enable faster
    unit tests. *)

val resize :
  __context:Context.t -> vdi:[ `VDI ] API.Ref.t -> size:int64 -> unit
val generate_config :
  __context:Context.t -> host:'a -> vdi:[ `VDI ] API.Ref.t -> string
val clone :
  __context:Context.t ->
  vdi:[ `VDI ] API.Ref.t ->
  driver_params:(string * string) list -> [ `VDI ] API.Ref.t
val copy :
  __context:Context.t ->
  vdi:[ `VDI ] API.Ref.t ->
  sr:'a Ref.t ->
  base_vdi:API.ref_VDI ->
  into_vdi:[ `VDI ] API.Ref.t Client.Id.t -> [ `VDI ] API.Ref.t Client.Id.t
val force_unlock : __context:'a -> vdi:'b -> 'c
val set_sharable :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:bool -> unit
val set_managed :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:bool -> unit
val set_read_only :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:bool -> unit
val set_missing :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:bool -> unit
val set_virtual_size :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:int64 -> unit
val set_physical_utilisation :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:int64 -> unit
val set_is_a_snapshot :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:bool -> unit
val set_snapshot_of :
  __context:Context.t ->
  self:[ `VDI ] API.Ref.t -> value:[ `VDI ] API.Ref.t -> unit
val set_snapshot_time :
  __context:Context.t ->
  self:[ `VDI ] API.Ref.t -> value:API.Date.iso8601 -> unit
val set_metadata_of_pool :
  __context:Context.t ->
  self:[ `VDI ] API.Ref.t -> value:[ `pool ] API.Ref.t -> unit
val set_on_boot :
  __context:Context.t ->
  self:[ `VDI ] API.Ref.t -> value:API.on_boot -> unit
val set_allow_caching :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:bool -> unit
val set_name_label :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:string -> unit
val set_name_description :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> value:string -> unit
val checksum : __context:Context.t -> self:API.ref_VDI -> string

(** Open a foreign database on a VDI *)
val open_database :
  __context:Context.t -> self:[ `VDI ] API.Ref.t -> API.ref_session

val read_database_pool_uuid : __context:'a -> self:API.ref_VDI -> string

val enable_cbt :
  __context:Context.t -> self:API.ref_VDI -> unit
val disable_cbt :
  __context:Context.t -> self:API.ref_VDI -> unit
val set_cbt_enabled :
  __context:Context.t -> self:API.ref_VDI -> value:bool -> unit
val list_changed_blocks :
  __context:Context.t -> vdi_from:API.ref_VDI -> vdi_to:API.ref_VDI -> string
val get_nbd_info :
  __context:Context.t -> self:API.ref_VDI -> API.vdi_nbd_server_info_t_set
val _get_nbd_info :
  __context:Context.t -> self:API.ref_VDI -> get_server_certificate:(host : [ `host ] Ref.t -> string) -> API.vdi_nbd_server_info_t_set
(** This version of {!get_nbd_info} is only here for unit testing: a mock
    [get_server_certificate] function can be provided to avoid querying the real
    certificate using the Client module, which is what {!get_nbd_info} does,
    which would cause the unit test to fail. *)
