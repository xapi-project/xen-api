(*
 * Copyright (c) Cloud Software Group
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

module SXM : Debug.DEBUG

module State : sig
  module Receive_state : sig
    type t = {
        sr: Storage_interface.sr
      ; dummy_vdi: Storage_interface.vdi
      ; leaf_vdi: Storage_interface.vdi
      ; leaf_dp: string
      ; parent_vdi: Storage_interface.vdi
      ; remote_vdi: Storage_interface.vdi
      ; mirror_vm: Storage_interface.vm
    }

    val t_sr : (Storage_interface.sr, t) Rpc.Types.field

    val t_dummy_vdi : (Storage_interface.vdi, t) Rpc.Types.field

    val t_leaf_vdi : (Storage_interface.vdi, t) Rpc.Types.field

    val t_leaf_dp : (string, t) Rpc.Types.field

    val t_parent_vdi : (Storage_interface.vdi, t) Rpc.Types.field

    val t_remote_vdi : (Storage_interface.vdi, t) Rpc.Types.field

    val t_mirror_vm : (Storage_interface.vm, t) Rpc.Types.field

    val typ_of : t Rpc.Types.typ

    val t : t Rpc.Types.def

    val rpc_of_t : t -> Rpc.t

    val t_of_rpc : Rpc.t -> t
  end

  module Send_state : sig
    type remote_info = {
        dp: string
      ; vdi: Storage_interface.vdi
      ; url: string
      ; verify_dest: bool
    }

    val remote_info_dp : (string, remote_info) Rpc.Types.field

    val remote_info_vdi : (Storage_interface.vdi, remote_info) Rpc.Types.field

    val remote_info_url : (string, remote_info) Rpc.Types.field

    val remote_info_verify_dest : (bool, remote_info) Rpc.Types.field

    val typ_of_remote_info : remote_info Rpc.Types.typ

    val remote_info : remote_info Rpc.Types.def

    type tapdev = Tapctl.tapdev

    val typ_of_tapdev : Tapctl.tapdev Rpc.Types.typ

    type handle = Scheduler.handle

    val typ_of_handle : Scheduler.handle Rpc.Types.typ

    type t = {
        url: string
      ; dest_sr: Storage_interface.sr
      ; remote_info: remote_info option
      ; local_dp: string
      ; tapdev: tapdev option
      ; mutable failed: bool
      ; mutable watchdog: handle option
    }

    val t_url : (string, t) Rpc.Types.field

    val t_dest_sr : (Storage_interface.sr, t) Rpc.Types.field

    val t_remote_info : (remote_info option, t) Rpc.Types.field

    val t_local_dp : (string, t) Rpc.Types.field

    val t_tapdev : (tapdev option, t) Rpc.Types.field

    val t_failed : (bool, t) Rpc.Types.field

    val t_watchdog : (handle option, t) Rpc.Types.field

    val typ_of : t Rpc.Types.typ

    val t : t Rpc.Types.def

    val rpc_of_t : t -> Rpc.t

    val t_of_rpc : Rpc.t -> t
  end

  module Copy_state : sig
    type t = {
        base_dp: string
      ; leaf_dp: string
      ; remote_dp: string
      ; dest_sr: Storage_interface.sr
      ; copy_vdi: Storage_interface.vdi
      ; remote_url: string
      ; verify_dest: bool
    }

    val t_base_dp : (string, t) Rpc.Types.field

    val t_leaf_dp : (string, t) Rpc.Types.field

    val t_remote_dp : (string, t) Rpc.Types.field

    val t_dest_sr : (Storage_interface.sr, t) Rpc.Types.field

    val t_copy_vdi : (Storage_interface.vdi, t) Rpc.Types.field

    val t_remote_url : (string, t) Rpc.Types.field

    val t_verify_dest : (bool, t) Rpc.Types.field

    val typ_of : t Rpc.Types.typ

    val t : t Rpc.Types.def

    val rpc_of_t : t -> Rpc.t

    val t_of_rpc : Rpc.t -> t
  end

  val loaded : bool ref

  val mutex : Mutex.t

  type send_table = (string, Send_state.t) Hashtbl.t

  type recv_table = (string, Receive_state.t) Hashtbl.t

  type copy_table = (string, Copy_state.t) Hashtbl.t

  type osend

  type orecv

  type ocopy

  type _ operation =
    | Send_op : Send_state.t -> osend operation
    | Recv_op : Receive_state.t -> orecv operation
    | Copy_op : Copy_state.t -> ocopy operation

  type _ table =
    | Send_table : send_table -> osend table
    | Recv_table : recv_table -> orecv table
    | Copy_table : copy_table -> ocopy table

  val active_send : send_table

  val active_recv : recv_table

  val active_copy : copy_table

  val table_of_op : 'a operation -> 'a table

  val persist_root : string ref

  val path_of_table : 'a table -> string

  val rpc_of_table : 'a table -> Rpc.t

  val to_string : 'a table -> string

  val rpc_of_path : string -> Rpc.t

  val load_one : 'a table -> unit

  val load : unit -> unit

  val save_one : 'a table -> unit

  val save : unit -> unit

  val access_table : save_after:bool -> ('a -> 'b) -> 'a -> 'b

  val map_of :
       unit
    -> (string * Send_state.t) list
       * (string * Receive_state.t) list
       * (string * Copy_state.t) list

  val add : string -> 'a operation -> unit

  val find : 'a -> ('a, 'b) Hashtbl.t -> 'b option

  val remove : 'a -> ('a, 'b) Hashtbl.t -> unit

  val clear : unit -> unit

  val remove_local_mirror : string -> unit

  val remove_receive_mirror : string -> unit

  val remove_copy : string -> unit

  val find_active_local_mirror : string -> Send_state.t option

  val find_active_receive_mirror : string -> Receive_state.t option

  val find_active_copy : string -> Copy_state.t option

  val mirror_id_of : Storage_interface.sr * Storage_interface.vdi -> string

  val of_mirror_id : string -> Storage_interface.sr * Storage_interface.vdi

  val copy_id_of : Storage_interface.sr * Storage_interface.vdi -> string

  val of_copy_id : string -> Storage_interface.sr * Storage_interface.vdi
end

val vdi_info :
  Storage_interface.async_result_t option -> Storage_interface.vdi_info

val remove_from_sm_config :
  Storage_interface.vdi_info -> string -> Storage_interface.vdi_info

val add_to_sm_config :
  Storage_interface.vdi_info -> string -> string -> Storage_interface.vdi_info

val with_http :
     Http.Request.t
  -> (Http.Response.t * Unix.file_descr -> 'a)
  -> Unix.file_descr
  -> 'a
