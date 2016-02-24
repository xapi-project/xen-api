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
(** Module that defines public API functions for Session objects
 * @group XenAPI functions
*)

(** {2 (Fill in Title!)} *)

(* TODO: consider updating sm_exec.ml and removing login_no_password from this mli *)
val login_no_password: __context:Context.t -> uname:string option -> host:[ `host ] Ref.t -> pool:bool -> is_local_superuser:bool -> subject:[ `subject ] Ref.t -> auth_user_sid:string -> auth_user_name:string -> rbac_permissions:string list -> [ `session ] Ref.t

(* public functions *)
val destroy_db_session: __context:Context.t -> self:API.ref_session -> unit
val revalidate_all_sessions: __context:Context.t -> unit
val consider_touching_session: (Rpc.call -> Rpc.response) -> API.ref_session -> unit -> unit
val slave_login: __context:Context.t -> host:[ `host ] Ref.t -> psecret:string -> [ `session ] Ref.t
val slave_local_login: __context:Context.t -> psecret:string -> API.ref_session
val slave_local_login_with_password: __context:Context.t -> uname:string -> pwd:string -> API.ref_session
val login_with_password: __context:Context.t ->  uname:string ->pwd:string -> version:'a -> originator:string -> [ `session ] Ref.t
val change_password: __context:Context.t -> old_pwd:string -> new_pwd:string -> unit
val logout: __context:Context.t -> unit
val local_logout: __context:Context.t -> unit
val get_group_subject_identifier_from_session: __context:Context.t -> session:[ `session ] Ref.t -> string
val get_all_subject_identifiers: __context:Context.t -> string list
val logout_subject_identifier: __context:Context.t -> subject_identifier:string -> unit
val get_top: __context:Context.t -> self:API.ref_session -> API.ref_session
val create_readonly_session:
  __context:Context.t -> uname:string -> db_ref:Db_ref.t option -> API.ref_session
val create_from_db_file: __context:Context.t -> filename:string -> API.ref_session
