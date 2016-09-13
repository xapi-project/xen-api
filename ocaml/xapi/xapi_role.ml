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
(** Module that defines API functions for Role objects
 * @group XenAPI functions
*)

module D = Debug.Make(struct let name="xapi_role" end)
open D

open Db_actions

(* A note on roles: *)
(* Here, roles and permissons are treated as a recursive type, where the   *)
(* permissions are the leaves and roles are intermediate nodes of the tree *)
(* For each permission there is one and only one XAPI/HTTP call *)

let get_all_static_roles =
  Rbac_static.all_static_permissions @ Rbac_static.all_static_roles

(* In Db, Ref is a pointer to the hashtable row. Here, ref="OpaqueRef:"^uuid *)
let ref_of_role ~role = String_to_DM.ref_role (Ref.ref_prefix ^ role.role_uuid)

(* efficient look-up structures *)
let static_role_by_ref_tbl = Hashtbl.create (List.length get_all_static_roles)
let static_role_by_uuid_tbl = Hashtbl.create (List.length get_all_static_roles)
let static_role_by_name_label_tbl = Hashtbl.create (List.length get_all_static_roles)
let _ =
  List.iter (* initialize static_role_by_ref_tbl *)
    (fun r->Hashtbl.add static_role_by_ref_tbl (ref_of_role r) r)
    get_all_static_roles;
  List.iter (* initialize static_role_by_uuid_tbl *)
    (fun r->Hashtbl.add static_role_by_uuid_tbl (r.role_uuid) r)
    get_all_static_roles;
  List.iter (* initialize static_role_by_name_tbl *)
    (fun r->Hashtbl.add static_role_by_name_label_tbl (r.role_name_label) r)
    get_all_static_roles

let find_role_by_ref ref = Hashtbl.find static_role_by_ref_tbl ref
let find_role_by_uuid uuid = Hashtbl.find static_role_by_uuid_tbl uuid
let find_role_by_name_label name_label = Hashtbl.find static_role_by_name_label_tbl name_label

(*    val get_all : __context:Context.t -> ref_role_set*)
let get_all ~__context =
  List.map (fun r -> ref_of_role r) get_all_static_roles
(*@ (* concatenate with Db table *)
  	Db.Role.get_all ~__context*)

let is_valid_role ~__context ~role =
  Hashtbl.mem static_role_by_ref_tbl role

let get_common ~__context ~self ~static_fn ~db_fn =
  try (* first look up across the static roles *)
    let static_record = find_role_by_ref self in
    static_fn static_record
  with Not_found -> (* then look up across the roles in the Db *)
    db_fn ~__context ~self

(*    val get_record : __context:Context.t -> self:ref_role -> role_t*)
let get_api_record ~static_record =
  {	(* Db_actions.role_t -> API.role_t *)
    API.role_uuid=static_record.Db_actions.role_uuid;
    API.role_name_label=static_record.Db_actions.role_name_label;
    API.role_name_description=static_record.Db_actions.role_name_description;
    API.role_subroles=static_record.Db_actions.role_subroles;
    (*API.role_is_basic=static_record.Db_actions.role_is_basic;*)
    (*API.role_is_complete=static_record.Db_actions.role_is_complete;*)
    (*API.role_subjects=static_record.Db_actions.role_subjects;*)
  }
let get_record ~__context ~self =
  get_common ~__context ~self
    ~static_fn:(fun static_record -> get_api_record static_record)
    ~db_fn:(fun ~__context ~self -> Db.Role.get_record ~__context ~self)

(*    val get_all_records_where : __context:Context.t -> expr:string -> ref_role_to_role_t_map*)
let expr_no_permissions = "subroles<>[]"
let expr_only_permissions = "subroles=[]"
let get_all_records_where ~__context ~expr =
  if expr = expr_no_permissions then (* composite role, ie. not a permission *)
    List.map
      (fun r -> ((ref_of_role r),(get_api_record ~static_record:r)))
      Rbac_static.all_static_roles
  else if expr = expr_only_permissions then (* composite role, ie. a permission *)
    List.map
      (fun r -> ((ref_of_role r),(get_api_record ~static_record:r)))
      Rbac_static.all_static_permissions
  else (* anything in this table, ie. roles+permissions *)
    List.map
      (fun r -> ((ref_of_role r),(get_api_record ~static_record:r)))
      get_all_static_roles
(*@ (* concatenate with Db table *)
  			(* TODO: this line is crashing for some unknown reason, but not needed in RBAC 1 *)
  			Db.Role.get_all_records_where ~__context ~expr*)

(*    val get_all_records : __context:Context.t -> ref_role_to_role_t_map*)
let get_all_records ~__context =
  get_all_records_where ~__context ~expr:"True"

(*    val get_by_uuid : __context:Context.t -> uuid:string -> ref_role*)
let get_by_uuid ~__context ~uuid =
  try
    let static_record = find_role_by_uuid uuid in
    ref_of_role static_record
  with Not_found ->
    (* pass-through to Db *)
    Db.Role.get_by_uuid ~__context ~uuid

let get_by_name_label ~__context ~label =
  try
    let static_record = find_role_by_name_label label in
    [ref_of_role static_record]
  with Not_found ->
    (* pass-through to Db *)
    Db.Role.get_by_name_label ~__context ~label

(*    val get_uuid : __context:Context.t -> self:ref_role -> string*)
let get_uuid ~__context ~self =
  get_common ~__context ~self
    ~static_fn:(fun static_record -> static_record.role_uuid)
    ~db_fn:(fun ~__context ~self -> Db.Role.get_uuid ~__context ~self)

(*    val get_name : __context:Context.t -> self:ref_role -> string*)
let get_name_label ~__context ~self =
  get_common ~__context ~self
    ~static_fn:(fun static_record -> static_record.role_name_label)
    ~db_fn:(fun ~__context ~self -> Db.Role.get_name_label ~__context ~self)

(*    val get_description : __context:Context.t -> self:ref_role -> string*)
let get_name_description ~__context ~self =
  get_common ~__context ~self
    ~static_fn:(fun static_record -> static_record.role_name_description)
    ~db_fn:(fun ~__context ~self -> Db.Role.get_name_description ~__context ~self)

(*    val get_permissions : __context:Context.t -> self:ref_role -> string_set*)
let get_subroles ~__context ~self =
  get_common ~__context ~self
    ~static_fn:(fun static_record -> static_record.role_subroles)
    ~db_fn:(fun ~__context ~self -> Db.Role.get_subroles ~__context ~self)

(*    val get_is_basic : __context:Context.t -> self:ref_role -> bool*)
(*let get_is_basic ~__context ~self =
  	get_common ~__context ~self
  		~static_fn:(fun static_record -> static_record.role_is_basic)
  		~db_fn:(fun ~__context ~self -> Db.Role.get_is_basic ~__context ~self)
*)
(*    val get_is_complete : __context:Context.t -> self:ref_role -> bool*)
(*let get_is_complete ~__context ~self =
  	get_common ~__context ~self
  		~static_fn:(fun static_record -> static_record.role_is_complete)
  		~db_fn:(fun ~__context ~self -> Db.Role.get_is_complete ~__context ~self)
*)

(* XenCenter needs these functions *)
(*
1. get_all_allowed_calls: role->[permission]
2. get_all_allowed_calls: subject_identifier->[permission]  (see Xapi_subject)
3. get_all_roles: permission->[roles]
*)

(* 1. get_all_allowed_calls: role->[permission] *)
(* This function recursively expands a role into its basic permission set. In other words, it *)
(* returns the leaves of the tree whose root is the role passed as parameter *)
let get_permissions_common ~__context ~role ~ret_value_fn =
  let rec rec_get_permissions_of_role ~__context ~role =
    let subroles = get_subroles ~__context ~self:role in
    if List.length subroles = 0
    then (* base case = leaf node = permission is role itself *)
      [ret_value_fn role]
    else (* step = go recursively down composite roles *)
      (List.fold_left
         (fun accu role ->
            List.rev_append
              (rec_get_permissions_of_role ~__context ~role)
              accu
         )
         []
         (subroles)
      )
  in
  Stdext.Listext.List.setify (rec_get_permissions_of_role ~__context ~role)

let get_permissions ~__context ~self =
  get_permissions_common ~__context ~role:self
    ~ret_value_fn:(fun role -> role)

let get_permissions_name_label ~__context ~self =
  get_permissions_common ~__context ~role:self
    ~ret_value_fn:(fun role -> get_name_label ~__context ~self:role)

(*3. get_all_roles: permission->[roles]*)
(* return all roles that contain this permission *)
(* including the transitive closure *)
let get_by_permission_common ~__context ~permission ~cmp_fn =
  List.filter
    (fun role -> List.exists (cmp_fn) (get_permissions ~__context ~self:role))
    (List.filter
       (fun r -> r <> permission) (* do not include permission itself *)
       (get_all ~__context) (* get all roles and permissions *)
    )

let get_by_permission ~__context ~permission =
  get_by_permission_common ~__context ~permission
    ~cmp_fn:(fun perm -> permission = perm)

let get_by_permission_name_label ~__context ~label =
  let permission =
    let ps = get_by_name_label ~__context ~label in
    if List.length ps > 0
    then List.hd ps (* names are unique, there's either 0 or 1*)
    else Ref.null (* name not found *)
  in
  get_by_permission_common ~__context ~permission
    ~cmp_fn:(fun perm -> label = (get_name_label ~__context ~self:perm))


(*
(* SETTTERS DO NOTHING IN RBAC 1.0 *)
(* For RBAC 2.0, with dynamic roles table, set Role.force_custom_actions=Some(RW)*)
(* in datamodel.ml and implement the functions below *)

(*    val create : __context:Context.t -> id:string -> name:string -> description:string -> permissions:string_set -> is_basic:bool -> is_complete:bool -> ref_role*)
(* we do not allow repeated name_labels in the role table *)
let create ~__context ~name_label ~name_description ~subroles =
	(* disabled in RBAC 1.0 *)
	(*
	let ref=Ref.make() in
	let uuid=Uuid.to_string (Uuid.make_uuid()) in
	(* TODO: verify the uniqueness of id *)
	if id = "no"
	then raise (Api_errors.Server_error (Api_errors.role_not_found, []))
	else
	Db.Role.create ~__context ~ref ~uuid ~id ~name ~description ~permissions ~is_basic ~is_complete;
	ref
	*)
	Ref.null

(*    val destroy : __context:Context.t -> self:ref_role -> unit*)
let destroy ~__context ~self =
	(* disabled in RBAC 1.0 *)
	(* in RBAC 2.0: it is only possible to delete a role if it is not in*)
	(* any subject.roles fields *)
	(* Db.Role.destroy ~__context ~self*)
	()

(*    val set_uuid : __context:Context.t -> self:ref_role -> value:string -> unit*)
let set_uuid ~__context ~self ~value = ()
(*    val set_id : __context:Context.t -> self:ref_role -> value:string -> unit*)
(*let set_id ~__context ~self ~value = ()*)
(*    val set_name : __context:Context.t -> self:ref_role -> value:string -> unit*)
(* we do not allow repeated name_labels in the role table *)
let set_name_label ~__context ~self ~value = ()
(*    val set_description : __context:Context.t -> self:ref_role -> value:string -> unit*)
let set_name_description ~__context ~self ~value = ()
(*    val set_permissions : __context:Context.t -> self:ref_role -> value:string_set -> unit*)
let set_subroles ~__context ~self ~value = ()
(*    val add_permissions : __context:Context.t -> self:ref_role -> value:string -> unit*)
let add_subroles ~__context ~self ~value = ()
(*    val remove_permissions : __context:Context.t -> self:ref_role -> value:string -> unit*)
let remove_subroles ~__context ~self ~value = ()
(*    val set_is_basic : __context:Context.t -> self:ref_role -> value:bool -> unit*)
(*let set_is_basic ~__context ~self ~value = ()*)
(*    val set_is_complete : __context:Context.t -> self:ref_role -> value:bool -> unit*)
(*let set_is_complete ~__context ~self ~value = ()*)
*)
