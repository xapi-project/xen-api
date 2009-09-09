module D = Debug.Debugger(struct let name="xapi_role" end)
open D

(* A note on roles: *)
(* Here, roles and permissons are treated as a recursive type, where the   *)
(* permissions are the leaves and roles are intermediate nodes of the tree *)
(* For each permission there is one and only one XAPI/HTTP call *) 

(* Available static-role aliases in the Role language *)
(* used in the roles field of the subject class *)
let role_name_pool_admin = "pool-admin"
let role_name_pool_operator = "pool-operator"
let role_name_vm_power_admin = "vm-power-admin"
let role_name_vm_admin = "vm-admin"
let role_name_vm_operator = "vm-operator"
let role_name_read_only = "read-only"

let ref_prefix = "OpaqueRef:" (* why doesnt this work? -> Ref.ref_prefix*)

open Db_actions

(* temporary location for permissions *)
(* permissions should be automatically generated into autogen/permissions.ml *)
let permission_description = "A basic permission"
let permission_Subject_add =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000001";
		role_name_label = "P_Subject_add";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}
let permission_Pool_join =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000002";
		role_name_label = "P_Pool_start";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}
let permission_VM_migrate =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000003";
		role_name_label = "P_VM_migrate";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}
let permission_VM_start_on =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000004";
		role_name_label = "P_VM_start_on";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}
let permission_VM_clone =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000005";
		role_name_label = "P_VM_clone";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}
let permission_VM_start =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000006";
		role_name_label = "P_VM_start";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}
let permission_VM_stop =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000007";
		role_name_label = "P_VM_stop";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}
let permission_VM_get_all =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000008";
		role_name_label = "P_VM_get_all";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}
let permission_Host_get_all =
	{
		role_uuid = "6e7b1a28-db39-c637-1111-000000000009";
		role_name_label = "P_Host_get_all";
		role_name_description = permission_description;
		role_subroles = []; (* permission cannot have any subroles *)
	}

let get_refs permissions = List.map (fun p->Ref.of_string (ref_prefix ^ p.role_uuid)) permissions

let role_permissions_read_only = [permission_Host_get_all;permission_VM_get_all]
let role_permissions_vm_operator = role_permissions_read_only @
	[permission_VM_start]
let role_permissions_vm_admin = role_permissions_vm_operator @
	[permission_VM_clone]
let role_permissions_vm_power_admin = role_permissions_vm_admin @
	[permission_VM_start_on;permission_VM_migrate]
let role_permissions_pool_operator = role_permissions_vm_power_admin @
	[permission_Pool_join]
let role_permissions_pool_admin = role_permissions_pool_operator @
	[permission_Subject_add]
let all_static_permissions = role_permissions_pool_admin



(* Available static Roles *)
let role_pool_admin =
	{
		role_uuid = "6e7b1a28-db39-c637-8624-0000000000a0";
		role_name_label = role_name_pool_admin;
		role_name_description = "The Pool Administrator role can do anything";
		role_subroles = get_refs role_permissions_pool_admin;
		(*role_is_basic = false;*)
		(*role_is_complete = true;*)
		}
let role_pool_operator =
	{
		role_uuid = "6e7b1a28-db39-c637-8624-0000000000a1";
		role_name_label = role_name_pool_operator;
		role_name_description = "The Pool Operator can do almost anything";
		role_subroles = get_refs role_permissions_pool_operator;
		(*role_is_basic = false;*)
		(*role_is_complete = true;*)
	}
let role_vm_power_admin =
	{
		role_uuid = "6e7b1a28-db39-c637-8624-0000000000a2";
		role_name_label = role_name_vm_power_admin;
		role_name_description = "The VM Power Administrator role can do anything affecting VM properties across the pool";
		role_subroles = get_refs role_permissions_vm_power_admin;
		(*role_is_basic = false;*)
		(*role_is_complete = true;*)
	}
let role_vm_admin =
	{
		role_uuid = "6e7b1a28-db39-c637-8624-0000000000a3";
		role_name_label = role_name_vm_admin;
		role_name_description = "The VM Administrator role can do anything to a VM";
		role_subroles = get_refs role_permissions_vm_admin;
		(*role_is_basic = false;*)
		(*role_is_complete = true;*)
	}
let role_vm_operator =
	{
		role_uuid = "6e7b1a28-db39-c637-8624-0000000000a4";
		role_name_label = role_name_vm_operator;
		role_name_description = "The VM Operator role can do anything to an already existing VM";
		role_subroles = get_refs role_permissions_vm_operator;
		(*role_is_basic = false;*)
		(*role_is_complete = true;*)
	}
let role_read_only =
	{
		role_uuid = "6e7b1a28-db39-c637-8624-0000000000a5";
		role_name_label = role_name_read_only;
		role_name_description = "The Read-Only role can only read values";
		role_subroles = get_refs role_permissions_read_only;
		(*role_is_basic = false;*)
		(*role_is_complete = true;*)
	}

let get_all_static_roles =
	[
		role_pool_admin;
		role_pool_operator;
		role_vm_power_admin;
		role_vm_admin;
		role_vm_operator;
		role_read_only
	] @ all_static_permissions

(* IN DB, REF IS A POINTER TO THE HASHTABLE ROW *)
(* WE DO NOT (yet) USE A HASHTABLE FOR STATIC ROLES, SO WE DO REF = "OpaqueRef:"^UUID *)
let ref_of_role ~role = String_to_DM.ref_role (ref_prefix ^ role.role_uuid)

(*    val get_all : __context:Context.t -> ref_role_set*)
(*let get_all ~__context = [Ref.make();Ref.make();Ref.make()]*)
let get_all ~__context = 
	List.map (fun r -> ref_of_role r) get_all_static_roles
	(*@ (* concatenate with Db table *)
	Db.Role.get_all ~__context*)

let is_valid_role ~__context ~role =
	List.exists (fun a_role -> a_role = role) (get_all ~__context)

let get_common ~__context ~self ~static_fn ~db_fn =
	try (* first look up across the static roles *)
		let static_record = 
			(List.find 
				(fun a_role -> (ref_of_role a_role) = self) 
				get_all_static_roles
			)
		in
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
let get_all_records_where ~__context ~expr = (*[(Ref.make(),dummy_api_role)]*)
	List.map 
		(fun r -> ((ref_of_role r),(get_api_record ~static_record:r)))
		get_all_static_roles
	(*@ (* concatenate with Db table *)
	Db.Role.get_all_records_where ~__context ~expr*)

(*    val get_all_records : __context:Context.t -> ref_role_to_role_t_map*)
let get_all_records ~__context =
	get_all_records_where ~__context ~expr:"True"

(*    val get_by_uuid : __context:Context.t -> uuid:string -> ref_role*)
let get_by_uuid ~__context ~uuid =
	try
		let static_record = 
			(List.find (fun a_role -> (a_role.role_uuid) = uuid) get_all_static_roles)
		in
		ref_of_role static_record
	with Not_found ->
		(* pass-through to Db *)
		Db.Role.get_by_uuid ~__context ~uuid

let get_by_name_label ~__context ~label =
	try
		let static_record = 
			(List.find (fun a_role -> (a_role.role_name_label) = label) get_all_static_roles)
		in
		[ref_of_role static_record]
	with Not_found ->
		(* pass-through to Db *)
		Db.Role.get_by_name_label ~__context ~label

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

(* SETTTERS DO NOTHING IN RBAC 1.0 *)
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
	Listext.List.setify (rec_get_permissions_of_role ~__context ~role)

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
