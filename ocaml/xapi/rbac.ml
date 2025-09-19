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

module D = Debug.Make (struct let name = "rbac" end)

open D

let trackid session_id = Context.trackid_of_session (Some session_id)

(* From the Requirements:

   1) Since we rely on an external directory/authentication service, disabling this
   external authentication effectively disables RBAC too. When disabled like
   this only the local root account can be used so the system "fails secure".
   Given this we do not need any separate mechanism to enable/disable RBAC.

   2) At all times the RBAC policy will be applied; the license state will only
   affect the Pool Administrator's ability to modify the subject -> role mapping.

   3) The local superuser (root) has the "Pool Admin" role.
   4) If a subject has no roles assigned then, authentication will fail with an
   error such as PERMISSION_DENIED.

   5) To guarantee the new role takes effect, the user should be logged out and
   forced to log back in again (requires "Logout active user connections" permission)
   (So, there's no need to update the session immediately after modifying either the
   Subject.roles field or the Roles.subroles field, or for this function to
   immediately reflect any modifications in these fields without the user logging out
   and in again)

    *  Definitions:
    * R = { "Pool Admin" "Pool Operator" "VM Power Admin" "VM Admin" "VM Operator" "Read Only" }
          o i.e. R is the static set of roles
    * perm(r) = the static set of permissions associated with role r
          o e.g. perm("Pool Admin") gives the "Pool Admin" column from the above table
    * subject_role(sid) = the set of roles assigned to subject sid
          o This is the user-configurable subject -> role mapping
    * subject(s) = the unique subject ID associated with the user who logged in and got session s
    * groups(sid) = the set of group subject IDs associated with user subject ID sid
          o Note that a user is allowed to log in either because subject(s) is explicitly listed or because one or more of groups(sid) is explicitly listed

    * A session s will be permitted to call a XenAPI function associated with permission p iff
          o p is a member of all_perms(s) where
                + all_perms(s) = Union over r in roles(s) of perm(r) where
                      # roles(s) = Union over sid in all_sids(s) of subject_role(sid) where
                            * all_sids(s) = Union(subject(s), groups(subject(s))
    * and denied otherwise.

   Intuitively the 3 nested Unions above come from the 3 one -> many relationships:
   1. a session (i.e. a user) has multiple subject IDs;
   2. a subject ID has multiple roles;
   3. a role has multiple permissions.
*)

(* efficient look-up structures *)
(* Speed-up rbac lookup: (session_ref, permissions set) hashtabl *)
let session_permissions_tbl = Hashtbl.create 256 (* initial 256 sessions *)

module Permission_set = Set.Make (String)

let permission_set = Permission_set.of_list

let create_session_permissions_tbl ~session_id ~rbac_permissions =
  if Pool_role.is_master ()
  (* Create this structure on the master only, *)
  (* so as to avoid heap-leaking on the slaves *)
  then (
    debug "Creating permission-set tree for session %s"
      (Context.trackid_of_session (Some session_id)) ;
    let permission_tree = permission_set rbac_permissions in
    Hashtbl.replace session_permissions_tbl session_id permission_tree ;
    Some permission_tree
  ) else
    None

let destroy_session_permissions_tbl ~session_id =
  Hashtbl.remove session_permissions_tbl session_id

(* create a key permission name that can be in the session *)
let get_key_permission_name permission key_name =
  permission ^ "/key:" ^ key_name

(* create a key-error permission name that is never in the session *)
let get_keyERR_permission_name permission err = permission ^ "/keyERR:" ^ err

let permission_of_action ?args ~keys _action =
  (* all permissions are in lowercase, see gen_rbac.writer_ *)
  let action = String.lowercase_ascii _action in
  if keys = [] then
    (* most actions do not use rbac-guarded map keys in the arguments *)
    action
  else (* only actions with rbac-guarded map keys fall here *)
    match args with
    | None ->
        (* this should never happen *)
        debug "DENYING access: no args for keyed-action %s" action ;
        get_keyERR_permission_name action "DENY_NOARGS" (* will always deny *)
    | Some keys_values -> (
      match List.assoc_opt "key" keys_values with
      | None ->
          (* this should never happen *)
          debug "DENYING access: no 'key' argument in the action %s" action ;
          get_keyERR_permission_name action "DENY_NOKEY"
      | Some (Rpc.String key_name_in_args) -> (
        try
          let key_name =
            List.find
              (fun key_name ->
                if String.ends_with ~suffix:"*" key_name then
                  (* resolve wildcards at the end *)
                  String.starts_with
                    ~prefix:(String.sub key_name 0 (String.length key_name - 1))
                    key_name_in_args
                else (* no wildcards to resolve *)
                  key_name = key_name_in_args
              )
              keys
          in
          get_key_permission_name action (String.lowercase_ascii key_name)
        with Not_found ->
          (* expected, key_in_args is not rbac-protected *)
          action
      )
      | Some value ->
          (* this should never happen *)
          debug
            "DENYING access: wrong XML value [%s] in the 'key' argument of \
             action %s"
            (Rpc.to_string value) action ;
          get_keyERR_permission_name action "DENY_NOVALUE"
    )

let is_permission_in_session ~session_id ~permission ~session =
  let find_linear elem set = List.exists (fun e -> e = elem) set in
  let find_log elem set = Permission_set.mem elem set in
  let permission_tree =
    match Hashtbl.find_opt session_permissions_tbl session_id with
    | None ->
        create_session_permissions_tbl ~session_id
          ~rbac_permissions:session.API.session_rbac_permissions
    | x ->
        x
  in
  match permission_tree with
  | Some permission_tree ->
      find_log permission permission_tree
  | None ->
      find_linear permission session.API.session_rbac_permissions

open Db_actions

(* Given a list of permissions, determine if the given session is
   permitted to perform the related actions. If not, stop and return the
   first disallowed permission. This stops us doing redundant checks
   but also is consistent with the current RBAC error reporting, where
   a single action is usually reported. *)
let find_first_disallowed_permission ~__context ~session_id ~permissions =
  let is_local_session () =
    Session_check.is_local_session __context session_id
  in
  let doesn't_have_permission session permission =
    is_permission_in_session ~session_id ~permission ~session = false
  in
  (* Test session properties before querying permission sets. *)
  if is_local_session () then
    None
  else
    let session = DB_Action.Session.get_record ~__context ~self:session_id in
    if session.API.session_is_local_superuser then
      None
    else
      List.find_opt (doesn't_have_permission session) permissions

(* Determine if session has a given permission. *)
let is_access_allowed ~__context ~session_id ~permission =
  find_first_disallowed_permission ~__context ~session_id
    ~permissions:[permission]
  |> Option.is_none

let get_session_of_context ~__context ~permission =
  try Context.get_session_id __context
  with Failure _ ->
    let msg = "no session in context" in
    raise Api_errors.(Server_error (rbac_permission_denied, [permission; msg]))

let disallowed_permission_exn ?(extra_dmsg = "") ?(extra_msg = "") ~__context
    ~permission ~action =
  let session_id = get_session_of_context ~__context ~permission in
  let allowed_roles =
    try
      Xapi_role.get_by_permission_name_label ~__context ~label:permission
      |> List.map (fun self -> Xapi_role.get_name_label ~__context ~self)
      |> String.concat ", "
    with e ->
      debug "Could not obtain allowed roles for %s (%s)" permission
        (ExnHelper.string_of_exn e) ;
      "<Could not obtain the list.>"
  in
  let msg =
    Printf.sprintf
      "No permission in user session. (Roles with this permission: %s)%s"
      allowed_roles extra_msg
  in
  debug "%s[%s]: %s %s %s" action permission msg (trackid session_id) extra_dmsg ;
  raise Api_errors.(Server_error (rbac_permission_denied, [permission; msg]))

let nofn () = ()

let check ?(extra_dmsg = "") ?(extra_msg = "") ?args ?(keys = []) ~__context ~fn
    session_id action =
  let permission = permission_of_action action ?args ~keys in
  let allow_access () =
    (* Allow access to action. *)
    let sexpr_of_args = Rbac_audit.allowed_pre_fn ~__context ~action ?args () in
    try
      (* Call the RBAC-protected function. *)
      let result = fn () in
      Rbac_audit.allowed_post_fn_ok ~__context ~session_id ~action ~permission
        ?sexpr_of_args ?args ~result () ;
      result
    with error ->
      (* Catch all exceptions and log to RBAC audit log. *)
      Backtrace.is_important error ;
      Rbac_audit.allowed_post_fn_error ~__context ~session_id ~action
        ~permission ?sexpr_of_args ?args ~error () ;
      (* Re-raise. *)
      raise error
  in
  let deny_access () =
    (*  Deny access to action, raising an exception. *)
    Rbac_audit.denied ~__context ~session_id ~action ~permission ?args () ;
    raise
      (disallowed_permission_exn ~extra_dmsg ~extra_msg ~__context ~permission
         ~action
      )
  in
  if is_access_allowed ~__context ~session_id ~permission then
    allow_access ()
  else
    deny_access ()

let assert_permission_name ~__context ~permission =
  let session_id = get_session_of_context ~__context ~permission in
  check ~__context ~fn:nofn session_id permission

let assert_permission ~__context ~permission =
  assert_permission_name ~__context ~permission:permission.role_name_label

(* Populates assert_permission_fn on behalf of TaskHelper to
   avoid a dependency cycle. *)
let () =
  if !TaskHelper.rbac_assert_permission_fn = None then
    TaskHelper.rbac_assert_permission_fn := Some assert_permission

let has_permission_name ~__context ~permission =
  let session_id = get_session_of_context ~__context ~permission in
  is_access_allowed ~__context ~session_id ~permission

let has_permission ~__context ~permission =
  has_permission_name ~__context ~permission:permission.role_name_label

let check_with_new_task ?(extra_dmsg = "") ?(extra_msg = "")
    ?(task_desc = "check") ?args ~fn session_id action =
  let task_desc = task_desc ^ ":" ^ action in
  Server_helpers.exec_with_new_task task_desc (fun __context ->
      check ~extra_dmsg ~extra_msg ~__context ?args ~fn session_id action
  )

(* used by xapi_http.ml to decide if rbac checks should be applied *)
let is_rbac_enabled_for_http_action http_action_name =
  not
    (List.mem http_action_name Datamodel.public_http_actions_with_no_rbac_check)
