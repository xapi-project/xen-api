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
(* Auto code-generator for the static relations between static roles and *)
(* permissions in the datamodel. *)
(* marcusg 21/07/2009*)

module O = Ocaml_syntax
module DT = Datamodel_types
module DU = Datamodel_utils
module DM = Datamodel
module OU = Ocaml_utils
module Client = Gen_client
open DT


let rec role_idx = function
  | (_,[])->(-1)
  |(e1,e2::xs)-> if e1=e2 then 0 else 1+(role_idx (e1,xs))

let internal_role_local_root = "_local_root_"

(* the output of this function is used as input by the automatic tests *)
let writer_csv static_roles_permissions static_permissions_roles =
  (Printf.sprintf "%s,PERMISSION/ROLE,%s\n"
     (let t = Debug.gettimestring () in (String.sub t 0 ((String.length t)-1)))
     (* role titles are ordered by roles in roles_all *)
     (List.fold_left (fun rr r->rr^r^",") "" Datamodel_roles.roles_all)
  )
  ^List.fold_left
    (fun acc (permission,roles) ->
       (Printf.sprintf ",%s," permission)
       ^(List.fold_left
           (fun acc role -> if (List.exists (fun r->r=role) roles) then "X,"^acc else ","^acc)
           ""
           (List.rev Datamodel_roles.roles_all) (* Xs are ordered by roles in roles_all *)
        )
       ^"\n"
       ^acc
    )
    ""
    static_permissions_roles

let hash2uuid str =
  let h = Digest.string str in
  let hex = Digest.to_hex h in
  let int_array hex =
    let l = ref [] in
    Scanf.sscanf
      hex
      "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x"
      (fun a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 ->
         l := [a0;a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;]);
    Array.of_list !l
  in
  Uuid.string_of_uuid (Uuid.uuid_of_int_array (int_array hex))

let replace_char str c1 c2 =
  let buf = Bytes.of_string str in (*defensive copy*)
  String.iteri (fun i _ ->
    if str.[i]=c1 then Bytes.set buf i c2 else ()
  ) str;
  Bytes.unsafe_to_string buf

let role_uuid name = hash2uuid name

let permission_description = "A basic permission"
let permission_name wire_name =
  let open Xapi_stdext_std in
  let s1 =replace_char (Printf.sprintf "permission_%s" wire_name) '.' '_' in
  let s2 = replace_char s1 '/' '_' in
  let s3 = Xstringext.String.replace "*" "WILDCHAR" s2 in
  Xstringext.String.replace ":" "_" s3

let permission_index = ref 0
let writer_permission name nperms =
  let permission_uuid = role_uuid name in
  (*let permission_ref = role_ref name in*)
  let permission_name_label =
    (String.lowercase_ascii name) (* lowercase here asked by GUI team *)
  in
  permission_index := !permission_index+1;
  let permission_number = (Printf.sprintf "%i/%i" !permission_index nperms) in
  (Printf.sprintf "let %s = \n  { (* %s *)\n" (permission_name name) permission_number)
  (*^(Printf.sprintf "  role_ref = \"%s\";\n" permission_ref)*)
  ^(Printf.sprintf "  role_uuid = \"%s\";\n" permission_uuid)
  ^(Printf.sprintf "  role_name_label = \"%s\";\n" permission_name_label)
  ^(Printf.sprintf "  role_name_description = permission_description;\n")
  ^(Printf.sprintf "  role_subroles = []; (* permission cannot have any subroles *)\n")
  ^(Printf.sprintf "  }\n")

let role_label role = replace_char (Printf.sprintf "role_%s" role) '-' '_'
(*let subroles_label role = (Printf.sprintf "subroles_of_%s" (role_label role))*)
let permissions_label role = (Printf.sprintf "permissions_of_%s" (role_label role))

let role_index = ref 0
let writer_role name nroles =
  let role_uuid =
    if name = Datamodel_roles.role_pool_admin
    (* pool-admin role has a fixed uuid because it's the default role in Datamodel subject's roles field *)
    then Constants.rbac_pool_admin_uuid
    (* all the other roles use a hash as uuid *)
    else role_uuid name
  in
  (*let role_ref = role_ref name in*)
  let role_name_label =
    (String.lowercase_ascii name) (* lowercase here asked by GUI team *)
  in
  role_index := !role_index+1;
  let role_number = (Printf.sprintf "%i/%i" !role_index nroles) in
  let role_description =
    try List.assoc role_name_label Datamodel_roles.role_description
    with Not_found ->
      failwith (Printf.sprintf
                  "Check Datamodel_roles.role_description: there's no role description for role %s"
                  role_name_label
               )
  in
  (Printf.sprintf "let %s = \n  { (* %s *)\n" (role_label name) role_number)
  (*^(Printf.sprintf "  role_ref = \"%s\";\n" role_ref)*)
  ^(Printf.sprintf "  role_uuid = \"%s\";\n" role_uuid)
  ^(Printf.sprintf "  role_name_label = \"%s\";\n" role_name_label)
  ^(Printf.sprintf "  role_name_description = \"%s\";\n" role_description)
  ^(Printf.sprintf "  role_subroles = get_refs %s;\n" (permissions_label name))
  ^(Printf.sprintf "  }\n")

(*
let get_ref name =
	List.map (fun p->(*Ref.of_string*) (role_ref name)) permissions
*)

(* the output of this function generates ocaml/autogen/rbac-static.ml *)
let writer_stdout static_roles_permissions static_permissions_roles =
  let nperms = List.length static_permissions_roles in
  let nroles = List.length static_roles_permissions in
  (Printf.sprintf "(* This file contains relations between static roles and permissions *)")
  ^(Printf.sprintf "\n(* Auto-generated from the role flags in the datamodel *)\n\n")
  (* 0. ml header/imports *)
  ^(Printf.sprintf "open Db_actions\n\n")
  (* 1. the static permissions *)
  ^(Printf.sprintf "let permission_description = \"%s\"\n\n" permission_description)
  ^(List.fold_left
      (fun acc (perm,_) -> acc^(writer_permission perm nperms))
      ""
      static_permissions_roles
   )
  (* 2. static_roles<->permissions *)
  ^(List.fold_left
      (fun acc (_role,perms) ->
         (* role's list of permissions *)
         let permissions_label = permissions_label _role in
         (*let subroles_label = subroles_label _role in*)
         acc^
         (Printf.sprintf "(* %i elements in %s *)\n" (List.length perms) permissions_label)
         ^(Printf.sprintf "let %s = [" permissions_label)
         ^(List.fold_left
             (fun acc perm -> (Printf.sprintf "%s; " (permission_name perm))^acc)
             ""
             perms
          )
         ^Printf.sprintf "]\n\n"
         (*			(* role's list of permission refs *)
           			^(Printf.sprintf "(* %i elements in %s *)\n" (List.length perms) subroles_label)
           			^(Printf.sprintf "let %s = [" subroles_label)
           			^(List.fold_left
           				(fun acc perm -> (Printf.sprintf "\"%s\"; " (role_ref perm))^acc)
           				""
           				perms
           			)
           			^Printf.sprintf "]\n\n"
         *)
      )
      ""
      static_roles_permissions
   )
  (* 3. all static permissions *)
  ^("let all_static_permissions = permissions_of_role_pool_admin\n")
  (* 4. list of static roles *)
  ^"let get_refs permissions = List.map (fun p->Ref.of_string (Ref.ref_prefix ^ p.role_uuid)) permissions\n\n"
  ^(List.fold_left
      (fun acc (role,_) -> acc^(writer_role role nroles))
      ""
      static_roles_permissions
   )
  (* 5. all static roles *)
  ^("let all_static_roles = \n[\n")
  ^(List.fold_left
      (fun acc (role,_) -> acc^(Printf.sprintf "  %s;\n" (role_label role)))
      ""
      static_roles_permissions
   )
  ^("]\n")

(* This function maps a string xperm and an extra-str-list into *)
(* a dictionary entry (xperm,extra-str-list::original-str-list), *)
(* and returns the resulting dictionary *)
let rec concat = function
  | (xperm,rs,[]) ->
    let (r1,r2)=(List.partition (fun (r,_)->r=internal_role_local_root) rs) in
    let r,perms = match r1 with []->(internal_role_local_root,[])|r1::_->r1 in
    ((r,xperm::perms)::r2)
  | (xperm,rs,xr::extra_rs) ->
    let (r1,r2)=(List.partition (fun (r,_)->r=xr) rs) in
    let r,perms = match r1 with []->(xr,[])|r1::_->r1 in
    concat (xperm,((r,xperm::perms)::r2),extra_rs)

let get_key_permission_name permission key_name =
  permission ^ "/key:" ^ key_name

let add_permission_to_roles roles_permissions (obj: obj) (x: message) =
  let msg_allowed_roles = x.msg_allowed_roles in
  let msg_map_keys_roles = x.msg_map_keys_roles in
  let wire_name = DU.wire_name ~sync:true obj x in
  match msg_allowed_roles with
  | None -> (
      (*roles_permissions (*<-in case no-role messages are allowed, use this*)*)
      (* a message should have at least one role *)
      failwith (Printf.sprintf "No roles for message %s" wire_name);
    )
  | Some(allowed_roles) ->
    let with_msg_roles_permissions =
      (concat (wire_name,roles_permissions,allowed_roles))
    in
    List.fold_left
      (fun rsps (k,rs)->
         let wire_name_key = get_key_permission_name wire_name k in
         match rs with
         |None->failwith (Printf.sprintf "No roles for key %s" wire_name_key)
         |Some(allowed_roles)->(concat (wire_name_key, rsps, allowed_roles))
      )
      with_msg_roles_permissions
      msg_map_keys_roles

let get_http_permissions_roles =
  List.fold_left
    (fun acc (http_permission,(_,_,_,_,some_roles,sub_actions))-> acc @
                                                                  let open Xapi_stdext_pervasives in
                                                                  let roles = Pervasiveext.default [] some_roles in
                                                                  (Datamodel.rbac_http_permission_prefix ^ http_permission, roles)
                                                                  ::
                                                                  (List.map (* sub_actions for this http_permission *)
                                                                     (fun (sub_action,some_roles)->
                                                                        let roles = Pervasiveext.default [] some_roles in
                                                                        (Datamodel.rbac_http_permission_prefix ^ http_permission
                                                                         ^ "/" ^ sub_action, roles)
                                                                     )
                                                                     sub_actions
                                                                  )
    )
    []
    Datamodel.http_actions

let get_extra_permissions_roles =
  let open Xapi_stdext_pervasives in
  List.map
    (fun (p,rs)->(p,Pervasiveext.default [] rs))
    Datamodel.extra_permissions

(* Returns a (permission, static_role list) list generated from datamodel.ml *)
let gen_roles_of_permissions roles_permissions =
(*
(* Lists all api call names available *)
let apicalls obj =
  let objmsgs obj = List.map (fun msg -> Printf.sprintf "\"%s\";" (DU.wire_name ~sync:true obj msg)) obj.messages in
  let allmsg = List.map (fun obj -> String.concat "" (objmsgs obj)) all_objs in
  allmsg
*)
  let rec _permissions_roles = function
    | (acc,[]) -> acc
    | (acc,(role,permissions)::rps) ->
      _permissions_roles ((concat (role,acc,permissions)),rps)
  in
  (* sort roles in each api-call/permission *)
  let sort_fn a b = (* 0 if equal, + if a>b, - if a<b *)
    (role_idx (a,Datamodel_roles.roles_all))-(role_idx (b,Datamodel_roles.roles_all))
  in
  let permissions_roles =
    (List.map
       (fun (permission,roles) -> (permission,List.sort sort_fn roles))
       (_permissions_roles ([],roles_permissions))
    )
  in
  permissions_roles

(* Returns a (static_role,permission list) list generated from datamodel.ml *)
let gen_permissions_of_static_roles highapi =
  let api = Client.client_api ~sync:true highapi in
  let all_objs = Dm_api.objects_of_api api in

  let rec get_roles_permissions_of_objs = function
    | (acc,[]) -> acc
    | (acc,obj::objs) ->
      begin
        let rec get_roles_permissions_of_obj_msgs = function
          | (acc,[]) -> acc
          | (acc,msg::msgs) ->
            get_roles_permissions_of_obj_msgs
              ((add_permission_to_roles acc obj msg),msgs)
        in
        get_roles_permissions_of_objs
          ((get_roles_permissions_of_obj_msgs (acc,obj.messages)),objs)
      end
  in
  let api_roles_permissions =
    (get_roles_permissions_of_objs ([],all_objs)) (*api*)
  in
  let roles_permissions = (*api+http+extra*)
    List.rev
      (List.fold_left
         (List.fold_left (fun arps (hr,hps) -> (concat (hr,arps,hps))))
         api_roles_permissions
         [get_http_permissions_roles;get_extra_permissions_roles]
      )
  in

  let _permissions_roles = gen_roles_of_permissions roles_permissions in
  let _,permissions_roles = (* ignore the _local_root_ permission *)
    List.partition (fun (r,_)->r=internal_role_local_root) _permissions_roles
  in

  if !Gen_server.enable_debugging
  then begin (* for rbac_static.csv *)
    writer_csv roles_permissions permissions_roles
  end
  else begin (* for rbac_static.ml *)
    let _,roles_permissions = (* ignore the _local_root_ internal role *)
      List.partition (fun (r,_)->r=internal_role_local_root) roles_permissions
    in
    writer_stdout roles_permissions permissions_roles
  end


