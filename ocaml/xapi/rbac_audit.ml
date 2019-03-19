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
module Audit = Debug.Make(struct let name="audit" end)
module D = Debug.Make(struct let name="rbac_audit" end)

(* Rbac Audit fields:

    * Already existing fields in debug records:
          o $timestamp
          o type ('info')
          o $hostname
          o $xapi thread number
          o $xapi task name/id
          o 'audit' (the record key indicating an API call audit record)

    * Extra RBAC-specific fields for _side-effecting_ API calls (CP-706) as s-exprs
      (ie. read-only calls to DB fields are not currently logged):
          o $session's trackid
          o $session's subject_identifier (when available)
          o $session's username (when available)
          o ('DENIED'|'ALLOWED')
          o ('OK'|'ERROR:'$exception/error code field)
          o $call type ('api'|'http')
          o $api/http call name [for http-level operations, at least import/export,host/pool-backup,guest/host-console-access]
          o $important-call-parameters (eg. vm_uuid, host_uuid), as s-exprs
                + human-readable names for each important-call-parameters (eg. vm_name, host_name)
*)

let trackid session_id = (Context.trackid_of_session (Some session_id))

open Db_actions
open Db_filter_types


let is_http action =
  Stdext.Xstringext.String.startswith Datamodel.rbac_http_permission_prefix action

let call_type_of ~action =
  if is_http action then "HTTP" else "API"

let str_local_session = "LOCAL_SESSION"
let str_local_superuser = "LOCAL_SUPERUSER"


let get_subject_common ~__context ~session_id ~fnname
    ~fn_if_local_session ~fn_if_local_superuser ~fn_if_subject =
  try
    if Session_check.is_local_session __context session_id
    then (fn_if_local_session ())
    else
    if (DB_Action.Session.get_is_local_superuser ~__context ~self:session_id)
    then (fn_if_local_superuser ())
    else (fn_if_subject ())
  with
  | e -> begin
      D.debug "error %s for %s:%s"
        fnname (trackid session_id) (ExnHelper.string_of_exn e);
      "" (* default value returned after an internal error *)
    end

let get_subject_identifier __context session_id =
  get_subject_common ~__context ~session_id
    ~fnname:"get_subject_identifier"
    ~fn_if_local_session:(fun()->str_local_session)
    ~fn_if_local_superuser:(fun()->str_local_superuser)
    ~fn_if_subject:(
      fun()->DB_Action.Session.get_auth_user_sid ~__context ~self:session_id
    )

let get_subject_name __context session_id =
  get_subject_common ~__context ~session_id
    ~fnname:"get_subject_name"
    ~fn_if_local_session:(fun()->
        (* we are in emergency mode here, do not call DB_Action:
           			 - local sessions are not in the normal DB
           			 - local sessions do not have a username field
           			 - DB_Action will block forever trying to access an inaccessible master
           			*)
        ""
      )
    ~fn_if_local_superuser:(fun()->
        DB_Action.Session.get_auth_user_name ~__context ~self:session_id
      )
    ~fn_if_subject:(fun()->
        DB_Action.Session.get_auth_user_name ~__context ~self:session_id
      )

(*given a ref-value, return a human-friendly value associated with that ref*)
let get_obj_of_ref_common obj_ref fn =
  let indexrec = Ref_index.lookup obj_ref in
  match indexrec with
  | None -> None
  | Some indexrec -> fn indexrec

let get_obj_of_ref obj_ref =
  get_obj_of_ref_common obj_ref
    (fun irec -> Some(irec.Ref_index.name_label, irec.Ref_index.uuid, irec.Ref_index._ref))

let get_obj_name_of_ref obj_ref =
  get_obj_of_ref_common obj_ref (fun irec -> irec.Ref_index.name_label)

let get_obj_uuid_of_ref obj_ref =
  get_obj_of_ref_common obj_ref (fun irec -> Some(irec.Ref_index.uuid))

let get_obj_ref_of_ref obj_ref =
  get_obj_of_ref_common obj_ref (fun irec -> Some(irec.Ref_index._ref))

let get_sexpr_arg name name_of_ref uuid_of_ref ref_value : SExpr.t =
  SExpr.Node
    ( (* s-expr lib should properly escape malicious values *)
      (SExpr.String name)::
      (SExpr.String name_of_ref)::
      (SExpr.String uuid_of_ref)::
      (SExpr.String ref_value)::
      []
    )

(* given a list of (name,'',ref-value) triplets, *)
(* map '' -> friendly-value of ref-value. *)
(* used on the master to map missing reference names from slaves *)
let get_obj_names_of_refs (obj_ref_list : SExpr.t list) : SExpr.t list=
  List.map
    (fun obj_ref ->
       match obj_ref with
       |SExpr.Node (SExpr.String name::SExpr.String ""::
                    SExpr.String ""::SExpr.String ref_value::[]) ->
         get_sexpr_arg
           name
           (match (get_obj_name_of_ref ref_value) with
            | None -> "" (* ref_value is not a ref! *)
            | Some obj_name -> obj_name (* the missing name *)
           )
           (match (get_obj_uuid_of_ref ref_value) with
            | None -> "" (* ref_value is not a ref! *)
            | Some obj_uuid -> obj_uuid (* the missing uuid *)
           )
           ref_value
       |_->obj_ref (* do nothing if not a triplet *)
    )
    obj_ref_list

(* unwrap the audit record and add names to the arg refs *)
(* this is necessary because we can only obtain the ref names *)
(* on the master, and http audit records can come from slaves *)
let populate_audit_record_with_obj_names_of_refs line =
  try
    let sexpr_idx = (String.index line ']') + 1 in
    let before_sexpr_str = String.sub line 0 sexpr_idx in
    (* remove the [...] prefix *)
    let sexpr_str = Stdext.Xstringext.String.sub_to_end line sexpr_idx in
    let sexpr = SExpr_TS.of_string sexpr_str	in
    match sexpr with
    |SExpr.Node els -> begin
        if List.length els = 0
        then line
        else
          let (args:SExpr.t) = List.hd (List.rev els) in
          (match List.partition (fun (e:SExpr.t) ->e<>args) els with
           |prefix, ((SExpr.Node arg_list)::[]) ->
             (* paste together the prefix of original audit record *)
             before_sexpr_str^" "^
             (SExpr.string_of
                (SExpr.Node (
                    prefix@
                    ((SExpr.Node (get_obj_names_of_refs arg_list))::
                     [])
                  ))
             )
           |prefix,_->line
          )
      end
    |_->line
  with e ->
    D.debug "error populating audit record arg names: %s"
      (ExnHelper.string_of_exn e)
    ;
    line

let action_params_whitelist =
  [ (* manual params asked by audit report team *)
    ("host.create",["hostname";"address";"external_auth_type";"external_auth_service_name";"edition"]);
    ("VM.create",["is_a_template";"memory_target";"memory_static_max";"memory_dynamic_max";"memory_dynamic_min";"memory_static_min";"ha_always_run";"ha_restart_priority"]);
    ("host.set_address",["value"]);
    ("VM.migrate",["dest";"live"]);
    ("VM.start_on",["start_paused";"force"]);
    ("VM.start",["start_paused";"force"]);
    ("pool.create_VLAN",["device";"vLAN"]);
    ("pool.join_force",["master_address"]);
    ("pool.join",["master_address"]);
    ("pool.enable_external_auth",["service_name";"auth_type"]);
    ("host.enable_external_auth",["service_name";"auth_type"]);
    ("subject.create",["subject_identifier";"other_config"]);
    ("subject.create.other_config",["subject-name"]);
    (* used for VMPP alert logs *)
    ("message.create",["name";"body"]);
    ("VMPP.create_alert",["name";"data"]);
  ]

let action_params_zip =
  [ (* params that should be compressed *)
    ("VMPP.create_alert",["data"]);
  ]

let zip data = (* todo: remove i/o, make this more efficient *)
  try
    let tmp_path = Filename.temp_file "zip-" ".dat" in
    let zdata = ref (Bytes.empty) in
    Stdext.Pervasiveext.finally
      (fun ()->
         Stdext.Unixext.atomic_write_to_file tmp_path 0o600
           (fun fd ->
              Gzip.compress fd (fun fd ->
                  let len = String.length data in
                  let written = Unix.write_substring fd data 0 len in
                  if written <> len then failwith (Printf.sprintf "zip: wrote only %i bytes of %i" written len)
                )
           );
         let fd_in = Unix.openfile tmp_path [ Unix.O_RDONLY] 0o400 in
         Stdext.Pervasiveext.finally
           (fun ()->
              let cin = Unix.in_channel_of_descr fd_in in
              let cin_len = in_channel_length cin in
              zdata := (Bytes.create cin_len);
              for i = 1 to cin_len do
                Bytes.set !zdata (i-1) (input_char cin);
              done
           )
           (fun ()-> Unix.close fd_in)
      )
      (fun ()-> Sys.remove tmp_path)
    ;
    let b64zdata = Base64.encode_string (Bytes.unsafe_to_string !zdata) in
    b64zdata
  with e->
    D.debug "error %s zipping data: %s" (ExnHelper.string_of_exn e) data;
    ""

(* manual ref getters *)
let get_subject_other_config_subject_name __context self =
  try
    List.assoc
      "subject-name"
      (DB_Action.Subject.get_other_config ~__context ~self:(Ref.of_string self))
  with e ->
    D.debug "couldn't get Subject.other_config.subject-name for ref %s: %s" self (ExnHelper.string_of_exn e);
    ""

let get_role_name_label __context self =
  try
    (*Xapi_role.get_name_label ~__context ~self:(Ref.of_string self)*)
    (*DB_Action.Role.get_name_label ~__context ~self:(Ref.of_string self)*)
    let ps=(Rbac_static.all_static_roles@Rbac_static.all_static_permissions) in
    let p=List.find (fun p->Ref.ref_prefix^p.role_uuid=self) ps in
    p.role_name_label
  with e ->
    D.debug "couldn't get Role.name_label for ref %s: %s" self (ExnHelper.string_of_exn e);
    ""

let action_param_ref_getter_fn =
  [ (* manual override on ref getters *)
    ("subject.destroy",["self",(fun _ctx _ref->get_subject_other_config_subject_name _ctx _ref)]);
    ("subject.remove_from_roles",["self",(fun _ctx _ref->get_subject_other_config_subject_name _ctx _ref);"role",(fun _ctx _ref->get_role_name_label _ctx _ref)]);
    ("subject.add_to_roles",["self",(fun _ctx _ref->get_subject_other_config_subject_name _ctx _ref);"role",(fun _ctx _ref->get_role_name_label _ctx _ref)]);
  ]

(* get a namevalue directly from db, instead from db_cache *)
let get_db_namevalue __context name action _ref =
  if List.mem_assoc action action_param_ref_getter_fn
  then (
    let params=List.assoc action action_param_ref_getter_fn in
    if List.mem_assoc name params then (
      let getter_fn=List.assoc name params in
      getter_fn __context _ref
    )
    else
      "" (* default value empty *)
  )
  else
    "" (* default value empty *)

(* Map selected xapi call arguments into audit sexpr arguments.
    Not all parameters are mapped into audit log arguments because
    some, like passwords, are sensitive and should not be persisted
    into the audit log. Use heuristics to map non-sensitive parameters.
*)
let rec sexpr_args_of __context name rpc_value action =
  let is_selected_action_param action_params =
    (if List.mem_assoc action action_params
     then
       let params=List.assoc action action_params in
       List.mem name params
     else
       false
    )
  in
  (* heuristic 1: print descriptive arguments in the xapi call *)
  if (List.mem name ["name";"label";"description";"name_label";"name_description";"new_name"]) (* param for any action *) || (is_selected_action_param action_params_whitelist) (* action+param pair *)
  then
    ( match rpc_value with
      | Rpc.String value ->
        Some (get_sexpr_arg
                name
                (if is_selected_action_param action_params_zip
                 then (zip value)
                 else value
                )
                "" ""
             )
      | Rpc.Dict _ ->
        Some (SExpr.Node
                (
                  (SExpr.String name)
                  ::(SExpr.Node (sexpr_of_parameters __context (action^"."^name) (Some (["__structure"],[rpc_value]))))
                  ::(SExpr.String "")
                  ::(SExpr.String "")
                  ::[]
                )
             )
      | _-> (*D.debug "sexpr_args_of:value=%s" (Xml.to_string xml_value);*)
        (*None*)
        Some (get_sexpr_arg name (Rpc.to_string rpc_value) "" "")
    )
  else
    (* heuristic 2: print uuid/refs arguments in the xapi call *)
    match rpc_value with
    | Rpc.String value -> (
        let name_uuid_ref = get_obj_of_ref value in
        match name_uuid_ref with
        | None ->
          if Stdext.Xstringext.String.startswith Ref.ref_prefix value
          then (* it's a ref, just not in the db cache *)
            Some (get_sexpr_arg name (get_db_namevalue __context name action value) "" value)
          else (* ignore values that are not a ref *)
            None
        | Some(_name_of_ref_value, uuid_of_ref_value, ref_of_ref_value) ->
          let name_of_ref_value = (match _name_of_ref_value with|None->(get_db_namevalue __context name action ref_of_ref_value)|Some ""->(get_db_namevalue __context name action ref_of_ref_value)|Some a -> a) in
          Some (get_sexpr_arg name name_of_ref_value uuid_of_ref_value ref_of_ref_value)
      )
    |_-> None

and
  (* Given an action and its parameters, *)
  (* return the marshalled uuid params and corresponding names *)
  (*let rec*) sexpr_of_parameters __context action args : SExpr.t list =
  match args with
  | None -> []
  | Some (str_names,rpc_values) ->
    begin
      if (List.length str_names) <> (List.length rpc_values)
      then
        ( (* debug mode *)
          D.warn "cannot marshall arguments for the action %s: name and value list lengths don't match. str_names=[%s], xml_values=[%s]" action ((List.fold_left (fun ss s->ss^s^",") "" str_names)) ((List.fold_left (fun ss s->ss^(Rpc.to_string s)^",") "" rpc_values));
          []
        )
      else
        List.fold_right2
          (fun str_name rpc_value (params:SExpr.t list) ->
             if str_name = "session_id"
             then params (* ignore session_id param *)
             else
               (* if it is a constructor structure, need to rewrap params *)
             if str_name = "__structure"
             then match rpc_value with
               | Rpc.Dict d ->
                 let names = List.map fst d in
                 let values = List.map snd d in
                 let myparam = sexpr_of_parameters __context action (Some (names,values)) in
                 myparam@params
               | rpc_value ->
                 (match (sexpr_args_of __context str_name rpc_value action)
                  with None->params|Some p->p::params
                 )
             else
               (* the expected list of xml arguments *)
               begin
                 (match (sexpr_args_of __context str_name rpc_value action)
                  with None->params|Some p->p::params
                 )
               end
          )
          str_names
          rpc_values
          []
    end

let has_to_audit action =
  let has_side_effect action =
    not (Stdext.Xstringext.String.has_substr action ".get") (* TODO: a bit slow? *)
  in
  (!Xapi_globs.log_getter || (has_side_effect action))
  &&
  not ( (* these actions are ignored *)
    List.mem action
      [ (* list of _actions_ filtered out from the audit log *)
        "session.local_logout"; (* session logout have their own *)
        "session.logout"; (* rbac_audit calls, because after logout *)
        (* the session is destroyed and no audit is possible*)
        "event.next"; (* this action is just spam in the audit log*)
        "event.from"; (* spam *)
        "http/get_rrd_updates"; (* spam *)
        "http/post_remote_db_access"; (* spam *)
        "host.tickle_heartbeat"; (* spam *)
      ]
  )

let wrap fn =
  try fn ()
  with e -> (* never bubble up the error here *)
    D.debug "ignoring %s" (ExnHelper.string_of_exn e)

(* Extra info required for the WLB audit report. *)
let add_dummy_args __context action args =
  match args with
  | None -> args
  | Some (str_names, rpc_values) -> begin
      let rec find_self str_names rpc_values =
        match str_names, rpc_values with
        | ("self"::_), (rpc::_) -> rpc
        | (_::str_names'), (_::rpc_values') -> find_self str_names' rpc_values'
        | _, _ -> raise Not_found
      in
      match action with
      (* Add VDI info for VBD.destroy *)
      | "VBD.destroy" -> begin
          try
            let vbd = API.ref_VBD_of_rpc (find_self str_names rpc_values) in
            let vdi = DB_Action.VBD.get_VDI __context vbd in
            Some (str_names@["VDI"], rpc_values@[API.rpc_of_ref_VDI vdi])
          with e ->
            D.debug "couldn't get VDI ref for VBD: %s" (ExnHelper.string_of_exn e);
            args
        end
      | _ -> args
    end

let sexpr_of __context session_id allowed_denied ok_error result_error ?args ?sexpr_of_args action permission =
  let result_error =
    if result_error = "" then result_error else ":"^result_error
  in
  (*let (params:SExpr.t list) = (string_of_parameters action args) in*)
  SExpr.Node (
    SExpr.String (trackid session_id)::
    SExpr.String (get_subject_identifier __context session_id)::
    SExpr.String (get_subject_name __context session_id)::
    SExpr.String (allowed_denied)::
    SExpr.String (ok_error ^ result_error)::
    SExpr.String (call_type_of action)::
    (*SExpr.String (Helper_hostname.get_hostname ())::*)
    SExpr.String action::
    (SExpr.Node (
        match sexpr_of_args with
        | None -> (let args' = add_dummy_args __context action args in sexpr_of_parameters __context action args')
        | Some sexpr_of_args -> sexpr_of_args
      ))::
    []
  )

let append_line = Audit.audit

let fn_append_to_master_audit_log = ref None

let audit_line_of __context session_id allowed_denied ok_error result_error action permission ?args ?sexpr_of_args () =
  let _line =
    (SExpr.string_of
       (sexpr_of __context session_id allowed_denied
          ok_error result_error ?args ?sexpr_of_args action permission
       )
    )
  in
  let line = Stdext.Xstringext.String.replace "\n" " " _line in (* no \n in line *)
  let line = Stdext.Xstringext.String.replace "\r" " " line in (* no \r in line *)

  let audit_line = append_line "%s" line in
  (*D.debug "line=%s, audit_line=%s" line audit_line;*)
  match !fn_append_to_master_audit_log with
  | None -> ()
  | Some fn -> fn __context action audit_line

let allowed_pre_fn ~__context ~action ?args () =
  try
    if (has_to_audit action)
    (* for now, we only cache arg results for destroy actions *)
    && (Stdext.Xstringext.String.has_substr action ".destroy")
    then let args' = add_dummy_args __context action args in Some(sexpr_of_parameters __context action args')
    else None
  with e ->
    D.debug "ignoring %s" (ExnHelper.string_of_exn e);
    None

let allowed_post_fn_ok ~__context ~session_id ~action ~permission ?sexpr_of_args ?args ?result () =
  wrap (fun () ->
      if has_to_audit action then
        audit_line_of __context session_id "ALLOWED" "OK" "" action permission ?sexpr_of_args ?args ()
    )

let allowed_post_fn_error ~__context ~session_id ~action ~permission ?sexpr_of_args ?args ?error () =
  wrap (fun () ->
      if has_to_audit action then
        let error_str =
          match error with
          | None -> ""
          | Some error -> (ExnHelper.string_of_exn error)
        in
        audit_line_of __context session_id "ALLOWED" "ERROR" error_str action permission ?sexpr_of_args  ?args ()
    )

let denied ~__context ~session_id ~action ~permission ?args () =
  wrap (fun () ->
      if has_to_audit action then
        audit_line_of __context session_id "DENIED" "" "" action permission ?args ()
    )

let session_create_or_destroy ~create ~__context ~session_id ~uname =
  wrap (fun () ->
      let session_rec = DB_Action.Session.get_record ~__context ~self:session_id in
      let s_is_intrapool = session_rec.API.session_pool in
      let s_is_lsu = session_rec.API.session_is_local_superuser in
      (* filters out intra-pool logins to avoid spamming the audit log *)
      if (not s_is_intrapool) && (not s_is_lsu) then (
        let action = (if create then "session.create" else "session.destroy") in
        let originator = session_rec.API.session_originator in
        let sexpr_of_args =
          (get_sexpr_arg "originator" originator "" "")::
          []
        in
        let sexpr_of_args =
          if create then
            (get_sexpr_arg "uname" (match uname with None->""|Some u->u) "" "")::
            sexpr_of_args
          else
            sexpr_of_args
        in
        allowed_post_fn_ok ~__context ~session_id ~action ~sexpr_of_args ~permission:action ()
      )
    )

let session_destroy ~__context ~session_id =
  session_create_or_destroy ~uname:None ~create:false ~__context ~session_id

let session_create ~__context ~session_id ~uname =
  session_create_or_destroy ~create:true ~__context ~session_id ~uname
