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
(** Data Model and Message Specification for Xen Management Tools *)

open Datamodel_types
open Datamodel_common
open Datamodel_roles

let errors = Datamodel_errors.errors
let messages = Datamodel_errors.messages
let roles_all = roles_all

let api_version_major = Datamodel_common.api_version_major
let api_version_minor = Datamodel_common.api_version_minor


module Session = struct
  let login  = call ~flags:[]
      ~name:"login_with_password"
      ~in_product_since:rel_rio
      ~doc:"Attempt to authenticate the user, returning a session reference if successful"
      ~result:(Ref _session,"reference of newly created session")
      ~versioned_params:
        [{param_type=String; param_name="uname"; param_doc="Username for login."; param_release=rio_release; param_default=None};
         {param_type=String; param_name="pwd"; param_doc="Password for login."; param_release=rio_release; param_default=None};
         {param_type=String; param_name="version"; param_doc="Client API version."; param_release=miami_release; param_default=Some (VString "1.1")};
         {param_type=String; param_name="originator"; param_doc="Key string for distinguishing different API users sharing the same login name."; param_release=clearwater_release; param_default=Some (VString "")}
        ]
      ~errs:[Api_errors.session_authentication_failed; Api_errors.host_is_slave]
      ~secret:true
      ~allowed_roles:_R_ALL (*any static role can try to create a user session*)
      ()

  let slave_login  = call ~flags:[]
      ~name:"slave_login"
      ~doc:"Attempt to authenticate to the pool master by presenting the slave's host ref and pool secret"
      ~result:(Ref _session,"ID of newly created session")
      ~params:[
        Ref _host, "host", "Host id of slave";
        String, "psecret", "Pool secret"
      ]
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~secret:true
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_ADMIN (*system can create a slave session !!! *)
      ()

  let slave_local_login = call ~flags:[]
      ~in_product_since:rel_miami
      ~name:"slave_local_login"
      ~doc:"Authenticate locally against a slave in emergency mode. Note the resulting sessions are only good for use on this host."
      ~result:(Ref _session,"ID of newly created session")
      ~params:[
        String, "psecret", "Pool secret"
      ]
      ~in_oss_since:None
      ~secret:true
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_ADMIN (*system can create a slave session*)
      ()

  let slave_local_login_with_password = call ~flags:[]
      ~in_product_since:rel_miami
      ~name:"slave_local_login_with_password"
      ~doc:"Authenticate locally against a slave in emergency mode. Note the resulting sessions are only good for use on this host."
      ~result:(Ref _session,"ID of newly created session")
      ~params:[
        String, "uname", "Username for login.";
        String, "pwd", "Password for login.";
      ]
      ~in_oss_since:None
      ~secret:true
      ~allowed_roles:_R_POOL_ADMIN (*only root can do an emergency slave login*)
      ()

  let create_from_db_file = call
      ~lifecycle:[Published, rel_dundee, ""]
      ~name:"create_from_db_file"
      ~params:[String, "filename", "Database dump filename."]
      ~result:(Ref _session, "ID of newly created session")
      ~in_oss_since:None
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let local_logout = call ~flags:[`Session]
      ~in_product_since:rel_miami
      ~name:"local_logout"
      ~doc:"Log out of local session."
      ~params:[]
      ~in_oss_since:None
      ~allowed_roles:_R_POOL_ADMIN (*system can destroy a local session*)
      ()

  let logout = call ~flags:[`Session]
      ~in_product_since:rel_rio
      ~name:"logout"
      ~doc:"Log out of a session"
      ~params:[]
      ~allowed_roles:_R_ALL (*any role can destroy a known user session*)
      ()

  let change_password = call ~flags:[`Session]
      ~name:"change_password"
      ~doc:"Change the account password; if your session is authenticated with root priviledges then the old_pwd is validated and the new_pwd is set regardless"
      ~params:[
        String, "old_pwd", "Old password for account";
        String, "new_pwd", "New password for account"
      ]
      ~in_product_since:rel_rio
      ~in_oss_since:None
      ~allowed_roles:_R_LOCAL_ROOT_ONLY (*not even pool-admin can change passwords, only root*)
      ()

  let get_all_subject_identifiers = call
      ~name:"get_all_subject_identifiers"
      ~doc:"Return a list of all the user subject-identifiers of all existing sessions"
      ~result:(Set (String), "The list of user subject-identifiers of all existing sessions")
      ~params:[]
      ~in_product_since:rel_george
      ~in_oss_since:None
      ~allowed_roles:_R_ALL
      ()

  let logout_subject_identifier = call
      ~name:"logout_subject_identifier"
      ~doc:"Log out all sessions associated to a user subject-identifier, except the session associated with the context calling this function"
      ~params:[
        String, "subject_identifier", "User subject-identifier of the sessions to be destroyed"
      ]
      ~in_product_since:rel_george
      ~in_oss_since:None
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_session ~descr:"A session" ~gen_events:false
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages:[
        login;
        logout;
        change_password;
        slave_login;
        slave_local_login;
        slave_local_login_with_password;
        create_from_db_file;
        local_logout;
        get_all_subject_identifiers;
        logout_subject_identifier;
      ] ~contents:[
      uid _session;
      field ~qualifier:DynamicRO ~ty:(Ref _host)
        "this_host" "Currently connected host";
      field ~qualifier:DynamicRO ~ty:(Ref _user)
        "this_user" "Currently connected user";
      field ~qualifier:DynamicRO ~ty:DateTime
        "last_active" "Timestamp for last time session was active";
      field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None
        "pool" "True if this session relates to a intra-pool login, false otherwise";
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      field ~in_product_since:rel_george ~qualifier:DynamicRO ~default_value:(Some (VBool false)) ~ty:Bool "is_local_superuser" "true iff this session was created using local superuser credentials";
      field ~in_product_since:rel_george ~qualifier:DynamicRO ~default_value:(Some (VRef null_ref)) ~ty:(Ref _subject) "subject" "references the subject instance that created the session. If a session instance has is_local_superuser set, then the value of this field is undefined.";
      field ~in_product_since:rel_george ~qualifier:DynamicRO ~default_value:(Some(VDateTime(Date.of_float 0.))) ~ty:DateTime "validation_time" "time when session was last validated";
      field ~in_product_since:rel_george ~qualifier:DynamicRO ~default_value:(Some(VString(""))) ~ty:String "auth_user_sid" "the subject identifier of the user that was externally authenticated. If a session instance has is_local_superuser set, then the value of this field is undefined.";
      field ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~default_value:(Some(VString(""))) ~ty:String "auth_user_name" "the subject name of the user that was externally authenticated. If a session instance has is_local_superuser set, then the value of this field is undefined.";
      field ~in_product_since:rel_midnight_ride ~qualifier:StaticRO ~default_value:(Some(VSet [])) ~ty:(Set(String)) "rbac_permissions" "list with all RBAC permissions for this session";
      field ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:(Set(Ref _task)) "tasks" "list of tasks created using the current session";
      field ~in_product_since:rel_midnight_ride ~qualifier:StaticRO ~default_value:(Some (VRef null_ref)) ~ty:(Ref _session) "parent" "references the parent session that created this session";
      field ~in_product_since:rel_clearwater ~qualifier:DynamicRO ~default_value:(Some(VString(""))) ~ty:String  "originator" "a key string provided by a API user to distinguish itself from other users sharing the same login name";
    ]
      ()
end


module Task = struct
  (* NB: the status 'cancelling' is not being used, nor should it ever be used. It should be purged from here! *)
  let status_type = Enum("task_status_type", [ "pending", "task is in progress";
                                               "success", "task was completed successfully";
                                               "failure", "task has failed";
                                               "cancelling", "task is being cancelled";
                                               "cancelled", "task has been cancelled";
                                             ])


  let cancel = call

      ~name:"cancel"
      ~in_product_since:rel_rio
      ~doc:"Request that a task be cancelled. Note that a task may fail to be cancelled and may complete or fail normally and note that, even when a task does cancel, it might take an arbitrary amount of time."
      ~params:[Ref _task, "task", "The task"]
      ~errs:[Api_errors.operation_not_allowed]
      ~allowed_roles:_R_READ_ONLY (* POOL_OP can cancel any tasks, others can cancel only owned tasks *)
      ()


  let create = call ~flags:[`Session]
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~name:"create"
      ~doc:"Create a new task object which must be manually destroyed."
      ~params:[String, "label", "short label for the new task";
               String, "description", "longer description for the new task"]
      ~result:(Ref _task, "The reference of the created task object")
      ~allowed_roles:_R_READ_ONLY (* any subject can create tasks *)
      ()

  let destroy = call ~flags:[`Session]
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~name:"destroy"
      ~doc:"Destroy the task object"
      ~params:[Ref _task, "self", "Reference to the task object"]
      ~allowed_roles:_R_READ_ONLY (* POOL_OP can destroy any tasks, others can destroy only owned tasks *)
      ()

  let set_status = call ~flags:[`Session]
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~name:"set_status"
      ~doc:"Set the task status"
      ~params:[Ref _task, "self", "Reference to the task object";
               status_type, "value", "task status value to be set"]
      ~allowed_roles:_R_READ_ONLY (* POOL_OP can set status for any tasks, others can set status only for owned tasks *)
      ()

  (* this permission allows to destroy any task, instead of only the owned ones *)
  let extra_permission_task_destroy_any = "task.destroy/any"

  let task_allowed_operations =
    Enum ("task_allowed_operations", List.map operation_enum [ cancel; destroy ])

  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_task ~descr:"A long-running asynchronous task" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages: [
        create;
        destroy;
        cancel;
        set_status ]
      ~contents: ([
          uid _task;
          namespace ~name:"name" ~contents:(names oss_since_303 DynamicRO) ();
        ] @ (allowed_and_current_operations task_allowed_operations) @ [
            field ~qualifier:DynamicRO ~ty:DateTime "created" "Time task was created";
            field ~qualifier:DynamicRO ~ty:DateTime "finished" "Time task finished (i.e. succeeded or failed). If task-status is pending, then the value of this field has no meaning";
            field ~qualifier:DynamicRO ~ty:status_type "status" "current status of the task";
            field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _session) "session" "the session that created the task";
            field ~qualifier:DynamicRO ~ty:(Ref _host) "resident_on" "the host on which the task is running";
            field ~qualifier:DynamicRO ~ty:Float "progress" "This field contains the estimated fraction of the task which is complete. This field should not be used to determine whether the task is complete - for this the status field of the task should be used.";
            field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Int "externalpid" "If the task has spawned a program, the field record the PID of the process that the task is waiting on. (-1 if no waiting completion of an external program )";
            field ~in_oss_since:None ~internal_deprecated_since:rel_boston ~internal_only:true ~qualifier:DynamicRO ~ty:Int "stunnelpid" "If the task has been forwarded, this field records the pid of the stunnel process spawned to manage the forwarding connection";
            field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Bool "forwarded" "True if this task has been forwarded to a slave";
            field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _host) "forwarded_to" "The host to which the task has been forwarded";
            field ~qualifier:DynamicRO ~ty:String "type" "if the task has completed successfully, this field contains the type of the encoded result (i.e. name of the class whose reference is in the result field). Undefined otherwise.";
            field ~qualifier:DynamicRO ~ty:String "result" "if the task has completed successfully, this field contains the result value (either Void or an object reference). Undefined otherwise.";
            field ~qualifier:DynamicRO ~ty:(Set String) "error_info" "if the task has failed, this field contains the set of associated error strings. Undefined otherwise.";
            field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("applies_to",(_R_VM_OP));("XenCenterUUID",(_R_VM_OP));("XenCenterMeddlingActionTitle",(_R_VM_OP))];
            (* field ~ty:(Set(Ref _alert)) ~in_product_since:rel_miami ~qualifier:DynamicRO "alerts" "all alerts related to this task"; *)
            field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VRef "")) ~ty:(Ref _task) "subtask_of" "Ref pointing to the task this is a substask of.";
            field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Set (Ref _task)) "subtasks"   "List pointing to all the substasks.";
            field ~qualifier:DynamicRO ~in_product_since:rel_dundee ~ty:String ~default_value:(Some (VString (Sexplib.Sexp.to_string (Backtrace.(sexp_of_t empty))))) "backtrace" "Function call trace for debugging.";
          ])
      ()
end

(** Many of the objects need to record IO bandwidth *)
let iobandwidth =
  let msg = "Disabled and replaced by RRDs" in
  [ field ~persist:false ~qualifier:DynamicRO ~ty:Float "read_kbs" "Read bandwidth (KiB/s)"
      ~lifecycle:[ Removed, rel_tampa, msg]
  ; field ~persist:false ~qualifier:DynamicRO ~ty:Float "write_kbs" "Write bandwidth (KiB/s)"
      ~lifecycle:[ Removed, rel_tampa, msg]
  ]

(** Human users *)
module User = struct
  let t = (* DEPRECATED in favor of subject *)
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_user ~descr:"A user of the system" ~gen_events:false
      ~lifecycle:[
        Published, rel_rio, "A user of the system";
        Deprecated, rel_george, "Deprecated in favor of subject";
      ]
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages:[] ~contents:
      [ uid _user;
        field ~qualifier:StaticRO "short_name" "short name (e.g. userid)";
        field "fullname" "full name";
        field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
      ()
end




(* Management of host crash dumps. Note that this would be neater if crashes were stored in
   VDIs like VM crashes, however the nature of a host crash dump is that the dom0 has crashed
   and has no access to any fancy storage drivers or tools. Plus a host is not guaranteed to
   have any SRs at all. *)

module Host_crashdump = struct
  let destroy = call
      ~name:"destroy"
      ~doc:"Destroy specified host crash dump, removing it from the disk."
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[ Ref _host_crashdump, "self", "The host crashdump to destroy" ]
      ~allowed_roles:_R_POOL_OP
      ()

  let upload = call
      ~name:"upload"
      ~doc:"Upload the specified host crash dump to a specified URL"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[ Ref _host_crashdump, "self", "The host crashdump to upload";
                String, "url", "The URL to upload to";
                Map(String, String), "options", "Extra configuration operations" ]
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host_crashdump ~gen_events:true
      ~descr:"Represents a host crash dump"
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages: [destroy; upload]
      ~contents:
        [ uid ~in_oss_since:None _host_crashdump;
          field ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _host) "host" "Host the crashdump relates to";
          field ~in_oss_since:None ~qualifier:DynamicRO ~ty:DateTime "timestamp" "Time the crash happened";
          field ~in_oss_since:None ~qualifier:DynamicRO ~ty:Int "size" "Size of the crashdump";
          field ~qualifier:StaticRO ~ty:String ~in_oss_since:None ~internal_only:true "filename" "filename of crash dir";
          field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
        ]
      ()
end

(* New Ely pool update mechanism *)
module Pool_update = struct

  let livepatch_status =
    Enum ("livepatch_status",
          [
            "ok_livepatch_complete", "An applicable live patch exists for every required component";
            "ok_livepatch_incomplete", "An applicable live patch exists but it is not sufficient";
            "ok", "There is no applicable live patch"
          ])

  let after_apply_guidance =
    Enum ("update_after_apply_guidance",
          [ "restartHVM",  "This update requires HVM guests to be restarted once applied.";
            "restartPV",   "This update requires PV guests to be restarted once applied.";
            "restartHost", "This update requires the host to be restarted once applied.";
            "restartXAPI", "This update requires XAPI to be restarted once applied.";
          ])

  let introduce = call
      ~name:"introduce"
      ~doc:"Introduce update VDI"
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _vdi, "vdi", "The VDI which contains a software update." ]
      ~result:(Ref _pool_update, "the introduced pool update")
      ~allowed_roles:_R_POOL_OP
      ()

  let precheck = call
      ~name:"precheck"
      ~doc:"Execute the precheck stage of the selected update on a host"
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _pool_update, "self", "The update whose prechecks will be run"; Ref _host, "host", "The host to run the prechecks on." ]
      ~result:(livepatch_status, "The precheck pool update")
      ~allowed_roles:_R_POOL_OP
      ~forward_to:(HostExtension "pool_update.precheck")
      ()

  let apply = call
      ~name:"apply"
      ~doc:"Apply the selected update to a host"
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _pool_update, "self", "The update to apply"; Ref _host, "host", "The host to apply the update to." ]
      ~allowed_roles:_R_POOL_OP
      ~forward_to:(HostExtension "pool_update.apply")
      ()

  let pool_apply = call
      ~name:"pool_apply"
      ~doc:"Apply the selected update to all hosts in the pool"
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _pool_update, "self", "The update to apply"]
      ~allowed_roles:_R_POOL_OP
      ()

  let pool_clean = call
      ~name:"pool_clean"
      ~doc:"Removes the update's files from all hosts in the pool, but does not revert the update"
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _pool_update, "self", "The update to clean up" ]
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~doc:"Removes the database entry. Only works on unapplied update."
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _pool_update, "self", "The update to destroy" ]
      ~allowed_roles:_R_POOL_OP
      ()

  let attach = call
      ~name:"attach"
      ~hide_from_docs:true
      ~doc:"Attach the pool update VDI"
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _pool_update, "self", "The update to be attached"]
      ~result:(String, "The file URL of pool update")
      ~allowed_roles:_R_POOL_OP
      ()

  let detach = call
      ~name:"detach"
      ~hide_from_docs:true
      ~doc:"Detach the pool update VDI"
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _pool_update, "self", "The update to be detached"]
      ~allowed_roles:_R_POOL_OP
      ()

  let resync_host = call
      ~name:"resync_host"
      ~hide_from_docs:true
      ~doc:"Resync the applied updates of the host"
      ~in_oss_since:None
      ~in_product_since:rel_ely
      ~params:[ Ref _host, "host", "The host to resync the applied updates"]
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj ~in_db:true
      ~in_product_since:rel_ely
      ~in_oss_since:None
      ~internal_deprecated_since:None

      ~persist:PersistEverything
      ~gen_constructor_destructor:false
      ~gen_events:true

      ~name:_pool_update
      ~descr:"Pool-wide updates to the host software"
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[
        introduce;
        precheck;
        apply;
        pool_apply;
        pool_clean;
        destroy;
        attach;
        detach;
        resync_host;
      ]
      ~contents:
        [ uid       ~in_oss_since:None _pool_update;
          namespace ~name:"name" ~contents:(names None StaticRO) ();
          field     ~in_product_since:rel_ely ~default_value:(Some (VString "")) ~in_oss_since:None ~qualifier:StaticRO ~ty:String "version" "Update version number";
          field     ~in_product_since:rel_ely ~default_value:(Some (VInt Int64.zero)) ~in_oss_since:None ~qualifier:StaticRO ~ty:Int "installation_size" "Size of the update in bytes";
          field     ~in_product_since:rel_ely ~default_value:(Some (VString "")) ~in_oss_since:None ~qualifier:StaticRO ~ty:String "key" "GPG key of the update";
          field     ~in_product_since:rel_ely ~default_value:(Some (VSet [])) ~in_oss_since:None ~qualifier:StaticRO ~ty:(Set after_apply_guidance) "after_apply_guidance" "What the client should do after this update has been applied.";
          field     ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _vdi) "vdi" "VDI the update was uploaded to";
          field     ~in_product_since:rel_ely ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host)) "hosts" "The hosts that have applied this update.";
          field     ~in_product_since:rel_inverness
            ~default_value:(Some (VMap []))
            ~in_oss_since:None
            ~ty:(Map(String, String))
            "other_config"
            "additional configuration";
          field     ~in_product_since:rel_inverness
            ~default_value:(Some (VBool false))
            ~in_oss_since:None
            ~qualifier:StaticRO
            ~ty:Bool
            "enforce_homogeneity"
            "Flag - if true, all hosts in a pool must apply this update";
        ]
      ()
end

(* New Miami pool patching mechanism *)

module Pool_patch = struct
  let after_apply_guidance =
    Enum ("after_apply_guidance",
          [ "restartHVM",  "This patch requires HVM guests to be restarted once applied.";
            "restartPV",   "This patch requires PV guests to be restarted once applied.";
            "restartHost", "This patch requires the host to be restarted once applied.";
            "restartXAPI", "This patch requires XAPI to be restarted once applied.";
          ])

  let apply = call
      ~name:"apply"
      ~doc:"Apply the selected patch to a host and return its output"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[ Ref _pool_patch, "self", "The patch to apply"; Ref _host, "host", "The host to apply the patch too" ]
      ~result:(String, "the output of the patch application process")
      ~allowed_roles:_R_POOL_OP
      ~internal_deprecated_since:rel_ely
      ()

  let precheck = call
      ~name:"precheck"
      ~doc:"Execute the precheck stage of the selected patch on a host and return its output"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[ Ref _pool_patch, "self", "The patch whose prechecks will be run"; Ref _host, "host", "The host to run the prechecks on" ]
      ~result:(String, "the output of the patch prechecks")
      ~allowed_roles:_R_POOL_OP
      ~internal_deprecated_since:rel_ely
      ()

  let clean = call
      ~name:"clean"
      ~doc:"Removes the patch's files from the server"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[ Ref _pool_patch, "self", "The patch to clean up" ]
      ~allowed_roles:_R_POOL_OP
      ~internal_deprecated_since:rel_ely
      ()

  let clean_on_host = call
      ~name:"clean_on_host"
      ~doc:"Removes the patch's files from the specified host"
      ~in_oss_since:None
      ~in_product_since:rel_tampa
      ~params:[ Ref _pool_patch, "self", "The patch to clean up"; Ref _host, "host", "The host on which to clean the patch"  ]
      ~allowed_roles:_R_POOL_OP
      ~internal_deprecated_since:rel_ely
      ()

  let pool_clean = call
      ~name:"pool_clean"
      ~doc:"Removes the patch's files from all hosts in the pool, but does not remove the database entries"
      ~in_oss_since:None
      ~in_product_since:rel_tampa
      ~params:[ Ref _pool_patch, "self", "The patch to clean up" ]
      ~allowed_roles:_R_POOL_OP
      ~internal_deprecated_since:rel_ely
      ()

  let destroy = call
      ~name:"destroy"
      ~doc:"Removes the patch's files from all hosts in the pool, and removes the database entries.  Only works on unapplied patches."
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[ Ref _pool_patch, "self", "The patch to destroy" ]
      ~allowed_roles:_R_POOL_OP
      ~internal_deprecated_since:rel_ely
      ()

  let pool_apply = call
      ~name:"pool_apply"
      ~doc:"Apply the selected patch to all hosts in the pool and return a map of host_ref -> patch output"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[ Ref _pool_patch, "self", "The patch to apply"]
      ~allowed_roles:_R_POOL_OP
      ~internal_deprecated_since:rel_ely
      ()

  let t =
    create_obj ~in_db:true
      ~in_product_since:rel_miami
      ~in_oss_since:None
      ~internal_deprecated_since:(Some rel_ely)

      ~persist:PersistEverything
      ~gen_constructor_destructor:false
      ~gen_events:true

      ~name:_pool_patch
      ~descr:"Pool-wide patches"
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[
        apply;
        pool_apply;
        precheck;
        clean;
        pool_clean;
        destroy;
        clean_on_host;
      ]
      ~contents:
        [ uid       ~in_oss_since:None _pool_patch;
          namespace ~name:"name" ~contents:(names None StaticRO) ();
          field     ~in_product_since:rel_miami ~default_value:(Some (VString "")) ~in_oss_since:None ~qualifier:StaticRO ~ty:String "version" "Patch version number";
          field     ~in_product_since:rel_miami ~default_value:(Some (VString "")) ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:String "filename" "Filename of the patch";
          field     ~in_product_since:rel_miami ~default_value:(Some (VInt Int64.zero)) ~in_oss_since:None ~qualifier:DynamicRO ~ty:Int "size" "Size of the patch";
          field     ~in_product_since:rel_miami ~default_value:(Some (VBool false)) ~in_oss_since:None ~qualifier:DynamicRO ~ty:Bool "pool_applied" "This patch should be applied across the entire pool";
          field     ~in_product_since:rel_miami ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host_patch)) "host_patches" "This hosts this patch is applied to.";
          field     ~in_product_since:rel_miami ~default_value:(Some (VSet [])) ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set after_apply_guidance) "after_apply_guidance" "What the client should do after this patch has been applied.";
          field     ~in_product_since:rel_ely   ~default_value:(Some (VRef null_ref)) ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _pool_update) "pool_update" "A reference to the associated pool_update object";
          field     ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~in_oss_since:None  ~ty:(Map(String, String)) "other_config" "additional configuration";
        ]
      ()
end

(* Management of host patches. Just like the crash dumps it would be marginally neater if
   the patches were stored as VDIs. *)

module Host_patch = struct
  let destroy = call
      ~name:"destroy"
      ~doc:"Destroy the specified host patch, removing it from the disk. This does NOT reverse the patch"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[ Ref _host_patch, "self", "The patch to destroy" ]
      ~internal_deprecated_since: rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let apply = call
      ~name:"apply"
      ~doc:"Apply the selected patch and return its output"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[ Ref _host_patch, "self", "The patch to apply" ]
      ~result:(String, "the output of the patch application process")
      ~internal_deprecated_since: rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:(Some rel_ely) ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host_patch ~gen_events:true
      ~descr:"Represents a patch stored on a server"
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages: [
        destroy;
        apply;
      ]
      ~contents:
        [ uid ~in_oss_since:None _host_patch;
          namespace ~name:"name" ~contents:(names None StaticRO) ();
          field ~in_oss_since:None ~qualifier:StaticRO ~ty:String "version" "Patch version number";
          field ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _host) "host" "Host the patch relates to";
          field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:String "filename" "Filename of the patch";
          field ~in_oss_since:None ~qualifier:DynamicRO ~ty:Bool "applied" "True if the patch has been applied";
          field ~in_oss_since:None ~qualifier:DynamicRO ~ty:DateTime "timestamp_applied" "Time the patch was applied";
          field ~in_oss_since:None ~qualifier:DynamicRO ~ty:Int "size" "Size of the patch";
          field ~in_product_since:rel_miami ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _pool_patch) ~default_value:(Some (VRef "")) "pool_patch" "The patch applied";
          field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~in_oss_since:None  ~ty:(Map(String, String)) "other_config" "additional configuration";
        ]
      ()
end


module Host_metrics = struct

let host_metrics_memory =
  let field = field ~ty:Int in
  [
    field ~qualifier:DynamicRO "total" "Total host memory (bytes)" ~doc_tags:[Memory];
    field "free" "Free host memory (bytes)"
      ~lifecycle:
        [ Deprecated, rel_midnight_ride, "Will be disabled in favour of RRD"
        ; Removed, rel_tampa, "Disabled in favour of RRD"
        ]
      ~qualifier:DynamicRO
      ~doc_tags:[Memory];
  ]

  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host_metrics ~descr:"The metrics associated with a host" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[] ~contents:
      [ uid _host_metrics;
        namespace ~name:"memory" ~contents:host_metrics_memory ();
        field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None "live" "Pool master thinks this host is live";
        field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
        field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
      ()
end

(** HostCPU *)

module Host_cpu = struct
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_hostcpu ~descr:"A physical CPU" ~gen_events:true
      ~lifecycle:[
        Published, rel_rio, "A physical CPU";
        Deprecated, rel_midnight_ride, "Deprecated in favour of the Host.cpu_info field";
      ]
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[] ~contents:
      [ uid _hostcpu;
        field ~qualifier:DynamicRO ~ty:(Ref _host) "host" "the host the CPU is in";
        field ~qualifier:DynamicRO ~ty:Int "number" "the number of the physical CPU within the host";
        field ~qualifier:DynamicRO ~ty:String "vendor" "the vendor of the physical CPU";
        field ~qualifier:DynamicRO ~ty:Int "speed" "the speed of the physical CPU";
        field ~qualifier:DynamicRO ~ty:String "modelname" "the model name of the physical CPU";
        field ~qualifier:DynamicRO ~ty:Int "family" "the family (number) of the physical CPU";
        field ~qualifier:DynamicRO ~ty:Int "model" "the model number of the physical CPU";
        field ~qualifier:DynamicRO ~ty:String "stepping" "the stepping of the physical CPU";
        field ~qualifier:DynamicRO ~ty:String "flags" "the flags of the physical CPU (a decoded version of the features field)";
        field ~qualifier:DynamicRO ~ty:String "features" "the physical CPU feature bitmap";
        field ~qualifier:DynamicRO ~persist:false ~ty:Float "utilisation" "the current CPU utilisation";
        field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
      ()
end

(** Disk and network interfaces are associated with QoS parameters: *)
let qos devtype =
  [ field  "algorithm_type" "QoS algorithm to use";
    field  ~ty:(Map(String,String)) "algorithm_params"
      "parameters for chosen QoS algorithm";
    field ~qualifier:DynamicRO  ~ty:(Set String) "supported_algorithms"
      ("supported QoS algorithms for this " ^ devtype);
  ]

module Network = struct

  let operations =
    Enum ("network_operations",
          [ "attaching", "Indicates this network is attaching to a VIF or PIF" ])

  let default_locking_mode =
    Enum ("network_default_locking_mode", [
        "unlocked", "Treat all VIFs on this network with locking_mode = 'default' as if they have locking_mode = 'unlocked'";
        "disabled", "Treat all VIFs on this network with locking_mode = 'default' as if they have locking_mode = 'disabled'";
      ])


  let attach = call
      ~name:"attach"
      ~doc:"Makes the network immediately available on a particular host"
      ~params:[Ref _network, "network", "network to which this interface should be connected";
               Ref _host, "host", "physical machine to which this PIF is connected"]
      ~in_product_since:rel_miami
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_OP
      ()

  let purpose = Enum ("network_purpose", [
      "nbd", "Network Block Device service using TLS";
      "insecure_nbd", "Network Block Device service without integrity or confidentiality: NOT RECOMMENDED";
      (* We should (re-)add other purposes as and when we write code with behaviour that depends on them,
       * e.g. management, storage, guest, himn... unmanaged? *)
    ])

  let introduce_params first_rel =
    [
      {param_type=String; param_name="name_label"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=String; param_name="name_description"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Int; param_name="MTU"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Map(String,String); param_name="other_config"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=String; param_name="bridge"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Bool; param_name="managed"; param_doc=""; param_release=falcon_release; param_default=None};
      {param_type=Set(purpose); param_name="purpose"; param_doc=""; param_release=inverness_release; param_default=None};
    ]

  (* network pool introduce is used to copy network records on pool join -- it's the network analogue of VDI/PIF.pool_introduce *)
  let pool_introduce = call
      ~name:"pool_introduce"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~versioned_params:(introduce_params miami_release)
      ~doc:"Create a new network record in the database only"
      ~result:(Ref _network, "The ref of the newly created network record.")
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_OP
      ()

  let create_new_blob = call
      ~name: "create_new_blob"
      ~in_product_since:rel_orlando
      ~doc:"Create a placeholder for a named binary blob of data that is associated with this pool"
      ~versioned_params:
        [{param_type=Ref _network; param_name="network"; param_doc="The network"; param_release=orlando_release; param_default=None};
         {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
         {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
         {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
        ]
      ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
      ~allowed_roles:_R_POOL_OP
      ()

  let set_default_locking_mode = call
      ~name:"set_default_locking_mode"
      ~in_product_since:rel_tampa
      ~doc:"Set the default locking mode for VIFs attached to this network"
      ~params:[
        Ref _network, "network", "The network";
        default_locking_mode, "value", "The default locking mode for VIFs attached to this network.";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let attach_for_vm = call
      ~name:"attach_for_vm"
      ~doc:"Attaches all networks needed by a given VM on a particular host"
      ~params:[
        Ref _host, "host", "Physical machine to which the networks are to be attached";
        Ref _vm, "vm", "The virtual machine"
      ]
      ~in_product_since:rel_tampa
      ~hide_from_docs:true
      ~allowed_roles:_R_VM_POWER_ADMIN
      ()

  let detach_for_vm = call
      ~name:"detach_for_vm"
      ~doc:"Detaches all networks of a given VM from a particular host"
      ~params:[
        Ref _host, "host", "Physical machine from which the networks are to be attached";
        Ref _vm, "vm", "The virtual machine"
      ]
      ~in_product_since:rel_tampa
      ~hide_from_docs:true
      ~allowed_roles:_R_VM_POWER_ADMIN
      ()

  let add_purpose = call
      ~name:"add_purpose"
      ~doc:"Give a network a new purpose (if not present already)"
      ~params:[
        Ref _network, "self", "The network";
        purpose, "value", "The purpose to add";
      ]
      ~errs:[Api_errors.network_incompatible_purposes]
      ~in_product_since:rel_inverness
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let remove_purpose = call
      ~name:"remove_purpose"
      ~doc:"Remove a purpose from a network (if present)"
      ~params:[
        Ref _network, "self", "The network";
        purpose, "value", "The purpose to remove";
      ]
      ~in_product_since:rel_inverness
      ~allowed_roles:_R_POOL_ADMIN
      ()

  (** A virtual network *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_network ~descr:"A virtual network" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN (* vm admins can create/destroy networks without PIFs *)
      ~doc_tags:[Networking]
      ~messages:[attach; pool_introduce; create_new_blob; set_default_locking_mode;
                 attach_for_vm; detach_for_vm; add_purpose; remove_purpose]
      ~contents:
        ([
          uid _network;
          namespace ~name:"name" ~contents:(names ~writer_roles:_R_POOL_OP oss_since_303 RW) ();
        ] @ (allowed_and_current_operations ~writer_roles:_R_POOL_OP operations) @ [
            field ~qualifier:DynamicRO ~ty:(Set (Ref _vif)) "VIFs" "list of connected vifs";
            field ~qualifier:DynamicRO ~ty:(Set (Ref _pif)) "PIFs" "list of connected pifs";
            field ~qualifier:RW ~ty:Int ~default_value:(Some (VInt 1500L)) ~in_product_since:rel_midnight_ride "MTU" "MTU in octets";
            field ~writer_roles:_R_POOL_OP ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP));("XenCenterCreateInProgress",(_R_VM_OP))];
            field ~lifecycle:[Published, rel_rio, ""; Changed, rel_falcon, "Added to the constructor (network.create)"] ~in_oss_since:None ~qualifier:StaticRO  ~ty:String ~default_value:(Some (VString "")) "bridge" "name of the bridge corresponding to this network on the local host";
            field ~lifecycle:[Published, rel_falcon, ""] ~qualifier:StaticRO ~ty:Bool ~default_value:(Some (VBool true)) "managed" "true if the bridge is managed by xapi";
            field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this network";
            field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
            field ~qualifier:DynamicRO ~in_product_since:rel_tampa ~default_value:(Some (VEnum "unlocked")) ~ty:default_locking_mode "default_locking_mode" "The network will use this value to determine the behaviour of all VIFs where locking_mode = default";
            field ~qualifier:DynamicRO ~in_product_since:rel_creedence ~default_value:(Some (VMap [])) ~ty:(Map (Ref _vif, String)) "assigned_ips" "The IP addresses assigned to VIFs on networks that have active xapi-managed DHCP";
            field ~qualifier:DynamicRO ~in_product_since:rel_inverness ~default_value:(Some (VSet [])) ~ty:(Set purpose) "purpose" "Set of purposes for which the server will use this network";
          ])
      ()
end

module PIF = struct
  let create_VLAN = call
      ~name:"create_VLAN"
      ~in_product_since:rel_rio
      ~doc:"Create a VLAN interface from an existing physical interface. This call is deprecated: use VLAN.create instead"
      ~lifecycle:[
        Published, rel_rio, "Create a VLAN interface from an existing physical interface";
        Deprecated, rel_miami, "Replaced by VLAN.create";
      ]
      ~params:[String, "device", "physical interface on which to create the VLAN interface";
               Ref _network, "network", "network to which this interface should be connected";
               Ref _host, "host", "physical machine to which this PIF is connected";
               Int, "VLAN", "VLAN tag for the new interface"]
      ~result:(Ref _pif, "The reference of the created PIF object")
      ~errs:[Api_errors.vlan_tag_invalid]
      ~internal_deprecated_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~in_product_since:rel_rio
      ~doc:"Destroy the PIF object (provided it is a VLAN interface). This call is deprecated: use VLAN.destroy or Bond.destroy instead"
      ~lifecycle:[
        Published, rel_rio, "Destroy the PIF object (provided it is a VLAN interface)";
        Deprecated, rel_miami, "Replaced by VLAN.destroy and Bond.destroy";
      ]
      ~params:[Ref _pif, "self", "the PIF object to destroy"]
      ~errs:[Api_errors.pif_is_physical]
      ~internal_deprecated_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let plug = call
      ~name:"plug"
      ~doc:"Attempt to bring up a physical interface"
      ~params:[Ref _pif, "self", "the PIF object to plug"]
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ~errs:[Api_errors.transport_pif_not_configured]
      ()

  let unplug = call
      ~name:"unplug"
      ~doc:"Attempt to bring down a physical interface"
      ~params:[Ref _pif, "self", "the PIF object to unplug"]
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ~errs:[Api_errors.ha_operation_would_break_failover_plan;
             Api_errors.vif_in_use;
             Api_errors.pif_does_not_allow_unplug;
             Api_errors.pif_has_fcoe_sr_in_use]
      ()

  let ip_configuration_mode = Enum ("ip_configuration_mode",
                                    [ "None", "Do not acquire an IP address";
                                      "DHCP", "Acquire an IP address by DHCP";
                                      "Static", "Static IP address configuration" ])

  let reconfigure_ip = call
      ~name:"reconfigure_ip"
      ~doc:"Reconfigure the IP address settings for this interface"
      ~params:[Ref _pif, "self", "the PIF object to reconfigure";
               ip_configuration_mode, "mode", "whether to use dynamic/static/no-assignment";
               String, "IP", "the new IP address";
               String, "netmask", "the new netmask";
               String, "gateway", "the new gateway";
               String, "DNS", "the new DNS settings";
              ]
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let ipv6_configuration_mode = Enum ("ipv6_configuration_mode",
                                      [ "None", "Do not acquire an IPv6 address";
                                        "DHCP", "Acquire an IPv6 address by DHCP";
                                        "Static", "Static IPv6 address configuration";
                                        "Autoconf", "Router assigned prefix delegation IPv6 allocation" ])

  let reconfigure_ipv6 = call
      ~name:"reconfigure_ipv6"
      ~doc:"Reconfigure the IPv6 address settings for this interface"
      ~params:[Ref _pif, "self", "the PIF object to reconfigure";
               ipv6_configuration_mode, "mode", "whether to use dynamic/static/no-assignment";
               String, "IPv6", "the new IPv6 address (in <addr>/<prefix length> format)";
               String, "gateway", "the new gateway";
               String, "DNS", "the new DNS settings";
              ]
      ~lifecycle:[Prototyped, rel_tampa, ""]
      ~allowed_roles:_R_POOL_OP
      ()

  let primary_address_type = Enum ("primary_address_type",
                                   [ "IPv4", "Primary address is the IPv4 address";
                                     "IPv6", "Primary address is the IPv6 address" ])

  let set_primary_address_type = call
      ~name:"set_primary_address_type"
      ~doc:"Change the primary address type used by this PIF"
      ~params:[Ref _pif, "self", "the PIF object to reconfigure";
               primary_address_type, "primary_address_type", "Whether to prefer IPv4 or IPv6 connections";
              ]
      ~lifecycle:[Prototyped, rel_tampa, ""]
      ~allowed_roles:_R_POOL_OP
      ()

  let scan = call
      ~name:"scan"
      ~doc:"Scan for physical interfaces on a host and create PIF objects to represent them"
      ~params:[Ref _host, "host", "The host on which to scan"]
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let introduce_params =
    [
      {param_type=Ref _host; param_name="host"; param_doc="The host on which the interface exists"; param_release=miami_release; param_default=None};
      {param_type=String; param_name="MAC"; param_doc="The MAC address of the interface"; param_release=miami_release; param_default=None};
      {param_type=String; param_name="device"; param_doc="The device name to use for the interface"; param_release=miami_release; param_default=None};
      {param_type=Bool; param_name="managed"; param_doc="Indicates whether the interface is managed by xapi (defaults to \"true\")"; param_release=vgpu_productisation_release; param_default=Some (VBool true)};
    ]

  let introduce = call
      ~name:"introduce"
      ~doc:"Create a PIF object matching a particular network interface"
      ~versioned_params:introduce_params
      ~in_product_since:rel_miami
      ~result:(Ref _pif, "The reference of the created PIF object")
      ~allowed_roles:_R_POOL_OP
      ()

  let forget = call
      ~name:"forget"
      ~doc:"Destroy the PIF object matching a particular network interface"
      ~params:[Ref _pif, "self", "The PIF object to destroy"]
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ~errs:[Api_errors.pif_tunnel_still_exists]
      ()

  let pool_introduce_params first_rel =
    [
      {param_type=String; param_name="device"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Ref _network; param_name="network"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Ref _host; param_name="host"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=String; param_name="MAC"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Int; param_name="MTU"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Int; param_name="VLAN"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Bool; param_name="physical"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=ip_configuration_mode; param_name="ip_configuration_mode"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=String; param_name="IP"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=String; param_name="netmask"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=String; param_name="gateway"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=String; param_name="DNS"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Ref _bond; param_name="bond_slave_of"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Ref _vlan; param_name="VLAN_master_of"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Bool; param_name="management"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Map(String, String); param_name="other_config"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Bool; param_name="disallow_unplug"; param_doc=""; param_release=orlando_release; param_default=Some (VBool false)};
      {param_type=ipv6_configuration_mode; param_name="ipv6_configuration_mode"; param_doc=""; param_release=boston_release; param_default=Some (VEnum "None")};
      {param_type=(Set(String)); param_name="IPv6"; param_doc=""; param_release=boston_release; param_default=Some (VSet [])};
      {param_type=String; param_name="ipv6_gateway"; param_doc=""; param_release=boston_release; param_default=Some (VString "")};
      {param_type=primary_address_type; param_name="primary_address_type"; param_doc=""; param_release=boston_release; param_default=Some (VEnum "IPv4")};
      {param_type=Bool; param_name="managed"; param_doc=""; param_release=vgpu_productisation_release; param_default=Some (VBool true)};
      {param_type=Map(String, String); param_name="properties"; param_doc=""; param_release=creedence_release; param_default=Some (VMap [])};
    ]

  (* PIF pool introduce is used to copy PIF records on pool join -- it's the PIF analogue of VDI.pool_introduce *)
  let pool_introduce = call
      ~name:"pool_introduce"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~versioned_params:(pool_introduce_params miami_release)
      ~doc:"Create a new PIF record in the database only"
      ~result:(Ref _pif, "The ref of the newly created PIF record.")
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_OP
      ()

  let db_introduce = call
      ~name:"db_introduce"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~versioned_params:(pool_introduce_params orlando_release)
      ~doc:"Create a new PIF record in the database only"
      ~result:(Ref _pif, "The ref of the newly created PIF record.")
      ~hide_from_docs:false
      ~allowed_roles:_R_POOL_OP
      ()

  let db_forget = call
      ~name:"db_forget"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~params:[ Ref _pif, "self", "The ref of the PIF whose database record should be destroyed" ]
      ~doc:"Destroy a PIF database record."
      ~hide_from_docs:false
      ~allowed_roles:_R_POOL_OP
      ()

  let set_property = call
      ~name:"set_property"
      ~doc:"Set the value of a property of the PIF"
      ~params:[
        Ref _pif, "self", "The PIF";
        String, "name", "The property name";
        String, "value", "The property value";
      ]
      ~lifecycle:[Published, rel_creedence, ""]
      ~allowed_roles:_R_POOL_OP
      ()

  let igmp_status =
    Enum ("pif_igmp_status", [
        "enabled", "IGMP Snooping is enabled in the corresponding backend bridge.'";
        "disabled", "IGMP Snooping is disabled in the corresponding backend bridge.'";
        "unknown", "IGMP snooping status is unknown. If this is a VLAN master, then please consult the underlying VLAN slave PIF."
      ])

  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_pif ~descr:"A physical network interface (note separate VLANs are represented as several PIFs)"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~doc_tags:[Networking]
      ~messages:[
        create_VLAN;
        destroy;
        reconfigure_ip;
        reconfigure_ipv6;
        set_primary_address_type;
        scan;
        introduce;
        forget;
        unplug;
        plug;
        pool_introduce;
        db_introduce;
        db_forget;
        set_property
      ]
      ~contents:[ uid _pif;
                  (* qualifier changed RW -> StaticRO in Miami *)
                  field ~qualifier:StaticRO "device" "machine-readable name of the interface (e.g. eth0)";
                  field ~qualifier:StaticRO ~ty:(Ref _network) "network" "virtual network to which this pif is connected";
                  field ~qualifier:StaticRO ~ty:(Ref _host) "host" "physical machine to which this pif is connected";
                  (* qualifier changed RW -> StaticRO in Miami *)
                  field ~qualifier:StaticRO "MAC" "ethernet MAC address of physical interface";
                  (* qualifier changed RW -> StaticRO in Miami *)
                  field ~qualifier:StaticRO ~ty:Int "MTU" "MTU in octets";
                  (* qualifier changed RW -> StaticRO in Miami *)
                  field ~qualifier:StaticRO ~ty:Int "VLAN" "VLAN tag for all traffic passing through this interface";
                  field ~in_oss_since:None ~internal_only:true "device_name" "actual dom0 device name";
                  field ~qualifier:DynamicRO ~ty:(Ref _pif_metrics) "metrics" "metrics associated with this PIF";
                  field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "physical" "true if this represents a physical network interface" ~default_value:(Some (VBool false));
                  field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "currently_attached" "true if this interface is online" ~default_value:(Some (VBool true));
                  field ~in_oss_since:None ~ty:ip_configuration_mode ~in_product_since:rel_miami ~qualifier:DynamicRO "ip_configuration_mode" "Sets if and how this interface gets an IP address" ~default_value:(Some (VEnum "None"));
                  field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "IP" "IP address" ~default_value:(Some (VString ""));
                  field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "netmask" "IP netmask" ~default_value:(Some (VString ""));
                  field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "gateway" "IP gateway" ~default_value:(Some (VString ""));
                  field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "DNS" "IP address of DNS servers to use" ~default_value:(Some (VString ""));
                  field ~in_oss_since:None ~ty:(Ref _bond) ~in_product_since:rel_miami ~qualifier:DynamicRO "bond_slave_of" "Indicates which bond this interface is part of" ~default_value:(Some (VRef ""));
                  field ~in_oss_since:None ~ty:(Set(Ref _bond)) ~in_product_since:rel_miami ~qualifier:DynamicRO "bond_master_of" "Indicates this PIF represents the results of a bond";
                  field ~in_oss_since:None ~ty:(Ref _vlan) ~in_product_since:rel_miami ~qualifier:DynamicRO "VLAN_master_of" "Indicates wich VLAN this interface receives untagged traffic from" ~default_value:(Some (VRef ""));
                  field ~in_oss_since:None ~ty:(Set(Ref _vlan)) ~in_product_since:rel_miami ~qualifier:DynamicRO "VLAN_slave_of" "Indicates which VLANs this interface transmits tagged traffic to";
                  field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "management" "Indicates whether the control software is listening for connections on this interface" ~default_value:(Some (VBool false));
                  field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "Additional configuration";
                  field ~in_product_since:rel_orlando ~default_value:(Some (VBool false)) ~ty:Bool "disallow_unplug" "Prevent this PIF from being unplugged; set this to notify the management tool-stack that the PIF has a special use and should not be unplugged under any circumstances (e.g. because you're running storage traffic over it)";
                  field ~in_oss_since:None ~ty:(Set(Ref _tunnel)) ~lifecycle:[Published, rel_cowley, "Indicates to which tunnel this PIF gives access"] ~qualifier:DynamicRO "tunnel_access_PIF_of" "Indicates to which tunnel this PIF gives access";
                  field ~in_oss_since:None ~ty:(Set(Ref _tunnel)) ~lifecycle:[Published, rel_cowley, "Indicates to which tunnel this PIF provides transport"] ~qualifier:DynamicRO "tunnel_transport_PIF_of" "Indicates to which tunnel this PIF provides transport";
                  field ~in_oss_since:None ~ty:ipv6_configuration_mode ~lifecycle:[Prototyped, rel_tampa, ""] ~qualifier:DynamicRO "ipv6_configuration_mode" "Sets if and how this interface gets an IPv6 address" ~default_value:(Some (VEnum "None"));
                  field ~in_oss_since:None ~ty:(Set(String)) ~lifecycle:[Prototyped, rel_tampa, ""] ~qualifier:DynamicRO "IPv6" "IPv6 address" ~default_value:(Some (VSet []));
                  field ~in_oss_since:None ~ty:String ~lifecycle:[Prototyped, rel_tampa, ""] ~qualifier:DynamicRO "ipv6_gateway" "IPv6 gateway" ~default_value:(Some (VString ""));
                  field ~in_oss_since:None ~ty:primary_address_type ~lifecycle:[Prototyped, rel_tampa, ""] ~qualifier:DynamicRO "primary_address_type" "Which protocol should define the primary address of this interface" ~default_value:(Some (VEnum "IPv4"));
                  field ~in_oss_since:None ~ty:Bool ~lifecycle:[Published, rel_vgpu_productisation, ""] ~qualifier:StaticRO "managed"
                    "Indicates whether the interface is managed by xapi. If \
                     it is not, then xapi will not configure the interface, \
                     the commands PIF.plug/unplug/reconfigure_ip(v6) \
                     can not be used, nor can the interface be bonded or have \
                     VLANs based on top through xapi." ~default_value:(Some (VBool true));
                  field ~lifecycle:[Published, rel_creedence, ""] ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "properties" "Additional configuration properties for the interface.";
                  field ~lifecycle:[Published, rel_dundee, ""] ~qualifier:DynamicRO ~ty:(Set(String)) ~default_value:(Some (VSet [])) "capabilities" "Additional capabilities on the interface.";
                  field ~lifecycle:[Published, rel_inverness, ""] ~qualifier:DynamicRO ~ty:igmp_status ~default_value:(Some (VEnum "unknown")) "igmp_snooping_status" "The IGMP snooping status of the corresponding network bridge";
                  field ~in_oss_since:None ~ty:(Set (Ref _network_sriov)) ~in_product_since:rel_kolkata ~qualifier:DynamicRO "sriov_physical_PIF_of" "Indicates which network_sriov this interface is physical of";
                  field ~in_oss_since:None ~ty:(Set (Ref _network_sriov)) ~in_product_since:rel_kolkata ~qualifier:DynamicRO "sriov_logical_PIF_of" "Indicates which network_sriov this interface is logical of";
                  field ~qualifier:DynamicRO ~ty:(Ref _pci) ~lifecycle:[Published, rel_kolkata, ""] ~default_value:(Some (VRef null_ref)) "PCI" "Link to underlying PCI device";
                ]
      ()
end

module PIF_metrics = struct
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_pif_metrics ~descr:"The metrics associated with a physical network interface"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~doc_tags:[Networking]
      ~messages:[] ~contents:
      [ uid _pif_metrics;
        namespace ~name:"io" ~contents:iobandwidth ();
        field ~qualifier:DynamicRO ~ty:Bool "carrier" "Report if the PIF got a carrier or not";
        field ~qualifier:DynamicRO ~ty:String "vendor_id" "Report vendor ID";
        field ~qualifier:DynamicRO ~ty:String "vendor_name" "Report vendor name";
        field ~qualifier:DynamicRO ~ty:String "device_id" "Report device ID";
        field ~qualifier:DynamicRO ~ty:String "device_name" "Report device name";
        field ~qualifier:DynamicRO ~ty:Int "speed" "Speed of the link (if available)";
        field ~qualifier:DynamicRO ~ty:Bool "duplex" "Full duplex capability of the link (if available)";
        field ~qualifier:DynamicRO ~ty:String "pci_bus_path" "PCI bus path of the pif (if available)";
        field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
        field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
      ()
end

module Bond = struct

  let mode =
    Enum ("bond_mode", [
        "balance-slb", "Source-level balancing";
        "active-backup", "Active/passive bonding: only one NIC is carrying traffic";
        "lacp", "Link aggregation control protocol";
      ])

  let create = call
      ~name:"create"
      ~doc:"Create an interface bond"
      ~versioned_params:[
        {param_type=Ref _network; param_name="network"; param_doc="Network to add the bonded PIF to"; param_release=miami_release; param_default=None};
        {param_type=Set (Ref _pif); param_name="members"; param_doc="PIFs to add to this bond"; param_release=miami_release; param_default=None};
        {param_type=String; param_name="MAC"; param_doc="The MAC address to use on the bond itself. If this parameter is the empty string then the bond will inherit its MAC address from the primary slave."; param_release=miami_release; param_default=None};
        {param_type=mode; param_name="mode"; param_doc="Bonding mode to use for the new bond"; param_release=boston_release; param_default=Some (VEnum "balance-slb")};
        {param_type=Map (String, String); param_name="properties"; param_doc="Additional configuration parameters specific to the bond mode"; param_release=tampa_release; param_default=Some (VMap [])};
      ]
      ~result:(Ref _bond, "The reference of the created Bond object")
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~doc:"Destroy an interface bond"
      ~params:[Ref _bond, "self", "Bond to destroy"]
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let set_mode = call
      ~name:"set_mode"
      ~doc:"Change the bond mode"
      ~params:[
        Ref _bond, "self", "The bond";
        mode, "value", "The new bond mode";
      ]
      ~lifecycle:[Published, rel_boston, ""]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_property = call
      ~name:"set_property"
      ~doc:"Set the value of a property of the bond"
      ~params:[
        Ref _bond, "self", "The bond";
        String, "name", "The property name";
        String, "value", "The property value";
      ]
      ~in_product_since:rel_tampa
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj ~in_db:true ~in_product_since:rel_miami ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_bond ~descr:"" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~doc_tags:[Networking]
      ~messages:[
        create;
        destroy;
        set_mode;
        set_property;
      ]
      ~contents:
        [ uid _bond;
          field ~in_oss_since:None ~in_product_since:rel_miami ~qualifier:StaticRO ~ty:(Ref _pif) "master" "The bonded interface" ~default_value:(Some (VRef ""));
          field ~in_oss_since:None ~in_product_since:rel_miami ~qualifier:DynamicRO ~ty:(Set(Ref _pif)) "slaves" "The interfaces which are part of this bond";
          field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
          field ~lifecycle:[Published, rel_boston, ""] ~qualifier:DynamicRO ~default_value:(Some (VRef null_ref)) ~ty:(Ref _pif) "primary_slave" "The PIF of which the IP configuration and MAC were copied to the bond, and which will receive all configuration/VLANs/VIFs on the bond if the bond is destroyed";
          field ~lifecycle:[Published, rel_boston, ""] ~qualifier:DynamicRO ~default_value:(Some (VEnum "balance-slb")) ~ty:mode "mode" "The algorithm used to distribute traffic among the bonded NICs";
          field ~in_oss_since:None ~in_product_since:rel_tampa ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "properties" "Additional configuration properties specific to the bond mode.";
          field ~in_oss_since:None ~in_product_since:rel_tampa ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "links_up" "Number of links up in this bond";
        ]
      ()
end

module VLAN = struct

  let introduce_params first_rel =
    [
      {param_type=Ref _pif; param_name="tagged_PIF"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Ref _pif; param_name="untagged_PIF"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Int; param_name="tag"; param_doc=""; param_release=first_rel; param_default=None};
      {param_type=Map(String, String); param_name="other_config"; param_doc=""; param_release=first_rel; param_default=None};
    ]

  (* vlan pool introduce is used to copy management vlan record on pool join -- it's the vlan analogue of VDI/PIF.pool_introduce *)
  let pool_introduce = call
      ~name:"pool_introduce"
      ~in_oss_since:None
      ~in_product_since:rel_inverness
      ~versioned_params:(introduce_params inverness_release)
      ~doc:"Create a new vlan record in the database only"
      ~result:(Ref _vlan, "The reference of the created VLAN object")
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_OP
      ()

  let create = call
      ~name:"create"
      ~doc:"Create a VLAN mux/demuxer"
      ~params:[ Ref _pif, "tagged_PIF", "PIF which receives the tagged traffic";
                Int, "tag", "VLAN tag to use";
                Ref _network, "network", "Network to receive the untagged traffic" ]
      ~result:(Ref _vlan, "The reference of the created VLAN object")
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~doc:"Destroy a VLAN mux/demuxer"
      ~params:[Ref _vlan, "self", "VLAN mux/demuxer to destroy"]
      ~in_product_since:rel_miami
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj ~in_db:true ~in_product_since:rel_miami ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_vlan ~descr:"A VLAN mux/demux" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~doc_tags:[Networking]
      ~messages:[
        pool_introduce;
        create;
        destroy
      ]
      ~contents:[
        uid _vlan;
        field ~qualifier:StaticRO ~ty:(Ref _pif) ~in_product_since:rel_miami "tagged_PIF" "interface on which traffic is tagged" ~default_value:(Some (VRef ""));
        field ~qualifier:DynamicRO ~ty:(Ref _pif) ~in_product_since:rel_miami "untagged_PIF" "interface on which traffic is untagged" ~default_value:(Some (VRef ""));
        field ~qualifier:StaticRO ~ty:Int ~in_product_since:rel_miami "tag" "VLAN tag in use" ~default_value:(Some (VInt (-1L)));
        field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
      ()
end

module Tunnel = struct

  let create = call
      ~name:"create"
      ~doc:"Create a tunnel"
      ~params:[ Ref _pif, "transport_PIF", "PIF which receives the tagged traffic";
                Ref _network, "network", "Network to receive the tunnelled traffic" ]
      ~result:(Ref _tunnel, "The reference of the created tunnel object")
      ~lifecycle:[Published, rel_cowley, "Create a tunnel"]
      ~allowed_roles:_R_POOL_OP
      ~errs:[Api_errors.openvswitch_not_active; Api_errors.transport_pif_not_configured; Api_errors.is_tunnel_access_pif]
      ()

  let destroy = call
      ~name:"destroy"
      ~doc:"Destroy a tunnel"
      ~params:[Ref _tunnel, "self", "tunnel to destroy"]
      ~lifecycle:[Published, rel_cowley, "Destroy a tunnel"]
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj ~in_db:true ~lifecycle:[Published, rel_cowley, "A tunnel for network traffic"] ~in_oss_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_tunnel ~descr:"A tunnel for network traffic" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~doc_tags:[Networking]
      ~messages:[ create; destroy ]
      ~contents:([
          uid _tunnel ~lifecycle:[Published, rel_cowley, "Unique identifier/object reference"];
          field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle:[Published, rel_cowley, "The interface through which the tunnel is accessed"] "access_PIF" "The interface through which the tunnel is accessed" ~default_value:(Some (VRef ""));
          field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle:[Published, rel_cowley, "The interface used by the tunnel"] "transport_PIF" "The interface used by the tunnel" ~default_value:(Some (VRef ""));
          field ~ty:(Map(String, String)) ~lifecycle:[Published, rel_cowley, "Status information about the tunnel"] "status" "Status information about the tunnel" ~default_value:(Some (VMap [VString "active", VString "false"]));
          field ~lifecycle:[Published, rel_cowley, "Additional configuration"] ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "Additional configuration";
        ])
      ()
end

module PBD = struct
  let plug = call
      ~name:"plug"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~doc:"Activate the specified PBD, causing the referenced SR to be attached and scanned"
      ~params:[Ref _pbd, "self", "The PBD to activate"]
      ~errs:[Api_errors.sr_unknown_driver]
      ~allowed_roles:_R_POOL_OP
      ()

  let unplug = call
      ~name:"unplug"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~doc:"Deactivate the specified PBD, causing the referenced SR to be detached and nolonger scanned"
      ~params:[Ref _pbd, "self", "The PBD to deactivate"]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_device_config = call
      ~name:"set_device_config"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[Ref _pbd, "self", "The PBD to modify";
               Map(String, String), "value", "The new value of the PBD's device_config"]
      ~doc:"Sets the PBD's device_config field"
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_pbd ~descr:"The physical block devices through which hosts access SRs"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[
        plug;
        unplug;
        set_device_config
      ] ~contents:
      [ uid _pbd;
        field ~qualifier:StaticRO ~ty:(Ref _host) "host" "physical machine on which the pbd is available";
        field ~qualifier:StaticRO ~ty:(Ref _sr) "SR" "the storage repository that the pbd realises";
        field ~ty:(Map(String, String)) ~qualifier:StaticRO "device_config" "a config string to string map that is provided to the host's SR-backend-driver";
        field ~ty:Bool ~qualifier:DynamicRO "currently_attached" "is the SR currently attached on this host?";
        field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
      ()
end

(* These are included in vbds and vifs -- abstracted here to keep both these uses consistent *)
let device_status_fields =
  [
    field ~ty:Bool ~qualifier:DynamicRO "currently_attached" "is the device currently attached (erased on reboot)";
    field ~ty:Int ~qualifier:DynamicRO "status_code" "error/success code associated with last attach-operation (erased on reboot)";
    field ~ty:String ~qualifier:DynamicRO "status_detail" "error/success information associated with last attach-operation status (erased on reboot)";
    field ~ty:(Map(String, String)) ~qualifier:DynamicRO "runtime_properties" "Device runtime properties"
  ]

module VIF = struct
  (* VIF messages *)

  let ipv4_configuration_mode = Enum ("vif_ipv4_configuration_mode", [
      "None", "Follow the default IPv4 configuration of the guest (this is guest-dependent)";
      "Static", "Static IPv4 address configuration";
    ])

  let ipv6_configuration_mode = Enum ("vif_ipv6_configuration_mode", [
      "None", "Follow the default IPv6 configuration of the guest (this is guest-dependent)";
      "Static", "Static IPv6 address configuration";
    ])

  let plug = call
      ~name:"plug"
      ~in_product_since:rel_rio
      ~doc:"Hotplug the specified VIF, dynamically attaching it to the running VM"
      ~params:[Ref _vif, "self", "The VIF to hotplug"]
      ~allowed_roles:_R_VM_ADMIN
      ()

  let unplug = call
      ~name:"unplug"
      ~in_product_since:rel_rio
      ~doc:"Hot-unplug the specified VIF, dynamically unattaching it from the running VM"
      ~params:[Ref _vif, "self", "The VIF to hot-unplug"]
      ~allowed_roles:_R_VM_ADMIN
      ()

  let unplug_force = call
      ~name:"unplug_force"
      ~in_product_since:rel_boston
      ~doc:"Forcibly unplug the specified VIF"
      ~params:[Ref _vif, "self", "The VIF to forcibly unplug"]
      ~allowed_roles:_R_VM_ADMIN
      ()

  let move = call
      ~name:"move"
      ~in_product_since:rel_ely
      ~doc:"Move the specified VIF to the specified network, even while the VM is running"
      ~params:[Ref _vif, "self", "The VIF to move";
               Ref _network, "network", "The network to move it to"]
      ~allowed_roles:_R_VM_ADMIN
      ()

  let operations =
    Enum ("vif_operations",
          [ "attach", "Attempting to attach this VIF to a VM";
            "plug", "Attempting to hotplug this VIF";
            "unplug", "Attempting to hot unplug this VIF";
          ])

  let locking_mode =
    Enum ("vif_locking_mode", [
        "network_default", "No specific configuration set - default network policy applies";
        "locked", "Only traffic to a specific MAC and a list of IPv4 or IPv6 addresses is permitted";
        "unlocked", "All traffic is permitted";
        "disabled", "No traffic is permitted";
      ])

  let set_locking_mode = call
      ~name:"set_locking_mode"
      ~in_product_since:rel_tampa
      ~doc:"Set the locking mode for this VIF"
      ~params:[
        Ref _vif, "self", "The VIF whose locking mode will be set";
        locking_mode, "value", "The new locking mode for the VIF";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_ipv4_allowed = call
      ~name:"set_ipv4_allowed"
      ~in_product_since:rel_tampa
      ~doc:"Set the IPv4 addresses to which traffic on this VIF can be restricted"
      ~params:[
        Ref _vif, "self", "The VIF which the IP addresses will be associated with";
        Set String, "value", "The IP addresses which will be associated with the VIF";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let add_ipv4_allowed = call
      ~name:"add_ipv4_allowed"
      ~in_product_since:rel_tampa
      ~doc:"Associates an IPv4 address with this VIF"
      ~params:[
        Ref _vif, "self", "The VIF which the IP address will be associated with";
        String, "value", "The IP address which will be associated with the VIF";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let remove_ipv4_allowed = call
      ~name:"remove_ipv4_allowed"
      ~in_product_since:rel_tampa
      ~doc:"Removes an IPv4 address from this VIF"
      ~params:[
        Ref _vif, "self", "The VIF from which the IP address will be removed";
        String, "value", "The IP address which will be removed from the VIF";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_ipv6_allowed = call
      ~name:"set_ipv6_allowed"
      ~in_product_since:rel_tampa
      ~doc:"Set the IPv6 addresses to which traffic on this VIF can be restricted"
      ~params:[
        Ref _vif, "self", "The VIF which the IP addresses will be associated with";
        Set String, "value", "The IP addresses which will be associated with the VIF";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let add_ipv6_allowed = call
      ~name:"add_ipv6_allowed"
      ~in_product_since:rel_tampa
      ~doc:"Associates an IPv6 address with this VIF"
      ~params:[
        Ref _vif, "self", "The VIF which the IP address will be associated with";
        String, "value", "The IP address which will be associated with the VIF";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let remove_ipv6_allowed = call
      ~name:"remove_ipv6_allowed"
      ~in_product_since:rel_tampa
      ~doc:"Removes an IPv6 address from this VIF"
      ~params:[
        Ref _vif, "self", "The VIF from which the IP address will be removed";
        String, "value", "The IP address which will be removed from the VIF";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let configure_ipv4 = call
      ~name:"configure_ipv4"
      ~in_product_since:rel_dundee
      ~doc:"Configure IPv4 settings for this virtual interface"
      ~versioned_params:[
        {param_type=Ref _vif; param_name="self"; param_doc="The VIF to configure"; param_release=dundee_release; param_default=None};
        {param_type=ipv4_configuration_mode; param_name="mode"; param_doc="Whether to use static or no IPv4 assignment"; param_release=dundee_release; param_default=None};
        {param_type=String; param_name="address"; param_doc="The IPv4 address in <addr>/<prefix length> format (for static mode only)"; param_release=dundee_release; param_default=Some(VString "")};
        {param_type=String; param_name="gateway"; param_doc="The IPv4 gateway (for static mode only; leave empty to not set a gateway)"; param_release=dundee_release; param_default=Some(VString "")}
      ]
      ~allowed_roles:_R_VM_OP
      ()

  let configure_ipv6 = call
      ~name:"configure_ipv6"
      ~in_product_since:rel_dundee
      ~doc:"Configure IPv6 settings for this virtual interface"
      ~versioned_params:[
        {param_type=Ref _vif; param_name="self"; param_doc="The VIF to configure"; param_release=dundee_release; param_default=None};
        {param_type=ipv6_configuration_mode; param_name="mode"; param_doc="Whether to use static or no IPv6 assignment"; param_release=dundee_release; param_default=None};
        {param_type=String; param_name="address"; param_doc="The IPv6 address in <addr>/<prefix length> format (for static mode only)"; param_release=dundee_release; param_default=Some(VString "")};
        {param_type=String; param_name="gateway"; param_doc="The IPv6 gateway (for static mode only; leave empty to not set a gateway)"; param_release=dundee_release; param_default=Some(VString "")}
      ]
      ~allowed_roles:_R_VM_OP
      ()

  (** A virtual network interface *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vif ~descr:"A virtual network interface"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~doc_tags:[Networking]
      ~messages:[plug; unplug; unplug_force; move; set_locking_mode;
                 set_ipv4_allowed; add_ipv4_allowed; remove_ipv4_allowed; set_ipv6_allowed; add_ipv6_allowed; remove_ipv6_allowed;
                 configure_ipv4; configure_ipv6]
      ~contents:
        ([ uid _vif;
         ] @ (allowed_and_current_operations operations) @ [
           field ~qualifier:StaticRO "device" "order in which VIF backends are created by xapi";
           field ~qualifier:StaticRO ~ty:(Ref _network) "network" "virtual network to which this vif is connected";
           field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "virtual machine to which this vif is connected";
           field ~qualifier:StaticRO ~ty:String "MAC" "ethernet MAC address of virtual interface, as exposed to guest";
           field ~qualifier:StaticRO ~ty:Int "MTU" "MTU in octets";
           field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Bool "reserved" "true if the VIF is reserved pending a reboot/migrate";
           field ~ty:(Map(String, String)) "other_config" "additional configuration";
         ] @ device_status_fields @
         [ namespace ~name:"qos" ~contents:(qos "VIF") (); ] @
         [ field ~qualifier:DynamicRO ~ty:(Ref _vif_metrics) ~lifecycle: [Removed, rel_tampa, "Disabled in favour of RRDs"] "metrics" "metrics associated with this VIF";
           field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VBool false)) ~ty:Bool "MAC_autogenerated" "true if the MAC was autogenerated; false indicates it was set manually";
           field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VEnum "network_default")) ~ty:locking_mode "locking_mode" "current locking mode of the VIF";
           field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VSet [])) ~ty:(Set (String)) "ipv4_allowed" "A list of IPv4 addresses which can be used to filter traffic passing through this VIF";
           field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VSet [])) ~ty:(Set (String)) "ipv6_allowed" "A list of IPv6 addresses which can be used to filter traffic passing through this VIF";
           field ~ty:ipv4_configuration_mode ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv4_configuration_mode" "Determines whether IPv4 addresses are configured on the VIF" ~default_value:(Some (VEnum "None"));
           field ~ty:(Set (String)) ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv4_addresses" "IPv4 addresses in CIDR format" ~default_value:(Some (VSet []));
           field ~ty:String ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv4_gateway" "IPv4 gateway (the empty string means that no gateway is set)" ~default_value:(Some (VString ""));
           field ~ty:ipv6_configuration_mode ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv6_configuration_mode" "Determines whether IPv6 addresses are configured on the VIF" ~default_value:(Some (VEnum "None"));
           field ~ty:(Set (String)) ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv6_addresses" "IPv6 addresses in CIDR format" ~default_value:(Some (VSet []));
           field ~ty:String ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv6_gateway" "IPv6 gateway (the empty string means that no gateway is set)" ~default_value:(Some (VString ""));
           field ~ty:(Ref _pci) ~in_product_since:rel_kolkata ~internal_only:true ~qualifier:DynamicRO "reserved_pci" "pci of network SR-IOV VF which is reserved for this vif" ~default_value:(Some (VRef null_ref));
         ])
      ()
end

module VIF_metrics = struct
  let t =
    create_obj
      ~lifecycle:
        [ Published, rel_rio, "The metrics associated with a virtual network device"
        ; Removed, rel_tampa, "Disabled in favour of RRDs"
        ]
      ~in_db:true
      ~in_oss_since:oss_since_303
      ~persist:PersistNothing
      ~gen_constructor_destructor:false
      ~name:_vif_metrics
      ~descr:"The metrics associated with a virtual network device"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~doc_tags:[Networking]
      ~messages:[] ~contents:
      [ uid _vif_metrics;
        namespace ~name:"io" ~contents:iobandwidth ();
        field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
        field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
      ()
end

module Data_source = struct
  let t =
    create_obj ~in_db:false ~in_product_since:rel_orlando ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_data_source ~descr:"Data sources for logging in RRDs"
      ~gen_events:false
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages:[] ~contents:
      [ namespace ~name:"name" ~contents:(names oss_since_303 DynamicRO) ();
        field ~qualifier:DynamicRO ~ty:Bool "enabled" "true if the data source is being logged";
        field ~qualifier:DynamicRO ~ty:Bool "standard" "true if the data source is enabled by default. Non-default data sources cannot be disabled";
        field ~qualifier:DynamicRO ~ty:String "units" "the units of the value";
        field ~qualifier:DynamicRO ~ty:Float "min" "the minimum value of the data source";
        field ~qualifier:DynamicRO ~ty:Float "max" "the maximum value of the data source";
        field ~qualifier:DynamicRO ~ty:Float "value" "current value of the data source" ]

      ()
end


module SR = struct
  let operations =
    Enum ("storage_operations",
          [ "scan", "Scanning backends for new or deleted VDIs";
            "destroy", "Destroying the SR";
            "forget", "Forgetting about SR";
            "plug", "Plugging a PBD into this SR";
            "unplug", "Unplugging a PBD from this SR";
            "update", "Refresh the fields on the SR";
            "vdi_create", "Creating a new VDI";
            "vdi_introduce", "Introducing a new VDI";
            "vdi_destroy", "Destroying a VDI";
            "vdi_resize", "Resizing a VDI";
            "vdi_clone", "Cloneing a VDI";
            "vdi_snapshot", "Snapshotting a VDI";
            "vdi_mirror", "Mirroring a VDI";
            "vdi_enable_cbt", "Enabling changed block tracking for a VDI";
            "vdi_disable_cbt", "Disabling changed block tracking for a VDI";
            "vdi_data_destroy", "Deleting the data of the VDI";
            "vdi_list_changed_blocks", "Exporting a bitmap that shows the changed blocks between two VDIs";
            "vdi_set_on_boot", "Setting the on_boot field of the VDI";
            "pbd_create", "Creating a PBD for this SR";
            "pbd_destroy", "Destroying one of this SR's PBDs"; ])

  let dev_config_param =
    {param_type=Map(String,String); param_name="device_config"; param_doc="The device config string that will be passed to backend SR driver"; param_release=rio_release; param_default=None}

  let host_param =
    {param_type=Ref _host; param_name="host"; param_doc="The host to create/make the SR on"; param_release=rio_release; param_default=None}

  let physical_size_param =
    {param_type=Int; param_name="physical_size"; param_doc="The physical size of the new storage repository"; param_release=rio_release; param_default=None}

  let shared_param =
    {param_type=Bool; param_name="shared"; param_doc="True if the SR (is capable of) being shared by multiple hosts"; param_release=rio_release; param_default=None}

  let create_common =
    [
      {param_type=String; param_name="name_label"; param_doc="The name of the new storage repository"; param_release=rio_release; param_default=None};
      {param_type=String; param_name="name_description"; param_doc="The description of the new storage repository"; param_release=rio_release; param_default=None};
      {param_type=String; param_name="type"; param_doc="The type of the SR; used to specify the SR backend driver to use"; param_release=rio_release; param_default=None};
      {param_type=String; param_name="content_type"; param_doc="The type of the new SRs content, if required (e.g. ISOs)"; param_release=rio_release; param_default=None};
    ]

  let sm_config =
    {param_type=Map(String,String); param_name="sm_config"; param_doc="Storage backend specific configuration options"; param_release=miami_release; param_default=Some (VMap [])}


  let create = call
      ~name:"create"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~versioned_params:(host_param::dev_config_param::physical_size_param::(create_common @  [ shared_param; sm_config ] ))
      ~doc:"Create a new Storage Repository and introduce it into the managed system, creating both SR record and PBD record to attach it to current host (with specified device_config parameters)"
      ~result:(Ref _sr, "The reference of the newly created Storage Repository.")
      ~errs:[Api_errors.sr_unknown_driver]
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy_self_param =
    (Ref _sr, "sr", "The SR to destroy")

  let destroy = call
      ~name:"destroy"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~doc:"Destroy specified SR, removing SR-record from database and remove SR from disk. (In order to affect this operation the appropriate device_config is read from the specified SR's PBD on current host)"
      ~errs:[Api_errors.sr_has_pbd]
      ~params:[destroy_self_param]
      ~allowed_roles:_R_POOL_OP
      ()

  let forget = call
      ~name:"forget"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~doc:"Removing specified SR-record from database, without attempting to remove SR from disk"
      ~params:[destroy_self_param]
      ~errs:[Api_errors.sr_has_pbd]
      ~allowed_roles:_R_POOL_OP
      ()

  let introduce =
    call
      ~name:"introduce"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~versioned_params:({param_type=String; param_name="uuid"; param_doc="The uuid assigned to the introduced SR"; param_release=rio_release; param_default=None}::(create_common @ [shared_param; sm_config]))
      ~doc:"Introduce a new Storage Repository into the managed system"
      ~result:(Ref _sr, "The reference of the newly introduced Storage Repository.")
      ~allowed_roles:_R_POOL_OP
      ()

  let probe = call
      ~name:"probe"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~versioned_params:[host_param; dev_config_param; {param_type=String; param_name="type"; param_doc="The type of the SR; used to specify the SR backend driver to use"; param_release=miami_release; param_default=None}; sm_config]
      ~doc:"Perform a backend-specific scan, using the given device_config.  If the device_config is complete, then this will return a list of the SRs present of this type on the device, if any.  If the device_config is partial, then a backend-specific scan will be performed, returning results that will guide the user in improving the device_config."
      ~result:(String, "An XML fragment containing the scan results.  These are specific to the scan being performed, and the backend.")
      ~allowed_roles:_R_POOL_OP
      ()

  let make = call
      ~name:"make"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~internal_deprecated_since:rel_miami
      ~lifecycle:[
        Published, rel_rio, "Create a new Storage Repository on disk";
        Deprecated, rel_miami, "Use SR.create instead"
      ]
      ~versioned_params:(host_param::dev_config_param::physical_size_param::(create_common @ [sm_config]))
      ~doc:"Create a new Storage Repository on disk. This call is deprecated: use SR.create instead."
      ~result:(String, "The uuid of the newly created Storage Repository.")
      ~allowed_roles:_R_POOL_OP
      ()

  let get_supported_types = call
      ~name:"get_supported_types"
      ~in_product_since:rel_rio
      ~flags:[`Session]
      ~doc:"Return a set of all the SR types supported by the system"
      ~params:[]
      ~result:(Set String, "the supported SR types")
      ~allowed_roles:_R_READ_ONLY
      ()

  let scan = call
      ~name:"scan"
      ~in_product_since:rel_rio
      ~doc:"Refreshes the list of VDIs associated with an SR"
      ~params:[Ref _sr, "sr", "The SR to scan" ]
      ~allowed_roles:_R_VM_POWER_ADMIN
      ()

  (* Nb, although this is a new explicit call, it's actually been in the API since rio - just autogenerated. So no setting of rel_miami. *)
  let set_shared = call
      ~name:"set_shared"
      ~in_product_since:rel_rio
      ~doc:"Sets the shared flag on the SR"
      ~params:[Ref _sr, "sr", "The SR";
               Bool, "value", "True if the SR is shared"]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_name_label = call
      ~name:"set_name_label"
      ~in_product_since:rel_rio
      ~doc:"Set the name label of the SR"
      ~params:[Ref _sr, "sr", "The SR";
               String, "value", "The name label for the SR"]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_name_description = call
      ~name:"set_name_description"
      ~in_product_since:rel_rio
      ~doc:"Set the name description of the SR"
      ~params:[Ref _sr, "sr", "The SR";
               String, "value", "The name description for the SR"]
      ~allowed_roles:_R_POOL_OP
      ()

  let create_new_blob = call
      ~name: "create_new_blob"
      ~in_product_since:rel_orlando
      ~doc:"Create a placeholder for a named binary blob of data that is associated with this SR"
      ~versioned_params:
        [{param_type=Ref _sr; param_name="sr"; param_doc="The SR"; param_release=orlando_release; param_default=None};
         {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
         {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
         {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
        ]
      ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
      ~allowed_roles:_R_POOL_OP
      ()

  let get_data_sources = call
      ~name:"get_data_sources"
      ~in_oss_since:None
      ~in_product_since:rel_dundee
      ~doc:""
      ~result:(Set (Record _data_source), "A set of data sources")
      ~params:[Ref _sr, "sr", "The SR to interrogate"]
      ~errs:[]
      ~flags:[`Session]
      ~allowed_roles:_R_READ_ONLY
      ()

  let record_data_source = call
      ~name:"record_data_source"
      ~in_oss_since:None
      ~in_product_since:rel_dundee
      ~doc:"Start recording the specified data source"
      ~params:[Ref _sr, "sr", "The SR";
               String, "data_source", "The data source to record"]
      ~errs:[]
      ~flags:[`Session]
      ~allowed_roles:_R_POOL_OP
      ()

  let query_data_source = call
      ~name:"query_data_source"
      ~in_oss_since:None
      ~in_product_since:rel_dundee
      ~doc:"Query the latest value of the specified data source"
      ~params:[Ref _sr, "sr", "The SR";
               String, "data_source", "The data source to query"]
      ~result:(Float,"The latest value, averaged over the last 5 seconds")
      ~errs:[]
      ~flags:[`Session]
      ~allowed_roles:_R_READ_ONLY
      ()

  let forget_data_source_archives = call
      ~name:"forget_data_source_archives"
      ~in_oss_since:None
      ~in_product_since:rel_dundee
      ~doc:"Forget the recorded statistics related to the specified data source"
      ~params:[Ref _sr, "sr", "The SR";
               String, "data_source", "The data source whose archives are to be forgotten"]
      ~flags:[`Session]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_virtual_allocation = call
      ~name:"set_virtual_allocation"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[Ref _sr, "self", "The SR to modify";
               Int, "value", "The new value of the SR's virtual_allocation"]
      ~flags:[`Session]
      ~doc:"Sets the SR's virtual_allocation field"
      ~allowed_roles:_R_POOL_OP
      ()

  let set_physical_size = call
      ~name:"set_physical_size"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[Ref _sr, "self", "The SR to modify";
               Int, "value", "The new value of the SR's physical_size"]
      ~flags:[`Session]
      ~doc:"Sets the SR's physical_size field"
      ~allowed_roles:_R_POOL_OP
      ()

  let set_physical_utilisation = call
      ~name:"set_physical_utilisation"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~flags:[`Session]
      ~params:[Ref _sr, "self", "The SR to modify";
               Int, "value", "The new value of the SR's physical utilisation"]
      ~doc:"Sets the SR's physical_utilisation field"
      ~allowed_roles:_R_POOL_OP
      ()

  let update = call
      ~name:"update"
      ~in_oss_since:None
      ~in_product_since:rel_symc
      ~params:[Ref _sr, "sr", "The SR whose fields should be refreshed" ]
      ~doc:"Refresh the fields on the SR object"
      ~allowed_roles:_R_POOL_OP
      ()

  let assert_can_host_ha_statefile = call
      ~name:"assert_can_host_ha_statefile"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~params:[Ref _sr, "sr", "The SR to query" ]
      ~doc:"Returns successfully if the given SR can host an HA statefile. Otherwise returns an error to explain why not"
      ~allowed_roles:_R_POOL_OP
      ()

  let assert_supports_database_replication = call
      ~name:"assert_supports_database_replication"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _sr, "sr", "The SR to query"]
      ~doc:"Returns successfully if the given SR supports database replication. Otherwise returns an error to explain why not."
      ~allowed_roles:_R_POOL_OP
      ()

  let enable_database_replication = call
      ~name:"enable_database_replication"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _sr, "sr", "The SR to which metadata should be replicated"]
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_database_replication = call
      ~name:"disable_database_replication"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _sr, "sr", "The SR to which metadata should be no longer replicated"]
      ~allowed_roles:_R_POOL_OP
      ()

  (** A storage repository. Note we overide default create/destroy methods with our own here... *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_sr ~descr:"A storage repository"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[ create; introduce; make; destroy; forget;
                  update;
                  get_supported_types; scan; probe; set_shared;
                  set_name_label; set_name_description;
                  create_new_blob;
                  set_physical_size; set_virtual_allocation; set_physical_utilisation;
                  assert_can_host_ha_statefile;
                  assert_supports_database_replication;
                  enable_database_replication;
                  disable_database_replication;
                  get_data_sources;
                  record_data_source;
                  query_data_source;
                  forget_data_source_archives;

                ]
      ~contents:
        ([ uid _sr;
           namespace ~name:"name" ~contents:(names oss_since_303 StaticRO) ();
         ] @ (allowed_and_current_operations operations) @ [
           field ~ty:(Set(Ref _vdi)) ~qualifier:DynamicRO "VDIs" "all virtual disks known to this storage repository";
           field ~qualifier:DynamicRO ~ty:(Set (Ref _pbd)) "PBDs" "describes how particular hosts can see this storage repository";
           field ~ty:Int ~qualifier:DynamicRO "virtual_allocation" "sum of virtual_sizes of all VDIs in this storage repository (in bytes)";
           field ~ty:Int ~qualifier:DynamicRO "physical_utilisation" "physical space currently utilised on this storage repository (in bytes). Note that for sparse disk formats, physical_utilisation may be less than virtual_allocation";
           field ~ty:Int ~qualifier:StaticRO "physical_size" "total physical size of the repository (in bytes)";
           field ~qualifier:StaticRO "type" "type of the storage repository";
           field ~qualifier:StaticRO "content_type" "the type of the SR's content, if required (e.g. ISOs)";
           field ~qualifier:DynamicRO "shared" ~ty:Bool "true if this SR is (capable of being) shared between multiple hosts";
           field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
           field  ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
           field ~ty:Bool ~qualifier:DynamicRO ~in_oss_since:None ~internal_only:true "default_vdi_visibility" "";
           field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "sm_config" "SM dependent data" ~default_value:(Some (VMap []));
           field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this SR";
           field ~qualifier:DynamicRO ~in_product_since:rel_cowley ~ty:Bool ~default_value:(Some (VBool false)) "local_cache_enabled" "True if this SR is assigned to be the local cache for its host";
           field ~qualifier:DynamicRO ~in_product_since:rel_boston ~ty:(Ref _dr_task) ~default_value:(Some (VRef null_ref)) "introduced_by" "The disaster recovery task which introduced this SR";
           field ~qualifier:DynamicRO ~lifecycle:[Published, rel_dundee, ""] ~ty:Bool ~default_value:(Some (VBool false)) "clustered" "True if the SR is using aggregated local storage";
           field ~qualifier:DynamicRO ~lifecycle:[Published, rel_dundee, ""] ~ty:Bool ~default_value:(Some (VBool false)) "is_tools_sr" "True if this is the SR that contains the Tools ISO VDIs";
         ])
      ()

end

module SM = struct
  (** XXX: just make this a field and be done with it. Cowardly refusing to change the schema for now. *)
  let get_driver_filename = call
      ~name:"get_driver_filename"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~params:[Ref _sm, "self", "The SM to query" ]
      ~result:(String, "The SM's driver_filename field")
      ~doc:"Gets the SM's driver_filename field"
      ()

  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_sm ~descr:"A storage manager plugin"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[ ]
      ~contents:
        ([ uid _sm;
           namespace ~name:"name" ~contents:(names None DynamicRO) ();
           field ~in_oss_since:None ~qualifier:DynamicRO "type" "SR.type";
           field ~in_oss_since:None ~qualifier:DynamicRO "vendor" "Vendor who created this plugin";
           field ~in_oss_since:None ~qualifier:DynamicRO "copyright" "Entity which owns the copyright of this plugin";
           field ~in_oss_since:None ~qualifier:DynamicRO "version" "Version of the plugin";
           field ~in_oss_since:None ~qualifier:DynamicRO "required_api_version" "Minimum SM API version required on the server";
           field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map(String,String)) "configuration" "names and descriptions of device config keys";
           field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_miami ~lifecycle:[ Deprecated, rel_clearwater, "Use SM.features instead"; ] ~ty:(Set(String)) "capabilities" "capabilities of the SM plugin" ~default_value:(Some (VSet []));
           field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_clearwater ~ty:(Map(String, Int)) "features" "capabilities of the SM plugin, with capability version numbers" ~default_value:(Some (VMap []));
           field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
           field ~in_product_since:rel_orlando ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String "driver_filename" "filename of the storage driver";
           field ~in_product_since:rel_dundee ~qualifier:DynamicRO ~default_value:(Some (VSet [])) ~ty:(Set String) "required_cluster_stack" "The storage plugin requires that one of these cluster stacks is configured and running.";
         ])
      ()
end

module LVHD = struct
  let enable_thin_provisioning = call
      ~name:"enable_thin_provisioning"
      ~in_oss_since:None
      ~in_product_since:rel_dundee
      ~allowed_roles:_R_POOL_ADMIN
      ~params:[
        Ref _host, "host", "The LVHD Host to upgrade to being thin-provisioned.";
        Ref _sr, "SR", "The LVHD SR to upgrade to being thin-provisioned.";
        Int, "initial_allocation", "The initial amount of space to allocate to a newly-created VDI in bytes";
        Int, "allocation_quantum", "The amount of space to allocate to a VDI when it needs to be enlarged in bytes";
      ]
      ~doc:"Upgrades an LVHD SR to enable thin-provisioning. Future VDIs created in this SR will be thinly-provisioned, although existing VDIs will be left alone. Note that the SR must be attached to the SRmaster for upgrade to work."
      ~forward_to:(HostExtension "LVHD.enable_thin_provisioning")
      ~result:(String, "Message from LVHD.enable_thin_provisioning extension")
      ()

  let t =
    create_obj ~in_db:true ~in_product_since:rel_dundee ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_lvhd ~descr:"LVHD SR specific operations"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages:[
        enable_thin_provisioning;
      ]
      ~contents: [
        uid _lvhd;
      ]
      ()
end

(* --- rws: removed this after talking to Andy and Julian
   let filesystem =
   { name = _filesystem; description = "An on-disk filesystem";
    messages = [];
    contents =
      field "uuid" "globally-unique ID" ::
    let field ?(ty=Int) = field ~qualifier:DynamicRO ~ty in
    [ field "block_size" "block size";
      field "total_blocks" "total blocks on disk";
      field "available_blocks" "blocks available for allocation";
      field "used_blocks" "blocks already in use";
      field "percentage_free" "Percentage of free space left in filesystem";
      field ~ty:String "type" "filesystem type" ] }
*)

module Vdi_nbd_server_info = struct
  let t =
    let lifecycle = [Published, rel_inverness, ""] in
    create_obj
      ~in_db:false
      ~persist:PersistNothing
      ~gen_constructor_destructor:false
      ~lifecycle
      ~in_oss_since:None
      ~name:_vdi_nbd_server_info
      ~descr:"Details for connecting to a VDI using the Network Block Device protocol"
      ~gen_events:false
      ~messages:[]
      ~doccomments:[]
      ~messages_default_allowed_roles:(Some []) (* No messages, so no roles allowed to use them *)
      ~contents:
        [ (* uid _vdi_nbd_server_info; The uuid is not needed here and only adds inconvenience. *)
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "exportname" "The exportname to request over NBD. This holds details including an authentication token, so it must be protected appropriately. Clients should regard the exportname as an opaque string or token.";
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "address" "An address on which the server can be reached; this can be IPv4, IPv6, or a DNS name.";
          field ~qualifier:DynamicRO ~lifecycle ~ty:Int "port" "The TCP port";
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "cert" "The TLS certificate of the server";
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "subject" "For convenience, this redundant field holds a DNS (hostname) subject of the certificate. This can be a wildcard, but only for a certificate that has a wildcard subject and no concrete hostname subjects.";
        ] ()
end


module VDI = struct
  (** Each disk is associated with a vdi_type: (a 'style' of disk?) *)
  let type' = Enum ("vdi_type", [ "system",    "a disk that may be replaced on upgrade";
                                  "user",      "a disk that is always preserved on upgrade";
                                  "ephemeral", "a disk that may be reformatted on upgrade";
                                  "suspend",   "a disk that stores a suspend image";
                                  "crashdump", "a disk that stores VM crashdump information";
                                  "ha_statefile", "a disk used for HA storage heartbeating";
                                  "metadata", "a disk used for HA Pool metadata";
                                  "redo_log", "a disk used for a general metadata redo-log";
                                  "rrd", "a disk that stores SR-level RRDs";
                                  "pvs_cache", "a disk that stores PVS cache data";
                                  "cbt_metadata", "Metadata about a snapshot VDI that has been deleted: the set of blocks that changed between some previous version of the disk and the version tracked by the snapshot.";
                                ])
  let snapshot = call
      ~name:"snapshot"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~versioned_params:
        [{param_type=Ref _vdi; param_name="vdi"; param_doc="The VDI to snapshot"; param_release=rio_release; param_default=None};
         {param_type=Map (String, String); param_name="driver_params"; param_doc="Optional parameters that can be passed through to backend driver in order to specify storage-type-specific snapshot options"; param_release=miami_release; param_default=Some (VMap [])}
        ]
      ~doc:"Take a read-only snapshot of the VDI, returning a reference to the snapshot. If any driver_params are specified then these are passed through to the storage-specific substrate driver that takes the snapshot. NB the snapshot lives in the same Storage Repository as its parent."
      ~result:(Ref _vdi, "The ID of the newly created VDI.")
      ~allowed_roles:_R_VM_ADMIN
      ~doc_tags:[Snapshots]
      ()

  let clone = call
      ~name:"clone"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[Ref _vdi, "vdi", "The VDI to clone"]
      ~versioned_params:
        [{param_type=Ref _vdi; param_name="vdi"; param_doc="The VDI to clone"; param_release=rio_release; param_default=None};
         {param_type=Map (String, String); param_name="driver_params"; param_doc="Optional parameters that are passed through to the backend driver in order to specify storage-type-specific clone options"; param_release=miami_release; param_default=Some (VMap [])}
        ]
      ~doc:"Take an exact copy of the VDI and return a reference to the new disk. If any driver_params are specified then these are passed through to the storage-specific substrate driver that implements the clone operation. NB the clone lives in the same Storage Repository as its parent."
      ~result:(Ref _vdi, "The ID of the newly created VDI.")
      ~allowed_roles:_R_VM_ADMIN
      ~doc_tags:[Snapshots]
      ()

  let resize = call
      ~name:"resize"
      ~in_product_since:rel_rio
      ~in_oss_since:None
      ~params:[Ref _vdi, "vdi", "The VDI to resize"; Int, "size", "The new size of the VDI" ]
      ~doc:"Resize the VDI."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let resize_online = call
      ~name:"resize_online"
      ~in_oss_since:None
      ~lifecycle: [
        Published, rel_rio, "";
        Removed, rel_inverness, "Online VDI resize is not supported by any of the storage backends."
      ]
      ~params:[Ref _vdi, "vdi", "The VDI to resize"; Int, "size", "The new size of the VDI" ]
      ~doc:"Resize the VDI which may or may not be attached to running guests."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let copy = call
      ~name:"copy"
      ~lifecycle:[
        Published, rel_rio, "Copies a VDI to an SR. There must be a host that can see both the source and destination SRs simultaneously";
        Extended, rel_cowley, "The copy can now be performed between any two SRs.";
        Extended, rel_clearwater_felton, "The copy can now be performed into a pre-created VDI. It is now possible to request copying only changed blocks from a base VDI"; ]
      ~in_oss_since:None
      ~versioned_params:
        [{param_type=Ref _vdi; param_name="vdi"; param_doc="The VDI to copy"; param_release=rio_release; param_default=None};
         {param_type=Ref _sr; param_name="sr"; param_doc="The destination SR (only required if the destination VDI is not specified"; param_release=rio_release; param_default=Some (VString null_ref)};
         {param_type=Ref _vdi; param_name="base_vdi"; param_doc="The base VDI (only required if copying only changed blocks, by default all blocks will be copied)"; param_release=clearwater_felton_release; param_default=Some (VRef null_ref)};
         {param_type=Ref _vdi; param_name="into_vdi"; param_doc="The destination VDI to copy blocks into (if omitted then a destination SR must be provided and a fresh VDI will be created)"; param_release=clearwater_felton_release; param_default=Some (VString null_ref)};
        ]
      ~doc:"Copy either a full VDI or the block differences between two VDIs into either a fresh VDI or an existing VDI."
      ~errs:[Api_errors.vdi_readonly; Api_errors.vdi_too_small; Api_errors.vdi_not_sparse]
      ~result:(Ref _vdi, "The reference of the VDI where the blocks were written.")
      ~allowed_roles:_R_VM_ADMIN
      ()

  let pool_migrate = call
      ~name:"pool_migrate"
      ~in_oss_since:None
      ~in_product_since:rel_tampa
      ~params:[ Ref _vdi, "vdi", "The VDI to migrate"
              ; Ref _sr, "sr", "The destination SR"
              ; Map (String, String), "options", "Other parameters" ]
      ~result:(Ref _vdi, "The new reference of the migrated VDI.")
      ~doc:"Migrate a VDI, which may be attached to a running guest, to a different SR. The destination SR must be visible to the guest."
      ~allowed_roles:_R_VM_POWER_ADMIN
      ()

  let introduce_params first_rel =
    [
      {param_type=String; param_name="uuid"; param_doc="The uuid of the disk to introduce"; param_release=first_rel; param_default=None};
      {param_type=String; param_name="name_label"; param_doc="The name of the disk record"; param_release=first_rel; param_default=None};
      {param_type=String; param_name="name_description"; param_doc="The description of the disk record"; param_release=first_rel; param_default=None};
      {param_type=Ref _sr; param_name="SR"; param_doc="The SR that the VDI is in"; param_release=first_rel; param_default=None};
      {param_type=type'; param_name="type"; param_doc="The type of the VDI"; param_release=first_rel; param_default=None};
      {param_type=Bool; param_name="sharable"; param_doc="true if this disk may be shared"; param_release=first_rel; param_default=None};
      {param_type=Bool; param_name="read_only"; param_doc="true if this disk may ONLY be mounted read-only"; param_release=first_rel; param_default=None};
      {param_type=Map(String, String); param_name="other_config"; param_doc="additional configuration"; param_release=first_rel; param_default=None};
      {param_type=String; param_name="location"; param_doc="location information"; param_release=first_rel; param_default=None};
      {param_type=Map(String, String); param_name="xenstore_data"; param_doc="Data to insert into xenstore"; param_release=first_rel; param_default=Some (VMap [])};
      {param_type=Map(String, String); param_name="sm_config"; param_doc="Storage-specific config"; param_release=miami_release; param_default=Some (VMap [])};
      {param_type=Bool; param_name = "managed"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VBool true) };
      {param_type=Int; param_name="virtual_size"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VInt 0L) };
      {param_type=Int; param_name="physical_utilisation"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VInt 0L) };
      {param_type=Ref _pool; param_name="metadata_of_pool"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VRef "") };
      {param_type=Bool; param_name="is_a_snapshot"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VBool false) };
      {param_type=DateTime; param_name="snapshot_time"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VDateTime Date.never) };
      {param_type=Ref _vdi; param_name="snapshot_of"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VRef "") };


    ]

  (* This used to be called VDI.introduce but it was always an internal call *)
  let pool_introduce = call
      ~name:"pool_introduce"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~versioned_params:(
        (introduce_params miami_release) @
        [{ param_type=Bool; param_name="cbt_enabled"; param_doc="True if changed blocks are tracked for this VDI"; param_release=inverness_release; param_default= Some(VBool false) }]
      )
      ~doc:"Create a new VDI record in the database only"
      ~result:(Ref _vdi, "The ref of the newly created VDI record.")
      ~hide_from_docs:true
      ~allowed_roles:_R_VM_ADMIN
      ()

  let db_introduce = { pool_introduce with msg_name = "db_introduce"; msg_hide_from_docs = false }

  let db_forget = call
      ~name:"db_forget"
      ~in_oss_since:None
      ~params:[Ref _vdi, "vdi", "The VDI to forget about"]
      ~doc:"Removes a VDI record from the database"
      ~in_product_since:rel_miami
      ~allowed_roles:_R_VM_ADMIN
      ()

  let introduce = call
      ~name:"introduce"
      ~in_oss_since:None
      ~versioned_params:(introduce_params rio_release)
      ~doc:"Create a new VDI record in the database only"
      ~result:(Ref _vdi, "The ref of the newly created VDI record.")
      ~errs:[Api_errors.sr_operation_not_supported]
      ~in_product_since:rel_miami
      ~allowed_roles:_R_VM_ADMIN
      ()

  let forget = call
      ~name:"forget"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[Ref _vdi, "vdi", "The VDI to forget about"]
      ~doc:"Removes a VDI record from the database"
      ~allowed_roles:_R_VM_ADMIN
      ()

  let force_unlock = call
      ~name:"force_unlock"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~internal_deprecated_since:rel_miami
      ~params:[Ref _vdi, "vdi", "The VDI to forcibly unlock"]
      ~doc:"Steals the lock on this VDI and leaves it unlocked. This function is extremely dangerous. This call is deprecated."
      ~hide_from_docs:true
      ~allowed_roles:_R_VM_ADMIN
      ()

  let update = call
      ~name:"update"
      ~in_oss_since:None
      ~params:[Ref _vdi, "vdi", "The VDI whose stats (eg size) should be updated" ]
      ~doc:"Ask the storage backend to refresh the fields in the VDI object"
      ~errs:[Api_errors.sr_operation_not_supported]
      ~in_product_since:rel_symc
      ~allowed_roles:_R_VM_ADMIN
      ()

  let operations =
    Enum ("vdi_operations",
          [ "clone", "Cloning the VDI";
            "copy", "Copying the VDI";
            "resize", "Resizing the VDI";
            "resize_online", "Resizing the VDI which may or may not be online";
            "snapshot", "Snapshotting the VDI";
            "mirror", "Mirroring the VDI";
            "destroy", "Destroying the VDI";
            "forget", "Forget about the VDI";
            "update", "Refreshing the fields of the VDI";
            "force_unlock", "Forcibly unlocking the VDI";
            "generate_config", "Generating static configuration";
            "enable_cbt", "Enabling changed block tracking for a VDI";
            "disable_cbt", "Disabling changed block tracking for a VDI";
            "data_destroy", "Deleting the data of the VDI";
            "list_changed_blocks", "Exporting a bitmap that shows the changed blocks between two VDIs";
            "set_on_boot", "Setting the on_boot field of the VDI";
            "blocked", "Operations on this VDI are temporarily blocked";
          ])

  let set_missing = call
      ~name:"set_missing"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Bool, "value", "The new value of the VDI's missing field"]
      ~doc:"Sets the VDI's missing field"
      ~flags:[`Session]
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_read_only = call
      ~name:"set_read_only"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Bool, "value", "The new value of the VDI's read_only field"]
      ~flags:[`Session]
      ~doc:"Sets the VDI's read_only field"
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_sharable = call
      ~name:"set_sharable"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Bool, "value", "The new value of the VDI's sharable field"]
      ~flags:[`Session]
      ~doc:"Sets the VDI's sharable field"
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_managed = call
      ~name:"set_managed"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Bool, "value", "The new value of the VDI's managed field"]
      ~flags:[`Session]
      ~doc:"Sets the VDI's managed field"
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_virtual_size = call
      ~name:"set_virtual_size"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Int, "value", "The new value of the VDI's virtual size"]
      ~flags:[`Session]
      ~doc:"Sets the VDI's virtual_size field"
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_physical_utilisation = call
      ~name:"set_physical_utilisation"
      ~in_oss_since:None
      ~in_product_since:rel_miami
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Int, "value", "The new value of the VDI's physical utilisation"]
      ~flags:[`Session]
      ~doc:"Sets the VDI's physical_utilisation field"
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_is_a_snapshot = call
      ~name:"set_is_a_snapshot"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Bool, "value", "The new value indicating whether this VDI is a snapshot"]
      ~flags:[`Session]
      ~doc:"Sets whether this VDI is a snapshot"
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_snapshot_of = call
      ~name:"set_snapshot_of"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Ref _vdi, "value", "The VDI of which this VDI is a snapshot"]
      ~flags:[`Session]
      ~doc:"Sets the VDI of which this VDI is a snapshot"
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_snapshot_time = call
      ~name:"set_snapshot_time"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _vdi, "self", "The VDI to modify";
               DateTime, "value", "The snapshot time of this VDI."]
      ~flags:[`Session]
      ~doc:"Sets the snapshot time of this VDI."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_metadata_of_pool = call
      ~name:"set_metadata_of_pool"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Ref _pool, "value", "The pool whose metadata is contained by this VDI"]
      ~flags:[`Session]
      ~doc:"Records the pool whose metadata is contained by this VDI."
      ~allowed_roles:_R_VM_ADMIN
      ()

  (** An API call for debugging and testing only *)
  let generate_config = call
      ~name:"generate_config"
      ~in_oss_since:None
      ~in_product_since:rel_orlando
      ~params:[Ref _host, "host", "The host on which to generate the configuration";
               Ref _vdi, "vdi", "The VDI to generate the configuration for" ]
      ~result:(String, "The generated static configuration")
      ~doc:"Internal function for debugging only"
      ~hide_from_docs:true
      ~allowed_roles:_R_VM_ADMIN
      ()

  let on_boot = Enum ("on_boot", [
      "reset", "When a VM containing this VDI is started, the contents of the VDI are reset to the state they were in when this flag was last set.";
      "persist", "Standard behaviour.";
    ])

  let set_on_boot = call
      ~name:"set_on_boot"
      ~in_oss_since:None
      ~in_product_since:rel_cowley
      ~params:[Ref _vdi, "self", "The VDI to modify";
               on_boot, "value", "The value to set"]
      ~doc:"Set the value of the on_boot parameter. This value can only be changed when the VDI is not attached to a running VM."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_allow_caching = call
      ~name:"set_allow_caching"
      ~in_oss_since:None
      ~in_product_since:rel_cowley
      ~params:[Ref _vdi, "self", "The VDI to modify";
               Bool, "value", "The value to set"]
      ~doc:"Set the value of the allow_caching parameter. This value can only be changed when the VDI is not attached to a running VM. The caching behaviour is only affected by this flag for VHD-based VDIs that have one parent and no child VHDs. Moreover, caching only takes place when the host running the VM containing this VDI has a nominated SR for local caching."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_name_label = call
      ~name:"set_name_label"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[Ref _vdi, "self", "The VDI to modify";
               String, "value", "The name lable for the VDI"]
      ~doc:"Set the name label of the VDI. This can only happen when then its SR is currently attached."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_name_description = call
      ~name:"set_name_description"
      ~in_oss_since:None
      ~in_product_since:rel_rio
      ~params:[Ref _vdi, "self", "The VDI to modify";
               String, "value", "The name description for the VDI"]
      ~doc:"Set the name description of the VDI. This can only happen when its SR is currently attached."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let open_database = call
      ~name:"open_database"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _vdi, "self", "The VDI which contains the database to open"]
      ~result:(Ref _session, "A session which can be used to query the database")
      ~doc:"Load the metadata found on the supplied VDI and return a session reference which can be used in XenAPI calls to query its contents."
      ~allowed_roles:_R_POOL_OP
      ()

  let checksum = call
      ~name:"checksum"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _vdi, "self", "The VDI to checksum"]
      ~result:(String, "The md5sum of the vdi")
      ~doc:"Internal function to calculate VDI checksum and return a string"
      ~hide_from_docs:true
      ~allowed_roles:_R_VM_ADMIN (* Conceptually, this is not correct. We do it
                                    	                              this way only to follow the previous
                                    	                              convention. It is supposed to fix by future
                                    	                              version of RBAC *)
      ()

  let read_database_pool_uuid = call
      ~name:"read_database_pool_uuid"
      ~in_oss_since:None
      ~in_product_since:rel_boston
      ~params:[Ref _vdi, "self", "The metadata VDI to look up in the cache."]
      ~result:(String, "The cached pool UUID of the database on the VDI.")
      ~doc:"Check the VDI cache for the pool UUID of the database on this VDI."
      ~allowed_roles:_R_READ_ONLY
      ()

  let enable_cbt = call
      ~name:"enable_cbt"
      ~in_oss_since:None
      ~in_product_since:rel_inverness
      ~params:[Ref _vdi, "self", "The VDI for which CBT should be enabled"]
      ~errs:[
        Api_errors.sr_operation_not_supported;
        Api_errors.vdi_missing;
        Api_errors.sr_not_attached;
        Api_errors.sr_no_pbds;
        Api_errors.operation_not_allowed;
        Api_errors.vdi_incompatible_type;
        Api_errors.vdi_on_boot_mode_incompatible_with_operation;
      ]
      ~doc:"Enable changed block tracking for the VDI. This call is idempotent - enabling CBT for a VDI for which CBT is already enabled results in a no-op, and no error will be thrown."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let disable_cbt = call
      ~name:"disable_cbt"
      ~in_oss_since:None
      ~in_product_since:rel_inverness
      ~params:[Ref _vdi, "self", "The VDI for which CBT should be disabled"]
      ~errs:[
        Api_errors.sr_operation_not_supported;
        Api_errors.vdi_missing;
        Api_errors.sr_not_attached;
        Api_errors.sr_no_pbds;
        Api_errors.operation_not_allowed;
        Api_errors.vdi_incompatible_type;
        Api_errors.vdi_on_boot_mode_incompatible_with_operation;
      ]
      ~doc:"Disable changed block tracking for the VDI. This call is only allowed on VDIs that support enabling CBT. It is an idempotent operation - disabling CBT for a VDI for which CBT is not enabled results in a no-op, and no error will be thrown."
      ~allowed_roles:_R_VM_ADMIN
      ()

  (** This command is for internal use by SM to set the cbt_enabled field when it needs to disable cbt for its own reasons. This command should be removed once SMAPIv3 is implemented *)
  let set_cbt_enabled = call
      ~name:"set_cbt_enabled"
      ~in_oss_since:None
      ~in_product_since:rel_inverness
      ~params:[Ref _vdi, "self", "The VDI for which CBT enabled status should be set";
               Bool, "value", "The value to set"]
      ~errs:[]
      ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ()

  let data_destroy = call
      ~name:"data_destroy"
      ~in_oss_since:None
      ~in_product_since:rel_inverness
      ~params:[Ref _vdi, "self", "The VDI whose data should be deleted."]
      ~errs:[
        Api_errors.sr_operation_not_supported;
        Api_errors.vdi_missing;
        Api_errors.sr_not_attached;
        Api_errors.sr_no_pbds;
        Api_errors.operation_not_allowed;
        Api_errors.vdi_incompatible_type;
        Api_errors.vdi_no_cbt_metadata;
        Api_errors.vdi_in_use;
        Api_errors.vdi_is_a_physical_device;
      ]
      ~doc:"Delete the data of the snapshot VDI, but keep its changed block tracking metadata. When successful, this call changes the type of the VDI to cbt_metadata. This operation is idempotent: calling it on a VDI of type cbt_metadata results in a no-op, and no error will be thrown."
      ~allowed_roles:_R_VM_ADMIN
      ()

  let list_changed_blocks = call
      ~name:"list_changed_blocks"
      ~in_oss_since:None
      ~in_product_since:rel_inverness
      ~params:
        [ Ref _vdi, "vdi_from", "The first VDI."
        ; Ref _vdi, "vdi_to", "The second VDI."
        ]
      ~errs:
        [ Api_errors.sr_operation_not_supported
        ; Api_errors.vdi_missing
        ; Api_errors.sr_not_attached
        ; Api_errors.sr_no_pbds
        ; Api_errors.vdi_in_use
        ]
      ~result:(String, "A base64 string-encoding of the bitmap showing which blocks differ in the two VDIs.")
      ~doc:"Compare two VDIs in 64k block increments and report which blocks differ. This operation is not allowed when vdi_to is attached to a VM."
      ~allowed_roles:_R_VM_OP
      ()


  let get_nbd_info = call
      ~name:"get_nbd_info"
      ~in_oss_since:None
      ~in_product_since:rel_inverness
      ~params:[Ref _vdi, "self", "The VDI to access via Network Block Device protocol"]
      ~errs: [Api_errors.vdi_incompatible_type]
      ~result:(Set (Record _vdi_nbd_server_info), "The details necessary for connecting to the VDI over NBD. This includes an authentication token, so must be treated as sensitive material and must not be sent over insecure networks.")
      ~doc:"Get details specifying how to access this VDI via a Network Block Device server. For each of a set of NBD server addresses on which the VDI is available, the return value set contains a vdi_nbd_server_info object that contains an exportname to request once the NBD connection is established, and connection details for the address. An empty list is returned if there is no network that has a PIF on a host with access to the relevant SR, or if no such network has been assigned an NBD-related purpose in its purpose field. To access the given VDI, any of the vdi_nbd_server_info objects can be used to make a connection to a server, and then the VDI will be available by requesting the exportname."
      ~flags:[`Session] (* no async *)
      ~allowed_roles:_R_VM_ADMIN
      ()


  (** A virtual disk *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vdi ~descr:"A virtual disk image"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[snapshot; clone; resize;
                 resize_online;
                 introduce; pool_introduce;
                 db_introduce; db_forget;
                 update;
                 copy;
                 force_unlock; set_managed;
                 forget;
                 set_sharable;
                 set_read_only;
                 set_missing;
                 set_virtual_size;
                 set_physical_utilisation;
                 set_is_a_snapshot;
                 set_snapshot_of;
                 set_snapshot_time;
                 set_metadata_of_pool;
                 set_name_label;
                 set_name_description;
                 generate_config;
                 set_on_boot;
                 set_allow_caching;
                 open_database;
                 checksum;
                 read_database_pool_uuid;
                 pool_migrate;
                 enable_cbt;
                 disable_cbt;
                 set_cbt_enabled;
                 data_destroy;
                 list_changed_blocks;
                 get_nbd_info;
                ]
      ~contents:
        ([ uid _vdi;
           namespace ~name:"name" ~contents:(names oss_since_303 StaticRO) ();
         ] @ (allowed_and_current_operations operations) @ [
           field ~qualifier:StaticRO ~ty:(Ref _sr) "SR" "storage repository in which the VDI resides";
           field ~qualifier:DynamicRO ~ty:(Set (Ref _vbd)) "VBDs" "list of vbds that refer to this disk";
           field ~qualifier:DynamicRO ~ty:(Set (Ref _crashdump)) "crash_dumps" "list of crash dumps that refer to this disk";
           field ~qualifier:StaticRO ~ty:Int "virtual_size" "size of disk as presented to the guest (in bytes). Note that, depending on storage backend type, requested size may not be respected exactly";
           field ~qualifier:DynamicRO ~ty:Int "physical_utilisation" "amount of physical space that the disk image is currently taking up on the storage repository (in bytes)";
           field ~qualifier:StaticRO ~ty:type' "type" "type of the VDI";
           field ~qualifier:StaticRO ~ty:Bool "sharable" "true if this disk may be shared";
           field ~qualifier:StaticRO ~ty:Bool "read_only" "true if this disk may ONLY be mounted read-only";
           field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
           field ~qualifier:DynamicRO ~ty:Bool "storage_lock" "true if this disk is locked at the storage level";
           (* XXX: location field was in the database in rio, now API in miami *)
           field ~in_oss_since:None ~in_product_since:rel_miami ~ty:String ~qualifier:DynamicRO ~default_value:(Some (VString "")) "location" "location information";
           field ~in_oss_since:None ~ty:Bool ~qualifier:DynamicRO "managed" "";
           field ~in_oss_since:None ~ty:Bool ~qualifier:DynamicRO "missing" "true if SR scan operation reported this VDI as not present on disk";
           field ~in_oss_since:None ~ty:(Ref _vdi) ~qualifier:DynamicRO ~lifecycle:[Published, rel_rio, ""; Deprecated, rel_ely, "The field was never used."] "parent" "This field is always null. Deprecated";
           field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "xenstore_data" "data to be inserted into the xenstore tree (/local/domain/0/backend/vbd/<domid>/<device-id>/sm-data) after the VDI is attached. This is generally set by the SM backends on vdi_attach." ~default_value:(Some (VMap []));
           field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "sm_config" "SM dependent data" ~default_value:(Some (VMap []));

           field ~in_product_since:rel_orlando ~default_value:(Some (VBool false))          ~qualifier:DynamicRO ~ty:Bool ~doc_tags:[Snapshots] "is_a_snapshot" "true if this is a snapshot.";
           field ~in_product_since:rel_orlando ~default_value:(Some (VRef ""))              ~qualifier:DynamicRO ~ty:(Ref _vdi) ~doc_tags:[Snapshots] "snapshot_of" "Ref pointing to the VDI this snapshot is of.";
           field ~in_product_since:rel_orlando                                              ~qualifier:DynamicRO ~ty:(Set (Ref _vdi)) ~doc_tags:[Snapshots] "snapshots" "List pointing to all the VDIs snapshots.";
           field ~in_product_since:rel_orlando ~default_value:(Some (VDateTime Date.never)) ~qualifier:DynamicRO ~ty:DateTime ~doc_tags:[Snapshots] "snapshot_time" "Date/time when this snapshot was created.";
           field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
           field ~in_product_since:rel_cowley ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "allow_caching" "true if this VDI is to be cached in the local cache SR";
           field ~in_product_since:rel_cowley ~qualifier:DynamicRO ~ty:on_boot ~default_value:(Some (VEnum "persist")) "on_boot" "The behaviour of this VDI on a VM boot";
           field ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:(Ref _pool) ~default_value:(Some (VRef null_ref)) "metadata_of_pool" "The pool whose metadata is contained in this VDI";
           field ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "metadata_latest" "Whether this VDI contains the latest known accessible metadata for the pool";
           field ~lifecycle:[Published, rel_dundee, ""] ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "is_tools_iso" "Whether this VDI is a Tools ISO";
           field ~lifecycle:[Published, rel_inverness, ""] ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "cbt_enabled" "True if changed blocks are tracked for this VDI" ~doc_tags:[Snapshots];
         ])
      ()
end

module VBD = struct
  (** Virtual disk interfaces have a mode parameter: *)
  let mode = Enum ("vbd_mode", [ "RO", "only read-only access will be allowed";
                                 "RW", "read-write access will be allowed" ])

  let type' = Enum ("vbd_type",
                    [ "CD", "VBD will appear to guest as CD";
                      "Disk", "VBD will appear to guest as disk";
                      "Floppy", "VBD will appear as a floppy"])

  let operations =
    Enum ("vbd_operations",
          [ "attach", "Attempting to attach this VBD to a VM";
            "eject", "Attempting to eject the media from this VBD";
            "insert", "Attempting to insert new media into this VBD";
            "plug", "Attempting to hotplug this VBD";
            "unplug", "Attempting to hot unplug this VBD";
            "unplug_force", "Attempting to forcibly unplug this VBD";
            "pause", "Attempting to pause a block device backend";
            "unpause", "Attempting to unpause a block device backend";
          ])

  let eject = call
      ~name:"eject"
      ~in_product_since:rel_rio
      ~doc:"Remove the media from the device and leave it empty"
      ~params:[Ref _vbd, "vbd", "The vbd representing the CDROM-like device"]
      ~errs:[Api_errors.vbd_not_removable_media; Api_errors.vbd_is_empty]
      ~allowed_roles:_R_VM_OP
      ()

  let insert = call
      ~name:"insert"
      ~in_product_since:rel_rio
      ~doc:"Insert new media into the device"
      ~params:[Ref _vbd, "vbd", "The vbd representing the CDROM-like device";
               Ref _vdi, "vdi", "The new VDI to 'insert'"]
      ~errs:[Api_errors.vbd_not_removable_media; Api_errors.vbd_not_empty]
      ~allowed_roles:_R_VM_OP
      ()

  let plug = call
      ~name:"plug"
      ~in_product_since:rel_rio
      ~doc:"Hotplug the specified VBD, dynamically attaching it to the running VM"
      ~params:[Ref _vbd, "self", "The VBD to hotplug"]
      ~allowed_roles:_R_VM_ADMIN
      ()

  let unplug = call
      ~name:"unplug"
      ~in_product_since:rel_rio
      ~doc:"Hot-unplug the specified VBD, dynamically unattaching it from the running VM"
      ~params:[Ref _vbd, "self", "The VBD to hot-unplug"]
      ~errs:[Api_errors.device_detach_rejected; Api_errors.device_already_detached]
      ~allowed_roles:_R_VM_ADMIN
      ()

  let unplug_force = call
      ~name:"unplug_force"
      ~in_product_since:rel_rio
      ~doc:"Forcibly unplug the specified VBD"
      ~params:[Ref _vbd, "self", "The VBD to forcibly unplug"]
      ~allowed_roles:_R_VM_ADMIN
      ()

  let unplug_force_no_safety_check = call
      ~name:"unplug_force_no_safety_check"
      ~doc:"Deprecated: use 'unplug_force' instead. Forcibly unplug \
            the specified VBD without any safety checks. This is an \
            extremely dangerous operation in the general case that \
            can cause guest crashes and data corruption; it should \
            be called with extreme caution. Functionally equivalent \
            with 'unplug_force'."
      ~params:[Ref _vbd, "self", "The VBD to forcibly unplug (no safety checks are applied to test if the device supports surprise-remove)"]
      ~internal_deprecated_since:rel_ely
      ~hide_from_docs:true
      ~in_product_since:rel_symc
      ~allowed_roles:_R_VM_ADMIN
      ()

  let pause = call
      ~name:"pause"
      ~doc:"Stop the backend device servicing requests so that an operation can be performed on the disk (eg live resize, snapshot)"
      ~params:[Ref _vbd, "self", "The VBD to pause"]
      ~hide_from_docs:true
      ~in_product_since:rel_symc
      ~result:(String, "Token to uniquely identify this pause instance, used to match the corresponding unpause") (* new in MR *)
      ~allowed_roles:_R_VM_ADMIN
      ()

  let unpause = call
      ~name:"unpause"
      ~doc:"Restart the backend device after it was paused while an operation was performed on the disk (eg live resize, snapshot)"
      ~versioned_params:
        [{param_type=Ref _vbd; param_name="self"; param_doc="The VBD to unpause"; param_release=miami_symc_release; param_default=None};
         {param_type=String; param_name="token"; param_doc="The token from VBD.pause"; param_release=orlando_release; param_default=Some(VString "")}]
      ~hide_from_docs:true
      ~in_product_since:rel_symc
      ~allowed_roles:_R_VM_ADMIN
      ()

  let assert_attachable = call
      ~name:"assert_attachable"
      ~in_product_since:rel_rio
      ~doc:"Throws an error if this VBD could not be attached to this VM if the VM were running. Intended for debugging."
      ~params:[Ref _vbd, "self", "The VBD to query"]
      ~in_oss_since:None
      ~allowed_roles:_R_VM_ADMIN
      ()

  let set_mode = call
    ~name:"set_mode"
    ~in_product_since:rel_rio
    ~doc:"Sets the mode of the VBD. The power_state of the VM must be halted."
    ~params:[
      Ref _vbd, "self", "Reference to the object";
      mode, "value", "New value to set";
    ]
    ~in_oss_since:None
    ~allowed_roles:_R_VM_ADMIN
    ()

  (** A virtual disk interface *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vbd ~descr:"A virtual block device"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages: [ eject; insert; plug; unplug; unplug_force; unplug_force_no_safety_check; assert_attachable;
                   pause; unpause; set_mode
                 ]
      ~contents:
        ([ uid _vbd;
         ] @ (allowed_and_current_operations operations) @ [
           field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine";
           field ~qualifier:StaticRO ~ty:(Ref _vdi) "VDI" "the virtual disk";

           field ~qualifier:DynamicRO "device" "device seen by the guest e.g. hda1";
           field "userdevice" "user-friendly device name e.g. 0,1,2,etc.";
           field ~ty:Bool "bootable" "true if this VBD is bootable";
           field ~qualifier:StaticRO ~ty:mode "mode" "the mode the VBD should be mounted with";
           field ~ty:type' "type" "how the VBD will appear to the guest (e.g. disk or CD)";
           field ~in_oss_since:None ~in_product_since:rel_miami ~ty:Bool ~default_value:(Some (VBool true))
             "unpluggable" "true if this VBD will support hot-unplug";
           field ~qualifier:DynamicRO ~ty:Bool "storage_lock" "true if a storage level lock was acquired";
           field ~qualifier:StaticRO ~ty:Bool "empty" "if true this represents an empty drive";
           field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "reserved" "true if the VBD is reserved pending a reboot/migrate";
           field ~ty:(Map(String, String)) "other_config" "additional configuration";
         ]
         @ device_status_fields @
         [ namespace ~name:"qos" ~contents:(qos "VBD") (); ] @
         [ field ~qualifier:DynamicRO ~ty:(Ref _vbd_metrics) ~lifecycle: [Removed, rel_tampa, "Disabled in favour of RRDs"] "metrics" "metrics associated with this VBD"; ])
      ()
end

module VBD_metrics = struct
  let t =
    create_obj
      ~lifecycle:
        [ Published, rel_rio, "The metrics associated with a virtual block device"
        ; Removed, rel_tampa, "Disabled in favour of RRD"
        ]
      ~in_db:true
      ~in_oss_since:oss_since_303
      ~persist:PersistNothing
      ~gen_constructor_destructor:false
      ~name:_vbd_metrics
      ~descr:"The metrics associated with a virtual block device"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[] ~contents:
      [ uid _vbd_metrics;
        namespace ~name:"io" ~contents:iobandwidth ();
        field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
        field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
      ()
end

module Crashdump = struct
  let destroy = call
      ~name:"destroy"
      ~in_product_since:rel_rio
      ~doc:"Destroy the specified crashdump"
      ~params:[Ref _crashdump, "self", "The crashdump to destroy"]
      ~allowed_roles:_R_POOL_OP
      ()


  (** A crashdump for a particular VM, stored in a particular VDI *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:(Some rel_inverness) ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_crashdump ~descr:"A VM crashdump"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages: [destroy]
      ~contents:
        ([ uid _crashdump;
           field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine";
           field ~qualifier:StaticRO ~ty:(Ref _vdi) "VDI" "the virtual disk";
           field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
         ])
      ()
end


module Auth = struct
  (** Auth class *)
  let get_subject_identifier = call ~flags:[`Session]
      ~name:"get_subject_identifier"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~params:[
        (*Ref _auth, "auth", "???";*)
        String, "subject_name", "The human-readable subject_name, such as a username or a groupname" ;
      ]
      ~result:(String, "the subject_identifier obtained from the external directory service")
      ~doc:"This call queries the external directory service to obtain the subject_identifier as a string from the human-readable subject_name"
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_subject_information_from_identifier = call ~flags:[`Session]
      ~name:"get_subject_information_from_identifier"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~params:[
        String, "subject_identifier", "A string containing the subject_identifier, unique in the external directory service"
      ]
      ~result:(Map(String,String), "key-value pairs containing at least a key called subject_name")
      ~doc:"This call queries the external directory service to obtain the user information (e.g. username, organization etc) from the specified subject_identifier"
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_group_membership = call ~flags:[`Session]
      ~name:"get_group_membership"
      ~in_oss_since:None
      ~in_product_since:rel_george
      ~params:[
        String, "subject_identifier", "A string containing the subject_identifier, unique in the external directory service"
      ]
      ~result:(Set(String), "set of subject_identifiers that provides the group membership of subject_identifier passed as argument, it contains, recursively, all groups a subject_identifier is member of.")
      ~doc:"This calls queries the external directory service to obtain the transitively-closed set of groups that the the subject_identifier is member of."
      ~allowed_roles:_R_READ_ONLY
      ()

  let t =
    create_obj ~in_db:false ~in_product_since:rel_george ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_auth ~descr:"Management of remote authentication services"
      ~gen_events:false
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_READ_ONLY
      ~messages: [get_subject_identifier;
                  get_subject_information_from_identifier;
                  get_group_membership;]
      ~contents:[]
      ()
end

module Subject = struct
  (** Subject class *)
  let add_to_roles = call ~flags:[`Session]
      ~name:"add_to_roles"
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~params:[
        Ref _subject, "self", "The subject who we want to add the role to";
        Ref _role, "role", "The unique role reference" ;
      ]
      ~doc:"This call adds a new role to a subject"
      ~allowed_roles:_R_POOL_ADMIN
      ()
  let remove_from_roles = call ~flags:[`Session]
      ~name:"remove_from_roles"
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~params:[
        Ref _subject, "self", "The subject from whom we want to remove the role";
        Ref _role, "role", "The unique role reference in the subject's roles field" ;
      ]
      ~doc:"This call removes a role from a subject"
      ~allowed_roles:_R_POOL_ADMIN
      ()
  let get_permissions_name_label = call ~flags:[`Session]
      ~name:"get_permissions_name_label"
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~params:[
        Ref _subject, "self", "The subject whose permissions will be retrieved";
      ]
      ~result:(Set(String), "a list of permission names")
      ~doc:"This call returns a list of permission names given a subject"
      ~allowed_roles:_R_READ_ONLY
      ()
  (* a subject is a user/group that can log in xapi *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_george ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_subject ~descr:"A user or group that can log in xapi"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages: [
        add_to_roles;
        remove_from_roles;
        get_permissions_name_label;
      ]
      ~contents:[uid ~in_oss_since:None _subject;
                 field ~in_product_since:rel_george ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "subject_identifier" "the subject identifier, unique in the external directory service";
                 field ~in_product_since:rel_george ~default_value:(Some (VMap [])) ~qualifier:StaticRO ~ty:(Map(String, String)) "other_config" "additional configuration";
                 (* DynamicRO fields do not show up in the constructor, as it should be because a subject must be created without receiving any roles as a parameter *)
                 field ~in_product_since:rel_midnight_ride ~default_value:(Some (VSet [
                     (VRef ("OpaqueRef:"^Constants.rbac_pool_admin_uuid))])) (* pool-admin, according to rbac_static.ml, used during upgrade from pre-rbac xapis *)
                   ~ignore_foreign_key:true ~qualifier:DynamicRO ~ty:(Set((Ref _role))) "roles" "the roles associated with this subject";
                ]
      ()
end

module Role = struct
  (** Role class *)
  let get_permissions = call ~flags:[`Session]
      ~name:"get_permissions"
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~params:[
        Ref _role, "self", "a reference to a role";
      ]
      ~result:(Set(Ref _role), "a list of permissions")
      ~doc:"This call returns a list of permissions given a role"
      ~allowed_roles:_R_READ_ONLY
      ()
  let get_permissions_name_label = call ~flags:[`Session]
      ~name:"get_permissions_name_label"
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~params:[
        Ref _role, "self", "a reference to a role";
      ]
      ~result:(Set(String), "a list of permission names")
      ~doc:"This call returns a list of permission names given a role"
      ~allowed_roles:_R_READ_ONLY
      ()
  let get_by_permission = call ~flags:[`Session]
      ~name:"get_by_permission"
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~params:[
        Ref _role, "permission", "a reference to a permission" ;
      ]
      ~result:(Set(Ref _role), "a list of references to roles")
      ~doc:"This call returns a list of roles given a permission"
      ~allowed_roles:_R_READ_ONLY
      ()
  let get_by_permission_name_label = call ~flags:[`Session]
      ~name:"get_by_permission_name_label"
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~params:[
        String, "label", "The short friendly name of the role" ;
      ]
      ~result:(Set(Ref _role), "a list of references to roles")
      ~doc:"This call returns a list of roles given a permission name"
      ~allowed_roles:_R_READ_ONLY
      ()

  (* A role defines a set of API call privileges associated with a subject *)
  (* A role is synonymous to permission or privilege *)
  (* A role is a recursive definition: it is either a basic role or it points to a set of roles *)
  (* - full/complete role: is the one meant to be used by the end-user, a root in the tree of roles *)
  (* - basic role: is the 1x1 mapping to each XAPI/HTTP call being protected, a leaf in the tree of roles *)
  (* - intermediate role: an intermediate node in the recursive tree of roles, usually not meant to the end-user *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_midnight_ride ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_role ~descr:"A set of permissions associated with a subject"
      ~gen_events:true
      ~force_custom_actions:(Some(StaticRO)) (* force custom actions for getters *)
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages: [
        get_permissions;
        get_permissions_name_label;
        get_by_permission;
        get_by_permission_name_label;
      ]
      ~contents: [uid ~in_oss_since:None _role;
                  namespace ~name:"name" ~contents:(
                    [
                      field ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "label" "a short user-friendly name for the role";
                      field ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "description" "what this role is for";
                    ]) ();
                  field ~in_product_since:rel_midnight_ride ~default_value:(Some (VSet [])) ~ignore_foreign_key:true ~qualifier:StaticRO ~ty:(Set(Ref _role)) "subroles" "a list of pointers to other roles or permissions";
                  (*RBAC2: field ~in_product_since:rel_midnight_ride ~default_value:(Some (VBool false)) ~qualifier:StaticRO ~ty:Bool "is_complete" "if this is a complete role, meant to be used by the end-user";*)
                 ]
      ()
end


module VTPM = struct
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vtpm ~descr:"A virtual TPM device"
      ~gen_events:false
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[]
      ~contents:
        [ uid _vtpm;
          field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine";
          field ~qualifier:StaticRO ~ty:(Ref _vm) "backend" "the domain where the backend is located" ]
      ()
end

module Console = struct
  (** Console protocols *)
  let protocol = Enum("console_protocol", [
      "vt100", "VT100 terminal";
      "rfb", "Remote FrameBuffer protocol (as used in VNC)";
      "rdp", "Remote Desktop Protocol"
    ])

  (** A virtual console device *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_console ~descr:"A console"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[]  ~contents:
      [ uid _console;
        field ~qualifier:DynamicRO ~ty:protocol "protocol" "the protocol used by this console";
        field ~qualifier:DynamicRO ~ty:String "location" "URI for the console service";
        field ~qualifier:DynamicRO ~ty:(Ref _vm) "VM" "VM to which this console is attached";
        field  ~ty:(Map(String, String)) "other_config" "additional configuration";
        field ~in_oss_since:None ~internal_only:true ~ty:Int "port" "port in dom0 on which the console server is listening";
      ]
      ()
end


module VM_metrics = struct

  let vm_memory_metrics =
    [
      field ~qualifier:DynamicRO ~ty:Int "actual" "Guest's actual memory (bytes)" ~persist:false
    ]

  let vm_vcpu_metrics =
    [
      field ~qualifier:DynamicRO ~ty:Int "number" "Current number of VCPUs" ~persist:true;
      field ~qualifier:DynamicRO ~ty:(Map (Int, Float)) ~persist:false "utilisation" "Utilisation for all of guest's current VCPUs"
        ~lifecycle:[Removed, rel_tampa, "Disabled in favour of RRDs"];
      field ~qualifier:DynamicRO ~ty:(Map (Int, Int)) "CPU" "VCPU to PCPU map" ~persist:false;
      field ~qualifier:DynamicRO ~ty:(Map (String, String)) "params" "The live equivalent to VM.VCPUs_params" ~persist:false;
      field ~qualifier:DynamicRO ~ty:(Map (Int, Set String)) "flags" "CPU flags (blocked,online,running)" ~persist:false;
    ]

  let t =
    create_obj
      ~in_db:true
      ~in_product_since:rel_rio
      ~in_oss_since:oss_since_303
      ~internal_deprecated_since:None
      ~persist:PersistEverything
      ~gen_constructor_destructor:false
      ~name:_vm_metrics
      ~descr:"The metrics associated with a VM"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[]
      ~contents:
        [ uid _vm_metrics
        ; namespace ~name:"memory" ~contents:vm_memory_metrics ()
        ; namespace ~name:"VCPUs" ~contents:vm_vcpu_metrics ()
        ; field ~qualifier:DynamicRO ~ty:(Set (String))
            "state" "The state of the guest, eg blocked, dying etc"
            ~persist:false
        ; field ~qualifier:DynamicRO ~ty:DateTime
            "start_time" "Time at which this VM was last booted"
        ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:DateTime
            "install_time" "Time at which the VM was installed"
        ; field ~qualifier:DynamicRO ~ty:DateTime
            "last_updated" "Time at which this information was last updated"
            ~persist:false
        ; field ~in_product_since:rel_orlando ~default_value:(Some (VMap []))
            ~ty:(Map(String, String))
            "other_config" "additional configuration"
            ~persist:false
        ; field ~in_product_since:rel_ely ~default_value:(Some (VBool false))
            ~ty:Bool ~qualifier:DynamicRO
            "hvm" "hardware virtual machine"
            ~persist:false
        ; field ~in_product_since:rel_ely ~default_value:(Some (VBool false))
            ~ty:Bool ~qualifier:DynamicRO
            "nested_virt" "VM supports nested virtualisation"
            ~persist:false
        ; field ~in_product_since:rel_ely ~default_value:(Some (VBool false))
            ~ty:Bool ~qualifier:DynamicRO
            "nomigrate" "VM is immobile and can't migrate between hosts"
            ~persist:false
        ; field ~lifecycle:[Published, rel_jura, ""] ~default_value:(Some (VEnum "unspecified"))
            ~ty:Datamodel_vm.domain_type ~qualifier:DynamicRO
            "current_domain_type" "The current domain type of the VM (for running,\
             suspended, or paused VMs). The last-known domain type for halted VMs."
        ]
      ()
end

let tristate_type = Enum ("tristate_type",
                          [
                            "yes", "Known to be true";
                            "no", "Known to be false";
                            "unspecified", "Unknown or unspecified";
                          ])

module VM_guest_metrics = struct
  (* Some of this stuff needs to persist (like PV drivers vsns etc.) so we know about what's likely to be in the VM even when it's off.
     Other things don't need to persist, so we specify these on a per-field basis *)
  let t =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_vm_guest_metrics ~descr:"The metrics reported by the guest (as opposed to inferred from outside)"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[] ~contents:
      [ uid _vm_guest_metrics;
        field ~qualifier:DynamicRO ~ty:(Map(String, String)) "os_version" "version of the OS";
        field ~qualifier:DynamicRO ~ty:(Map(String, String)) "PV_drivers_version"
          "version of the PV drivers";
        field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None
          ~lifecycle:[
            Published, rel_rio, "true if the PV drivers appear to be up to date";
            Deprecated, rel_dundee, "Deprecated in favour of PV_drivers_detected, and redefined in terms of it"
          ]
          "PV_drivers_up_to_date" "Logically equivalent to PV_drivers_detected";
        field ~qualifier:DynamicRO ~ty:(Map(String, String))
          ~lifecycle:[
            Published, rel_rio, "free/used/total";
            Removed, rel_george, "Disabled in favour of the RRDs, to improve scalability"
          ]
          "memory" "This field exists but has no data. Use the memory and memory_internal_free RRD data-sources instead.";
        field ~qualifier:DynamicRO ~ty:(Map(String, String))
          ~lifecycle:[
            Published, rel_rio, "Disk configuration/free space";
            Removed, rel_orlando, "No data"
          ]
          "disks" "This field exists but has no data.";
        field ~qualifier:DynamicRO ~ty:(Map(String, String)) "networks" "network configuration";
        field ~qualifier:DynamicRO ~ty:(Map(String, String)) "other" "anything else";
        field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
        field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
        field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VBool false)) ~ty:Bool "live" "True if the guest is sending heartbeat messages via the guest agent";
        field ~qualifier:DynamicRO ~lifecycle:[Published, rel_dundee, "To be used where relevant and available instead of checking PV driver version."] ~ty:tristate_type ~default_value:(Some (VEnum "unspecified")) "can_use_hotplug_vbd" "The guest's statement of whether it supports VBD hotplug, i.e. whether it is capable of responding immediately to instantiation of a new VBD by bringing online a new PV block device. If the guest states that it is not capable, then the VBD plug and unplug operations will not be allowed while the guest is running.";
        field ~qualifier:DynamicRO ~lifecycle:[Published, rel_dundee, "To be used where relevant and available instead of checking PV driver version."] ~ty:tristate_type ~default_value:(Some (VEnum "unspecified")) "can_use_hotplug_vif" "The guest's statement of whether it supports VIF hotplug, i.e. whether it is capable of responding immediately to instantiation of a new VIF by bringing online a new PV network device. If the guest states that it is not capable, then the VIF plug and unplug operations will not be allowed while the guest is running.";
        field ~qualifier:DynamicRO ~lifecycle:[Published, rel_dundee, ""] ~ty:Bool ~default_value:(Some (VBool false)) "PV_drivers_detected" "At least one of the guest's devices has successfully connected to the backend.";
      ]
      ()
end

module VMPP = struct
  (* VM protection policy *)
  let removed = [
    Published, rel_cowley, "";
    Removed, rel_clearwater, "The VMPR feature was removed";
  ]
  let protect_now = call ~flags:[`Session]
      ~name:"protect_now"
      ~lifecycle:removed
      ~params:[Ref _vmpp, "vmpp", "The protection policy to execute";]
      ~doc:"This call executes the protection policy immediately"
      ~allowed_roles:_R_POOL_OP
      ~result:(String, "An XMLRPC result")
      ()
  let archive_now = call ~flags:[`Session]
      ~name:"archive_now"
      ~lifecycle:removed
      ~params:[Ref _vm, "snapshot", "The snapshot to archive";]
      ~doc:"This call archives the snapshot provided as a parameter"
      ~allowed_roles:_R_VM_POWER_ADMIN
      ~result:(String, "An XMLRPC result")
      ()
  let create_alert = call ~flags:[`Session]
      ~name:"create_alert"
      ~lifecycle:removed
      ~params:[Ref _vmpp, "vmpp", "The protection policy where the alert should be created";
               String, "name", "The name of the message";
               Int, "priority", "The priority of the message";
               String, "body", "The body of the email message";
               String, "data", "The data in xml";
              ]
      ~doc:"This call creates an alert for some protection policy"
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~hide_from_docs:true
      ()
  let get_alerts = call ~flags:[`Session]
      ~name:"get_alerts"
      ~lifecycle:removed
      ~params:[Ref _vmpp, "vmpp", "The protection policy";
               Int, "hours_from_now", "how many hours in the past the oldest record to fetch is";
              ]
      ~doc:"This call fetches a history of alerts for a given protection policy"
      ~allowed_roles:_R_POOL_OP
      ~result:(Set (String), "A list of alerts encoded in xml")
      ()
  let backup_type = Enum ("vmpp_backup_type",
                          [
                            "snapshot", "The backup is a snapshot";
                            "checkpoint", "The backup is a checkpoint";
                          ])
  let backup_frequency = Enum ("vmpp_backup_frequency",
                               [
                                 "hourly", "Hourly backups";
                                 "daily", "Daily backups";
                                 "weekly", "Weekly backups";
                               ])
  let archive_frequency = Enum ("vmpp_archive_frequency",
                                [
                                  "never", "Never archive";
                                  "always_after_backup", "Archive after backup";
                                  "daily", "Daily archives";
                                  "weekly", "Weekly backups";
                                ])
  let archive_target_type = Enum ("vmpp_archive_target_type",
                                  [
                                    "none", "No target config";
                                    "cifs", "CIFS target config";
                                    "nfs", "NFS target config";
                                  ])
  let schedule_min = "min"
  let schedule_hour = "hour"
  let schedule_days = "days"
  let archive_target_config_location = "location"
  let archive_target_config_username = "username"
  let archive_target_config_password = "password"
  let set_backup_retention_value = call ~flags:[`Session]
      ~name:"set_backup_retention_value"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        Int, "value", "the value to set"
      ]
      ()
  let set_is_backup_running = call ~flags:[`Session]
      ~name:"set_is_backup_running"
      ~lifecycle:removed
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        Bool, "value", "true to mark this protection policy's backup is running"
      ]
      ~doc:"Set the value of the is_backup_running field"
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~hide_from_docs:true
      ()
  let set_is_archive_running = call ~flags:[`Session]
      ~name:"set_is_archive_running"
      ~lifecycle:removed
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        Bool, "value", "true to mark this protection policy's archive is running"
      ]
      ~doc:"Set the value of the is_archive_running field"
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~hide_from_docs:true
      ()
  let set_is_alarm_enabled = call ~flags:[`Session]
      ~name:"set_is_alarm_enabled"
      ~lifecycle:removed
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        Bool, "value", "true if alarm is enabled for this policy"
      ]
      ~doc:"Set the value of the is_alarm_enabled field"
      ~allowed_roles:_R_POOL_OP
      ()
  let set_archive_frequency = call ~flags:[`Session]
      ~name:"set_archive_frequency"
      ~lifecycle:removed
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        archive_frequency, "value", "the archive frequency"
      ]
      ~doc:"Set the value of the archive_frequency field"
      ~allowed_roles:_R_POOL_OP
      ()
  let set_archive_target_type = call ~flags:[`Session]
      ~name:"set_archive_target_type"
      ~lifecycle:removed
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        archive_target_type, "value", "the archive target config type"
      ]
      ~doc:"Set the value of the archive_target_config_type field"
      ~allowed_roles:_R_POOL_OP
      ()
  let set_backup_frequency = call ~flags:[`Session]
      ~name:"set_backup_frequency"
      ~lifecycle:removed
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        backup_frequency, "value", "the backup frequency"
      ]
      ~doc:"Set the value of the backup_frequency field"
      ~allowed_roles:_R_POOL_OP
      ()
  let set_backup_schedule = call ~flags:[`Session]
      ~name:"set_backup_schedule"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        Map(String,String), "value", "the value to set"
      ]
      ()
  let set_archive_target_config = call ~flags:[`Session]
      ~name:"set_archive_target_config"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        Map(String,String), "value", "the value to set"
      ]
      ()
  let set_archive_schedule = call ~flags:[`Session]
      ~name:"set_archive_schedule"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        Map(String,String), "value", "the value to set"
      ]
      ()
  let set_alarm_config = call ~flags:[`Session]
      ~name:"set_alarm_config"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        Map(String,String), "value", "the value to set"
      ]
      ()
  let set_backup_last_run_time = call ~flags:[`Session]
      ~name:"set_backup_last_run_time"
      ~lifecycle:removed
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        DateTime, "value", "the value to set"
      ]
      ()
  let set_archive_last_run_time = call ~flags:[`Session]
      ~name:"set_archive_last_run_time"
      ~lifecycle:removed
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        DateTime, "value", "the value to set"
      ]
      ()
  let add_to_backup_schedule = call ~flags:[`Session]
      ~name:"add_to_backup_schedule"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        String, "key", "the key to add";
        String, "value", "the value to add";
      ]
      ()
  let add_to_archive_target_config = call ~flags:[`Session]
      ~name:"add_to_archive_target_config"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        String, "key", "the key to add";
        String, "value", "the value to add";
      ]
      ()
  let add_to_archive_schedule = call ~flags:[`Session]
      ~name:"add_to_archive_schedule"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        String, "key", "the key to add";
        String, "value", "the value to add";
      ]
      ()
  let add_to_alarm_config = call ~flags:[`Session]
      ~name:"add_to_alarm_config"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        String, "key", "the key to add";
        String, "value", "the value to add";
      ]
      ()
  let remove_from_backup_schedule = call ~flags:[`Session]
      ~name:"remove_from_backup_schedule"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        String, "key", "the key to remove";
      ]
      ()
  let remove_from_archive_target_config = call ~flags:[`Session]
      ~name:"remove_from_archive_target_config"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        String, "key", "the key to remove";
      ]
      ()
  let remove_from_archive_schedule = call ~flags:[`Session]
      ~name:"remove_from_archive_schedule"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        String, "key", "the key to remove";
      ]
      ()
  let remove_from_alarm_config = call ~flags:[`Session]
      ~name:"remove_from_alarm_config"
      ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmpp, "self", "The protection policy";
        String, "key", "the key to remove";
      ]
      ()
  let t =
    create_obj ~in_db:true ~in_oss_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vmpp ~descr:"VM Protection Policy"
      ~gen_events:true
      ~lifecycle:removed
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[
        protect_now;
        archive_now;
        create_alert;
        get_alerts;
        set_backup_retention_value;
        set_is_backup_running;
        set_is_archive_running;
        set_backup_frequency;
        set_backup_schedule;
        set_archive_frequency;
        set_archive_schedule;
        set_archive_target_type;
        set_archive_target_config;
        set_is_alarm_enabled;
        set_alarm_config;
        add_to_backup_schedule;
        add_to_archive_target_config;
        add_to_archive_schedule;
        add_to_alarm_config;
        remove_from_backup_schedule;
        remove_from_archive_target_config;
        remove_from_archive_schedule;
        remove_from_alarm_config;
        set_backup_last_run_time;
        set_archive_last_run_time;
      ]
      ~contents:[
        uid ~lifecycle:removed _vmpp;
        namespace ~name:"name" ~contents:(names None RW) ();
        field ~lifecycle:removed ~qualifier:RW ~ty:Bool "is_policy_enabled" "enable or disable this policy" ~default_value:(Some (VBool true));
        field ~lifecycle:removed ~qualifier:RW ~ty:backup_type "backup_type" "type of the backup sub-policy" ~default_value:(Some (VEnum "snapshot"));
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:Int "backup_retention_value" "maximum number of backups that should be stored at any time" ~default_value:(Some (VInt 7L));
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:backup_frequency "backup_frequency" "frequency of the backup schedule" ~default_value:(Some (VEnum "daily"));
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:(Map (String,String)) "backup_schedule" "schedule of the backup containing 'hour', 'min', 'days'. Date/time-related information is in Local Timezone" ~default_value:(Some (VMap []));
        field ~lifecycle:removed ~qualifier:DynamicRO ~ty:Bool "is_backup_running" "true if this protection policy's backup is running";
        field ~lifecycle:removed ~qualifier:DynamicRO ~ty:DateTime "backup_last_run_time" "time of the last backup" ~default_value:(Some(VDateTime(Date.of_float 0.)));
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:archive_target_type "archive_target_type" "type of the archive target config" ~default_value:(Some (VEnum "none"));
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:(Map (String,String)) "archive_target_config" "configuration for the archive, including its 'location', 'username', 'password'" ~default_value:(Some (VMap []));
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:archive_frequency "archive_frequency" "frequency of the archive schedule" ~default_value:(Some (VEnum "never"));
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:(Map (String,String)) "archive_schedule" "schedule of the archive containing 'hour', 'min', 'days'. Date/time-related information is in Local Timezone" ~default_value:(Some (VMap []));
        field ~lifecycle:removed ~qualifier:DynamicRO ~ty:Bool "is_archive_running" "true if this protection policy's archive is running";
        field ~lifecycle:removed ~qualifier:DynamicRO ~ty:DateTime "archive_last_run_time" "time of the last archive" ~default_value:(Some(VDateTime(Date.of_float 0.)));
        field ~lifecycle:removed ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) "VMs" "all VMs attached to this protection policy";
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:Bool "is_alarm_enabled" "true if alarm is enabled for this policy" ~default_value:(Some (VBool false));
        field ~lifecycle:removed ~qualifier:StaticRO ~ty:(Map (String,String)) "alarm_config" "configuration for the alarm" ~default_value:(Some (VMap []));
        field ~lifecycle:removed ~qualifier:DynamicRO ~ty:(Set (String)) "recent_alerts" "recent alerts" ~default_value:(Some (VSet []));
      ]
      ()
end

module VMSS = struct

  (* VM schedule snapshot *)
  let snapshot_now = call ~flags:[`Session]
      ~name:"snapshot_now"
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~params:[Ref _vmss, "vmss", "Snapshot Schedule to execute";]
      ~doc:"This call executes the snapshot schedule immediately"
      ~allowed_roles:_R_POOL_OP
      ~result:(String, "An XMLRPC result")
      ()

  let type' = Enum ("vmss_type",
                    [
                      "snapshot", "The snapshot is a disk snapshot";
                      "checkpoint", "The snapshot is a checkpoint";
                      "snapshot_with_quiesce", "The snapshot is a VSS";
                    ])

  let frequency = Enum ("vmss_frequency",
                        [
                          "hourly", "Hourly snapshots";
                          "daily", "Daily snapshots";
                          "weekly", "Weekly snapshots";
                        ])

  let schedule_min = "min"
  let schedule_hour = "hour"
  let schedule_days = "days"

  let set_retained_snapshots = call ~flags:[`Session]
      ~name:"set_retained_snapshots"
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmss, "self", "The schedule snapshot";
        Int, "value", "the value to set"
      ]
      ()

  let set_frequency = call ~flags:[`Session]
      ~name:"set_frequency"
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~params:[
        Ref _vmss, "self", "The snapshot schedule";
        frequency, "value", "the snapshot schedule frequency"
      ]
      ~doc:"Set the value of the frequency field"
      ~allowed_roles:_R_POOL_OP
      ()

  let set_schedule = call ~flags:[`Session]
      ~name:"set_schedule"
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmss, "self", "The snapshot schedule";
        Map(String,String), "value", "the value to set"
      ]
      ()

  let set_last_run_time = call ~flags:[`Session]
      ~name:"set_last_run_time"
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~params:[
        Ref _vmss, "self", "The snapshot schedule";
        DateTime, "value", "the value to set"
      ]
      ()

  let add_to_schedule = call ~flags:[`Session]
      ~name:"add_to_schedule"
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmss, "self", "The snapshot schedule";
        String, "key", "the key to add";
        String, "value", "the value to add";
      ]
      ()

  let remove_from_schedule = call ~flags:[`Session]
      ~name:"remove_from_schedule"
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmss, "self", "The snapshot schedule";
        String, "key", "the key to remove";
      ]
      ()

<<<<<<< HEAD
  let set_type = call ~flags:[`Session]
      ~name:"set_type"
      ~in_oss_since:None
      ~in_product_since:rel_falcon
      ~allowed_roles:_R_POOL_OP
      ~params:[
        Ref _vmss, "self", "The snapshot schedule";
        type', "value", "the snapshot schedule type"
=======
let host_certificate_uninstall = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~pool_internal:true
    ~hide_from_docs:true
    ~name:"certificate_uninstall"
    ~doc:"Remove an SSL certificate from this host."
    ~params:[Ref _host, "host", "The host";
             String, "name", "The certificate name"]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let host_certificate_list = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~pool_internal:true
    ~hide_from_docs:true
    ~name:"certificate_list"
    ~doc:"List all installed SSL certificates."
    ~params:[Ref _host, "host", "The host"]
    ~result:(Set(String),"All installed certificates")
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let host_crl_install = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~pool_internal:true
    ~hide_from_docs:true
    ~name:"crl_install"
    ~doc:"Install an SSL certificate revocation list to this host."
    ~params:[Ref _host, "host", "The host";
             String, "name", "A name to give the CRL";
             String, "crl", "The CRL"]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let host_crl_uninstall = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~pool_internal:true
    ~hide_from_docs:true
    ~name:"crl_uninstall"
    ~doc:"Remove an SSL certificate revocation list from this host."
    ~params:[Ref _host, "host", "The host";
             String, "name", "The CRL name"]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let host_crl_list = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~pool_internal:true
    ~hide_from_docs:true
    ~name:"crl_list"
    ~doc:"List all installed SSL certificate revocation lists."
    ~params:[Ref _host, "host", "The host"]
    ~result:(Set(String),"All installed CRLs")
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let host_certificate_sync = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~pool_internal:true
    ~hide_from_docs:true
    ~name:"certificate_sync"
    ~doc:"Resync installed SSL certificates and CRLs."
    ~params:[Ref _host, "host", "The host"]
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let host_get_server_certificate = call
    ~in_oss_since:None
    ~lifecycle:[Published, rel_george, ""; Changed, rel_inverness, "Now available to all RBAC roles."]
    ~name:"get_server_certificate"
    ~doc:"Get the installed server public TLS certificate."
    ~params:[Ref _host, "host", "The host"]
    ~result:(String,"The installed server public TLS certificate, in PEM form.")
    ~allowed_roles:_R_READ_ONLY
    ()

let host_display =
  Enum ("host_display", [
      "enabled", "This host is outputting its console to a physical display device";
      "disable_on_reboot", "The host will stop outputting its console to a physical display device on next boot";
      "disabled", "This host is not outputting its console to a physical display device";
      "enable_on_reboot", "The host will start outputting its console to a physical display device on next boot";
    ])

let host_operations =
  Enum ("host_allowed_operations",
        [ "provision", "Indicates this host is able to provision another VM";
          "evacuate", "Indicates this host is evacuating";
          "shutdown", "Indicates this host is in the process of shutting itself down";
          "reboot", "Indicates this host is in the process of rebooting";
          "power_on", "Indicates this host is in the process of being powered on";
          "vm_start", "This host is starting a VM";
          "vm_resume", "This host is resuming a VM";
          "vm_migrate", "This host is the migration target of a VM";
        ])

let host_enable_external_auth = call ~flags:[`Session]
    ~name:"enable_external_auth"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~params:[
      Ref _host, "host", "The host whose external authentication should be enabled";
      Map (String,String), "config", "A list of key-values containing the configuration data" ;
      String, "service_name", "The name of the service" ;
      String, "auth_type", "The type of authentication (e.g. AD for Active Directory)"
    ]
    ~doc:"This call enables external authentication on a host"
    ~allowed_roles:_R_POOL_ADMIN
    ()

let host_disable_external_auth = call ~flags:[`Session]
    ~name:"disable_external_auth"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~versioned_params:[
      {param_type=Ref _host; param_name="host"; param_doc="The host whose external authentication should be disabled"; param_release=george_release; param_default=None};
      {param_type=Map (String, String); param_name="config"; param_doc="Optional parameters as a list of key-values containing the configuration data"; param_release=george_release; param_default=Some (VMap [])}
    ]
    ~doc:"This call disables external authentication on the local host"
    ~allowed_roles:_R_POOL_ADMIN
    ()

let host_set_license_params = call
    ~name:"set_license_params"
    ~in_product_since:rel_orlando (* actually update 3 aka floodgate *)
    ~doc:"Set the new license details in the database, trigger a recomputation of the pool SKU"
    ~params:[
      Ref _host, "self", "The host";
      Map(String, String), "value", "The license_params"
    ]
    ~hide_from_docs:true
    ~pool_internal:true
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let host_apply_edition = call ~flags:[`Session]
    ~name:"apply_edition"
    ~in_product_since:rel_midnight_ride
    ~doc:"Change to another edition, or reactivate the current edition after a license has expired. This may be subject to the successful checkout of an appropriate license."
    ~versioned_params:[
      {param_type=Ref _host; param_name="host"; param_doc="The host"; param_release=midnight_ride_release; param_default=None};
      {param_type=String; param_name="edition"; param_doc="The requested edition"; param_release=midnight_ride_release; param_default=None};
      {param_type=Bool; param_name="force"; param_doc="Update the license params even if the apply call fails"; param_release=clearwater_release; param_default=Some (VBool false)};
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let host_set_power_on_mode = call
    ~name:"set_power_on_mode"
    ~in_product_since:rel_midnight_ride
    ~doc:"Set the power-on-mode, host, user and password "
    ~params:[
      Ref _host, "self", "The host";
      String, "power_on_mode", "power-on-mode can be empty,iLO,wake-on-lan, DRAC or other";
      Map(String, String), "power_on_config", "Power on config";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let host_set_ssl_legacy = call
    ~name:"set_ssl_legacy"
    ~lifecycle:[Published, rel_dundee, ""]
    ~doc:"Enable/disable SSLv3 for interoperability with older versions of XenServer. When this is set to a different value, the host immediately restarts its SSL/TLS listening service; typically this takes less than a second but existing connections to it will be broken. XenAPI login sessions will remain valid."
    ~params:[
      Ref _host, "self", "The host";
      Bool, "value", "True to allow SSLv3 and ciphersuites as used in old XenServer versions";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let host_set_cpu_features = call ~flags:[`Session]
    ~name:"set_cpu_features"
    ~in_product_since:rel_midnight_ride
    ~doc:"Set the CPU features to be used after a reboot, if the given features string is valid."
    ~params:[
      Ref _host, "host", "The host";
      String, "features", "The features string (32 hexadecimal digits)"
    ]
    ~allowed_roles:_R_POOL_OP
    ~lifecycle:[Published, rel_midnight_ride, ""; Removed, rel_dundee, "Manual CPU feature setting was removed"]
    ()

let host_reset_cpu_features = call ~flags:[`Session]
    ~name:"reset_cpu_features"
    ~in_product_since:rel_midnight_ride
    ~doc:"Remove the feature mask, such that after a reboot all features of the CPU are enabled."
    ~params:[
      Ref _host, "host", "The host"
    ]
    ~allowed_roles:_R_POOL_OP
    ~lifecycle:[Published, rel_midnight_ride, ""; Removed, rel_dundee, "Manual CPU feature setting was removed"]
    ()

let host_reset_networking = call
    ~name:"reset_networking"
    ~lifecycle:[]
    ~doc:"Purge all network-related metadata associated with the given host."
    ~params:[Ref _host, "host", "The Host to modify"]
    ~allowed_roles:_R_POOL_OP
    ~hide_from_docs:true
    ()

let host_enable_local_storage_caching = call ~flags:[`Session]
    ~name:"enable_local_storage_caching"
    ~in_product_since:rel_cowley
    ~doc:"Enable the use of a local SR for caching purposes"
    ~params:[
      Ref _host, "host", "The host";
      Ref _sr, "sr", "The SR to use as a local cache"
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let host_disable_local_storage_caching = call ~flags:[`Session]
    ~name:"disable_local_storage_caching"
    ~in_product_since:rel_cowley
    ~doc:"Disable the use of a local SR for caching purposes"
    ~params:[
      Ref _host, "host", "The host"
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let host_get_sm_diagnostics = call ~flags:[`Session]
    ~name:"get_sm_diagnostics"
    ~in_product_since:rel_boston
    ~doc:"Return live SM diagnostics"
    ~params:[
      Ref _host, "host", "The host"
    ]
    ~result:(String, "Printable diagnostic data")
    ~allowed_roles:_R_POOL_OP
    ~hide_from_docs:true
    ()

let host_get_thread_diagnostics = call ~flags:[`Session]
    ~name:"get_thread_diagnostics"
    ~in_product_since:rel_boston
    ~doc:"Return live thread diagnostics"
    ~params:[
      Ref _host, "host", "The host"
    ]
    ~result:(String, "Printable diagnostic data")
    ~allowed_roles:_R_POOL_OP
    ~hide_from_docs:true
    ()

let host_sm_dp_destroy = call ~flags:[`Session]
    ~name:"sm_dp_destroy"
    ~in_product_since:rel_boston
    ~doc:"Attempt to cleanup and destroy a named SM datapath"
    ~params:[
      Ref _host, "host", "The host";
      String, "dp", "The datapath";
      Bool, "allow_leak", "If true, all records of the datapath will be removed even if the datapath could not be destroyed cleanly.";
    ]
    ~allowed_roles:_R_POOL_OP
    ~hide_from_docs:true
    ()

let host_sync_vlans = call ~flags:[`Session]
    ~name:"sync_vlans"
    ~lifecycle:[]
    ~doc:"Synchronise VLANs on given host with the master's VLANs"
    ~params:[
      Ref _host, "host", "The host";
    ]
    ~hide_from_docs:true
    ~pool_internal:true
    ~allowed_roles:_R_POOL_OP
    ()

let host_sync_tunnels = call ~flags:[`Session]
    ~name:"sync_tunnels"
    ~lifecycle:[]
    ~doc:"Synchronise tunnels on given host with the master's tunnels"
    ~params:[
      Ref _host, "host", "The host";
    ]
    ~hide_from_docs:true
    ~pool_internal:true
    ~allowed_roles:_R_POOL_OP
    ()

let host_sync_network_sriovs = call ~flags:[`Session]
    ~name:"sync_network_sriovs"
    ~lifecycle:[Published, rel_kolkata, ""]
    ~doc:"Synchronise network sriovs on given host with the master's network sriovs"
    ~params:[
      Ref _host, "host", "The host";
    ]
    ~hide_from_docs:true
    ~pool_internal:true
    ~allowed_roles:_R_POOL_OP
    ()

let host_sync_pif_currently_attached = call ~flags:[`Session]
    ~name:"sync_pif_currently_attached"
    ~lifecycle:[]
    ~doc:"Synchronise tunnels on given host with the master's tunnels"
    ~params:[
      Ref _host, "host", "The host";
      Set String, "bridges", "A list of bridges that are currently up";
    ]
    ~hide_from_docs:true
    ~pool_internal:true
    ~allowed_roles:_R_POOL_OP
    ()

let host_enable_display = call
    ~name:"enable_display"
    ~lifecycle:[Published, rel_cream, ""]
    ~doc:"Enable console output to the physical display device next time this host boots"
    ~params:[
      Ref _host, "host", "The host";
    ]
    ~result:(host_display, "This host's physical display usage")
    ~allowed_roles:_R_POOL_OP
    ()

let host_disable_display = call
    ~name:"disable_display"
    ~lifecycle:[Published, rel_cream, ""]
    ~doc:"Disable console output to the physical display device next time this host boots"
    ~params:[
      Ref _host, "host", "The host";
    ]
    ~result:(host_display, "This host's physical display usage")
    ~allowed_roles:_R_POOL_OP
    ()

let host_apply_guest_agent_config = call
    ~name:"apply_guest_agent_config"
    ~lifecycle:[Published, rel_dundee, ""]
    ~doc:"Signal to the host that the pool-wide guest agent config has changed"
    ~params:[
      Ref _host, "host", "The host";
    ]
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_ADMIN
    ()

let host_mxgpu_vf_setup = call
    ~name:"mxgpu_vf_setup"
    ~lifecycle:[Published, rel_falcon, ""]
    ~doc:"Ensure the driver (kernel module) for MxGPU is loaded on the host, and create PCI objects for any new PCI devices (virtual functions) that the module makes visible."
    ~params:[
      Ref _host, "host", "The host";
    ]
    ~hide_from_docs:true
    ~pool_internal:true
    ~allowed_roles:_R_VM_OP
    ()

let host_allocate_resources_for_vm = call
    ~name:"allocate_resources_for_vm"
    ~lifecycle:[Published, rel_inverness, ""]
    ~doc:"Reserves the resources for a VM by setting the 'scheduled_to_be_resident_on' fields"
    ~params:[
      Ref _host, "self", "The host";
      Ref _vm, "vm", "The VM";
      Bool, "live", "Is this part of a live migration?"
    ]
    ~hide_from_docs:true
    ~allowed_roles:_R_VM_OP
    ()

(** Hosts *)
let host =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host ~descr:"A physical host" ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages: [host_disable; host_enable; host_shutdown; host_reboot; host_dmesg; host_dmesg_clear; host_get_log; host_send_debug_keys; host_bugreport_upload; host_list_methods; host_license_apply; host_license_add; host_license_remove; host_create; host_destroy;
                host_power_on;
                host_set_license_params;
                host_emergency_ha_disable;
                host_ha_disarm_fencing; host_preconfigure_ha; host_ha_join_liveset;
                host_ha_disable_failover_decisions;
                host_ha_wait_for_shutdown_via_statefile;
                host_ha_stop_daemon;
                host_ha_release_resources;
                host_ha_xapi_healthcheck;
                host_local_assert_healthy;
                host_request_backup;
                host_request_config_file_sync;
                host_propose_new_master; host_commit_new_master; host_abort_new_master;
                host_get_data_sources;
                host_record_data_source;
                host_query_data_source;
                host_forget_data_source_archives;
                host_assert_can_evacuate;
                host_get_vms_which_prevent_evacuation;
                host_get_uncooperative_resident_VMs;
                host_get_uncooperative_domains;
                host_evacuate;
                host_signal_networking_change;
                host_notify;
                host_syslog_reconfigure;
                host_management_reconfigure;
                host_local_management_reconfigure;
                host_management_disable;
                host_get_management_interface;
                host_get_system_status_capabilities;
                host_get_diagnostic_timing_stats;
                host_restart_agent;
                host_shutdown_agent;
                host_set_hostname_live;
                host_is_in_emergency_mode;
                host_compute_free_memory;
                host_compute_memory_overhead;
                host_tickle_heartbeat;
                host_sync_data;
                host_backup_rrds;
                host_create_new_blob;
                host_call_plugin;
                host_has_extension;
                host_call_extension;
                host_get_servertime;
                host_get_server_localtime;
                host_enable_binary_storage;
                host_disable_binary_storage;
                host_enable_external_auth;
                host_disable_external_auth;
                host_retrieve_wlb_evacuate_recommendations;
                host_certificate_install;
                host_certificate_uninstall;
                host_certificate_list;
                host_crl_install;
                host_crl_uninstall;
                host_crl_list;
                host_certificate_sync;
                host_get_server_certificate;
                host_update_pool_secret;
                host_update_master;
                host_attach_static_vdis;
                host_detach_static_vdis;
                host_set_localdb_key;
                host_apply_edition;
                host_refresh_pack_info;
                host_set_power_on_mode;
                host_set_cpu_features;
                host_reset_cpu_features;
                host_reset_networking;
                host_enable_local_storage_caching;
                host_disable_local_storage_caching;
                host_get_sm_diagnostics;
                host_get_thread_diagnostics;
                host_sm_dp_destroy;
                host_sync_vlans;
                host_sync_tunnels;
                host_sync_network_sriovs;
                host_sync_pif_currently_attached;
                host_migrate_receive;
                host_declare_dead;
                host_enable_display;
                host_disable_display;
                host_set_ssl_legacy;
                host_apply_guest_agent_config;
                host_mxgpu_vf_setup;
                host_allocate_resources_for_vm;
               ]
    ~contents:
      ([ uid _host;
         namespace ~name:"name" ~contents:(names None RW) ();
         namespace ~name:"memory" ~contents:host_memory ();
       ] @ (allowed_and_current_operations host_operations) @ [
         namespace ~name:"API_version" ~contents:api_version ();
         field ~qualifier:DynamicRO ~ty:Bool "enabled" "True if the host is currently enabled";
         field ~qualifier:StaticRO ~ty:(Map(String, String)) "software_version" "version strings";
         field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
         field ~qualifier:StaticRO ~ty:(Set(String)) "capabilities" "Xen capabilities";
         field ~qualifier:DynamicRO ~ty:(Map(String, String)) "cpu_configuration" "The CPU configuration on this host.  May contain keys such as \"nr_nodes\", \"sockets_per_node\", \"cores_per_socket\", or \"threads_per_core\"";
         field ~qualifier:DynamicRO ~ty:String "sched_policy" "Scheduler policy currently in force on this host";
         field ~qualifier:DynamicRO ~ty:(Set String) "supported_bootloaders" "a list of the bootloaders installed on the machine";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) "resident_VMs" "list of VMs currently resident on host";
         field ~qualifier:RW ~ty:(Map(String, String)) "logging" "logging configuration";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _pif)) ~doc_tags:[Networking] "PIFs" "physical network interfaces";
         field ~qualifier:RW ~ty:(Ref _sr) "suspend_image_sr" "The SR in which VDIs for suspend images are created";
         field ~qualifier:RW ~ty:(Ref _sr) "crash_dump_sr" "The SR in which VDIs for crash dumps are created";
         field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host_crashdump)) "crashdumps" "Set of host crash dumps";
         field ~in_oss_since:None ~internal_deprecated_since:rel_ely ~qualifier:DynamicRO ~ty:(Set (Ref _host_patch)) "patches" "Set of host patches";
         field ~in_oss_since:None ~in_product_since:rel_ely ~qualifier:DynamicRO ~ty:(Set (Ref _pool_update)) "updates" "Set of updates";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _pbd)) "PBDs" "physical blockdevices";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _hostcpu)) "host_CPUs" "The physical CPUs on this host";
         field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "cpu_info" "Details about the physical CPUs on this host";
         field ~in_oss_since:None ~qualifier:RW ~ty:String ~doc_tags:[Networking] "hostname" "The hostname of this host";
         field ~in_oss_since:None ~qualifier:RW ~ty:String ~doc_tags:[Networking] "address" "The address by which this host can be contacted from any other host in the pool";
         field ~qualifier:DynamicRO ~ty:(Ref _host_metrics) "metrics" "metrics associated with this host";
         field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map (String,String)) "license_params" "State of the current license";
         field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Int "boot_free_mem" "Free memory on host at boot time";
         field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_statefiles" "The set of statefiles accessible from this host";
         field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_network_peers" "The set of hosts visible via the network from this host";
         field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String,Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this host";
         field ~writer_roles:_R_VM_OP ~qualifier:RW ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
         field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VString "")) ~ty:String "external_auth_type" "type of external authentication service configured; empty if none configured.";
         field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VString "")) ~ty:String "external_auth_service_name" "name of external authentication service configured; empty if none configured.";
         field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VMap [])) ~ty:(Map (String,String)) "external_auth_configuration" "configuration specific to external authentication service";
         field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~ty:String "edition" "Product edition";
         field ~qualifier:RW ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [VString "address", VString "localhost"; VString "port", VString "27000"])) ~ty:(Map (String, String)) "license_server" "Contact information of the license server";
         field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map (String,String)) "bios_strings" "BIOS strings";
         field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~ty:String "power_on_mode" "The power on mode";
         field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "power_on_config" "The power on config";
         field ~qualifier:StaticRO ~in_product_since:rel_cowley ~default_value:(Some (VRef null_ref)) ~ty:(Ref _sr) "local_cache_sr" "The SR that is used as a local cache";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Map (String, String)) ~default_value:(Some (VMap []))
           "chipset_info" "Information about chipset features";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _pci)) "PCIs" "List of PCI devices in the host";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _pgpu)) "PGPUs" "List of physical GPUs in the host";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_inverness, ""] ~ty:(Set (Ref _pusb)) "PUSBs" "List of physical USBs in the host";
         field ~qualifier:StaticRO ~lifecycle:[Published, rel_dundee, ""] ~ty:Bool ~default_value:(Some (VBool true)) "ssl_legacy" "Allow SSLv3 protocol and ciphersuites as used by older XenServers. This controls both incoming and outgoing connections. When this is set to a different value, the host immediately restarts its SSL/TLS listening service; typically this takes less than a second but existing connections to it will be broken. XenAPI login sessions will remain valid.";
         field ~qualifier:RW ~in_product_since:rel_tampa ~default_value:(Some (VMap [])) ~ty:(Map (String, String)) "guest_VCPUs_params" "VCPUs params to apply to all resident guests";
         field ~qualifier:RW ~in_product_since:rel_cream ~default_value:(Some (VEnum "enabled")) ~ty:host_display "display" "indicates whether the host is configured to output its console to a physical display device";
         field ~qualifier:DynamicRO ~in_product_since:rel_cream ~default_value:(Some (VSet [VInt 0L])) ~ty:(Set (Int)) "virtual_hardware_platform_versions" "The set of versions of the virtual hardware platform that the host can offer to its guests";
         field ~qualifier:DynamicRO ~default_value:(Some (VRef null_ref)) ~in_product_since:rel_ely ~ty:(Ref _vm) "control_domain" "The control domain (domain 0)";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_ely, ""] ~ty:(Set (Ref _pool_update)) ~ignore_foreign_key:true "updates_requiring_reboot" "List of updates which require reboot";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_falcon, ""] ~ty:(Set (Ref _feature)) "features" "List of features available on this host"
       ])
    ()

let host_metrics =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host_metrics ~descr:"The metrics associated with a host" ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[] ~contents:
    [ uid _host_metrics;
      namespace ~name:"memory" ~contents:host_metrics_memory ();
      field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None "live" "Pool master thinks this host is live";
      field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
      field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ]
    ()

(** HostCPU *)

let hostcpu =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_hostcpu ~descr:"A physical CPU" ~gen_events:true
    ~lifecycle:[
      Published, rel_rio, "A physical CPU";
      Deprecated, rel_midnight_ride, "Deprecated in favour of the Host.cpu_info field";
    ]
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[] ~contents:
    [ uid _hostcpu;
      field ~qualifier:DynamicRO ~ty:(Ref _host) "host" "the host the CPU is in";
      field ~qualifier:DynamicRO ~ty:Int "number" "the number of the physical CPU within the host";
      field ~qualifier:DynamicRO ~ty:String "vendor" "the vendor of the physical CPU";
      field ~qualifier:DynamicRO ~ty:Int "speed" "the speed of the physical CPU";
      field ~qualifier:DynamicRO ~ty:String "modelname" "the model name of the physical CPU";
      field ~qualifier:DynamicRO ~ty:Int "family" "the family (number) of the physical CPU";
      field ~qualifier:DynamicRO ~ty:Int "model" "the model number of the physical CPU";
      field ~qualifier:DynamicRO ~ty:String "stepping" "the stepping of the physical CPU";
      field ~qualifier:DynamicRO ~ty:String "flags" "the flags of the physical CPU (a decoded version of the features field)";
      field ~qualifier:DynamicRO ~ty:String "features" "the physical CPU feature bitmap";
      field ~qualifier:DynamicRO ~persist:false ~ty:Float "utilisation" "the current CPU utilisation";
      field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ]
    ()

(** Disk and network interfaces are associated with QoS parameters: *)
let qos devtype =
  [ field  "algorithm_type" "QoS algorithm to use";
    field  ~ty:(Map(String,String)) "algorithm_params"
      "parameters for chosen QoS algorithm";
    field ~qualifier:DynamicRO  ~ty:(Set String) "supported_algorithms"
      ("supported QoS algorithms for this " ^ devtype);
  ]

let network_operations =
  Enum ("network_operations",
        [ "attaching", "Indicates this network is attaching to a VIF or PIF" ])

let network_default_locking_mode =
  Enum ("network_default_locking_mode", [
      "unlocked", "Treat all VIFs on this network with locking_mode = 'default' as if they have locking_mode = 'unlocked'";
      "disabled", "Treat all VIFs on this network with locking_mode = 'default' as if they have locking_mode = 'disabled'";
    ])


let network_attach = call
    ~name:"attach"
    ~doc:"Makes the network immediately available on a particular host"
    ~params:[Ref _network, "network", "network to which this interface should be connected";
             Ref _host, "host", "physical machine to which this PIF is connected"]
    ~in_product_since:rel_miami
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_OP
    ()

let network_purpose = Enum ("network_purpose", [
    "nbd", "Network Block Device service using TLS";
    "insecure_nbd", "Network Block Device service without integrity or confidentiality: NOT RECOMMENDED";
    (* We should (re-)add other purposes as and when we write code with behaviour that depends on them,
     * e.g. management, storage, guest, himn... unmanaged? *)
  ])

let network_introduce_params first_rel =
  [
    {param_type=String; param_name="name_label"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="name_description"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Int; param_name="MTU"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Map(String,String); param_name="other_config"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="bridge"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="managed"; param_doc=""; param_release=falcon_release; param_default=None};
    {param_type=Set(network_purpose); param_name="purpose"; param_doc=""; param_release=inverness_release; param_default=None};
  ]

(* network pool introduce is used to copy network records on pool join -- it's the network analogue of VDI/PIF.pool_introduce *)
let network_pool_introduce = call
    ~name:"pool_introduce"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~versioned_params:(network_introduce_params miami_release)
    ~doc:"Create a new network record in the database only"
    ~result:(Ref _network, "The ref of the newly created network record.")
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_OP
    ()

let network_create_new_blob = call
    ~name: "create_new_blob"
    ~in_product_since:rel_orlando
    ~doc:"Create a placeholder for a named binary blob of data that is associated with this pool"
    ~versioned_params:
      [{param_type=Ref _network; param_name="network"; param_doc="The network"; param_release=orlando_release; param_default=None};
       {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
       {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
       {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
      ]
    ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
    ~allowed_roles:_R_POOL_OP
    ()

let network_set_default_locking_mode = call
    ~name:"set_default_locking_mode"
    ~in_product_since:rel_tampa
    ~doc:"Set the default locking mode for VIFs attached to this network"
    ~params:[
      Ref _network, "network", "The network";
      network_default_locking_mode, "value", "The default locking mode for VIFs attached to this network.";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let network_attach_for_vm = call
    ~name:"attach_for_vm"
    ~doc:"Attaches all networks needed by a given VM on a particular host"
    ~params:[
      Ref _host, "host", "Physical machine to which the networks are to be attached";
      Ref _vm, "vm", "The virtual machine"
    ]
    ~in_product_since:rel_tampa
    ~hide_from_docs:true
    ~allowed_roles:_R_VM_POWER_ADMIN
    ()

let network_detach_for_vm = call
    ~name:"detach_for_vm"
    ~doc:"Detaches all networks of a given VM from a particular host"
    ~params:[
      Ref _host, "host", "Physical machine from which the networks are to be attached";
      Ref _vm, "vm", "The virtual machine"
    ]
    ~in_product_since:rel_tampa
    ~hide_from_docs:true
    ~allowed_roles:_R_VM_POWER_ADMIN
    ()

let network_add_purpose = call
    ~name:"add_purpose"
    ~doc:"Give a network a new purpose (if not present already)"
    ~params:[
      Ref _network, "self", "The network";
      network_purpose, "value", "The purpose to add";
    ]
    ~errs:[Api_errors.network_incompatible_purposes]
    ~in_product_since:rel_inverness
    ~allowed_roles:_R_POOL_ADMIN
    ()

let network_remove_purpose = call
    ~name:"remove_purpose"
    ~doc:"Remove a purpose from a network (if present)"
    ~params:[
      Ref _network, "self", "The network";
      network_purpose, "value", "The purpose to remove";
    ]
    ~in_product_since:rel_inverness
    ~allowed_roles:_R_POOL_ADMIN
    ()

(** A virtual network *)
let network =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_network ~descr:"A virtual network" ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN (* vm admins can create/destroy networks without PIFs *)
    ~doc_tags:[Networking]
    ~messages:[network_attach; network_pool_introduce; network_create_new_blob; network_set_default_locking_mode;
               network_attach_for_vm; network_detach_for_vm; network_add_purpose; network_remove_purpose]
    ~contents:
      ([
        uid _network;
        namespace ~name:"name" ~contents:(names ~writer_roles:_R_POOL_OP oss_since_303 RW) ();
      ] @ (allowed_and_current_operations ~writer_roles:_R_POOL_OP network_operations) @ [
          field ~qualifier:DynamicRO ~ty:(Set (Ref _vif)) "VIFs" "list of connected vifs";
          field ~qualifier:DynamicRO ~ty:(Set (Ref _pif)) "PIFs" "list of connected pifs";
          field ~qualifier:RW ~ty:Int ~default_value:(Some (VInt 1500L)) ~in_product_since:rel_midnight_ride "MTU" "MTU in octets";
          field ~writer_roles:_R_POOL_OP ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP));("XenCenterCreateInProgress",(_R_VM_OP))];
          field ~lifecycle:[Published, rel_rio, ""; Changed, rel_falcon, "Added to the constructor (network.create)"] ~in_oss_since:None ~qualifier:StaticRO  ~ty:String ~default_value:(Some (VString "")) "bridge" "name of the bridge corresponding to this network on the local host";
          field ~lifecycle:[Published, rel_falcon, ""] ~qualifier:StaticRO ~ty:Bool ~default_value:(Some (VBool true)) "managed" "true if the bridge is managed by xapi";
          field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this network";
          field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
          field ~qualifier:DynamicRO ~in_product_since:rel_tampa ~default_value:(Some (VEnum "unlocked")) ~ty:network_default_locking_mode "default_locking_mode" "The network will use this value to determine the behaviour of all VIFs where locking_mode = default";
          field ~qualifier:DynamicRO ~in_product_since:rel_creedence ~default_value:(Some (VMap [])) ~ty:(Map (Ref _vif, String)) "assigned_ips" "The IP addresses assigned to VIFs on networks that have active xapi-managed DHCP";
          field ~qualifier:DynamicRO ~in_product_since:rel_inverness ~default_value:(Some (VSet [])) ~ty:(Set network_purpose) "purpose" "Set of purposes for which the server will use this network";
        ])
    ()

let pif_create_VLAN = call
    ~name:"create_VLAN"
    ~in_product_since:rel_rio
    ~doc:"Create a VLAN interface from an existing physical interface. This call is deprecated: use VLAN.create instead"
    ~lifecycle:[
      Published, rel_rio, "Create a VLAN interface from an existing physical interface";
      Deprecated, rel_miami, "Replaced by VLAN.create";
    ]
    ~params:[String, "device", "physical interface on which to create the VLAN interface";
             Ref _network, "network", "network to which this interface should be connected";
             Ref _host, "host", "physical machine to which this PIF is connected";
             Int, "VLAN", "VLAN tag for the new interface"]
    ~result:(Ref _pif, "The reference of the created PIF object")
    ~errs:[Api_errors.vlan_tag_invalid]
    ~internal_deprecated_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ()

let pif_destroy = call
    ~name:"destroy"
    ~in_product_since:rel_rio
    ~doc:"Destroy the PIF object (provided it is a VLAN interface). This call is deprecated: use VLAN.destroy or Bond.destroy instead"
    ~lifecycle:[
      Published, rel_rio, "Destroy the PIF object (provided it is a VLAN interface)";
      Deprecated, rel_miami, "Replaced by VLAN.destroy and Bond.destroy";
    ]
    ~params:[Ref _pif, "self", "the PIF object to destroy"]
    ~errs:[Api_errors.pif_is_physical]
    ~internal_deprecated_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ()

let pif_plug = call
    ~name:"plug"
    ~doc:"Attempt to bring up a physical interface"
    ~params:[Ref _pif, "self", "the PIF object to plug"]
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ~errs:[Api_errors.transport_pif_not_configured]
    ()

let pif_unplug = call
    ~name:"unplug"
    ~doc:"Attempt to bring down a physical interface"
    ~params:[Ref _pif, "self", "the PIF object to unplug"]
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ~errs:[Api_errors.ha_operation_would_break_failover_plan;
           Api_errors.vif_in_use;
           Api_errors.pif_does_not_allow_unplug;
           Api_errors.pif_has_fcoe_sr_in_use]
    ()

let pif_ip_configuration_mode = Enum ("ip_configuration_mode",
                                      [ "None", "Do not acquire an IP address";
                                        "DHCP", "Acquire an IP address by DHCP";
                                        "Static", "Static IP address configuration" ])

let pif_reconfigure_ip = call
    ~name:"reconfigure_ip"
    ~doc:"Reconfigure the IP address settings for this interface"
    ~params:[Ref _pif, "self", "the PIF object to reconfigure";
             pif_ip_configuration_mode, "mode", "whether to use dynamic/static/no-assignment";
             String, "IP", "the new IP address";
             String, "netmask", "the new netmask";
             String, "gateway", "the new gateway";
             String, "DNS", "the new DNS settings";
            ]
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ()

let pif_ipv6_configuration_mode = Enum ("ipv6_configuration_mode",
                                        [ "None", "Do not acquire an IPv6 address";
                                          "DHCP", "Acquire an IPv6 address by DHCP";
                                          "Static", "Static IPv6 address configuration";
                                          "Autoconf", "Router assigned prefix delegation IPv6 allocation" ])

let pif_reconfigure_ipv6 = call
    ~name:"reconfigure_ipv6"
    ~doc:"Reconfigure the IPv6 address settings for this interface"
    ~params:[Ref _pif, "self", "the PIF object to reconfigure";
             pif_ipv6_configuration_mode, "mode", "whether to use dynamic/static/no-assignment";
             String, "IPv6", "the new IPv6 address (in <addr>/<prefix length> format)";
             String, "gateway", "the new gateway";
             String, "DNS", "the new DNS settings";
            ]
    ~lifecycle:[Prototyped, rel_tampa, ""]
    ~allowed_roles:_R_POOL_OP
    ()

let pif_primary_address_type = Enum ("primary_address_type",
                                     [ "IPv4", "Primary address is the IPv4 address";
                                       "IPv6", "Primary address is the IPv6 address" ])

let pif_set_primary_address_type = call
    ~name:"set_primary_address_type"
    ~doc:"Change the primary address type used by this PIF"
    ~params:[Ref _pif, "self", "the PIF object to reconfigure";
             pif_primary_address_type, "primary_address_type", "Whether to prefer IPv4 or IPv6 connections";
            ]
    ~lifecycle:[Prototyped, rel_tampa, ""]
    ~allowed_roles:_R_POOL_OP
    ()

let pif_scan = call
    ~name:"scan"
    ~doc:"Scan for physical interfaces on a host and create PIF objects to represent them"
    ~params:[Ref _host, "host", "The host on which to scan"]
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ()

let pif_introduce_params =
  [
    {param_type=Ref _host; param_name="host"; param_doc="The host on which the interface exists"; param_release=miami_release; param_default=None};
    {param_type=String; param_name="MAC"; param_doc="The MAC address of the interface"; param_release=miami_release; param_default=None};
    {param_type=String; param_name="device"; param_doc="The device name to use for the interface"; param_release=miami_release; param_default=None};
    {param_type=Bool; param_name="managed"; param_doc="Indicates whether the interface is managed by xapi (defaults to \"true\")"; param_release=vgpu_productisation_release; param_default=Some (VBool true)};
  ]

let pif_introduce = call
    ~name:"introduce"
    ~doc:"Create a PIF object matching a particular network interface"
    ~versioned_params:pif_introduce_params
    ~in_product_since:rel_miami
    ~result:(Ref _pif, "The reference of the created PIF object")
    ~allowed_roles:_R_POOL_OP
    ()

let pif_forget = call
    ~name:"forget"
    ~doc:"Destroy the PIF object matching a particular network interface"
    ~params:[Ref _pif, "self", "The PIF object to destroy"]
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ~errs:[Api_errors.pif_tunnel_still_exists]
    ()

let pif_pool_introduce_params first_rel =
  [
    {param_type=String; param_name="device"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _network; param_name="network"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _host; param_name="host"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="MAC"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Int; param_name="MTU"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Int; param_name="VLAN"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="physical"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=pif_ip_configuration_mode; param_name="ip_configuration_mode"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="IP"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="netmask"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="gateway"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="DNS"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _bond; param_name="bond_slave_of"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _vlan; param_name="VLAN_master_of"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="management"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Map(String, String); param_name="other_config"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="disallow_unplug"; param_doc=""; param_release=orlando_release; param_default=Some (VBool false)};
    {param_type=pif_ipv6_configuration_mode; param_name="ipv6_configuration_mode"; param_doc=""; param_release=boston_release; param_default=Some (VEnum "None")};
    {param_type=(Set(String)); param_name="IPv6"; param_doc=""; param_release=boston_release; param_default=Some (VSet [])};
    {param_type=String; param_name="ipv6_gateway"; param_doc=""; param_release=boston_release; param_default=Some (VString "")};
    {param_type=pif_primary_address_type; param_name="primary_address_type"; param_doc=""; param_release=boston_release; param_default=Some (VEnum "IPv4")};
    {param_type=Bool; param_name="managed"; param_doc=""; param_release=vgpu_productisation_release; param_default=Some (VBool true)};
    {param_type=Map(String, String); param_name="properties"; param_doc=""; param_release=creedence_release; param_default=Some (VMap [])};
  ]

(* PIF pool introduce is used to copy PIF records on pool join -- it's the PIF analogue of VDI.pool_introduce *)
let pif_pool_introduce = call
    ~name:"pool_introduce"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~versioned_params:(pif_pool_introduce_params miami_release)
    ~doc:"Create a new PIF record in the database only"
    ~result:(Ref _pif, "The ref of the newly created PIF record.")
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_OP
    ()

let pif_db_introduce = call
    ~name:"db_introduce"
    ~in_oss_since:None
    ~in_product_since:rel_orlando
    ~versioned_params:(pif_pool_introduce_params orlando_release)
    ~doc:"Create a new PIF record in the database only"
    ~result:(Ref _pif, "The ref of the newly created PIF record.")
    ~hide_from_docs:false
    ~allowed_roles:_R_POOL_OP
    ()

let pif_db_forget = call
    ~name:"db_forget"
    ~in_oss_since:None
    ~in_product_since:rel_orlando
    ~params:[ Ref _pif, "self", "The ref of the PIF whose database record should be destroyed" ]
    ~doc:"Destroy a PIF database record."
    ~hide_from_docs:false
    ~allowed_roles:_R_POOL_OP
    ()

let pif_set_property = call
    ~name:"set_property"
    ~doc:"Set the value of a property of the PIF"
    ~params:[
      Ref _pif, "self", "The PIF";
      String, "name", "The property name";
      String, "value", "The property value";
    ]
    ~lifecycle:[Published, rel_creedence, ""]
    ~allowed_roles:_R_POOL_OP
    ()

let pif_igmp_status =
  Enum ("pif_igmp_status", [
      "enabled", "IGMP Snooping is enabled in the corresponding backend bridge.'";
      "disabled", "IGMP Snooping is disabled in the corresponding backend bridge.'";
      "unknown", "IGMP snooping status is unknown. If this is a VLAN master, then please consult the underlying VLAN slave PIF."
    ])

let pif =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_pif ~descr:"A physical network interface (note separate VLANs are represented as several PIFs)"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~doc_tags:[Networking]
    ~messages:[pif_create_VLAN; pif_destroy; pif_reconfigure_ip; pif_reconfigure_ipv6; pif_set_primary_address_type; pif_scan; pif_introduce; pif_forget;
               pif_unplug; pif_plug; pif_pool_introduce;
               pif_db_introduce; pif_db_forget; pif_set_property
              ] ~contents:
    [ uid _pif;
      (* qualifier changed RW -> StaticRO in Miami *)
      field ~qualifier:StaticRO "device" "machine-readable name of the interface (e.g. eth0)";
      field ~qualifier:StaticRO ~ty:(Ref _network) "network" "virtual network to which this pif is connected";
      field ~qualifier:StaticRO ~ty:(Ref _host) "host" "physical machine to which this pif is connected";
      (* qualifier changed RW -> StaticRO in Miami *)
      field ~qualifier:StaticRO "MAC" "ethernet MAC address of physical interface";
      (* qualifier changed RW -> StaticRO in Miami *)
      field ~qualifier:StaticRO ~ty:Int "MTU" "MTU in octets";
      (* qualifier changed RW -> StaticRO in Miami *)
      field ~qualifier:StaticRO ~ty:Int "VLAN" "VLAN tag for all traffic passing through this interface";
      field ~in_oss_since:None ~internal_only:true "device_name" "actual dom0 device name";
      field ~qualifier:DynamicRO ~ty:(Ref _pif_metrics) "metrics" "metrics associated with this PIF";
      field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "physical" "true if this represents a physical network interface" ~default_value:(Some (VBool false));
      field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "currently_attached" "true if this interface is online" ~default_value:(Some (VBool true));
      field ~in_oss_since:None ~ty:pif_ip_configuration_mode ~in_product_since:rel_miami ~qualifier:DynamicRO "ip_configuration_mode" "Sets if and how this interface gets an IP address" ~default_value:(Some (VEnum "None"));
      field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "IP" "IP address" ~default_value:(Some (VString ""));
      field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "netmask" "IP netmask" ~default_value:(Some (VString ""));
      field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "gateway" "IP gateway" ~default_value:(Some (VString ""));
      field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "DNS" "IP address of DNS servers to use" ~default_value:(Some (VString ""));
      field ~in_oss_since:None ~ty:(Ref _bond) ~in_product_since:rel_miami ~qualifier:DynamicRO "bond_slave_of" "Indicates which bond this interface is part of" ~default_value:(Some (VRef ""));
      field ~in_oss_since:None ~ty:(Set(Ref _bond)) ~in_product_since:rel_miami ~qualifier:DynamicRO "bond_master_of" "Indicates this PIF represents the results of a bond";
      field ~in_oss_since:None ~ty:(Ref _vlan) ~in_product_since:rel_miami ~qualifier:DynamicRO "VLAN_master_of" "Indicates wich VLAN this interface receives untagged traffic from" ~default_value:(Some (VRef ""));
      field ~in_oss_since:None ~ty:(Set(Ref _vlan)) ~in_product_since:rel_miami ~qualifier:DynamicRO "VLAN_slave_of" "Indicates which VLANs this interface transmits tagged traffic to";
      field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "management" "Indicates whether the control software is listening for connections on this interface" ~default_value:(Some (VBool false));
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "Additional configuration";
      field ~in_product_since:rel_orlando ~default_value:(Some (VBool false)) ~ty:Bool "disallow_unplug" "Prevent this PIF from being unplugged; set this to notify the management tool-stack that the PIF has a special use and should not be unplugged under any circumstances (e.g. because you're running storage traffic over it)";
      field ~in_oss_since:None ~ty:(Set(Ref _tunnel)) ~lifecycle:[Published, rel_cowley, "Indicates to which tunnel this PIF gives access"] ~qualifier:DynamicRO "tunnel_access_PIF_of" "Indicates to which tunnel this PIF gives access";
      field ~in_oss_since:None ~ty:(Set(Ref _tunnel)) ~lifecycle:[Published, rel_cowley, "Indicates to which tunnel this PIF provides transport"] ~qualifier:DynamicRO "tunnel_transport_PIF_of" "Indicates to which tunnel this PIF provides transport";
      field ~in_oss_since:None ~ty:pif_ipv6_configuration_mode ~lifecycle:[Prototyped, rel_tampa, ""] ~qualifier:DynamicRO "ipv6_configuration_mode" "Sets if and how this interface gets an IPv6 address" ~default_value:(Some (VEnum "None"));
      field ~in_oss_since:None ~ty:(Set(String)) ~lifecycle:[Prototyped, rel_tampa, ""] ~qualifier:DynamicRO "IPv6" "IPv6 address" ~default_value:(Some (VSet []));
      field ~in_oss_since:None ~ty:String ~lifecycle:[Prototyped, rel_tampa, ""] ~qualifier:DynamicRO "ipv6_gateway" "IPv6 gateway" ~default_value:(Some (VString ""));
      field ~in_oss_since:None ~ty:pif_primary_address_type ~lifecycle:[Prototyped, rel_tampa, ""] ~qualifier:DynamicRO "primary_address_type" "Which protocol should define the primary address of this interface" ~default_value:(Some (VEnum "IPv4"));
      field ~in_oss_since:None ~ty:Bool ~lifecycle:[Published, rel_vgpu_productisation, ""] ~qualifier:StaticRO "managed" "Indicates whether the interface \
                                                                                                                           		is managed by xapi. If it is not, then xapi will not configure the interface, the commands PIF.plug/unplug/reconfigure_ip(v6) \
                                                                                                                           		can not be used, nor can the interface be bonded or have VLANs based on top through xapi." ~default_value:(Some (VBool true));
      field ~lifecycle:[Published, rel_creedence, ""] ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "properties" "Additional configuration properties for the interface.";
      field ~lifecycle:[Published, rel_dundee, ""] ~qualifier:DynamicRO ~ty:(Set(String)) ~default_value:(Some (VSet [])) "capabilities" "Additional capabilities on the interface.";
      field ~lifecycle:[Published, rel_inverness, ""] ~qualifier:DynamicRO ~ty:pif_igmp_status ~default_value:(Some (VEnum "unknown")) "igmp_snooping_status" "The IGMP snooping status of the corresponding network bridge";
      field ~in_oss_since:None ~ty:(Set (Ref _network_sriov)) ~in_product_since:rel_kolkata ~qualifier:DynamicRO "sriov_physical_PIF_of" "Indicates which network_sriov this interface is physical of";
      field ~in_oss_since:None ~ty:(Set (Ref _network_sriov)) ~in_product_since:rel_kolkata ~qualifier:DynamicRO "sriov_logical_PIF_of" "Indicates which network_sriov this interface is logical of";
      field ~qualifier:DynamicRO ~ty:(Ref _pci) ~lifecycle:[Published, rel_kolkata, ""] ~default_value:(Some (VRef null_ref)) "PCI" "Link to underlying PCI device";
    ]
    ()

let pif_metrics =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_pif_metrics ~descr:"The metrics associated with a physical network interface"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~doc_tags:[Networking]
    ~messages:[] ~contents:
    [ uid _pif_metrics;
      namespace ~name:"io" ~contents:iobandwidth ();
      field ~qualifier:DynamicRO ~ty:Bool "carrier" "Report if the PIF got a carrier or not";
      field ~qualifier:DynamicRO ~ty:String "vendor_id" "Report vendor ID";
      field ~qualifier:DynamicRO ~ty:String "vendor_name" "Report vendor name";
      field ~qualifier:DynamicRO ~ty:String "device_id" "Report device ID";
      field ~qualifier:DynamicRO ~ty:String "device_name" "Report device name";
      field ~qualifier:DynamicRO ~ty:Int "speed" "Speed of the link (if available)";
      field ~qualifier:DynamicRO ~ty:Bool "duplex" "Full duplex capability of the link (if available)";
      field ~qualifier:DynamicRO ~ty:String "pci_bus_path" "PCI bus path of the pif (if available)";
      field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
      field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ]
    ()


let bond_mode =
  Enum ("bond_mode", [
      "balance-slb", "Source-level balancing";
      "active-backup", "Active/passive bonding: only one NIC is carrying traffic";
      "lacp", "Link aggregation control protocol";
    ])

let bond_create = call
    ~name:"create"
    ~doc:"Create an interface bond"
    ~versioned_params:[
      {param_type=Ref _network; param_name="network"; param_doc="Network to add the bonded PIF to"; param_release=miami_release; param_default=None};
      {param_type=Set (Ref _pif); param_name="members"; param_doc="PIFs to add to this bond"; param_release=miami_release; param_default=None};
      {param_type=String; param_name="MAC"; param_doc="The MAC address to use on the bond itself. If this parameter is the empty string then the bond will inherit its MAC address from the primary slave."; param_release=miami_release; param_default=None};
      {param_type=bond_mode; param_name="mode"; param_doc="Bonding mode to use for the new bond"; param_release=boston_release; param_default=Some (VEnum "balance-slb")};
      {param_type=Map (String, String); param_name="properties"; param_doc="Additional configuration parameters specific to the bond mode"; param_release=tampa_release; param_default=Some (VMap [])};
    ]
    ~result:(Ref _bond, "The reference of the created Bond object")
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ()

let bond_destroy = call
    ~name:"destroy"
    ~doc:"Destroy an interface bond"
    ~params:[Ref _bond, "self", "Bond to destroy"]
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ()

let bond_set_mode = call
    ~name:"set_mode"
    ~doc:"Change the bond mode"
    ~params:[
      Ref _bond, "self", "The bond";
      bond_mode, "value", "The new bond mode";
    ]
    ~lifecycle:[Published, rel_boston, ""]
    ~allowed_roles:_R_POOL_OP
    ()

let bond_set_property = call
    ~name:"set_property"
    ~doc:"Set the value of a property of the bond"
    ~params:[
      Ref _bond, "self", "The bond";
      String, "name", "The property name";
      String, "value", "The property value";
    ]
    ~in_product_since:rel_tampa
    ~allowed_roles:_R_POOL_OP
    ()

let bond =
  create_obj ~in_db:true ~in_product_since:rel_miami ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_bond ~descr:"" ~gen_events:true ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~doc_tags:[Networking]
    ~messages:[ bond_create; bond_destroy; bond_set_mode; bond_set_property ]
    ~contents:
      [ uid _bond;
        field ~in_oss_since:None ~in_product_since:rel_miami ~qualifier:StaticRO ~ty:(Ref _pif) "master" "The bonded interface" ~default_value:(Some (VRef ""));
        field ~in_oss_since:None ~in_product_since:rel_miami ~qualifier:DynamicRO ~ty:(Set(Ref _pif)) "slaves" "The interfaces which are part of this bond";
        field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
        field ~lifecycle:[Published, rel_boston, ""] ~qualifier:DynamicRO ~default_value:(Some (VRef null_ref)) ~ty:(Ref _pif) "primary_slave" "The PIF of which the IP configuration and MAC were copied to the bond, and which will receive all configuration/VLANs/VIFs on the bond if the bond is destroyed";
        field ~lifecycle:[Published, rel_boston, ""] ~qualifier:DynamicRO ~default_value:(Some (VEnum "balance-slb")) ~ty:bond_mode "mode" "The algorithm used to distribute traffic among the bonded NICs";
        field ~in_oss_since:None ~in_product_since:rel_tampa ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "properties" "Additional configuration properties specific to the bond mode.";
        field ~in_oss_since:None ~in_product_since:rel_tampa ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "links_up" "Number of links up in this bond";
      ]
    ()

let vlan_introduce_params first_rel =
  [
    {param_type=Ref _pif; param_name="tagged_PIF"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _pif; param_name="untagged_PIF"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Int; param_name="tag"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Map(String, String); param_name="other_config"; param_doc=""; param_release=first_rel; param_default=None};
  ]

(* vlan pool introduce is used to copy management vlan record on pool join -- it's the vlan analogue of VDI/PIF.pool_introduce *)
let vlan_pool_introduce = call
    ~name:"pool_introduce"
    ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~versioned_params:(vlan_introduce_params inverness_release)
    ~doc:"Create a new vlan record in the database only"
    ~result:(Ref _vlan, "The reference of the created VLAN object")
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_OP
    ()

let vlan_create = call
    ~name:"create"
    ~doc:"Create a VLAN mux/demuxer"
    ~params:[ Ref _pif, "tagged_PIF", "PIF which receives the tagged traffic";
              Int, "tag", "VLAN tag to use";
              Ref _network, "network", "Network to receive the untagged traffic" ]
    ~result:(Ref _vlan, "The reference of the created VLAN object")
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ()

let vlan_destroy = call
    ~name:"destroy"
    ~doc:"Destroy a VLAN mux/demuxer"
    ~params:[Ref _vlan, "self", "VLAN mux/demuxer to destroy"]
    ~in_product_since:rel_miami
    ~allowed_roles:_R_POOL_OP
    ()

let vlan =
  create_obj ~in_db:true ~in_product_since:rel_miami ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_vlan ~descr:"A VLAN mux/demux" ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~doc_tags:[Networking]
    ~messages:[ vlan_pool_introduce; vlan_create; vlan_destroy ] ~contents:
    ([
      uid _vlan;
      field ~qualifier:StaticRO ~ty:(Ref _pif) ~in_product_since:rel_miami "tagged_PIF" "interface on which traffic is tagged" ~default_value:(Some (VRef ""));
      field ~qualifier:DynamicRO ~ty:(Ref _pif) ~in_product_since:rel_miami "untagged_PIF" "interface on which traffic is untagged" ~default_value:(Some (VRef ""));
      field ~qualifier:StaticRO ~ty:Int ~in_product_since:rel_miami "tag" "VLAN tag in use" ~default_value:(Some (VInt (-1L)));
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ])
    ()

let tunnel_create = call
    ~name:"create"
    ~doc:"Create a tunnel"
    ~params:[ Ref _pif, "transport_PIF", "PIF which receives the tagged traffic";
              Ref _network, "network", "Network to receive the tunnelled traffic" ]
    ~result:(Ref _tunnel, "The reference of the created tunnel object")
    ~lifecycle:[Published, rel_cowley, "Create a tunnel"]
    ~allowed_roles:_R_POOL_OP
    ~errs:[Api_errors.openvswitch_not_active; Api_errors.transport_pif_not_configured; Api_errors.is_tunnel_access_pif]
    ()

let tunnel_destroy = call
    ~name:"destroy"
    ~doc:"Destroy a tunnel"
    ~params:[Ref _tunnel, "self", "tunnel to destroy"]
    ~lifecycle:[Published, rel_cowley, "Destroy a tunnel"]
    ~allowed_roles:_R_POOL_OP
    ()

let tunnel =
  create_obj ~in_db:true ~lifecycle:[Published, rel_cowley, "A tunnel for network traffic"] ~in_oss_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_tunnel ~descr:"A tunnel for network traffic" ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~doc_tags:[Networking]
    ~messages:[ tunnel_create; tunnel_destroy ]
    ~contents:([
        uid _tunnel ~lifecycle:[Published, rel_cowley, "Unique identifier/object reference"];
        field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle:[Published, rel_cowley, "The interface through which the tunnel is accessed"] "access_PIF" "The interface through which the tunnel is accessed" ~default_value:(Some (VRef ""));
        field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle:[Published, rel_cowley, "The interface used by the tunnel"] "transport_PIF" "The interface used by the tunnel" ~default_value:(Some (VRef ""));
        field ~ty:(Map(String, String)) ~lifecycle:[Published, rel_cowley, "Status information about the tunnel"] "status" "Status information about the tunnel" ~default_value:(Some (VMap [VString "active", VString "false"]));
        field ~lifecycle:[Published, rel_cowley, "Additional configuration"] ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "Additional configuration";
      ])
    ()

let pbd_set_device_config = call
    ~name:"set_device_config"
    ~in_oss_since:None
    ~in_product_since:rel_miami
    ~params:[Ref _pbd, "self", "The PBD to modify";
             Map(String, String), "value", "The new value of the PBD's device_config"]
    ~doc:"Sets the PBD's device_config field"
    ~allowed_roles:_R_POOL_OP
    ()

let pbd =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_pbd ~descr:"The physical block devices through which hosts access SRs"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[
      pbd_plug; pbd_unplug;
      pbd_set_device_config
    ] ~contents:
    [ uid _pbd;
      field ~qualifier:StaticRO ~ty:(Ref _host) "host" "physical machine on which the pbd is available";
      field ~qualifier:StaticRO ~ty:(Ref _sr) "SR" "the storage repository that the pbd realises";
      field ~ty:(Map(String, String)) ~qualifier:StaticRO "device_config" "a config string to string map that is provided to the host's SR-backend-driver";
      field ~ty:Bool ~qualifier:DynamicRO "currently_attached" "is the SR currently attached on this host?";
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ]
    ()

(* These are included in vbds and vifs -- abstracted here to keep both these uses consistent *)
let device_status_fields =
  [
    field ~ty:Bool ~qualifier:DynamicRO "currently_attached" "is the device currently attached (erased on reboot)";
    field ~ty:Int ~qualifier:DynamicRO "status_code" "error/success code associated with last attach-operation (erased on reboot)";
    field ~ty:String ~qualifier:DynamicRO "status_detail" "error/success information associated with last attach-operation status (erased on reboot)";
    field ~ty:(Map(String, String)) ~qualifier:DynamicRO "runtime_properties" "Device runtime properties"
  ]

(* VIF messages *)

let vif_ipv4_configuration_mode = Enum ("vif_ipv4_configuration_mode", [
    "None", "Follow the default IPv4 configuration of the guest (this is guest-dependent)";
    "Static", "Static IPv4 address configuration";
  ])

let vif_ipv6_configuration_mode = Enum ("vif_ipv6_configuration_mode", [
    "None", "Follow the default IPv6 configuration of the guest (this is guest-dependent)";
    "Static", "Static IPv6 address configuration";
  ])

let vif_plug = call
    ~name:"plug"
    ~in_product_since:rel_rio
    ~doc:"Hotplug the specified VIF, dynamically attaching it to the running VM"
    ~params:[Ref _vif, "self", "The VIF to hotplug"]
    ~allowed_roles:_R_VM_ADMIN
    ()

let vif_unplug = call
    ~name:"unplug"
    ~in_product_since:rel_rio
    ~doc:"Hot-unplug the specified VIF, dynamically unattaching it from the running VM"
    ~params:[Ref _vif, "self", "The VIF to hot-unplug"]
    ~allowed_roles:_R_VM_ADMIN
    ()

let vif_unplug_force = call
    ~name:"unplug_force"
    ~in_product_since:rel_boston
    ~doc:"Forcibly unplug the specified VIF"
    ~params:[Ref _vif, "self", "The VIF to forcibly unplug"]
    ~allowed_roles:_R_VM_ADMIN
    ()

let vif_move = call
    ~name:"move"
    ~in_product_since:rel_ely
    ~doc:"Move the specified VIF to the specified network, even while the VM is running"
    ~params:[Ref _vif, "self", "The VIF to move";
             Ref _network, "network", "The network to move it to"]
    ~allowed_roles:_R_VM_ADMIN
    ()

let vif_operations =
  Enum ("vif_operations",
        [ "attach", "Attempting to attach this VIF to a VM";
          "plug", "Attempting to hotplug this VIF";
          "unplug", "Attempting to hot unplug this VIF";
        ])

let vif_locking_mode =
  Enum ("vif_locking_mode", [
      "network_default", "No specific configuration set - default network policy applies";
      "locked", "Only traffic to a specific MAC and a list of IPv4 or IPv6 addresses is permitted";
      "unlocked", "All traffic is permitted";
      "disabled", "No traffic is permitted";
    ])

let vif_set_locking_mode = call
    ~name:"set_locking_mode"
    ~in_product_since:rel_tampa
    ~doc:"Set the locking mode for this VIF"
    ~params:[
      Ref _vif, "self", "The VIF whose locking mode will be set";
      vif_locking_mode, "value", "The new locking mode for the VIF";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let vif_set_ipv4_allowed = call
    ~name:"set_ipv4_allowed"
    ~in_product_since:rel_tampa
    ~doc:"Set the IPv4 addresses to which traffic on this VIF can be restricted"
    ~params:[
      Ref _vif, "self", "The VIF which the IP addresses will be associated with";
      Set String, "value", "The IP addresses which will be associated with the VIF";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let vif_add_ipv4_allowed = call
    ~name:"add_ipv4_allowed"
    ~in_product_since:rel_tampa
    ~doc:"Associates an IPv4 address with this VIF"
    ~params:[
      Ref _vif, "self", "The VIF which the IP address will be associated with";
      String, "value", "The IP address which will be associated with the VIF";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let vif_remove_ipv4_allowed = call
    ~name:"remove_ipv4_allowed"
    ~in_product_since:rel_tampa
    ~doc:"Removes an IPv4 address from this VIF"
    ~params:[
      Ref _vif, "self", "The VIF from which the IP address will be removed";
      String, "value", "The IP address which will be removed from the VIF";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let vif_set_ipv6_allowed = call
    ~name:"set_ipv6_allowed"
    ~in_product_since:rel_tampa
    ~doc:"Set the IPv6 addresses to which traffic on this VIF can be restricted"
    ~params:[
      Ref _vif, "self", "The VIF which the IP addresses will be associated with";
      Set String, "value", "The IP addresses which will be associated with the VIF";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let vif_add_ipv6_allowed = call
    ~name:"add_ipv6_allowed"
    ~in_product_since:rel_tampa
    ~doc:"Associates an IPv6 address with this VIF"
    ~params:[
      Ref _vif, "self", "The VIF which the IP address will be associated with";
      String, "value", "The IP address which will be associated with the VIF";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let vif_remove_ipv6_allowed = call
    ~name:"remove_ipv6_allowed"
    ~in_product_since:rel_tampa
    ~doc:"Removes an IPv6 address from this VIF"
    ~params:[
      Ref _vif, "self", "The VIF from which the IP address will be removed";
      String, "value", "The IP address which will be removed from the VIF";
    ]
    ~allowed_roles:_R_POOL_OP
    ()

let vif_configure_ipv4 = call
    ~name:"configure_ipv4"
    ~in_product_since:rel_dundee
    ~doc:"Configure IPv4 settings for this virtual interface"
    ~versioned_params:[
      {param_type=Ref _vif; param_name="self"; param_doc="The VIF to configure"; param_release=dundee_release; param_default=None};
      {param_type=vif_ipv4_configuration_mode; param_name="mode"; param_doc="Whether to use static or no IPv4 assignment"; param_release=dundee_release; param_default=None};
      {param_type=String; param_name="address"; param_doc="The IPv4 address in <addr>/<prefix length> format (for static mode only)"; param_release=dundee_release; param_default=Some(VString "")};
      {param_type=String; param_name="gateway"; param_doc="The IPv4 gateway (for static mode only; leave empty to not set a gateway)"; param_release=dundee_release; param_default=Some(VString "")}
    ]
    ~allowed_roles:_R_VM_OP
    ()

let vif_configure_ipv6 = call
    ~name:"configure_ipv6"
    ~in_product_since:rel_dundee
    ~doc:"Configure IPv6 settings for this virtual interface"
    ~versioned_params:[
      {param_type=Ref _vif; param_name="self"; param_doc="The VIF to configure"; param_release=dundee_release; param_default=None};
      {param_type=vif_ipv6_configuration_mode; param_name="mode"; param_doc="Whether to use static or no IPv6 assignment"; param_release=dundee_release; param_default=None};
      {param_type=String; param_name="address"; param_doc="The IPv6 address in <addr>/<prefix length> format (for static mode only)"; param_release=dundee_release; param_default=Some(VString "")};
      {param_type=String; param_name="gateway"; param_doc="The IPv6 gateway (for static mode only; leave empty to not set a gateway)"; param_release=dundee_release; param_default=Some(VString "")}
    ]
    ~allowed_roles:_R_VM_OP
    ()

(** A virtual network interface *)
let vif =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vif ~descr:"A virtual network interface"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~doc_tags:[Networking]
    ~messages:[vif_plug; vif_unplug; vif_unplug_force; vif_move; vif_set_locking_mode;
               vif_set_ipv4_allowed; vif_add_ipv4_allowed; vif_remove_ipv4_allowed; vif_set_ipv6_allowed; vif_add_ipv6_allowed; vif_remove_ipv6_allowed;
               vif_configure_ipv4; vif_configure_ipv6]
    ~contents:
      ([ uid _vif;
       ] @ (allowed_and_current_operations vif_operations) @ [
         field ~qualifier:StaticRO "device" "order in which VIF backends are created by xapi";
         field ~qualifier:StaticRO ~ty:(Ref _network) "network" "virtual network to which this vif is connected";
         field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "virtual machine to which this vif is connected";
         field ~qualifier:StaticRO ~ty:String "MAC" "ethernet MAC address of virtual interface, as exposed to guest";
         field ~qualifier:StaticRO ~ty:Int "MTU" "MTU in octets";
         field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Bool "reserved" "true if the VIF is reserved pending a reboot/migrate";
         field ~ty:(Map(String, String)) "other_config" "additional configuration";
       ] @ device_status_fields @
       [ namespace ~name:"qos" ~contents:(qos "VIF") (); ] @
       [ field ~qualifier:DynamicRO ~ty:(Ref _vif_metrics) ~lifecycle: [Removed, rel_tampa, "Disabled in favour of RRDs"] "metrics" "metrics associated with this VIF";
         field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VBool false)) ~ty:Bool "MAC_autogenerated" "true if the MAC was autogenerated; false indicates it was set manually";
         field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VEnum "network_default")) ~ty:vif_locking_mode "locking_mode" "current locking mode of the VIF";
         field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VSet [])) ~ty:(Set (String)) "ipv4_allowed" "A list of IPv4 addresses which can be used to filter traffic passing through this VIF";
         field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VSet [])) ~ty:(Set (String)) "ipv6_allowed" "A list of IPv6 addresses which can be used to filter traffic passing through this VIF";
         field ~ty:vif_ipv4_configuration_mode ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv4_configuration_mode" "Determines whether IPv4 addresses are configured on the VIF" ~default_value:(Some (VEnum "None"));
         field ~ty:(Set (String)) ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv4_addresses" "IPv4 addresses in CIDR format" ~default_value:(Some (VSet []));
         field ~ty:String ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv4_gateway" "IPv4 gateway (the empty string means that no gateway is set)" ~default_value:(Some (VString ""));
         field ~ty:vif_ipv6_configuration_mode ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv6_configuration_mode" "Determines whether IPv6 addresses are configured on the VIF" ~default_value:(Some (VEnum "None"));
         field ~ty:(Set (String)) ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv6_addresses" "IPv6 addresses in CIDR format" ~default_value:(Some (VSet []));
         field ~ty:String ~in_product_since:rel_dundee ~qualifier:DynamicRO "ipv6_gateway" "IPv6 gateway (the empty string means that no gateway is set)" ~default_value:(Some (VString ""));
         field ~ty:(Ref _pci) ~in_product_since:rel_kolkata ~internal_only:true ~qualifier:DynamicRO "reserved_pci" "pci of network SR-IOV VF which is reserved for this vif" ~default_value:(Some (VRef null_ref));
       ])
    ()

let vif_metrics =
  create_obj
    ~lifecycle:
      [ Published, rel_rio, "The metrics associated with a virtual network device"
      ; Removed, rel_tampa, "Disabled in favour of RRDs"
      ]
    ~in_db:true
    ~in_oss_since:oss_since_303
    ~persist:PersistNothing
    ~gen_constructor_destructor:false
    ~name:_vif_metrics
    ~descr:"The metrics associated with a virtual network device"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~doc_tags:[Networking]
    ~messages:[] ~contents:
    [ uid _vif_metrics;
      namespace ~name:"io" ~contents:iobandwidth ();
      field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
      field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ]
    ()

let data_source =
  create_obj ~in_db:false ~in_product_since:rel_orlando ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_data_source ~descr:"Data sources for logging in RRDs"
    ~gen_events:false
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages:[] ~contents:
    [ namespace ~name:"name" ~contents:(names oss_since_303 DynamicRO) ();
      field ~qualifier:DynamicRO ~ty:Bool "enabled" "true if the data source is being logged";
      field ~qualifier:DynamicRO ~ty:Bool "standard" "true if the data source is enabled by default. Non-default data sources cannot be disabled";
      field ~qualifier:DynamicRO ~ty:String "units" "the units of the value";
      field ~qualifier:DynamicRO ~ty:Float "min" "the minimum value of the data source";
      field ~qualifier:DynamicRO ~ty:Float "max" "the maximum value of the data source";
      field ~qualifier:DynamicRO ~ty:Float "value" "current value of the data source" ]

    ()

let storage_operations =
  Enum ("storage_operations",
        [ "scan", "Scanning backends for new or deleted VDIs";
          "destroy", "Destroying the SR";
          "forget", "Forgetting about SR";
          "plug", "Plugging a PBD into this SR";
          "unplug", "Unplugging a PBD from this SR";
          "update", "Refresh the fields on the SR";
          "vdi_create", "Creating a new VDI";
          "vdi_introduce", "Introducing a new VDI";
          "vdi_destroy", "Destroying a VDI";
          "vdi_resize", "Resizing a VDI";
          "vdi_clone", "Cloneing a VDI";
          "vdi_snapshot", "Snapshotting a VDI";
          "vdi_mirror", "Mirroring a VDI";
          "vdi_enable_cbt", "Enabling changed block tracking for a VDI";
          "vdi_disable_cbt", "Disabling changed block tracking for a VDI";
          "vdi_data_destroy", "Deleting the data of the VDI";
          "vdi_list_changed_blocks", "Exporting a bitmap that shows the changed blocks between two VDIs";
          "vdi_set_on_boot", "Setting the on_boot field of the VDI";
          "pbd_create", "Creating a PBD for this SR";
          "pbd_destroy", "Destroying one of this SR's PBDs"; ])

let sr_set_virtual_allocation = call
    ~name:"set_virtual_allocation"
    ~in_oss_since:None
    ~in_product_since:rel_miami
    ~params:[Ref _sr, "self", "The SR to modify";
             Int, "value", "The new value of the SR's virtual_allocation"]
    ~flags:[`Session]
    ~doc:"Sets the SR's virtual_allocation field"
    ~allowed_roles:_R_POOL_OP
    ()

let sr_set_physical_size = call
    ~name:"set_physical_size"
    ~in_oss_since:None
    ~in_product_since:rel_miami
    ~params:[Ref _sr, "self", "The SR to modify";
             Int, "value", "The new value of the SR's physical_size"]
    ~flags:[`Session]
    ~doc:"Sets the SR's physical_size field"
    ~allowed_roles:_R_POOL_OP
    ()

let sr_set_physical_utilisation = call
    ~name:"set_physical_utilisation"
    ~in_oss_since:None
    ~in_product_since:rel_miami
    ~flags:[`Session]
    ~params:[Ref _sr, "self", "The SR to modify";
             Int, "value", "The new value of the SR's physical utilisation"]
    ~doc:"Sets the SR's physical_utilisation field"
    ~allowed_roles:_R_POOL_OP
    ()

let sr_update = call
    ~name:"update"
    ~in_oss_since:None
    ~in_product_since:rel_symc
    ~params:[Ref _sr, "sr", "The SR whose fields should be refreshed" ]
    ~doc:"Refresh the fields on the SR object"
    ~allowed_roles:_R_POOL_OP
    ()

let sr_assert_can_host_ha_statefile = call
    ~name:"assert_can_host_ha_statefile"
    ~in_oss_since:None
    ~in_product_since:rel_orlando
    ~params:[Ref _sr, "sr", "The SR to query" ]
    ~doc:"Returns successfully if the given SR can host an HA statefile. Otherwise returns an error to explain why not"
    ~allowed_roles:_R_POOL_OP
    ()

let sr_assert_supports_database_replication = call
    ~name:"assert_supports_database_replication"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _sr, "sr", "The SR to query"]
    ~doc:"Returns successfully if the given SR supports database replication. Otherwise returns an error to explain why not."
    ~allowed_roles:_R_POOL_OP
    ()

let sr_enable_database_replication = call
    ~name:"enable_database_replication"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _sr, "sr", "The SR to which metadata should be replicated"]
    ~allowed_roles:_R_POOL_OP
    ()

let sr_disable_database_replication = call
    ~name:"disable_database_replication"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _sr, "sr", "The SR to which metadata should be no longer replicated"]
    ~allowed_roles:_R_POOL_OP
    ()

(** A storage repository. Note we overide default create/destroy methods with our own here... *)
let storage_repository =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_sr ~descr:"A storage repository"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[ sr_create; sr_introduce; sr_make; sr_destroy; sr_forget;
                sr_update;
                sr_get_supported_types; sr_scan; sr_probe; sr_set_shared;
                sr_set_name_label; sr_set_name_description;
                sr_create_new_blob;
                sr_set_physical_size; sr_set_virtual_allocation; sr_set_physical_utilisation;
                sr_assert_can_host_ha_statefile;
                sr_assert_supports_database_replication;
                sr_enable_database_replication;
                sr_disable_database_replication;
                sr_get_data_sources;
                sr_record_data_source;
                sr_query_data_source;
                sr_forget_data_source_archives;

              ]
    ~contents:
      ([ uid _sr;
         namespace ~name:"name" ~contents:(names oss_since_303 StaticRO) ();
       ] @ (allowed_and_current_operations storage_operations) @ [
         field ~ty:(Set(Ref _vdi)) ~qualifier:DynamicRO "VDIs" "all virtual disks known to this storage repository";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _pbd)) "PBDs" "describes how particular hosts can see this storage repository";
         field ~ty:Int ~qualifier:DynamicRO "virtual_allocation" "sum of virtual_sizes of all VDIs in this storage repository (in bytes)";
         field ~ty:Int ~qualifier:DynamicRO "physical_utilisation" "physical space currently utilised on this storage repository (in bytes). Note that for sparse disk formats, physical_utilisation may be less than virtual_allocation";
         field ~ty:Int ~qualifier:StaticRO "physical_size" "total physical size of the repository (in bytes)";
         field ~qualifier:StaticRO "type" "type of the storage repository";
         field ~qualifier:StaticRO "content_type" "the type of the SR's content, if required (e.g. ISOs)";
         field ~qualifier:DynamicRO "shared" ~ty:Bool "true if this SR is (capable of being) shared between multiple hosts";
         field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
         field  ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
         field ~ty:Bool ~qualifier:DynamicRO ~in_oss_since:None ~internal_only:true "default_vdi_visibility" "";
         field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "sm_config" "SM dependent data" ~default_value:(Some (VMap []));
         field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this SR";
         field ~qualifier:DynamicRO ~in_product_since:rel_cowley ~ty:Bool ~default_value:(Some (VBool false)) "local_cache_enabled" "True if this SR is assigned to be the local cache for its host";
         field ~qualifier:DynamicRO ~in_product_since:rel_boston ~ty:(Ref _dr_task) ~default_value:(Some (VRef null_ref)) "introduced_by" "The disaster recovery task which introduced this SR";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_dundee, ""] ~ty:Bool ~default_value:(Some (VBool false)) "clustered" "True if the SR is using aggregated local storage";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_dundee, ""] ~ty:Bool ~default_value:(Some (VBool false)) "is_tools_sr" "True if this is the SR that contains the Tools ISO VDIs";
       ])
    ()

(** XXX: just make this a field and be done with it. Cowardly refusing to change the schema for now. *)
let sm_get_driver_filename = call
    ~name:"get_driver_filename"
    ~in_oss_since:None
    ~in_product_since:rel_orlando
    ~params:[Ref _sm, "self", "The SM to query" ]
    ~result:(String, "The SM's driver_filename field")
    ~doc:"Gets the SM's driver_filename field"
    ()

let storage_plugin =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_sm ~descr:"A storage manager plugin"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[ ]
    ~contents:
      ([ uid _sm;
         namespace ~name:"name" ~contents:(names None DynamicRO) ();
         field ~in_oss_since:None ~qualifier:DynamicRO "type" "SR.type";
         field ~in_oss_since:None ~qualifier:DynamicRO "vendor" "Vendor who created this plugin";
         field ~in_oss_since:None ~qualifier:DynamicRO "copyright" "Entity which owns the copyright of this plugin";
         field ~in_oss_since:None ~qualifier:DynamicRO "version" "Version of the plugin";
         field ~in_oss_since:None ~qualifier:DynamicRO "required_api_version" "Minimum SM API version required on the server";
         field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map(String,String)) "configuration" "names and descriptions of device config keys";
         field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_miami ~lifecycle:[ Deprecated, rel_clearwater, "Use SM.features instead"; ] ~ty:(Set(String)) "capabilities" "capabilities of the SM plugin" ~default_value:(Some (VSet []));
         field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_clearwater ~ty:(Map(String, Int)) "features" "capabilities of the SM plugin, with capability version numbers" ~default_value:(Some (VMap []));
         field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
         field ~in_product_since:rel_orlando ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String "driver_filename" "filename of the storage driver";
         field ~in_product_since:rel_dundee ~qualifier:DynamicRO ~default_value:(Some (VSet [])) ~ty:(Set String) "required_cluster_stack" "The storage plugin requires that one of these cluster stacks is configured and running.";
       ])
    ()

let lvhd_enable_thin_provisioning = call
    ~name:"enable_thin_provisioning"
    ~in_oss_since:None
    ~in_product_since:rel_dundee
    ~allowed_roles:_R_POOL_ADMIN
    ~params:[
      Ref _host, "host", "The LVHD Host to upgrade to being thin-provisioned.";
      Ref _sr, "SR", "The LVHD SR to upgrade to being thin-provisioned.";
      Int, "initial_allocation", "The initial amount of space to allocate to a newly-created VDI in bytes";
      Int, "allocation_quantum", "The amount of space to allocate to a VDI when it needs to be enlarged in bytes";
    ]
    ~doc:"Upgrades an LVHD SR to enable thin-provisioning. Future VDIs created in this SR will be thinly-provisioned, although existing VDIs will be left alone. Note that the SR must be attached to the SRmaster for upgrade to work."
    ~forward_to:(HostExtension "LVHD.enable_thin_provisioning")
    ~result:(String, "Message from LVHD.enable_thin_provisioning extension")
    ()

let lvhd =
  create_obj ~in_db:true ~in_product_since:rel_dundee ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_lvhd ~descr:"LVHD SR specific operations"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages:[
      lvhd_enable_thin_provisioning;
    ]
    ~contents: [
      uid _lvhd;
    ]
    ()

(* --- rws: removed this after talking to Andy and Julian
   let filesystem =
   { name = _filesystem; description = "An on-disk filesystem";
    messages = [];
    contents =
      field "uuid" "globally-unique ID" ::
    let field ?(ty=Int) = field ~qualifier:DynamicRO ~ty in
    [ field "block_size" "block size";
      field "total_blocks" "total blocks on disk";
      field "available_blocks" "blocks available for allocation";
      field "used_blocks" "blocks already in use";
      field "percentage_free" "Percentage of free space left in filesystem";
      field ~ty:String "type" "filesystem type" ] }
*)


(** Each disk is associated with a vdi_type: (a 'style' of disk?) *)
let vdi_type = Enum ("vdi_type", [ "system",    "a disk that may be replaced on upgrade";
                                   "user",      "a disk that is always preserved on upgrade";
                                   "ephemeral", "a disk that may be reformatted on upgrade";
                                   "suspend",   "a disk that stores a suspend image";
                                   "crashdump", "a disk that stores VM crashdump information";
                                   "ha_statefile", "a disk used for HA storage heartbeating";
                                   "metadata", "a disk used for HA Pool metadata";
                                   "redo_log", "a disk used for a general metadata redo-log";
                                   "rrd", "a disk that stores SR-level RRDs";
                                   "pvs_cache", "a disk that stores PVS cache data";
                                   "cbt_metadata", "Metadata about a snapshot VDI that has been deleted: the set of blocks that changed between some previous version of the disk and the version tracked by the snapshot.";
                                 ])

let vdi_introduce_params first_rel =
  [
    {param_type=String; param_name="uuid"; param_doc="The uuid of the disk to introduce"; param_release=first_rel; param_default=None};
    {param_type=String; param_name="name_label"; param_doc="The name of the disk record"; param_release=first_rel; param_default=None};
    {param_type=String; param_name="name_description"; param_doc="The description of the disk record"; param_release=first_rel; param_default=None};
    {param_type=Ref _sr; param_name="SR"; param_doc="The SR that the VDI is in"; param_release=first_rel; param_default=None};
    {param_type=vdi_type; param_name="type"; param_doc="The type of the VDI"; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="sharable"; param_doc="true if this disk may be shared"; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="read_only"; param_doc="true if this disk may ONLY be mounted read-only"; param_release=first_rel; param_default=None};
    {param_type=Map(String, String); param_name="other_config"; param_doc="additional configuration"; param_release=first_rel; param_default=None};
    {param_type=String; param_name="location"; param_doc="location information"; param_release=first_rel; param_default=None};
    {param_type=Map(String, String); param_name="xenstore_data"; param_doc="Data to insert into xenstore"; param_release=first_rel; param_default=Some (VMap [])};
    {param_type=Map(String, String); param_name="sm_config"; param_doc="Storage-specific config"; param_release=miami_release; param_default=Some (VMap [])};
    {param_type=Bool; param_name = "managed"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VBool true) };
    {param_type=Int; param_name="virtual_size"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VInt 0L) };
    {param_type=Int; param_name="physical_utilisation"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VInt 0L) };
    {param_type=Ref _pool; param_name="metadata_of_pool"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VRef "") };
    {param_type=Bool; param_name="is_a_snapshot"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VBool false) };
    {param_type=DateTime; param_name="snapshot_time"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VDateTime Date.never) };
    {param_type=Ref _vdi; param_name="snapshot_of"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VRef "") };


  ]

(* This used to be called VDI.introduce but it was always an internal call *)
let vdi_pool_introduce = call
    ~name:"pool_introduce"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~versioned_params:(
      (vdi_introduce_params miami_release) @
      [{ param_type=Bool; param_name="cbt_enabled"; param_doc="True if changed blocks are tracked for this VDI"; param_release=inverness_release; param_default= Some(VBool false) }]
    )
    ~doc:"Create a new VDI record in the database only"
    ~result:(Ref _vdi, "The ref of the newly created VDI record.")
    ~hide_from_docs:true
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_db_introduce = { vdi_pool_introduce with msg_name = "db_introduce"; msg_hide_from_docs = false }

let vdi_db_forget = call
    ~name:"db_forget"
    ~in_oss_since:None
    ~params:[Ref _vdi, "vdi", "The VDI to forget about"]
    ~doc:"Removes a VDI record from the database"
    ~in_product_since:rel_miami
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_introduce = call
    ~name:"introduce"
    ~in_oss_since:None
    ~versioned_params:(vdi_introduce_params rio_release)
    ~doc:"Create a new VDI record in the database only"
    ~result:(Ref _vdi, "The ref of the newly created VDI record.")
    ~errs:[Api_errors.sr_operation_not_supported]
    ~in_product_since:rel_miami
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_forget = call
    ~name:"forget"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[Ref _vdi, "vdi", "The VDI to forget about"]
    ~doc:"Removes a VDI record from the database"
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_force_unlock = call
    ~name:"force_unlock"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~internal_deprecated_since:rel_miami
    ~params:[Ref _vdi, "vdi", "The VDI to forcibly unlock"]
    ~doc:"Steals the lock on this VDI and leaves it unlocked. This function is extremely dangerous. This call is deprecated."
    ~hide_from_docs:true
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_update = call
    ~name:"update"
    ~in_oss_since:None
    ~params:[Ref _vdi, "vdi", "The VDI whose stats (eg size) should be updated" ]
    ~doc:"Ask the storage backend to refresh the fields in the VDI object"
    ~errs:[Api_errors.sr_operation_not_supported]
    ~in_product_since:rel_symc
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_operations =
  Enum ("vdi_operations",
        [ "clone", "Cloning the VDI";
          "copy", "Copying the VDI";
          "resize", "Resizing the VDI";
          "resize_online", "Resizing the VDI which may or may not be online";
          "snapshot", "Snapshotting the VDI";
          "mirror", "Mirroring the VDI";
          "destroy", "Destroying the VDI";
          "forget", "Forget about the VDI";
          "update", "Refreshing the fields of the VDI";
          "force_unlock", "Forcibly unlocking the VDI";
          "generate_config", "Generating static configuration";
          "enable_cbt", "Enabling changed block tracking for a VDI";
          "disable_cbt", "Disabling changed block tracking for a VDI";
          "data_destroy", "Deleting the data of the VDI";
          "list_changed_blocks", "Exporting a bitmap that shows the changed blocks between two VDIs";
          "set_on_boot", "Setting the on_boot field of the VDI";
          "blocked", "Operations on this VDI are temporarily blocked";
        ])

let vdi_set_missing = call
    ~name:"set_missing"
    ~in_oss_since:None
    ~in_product_since:rel_miami
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Bool, "value", "The new value of the VDI's missing field"]
    ~doc:"Sets the VDI's missing field"
    ~flags:[`Session]
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_read_only = call
    ~name:"set_read_only"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Bool, "value", "The new value of the VDI's read_only field"]
    ~flags:[`Session]
    ~doc:"Sets the VDI's read_only field"
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_sharable = call
    ~name:"set_sharable"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Bool, "value", "The new value of the VDI's sharable field"]
    ~flags:[`Session]
    ~doc:"Sets the VDI's sharable field"
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_managed = call
    ~name:"set_managed"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Bool, "value", "The new value of the VDI's managed field"]
    ~flags:[`Session]
    ~doc:"Sets the VDI's managed field"
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_virtual_size = call
    ~name:"set_virtual_size"
    ~in_oss_since:None
    ~in_product_since:rel_miami
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Int, "value", "The new value of the VDI's virtual size"]
    ~flags:[`Session]
    ~doc:"Sets the VDI's virtual_size field"
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_physical_utilisation = call
    ~name:"set_physical_utilisation"
    ~in_oss_since:None
    ~in_product_since:rel_miami
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Int, "value", "The new value of the VDI's physical utilisation"]
    ~flags:[`Session]
    ~doc:"Sets the VDI's physical_utilisation field"
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_is_a_snapshot = call
    ~name:"set_is_a_snapshot"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Bool, "value", "The new value indicating whether this VDI is a snapshot"]
    ~flags:[`Session]
    ~doc:"Sets whether this VDI is a snapshot"
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_snapshot_of = call
    ~name:"set_snapshot_of"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Ref _vdi, "value", "The VDI of which this VDI is a snapshot"]
    ~flags:[`Session]
    ~doc:"Sets the VDI of which this VDI is a snapshot"
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_snapshot_time = call
    ~name:"set_snapshot_time"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _vdi, "self", "The VDI to modify";
             DateTime, "value", "The snapshot time of this VDI."]
    ~flags:[`Session]
    ~doc:"Sets the snapshot time of this VDI."
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_metadata_of_pool = call
    ~name:"set_metadata_of_pool"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Ref _pool, "value", "The pool whose metadata is contained by this VDI"]
    ~flags:[`Session]
    ~doc:"Records the pool whose metadata is contained by this VDI."
    ~allowed_roles:_R_VM_ADMIN
    ()

(** An API call for debugging and testing only *)
let vdi_generate_config = call
    ~name:"generate_config"
    ~in_oss_since:None
    ~in_product_since:rel_orlando
    ~params:[Ref _host, "host", "The host on which to generate the configuration";
             Ref _vdi, "vdi", "The VDI to generate the configuration for" ]
    ~result:(String, "The generated static configuration")
    ~doc:"Internal function for debugging only"
    ~hide_from_docs:true
    ~allowed_roles:_R_VM_ADMIN
    ()

let on_boot = Enum ("on_boot", [
    "reset", "When a VM containing this VDI is started, the contents of the VDI are reset to the state they were in when this flag was last set.";
    "persist", "Standard behaviour.";
  ])

let vdi_set_on_boot = call
    ~name:"set_on_boot"
    ~in_oss_since:None
    ~in_product_since:rel_cowley
    ~params:[Ref _vdi, "self", "The VDI to modify";
             on_boot, "value", "The value to set"]
    ~doc:"Set the value of the on_boot parameter. This value can only be changed when the VDI is not attached to a running VM."
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_allow_caching = call
    ~name:"set_allow_caching"
    ~in_oss_since:None
    ~in_product_since:rel_cowley
    ~params:[Ref _vdi, "self", "The VDI to modify";
             Bool, "value", "The value to set"]
    ~doc:"Set the value of the allow_caching parameter. This value can only be changed when the VDI is not attached to a running VM. The caching behaviour is only affected by this flag for VHD-based VDIs that have one parent and no child VHDs. Moreover, caching only takes place when the host running the VM containing this VDI has a nominated SR for local caching."
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_name_label = call
    ~name:"set_name_label"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[Ref _vdi, "self", "The VDI to modify";
             String, "value", "The name lable for the VDI"]
    ~doc:"Set the name label of the VDI. This can only happen when then its SR is currently attached."
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_set_name_description = call
    ~name:"set_name_description"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[Ref _vdi, "self", "The VDI to modify";
             String, "value", "The name description for the VDI"]
    ~doc:"Set the name description of the VDI. This can only happen when its SR is currently attached."
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_open_database = call
    ~name:"open_database"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _vdi, "self", "The VDI which contains the database to open"]
    ~result:(Ref _session, "A session which can be used to query the database")
    ~doc:"Load the metadata found on the supplied VDI and return a session reference which can be used in XenAPI calls to query its contents."
    ~allowed_roles:_R_POOL_OP
    ()

let vdi_checksum = call
    ~name:"checksum"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _vdi, "self", "The VDI to checksum"]
    ~result:(String, "The md5sum of the vdi")
    ~doc:"Internal function to calculate VDI checksum and return a string"
    ~hide_from_docs:true
    ~allowed_roles:_R_VM_ADMIN (* Conceptually, this is not correct. We do it
                                  	                              this way only to follow the previous
                                  	                              convention. It is supposed to fix by future
                                  	                              version of RBAC *)
    ()

let vdi_read_database_pool_uuid = call
    ~name:"read_database_pool_uuid"
    ~in_oss_since:None
    ~in_product_since:rel_boston
    ~params:[Ref _vdi, "self", "The metadata VDI to look up in the cache."]
    ~result:(String, "The cached pool UUID of the database on the VDI.")
    ~doc:"Check the VDI cache for the pool UUID of the database on this VDI."
    ~allowed_roles:_R_READ_ONLY
    ()

let vdi_enable_cbt = call
    ~name:"enable_cbt"
    ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~params:[Ref _vdi, "self", "The VDI for which CBT should be enabled"]
    ~errs:[
      Api_errors.sr_operation_not_supported;
      Api_errors.vdi_missing;
      Api_errors.sr_not_attached;
      Api_errors.sr_no_pbds;
      Api_errors.operation_not_allowed;
      Api_errors.vdi_incompatible_type;
      Api_errors.vdi_on_boot_mode_incompatible_with_operation;
    ]
    ~doc:"Enable changed block tracking for the VDI. This call is idempotent - enabling CBT for a VDI for which CBT is already enabled results in a no-op, and no error will be thrown."
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_disable_cbt = call
    ~name:"disable_cbt"
    ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~params:[Ref _vdi, "self", "The VDI for which CBT should be disabled"]
    ~errs:[
      Api_errors.sr_operation_not_supported;
      Api_errors.vdi_missing;
      Api_errors.sr_not_attached;
      Api_errors.sr_no_pbds;
      Api_errors.operation_not_allowed;
      Api_errors.vdi_incompatible_type;
      Api_errors.vdi_on_boot_mode_incompatible_with_operation;
    ]
    ~doc:"Disable changed block tracking for the VDI. This call is only allowed on VDIs that support enabling CBT. It is an idempotent operation - disabling CBT for a VDI for which CBT is not enabled results in a no-op, and no error will be thrown."
    ~allowed_roles:_R_VM_ADMIN
    ()

(** This command is for internal use by SM to set the cbt_enabled field when it needs to disable cbt for its own reasons. This command should be removed once SMAPIv3 is implemented *)
let vdi_set_cbt_enabled = call
    ~name:"set_cbt_enabled"
    ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~params:[Ref _vdi, "self", "The VDI for which CBT enabled status should be set";
             Bool, "value", "The value to set"]
    ~errs:[]
    ~hide_from_docs:true
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let vdi_data_destroy = call
    ~name:"data_destroy"
    ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~params:[Ref _vdi, "self", "The VDI whose data should be deleted."]
    ~errs:[
      Api_errors.sr_operation_not_supported;
      Api_errors.vdi_missing;
      Api_errors.sr_not_attached;
      Api_errors.sr_no_pbds;
      Api_errors.operation_not_allowed;
      Api_errors.vdi_incompatible_type;
      Api_errors.vdi_no_cbt_metadata;
      Api_errors.vdi_in_use;
      Api_errors.vdi_is_a_physical_device;
    ]
    ~doc:"Delete the data of the snapshot VDI, but keep its changed block tracking metadata. When successful, this call changes the type of the VDI to cbt_metadata. This operation is idempotent: calling it on a VDI of type cbt_metadata results in a no-op, and no error will be thrown."
    ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_list_changed_blocks = call
    ~name:"list_changed_blocks"
    ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~params:
      [ Ref _vdi, "vdi_from", "The first VDI."
      ; Ref _vdi, "vdi_to", "The second VDI."
      ]
    ~errs:
      [ Api_errors.sr_operation_not_supported
      ; Api_errors.vdi_missing
      ; Api_errors.sr_not_attached
      ; Api_errors.sr_no_pbds
      ; Api_errors.vdi_in_use
      ]
    ~result:(String, "A base64 string-encoding of the bitmap showing which blocks differ in the two VDIs.")
    ~doc:"Compare two VDIs in 64k block increments and report which blocks differ. This operation is not allowed when vdi_to is attached to a VM."
    ~allowed_roles:_R_VM_OP
    ()

module Vdi_nbd_server_info = struct
  let vdi_nbd_server_info =
    let lifecycle = [Published, rel_inverness, ""] in
    create_obj
      ~in_db:false
      ~persist:PersistNothing
      ~gen_constructor_destructor:false
      ~lifecycle
      ~in_oss_since:None
      ~name:_vdi_nbd_server_info
      ~descr:"Details for connecting to a VDI using the Network Block Device protocol"
      ~gen_events:false
      ~messages:[]
      ~doccomments:[]
      ~messages_default_allowed_roles:(Some []) (* No messages, so no roles allowed to use them *)
      ~contents:
        [ (* uid _vdi_nbd_server_info; The uuid is not needed here and only adds inconvenience. *)
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "exportname" "The exportname to request over NBD. This holds details including an authentication token, so it must be protected appropriately. Clients should regard the exportname as an opaque string or token.";
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "address" "An address on which the server can be reached; this can be IPv4, IPv6, or a DNS name.";
          field ~qualifier:DynamicRO ~lifecycle ~ty:Int "port" "The TCP port";
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "cert" "The TLS certificate of the server";
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "subject" "For convenience, this redundant field holds a DNS (hostname) subject of the certificate. This can be a wildcard, but only for a certificate that has a wildcard subject and no concrete hostname subjects.";
        ] ()
end
let vdi_nbd_server_info = Vdi_nbd_server_info.vdi_nbd_server_info

let vdi_get_nbd_info = call
    ~name:"get_nbd_info"
    ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~params:[Ref _vdi, "self", "The VDI to access via Network Block Device protocol"]
    ~errs: [Api_errors.vdi_incompatible_type]
    ~result:(Set (Record _vdi_nbd_server_info), "The details necessary for connecting to the VDI over NBD. This includes an authentication token, so must be treated as sensitive material and must not be sent over insecure networks.")
    ~doc:"Get details specifying how to access this VDI via a Network Block Device server. For each of a set of NBD server addresses on which the VDI is available, the return value set contains a vdi_nbd_server_info object that contains an exportname to request once the NBD connection is established, and connection details for the address. An empty list is returned if there is no network that has a PIF on a host with access to the relevant SR, or if no such network has been assigned an NBD-related purpose in its purpose field. To access the given VDI, any of the vdi_nbd_server_info objects can be used to make a connection to a server, and then the VDI will be available by requesting the exportname."
    ~flags:[`Session] (* no async *)
    ~allowed_roles:_R_VM_ADMIN
    ()


(** A virtual disk *)
let vdi =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vdi ~descr:"A virtual disk image"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~messages:[vdi_snapshot; vdi_clone; vdi_resize;
               vdi_resize_online;
               vdi_introduce; vdi_pool_introduce;
               vdi_db_introduce; vdi_db_forget;
               vdi_update;
               vdi_copy;
               vdi_force_unlock; vdi_set_managed;
               vdi_forget;
               vdi_set_sharable;
               vdi_set_read_only;
               vdi_set_missing;
               vdi_set_virtual_size;
               vdi_set_physical_utilisation;
               vdi_set_is_a_snapshot;
               vdi_set_snapshot_of;
               vdi_set_snapshot_time;
               vdi_set_metadata_of_pool;
               vdi_set_name_label;
               vdi_set_name_description;
               vdi_generate_config;
               vdi_set_on_boot;
               vdi_set_allow_caching;
               vdi_open_database;
               vdi_checksum;
               vdi_read_database_pool_uuid;
               vdi_pool_migrate;
               vdi_enable_cbt;
               vdi_disable_cbt;
               vdi_set_cbt_enabled;
               vdi_data_destroy;
               vdi_list_changed_blocks;
               vdi_get_nbd_info;
              ]
    ~contents:
      ([ uid _vdi;
         namespace ~name:"name" ~contents:(names oss_since_303 StaticRO) ();
       ] @ (allowed_and_current_operations vdi_operations) @ [
         field ~qualifier:StaticRO ~ty:(Ref _sr) "SR" "storage repository in which the VDI resides";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _vbd)) "VBDs" "list of vbds that refer to this disk";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _crashdump)) "crash_dumps" "list of crash dumps that refer to this disk";
         field ~qualifier:StaticRO ~ty:Int "virtual_size" "size of disk as presented to the guest (in bytes). Note that, depending on storage backend type, requested size may not be respected exactly";
         field ~qualifier:DynamicRO ~ty:Int "physical_utilisation" "amount of physical space that the disk image is currently taking up on the storage repository (in bytes)";
         field ~qualifier:StaticRO ~ty:vdi_type "type" "type of the VDI";
         field ~qualifier:StaticRO ~ty:Bool "sharable" "true if this disk may be shared";
         field ~qualifier:StaticRO ~ty:Bool "read_only" "true if this disk may ONLY be mounted read-only";
         field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
         field ~qualifier:DynamicRO ~ty:Bool "storage_lock" "true if this disk is locked at the storage level";
         (* XXX: location field was in the database in rio, now API in miami *)
         field ~in_oss_since:None ~in_product_since:rel_miami ~ty:String ~qualifier:DynamicRO ~default_value:(Some (VString "")) "location" "location information";
         field ~in_oss_since:None ~ty:Bool ~qualifier:DynamicRO "managed" "";
         field ~in_oss_since:None ~ty:Bool ~qualifier:DynamicRO "missing" "true if SR scan operation reported this VDI as not present on disk";
         field ~in_oss_since:None ~ty:(Ref _vdi) ~qualifier:DynamicRO ~lifecycle:[Published, rel_rio, ""; Deprecated, rel_ely, "The field was never used."] "parent" "This field is always null. Deprecated";
         field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "xenstore_data" "data to be inserted into the xenstore tree (/local/domain/0/backend/vbd/<domid>/<device-id>/sm-data) after the VDI is attached. This is generally set by the SM backends on vdi_attach." ~default_value:(Some (VMap []));
         field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "sm_config" "SM dependent data" ~default_value:(Some (VMap []));

         field ~in_product_since:rel_orlando ~default_value:(Some (VBool false))          ~qualifier:DynamicRO ~ty:Bool ~doc_tags:[Snapshots] "is_a_snapshot" "true if this is a snapshot.";
         field ~in_product_since:rel_orlando ~default_value:(Some (VRef ""))              ~qualifier:DynamicRO ~ty:(Ref _vdi) ~doc_tags:[Snapshots] "snapshot_of" "Ref pointing to the VDI this snapshot is of.";
         field ~in_product_since:rel_orlando                                              ~qualifier:DynamicRO ~ty:(Set (Ref _vdi)) ~doc_tags:[Snapshots] "snapshots" "List pointing to all the VDIs snapshots.";
         field ~in_product_since:rel_orlando ~default_value:(Some (VDateTime Date.never)) ~qualifier:DynamicRO ~ty:DateTime ~doc_tags:[Snapshots] "snapshot_time" "Date/time when this snapshot was created.";
         field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
         field ~in_product_since:rel_cowley ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "allow_caching" "true if this VDI is to be cached in the local cache SR";
         field ~in_product_since:rel_cowley ~qualifier:DynamicRO ~ty:on_boot ~default_value:(Some (VEnum "persist")) "on_boot" "The behaviour of this VDI on a VM boot";
         field ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:(Ref _pool) ~default_value:(Some (VRef null_ref)) "metadata_of_pool" "The pool whose metadata is contained in this VDI";
         field ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "metadata_latest" "Whether this VDI contains the latest known accessible metadata for the pool";
         field ~lifecycle:[Published, rel_dundee, ""] ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "is_tools_iso" "Whether this VDI is a Tools ISO";
         field ~lifecycle:[Published, rel_inverness, ""] ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "cbt_enabled" "True if changed blocks are tracked for this VDI" ~doc_tags:[Snapshots];
       ])
    ()

(** Virtual disk interfaces have a mode parameter: *)
let vbd_mode = Enum ("vbd_mode", [ "RO", "only read-only access will be allowed";
                                   "RW", "read-write access will be allowed" ])

let vbd_type = Enum ("vbd_type",
                     [ "CD", "VBD will appear to guest as CD";
                       "Disk", "VBD will appear to guest as disk";
                       "Floppy", "VBD will appear as a floppy"])

let vbd_operations =
  Enum ("vbd_operations",
        [ "attach", "Attempting to attach this VBD to a VM";
          "eject", "Attempting to eject the media from this VBD";
          "insert", "Attempting to insert new media into this VBD";
          "plug", "Attempting to hotplug this VBD";
          "unplug", "Attempting to hot unplug this VBD";
          "unplug_force", "Attempting to forcibly unplug this VBD";
          "pause", "Attempting to pause a block device backend";
          "unpause", "Attempting to unpause a block device backend";
        ])

(** A virtual disk interface *)
let vbd =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vbd ~descr:"A virtual block device"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~messages: [ vbd_eject; vbd_insert; vbd_plug; vbd_unplug; vbd_unplug_force; vbd_unplug_force_no_safety_check; vbd_assert_attachable;
                 vbd_pause; vbd_unpause;
               ]
    ~contents:
      ([ uid _vbd;
       ] @ (allowed_and_current_operations vbd_operations) @ [
         field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine";
         field ~qualifier:StaticRO ~ty:(Ref _vdi) "VDI" "the virtual disk";

         field ~qualifier:DynamicRO "device" "device seen by the guest e.g. hda1";
         field "userdevice" "user-friendly device name e.g. 0,1,2,etc.";
         field ~ty:Bool "bootable" "true if this VBD is bootable";
         field ~effect:true ~ty:vbd_mode "mode" "the mode the VBD should be mounted with";
         field ~ty:vbd_type "type" "how the VBD will appear to the guest (e.g. disk or CD)";
         field ~in_oss_since:None ~in_product_since:rel_miami ~ty:Bool ~default_value:(Some (VBool true))
           "unpluggable" "true if this VBD will support hot-unplug";
         field ~qualifier:DynamicRO ~ty:Bool "storage_lock" "true if a storage level lock was acquired";
         field ~qualifier:StaticRO ~ty:Bool "empty" "if true this represents an empty drive";
         field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "reserved" "true if the VBD is reserved pending a reboot/migrate";
         field ~ty:(Map(String, String)) "other_config" "additional configuration";
       ]
       @ device_status_fields @
       [ namespace ~name:"qos" ~contents:(qos "VBD") (); ] @
       [ field ~qualifier:DynamicRO ~ty:(Ref _vbd_metrics) ~lifecycle: [Removed, rel_tampa, "Disabled in favour of RRDs"] "metrics" "metrics associated with this VBD"; ])
    ()

let vbd_metrics =
  create_obj
    ~lifecycle:
      [ Published, rel_rio, "The metrics associated with a virtual block device"
      ; Removed, rel_tampa, "Disabled in favour of RRD"
      ]
    ~in_db:true
    ~in_oss_since:oss_since_303
    ~persist:PersistNothing
    ~gen_constructor_destructor:false
    ~name:_vbd_metrics
    ~descr:"The metrics associated with a virtual block device"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~messages:[] ~contents:
    [ uid _vbd_metrics;
      namespace ~name:"io" ~contents:iobandwidth ();
      field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
      field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ]
    ()

let crashdump_destroy = call
    ~name:"destroy"
    ~in_product_since:rel_rio
    ~doc:"Destroy the specified crashdump"
    ~params:[Ref _crashdump, "self", "The crashdump to destroy"]
    ~allowed_roles:_R_POOL_OP
    ()


(** A crashdump for a particular VM, stored in a particular VDI *)
let crashdump =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:(Some rel_inverness) ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_crashdump ~descr:"A VM crashdump"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages: [crashdump_destroy]
    ~contents:
      ([ uid _crashdump;
         field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine";
         field ~qualifier:StaticRO ~ty:(Ref _vdi) "VDI" "the virtual disk";
         field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
       ])
    ()

let pool_operations =
  Enum ("pool_allowed_operations",
        [ "ha_enable", "Indicates this pool is in the process of enabling HA";
          "ha_disable", "Indicates this pool is in the process of disabling HA";
        ])

let pool_enable_ha = call
    ~in_product_since:rel_miami
    ~name:"enable_ha"
    ~in_oss_since:None
    ~versioned_params:
      [{param_type=Set(Ref _sr); param_name="heartbeat_srs"; param_doc="Set of SRs to use for storage heartbeating"; param_release=miami_release; param_default=None };
       {param_type=Map(String, String); param_name="configuration"; param_doc="Detailed HA configuration to apply"; param_release=miami_release; param_default=None };
      ]
    ~doc:"Turn on High Availability mode"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_disable_ha = call
    ~in_product_since:rel_miami
    ~name:"disable_ha"
    ~in_oss_since:None
    ~params:[]
    ~doc:"Turn off High Availability mode"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_sync_database = call
    ~name:"sync_database"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[]
    ~doc:"Forcibly synchronise the database now"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_designate_new_master = call
    ~in_product_since:rel_miami
    ~name:"designate_new_master"
    ~in_oss_since:None
    ~params:[Ref _host, "host", "The host who should become the new master"]
    ~doc:"Perform an orderly handover of the role of master to the referenced host."
    ~allowed_roles:_R_POOL_OP
    ()

let pool_join = call
    ~name:"join"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[String, "master_address", "The hostname of the master of the pool to join";
             String, "master_username", "The username of the master (for initial authentication)";
             String, "master_password", "The password for the master (for initial authentication)";
            ]
    ~errs:[Api_errors.pool_joining_host_cannot_contain_shared_SRs]
    ~doc:"Instruct host to join a new pool"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_join_force = call
    ~name:"join_force"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[String, "master_address", "The hostname of the master of the pool to join";
             String, "master_username", "The username of the master (for initial authentication)";
             String, "master_password", "The password for the master (for initial authentication)";
            ]
    ~doc:"Instruct host to join a new pool"
    ~allowed_roles:_R_POOL_OP
    ()


let pool_slave_reset_master = call ~flags:[`Session]
    ~name:"emergency_reset_master"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[
      String, "master_address", "The hostname of the master";
    ]
    ~doc:"Instruct a slave already in a pool that the master has changed"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_transition_to_master = call ~flags:[`Session]
    ~name:"emergency_transition_to_master"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[]
    ~doc:"Instruct host that's currently a slave to transition to being master"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_recover_slaves = call
    ~name:"recover_slaves"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[]
    ~result:(Set (Ref _host), "list of hosts whose master address were successfully reset")
    ~doc:"Instruct a pool master, M, to try and contact its slaves and, if slaves are in emergency mode, reset their master address to M."
    ~allowed_roles:_R_POOL_OP
    ()

let pool_eject = call
    ~name:"eject"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[Ref _host, "host", "The host to eject"]
    ~doc:"Instruct a pool master to eject a host from the pool"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_initial_auth = call
    ~name:"initial_auth"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[]
    ~result:(String, "")
    ~doc:"Internal use only"
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_OP
    ()

let pool_create_VLAN_from_PIF = call
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~name:"create_VLAN_from_PIF"
    ~doc:"Create a pool-wide VLAN by taking the PIF."
    ~params:[Ref _pif, "pif", "physical interface on any particular host, that identifies the PIF on which to create the (pool-wide) VLAN interface";
             Ref _network, "network", "network to which this interface should be connected";
             Int, "VLAN", "VLAN tag for the new interface"]
    ~result:(Set (Ref _pif), "The references of the created PIF objects")
    ~errs:[Api_errors.vlan_tag_invalid]
    ~allowed_roles:_R_POOL_OP
    ()

(* !! THIS IS BROKEN; it takes a device name which in the case of a bond is not homogeneous across all pool hosts.
      See CA-22613. !! *)
let pool_create_VLAN = call
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~name:"create_VLAN"
    ~doc:"Create PIFs, mapping a network to the same physical interface/VLAN on each host. This call is deprecated: use Pool.create_VLAN_from_PIF instead."
    ~params:[String, "device", "physical interface on which to create the VLAN interface";
             Ref _network, "network", "network to which this interface should be connected";
             Int, "VLAN", "VLAN tag for the new interface"]
    ~result:(Set (Ref _pif), "The references of the created PIF objects")
    ~errs:[Api_errors.vlan_tag_invalid]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_management_reconfigure = call
    ~name:"management_reconfigure"
    ~in_oss_since:None
    ~in_product_since:rel_inverness
    ~params:[
      Ref _network, "network", "The network";
    ]
    ~doc:"Reconfigure the management network interface for all Hosts in the Pool"
    ~errs:[ Api_errors.ha_is_enabled;
            Api_errors.pif_not_present;
            Api_errors.cannot_plug_bond_slave;
            Api_errors.pif_incompatible_primary_address_type;
            Api_errors.pif_has_no_network_configuration;
            Api_errors.pif_has_no_v6_network_configuration
          ]
    ~allowed_roles:_R_POOL_OP
    ()

let hello_return = Enum("hello_return", [
    "ok", "";
    "unknown_host", "";
    "cannot_talk_back", ""
  ])

let pool_hello = call
    ~name:"hello"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[String, "host_uuid", "";
             String, "host_address", ""
            ]
    ~result:(hello_return, "")
    ~doc:"Internal use only"
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_OP
    ()

let pool_slave_network_report = call
    ~name:"slave_network_report"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~doc:"Internal use only"
    ~params:[Map (String, String), "phydevs", "(device,bridge) pairs of physical NICs on slave";
             Map (String, String), "dev_to_mac", "(device,mac) pairs of physical NICs on slave";
             Map (String, Int), "dev_to_mtu", "(device,mtu) pairs of physical NICs on slave";
             Ref _host, "slave_host", "the host that the PIFs will be attached to when created"
            ]
    ~result:(Set(Ref _pif), "refs for pifs corresponding to device list")
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_ping_slave = call ~flags:[`Session]
    ~name:"is_slave"
    ~in_oss_since:None
    ~in_product_since:rel_rio
    ~params:[Ref _host, "host", ""]
    ~doc:"Internal use only"
    ~result:(Bool, "returns false if pinged host is master [indicating critical error condition]; true if pinged host is slave")
    ~hide_from_docs:true
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_ha_prevent_restarts_for = call ~flags:[`Session]
    ~name:"ha_prevent_restarts_for"
    ~in_product_since:rel_orlando_update_1
    ~doc:"When this call returns the VM restart logic will not run for the requested number of seconds. If the argument is zero then the restart thread is immediately unblocked"
    ~params:[Int, "seconds", "The number of seconds to block the restart thread for"]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_ha_failover_plan_exists = call ~flags:[`Session]
    ~name:"ha_failover_plan_exists"
    ~in_product_since:rel_orlando
    ~doc:"Returns true if a VM failover plan exists for up to 'n' host failures"
    ~params:[Int, "n", "The number of host failures to plan for" ]
    ~result:(Bool, "true if a failover plan exists for the supplied number of host failures")
    ~allowed_roles:_R_POOL_OP
    ()

let pool_ha_compute_max_host_failures_to_tolerate = call ~flags:[`Session]
    ~name:"ha_compute_max_host_failures_to_tolerate"
    ~in_product_since:rel_orlando
    ~doc:"Returns the maximum number of host failures we could tolerate before we would be unable to restart configured VMs"
    ~params:[]
    ~result:(Int, "maximum value for ha_host_failures_to_tolerate given current configuration")
    ~allowed_roles:_R_POOL_OP
    ()

let pool_ha_compute_hypothetical_max_host_failures_to_tolerate = call ~flags:[`Session]
    ~name:"ha_compute_hypothetical_max_host_failures_to_tolerate"
    ~in_product_since:rel_orlando
    ~doc:"Returns the maximum number of host failures we could tolerate before we would be unable to restart the provided VMs"
    ~params:[ Map(Ref _vm, String), "configuration", "Map of protected VM reference to restart priority" ]
    ~result:(Int, "maximum value for ha_host_failures_to_tolerate given provided configuration")
    ~allowed_roles:_R_READ_ONLY
    ()

let pool_ha_compute_vm_failover_plan = call ~flags:[`Session]
    ~name:"ha_compute_vm_failover_plan"
    ~in_product_since:rel_orlando
    ~doc:"Return a VM failover plan assuming a given subset of hosts fail"
    ~params:[Set(Ref _host), "failed_hosts", "The set of hosts to assume have failed";
             Set(Ref _vm), "failed_vms", "The set of VMs to restart" ]
    ~result:(Map(Ref _vm, Map(String, String)), "VM failover plan: a map of VM to host to restart the host on")
    ~allowed_roles:_R_POOL_OP
    ()

let pool_create_new_blob = call
    ~name: "create_new_blob"
    ~in_product_since:rel_orlando
    ~doc:"Create a placeholder for a named binary blob of data that is associated with this pool"
    ~versioned_params:
      [{param_type=Ref _pool; param_name="pool"; param_doc="The pool"; param_release=orlando_release; param_default=None};
       {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
       {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
       {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
      ]
    ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
    ~allowed_roles:_R_POOL_OP
    ()

let pool_set_ha_host_failures_to_tolerate = call
    ~name:"set_ha_host_failures_to_tolerate"
    ~in_product_since:rel_orlando
    ~doc:"Set the maximum number of host failures to consider in the HA VM restart planner"
    ~params:[Ref _pool, "self", "The pool";
             Int, "value", "New number of host failures to consider"]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_ha_schedule_plan_recomputation = call
    ~name:"ha_schedule_plan_recomputation"
    ~in_product_since:rel_orlando
    ~doc:"Signal that the plan should be recomputed (eg a host has come online)"
    ~params:[]
    ~hide_from_docs:true
    ~pool_internal:true
    ~allowed_roles:_R_LOCAL_ROOT_ONLY
    ()

let pool_enable_binary_storage = call
    ~name:"enable_binary_storage"
    ~in_product_since:rel_orlando
    ~hide_from_docs:true
    ~doc:"Enable the storage of larger objects, such as RRDs, messages and binary blobs across all hosts in the pool"
    ~params:[]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_disable_binary_storage = call
    ~name:"disable_binary_storage"
    ~in_product_since:rel_orlando
    ~hide_from_docs:true
    ~doc:"Disable the storage of larger objects, such as RRDs, messages and binary blobs across all hosts in the pool. This will destroy all of these objects where they exist."
    ~params:[]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_enable_external_auth = call ~flags:[`Session]
    ~name:"enable_external_auth"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~params:[
      Ref _pool, "pool", "The pool whose external authentication should be enabled";
      Map (String,String), "config", "A list of key-values containing the configuration data" ;
      String, "service_name", "The name of the service" ;
      String, "auth_type", "The type of authentication (e.g. AD for Active Directory)"
    ]
    ~doc:"This call enables external authentication on all the hosts of the pool"
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_disable_external_auth = call ~flags:[`Session]
    ~name:"disable_external_auth"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~versioned_params:[
      {param_type=Ref _pool; param_name="pool"; param_doc="The pool whose external authentication should be disabled"; param_release=george_release; param_default=None};
      {param_type=Map (String, String); param_name="config"; param_doc="Optional parameters as a list of key-values containing the configuration data"; param_release=george_release; param_default=Some (VMap [])}
    ]
    ~doc:"This call disables external authentication on all the hosts of the pool"
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_detect_nonhomogeneous_external_auth = call ~flags:[`Session]
    ~name:"detect_nonhomogeneous_external_auth"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~params:[
      Ref _pool, "pool", "The pool where to detect non-homogeneous external authentication configuration";
    ]
    ~doc:"This call asynchronously detects if the external authentication configuration in any slave is different from that in the master and raises appropriate alerts"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_initialize_wlb = call
    ~name:"initialize_wlb"
    ~in_product_since:rel_george
    ~doc:"Initializes workload balancing monitoring on this pool with the specified wlb server"
    ~params:[String, "wlb_url", "The ip address and port to use when accessing the wlb server";
             String, "wlb_username", "The username used to authenticate with the wlb server";
             String, "wlb_password", "The password used to authenticate with the wlb server";
             String, "xenserver_username", "The username used by the wlb server to authenticate with the xenserver";
             String, "xenserver_password", "The password used by the wlb server to authenticate with the xenserver"]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_deconfigure_wlb = call
    ~name:"deconfigure_wlb"
    ~in_product_since:rel_george
    ~doc:"Permanently deconfigures workload balancing monitoring on this pool"
    ~params:[]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_send_wlb_configuration = call
    ~name:"send_wlb_configuration"
    ~in_product_since:rel_george
    ~doc:"Sets the pool optimization criteria for the workload balancing server"
    ~params:[Map(String, String), "config", "The configuration to use in optimizing this pool"]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_retrieve_wlb_configuration = call
    ~name:"retrieve_wlb_configuration"
    ~in_product_since:rel_george
    ~doc:"Retrieves the pool optimization criteria from the workload balancing server"
    ~params:[]
    ~result:(Map(String,String), "The configuration used in optimizing this pool")
    ~allowed_roles:_R_READ_ONLY
    ()

let pool_retrieve_wlb_recommendations = call
    ~name:"retrieve_wlb_recommendations"
    ~in_product_since:rel_george
    ~doc:"Retrieves vm migrate recommendations for the pool from the workload balancing server"
    ~params:[]
    ~result:(Map(Ref _vm,Set(String)), "The list of vm migration recommendations")
    ~allowed_roles:_R_READ_ONLY
    ()

let pool_send_test_post = call
    ~name:"send_test_post"
    ~in_product_since:rel_george
    ~doc:"Send the given body to the given host and port, using HTTPS, and print the response.  This is used for debugging the SSL layer."
    ~params:[(String, "host", ""); (Int, "port", ""); (String, "body", "")]
    ~result:(String, "The response")
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_certificate_install = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~name:"certificate_install"
    ~doc:"Install an SSL certificate pool-wide."
    ~params:[String, "name", "A name to give the certificate";
             String, "cert", "The certificate"]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_certificate_uninstall = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~name:"certificate_uninstall"
    ~doc:"Remove an SSL certificate."
    ~params:[String, "name", "The certificate name"]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_certificate_list = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~name:"certificate_list"
    ~doc:"List all installed SSL certificates."
    ~result:(Set(String),"All installed certificates")
    ~allowed_roles:_R_POOL_OP
    ()

let pool_crl_install = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~name:"crl_install"
    ~doc:"Install an SSL certificate revocation list, pool-wide."
    ~params:[String, "name", "A name to give the CRL";
             String, "cert", "The CRL"]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_crl_uninstall = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~name:"crl_uninstall"
    ~doc:"Remove an SSL certificate revocation list."
    ~params:[String, "name", "The CRL name"]
    ~allowed_roles:_R_POOL_OP
    ()

let pool_crl_list = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~name:"crl_list"
    ~doc:"List all installed SSL certificate revocation lists."
    ~result:(Set(String), "All installed CRLs")
    ~allowed_roles:_R_POOL_OP
    ()

let pool_certificate_sync = call
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~name:"certificate_sync"
    ~doc:"Sync SSL certificates from master to slaves."
    ~allowed_roles:_R_POOL_OP
    ()

let pool_enable_redo_log = call
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~name:"enable_redo_log"
    ~params:[Ref _sr, "sr", "SR to hold the redo log."]
    ~doc:"Enable the redo log on the given SR and start using it, unless HA is enabled."
    ~allowed_roles:_R_POOL_OP
    ()

let pool_disable_redo_log = call
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~name:"disable_redo_log"
    ~doc:"Disable the redo log if in use, unless HA is enabled."
    ~allowed_roles:_R_POOL_OP
    ()

let pool_audit_log_append = call
    ~in_oss_since:None
    ~pool_internal:true
    ~hide_from_docs:true
    ~in_product_since:rel_midnight_ride
    ~name:"audit_log_append"
    ~params:[String, "line", "line to be appended to the audit log"]
    ~doc:"Append a line to the audit log on the master."
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_set_vswitch_controller = call
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~lifecycle:[
      Published, rel_midnight_ride, "Set the IP address of the vswitch controller.";
      Extended, rel_cowley, "Allow to be set to the empty string (no controller is used).";
      Deprecated, rel_falcon, "Deprecated: use 'SDN_controller.introduce' and 'SDN_controller.forget' instead."]
    ~name:"set_vswitch_controller"
    ~params:[String, "address", "IP address of the vswitch controller."]
    ~doc:"Set the IP address of the vswitch controller."
    ~allowed_roles:_R_POOL_OP
    ()

let pool_test_archive_target = call ~flags:[`Session]
    ~name:"test_archive_target"
    ~in_oss_since:None
    ~in_product_since:rel_cowley
    ~params:[Ref _pool, "self", "Reference to the pool";
             Map(String,String), "config", "Location config settings to test";
            ]
    ~doc:"This call tests if a location is valid"
    ~allowed_roles:_R_POOL_OP
    ~result:(String, "An XMLRPC result")
    ()

let pool_enable_local_storage_caching = call
    ~name:"enable_local_storage_caching"
    ~in_oss_since:None
    ~in_product_since:rel_cowley
    ~params:[Ref _pool, "self", "Reference to the pool"]
    ~doc:"This call attempts to enable pool-wide local storage caching"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_disable_local_storage_caching = call
    ~name:"disable_local_storage_caching"
    ~in_oss_since:None
    ~in_product_since:rel_cowley
    ~params:[Ref _pool, "self", "Reference to the pool"]
    ~doc:"This call disables pool-wide local storage caching"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_get_license_state = call
    ~name:"get_license_state"
    ~in_oss_since:None
    ~in_product_since:rel_clearwater
    ~params:[Ref _pool, "self", "Reference to the pool"]
    ~doc:"This call returns the license state for the pool"
    ~allowed_roles:_R_READ_ONLY
    ~result:(Map(String,String), "The pool's license state")
    ()

let pool_apply_edition = call
    ~name:"apply_edition"
    ~in_oss_since:None
    ~in_product_since:rel_clearwater
    ~params:[
      Ref _pool, "self", "Reference to the pool";
      String, "edition", "The requested edition";
    ]
    ~doc:"Apply an edition to all hosts in the pool"
    ~allowed_roles:_R_POOL_OP
    ()

let pool_enable_ssl_legacy = call
    ~name:"enable_ssl_legacy"
    ~in_oss_since:None
    ~lifecycle:[
      Published, rel_dundee, "";
    ]
    ~params:[Ref _pool, "self", "(ignored)";]
    ~doc:"Sets ssl_legacy true on each host, pool-master last. See Host.ssl_legacy and Host.set_ssl_legacy."
    ~allowed_roles:_R_POOL_OP
    ()

let pool_disable_ssl_legacy = call
    ~name:"disable_ssl_legacy"
    ~in_oss_since:None
    ~lifecycle:[
      Published, rel_dundee, "";
    ]
    ~params:[Ref _pool, "self", "(ignored)";]
    ~doc:"Sets ssl_legacy true on each host, pool-master last. See Host.ssl_legacy and Host.set_ssl_legacy."
    ~allowed_roles:_R_POOL_OP
    ()

let pool_set_igmp_snooping_enabled = call  
    ~in_oss_since:None  
    ~lifecycle:[  
      Published, rel_inverness, "Enable or disable IGMP Snooping on the pool.";  
    ]  
    ~name:"set_igmp_snooping_enabled"  
    ~params:[  
      Ref _pool, "self", "The pool";  
      Bool, "value", "Enable or disable IGMP Snooping on the pool"  
    ]  
    ~doc:"Enable or disable IGMP Snooping on the pool."  
    ~allowed_roles:_R_POOL_OP  
    ()

let pool_has_extension = call
    ~name:"has_extension"
    ~in_product_since:rel_dundee
    ~doc:"Return true if the extension is available on the pool"
    ~params:[
      Ref _pool, "self", "The pool";
      String, "name", "The name of the API call"
    ]
    ~result:(Bool, "True if the extension exists, false otherwise")
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_add_to_guest_agent_config = call
    ~name:"add_to_guest_agent_config"
    ~in_product_since:rel_dundee
    ~doc:"Add a key-value pair to the pool-wide guest agent configuration"
    ~params:[
      Ref _pool, "self", "The pool";
      String, "key", "The key to add";
      String, "value", "The value to add";
    ]
    ~allowed_roles:_R_POOL_ADMIN
    ()

let pool_remove_from_guest_agent_config = call
    ~name:"remove_from_guest_agent_config"
    ~in_product_since:rel_dundee
    ~doc:"Remove a key-value pair from the pool-wide guest agent configuration"
    ~params:[
      Ref _pool, "self", "The pool";
      String, "key", "The key to remove";
    ]
    ~allowed_roles:_R_POOL_ADMIN
    ()

(** A pool class *)
let pool =
  create_obj
    ~in_db:true
    ~in_product_since:rel_rio
    ~in_oss_since:None
    ~internal_deprecated_since:None
    ~persist:PersistEverything
    ~gen_constructor_destructor:false
    ~name:_pool
    ~descr:"Pool-wide information"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:
      [ pool_join
      ; pool_join_force
      ; pool_eject
      ; pool_initial_auth
      ; pool_transition_to_master
      ; pool_slave_reset_master
      ; pool_recover_slaves
      ; pool_hello
      ; pool_ping_slave
      ; pool_create_VLAN
      ; pool_management_reconfigure
      ; pool_create_VLAN_from_PIF
      ; pool_slave_network_report
      ; pool_enable_ha
      ; pool_disable_ha
      ; pool_sync_database
      ; pool_designate_new_master
      ; pool_ha_prevent_restarts_for
      ; pool_ha_failover_plan_exists
      ; pool_ha_compute_max_host_failures_to_tolerate
      ; pool_ha_compute_hypothetical_max_host_failures_to_tolerate
      ; pool_ha_compute_vm_failover_plan
      ; pool_set_ha_host_failures_to_tolerate
      ; pool_create_new_blob
      ; pool_ha_schedule_plan_recomputation
      ; pool_enable_binary_storage
      ; pool_disable_binary_storage
      ; pool_enable_external_auth
      ; pool_disable_external_auth
      ; pool_detect_nonhomogeneous_external_auth
      ; pool_initialize_wlb
      ; pool_deconfigure_wlb
      ; pool_send_wlb_configuration
      ; pool_retrieve_wlb_configuration
      ; pool_retrieve_wlb_recommendations
      ; pool_send_test_post
      ; pool_certificate_install
      ; pool_certificate_uninstall
      ; pool_certificate_list
      ; pool_crl_install
      ; pool_crl_uninstall
      ; pool_crl_list
      ; pool_certificate_sync
      ; pool_enable_redo_log
      ; pool_disable_redo_log
      ; pool_audit_log_append
      ; pool_set_vswitch_controller
      ; pool_test_archive_target
      ; pool_enable_local_storage_caching
      ; pool_disable_local_storage_caching
      ; pool_get_license_state
      ; pool_apply_edition
      ; pool_enable_ssl_legacy
      ; pool_disable_ssl_legacy
      ; pool_set_igmp_snooping_enabled
      ; pool_has_extension
      ; pool_add_to_guest_agent_config
      ; pool_remove_from_guest_agent_config
      ]
    ~contents:
      ([uid ~in_oss_since:None _pool] @
       [ field ~in_oss_since:None ~qualifier:RW ~ty:String "name_label" "Short name"
       ; field ~in_oss_since:None ~qualifier:RW ~ty:String "name_description" "Description"
       ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Ref _host) "master" "The host that is pool master"
       ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "default_SR" "Default SR for VDIs"
       ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "suspend_image_SR" "The SR in which VDIs for suspend images are created"
       ; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "crash_dump_SR" "The SR in which VDIs for crash dumps are created"
       ; field ~in_oss_since:None ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP));("EMPTY_FOLDERS",(_R_VM_OP))]
       ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "ha_enabled" "true if HA is enabled on the pool, false otherwise"
       ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "ha_configuration" "The current HA configuration"
       ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_statefiles" "HA statefile VDIs in use"
       ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "ha_host_failures_to_tolerate" "Number of host failures to tolerate before the Pool is declared to be overcommitted"
       ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "ha_plan_exists_for" "Number of future host failures we have managed to find a plan for. Once this reaches zero any future host failures will cause the failure of protected VMs."
       ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "ha_allow_overcommit" "If set to false then operations which would cause the Pool to become overcommitted will be blocked."
       ; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "ha_overcommitted" "True if the Pool is considered to be overcommitted i.e. if there exist insufficient physical resources to tolerate the configured number of host failures"
       ; field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this pool"
       ; field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes"
       ; field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "gui_config" "gui-specific configuration for pool"
       ; field ~writer_roles:_R_POOL_OP ~in_product_since:rel_dundee ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "health_check_config" "Configuration for the automatic health check feature"
       ; field ~in_product_since:rel_george ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "wlb_url" "Url for the configured workload balancing host"
       ; field ~in_product_since:rel_george ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "wlb_username" "Username for accessing the workload balancing host"
       ; field ~in_product_since:rel_george ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _secret) "wlb_password" "Password for accessing the workload balancing host"
       ; field ~in_product_since:rel_george ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "wlb_enabled" "true if workload balancing is enabled on the pool, false otherwise"
       ; field ~in_product_since:rel_george ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "wlb_verify_cert" "true if communication with the WLB server should enforce SSL certificate verification."
       ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "redo_log_enabled" "true a redo-log is to be used other than when HA is enabled, false otherwise"
       ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:(Ref _vdi) ~default_value:(Some (VRef null_ref)) "redo_log_vdi" "indicates the VDI to use for the redo-log other than when HA is enabled"
       ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "vswitch_controller" "address of the vswitch controller"
           ~lifecycle:[
             Published, rel_midnight_ride, "the IP address of the vswitch controller.";
             Deprecated, rel_falcon, "Deprecated: set the IP address of the vswitch controller in SDN_controller instead."]
       ; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "restrictions" "Pool-wide restrictions currently in effect"
       ; field ~in_oss_since:None ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:(Set (Ref _vdi)) "metadata_VDIs" "The set of currently known metadata VDIs for this pool"
       ; field ~in_oss_since:None ~in_product_since:rel_dundee ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String "ha_cluster_stack" "The HA cluster stack that is currently in use. Only valid when HA is enabled."
       ] @ (allowed_and_current_operations pool_operations) @
       [ field ~in_oss_since:None ~in_product_since:rel_dundee ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "guest_agent_config" "Pool-wide guest agent configuration information"
       ; field ~qualifier:DynamicRO ~in_product_since:rel_dundee ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "cpu_info" "Details about the physical CPUs on the pool"
       ; field ~qualifier:RW ~in_product_since:rel_dundee ~default_value:(Some (VBool false)) ~ty:Bool "policy_no_vendor_device" "The pool-wide policy for clients on whether to use the vendor device or not on newly created VMs. This field will also be consulted if the 'has_vendor_device' field is not specified in the VM.create call."
       ; field ~qualifier:RW ~in_product_since:rel_ely ~default_value:(Some (VBool false)) ~ty:Bool "live_patching_disabled" "The pool-wide flag to show if the live patching feauture is disabled or not."
       ; field ~in_product_since:rel_inverness ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "igmp_snooping_enabled" "true if IGMP snooping is enabled in the pool, false otherwise."
       ])
    ()

(** Auth class *)
let auth_get_subject_identifier = call ~flags:[`Session]
    ~name:"get_subject_identifier"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~params:[
      (*Ref _auth, "auth", "???";*)
      String, "subject_name", "The human-readable subject_name, such as a username or a groupname" ;
    ]
    ~result:(String, "the subject_identifier obtained from the external directory service")
    ~doc:"This call queries the external directory service to obtain the subject_identifier as a string from the human-readable subject_name"
    ~allowed_roles:_R_READ_ONLY
    ()

let auth_get_subject_information_from_identifier = call ~flags:[`Session]
    ~name:"get_subject_information_from_identifier"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~params:[
      String, "subject_identifier", "A string containing the subject_identifier, unique in the external directory service"
    ]
    ~result:(Map(String,String), "key-value pairs containing at least a key called subject_name")
    ~doc:"This call queries the external directory service to obtain the user information (e.g. username, organization etc) from the specified subject_identifier"
    ~allowed_roles:_R_READ_ONLY
    ()

let auth_get_group_membership = call ~flags:[`Session]
    ~name:"get_group_membership"
    ~in_oss_since:None
    ~in_product_since:rel_george
    ~params:[
      String, "subject_identifier", "A string containing the subject_identifier, unique in the external directory service"
    ]
    ~result:(Set(String), "set of subject_identifiers that provides the group membership of subject_identifier passed as argument, it contains, recursively, all groups a subject_identifier is member of.")
    ~doc:"This calls queries the external directory service to obtain the transitively-closed set of groups that the the subject_identifier is member of."
    ~allowed_roles:_R_READ_ONLY
    ()

let auth =
  create_obj ~in_db:false ~in_product_since:rel_george ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_auth ~descr:"Management of remote authentication services"
    ~gen_events:false
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_READ_ONLY
    ~messages: [auth_get_subject_identifier;
                auth_get_subject_information_from_identifier;
                auth_get_group_membership;]
    ~contents:[]
    ()

(** Subject class *)
let subject_add_to_roles = call ~flags:[`Session]
    ~name:"add_to_roles"
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~params:[
      Ref _subject, "self", "The subject who we want to add the role to";
      Ref _role, "role", "The unique role reference" ;
    ]
    ~doc:"This call adds a new role to a subject"
    ~allowed_roles:_R_POOL_ADMIN
    ()
let subject_remove_from_roles = call ~flags:[`Session]
    ~name:"remove_from_roles"
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~params:[
      Ref _subject, "self", "The subject from whom we want to remove the role";
      Ref _role, "role", "The unique role reference in the subject's roles field" ;
    ]
    ~doc:"This call removes a role from a subject"
    ~allowed_roles:_R_POOL_ADMIN
    ()
let subject_get_permissions_name_label = call ~flags:[`Session]
    ~name:"get_permissions_name_label"
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~params:[
      Ref _subject, "self", "The subject whose permissions will be retrieved";
    ]
    ~result:(Set(String), "a list of permission names")
    ~doc:"This call returns a list of permission names given a subject"
    ~allowed_roles:_R_READ_ONLY
    ()
(* a subject is a user/group that can log in xapi *)
let subject =
  create_obj ~in_db:true ~in_product_since:rel_george ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_subject ~descr:"A user or group that can log in xapi"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages: [
      subject_add_to_roles;
      subject_remove_from_roles;
      subject_get_permissions_name_label;
    ]
    ~contents:[uid ~in_oss_since:None _subject;
               field ~in_product_since:rel_george ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "subject_identifier" "the subject identifier, unique in the external directory service";
               field ~in_product_since:rel_george ~default_value:(Some (VMap [])) ~qualifier:StaticRO ~ty:(Map(String, String)) "other_config" "additional configuration";
               (* DynamicRO fields do not show up in the constructor, as it should be because a subject must be created without receiving any roles as a parameter *)
               field ~in_product_since:rel_midnight_ride ~default_value:(Some (VSet [
                   (VRef ("OpaqueRef:"^Constants.rbac_pool_admin_uuid))])) (* pool-admin, according to rbac_static.ml, used during upgrade from pre-rbac xapis *)
                 ~ignore_foreign_key:true ~qualifier:DynamicRO ~ty:(Set((Ref _role))) "roles" "the roles associated with this subject";
              ]
    ()

(** Role class *)
let role_get_permissions = call ~flags:[`Session]
    ~name:"get_permissions"
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~params:[
      Ref _role, "self", "a reference to a role";
    ]
    ~result:(Set(Ref _role), "a list of permissions")
    ~doc:"This call returns a list of permissions given a role"
    ~allowed_roles:_R_READ_ONLY
    ()
let role_get_permissions_name_label = call ~flags:[`Session]
    ~name:"get_permissions_name_label"
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~params:[
      Ref _role, "self", "a reference to a role";
    ]
    ~result:(Set(String), "a list of permission names")
    ~doc:"This call returns a list of permission names given a role"
    ~allowed_roles:_R_READ_ONLY
    ()
let role_get_by_permission = call ~flags:[`Session]
    ~name:"get_by_permission"
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~params:[
      Ref _role, "permission", "a reference to a permission" ;
    ]
    ~result:(Set(Ref _role), "a list of references to roles")
    ~doc:"This call returns a list of roles given a permission"
    ~allowed_roles:_R_READ_ONLY
    ()
let role_get_by_permission_name_label = call ~flags:[`Session]
    ~name:"get_by_permission_name_label"
    ~in_oss_since:None
    ~in_product_since:rel_midnight_ride
    ~params:[
      String, "label", "The short friendly name of the role" ;
    ]
    ~result:(Set(Ref _role), "a list of references to roles")
    ~doc:"This call returns a list of roles given a permission name"
    ~allowed_roles:_R_READ_ONLY
    ()

(* A role defines a set of API call privileges associated with a subject *)
(* A role is synonymous to permission or privilege *)
(* A role is a recursive definition: it is either a basic role or it points to a set of roles *)
(* - full/complete role: is the one meant to be used by the end-user, a root in the tree of roles *)
(* - basic role: is the 1x1 mapping to each XAPI/HTTP call being protected, a leaf in the tree of roles *)
(* - intermediate role: an intermediate node in the recursive tree of roles, usually not meant to the end-user *)
let role =
  create_obj ~in_db:true ~in_product_since:rel_midnight_ride ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_role ~descr:"A set of permissions associated with a subject"
    ~gen_events:true
    ~force_custom_actions:(Some(StaticRO)) (* force custom actions for getters *)
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages: [
      role_get_permissions;
      role_get_permissions_name_label;
      role_get_by_permission;
      role_get_by_permission_name_label;
    ]
    ~contents: [uid ~in_oss_since:None _role;
                namespace ~name:"name" ~contents:(
                  [
                    field ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "label" "a short user-friendly name for the role";
                    field ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "description" "what this role is for";
                  ]) ();
                field ~in_product_since:rel_midnight_ride ~default_value:(Some (VSet [])) ~ignore_foreign_key:true ~qualifier:StaticRO ~ty:(Set(Ref _role)) "subroles" "a list of pointers to other roles or permissions";
                (*RBAC2: field ~in_product_since:rel_midnight_ride ~default_value:(Some (VBool false)) ~qualifier:StaticRO ~ty:Bool "is_complete" "if this is a complete role, meant to be used by the end-user";*)
               ]
    ()

(** A virtual disk interface *)
let vtpm =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vtpm ~descr:"A virtual TPM device"
    ~gen_events:false
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~messages:[]
    ~contents:
      [ uid _vtpm;
        field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine";
        field ~qualifier:StaticRO ~ty:(Ref _vm) "backend" "the domain where the backend is located" ]
    ()

(** Console protocols *)
let console_protocol = Enum("console_protocol", [
    "vt100", "VT100 terminal";
    "rfb", "Remote FrameBuffer protocol (as used in VNC)";
    "rdp", "Remote Desktop Protocol"
  ])

(** A virtual console device *)
let console =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_console ~descr:"A console"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~messages:[]  ~contents:
    [ uid _console;
      field ~qualifier:DynamicRO ~ty:console_protocol "protocol" "the protocol used by this console";
      field ~qualifier:DynamicRO ~ty:String "location" "URI for the console service";
      field ~qualifier:DynamicRO ~ty:(Ref _vm) "VM" "VM to which this console is attached";
      field  ~ty:(Map(String, String)) "other_config" "additional configuration";
      field ~in_oss_since:None ~internal_only:true ~ty:Int "port" "port in dom0 on which the console server is listening";
    ]
    ()

(* PV domain booting *)
let pv =
  [
    field "bootloader" "name of or path to bootloader";
    field "kernel" "path to the kernel";
    field "ramdisk" "path to the initrd";
    field "args" "kernel command-line arguments";
    field "bootloader_args" "miscellaneous arguments for the bootloader";
    field ~in_oss_since:None "legacy_args" "to make Zurich guests boot";
  ]

(** HVM domain booting *)
let hvm =
  [
    field "boot_policy" "HVM boot policy";
    field ~ty:(Map(String, String)) "boot_params" "HVM boot params";
    field ~writer_roles:_R_VM_POWER_ADMIN ~in_oss_since:None ~ty:Float ~in_product_since:rel_miami ~qualifier:StaticRO "shadow_multiplier" "multiplier applied to the amount of shadow that will be made available to the guest" ~default_value:(Some (VFloat 1.))
  ]

(** Action to take on guest reboot/power off/sleep etc *)
(*
let power_behaviour =
  Enum ("power_behaviour", [ "destroy", "destroy the VM state";
			     "restart", "automatically restart the VM";
			     "preserve", "leave VM running";
			     "rename_restart", "leave VM running and restart a new one" ])
*)
let on_crash_behaviour =
  Enum ("on_crash_behaviour", [ "destroy", "destroy the VM state";
                                "coredump_and_destroy", "record a coredump and then destroy the VM state";
                                "restart", "restart the VM";
                                "coredump_and_restart", "record a coredump and then restart the VM";
                                "preserve", "leave the crashed VM paused";
                                "rename_restart", "rename the crashed VM and start a new copy" ])

let on_normal_exit_behaviour =
  Enum ("on_normal_exit", [ "destroy", "destroy the VM state";
                            "restart", "restart the VM" ])


(** Virtual CPUs *)
let vcpus =
  [
    field ~ty:(Map(String, String)) "params" "configuration parameters for the selected VCPU policy";
    field ~qualifier:StaticRO ~ty:Int "max" "Max number of VCPUs";
    field ~qualifier:StaticRO ~ty:Int "at_startup" "Boot number of VCPUs";
  ]

(** Default actions *)
let actions =
  let crash = field ~effect:true ~ty:on_crash_behaviour in
  let normal = field ~effect:true ~ty:on_normal_exit_behaviour in
  [
    normal "after_shutdown" "action to take after the guest has shutdown itself";
    normal "after_reboot" "action to take after the guest has rebooted itself";
    crash "after_crash" "action to take if the guest crashes";
  ]

let vm_power_state =
  Enum ("vm_power_state", [ "Halted", "VM is offline and not using any resources";
                            "Paused", "All resources have been allocated but the VM itself is paused and its vCPUs are not running";
                            "Running", "Running";
                            "Suspended", "VM state has been saved to disk and it is nolonger running. Note that disks remain in-use while the VM is suspended."])

let vm_operations =
  Enum ("vm_operations",
        List.map operation_enum
          [ vm_snapshot; vm_clone; vm_copy; vm_create_template; vm_revert; vm_checkpoint; vm_snapshot_with_quiesce;
            vm_provision; vm_start; vm_start_on; vm_pause; vm_unpause; vm_cleanShutdown;
            vm_cleanReboot; vm_hardShutdown; vm_stateReset; vm_hardReboot;
            vm_suspend; csvm; vm_resume; vm_resume_on;
            vm_pool_migrate;
            vm_migrate_send;
            vm_get_boot_record; vm_send_sysrq; vm_send_trigger;
            vm_query_services;vm_shutdown;
            vm_call_plugin;
          ]
        @ [ "changing_memory_live", "Changing the memory settings";
            "awaiting_memory_live", "Waiting for the memory settings to change";
            "changing_dynamic_range", "Changing the memory dynamic range";
            "changing_static_range", "Changing the memory static range";
            "changing_memory_limits", "Changing the memory limits";
            "changing_shadow_memory", "Changing the shadow memory for a halted VM.";
            "changing_shadow_memory_live", "Changing the shadow memory for a running VM.";
            "changing_VCPUs", "Changing VCPU settings for a halted VM.";
            "changing_VCPUs_live", "Changing VCPU settings for a running VM.";
            "assert_operation_valid", "";
            "data_source_op", "Add, remove, query or list data sources";
            "update_allowed_operations", "";
            "make_into_template", "Turning this VM into a template";
            "import", "importing a VM from a network stream";
            "export", "exporting a VM to a network stream";
            "metadata_export", "exporting VM metadata to a network stream";
            "reverting", "Reverting the VM to a previous snapshotted state";
            "destroy", "refers to the act of uninstalling the VM";
          ]
       )

(** VM (or 'guest') configuration: *)
let vm =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vm ~descr:"A virtual machine (or 'guest')."
    ~gen_events:true
    ~doccomments:[ "destroy", "Destroy the specified VM.  The VM is completely removed from the system.  This function can only be called when the VM is in the Halted State.";
                   "create", "NOT RECOMMENDED! VM.clone or VM.copy (or VM.import) is a better choice in almost all situations. The standard way to obtain a new VM is to call VM.clone on a template VM, then call VM.provision on the new clone. Caution: if VM.create is used and then the new VM is attached to a virtual disc that has an operating system already installed, then there is no guarantee that the operating system will boot and run. Any software that calls VM.create on a future version of this API may fail or give unexpected results. For example this could happen if an additional parameter were added to VM.create. VM.create is intended only for use in the automatic creation of the system VM templates. It creates a new VM instance, and returns its handle.";
                 ]
    ~lifecycle:[
      Published, rel_rio, "";
    ]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~messages:[ vm_snapshot; vm_snapshot_with_quiesce; vm_clone; vm_copy; vm_revert; vm_checkpoint;
                vm_provision; vm_start; vm_start_on; vm_pause; vm_unpause; vm_cleanShutdown;vm_shutdown;
                vm_cleanReboot; vm_hardShutdown; vm_stateReset; vm_hardReboot; vm_suspend; csvm; vm_resume;
                vm_set_is_default_template;
                vm_hardReboot_internal;
                vm_resume_on;
                vm_pool_migrate; vm_pool_migrate_complete;
                set_vcpus_number_live;
                vm_add_to_VCPUs_params_live;
                vm_set_ha_restart_priority;  (* updates the allowed-operations of the VM *)
                vm_set_ha_always_run;        (* updates the allowed-operations of the VM *)
                vm_compute_memory_overhead;
                vm_set_memory_dynamic_max;
                vm_set_memory_dynamic_min;
                vm_set_memory_dynamic_range;
                vm_set_memory_static_max;
                vm_set_memory_static_min;
                vm_set_memory_static_range;
                vm_set_memory_limits;
                vm_set_memory;
                vm_set_memory_target_live;
                vm_wait_memory_target_live;
                vm_get_cooperative;
                vm_set_HVM_shadow_multiplier;
                vm_set_shadow_multiplier_live;
                vm_set_VCPUs_max;
                vm_set_VCPUs_at_startup;
                vm_send_sysrq; vm_send_trigger;
                vm_maximise_memory;
                vm_migrate_send;
                vm_assert_can_migrate;
                vm_assert_can_migrate_sender;
                vm_get_boot_record;
                vm_get_data_sources; vm_record_data_source; vm_query_data_source; vm_forget_data_source_archives;
                assert_operation_valid vm_operations _vm _self;
                update_allowed_operations vm_operations _vm _self;
                vm_get_allowed_VBD_devices;
                vm_get_allowed_VIF_devices;
                vm_get_possible_hosts;
                vm_assert_can_boot_here;
                vm_atomic_set_resident_on;
                vm_create_new_blob;
                vm_s3_suspend;
                vm_s3_resume;
                vm_assert_agile;
                vm_update_snapshot_metadata;
                vm_retrieve_wlb_recommendations;
                vm_set_bios_strings;
                vm_copy_bios_strings;
                vm_set_protection_policy;
                vm_set_snapshot_schedule;
                vm_set_start_delay;
                vm_set_shutdown_delay;
                vm_set_order;
                vm_set_suspend_VDI;
                vm_assert_can_be_recovered;
                vm_get_SRs_required_for_recovery;
                vm_recover;
                vm_import_convert;
                vm_set_appliance;
                vm_query_services;
                vm_call_plugin;
                vm_set_has_vendor_device;
                vm_import;
              ]
    ~contents:
      ([ uid _vm;
       ] @ (allowed_and_current_operations vm_operations) @ [
         field ~writer_roles:_R_VM_OP ~qualifier:DynamicRO ~ty:vm_power_state "power_state" "Current power state of the machine";
         namespace ~name:"name" ~contents:(names oss_since_303 RW) ();

         field ~ty:Int "user_version" "Creators of VMs and templates may store version information here.";
         field ~effect:true ~ty:Bool "is_a_template" "true if this is a template. Template VMs can never be started, they are used only for cloning other VMs";
         field ~ty:Bool ~default_value:(Some (VBool false)) ~qualifier:DynamicRO ~writer_roles:_R_POOL_ADMIN ~lifecycle:[Published, rel_falcon, "Identifies XenServer default templates"] "is_default_template" "true if this is a default template. Default template VMs can never be started or migrated, they are used only for cloning other VMs";
         field ~qualifier:DynamicRO ~ty:(Ref _vdi) "suspend_VDI" "The VDI that a suspend image is stored on. (Only has meaning if VM is currently suspended)";

         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~ty:(Ref _host) "resident_on" "the host the VM is currently resident on";
         field ~writer_roles:_R_VM_POWER_ADMIN ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _host) "scheduled_to_be_resident_on" "the host on which the VM is due to be started/resumed/migrated. This acts as a memory reservation indicator";
         field ~writer_roles:_R_VM_POWER_ADMIN ~in_oss_since:None ~ty:(Ref _host) "affinity" "A host which the VM has some affinity for (or NULL). This is used as a hint to the start call when it decides where to run the VM. Resource constraints may cause the VM to be started elsewhere.";

         namespace ~name:"memory" ~contents:guest_memory ();
         namespace ~name:"VCPUs" ~contents:vcpus ();
         namespace ~name:"actions" ~contents:actions ();

         field ~writer_roles:_R_POOL_ADMIN ~qualifier:DynamicRO ~ty:(Set (Ref _console)) "consoles" "virtual console devices";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _vif)) ~doc_tags:[Networking] "VIFs" "virtual network interfaces";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _vbd)) "VBDs" "virtual block devices";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _vusb)) "VUSBs" "vitual usb devices";
         field ~writer_roles:_R_POOL_ADMIN ~qualifier:DynamicRO ~ty:(Set (Ref _crashdump)) "crash_dumps" "crash dumps associated with this VM";
         field ~qualifier:DynamicRO ~ty:(Set (Ref _vtpm)) "VTPMs" "virtual TPMs";

         namespace ~name:"PV" ~contents:pv ();
         namespace ~name:"HVM" ~contents:hvm ();
         field ~ty:(Map(String, String)) "platform" "platform-specific configuration";

         field ~lifecycle:[
           Published, rel_rio, "PCI bus path for pass-through devices";
           Deprecated, rel_boston, "Field was never used"]
           "PCI_bus" "PCI bus path for pass-through devices";
         field  ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:["pci", _R_POOL_ADMIN; ("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
         field ~qualifier:DynamicRO ~ty:Int "domid" "domain ID (if available, -1 otherwise)";
         field ~qualifier:DynamicRO ~in_oss_since:None ~ty:String "domarch" "Domain architecture (if available, null string otherwise)";
         field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map(String, String)) "last_boot_CPU_flags" "describes the CPU flags on which the VM was last booted";
         field ~qualifier:DynamicRO ~ty:Bool "is_control_domain" "true if this is a control domain (domain 0 or a driver domain)";
         field ~qualifier:DynamicRO ~ty:(Ref _vm_metrics) "metrics" "metrics associated with this VM";
         field ~qualifier:DynamicRO ~ty:(Ref _vm_guest_metrics) "guest_metrics" "metrics associated with the running guest";
         (* This was an internal field in Rio, Miami beta1, Miami beta2 but is now exposed so that
            	   it will be included automatically in Miami GA exports and can be restored, important if
            	   the VM is in a suspended state *)
         field ~in_oss_since:None ~internal_only:false ~in_product_since:rel_miami ~qualifier:DynamicRO ~ty:String "last_booted_record" "marshalled value containing VM record at time of last boot, updated dynamically to reflect the runtime state of the domain" ~default_value:(Some (VString ""));
         field ~in_oss_since:None ~ty:String "recommendations" "An XML specification of recommended values and ranges for properties of this VM";
         field ~effect:true ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "xenstore_data" "data to be inserted into the xenstore tree (/local/domain/<domid>/vm-data) after the VM is created." ~default_value:(Some (VMap []));
         field ~writer_roles:_R_POOL_OP ~in_oss_since:None ~ty:Bool ~in_product_since:rel_orlando ~internal_deprecated_since:rel_boston ~qualifier:StaticRO "ha_always_run" "if true then the system will attempt to keep the VM running as much as possible." ~default_value:(Some (VBool false));
         field ~writer_roles:_R_POOL_OP ~in_oss_since:None ~ty:String ~in_product_since:rel_orlando ~qualifier:StaticRO "ha_restart_priority" "has possible values: \"best-effort\" meaning \"try to restart this VM if possible but don't consider the Pool to be overcommitted if this is not possible\"; \"restart\" meaning \"this VM should be restarted\"; \"\" meaning \"do not try to restart this VM\"" ~default_value:(Some (VString ""));
         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VBool false))          ~ty:Bool            "is_a_snapshot" "true if this is a snapshot. Snapshotted VMs can never be started, they are used only for cloning other VMs";
         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VRef ""))              ~ty:(Ref _vm)       "snapshot_of" "Ref pointing to the VM this snapshot is of.";
         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando                                              ~ty:(Set (Ref _vm)) "snapshots" "List pointing to all the VM snapshots.";
         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VDateTime Date.never)) ~ty:DateTime        "snapshot_time" "Date/time when this snapshot was created.";
         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VString ""))           ~ty:String          "transportable_snapshot_id" "Transportable ID of the snapshot VM";
         field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this VM";
         field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
         field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~qualifier:RW ~ty:(Map(vm_operations, String)) "blocked_operations" "List of operations which have been explicitly blocked and an error code";

         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap []))    ~ty:(Map (String, String)) "snapshot_info"     "Human-readable information concerning this snapshot";
         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~ty:String                 "snapshot_metadata" "Encoded information about the VM's metadata this is a snapshot of";

         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VRef "")) ~ty:(Ref _vm)       "parent"       "Ref pointing to the parent of this VM";
         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride                                 ~ty:(Set (Ref _vm)) "children"     "List pointing to all the children of this VM";

         field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map (String,String)) "bios_strings" "BIOS strings";
         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:StaticRO ~lifecycle:[Published, rel_cowley, ""; Deprecated, rel_clearwater, "The VMPR feature was removed"] ~default_value:(Some (VRef null_ref)) ~ty:(Ref _vmpp) "protection_policy" "Ref pointing to a protection policy for this VM";
         field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~lifecycle:[Published, rel_cowley, ""; Deprecated, rel_clearwater, "The VMPR feature was removed"] ~default_value:(Some (VBool false)) ~ty:Bool "is_snapshot_from_vmpp" "true if this snapshot was created by the protection policy";

         field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:StaticRO ~in_product_since:rel_falcon ~default_value:(Some (VRef (null_ref))) ~ty:(Ref _vmss) "snapshot_schedule" "Ref pointing to a snapshot schedule for this VM";
         field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~in_product_since:rel_falcon ~default_value:(Some (VBool false)) ~ty:Bool "is_vmss_snapshot" "true if this snapshot was created by the snapshot schedule";

         field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~ty:(Ref _vm_appliance) ~default_value:(Some (VRef null_ref)) "appliance" "the appliance to which this VM belongs";
         field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~in_product_since:rel_boston ~default_value:(Some (VInt 0L)) ~ty:Int "start_delay" "The delay to wait before proceeding to the next order in the startup sequence (seconds)";
         field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~in_product_since:rel_boston ~default_value:(Some (VInt 0L)) ~ty:Int "shutdown_delay" "The delay to wait before proceeding to the next order in the shutdown sequence (seconds)";
         field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~in_product_since:rel_boston ~default_value:(Some (VInt 0L)) ~ty:Int "order" "The point in the startup or shutdown sequence at which this VM will be started";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _vgpu)) "VGPUs" "Virtual GPUs";
         field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _pci)) "attached_PCIs" "Currently passed-through PCI devices";
         field ~writer_roles:_R_VM_ADMIN ~qualifier:RW ~in_product_since:rel_boston ~default_value:(Some (VRef null_ref)) ~ty:(Ref _sr) "suspend_SR" "The SR on which a suspend image is stored";
         field ~qualifier:StaticRO ~in_product_since:rel_boston ~default_value:(Some (VInt 0L)) ~ty:Int "version" "The number of times this VM has been recovered";
         field ~qualifier:StaticRO ~in_product_since:rel_clearwater ~default_value:(Some (VString "0:0")) ~ty:(String) "generation_id" "Generation ID of the VM";
         field ~writer_roles:_R_VM_ADMIN ~qualifier:RW ~in_product_since:rel_cream ~default_value:(Some (VInt 0L)) ~ty:Int "hardware_platform_version" "The host virtual hardware platform version the VM can run on";
         field ~qualifier:StaticRO ~lifecycle:[Published, rel_dundee, ""] ~doc_tags:[Windows] ~default_value:(Some (VCustom (String.concat "\n" [
             "(try Rpc.Bool (";
             "let pool = List.hd (Db_actions.DB_Action.Pool.get_all ~__context) in";
             "let restrictions = Db_actions.DB_Action.Pool.get_restrictions ~__context ~self:pool in ";
             "let vendor_device_allowed = try List.assoc \"restrict_pci_device_for_auto_update\" restrictions = \"false\" with _ -> false in";
             "let policy_says_its_ok = not (Db_actions.DB_Action.Pool.get_policy_no_vendor_device ~__context ~self:pool) in";
             "vendor_device_allowed && policy_says_its_ok) with e -> D.error \"Failure when defaulting has_vendor_device field: %s\" (Printexc.to_string e); Rpc.Bool false)"], VBool false)))
           ~ty:Bool "has_vendor_device" "When an HVM guest starts, this controls the presence of the emulated C000 PCI device which triggers Windows Update to fetch or update PV drivers.";
         field ~qualifier:DynamicRO ~ty:Bool ~lifecycle:[Published, rel_ely, ""] ~default_value:(Some (VBool false))
           "requires_reboot" "Indicates whether a VM requires a reboot in order to update its configuration, e.g. its memory allocation.";
         field ~qualifier:StaticRO ~ty:String ~in_product_since:rel_ely ~default_value:(Some (VString "")) "reference_label" "Textual reference to the template used to create a VM. This can be used by clients in need of an immutable reference to the template since the latter's uuid and name_label may change, for example, after a package installation or upgrade."
       ])
    ()

let vm_memory_metrics =
  [
    field ~qualifier:DynamicRO ~ty:Int "actual" "Guest's actual memory (bytes)" ~persist:false
  ]

let vm_vcpu_metrics =
  [
    field ~qualifier:DynamicRO ~ty:Int "number" "Current number of VCPUs" ~persist:true;
    field ~qualifier:DynamicRO ~ty:(Map (Int, Float)) ~persist:false "utilisation" "Utilisation for all of guest's current VCPUs"
      ~lifecycle:[Removed, rel_tampa, "Disabled in favour of RRDs"];
    field ~qualifier:DynamicRO ~ty:(Map (Int, Int)) "CPU" "VCPU to PCPU map" ~persist:false;
    field ~qualifier:DynamicRO ~ty:(Map (String, String)) "params" "The live equivalent to VM.VCPUs_params" ~persist:false;
    field ~qualifier:DynamicRO ~ty:(Map (Int, Set String)) "flags" "CPU flags (blocked,online,running)" ~persist:false;
  ]

let vm_metrics =
  create_obj
    ~in_db:true
    ~in_product_since:rel_rio
    ~in_oss_since:oss_since_303
    ~internal_deprecated_since:None
    ~persist:PersistEverything
    ~gen_constructor_destructor:false
    ~name:_vm_metrics
    ~descr:"The metrics associated with a VM"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~messages:[]
    ~contents:
      [ uid _vm_metrics
      ; namespace ~name:"memory" ~contents:vm_memory_metrics ()
      ; namespace ~name:"VCPUs" ~contents:vm_vcpu_metrics ()
      ; field ~qualifier:DynamicRO ~ty:(Set (String))
          "state" "The state of the guest, eg blocked, dying etc"
          ~persist:false
      ; field ~qualifier:DynamicRO ~ty:DateTime
          "start_time" "Time at which this VM was last booted"
      ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:DateTime
          "install_time" "Time at which the VM was installed"
      ; field ~qualifier:DynamicRO ~ty:DateTime
          "last_updated" "Time at which this information was last updated"
          ~persist:false
      ; field ~in_product_since:rel_orlando ~default_value:(Some (VMap []))
          ~ty:(Map(String, String))
          "other_config" "additional configuration"
          ~persist:false
      ; field ~in_product_since:rel_ely ~default_value:(Some (VBool false))
          ~ty:Bool ~qualifier:DynamicRO
          "hvm" "hardware virtual machine"
          ~persist:false
      ; field ~in_product_since:rel_ely ~default_value:(Some (VBool false))
          ~ty:Bool ~qualifier:DynamicRO
          "nested_virt" "VM supports nested virtualisation"
          ~persist:false
      ; field ~in_product_since:rel_ely ~default_value:(Some (VBool false))
          ~ty:Bool ~qualifier:DynamicRO
          "nomigrate" "VM is immobile and can't migrate between hosts"
          ~persist:false
>>>>>>> ed38131... CP-27201 Sync network sriov from master to slave when slave restarting
      ]
      ()

  let t =
    create_obj ~in_db:true ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vmss ~descr:"VM Snapshot Schedule"
      ~gen_events:true
      ~in_product_since:rel_falcon
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[
        snapshot_now;
        set_retained_snapshots;
        set_frequency;
        set_schedule;
        add_to_schedule;
        remove_from_schedule;
        set_last_run_time;
        set_type;
      ]
      ~contents:[
        uid _vmss;
        namespace ~name:"name" ~contents:(names None RW) ();
        field ~qualifier:RW ~ty:Bool "enabled" "enable or disable this snapshot schedule" ~default_value:(Some (VBool true));
        field ~qualifier:StaticRO ~ty:type' "type" "type of the snapshot schedule";
        field ~qualifier:StaticRO ~ty:Int "retained_snapshots" "maximum number of snapshots that should be stored at any time" ~default_value:(Some (VInt 7L));
        field ~qualifier:StaticRO ~ty:frequency "frequency" "frequency of taking snapshot from snapshot schedule";
        field ~qualifier:StaticRO ~ty:(Map (String,String)) "schedule" "schedule of the snapshot containing 'hour', 'min', 'days'. Date/time-related information is in Local Timezone" ~default_value:(Some (VMap []));
        field ~qualifier:DynamicRO ~ty:DateTime "last_run_time" "time of the last snapshot" ~default_value:(Some(VDateTime(Date.of_float 0.)));
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) "VMs" "all VMs attached to this snapshot schedule";
      ]
      ()

end


module VM_appliance = struct
  (* VM appliance *)
  let operations = Enum ("vm_appliance_operation",
                         [
                           "start", "Start";
                           "clean_shutdown", "Clean shutdown";
                           "hard_shutdown", "Hard shutdown";
                           "shutdown", "Shutdown";
                         ])

  let start = call
      ~name:"start"
      ~in_product_since:rel_boston
      ~params:[
        Ref _vm_appliance, "self", "The VM appliance";
        Bool, "paused", "Instantiate all VMs belonging to this appliance in paused state if set to true."
      ]
      ~errs:[Api_errors.operation_partially_failed]
      ~doc:"Start all VMs in the appliance"
      ~allowed_roles:_R_POOL_OP
      ()
  let clean_shutdown = call
      ~name:"clean_shutdown"
      ~in_product_since:rel_boston
      ~params:[Ref _vm_appliance, "self", "The VM appliance"]
      ~errs:[Api_errors.operation_partially_failed]
      ~doc:"Perform a clean shutdown of all the VMs in the appliance"
      ~allowed_roles:_R_POOL_OP
      ()
  let hard_shutdown = call
      ~name:"hard_shutdown"
      ~in_product_since:rel_boston
      ~params:[Ref _vm_appliance, "self", "The VM appliance"]
      ~errs:[Api_errors.operation_partially_failed]
      ~doc:"Perform a hard shutdown of all the VMs in the appliance"
      ~allowed_roles:_R_POOL_OP
      ()
  let shutdown = call
      ~name:"shutdown"
      ~in_product_since:rel_boston
      ~params:[Ref _vm_appliance, "self", "The VM appliance"]
      ~errs:[Api_errors.operation_partially_failed]
      ~doc:"For each VM in the appliance, try to shut it down cleanly. If this fails, perform a hard shutdown of the VM."
      ~allowed_roles:_R_POOL_OP
      ()
  let assert_can_be_recovered = call
      ~name:"assert_can_be_recovered"
      ~in_product_since:rel_boston
      ~params:[Ref _vm_appliance, "self", "The VM appliance to recover";
               Ref _session, "session_to", "The session to which the VM appliance is to be recovered."]
      ~errs:[Api_errors.vm_requires_sr]
      ~doc:"Assert whether all SRs required to recover this VM appliance are available."
      ~allowed_roles:_R_READ_ONLY
      ()
  let get_SRs_required_for_recovery = call
      ~name:"get_SRs_required_for_recovery"
      ~in_product_since:rel_creedence
      ~params:[Ref _vm_appliance , "self" , "The VM appliance for which the required list of SRs has to be recovered.";
               Ref _session , "session_to", "The session to which the list of SRs have to be recovered ."]
      ~result:(Set(Ref _sr), "refs for SRs required to recover the VM")
      ~errs:[]
      ~doc:"Get the list of SRs required by the VM appliance to recover."
      ~allowed_roles:_R_READ_ONLY
      ()
  let recover = call
      ~name:"recover"
      ~in_product_since:rel_boston
      ~params:[Ref _vm_appliance, "self", "The VM appliance to recover";
               Ref _session, "session_to", "The session to which the VM appliance is to be recovered.";
               Bool, "force", "Whether the VMs should replace newer versions of themselves."]
      ~errs:[Api_errors.vm_requires_sr]
      ~doc:"Recover the VM appliance"
      ~allowed_roles:_R_READ_ONLY
      ()


  let t =
    create_obj ~in_db:true ~in_product_since:rel_boston ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vm_appliance ~descr:"VM appliance"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[
        start;
        clean_shutdown;
        hard_shutdown;
        shutdown;
        assert_can_be_recovered;
        get_SRs_required_for_recovery;
        recover;
      ]
      ~contents:([
          uid _vm_appliance;
          namespace ~name:"name" ~contents:(names None RW) ();
        ] @ (allowed_and_current_operations operations) @ [
            field ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) "VMs" "all VMs in this appliance";
          ])
      ()
end


module DR_task = struct
  (* DR_task *)
  let create = call
      ~name:"create"
      ~in_product_since:rel_boston
      ~params:[
        String, "type", "The SR driver type of the SRs to introduce";
        Map(String, String), "device_config", "The device configuration of the SRs to introduce";
        Set(String), "whitelist", "The devices to use for disaster recovery"
      ]
      ~result:(Ref _dr_task, "The reference to the created task")
      ~doc:"Create a disaster recovery task which will query the supplied list of devices"
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~in_product_since:rel_boston
      ~params:[
        Ref _dr_task, "self", "The disaster recovery task to destroy"
      ]
      ~doc:"Destroy the disaster recovery task, detaching and forgetting any SRs introduced which are no longer required"
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj
      ~in_db:true
      ~in_product_since:rel_boston
      ~in_oss_since:None
      ~internal_deprecated_since:None
      ~persist:PersistEverything
      ~gen_constructor_destructor:false
      ~name:_dr_task
      ~descr:"DR task"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[create; destroy]
      ~contents:[
        uid _dr_task;
        field ~qualifier:DynamicRO ~ty:(Set (Ref _sr)) "introduced_SRs" "All SRs introduced by this appliance";
      ]
      ()
end


(** events handling: *)

module Event = struct
  let operation = Enum ("event_operation",
                        [ "add", "An object has been created";
                          "del", "An object has been deleted";
                          "mod", "An object has been modified"])
  let register = call
      ~name:"register"
      ~in_product_since:rel_rio
      ~params:[Set String, "classes", "register for events for the indicated classes"]
      ~doc:"Registers this session with the event system.  Specifying * as the desired class will register for all classes."
      ~allowed_roles:_R_ALL
      ()
  let unregister = call
      ~name:"unregister"
      ~in_product_since:rel_rio
      ~params:[Set String, "classes", "remove this session's registration for the indicated classes"]
      ~doc:"Unregisters this session with the event system"
      ~allowed_roles:_R_ALL
      ()
  let next = call
      ~name:"next" ~params:[]
      ~in_product_since:rel_rio
      ~doc:"Blocking call which returns a (possibly empty) batch of events. This method is only recommended for legacy use. New development should use event.from which supercedes this method. "
      ~custom_marshaller:true
      ~flags:[`Session]
      ~result:(Set (Record _event), "the batch of events")
      ~errs:[Api_errors.session_not_registered;Api_errors.events_lost]
      ~allowed_roles:_R_ALL
      ()
  let from = call
      ~name:"from"
      ~params:[Set String, "classes", "register for events for the indicated classes";
               String, "token", "A token representing the point from which to generate database events. The empty string represents the beginning.";
               Float, "timeout", "Return after this many seconds if no events match";
              ]
      ~in_product_since:rel_boston
      ~doc:"Blocking call which returns a new token and a (possibly empty) batch of events. The returned token can be used in subsequent calls to this function."
      ~custom_marshaller:true
      ~flags:[`Session]
      ~result:(Set (Record _event), "the batch of events")
      ~errs:[Api_errors.session_not_registered;Api_errors.events_lost]
      ~allowed_roles:_R_ALL
      ()
  let get_current_id = call
      ~name:"get_current_id" ~params:[]
      ~in_product_since:rel_rio
      ~doc:"Return the ID of the next event to be generated by the system"
      ~flags:[`Session]
      ~result:(Int, "the event ID")
      ~allowed_roles:_R_ALL
      ()
  let inject = call
      ~name:"inject" ~params:[
      String, "class", "class of the object";
      String, "ref", "A reference to the object that will be changed.";
    ]
      ~in_product_since:rel_tampa
      ~doc:"Injects an artificial event on the given object and return the corresponding ID"
      ~flags:[`Session]
      ~result:(String, "the event ID")
      ~allowed_roles:_R_ALL
      ()
  (* !!! This should call create_obj ~in_db:true like everything else... !!! *)

  let t =
    {
      obj_lifecycle=[];
      name = _event;
      gen_events = false;
      description = "Asynchronous event registration and handling";
      gen_constructor_destructor = false;
      doccomments = [];
      msg_lifecycles = [];
      messages = [ register; unregister; next; from; get_current_id; inject ];
      obj_release = {internal=get_product_releases rel_rio; opensource=get_oss_releases (Some "3.0.3"); internal_deprecated_since=None};
      contents = [
        field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:Int "id" "An ID, monotonically increasing, and local to the current session";
        field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:DateTime "timestamp" "The time at which the event occurred";
        field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String "class" "The name of the class of the object that changed";
        field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:operation "operation" "The operation that was performed";
        field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String "ref" "A reference to the object that changed";
        field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String "obj_uuid" "The uuid of the object that changed";
      ];
      persist = PersistNothing;
      in_database=false;
      force_custom_actions=None;
      obj_allowed_roles=_R_POOL_ADMIN;
      obj_implicit_msg_allowed_roles=_R_ALL;
      obj_doc_tags=[];
    }
end

(** Blobs - binary blobs of data *)

module Blob = struct
  let create = call
      ~name:"create"
      ~in_product_since:rel_orlando
      ~versioned_params:
        [{param_type=String; param_name="mime_type"; param_doc="The mime-type of the blob. Defaults to 'application/octet-stream' if the empty string is supplied"; param_release=orlando_release; param_default=None};
         {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}]
      ~doc:"Create a placeholder for a binary blob"
      ~flags:[`Session]
      ~result:(Ref _blob, "The reference to the created blob")
      ~allowed_roles:_R_POOL_OP
      ()
  let destroy = call
      ~name:"destroy"
      ~in_product_since:rel_orlando
      ~params:[Ref _blob, "self", "The reference of the blob to destroy"]
      ~flags:[`Session]
      ~allowed_roles:_R_POOL_OP
      ()
  let t =

    create_obj ~in_db:true ~in_product_since:rel_orlando ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_blob ~descr:"A placeholder for a binary blob"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[create;destroy] ~contents:
      [ uid _blob;
        namespace ~name:"name" ~contents:(names oss_since_303 RW) ();
        field ~qualifier:DynamicRO ~ty:Int "size" "Size of the binary data, in bytes";
        field ~writer_roles:_R_POOL_OP ~qualifier:RW ~in_product_since:rel_tampa ~default_value:(Some (VBool false)) ~ty:Bool "public" "True if the blob is publicly accessible";
        field ~qualifier:StaticRO ~ty:DateTime "last_updated" "Time at which the data in the blob was last updated";
        field ~qualifier:StaticRO ~ty:String "mime_type" "The mime type associated with this object. Defaults to 'application/octet-stream' if the empty string is supplied"]
      ()
end

module Message = struct
  let cls =
    Enum ("cls", [ "VM", "VM";
                   "Host", "Host";
                   "SR", "SR";
                   "Pool","Pool";
                   "VMPP","VMPP";
                   "VMSS", "VMSS";
                   "PVS_proxy","PVS_proxy";
                   "VDI","VDI";
                 ])

  let create = call
      ~name:"create"
      ~in_product_since:rel_orlando
      ~params:[String, "name", "The name of the message";
               Int, "priority", "The priority of the message";
               cls, "cls", "The class of object this message is associated with";
               String, "obj_uuid", "The uuid of the object this message is associated with";
               String, "body", "The body of the message"]
      ~flags:[`Session]
      ~result:(Ref _message, "The reference of the created message")
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~in_product_since:rel_orlando
      ~params:[Ref _message, "self", "The reference of the message to destroy"]
      ~flags:[`Session]
      ~allowed_roles:_R_POOL_OP
      ()

  let get_all = call
      ~name:"get_all"
      ~in_product_since:rel_orlando
      ~params:[]
      ~flags:[`Session]
      ~result:(Set(Ref _message), "The references to the messages")
      ~allowed_roles:_R_READ_ONLY
      ()

  let get = call
      ~name:"get"
      ~in_product_since:rel_orlando
      ~params:[cls, "cls", "The class of object";
               String, "obj_uuid", "The uuid of the object";
               DateTime, "since", "The cutoff time"]
      ~flags:[`Session]
      ~result:(Map(Ref _message, Record _message), "The relevant messages")
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_since = call
      ~name:"get_since"
      ~in_product_since:rel_orlando
      ~params:[DateTime, "since", "The cutoff time"]
      ~flags:[`Session]
      ~result:(Map(Ref _message, Record _message), "The relevant messages")
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_by_uuid = call
      ~name:"get_by_uuid"
      ~in_product_since:rel_orlando
      ~params:[String, "uuid", "The uuid of the message"]
      ~flags:[`Session]
      ~result:(Ref _message, "The message reference")
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_record = call
      ~name:"get_record"
      ~in_product_since:rel_orlando
      ~params:[Ref _message, "self", "The reference to the message"]
      ~flags:[`Session]
      ~result:(Record _message, "The message record")
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_all_records = call
      ~name:"get_all_records"
      ~in_product_since:rel_orlando
      ~params:[]
      ~flags:[`Session]
      ~result:(Map(Ref _message, Record _message), "The messages")
      ~allowed_roles:_R_READ_ONLY
      ()

  let get_all_records_where = call
      ~name:"get_all_records_where"
      ~in_product_since:rel_orlando
      ~params:[String, "expr", "The expression to match (not currently used)"]
      ~flags:[`Session]
      ~result:(Map(Ref _message, Record _message), "The messages")
      ~allowed_roles:_R_READ_ONLY
      ()

  let t =
    create_obj ~in_db:false ~in_product_since:rel_orlando ~in_oss_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_message ~descr:"An message for the attention of the administrator" ~gen_events:true
      ~doccomments:[] ~internal_deprecated_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[create;destroy;get;get_all; get_since; get_record; get_by_uuid; get_all_records; get_all_records_where] ~contents:
      [ uid _message;
        field ~qualifier:DynamicRO ~ty:String "name" "The name of the message";
        field ~qualifier:DynamicRO ~ty:Int "priority" "The message priority, 0 being low priority";
        field ~qualifier:DynamicRO ~ty:cls "cls" "The class of the object this message is associated with";
        field ~qualifier:DynamicRO ~ty:String "obj_uuid" "The uuid of the object this message is associated with";
        field ~qualifier:DynamicRO ~ty:DateTime "timestamp" "The time at which the message was created";
        field ~qualifier:DynamicRO ~ty:String "body" "The body of the message"; ]
      ()

end

module Secret = struct

  let introduce = call
      ~name:"introduce"
      ~in_product_since:rel_midnight_ride
      ~versioned_params:[
        {param_type=String; param_name="uuid"; param_doc=""; param_release=midnight_ride_release; param_default=None};
        {param_type=String; param_name="value"; param_doc=""; param_release=midnight_ride_release; param_default=None};
        {param_type=(Map (String, String)); param_name="other_config"; param_doc=""; param_release=boston_release; param_default=Some (VMap [])}
      ]
      ~flags:[`Session]
      ~result:(Ref _secret, "")
      ~secret:true
      ~hide_from_docs:true
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj
      ~descr:"A secret"
      ~doccomments:[]
      ~gen_constructor_destructor:true
      ~gen_events:false
      ~in_db:true
      ~in_oss_since:None
      ~in_product_since:rel_midnight_ride
      ~internal_deprecated_since:None
      ~messages:[introduce]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~implicit_messages_allowed_roles:_R_POOL_OP
      ~name:_secret
      ~persist:PersistEverything
      ~contents:
        [ uid ~reader_roles:_R_POOL_OP _secret
        ; field ~reader_roles:_R_POOL_OP ~qualifier:RW ~ty:String "value" "the secret"
        ; field ~qualifier:RW ~ty:(Map (String,String)) "other_config" "other_config" ~default_value:(Some (VMap []));
        ]
      ()
end

(*

let alert =
  create_obj ~in_product_since:rel_miami ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_alert ~descr:"Notification information"
    ~gen_events:true
    ~doccomments:[]
    ~messages: []
    ~contents:
    [
     uid ~in_oss_since:None _alert;
     field ~in_oss_since:None ~qualifier:StaticRO ~ty:String "message" "description of the alert";
     field ~in_oss_since:None ~qualifier:StaticRO ~ty:(Map (String, String)) ~default_value:(Some (VMap [])) "params" "parameters of the alert";
     field ~in_oss_since:None ~qualifier:StaticRO ~ty:alert_level "level" "level of importance (info/warning/error/critical)";
     field ~in_oss_since:None ~qualifier:DynamicRO ~ty:Bool "system" "system task";
     field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Ref _task) "task" "task related to this alert (null reference if there's no task associated)";
    ]
    ()
*)

(** network sriov **)
module Network_sriov = struct
  let lifecycle = [Published, rel_kolkata, ""]

  let sriov_configuration_mode = Enum ("sriov_configuration_mode",
    [
      "sysfs", "Configure network sriov by sysfs, do not need reboot";
      "modprobe", "Configure network sriov by modbrope, need reboot";
      "unknown", "Unknown mode";
    ])

  let create = call
      ~name:"create"
      ~doc:"Enable SR-IOV on the specific PIF. It will create a network-sriov based on the specific PIF and automatically create a logical PIF to connect the specific network."
      ~params:[Ref _pif, "pif", "PIF on which to enable SR-IOV";
               Ref _network, "network", "Network to connect SR-IOV virtual functions with VM VIFs"]
      ~result:(Ref _network_sriov, "The reference of the created network_sriov object")
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~doc:"Disable SR-IOV on the specific PIF. It will destroy the network-sriov and the logical PIF accordingly."
      ~params:[Ref _network_sriov, "self", "SRIOV to destroy"]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let get_remaining_capacity = call
      ~name:"get_remaining_capacity"
      ~doc:"Get the number of free SR-IOV VFs on the associated PIF"
      ~params:[Ref _network_sriov, "self", "the NETWORK_SRIOV object"]
      ~lifecycle
      ~result:(Int, "The number of free SR-IOV VFs on the associated PIF")
      ~allowed_roles:_R_READ_ONLY
      ()

  let t =
    create_obj
      ~name:_network_sriov
      ~descr:"network-sriov which connects logical pif and physical pif"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~messages:[create; destroy; get_remaining_capacity]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:
        ([
          uid _network_sriov;
          field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle "physical_PIF" "The PIF that has SR-IOV enabled" ~default_value:(Some (VRef ""));
          field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle "logical_PIF" "The logical PIF to connect to the SR-IOV network after enable SR-IOV on the physical PIF" ~default_value:(Some (VRef ""));
          field ~qualifier:DynamicRO ~ty:Bool ~lifecycle "requires_reboot" "Indicates whether the host need to be rebooted before SR-IOV is enabled on the physical PIF" ~default_value:(Some (VBool false));
          field ~qualifier:DynamicRO ~ty:sriov_configuration_mode ~lifecycle "configuration_mode" "The mode for configure network sriov" ~default_value:(Some (VEnum "unknown"));
        ])
      ()
end

(** PCI devices *)

module PCI = struct
  let t =
    create_obj
      ~name:_pci
      ~descr:"A PCI device"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle:[Published, rel_boston, ""]
      ~messages:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:[
        uid _pci ~lifecycle:[Published, rel_boston, ""];
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[] "class_id" "PCI class ID" ~default_value:(Some (VString "")) ~internal_only:true;
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_boston, ""] "class_name" "PCI class name" ~default_value:(Some (VString ""));
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[] "vendor_id" "Vendor ID" ~default_value:(Some (VString "")) ~internal_only:true;
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_boston, ""] "vendor_name" "Vendor name" ~default_value:(Some (VString ""));
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[] "device_id" "Device ID" ~default_value:(Some (VString "")) ~internal_only:true;
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_boston, ""] "device_name" "Device name" ~default_value:(Some (VString ""));
        field ~qualifier:StaticRO ~ty:(Ref _host) ~lifecycle:[Published, rel_boston, ""] "host" "Physical machine that owns the PCI device" ~default_value:(Some (VRef null_ref));
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_boston, ""] "pci_id" "PCI ID of the physical device" ~default_value:(Some (VString ""));
        field ~qualifier:DynamicRO ~ty:Int ~lifecycle:[] ~default_value:(Some (VInt 1L)) "functions" "Number of physical + virtual PCI functions" ~internal_only:true;
        field ~qualifier:DynamicRO ~ty:(Set (Ref _pci)) ~lifecycle:[Published, rel_falcon, ""] "virtual_functions" "Set of VF PCI devices provided by this physical (PF) PCI device" ~internal_only:true;
        field ~qualifier:StaticRO ~ty:(Ref _pci) ~lifecycle:[Published, rel_falcon, ""] "physical_function" "The PF (physical PCI device) that provides this VF" ~default_value:(Some (VRef null_ref)) ~internal_only:true;
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) ~lifecycle:[] "attached_VMs"
          "VMs that currently have a function of this PCI device passed-through to them" ~internal_only:true;
        field ~qualifier:DynamicRO ~ty:(Set (Ref _pci)) ~lifecycle:[Published, rel_boston, ""] "dependencies" "List of dependent PCI devices" ~ignore_foreign_key:true;
        field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_boston, ""] "other_config" "Additional configuration" ~default_value:(Some (VMap []));
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[] "subsystem_vendor_id" "Subsystem vendor ID" ~default_value:(Some (VString "")) ~internal_only:true;
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_clearwater_whetstone, ""] "subsystem_vendor_name" "Subsystem vendor name" ~default_value:(Some (VString ""));
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[] "subsystem_device_id" "Subsystem device ID" ~default_value:(Some (VString "")) ~internal_only:true;
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_clearwater_whetstone, ""] "subsystem_device_name" "Subsystem device name" ~default_value:(Some (VString ""));
        field ~qualifier:DynamicRO ~ty:(Ref _vm) ~lifecycle:[Published, rel_falcon, ""] ~internal_only:true "scheduled_to_be_attached_to" "The VM to which this PCI device is scheduled to be attached (passed through)" ~default_value:(Some (VRef null_ref));
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_kolkata, ""] "driver_name" "Driver name" ~default_value:(Some (VString ""));
      ]
      ()
end

(** Physical GPUs (pGPU) *)

module PGPU = struct
  let dom0_access =
    Enum ("pgpu_dom0_access", [
        "enabled", "dom0 can access this device as normal";
        "disable_on_reboot", "On host reboot dom0 will be blocked from accessing this device";
        "disabled", "dom0 cannot access this device";
        "enable_on_reboot", "On host reboot dom0 will be allowed to access this device";
      ])

  let add_enabled_VGPU_types = call
      ~name:"add_enabled_VGPU_types"
      ~lifecycle:[Published, rel_vgpu_tech_preview, ""]
      ~versioned_params:[
        {
          param_type = (Ref _pgpu);
          param_name = "self";
          param_doc = "The PGPU to which we are adding an enabled VGPU type";
          param_release = vgpu_tech_preview_release;
          param_default = None;
        };
        {
          param_type = (Ref _vgpu_type);
          param_name = "value";
          param_doc = "The VGPU type to enable";
          param_release = vgpu_tech_preview_release;
          param_default = None;
        };
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let remove_enabled_VGPU_types = call
      ~name:"remove_enabled_VGPU_types"
      ~lifecycle:[Published, rel_vgpu_tech_preview, ""]
      ~versioned_params:[
        {
          param_type = (Ref _pgpu);
          param_name = "self";
          param_doc = "The PGPU from which we are removing an enabled VGPU type";
          param_release = vgpu_tech_preview_release;
          param_default = None;
        };
        {
          param_type = (Ref _vgpu_type);
          param_name = "value";
          param_doc = "The VGPU type to disable";
          param_release = vgpu_tech_preview_release;
          param_default = None;
        };
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_enabled_VGPU_types = call
      ~name:"set_enabled_VGPU_types"
      ~lifecycle:[Published, rel_vgpu_tech_preview, ""]
      ~versioned_params:[
        {
          param_type = (Ref _pgpu);
          param_name = "self";
          param_doc = "The PGPU on which we are enabling a set of VGPU types";
          param_release = vgpu_tech_preview_release;
          param_default = None;
        };
        {
          param_type = Set (Ref _vgpu_type);
          param_name = "value";
          param_doc = "The VGPU types to enable";
          param_release = vgpu_tech_preview_release;
          param_default = None;
        };
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let set_GPU_group = call
      ~name:"set_GPU_group"
      ~lifecycle:[Published, rel_vgpu_tech_preview, ""]
      ~versioned_params:[
        {param_type=(Ref _pgpu); param_name="self"; param_doc="The PGPU to move to a new group"; param_release=vgpu_tech_preview_release; param_default=None};
        {param_type=(Ref _gpu_group); param_name="value"; param_doc="The group to which the PGPU will be moved"; param_release=vgpu_tech_preview_release; param_default=None};
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let get_remaining_capacity = call
      ~name:"get_remaining_capacity"
      ~lifecycle:[Published, rel_vgpu_tech_preview, ""]
      ~versioned_params:[
        {
          param_type = (Ref _pgpu);
          param_name = "self";
          param_doc = "The PGPU to query";
          param_release = vgpu_tech_preview_release;
          param_default = None;
        };
        {
          param_type = (Ref _vgpu_type);
          param_name = "vgpu_type";
          param_doc = "The VGPU type for which we want to find the number of VGPUs which can still be started on this PGPU";
          param_release = vgpu_tech_preview_release;
          param_default = None;
        };
      ]
      ~result:(Int, "The number of VGPUs of the specified type which can still be started on this PGPU")
      ~allowed_roles:_R_READ_ONLY
      ()

  let enable_dom0_access = call
      ~name:"enable_dom0_access"
      ~lifecycle:[Published, rel_cream, ""]
      ~versioned_params:[
        {
          param_type = (Ref _pgpu);
          param_name = "self";
          param_doc = "The PGPU to which dom0 will be granted access";
          param_release = cream_release;
          param_default = None;
        };
      ]
      ~result:(dom0_access, "The accessibility of this PGPU from dom0")
      ~allowed_roles:_R_POOL_OP
      ()

  let disable_dom0_access = call
      ~name:"disable_dom0_access"
      ~lifecycle:[Published, rel_cream, ""]
      ~versioned_params:[
        {
          param_type = (Ref _pgpu);
          param_name = "self";
          param_doc = "The PGPU to which dom0 will be denied access";
          param_release = cream_release;
          param_default = None;
        };
      ]
      ~result:(dom0_access, "The accessibility of this PGPU from dom0")
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj
      ~name:_pgpu
      ~descr:"A physical GPU (pGPU)"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle:[Published, rel_boston, ""]
      ~messages:[
        add_enabled_VGPU_types;
        remove_enabled_VGPU_types;
        set_enabled_VGPU_types;
        set_GPU_group;
        get_remaining_capacity;
        enable_dom0_access;
        disable_dom0_access;
      ]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:[
        uid _pgpu ~lifecycle:[Published, rel_boston, ""];
        field ~qualifier:StaticRO ~ty:(Ref _pci) ~lifecycle:[Published, rel_boston, ""] "PCI" "Link to underlying PCI device" ~default_value:(Some (VRef null_ref));
        field ~qualifier:StaticRO ~ty:(Ref _gpu_group) ~lifecycle:[Published, rel_boston, ""] "GPU_group" "GPU group the pGPU is contained in" ~default_value:(Some (VRef null_ref));
        field ~qualifier:DynamicRO ~ty:(Ref _host) ~lifecycle:[Published, rel_boston, ""] "host" "Host that owns the GPU" ~default_value:(Some (VRef null_ref));
        field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_boston, ""] "other_config" "Additional configuration" ~default_value:(Some (VMap []));
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type)) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "supported_VGPU_types" "List of VGPU types supported by the underlying hardware";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type)) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "enabled_VGPU_types" "List of VGPU types which have been enabled for this PGPU";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu)) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "resident_VGPUs" "List of VGPUs running on this PGPU";
        field ~qualifier:StaticRO ~ty:Int ~lifecycle:[Published, rel_vgpu_tech_preview, ""] ~internal_only:true ~default_value:(Some (VInt Constants.pgpu_default_size)) "size" "Abstract size of this PGPU";
        field ~qualifier:DynamicRO ~ty:(Map (Ref _vgpu_type, Int)) ~lifecycle:[Published, rel_vgpu_productisation, ""] ~default_value:(Some (VMap [])) "supported_VGPU_max_capacities" "A map relating each VGPU type supported on this GPU to the maximum number of VGPUs of that type which can run simultaneously on this GPU";
        field ~qualifier:DynamicRO ~ty:(dom0_access) ~lifecycle:[Published, rel_cream, ""] ~default_value:(Some (VEnum "enabled")) "dom0_access" "The accessibility of this device from dom0";
        field ~qualifier:DynamicRO ~ty:Bool ~lifecycle:[Published, rel_cream, ""] ~default_value:(Some (VBool false)) "is_system_display_device" "Is this device the system display device";
        field ~qualifier:DynamicRO ~ty:(Map (String,String)) ~lifecycle:[Published, rel_inverness, ""] ~default_value:(Some (VMap [])) "compatibility_metadata" "PGPU metadata to determine whether a VGPU can migrate between two PGPUs";
      ]
      ()
end

(** Groups of GPUs *)

module GPU_group = struct
  let create = call
      ~name:"create"
      ~lifecycle:[Published, rel_boston, ""]
      ~versioned_params:[
        {param_type=(String); param_name="name_label"; param_doc=""; param_release=boston_release; param_default=Some (VString "")};
        {param_type=(String); param_name="name_description"; param_doc=""; param_release=boston_release; param_default=Some (VString "")};
        {param_type=(Map (String, String)); param_name="other_config"; param_doc=""; param_release=boston_release; param_default=Some (VMap [])}
      ]
      ~result:(Ref _gpu_group, "")
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~lifecycle:[Published, rel_boston, ""]
      ~params:[
        Ref _gpu_group, "self", "The GPU group to destroy"
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let update_enabled_VGPU_types = call
      ~name:"update_enabled_VGPU_types"
      ~hide_from_docs:true
      ~lifecycle:[Published, rel_vgpu_productisation, ""]
      ~params:[
        Ref _gpu_group, "self", "The GPU group to update";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let update_supported_VGPU_types = call
      ~name:"update_supported_VGPU_types"
      ~hide_from_docs:true
      ~lifecycle:[Published, rel_vgpu_productisation, ""]
      ~params:[
        Ref _gpu_group, "self", "The GPU group to update";
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let get_remaining_capacity = call
      ~name:"get_remaining_capacity"
      ~lifecycle:[Published, rel_vgpu_tech_preview, ""]
      ~params:[
        Ref _gpu_group, "self", "The GPU group to query";
        Ref _vgpu_type, "vgpu_type", "The VGPU_type for which the remaining capacity will be calculated";
      ]
      ~result:(Int, "The number of VGPUs of the given type which can still be started on the PGPUs in the group")
      ~allowed_roles:_R_READ_ONLY
      ()

  let allocation_algorithm =
    Enum ("allocation_algorithm",
          [ "breadth_first", "vGPUs of a given type are allocated evenly across supporting pGPUs.";
            "depth_first", "vGPUs of a given type are allocated on supporting pGPUs until they are full."]
         )

  let t =
    create_obj
      ~name:_gpu_group
      ~descr:"A group of compatible GPUs across the resource pool"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle:[Published, rel_boston, ""]
      ~messages:[
        create;
        destroy;
        update_enabled_VGPU_types;
        update_supported_VGPU_types;
        get_remaining_capacity;
      ]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:[
        uid _gpu_group ~lifecycle:[Published, rel_boston, ""];
        namespace ~name:"name" ~contents:(names None RW ~lifecycle:[Published, rel_boston, ""]) ();
        field ~qualifier:DynamicRO ~ty:(Set (Ref _pgpu)) ~lifecycle:[Published, rel_boston, ""] "PGPUs" "List of pGPUs in the group";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu)) ~lifecycle:[Published, rel_boston, ""] "VGPUs" "List of vGPUs using the group";
        field ~qualifier:DynamicRO ~ty:(Set String) ~lifecycle:[Published, rel_boston, ""] "GPU_types" "List of GPU types (vendor+device ID) that can be in this group" ~default_value:(Some (VSet []));
        field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_boston, ""] "other_config" "Additional configuration" ~default_value:(Some (VMap []));
        field ~qualifier:RW ~ty:allocation_algorithm ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "allocation_algorithm" "Current allocation of vGPUs to pGPUs for this group" ~default_value:(Some (VEnum "depth_first"));
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type)) ~lifecycle:[Published, rel_vgpu_productisation, ""] "supported_VGPU_types" "vGPU types supported on at least one of the pGPUs in this group";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type)) ~lifecycle:[Published, rel_vgpu_productisation, ""] "enabled_VGPU_types" "vGPU types supported on at least one of the pGPUs in this group";
      ]
      ()
end


(** Virtual GPUs (vGPU) *)

module VGPU = struct
  let create = call
      ~name:"create"
      ~lifecycle:[Published, rel_boston, ""]
      ~versioned_params:[
        {param_type=(Ref _vm); param_name="VM"; param_doc=""; param_release=boston_release; param_default=None};
        {param_type=(Ref _gpu_group); param_name="GPU_group"; param_doc=""; param_release=boston_release; param_default=None};
        {param_type=String; param_name="device"; param_doc=""; param_release=boston_release; param_default=Some (VString "0")};
        {param_type=(Map (String, String)); param_name="other_config"; param_doc=""; param_release=boston_release; param_default=Some (VMap [])};
        {param_type=(Ref _vgpu_type); param_name="type"; param_doc=""; param_release=vgpu_tech_preview_release; param_default=(Some (VRef null_ref))};
      ]
      ~result:(Ref _vgpu, "reference to the newly created object")
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~lifecycle:[Published, rel_boston, ""]
      ~params:[
        Ref _vgpu, "self", "The vGPU to destroy"
      ]
      ~allowed_roles:_R_POOL_OP
      ()

  let atomic_set_resident_on = call
      ~name:"atomic_set_resident_on"
      ~lifecycle:[Published, rel_dundee, ""]
      ~params:[
        Ref _vgpu, "self", "The vGPU to modify";
        Ref _pgpu, "value", "The pGPU on which the vGPU is running";
      ]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~hide_from_docs:true
      ~pool_internal:true
      ()

  let t =
    create_obj
      ~name:_vgpu
      ~descr:"A virtual GPU (vGPU)"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle:[Published, rel_boston, ""]
      ~messages:[create; destroy; atomic_set_resident_on]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:[
        uid _vgpu ~lifecycle:[Published, rel_boston, ""];
        field ~qualifier:DynamicRO ~ty:(Ref _vm) ~lifecycle:[Published, rel_boston, ""] "VM" "VM that owns the vGPU";
        field ~qualifier:DynamicRO ~ty:(Ref _gpu_group) ~lifecycle:[Published, rel_boston, ""] "GPU_group" "GPU group used by the vGPU";
        field ~qualifier:DynamicRO ~ty:String ~lifecycle:[Published, rel_boston, ""] ~default_value:(Some (VString "0")) "device" "Order in which the devices are plugged into the VM";
        field ~qualifier:DynamicRO ~ty:Bool ~lifecycle:[Published, rel_boston, ""] ~default_value:(Some (VBool false)) "currently_attached" "Reflects whether the virtual device is currently connected to a physical device";
        field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_boston, ""] "other_config" "Additional configuration" ~default_value:(Some (VMap []));
        field ~qualifier:DynamicRO ~ty:(Ref _vgpu_type) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "type" "Preset type for this VGPU" ~default_value:(Some (VRef null_ref));
        field ~qualifier:DynamicRO ~ty:(Ref _pgpu) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "resident_on" "The PGPU on which this VGPU is running" ~default_value:(Some (VRef null_ref));
        field ~qualifier:DynamicRO ~ty:(Ref _pgpu) ~lifecycle:[Published, rel_dundee, ""] "scheduled_to_be_resident_on" "The PGPU on which this VGPU is scheduled to run" ~default_value:(Some (VRef null_ref));
        field ~qualifier:DynamicRO ~ty:(Map (String,String)) ~lifecycle:[Published, rel_inverness, ""] ~default_value:(Some (VMap [])) "compatibility_metadata" "VGPU metadata to determine whether a VGPU can migrate between two PGPUs";
      ]
      ()
end

(** Virtual GPU types (i.e. preset sizes) *)

module VGPU_type = struct
  let implementation =
    Enum ("vgpu_type_implementation", [
        "passthrough", "Pass through an entire physical GPU to a guest";
        "nvidia", "vGPU using NVIDIA hardware";
        "gvt_g", "vGPU using Intel GVT-g";
        "mxgpu", "vGPU using AMD MxGPU";
      ])

  let t =
    create_obj
      ~name:_vgpu_type
      ~descr:"A type of virtual GPU"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle:[Published, rel_vgpu_tech_preview, ""]
      ~messages:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:[
        uid _vgpu_type ~lifecycle:[Published, rel_vgpu_tech_preview, ""];
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_vgpu_tech_preview, ""] ~default_value:(Some (VString "")) "vendor_name" "Name of VGPU vendor";
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_vgpu_tech_preview, ""] ~default_value:(Some (VString "")) "model_name" "Model name associated with the VGPU type";
        field ~qualifier:StaticRO ~ty:Int ~lifecycle:[Published, rel_vgpu_tech_preview, ""] ~default_value:(Some (VInt 0L)) "framebuffer_size" "Framebuffer size of the VGPU type, in bytes";
        field ~qualifier:StaticRO ~ty:Int ~lifecycle:[Published, rel_vgpu_tech_preview, ""] ~default_value:(Some (VInt 0L)) "max_heads" "Maximum number of displays supported by the VGPU type";
        field ~qualifier:StaticRO ~ty:Int ~lifecycle:[Published, rel_vgpu_productisation, ""] ~default_value:(Some (VInt 0L)) "max_resolution_x" "Maximum resolution (width) supported by the VGPU type";
        field ~qualifier:StaticRO ~ty:Int ~lifecycle:[Published, rel_vgpu_productisation, ""] ~default_value:(Some (VInt 0L)) "max_resolution_y" "Maximum resolution (height) supported by the VGPU type";
        field ~qualifier:StaticRO ~ty:Int ~lifecycle:[Published, rel_vgpu_tech_preview, ""] ~internal_only:true ~default_value:(Some (VInt 0L)) "size" "Abstract size for tracking PGPU utilisation";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _pgpu)) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "supported_on_PGPUs" "List of PGPUs that support this VGPU type";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _pgpu)) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "enabled_on_PGPUs" "List of PGPUs that have this VGPU type enabled";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu)) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] "VGPUs" "List of VGPUs of this type";
        field ~qualifier:StaticRO ~ty:(Map (String, String)) ~lifecycle:[Published, rel_vgpu_tech_preview, ""] ~default_value:(Some (VMap [])) ~internal_only:true "internal_config" "Extra configuration information for internal use.";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _gpu_group)) ~lifecycle:[Published, rel_vgpu_productisation, ""] "supported_on_GPU_groups" "List of GPU groups in which at least one PGPU supports this VGPU type";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _gpu_group)) ~lifecycle:[Published, rel_vgpu_productisation, ""] "enabled_on_GPU_groups" "List of GPU groups in which at least one have this VGPU type enabled";
        field ~qualifier:StaticRO ~ty:implementation ~lifecycle:[Published, rel_dundee, ""] ~default_value:(Some (VEnum "passthrough")) "implementation" "The internal implementation of this VGPU type";
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_dundee, ""] ~default_value:(Some (VString "")) "identifier" "Key used to identify VGPU types and avoid creating duplicates - this field is used internally and not intended for interpretation by API clients";
        field ~qualifier: StaticRO ~ty:Bool ~lifecycle:[Published, rel_dundee, ""] ~default_value:(Some (VBool false)) "experimental" "Indicates whether VGPUs of this type should be considered experimental";
      ]
      ()
end

module PVS_site = struct
  let lifecycle = [Published, rel_ely, ""]

  let introduce = call
      ~name:"introduce"
      ~doc:"Introduce new PVS site"
      ~result:(Ref _pvs_site, "the new PVS site")
      ~params:
        [ String, "name_label", "name of the PVS site"
        ; String, "name_description", "description of the PVS site"
        ; String, "PVS_uuid", "unique identifier of the PVS site"
        ]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let forget = call
      ~name:"forget"
      ~doc:"Remove a site's meta data"
      ~params:
        [ Ref _pvs_site, "self", "this PVS site"
        ]
      ~errs:[
        Api_errors.pvs_site_contains_running_proxies;
        Api_errors.pvs_site_contains_servers;
      ]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let set_PVS_uuid = call
      ~name:"set_PVS_uuid"
      ~doc:"Update the PVS UUID of the PVS site"
      ~params:
        [ Ref _pvs_site, "self", "this PVS site"
        ; String, "value", "PVS UUID to be used"
        ]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    let null_str = Some (VString "") in
    let null_set = Some (VSet []) in
    create_obj
      ~name: _pvs_site
      ~descr:"machines serving blocks of data for provisioning VMs"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~persist:PersistEverything
      ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [ uid     _pvs_site ~lifecycle

        ; namespace ~name:"name" ~contents:(names None RW ~lifecycle) ()

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:String "PVS_uuid" ~default_value:null_str
            "Unique identifier of the PVS site, as configured in PVS"

        ; field   ~qualifier:DynamicRO ~lifecycle
            ~ty:(Set (Ref _pvs_cache_storage)) "cache_storage" ~default_value:null_set
            ~ignore_foreign_key:true
            "The SR used by PVS proxy for the cache"

        ; field   ~qualifier:DynamicRO ~lifecycle
            ~ty:(Set (Ref _pvs_server)) "servers"
            "The set of PVS servers in the site"

        ; field   ~qualifier:DynamicRO ~lifecycle
            ~ty:(Set (Ref _pvs_proxy)) "proxies"
            "The set of proxies associated with the site"
        ]
      ~messages:
        [ introduce
        ; forget
        ; set_PVS_uuid
        ]
      ()
end

module PVS_server = struct
  let lifecycle = [Published, rel_ely, ""]

  let introduce = call
      ~name:"introduce"
      ~doc:"introduce new PVS server"
      ~result:(Ref _pvs_server, "the new PVS server")
      ~params:
        [ Set(String),"addresses","IPv4 addresses of the server"
        ; Int, "first_port", "first UDP port accepted by this server"
        ; Int, "last_port", "last UDP port accepted by this server"
        ; Ref(_pvs_site), "site", "PVS site this server is a part of"
        ]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let forget = call
      ~name:"forget"
      ~doc:"forget a PVS server"
      ~params:
        [ Ref _pvs_server, "self", "this PVS server"
        ]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    let null_ref = Some (VRef null_ref) in
    let null_int = Some (VInt 0L) in
    let null_set=  Some (VSet []) in
    create_obj
      ~name: _pvs_server
      ~descr:"individual machine serving provisioning (block) data"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~persist:PersistEverything
      ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [ uid     _pvs_server ~lifecycle

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:(Set String) "addresses" ~default_value:null_set
            "IPv4 addresses of this server"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:Int "first_port" ~default_value:null_int
            "First UDP port accepted by this server"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:Int "last_port" ~default_value:null_int
            "Last UDP port accepted by this server"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:(Ref _pvs_site) "site" ~default_value:null_ref
            "PVS site this server is part of"
        ]
      ~messages:
        [ introduce
        ; forget
        ]
      ()
end

module PVS_proxy = struct
  let lifecycle = [Published, rel_ely, ""]

  let status = Enum ("pvs_proxy_status", [
      "stopped", "The proxy is not currently running";
      "initialised", "The proxy is setup but has not yet cached anything";
      "caching", "The proxy is currently caching data";
      "incompatible_write_cache_mode", "The PVS device is configured to use an incompatible write-cache mode";
      "incompatible_protocol_version", "The PVS protocol in use is not compatible with the PVS proxy";
    ])

  let create = call
      ~name:"create"
      ~doc:"Configure a VM/VIF to use a PVS proxy"
      ~result:(Ref _pvs_proxy, "the new PVS proxy")
      ~params:
        [ Ref _pvs_site   , "site","PVS site that we proxy for"
        ; Ref _vif        , "VIF", "VIF for the VM that needs to be proxied"
        ]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let destroy = call
      ~name:"destroy"
      ~doc:"remove (or switch off) a PVS proxy for this VM"
      ~params:
        [ Ref _pvs_proxy  , "self", "this PVS proxy"
        ]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    let null_ref  = Some (VRef null_ref) in
    let null_bool = Some (VBool false) in
    create_obj
      ~name: _pvs_proxy
      ~descr:"a proxy connects a VM/VIF with a PVS site"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~persist:PersistEverything
      ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [ uid     _pvs_proxy ~lifecycle

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:(Ref _pvs_site) "site" ~default_value:null_ref
            "PVS site this proxy is part of"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:(Ref _vif) "VIF" ~default_value:null_ref
            "VIF of the VM using the proxy"

        ; field   ~qualifier:DynamicRO ~lifecycle
            ~ty:Bool "currently_attached" ~default_value:null_bool
            "true = VM is currently proxied"

        ; field   ~qualifier:DynamicRO ~lifecycle
            ~ty:status "status" ~default_value:(Some (VEnum "stopped"))
            "The run-time status of the proxy"
        ]
      ~messages:
        [ create
        ; destroy
        ]
      ()
end

module PVS_cache_storage = struct
  let lifecycle = [Published, rel_ely, ""]

  let t =
    let null_ref  = Some (VRef null_ref) in
    create_obj
      ~name: _pvs_cache_storage
      ~descr:"Describes the storage that is available to a PVS site for caching purposes"
      ~doccomments:[]
      ~gen_constructor_destructor:true
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~persist:PersistEverything
      ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [ uid     _pvs_cache_storage ~lifecycle

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:(Ref _host) "host" ~default_value:null_ref
            "The host on which this object defines PVS cache storage"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:(Ref _sr) "SR" ~default_value:null_ref
            "SR providing storage for the PVS cache"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:(Ref _pvs_site) "site" ~default_value:null_ref
            "The PVS_site for which this object defines the storage"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:Int "size" ~default_value:(Some (VInt (Int64.of_int (20 * 1024 * 1024 * 1024))))
            "The size of the cache VDI (in bytes)"

        ; field  ~qualifier:DynamicRO ~lifecycle
            ~ty:(Ref _vdi) "VDI" ~default_value:null_ref
            "The VDI used for caching"
        ]

      ~messages:
        [
        ]
      ()
end

(* ---------------
   Features
   ----------------*)

(** A new piece of functionality *)
module Feature = struct
  let t =
    create_obj
      ~name:_feature
      ~descr: "A new piece of functionality"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle:[Published, rel_falcon, ""]
      ~messages:[]
      ~messages_default_allowed_roles:_R_LOCAL_ROOT_ONLY
      ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:[
        uid _feature ~lifecycle:[Published, rel_falcon, ""];
        namespace ~name:"name" ~contents:(names None StaticRO) ();
        field ~qualifier:DynamicRO ~ty:Bool ~lifecycle:[Published, rel_falcon, ""] ~default_value:(Some (VBool false))
          "enabled" "Indicates whether the feature is enabled";
        field ~qualifier:StaticRO ~ty:Bool ~lifecycle:[Published, rel_falcon, ""] ~default_value:(Some (VBool false))
          "experimental" "Indicates whether the feature is experimental (as opposed to stable and fully supported)";
        field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_falcon, ""] ~default_value:(Some (VString "1.0"))
          "version" "The version of this feature";
        field ~qualifier:DynamicRO ~ty:(Ref _host) ~lifecycle:[Published, rel_falcon, ""]
          "host" "The host where this feature is available";
      ]
      ()
end

module SDN_controller = struct
  let lifecycle = [Published, rel_falcon, ""]

  let sdn_controller_protocol = Enum ("sdn_controller_protocol", [
      "ssl", "Active ssl connection";
      "pssl", "Passive ssl connection";
    ])

  let introduce = call
      ~name:"introduce"
      ~doc:"Introduce an SDN controller to the pool."
      ~result:(Ref _sdn_controller, "the introduced SDN controller")
      ~versioned_params:[
        {param_type=sdn_controller_protocol; param_name="protocol"; param_doc="Protocol to connect with the controller."; param_release=falcon_release; param_default=(Some (VEnum "ssl"))};
        {param_type=String; param_name="address"; param_doc="IP address of the controller."; param_release=falcon_release; param_default=(Some (VString ""))};
        {param_type=Int; param_name="port"; param_doc="TCP port of the controller."; param_release=falcon_release; param_default=(Some (VInt 0L)) }
      ]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let forget = call
      ~name:"forget"
      ~doc:"Remove the OVS manager of the pool and destroy the db record."
      ~params: [ Ref _sdn_controller, "self", "this SDN controller"]
      ~lifecycle
      ~allowed_roles:_R_POOL_OP
      ()

  let t =
    create_obj
      ~name: _sdn_controller
      ~descr:"Describes the SDN controller that is to connect with the pool"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~persist:PersistEverything
      ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [ uid     _sdn_controller ~lifecycle

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:sdn_controller_protocol "protocol" ~default_value:(Some (VEnum "ssl"))
            "Protocol to connect with SDN controller"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:String "address" ~default_value:(Some (VString ""))
            "IP address of the controller"

        ; field   ~qualifier:StaticRO ~lifecycle
            ~ty:Int "port" ~default_value:(Some (VInt 0L))
            "TCP port of the controller"
        ]
      ~messages:
        [ introduce
        ; forget
        ]
      ()
end

module PUSB = struct
  let lifecycle = [Published, rel_inverness, ""]

  let scan = call
      ~name:"scan"
      ~lifecycle
      ~params:[
        Ref _host, "host", "The host";
      ]
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let set_passthrough_enabled = call
      ~name:"set_passthrough_enabled"
      ~lifecycle
      ~params:[
        Ref _pusb, "self",  "this PUSB";
        Bool,     "value", "passthrough is enabled when true and disabled with false"
      ]
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let t =
    create_obj
      ~name: _pusb
      ~descr:"A physical USB device"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~persist:PersistEverything
      ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~contents:
        [
          uid    _pusb ~lifecycle;
          field ~qualifier:StaticRO ~ty:(Ref _usb_group) ~lifecycle "USB_group" "USB group the PUSB is contained in" ~default_value:(Some (VRef null_ref));
          field ~qualifier:StaticRO ~ty:(Ref _host) ~lifecycle "host" "Physical machine that owns the USB device" ~default_value:(Some (VRef null_ref));
          field ~qualifier:StaticRO ~ty:String ~lifecycle "path" "port path of USB device"~default_value:(Some (VString ""));
          field ~qualifier:StaticRO ~ty:String ~lifecycle "vendor_id" "vendor id of the USB device" ~default_value:(Some (VString ""));
          field ~qualifier:StaticRO ~ty:String ~lifecycle "vendor_desc" "vendor description of the USB device" ~default_value:(Some (VString ""));
          field ~qualifier:StaticRO ~ty:String ~lifecycle "product_id" "product id of the USB device" ~default_value:(Some (VString ""));
          field ~qualifier:StaticRO ~ty:String ~lifecycle "product_desc" "product description of the USB device" ~default_value:(Some (VString ""));
          field ~qualifier:StaticRO ~ty:String ~lifecycle "serial" "serial of the USB device" ~default_value:(Some (VString ""));
          field ~qualifier:StaticRO ~ty:String ~lifecycle "version" "USB device version" ~default_value:(Some (VString ""));
          field ~qualifier:StaticRO ~ty:String ~lifecycle "description" "USB device description" ~default_value:(Some (VString ""));
          field ~qualifier:DynamicRO ~ty:Bool ~lifecycle "passthrough_enabled" "enabled for passthrough" ~default_value:(Some (VBool false));
          field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_inverness, ""] "other_config" "additional configuration" ~default_value:(Some (VMap []));
        ]
      ~messages:
        [
          scan;
          set_passthrough_enabled;
        ]
      ()
end

(** Groups of USBs *)

module USB_group = struct
  let lifecycle = [Published, rel_inverness, ""]

  let create = call
      ~name:"create"
      ~lifecycle
      ~versioned_params:[
        {param_type=(String); param_name="name_label"; param_doc=""; param_release=inverness_release; param_default=Some (VString "")};
        {param_type=(String); param_name="name_description"; param_doc=""; param_release=inverness_release; param_default=Some (VString "")};
        {param_type=(Map (String, String)); param_name="other_config"; param_doc=""; param_release=inverness_release; param_default=Some (VMap [])}
      ]
      ~result:(Ref _usb_group, "")
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let destroy = call
      ~name:"destroy"
      ~lifecycle
      ~params:[
        Ref _usb_group, "self", "The USB group to destroy"
      ]
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let t =
    create_obj
      ~name:_usb_group
      ~descr:"A group of compatible USBs across the resource pool"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:[
        uid _usb_group ~lifecycle;
        namespace ~name:"name" ~contents:(names None RW ~lifecycle) ();
        field ~qualifier:DynamicRO ~ty:(Set (Ref _pusb)) ~lifecycle "PUSBs" "List of PUSBs in the group";
        field ~qualifier:DynamicRO ~ty:(Set (Ref _vusb)) ~lifecycle "VUSBs" "List of VUSBs using the group";
        field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle "other_config" "Additional configuration" ~default_value:(Some (VMap []));
      ]
      ~messages:[
        create;
        destroy;
      ]
      ()
end

module VUSB = struct
  let lifecycle = [Published, rel_inverness, ""]

  let vusb_operations =
    Enum ("vusb_operations",
          [
            "attach", "Attempting to attach this VUSB to a VM";
            "plug", "Attempting to plug this VUSB into a VM";
            "unplug", "Attempting to hot unplug this VUSB";
          ])

  let create = call
      ~name:"create"
      ~in_oss_since:None
      ~versioned_params:[
        {param_type=(Ref _vm); param_name="VM"; param_doc="The VM"; param_release=inverness_release; param_default=None};
        {param_type=(Ref _usb_group); param_name="USB_group"; param_doc=""; param_release=inverness_release; param_default=None};
        {param_type=(Map (String, String)); param_name="other_config"; param_doc=""; param_release=inverness_release; param_default=Some (VMap [])};
      ]
      ~lifecycle
      ~doc:"Create a new VUSB record in the database only"
      ~result:(Ref _vusb, "The ref of the newly created VUSB record.")
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let unplug = call
      ~name:"unplug"
      ~doc:"Unplug the vusb device from the vm."
      ~params: [ Ref _vusb, "self", "vusb deivce"]
      ~lifecycle
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let destroy = call
      ~name:"destroy"
      ~in_oss_since:None
      ~params:[ Ref _vusb, "self", "The VUSB to destroy about" ]
      ~doc:"Removes a VUSB record from the database"
      ~lifecycle
      ~allowed_roles:_R_POOL_ADMIN
      ()

  let t =
    create_obj
      ~name: _vusb
      ~descr:"Describes the vusb device"
      ~doccomments:[]
      ~gen_constructor_destructor:false
      ~gen_events:true
      ~in_db:true
      ~lifecycle
      ~persist:PersistEverything
      ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~contents:
        ([
          uid    _vusb ~lifecycle;
        ] @ (allowed_and_current_operations vusb_operations) @ [
            field ~qualifier:DynamicRO ~ty:(Ref _vm) ~lifecycle "VM" "VM that owns the VUSB";
            field ~qualifier:DynamicRO ~ty:(Ref _usb_group) ~lifecycle "USB_group" "USB group used by the VUSB";
            field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle "other_config" "Additional configuration" ~default_value:(Some (VMap []));
            field ~qualifier:DynamicRO ~ty:Bool  "currently_attached" "is the device currently attached" ~default_value:(Some (VBool false));
          ])
      ~messages:
        [ create
        ; unplug
        ; destroy
        ]
      ()
end

(******************************************************************************************)

(** All the objects in the system in order they will appear in documentation: *)
let all_system =
  [
    Session.t;
    Auth.t;
    Subject.t;
    Role.t;
    Task.t;
    Event.t;
    (* alert; *)

    Datamodel_pool.t;
    Pool_patch.t;
    Pool_update.t;

    Datamodel_vm.t;
    VM_metrics.t;
    VM_guest_metrics.t;
    VMPP.t;
    VMSS.t;
    VM_appliance.t;
    DR_task.t;
    Datamodel_host.t;
    Host_crashdump.t;
    Host_patch.t;
    Host_metrics.t;
    Host_cpu.t;
    (* network_manager; *)
    Network.t;
    VIF.t;
    VIF_metrics.t;
    PIF.t;
    PIF_metrics.t;
    Bond.t;
    VLAN.t;
    SM.t;
    SR.t;
    LVHD.t;
    VDI.t;
    VBD.t;
    VBD_metrics.t;
    PBD.t;
    Crashdump.t;
    (* misc *)
    VTPM.t;
    Console.t;
    (* filesystem; *)
    User.t;
    Data_source.t;
    Blob.t;
    Message.t;
    Secret.t;
    Tunnel.t;
    Network_sriov.t;
    PCI.t;
    PGPU.t;
    GPU_group.t;
    VGPU.t;
    VGPU_type.t;
    PVS_site.t;
    PVS_server.t;
    PVS_proxy.t;
    PVS_cache_storage.t;
    Feature.t;
    SDN_controller.t;
    Vdi_nbd_server_info.t;
    PUSB.t;
    USB_group.t;
    VUSB.t;
  ]

(** These are the pairs of (object, field) which are bound together in the database schema *)
(* If the relation is one-to-many, the "many" nodes (one edge each) must come before the "one" node (many edges) *)
(* If the relation is many-to-many, then the field which will be manually
 * updated must come first. The second field will be automatically * kept
 * up-to-date. *)
let all_relations =
  [
    (* snapshots *)
    (_vm, "snapshot_of"), (_vm, "snapshots");
    (_vdi, "snapshot_of"), (_vdi, "snapshots");
    (_vm, "parent"), (_vm, "children");

    (* subtasks hierarchy *)
    (_task, "subtask_of"), (_task, "subtasks");
    (_task, "session"), (_session, "tasks");

    (_pif, "bond_slave_of"), (_bond, "slaves");
    (_bond, "master"), (_pif, "bond_master_of");
    (_vlan, "tagged_PIF"), (_pif, "VLAN_slave_of");
    (_tunnel, "access_PIF"), (_pif, "tunnel_access_PIF_of");
    (_tunnel, "transport_PIF"), (_pif, "tunnel_transport_PIF_of");

    (_pbd, "host"), (_host, "PBDs");
    (_pbd, "SR"), (_sr, "PBDs");

    (_vbd, "VDI"), (_vdi, "VBDs");
    (_crashdump, "VDI"), (_vdi, "crash_dumps");
    (*  (_vdi, "parent"), (_vdi, "children"); *)

    (_vbd, "VM"), (_vm, "VBDs");
    (_crashdump, "VM"), (_vm, "crash_dumps");

    (* VM <-> VIF <-> network *)
    (_vif, "VM"), (_vm, "VIFs");
    (_vif, "network"), (_network, "VIFs");

    (* host <-> PIF <-> network *)
    (_pif, "host"), (_host, "PIFs");
    (_pif, "network"), (_network, "PIFs");

    (_vdi, "SR"), (_sr, "VDIs");

    (*  (_alert, "task"), (_task, "alerts"); *)

    (_vtpm, "VM"), (_vm, "VTPMs");
    (_console, "VM"), (_vm, "consoles");

    (_vm, "resident_on"), (_host, "resident_VMs");
    (_hostcpu, "host"), (_host, "host_CPUs");

    (_host_crashdump, "host"), (_host, "crashdumps");
    (_host_patch, "host"), (_host, "patches");
    (_host_patch, "pool_patch"), (_pool_patch, "host_patches");

    (_host, "updates"), (_pool_update, "hosts");

    (_subject, "roles"), (_subject, "roles");
    (*(_subject, "roles"), (_role, "subjects");*)
    (_role, "subroles"), (_role, "subroles");

    (_vm, "protection_policy"), (_vmpp, "VMs");
    (_vm, "snapshot_schedule"), (_vmss, "VMs");
    (_vm, "appliance"), (_vm_appliance, "VMs");

    (_pgpu, "GPU_group"), (_gpu_group, "PGPUs");
    (_vgpu, "GPU_group"), (_gpu_group, "VGPUs");
    (_vgpu, "type"), (_vgpu_type, "VGPUs");
    (_vgpu, "VM"), (_vm, "VGPUs");
    (_vgpu, "resident_on"), (_pgpu, "resident_VGPUs");
    (_pgpu, "supported_VGPU_types"), (_vgpu_type, "supported_on_PGPUs");
    (_pgpu, "enabled_VGPU_types"), (_vgpu_type, "enabled_on_PGPUs");
    (_gpu_group, "supported_VGPU_types"), (_vgpu_type, "supported_on_GPU_groups");
    (_gpu_group, "enabled_VGPU_types"), (_vgpu_type, "enabled_on_GPU_groups");
    (_pci, "host"), (_host, "PCIs");
    (_pgpu, "host"), (_host, "PGPUs");
    (_pci, "attached_VMs"), (_vm, "attached_PCIs");
    (_pci, "physical_function"), (_pci, "virtual_functions");

    (_vdi, "metadata_of_pool"), (_pool, "metadata_VDIs");
    (_sr, "introduced_by"), (_dr_task, "introduced_SRs");
    (_pvs_server, "site"), (_pvs_site, "servers");
    (_pvs_proxy,  "site"), (_pvs_site, "proxies");
    (_pvs_cache_storage,  "site"), (_pvs_site, "cache_storage");

    (_pusb, "host"), (_host, "PUSBs");
    (_pusb, "USB_group"), (_usb_group, "PUSBs");
    (_vusb, "USB_group"), (_usb_group, "VUSBs");
    (_vusb, "VM"), (_vm, "VUSBs");

    (_feature, "host"), (_host, "features");
    (_network_sriov, "physical_PIF"), (_pif, "sriov_physical_PIF_of");
    (_network_sriov, "logical_PIF"), (_pif, "sriov_logical_PIF_of");
  ]

(** the full api specified here *)
let all_api = Dm_api.make (all_system, all_relations)

(** These are the "emergency" calls that can be performed when a host is in "emergency mode" *)
let emergency_calls =
  [ (Datamodel_pool.t,Datamodel_pool.slave_reset_master);
    (Datamodel_pool.t,Datamodel_pool.transition_to_master);
    (Datamodel_pool.t,Datamodel_pool.ping_slave);
    (Session.t,Session.slave_local_login);
    (Session.t,Session.slave_local_login_with_password);
    (Session.t,Session.local_logout);
    (Datamodel_host.t,Datamodel_host.propose_new_master);
    (Datamodel_host.t,Datamodel_host.commit_new_master);
    (Datamodel_host.t,Datamodel_host.abort_new_master);
    (Datamodel_host.t,Datamodel_host.local_assert_healthy);
    (Datamodel_host.t,Datamodel_host.signal_networking_change);
    (Datamodel_host.t,Datamodel_host.local_management_reconfigure);
    (Datamodel_host.t,Datamodel_host.ha_xapi_healthcheck);
    (Datamodel_host.t,Datamodel_host.emergency_ha_disable);
    (Datamodel_host.t,Datamodel_host.management_disable);
    (Datamodel_host.t,Datamodel_host.get_system_status_capabilities);
    (Datamodel_host.t,Datamodel_host.is_in_emergency_mode);
    (Datamodel_host.t,Datamodel_host.shutdown_agent);
  ]

(** Whitelist of calls that will not get forwarded from the slave to master via the unix domain socket *)
let whitelist = [ (Session.t,Session.login);
                  (Session.t,Session.slave_login);
                ] @ emergency_calls

(* perform consistency checks on api at initialisation time *)
let _ = Dm_api.check all_api (List.map (fun (obj,msg) -> obj.name, msg.msg_name) emergency_calls)

(** List of classes to skip generating async handlers for *)
let no_async_messages_for = [ _session; _event; (* _alert; *) _task; _data_source; _blob ]

(** List of classes to generate 'get_all' messages for. Currently we don't
 ** allow a user to enumerate all the VBDs or VDIs directly: that must be
 ** through a VM or SR. *)
(* Note on the above: it looks like we _do_ have {VBD,VDI}.get_all! *)
let expose_get_all_messages_for = [
  _task;
  (* _alert; *)
  _host;
  _host_metrics;
  _hostcpu;
  _sr;
  _vm;
  _vm_metrics;
  _vm_guest_metrics;
  _network;
  _vif;
  _vif_metrics;
  _pif;
  _pif_metrics;
  _pbd;
  _vdi;
  _vbd;
  _vbd_metrics;
  _console;
  _crashdump;
  _host_crashdump;
  _host_patch;
  _pool;
  _sm;
  _pool_patch;
  _pool_update;
  _bond;
  _vlan;
  _blob;
  _subject;
  _role;
  _secret;
  _tunnel;
  _vmpp;
  _vmss;
  _vm_appliance;
  _pci;
  _pgpu;
  _gpu_group;
  _vgpu;
  _vgpu_type;
  _dr_task;
  _pvs_site;
  _pvs_server;
  _pvs_proxy;
  _pvs_cache_storage;
  _feature;
  _sdn_controller;
  _network_sriov;
  (* _vdi_nbd_server_info must NOT be included here *)
  _pusb;
  _usb_group;
  _vusb;
]

let no_task_id_for = [ _task; (* _alert; *) _event ]

let current_operations_for = [ _vm; (* _vdi; _host; _sr *) ]

(*** HTTP actions ***)

type action_arg =   (* I'm not using Datamodel_types here because we need varargs *)
    String_query_arg of string |
    Int64_query_arg of string |
    Bool_query_arg of string |
    Varargs_query_arg

type http_meth = Get | Put | Post | Connect | Options
let rbac_http_permission_prefix = "http/"

(* Each action has:
   (unique public name, (HTTP method, URI, whether to expose in SDK, [args to expose in SDK], [allowed_roles], [(sub-action,allowed_roles)]))
*)
let http_actions = [
  ("get_services", (Get, Constants.services_uri, true, [], _R_READ_ONLY, []));
  ("post_services", (Post, Constants.services_uri, false, [], _R_POOL_ADMIN, []));
  ("put_services", (Put, Constants.services_uri, false, [], _R_POOL_ADMIN, []));
  ("post_remote_db_access", (Post, Constants.remote_db_access_uri, false, [], _R_POOL_ADMIN, []));
  ("post_remote_db_access_v2", (Post, Constants.remote_db_access_uri_v2, false, [], _R_POOL_ADMIN, []));
  ("connect_migrate", (Connect, Constants.migrate_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("get_services_xenops", (Get, Constants.xenops_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("post_services_xenops", (Post, Constants.xenops_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("put_services_xenops", (Put, Constants.xenops_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("get_services_sm", (Get, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("post_services_sm", (Post, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("put_services_sm", (Put, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("put_import", (Put, Constants.import_uri, true,
                  [Bool_query_arg "restore"; Bool_query_arg "force"; String_query_arg "sr_id"], _R_VM_ADMIN, []));
  ("put_import_metadata", (Put, Constants.import_metadata_uri, true,
                           [Bool_query_arg "restore"; Bool_query_arg "force"], _R_VM_ADMIN, []));
  ("put_import_raw_vdi", (Put, Constants.import_raw_vdi_uri, true, [String_query_arg "vdi"], _R_VM_ADMIN, []));
  ("get_export", (Get, Constants.export_uri, true, [String_query_arg "uuid"], _R_VM_ADMIN, []));
  ("get_export_metadata", (Get, Constants.export_metadata_uri, true, [String_query_arg "uuid"], _R_VM_ADMIN, []));
  ("get_export_raw_vdi", (Get, Constants.export_raw_vdi_uri, true, [String_query_arg "vdi"], _R_VM_ADMIN, []));
  ("connect_console", (Connect, Constants.console_uri, false, [], _R_VM_OP,
                       [("host_console", _R_POOL_ADMIN)])); (* only _R_POOL_ADMIN can access the host/Dom0 console *)
  ("connect_console_ws", (Get, Constants.console_uri, false, [], _R_VM_OP,
                          [("host_console_ws", _R_POOL_ADMIN)])); (* only _R_POOL_ADMIN can access the host/Dom0 console *)
  ("get_root", (Get, "/", false, [], _R_READ_ONLY, []));
  ("post_cli", (Post, Constants.cli_uri, false, [], _R_READ_ONLY, []));
  ("get_host_backup", (Get, Constants.host_backup_uri, true, [], _R_POOL_ADMIN, []));
  ("put_host_restore", (Put, Constants.host_restore_uri, true, [], _R_POOL_ADMIN, []));
  ("get_host_logs_download", (Get, Constants.host_logs_download_uri, true, [], _R_POOL_OP, []));
  ("put_pool_patch_upload", (Put, Constants.pool_patch_upload_uri, true, [], _R_POOL_OP, []));
  ("get_pool_patch_download", (Get, Constants.pool_patch_download_uri, true, [String_query_arg "uuid"], _R_POOL_OP, []));
  ("put_oem_patch_stream", (Put, Constants.oem_patch_stream_uri, true, [], _R_POOL_OP, []));
  ("get_vncsnapshot", (Get, Constants.vncsnapshot_uri, true, [String_query_arg "uuid"], _R_VM_OP,
                       [("host_console", _R_POOL_ADMIN)])); (* only _R_POOL_ADMIN can snapshot host/Dom0 console *)
  ("get_pool_xml_db_sync", (Get, Constants.pool_xml_db_sync, true, [], _R_POOL_ADMIN, []));
  ("put_pool_xml_db_sync", (Put, Constants.pool_xml_db_sync, false, [], _R_POOL_ADMIN, []));
  ("get_config_sync", (Get, Constants.config_sync_uri, false, [], _R_POOL_ADMIN, []));
  ("get_vm_connect", (Get, Constants.vm_connect_uri, false, [], _R_POOL_ADMIN, []));
  ("put_vm_connect", (Put, Constants.vm_connect_uri, false, [], _R_POOL_ADMIN, []));
  ("get_system_status", (Get, Constants.system_status_uri, true,
                         [String_query_arg "entries"; String_query_arg "output"], _R_POOL_OP, []));
  (Constants.get_vm_rrd, (Get, Constants.get_vm_rrd_uri, true, [String_query_arg "uuid"], _R_READ_ONLY, []));
  (Constants.get_host_rrd, (Get, Constants.get_host_rrd_uri, true, [Bool_query_arg "json"], _R_READ_ONLY, []));
  (Constants.get_sr_rrd, (Get, Constants.get_sr_rrd_uri, true, [String_query_arg "uuid"], _R_READ_ONLY, []));
  (Constants.get_rrd_updates, (Get, Constants.get_rrd_updates_uri, true,
                               [Int64_query_arg "start"; String_query_arg "cf"; Int64_query_arg "interval";
                                Bool_query_arg "host"; String_query_arg "uuid"; Bool_query_arg "json"], _R_READ_ONLY, []));
  (Constants.put_rrd, (Put, Constants.put_rrd_uri, false, [], _R_POOL_ADMIN, []));
  ("get_blob", (Get, Constants.blob_uri, false, [], _R_READ_ONLY, []));
  ("put_blob", (Put, Constants.blob_uri, true, [String_query_arg "ref"], _R_VM_POWER_ADMIN, []));
  ("get_message_rss_feed", (Get, Constants.message_rss_feed, false, [], _R_POOL_ADMIN, []));  (* not enabled in xapi *)
  ("put_messages", (Put, Constants.message_put_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("connect_remotecmd", (Connect, Constants.remotecmd_uri, false, [], _R_POOL_ADMIN, []));
  ("get_wlb_report", (Get, Constants.wlb_report_uri, true,
                      [String_query_arg "report"; Varargs_query_arg], _R_READ_ONLY, []));
  ("get_wlb_diagnostics", (Get, Constants.wlb_diagnostics_uri, true, [], _R_READ_ONLY, []));
  ("get_audit_log", (Get, Constants.audit_log_uri, true, [], _R_READ_ONLY, []));

  (* XMLRPC callback *)
  ("post_root", (Post, "/", false, [], _R_READ_ONLY, []));
  (* JSON callback *)
  ("post_json", (Post, Constants.json_uri, false, [], _R_READ_ONLY, []));
  ("post_root_options", (Options, "/", false, [], _R_READ_ONLY, []));
  ("post_json_options", (Options, Constants.json_uri, false, [], _R_READ_ONLY, []));
  ("post_jsonrpc", (Post, Constants.jsonrpc_uri, false, [], _R_READ_ONLY, []));
  ("post_jsonrpc_options", (Options, Constants.jsonrpc_uri, false, [], _R_READ_ONLY, []));
  ("get_pool_update_download", (Get, Constants.get_pool_update_download_uri, false, [], _R_READ_ONLY, []));
]

(* these public http actions will NOT be checked by RBAC *)
(* they are meant to be used in exceptional cases where RBAC is already *)
(* checked inside them, such as in the XMLRPC (API) calls *)
let public_http_actions_with_no_rbac_check =
  [
    "post_root"; (* XMLRPC (API) calls -> checks RBAC internally *)
    "post_cli";  (* CLI commands -> calls XMLRPC *)
    "post_json"; (* JSON -> calls XMLRPC *)
    "get_root";  (* Make sure that downloads, personal web pages etc do not go through RBAC asking for a password or session_id *)
    (* also, without this line, quicktest_http.ml fails on non_resource_cmd and bad_resource_cmd with a 401 instead of 404 *)
    "get_blob"; (* Public blobs don't need authentication *)
    "post_root_options"; (* Preflight-requests are not RBAC checked *)
    "post_json_options";
    "post_jsonrpc";
    "post_jsonrpc_options";
    "get_pool_update_download";
  ]

(* permissions not associated with any object message or field *)
let extra_permissions = [
  (Task.extra_permission_task_destroy_any, _R_POOL_OP); (* only POOL_OP can destroy any tasks *)
]
