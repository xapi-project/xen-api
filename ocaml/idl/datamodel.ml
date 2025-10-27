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
open Lifecycle
open Datamodel_common
open Datamodel_roles

let errors = Datamodel_errors.errors

let messages = Datamodel_errors.messages

let roles_all = roles_all

let api_version_major = Datamodel_common.api_version_major

let api_version_minor = Datamodel_common.api_version_minor

module Session = struct
  let login =
    call ~flags:[] ~name:"login_with_password"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Attempt to authenticate the user, returning a session reference \
             if successful"
          )
        ]
      ~doc:
        "Attempt to authenticate the user, returning a session reference if \
         successful"
      ~result:(Ref _session, "reference of newly created session")
      ~versioned_params:
        [
          {
            param_type= String
          ; param_name= "uname"
          ; param_doc= "Username for login."
          ; param_release= rio_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "pwd"
          ; param_doc= "Password for login."
          ; param_release= rio_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "version"
          ; param_doc= "Client API version."
          ; param_release= miami_release
          ; param_default= Some (VString "1.1")
          }
        ; {
            param_type= String
          ; param_name= "originator"
          ; param_doc=
              "Key string for distinguishing different API users sharing the \
               same login name."
          ; param_release= clearwater_release
          ; param_default= Some (VString "")
          }
        ]
      ~errs:[Api_errors.session_authentication_failed; Api_errors.host_is_slave]
      ~secret:true
      ~allowed_roles:_R_ALL (*any static role can try to create a user session*)
      ()

  let slave_login =
    call ~flags:[] ~name:"slave_login"
      ~doc:
        "Attempt to authenticate to the pool master by presenting the slave's \
         host ref and pool secret"
      ~result:(Ref _session, "ID of newly created session")
      ~params:
        [
          (Ref _host, "host", "Host id of slave")
        ; (SecretString, "psecret", "Pool secret")
        ]
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Attempt to authenticate to the pool master by presenting the \
             slave's host ref and pool secret"
          )
        ]
      ~secret:true ~hide_from_docs:true ~allowed_roles:_R_POOL_ADMIN
      (*system can create a slave session !!! *) ()

  let slave_local_login =
    call ~flags:[]
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Authenticate locally against a slave in emergency mode. Note the \
             resulting sessions are only good for use on this host."
          )
        ]
      ~name:"slave_local_login"
      ~doc:
        "Authenticate locally against a slave in emergency mode. Note the \
         resulting sessions are only good for use on this host."
      ~result:(Ref _session, "ID of newly created session")
      ~params:[(SecretString, "psecret", "Pool secret")]
      ~in_oss_since:None ~secret:true ~hide_from_docs:true
      ~allowed_roles:_R_POOL_ADMIN (*system can create a slave session*) ()

  let slave_local_login_with_password =
    call ~flags:[]
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Authenticate locally against a slave in emergency mode. Note the \
             resulting sessions are only good for use on this host."
          )
        ]
      ~name:"slave_local_login_with_password"
      ~doc:
        "Authenticate locally against a slave in emergency mode. Note the \
         resulting sessions are only good for use on this host."
      ~result:(Ref _session, "ID of newly created session")
      ~params:
        [
          (String, "uname", "Username for login.")
        ; (String, "pwd", "Password for login.")
        ]
      ~in_oss_since:None ~secret:true
      ~allowed_roles:_R_POOL_ADMIN (*only root can do an emergency slave login*)
      ()

  let create_from_db_file =
    call
      ~lifecycle:[(Published, rel_dundee, "")]
      ~name:"create_from_db_file"
      ~params:[(String, "filename", "Database dump filename.")]
      ~result:(Ref _session, "ID of newly created session")
      ~in_oss_since:None ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let local_logout =
    call ~flags:[`Session]
      ~lifecycle:[(Published, rel_miami, "Log out of local session.")]
      ~name:"local_logout" ~doc:"Log out of local session." ~params:[]
      ~in_oss_since:None ~allowed_roles:_R_POOL_ADMIN
      (*system can destroy a local session*) ()

  let logout =
    call ~flags:[`Session]
      ~lifecycle:[(Published, rel_rio, "Log out of a session")]
      ~name:"logout" ~doc:"Log out of a session" ~params:[]
      ~allowed_roles:_R_ALL (*any role can destroy a known user session*) ()

  let change_password =
    call ~flags:[`Session] ~name:"change_password"
      ~doc:
        "Change the account password; if your session is authenticated with \
         root privileges then the old_pwd is validated and the new_pwd is set \
         regardless"
      ~params:
        [
          (String, "old_pwd", "Old password for account")
        ; (String, "new_pwd", "New password for account")
        ]
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Change the account password; if your session is authenticated \
             with root privileges then the old_pwd is validated and the \
             new_pwd is set regardless"
          )
        ]
      ~in_oss_since:None ~allowed_roles:_R_LOCAL_ROOT_ONLY
      (*not even pool-admin can change passwords, only root*) ()

  let get_all_subject_identifiers =
    call ~name:"get_all_subject_identifiers"
      ~doc:
        "Return a list of all the user subject-identifiers of all existing \
         sessions"
      ~result:
        ( Set String
        , "The list of user subject-identifiers of all existing sessions"
        )
      ~params:[]
      ~lifecycle:
        [
          ( Published
          , rel_george
          , "Return a list of all the user subject-identifiers of all existing \
             sessions"
          )
        ]
      ~in_oss_since:None ~allowed_roles:_R_ALL ()

  let logout_subject_identifier =
    call ~name:"logout_subject_identifier"
      ~doc:
        "Log out all sessions associated to a user subject-identifier, except \
         the session associated with the context calling this function"
      ~params:
        [
          ( String
          , "subject_identifier"
          , "User subject-identifier of the sessions to be destroyed"
          )
        ]
      ~lifecycle:
        [
          ( Published
          , rel_george
          , "Log out all sessions associated to a user subject-identifier, \
             except the session associated with the context calling this \
             function"
          )
        ]
      ~in_oss_since:None ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A session")]
      ~in_oss_since:oss_since_303 ~persist:PersistNothing
      ~gen_constructor_destructor:false ~name:_session ~descr:"A session"
      ~gen_events:false ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages:
        [
          login
        ; logout
        ; change_password
        ; slave_login
        ; slave_local_login
        ; slave_local_login_with_password
        ; create_from_db_file
        ; local_logout
        ; get_all_subject_identifiers
        ; logout_subject_identifier
        ]
      ~contents:
        [
          uid _session
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
        ; field ~qualifier:DynamicRO ~ty:(Ref _host)
            ~lifecycle:[(Published, rel_rio, "Currently connected host")]
            "this_host" "Currently connected host"
        ; field ~qualifier:DynamicRO ~ty:(Ref _user)
            ~lifecycle:[(Published, rel_rio, "Currently connected user")]
            "this_user" "Currently connected user"
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Timestamp for last time session was active"
                )
              ]
            "last_active" "Timestamp for last time session was active"
        ; field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "True if this session relates to a intra-pool login, false \
                   otherwise"
                )
              ]
            "pool"
            "True if this session relates to a intra-pool login, false \
             otherwise"
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_george
                , "true iff this session was created using local superuser \
                   credentials"
                )
              ]
            ~qualifier:DynamicRO ~default_value:(Some (VBool false)) ~ty:Bool
            "is_local_superuser"
            "true iff this session was created using local superuser \
             credentials"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_george
                , "references the subject instance that created the session. \
                   If a session instance has is_local_superuser set, then the \
                   value of this field is undefined."
                )
              ]
            ~qualifier:DynamicRO ~default_value:(Some (VRef null_ref))
            ~ty:(Ref _subject) "subject"
            "references the subject instance that created the session. If a \
             session instance has is_local_superuser set, then the value of \
             this field is undefined."
        ; field
            ~lifecycle:
              [(Published, rel_george, "time when session was last validated")]
            ~qualifier:DynamicRO ~default_value:(Some (VDateTime Date.epoch))
            ~ty:DateTime "validation_time"
            "time when session was last validated"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_george
                , "the subject identifier of the user that was externally \
                   authenticated. If a session instance has is_local_superuser \
                   set, then the value of this field is undefined."
                )
              ]
            ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String
            "auth_user_sid"
            "the subject identifier of the user that was externally \
             authenticated. If a session instance has is_local_superuser set, \
             then the value of this field is undefined."
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "the subject name of the user that was externally \
                   authenticated. If a session instance has is_local_superuser \
                   set, then the value of this field is undefined."
                )
              ]
            ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String
            "auth_user_name"
            "the subject name of the user that was externally authenticated. \
             If a session instance has is_local_superuser set, then the value \
             of this field is undefined."
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "list with all RBAC permissions for this session"
                )
              ]
            ~qualifier:StaticRO ~default_value:(Some (VSet [])) ~ty:(Set String)
            "rbac_permissions" "list with all RBAC permissions for this session"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "list of tasks created using the current session"
                )
              ]
            ~qualifier:DynamicRO ~ty:(Set (Ref _task)) "tasks"
            "list of tasks created using the current session"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "references the parent session that created this session"
                )
              ]
            ~qualifier:StaticRO ~default_value:(Some (VRef null_ref))
            ~ty:(Ref _session) "parent"
            "references the parent session that created this session"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_clearwater
                , "a key string provided by a API user to distinguish itself \
                   from other users sharing the same login name"
                )
              ]
            ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String
            "originator"
            "a key string provided by a API user to distinguish itself from \
             other users sharing the same login name"
        ; field
            ~lifecycle:
              [
                ( Published
                , "21.2.0"
                , "indicates whether this session was authenticated using a \
                   client certificate"
                )
              ]
            ~qualifier:DynamicRO ~default_value:(Some (VBool false)) ~ty:Bool
            "client_certificate"
            "indicates whether this session was authenticated using a client \
             certificate"
        ]
      ()
end

module Task = struct
  (* NB: the status 'cancelling' is not being used, nor should it ever be used. It should be purged from here! *)
  let status_type =
    Enum
      ( "task_status_type"
      , [
          ("pending", "task is in progress")
        ; ("success", "task was completed successfully")
        ; ("failure", "task has failed")
        ; ("cancelling", "task is being cancelled")
        ; ("cancelled", "task has been cancelled")
        ]
      )

  let cancel =
    call ~name:"cancel"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Request that a task be cancelled. Note that a task may fail to be \
             cancelled and may complete or fail normally and note that, even \
             when a task does cancel, it might take an arbitrary amount of \
             time."
          )
        ]
      ~doc:
        "Request that a task be cancelled. Note that a task may fail to be \
         cancelled and may complete or fail normally and note that, even when \
         a task does cancel, it might take an arbitrary amount of time."
      ~params:[(Ref _task, "task", "The task")]
      ~errs:[Api_errors.operation_not_allowed]
      ~allowed_roles:_R_READ_ONLY
      (* POOL_OP can cancel any tasks, others can cancel only owned tasks *)
      ()

  let create =
    call ~flags:[`Session] ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Create a new task object which must be manually destroyed."
          )
        ]
      ~name:"create"
      ~doc:"Create a new task object which must be manually destroyed."
      ~params:
        [
          (String, "label", "short label for the new task")
        ; (String, "description", "longer description for the new task")
        ]
      ~result:(Ref _task, "The reference of the created task object")
      ~allowed_roles:_R_READ_ONLY (* any subject can create tasks *) ()

  let destroy =
    call ~flags:[`Session] ~in_oss_since:None
      ~lifecycle:[(Published, rel_rio, "Destroy the task object")]
      ~name:"destroy" ~doc:"Destroy the task object"
      ~params:[(Ref _task, "self", "Reference to the task object")]
      ~allowed_roles:_R_READ_ONLY
      (* POOL_OP can destroy any tasks, others can destroy only owned tasks *)
      ()

  let set_status =
    call ~flags:[`Session] ~in_oss_since:None
      ~lifecycle:[(Published, rel_falcon, "Set the task status")]
      ~name:"set_status" ~doc:"Set the task status"
      ~params:
        [
          (Ref _task, "self", "Reference to the task object")
        ; (status_type, "value", "task status value to be set")
        ]
      ~allowed_roles:_R_READ_ONLY
      (* POOL_OP can set status for any tasks, others can set status only for owned tasks *)
      ()

  let set_progress =
    call ~flags:[`Session] ~in_oss_since:None
      ~lifecycle:[(Published, rel_stockholm, "Set the task progress")]
      ~name:"set_progress" ~doc:"Set the task progress"
      ~params:
        [
          (Ref _task, "self", "Reference to the task object")
        ; (Float, "value", "Task progress value to be set")
        ]
      ~allowed_roles:_R_READ_ONLY
      (* POOL_OP can set status for any tasks, others can set status only for owned tasks *)
      ()

  let set_result =
    call ~flags:[`Session] ~in_oss_since:None
      ~lifecycle:[(Published, "21.3.0", "")]
      ~name:"set_result" ~doc:"Set the task result"
      ~params:
        [
          (Ref _task, "self", "Reference to the task object")
        ; (String, "value", "Task result to be set")
        ]
      ~allowed_roles:_R_READ_ONLY
      (* POOL_OP can set result for any tasks, others can set result only for owned tasks *)
      ()

  let set_error_info =
    call ~flags:[`Session] ~in_oss_since:None
      ~lifecycle:[(Published, "21.3.0", "")]
      ~name:"set_error_info" ~doc:"Set the task error info"
      ~params:
        [
          (Ref _task, "self", "Reference to the task object")
        ; (Set String, "value", "Task error info to be set")
        ]
      ~allowed_roles:_R_READ_ONLY
      (* POOL_OP can set error_info for any tasks, others can set error_info only for owned tasks *)
      ()

  let set_resident_on =
    call ~flags:[`Session] ~in_oss_since:None
      ~lifecycle:[(Published, "21.3.0", "")]
      ~name:"set_resident_on" ~doc:"Set the resident on field"
      ~params:
        [
          (Ref _task, "self", "Reference to the task object")
        ; (Ref _host, "value", "Resident on to be set")
        ]
      ~allowed_roles:_R_READ_ONLY
      (* POOL_OP can set resident_on for any tasks, others can set error_info only for owned tasks *)
      ()

  (* this permission allows to destroy any task, instead of only the owned ones *)
  let extra_permission_task_destroy_any = "task.destroy/any"

  let task_allowed_operations =
    Enum ("task_allowed_operations", List.map operation_enum [cancel; destroy])

  module Special = struct
    (* These keys are usually ascribed to the field directly but,
       since we are providing custom implementations, we ascribe them
       to the messages themselves.

       Note that only the "add_to" and "remove_from" messages are
       protected by these keys. This is because the current RBAC logic
       is special cased to those messages. The "set_other_config"
       message has a relaxed RBAC restriction by comparison, and its
       checking logic is defined in terms of the permissions created
       for the "add_to" and "remove_from" operations.

       The difference is subtle: if a session attempts to perform
       "add_to"/"remove_from" upon "other_config", those operations
       are purely destructive and RBAC checking can be done by the
       current logic in Rbac.check (which guards the action). However,
       in the case of "set_other_config", we relax the restriction and
       must do the RBAC checking ourselves. The relaxed restriction is
       that the call may maintain entries that it cannot change
       itself. This means a read-only user can technically supply a
       map containing privileged entries, so long as those entries are
       already present. This allows read-only users to update a subset
       of the entries within "other_config".
    *)
    let protected_keys =
      [
        ("applies_to", _R_VM_OP)
      ; ("XenCenterUUID", _R_VM_OP)
      ; ("XenCenterMeddlingActionTitle", _R_VM_OP)
      ]

    let call = call ~lifecycle:[] ~errs:[] ~allowed_roles:_R_READ_ONLY

    let add_to_other_config =
      call ~name:"add_to_other_config"
        ~doc:
          "Add the given key-value pair to the other_config field of the given \
           task."
        ~params:
          [
            (Ref _task, "self", "Task object to modify")
          ; (String, "key", "Key to add")
          ; (String, "value", "Value to add")
          ]
        ~map_keys_roles:protected_keys ()

    let remove_from_other_config =
      call ~name:"remove_from_other_config"
        ~doc:
          "Remove the given key and its corresponding value from the \
           other_config field of the given task. If the key is not in that \
           Map, then do nothing."
        ~params:
          [
            (Ref _task, "self", "Task object to modify")
          ; (String, "key", "Key of entry to remove")
          ]
          (* Privileged key permissions are generated for each of these protected keys. *)
        ~map_keys_roles:protected_keys ()

    (* We cannot cite the protected keys here as the current RBAC
       logic only works for "add_to" and "remove_from" and, even if it
       did, it is too strict. *)
    let set_other_config =
      call ~name:"set_other_config"
        ~doc:"Set the other_config field of the given task."
        ~params:
          [
            (Ref _task, "self", "Task object to modify")
          ; (Map (String, String), "value", "New value to set")
          ]
        ()
  end

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A long-running asynchronous task")]
      ~in_oss_since:oss_since_303 ~persist:PersistNothing
      ~gen_constructor_destructor:false ~name:_task
      ~descr:"A long-running asynchronous task" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [
          create
        ; destroy
        ; cancel
        ; set_status
        ; set_progress
        ; set_result
        ; set_error_info
        ; Special.add_to_other_config
        ; Special.remove_from_other_config
        ; Special.set_other_config
        ]
      ~contents:
        ([
           uid _task
             ~lifecycle:
               [(Published, rel_rio, "Unique identifier/object reference")]
         ; namespace ~name:"name"
             ~contents:
               (names
                  ~lifecycle:[(Published, rel_rio, "")]
                  oss_since_303 DynamicRO
               )
             ()
         ]
        @ allowed_and_current_operations task_allowed_operations
        @ [
            field ~qualifier:DynamicRO ~ty:DateTime
              ~lifecycle:[(Published, rel_rio, "Time task was created")]
              "created" "Time task was created"
          ; field ~qualifier:DynamicRO ~ty:DateTime
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "Time task finished (i.e. succeeded or failed). If \
                     task-status is pending, then the value of this field has \
                     no meaning"
                  )
                ]
              "finished"
              "Time task finished (i.e. succeeded or failed). If task-status \
               is pending, then the value of this field has no meaning"
          ; field ~qualifier:DynamicRO ~ty:status_type
              ~lifecycle:[(Published, rel_rio, "current status of the task")]
              "status" "current status of the task"
          ; field ~in_oss_since:None
              ~lifecycle:
                [(Published, rel_rio, "the session that created the task")]
              ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _session)
              "session" "the session that created the task"
          ; field ~qualifier:DynamicRO ~ty:(Ref _host)
              ~lifecycle:
                [(Published, rel_rio, "the host on which the task is running")]
              "resident_on" "the host on which the task is running"
          ; field ~qualifier:DynamicRO ~ty:Float
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "This field contains the estimated fraction of the task \
                     which is complete. This field should not be used to \
                     determine whether the task is complete - for this the \
                     status field of the task should be used."
                  )
                ]
              "progress"
              "This field contains the estimated fraction of the task which is \
               complete. This field should not be used to determine whether \
               the task is complete - for this the status field of the task \
               should be used."
          ; field ~in_oss_since:None
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "If the task has spawned a program, the field record the \
                     PID of the process that the task is waiting on. (-1 if no \
                     waiting completion of an external program )"
                  )
                ]
              ~internal_only:true ~qualifier:DynamicRO ~ty:Int "externalpid"
              "If the task has spawned a program, the field record the PID of \
               the process that the task is waiting on. (-1 if no waiting \
               completion of an external program )"
          ; field ~in_oss_since:None
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "If the task has been forwarded, this field records the \
                     pid of the stunnel process spawned to manage the \
                     forwarding connection"
                  )
                ; (Deprecated, rel_boston, "")
                ]
              ~internal_only:true ~qualifier:DynamicRO ~ty:Int "stunnelpid"
              "If the task has been forwarded, this field records the pid of \
               the stunnel process spawned to manage the forwarding connection"
          ; field ~in_oss_since:None
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "True if this task has been forwarded to a slave"
                  )
                ]
              ~internal_only:true ~qualifier:DynamicRO ~ty:Bool "forwarded"
              "True if this task has been forwarded to a slave"
          ; field ~in_oss_since:None
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "The host to which the task has been forwarded"
                  )
                ]
              ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _host)
              "forwarded_to" "The host to which the task has been forwarded"
          ; field ~qualifier:DynamicRO ~ty:String
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "if the task has completed successfully, this field \
                     contains the type of the encoded result (i.e. name of the \
                     class whose reference is in the result field). Undefined \
                     otherwise."
                  )
                ]
              "type"
              "if the task has completed successfully, this field contains the \
               type of the encoded result (i.e. name of the class whose \
               reference is in the result field). Undefined otherwise."
          ; field ~qualifier:DynamicRO ~ty:String
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "if the task has completed successfully, this field \
                     contains the result value (either Void or an object \
                     reference). Undefined otherwise."
                  )
                ]
              "result"
              "if the task has completed successfully, this field contains the \
               result value (either Void or an object reference). Undefined \
               otherwise."
          ; field ~qualifier:DynamicRO ~ty:(Set String)
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "if the task has failed, this field contains the set of \
                     associated error strings. Undefined otherwise."
                  )
                ]
              "error_info"
              "if the task has failed, this field contains the set of \
               associated error strings. Undefined otherwise."
          ; field ~qualifier:DynamicRO
              ~lifecycle:[(Published, rel_miami, "additional configuration")]
              ~default_value:(Some (VMap []))
              ~ty:(Map (String, String))
              "other_config" "additional configuration"
              ~map_keys_roles:
                [
                  ("applies_to", _R_VM_OP)
                ; ("XenCenterUUID", _R_VM_OP)
                ; ("XenCenterMeddlingActionTitle", _R_VM_OP)
                ]
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "Ref pointing to the task this is a substask of."
                  )
                ]
              ~default_value:(Some (VRef "")) ~ty:(Ref _task) "subtask_of"
              "Ref pointing to the task this is a substask of."
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  (Published, rel_orlando, "List pointing to all the substasks.")
                ]
              ~ty:(Set (Ref _task)) "subtasks"
              "List pointing to all the substasks."
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [(Published, rel_dundee, "Function call trace for debugging.")]
              ~ty:String
              ~default_value:
                (Some
                   (VString (Sexplib0.Sexp.to_string Backtrace.(sexp_of_t empty))
                   )
                )
              "backtrace" "Function call trace for debugging."
          ]
        )
      ()
end

(** Many of the objects need to record IO bandwidth *)
let iobandwidth =
  let msg = "Disabled and replaced by RRDs" in
  [
    field ~persist:false ~qualifier:DynamicRO ~ty:Float
      ~default_value:(Some (VFloat 0.)) "read_kbs" "Read bandwidth (KiB/s)"
      ~lifecycle:
        [
          (Published, rel_rio, "")
        ; (Deprecated, rel_tampa, "Dummy transition")
        ; (Removed, rel_tampa, msg)
        ]
  ; field ~persist:false ~qualifier:DynamicRO ~ty:Float
      ~default_value:(Some (VFloat 0.)) "write_kbs" "Write bandwidth (KiB/s)"
      ~lifecycle:
        [
          (Published, rel_rio, "")
        ; (Deprecated, rel_tampa, "Dummy transition")
        ; (Removed, rel_tampa, msg)
        ]
  ]

(** Human users *)
module User = struct
  let t =
    (* DEPRECATED in favor of subject *)
    create_obj ~in_db:true ~in_oss_since:oss_since_303
      ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_user
      ~descr:"A user of the system" ~gen_events:false
      ~lifecycle:
        [
          (Published, rel_rio, "A user of the system")
        ; (Deprecated, rel_george, "Deprecated in favor of subject")
        ]
      ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_ADMIN ~messages:[]
      ~contents:
        [
          uid _user
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
        ; field ~qualifier:StaticRO
            ~lifecycle:[(Published, rel_rio, "short name (e.g. userid)")]
            "short_name" "short name (e.g. userid)"
        ; field
            ~lifecycle:[(Published, rel_rio, "full name")]
            "fullname" "full name"
        ; field
            ~lifecycle:[(Published, rel_orlando, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

(* Management of host crash dumps. Note that this would be neater if crashes were stored in
   VDIs like VM crashes, however the nature of a host crash dump is that the dom0 has crashed
   and has no access to any fancy storage drivers or tools. Plus a host is not guaranteed to
   have any SRs at all. *)

module Host_crashdump = struct
  let destroy =
    call ~name:"destroy"
      ~doc:"Destroy specified host crash dump, removing it from the disk."
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Destroy specified host crash dump, removing it from the disk."
          )
        ]
      ~params:[(Ref _host_crashdump, "self", "The host crashdump to destroy")]
      ~allowed_roles:_R_POOL_OP ()

  let upload =
    call ~name:"upload"
      ~doc:"Upload the specified host crash dump to a specified URL"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Upload the specified host crash dump to a specified URL"
          )
        ]
      ~params:
        [
          (Ref _host_crashdump, "self", "The host crashdump to upload")
        ; (String, "url", "The URL to upload to")
        ; (Map (String, String), "options", "Extra configuration operations")
        ]
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "Represents a host crash dump")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_host_crashdump ~gen_events:true
      ~descr:"Represents a host crash dump" ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~messages:[destroy; upload]
      ~contents:
        [
          uid ~in_oss_since:None
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _host_crashdump
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Host the crashdump relates to")]
            ~qualifier:StaticRO ~ty:(Ref _host) "host"
            "Host the crashdump relates to"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Time the crash happened")]
            ~qualifier:DynamicRO ~ty:DateTime "timestamp"
            "Time the crash happened"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Size of the crashdump")]
            ~qualifier:DynamicRO ~ty:Int "size" "Size of the crashdump"
        ; field ~qualifier:StaticRO ~ty:String ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "filename of crash dir")]
            ~internal_only:true "filename" "filename of crash dir"
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

(* New Ely pool update mechanism *)
module Pool_update = struct
  let livepatch_status =
    Enum
      ( "livepatch_status"
      , [
          ( "ok_livepatch_complete"
          , "An applicable live patch exists for every required component"
          )
        ; ( "ok_livepatch_incomplete"
          , "An applicable live patch exists but it is not sufficient"
          )
        ; ("ok", "There is no applicable live patch")
        ]
      )

  let after_apply_guidance =
    Enum
      ( "update_after_apply_guidance"
      , [
          ( "restartHVM"
          , "This update requires HVM guests to be restarted once applied."
          )
        ; ( "restartPV"
          , "This update requires PV guests to be restarted once applied."
          )
        ; ( "restartHost"
          , "This update requires the host to be restarted once applied."
          )
        ; ( "restartXAPI"
          , "This update requires XAPI to be restarted once applied."
          )
        ]
      )

  let introduce =
    call ~name:"introduce" ~doc:"Introduce update VDI" ~in_oss_since:None
      ~lifecycle:[(Published, rel_ely, "Introduce update VDI")]
      ~params:[(Ref _vdi, "vdi", "The VDI which contains a software update.")]
      ~result:(Ref _pool_update, "the introduced pool update")
      ~allowed_roles:_R_POOL_OP ()

  let precheck =
    call ~name:"precheck"
      ~doc:"Execute the precheck stage of the selected update on a host"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_ely
          , "Execute the precheck stage of the selected update on a host"
          )
        ]
      ~params:
        [
          (Ref _pool_update, "self", "The update whose prechecks will be run")
        ; (Ref _host, "host", "The host to run the prechecks on.")
        ]
      ~result:(livepatch_status, "The precheck pool update")
      ~allowed_roles:_R_POOL_OP
      ~forward_to:(HostExtension "pool_update.precheck") ()

  let apply =
    call ~name:"apply" ~doc:"Apply the selected update to a host"
      ~in_oss_since:None
      ~lifecycle:[(Published, rel_ely, "Apply the selected update to a host")]
      ~params:
        [
          (Ref _pool_update, "self", "The update to apply")
        ; (Ref _host, "host", "The host to apply the update to.")
        ]
      ~allowed_roles:_R_POOL_OP ~forward_to:(HostExtension "pool_update.apply")
      ()

  let pool_apply =
    call ~name:"pool_apply"
      ~doc:"Apply the selected update to all hosts in the pool"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_ely
          , "Apply the selected update to all hosts in the pool"
          )
        ]
      ~params:[(Ref _pool_update, "self", "The update to apply")]
      ~allowed_roles:_R_POOL_OP ()

  let pool_clean =
    call ~name:"pool_clean"
      ~doc:
        "Removes the update's files from all hosts in the pool, but does not \
         revert the update"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_ely
          , "Removes the update's files from all hosts in the pool, but does \
             not revert the update"
          )
        ]
      ~params:[(Ref _pool_update, "self", "The update to clean up")]
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~doc:"Removes the database entry. Only works on unapplied update."
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_ely
          , "Removes the database entry. Only works on unapplied update."
          )
        ]
      ~params:[(Ref _pool_update, "self", "The update to destroy")]
      ~allowed_roles:_R_POOL_OP ()

  let attach =
    call ~name:"attach" ~hide_from_docs:true ~doc:"Attach the pool update VDI"
      ~in_oss_since:None
      ~lifecycle:[(Published, rel_ely, "Attach the pool update VDI")]
      ~versioned_params:
        [
          {
            param_type= Ref _pool_update
          ; param_name= "self"
          ; param_doc= "The update to be attached"
          ; param_release= ely_release
          ; param_default= None
          }
        ; {
            param_type= Bool
          ; param_name= "use_localhost_proxy"
          ; param_doc= "Use the localhost proxy"
          ; param_release= naples_release
          ; param_default= Some (VBool false)
          }
        ]
      ~result:(String, "The file URL of pool update")
      ~allowed_roles:_R_POOL_OP ()

  let detach =
    call ~name:"detach" ~hide_from_docs:true ~doc:"Detach the pool update VDI"
      ~in_oss_since:None
      ~lifecycle:[(Published, rel_ely, "Detach the pool update VDI")]
      ~params:[(Ref _pool_update, "self", "The update to be detached")]
      ~allowed_roles:_R_POOL_OP ()

  let resync_host =
    call ~name:"resync_host" ~hide_from_docs:true
      ~doc:"Resync the applied updates of the host" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_ely, "Resync the applied updates of the host")]
      ~params:[(Ref _host, "host", "The host to resync the applied updates")]
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [(Published, rel_ely, "Pool-wide updates to the host software")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~gen_events:true ~name:_pool_update
      ~descr:"Pool-wide updates to the host software" ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [
          introduce
        ; precheck
        ; apply
        ; pool_apply
        ; pool_clean
        ; destroy
        ; attach
        ; detach
        ; resync_host
        ]
      ~contents:
        [
          uid ~in_oss_since:None
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _pool_update
        ; namespace ~name:"name"
            ~contents:(names None StaticRO ~lifecycle:[(Published, rel_rio, "")])
            ()
        ; field
            ~lifecycle:[(Published, rel_ely, "Update version number")]
            ~default_value:(Some (VString "")) ~in_oss_since:None
            ~qualifier:StaticRO ~ty:String "version" "Update version number"
        ; field
            ~lifecycle:[(Published, rel_ely, "Size of the update in bytes")]
            ~default_value:(Some (VInt Int64.zero)) ~in_oss_since:None
            ~qualifier:StaticRO ~ty:Int "installation_size"
            "Size of the update in bytes"
        ; field
            ~lifecycle:[(Published, rel_ely, "GPG key of the update")]
            ~default_value:(Some (VString "")) ~in_oss_since:None
            ~qualifier:StaticRO ~ty:String "key" "GPG key of the update"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_ely
                , "What the client should do after this update has been \
                   applied."
                )
              ]
            ~default_value:(Some (VSet [])) ~in_oss_since:None
            ~qualifier:StaticRO ~ty:(Set after_apply_guidance)
            "after_apply_guidance"
            "What the client should do after this update has been applied."
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "VDI the update was uploaded to")]
            ~qualifier:StaticRO ~ty:(Ref _vdi) "vdi"
            "VDI the update was uploaded to"
        ; field
            ~lifecycle:
              [(Published, rel_ely, "The hosts that have applied this update.")]
            ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host))
            "hosts" "The hosts that have applied this update."
        ; field
            ~lifecycle:[(Published, rel_inverness, "additional configuration")]
            ~default_value:(Some (VMap [])) ~in_oss_since:None
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_inverness
                , "Flag - if true, all hosts in a pool must apply this update"
                )
              ]
            ~default_value:(Some (VBool false)) ~in_oss_since:None
            ~qualifier:StaticRO ~ty:Bool "enforce_homogeneity"
            "Flag - if true, all hosts in a pool must apply this update"
        ]
      ()
end

(* New Miami pool patching mechanism *)

module Pool_patch = struct
  let after_apply_guidance =
    Enum
      ( "after_apply_guidance"
      , [
          ( "restartHVM"
          , "This patch requires HVM guests to be restarted once applied."
          )
        ; ( "restartPV"
          , "This patch requires PV guests to be restarted once applied."
          )
        ; ( "restartHost"
          , "This patch requires the host to be restarted once applied."
          )
        ; ( "restartXAPI"
          , "This patch requires XAPI to be restarted once applied."
          )
        ]
      )

  let apply =
    call ~name:"apply"
      ~doc:"Apply the selected patch to a host and return its output"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Apply the selected patch to a host and return its output"
          )
        ; (Deprecated, rel_ely, "")
        ]
      ~params:
        [
          (Ref _pool_patch, "self", "The patch to apply")
        ; (Ref _host, "host", "The host to apply the patch too")
        ]
      ~result:(String, "the output of the patch application process")
      ~allowed_roles:_R_POOL_OP ()

  let precheck =
    call ~name:"precheck"
      ~doc:
        "Execute the precheck stage of the selected patch on a host and return \
         its output"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Execute the precheck stage of the selected patch on a host and \
             return its output"
          )
        ; (Deprecated, rel_ely, "")
        ]
      ~params:
        [
          (Ref _pool_patch, "self", "The patch whose prechecks will be run")
        ; (Ref _host, "host", "The host to run the prechecks on")
        ]
      ~result:(String, "the output of the patch prechecks")
      ~allowed_roles:_R_POOL_OP ()

  let clean =
    call ~name:"clean" ~doc:"Removes the patch's files from the server"
      ~in_oss_since:None
      ~lifecycle:
        [
          (Published, rel_miami, "Removes the patch's files from the server")
        ; (Deprecated, rel_ely, "")
        ]
      ~params:[(Ref _pool_patch, "self", "The patch to clean up")]
      ~allowed_roles:_R_POOL_OP ()

  let clean_on_host =
    call ~name:"clean_on_host"
      ~doc:"Removes the patch's files from the specified host"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Removes the patch's files from the specified host"
          )
        ; (Deprecated, rel_ely, "")
        ]
      ~params:
        [
          (Ref _pool_patch, "self", "The patch to clean up")
        ; (Ref _host, "host", "The host on which to clean the patch")
        ]
      ~allowed_roles:_R_POOL_OP ()

  let pool_clean =
    call ~name:"pool_clean"
      ~doc:
        "Removes the patch's files from all hosts in the pool, but does not \
         remove the database entries"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Removes the patch's files from all hosts in the pool, but does \
             not remove the database entries"
          )
        ; (Deprecated, rel_ely, "")
        ]
      ~params:[(Ref _pool_patch, "self", "The patch to clean up")]
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~doc:
        "Removes the patch's files from all hosts in the pool, and removes the \
         database entries.  Only works on unapplied patches."
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Removes the patch's files from all hosts in the pool, and removes \
             the database entries.  Only works on unapplied patches."
          )
        ; (Deprecated, rel_ely, "")
        ]
      ~params:[(Ref _pool_patch, "self", "The patch to destroy")]
      ~allowed_roles:_R_POOL_OP ()

  let pool_apply =
    call ~name:"pool_apply"
      ~doc:
        "Apply the selected patch to all hosts in the pool and return a map of \
         host_ref -> patch output"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Apply the selected patch to all hosts in the pool and return a \
             map of host_ref -> patch output"
          )
        ; (Deprecated, rel_ely, "")
        ]
      ~params:[(Ref _pool_patch, "self", "The patch to apply")]
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [(Published, rel_miami, "Pool-wide patches"); (Deprecated, rel_ely, "")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~gen_events:true ~name:_pool_patch
      ~descr:"Pool-wide patches" ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [apply; pool_apply; precheck; clean; pool_clean; destroy; clean_on_host]
      ~contents:
        [
          uid ~in_oss_since:None
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _pool_patch
        ; namespace ~name:"name"
            ~contents:(names None StaticRO ~lifecycle:[(Published, rel_rio, "")])
            ()
        ; field
            ~lifecycle:[(Published, rel_miami, "Patch version number")]
            ~default_value:(Some (VString "")) ~in_oss_since:None
            ~qualifier:StaticRO ~ty:String "version" "Patch version number"
        ; field
            ~lifecycle:[(Published, rel_miami, "Filename of the patch")]
            ~default_value:(Some (VString "")) ~in_oss_since:None
            ~internal_only:true ~qualifier:DynamicRO ~ty:String "filename"
            "Filename of the patch"
        ; field
            ~lifecycle:[(Published, rel_miami, "Size of the patch")]
            ~default_value:(Some (VInt Int64.zero)) ~in_oss_since:None
            ~qualifier:DynamicRO ~ty:Int "size" "Size of the patch"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "This patch should be applied across the entire pool"
                )
              ]
            ~default_value:(Some (VBool false)) ~in_oss_since:None
            ~qualifier:DynamicRO ~ty:Bool "pool_applied"
            "This patch should be applied across the entire pool"
        ; field
            ~lifecycle:
              [(Published, rel_miami, "This hosts this patch is applied to.")]
            ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host_patch))
            "host_patches" "This hosts this patch is applied to."
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "What the client should do after this patch has been applied."
                )
              ]
            ~default_value:(Some (VSet [])) ~in_oss_since:None
            ~qualifier:DynamicRO ~ty:(Set after_apply_guidance)
            "after_apply_guidance"
            "What the client should do after this patch has been applied."
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_ely
                , "A reference to the associated pool_update object"
                )
              ]
            ~default_value:(Some (VRef null_ref)) ~in_oss_since:None
            ~qualifier:StaticRO ~ty:(Ref _pool_update) "pool_update"
            "A reference to the associated pool_update object"
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap [])) ~in_oss_since:None
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

(* Management of host patches. Just like the crash dumps it would be marginally neater if
   the patches were stored as VDIs. *)

module Host_patch = struct
  let destroy =
    call ~name:"destroy"
      ~doc:
        "Destroy the specified host patch, removing it from the disk. This \
         does NOT reverse the patch"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Destroy the specified host patch, removing it from the disk. This \
             does NOT reverse the patch"
          )
        ; (Deprecated, rel_miami, "")
        ]
      ~params:[(Ref _host_patch, "self", "The patch to destroy")]
      ~allowed_roles:_R_POOL_OP ()

  let apply =
    call ~name:"apply" ~doc:"Apply the selected patch and return its output"
      ~in_oss_since:None
      ~lifecycle:
        [
          (Published, rel_rio, "Apply the selected patch and return its output")
        ; (Deprecated, rel_miami, "")
        ]
      ~params:[(Ref _host_patch, "self", "The patch to apply")]
      ~result:(String, "the output of the patch application process")
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [
          (Published, rel_rio, "Represents a patch stored on a server")
        ; (Deprecated, rel_ely, "")
        ]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_host_patch ~gen_events:true
      ~descr:"Represents a patch stored on a server" ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~messages:[destroy; apply]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            ~in_oss_since:None _host_patch
        ; namespace ~name:"name"
            ~contents:(names None StaticRO ~lifecycle:[(Published, rel_rio, "")])
            ()
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Patch version number")]
            ~qualifier:StaticRO ~ty:String "version" "Patch version number"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Host the patch relates to")]
            ~qualifier:StaticRO ~ty:(Ref _host) "host"
            "Host the patch relates to"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Filename of the patch")]
            ~internal_only:true ~qualifier:DynamicRO ~ty:String "filename"
            "Filename of the patch"
        ; field ~in_oss_since:None
            ~lifecycle:
              [(Published, rel_rio, "True if the patch has been applied")]
            ~qualifier:DynamicRO ~ty:Bool "applied"
            "True if the patch has been applied"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Time the patch was applied")]
            ~qualifier:DynamicRO ~ty:DateTime "timestamp_applied"
            "Time the patch was applied"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Size of the patch")]
            ~qualifier:DynamicRO ~ty:Int "size" "Size of the patch"
        ; field
            ~lifecycle:[(Published, rel_miami, "The patch applied")]
            ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _pool_patch)
            ~default_value:(Some (VRef "")) "pool_patch" "The patch applied"
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap [])) ~in_oss_since:None
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

module Host_metrics = struct
  let host_metrics_memory =
    let field = field ~ty:Int in
    [
      field ~qualifier:DynamicRO
        ~lifecycle:[(Published, rel_rio, "Total host memory (bytes)")]
        "total" "Total host memory (bytes)" ~doc_tags:[Memory]
    ; field "free" "Free host memory (bytes)" ~default_value:(Some (VInt 0L))
        ~lifecycle:
          [
            (Published, rel_rio, "")
          ; (Deprecated, rel_midnight_ride, "Will be disabled in favour of RRD")
          ; (Removed, rel_tampa, "Disabled in favour of RRD")
          ]
        ~qualifier:DynamicRO ~doc_tags:[Memory]
    ]

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "The metrics associated with a host")]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_host_metrics
      ~descr:"The metrics associated with a host" ~gen_events:true
      ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_OP ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _host_metrics
        ; namespace ~name:"memory" ~contents:host_metrics_memory ()
        ; field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None "live"
            ~lifecycle:
              [(Published, rel_rio, "Pool master thinks this host is live")]
            "Pool master thinks this host is live"
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Time at which this information was last updated"
                )
              ]
            "last_updated" "Time at which this information was last updated"
        ; field
            ~lifecycle:[(Published, rel_orlando, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

(** HostCPU *)

module Host_cpu = struct
  let t =
    create_obj ~in_db:true ~in_oss_since:oss_since_303
      ~persist:PersistEverything ~gen_constructor_destructor:false
      ~name:_hostcpu ~descr:"A physical CPU" ~gen_events:true
      ~lifecycle:
        [
          (Published, rel_rio, "A physical CPU")
        ; ( Deprecated
          , rel_midnight_ride
          , "Deprecated in favour of the Host.cpu_info field"
          )
        ]
      ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_OP ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _hostcpu
        ; field ~qualifier:DynamicRO ~ty:(Ref _host)
            ~lifecycle:[(Published, rel_rio, "the host the CPU is in")]
            "host" "the host the CPU is in"
        ; field ~qualifier:DynamicRO ~ty:Int
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "the number of the physical CPU within the host"
                )
              ]
            "number" "the number of the physical CPU within the host"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "the vendor of the physical CPU")]
            "vendor" "the vendor of the physical CPU"
        ; field ~qualifier:DynamicRO ~ty:Int
            ~lifecycle:[(Published, rel_rio, "the speed of the physical CPU")]
            "speed" "the speed of the physical CPU"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:
              [(Published, rel_rio, "the model name of the physical CPU")]
            "modelname" "the model name of the physical CPU"
        ; field ~qualifier:DynamicRO ~ty:Int
            ~lifecycle:
              [(Published, rel_rio, "the family (number) of the physical CPU")]
            "family" "the family (number) of the physical CPU"
        ; field ~qualifier:DynamicRO ~ty:Int
            ~lifecycle:
              [(Published, rel_rio, "the model number of the physical CPU")]
            "model" "the model number of the physical CPU"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:
              [(Published, rel_rio, "the stepping of the physical CPU")]
            "stepping" "the stepping of the physical CPU"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "the flags of the physical CPU (a decoded version of the \
                   features field)"
                )
              ]
            "flags"
            "the flags of the physical CPU (a decoded version of the features \
             field)"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "the physical CPU feature bitmap")]
            "features" "the physical CPU feature bitmap"
        ; field ~qualifier:DynamicRO ~persist:false ~ty:Float
            ~lifecycle:[(Published, rel_rio, "the current CPU utilisation")]
            "utilisation" "the current CPU utilisation"
        ; field
            ~lifecycle:[(Published, rel_orlando, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

(** Disk and network interfaces are associated with QoS parameters: *)
let qos devtype =
  [
    field
      ~lifecycle:[(Published, rel_rio, "QoS algorithm to use")]
      "algorithm_type" "QoS algorithm to use"
  ; field
      ~ty:(Map (String, String))
      ~lifecycle:[(Published, rel_rio, "parameters for chosen QoS algorithm")]
      "algorithm_params" "parameters for chosen QoS algorithm"
  ; field ~qualifier:DynamicRO ~ty:(Set String)
      ~lifecycle:
        [(Published, rel_rio, "supported QoS algorithms for this " ^ devtype)]
      "supported_algorithms"
      ("supported QoS algorithms for this " ^ devtype)
  ]

module Network = struct
  let operations =
    Enum
      ( "network_operations"
      , [("attaching", "Indicates this network is attaching to a VIF or PIF")]
      )

  let default_locking_mode =
    Enum
      ( "network_default_locking_mode"
      , [
          ( "unlocked"
          , "Treat all VIFs on this network with locking_mode = 'default' as \
             if they have locking_mode = 'unlocked'"
          )
        ; ( "disabled"
          , "Treat all VIFs on this network with locking_mode = 'default' as \
             if they have locking_mode = 'disabled'"
          )
        ]
      )

  let attach =
    call ~name:"attach"
      ~doc:"Makes the network immediately available on a particular host"
      ~params:
        [
          ( Ref _network
          , "network"
          , "network to which this interface should be connected"
          )
        ; (Ref _host, "host", "physical machine to which this PIF is connected")
        ]
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Makes the network immediately available on a particular host"
          )
        ]
      ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

  let purpose =
    Enum
      ( "network_purpose"
      , [
          ("nbd", "Network Block Device service using TLS")
        ; ( "insecure_nbd"
          , "Network Block Device service without integrity or \
             confidentiality: NOT RECOMMENDED"
          )
          (* We should (re-)add other purposes as and when we write code with behaviour that depends on them,
           * e.g. management, storage, guest, himn... unmanaged? *)
        ]
      )

  let introduce_params first_rel =
    [
      {
        param_type= String
      ; param_name= "name_label"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "name_description"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Int
      ; param_name= "MTU"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Map (String, String)
      ; param_name= "other_config"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "bridge"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Bool
      ; param_name= "managed"
      ; param_doc= ""
      ; param_release= falcon_release
      ; param_default= None
      }
    ; {
        param_type= Set purpose
      ; param_name= "purpose"
      ; param_doc= ""
      ; param_release= inverness_release
      ; param_default= None
      }
    ]

  (* network pool introduce is used to copy network records on pool join -- it's the network analogue of VDI/PIF.pool_introduce *)
  let pool_introduce =
    call ~name:"pool_introduce" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Create a new network record in the database only"
          )
        ]
      ~versioned_params:(introduce_params miami_release)
      ~doc:"Create a new network record in the database only"
      ~result:(Ref _network, "The ref of the newly created network record.")
      ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

  let create_new_blob =
    call ~name:"create_new_blob"
      ~lifecycle:
        [
          ( Published
          , rel_orlando
          , "Create a placeholder for a named binary blob of data that is \
             associated with this pool"
          )
        ]
      ~doc:
        "Create a placeholder for a named binary blob of data that is \
         associated with this pool"
      ~versioned_params:
        [
          {
            param_type= Ref _network
          ; param_name= "network"
          ; param_doc= "The network"
          ; param_release= orlando_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "name"
          ; param_doc= "The name associated with the blob"
          ; param_release= orlando_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "mime_type"
          ; param_doc=
              "The mime type for the data. Empty string translates to \
               application/octet-stream"
          ; param_release= orlando_release
          ; param_default= None
          }
        ; {
            param_type= Bool
          ; param_name= "public"
          ; param_doc= "True if the blob should be publicly available"
          ; param_release= tampa_release
          ; param_default= Some (VBool false)
          }
        ]
      ~result:
        (Ref _blob, "The reference of the blob, needed for populating its data")
      ~allowed_roles:_R_POOL_OP ()

  let set_default_locking_mode =
    call ~name:"set_default_locking_mode"
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Set the default locking mode for VIFs attached to this network"
          )
        ]
      ~doc:"Set the default locking mode for VIFs attached to this network"
      ~params:
        [
          (Ref _network, "network", "The network")
        ; ( default_locking_mode
          , "value"
          , "The default locking mode for VIFs attached to this network."
          )
        ]
      ~allowed_roles:_R_POOL_OP ()

  let attach_for_vm =
    call ~name:"attach_for_vm"
      ~doc:"Attaches all networks needed by a given VM on a particular host"
      ~params:
        [
          ( Ref _host
          , "host"
          , "Physical machine to which the networks are to be attached"
          )
        ; (Ref _vm, "vm", "The virtual machine")
        ]
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Attaches all networks needed by a given VM on a particular host"
          )
        ]
      ~hide_from_docs:true ~allowed_roles:_R_VM_POWER_ADMIN ()

  let detach_for_vm =
    call ~name:"detach_for_vm"
      ~doc:"Detaches all networks of a given VM from a particular host"
      ~params:
        [
          ( Ref _host
          , "host"
          , "Physical machine from which the networks are to be attached"
          )
        ; (Ref _vm, "vm", "The virtual machine")
        ]
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Detaches all networks of a given VM from a particular host"
          )
        ]
      ~hide_from_docs:true ~allowed_roles:_R_VM_POWER_ADMIN ()

  let add_purpose =
    call ~name:"add_purpose"
      ~doc:"Give a network a new purpose (if not present already)"
      ~params:
        [
          (Ref _network, "self", "The network")
        ; (purpose, "value", "The purpose to add")
        ]
      ~errs:[Api_errors.network_incompatible_purposes]
      ~lifecycle:
        [
          ( Published
          , rel_inverness
          , "Give a network a new purpose (if not present already)"
          )
        ]
      ~allowed_roles:_R_POOL_ADMIN ()

  let remove_purpose =
    call ~name:"remove_purpose"
      ~doc:"Remove a purpose from a network (if present)"
      ~params:
        [
          (Ref _network, "self", "The network")
        ; (purpose, "value", "The purpose to remove")
        ]
      ~lifecycle:
        [
          ( Published
          , rel_inverness
          , "Remove a purpose from a network (if present)"
          )
        ]
      ~allowed_roles:_R_POOL_ADMIN ()

  (** A virtual network *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A virtual network")]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_network ~descr:"A virtual network"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
        (* vm admins can create/destroy networks without PIFs *)
      ~doc_tags:[Networking]
      ~messages:
        [
          attach
        ; pool_introduce
        ; create_new_blob
        ; set_default_locking_mode
        ; attach_for_vm
        ; detach_for_vm
        ; add_purpose
        ; remove_purpose
        ]
      ~contents:
        ([
           uid
             ~lifecycle:
               [(Published, rel_rio, "Unique identifier/object reference")]
             _network
         ; namespace ~name:"name"
             ~contents:
               (names ~writer_roles:_R_POOL_OP
                  ~lifecycle:[(Published, rel_rio, "")]
                  oss_since_303 RW
               )
             ()
         ]
        @ allowed_and_current_operations ~writer_roles:_R_POOL_OP operations
        @ [
            field ~qualifier:DynamicRO ~ty:(Set (Ref _vif))
              ~lifecycle:[(Published, rel_rio, "list of connected vifs")]
              "VIFs" "list of connected vifs"
          ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pif))
              ~lifecycle:[(Published, rel_rio, "list of connected pifs")]
              "PIFs" "list of connected pifs"
          ; field ~qualifier:RW ~ty:Int ~default_value:(Some (VInt 1500L))
              ~lifecycle:[(Published, rel_midnight_ride, "MTU in octets")]
              "MTU" "MTU in octets"
          ; field ~writer_roles:_R_POOL_OP
              ~ty:(Map (String, String))
              "other_config" "additional configuration"
              ~map_keys_roles:
                [
                  ("folder", _R_VM_OP)
                ; ("XenCenter.CustomFields.*", _R_VM_OP)
                ; ("XenCenterCreateInProgress", _R_VM_OP)
                ]
              ~lifecycle:[(Published, rel_rio, "additional configuration")]
          ; field
              ~lifecycle:
                [
                  (Published, rel_rio, "")
                ; ( Changed
                  , rel_falcon
                  , "Added to the constructor (network.create)"
                  )
                ]
              ~in_oss_since:None ~qualifier:StaticRO ~ty:String
              ~default_value:(Some (VString "")) "bridge"
              "name of the bridge corresponding to this network on the local \
               host"
          ; field
              ~lifecycle:[(Published, rel_falcon, "")]
              ~qualifier:StaticRO ~ty:Bool ~default_value:(Some (VBool true))
              "managed" "true if the bridge is managed by xapi"
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "Binary blobs associated with this network"
                  )
                ]
              ~ty:(Map (String, Ref _blob))
              ~default_value:(Some (VMap [])) "blobs"
              "Binary blobs associated with this network"
          ; field ~writer_roles:_R_VM_OP
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "user-specified tags for categorization purposes"
                  )
                ]
              ~default_value:(Some (VSet [])) ~ty:(Set String) "tags"
              "user-specified tags for categorization purposes"
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_tampa
                  , "The network will use this value to determine the \
                     behaviour of all VIFs where locking_mode = default"
                  )
                ]
              ~default_value:(Some (VEnum "unlocked")) ~ty:default_locking_mode
              "default_locking_mode"
              "The network will use this value to determine the behaviour of \
               all VIFs where locking_mode = default"
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_creedence
                  , "The IP addresses assigned to VIFs on networks that have \
                     active xapi-managed DHCP"
                  )
                ]
              ~default_value:(Some (VMap []))
              ~ty:(Map (Ref _vif, String))
              "assigned_ips"
              "The IP addresses assigned to VIFs on networks that have active \
               xapi-managed DHCP"
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_inverness
                  , "Set of purposes for which the server will use this network"
                  )
                ]
              ~default_value:(Some (VSet [])) ~ty:(Set purpose) "purpose"
              "Set of purposes for which the server will use this network"
          ]
        )
      ()
end

module PIF = struct
  let create_VLAN =
    call ~name:"create_VLAN"
      ~doc:
        "Create a VLAN interface from an existing physical interface. This \
         call is deprecated: use VLAN.create instead"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Create a VLAN interface from an existing physical interface"
          )
        ; (Deprecated, rel_miami, "Replaced by VLAN.create")
        ]
      ~params:
        [
          ( String
          , "device"
          , "physical interface on which to create the VLAN interface"
          )
        ; ( Ref _network
          , "network"
          , "network to which this interface should be connected"
          )
        ; (Ref _host, "host", "physical machine to which this PIF is connected")
        ; (Int, "VLAN", "VLAN tag for the new interface")
        ]
      ~result:(Ref _pif, "The reference of the created PIF object")
      ~errs:[Api_errors.vlan_tag_invalid]
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~doc:
        "Destroy the PIF object (provided it is a VLAN interface). This call \
         is deprecated: use VLAN.destroy or Bond.destroy instead"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Destroy the PIF object (provided it is a VLAN interface)"
          )
        ; (Deprecated, rel_miami, "Replaced by VLAN.destroy and Bond.destroy")
        ]
      ~params:[(Ref _pif, "self", "the PIF object to destroy")]
      ~errs:[Api_errors.pif_is_physical]
      ~allowed_roles:_R_POOL_OP ()

  let plug =
    call ~name:"plug" ~doc:"Attempt to bring up a physical interface"
      ~params:[(Ref _pif, "self", "the PIF object to plug")]
      ~lifecycle:
        [(Published, rel_miami, "Attempt to bring up a physical interface")]
      ~allowed_roles:_R_POOL_OP
      ~errs:[Api_errors.transport_pif_not_configured]
      ()

  let unplug =
    call ~name:"unplug" ~doc:"Attempt to bring down a physical interface"
      ~params:[(Ref _pif, "self", "the PIF object to unplug")]
      ~lifecycle:
        [(Published, rel_miami, "Attempt to bring down a physical interface")]
      ~allowed_roles:_R_POOL_OP
      ~errs:
        [
          Api_errors.ha_operation_would_break_failover_plan
        ; Api_errors.vif_in_use
        ; Api_errors.pif_does_not_allow_unplug
        ; Api_errors.pif_has_fcoe_sr_in_use
        ]
      ()

  let set_disallow_unplug =
    call ~name:"set_disallow_unplug"
      ~doc:"Set whether unplugging the PIF is allowed" ~hide_from_docs:false
      ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_orlando, "Set whether unplugging the PIF is allowed")]
      ~params:
        [
          (Ref _pif, "self", "Reference to the object")
        ; (Bool, "value", "New value to set")
        ]
      ~allowed_roles:_R_POOL_OP
      ~errs:Api_errors.[other_operation_in_progress; clustering_enabled]
      ()

  let ip_configuration_mode =
    Enum
      ( "ip_configuration_mode"
      , [
          ("None", "Do not acquire an IP address")
        ; ("DHCP", "Acquire an IP address by DHCP")
        ; ("Static", "Static IP address configuration")
        ]
      )

  let reconfigure_ip =
    call ~name:"reconfigure_ip"
      ~doc:"Reconfigure the IP address settings for this interface"
      ~params:
        [
          (Ref _pif, "self", "the PIF object to reconfigure")
        ; ( ip_configuration_mode
          , "mode"
          , "whether to use dynamic/static/no-assignment"
          )
        ; (String, "IP", "the new IP address")
        ; (String, "netmask", "the new netmask")
        ; (String, "gateway", "the new gateway")
        ; (String, "DNS", "the new DNS settings")
        ]
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Reconfigure the IP address settings for this interface"
          )
        ]
      ~allowed_roles:_R_POOL_OP
      ~errs:Api_errors.[clustering_enabled]
      ()

  let ipv6_configuration_mode =
    Enum
      ( "ipv6_configuration_mode"
      , [
          ("None", "Do not acquire an IPv6 address")
        ; ("DHCP", "Acquire an IPv6 address by DHCP")
        ; ("Static", "Static IPv6 address configuration")
        ; ("Autoconf", "Router assigned prefix delegation IPv6 allocation")
        ]
      )

  let reconfigure_ipv6 =
    call ~name:"reconfigure_ipv6"
      ~doc:"Reconfigure the IPv6 address settings for this interface"
      ~params:
        [
          (Ref _pif, "self", "the PIF object to reconfigure")
        ; ( ipv6_configuration_mode
          , "mode"
          , "whether to use dynamic/static/no-assignment"
          )
        ; ( String
          , "IPv6"
          , "the new IPv6 address (in <addr>/<prefix length> format)"
          )
        ; (String, "gateway", "the new gateway")
        ; (String, "DNS", "the new DNS settings")
        ]
      ~lifecycle:[(Published, rel_tampa, "")]
      ~allowed_roles:_R_POOL_OP
      ~errs:Api_errors.[clustering_enabled]
      ()

  let primary_address_type =
    Enum
      ( "primary_address_type"
      , [
          ("IPv4", "Primary address is the IPv4 address")
        ; ("IPv6", "Primary address is the IPv6 address")
        ]
      )

  let set_primary_address_type =
    call ~name:"set_primary_address_type"
      ~doc:"Change the primary address type used by this PIF"
      ~params:
        [
          (Ref _pif, "self", "the PIF object to reconfigure")
        ; ( primary_address_type
          , "primary_address_type"
          , "Whether to prefer IPv4 or IPv6 connections"
          )
        ]
      ~lifecycle:[(Published, rel_tampa, "")]
      ~allowed_roles:_R_POOL_OP ()

  let scan =
    call ~name:"scan"
      ~doc:
        "Scan for physical interfaces on a host and create PIF objects to \
         represent them"
      ~params:[(Ref _host, "host", "The host on which to scan")]
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Scan for physical interfaces on a host and create PIF objects to \
             represent them"
          )
        ]
      ~allowed_roles:_R_POOL_OP ()

  let introduce_params =
    [
      {
        param_type= Ref _host
      ; param_name= "host"
      ; param_doc= "The host on which the interface exists"
      ; param_release= miami_release
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "MAC"
      ; param_doc= "The MAC address of the interface"
      ; param_release= miami_release
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "device"
      ; param_doc= "The device name to use for the interface"
      ; param_release= miami_release
      ; param_default= None
      }
    ; {
        param_type= Bool
      ; param_name= "managed"
      ; param_doc=
          "Indicates whether the interface is managed by xapi (defaults to \
           \"true\")"
      ; param_release= vgpu_productisation_release
      ; param_default= Some (VBool true)
      }
    ]

  let introduce =
    call ~name:"introduce"
      ~doc:"Create a PIF object matching a particular network interface"
      ~versioned_params:introduce_params
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Create a PIF object matching a particular network interface"
          )
        ]
      ~result:(Ref _pif, "The reference of the created PIF object")
      ~allowed_roles:_R_POOL_OP ()

  let forget =
    call ~name:"forget"
      ~doc:"Destroy the PIF object matching a particular network interface"
      ~params:[(Ref _pif, "self", "The PIF object to destroy")]
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Destroy the PIF object matching a particular network interface"
          )
        ]
      ~allowed_roles:_R_POOL_OP
      ~errs:Api_errors.[pif_tunnel_still_exists; clustering_enabled]
      ()

  let pool_introduce_params first_rel =
    [
      {
        param_type= String
      ; param_name= "device"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Ref _network
      ; param_name= "network"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Ref _host
      ; param_name= "host"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "MAC"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Int
      ; param_name= "MTU"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Int
      ; param_name= "VLAN"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Bool
      ; param_name= "physical"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= ip_configuration_mode
      ; param_name= "ip_configuration_mode"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "IP"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "netmask"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "gateway"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "DNS"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Ref _bond
      ; param_name= "bond_slave_of"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Ref _vlan
      ; param_name= "VLAN_master_of"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Bool
      ; param_name= "management"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Map (String, String)
      ; param_name= "other_config"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Bool
      ; param_name= "disallow_unplug"
      ; param_doc= ""
      ; param_release= orlando_release
      ; param_default= Some (VBool false)
      }
    ; {
        param_type= ipv6_configuration_mode
      ; param_name= "ipv6_configuration_mode"
      ; param_doc= ""
      ; param_release= boston_release
      ; param_default= Some (VEnum "None")
      }
    ; {
        param_type= Set String
      ; param_name= "IPv6"
      ; param_doc= ""
      ; param_release= boston_release
      ; param_default= Some (VSet [])
      }
    ; {
        param_type= String
      ; param_name= "ipv6_gateway"
      ; param_doc= ""
      ; param_release= boston_release
      ; param_default= Some (VString "")
      }
    ; {
        param_type= primary_address_type
      ; param_name= "primary_address_type"
      ; param_doc= ""
      ; param_release= boston_release
      ; param_default= Some (VEnum "IPv4")
      }
    ; {
        param_type= Bool
      ; param_name= "managed"
      ; param_doc= ""
      ; param_release= vgpu_productisation_release
      ; param_default= Some (VBool true)
      }
    ; {
        param_type= Map (String, String)
      ; param_name= "properties"
      ; param_doc= ""
      ; param_release= creedence_release
      ; param_default= Some (VMap [])
      }
    ]

  (* PIF pool introduce is used to copy PIF records on pool join -- it's the PIF analogue of VDI.pool_introduce *)
  let pool_introduce =
    call ~name:"pool_introduce" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_rio, "Create a new PIF record in the database only")]
      ~versioned_params:(pool_introduce_params miami_release)
      ~doc:"Create a new PIF record in the database only"
      ~result:(Ref _pif, "The ref of the newly created PIF record.")
      ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

  let db_introduce =
    call ~name:"db_introduce" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_orlando
          , "Create a new PIF record in the database only"
          )
        ]
      ~versioned_params:(pool_introduce_params orlando_release)
      ~doc:"Create a new PIF record in the database only"
      ~result:(Ref _pif, "The ref of the newly created PIF record.")
      ~hide_from_docs:false ~allowed_roles:_R_POOL_OP ()

  let db_forget =
    call ~name:"db_forget" ~in_oss_since:None
      ~lifecycle:[(Published, rel_orlando, "Destroy a PIF database record.")]
      ~params:
        [
          ( Ref _pif
          , "self"
          , "The ref of the PIF whose database record should be destroyed"
          )
        ]
      ~doc:"Destroy a PIF database record." ~hide_from_docs:false
      ~allowed_roles:_R_POOL_OP ()

  let set_property =
    call ~name:"set_property" ~doc:"Set the value of a property of the PIF"
      ~params:
        [
          (Ref _pif, "self", "The PIF")
        ; (String, "name", "The property name")
        ; (String, "value", "The property value")
        ]
      ~lifecycle:[(Published, rel_creedence, "")]
      ~allowed_roles:_R_POOL_OP ()

  let igmp_status =
    Enum
      ( "pif_igmp_status"
      , [
          ( "enabled"
          , "IGMP Snooping is enabled in the corresponding backend bridge.'"
          )
        ; ( "disabled"
          , "IGMP Snooping is disabled in the corresponding backend bridge.'"
          )
        ; ( "unknown"
          , "IGMP snooping status is unknown. If this is a VLAN master, then \
             please consult the underlying VLAN slave PIF."
          )
        ]
      )

  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "A physical network interface (note separate VLANs are represented \
             as several PIFs)"
          )
        ]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_pif
      ~descr:
        "A physical network interface (note separate VLANs are represented as \
         several PIFs)"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~doc_tags:[Networking]
      ~messages:
        [
          create_VLAN
        ; destroy
        ; reconfigure_ip
        ; reconfigure_ipv6
        ; set_primary_address_type
        ; scan
        ; introduce
        ; forget
        ; unplug
        ; set_disallow_unplug
        ; plug
        ; pool_introduce
        ; db_introduce
        ; db_forget
        ; set_property
        ]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _pif
        ; (* qualifier changed RW -> StaticRO in Miami *)
          field ~qualifier:StaticRO
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "machine-readable name of the interface (e.g. eth0)"
                )
              ]
            "device" "machine-readable name of the interface (e.g. eth0)"
        ; field ~qualifier:StaticRO ~ty:(Ref _network)
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "virtual network to which this pif is connected"
                )
              ]
            "network" "virtual network to which this pif is connected"
        ; field ~qualifier:StaticRO ~ty:(Ref _host)
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "physical machine to which this pif is connected"
                )
              ]
            "host" "physical machine to which this pif is connected"
        ; (* qualifier changed RW -> StaticRO in Miami *)
          field ~qualifier:StaticRO
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "ethernet MAC address of physical interface"
                )
              ]
            "MAC" "ethernet MAC address of physical interface"
        ; (* qualifier changed RW -> StaticRO in Miami *)
          field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:[(Published, rel_rio, "MTU in octets")]
            "MTU" "MTU in octets"
        ; (* qualifier changed RW -> StaticRO in Miami *)
          field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "VLAN tag for all traffic passing through this interface"
                )
              ]
            "VLAN" "VLAN tag for all traffic passing through this interface"
        ; field ~in_oss_since:None ~internal_only:true
            ~lifecycle:[(Published, rel_rio, "actual dom0 device name")]
            "device_name" "actual dom0 device name"
        ; field ~qualifier:DynamicRO ~ty:(Ref _pif_metrics)
            ~lifecycle:
              [(Published, rel_rio, "metrics associated with this PIF")]
            "metrics" "metrics associated with this PIF"
        ; field ~in_oss_since:None ~ty:Bool
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "true if this represents a physical network interface"
                )
              ]
            ~qualifier:DynamicRO "physical"
            "true if this represents a physical network interface"
            ~default_value:(Some (VBool false))
        ; field ~in_oss_since:None ~ty:Bool
            ~lifecycle:
              [(Published, rel_miami, "true if this interface is online")]
            ~qualifier:DynamicRO "currently_attached"
            "true if this interface is online" ~default_value:(Some (VBool true))
        ; field ~in_oss_since:None ~ty:ip_configuration_mode
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "Sets if and how this interface gets an IP address"
                )
              ]
            ~qualifier:DynamicRO "ip_configuration_mode"
            "Sets if and how this interface gets an IP address"
            ~default_value:(Some (VEnum "None"))
        ; field ~in_oss_since:None ~ty:String
            ~lifecycle:[(Published, rel_miami, "IP address")]
            ~qualifier:DynamicRO "IP" "IP address"
            ~default_value:(Some (VString ""))
        ; field ~in_oss_since:None ~ty:String
            ~lifecycle:[(Published, rel_miami, "IP netmask")]
            ~qualifier:DynamicRO "netmask" "IP netmask"
            ~default_value:(Some (VString ""))
        ; field ~in_oss_since:None ~ty:String
            ~lifecycle:[(Published, rel_miami, "IP gateway")]
            ~qualifier:DynamicRO "gateway" "IP gateway"
            ~default_value:(Some (VString ""))
        ; field ~in_oss_since:None ~ty:String
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "Comma separated list of the IP addresses of the DNS servers \
                   to use"
                )
              ]
            ~qualifier:DynamicRO "DNS"
            "Comma separated list of the IP addresses of the DNS servers to use"
            ~default_value:(Some (VString ""))
        ; field ~in_oss_since:None ~ty:(Ref _bond)
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "Indicates which bond this interface is part of"
                )
              ]
            ~qualifier:DynamicRO "bond_slave_of"
            "Indicates which bond this interface is part of"
            ~default_value:(Some (VRef ""))
        ; field ~in_oss_since:None ~ty:(Set (Ref _bond))
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "Indicates this PIF represents the results of a bond"
                )
              ]
            ~qualifier:DynamicRO "bond_master_of"
            "Indicates this PIF represents the results of a bond"
        ; field ~in_oss_since:None ~ty:(Ref _vlan)
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "Indicates which VLAN this interface receives untagged \
                   traffic from"
                )
              ]
            ~qualifier:DynamicRO "VLAN_master_of"
            "Indicates which VLAN this interface receives untagged traffic from"
            ~default_value:(Some (VRef ""))
        ; field ~in_oss_since:None ~ty:(Set (Ref _vlan))
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "Indicates which VLANs this interface transmits tagged \
                   traffic to"
                )
              ]
            ~qualifier:DynamicRO "VLAN_slave_of"
            "Indicates which VLANs this interface transmits tagged traffic to"
        ; field ~in_oss_since:None ~ty:Bool
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "Indicates whether the control software is listening for \
                   connections on this interface"
                )
              ]
            ~qualifier:DynamicRO "management"
            "Indicates whether the control software is listening for \
             connections on this interface"
            ~default_value:(Some (VBool false))
        ; field
            ~lifecycle:[(Published, rel_miami, "Additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "Additional configuration"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_orlando
                , "Prevent this PIF from being unplugged; set this to notify \
                   the management tool-stack that the PIF has a special use \
                   and should not be unplugged under any circumstances (e.g. \
                   because you're running storage traffic over it)"
                )
              ]
            ~qualifier:DynamicRO ~default_value:(Some (VBool false)) ~ty:Bool
            "disallow_unplug"
            "Prevent this PIF from being unplugged; set this to notify the \
             management tool-stack that the PIF has a special use and should \
             not be unplugged under any circumstances (e.g. because you're \
             running storage traffic over it)"
        ; field ~in_oss_since:None ~ty:(Set (Ref _tunnel))
            ~lifecycle:
              [
                ( Published
                , rel_cowley
                , "Indicates to which tunnel this PIF gives access"
                )
              ]
            ~qualifier:DynamicRO "tunnel_access_PIF_of"
            "Indicates to which tunnel this PIF gives access"
        ; field ~in_oss_since:None ~ty:(Set (Ref _tunnel))
            ~lifecycle:
              [
                ( Published
                , rel_cowley
                , "Indicates to which tunnel this PIF provides transport"
                )
              ]
            ~qualifier:DynamicRO "tunnel_transport_PIF_of"
            "Indicates to which tunnel this PIF provides transport"
        ; field ~in_oss_since:None ~ty:ipv6_configuration_mode
            ~lifecycle:[(Published, rel_tampa, "")]
            ~qualifier:DynamicRO "ipv6_configuration_mode"
            "Sets if and how this interface gets an IPv6 address"
            ~default_value:(Some (VEnum "None"))
        ; field ~in_oss_since:None ~ty:(Set String)
            ~lifecycle:[(Published, rel_tampa, "")]
            ~qualifier:DynamicRO "IPv6" "IPv6 address"
            ~default_value:(Some (VSet []))
        ; field ~in_oss_since:None ~ty:String
            ~lifecycle:[(Published, rel_tampa, "")]
            ~qualifier:DynamicRO "ipv6_gateway" "IPv6 gateway"
            ~default_value:(Some (VString ""))
        ; field ~in_oss_since:None ~ty:primary_address_type
            ~lifecycle:[(Published, rel_tampa, "")]
            ~qualifier:DynamicRO "primary_address_type"
            "Which protocol should define the primary address of this interface"
            ~default_value:(Some (VEnum "IPv4"))
        ; field ~in_oss_since:None ~ty:Bool
            ~lifecycle:[(Published, rel_vgpu_productisation, "")]
            ~qualifier:StaticRO "managed"
            "Indicates whether the interface is managed by xapi. If it is not, \
             then xapi will not configure the interface, the commands \
             PIF.plug/unplug/reconfigure_ip(v6) cannot be used, nor can the \
             interface be bonded or have VLANs based on top through xapi."
            ~default_value:(Some (VBool true))
        ; field
            ~lifecycle:[(Published, rel_creedence, "")]
            ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap [])) "properties"
            "Additional configuration properties for the interface."
        ; field
            ~lifecycle:[(Published, rel_dundee, "")]
            ~qualifier:DynamicRO ~ty:(Set String)
            ~default_value:(Some (VSet [])) "capabilities"
            "Additional capabilities on the interface."
        ; field
            ~lifecycle:[(Published, rel_inverness, "")]
            ~qualifier:DynamicRO ~ty:igmp_status
            ~default_value:(Some (VEnum "unknown")) "igmp_snooping_status"
            "The IGMP snooping status of the corresponding network bridge"
        ; field ~in_oss_since:None ~ty:(Set (Ref _network_sriov))
            ~lifecycle:
              [
                ( Published
                , rel_kolkata
                , "Indicates which network_sriov this interface is physical of"
                )
              ]
            ~qualifier:DynamicRO "sriov_physical_PIF_of"
            "Indicates which network_sriov this interface is physical of"
        ; field ~in_oss_since:None ~ty:(Set (Ref _network_sriov))
            ~lifecycle:
              [
                ( Published
                , rel_kolkata
                , "Indicates which network_sriov this interface is logical of"
                )
              ]
            ~qualifier:DynamicRO "sriov_logical_PIF_of"
            "Indicates which network_sriov this interface is logical of"
        ; field ~qualifier:DynamicRO ~ty:(Ref _pci)
            ~lifecycle:[(Published, rel_kolkata, "")]
            ~default_value:(Some (VRef null_ref)) "PCI"
            "Link to underlying PCI device"
        ]
      ()
end

module PIF_metrics = struct
  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "The metrics associated with a physical network interface"
          )
        ]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_pif_metrics
      ~descr:"The metrics associated with a physical network interface"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~doc_tags:[Networking]
      ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _pif_metrics
        ; namespace ~name:"io" ~contents:iobandwidth ()
        ; field ~qualifier:DynamicRO ~ty:Bool
            ~lifecycle:
              [(Published, rel_rio, "Report if the PIF got a carrier or not")]
            "carrier" "Report if the PIF got a carrier or not"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "Report vendor ID")]
            "vendor_id" "Report vendor ID"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "Report vendor name")]
            "vendor_name" "Report vendor name"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "Report device ID")]
            "device_id" "Report device ID"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "Report device name")]
            "device_name" "Report device name"
        ; field ~qualifier:DynamicRO ~ty:Int
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Speed of the link in Mbit/s (if available)"
                )
              ]
            "speed" "Speed of the link in Mbit/s (if available)"
        ; field ~qualifier:DynamicRO ~ty:Bool
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Full duplex capability of the link (if available)"
                )
              ]
            "duplex" "Full duplex capability of the link (if available)"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:
              [(Published, rel_rio, "PCI bus path of the pif (if available)")]
            "pci_bus_path" "PCI bus path of the pif (if available)"
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Time at which this information was last updated"
                )
              ]
            "last_updated" "Time at which this information was last updated"
        ; field
            ~lifecycle:[(Published, rel_orlando, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

module Bond = struct
  let mode =
    Enum
      ( "bond_mode"
      , [
          ("balance-slb", "Source-level balancing")
        ; ( "active-backup"
          , "Active/passive bonding: only one NIC is carrying traffic"
          )
        ; ("lacp", "Link aggregation control protocol")
        ]
      )

  let create =
    call ~name:"create" ~doc:"Create an interface bond"
      ~versioned_params:
        [
          {
            param_type= Ref _network
          ; param_name= "network"
          ; param_doc= "Network to add the bonded PIF to"
          ; param_release= miami_release
          ; param_default= None
          }
        ; {
            param_type= Set (Ref _pif)
          ; param_name= "members"
          ; param_doc= "PIFs to add to this bond"
          ; param_release= miami_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "MAC"
          ; param_doc=
              "The MAC address to use on the bond itself. If this parameter is \
               the empty string then the bond will inherit its MAC address \
               from the primary slave."
          ; param_release= miami_release
          ; param_default= None
          }
        ; {
            param_type= mode
          ; param_name= "mode"
          ; param_doc= "Bonding mode to use for the new bond"
          ; param_release= boston_release
          ; param_default= Some (VEnum "balance-slb")
          }
        ; {
            param_type= Map (String, String)
          ; param_name= "properties"
          ; param_doc=
              "Additional configuration parameters specific to the bond mode"
          ; param_release= tampa_release
          ; param_default= Some (VMap [])
          }
        ]
      ~result:(Ref _bond, "The reference of the created Bond object")
      ~lifecycle:[(Published, rel_miami, "Create an interface bond")]
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy" ~doc:"Destroy an interface bond"
      ~params:[(Ref _bond, "self", "Bond to destroy")]
      ~lifecycle:[(Published, rel_miami, "Destroy an interface bond")]
      ~allowed_roles:_R_POOL_OP ()

  let set_mode =
    call ~name:"set_mode" ~doc:"Change the bond mode"
      ~params:
        [(Ref _bond, "self", "The bond"); (mode, "value", "The new bond mode")]
      ~lifecycle:[(Published, rel_boston, "")]
      ~allowed_roles:_R_POOL_OP ()

  let set_property =
    call ~name:"set_property" ~doc:"Set the value of a property of the bond"
      ~params:
        [
          (Ref _bond, "self", "The bond")
        ; (String, "name", "The property name")
        ; (String, "value", "The property value")
        ]
      ~lifecycle:
        [(Published, rel_tampa, "Set the value of a property of the bond")]
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "A Network bond that combines physical network interfaces, also \
             known as link aggregation"
          )
        ]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_bond
      ~descr:
        "A Network bond that combines physical network interfaces, also known \
         as link aggregation"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~doc_tags:[Networking]
      ~messages:[create; destroy; set_mode; set_property]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _bond
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_miami, "The bonded interface")]
            ~qualifier:StaticRO ~ty:(Ref _pif) "master" "The bonded interface"
            ~default_value:(Some (VRef ""))
        ; field ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_miami
                , "The interfaces which are part of this bond"
                )
              ]
            ~qualifier:DynamicRO ~ty:(Set (Ref _pif)) "slaves"
            "The interfaces which are part of this bond"
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ; field
            ~lifecycle:[(Published, rel_boston, "")]
            ~qualifier:DynamicRO ~default_value:(Some (VRef null_ref))
            ~ty:(Ref _pif) "primary_slave"
            "The PIF of which the IP configuration and MAC were copied to the \
             bond, and which will receive all configuration/VLANs/VIFs on the \
             bond if the bond is destroyed"
        ; field
            ~lifecycle:[(Published, rel_boston, "")]
            ~qualifier:DynamicRO ~default_value:(Some (VEnum "balance-slb"))
            ~ty:mode "mode"
            "The algorithm used to distribute traffic among the bonded NICs"
        ; field ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_tampa
                , "Additional configuration properties specific to the bond \
                   mode."
                )
              ]
            ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap [])) "properties"
            "Additional configuration properties specific to the bond mode."
        ; field ~in_oss_since:None
            ~lifecycle:
              [(Published, rel_tampa, "Number of links up in this bond")]
            ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L))
            "links_up" "Number of links up in this bond"
        ; field
            ~lifecycle:[(Published, rel_quebec, "")]
            ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool true))
            "auto_update_mac"
            "true if the MAC was taken from the primary slave when the bond \
             was created, and false if the client specified the MAC"
        ]
      ()
end

module VLAN = struct
  let introduce_params first_rel =
    [
      {
        param_type= Ref _pif
      ; param_name= "tagged_PIF"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Ref _pif
      ; param_name= "untagged_PIF"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Int
      ; param_name= "tag"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Map (String, String)
      ; param_name= "other_config"
      ; param_doc= ""
      ; param_release= first_rel
      ; param_default= None
      }
    ]

  (* vlan pool introduce is used to copy management vlan record on pool join -- it's the vlan analogue of VDI/PIF.pool_introduce *)
  let pool_introduce =
    call ~name:"pool_introduce" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_inverness
          , "Create a new vlan record in the database only"
          )
        ]
      ~versioned_params:(introduce_params inverness_release)
      ~doc:"Create a new vlan record in the database only"
      ~result:(Ref _vlan, "The reference of the created VLAN object")
      ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

  let create =
    call ~name:"create" ~doc:"Create a VLAN mux/demuxer"
      ~params:
        [
          (Ref _pif, "tagged_PIF", "PIF which receives the tagged traffic")
        ; (Int, "tag", "VLAN tag to use")
        ; (Ref _network, "network", "Network to receive the untagged traffic")
        ]
      ~result:(Ref _vlan, "The reference of the created VLAN object")
      ~lifecycle:[(Published, rel_miami, "Create a VLAN mux/demuxer")]
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy" ~doc:"Destroy a VLAN mux/demuxer"
      ~params:[(Ref _vlan, "self", "VLAN mux/demuxer to destroy")]
      ~lifecycle:[(Published, rel_miami, "Destroy a VLAN mux/demuxer")]
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_miami, "A VLAN mux/demux")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_vlan ~descr:"A VLAN mux/demux"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~doc_tags:[Networking]
      ~messages:[pool_introduce; create; destroy]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _vlan
        ; field ~qualifier:StaticRO ~ty:(Ref _pif)
            ~lifecycle:
              [(Published, rel_miami, "interface on which traffic is tagged")]
            "tagged_PIF" "interface on which traffic is tagged"
            ~default_value:(Some (VRef ""))
        ; field ~qualifier:DynamicRO ~ty:(Ref _pif)
            ~lifecycle:
              [(Published, rel_miami, "interface on which traffic is untagged")]
            "untagged_PIF" "interface on which traffic is untagged"
            ~default_value:(Some (VRef ""))
        ; field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:[(Published, rel_miami, "VLAN tag in use")]
            "tag" "VLAN tag in use" ~default_value:(Some (VInt (-1L)))
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

module Tunnel = struct
  let tunnel_protocol =
    Enum
      ("tunnel_protocol", [("gre", "GRE protocol"); ("vxlan", "VxLAN Protocol")])

  let create =
    call ~name:"create" ~doc:"Create a tunnel"
      ~versioned_params:
        [
          {
            param_type= Ref _pif
          ; param_name= "transport_PIF"
          ; param_doc= "PIF which receives the tagged traffic"
          ; param_release= dundee_release
          ; param_default= None
          }
        ; {
            param_type= Ref _network
          ; param_name= "network"
          ; param_doc= "Network to receive the tunnelled traffic"
          ; param_release= dundee_release
          ; param_default= None
          }
        ; {
            param_type= tunnel_protocol
          ; param_name= "protocol"
          ; param_doc= "Protocol used for the tunnel (GRE or VxLAN)"
          ; param_release= numbered_release "1.250.0"
          ; param_default= Some (VEnum "gre")
          }
        ]
      ~result:(Ref _tunnel, "The reference of the created tunnel object")
      ~lifecycle:[(Published, rel_cowley, "Create a tunnel")]
      ~allowed_roles:_R_POOL_OP
      ~errs:
        [
          Api_errors.openvswitch_not_active
        ; Api_errors.transport_pif_not_configured
        ; Api_errors.is_tunnel_access_pif
        ]
      ()

  let destroy =
    call ~name:"destroy" ~doc:"Destroy a tunnel"
      ~params:[(Ref _tunnel, "self", "tunnel to destroy")]
      ~lifecycle:[(Published, rel_cowley, "Destroy a tunnel")]
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_cowley, "A tunnel for network traffic")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_tunnel
      ~descr:"A tunnel for network traffic" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~doc_tags:[Networking]
      ~messages:[create; destroy]
      ~contents:
        [
          uid _tunnel
            ~lifecycle:
              [(Published, rel_cowley, "Unique identifier/object reference")]
        ; field ~qualifier:StaticRO ~ty:(Ref _pif)
            ~lifecycle:
              [
                ( Published
                , rel_cowley
                , "The interface through which the tunnel is accessed"
                )
              ]
            "access_PIF" "The interface through which the tunnel is accessed"
            ~default_value:(Some (VRef ""))
        ; field ~qualifier:StaticRO ~ty:(Ref _pif)
            ~lifecycle:
              [(Published, rel_cowley, "The interface used by the tunnel")]
            "transport_PIF" "The interface used by the tunnel"
            ~default_value:(Some (VRef ""))
        ; field
            ~ty:(Map (String, String))
            ~lifecycle:
              [(Published, rel_cowley, "Status information about the tunnel")]
            "status" "Status information about the tunnel"
            ~default_value:(Some (VMap [(VString "active", VString "false")]))
        ; field
            ~lifecycle:[(Published, rel_cowley, "Additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "Additional configuration"
        ; field ~ty:tunnel_protocol ~default_value:(Some (VEnum "gre"))
            ~lifecycle:[(Published, "1.250.0", "Add protocol field to tunnel")]
            "protocol" "The protocol used for tunneling (either GRE or VxLAN)"
        ]
      ()
end

module PBD = struct
  let plug =
    call ~name:"plug" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Activate the specified PBD, causing the referenced SR to be \
             attached and scanned"
          )
        ]
      ~doc:
        "Activate the specified PBD, causing the referenced SR to be attached \
         and scanned"
      ~params:[(Ref _pbd, "self", "The PBD to activate")]
      ~errs:[Api_errors.sr_unknown_driver]
      ~allowed_roles:_R_POOL_OP ()

  let unplug =
    call ~name:"unplug" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Deactivate the specified PBD, causing the referenced SR to be \
             detached and nolonger scanned"
          )
        ]
      ~doc:
        "Deactivate the specified PBD, causing the referenced SR to be \
         detached and nolonger scanned"
      ~params:[(Ref _pbd, "self", "The PBD to deactivate")]
      ~allowed_roles:_R_POOL_OP ()

  let set_device_config =
    call ~name:"set_device_config" ~in_oss_since:None
      ~lifecycle:[(Published, rel_miami, "Sets the PBD's device_config field")]
      ~params:
        [
          (Ref _pbd, "self", "The PBD to modify")
        ; ( Map (String, String)
          , "value"
          , "The new value of the PBD's device_config"
          )
        ]
      ~doc:"Sets the PBD's device_config field" ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "The physical block devices through which hosts access SRs"
          )
        ]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_pbd
      ~descr:"The physical block devices through which hosts access SRs"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[plug; unplug; set_device_config]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _pbd
        ; field ~qualifier:StaticRO ~ty:(Ref _host)
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "physical machine on which the pbd is available"
                )
              ]
            "host" "physical machine on which the pbd is available"
        ; field ~qualifier:StaticRO ~ty:(Ref _sr)
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "the storage repository that the pbd realises"
                )
              ]
            "SR" "the storage repository that the pbd realises"
        ; field
            ~ty:(Map (String, String))
            ~qualifier:StaticRO "device_config"
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "a config string to string map that is provided to the \
                   host's SR-backend-driver"
                )
              ]
            "a config string to string map that is provided to the host's \
             SR-backend-driver"
        ; field ~ty:Bool ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "is the SR currently attached on this host?"
                )
              ]
            "currently_attached" "is the SR currently attached on this host?"
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

(* These are included in vbds and vifs -- abstracted here to keep both these uses consistent *)
let device_status_fields =
  [
    field ~ty:Bool ~qualifier:StaticRO ~default_value:(Some (VBool false))
      ~lifecycle:
        [
          (Published, rel_rio, "")
        ; ( Changed
          , "1.257.0"
          , "Made StaticRO to allow plugged VIF and VBD creation for Suspended \
             VM"
          )
        ]
      "currently_attached" "is the device currently attached (erased on reboot)"
  ; field ~ty:Int ~qualifier:DynamicRO
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "error/success code associated with last attach-operation (erased \
             on reboot)"
          )
        ]
      "status_code"
      "error/success code associated with last attach-operation (erased on \
       reboot)"
  ; field ~ty:String ~qualifier:DynamicRO
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "error/success information associated with last attach-operation \
             status (erased on reboot)"
          )
        ]
      "status_detail"
      "error/success information associated with last attach-operation status \
       (erased on reboot)"
  ; field
      ~ty:(Map (String, String))
      ~qualifier:DynamicRO
      ~lifecycle:[(Published, rel_rio, "Device runtime properties")]
      "runtime_properties" "Device runtime properties"
  ]

module VIF = struct
  (* VIF messages *)

  let ipv4_configuration_mode =
    Enum
      ( "vif_ipv4_configuration_mode"
      , [
          ( "None"
          , "Follow the default IPv4 configuration of the guest (this is \
             guest-dependent)"
          )
        ; ("Static", "Static IPv4 address configuration")
        ]
      )

  let ipv6_configuration_mode =
    Enum
      ( "vif_ipv6_configuration_mode"
      , [
          ( "None"
          , "Follow the default IPv6 configuration of the guest (this is \
             guest-dependent)"
          )
        ; ("Static", "Static IPv6 address configuration")
        ]
      )

  let plug =
    call ~name:"plug"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Hotplug the specified VIF, dynamically attaching it to the \
             running VM"
          )
        ]
      ~doc:
        "Hotplug the specified VIF, dynamically attaching it to the running VM"
      ~params:[(Ref _vif, "self", "The VIF to hotplug")]
      ~allowed_roles:_R_VM_ADMIN ()

  let unplug =
    call ~name:"unplug"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Hot-unplug the specified VIF, dynamically unattaching it from the \
             running VM"
          )
        ]
      ~doc:
        "Hot-unplug the specified VIF, dynamically unattaching it from the \
         running VM"
      ~params:[(Ref _vif, "self", "The VIF to hot-unplug")]
      ~allowed_roles:_R_VM_ADMIN ()

  let unplug_force =
    call ~name:"unplug_force"
      ~lifecycle:[(Published, rel_boston, "Forcibly unplug the specified VIF")]
      ~doc:"Forcibly unplug the specified VIF"
      ~params:[(Ref _vif, "self", "The VIF to forcibly unplug")]
      ~allowed_roles:_R_VM_ADMIN ()

  let move =
    call ~name:"move"
      ~lifecycle:
        [
          ( Published
          , rel_ely
          , "Move the specified VIF to the specified network, even while the \
             VM is running"
          )
        ]
      ~doc:
        "Move the specified VIF to the specified network, even while the VM is \
         running"
      ~params:
        [
          (Ref _vif, "self", "The VIF to move")
        ; (Ref _network, "network", "The network to move it to")
        ]
      ~allowed_roles:_R_VM_ADMIN ()

  let operations =
    Enum
      ( "vif_operations"
      , [
          ("attach", "Attempting to attach this VIF to a VM")
        ; ("plug", "Attempting to hotplug this VIF")
        ; ("unplug", "Attempting to hot unplug this VIF")
        ]
      )

  let locking_mode =
    Enum
      ( "vif_locking_mode"
      , [
          ( "network_default"
          , "No specific configuration set - default network policy applies"
          )
        ; ( "locked"
          , "Only traffic to a specific MAC and a list of IPv4 or IPv6 \
             addresses is permitted"
          )
        ; ("unlocked", "All traffic is permitted")
        ; ("disabled", "No traffic is permitted")
        ]
      )

  let set_locking_mode =
    call ~name:"set_locking_mode"
      ~lifecycle:[(Published, rel_tampa, "Set the locking mode for this VIF")]
      ~doc:"Set the locking mode for this VIF"
      ~params:
        [
          (Ref _vif, "self", "The VIF whose locking mode will be set")
        ; (locking_mode, "value", "The new locking mode for the VIF")
        ]
      ~allowed_roles:_R_POOL_OP ()

  let set_ipv4_allowed =
    call ~name:"set_ipv4_allowed"
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Set the IPv4 addresses to which traffic on this VIF can be \
             restricted"
          )
        ]
      ~doc:
        "Set the IPv4 addresses to which traffic on this VIF can be restricted"
      ~params:
        [
          ( Ref _vif
          , "self"
          , "The VIF which the IP addresses will be associated with"
          )
        ; ( Set String
          , "value"
          , "The IP addresses which will be associated with the VIF"
          )
        ]
      ~allowed_roles:_R_POOL_OP ()

  let add_ipv4_allowed =
    call ~name:"add_ipv4_allowed"
      ~lifecycle:
        [(Published, rel_tampa, "Associates an IPv4 address with this VIF")]
      ~doc:"Associates an IPv4 address with this VIF"
      ~params:
        [
          ( Ref _vif
          , "self"
          , "The VIF which the IP address will be associated with"
          )
        ; ( String
          , "value"
          , "The IP address which will be associated with the VIF"
          )
        ]
      ~allowed_roles:_R_POOL_OP ()

  let remove_ipv4_allowed =
    call ~name:"remove_ipv4_allowed"
      ~lifecycle:
        [(Published, rel_tampa, "Removes an IPv4 address from this VIF")]
      ~doc:"Removes an IPv4 address from this VIF"
      ~params:
        [
          (Ref _vif, "self", "The VIF from which the IP address will be removed")
        ; (String, "value", "The IP address which will be removed from the VIF")
        ]
      ~allowed_roles:_R_POOL_OP ()

  let set_ipv6_allowed =
    call ~name:"set_ipv6_allowed"
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Set the IPv6 addresses to which traffic on this VIF can be \
             restricted"
          )
        ]
      ~doc:
        "Set the IPv6 addresses to which traffic on this VIF can be restricted"
      ~params:
        [
          ( Ref _vif
          , "self"
          , "The VIF which the IP addresses will be associated with"
          )
        ; ( Set String
          , "value"
          , "The IP addresses which will be associated with the VIF"
          )
        ]
      ~allowed_roles:_R_POOL_OP ()

  let add_ipv6_allowed =
    call ~name:"add_ipv6_allowed"
      ~lifecycle:
        [(Published, rel_tampa, "Associates an IPv6 address with this VIF")]
      ~doc:"Associates an IPv6 address with this VIF"
      ~params:
        [
          ( Ref _vif
          , "self"
          , "The VIF which the IP address will be associated with"
          )
        ; ( String
          , "value"
          , "The IP address which will be associated with the VIF"
          )
        ]
      ~allowed_roles:_R_POOL_OP ()

  let remove_ipv6_allowed =
    call ~name:"remove_ipv6_allowed"
      ~lifecycle:
        [(Published, rel_tampa, "Removes an IPv6 address from this VIF")]
      ~doc:"Removes an IPv6 address from this VIF"
      ~params:
        [
          (Ref _vif, "self", "The VIF from which the IP address will be removed")
        ; (String, "value", "The IP address which will be removed from the VIF")
        ]
      ~allowed_roles:_R_POOL_OP ()

  let configure_ipv4 =
    call ~name:"configure_ipv4"
      ~lifecycle:
        [
          ( Published
          , rel_dundee
          , "Configure IPv4 settings for this virtual interface"
          )
        ]
      ~doc:"Configure IPv4 settings for this virtual interface"
      ~versioned_params:
        [
          {
            param_type= Ref _vif
          ; param_name= "self"
          ; param_doc= "The VIF to configure"
          ; param_release= dundee_release
          ; param_default= None
          }
        ; {
            param_type= ipv4_configuration_mode
          ; param_name= "mode"
          ; param_doc= "Whether to use static or no IPv4 assignment"
          ; param_release= dundee_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "address"
          ; param_doc=
              "The IPv4 address in <addr>/<prefix length> format (for static \
               mode only)"
          ; param_release= dundee_release
          ; param_default= Some (VString "")
          }
        ; {
            param_type= String
          ; param_name= "gateway"
          ; param_doc=
              "The IPv4 gateway (for static mode only; leave empty to not set \
               a gateway)"
          ; param_release= dundee_release
          ; param_default= Some (VString "")
          }
        ]
      ~allowed_roles:_R_VM_OP ()

  let configure_ipv6 =
    call ~name:"configure_ipv6"
      ~lifecycle:
        [
          ( Published
          , rel_dundee
          , "Configure IPv6 settings for this virtual interface"
          )
        ]
      ~doc:"Configure IPv6 settings for this virtual interface"
      ~versioned_params:
        [
          {
            param_type= Ref _vif
          ; param_name= "self"
          ; param_doc= "The VIF to configure"
          ; param_release= dundee_release
          ; param_default= None
          }
        ; {
            param_type= ipv6_configuration_mode
          ; param_name= "mode"
          ; param_doc= "Whether to use static or no IPv6 assignment"
          ; param_release= dundee_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "address"
          ; param_doc=
              "The IPv6 address in <addr>/<prefix length> format (for static \
               mode only)"
          ; param_release= dundee_release
          ; param_default= Some (VString "")
          }
        ; {
            param_type= String
          ; param_name= "gateway"
          ; param_doc=
              "The IPv6 gateway (for static mode only; leave empty to not set \
               a gateway)"
          ; param_release= dundee_release
          ; param_default= Some (VString "")
          }
        ]
      ~allowed_roles:_R_VM_OP ()

  (** A virtual network interface *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A virtual network interface")]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_vif
      ~descr:"A virtual network interface" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN ~doc_tags:[Networking]
      ~messages:
        [
          plug
        ; unplug
        ; unplug_force
        ; move
        ; set_locking_mode
        ; set_ipv4_allowed
        ; add_ipv4_allowed
        ; remove_ipv4_allowed
        ; set_ipv6_allowed
        ; add_ipv6_allowed
        ; remove_ipv6_allowed
        ; configure_ipv4
        ; configure_ipv6
        ]
      ~contents:
        ([
           uid
             ~lifecycle:
               [(Published, rel_rio, "Unique identifier/object reference")]
             _vif
         ]
        @ allowed_and_current_operations operations
        @ [
            field ~qualifier:StaticRO
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "order in which VIF backends are created by xapi"
                  )
                ]
              "device"
              "order in which VIF backends are created by xapi. Guaranteed to \
               be an unsigned decimal integer."
          ; field ~qualifier:StaticRO ~ty:(Ref _network)
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "virtual network to which this vif is connected"
                  )
                ]
              "network" "virtual network to which this vif is connected"
          ; field ~qualifier:StaticRO ~ty:(Ref _vm)
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "virtual machine to which this vif is connected"
                  )
                ]
              "VM" "virtual machine to which this vif is connected"
          ; field ~qualifier:StaticRO ~ty:String
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "ethernet MAC address of virtual interface, as exposed to \
                     guest"
                  )
                ]
              "MAC"
              "ethernet MAC address of virtual interface, as exposed to guest"
          ; field ~qualifier:StaticRO ~ty:Int
              ~lifecycle:[(Published, rel_rio, "MTU in octets")]
              "MTU" "MTU in octets"
          ; field ~in_oss_since:None
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "true if the VIF is reserved pending a reboot/migrate"
                  )
                ]
              ~internal_only:true ~qualifier:DynamicRO ~ty:Bool "reserved"
              "true if the VIF is reserved pending a reboot/migrate"
          ; field
              ~ty:(Map (String, String))
              ~lifecycle:[(Published, rel_rio, "additional configuration")]
              "other_config" "additional configuration"
          ]
        @ device_status_fields
        @ [namespace ~name:"qos" ~contents:(qos "VIF") ()]
        @ [
            field ~qualifier:DynamicRO ~ty:(Ref _vif_metrics)
              ~default_value:(Some (VRef null_ref))
              ~lifecycle:
                [
                  (Published, rel_rio, "")
                ; (Deprecated, rel_tampa, "Dummy transition")
                ; (Removed, rel_tampa, "Disabled in favour of RRDs")
                ]
              "metrics" "metrics associated with this VIF"
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_george
                  , "true if the MAC was autogenerated; false indicates it was \
                     set manually"
                  )
                ]
              ~default_value:(Some (VBool false)) ~ty:Bool "MAC_autogenerated"
              "true if the MAC was autogenerated; false indicates it was set \
               manually"
          ; field ~qualifier:StaticRO
              ~lifecycle:
                [(Published, rel_tampa, "current locking mode of the VIF")]
              ~default_value:(Some (VEnum "network_default")) ~ty:locking_mode
              "locking_mode" "current locking mode of the VIF"
          ; field ~qualifier:StaticRO
              ~lifecycle:
                [
                  ( Published
                  , rel_tampa
                  , "A list of IPv4 addresses which can be used to filter \
                     traffic passing through this VIF"
                  )
                ]
              ~default_value:(Some (VSet [])) ~ty:(Set String) "ipv4_allowed"
              "A list of IPv4 addresses which can be used to filter traffic \
               passing through this VIF"
          ; field ~qualifier:StaticRO
              ~lifecycle:
                [
                  ( Published
                  , rel_tampa
                  , "A list of IPv6 addresses which can be used to filter \
                     traffic passing through this VIF"
                  )
                ]
              ~default_value:(Some (VSet [])) ~ty:(Set String) "ipv6_allowed"
              "A list of IPv6 addresses which can be used to filter traffic \
               passing through this VIF"
          ; field ~ty:ipv4_configuration_mode
              ~lifecycle:
                [
                  ( Published
                  , rel_dundee
                  , "Determines whether IPv4 addresses are configured on the \
                     VIF"
                  )
                ]
              ~qualifier:DynamicRO "ipv4_configuration_mode"
              "Determines whether IPv4 addresses are configured on the VIF"
              ~default_value:(Some (VEnum "None"))
          ; field ~ty:(Set String)
              ~lifecycle:
                [(Published, rel_dundee, "IPv4 addresses in CIDR format")]
              ~qualifier:DynamicRO "ipv4_addresses"
              "IPv4 addresses in CIDR format" ~default_value:(Some (VSet []))
          ; field ~ty:String
              ~lifecycle:
                [
                  ( Published
                  , rel_dundee
                  , "IPv4 gateway (the empty string means that no gateway is \
                     set)"
                  )
                ]
              ~qualifier:DynamicRO "ipv4_gateway"
              "IPv4 gateway (the empty string means that no gateway is set)"
              ~default_value:(Some (VString ""))
          ; field ~ty:ipv6_configuration_mode
              ~lifecycle:
                [
                  ( Published
                  , rel_dundee
                  , "Determines whether IPv6 addresses are configured on the \
                     VIF"
                  )
                ]
              ~qualifier:DynamicRO "ipv6_configuration_mode"
              "Determines whether IPv6 addresses are configured on the VIF"
              ~default_value:(Some (VEnum "None"))
          ; field ~ty:(Set String)
              ~lifecycle:
                [(Published, rel_dundee, "IPv6 addresses in CIDR format")]
              ~qualifier:DynamicRO "ipv6_addresses"
              "IPv6 addresses in CIDR format" ~default_value:(Some (VSet []))
          ; field ~ty:String
              ~lifecycle:
                [
                  ( Published
                  , rel_dundee
                  , "IPv6 gateway (the empty string means that no gateway is \
                     set)"
                  )
                ]
              ~qualifier:DynamicRO "ipv6_gateway"
              "IPv6 gateway (the empty string means that no gateway is set)"
              ~default_value:(Some (VString ""))
          ; field ~ty:(Ref _pci)
              ~lifecycle:
                [
                  ( Published
                  , rel_kolkata
                  , "pci of network SR-IOV VF which is reserved for this vif"
                  )
                ]
              ~internal_only:true ~qualifier:DynamicRO "reserved_pci"
              "pci of network SR-IOV VF which is reserved for this vif"
              ~default_value:(Some (VRef null_ref))
          ]
        )
      ()
end

module VIF_metrics = struct
  let t =
    create_obj
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "The metrics associated with a virtual network device"
          )
        ; (Deprecated, rel_tampa, "Dummy transition")
        ; (Removed, rel_tampa, "Disabled in favour of RRDs")
        ]
      ~in_db:true ~in_oss_since:oss_since_303 ~persist:PersistNothing
      ~gen_constructor_destructor:false ~name:_vif_metrics
      ~descr:"The metrics associated with a virtual network device"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN ~doc_tags:[Networking]
      ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _vif_metrics
        ; namespace ~name:"io" ~contents:iobandwidth ()
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Time at which this information was last updated"
                )
              ]
            "last_updated" "Time at which this information was last updated"
        ; field
            ~lifecycle:[(Published, rel_orlando, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

module Data_source = struct
  let t =
    create_obj ~in_db:false
      ~lifecycle:[(Published, rel_orlando, "Data sources for logging in RRDs")]
      ~in_oss_since:None ~persist:PersistNothing
      ~gen_constructor_destructor:false ~name:_data_source
      ~descr:"Data sources for logging in RRDs" ~gen_events:false
      ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_ADMIN ~messages:[]
      ~contents:
        [
          namespace ~name:"name"
            ~contents:
              (names oss_since_303 DynamicRO
                 ~lifecycle:[(Published, rel_rio, "")]
              )
            ()
        ; field ~qualifier:DynamicRO ~ty:Bool
            ~lifecycle:
              [(Published, rel_rio, "true if the data source is being logged")]
            "enabled" "true if the data source is being logged"
        ; field ~qualifier:DynamicRO ~ty:Bool
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "true if the data source is enabled by default. Non-default \
                   data sources cannot be disabled"
                )
              ]
            "standard"
            "true if the data source is enabled by default. Non-default data \
             sources cannot be disabled"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "the units of the value")]
            "units" "the units of the value"
        ; field ~qualifier:DynamicRO ~ty:Float
            ~lifecycle:
              [(Published, rel_rio, "the minimum value of the data source")]
            "min" "the minimum value of the data source"
        ; field ~qualifier:DynamicRO ~ty:Float
            ~lifecycle:
              [(Published, rel_rio, "the maximum value of the data source")]
            "max" "the maximum value of the data source"
        ; field ~qualifier:DynamicRO ~ty:Float
            ~lifecycle:
              [(Published, rel_rio, "current value of the data source")]
            "value" "current value of the data source"
        ]
      ()
end

module Sr_stat = struct
  let health =
    Enum
      ( "sr_health"
      , [
          ("healthy", "Storage is fully available")
        ; ("recovering", "Storage is busy recovering, e.g. rebuilding mirrors.")
        ; ( "unreachable"
          , "Storage is unreachable but may be recoverable with admin \
             intervention"
          )
        ; ( "unavailable"
          , "Storage is unavailable, a host reboot will be required"
          )
        ]
      )

  let t =
    let lifecycle =
      [
        (Prototyped, rel_kolkata, "")
      ; (Published, rel_lima, "")
      ; ( Extended
        , "24.17.0"
        , "Enum extended with 'unreachable' and 'unavailable' values"
        )
      ]
    in
    create_obj ~in_db:false ~persist:PersistNothing
      ~gen_constructor_destructor:false ~lifecycle ~in_oss_since:None
      ~name:_sr_stat
      ~descr:"A set of high-level properties associated with an SR."
      ~gen_events:false ~messages:[] ~doccomments:[]
      ~messages_default_allowed_roles:(Some [])
        (* No messages, so no roles allowed to use them *)
      ~contents:
        [
          field ~qualifier:DynamicRO ~lifecycle ~ty:(Option String) "uuid"
            "Uuid that uniquely identifies this SR, if one is available."
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:String "name_label"
            "Short, human-readable label for the SR."
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:String "name_description"
            "Longer, human-readable description of the SR. Descriptions are \
             generally only displayed by clients when the user is examining \
             SRs in detail."
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:Int "free_space"
            "Number of bytes free on the backing storage (in bytes)"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:Int "total_space"
            "Total physical size of the backing storage (in bytes)"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:Bool "clustered"
            "Indicates whether the SR uses clustered local storage."
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:health "health"
            "The health status of the SR."
        ]
      ()
end

module Probe_result = struct
  let t =
    let lifecycle =
      [(Prototyped, rel_kolkata, ""); (Published, rel_lima, "")]
    in
    create_obj ~in_db:false ~persist:PersistNothing
      ~gen_constructor_destructor:false ~lifecycle ~in_oss_since:None
      ~name:_probe_result
      ~descr:
        "A set of properties that describe one result element of SR.probe. \
         Result elements and properties can change dynamically based on \
         changes to the the SR.probe input-parameters or the target."
      ~gen_events:false ~messages:[] ~doccomments:[]
      ~messages_default_allowed_roles:(Some [])
        (* No messages, so no roles allowed to use them *)
      ~contents:
        [
          field ~qualifier:DynamicRO ~lifecycle
            ~ty:(Map (String, String))
            "configuration"
            "Plugin-specific configuration which describes where and how to \
             locate the storage repository. This may include the physical \
             block device name, a remote NFS server and path or an RBD storage \
             pool."
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:Bool "complete"
            "True if this configuration is complete and can be used to call \
             SR.create. False if it requires further iterative calls to \
             SR.probe, to potentially narrow down on a configuration that can \
             be used."
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:(Option (Record _sr_stat))
            "sr" "Existing SR found for this configuration"
        ; field ~qualifier:DynamicRO ~lifecycle
            ~ty:(Map (String, String))
            "extra_info"
            "Additional plugin-specific information about this configuration, \
             that might be of use for an API user. This can for example \
             include the LUN or the WWPN."
        ]
      ()
end

module SR = struct
  let operations =
    Enum
      ( "storage_operations"
      , [
          ("scan", "Scanning backends for new or deleted VDIs")
        ; ("destroy", "Destroying the SR")
        ; ("forget", "Forgetting about SR")
        ; ("plug", "Plugging a PBD into this SR")
        ; ("unplug", "Unplugging a PBD from this SR")
        ; ("update", "Refresh the fields on the SR")
        ; ("vdi_create", "Creating a new VDI")
        ; ("vdi_introduce", "Introducing a new VDI")
        ; ("vdi_destroy", "Destroying a VDI")
        ; ("vdi_resize", "Resizing a VDI")
        ; ("vdi_clone", "Cloneing a VDI")
        ; ("vdi_snapshot", "Snapshotting a VDI")
        ; ("vdi_mirror", "Mirroring a VDI")
        ; ("vdi_enable_cbt", "Enabling changed block tracking for a VDI")
        ; ("vdi_disable_cbt", "Disabling changed block tracking for a VDI")
        ; ("vdi_data_destroy", "Deleting the data of the VDI")
        ; ( "vdi_list_changed_blocks"
          , "Exporting a bitmap that shows the changed blocks between two VDIs"
          )
        ; ("vdi_set_on_boot", "Setting the on_boot field of the VDI")
        ; ("vdi_blocked", "Blocking other operations for a VDI")
        ; ("vdi_copy", "Copying the VDI")
        ; ("vdi_force_unlock", "Forcefully unlocking the VDI")
        ; ("vdi_forget", "Forgetting about the VDI")
        ; ("vdi_generate_config", "Generating the configuration of the VDI")
        ; ("vdi_resize_online", "Resizing the VDI online")
        ; ("vdi_update", "Refreshing the fields on the VDI")
        ; ("pbd_create", "Creating a PBD for this SR")
        ; ("pbd_destroy", "Destroying one of this SR's PBDs")
        ]
      )

  let dev_config_param =
    {
      param_type= Map (String, String)
    ; param_name= "device_config"
    ; param_doc=
        "The device config string that will be passed to backend SR driver"
    ; param_release= rio_release
    ; param_default= None
    }

  let host_param =
    {
      param_type= Ref _host
    ; param_name= "host"
    ; param_doc= "The host to create/make the SR on"
    ; param_release= rio_release
    ; param_default= None
    }

  let physical_size_param =
    {
      param_type= Int
    ; param_name= "physical_size"
    ; param_doc= "The physical size of the new storage repository"
    ; param_release= rio_release
    ; param_default= None
    }

  let shared_param =
    {
      param_type= Bool
    ; param_name= "shared"
    ; param_doc= "True if the SR (is capable of) being shared by multiple hosts"
    ; param_release= rio_release
    ; param_default= None
    }

  let create_common =
    [
      {
        param_type= String
      ; param_name= "name_label"
      ; param_doc= "The name of the new storage repository"
      ; param_release= rio_release
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "name_description"
      ; param_doc= "The description of the new storage repository"
      ; param_release= rio_release
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "type"
      ; param_doc=
          "The type of the SR; used to specify the SR backend driver to use"
      ; param_release= rio_release
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "content_type"
      ; param_doc= "The type of the new SRs content, if required (e.g. ISOs)"
      ; param_release= rio_release
      ; param_default= None
      }
    ]

  let sm_config =
    {
      param_type= Map (String, String)
    ; param_name= "sm_config"
    ; param_doc= "Storage backend specific configuration options"
    ; param_release= miami_release
    ; param_default= Some (VMap [])
    }

  let create =
    call ~name:"create" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Create a new Storage Repository and introduce it into the managed \
             system, creating both SR record and PBD record to attach it to \
             current host (with specified device_config parameters)"
          )
        ]
      ~versioned_params:
        (host_param
        :: dev_config_param
        :: physical_size_param
        :: (create_common @ [shared_param; sm_config])
        )
      ~doc:
        "Create a new Storage Repository and introduce it into the managed \
         system, creating both SR record and PBD record to attach it to \
         current host (with specified device_config parameters)"
      ~result:(Ref _sr, "The reference of the newly created Storage Repository.")
      ~errs:[Api_errors.sr_unknown_driver]
      ~allowed_roles:_R_POOL_OP ()

  let destroy_self_param = (Ref _sr, "sr", "The SR to destroy")

  let destroy =
    call ~name:"destroy" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Destroy specified SR, removing SR-record from database and remove \
             SR from disk. (In order to affect this operation the appropriate \
             device_config is read from the specified SR's PBD on current \
             host)"
          )
        ]
      ~doc:
        "Destroy specified SR, removing SR-record from database and remove SR \
         from disk. (In order to affect this operation the appropriate \
         device_config is read from the specified SR's PBD on current host)"
      ~errs:[Api_errors.sr_has_pbd] ~params:[destroy_self_param]
      ~allowed_roles:_R_POOL_OP ()

  let forget =
    call ~name:"forget" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Removing specified SR-record from database, without attempting to \
             remove SR from disk"
          )
        ]
      ~doc:
        "Removing specified SR-record from database, without attempting to \
         remove SR from disk"
      ~params:[destroy_self_param] ~errs:[Api_errors.sr_has_pbd]
      ~allowed_roles:_R_POOL_OP ()

  let introduce =
    call ~name:"introduce" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Introduce a new Storage Repository into the managed system"
          )
        ]
      ~versioned_params:
        ({
           param_type= String
         ; param_name= "uuid"
         ; param_doc= "The uuid assigned to the introduced SR"
         ; param_release= rio_release
         ; param_default= None
         }
        :: (create_common @ [shared_param; sm_config])
        )
      ~doc:"Introduce a new Storage Repository into the managed system"
      ~result:
        (Ref _sr, "The reference of the newly introduced Storage Repository.")
      ~allowed_roles:_R_POOL_OP ()

  let probe =
    call ~name:"probe" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_miami
          , "Perform a backend-specific scan, using the given device_config.  \
             If the device_config is complete, then this will return a list of \
             the SRs present of this type on the device, if any.  If the \
             device_config is partial, then a backend-specific scan will be \
             performed, returning results that will guide the user in \
             improving the device_config."
          )
        ]
      ~versioned_params:
        [
          host_param
        ; dev_config_param
        ; {
            param_type= String
          ; param_name= "type"
          ; param_doc=
              "The type of the SR; used to specify the SR backend driver to use"
          ; param_release= miami_release
          ; param_default= None
          }
        ; sm_config
        ]
      ~doc:
        "Perform a backend-specific scan, using the given device_config.  If \
         the device_config is complete, then this will return a list of the \
         SRs present of this type on the device, if any.  If the device_config \
         is partial, then a backend-specific scan will be performed, returning \
         results that will guide the user in improving the device_config."
      ~result:
        ( String
        , "An XML fragment containing the scan results.  These are specific to \
           the scan being performed, and the backend."
        )
      ~allowed_roles:_R_POOL_OP ()

  let probe_ext =
    call ~name:"probe_ext" ~in_oss_since:None
      ~lifecycle:[(Prototyped, rel_kolkata, ""); (Published, rel_lima, "")]
      ~versioned_params:
        [
          {host_param with param_release= kolkata_release}
        ; {dev_config_param with param_release= kolkata_release}
        ; {
            param_type= String
          ; param_name= "type"
          ; param_doc=
              "The type of the SR; used to specify the SR backend driver to use"
          ; param_release= kolkata_release
          ; param_default= None
          }
        ; {sm_config with param_release= kolkata_release}
        ]
      ~doc:
        "Perform a backend-specific scan, using the given device_config.  If \
         the device_config is complete, then this will return a list of the \
         SRs present of this type on the device, if any.  If the device_config \
         is partial, then a backend-specific scan will be performed, returning \
         results that will guide the user in improving the device_config."
      ~result:
        ( Set (Record _probe_result)
        , "A set of records containing the scan results."
        )
      ~allowed_roles:_R_POOL_OP ()

  let make =
    call ~name:"make" ~in_oss_since:None
      ~lifecycle:
        [
          (Published, rel_rio, "Create a new Storage Repository on disk")
        ; (Deprecated, rel_miami, "Use SR.create instead")
        ]
      ~versioned_params:
        (host_param
        :: dev_config_param
        :: physical_size_param
        :: (create_common @ [sm_config])
        )
      ~doc:
        "Create a new Storage Repository on disk. This call is deprecated: use \
         SR.create instead."
      ~result:(String, "The uuid of the newly created Storage Repository.")
      ~allowed_roles:_R_POOL_OP ()

  let get_supported_types =
    call ~name:"get_supported_types"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Return a set of all the SR types supported by the system"
          )
        ]
      ~flags:[`Session]
      ~doc:"Return a set of all the SR types supported by the system" ~params:[]
      ~result:(Set String, "the supported SR types")
      ~allowed_roles:_R_READ_ONLY ()

  let scan =
    call ~name:"scan"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Refreshes the list of VDIs associated with an SR"
          )
        ]
      ~doc:"Refreshes the list of VDIs associated with an SR"
      ~params:[(Ref _sr, "sr", "The SR to scan")]
      ~allowed_roles:_R_VM_POWER_ADMIN ()

  (* Nb, although this is a new explicit call, it's actually been in the API since rio - just autogenerated. So no setting of rel_miami. *)
  let set_shared =
    call ~name:"set_shared"
      ~lifecycle:[(Published, rel_rio, "Sets the shared flag on the SR")]
      ~doc:"Sets the shared flag on the SR"
      ~params:
        [(Ref _sr, "sr", "The SR"); (Bool, "value", "True if the SR is shared")]
      ~allowed_roles:_R_POOL_OP ()

  let set_name_label =
    call ~name:"set_name_label"
      ~lifecycle:[(Published, rel_rio, "Set the name label of the SR")]
      ~doc:"Set the name label of the SR"
      ~params:
        [
          (Ref _sr, "sr", "The SR")
        ; (String, "value", "The name label for the SR")
        ]
      ~allowed_roles:_R_POOL_OP ()

  let set_name_description =
    call ~name:"set_name_description"
      ~lifecycle:[(Published, rel_rio, "Set the name description of the SR")]
      ~doc:"Set the name description of the SR"
      ~params:
        [
          (Ref _sr, "sr", "The SR")
        ; (String, "value", "The name description for the SR")
        ]
      ~allowed_roles:_R_POOL_OP ()

  let create_new_blob =
    call ~name:"create_new_blob"
      ~lifecycle:
        [
          ( Published
          , rel_orlando
          , "Create a placeholder for a named binary blob of data that is \
             associated with this SR"
          )
        ]
      ~doc:
        "Create a placeholder for a named binary blob of data that is \
         associated with this SR"
      ~versioned_params:
        [
          {
            param_type= Ref _sr
          ; param_name= "sr"
          ; param_doc= "The SR"
          ; param_release= orlando_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "name"
          ; param_doc= "The name associated with the blob"
          ; param_release= orlando_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "mime_type"
          ; param_doc=
              "The mime type for the data. Empty string translates to \
               application/octet-stream"
          ; param_release= orlando_release
          ; param_default= None
          }
        ; {
            param_type= Bool
          ; param_name= "public"
          ; param_doc= "True if the blob should be publicly available"
          ; param_release= tampa_release
          ; param_default= Some (VBool false)
          }
        ]
      ~result:
        (Ref _blob, "The reference of the blob, needed for populating its data")
      ~allowed_roles:_R_POOL_OP ()

  let get_data_sources =
    call ~name:"get_data_sources" ~in_oss_since:None
      ~lifecycle:[(Published, rel_dundee, "")]
      ~doc:""
      ~result:(Set (Record _data_source), "A set of data sources")
      ~params:[(Ref _sr, "sr", "The SR to interrogate")]
      ~errs:[] ~flags:[`Session] ~allowed_roles:_R_READ_ONLY ()

  let record_data_source =
    call ~name:"record_data_source" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_dundee, "Start recording the specified data source")]
      ~doc:"Start recording the specified data source"
      ~params:
        [
          (Ref _sr, "sr", "The SR")
        ; (String, "data_source", "The data source to record")
        ]
      ~errs:[] ~flags:[`Session] ~allowed_roles:_R_POOL_OP ()

  let query_data_source =
    call ~name:"query_data_source" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_dundee
          , "Query the latest value of the specified data source"
          )
        ]
      ~doc:"Query the latest value of the specified data source"
      ~params:
        [
          (Ref _sr, "sr", "The SR")
        ; (String, "data_source", "The data source to query")
        ]
      ~result:(Float, "The latest value, averaged over the last 5 seconds")
      ~errs:[] ~flags:[`Session] ~allowed_roles:_R_READ_ONLY ()

  let forget_data_source_archives =
    call ~name:"forget_data_source_archives" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_dundee
          , "Forget the recorded statistics related to the specified data \
             source"
          )
        ]
      ~doc:"Forget the recorded statistics related to the specified data source"
      ~params:
        [
          (Ref _sr, "sr", "The SR")
        ; ( String
          , "data_source"
          , "The data source whose archives are to be forgotten"
          )
        ]
      ~flags:[`Session] ~allowed_roles:_R_POOL_OP ()

  let set_virtual_allocation =
    call ~name:"set_virtual_allocation" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_miami, "Sets the SR's virtual_allocation field")]
      ~params:
        [
          (Ref _sr, "self", "The SR to modify")
        ; (Int, "value", "The new value of the SR's virtual_allocation")
        ]
      ~flags:[`Session] ~doc:"Sets the SR's virtual_allocation field"
      ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let set_physical_size =
    call ~name:"set_physical_size" ~in_oss_since:None
      ~lifecycle:[(Published, rel_miami, "Sets the SR's physical_size field")]
      ~params:
        [
          (Ref _sr, "self", "The SR to modify")
        ; (Int, "value", "The new value of the SR's physical_size")
        ]
      ~flags:[`Session] ~doc:"Sets the SR's physical_size field"
      ~allowed_roles:_R_POOL_OP ()

  let set_physical_utilisation =
    call ~name:"set_physical_utilisation" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_miami, "Sets the SR's physical_utilisation field")]
      ~flags:[`Session]
      ~params:
        [
          (Ref _sr, "self", "The SR to modify")
        ; (Int, "value", "The new value of the SR's physical utilisation")
        ]
      ~doc:"Sets the SR's physical_utilisation field" ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let update =
    call ~name:"update" ~in_oss_since:None
      ~lifecycle:[(Published, rel_symc, "Refresh the fields on the SR object")]
      ~params:[(Ref _sr, "sr", "The SR whose fields should be refreshed")]
      ~doc:"Refresh the fields on the SR object" ~allowed_roles:_R_POOL_OP ()

  let assert_can_host_ha_statefile =
    call ~name:"assert_can_host_ha_statefile" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_orlando
          , "Returns successfully if the given SR can host an HA statefile. \
             Otherwise returns an error to explain why not"
          )
        ]
      ~params:[(Ref _sr, "sr", "The SR to query")]
      ~doc:
        "Returns successfully if the given SR can host an HA statefile. \
         Otherwise returns an error to explain why not"
      ~allowed_roles:_R_POOL_OP ()

  let assert_supports_database_replication =
    call ~name:"assert_supports_database_replication" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Returns successfully if the given SR supports database \
             replication. Otherwise returns an error to explain why not."
          )
        ]
      ~params:[(Ref _sr, "sr", "The SR to query")]
      ~doc:
        "Returns successfully if the given SR supports database replication. \
         Otherwise returns an error to explain why not."
      ~allowed_roles:_R_POOL_OP ()

  let enable_database_replication =
    call ~name:"enable_database_replication" ~in_oss_since:None
      ~lifecycle:[(Published, rel_boston, "")]
      ~params:[(Ref _sr, "sr", "The SR to which metadata should be replicated")]
      ~allowed_roles:_R_POOL_OP ()

  let disable_database_replication =
    call ~name:"disable_database_replication" ~in_oss_since:None
      ~lifecycle:[(Published, rel_boston, "")]
      ~params:
        [
          ( Ref _sr
          , "sr"
          , "The SR to which metadata should be no longer replicated"
          )
        ]
      ~allowed_roles:_R_POOL_OP ()

  let get_live_hosts =
    call ~in_oss_since:None ~name:"get_live_hosts"
      ~lifecycle:
        [(Published, rel_stockholm, "Get all live hosts attached to this SR")]
      ~doc:"Get all live hosts attached to this SR"
      ~params:[(Ref _sr, "sr", "The SR from which to query attached hosts")]
      ~allowed_roles:_R_POOL_OP ~hide_from_docs:true
      ~result:(Set (Ref _host), "A collection of live hosts attached to this SR")
      ()

  (** A storage repository. Note we overide default create/destroy methods with our own here... *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A storage repository")]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_sr ~descr:"A storage repository"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [
          create
        ; introduce
        ; make
        ; destroy
        ; forget
        ; update
        ; get_supported_types
        ; scan
        ; probe
        ; probe_ext
        ; set_shared
        ; set_name_label
        ; set_name_description
        ; create_new_blob
        ; set_physical_size
        ; set_virtual_allocation
        ; set_physical_utilisation
        ; assert_can_host_ha_statefile
        ; assert_supports_database_replication
        ; enable_database_replication
        ; disable_database_replication
        ; get_data_sources
        ; record_data_source
        ; query_data_source
        ; forget_data_source_archives
        ; get_live_hosts
        ]
      ~contents:
        ([
           uid
             ~lifecycle:
               [(Published, rel_rio, "Unique identifier/object reference")]
             _sr
         ; namespace ~name:"name"
             ~contents:
               (names oss_since_303 StaticRO
                  ~lifecycle:[(Published, rel_rio, "")]
               )
             ()
         ]
        @ allowed_and_current_operations operations
        @ [
            field ~ty:(Set (Ref _vdi)) ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "all virtual disks known to this storage repository"
                  )
                ]
              "VDIs" "all virtual disks known to this storage repository"
          ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pbd))
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "describes how particular hosts can see this storage \
                     repository"
                  )
                ]
              "PBDs"
              "describes how particular hosts can see this storage repository"
          ; field ~ty:Int ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "sum of virtual_sizes of all VDIs in this storage \
                     repository (in bytes)"
                  )
                ]
              "virtual_allocation"
              "sum of virtual_sizes of all VDIs in this storage repository (in \
               bytes)"
          ; field ~ty:Int ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "physical space currently utilised on this storage \
                     repository (in bytes). Note that for sparse disk formats, \
                     physical_utilisation may be less than virtual_allocation"
                  )
                ]
              "physical_utilisation"
              "physical space currently utilised on this storage repository \
               (in bytes). Note that for sparse disk formats, \
               physical_utilisation may be less than virtual_allocation"
          ; field ~ty:Int ~qualifier:StaticRO
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "total physical size of the repository (in bytes)"
                  )
                ]
              "physical_size" "total physical size of the repository (in bytes)"
          ; field ~qualifier:StaticRO
              ~lifecycle:
                [(Published, rel_rio, "type of the storage repository")]
              "type" "type of the storage repository"
          ; field ~qualifier:StaticRO
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "the type of the SR's content, if required (e.g. ISOs)"
                  )
                ]
              "content_type"
              "the type of the SR's content, if required (e.g. ISOs)"
          ; field ~qualifier:DynamicRO "shared" ~ty:Bool
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "true if this SR is (capable of being) shared between \
                     multiple hosts"
                  )
                ]
              "true if this SR is (capable of being) shared between multiple \
               hosts"
          ; field
              ~ty:(Map (String, String))
              "other_config" "additional configuration"
              ~map_keys_roles:
                [("folder", _R_VM_OP); ("XenCenter.CustomFields.*", _R_VM_OP)]
              ~lifecycle:[(Published, rel_rio, "additional configuration")]
          ; field ~writer_roles:_R_VM_OP
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "user-specified tags for categorization purposes"
                  )
                ]
              ~default_value:(Some (VSet [])) ~ty:(Set String) "tags"
              "user-specified tags for categorization purposes"
          ; field ~ty:Bool ~qualifier:DynamicRO ~in_oss_since:None
              ~lifecycle:[(Published, rel_rio, "")]
              ~internal_only:true "default_vdi_visibility" ""
          ; field ~in_oss_since:None
              ~ty:(Map (String, String))
              ~lifecycle:[(Published, rel_miami, "SM dependent data")]
              ~qualifier:RW "sm_config" "SM dependent data"
              ~default_value:(Some (VMap []))
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "Binary blobs associated with this SR"
                  )
                ]
              ~ty:(Map (String, Ref _blob))
              ~default_value:(Some (VMap [])) "blobs"
              "Binary blobs associated with this SR"
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_cowley
                  , "True if this SR is assigned to be the local cache for its \
                     host"
                  )
                ]
              ~ty:Bool ~default_value:(Some (VBool false)) "local_cache_enabled"
              "True if this SR is assigned to be the local cache for its host"
          ; field ~qualifier:DynamicRO
              ~lifecycle:
                [
                  ( Published
                  , rel_boston
                  , "The disaster recovery task which introduced this SR"
                  )
                ]
              ~ty:(Ref _dr_task) ~default_value:(Some (VRef null_ref))
              "introduced_by"
              "The disaster recovery task which introduced this SR"
          ; field ~qualifier:DynamicRO
              ~lifecycle:[(Published, rel_dundee, "")]
              ~ty:Bool ~default_value:(Some (VBool false)) "clustered"
              "True if the SR is using aggregated local storage"
          ; field ~qualifier:DynamicRO
              ~lifecycle:[(Published, rel_dundee, "")]
              ~ty:Bool ~default_value:(Some (VBool false)) "is_tools_sr"
              "True if this is the SR that contains the Tools ISO VDIs"
          ]
        )
      ()
end

module SM = struct
  (** XXX: just make this a field and be done with it. Cowardly refusing to change the schema for now. *)
  let get_driver_filename =
    call ~name:"get_driver_filename" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_orlando, "Gets the SM's driver_filename field")]
      ~params:[(Ref _sm, "self", "The SM to query")]
      ~result:(String, "The SM's driver_filename field")
      ~doc:"Gets the SM's driver_filename field" ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A storage manager plugin")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_sm
      ~descr:"A storage manager plugin" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _sm
        ; namespace ~name:"name"
            ~contents:
              (names None DynamicRO ~lifecycle:[(Published, rel_rio, "")])
            ()
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "SR.type")]
            ~qualifier:DynamicRO "type" "SR.type"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Vendor who created this plugin")]
            ~qualifier:DynamicRO "vendor" "Vendor who created this plugin"
        ; field ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Entity which owns the copyright of this plugin"
                )
              ]
            ~qualifier:DynamicRO "copyright"
            "Entity which owns the copyright of this plugin"
        ; field ~in_oss_since:None
            ~lifecycle:[(Published, rel_rio, "Version of the plugin")]
            ~qualifier:DynamicRO "version" "Version of the plugin"
        ; field ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Minimum SM API version required on the server"
                )
              ]
            ~qualifier:DynamicRO "required_api_version"
            "Minimum SM API version required on the server"
        ; field ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "names and descriptions of device config keys"
                )
              ]
            ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            "configuration" "names and descriptions of device config keys"
        ; field ~in_oss_since:None ~qualifier:DynamicRO
            ~lifecycle:
              [
                (Published, rel_miami, "")
              ; (Deprecated, rel_clearwater, "Use SM.features instead")
              ]
            ~ty:(Set String) "capabilities" "capabilities of the SM plugin"
            ~default_value:(Some (VSet []))
        ; field ~in_oss_since:None ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_clearwater
                , "capabilities of the SM plugin, with capability version \
                   numbers"
                )
              ; ( Changed
                , "24.37.0"
                , "features are now pool-wide, instead of what is available on \
                   the coordinator sm"
                )
              ]
            ~ty:(Map (String, Int))
            "features"
            "capabilities of the SM plugin, with capability version numbers"
            ~default_value:(Some (VMap []))
        ; field ~in_oss_since:None ~qualifier:DynamicRO ~lifecycle:[]
            ~ty:(Map (Ref _host, Set String))
            ~internal_only:true "host_pending_features"
            "SM features that are waiting to be declared per host."
            ~default_value:(Some (VMap []))
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ; field
            ~lifecycle:
              [(Published, rel_orlando, "filename of the storage driver")]
            ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String
            "driver_filename" "filename of the storage driver"
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_dundee
                , "The storage plugin requires that one of these cluster \
                   stacks is configured and running."
                )
              ]
            ~qualifier:DynamicRO ~default_value:(Some (VSet []))
            ~ty:(Set String) "required_cluster_stack"
            "The storage plugin requires that one of these cluster stacks is \
             configured and running."
        ]
      ()
end

module LVHD = struct
  let enable_thin_provisioning =
    call ~name:"enable_thin_provisioning" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_dundee
          , "Upgrades an LVHD SR to enable thin-provisioning. Future VDIs \
             created in this SR will be thinly-provisioned, although existing \
             VDIs will be left alone. Note that the SR must be attached to the \
             SRmaster for upgrade to work."
          )
        ]
      ~allowed_roles:_R_POOL_ADMIN
      ~params:
        [
          ( Ref _host
          , "host"
          , "The LVHD Host to upgrade to being thin-provisioned."
          )
        ; (Ref _sr, "SR", "The LVHD SR to upgrade to being thin-provisioned.")
        ; ( Int
          , "initial_allocation"
          , "The initial amount of space to allocate to a newly-created VDI in \
             bytes"
          )
        ; ( Int
          , "allocation_quantum"
          , "The amount of space to allocate to a VDI when it needs to be \
             enlarged in bytes"
          )
        ]
      ~doc:
        "Upgrades an LVHD SR to enable thin-provisioning. Future VDIs created \
         in this SR will be thinly-provisioned, although existing VDIs will be \
         left alone. Note that the SR must be attached to the SRmaster for \
         upgrade to work."
      ~forward_to:(HostExtension "LVHD.enable_thin_provisioning")
      ~result:(String, "Message from LVHD.enable_thin_provisioning extension")
      ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_dundee, "LVHD SR specific operations")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_lvhd
      ~descr:"LVHD SR specific operations" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages:[enable_thin_provisioning]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _lvhd
        ]
      ()
end

module Vdi_nbd_server_info = struct
  let t =
    let lifecycle = [(Published, rel_inverness, "")] in
    create_obj ~in_db:false ~persist:PersistNothing
      ~gen_constructor_destructor:false ~lifecycle ~in_oss_since:None
      ~name:_vdi_nbd_server_info
      ~descr:
        "Details for connecting to a VDI using the Network Block Device \
         protocol"
      ~gen_events:false ~messages:[] ~doccomments:[]
      ~messages_default_allowed_roles:(Some [])
        (* No messages, so no roles allowed to use them *)
      ~contents:
        [
          (* uid _vdi_nbd_server_info; The uuid is not needed here and only adds inconvenience. *)
          field ~qualifier:DynamicRO ~lifecycle ~ty:String "exportname"
            "The exportname to request over NBD. This holds details including \
             an authentication token, so it must be protected appropriately. \
             Clients should regard the exportname as an opaque string or \
             token."
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:String "address"
            "An address on which the server can be reached; this can be IPv4, \
             IPv6, or a DNS name."
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:Int "port" "The TCP port"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:String "cert"
            "The TLS certificate of the server"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:String "subject"
            "For convenience, this redundant field holds a DNS (hostname) \
             subject of the certificate. This can be a wildcard, but only for \
             a certificate that has a wildcard subject and no concrete \
             hostname subjects."
        ]
      ()
end

module VDI = struct
  (** Each disk is associated with a vdi_type: (a 'style' of disk?) *)
  let type' =
    Enum
      ( "vdi_type"
      , [
          ("system", "a disk that may be replaced on upgrade")
        ; ("user", "a disk that is always preserved on upgrade")
        ; ("ephemeral", "a disk that may be reformatted on upgrade")
        ; ("suspend", "a disk that stores a suspend image")
        ; ("crashdump", "a disk that stores VM crashdump information")
        ; ("ha_statefile", "a disk used for HA storage heartbeating")
        ; ("metadata", "a disk used for HA Pool metadata")
        ; ("redo_log", "a disk used for a general metadata redo-log")
        ; ("rrd", "a disk that stores SR-level RRDs")
        ; ("pvs_cache", "a disk that stores PVS cache data")
        ; ( "cbt_metadata"
          , "Metadata about a snapshot VDI that has been deleted: the set of \
             blocks that changed between some previous version of the disk and \
             the version tracked by the snapshot."
          )
        ]
      )

  let snapshot =
    call ~name:"snapshot" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Take a read-only snapshot of the VDI, returning a reference to \
             the snapshot. If any driver_params are specified then these are \
             passed through to the storage-specific substrate driver that \
             takes the snapshot. NB the snapshot lives in the same Storage \
             Repository as its parent."
          )
        ]
      ~versioned_params:
        [
          {
            param_type= Ref _vdi
          ; param_name= "vdi"
          ; param_doc= "The VDI to snapshot"
          ; param_release= rio_release
          ; param_default= None
          }
        ; {
            param_type= Map (String, String)
          ; param_name= "driver_params"
          ; param_doc=
              "Optional parameters that can be passed through to backend \
               driver in order to specify storage-type-specific snapshot \
               options"
          ; param_release= miami_release
          ; param_default= Some (VMap [])
          }
        ]
      ~doc:
        "Take a read-only snapshot of the VDI, returning a reference to the \
         snapshot. If any driver_params are specified then these are passed \
         through to the storage-specific substrate driver that takes the \
         snapshot. NB the snapshot lives in the same Storage Repository as its \
         parent."
      ~result:(Ref _vdi, "The ID of the newly created VDI.")
      ~allowed_roles:_R_VM_ADMIN ~doc_tags:[Snapshots] ()

  let clone =
    call ~name:"clone" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Take an exact copy of the VDI and return a reference to the new \
             disk. If any driver_params are specified then these are passed \
             through to the storage-specific substrate driver that implements \
             the clone operation. NB the clone lives in the same Storage \
             Repository as its parent."
          )
        ]
      ~params:[(Ref _vdi, "vdi", "The VDI to clone")]
      ~versioned_params:
        [
          {
            param_type= Ref _vdi
          ; param_name= "vdi"
          ; param_doc= "The VDI to clone"
          ; param_release= rio_release
          ; param_default= None
          }
        ; {
            param_type= Map (String, String)
          ; param_name= "driver_params"
          ; param_doc=
              "Optional parameters that are passed through to the backend \
               driver in order to specify storage-type-specific clone options"
          ; param_release= miami_release
          ; param_default= Some (VMap [])
          }
        ]
      ~doc:
        "Take an exact copy of the VDI and return a reference to the new disk. \
         If any driver_params are specified then these are passed through to \
         the storage-specific substrate driver that implements the clone \
         operation. NB the clone lives in the same Storage Repository as its \
         parent."
      ~result:(Ref _vdi, "The ID of the newly created VDI.")
      ~allowed_roles:_R_VM_ADMIN ~doc_tags:[Snapshots] ()

  let resize =
    call ~name:"resize"
      ~lifecycle:[(Published, rel_rio, "Resize the VDI.")]
      ~in_oss_since:None
      ~params:
        [
          (Ref _vdi, "vdi", "The VDI to resize")
        ; (Int, "size", "The new size of the VDI")
        ]
      ~doc:"Resize the VDI." ~allowed_roles:_R_VM_ADMIN ()

  let resize_online =
    call ~name:"resize_online" ~in_oss_since:None
      ~lifecycle:
        [
          (Published, rel_rio, "")
        ; (Deprecated, rel_inverness, "Dummy transition")
        ; ( Removed
          , rel_inverness
          , "Online VDI resize is not supported by any of the storage backends."
          )
        ]
      ~params:
        [
          (Ref _vdi, "vdi", "The VDI to resize")
        ; (Int, "size", "The new size of the VDI")
        ]
      ~doc:"Resize the VDI which may or may not be attached to running guests."
      ~allowed_roles:_R_VM_ADMIN ()

  let copy =
    call ~name:"copy"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Copies a VDI to an SR. There must be a host that can see both the \
             source and destination SRs simultaneously"
          )
        ; ( Extended
          , rel_cowley
          , "The copy can now be performed between any two SRs."
          )
        ; ( Extended
          , rel_clearwater_felton
          , "The copy can now be performed into a pre-created VDI. It is now \
             possible to request copying only changed blocks from a base VDI"
          )
        ]
      ~in_oss_since:None
      ~versioned_params:
        [
          {
            param_type= Ref _vdi
          ; param_name= "vdi"
          ; param_doc= "The VDI to copy"
          ; param_release= rio_release
          ; param_default= None
          }
        ; {
            param_type= Ref _sr
          ; param_name= "sr"
          ; param_doc=
              "The destination SR (only required if the destination VDI is not \
               specified"
          ; param_release= rio_release
          ; param_default= Some (VString null_ref)
          }
        ; {
            param_type= Ref _vdi
          ; param_name= "base_vdi"
          ; param_doc=
              "The base VDI (only required if copying only changed blocks, by \
               default all blocks will be copied)"
          ; param_release= clearwater_felton_release
          ; param_default= Some (VRef null_ref)
          }
        ; {
            param_type= Ref _vdi
          ; param_name= "into_vdi"
          ; param_doc=
              "The destination VDI to copy blocks into (if omitted then a \
               destination SR must be provided and a fresh VDI will be \
               created)"
          ; param_release= clearwater_felton_release
          ; param_default= Some (VString null_ref)
          }
        ]
      ~doc:
        "Copy either a full VDI or the block differences between two VDIs into \
         either a fresh VDI or an existing VDI."
      ~errs:
        [
          Api_errors.vdi_readonly
        ; Api_errors.vdi_too_small
        ; Api_errors.vdi_not_sparse
        ]
      ~result:
        (Ref _vdi, "The reference of the VDI where the blocks were written.")
      ~allowed_roles:_R_VM_ADMIN ()

  let pool_migrate =
    call ~name:"pool_migrate" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Migrate a VDI, which may be attached to a running guest, to a \
             different SR. The destination SR must be visible to the guest."
          )
        ]
      ~params:
        [
          (Ref _vdi, "vdi", "The VDI to migrate")
        ; (Ref _sr, "sr", "The destination SR")
        ; (Map (String, String), "options", "Other parameters")
        ]
      ~result:(Ref _vdi, "The new reference of the migrated VDI.")
      ~doc:
        "Migrate a VDI, which may be attached to a running guest, to a \
         different SR. The destination SR must be visible to the guest."
      ~allowed_roles:_R_VM_POWER_ADMIN ()

  let introduce_params first_rel =
    [
      {
        param_type= String
      ; param_name= "uuid"
      ; param_doc= "The uuid of the disk to introduce"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "name_label"
      ; param_doc= "The name of the disk record"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "name_description"
      ; param_doc= "The description of the disk record"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Ref _sr
      ; param_name= "SR"
      ; param_doc= "The SR that the VDI is in"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= type'
      ; param_name= "type"
      ; param_doc= "The type of the VDI"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Bool
      ; param_name= "sharable"
      ; param_doc= "true if this disk may be shared"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Bool
      ; param_name= "read_only"
      ; param_doc= "true if this disk may ONLY be mounted read-only"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Map (String, String)
      ; param_name= "other_config"
      ; param_doc= "additional configuration"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= String
      ; param_name= "location"
      ; param_doc= "location information"
      ; param_release= first_rel
      ; param_default= None
      }
    ; {
        param_type= Map (String, String)
      ; param_name= "xenstore_data"
      ; param_doc= "Data to insert into xenstore"
      ; param_release= first_rel
      ; param_default= Some (VMap [])
      }
    ; {
        param_type= Map (String, String)
      ; param_name= "sm_config"
      ; param_doc= "Storage-specific config"
      ; param_release= miami_release
      ; param_default= Some (VMap [])
      }
    ; {
        param_type= Bool
      ; param_name= "managed"
      ; param_doc= "Storage-specific config"
      ; param_release= tampa_release
      ; param_default= Some (VBool true)
      }
    ; {
        param_type= Int
      ; param_name= "virtual_size"
      ; param_doc= "Storage-specific config"
      ; param_release= tampa_release
      ; param_default= Some (VInt 0L)
      }
    ; {
        param_type= Int
      ; param_name= "physical_utilisation"
      ; param_doc= "Storage-specific config"
      ; param_release= tampa_release
      ; param_default= Some (VInt 0L)
      }
    ; {
        param_type= Ref _pool
      ; param_name= "metadata_of_pool"
      ; param_doc= "Storage-specific config"
      ; param_release= tampa_release
      ; param_default= Some (VRef "")
      }
    ; {
        param_type= Bool
      ; param_name= "is_a_snapshot"
      ; param_doc= "Storage-specific config"
      ; param_release= tampa_release
      ; param_default= Some (VBool false)
      }
    ; {
        param_type= DateTime
      ; param_name= "snapshot_time"
      ; param_doc=
          "Storage-specific config. When the timezone is missing, UTC is \
           assumed"
      ; param_release= tampa_release
      ; param_default= Some (VDateTime Date.epoch)
      }
    ; {
        param_type= Ref _vdi
      ; param_name= "snapshot_of"
      ; param_doc= "Storage-specific config"
      ; param_release= tampa_release
      ; param_default= Some (VRef "")
      }
    ]

  (* This used to be called VDI.introduce but it was always an internal call *)
  let pool_introduce =
    call ~name:"pool_introduce" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_rio, "Create a new VDI record in the database only")]
      ~versioned_params:
        (introduce_params miami_release
        @ [
            {
              param_type= Bool
            ; param_name= "cbt_enabled"
            ; param_doc= "True if changed blocks are tracked for this VDI"
            ; param_release= inverness_release
            ; param_default= Some (VBool false)
            }
          ]
        )
      ~doc:"Create a new VDI record in the database only"
      ~result:(Ref _vdi, "The ref of the newly created VDI record.")
      ~hide_from_docs:true ~allowed_roles:_R_VM_ADMIN ()

  let db_introduce =
    {
      pool_introduce with
      msg_name= "db_introduce"
    ; msg_hide_from_docs= true
    ; msg_allowed_roles= _R_LOCAL_ROOT_ONLY
    }

  let db_forget =
    call ~name:"db_forget" ~in_oss_since:None
      ~params:[(Ref _vdi, "vdi", "The VDI to forget about")]
      ~doc:"Removes a VDI record from the database" ~hide_from_docs:true
      ~lifecycle:
        [(Published, rel_miami, "Removes a VDI record from the database")]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let introduce =
    call ~name:"introduce" ~in_oss_since:None
      ~versioned_params:(introduce_params rio_release)
      ~doc:"Create a new VDI record in the database only"
      ~result:(Ref _vdi, "The ref of the newly created VDI record.")
      ~errs:[Api_errors.sr_operation_not_supported]
      ~lifecycle:
        [(Published, rel_miami, "Create a new VDI record in the database only")]
      ~allowed_roles:_R_VM_ADMIN ()

  let forget =
    call ~name:"forget" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_rio, "Removes a VDI record from the database")]
      ~params:[(Ref _vdi, "vdi", "The VDI to forget about")]
      ~doc:"Removes a VDI record from the database" ~allowed_roles:_R_VM_ADMIN
      ()

  let force_unlock =
    call ~name:"force_unlock" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Steals the lock on this VDI and leaves it unlocked. This function \
             is extremely dangerous. This call is deprecated."
          )
        ; (Deprecated, rel_miami, "")
        ]
      ~params:[(Ref _vdi, "vdi", "The VDI to forcibly unlock")]
      ~doc:
        "Steals the lock on this VDI and leaves it unlocked. This function is \
         extremely dangerous. This call is deprecated."
      ~hide_from_docs:true ~allowed_roles:_R_VM_ADMIN ()

  let update =
    call ~name:"update" ~in_oss_since:None
      ~params:
        [(Ref _vdi, "vdi", "The VDI whose stats (eg size) should be updated")]
      ~doc:"Ask the storage backend to refresh the fields in the VDI object"
      ~errs:[Api_errors.sr_operation_not_supported]
      ~lifecycle:
        [
          ( Published
          , rel_symc
          , "Ask the storage backend to refresh the fields in the VDI object"
          )
        ]
      ~allowed_roles:_R_VM_ADMIN ()

  let operations =
    Enum
      ( "vdi_operations"
      , [
          ("clone", "Cloning the VDI")
        ; ("copy", "Copying the VDI")
        ; ("resize", "Resizing the VDI")
        ; ("resize_online", "Resizing the VDI which may or may not be online")
        ; ("snapshot", "Snapshotting the VDI")
        ; ("mirror", "Mirroring the VDI")
        ; ("destroy", "Destroying the VDI")
        ; ("forget", "Forget about the VDI")
        ; ("update", "Refreshing the fields of the VDI")
        ; ("force_unlock", "Forcibly unlocking the VDI")
        ; ("generate_config", "Generating static configuration")
        ; ("enable_cbt", "Enabling changed block tracking for a VDI")
        ; ("disable_cbt", "Disabling changed block tracking for a VDI")
        ; ("data_destroy", "Deleting the data of the VDI")
        ; ( "list_changed_blocks"
          , "Exporting a bitmap that shows the changed blocks between two VDIs"
          )
        ; ("set_on_boot", "Setting the on_boot field of the VDI")
        ; ("blocked", "Operations on this VDI are temporarily blocked")
        ]
      )

  let set_missing =
    call ~name:"set_missing" ~in_oss_since:None
      ~lifecycle:[(Published, rel_miami, "Sets the VDI's missing field")]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (Bool, "value", "The new value of the VDI's missing field")
        ]
      ~doc:"Sets the VDI's missing field" ~hide_from_docs:true ~flags:[`Session]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let set_read_only =
    call ~name:"set_read_only" ~in_oss_since:None
      ~lifecycle:[(Published, rel_rio, "Sets the VDI's read_only field")]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (Bool, "value", "The new value of the VDI's read_only field")
        ]
      ~flags:[`Session] ~doc:"Sets the VDI's read_only field"
      ~allowed_roles:_R_VM_ADMIN ()

  let set_sharable =
    call ~name:"set_sharable" ~in_oss_since:None
      ~lifecycle:[(Published, rel_george, "Sets the VDI's sharable field")]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (Bool, "value", "The new value of the VDI's sharable field")
        ]
      ~flags:[`Session] ~doc:"Sets the VDI's sharable field"
      ~allowed_roles:_R_VM_ADMIN ()

  let set_managed =
    call ~name:"set_managed" ~in_oss_since:None
      ~lifecycle:[(Published, rel_rio, "Sets the VDI's managed field")]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (Bool, "value", "The new value of the VDI's managed field")
        ]
      ~flags:[`Session] ~doc:"Sets the VDI's managed field" ~hide_from_docs:true
      ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let set_virtual_size =
    call ~name:"set_virtual_size" ~in_oss_since:None
      ~lifecycle:[(Published, rel_miami, "Sets the VDI's virtual_size field")]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (Int, "value", "The new value of the VDI's virtual size")
        ]
      ~flags:[`Session] ~doc:"Sets the VDI's virtual_size field"
      ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let set_physical_utilisation =
    call ~name:"set_physical_utilisation" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_miami, "Sets the VDI's physical_utilisation field")]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (Int, "value", "The new value of the VDI's physical utilisation")
        ]
      ~flags:[`Session] ~doc:"Sets the VDI's physical_utilisation field"
      ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let set_is_a_snapshot =
    call ~name:"set_is_a_snapshot" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_boston, "Sets whether this VDI is a snapshot")]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; ( Bool
          , "value"
          , "The new value indicating whether this VDI is a snapshot"
          )
        ]
      ~flags:[`Session] ~doc:"Sets whether this VDI is a snapshot"
      ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let set_snapshot_of =
    call ~name:"set_snapshot_of" ~in_oss_since:None
      ~lifecycle:
        [
          (Published, rel_boston, "Sets the VDI of which this VDI is a snapshot")
        ]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (Ref _vdi, "value", "The VDI of which this VDI is a snapshot")
        ]
      ~flags:[`Session] ~doc:"Sets the VDI of which this VDI is a snapshot"
      ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let set_snapshot_time =
    call ~name:"set_snapshot_time" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_boston, "Sets the snapshot time of this VDI.")]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; ( DateTime
          , "value"
          , "The snapshot time of this VDI. When the timezone is missing, UTC \
             is assumed"
          )
        ]
      ~flags:[`Session] ~doc:"Sets the snapshot time of this VDI."
      ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let set_metadata_of_pool =
    call ~name:"set_metadata_of_pool" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Records the pool whose metadata is contained by this VDI."
          )
        ]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; ( Ref _pool
          , "value"
          , "The pool whose metadata is contained by this VDI"
          )
        ]
      ~flags:[`Session]
      ~doc:"Records the pool whose metadata is contained by this VDI."
      ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  (** An API call for debugging and testing only *)
  let generate_config =
    call ~name:"generate_config" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_orlando, "Internal function for debugging only")]
      ~params:
        [
          (Ref _host, "host", "The host on which to generate the configuration")
        ; (Ref _vdi, "vdi", "The VDI to generate the configuration for")
        ]
      ~result:(String, "The generated static configuration")
      ~doc:"Internal function for debugging only" ~hide_from_docs:true
      ~allowed_roles:_R_VM_ADMIN ()

  let on_boot =
    Enum
      ( "on_boot"
      , [
          ( "reset"
          , "When a VM containing this VDI is started, the contents of the VDI \
             are reset to the state they were in when this flag was last set."
          )
        ; ("persist", "Standard behaviour.")
        ]
      )

  let set_on_boot =
    call ~name:"set_on_boot" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_cowley
          , "Set the value of the on_boot parameter. This value can only be \
             changed when the VDI is not attached to a running VM."
          )
        ]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (on_boot, "value", "The value to set")
        ]
      ~doc:
        "Set the value of the on_boot parameter. This value can only be \
         changed when the VDI is not attached to a running VM."
      ~allowed_roles:_R_VM_ADMIN ()

  let set_allow_caching =
    call ~name:"set_allow_caching" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_cowley
          , "Set the value of the allow_caching parameter. This value can only \
             be changed when the VDI is not attached to a running VM. The \
             caching behaviour is only affected by this flag for VHD-based \
             VDIs that have one parent and no child VHDs. Moreover, caching \
             only takes place when the host running the VM containing this VDI \
             has a nominated SR for local caching."
          )
        ]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (Bool, "value", "The value to set")
        ]
      ~doc:
        "Set the value of the allow_caching parameter. This value can only be \
         changed when the VDI is not attached to a running VM. The caching \
         behaviour is only affected by this flag for VHD-based VDIs that have \
         one parent and no child VHDs. Moreover, caching only takes place when \
         the host running the VM containing this VDI has a nominated SR for \
         local caching."
      ~allowed_roles:_R_VM_ADMIN ()

  let set_name_label =
    call ~name:"set_name_label" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Set the name label of the VDI. This can only happen when then its \
             SR is currently attached."
          )
        ]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (String, "value", "The name lable for the VDI")
        ]
      ~doc:
        "Set the name label of the VDI. This can only happen when then its SR \
         is currently attached."
      ~allowed_roles:_R_VM_ADMIN ()

  let set_name_description =
    call ~name:"set_name_description" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Set the name description of the VDI. This can only happen when \
             its SR is currently attached."
          )
        ]
      ~params:
        [
          (Ref _vdi, "self", "The VDI to modify")
        ; (String, "value", "The name description for the VDI")
        ]
      ~doc:
        "Set the name description of the VDI. This can only happen when its SR \
         is currently attached."
      ~allowed_roles:_R_VM_ADMIN ()

  let open_database =
    call ~name:"open_database" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Load the metadata found on the supplied VDI and return a session \
             reference which can be used in API calls to query its contents."
          )
        ]
      ~params:
        [(Ref _vdi, "self", "The VDI which contains the database to open")]
      ~result:(Ref _session, "A session which can be used to query the database")
      ~doc:
        "Load the metadata found on the supplied VDI and return a session \
         reference which can be used in API calls to query its contents."
      ~allowed_roles:_R_POOL_OP ()

  let checksum =
    call ~name:"checksum" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Internal function to calculate VDI checksum and return a string"
          )
        ]
      ~params:[(Ref _vdi, "self", "The VDI to checksum")]
      ~result:(String, "The md5sum of the vdi")
      ~doc:"Internal function to calculate VDI checksum and return a string"
      ~hide_from_docs:true ~allowed_roles:_R_VM_ADMIN
      (* Conceptually, this is not correct. We do it
         	                              this way only to follow the previous
         	                              convention. It is supposed to fix by future
         	                              version of RBAC *)
      ()

  let read_database_pool_uuid =
    call ~name:"read_database_pool_uuid" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Check the VDI cache for the pool UUID of the database on this VDI."
          )
        ]
      ~params:[(Ref _vdi, "self", "The metadata VDI to look up in the cache.")]
      ~result:(String, "The cached pool UUID of the database on the VDI.")
      ~doc:"Check the VDI cache for the pool UUID of the database on this VDI."
      ~allowed_roles:_R_READ_ONLY ()

  let enable_cbt =
    call ~name:"enable_cbt" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_inverness
          , "Enable changed block tracking for the VDI. This call is \
             idempotent - enabling CBT for a VDI for which CBT is already \
             enabled results in a no-op, and no error will be thrown."
          )
        ]
      ~params:[(Ref _vdi, "self", "The VDI for which CBT should be enabled")]
      ~errs:
        [
          Api_errors.sr_operation_not_supported
        ; Api_errors.vdi_missing
        ; Api_errors.sr_not_attached
        ; Api_errors.sr_no_pbds
        ; Api_errors.operation_not_allowed
        ; Api_errors.vdi_incompatible_type
        ; Api_errors.vdi_on_boot_mode_incompatible_with_operation
        ]
      ~doc:
        "Enable changed block tracking for the VDI. This call is idempotent - \
         enabling CBT for a VDI for which CBT is already enabled results in a \
         no-op, and no error will be thrown."
      ~allowed_roles:_R_VM_ADMIN ()

  let disable_cbt =
    call ~name:"disable_cbt" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_inverness
          , "Disable changed block tracking for the VDI. This call is only \
             allowed on VDIs that support enabling CBT. It is an idempotent \
             operation - disabling CBT for a VDI for which CBT is not enabled \
             results in a no-op, and no error will be thrown."
          )
        ]
      ~params:[(Ref _vdi, "self", "The VDI for which CBT should be disabled")]
      ~errs:
        [
          Api_errors.sr_operation_not_supported
        ; Api_errors.vdi_missing
        ; Api_errors.sr_not_attached
        ; Api_errors.sr_no_pbds
        ; Api_errors.operation_not_allowed
        ; Api_errors.vdi_incompatible_type
        ; Api_errors.vdi_on_boot_mode_incompatible_with_operation
        ]
      ~doc:
        "Disable changed block tracking for the VDI. This call is only allowed \
         on VDIs that support enabling CBT. It is an idempotent operation - \
         disabling CBT for a VDI for which CBT is not enabled results in a \
         no-op, and no error will be thrown."
      ~allowed_roles:_R_VM_ADMIN ()

  (** This command is for internal use by SM to set the cbt_enabled field when it needs to disable cbt for its own reasons. This command should be removed once SMAPIv3 is implemented *)
  let set_cbt_enabled =
    call ~name:"set_cbt_enabled" ~in_oss_since:None
      ~lifecycle:[(Published, rel_inverness, "")]
      ~params:
        [
          ( Ref _vdi
          , "self"
          , "The VDI for which CBT enabled status should be set"
          )
        ; (Bool, "value", "The value to set")
        ]
      ~errs:[] ~hide_from_docs:true ~allowed_roles:_R_LOCAL_ROOT_ONLY ()

  let data_destroy =
    call ~name:"data_destroy" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_inverness
          , "Delete the data of the snapshot VDI, but keep its changed block \
             tracking metadata. When successful, this call changes the type of \
             the VDI to cbt_metadata. This operation is idempotent: calling it \
             on a VDI of type cbt_metadata results in a no-op, and no error \
             will be thrown."
          )
        ]
      ~params:[(Ref _vdi, "self", "The VDI whose data should be deleted.")]
      ~errs:
        [
          Api_errors.sr_operation_not_supported
        ; Api_errors.vdi_missing
        ; Api_errors.sr_not_attached
        ; Api_errors.sr_no_pbds
        ; Api_errors.operation_not_allowed
        ; Api_errors.vdi_incompatible_type
        ; Api_errors.vdi_no_cbt_metadata
        ; Api_errors.vdi_in_use
        ; Api_errors.vdi_is_a_physical_device
        ]
      ~doc:
        "Delete the data of the snapshot VDI, but keep its changed block \
         tracking metadata. When successful, this call changes the type of the \
         VDI to cbt_metadata. This operation is idempotent: calling it on a \
         VDI of type cbt_metadata results in a no-op, and no error will be \
         thrown."
      ~allowed_roles:_R_VM_ADMIN ()

  let list_changed_blocks =
    call ~name:"list_changed_blocks" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_inverness
          , "Compare two VDIs in 64k block increments and report which blocks \
             differ. This operation is not allowed when vdi_to is attached to \
             a VM."
          )
        ]
      ~params:
        [
          (Ref _vdi, "vdi_from", "The first VDI.")
        ; (Ref _vdi, "vdi_to", "The second VDI.")
        ]
      ~errs:
        [
          Api_errors.sr_operation_not_supported
        ; Api_errors.vdi_missing
        ; Api_errors.sr_not_attached
        ; Api_errors.sr_no_pbds
        ; Api_errors.vdi_in_use
        ]
      ~result:
        ( String
        , "A base64 string-encoding of the bitmap showing which blocks differ \
           in the two VDIs."
        )
      ~doc:
        "Compare two VDIs in 64k block increments and report which blocks \
         differ. This operation is not allowed when vdi_to is attached to a \
         VM."
      ~allowed_roles:_R_VM_OP ()

  let get_nbd_info =
    call ~name:"get_nbd_info" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_inverness
          , "Get details specifying how to access this VDI via a Network Block \
             Device server. For each of a set of NBD server addresses on which \
             the VDI is available, the return value set contains a \
             vdi_nbd_server_info object that contains an exportname to request \
             once the NBD connection is established, and connection details \
             for the address. An empty list is returned if there is no network \
             that has a PIF on a host with access to the relevant SR, or if no \
             such network has been assigned an NBD-related purpose in its \
             purpose field. To access the given VDI, any of the \
             vdi_nbd_server_info objects can be used to make a connection to a \
             server, and then the VDI will be available by requesting the \
             exportname."
          )
        ]
      ~params:
        [
          ( Ref _vdi
          , "self"
          , "The VDI to access via Network Block Device protocol"
          )
        ]
      ~errs:[Api_errors.vdi_incompatible_type]
      ~result:
        ( Set (Record _vdi_nbd_server_info)
        , "The details necessary for connecting to the VDI over NBD. This \
           includes an authentication token, so must be treated as sensitive \
           material and must not be sent over insecure networks."
        )
      ~doc:
        "Get details specifying how to access this VDI via a Network Block \
         Device server. For each of a set of NBD server addresses on which the \
         VDI is available, the return value set contains a vdi_nbd_server_info \
         object that contains an exportname to request once the NBD connection \
         is established, and connection details for the address. An empty list \
         is returned if there is no network that has a PIF on a host with \
         access to the relevant SR, or if no such network has been assigned an \
         NBD-related purpose in its purpose field. To access the given VDI, \
         any of the vdi_nbd_server_info objects can be used to make a \
         connection to a server, and then the VDI will be available by \
         requesting the exportname."
      ~flags:[`Session] (* no async *)
      ~allowed_roles:_R_VM_ADMIN ()

  (** A virtual disk *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A virtual disk image")]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_vdi ~descr:"A virtual disk image"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:
        [
          snapshot
        ; clone
        ; resize
        ; resize_online
        ; introduce
        ; pool_introduce
        ; db_introduce
        ; db_forget
        ; update
        ; copy
        ; force_unlock
        ; set_managed
        ; forget
        ; set_sharable
        ; set_read_only
        ; set_missing
        ; set_virtual_size
        ; set_physical_utilisation
        ; set_is_a_snapshot
        ; set_snapshot_of
        ; set_snapshot_time
        ; set_metadata_of_pool
        ; set_name_label
        ; set_name_description
        ; generate_config
        ; set_on_boot
        ; set_allow_caching
        ; open_database
        ; checksum
        ; read_database_pool_uuid
        ; pool_migrate
        ; enable_cbt
        ; disable_cbt
        ; set_cbt_enabled
        ; data_destroy
        ; list_changed_blocks
        ; get_nbd_info
        ]
      ~contents:
        ([
           uid
             ~lifecycle:
               [(Published, rel_rio, "Unique identifier/object reference")]
             _vdi
         ; namespace ~name:"name"
             ~contents:
               (names oss_since_303 StaticRO
                  ~lifecycle:[(Published, rel_rio, "")]
               )
             ()
         ]
        @ allowed_and_current_operations operations
        @ [
            field ~qualifier:StaticRO ~ty:(Ref _sr)
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "storage repository in which the VDI resides"
                  )
                ]
              "SR" "storage repository in which the VDI resides"
          ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vbd))
              ~lifecycle:
                [(Published, rel_rio, "list of vbds that refer to this disk")]
              "VBDs" "list of vbds that refer to this disk"
          ; field ~qualifier:DynamicRO ~ty:(Set (Ref _crashdump))
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "list of crash dumps that refer to this disk"
                  )
                ]
              "crash_dumps" "list of crash dumps that refer to this disk"
          ; field ~qualifier:StaticRO ~ty:Int
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "size of disk as presented to the guest (in bytes). Note \
                     that, depending on storage backend type, requested size \
                     may not be respected exactly"
                  )
                ]
              "virtual_size"
              "size of disk as presented to the guest (in bytes). Note that, \
               depending on storage backend type, requested size may not be \
               respected exactly"
          ; field ~qualifier:DynamicRO ~ty:Int
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "amount of physical space that the disk image is currently \
                     taking up on the storage repository (in bytes)"
                  )
                ]
              "physical_utilisation"
              "amount of physical space that the disk image is currently \
               taking up on the storage repository (in bytes)"
          ; field ~qualifier:StaticRO ~ty:type'
              ~lifecycle:[(Published, rel_rio, "type of the VDI")]
              "type" "type of the VDI"
          ; field ~qualifier:StaticRO ~ty:Bool
              ~lifecycle:
                [(Published, rel_rio, "true if this disk may be shared")]
              "sharable" "true if this disk may be shared"
          ; field ~qualifier:StaticRO ~ty:Bool
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "true if this disk may ONLY be mounted read-only"
                  )
                ]
              "read_only" "true if this disk may ONLY be mounted read-only"
          ; field
              ~ty:(Map (String, String))
              ~lifecycle:[(Published, rel_rio, "additional configuration")]
              "other_config" "additional configuration"
              ~map_keys_roles:
                [("folder", _R_VM_OP); ("XenCenter.CustomFields.*", _R_VM_OP)]
          ; field ~qualifier:DynamicRO ~ty:Bool "storage_lock"
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "true if this disk is locked at the storage level"
                  )
                ]
              "true if this disk is locked at the storage level"
          ; (* XXX: location field was in the database in rio, now API in miami *)
            field ~in_oss_since:None
              ~lifecycle:[(Published, rel_miami, "location information")]
              ~ty:String ~qualifier:DynamicRO ~default_value:(Some (VString ""))
              "location" "location information"
          ; field ~in_oss_since:None
              ~lifecycle:[(Published, rel_rio, "")]
              ~ty:Bool ~qualifier:DynamicRO "managed" ""
          ; field ~in_oss_since:None
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "true if SR scan operation reported this VDI as not \
                     present on disk"
                  )
                ]
              ~ty:Bool ~qualifier:DynamicRO "missing"
              "true if SR scan operation reported this VDI as not present on \
               disk"
          ; field ~in_oss_since:None ~ty:(Ref _vdi) ~qualifier:DynamicRO
              ~lifecycle:
                [
                  (Published, rel_rio, "")
                ; (Deprecated, rel_ely, "The field was never used.")
                ]
              "parent" "This field is always null. Deprecated"
          ; field ~in_oss_since:None
              ~ty:(Map (String, String))
              ~lifecycle:
                [
                  ( Published
                  , rel_miami
                  , "data to be inserted into the xenstore tree \
                     (/local/domain/0/backend/vbd/<domid>/<device-id>/sm-data) \
                     after the VDI is attached. This is generally set by the \
                     SM backends on vdi_attach."
                  )
                ]
              ~qualifier:RW "xenstore_data"
              "data to be inserted into the xenstore tree \
               (/local/domain/0/backend/vbd/<domid>/<device-id>/sm-data) after \
               the VDI is attached. This is generally set by the SM backends \
               on vdi_attach."
              ~default_value:(Some (VMap []))
          ; field ~in_oss_since:None
              ~ty:(Map (String, String))
              ~lifecycle:[(Published, rel_miami, "SM dependent data")]
              ~qualifier:RW "sm_config" "SM dependent data"
              ~default_value:(Some (VMap []))
          ; field
              ~lifecycle:
                [(Published, rel_orlando, "true if this is a snapshot.")]
              ~default_value:(Some (VBool false)) ~qualifier:DynamicRO ~ty:Bool
              ~doc_tags:[Snapshots] "is_a_snapshot"
              "true if this is a snapshot."
          ; field
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "Ref pointing to the VDI this snapshot is of."
                  )
                ]
              ~default_value:(Some (VRef "")) ~qualifier:DynamicRO
              ~ty:(Ref _vdi) ~doc_tags:[Snapshots] "snapshot_of"
              "Ref pointing to the VDI this snapshot is of."
          ; field
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "List pointing to all the VDIs snapshots."
                  )
                ]
              ~qualifier:DynamicRO ~ty:(Set (Ref _vdi)) ~doc_tags:[Snapshots]
              "snapshots" "List pointing to all the VDIs snapshots."
          ; field
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "Date/time when this snapshot was created."
                  )
                ]
              ~default_value:(Some (VDateTime Date.epoch)) ~qualifier:DynamicRO
              ~ty:DateTime ~doc_tags:[Snapshots] "snapshot_time"
              "Date/time when this snapshot was created."
          ; field ~writer_roles:_R_VM_OP
              ~lifecycle:
                [
                  ( Published
                  , rel_orlando
                  , "user-specified tags for categorization purposes"
                  )
                ]
              ~default_value:(Some (VSet [])) ~ty:(Set String) "tags"
              "user-specified tags for categorization purposes"
          ; field
              ~lifecycle:
                [
                  ( Published
                  , rel_cowley
                  , "true if this VDI is to be cached in the local cache SR"
                  )
                ]
              ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false))
              "allow_caching"
              "true if this VDI is to be cached in the local cache SR"
          ; field
              ~lifecycle:
                [
                  ( Published
                  , rel_cowley
                  , "The behaviour of this VDI on a VM boot"
                  )
                ]
              ~qualifier:DynamicRO ~ty:on_boot
              ~default_value:(Some (VEnum "persist")) "on_boot"
              "The behaviour of this VDI on a VM boot"
          ; field
              ~lifecycle:
                [
                  ( Published
                  , rel_boston
                  , "The pool whose metadata is contained in this VDI"
                  )
                ]
              ~qualifier:DynamicRO ~ty:(Ref _pool)
              ~default_value:(Some (VRef null_ref)) "metadata_of_pool"
              "The pool whose metadata is contained in this VDI"
          ; field
              ~lifecycle:
                [
                  ( Published
                  , rel_boston
                  , "Whether this VDI contains the latest known accessible \
                     metadata for the pool"
                  )
                ]
              ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false))
              "metadata_latest"
              "Whether this VDI contains the latest known accessible metadata \
               for the pool"
          ; field
              ~lifecycle:[(Published, rel_dundee, "")]
              ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false))
              "is_tools_iso" "Whether this VDI is a Tools ISO"
          ; field
              ~lifecycle:[(Published, rel_inverness, "")]
              ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false))
              "cbt_enabled" "True if changed blocks are tracked for this VDI"
              ~doc_tags:[Snapshots]
          ]
        )
      ()
end

module VBD = struct
  (** Virtual disk interfaces have a mode parameter: *)
  let mode =
    Enum
      ( "vbd_mode"
      , [
          ("RO", "only read-only access will be allowed")
        ; ("RW", "read-write access will be allowed")
        ]
      )

  let type' =
    Enum
      ( "vbd_type"
      , [
          ("CD", "VBD will appear to guest as CD")
        ; ("Disk", "VBD will appear to guest as disk")
        ; ("Floppy", "VBD will appear as a floppy")
        ]
      )

  let operations =
    Enum
      ( "vbd_operations"
      , [
          ("attach", "Attempting to attach this VBD to a VM")
        ; ("eject", "Attempting to eject the media from this VBD")
        ; ("insert", "Attempting to insert new media into this VBD")
        ; ("plug", "Attempting to hotplug this VBD")
        ; ("unplug", "Attempting to hot unplug this VBD")
        ; ("unplug_force", "Attempting to forcibly unplug this VBD")
        ; ("pause", "Attempting to pause a block device backend")
        ; ("unpause", "Attempting to unpause a block device backend")
        ]
      )

  let eject =
    call ~name:"eject"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Remove the media from the device and leave it empty"
          )
        ]
      ~doc:"Remove the media from the device and leave it empty"
      ~params:[(Ref _vbd, "vbd", "The vbd representing the CDROM-like device")]
      ~errs:[Api_errors.vbd_not_removable_media; Api_errors.vbd_is_empty]
      ~allowed_roles:_R_VM_OP ()

  let insert =
    call ~name:"insert"
      ~lifecycle:[(Published, rel_rio, "Insert new media into the device")]
      ~doc:"Insert new media into the device"
      ~params:
        [
          (Ref _vbd, "vbd", "The vbd representing the CDROM-like device")
        ; (Ref _vdi, "vdi", "The new VDI to 'insert'")
        ]
      ~errs:[Api_errors.vbd_not_removable_media; Api_errors.vbd_not_empty]
      ~allowed_roles:_R_VM_OP ()

  let plug =
    call ~name:"plug"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Hotplug the specified VBD, dynamically attaching it to the \
             running VM"
          )
        ]
      ~doc:
        "Hotplug the specified VBD, dynamically attaching it to the running VM"
      ~params:[(Ref _vbd, "self", "The VBD to hotplug")]
      ~allowed_roles:_R_VM_ADMIN ()

  let unplug =
    call ~name:"unplug"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Hot-unplug the specified VBD, dynamically unattaching it from the \
             running VM"
          )
        ]
      ~doc:
        "Hot-unplug the specified VBD, dynamically unattaching it from the \
         running VM"
      ~params:[(Ref _vbd, "self", "The VBD to hot-unplug")]
      ~errs:
        [Api_errors.device_detach_rejected; Api_errors.device_already_detached]
      ~allowed_roles:_R_VM_ADMIN ()

  let unplug_force =
    call ~name:"unplug_force"
      ~lifecycle:[(Published, rel_rio, "Forcibly unplug the specified VBD")]
      ~doc:"Forcibly unplug the specified VBD"
      ~params:[(Ref _vbd, "self", "The VBD to forcibly unplug")]
      ~allowed_roles:_R_VM_ADMIN ()

  let unplug_force_no_safety_check =
    call ~name:"unplug_force_no_safety_check"
      ~doc:
        "Deprecated: use 'unplug_force' instead. Forcibly unplug the specified \
         VBD without any safety checks. This is an extremely dangerous \
         operation in the general case that can cause guest crashes and data \
         corruption; it should be called with extreme caution. Functionally \
         equivalent with 'unplug_force'."
      ~params:
        [
          ( Ref _vbd
          , "self"
          , "The VBD to forcibly unplug (no safety checks are applied to test \
             if the device supports surprise-remove)"
          )
        ]
      ~hide_from_docs:true
      ~lifecycle:
        [
          ( Published
          , rel_symc
          , "Deprecated: use 'unplug_force' instead. Forcibly unplug the \
             specified VBD without any safety checks. This is an extremely \
             dangerous operation in the general case that can cause guest \
             crashes and data corruption; it should be called with extreme \
             caution. Functionally equivalent with 'unplug_force'."
          )
        ; (Deprecated, rel_ely, "")
        ]
      ~allowed_roles:_R_VM_ADMIN ()

  let pause =
    call ~name:"pause"
      ~doc:
        "Stop the backend device servicing requests so that an operation can \
         be performed on the disk (eg live resize, snapshot)"
      ~params:[(Ref _vbd, "self", "The VBD to pause")]
      ~hide_from_docs:true
      ~lifecycle:
        [
          ( Published
          , rel_symc
          , "Stop the backend device servicing requests so that an operation \
             can be performed on the disk (eg live resize, snapshot)"
          )
        ]
      ~result:
        ( String
        , "Token to uniquely identify this pause instance, used to match the \
           corresponding unpause"
        )
        (* new in MR *)
      ~allowed_roles:_R_VM_ADMIN ()

  let unpause =
    call ~name:"unpause"
      ~doc:
        "Restart the backend device after it was paused while an operation was \
         performed on the disk (eg live resize, snapshot)"
      ~versioned_params:
        [
          {
            param_type= Ref _vbd
          ; param_name= "self"
          ; param_doc= "The VBD to unpause"
          ; param_release= miami_symc_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "token"
          ; param_doc= "The token from VBD.pause"
          ; param_release= orlando_release
          ; param_default= Some (VString "")
          }
        ]
      ~hide_from_docs:true
      ~lifecycle:
        [
          ( Published
          , rel_symc
          , "Restart the backend device after it was paused while an operation \
             was performed on the disk (eg live resize, snapshot)"
          )
        ]
      ~allowed_roles:_R_VM_ADMIN ()

  let assert_attachable =
    call ~name:"assert_attachable"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Throws an error if this VBD could not be attached to this VM if \
             the VM were running. Intended for debugging."
          )
        ]
      ~doc:
        "Throws an error if this VBD could not be attached to this VM if the \
         VM were running. Intended for debugging."
      ~params:[(Ref _vbd, "self", "The VBD to query")]
      ~in_oss_since:None ~allowed_roles:_R_VM_ADMIN ()

  let set_mode =
    call ~name:"set_mode"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Sets the mode of the VBD. The power_state of the VM must be \
             halted."
          )
        ]
      ~doc:"Sets the mode of the VBD. The power_state of the VM must be halted."
      ~params:
        [
          (Ref _vbd, "self", "Reference to the object")
        ; (mode, "value", "New value to set")
        ]
      ~in_oss_since:None ~allowed_roles:_R_VM_ADMIN ()

  (** A virtual disk interface *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A virtual block device")]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_vbd
      ~descr:"A virtual block device" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:
        [
          eject
        ; insert
        ; plug
        ; unplug
        ; unplug_force
        ; unplug_force_no_safety_check
        ; assert_attachable
        ; pause
        ; unpause
        ; set_mode
        ]
      ~contents:
        ([
           uid
             ~lifecycle:
               [(Published, rel_rio, "Unique identifier/object reference")]
             _vbd
         ]
        @ allowed_and_current_operations operations
        @ [
            field ~qualifier:StaticRO ~ty:(Ref _vm)
              ~lifecycle:[(Published, rel_rio, "the virtual machine")]
              "VM" "the virtual machine"
          ; field ~qualifier:StaticRO ~ty:(Ref _vdi)
              ~lifecycle:[(Published, rel_rio, "the virtual disk")]
              "VDI" "the virtual disk"
          ; field ~qualifier:StaticRO ~ty:String
              ~default_value:(Some (VString ""))
              ~lifecycle:
                [
                  (Published, rel_rio, "")
                ; ( Changed
                  , "1.257.0"
                  , "Become static to allow plugged VBD creation for Suspended \
                     VM"
                  )
                ]
              "device" "device seen by the guest e.g. hda1"
          ; field
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "user-friendly device name e.g. 0,1,2,etc."
                  )
                ]
              "userdevice" "user-friendly device name e.g. 0,1,2,etc."
          ; field ~ty:Bool
              ~lifecycle:[(Published, rel_rio, "true if this VBD is bootable")]
              "bootable" "true if this VBD is bootable"
          ; field ~qualifier:StaticRO ~ty:mode
              ~lifecycle:
                [
                  (Published, rel_rio, "the mode the VBD should be mounted with")
                ]
              "mode" "the mode the VBD should be mounted with"
          ; field ~ty:type'
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "how the VBD will appear to the guest (e.g. disk or CD)"
                  )
                ]
              "type" "how the VBD will appear to the guest (e.g. disk or CD)"
          ; field ~in_oss_since:None
              ~lifecycle:
                [
                  ( Published
                  , rel_miami
                  , "true if this VBD will support hot-unplug"
                  )
                ]
              ~ty:Bool ~default_value:(Some (VBool true)) "unpluggable"
              "true if this VBD will support hot-unplug"
          ; field ~qualifier:DynamicRO ~ty:Bool
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "true if a storage level lock was acquired"
                  )
                ]
              "storage_lock" "true if a storage level lock was acquired"
          ; field ~qualifier:StaticRO ~ty:Bool
              ~lifecycle:
                [(Published, rel_rio, "if true this represents an empty drive")]
              "empty" "if true this represents an empty drive"
          ; field ~in_oss_since:None
              ~lifecycle:
                [
                  ( Published
                  , rel_rio
                  , "true if the VBD is reserved pending a reboot/migrate"
                  )
                ]
              ~internal_only:true ~qualifier:DynamicRO ~ty:Bool
              ~default_value:(Some (VBool false)) "reserved"
              "true if the VBD is reserved pending a reboot/migrate"
          ; field
              ~ty:(Map (String, String))
              ~lifecycle:[(Published, rel_rio, "additional configuration")]
              "other_config" "additional configuration"
          ]
        @ device_status_fields
        @ [namespace ~name:"qos" ~contents:(qos "VBD") ()]
        @ [
            field ~qualifier:DynamicRO ~ty:(Ref _vbd_metrics)
              ~default_value:(Some (VRef null_ref))
              ~lifecycle:
                [
                  (Published, rel_rio, "")
                ; (Deprecated, rel_tampa, "Dummy transition")
                ; (Removed, rel_tampa, "Disabled in favour of RRDs")
                ]
              "metrics" "metrics associated with this VBD"
          ]
        )
      ()
end

module VBD_metrics = struct
  let t =
    create_obj
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "The metrics associated with a virtual block device"
          )
        ; (Deprecated, rel_tampa, "Dummy transition")
        ; (Removed, rel_tampa, "Disabled in favour of RRD")
        ]
      ~in_db:true ~in_oss_since:oss_since_303 ~persist:PersistNothing
      ~gen_constructor_destructor:false ~name:_vbd_metrics
      ~descr:"The metrics associated with a virtual block device"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _vbd_metrics
        ; namespace ~name:"io" ~contents:iobandwidth ()
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~default_value:(Some (VDateTime Date.epoch))
            ~lifecycle:
              [
                (Published, rel_rio, "")
              ; (Deprecated, rel_tampa, "Dummy transition")
              ; (Removed, rel_tampa, "Disabled in favour of RRD")
              ]
            "last_updated" "Time at which this information was last updated"
        ; field
            ~lifecycle:
              [
                (Published, rel_orlando, "")
              ; (Deprecated, rel_tampa, "Dummy transition")
              ; (Removed, rel_tampa, "Disabled in favour of RRD")
              ]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

module Crashdump = struct
  let destroy =
    call ~name:"destroy"
      ~lifecycle:[(Published, rel_rio, "Destroy the specified crashdump")]
      ~doc:"Destroy the specified crashdump"
      ~params:[(Ref _crashdump, "self", "The crashdump to destroy")]
      ~allowed_roles:_R_POOL_OP ()

  (** A crashdump for a particular VM, stored in a particular VDI *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [
          (Published, rel_rio, "A VM crashdump"); (Deprecated, rel_inverness, "")
        ]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_crashdump ~descr:"A VM crashdump"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~messages:[destroy]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _crashdump
        ; field ~qualifier:StaticRO ~ty:(Ref _vm)
            ~lifecycle:[(Published, rel_rio, "the virtual machine")]
            "VM" "the virtual machine"
        ; field ~qualifier:StaticRO ~ty:(Ref _vdi)
            ~lifecycle:[(Published, rel_rio, "the virtual disk")]
            "VDI" "the virtual disk"
        ; field
            ~lifecycle:[(Published, rel_miami, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ]
      ()
end

module Auth = struct
  (** Auth class *)
  let get_subject_identifier =
    call ~flags:[`Session] ~name:"get_subject_identifier" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_george
          , "This call queries the external directory service to obtain the \
             subject_identifier as a string from the human-readable \
             subject_name"
          )
        ]
      ~params:
        [
          (*Ref _auth, "auth", "???";*)
          ( String
          , "subject_name"
          , "The human-readable subject_name, such as a username or a groupname"
          )
        ]
      ~result:
        ( String
        , "the subject_identifier obtained from the external directory service"
        )
      ~doc:
        "This call queries the external directory service to obtain the \
         subject_identifier as a string from the human-readable subject_name"
      ~allowed_roles:_R_READ_ONLY ()

  let get_subject_information_from_identifier =
    call ~flags:[`Session] ~name:"get_subject_information_from_identifier"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_george
          , "This call queries the external directory service to obtain the \
             user information (e.g. username, organization etc) from the \
             specified subject_identifier"
          )
        ]
      ~params:
        [
          ( String
          , "subject_identifier"
          , "A string containing the subject_identifier, unique in the \
             external directory service"
          )
        ]
      ~result:
        ( Map (String, String)
        , "key-value pairs containing at least a key called subject_name"
        )
      ~doc:
        "This call queries the external directory service to obtain the user \
         information (e.g. username, organization etc) from the specified \
         subject_identifier"
      ~allowed_roles:_R_READ_ONLY ()

  let get_group_membership =
    call ~flags:[`Session] ~name:"get_group_membership" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_george
          , "This calls queries the external directory service to obtain the \
             transitively-closed set of groups that the the subject_identifier \
             is member of."
          )
        ]
      ~params:
        [
          ( String
          , "subject_identifier"
          , "A string containing the subject_identifier, unique in the \
             external directory service"
          )
        ]
      ~result:
        ( Set String
        , "set of subject_identifiers that provides the group membership of \
           subject_identifier passed as argument, it contains, recursively, \
           all groups a subject_identifier is member of."
        )
      ~doc:
        "This calls queries the external directory service to obtain the \
         transitively-closed set of groups that the the subject_identifier is \
         member of."
      ~allowed_roles:_R_READ_ONLY ()

  let t =
    create_obj ~in_db:false
      ~lifecycle:
        [
          (Published, rel_george, "Management of remote authentication services")
        ]
      ~in_oss_since:None ~persist:PersistNothing
      ~gen_constructor_destructor:false ~name:_auth
      ~descr:"Management of remote authentication services" ~gen_events:false
      ~doccomments:[] ~messages_default_allowed_roles:_R_READ_ONLY
      ~messages:
        [
          get_subject_identifier
        ; get_subject_information_from_identifier
        ; get_group_membership
        ]
      ~contents:[] ()
end

module Subject = struct
  (** Subject class *)
  let add_to_roles =
    call ~flags:[`Session] ~name:"add_to_roles" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_midnight_ride
          , "This call adds a new role to a subject"
          )
        ]
      ~params:
        [
          (Ref _subject, "self", "The subject who we want to add the role to")
        ; (Ref _role, "role", "The unique role reference")
        ]
      ~doc:"This call adds a new role to a subject" ~allowed_roles:_R_POOL_ADMIN
      ()

  let remove_from_roles =
    call ~flags:[`Session] ~name:"remove_from_roles" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_midnight_ride
          , "This call removes a role from a subject"
          )
        ]
      ~params:
        [
          ( Ref _subject
          , "self"
          , "The subject from whom we want to remove the role"
          )
        ; ( Ref _role
          , "role"
          , "The unique role reference in the subject's roles field"
          )
        ]
      ~doc:"This call removes a role from a subject"
      ~allowed_roles:_R_POOL_ADMIN ()

  let get_permissions_name_label =
    call ~flags:[`Session] ~name:"get_permissions_name_label" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_midnight_ride
          , "This call returns a list of permission names given a subject"
          )
        ]
      ~params:
        [
          ( Ref _subject
          , "self"
          , "The subject whose permissions will be retrieved"
          )
        ]
      ~result:(Set String, "a list of permission names")
      ~doc:"This call returns a list of permission names given a subject"
      ~allowed_roles:_R_READ_ONLY ()

  (* a subject is a user/group that can log in xapi *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [(Published, rel_george, "A user or group that can log in xapi")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_subject
      ~descr:"A user or group that can log in xapi" ~gen_events:true
      ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages:[add_to_roles; remove_from_roles; get_permissions_name_label]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            ~in_oss_since:None _subject
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_george
                , "the subject identifier, unique in the external directory \
                   service"
                )
              ]
            ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String
            "subject_identifier"
            "the subject identifier, unique in the external directory service"
        ; field
            ~lifecycle:[(Published, rel_george, "additional configuration")]
            ~default_value:(Some (VMap [])) ~qualifier:StaticRO
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ; (* DynamicRO fields do not show up in the constructor, as it should be because a subject must be created without receiving any roles as a parameter *)
          field
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "the roles associated with this subject"
                )
              ]
            ~default_value:
              (Some (VSet [VRef ("OpaqueRef:" ^ Constants.rbac_pool_admin_uuid)])
            )
              (* pool-admin, according to rbac_static.ml, used during upgrade from pre-rbac xapis *)
            ~ignore_foreign_key:true ~qualifier:DynamicRO ~ty:(Set (Ref _role))
            "roles" "the roles associated with this subject"
        ]
      ()
end

module Role = struct
  (** Role class *)
  let get_permissions =
    call ~flags:[`Session] ~name:"get_permissions" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_midnight_ride
          , "This call returns a list of permissions given a role"
          )
        ]
      ~params:[(Ref _role, "self", "a reference to a role")]
      ~result:(Set (Ref _role), "a list of permissions")
      ~doc:"This call returns a list of permissions given a role"
      ~allowed_roles:_R_READ_ONLY ()

  let get_permissions_name_label =
    call ~flags:[`Session] ~name:"get_permissions_name_label" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_midnight_ride
          , "This call returns a list of permission names given a role"
          )
        ]
      ~params:[(Ref _role, "self", "a reference to a role")]
      ~result:(Set String, "a list of permission names")
      ~doc:"This call returns a list of permission names given a role"
      ~allowed_roles:_R_READ_ONLY ()

  let get_by_permission =
    call ~flags:[`Session] ~name:"get_by_permission" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_midnight_ride
          , "This call returns a list of roles given a permission"
          )
        ]
      ~params:[(Ref _role, "permission", "a reference to a permission")]
      ~result:(Set (Ref _role), "a list of references to roles")
      ~doc:"This call returns a list of roles given a permission"
      ~allowed_roles:_R_READ_ONLY ()

  let get_by_permission_name_label =
    call ~flags:[`Session] ~name:"get_by_permission_name_label"
      ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_midnight_ride
          , "This call returns a list of roles given a permission name"
          )
        ]
      ~params:[(String, "label", "The short friendly name of the role")]
      ~result:(Set (Ref _role), "a list of references to roles")
      ~doc:"This call returns a list of roles given a permission name"
      ~allowed_roles:_R_READ_ONLY ()

  (* A role defines a set of API call privileges associated with a subject *)
  (* A role is synonymous to permission or privilege *)
  (* A role is a recursive definition: it is either a basic role or it points to a set of roles *)
  (* - full/complete role: is the one meant to be used by the end-user, a root in the tree of roles *)
  (* - basic role: is the 1x1 mapping to each XAPI/HTTP call being protected, a leaf in the tree of roles *)
  (* - intermediate role: an intermediate node in the recursive tree of roles, usually not meant to the end-user *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [
          ( Published
          , rel_midnight_ride
          , "A set of permissions associated with a subject"
          )
        ]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_role
      ~descr:"A set of permissions associated with a subject" ~gen_events:true
      ~force_custom_actions:(Some StaticRO)
        (* force custom actions for getters *)
      ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~messages:
        [
          get_permissions
        ; get_permissions_name_label
        ; get_by_permission
        ; get_by_permission_name_label
        ]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            ~in_oss_since:None _role
        ; namespace ~name:"name"
            ~contents:
              [
                field
                  ~lifecycle:
                    [
                      ( Published
                      , rel_midnight_ride
                      , "a short user-friendly name for the role"
                      )
                    ]
                  ~default_value:(Some (VString "")) ~qualifier:StaticRO
                  ~ty:String "label" "a short user-friendly name for the role"
              ; field
                  ~lifecycle:
                    [(Published, rel_midnight_ride, "what this role is for")]
                  ~default_value:(Some (VString "")) ~qualifier:StaticRO
                  ~ty:String "description" "what this role is for"
              ]
            ()
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_midnight_ride
                , "a list of pointers to other roles or permissions"
                )
              ]
            ~default_value:(Some (VSet [])) ~ignore_foreign_key:true
            ~qualifier:StaticRO ~ty:(Set (Ref _role)) "subroles"
            "a list of pointers to other roles or permissions"
        ; field
            ~lifecycle:
              [
                ( Published
                , "22.5.0"
                , "Indicates whether the role is only to be assigned \
                   internally by xapi, or can be used by clients"
                )
              ]
            ~default_value:(Some (VBool false)) ~qualifier:DynamicRO ~ty:Bool
            "is_internal"
            "Indicates whether the role is only to be assigned internally by \
             xapi, or can be used by clients"
          (*RBAC2: field ~in_product_since:rel_midnight_ride ~default_value:(Some (VBool false)) ~qualifier:StaticRO ~ty:Bool "is_complete" "if this is a complete role, meant to be used by the end-user";*)
        ]
      ()
end

module Console = struct
  (** Console protocols *)
  let protocol =
    Enum
      ( "console_protocol"
      , [
          ("vt100", "VT100 terminal")
        ; ("rfb", "Remote FrameBuffer protocol (as used in VNC)")
        ; ("rdp", "Remote Desktop Protocol")
        ]
      )

  (** A virtual console device *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "A console")]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_console ~descr:"A console"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _console
        ; field ~qualifier:DynamicRO ~ty:protocol
            ~lifecycle:
              [(Published, rel_rio, "the protocol used by this console")]
            "protocol" "the protocol used by this console"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "URI for the console service")]
            "location" "URI for the console service"
        ; field ~qualifier:DynamicRO ~ty:(Ref _vm)
            ~lifecycle:
              [(Published, rel_rio, "VM to which this console is attached")]
            "VM" "VM to which this console is attached"
        ; field
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "additional configuration")]
            "other_config" "additional configuration"
        ; field ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "port in dom0 on which the console server is listening"
                )
              ]
            ~internal_only:true ~ty:Int "port"
            "port in dom0 on which the console server is listening"
        ]
      ()
end

module VM_metrics = struct
  let vm_memory_metrics =
    [
      field ~qualifier:DynamicRO ~ty:Int
        ~lifecycle:[(Published, rel_rio, "Guest's actual memory (bytes)")]
        "actual" "Guest's actual memory (bytes)" ~persist:false
    ]

  let vm_vcpu_metrics =
    [
      field ~qualifier:DynamicRO ~ty:Int
        ~lifecycle:[(Published, rel_rio, "Current number of VCPUs")]
        "number" "Current number of VCPUs" ~persist:true
    ; field ~qualifier:DynamicRO
        ~ty:(Map (Int, Float))
        ~persist:false "utilisation"
        "Utilisation for all of guest's current VCPUs"
        ~default_value:(Some (VMap []))
        ~lifecycle:
          [
            (Published, rel_rio, "")
          ; (Deprecated, rel_tampa, "Dummy transition")
          ; (Removed, rel_tampa, "Disabled in favour of RRDs")
          ]
    ; field ~qualifier:DynamicRO
        ~ty:(Map (Int, Int))
        ~lifecycle:[(Published, rel_rio, "VCPU to PCPU map")]
        "CPU" "VCPU to PCPU map" ~persist:false
    ; field ~qualifier:DynamicRO
        ~ty:(Map (String, String))
        ~lifecycle:
          [(Published, rel_rio, "The live equivalent to VM.VCPUs_params")]
        "params" "The live equivalent to VM.VCPUs_params" ~persist:false
    ; field ~qualifier:DynamicRO
        ~ty:(Map (Int, Set String))
        ~lifecycle:[(Published, rel_rio, "CPU flags (blocked,online,running)")]
        "flags" "CPU flags (blocked,online,running)" ~persist:false
    ]

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_rio, "The metrics associated with a VM")]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_vm_metrics
      ~descr:"The metrics associated with a VM" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _vm_metrics
        ; namespace ~name:"memory" ~contents:vm_memory_metrics ()
        ; namespace ~name:"VCPUs" ~contents:vm_vcpu_metrics ()
        ; field ~qualifier:DynamicRO ~ty:(Set String)
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The state of the guest, eg blocked, dying etc"
                )
              ]
            "state" "The state of the guest, eg blocked, dying etc"
            ~persist:false
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [(Published, rel_rio, "Time at which this VM was last booted")]
            "start_time" "Time at which this VM was last booted"
        ; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [(Published, rel_rio, "Time at which the VM was installed")]
            "install_time" "Time at which the VM was installed"
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Time at which this information was last updated"
                )
              ]
            "last_updated" "Time at which this information was last updated"
            ~persist:false
        ; field
            ~lifecycle:[(Published, rel_orlando, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration" ~persist:false
        ; field
            ~lifecycle:[(Published, rel_ely, "hardware virtual machine")]
            ~default_value:(Some (VBool false)) ~ty:Bool ~qualifier:DynamicRO
            "hvm" "hardware virtual machine" ~persist:false
        ; field
            ~lifecycle:
              [(Published, rel_ely, "VM supports nested virtualisation")]
            ~default_value:(Some (VBool false)) ~ty:Bool ~qualifier:DynamicRO
            "nested_virt" "VM supports nested virtualisation" ~persist:false
        ; field
            ~lifecycle:
              [
                ( Published
                , rel_ely
                , "VM is immobile and can't migrate between hosts"
                )
              ]
            ~default_value:(Some (VBool false)) ~ty:Bool ~qualifier:DynamicRO
            "nomigrate" "VM is immobile and can't migrate between hosts"
            ~persist:false
        ; field
            ~lifecycle:
              [
                (Prototyped, rel_jura, "Not yet implemented (for future use)")
              ; (Published, rel_kolkata, "This field now contains valid data")
              ]
            ~default_value:(Some (VEnum "unspecified"))
            ~ty:Datamodel_vm.domain_type ~qualifier:DynamicRO
            "current_domain_type"
            "The current domain type of the VM (for running,suspended, or \
             paused VMs). The last-known domain type for halted VMs."
        ]
      ()
end

let tristate_type =
  Enum
    ( "tristate_type"
    , [
        ("yes", "Known to be true")
      ; ("no", "Known to be false")
      ; ("unspecified", "Unknown or unspecified")
      ]
    )

module VM_guest_metrics = struct
  (* Some of this stuff needs to persist (like PV drivers vsns etc.) so we know about what's likely to be in the VM even when it's off.
     Other things don't need to persist, so we specify these on a per-field basis *)
  let t =
    create_obj ~in_db:true
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "The metrics reported by the guest (as opposed to inferred from \
             outside)"
          )
        ]
      ~in_oss_since:oss_since_303 ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_vm_guest_metrics
      ~descr:
        "The metrics reported by the guest (as opposed to inferred from \
         outside)"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN ~messages:[]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _vm_guest_metrics
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "version of the OS")]
            "os_version" "version of the OS"
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:[] "netbios_name" "The NETBIOS name of the machine"
            ~default_value:(Some (VMap []))
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "version of the PV drivers")]
            "PV_drivers_version" "version of the PV drivers"
        ; field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "true if the PV drivers appear to be up to date"
                )
              ; ( Deprecated
                , rel_dundee
                , "Deprecated in favour of PV_drivers_detected, and redefined \
                   in terms of it"
                )
              ]
            "PV_drivers_up_to_date"
            "Logically equivalent to PV_drivers_detected"
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap []))
            ~lifecycle:
              [
                (Published, rel_rio, "free/used/total")
              ; (Deprecated, rel_george, "Dummy transition")
              ; ( Removed
                , rel_george
                , "Disabled in favour of the RRDs, to improve scalability"
                )
              ]
            "memory"
            "This field exists but has no data. Use the memory and \
             memory_internal_free RRD data-sources instead."
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap []))
            ~lifecycle:
              [
                (Published, rel_rio, "Disk configuration/free space")
              ; (Deprecated, rel_orlando, "Dummy transition")
              ; (Removed, rel_orlando, "No data")
              ]
            "disks" "This field exists but has no data."
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "network configuration")]
            "networks" "network configuration"
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "anything else")]
            "other" "anything else"
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Time at which this information was last updated"
                )
              ]
            "last_updated" "Time at which this information was last updated"
        ; field
            ~lifecycle:[(Published, rel_orlando, "additional configuration")]
            ~default_value:(Some (VMap []))
            ~ty:(Map (String, String))
            "other_config" "additional configuration"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_orlando
                , "True if the guest is sending heartbeat messages via the \
                   guest agent"
                )
              ]
            ~default_value:(Some (VBool false)) ~ty:Bool "live"
            "True if the guest is sending heartbeat messages via the guest \
             agent"
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_dundee
                , "To be used where relevant and available instead of checking \
                   PV driver version."
                )
              ]
            ~ty:tristate_type ~default_value:(Some (VEnum "unspecified"))
            "can_use_hotplug_vbd"
            "The guest's statement of whether it supports VBD hotplug, i.e. \
             whether it is capable of responding immediately to instantiation \
             of a new VBD by bringing online a new PV block device. If the \
             guest states that it is not capable, then the VBD plug and unplug \
             operations will not be allowed while the guest is running."
        ; field ~qualifier:DynamicRO
            ~lifecycle:
              [
                ( Published
                , rel_dundee
                , "To be used where relevant and available instead of checking \
                   PV driver version."
                )
              ]
            ~ty:tristate_type ~default_value:(Some (VEnum "unspecified"))
            "can_use_hotplug_vif"
            "The guest's statement of whether it supports VIF hotplug, i.e. \
             whether it is capable of responding immediately to instantiation \
             of a new VIF by bringing online a new PV network device. If the \
             guest states that it is not capable, then the VIF plug and unplug \
             operations will not be allowed while the guest is running."
        ; field ~qualifier:DynamicRO
            ~lifecycle:[(Published, rel_dundee, "")]
            ~ty:Bool ~default_value:(Some (VBool false)) "PV_drivers_detected"
            "At least one of the guest's devices has successfully connected to \
             the backend."
        ; field ~qualifier:DynamicRO ~lifecycle:[]
            ~ty:(Map (String, String))
            ~default_value:(Some (VMap [])) "services"
            "The guest's services data."
        ]
      ()
end

module VMPP = struct
  (* VM protection policy *)
  let removed =
    [
      (Published, rel_cowley, "")
    ; (Deprecated, rel_clearwater, "Dummy transition")
    ; (Removed, rel_clearwater, "The VMPR feature was removed")
    ]

  let protect_now =
    call ~flags:[`Session] ~name:"protect_now" ~lifecycle:removed
      ~params:[(Ref _vmpp, "vmpp", "The protection policy to execute")]
      ~doc:"This call executes the protection policy immediately"
      ~allowed_roles:_R_POOL_OP
      ~result:(String, "An XMLRPC result")
      ()

  let archive_now =
    call ~flags:[`Session] ~name:"archive_now" ~lifecycle:removed
      ~params:[(Ref _vm, "snapshot", "The snapshot to archive")]
      ~doc:"This call archives the snapshot provided as a parameter"
      ~allowed_roles:_R_VM_POWER_ADMIN
      ~result:(String, "An XMLRPC result")
      ()

  let create_alert =
    call ~flags:[`Session] ~name:"create_alert" ~lifecycle:removed
      ~params:
        [
          ( Ref _vmpp
          , "vmpp"
          , "The protection policy where the alert should be created"
          )
        ; (String, "name", "The name of the message")
        ; (Int, "priority", "The priority of the message")
        ; (String, "body", "The body of the email message")
        ; (String, "data", "The data in xml")
        ]
      ~doc:"This call creates an alert for some protection policy"
      ~allowed_roles:_R_LOCAL_ROOT_ONLY ~hide_from_docs:true ()

  let get_alerts =
    call ~flags:[`Session] ~name:"get_alerts" ~lifecycle:removed
      ~params:
        [
          (Ref _vmpp, "vmpp", "The protection policy")
        ; ( Int
          , "hours_from_now"
          , "how many hours in the past the oldest record to fetch is"
          )
        ]
      ~doc:"This call fetches a history of alerts for a given protection policy"
      ~allowed_roles:_R_POOL_OP
      ~result:(Set String, "A list of alerts encoded in xml")
      ()

  let backup_type =
    Enum
      ( "vmpp_backup_type"
      , [
          ("snapshot", "The backup is a snapshot")
        ; ("checkpoint", "The backup is a checkpoint")
        ]
      )

  let backup_frequency =
    Enum
      ( "vmpp_backup_frequency"
      , [
          ("hourly", "Hourly backups")
        ; ("daily", "Daily backups")
        ; ("weekly", "Weekly backups")
        ]
      )

  let archive_frequency =
    Enum
      ( "vmpp_archive_frequency"
      , [
          ("never", "Never archive")
        ; ("always_after_backup", "Archive after backup")
        ; ("daily", "Daily archives")
        ; ("weekly", "Weekly backups")
        ]
      )

  let archive_target_type =
    Enum
      ( "vmpp_archive_target_type"
      , [
          ("none", "No target config")
        ; ("cifs", "CIFS target config")
        ; ("nfs", "NFS target config")
        ]
      )

  let schedule_min = "min"

  let schedule_hour = "hour"

  let schedule_days = "days"

  let archive_target_config_location = "location"

  let archive_target_config_username = "username"

  let archive_target_config_password = "password"

  let set_backup_retention_value =
    call ~flags:[`Session] ~name:"set_backup_retention_value" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (Int, "value", "the value to set")
        ]
      ()

  let set_is_backup_running =
    call ~flags:[`Session] ~name:"set_is_backup_running" ~lifecycle:removed
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; ( Bool
          , "value"
          , "true to mark this protection policy's backup is running"
          )
        ]
      ~doc:"Set the value of the is_backup_running field"
      ~allowed_roles:_R_LOCAL_ROOT_ONLY ~hide_from_docs:true ()

  let set_is_archive_running =
    call ~flags:[`Session] ~name:"set_is_archive_running" ~lifecycle:removed
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; ( Bool
          , "value"
          , "true to mark this protection policy's archive is running"
          )
        ]
      ~doc:"Set the value of the is_archive_running field"
      ~allowed_roles:_R_LOCAL_ROOT_ONLY ~hide_from_docs:true ()

  let set_is_alarm_enabled =
    call ~flags:[`Session] ~name:"set_is_alarm_enabled" ~lifecycle:removed
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (Bool, "value", "true if alarm is enabled for this policy")
        ]
      ~doc:"Set the value of the is_alarm_enabled field"
      ~allowed_roles:_R_POOL_OP ()

  let set_archive_frequency =
    call ~flags:[`Session] ~name:"set_archive_frequency" ~lifecycle:removed
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (archive_frequency, "value", "the archive frequency")
        ]
      ~doc:"Set the value of the archive_frequency field"
      ~allowed_roles:_R_POOL_OP ()

  let set_archive_target_type =
    call ~flags:[`Session] ~name:"set_archive_target_type" ~lifecycle:removed
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (archive_target_type, "value", "the archive target config type")
        ]
      ~doc:"Set the value of the archive_target_config_type field"
      ~allowed_roles:_R_POOL_OP ()

  let set_backup_frequency =
    call ~flags:[`Session] ~name:"set_backup_frequency" ~lifecycle:removed
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (backup_frequency, "value", "the backup frequency")
        ]
      ~doc:"Set the value of the backup_frequency field"
      ~allowed_roles:_R_POOL_OP ()

  let set_backup_schedule =
    call ~flags:[`Session] ~name:"set_backup_schedule" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (Map (String, String), "value", "the value to set")
        ]
      ()

  let set_archive_target_config =
    call ~flags:[`Session] ~name:"set_archive_target_config" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (Map (String, String), "value", "the value to set")
        ]
      ()

  let set_archive_schedule =
    call ~flags:[`Session] ~name:"set_archive_schedule" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (Map (String, String), "value", "the value to set")
        ]
      ()

  let set_alarm_config =
    call ~flags:[`Session] ~name:"set_alarm_config" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (Map (String, String), "value", "the value to set")
        ]
      ()

  let set_backup_last_run_time =
    call ~flags:[`Session] ~name:"set_backup_last_run_time" ~lifecycle:removed
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; ( DateTime
          , "value"
          , "The time at which the last backup was done. When the timezone is \
             missing, UTC is assumed"
          )
        ]
      ()

  let set_archive_last_run_time =
    call ~flags:[`Session] ~name:"set_archive_last_run_time" ~lifecycle:removed
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; ( DateTime
          , "value"
          , "The time at which the last archive was created. When the timezone \
             is missing, UTC is assumed"
          )
        ]
      ()

  let add_to_backup_schedule =
    call ~flags:[`Session] ~name:"add_to_backup_schedule" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (String, "key", "the key to add")
        ; (String, "value", "the value to add")
        ]
      ()

  let add_to_archive_target_config =
    call ~flags:[`Session] ~name:"add_to_archive_target_config"
      ~lifecycle:removed ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (String, "key", "the key to add")
        ; (String, "value", "the value to add")
        ]
      ()

  let add_to_archive_schedule =
    call ~flags:[`Session] ~name:"add_to_archive_schedule" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (String, "key", "the key to add")
        ; (String, "value", "the value to add")
        ]
      ()

  let add_to_alarm_config =
    call ~flags:[`Session] ~name:"add_to_alarm_config" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (String, "key", "the key to add")
        ; (String, "value", "the value to add")
        ]
      ()

  let remove_from_backup_schedule =
    call ~flags:[`Session] ~name:"remove_from_backup_schedule"
      ~lifecycle:removed ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (String, "key", "the key to remove")
        ]
      ()

  let remove_from_archive_target_config =
    call ~flags:[`Session] ~name:"remove_from_archive_target_config"
      ~lifecycle:removed ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (String, "key", "the key to remove")
        ]
      ()

  let remove_from_archive_schedule =
    call ~flags:[`Session] ~name:"remove_from_archive_schedule"
      ~lifecycle:removed ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (String, "key", "the key to remove")
        ]
      ()

  let remove_from_alarm_config =
    call ~flags:[`Session] ~name:"remove_from_alarm_config" ~lifecycle:removed
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmpp, "self", "The protection policy")
        ; (String, "key", "the key to remove")
        ]
      ()

  let t =
    create_obj ~in_db:true ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_vmpp ~descr:"VM Protection Policy"
      ~gen_events:true ~lifecycle:removed ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [
          protect_now
        ; archive_now
        ; create_alert
        ; get_alerts
        ; set_backup_retention_value
        ; set_is_backup_running
        ; set_is_archive_running
        ; set_backup_frequency
        ; set_backup_schedule
        ; set_archive_frequency
        ; set_archive_schedule
        ; set_archive_target_type
        ; set_archive_target_config
        ; set_is_alarm_enabled
        ; set_alarm_config
        ; add_to_backup_schedule
        ; add_to_archive_target_config
        ; add_to_archive_schedule
        ; add_to_alarm_config
        ; remove_from_backup_schedule
        ; remove_from_archive_target_config
        ; remove_from_archive_schedule
        ; remove_from_alarm_config
        ; set_backup_last_run_time
        ; set_archive_last_run_time
        ]
      ~contents:
        [
          uid ~lifecycle:removed _vmpp
        ; namespace ~name:"name" ~contents:(names None RW ~lifecycle:removed) ()
        ; field ~lifecycle:removed ~qualifier:RW ~ty:Bool "is_policy_enabled"
            "enable or disable this policy" ~default_value:(Some (VBool true))
        ; field ~lifecycle:removed ~qualifier:RW ~ty:backup_type "backup_type"
            "type of the backup sub-policy"
            ~default_value:(Some (VEnum "snapshot"))
        ; field ~lifecycle:removed ~qualifier:StaticRO ~ty:Int
            "backup_retention_value"
            "maximum number of backups that should be stored at any time"
            ~default_value:(Some (VInt 7L))
        ; field ~lifecycle:removed ~qualifier:StaticRO ~ty:backup_frequency
            "backup_frequency" "frequency of the backup schedule"
            ~default_value:(Some (VEnum "daily"))
        ; field ~lifecycle:removed ~qualifier:StaticRO
            ~ty:(Map (String, String))
            "backup_schedule"
            "schedule of the backup containing 'hour', 'min', 'days'. \
             Date/time-related information is in Local Timezone"
            ~default_value:(Some (VMap []))
        ; field ~lifecycle:removed ~qualifier:DynamicRO ~ty:Bool
            "is_backup_running"
            "true if this protection policy's backup is running"
        ; field ~lifecycle:removed ~qualifier:DynamicRO ~ty:DateTime
            "backup_last_run_time" "time of the last backup"
            ~default_value:(Some (VDateTime Date.epoch))
        ; field ~lifecycle:removed ~qualifier:StaticRO ~ty:archive_target_type
            "archive_target_type" "type of the archive target config"
            ~default_value:(Some (VEnum "none"))
        ; field ~lifecycle:removed ~qualifier:StaticRO
            ~ty:(Map (String, String))
            "archive_target_config"
            "configuration for the archive, including its 'location', \
             'username', 'password'"
            ~default_value:(Some (VMap []))
        ; field ~lifecycle:removed ~qualifier:StaticRO ~ty:archive_frequency
            "archive_frequency" "frequency of the archive schedule"
            ~default_value:(Some (VEnum "never"))
        ; field ~lifecycle:removed ~qualifier:StaticRO
            ~ty:(Map (String, String))
            "archive_schedule"
            "schedule of the archive containing 'hour', 'min', 'days'. \
             Date/time-related information is in Local Timezone"
            ~default_value:(Some (VMap []))
        ; field ~lifecycle:removed ~qualifier:DynamicRO ~ty:Bool
            "is_archive_running"
            "true if this protection policy's archive is running"
        ; field ~lifecycle:removed ~qualifier:DynamicRO ~ty:DateTime
            "archive_last_run_time" "time of the last archive"
            ~default_value:(Some (VDateTime Date.epoch))
        ; field ~lifecycle:removed ~qualifier:DynamicRO ~ty:(Set (Ref _vm))
            "VMs" "all VMs attached to this protection policy"
        ; field ~lifecycle:removed ~qualifier:StaticRO ~ty:Bool
            "is_alarm_enabled" "true if alarm is enabled for this policy"
            ~default_value:(Some (VBool false))
        ; field ~lifecycle:removed ~qualifier:StaticRO
            ~ty:(Map (String, String))
            "alarm_config" "configuration for the alarm"
            ~default_value:(Some (VMap []))
        ; field ~lifecycle:removed ~qualifier:DynamicRO ~ty:(Set String)
            "recent_alerts" "recent alerts" ~default_value:(Some (VSet []))
        ]
      ()
end

module VMSS = struct
  (* VM schedule snapshot *)
  let snapshot_now =
    call ~flags:[`Session] ~name:"snapshot_now" ~in_oss_since:None
      ~lifecycle:
        [
          ( Published
          , rel_falcon
          , "This call executes the snapshot schedule immediately"
          )
        ]
      ~params:[(Ref _vmss, "vmss", "Snapshot Schedule to execute")]
      ~doc:"This call executes the snapshot schedule immediately"
      ~allowed_roles:_R_POOL_OP
      ~result:(String, "An XMLRPC result")
      ()

  let type' =
    Enum
      ( "vmss_type"
      , [
          ("snapshot", "The snapshot is a disk snapshot")
        ; ("checkpoint", "The snapshot is a checkpoint")
        ; ("snapshot_with_quiesce", "Support for VSS has been removed.")
        ]
      )

  let frequency =
    Enum
      ( "vmss_frequency"
      , [
          ("hourly", "Hourly snapshots")
        ; ("daily", "Daily snapshots")
        ; ("weekly", "Weekly snapshots")
        ]
      )

  let schedule_min = "min"

  let schedule_hour = "hour"

  let schedule_days = "days"

  let set_retained_snapshots =
    call ~flags:[`Session] ~name:"set_retained_snapshots" ~in_oss_since:None
      ~lifecycle:[(Published, rel_falcon, "")]
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmss, "self", "The schedule snapshot")
        ; (Int, "value", "the value to set")
        ]
      ()

  let set_frequency =
    call ~flags:[`Session] ~name:"set_frequency" ~in_oss_since:None
      ~lifecycle:
        [(Published, rel_falcon, "Set the value of the frequency field")]
      ~params:
        [
          (Ref _vmss, "self", "The snapshot schedule")
        ; (frequency, "value", "the snapshot schedule frequency")
        ]
      ~doc:"Set the value of the frequency field" ~allowed_roles:_R_POOL_OP ()

  let set_schedule =
    call ~flags:[`Session] ~name:"set_schedule" ~in_oss_since:None
      ~lifecycle:[(Published, rel_falcon, "")]
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmss, "self", "The snapshot schedule")
        ; (Map (String, String), "value", "the value to set")
        ]
      ()

  let set_last_run_time =
    call ~flags:[`Session] ~name:"set_last_run_time" ~in_oss_since:None
      ~lifecycle:[(Published, rel_falcon, "")]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY
      ~params:
        [
          (Ref _vmss, "self", "The snapshot schedule")
        ; ( DateTime
          , "value"
          , "The time at which the schedule was last run. When the timezone is \
             missing, UTC is assumed"
          )
        ]
      ()

  let add_to_schedule =
    call ~flags:[`Session] ~name:"add_to_schedule" ~in_oss_since:None
      ~lifecycle:[(Published, rel_falcon, "")]
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmss, "self", "The snapshot schedule")
        ; (String, "key", "the key to add")
        ; (String, "value", "the value to add")
        ]
      ()

  let remove_from_schedule =
    call ~flags:[`Session] ~name:"remove_from_schedule" ~in_oss_since:None
      ~lifecycle:[(Published, rel_falcon, "")]
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmss, "self", "The snapshot schedule")
        ; (String, "key", "the key to remove")
        ]
      ()

  let set_type =
    call ~flags:[`Session] ~name:"set_type" ~in_oss_since:None
      ~lifecycle:[(Published, rel_falcon, "")]
      ~allowed_roles:_R_POOL_OP
      ~params:
        [
          (Ref _vmss, "self", "The snapshot schedule")
        ; (type', "value", "the snapshot schedule type")
        ]
      ()

  let t =
    create_obj ~in_db:true ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_vmss ~descr:"VM Snapshot Schedule"
      ~gen_events:true
      ~lifecycle:[(Published, rel_falcon, "VM Snapshot Schedule")]
      ~doccomments:[] ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [
          snapshot_now
        ; set_retained_snapshots
        ; set_frequency
        ; set_schedule
        ; add_to_schedule
        ; remove_from_schedule
        ; set_last_run_time
        ; set_type
        ]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _vmss
        ; namespace ~name:"name"
            ~contents:(names None RW ~lifecycle:[(Published, rel_rio, "")])
            ()
        ; field ~qualifier:RW ~ty:Bool
            ~lifecycle:
              [(Published, rel_rio, "enable or disable this snapshot schedule")]
            "enabled" "enable or disable this snapshot schedule"
            ~default_value:(Some (VBool true))
        ; field ~qualifier:StaticRO ~ty:type'
            ~lifecycle:[(Published, rel_rio, "type of the snapshot schedule")]
            "type" "type of the snapshot schedule"
        ; field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "maximum number of snapshots that should be stored at any \
                   time"
                )
              ]
            "retained_snapshots"
            "maximum number of snapshots that should be stored at any time"
            ~default_value:(Some (VInt 7L))
        ; field ~qualifier:StaticRO ~ty:frequency
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "frequency of taking snapshot from snapshot schedule"
                )
              ]
            "frequency" "frequency of taking snapshot from snapshot schedule"
        ; field ~qualifier:StaticRO
            ~ty:(Map (String, String))
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "schedule of the snapshot containing 'hour', 'min', 'days'. \
                   Date/time-related information is in Local Timezone"
                )
              ]
            "schedule"
            "schedule of the snapshot containing 'hour', 'min', 'days'. \
             Date/time-related information is in Local Timezone"
            ~default_value:(Some (VMap []))
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:[(Published, rel_rio, "time of the last snapshot")]
            "last_run_time" "time of the last snapshot"
            ~default_value:(Some (VDateTime Date.epoch))
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vm))
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "all VMs attached to this snapshot schedule"
                )
              ]
            "VMs" "all VMs attached to this snapshot schedule"
        ]
      ()
end

module VM_appliance = struct
  (* VM appliance *)
  let operations =
    Enum
      ( "vm_appliance_operation"
      , [
          ("start", "Start")
        ; ("clean_shutdown", "Clean shutdown")
        ; ("hard_shutdown", "Hard shutdown")
        ; ("shutdown", "Shutdown")
        ]
      )

  let start =
    call ~name:"start"
      ~lifecycle:[(Published, rel_boston, "Start all VMs in the appliance")]
      ~params:
        [
          (Ref _vm_appliance, "self", "The VM appliance")
        ; ( Bool
          , "paused"
          , "Instantiate all VMs belonging to this appliance in paused state \
             if set to true."
          )
        ]
      ~errs:[Api_errors.operation_partially_failed]
      ~doc:"Start all VMs in the appliance" ~allowed_roles:_R_POOL_OP ()

  let clean_shutdown =
    call ~name:"clean_shutdown"
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Perform a clean shutdown of all the VMs in the appliance"
          )
        ]
      ~params:[(Ref _vm_appliance, "self", "The VM appliance")]
      ~errs:[Api_errors.operation_partially_failed]
      ~doc:"Perform a clean shutdown of all the VMs in the appliance"
      ~allowed_roles:_R_POOL_OP ()

  let hard_shutdown =
    call ~name:"hard_shutdown"
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Perform a hard shutdown of all the VMs in the appliance"
          )
        ]
      ~params:[(Ref _vm_appliance, "self", "The VM appliance")]
      ~errs:[Api_errors.operation_partially_failed]
      ~doc:"Perform a hard shutdown of all the VMs in the appliance"
      ~allowed_roles:_R_POOL_OP ()

  let shutdown =
    call ~name:"shutdown"
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "For each VM in the appliance, try to shut it down cleanly. If \
             this fails, perform a hard shutdown of the VM."
          )
        ]
      ~params:[(Ref _vm_appliance, "self", "The VM appliance")]
      ~errs:[Api_errors.operation_partially_failed]
      ~doc:
        "For each VM in the appliance, try to shut it down cleanly. If this \
         fails, perform a hard shutdown of the VM."
      ~allowed_roles:_R_POOL_OP ()

  let assert_can_be_recovered =
    call ~name:"assert_can_be_recovered"
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Assert whether all SRs required to recover this VM appliance are \
             available."
          )
        ]
      ~params:
        [
          (Ref _vm_appliance, "self", "The VM appliance to recover")
        ; ( Ref _session
          , "session_to"
          , "The session to which the VM appliance is to be recovered."
          )
        ]
      ~errs:[Api_errors.vm_requires_sr]
      ~doc:
        "Assert whether all SRs required to recover this VM appliance are \
         available."
      ~allowed_roles:_R_READ_ONLY ()

  let get_SRs_required_for_recovery =
    call ~name:"get_SRs_required_for_recovery"
      ~lifecycle:
        [
          ( Published
          , rel_creedence
          , "Get the list of SRs required by the VM appliance to recover."
          )
        ]
      ~params:
        [
          ( Ref _vm_appliance
          , "self"
          , "The VM appliance for which the required list of SRs has to be \
             recovered."
          )
        ; ( Ref _session
          , "session_to"
          , "The session to which the list of SRs have to be recovered ."
          )
        ]
      ~result:(Set (Ref _sr), "refs for SRs required to recover the VM")
      ~errs:[]
      ~doc:"Get the list of SRs required by the VM appliance to recover."
      ~allowed_roles:_R_READ_ONLY ()

  let recover =
    call ~name:"recover"
      ~lifecycle:[(Published, rel_boston, "Recover the VM appliance")]
      ~params:
        [
          (Ref _vm_appliance, "self", "The VM appliance to recover")
        ; ( Ref _session
          , "session_to"
          , "The session to which the VM appliance is to be recovered."
          )
        ; ( Bool
          , "force"
          , "Whether the VMs should replace newer versions of themselves."
          )
        ]
      ~errs:[Api_errors.vm_requires_sr]
      ~doc:"Recover the VM appliance" ~allowed_roles:_R_READ_ONLY ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_boston, "VM appliance")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:true ~name:_vm_appliance ~descr:"VM appliance"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [
          start
        ; clean_shutdown
        ; hard_shutdown
        ; shutdown
        ; assert_can_be_recovered
        ; get_SRs_required_for_recovery
        ; recover
        ]
      ~contents:
        ([
           uid
             ~lifecycle:
               [(Published, rel_rio, "Unique identifier/object reference")]
             _vm_appliance
         ; namespace ~name:"name"
             ~contents:(names None RW ~lifecycle:[(Published, rel_rio, "")])
             ()
         ]
        @ allowed_and_current_operations operations
        @ [
            field ~qualifier:DynamicRO ~ty:(Set (Ref _vm))
              ~lifecycle:[(Published, rel_rio, "all VMs in this appliance")]
              "VMs" "all VMs in this appliance"
          ]
        )
      ()
end

module DR_task = struct
  (* DR_task *)
  let create =
    call ~name:"create"
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Create a disaster recovery task which will query the supplied \
             list of devices"
          )
        ]
      ~params:
        [
          (String, "type", "The SR driver type of the SRs to introduce")
        ; ( Map (String, String)
          , "device_config"
          , "The device configuration of the SRs to introduce"
          )
        ; (Set String, "whitelist", "The devices to use for disaster recovery")
        ]
      ~result:(Ref _dr_task, "The reference of the created DR_task")
      ~doc:
        "Create a disaster recovery task which will query the supplied list of \
         devices"
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Destroy the disaster recovery task, detaching and forgetting any \
             SRs introduced which are no longer required"
          )
        ]
      ~params:[(Ref _dr_task, "self", "The disaster recovery task to destroy")]
      ~doc:
        "Destroy the disaster recovery task, detaching and forgetting any SRs \
         introduced which are no longer required"
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_boston, "DR task")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_dr_task ~descr:"DR task"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~messages:[create; destroy]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _dr_task
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _sr))
            ~lifecycle:
              [(Published, rel_rio, "All SRs introduced by this appliance")]
            "introduced_SRs" "All SRs introduced by this appliance"
        ]
      ()
end

(** events handling: *)

module Event = struct
  let operation =
    Enum
      ( "event_operation"
      , [
          ("add", "An object has been created")
        ; ("del", "An object has been deleted")
        ; ("mod", "An object has been modified")
        ]
      )

  let register =
    call ~name:"register"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Registers this session with the event system for a set of given \
             classes. This method is only recommended for legacy use in \
             conjunction with event.next."
          )
        ; (Deprecated, rel_boston, "")
        ]
      ~params:
        [
          ( Set String
          , "classes"
          , "the classes for which the session will register with the event \
             system; specifying * as the desired class will register for all \
             classes"
          )
        ]
      ~doc:
        "Registers this session with the event system for a set of given \
         classes. This method is only recommended for legacy use in \
         conjunction with event.next."
      ~allowed_roles:_R_ALL ()

  let unregister =
    call ~name:"unregister"
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Removes this session's registration with the event system for a \
             set of given classes. This method is only recommended for legacy \
             use in conjunction with event.next."
          )
        ; (Deprecated, rel_boston, "")
        ]
      ~params:
        [
          ( Set String
          , "classes"
          , "the classes for which the session's registration with the event \
             system will be removed"
          )
        ]
      ~doc:
        "Removes this session's registration with the event system for a set \
         of given classes. This method is only recommended for legacy use in \
         conjunction with event.next."
      ~allowed_roles:_R_ALL ()

  let next =
    call ~name:"next" ~params:[]
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Blocking call which returns a (possibly empty) batch of events. \
             This method is only recommended for legacy use. New development \
             should use event.from which supersedes this method."
          )
        ; (Deprecated, rel_boston, "")
        ]
      ~doc:
        "Blocking call which returns a (possibly empty) batch of events. This \
         method is only recommended for legacy use.It stores events in a \
         buffer of limited size, raising EVENTS_LOST if too many events got \
         generated. New development should use event.from which supersedes \
         this method."
      ~custom_marshaller:true ~flags:[`Session]
      ~result:(Set (Record _event), "A set of events")
      ~errs:
        [
          Api_errors.session_not_registered
        ; Api_errors.events_lost
        ; Api_errors.event_subscription_parse_failure
        ]
      ~allowed_roles:_R_ALL ()

  let from =
    call ~name:"from"
      ~params:
        [
          ( Set String
          , "classes"
          , "register for events for the indicated classes"
          )
        ; ( String
          , "token"
          , "A token representing the point from which to generate database \
             events. The empty string represents the beginning."
          )
        ; (Float, "timeout", "Return after this many seconds if no events match")
        ]
      ~lifecycle:
        [
          ( Published
          , rel_boston
          , "Blocking call which returns a new token and a (possibly empty) \
             batch of events. The returned token can be used in subsequent \
             calls to this function."
          )
        ]
      ~doc:
        "Blocking call which returns a new token and a (possibly empty) batch \
         of events. The returned token can be used in subsequent calls to this \
         function. It eliminates redundant events (e.g. same field updated \
         multiple times)."
      ~custom_marshaller:true ~flags:[`Session]
      ~result:
        ( Set (Record _event)
        , "a structure consisting of a token ('token'), a map of valid \
           references per object type ('valid_ref_counts'), and a set of event \
           records ('events')."
        )
        (*In reality the event batch is not a set of records as stated here.
          Due to the difficulty of representing this in the datamodel, the doc is generated manually,
          so ensure the markdown_backend.ml and gen_json.ml is updated if something changes. *)
      ~errs:
        [
          Api_errors.event_from_token_parse_failure
        ; Api_errors.event_subscription_parse_failure
        ]
      ~allowed_roles:_R_ALL ()

  let get_current_id =
    call ~name:"get_current_id" ~params:[]
      ~lifecycle:
        [
          ( Published
          , rel_rio
          , "Return the ID of the next event to be generated by the system"
          )
        ]
      ~doc:"Return the ID of the next event to be generated by the system"
      ~flags:[`Session] ~result:(Int, "the event ID") ~allowed_roles:_R_ALL ()

  let inject =
    call ~name:"inject"
      ~params:
        [
          (String, "class", "class of the object")
        ; (String, "ref", "A reference to the object that will be changed.")
        ]
      ~lifecycle:
        [
          ( Published
          , rel_tampa
          , "Injects an artificial event on the given object and returns the \
             corresponding ID in the form of a token, which can be used as a \
             point of reference for database events. For example, to check \
             whether an object has reached the right state before attempting \
             an operation, one can inject an artificial event on the object \
             and wait until the token returned by consecutive event.from calls \
             is lexicographically greater than the one returned by \
             event.inject."
          )
        ]
      ~doc:
        "Injects an artificial event on the given object and returns the \
         corresponding ID in the form of a token, which can be used as a point \
         of reference for database events. For example, to check whether an \
         object has reached the right state before attempting an operation, \
         one can inject an artificial event on the object and wait until the \
         token returned by consecutive event.from calls is lexicographically \
         greater than the one returned by event.inject."
      ~flags:[`Session]
      ~result:(String, "the event ID in the form of a token")
      ~allowed_roles:_R_ALL ()
  (* !!! This should call create_obj ~in_db:true like everything else... !!! *)

  let t =
    {
      obj_lifecycle= Lifecycle.from [(Published, rel_rio, "")]
    ; name= _event
    ; gen_events= false
    ; description= "Asynchronous event registration and handling"
    ; gen_constructor_destructor= false
    ; doccomments= []
    ; messages= [register; unregister; next; from; get_current_id; inject]
    ; obj_release=
        {
          internal= get_product_releases rel_rio
        ; opensource= get_oss_releases (Some "3.0.3")
        ; internal_deprecated_since= None
        }
    ; contents=
        [
          field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:Int
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "An ID, monotonically increasing, and local to the current \
                   session"
                )
              ]
            "id"
            "An ID, monotonically increasing, and local to the current session"
        ; field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:DateTime
            ~lifecycle:
              [
                (Published, rel_rio, "The time at which the event occurred")
              ; (Deprecated, rel_boston, "")
              ]
            "timestamp" "The time at which the event occurred"
        ; field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The name of the class of the object that changed"
                )
              ]
            "class" "The name of the class of the object that changed"
        ; field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:operation
            ~lifecycle:
              [(Published, rel_rio, "The operation that was performed")]
            "operation" "The operation that was performed"
        ; field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String
            ~lifecycle:
              [(Published, rel_rio, "A reference to the object that changed")]
            "ref" "A reference to the object that changed"
        ; field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String
            ~lifecycle:
              [
                (Published, rel_rio, "The uuid of the object that changed")
              ; (Deprecated, rel_boston, "")
              ]
            "obj_uuid" "The uuid of the object that changed"
        ]
    ; (* As of tampa, the event record has one more field, snapshot, which is the record of the object changed.
         Due to the difficulty of representing this in the datamodel, the doc is generated manually,
         so ensure the markdown_backend.ml and gen_json.ml is updated if something changes. *)
      persist= PersistNothing
    ; in_database= false
    ; force_custom_actions= None
    ; obj_allowed_roles= _R_POOL_ADMIN
    ; obj_implicit_msg_allowed_roles= _R_ALL
    ; obj_doc_tags= []
    ; db_logging= None
    }
end

(** Blobs - binary blobs of data *)

module Blob = struct
  let create =
    call ~name:"create"
      ~lifecycle:
        [(Published, rel_orlando, "Create a placeholder for a binary blob")]
      ~versioned_params:
        [
          {
            param_type= String
          ; param_name= "mime_type"
          ; param_doc=
              "The mime-type of the blob. Defaults to \
               'application/octet-stream' if the empty string is supplied"
          ; param_release= orlando_release
          ; param_default= None
          }
        ; {
            param_type= Bool
          ; param_name= "public"
          ; param_doc= "True if the blob should be publicly available"
          ; param_release= tampa_release
          ; param_default= Some (VBool false)
          }
        ]
      ~doc:"Create a placeholder for a binary blob" ~flags:[`Session]
      ~result:(Ref _blob, "The reference of the created blob")
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:[(Ref _blob, "self", "The reference of the blob to destroy")]
      ~flags:[`Session] ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~in_db:true
      ~lifecycle:[(Published, rel_orlando, "A placeholder for a binary blob")]
      ~in_oss_since:None ~persist:PersistEverything
      ~gen_constructor_destructor:false ~name:_blob
      ~descr:"A placeholder for a binary blob" ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP ~messages:[create; destroy]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _blob
        ; namespace ~name:"name"
            ~contents:
              (names oss_since_303 RW ~lifecycle:[(Published, rel_rio, "")])
            ()
        ; field ~qualifier:DynamicRO ~ty:Int
            ~lifecycle:
              [(Published, rel_rio, "Size of the binary data, in bytes")]
            "size" "Size of the binary data, in bytes"
        ; field ~writer_roles:_R_POOL_OP ~qualifier:RW
            ~lifecycle:
              [
                (Published, rel_tampa, "True if the blob is publicly accessible")
              ]
            ~default_value:(Some (VBool false)) ~ty:Bool "public"
            "True if the blob is publicly accessible"
        ; field ~qualifier:StaticRO ~ty:DateTime
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "Time at which the data in the blob was last updated"
                )
              ]
            "last_updated" "Time at which the data in the blob was last updated"
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The mime type associated with this object. Defaults to \
                   'application/octet-stream' if the empty string is supplied"
                )
              ]
            "mime_type"
            "The mime type associated with this object. Defaults to \
             'application/octet-stream' if the empty string is supplied"
        ]
      ()
end

module Message = struct
  let cls =
    Enum
      ( "cls"
      , [
          ("VM", "VM")
        ; ("Host", "Host")
        ; ("SR", "SR")
        ; ("Pool", "Pool")
        ; ("VMPP", "VMPP")
        ; ("VMSS", "VMSS")
        ; ("PVS_proxy", "PVS_proxy")
        ; ("VDI", "VDI")
        ; ("Certificate", "Certificate")
        ]
      )

  let create =
    call ~name:"create"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:
        [
          (String, "name", "The name of the message")
        ; (Int, "priority", "The priority of the message")
        ; (cls, "cls", "The class of object this message is associated with")
        ; ( String
          , "obj_uuid"
          , "The uuid of the object this message is associated with"
          )
        ; (String, "body", "The body of the message")
        ]
      ~flags:[`Session]
      ~result:(Ref _message, "The reference of the created message")
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:
        [(Ref _message, "self", "The reference of the message to destroy")]
      ~flags:[`Session] ~allowed_roles:_R_POOL_OP ()

  let destroy_many =
    call ~name:"destroy_many" ~lifecycle:[]
      ~params:[(Set (Ref _message), "messages", "Messages to destroy")]
      ~allowed_roles:_R_POOL_OP ()

  let get_all =
    call ~name:"get_all"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:[] ~flags:[`Session]
      ~result:(Set (Ref _message), "The references to the messages")
      ~allowed_roles:_R_READ_ONLY ()

  let get =
    call ~name:"get"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:
        [
          (cls, "cls", "The class of object")
        ; (String, "obj_uuid", "The uuid of the object")
        ; ( DateTime
          , "since"
          , "The cutoff time. When the timezone is missing, UTC is assumed"
          )
        ]
      ~flags:[`Session]
      ~result:(Map (Ref _message, Record _message), "The relevant messages")
      ~allowed_roles:_R_READ_ONLY ()

  let get_since =
    call ~name:"get_since"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:
        [
          ( DateTime
          , "since"
          , "The cutoff time. When the timezone is missing, UTC is assumed"
          )
        ]
      ~flags:[`Session]
      ~result:(Map (Ref _message, Record _message), "The relevant messages")
      ~allowed_roles:_R_READ_ONLY ()

  let get_by_uuid =
    call ~name:"get_by_uuid"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:[(String, "uuid", "The uuid of the message")]
      ~flags:[`Session]
      ~result:(Ref _message, "The message reference")
      ~allowed_roles:_R_READ_ONLY ()

  let get_record =
    call ~name:"get_record"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:[(Ref _message, "self", "The reference to the message")]
      ~flags:[`Session]
      ~result:(Record _message, "The message record")
      ~allowed_roles:_R_READ_ONLY ()

  let get_all_records =
    call ~name:"get_all_records"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:[] ~flags:[`Session]
      ~result:(Map (Ref _message, Record _message), "The messages")
      ~allowed_roles:_R_READ_ONLY ()

  let get_all_records_where =
    call ~name:"get_all_records_where"
      ~lifecycle:[(Published, rel_orlando, "")]
      ~params:[(String, "expr", "The expression to match")]
      ~flags:[`Session]
      ~result:(Map (Ref _message, Record _message), "The messages")
      ~allowed_roles:_R_READ_ONLY ()

  let t =
    create_obj ~in_db:false
      ~lifecycle:
        [
          ( Published
          , rel_orlando
          , "An message for the attention of the administrator"
          )
        ]
      ~in_oss_since:None ~persist:PersistNothing
      ~gen_constructor_destructor:false ~name:_message
      ~descr:"An message for the attention of the administrator"
      ~gen_events:true ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:
        [
          create
        ; destroy
        ; destroy_many
        ; get
        ; get_all
        ; get_since
        ; get_record
        ; get_by_uuid
        ; get_all_records
        ; get_all_records_where
        ]
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _message
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "The name of the message")]
            "name" "The name of the message"
        ; field ~qualifier:DynamicRO ~ty:Int
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The message priority, 0 being low priority"
                )
              ]
            "priority" "The message priority, 0 being low priority"
        ; field ~qualifier:DynamicRO ~ty:cls
            ~lifecycle:
              [
                (Published, rel_orlando, "")
              ; (Extended, "1.313.0", "Added Certificate class")
              ]
            "cls" "The class of the object this message is associated with"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:
              [
                ( Published
                , rel_rio
                , "The uuid of the object this message is associated with"
                )
              ]
            "obj_uuid" "The uuid of the object this message is associated with"
        ; field ~qualifier:DynamicRO ~ty:DateTime
            ~lifecycle:
              [
                (Published, rel_rio, "The time at which the message was created")
              ]
            "timestamp" "The time at which the message was created"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_rio, "The body of the message")]
            "body" "The body of the message"
        ]
      ()
end

module Secret = struct
  let introduce =
    call ~name:"introduce"
      ~lifecycle:[(Published, rel_midnight_ride, "")]
      ~versioned_params:
        [
          {
            param_type= String
          ; param_name= "uuid"
          ; param_doc= ""
          ; param_release= midnight_ride_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "value"
          ; param_doc= ""
          ; param_release= midnight_ride_release
          ; param_default= None
          }
        ; {
            param_type= Map (String, String)
          ; param_name= "other_config"
          ; param_doc= ""
          ; param_release= boston_release
          ; param_default= Some (VMap [])
          }
        ]
      ~flags:[`Session] ~result:(Ref _secret, "") ~secret:true
      ~hide_from_docs:true ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~descr:"A secret" ~doccomments:[]
      ~gen_constructor_destructor:true ~gen_events:false ~in_db:true
      ~in_oss_since:None
      ~lifecycle:[(Published, rel_midnight_ride, "A secret")]
      ~messages:[introduce] ~messages_default_allowed_roles:_R_POOL_OP
      ~implicit_messages_allowed_roles:_R_POOL_OP ~name:_secret
      ~persist:PersistEverything
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            ~reader_roles:_R_POOL_OP _secret
        ; field ~reader_roles:_R_POOL_OP ~qualifier:RW ~ty:String
            ~lifecycle:[(Published, rel_rio, "the secret")]
            "value" "the secret"
        ; field ~qualifier:RW
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_rio, "other_config")]
            "other_config" "other_config" ~default_value:(Some (VMap []))
        ]
      ()
end

(** network sriov **)
module Network_sriov = struct
  let lifecycle = [(Published, rel_kolkata, "")]

  let sriov_configuration_mode =
    Enum
      ( "sriov_configuration_mode"
      , [
          ("sysfs", "Configure network sriov by sysfs, do not need reboot")
        ; ("modprobe", "Configure network sriov by modprobe, need reboot")
        ; ("manual", "Configure network sriov manually")
        ; ("unknown", "Unknown mode")
        ]
      )

  let create =
    call ~name:"create"
      ~doc:
        "Enable SR-IOV on the specific PIF. It will create a network-sriov \
         based on the specific PIF and automatically create a logical PIF to \
         connect the specific network."
      ~params:
        [
          (Ref _pif, "pif", "PIF on which to enable SR-IOV")
        ; ( Ref _network
          , "network"
          , "Network to connect SR-IOV virtual functions with VM VIFs"
          )
        ]
      ~result:
        (Ref _network_sriov, "The reference of the created network_sriov object")
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~doc:
        "Disable SR-IOV on the specific PIF. It will destroy the network-sriov \
         and the logical PIF accordingly."
      ~params:[(Ref _network_sriov, "self", "SRIOV to destroy")]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let get_remaining_capacity =
    call ~name:"get_remaining_capacity"
      ~doc:"Get the number of free SR-IOV VFs on the associated PIF"
      ~params:[(Ref _network_sriov, "self", "the NETWORK_SRIOV object")]
      ~lifecycle
      ~result:(Int, "The number of free SR-IOV VFs on the associated PIF")
      ~allowed_roles:_R_READ_ONLY ()

  let t =
    create_obj ~name:_network_sriov
      ~descr:"network-sriov which connects logical pif and physical pif"
      ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
      ~in_db:true ~lifecycle
      ~messages:[create; destroy; get_remaining_capacity]
      ~messages_default_allowed_roles:_R_POOL_OP ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:
        [
          uid
            ~lifecycle:
              [(Published, rel_rio, "Unique identifier/object reference")]
            _network_sriov
        ; field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle "physical_PIF"
            "The PIF that has SR-IOV enabled" ~default_value:(Some (VRef ""))
        ; field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle "logical_PIF"
            "The logical PIF to connect to the SR-IOV network after enable \
             SR-IOV on the physical PIF"
            ~default_value:(Some (VRef ""))
        ; field ~qualifier:DynamicRO ~ty:Bool ~lifecycle "requires_reboot"
            "Indicates whether the host need to be rebooted before SR-IOV is \
             enabled on the physical PIF"
            ~default_value:(Some (VBool false))
        ; field ~qualifier:DynamicRO ~ty:sriov_configuration_mode ~lifecycle
            "configuration_mode" "The mode for configure network sriov"
            ~default_value:(Some (VEnum "unknown"))
        ]
      ()
end

(** PCI devices *)
let pci_dom0_access =
  Enum
    ( "pci_dom0_access"
    , [
        ("enabled", "dom0 can access this device as normal")
      ; ( "disable_on_reboot"
        , "On host reboot dom0 will be blocked from accessing this device"
        )
      ; ("disabled", "dom0 cannot access this device")
      ; ( "enable_on_reboot"
        , "On host reboot dom0 will be allowed to access this device"
        )
      ]
    )

module PCI = struct
  let disable_dom0_access =
    call ~name:"disable_dom0_access" ~lifecycle:[]
      ~doc:
        "Hide a PCI device from the dom0 kernel. (Takes affect after next \
         boot.)"
      ~params:[(Ref _pci, "self", "The PCI to hide")]
      ~result:(pci_dom0_access, "The accessibility of this PCI from dom0")
      ~allowed_roles:_R_POOL_OP ()

  let enable_dom0_access =
    call ~name:"enable_dom0_access" ~lifecycle:[]
      ~doc:
        "Unhide a PCI device from the dom0 kernel. (Takes affect after next \
         boot.)"
      ~params:[(Ref _pci, "self", "The PCI to unhide")]
      ~result:(pci_dom0_access, "The accessibility of this PCI from dom0")
      ~allowed_roles:_R_POOL_OP ()

  let get_dom0_access_status =
    call ~name:"get_dom0_access_status" ~lifecycle:[]
      ~doc:"Return a PCI device dom0 access status."
      ~params:[(Ref _pci, "self", "The PCI")]
      ~result:(pci_dom0_access, "The accessibility of this PCI from dom0")
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~name:_pci ~descr:"A PCI device" ~doccomments:[]
      ~gen_constructor_destructor:false ~gen_events:true ~in_db:true
      ~lifecycle:[(Published, rel_boston, "")]
      ~messages:
        [disable_dom0_access; enable_dom0_access; get_dom0_access_status]
      ~messages_default_allowed_roles:_R_POOL_OP ~persist:PersistEverything
      ~in_oss_since:None ~db_logging:Log_destroy
      ~contents:
        [
          uid _pci ~lifecycle:[(Published, rel_boston, "")]
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_boston, "")]
            "class_id" "PCI class ID" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_boston, "")]
            "class_name" "PCI class name" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_boston, "")]
            "vendor_id" "Vendor ID" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_boston, "")]
            "vendor_name" "Vendor name" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_boston, "")]
            "device_id" "Device ID" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_boston, "")]
            "device_name" "Device name" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:(Ref _host)
            ~lifecycle:[(Published, rel_boston, "")]
            "host" "Physical machine that owns the PCI device"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_boston, "")]
            "pci_id" "PCI ID of the physical device"
            ~default_value:(Some (VString ""))
        ; field ~qualifier:DynamicRO ~ty:Int
            ~lifecycle:[(Published, rel_boston, "")]
            ~default_value:(Some (VInt 1L)) "functions"
            "Number of physical + virtual PCI functions" ~internal_only:true
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pci))
            ~lifecycle:[(Published, rel_falcon, "")]
            "virtual_functions"
            "Set of VF PCI devices provided by this physical (PF) PCI device"
            ~internal_only:true
        ; field ~qualifier:StaticRO ~ty:(Ref _pci)
            ~lifecycle:[(Published, rel_falcon, "")]
            "physical_function"
            "The PF (physical PCI device) that provides this VF"
            ~default_value:(Some (VRef null_ref)) ~internal_only:true
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vm))
            ~lifecycle:[(Published, rel_boston, "")]
            "attached_VMs"
            "VMs that currently have a function of this PCI device \
             passed-through to them"
            ~internal_only:true
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pci))
            ~lifecycle:[(Published, rel_boston, "")]
            "dependencies" "List of dependent PCI devices"
            ~ignore_foreign_key:true
        ; field ~qualifier:RW
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_boston, "")]
            "other_config" "Additional configuration"
            ~default_value:(Some (VMap []))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_clearwater_whetstone, "")]
            "subsystem_vendor_id" "Subsystem vendor ID"
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_clearwater_whetstone, "")]
            "subsystem_vendor_name" "Subsystem vendor name"
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_clearwater_whetstone, "")]
            "subsystem_device_id" "Subsystem device ID"
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_clearwater_whetstone, "")]
            "subsystem_device_name" "Subsystem device name"
            ~default_value:(Some (VString ""))
        ; field ~qualifier:DynamicRO ~ty:(Ref _vm)
            ~lifecycle:[(Published, rel_falcon, "")]
            ~internal_only:true "scheduled_to_be_attached_to"
            "The VM to which this PCI device is scheduled to be attached \
             (passed through)"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_kolkata, "")]
            "driver_name" "Driver name" ~default_value:(Some (VString ""))
        ]
      ()
end

(** Physical GPUs (pGPU) *)

module PGPU = struct
  let add_enabled_VGPU_types =
    call ~name:"add_enabled_VGPU_types"
      ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
      ~versioned_params:
        [
          {
            param_type= Ref _pgpu
          ; param_name= "self"
          ; param_doc= "The PGPU to which we are adding an enabled VGPU type"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ; {
            param_type= Ref _vgpu_type
          ; param_name= "value"
          ; param_doc= "The VGPU type to enable"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ]
      ~allowed_roles:_R_POOL_OP ()

  let remove_enabled_VGPU_types =
    call ~name:"remove_enabled_VGPU_types"
      ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
      ~versioned_params:
        [
          {
            param_type= Ref _pgpu
          ; param_name= "self"
          ; param_doc=
              "The PGPU from which we are removing an enabled VGPU type"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ; {
            param_type= Ref _vgpu_type
          ; param_name= "value"
          ; param_doc= "The VGPU type to disable"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ]
      ~allowed_roles:_R_POOL_OP ()

  let set_enabled_VGPU_types =
    call ~name:"set_enabled_VGPU_types"
      ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
      ~versioned_params:
        [
          {
            param_type= Ref _pgpu
          ; param_name= "self"
          ; param_doc= "The PGPU on which we are enabling a set of VGPU types"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ; {
            param_type= Set (Ref _vgpu_type)
          ; param_name= "value"
          ; param_doc= "The VGPU types to enable"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ]
      ~allowed_roles:_R_POOL_OP ()

  let set_GPU_group =
    call ~name:"set_GPU_group"
      ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
      ~versioned_params:
        [
          {
            param_type= Ref _pgpu
          ; param_name= "self"
          ; param_doc= "The PGPU to move to a new group"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ; {
            param_type= Ref _gpu_group
          ; param_name= "value"
          ; param_doc= "The group to which the PGPU will be moved"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ]
      ~allowed_roles:_R_POOL_OP ()

  let get_remaining_capacity =
    call ~name:"get_remaining_capacity"
      ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
      ~versioned_params:
        [
          {
            param_type= Ref _pgpu
          ; param_name= "self"
          ; param_doc= "The PGPU to query"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ; {
            param_type= Ref _vgpu_type
          ; param_name= "vgpu_type"
          ; param_doc=
              "The VGPU type for which we want to find the number of VGPUs \
               which can still be started on this PGPU"
          ; param_release= vgpu_tech_preview_release
          ; param_default= None
          }
        ]
      ~result:
        ( Int
        , "The number of VGPUs of the specified type which can still be \
           started on this PGPU"
        )
      ~allowed_roles:_R_READ_ONLY ()

  let enable_dom0_access =
    call ~name:"enable_dom0_access"
      ~lifecycle:
        [
          (Published, rel_cream, "")
        ; (Deprecated, "24.14.0", "Use PCI.enable_dom0_access instead.")
        ]
      ~versioned_params:
        [
          {
            param_type= Ref _pgpu
          ; param_name= "self"
          ; param_doc= "The PGPU to which dom0 will be granted access"
          ; param_release= cream_release
          ; param_default= None
          }
        ]
      ~result:(pci_dom0_access, "The accessibility of this PGPU from dom0")
      ~allowed_roles:_R_POOL_OP ()

  let disable_dom0_access =
    call ~name:"disable_dom0_access"
      ~lifecycle:
        [
          (Published, rel_cream, "")
        ; (Deprecated, "24.14.0", "Use PCI.disable_dom0_access instead.")
        ]
      ~versioned_params:
        [
          {
            param_type= Ref _pgpu
          ; param_name= "self"
          ; param_doc= "The PGPU to which dom0 will be denied access"
          ; param_release= cream_release
          ; param_default= None
          }
        ]
      ~result:(pci_dom0_access, "The accessibility of this PGPU from dom0")
      ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~name:_pgpu ~descr:"A physical GPU (pGPU)" ~doccomments:[]
      ~gen_constructor_destructor:false ~gen_events:true ~in_db:true
      ~lifecycle:[(Published, rel_boston, "")]
      ~messages:
        [
          add_enabled_VGPU_types
        ; remove_enabled_VGPU_types
        ; set_enabled_VGPU_types
        ; set_GPU_group
        ; get_remaining_capacity
        ; enable_dom0_access
        ; disable_dom0_access
        ]
      ~messages_default_allowed_roles:_R_POOL_OP ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:
        [
          uid _pgpu ~lifecycle:[(Published, rel_boston, "")]
        ; field ~qualifier:StaticRO ~ty:(Ref _pci)
            ~lifecycle:[(Published, rel_boston, "")]
            "PCI" "Link to underlying PCI device"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:StaticRO ~ty:(Ref _gpu_group)
            ~lifecycle:[(Published, rel_boston, "")]
            "GPU_group" "GPU group the pGPU is contained in"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:DynamicRO ~ty:(Ref _host)
            ~lifecycle:[(Published, rel_boston, "")]
            "host" "Host that owns the GPU" ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:RW
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_boston, "")]
            "other_config" "Additional configuration"
            ~default_value:(Some (VMap []))
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type))
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "supported_VGPU_types"
            "List of VGPU types supported by the underlying hardware"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type))
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "enabled_VGPU_types"
            "List of VGPU types which have been enabled for this PGPU"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu))
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "resident_VGPUs" "List of VGPUs running on this PGPU"
        ; field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            ~internal_only:true
            ~default_value:(Some (VInt Constants.pgpu_default_size)) "size"
            "Abstract size of this PGPU"
        ; field ~qualifier:DynamicRO
            ~ty:(Map (Ref _vgpu_type, Int))
            ~lifecycle:[(Published, rel_vgpu_productisation, "")]
            ~default_value:(Some (VMap [])) "supported_VGPU_max_capacities"
            "A map relating each VGPU type supported on this GPU to the \
             maximum number of VGPUs of that type which can run simultaneously \
             on this GPU"
        ; field ~qualifier:DynamicRO ~ty:pci_dom0_access
            ~lifecycle:
              [
                (Published, rel_cream, "")
              ; ( Deprecated
                , "24.14.0"
                , "Use PCI.get_dom0_access_status instead."
                )
              ]
            ~default_value:(Some (VEnum "enabled")) "dom0_access"
            "The accessibility of this device from dom0"
        ; field ~qualifier:DynamicRO ~ty:Bool
            ~lifecycle:[(Published, rel_cream, "")]
            ~default_value:(Some (VBool false)) "is_system_display_device"
            "Is this device the system display device"
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_inverness, "")]
            ~default_value:(Some (VMap [])) "compatibility_metadata"
            "PGPU metadata to determine whether a VGPU can migrate between two \
             PGPUs"
        ]
      ()
end

(** Groups of GPUs *)

module GPU_group = struct
  let create =
    call ~name:"create"
      ~lifecycle:[(Published, rel_boston, "")]
      ~versioned_params:
        [
          {
            param_type= String
          ; param_name= "name_label"
          ; param_doc= ""
          ; param_release= boston_release
          ; param_default= Some (VString "")
          }
        ; {
            param_type= String
          ; param_name= "name_description"
          ; param_doc= ""
          ; param_release= boston_release
          ; param_default= Some (VString "")
          }
        ; {
            param_type= Map (String, String)
          ; param_name= "other_config"
          ; param_doc= ""
          ; param_release= boston_release
          ; param_default= Some (VMap [])
          }
        ]
      ~result:(Ref _gpu_group, "The reference of the created GPU_group")
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~lifecycle:[(Published, rel_boston, "")]
      ~params:[(Ref _gpu_group, "self", "The GPU group to destroy")]
      ~allowed_roles:_R_POOL_OP ()

  let update_enabled_VGPU_types =
    call ~name:"update_enabled_VGPU_types" ~hide_from_docs:true
      ~lifecycle:[(Published, rel_vgpu_productisation, "")]
      ~params:[(Ref _gpu_group, "self", "The GPU group to update")]
      ~allowed_roles:_R_POOL_OP ()

  let update_supported_VGPU_types =
    call ~name:"update_supported_VGPU_types" ~hide_from_docs:true
      ~lifecycle:[(Published, rel_vgpu_productisation, "")]
      ~params:[(Ref _gpu_group, "self", "The GPU group to update")]
      ~allowed_roles:_R_POOL_OP ()

  let get_remaining_capacity =
    call ~name:"get_remaining_capacity"
      ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
      ~params:
        [
          (Ref _gpu_group, "self", "The GPU group to query")
        ; ( Ref _vgpu_type
          , "vgpu_type"
          , "The VGPU_type for which the remaining capacity will be calculated"
          )
        ]
      ~result:
        ( Int
        , "The number of VGPUs of the given type which can still be started on \
           the PGPUs in the group"
        )
      ~allowed_roles:_R_READ_ONLY ()

  let allocation_algorithm =
    Enum
      ( "allocation_algorithm"
      , [
          ( "breadth_first"
          , "vGPUs of a given type are allocated evenly across supporting \
             pGPUs."
          )
        ; ( "depth_first"
          , "vGPUs of a given type are allocated on supporting pGPUs until \
             they are full."
          )
        ]
      )

  let t =
    create_obj ~name:_gpu_group
      ~descr:"A group of compatible GPUs across the resource pool"
      ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
      ~in_db:true
      ~lifecycle:[(Published, rel_boston, "")]
      ~messages:
        [
          create
        ; destroy
        ; update_enabled_VGPU_types
        ; update_supported_VGPU_types
        ; get_remaining_capacity
        ]
      ~messages_default_allowed_roles:_R_POOL_OP ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:
        [
          uid _gpu_group ~lifecycle:[(Published, rel_boston, "")]
        ; namespace ~name:"name"
            ~contents:(names None RW ~lifecycle:[(Published, rel_boston, "")])
            ()
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pgpu))
            ~lifecycle:[(Published, rel_boston, "")]
            "PGPUs" "List of pGPUs in the group"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu))
            ~lifecycle:[(Published, rel_boston, "")]
            "VGPUs" "List of vGPUs using the group"
        ; field ~qualifier:DynamicRO ~ty:(Set String)
            ~lifecycle:[(Published, rel_boston, "")]
            "GPU_types"
            "List of GPU types (vendor+device ID) that can be in this group"
            ~default_value:(Some (VSet []))
        ; field ~qualifier:RW
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_boston, "")]
            "other_config" "Additional configuration"
            ~default_value:(Some (VMap []))
        ; field ~qualifier:RW ~ty:allocation_algorithm
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "allocation_algorithm"
            "Current allocation of vGPUs to pGPUs for this group"
            ~default_value:(Some (VEnum "depth_first"))
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type))
            ~lifecycle:[(Published, rel_vgpu_productisation, "")]
            "supported_VGPU_types"
            "vGPU types supported on at least one of the pGPUs in this group"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type))
            ~lifecycle:[(Published, rel_vgpu_productisation, "")]
            "enabled_VGPU_types"
            "vGPU types supported on at least one of the pGPUs in this group"
        ]
      ()
end

(** Virtual GPUs (vGPU) *)

module VGPU = struct
  let create =
    call ~name:"create"
      ~lifecycle:[(Published, rel_boston, "")]
      ~versioned_params:
        [
          {
            param_type= Ref _vm
          ; param_name= "VM"
          ; param_doc= ""
          ; param_release= boston_release
          ; param_default= None
          }
        ; {
            param_type= Ref _gpu_group
          ; param_name= "GPU_group"
          ; param_doc= ""
          ; param_release= boston_release
          ; param_default= None
          }
        ; {
            param_type= String
          ; param_name= "device"
          ; param_doc= ""
          ; param_release= boston_release
          ; param_default= Some (VString "0")
          }
        ; {
            param_type= Map (String, String)
          ; param_name= "other_config"
          ; param_doc= ""
          ; param_release= boston_release
          ; param_default= Some (VMap [])
          }
        ; {
            param_type= Ref _vgpu_type
          ; param_name= "type"
          ; param_doc= ""
          ; param_release= vgpu_tech_preview_release
          ; param_default= Some (VRef null_ref)
          }
        ]
      ~result:(Ref _vgpu, "The reference of the created VGPU object")
      ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy"
      ~lifecycle:[(Published, rel_boston, "")]
      ~params:[(Ref _vgpu, "self", "The vGPU to destroy")]
      ~allowed_roles:_R_POOL_OP ()

  let atomic_set_resident_on =
    call ~name:"atomic_set_resident_on"
      ~lifecycle:[(Published, rel_dundee, "")]
      ~params:
        [
          (Ref _vgpu, "self", "The vGPU to modify")
        ; (Ref _pgpu, "value", "The pGPU on which the vGPU is running")
        ]
      ~allowed_roles:_R_LOCAL_ROOT_ONLY ~hide_from_docs:true ~pool_internal:true
      ()

  let t =
    create_obj ~name:_vgpu ~descr:"A virtual GPU (vGPU)" ~doccomments:[]
      ~gen_constructor_destructor:false ~gen_events:true ~in_db:true
      ~lifecycle:[(Published, rel_boston, "")]
      ~messages:[create; destroy; atomic_set_resident_on]
      ~messages_default_allowed_roles:_R_POOL_OP ~persist:PersistEverything
      ~in_oss_since:None
      ~contents:
        [
          uid _vgpu ~lifecycle:[(Published, rel_boston, "")]
        ; field ~qualifier:DynamicRO ~ty:(Ref _vm)
            ~lifecycle:[(Published, rel_boston, "")]
            "VM" "VM that owns the vGPU"
        ; field ~qualifier:DynamicRO ~ty:(Ref _gpu_group)
            ~lifecycle:[(Published, rel_boston, "")]
            "GPU_group" "GPU group used by the vGPU"
        ; field ~qualifier:DynamicRO ~ty:String
            ~lifecycle:[(Published, rel_boston, "")]
            ~default_value:(Some (VString "0")) "device"
            "Order in which the devices are plugged into the VM"
        ; field ~qualifier:DynamicRO ~ty:Bool
            ~lifecycle:[(Published, rel_boston, "")]
            ~default_value:(Some (VBool false)) "currently_attached"
            "Reflects whether the virtual device is currently connected to a \
             physical device"
        ; field ~qualifier:RW
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_boston, "")]
            "other_config" "Additional configuration"
            ~default_value:(Some (VMap []))
        ; field ~qualifier:DynamicRO ~ty:(Ref _vgpu_type)
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "type" "Preset type for this VGPU"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:DynamicRO ~ty:(Ref _pgpu)
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "resident_on" "The PGPU on which this VGPU is running"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:DynamicRO ~ty:(Ref _pgpu)
            ~lifecycle:[(Published, rel_dundee, "")]
            "scheduled_to_be_resident_on"
            "The PGPU on which this VGPU is scheduled to run"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:DynamicRO
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_inverness, "")]
            ~default_value:(Some (VMap [])) "compatibility_metadata"
            "VGPU metadata to determine whether a VGPU can migrate between two \
             PGPUs"
        ; field ~qualifier:RW ~ty:String
            ~lifecycle:[(Published, rel_quebec, "")]
            ~default_value:(Some (VString "")) "extra_args"
            "Extra arguments for vGPU and passed to demu"
        ; field ~qualifier:DynamicRO ~ty:(Ref _pci)
            ~lifecycle:[(Published, rel_quebec, "")]
            ~default_value:(Some (VRef null_ref)) "PCI"
            "Device passed trough to VM, either as full device or SR-IOV \
             virtual function"
        ]
      ()
end

(** Virtual GPU types (i.e. preset sizes) *)

module VGPU_type = struct
  let implementation =
    Enum
      ( "vgpu_type_implementation"
      , [
          ("passthrough", "Pass through an entire physical GPU to a guest")
        ; ("nvidia", "vGPU using NVIDIA hardware")
        ; ("nvidia_sriov", "vGPU using NVIDIA hardware with SR-IOV")
        ; ("gvt_g", "vGPU using Intel GVT-g")
        ; ("mxgpu", "vGPU using AMD MxGPU")
        ]
      )

  let t =
    create_obj ~name:_vgpu_type ~descr:"A type of virtual GPU" ~doccomments:[]
      ~gen_constructor_destructor:false ~gen_events:true ~in_db:true
      ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
      ~messages:[] ~messages_default_allowed_roles:_R_POOL_OP
      ~persist:PersistEverything ~in_oss_since:None
      ~contents:
        [
          uid _vgpu_type ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            ~default_value:(Some (VString "")) "vendor_name"
            "Name of VGPU vendor"
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            ~default_value:(Some (VString "")) "model_name"
            "Model name associated with the VGPU type"
        ; field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            ~default_value:(Some (VInt 0L)) "framebuffer_size"
            "Framebuffer size of the VGPU type, in bytes"
        ; field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            ~default_value:(Some (VInt 0L)) "max_heads"
            "Maximum number of displays supported by the VGPU type"
        ; field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:[(Published, rel_vgpu_productisation, "")]
            ~default_value:(Some (VInt 0L)) "max_resolution_x"
            "Maximum resolution (width) supported by the VGPU type"
        ; field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:[(Published, rel_vgpu_productisation, "")]
            ~default_value:(Some (VInt 0L)) "max_resolution_y"
            "Maximum resolution (height) supported by the VGPU type"
        ; field ~qualifier:StaticRO ~ty:Int
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            ~internal_only:true ~default_value:(Some (VInt 0L)) "size"
            "Abstract size for tracking PGPU utilisation"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pgpu))
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "supported_on_PGPUs" "List of PGPUs that support this VGPU type"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pgpu))
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "enabled_on_PGPUs" "List of PGPUs that have this VGPU type enabled"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu))
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            "VGPUs" "List of VGPUs of this type"
        ; field ~qualifier:StaticRO
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_vgpu_tech_preview, "")]
            ~default_value:(Some (VMap [])) ~internal_only:true
            "internal_config"
            "Extra configuration information for internal use."
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _gpu_group))
            ~lifecycle:[(Published, rel_vgpu_productisation, "")]
            "supported_on_GPU_groups"
            "List of GPU groups in which at least one PGPU supports this VGPU \
             type"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _gpu_group))
            ~lifecycle:[(Published, rel_vgpu_productisation, "")]
            "enabled_on_GPU_groups"
            "List of GPU groups in which at least one have this VGPU type \
             enabled"
        ; field ~qualifier:StaticRO ~ty:implementation
            ~lifecycle:[(Published, rel_dundee, "")]
            ~default_value:(Some (VEnum "passthrough")) "implementation"
            "The internal implementation of this VGPU type"
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_dundee, "")]
            ~default_value:(Some (VString "")) "identifier"
            "Key used to identify VGPU types and avoid creating duplicates - \
             this field is used internally and not intended for interpretation \
             by API clients"
        ; field ~qualifier:StaticRO ~ty:Bool
            ~lifecycle:[(Published, rel_dundee, "")]
            ~default_value:(Some (VBool false)) "experimental"
            "Indicates whether VGPUs of this type should be considered \
             experimental"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type))
            ~lifecycle:[(Published, rel_quebec, "")]
            ~ignore_foreign_key:true ~default_value:(Some (VSet []))
            "compatible_types_in_vm"
            "List of VGPU types which are compatible in one VM"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu_type))
            ~lifecycle:[(Published, rel_quebec, "")]
            ~ignore_foreign_key:true ~default_value:(Some (VSet []))
            ~internal_only:true "compatible_types_on_pgpu"
            "List of VGPU types which are compatible on one PGPU"
        ]
      ()
end

module PVS_site = struct
  let lifecycle = [(Published, rel_ely, "")]

  let introduce =
    call ~name:"introduce" ~doc:"Introduce new PVS site"
      ~result:(Ref _pvs_site, "the new PVS site")
      ~params:
        [
          (String, "name_label", "name of the PVS site")
        ; (String, "name_description", "description of the PVS site")
        ; (String, "PVS_uuid", "unique identifier of the PVS site")
        ]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let forget =
    call ~name:"forget" ~doc:"Remove a site's meta data"
      ~params:[(Ref _pvs_site, "self", "this PVS site")]
      ~errs:
        [
          Api_errors.pvs_site_contains_running_proxies
        ; Api_errors.pvs_site_contains_servers
        ]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let set_PVS_uuid =
    call ~name:"set_PVS_uuid" ~doc:"Update the PVS UUID of the PVS site"
      ~params:
        [
          (Ref _pvs_site, "self", "this PVS site")
        ; (String, "value", "PVS UUID to be used")
        ]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let t =
    let null_str = Some (VString "") in
    let null_set = Some (VSet []) in
    create_obj ~name:_pvs_site
      ~descr:"machines serving blocks of data for provisioning VMs"
      ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
      ~in_db:true ~lifecycle ~persist:PersistEverything ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [
          uid _pvs_site ~lifecycle
        ; namespace ~name:"name" ~contents:(names None RW ~lifecycle) ()
        ; field ~qualifier:StaticRO ~lifecycle ~ty:String "PVS_uuid"
            ~default_value:null_str
            "Unique identifier of the PVS site, as configured in PVS"
        ; field ~qualifier:DynamicRO ~lifecycle
            ~ty:(Set (Ref _pvs_cache_storage)) "cache_storage"
            ~default_value:null_set ~ignore_foreign_key:true
            "The SR used by PVS proxy for the cache"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:(Set (Ref _pvs_server))
            "servers" "The set of PVS servers in the site"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:(Set (Ref _pvs_proxy))
            "proxies" "The set of proxies associated with the site"
        ]
      ~messages:[introduce; forget; set_PVS_uuid]
      ()
end

module PVS_server = struct
  let lifecycle = [(Published, rel_ely, "")]

  let introduce =
    call ~name:"introduce" ~doc:"introduce new PVS server"
      ~result:(Ref _pvs_server, "the new PVS server")
      ~params:
        [
          (Set String, "addresses", "IPv4/IPv6 addresses of the server")
        ; (Int, "first_port", "first UDP port accepted by this server")
        ; (Int, "last_port", "last UDP port accepted by this server")
        ; (Ref _pvs_site, "site", "PVS site this server is a part of")
        ]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let forget =
    call ~name:"forget" ~doc:"forget a PVS server"
      ~params:[(Ref _pvs_server, "self", "this PVS server")]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let t =
    let null_ref = Some (VRef null_ref) in
    let null_int = Some (VInt 0L) in
    let null_set = Some (VSet []) in
    create_obj ~name:_pvs_server
      ~descr:"individual machine serving provisioning (block) data"
      ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
      ~in_db:true ~lifecycle ~persist:PersistEverything ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [
          uid _pvs_server ~lifecycle
        ; field ~qualifier:StaticRO ~lifecycle ~ty:(Set String) "addresses"
            ~default_value:null_set "IPv4 addresses of this server"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:Int "first_port"
            ~default_value:null_int "First UDP port accepted by this server"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:Int "last_port"
            ~default_value:null_int "Last UDP port accepted by this server"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _pvs_site) "site"
            ~default_value:null_ref "PVS site this server is part of"
        ]
      ~messages:[introduce; forget] ()
end

module PVS_proxy = struct
  let lifecycle = [(Published, rel_ely, "")]

  let status =
    Enum
      ( "pvs_proxy_status"
      , [
          ("stopped", "The proxy is not currently running")
        ; ("initialised", "The proxy is setup but has not yet cached anything")
        ; ("caching", "The proxy is currently caching data")
        ; ( "incompatible_write_cache_mode"
          , "The PVS device is configured to use an incompatible write-cache \
             mode"
          )
        ; ( "incompatible_protocol_version"
          , "The PVS protocol in use is not compatible with the PVS proxy"
          )
        ]
      )

  let create =
    call ~name:"create" ~doc:"Configure a VM/VIF to use a PVS proxy"
      ~result:(Ref _pvs_proxy, "The reference of the created PVS proxy")
      ~params:
        [
          (Ref _pvs_site, "site", "PVS site that we proxy for")
        ; (Ref _vif, "VIF", "VIF for the VM that needs to be proxied")
        ]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let destroy =
    call ~name:"destroy" ~doc:"remove (or switch off) a PVS proxy for this VM"
      ~params:[(Ref _pvs_proxy, "self", "this PVS proxy")]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let t =
    let null_ref = Some (VRef null_ref) in
    let null_bool = Some (VBool false) in
    create_obj ~name:_pvs_proxy
      ~descr:"a proxy connects a VM/VIF with a PVS site" ~doccomments:[]
      ~gen_constructor_destructor:false ~gen_events:true ~in_db:true ~lifecycle
      ~persist:PersistEverything ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [
          uid _pvs_proxy ~lifecycle
        ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _pvs_site) "site"
            ~default_value:null_ref "PVS site this proxy is part of"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _vif) "VIF"
            ~default_value:null_ref "VIF of the VM using the proxy"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:Bool "currently_attached"
            ~default_value:null_bool "true = VM is currently proxied"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:status "status"
            ~default_value:(Some (VEnum "stopped"))
            "The run-time status of the proxy"
        ]
      ~messages:[create; destroy] ()
end

module PVS_cache_storage = struct
  let lifecycle = [(Published, rel_ely, "")]

  let t =
    let null_ref = Some (VRef null_ref) in
    create_obj ~name:_pvs_cache_storage
      ~descr:
        "Describes the storage that is available to a PVS site for caching \
         purposes"
      ~doccomments:[] ~gen_constructor_destructor:true ~gen_events:true
      ~in_db:true ~lifecycle ~persist:PersistEverything ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [
          uid _pvs_cache_storage ~lifecycle
        ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _host) "host"
            ~default_value:null_ref
            "The host on which this object defines PVS cache storage"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _sr) "SR"
            ~default_value:null_ref "SR providing storage for the PVS cache"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:(Ref _pvs_site) "site"
            ~default_value:null_ref
            "The PVS_site for which this object defines the storage"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:Int "size"
            ~default_value:(Some (VInt (Int64.of_int (20 * 1024 * 1024 * 1024))))
            "The size of the cache VDI (in bytes)"
        ; field ~qualifier:DynamicRO ~lifecycle ~ty:(Ref _vdi) "VDI"
            ~default_value:null_ref "The VDI used for caching"
        ]
      ~messages:[] ()
end

(* ---------------
   Features
   ----------------*)

(** A new piece of functionality *)
module Feature = struct
  let t =
    create_obj ~name:_feature ~descr:"A new piece of functionality"
      ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
      ~in_db:true
      ~lifecycle:[(Published, rel_falcon, "")]
      ~messages:[] ~messages_default_allowed_roles:_R_LOCAL_ROOT_ONLY
      ~persist:PersistEverything ~in_oss_since:None
      ~contents:
        [
          uid _feature ~lifecycle:[(Published, rel_falcon, "")]
        ; namespace ~name:"name"
            ~contents:(names None StaticRO ~lifecycle:[(Published, rel_rio, "")])
            ()
        ; field ~qualifier:DynamicRO ~ty:Bool
            ~lifecycle:[(Published, rel_falcon, "")]
            ~default_value:(Some (VBool false)) "enabled"
            "Indicates whether the feature is enabled"
        ; field ~qualifier:StaticRO ~ty:Bool
            ~lifecycle:[(Published, rel_falcon, "")]
            ~default_value:(Some (VBool false)) "experimental"
            "Indicates whether the feature is experimental (as opposed to \
             stable and fully supported)"
        ; field ~qualifier:StaticRO ~ty:String
            ~lifecycle:[(Published, rel_falcon, "")]
            ~default_value:(Some (VString "1.0")) "version"
            "The version of this feature"
        ; field ~qualifier:DynamicRO ~ty:(Ref _host)
            ~lifecycle:[(Published, rel_falcon, "")]
            "host" "The host where this feature is available"
        ]
      ()
end

module SDN_controller = struct
  let lifecycle = [(Published, rel_falcon, "")]

  let sdn_controller_protocol =
    Enum
      ( "sdn_controller_protocol"
      , [("ssl", "Active ssl connection"); ("pssl", "Passive ssl connection")]
      )

  let introduce =
    call ~name:"introduce" ~doc:"Introduce an SDN controller to the pool."
      ~result:(Ref _sdn_controller, "the introduced SDN controller")
      ~versioned_params:
        [
          {
            param_type= sdn_controller_protocol
          ; param_name= "protocol"
          ; param_doc= "Protocol to connect with the controller."
          ; param_release= falcon_release
          ; param_default= Some (VEnum "ssl")
          }
        ; {
            param_type= String
          ; param_name= "address"
          ; param_doc= "IP address of the controller."
          ; param_release= falcon_release
          ; param_default= Some (VString "")
          }
        ; {
            param_type= Int
          ; param_name= "port"
          ; param_doc= "TCP port of the controller."
          ; param_release= falcon_release
          ; param_default= Some (VInt 0L)
          }
        ]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let forget =
    call ~name:"forget"
      ~doc:"Remove the OVS manager of the pool and destroy the db record."
      ~params:[(Ref _sdn_controller, "self", "this SDN controller")]
      ~lifecycle ~allowed_roles:_R_POOL_OP ()

  let t =
    create_obj ~name:_sdn_controller
      ~descr:"Describes the SDN controller that is to connect with the pool"
      ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
      ~in_db:true ~lifecycle ~persist:PersistEverything ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_OP
      ~contents:
        [
          uid _sdn_controller ~lifecycle
        ; field ~qualifier:StaticRO ~lifecycle ~ty:sdn_controller_protocol
            "protocol" ~default_value:(Some (VEnum "ssl"))
            "Protocol to connect with SDN controller"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:String "address"
            ~default_value:(Some (VString "")) "IP address of the controller"
        ; field ~qualifier:StaticRO ~lifecycle ~ty:Int "port"
            ~default_value:(Some (VInt 0L)) "TCP port of the controller"
        ]
      ~messages:[introduce; forget] ()
end

module PUSB = struct
  let lifecycle = [(Published, rel_inverness, "")]

  let scan =
    call ~name:"scan" ~lifecycle
      ~params:[(Ref _host, "host", "The host")]
      ~allowed_roles:_R_POOL_ADMIN ()

  let set_passthrough_enabled =
    call ~name:"set_passthrough_enabled" ~lifecycle
      ~params:
        [
          (Ref _pusb, "self", "this PUSB")
        ; ( Bool
          , "value"
          , "passthrough is enabled when true and disabled with false"
          )
        ]
      ~allowed_roles:_R_POOL_ADMIN ()

  let t =
    create_obj ~name:_pusb ~descr:"A physical USB device" ~doccomments:[]
      ~gen_constructor_destructor:false ~gen_events:true ~in_db:true ~lifecycle
      ~persist:PersistEverything ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~contents:
        [
          uid _pusb ~lifecycle
        ; field ~qualifier:StaticRO ~ty:(Ref _usb_group) ~lifecycle "USB_group"
            "USB group the PUSB is contained in"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:StaticRO ~ty:(Ref _host) ~lifecycle "host"
            "Physical machine that owns the USB device"
            ~default_value:(Some (VRef null_ref))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "path"
            "port path of USB device" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "vendor_id"
            "vendor id of the USB device" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "vendor_desc"
            "vendor description of the USB device"
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "product_id"
            "product id of the USB device" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "product_desc"
            "product description of the USB device"
            ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "serial"
            "serial of the USB device" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "version"
            "USB device version" ~default_value:(Some (VString ""))
        ; field ~qualifier:StaticRO ~ty:String ~lifecycle "description"
            "USB device description" ~default_value:(Some (VString ""))
        ; field ~qualifier:DynamicRO ~ty:Bool ~lifecycle "passthrough_enabled"
            "enabled for passthrough" ~default_value:(Some (VBool false))
        ; field ~qualifier:RW
            ~ty:(Map (String, String))
            ~lifecycle:[(Published, rel_inverness, "")]
            "other_config" "additional configuration"
            ~default_value:(Some (VMap []))
        ; (* ideally we would use Float.nan here, but that makes the api generator hang *)
          field ~qualifier:StaticRO ~ty:Float
            ~lifecycle:[(Published, rel_stockholm, "")]
            "speed" "USB device speed"
            ~default_value:(Some (VFloat Constants.default_usb_speed))
        ]
      ~messages:[scan; set_passthrough_enabled]
      ()
end

(** Groups of USBs *)

module USB_group = struct
  let lifecycle = [(Published, rel_inverness, "")]

  let create =
    call ~name:"create" ~lifecycle
      ~versioned_params:
        [
          {
            param_type= String
          ; param_name= "name_label"
          ; param_doc= ""
          ; param_release= inverness_release
          ; param_default= Some (VString "")
          }
        ; {
            param_type= String
          ; param_name= "name_description"
          ; param_doc= ""
          ; param_release= inverness_release
          ; param_default= Some (VString "")
          }
        ; {
            param_type= Map (String, String)
          ; param_name= "other_config"
          ; param_doc= ""
          ; param_release= inverness_release
          ; param_default= Some (VMap [])
          }
        ]
      ~result:(Ref _usb_group, "The reference of the created USB_group")
      ~allowed_roles:_R_POOL_ADMIN ()

  let destroy =
    call ~name:"destroy" ~lifecycle
      ~params:[(Ref _usb_group, "self", "The USB group to destroy")]
      ~allowed_roles:_R_POOL_ADMIN ()

  let t =
    create_obj ~name:_usb_group
      ~descr:"A group of compatible USBs across the resource pool"
      ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
      ~in_db:true ~lifecycle ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~persist:PersistEverything ~in_oss_since:None
      ~contents:
        [
          uid _usb_group ~lifecycle
        ; namespace ~name:"name" ~contents:(names None RW ~lifecycle) ()
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _pusb)) ~lifecycle "PUSBs"
            "List of PUSBs in the group"
        ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vusb)) ~lifecycle "VUSBs"
            "List of VUSBs using the group"
        ; field ~qualifier:RW
            ~ty:(Map (String, String))
            ~lifecycle "other_config" "Additional configuration"
            ~default_value:(Some (VMap []))
        ]
      ~messages:[create; destroy] ()
end

module VUSB = struct
  let lifecycle = [(Published, rel_inverness, "")]

  let vusb_operations =
    Enum
      ( "vusb_operations"
      , [
          ("attach", "Attempting to attach this VUSB to a VM")
        ; ("plug", "Attempting to plug this VUSB into a VM")
        ; ("unplug", "Attempting to hot unplug this VUSB")
        ]
      )

  let create =
    call ~name:"create" ~in_oss_since:None
      ~versioned_params:
        [
          {
            param_type= Ref _vm
          ; param_name= "VM"
          ; param_doc= "The VM"
          ; param_release= inverness_release
          ; param_default= None
          }
        ; {
            param_type= Ref _usb_group
          ; param_name= "USB_group"
          ; param_doc= ""
          ; param_release= inverness_release
          ; param_default= None
          }
        ; {
            param_type= Map (String, String)
          ; param_name= "other_config"
          ; param_doc= ""
          ; param_release= inverness_release
          ; param_default= Some (VMap [])
          }
        ]
      ~lifecycle ~doc:"Create a new VUSB record in the database only"
      ~result:(Ref _vusb, "The ref of the newly created VUSB record.")
      ~allowed_roles:_R_POOL_ADMIN ()

  let unplug =
    call ~name:"unplug" ~doc:"Unplug the vusb device from the vm."
      ~params:[(Ref _vusb, "self", "vusb deivce")]
      ~lifecycle ~allowed_roles:_R_POOL_ADMIN ()

  let destroy =
    call ~name:"destroy" ~in_oss_since:None
      ~params:[(Ref _vusb, "self", "The VUSB to destroy about")]
      ~doc:"Removes a VUSB record from the database" ~lifecycle
      ~allowed_roles:_R_POOL_ADMIN ()

  let t =
    create_obj ~name:_vusb ~descr:"Describes the vusb device" ~doccomments:[]
      ~gen_constructor_destructor:false ~gen_events:true ~in_db:true ~lifecycle
      ~persist:PersistEverything ~in_oss_since:None
      ~messages_default_allowed_roles:_R_POOL_ADMIN
      ~contents:
        ([uid _vusb ~lifecycle]
        @ allowed_and_current_operations vusb_operations
        @ [
            field ~qualifier:DynamicRO ~ty:(Ref _vm) ~lifecycle "VM"
              "VM that owns the VUSB"
          ; field ~qualifier:DynamicRO ~ty:(Ref _usb_group) ~lifecycle
              "USB_group" "USB group used by the VUSB"
          ; field ~qualifier:RW
              ~ty:(Map (String, String))
              ~lifecycle "other_config" "Additional configuration"
              ~default_value:(Some (VMap []))
          ; field ~qualifier:DynamicRO ~ty:Bool
              ~lifecycle:
                [(Published, rel_rio, "is the device currently attached")]
              "currently_attached" "is the device currently attached"
              ~default_value:(Some (VBool false))
          ]
        )
      ~messages:[create; unplug; destroy] ()
end

(******************************************************************************************)

(** All the objects in the system in order they will appear in documentation: *)
let all_system =
  [
    Session.t
  ; Auth.t
  ; Subject.t
  ; Role.t
  ; Task.t
  ; Event.t
  ; (* alert; *)
    Datamodel_pool.t
  ; Pool_patch.t
  ; Pool_update.t
  ; Datamodel_vm.t
  ; VM_metrics.t
  ; VM_guest_metrics.t
  ; VMPP.t
  ; VMSS.t
  ; VM_appliance.t
  ; DR_task.t
  ; Datamodel_host.t
  ; Host_crashdump.t
  ; Host_patch.t
  ; Host_metrics.t
  ; Host_cpu.t
  ; (* network_manager; *)
    Network.t
  ; VIF.t
  ; VIF_metrics.t
  ; PIF.t
  ; PIF_metrics.t
  ; Bond.t
  ; VLAN.t
  ; SM.t
  ; SR.t
  ; Sr_stat.t
  ; Probe_result.t
  ; LVHD.t
  ; VDI.t
  ; VBD.t
  ; VBD_metrics.t
  ; PBD.t
  ; Crashdump.t
  ; (* misc *)
    Datamodel_vtpm.t
  ; Console.t
  ; (* filesystem; *)
    User.t
  ; Data_source.t
  ; Blob.t
  ; Message.t
  ; Secret.t
  ; Tunnel.t
  ; Network_sriov.t
  ; PCI.t
  ; PGPU.t
  ; GPU_group.t
  ; VGPU.t
  ; VGPU_type.t
  ; PVS_site.t
  ; PVS_server.t
  ; PVS_proxy.t
  ; PVS_cache_storage.t
  ; Feature.t
  ; SDN_controller.t
  ; Vdi_nbd_server_info.t
  ; PUSB.t
  ; USB_group.t
  ; VUSB.t
  ; Datamodel_cluster.t
  ; Datamodel_cluster_host.t
  ; Datamodel_certificate.t
  ; Datamodel_diagnostics.t
  ; Datamodel_repository.t
  ; Datamodel_observer.t
  ; Datamodel_vm_group.t
  ; Datamodel_host_driver.t
  ; Datamodel_driver_variant.t
  ]

(* If the relation is one-to-many, the "many" nodes (one edge each) must come before the "one" node (many edges) *)
(* If the relation is many-to-many, then the field which will be manually
 * updated must come first. The second field will be automatically * kept
 * up-to-date. *)

(**
   These are the pairs of (object, field) which are bound together in
   the database schema.

   It is assumed that, for any entry (p, p'), neither p nor p'
   appears in any other entry. It may be the case that p = p', which
   is the only instance where some object-field pair may appear more
   than once.

   This is implicitly assumed by other code which treats this list -
   and its symmetric closure - as an association list
   without duplicate keys. *)
let all_relations =
  [
    (* snapshots *)
    ((_vm, "snapshot_of"), (_vm, "snapshots"))
  ; ((_vdi, "snapshot_of"), (_vdi, "snapshots"))
  ; ((_vm, "parent"), (_vm, "children"))
  ; (* subtasks hierarchy *)
    ((_task, "subtask_of"), (_task, "subtasks"))
  ; ((_task, "session"), (_session, "tasks"))
  ; ((_pif, "bond_slave_of"), (_bond, "slaves"))
  ; ((_bond, "master"), (_pif, "bond_master_of"))
  ; ((_vlan, "tagged_PIF"), (_pif, "VLAN_slave_of"))
  ; ((_tunnel, "access_PIF"), (_pif, "tunnel_access_PIF_of"))
  ; ((_tunnel, "transport_PIF"), (_pif, "tunnel_transport_PIF_of"))
  ; ((_pbd, "host"), (_host, "PBDs"))
  ; ((_pbd, "SR"), (_sr, "PBDs"))
  ; ((_vbd, "VDI"), (_vdi, "VBDs"))
  ; ((_crashdump, "VDI"), (_vdi, "crash_dumps"))
  ; (*  (_vdi, "parent"), (_vdi, "children"); *)
    ((_vbd, "VM"), (_vm, "VBDs"))
  ; ((_crashdump, "VM"), (_vm, "crash_dumps"))
  ; (* VM <-> VIF <-> network *)
    ((_vif, "VM"), (_vm, "VIFs"))
  ; ((_vif, "network"), (_network, "VIFs"))
  ; ((_cluster_host, "cluster"), (_cluster, "cluster_hosts"))
  ; (* host <-> PIF <-> network *)
    ((_pif, "host"), (_host, "PIFs"))
  ; ((_pif, "network"), (_network, "PIFs"))
  ; ((_vdi, "SR"), (_sr, "VDIs"))
  ; (*  (_alert, "task"), (_task, "alerts"); *)
    ((_vtpm, "VM"), (_vm, "VTPMs"))
  ; ((_console, "VM"), (_vm, "consoles"))
  ; ((_vm, "resident_on"), (_host, "resident_VMs"))
  ; ((_hostcpu, "host"), (_host, "host_CPUs"))
  ; ((_host_crashdump, "host"), (_host, "crashdumps"))
  ; ((_host_patch, "host"), (_host, "patches"))
  ; ((_host_patch, "pool_patch"), (_pool_patch, "host_patches"))
  ; ((_host, "updates"), (_pool_update, "hosts"))
  ; ((_subject, "roles"), (_subject, "roles"))
  ; (*(_subject, "roles"), (_role, "subjects");*)
    ((_role, "subroles"), (_role, "subroles"))
  ; ((_vm, "protection_policy"), (_vmpp, "VMs"))
  ; ((_vm, "snapshot_schedule"), (_vmss, "VMs"))
  ; ((_vm, "appliance"), (_vm_appliance, "VMs"))
  ; ((_pgpu, "GPU_group"), (_gpu_group, "PGPUs"))
  ; ((_vgpu, "GPU_group"), (_gpu_group, "VGPUs"))
  ; ((_vgpu, "type"), (_vgpu_type, "VGPUs"))
  ; ((_vgpu, "VM"), (_vm, "VGPUs"))
  ; ((_vgpu, "resident_on"), (_pgpu, "resident_VGPUs"))
  ; ((_pgpu, "supported_VGPU_types"), (_vgpu_type, "supported_on_PGPUs"))
  ; ((_pgpu, "enabled_VGPU_types"), (_vgpu_type, "enabled_on_PGPUs"))
  ; ( (_gpu_group, "supported_VGPU_types")
    , (_vgpu_type, "supported_on_GPU_groups")
    )
  ; ((_gpu_group, "enabled_VGPU_types"), (_vgpu_type, "enabled_on_GPU_groups"))
  ; ((_pci, "host"), (_host, "PCIs"))
  ; ((_pgpu, "host"), (_host, "PGPUs"))
  ; ((_pci, "attached_VMs"), (_vm, "attached_PCIs"))
  ; ((_pci, "physical_function"), (_pci, "virtual_functions"))
  ; ((_vdi, "metadata_of_pool"), (_pool, "metadata_VDIs"))
  ; ((_sr, "introduced_by"), (_dr_task, "introduced_SRs"))
  ; ((_pvs_server, "site"), (_pvs_site, "servers"))
  ; ((_pvs_proxy, "site"), (_pvs_site, "proxies"))
  ; ((_pvs_cache_storage, "site"), (_pvs_site, "cache_storage"))
  ; ((_pusb, "host"), (_host, "PUSBs"))
  ; ((_pusb, "USB_group"), (_usb_group, "PUSBs"))
  ; ((_vusb, "USB_group"), (_usb_group, "VUSBs"))
  ; ((_vusb, "VM"), (_vm, "VUSBs"))
  ; ((_feature, "host"), (_host, "features"))
  ; ((_network_sriov, "physical_PIF"), (_pif, "sriov_physical_PIF_of"))
  ; ((_network_sriov, "logical_PIF"), (_pif, "sriov_logical_PIF_of"))
  ; ((_certificate, "host"), (_host, "certificates"))
  ; ((_vm, "groups"), (_vm_group, "VMs"))
  ; ((_driver_variant, "driver"), (_host_driver, "variants"))
  ]

let update_lifecycles =
  let replace_prototyped p ls =
    Lifecycle.from
      ((Prototyped, p, "")
      :: List.filter (function Prototyped, _, _ -> false | _ -> true) ls
      )
  in
  let replace_obj_lifecycle obj =
    let obj_lifecycle =
      match Datamodel_lifecycle.prototyped_of_class obj.name with
      | Some p ->
          replace_prototyped p obj.obj_lifecycle.transitions
      | None ->
          obj.obj_lifecycle
    in
    {obj with obj_lifecycle}
  in
  let replace_field_lifecycle obj_name fld =
    let lifecycle =
      match
        Datamodel_lifecycle.prototyped_of_field
          (obj_name, Escaping.escape_id fld.full_name)
      with
      | Some p ->
          replace_prototyped p fld.lifecycle.transitions
      | None ->
          fld.lifecycle
    in
    {fld with lifecycle}
  in
  let replace_message_lifecycle msg =
    let msg_lifecycle =
      match
        Datamodel_lifecycle.prototyped_of_message
          (msg.msg_obj_name, msg.msg_name)
      with
      | Some p ->
          replace_prototyped p msg.msg_lifecycle.transitions
      | None ->
          msg.msg_lifecycle
    in
    {msg with msg_lifecycle}
  in
  Dm_api.map replace_obj_lifecycle replace_field_lifecycle
    replace_message_lifecycle

(** the full api specified here *)
let all_api = Dm_api.make (all_system, all_relations) |> update_lifecycles

(** These are the "emergency" calls that can be performed when a host is in "emergency mode" *)
let emergency_calls =
  [
    (Datamodel_pool.t, Datamodel_pool.slave_reset_master)
  ; (Datamodel_pool.t, Datamodel_pool.transition_to_master)
  ; (Datamodel_pool.t, Datamodel_pool.ping_slave)
  ; (Session.t, Session.slave_local_login)
  ; (Session.t, Session.slave_local_login_with_password)
  ; (Session.t, Session.local_logout)
  ; (Datamodel_host.t, Datamodel_host.propose_new_master)
  ; (Datamodel_host.t, Datamodel_host.commit_new_master)
  ; (Datamodel_host.t, Datamodel_host.abort_new_master)
  ; (Datamodel_host.t, Datamodel_host.local_assert_healthy)
  ; (Datamodel_host.t, Datamodel_host.signal_networking_change)
  ; (Datamodel_host.t, Datamodel_host.local_management_reconfigure)
  ; (Datamodel_host.t, Datamodel_host.ha_xapi_healthcheck)
  ; (Datamodel_host.t, Datamodel_host.emergency_ha_disable)
  ; (Datamodel_host.t, Datamodel_host.management_disable)
  ; (Datamodel_host.t, Datamodel_host.get_system_status_capabilities)
  ; (Datamodel_host.t, Datamodel_host.is_in_emergency_mode)
  ; (Datamodel_host.t, Datamodel_host.shutdown_agent)
  ; (Datamodel_host.t, Datamodel_host.emergency_reset_server_certificate)
  ; (Datamodel_host.t, Datamodel_host.emergency_disable_tls_verification)
  ; (Datamodel_host.t, Datamodel_host.emergency_reenable_tls_verification)
  ; (Datamodel_host.t, Datamodel_host.emergency_clear_mandatory_guidance)
  ; (Datamodel_host.t, Datamodel_host.update_firewalld_service_status)
  ]

(** Whitelist of calls that will not get forwarded from the slave to master via the unix domain socket *)
let whitelist =
  [(Session.t, Session.login); (Session.t, Session.slave_login)]
  @ emergency_calls

(* perform consistency checks on api at initialisation time *)
let _ =
  Dm_api.check all_api
    (List.map (fun (obj, msg) -> (obj.name, msg.msg_name)) emergency_calls)

(** List of classes to skip generating async handlers for *)
let no_async_messages_for =
  [_session; _event; (* _alert; *) _task; _data_source; _blob]

(** List of classes to generate 'get_all' messages for. *)
let expose_get_all_messages_for =
  [
    _task
  ; (* _alert; *)
    _host
  ; _host_metrics
  ; _hostcpu
  ; _sr
  ; _vm
  ; _vm_metrics
  ; _vm_guest_metrics
  ; _network
  ; _vif
  ; _vif_metrics
  ; _pif
  ; _pif_metrics
  ; _pbd
  ; _vdi
  ; _vbd
  ; _vbd_metrics
  ; _console
  ; _crashdump
  ; _host_crashdump
  ; _host_patch
  ; _pool
  ; _sm
  ; _pool_patch
  ; _pool_update
  ; _bond
  ; _vlan
  ; _blob
  ; _subject
  ; _role
  ; _secret
  ; _tunnel
  ; _vmpp
  ; _vmss
  ; _vm_appliance
  ; _vm_group
  ; _pci
  ; _pgpu
  ; _gpu_group
  ; _vgpu
  ; _vgpu_type
  ; _dr_task
  ; _pvs_site
  ; _pvs_server
  ; _pvs_proxy
  ; _pvs_cache_storage
  ; _feature
  ; _sdn_controller
  ; _network_sriov
  ; (* _vdi_nbd_server_info must NOT be included here *)
    _pusb
  ; _usb_group
  ; _vusb
  ; _cluster
  ; _cluster_host
  ; _certificate
  ; _repository
  ; _vtpm
  ; _observer
  ; _host_driver
  ; _driver_variant
  ]

let no_task_id_for = [_task; (* _alert; *) _event]

let current_operations_for = [_vm (* _vdi; _host; _sr *)]

(*** HTTP actions ***)

type action_arg =
  (* I'm not using Datamodel_types here because we need varargs *)
  | String_query_arg of string
  | Int64_query_arg of string
  | Bool_query_arg of string
  | Varargs_query_arg

type http_meth = Get | Put | Post | Connect | Options

let rbac_http_permission_prefix = "http/"

(* Each action has:
   (unique public name, (HTTP method, URI, whether to expose in SDK, [args to expose in SDK], [allowed_roles], [(sub-action,allowed_roles)]))
*)
let http_actions =
  [
    ("get_services", (Get, Constants.services_uri, true, [], _R_READ_ONLY, []))
  ; ( "post_services"
    , (Post, Constants.services_uri, false, [], _R_POOL_ADMIN, [])
    )
  ; ("put_services", (Put, Constants.services_uri, false, [], _R_POOL_ADMIN, []))
  ; ( "post_remote_db_access"
    , (Post, Constants.remote_db_access_uri, false, [], _R_POOL_ADMIN, [])
    )
  ; ( "post_remote_db_access_v2"
    , (Post, Constants.remote_db_access_uri_v2, false, [], _R_POOL_ADMIN, [])
    )
  ; ( "get_services_xenops"
    , (Get, Constants.xenops_uri, false, [], _R_VM_POWER_ADMIN, [])
    )
  ; ( "post_services_xenops"
    , ( Post
      , Constants.xenops_uri
      , false
      , []
      , _R_VM_POWER_ADMIN ++ _R_CLIENT_CERT
      , []
      )
    )
  ; ( "put_services_xenops"
    , ( Put
      , Constants.xenops_uri
      , false
      , []
      , _R_VM_POWER_ADMIN ++ _R_CLIENT_CERT
      , []
      )
    )
  ; ( "get_services_sm"
    , (Get, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, [])
    )
  ; ( "post_services_sm"
    , (Post, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, [])
    )
  ; ( "put_services_sm"
    , (Put, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, [])
    )
  ; ( "put_import"
    , ( Put
      , Constants.import_uri
      , true
      , [
          Bool_query_arg "restore"
        ; Bool_query_arg "force"
        ; String_query_arg "sr_id"
        ]
      , _R_VM_ADMIN
      , []
      )
    )
  ; ( "put_import_metadata"
    , ( Put
      , Constants.import_metadata_uri
      , true
      , [
          Bool_query_arg "restore"
        ; Bool_query_arg "force"
        ; Bool_query_arg "dry_run"
        ]
      , _R_VM_ADMIN
      , []
      )
    )
  ; ( "put_import_raw_vdi"
    , ( Put
      , Constants.import_raw_vdi_uri
      , true
      , [
          String_query_arg "vdi"
        ; String_query_arg "format"
        ; Bool_query_arg "chunked"
        ]
      , _R_VM_ADMIN
      , []
      )
    )
  ; ( "get_export"
    , ( Get
      , Constants.export_uri
      , true
      , [
          String_query_arg "uuid"
        ; String_query_arg "use_compression"
        ; Bool_query_arg "preserve_power_state"
        ]
      , _R_VM_ADMIN
      , []
      )
    )
  ; ( "get_export_metadata"
    , ( Get
      , Constants.export_metadata_uri
      , true
      , [
          String_query_arg "uuid"
        ; Bool_query_arg "all"
        ; Bool_query_arg "include_dom0"
        ; Bool_query_arg "include_vhd_parents"
        ; Bool_query_arg "export_snapshots"
        ; String_query_arg "excluded_device_types"
        ]
      , _R_VM_ADMIN
      , []
      )
    )
  ; ( "get_export_raw_vdi"
    , ( Get
      , Constants.export_raw_vdi_uri
      , true
      , [String_query_arg "vdi"; String_query_arg "format"]
      , _R_VM_ADMIN
      , []
      )
    )
  ; ( "connect_console"
    , ( Connect
      , Constants.console_uri
      , false
      , []
      , _R_VM_OP
      , [("host_console", _R_POOL_ADMIN)]
      )
    )
  ; (* only _R_POOL_ADMIN can access the host/Dom0 console *)
    ( "connect_console_ws"
    , ( Get
      , Constants.console_uri
      , false
      , []
      , _R_VM_OP
      , [("host_console_ws", _R_POOL_ADMIN)]
      )
    )
  ; (* only _R_POOL_ADMIN can access the host/Dom0 console *)
    ("get_root", (Get, "/", false, [], _R_READ_ONLY, []))
  ; ("post_cli", (Post, Constants.cli_uri, false, [], _R_READ_ONLY, []))
  ; ( "get_host_backup"
    , (Get, Constants.host_backup_uri, true, [], _R_POOL_ADMIN, [])
    )
  ; ( "put_host_restore"
    , (Put, Constants.host_restore_uri, true, [], _R_POOL_ADMIN, [])
    )
  ; ( "get_host_logs_download"
    , (Get, Constants.host_logs_download_uri, true, [], _R_POOL_OP, [])
    )
  ; ( "put_pool_patch_upload"
    , ( Put
      , Constants.pool_patch_upload_uri
      , true
      , [String_query_arg "sr_id"]
      , _R_POOL_OP
      , []
      )
    )
  ; ( "get_vncsnapshot"
    , ( Get
      , Constants.vncsnapshot_uri
      , true
      , [String_query_arg "uuid"]
      , _R_VM_OP
      , [("host_console", _R_POOL_ADMIN)]
      )
    )
  ; (* only _R_POOL_ADMIN can snapshot host/Dom0 console *)
    ( "get_pool_xml_db_sync"
    , (Get, Constants.pool_xml_db_sync, true, [], _R_POOL_ADMIN, [])
    )
  ; ( "put_pool_xml_db_sync"
    , (Put, Constants.pool_xml_db_sync, false, [], _R_POOL_ADMIN, [])
    )
  ; ( "get_config_sync"
    , (Get, Constants.config_sync_uri, false, [], _R_POOL_ADMIN, [])
    )
  ; ( "get_system_status"
    , ( Get
      , Constants.system_status_uri
      , true
      , [String_query_arg "entries"; String_query_arg "output"]
      , _R_POOL_OP
      , []
      )
    )
  ; ( Constants.get_vm_rrd
    , ( Get
      , Constants.get_vm_rrd_uri
      , true
      , [String_query_arg "uuid"; Bool_query_arg "json"]
      , _R_READ_ONLY
      , []
      )
    )
  ; ( Constants.get_host_rrd
    , ( Get
      , Constants.get_host_rrd_uri
      , true
      , [Bool_query_arg "json"]
      , _R_READ_ONLY
      , []
      )
    )
  ; ( Constants.get_sr_rrd
    , ( Get
      , Constants.get_sr_rrd_uri
      , true
      , [String_query_arg "uuid"]
      , _R_READ_ONLY
      , []
      )
    )
  ; ( Constants.get_rrd_updates
    , ( Get
      , Constants.get_rrd_updates_uri
      , true
      , [
          Int64_query_arg "start"
        ; String_query_arg "cf"
        ; Int64_query_arg "interval"
        ; Bool_query_arg "host"
        ; String_query_arg "uuid"
        ; Bool_query_arg "json"
        ]
      , _R_READ_ONLY
      , []
      )
    )
  ; ( Constants.put_rrd
    , (Put, Constants.put_rrd_uri, false, [], _R_POOL_ADMIN, [])
    )
  ; ("get_blob", (Get, Constants.blob_uri, false, [], _R_READ_ONLY, []))
  ; ( "put_blob"
    , ( Put
      , Constants.blob_uri
      , true
      , [String_query_arg "ref"]
      , _R_VM_POWER_ADMIN
      , []
      )
    )
  ; ( "put_messages"
    , (Put, Constants.message_put_uri, false, [], _R_VM_POWER_ADMIN, [])
    )
  ; ( "connect_remotecmd"
    , (Connect, Constants.remotecmd_uri, false, [], _R_POOL_ADMIN, [])
    )
  ; ( "get_wlb_report"
    , ( Get
      , Constants.wlb_report_uri
      , true
      , [String_query_arg "report"; Varargs_query_arg]
      , _R_READ_ONLY
      , []
      )
    )
  ; ( "get_wlb_diagnostics"
    , (Get, Constants.wlb_diagnostics_uri, true, [], _R_READ_ONLY, [])
    )
  ; ( "get_audit_log"
    , ( Get
      , Constants.audit_log_uri
      , true
      , [String_query_arg "since"]
      , _R_READ_ONLY
      , []
      )
    )
  ; (* XMLRPC callback *)
    ("post_root", (Post, "/", false, [], _R_READ_ONLY, []))
  ; ("post_RPC2", (Post, "/RPC2", false, [], _R_READ_ONLY, []))
  ; (* JSON callback *)
    ("post_json", (Post, Constants.json_uri, false, [], _R_READ_ONLY, []))
  ; ("post_root_options", (Options, "/", false, [], _R_READ_ONLY, []))
  ; ( "post_json_options"
    , (Options, Constants.json_uri, false, [], _R_READ_ONLY, [])
    )
  ; ("post_jsonrpc", (Post, Constants.jsonrpc_uri, false, [], _R_READ_ONLY, []))
  ; ( "post_jsonrpc_options"
    , (Options, Constants.jsonrpc_uri, false, [], _R_READ_ONLY, [])
    )
  ; ( "get_pool_update_download"
    , (Get, Constants.get_pool_update_download_uri, false, [], _R_READ_ONLY, [])
    )
  ; ( "get_repository"
    , (Get, Constants.get_repository_uri, false, [], _R_LOCAL_ROOT_ONLY, [])
    )
  ; ( "get_enabled_repository"
    , ( Get
      , Constants.get_enabled_repository_uri
      , false
      , []
      , _R_POOL_OP ++ _R_CLIENT_CERT
      , []
      )
    )
  ; ( "get_host_updates"
    , ( Get
      , Constants.get_host_updates_uri
      , false
      , []
      , _R_POOL_OP ++ _R_CLIENT_CERT
      , []
      )
    )
  ; ( "get_updates"
    , ( Get
      , Constants.get_updates_uri
      , true
      , []
      , _R_POOL_OP ++ _R_CLIENT_CERT
      , []
      )
    )
  ; ("put_bundle", (Put, Constants.put_bundle_uri, true, [], _R_POOL_OP, []))
  ]

(* these public http actions will NOT be checked by RBAC *)
(* they are meant to be used in exceptional cases where RBAC is already *)
(* checked inside them, such as in the XMLRPC (API) calls *)
let public_http_actions_with_no_rbac_check =
  [
    "post_root"
  ; "post_RPC2"
  ; (* XMLRPC (API) calls -> checks RBAC internally *)
    "post_cli"
  ; (* CLI commands -> calls XMLRPC *)
    "post_json"
  ; (* JSON -> calls XMLRPC *)
    "get_root"
  ; (* Make sure that downloads, personal web pages etc do not go through RBAC asking for a password or session_id *)
    (* also, without this line, quicktest_http.ml fails on non_resource_cmd and bad_resource_cmd with a 401 instead of 404 *)
    "get_blob"
  ; (* Public blobs don't need authentication *)
    "post_root_options"
  ; (* Preflight-requests are not RBAC checked *)
    "post_json_options"
  ; "post_jsonrpc"
  ; "post_jsonrpc_options"
  ; "get_pool_update_download"
  ]

(* permissions not associated with any object message or field *)
let extra_permissions =
  [
    (Task.extra_permission_task_destroy_any, _R_POOL_OP)
    (* only POOL_OP can destroy any tasks *)
  ]

module StringMap = Map.Make (String)

type obj_states = {
    obj_state: Lifecycle.state
  ; msg_states: Lifecycle.state StringMap.t
  ; fld_states: Lifecycle.state StringMap.t
}

let all_lifecycles =
  let get_msg_state {msg_name; msg_lifecycle= {state; _}; _} =
    Seq.return (msg_name, state)
  in
  let rec get_content_states obj_name = function
    | Field {full_name; lifecycle= {state; _}; _} ->
        let fld_name = String.concat "__" full_name in
        Seq.return (fld_name, state)
    | Namespace (_, contents) ->
        List.to_seq contents |> Seq.concat_map (get_content_states obj_name)
  in
  let map_with f ls = List.to_seq ls |> Seq.concat_map f |> StringMap.of_seq in
  Dm_api.objects_of_api all_api
  |> map_with (fun {name; obj_lifecycle= {state; _}; messages; contents; _} ->
         let msg_states = map_with get_msg_state messages in
         let fld_states = map_with (get_content_states name) contents in
         let fld_states = StringMap.add "_ref" state fld_states in
         Seq.return (name, {obj_state= state; msg_states; fld_states})
     )
