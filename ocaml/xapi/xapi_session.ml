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
(** Module that defines API functions for Session objects
 * @group XenAPI functions
*)

(* include Custom_actions.DebugVersion.Session *)

module D = Debug.Make (struct let name = "xapi_session" end)

open D

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let total_sessions = Atomic.make 0

let get_total_sessions () = Atomic.get total_sessions |> Int64.of_int

module Date = Clock.Date
module Listext = Xapi_stdext_std.Listext
open Client
open Auth_signature
open Extauth

module AuthFail : sig
  (* stats are reset each time you query, so if there hasn't
     been a failed login attempt since the last time the stats
     were queried, you won't get any stats *)
  val get_stats_string : unit -> string option

  val on_fail :
       __context:Context.t
    -> now:Date.t
    -> uname:string option
    -> originator:string option
    -> record:[< `log_only | `log_and_alert]
    -> unit
end = struct
  type client = {
      user_agent: string option
    ; uname: string option
    ; originator: string option
    ; ip: string option
  }

  let client_of_info ~__context ~originator ~uname =
    let user_agent = Context.get_user_agent __context in
    let ip = Context.get_client_ip __context in
    (* check to make sure we have at least _some_ information *)
    if
      [user_agent; originator; uname; ip]
      |> List.for_all (function None | Some "" -> true | _ -> false)
    then
      None
    else
      Some {originator; uname; user_agent; ip}

  let string_of_client x =
    [
      ("username", x.uname)
    ; ("originator", x.originator)
    ; ("useragent", x.user_agent)
    ; ("ip", x.ip)
    ]
    |> List.filter_map (fun (label, value) ->
           match value with
           | None | Some "" ->
               None
           | Some value ->
               Some (Printf.sprintf "<%s>%s</%s>" label value label)
       )
    |> String.concat "\n"

  type client_failed_attempts = {
      client: client
    ; num_failed_attempts: int
    ; last_failed_attempt: Date.t
  }

  let up_to_3 xs x =
    List.stable_sort
      (fun a b -> Int.compare b.num_failed_attempts a.num_failed_attempts)
      (x :: xs)
    |> Listext.List.take 3

  let string_of_client_failed_attempts x =
    Printf.sprintf {|
<known>
%s
<number>%i</number>
<date>%s</date>
</known>|}
      (string_of_client x.client)
      x.num_failed_attempts
      (Date.to_rfc3339 x.last_failed_attempt)

  type stats = {
      total_num_failed_attempts: int
    ; top_3_worst_clients: client_failed_attempts list
          (* not necessarily 3, but <=3 *)
    ; unknown_client_failed_attempts: int
  }

  let string_of_stats
      {
        total_num_failed_attempts
      ; top_3_worst_clients
      ; unknown_client_failed_attempts
      } =
    let unknown =
      if unknown_client_failed_attempts = 0 then
        ""
      else
        Printf.sprintf {|
<unknown>%i</unknown>|} unknown_client_failed_attempts
    in
    let known_with_total =
      if top_3_worst_clients = [] then
        ""
      else
        Printf.sprintf {|
<total>%i</total>%s|} total_num_failed_attempts
          (top_3_worst_clients
          |> List.map string_of_client_failed_attempts
          |> String.concat ""
          )
    in
    Printf.sprintf {|<body>%s%s
</body>|} known_with_total unknown

  module Stats : sig
    val get : unit -> stats option

    (* returns the number of failures from this client since last call to [ get ] *)
    val record_client : client -> now:Date.t -> int

    (* returns number of failures from unknown clients since last call to [ get ] *)
    val record_unknown : unit -> int
  end = struct
    let m = Mutex.create ()

    let unknown_ctr = ref 0

    let record_unknown () =
      with_lock m (fun () ->
          let ctr = !unknown_ctr + 1 in
          unknown_ctr := ctr ;
          ctr
      )

    type value = {num_failed_attempts: int; last_failed_attempt: Date.t}

    let table = Hashtbl.create 10

    let record_client k ~now =
      with_lock m (fun () ->
          match Hashtbl.find_opt table k with
          | None ->
              Hashtbl.add table k
                {num_failed_attempts= 1; last_failed_attempt= now} ;
              1
          | Some ({num_failed_attempts; _} : value) ->
              let num_failed_attempts = num_failed_attempts + 1 in
              Hashtbl.replace table k
                {num_failed_attempts; last_failed_attempt= now} ;
              num_failed_attempts
      )

    let get () =
      let reset () =
        Hashtbl.reset table ;
        unknown_ctr := 0
      in
      with_lock m (fun () ->
          let unknown_client_failed_attempts = !unknown_ctr in
          if Hashtbl.length table = 0 && unknown_client_failed_attempts = 0 then
            None
          else
            let num_known_client_failed_attempts, top_3_worst_clients =
              Hashtbl.fold
                (fun client {num_failed_attempts; last_failed_attempt}
                     (ctr, worst_so_far) ->
                  ( ctr + num_failed_attempts
                  , up_to_3 worst_so_far
                      {client; num_failed_attempts; last_failed_attempt}
                  )
                )
                table (0, [])
            in
            reset () ;
            Some
              {
                total_num_failed_attempts=
                  num_known_client_failed_attempts
                  + unknown_client_failed_attempts
              ; top_3_worst_clients
              ; unknown_client_failed_attempts
              }
      )
  end

  let get_stats_string () = Stats.get () |> Option.map string_of_stats

  let on_fail ~__context ~now ~uname ~originator ~record =
    try
      match (client_of_info ~__context ~uname ~originator, record) with
      | None, `log_only ->
          warn "login failure from unknown client"
      | None, `log_and_alert ->
          let total_unknown_login_failures = Stats.record_unknown () in
          warn "login failure from unknown client, total= %i"
            total_unknown_login_failures
      | Some client, `log_only ->
          info "failed login attempt by client: %s" (string_of_client client)
      | Some client, _ ->
          let num_failed_attempts = Stats.record_client client ~now in
          info "failed login attempt #%i by client: %s" num_failed_attempts
            (string_of_client client)
    with e ->
      (* we don't expect this function to fail, but if it does we don't want to block callers *)
      error "AuthFail.on_fail_with_uname: unexpected error: '%s'"
        (Printexc.to_string e)
end

let _record_login_failure ~__context ~now ~uname ~originator ~record f =
  let on_fail e =
    AuthFail.on_fail ~__context ~now ~uname ~originator ~record ;
    raise e
  in
  try f () with
  | Auth_signature.Auth_failure _ as e ->
      on_fail e
  | Api_errors.Server_error (code, _) as e
    when code = Api_errors.session_authentication_failed ->
      on_fail e

let record_login_failure ~__context ~uname ~originator ~record f =
  Context.with_tracing ?originator ~__context __FUNCTION__ @@ fun __context ->
  let now = Date.now () in
  _record_login_failure ~__context ~now ~uname ~originator ~record f

let get_failed_login_stats = AuthFail.get_stats_string

let local_superuser = "root"

let xapi_internal_originator = "xapi"

let throttle_auth_internal = Locking_helpers.Semaphore.create "Internal auth"

let throttle_auth_external = Locking_helpers.Semaphore.create "External auth"

let with_throttle = Locking_helpers.Semaphore.execute

let set_local_auth_max_threads n =
  Locking_helpers.Semaphore.set_max throttle_auth_internal @@ Int64.to_int n

let set_ext_auth_max_threads n =
  Locking_helpers.Semaphore.set_max throttle_auth_external @@ Int64.to_int n

let do_external_auth ~__context uname pwd =
  with_throttle throttle_auth_external (fun () ->
      (Ext_auth.d ()).authenticate_username_password ~__context uname pwd
  )

let do_local_auth uname pwd =
  with_throttle throttle_auth_internal (fun () ->
      try Pam.authenticate uname pwd
      with Failure msg ->
        raise
          Api_errors.(Server_error (session_authentication_failed, [uname; msg]))
  )

let do_local_change_password uname newpwd =
  with_throttle throttle_auth_internal (fun () ->
      Pam.change_password uname newpwd
  )

let trackid session_id = Context.trackid_of_session (Some session_id)

(* finds the intersection between group_membership_closure and pool's table of subject_ids *)
let get_intersection ~__context subject_ids_in_db subject_identifier
    group_membership_closure =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let reflexive_membership_closure =
    subject_identifier :: group_membership_closure
  in
  let intersection =
    Listext.List.intersect reflexive_membership_closure subject_ids_in_db
  in
  intersection

let get_subject_in_intersection ~__context subjects_in_db intersection =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  List.find
    (fun subj ->
      (* is this the subject ref that returned the non-empty intersection?*)
      List.hd intersection
      = Db.Subject.get_subject_identifier ~__context ~self:subj
    )
    subjects_in_db

let get_permissions ~__context ~subject_membership =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  (* see also rbac.ml *)
  let get_union_of_subsets ~get_subset_fn ~set =
    Listext.List.setify
      (List.fold_left (* efficiently compute unions of subsets in set *)
         (fun accu elem -> List.rev_append (get_subset_fn elem) accu)
         [] set
      )
  in
  let role_membership =
    get_union_of_subsets (*automatically removes duplicated roles*)
      ~get_subset_fn:(fun subj -> Db.Subject.get_roles ~__context ~self:subj)
      ~set:subject_membership
  in
  let permission_membership =
    get_union_of_subsets (*automatically removes duplicated perms*)
      ~get_subset_fn:(fun role ->
        try
          Xapi_role.get_name_label ~__context ~self:role
          :: Xapi_role.get_permissions_name_label ~__context ~self:role
        with _ -> []
        (* if the role disappeared, ignore it *)
      )
      ~set:role_membership
  in
  permission_membership

(* CP-827: finds out if the subject was suspended (ie. disabled,expired,locked-out) *)
let is_subject_suspended ~__context ~cache subject_identifier =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  (* obtains the subject's info containing suspension information *)
  let info =
    try
      Xapi_subject.get_subject_information_from_identifier ~__context ~cache
        subject_identifier
    with Auth_signature.Subject_cannot_be_resolved | Not_found ->
      (* user was not found in external directory in order to obtain info *)
      debug "Subject %s not found in external directory while re-obtaining info"
        subject_identifier ;
      []
    (* returns no user info, which will result in is_suspended = true *)
  in
  let subject_name =
    if List.mem_assoc Auth_signature.subject_information_field_subject_name info
    then
      List.assoc Auth_signature.subject_information_field_subject_name info
    else
      ""
  in
  let get_suspension_value name info =
    if List.mem_assoc name info (* is the required field present? *) then
      String.lowercase_ascii (List.assoc name info) <> "false"
    (* no suspension only if value is explicitly false *)
    else
      true
    (* if we didn't find the field, assumes the worse, ie. subject is suspended *)
  in
  (* obtains each field that could suspend an existing subject *)
  let is_subject_account_disabled =
    get_suspension_value "subject-account-disabled" info
  in
  let is_subject_account_expired =
    get_suspension_value "subject-account-expired" info
  in
  let is_subject_account_locked =
    get_suspension_value "subject-account-locked" info
  in
  let is_subject_password_expired =
    get_suspension_value "subject-password-expired" info
  in
  debug
    "Subject Suspension Status: a.disabled=%B a.expired=%B a.locked=%B \
     p.expired=%B"
    is_subject_account_disabled is_subject_account_expired
    is_subject_account_locked is_subject_password_expired ;
  (* decides if the subject is suspended *)
  let is_suspended =
    (* either one of those is sufficient for suspension *)
    is_subject_account_disabled
    || is_subject_account_expired
    || is_subject_account_locked
    || is_subject_password_expired
  in
  if is_suspended then
    debug "Subject identifier %s is suspended" subject_identifier ;
  (is_suspended, subject_name)

let reusable_pool_session = Atomic.make Ref.null

let destroy_db_session ~__context ~self =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  if self <> Atomic.get reusable_pool_session then (
    Xapi_event.on_session_deleted self ;
    (* unregister from the event system *)
    (* This info line is important for tracking, auditability and client accountability purposes on XenServer *)
    (* Never print the session id nor uuid: they are secret values that should be known only to the user that *)
    (* logged in. Instead, we print a non-invertible hash as the tracking id for the session id *)
    (* see also task creation in context.ml *)
    (* CP-982: create tracking id in log files to link username to actions *)
    info "Session.destroy %s" (trackid self) ;
    Rbac_audit.session_destroy ~__context ~session_id:self ;
    (try Db.Session.destroy ~__context ~self with _ -> ()) ;
    Rbac.destroy_session_permissions_tbl ~session_id:self
  ) else
    info "Skipping Session.destroy for reusable pool session %s" (trackid self)

(* CP-703: ensure that activate sessions are invalidated in a bounded time *)
(* in response to external authentication/directory services updates, such as *)
(* e.g. group membership changes, or even account disabled *)
let revalidate_external_session ~__context ~session =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  try
    (* guard: we only want to revalidate external sessions, where is_local_superuser is false *)
    (* Neither do we want to revalidate the special read-only external database sessions, since they can exist independent of external authentication. *)
    if
      not
        (Db.Session.get_is_local_superuser ~__context ~self:session
        || Xapi_database.Db_backend.is_session_registered (Ref.string_of session)
        )
    then (
      (* 1. is the external authentication disabled in the pool? *)
      let master = Helpers.get_master ~__context in
      let auth_type = Db.Host.get_external_auth_type ~__context ~self:master in
      if auth_type = "" then (
        (* if so, we must immediatelly destroy this external session *)
        let msg =
          Printf.sprintf
            "External authentication has been disabled, destroying session %s"
            (trackid session)
        in
        debug "%s" msg ;
        destroy_db_session ~__context ~self:session
      ) else
        (* otherwise, we try to revalidate it against the external authentication service *)
        let session_lifespan = 60.0 *. 30.0 in
        (* allowed session lifespan = 30 minutes *)
        let random_lifespan = Random.float 60.0 *. 10.0 in

        (* extra random (up to 10min) lifespan to spread access to external directory *)

        (* 2. has the external session expired/does it need revalidation? *)
        let session_last_validation_time =
          Date.to_unix_time
            (Db.Session.get_validation_time ~__context ~self:session)
        in
        let now = Date.now () in
        let session_needs_revalidation =
          Date.to_unix_time now
          > session_last_validation_time +. session_lifespan +. random_lifespan
        in
        if session_needs_revalidation then (
          (* if so, then:*)
          debug "session %s needs revalidation" (trackid session) ;
          let authenticated_user_sid =
            Db.Session.get_auth_user_sid ~__context ~self:session
          in

          (* 2a. revalidate external authentication *)

          (* CP-827: if the user was suspended (disabled,expired,locked-out), then we must destroy the session *)
          let suspended, _ =
            is_subject_suspended ~__context ~cache:true authenticated_user_sid
          in
          let suspended =
            if suspended then
              is_subject_suspended ~__context ~cache:false
                authenticated_user_sid
              |> fst
            else
              suspended
          in
          if suspended then (
            debug
              "Subject (identifier %s) has been suspended, destroying session \
               %s"
              authenticated_user_sid (trackid session) ;
            (* we must destroy the session in this case *)
            destroy_db_session ~__context ~self:session
          ) else
            try
              (* if the user is not in the external directory service anymore, this call raises Not_found *)
              let group_membership_closure =
                (Ext_auth.d ()).query_group_membership ~__context
                  authenticated_user_sid
              in
              debug "obtained group membership for session %s, sid %s "
                (trackid session) authenticated_user_sid ;
              (* 2b. revalidate membership intersection *)
              (* this verifies if the user still has permission to have a session *)
              let subjects_in_db = Db.Subject.get_all ~__context in
              let subject_ids_in_db =
                List.map
                  (fun subj ->
                    Db.Subject.get_subject_identifier ~__context ~self:subj
                  )
                  subjects_in_db
              in
              let intersection =
                get_intersection ~__context subject_ids_in_db
                  authenticated_user_sid group_membership_closure
              in
              debug "verified intersection for session %s, sid %s "
                (trackid session) authenticated_user_sid ;
              let in_intersection = intersection <> [] in
              if not in_intersection then (
                (* empty intersection: externally-authenticated subject no longer has login rights in the pool *)
                let msg =
                  Printf.sprintf
                    "Subject (identifier %s) has no access rights in this \
                     pool, destroying session %s"
                    authenticated_user_sid (trackid session)
                in
                debug "%s" msg ;
                (* we must destroy the session in this case *)
                destroy_db_session ~__context ~self:session
              ) else (
                (* non-empty intersection: externally-authenticated subject still has login rights in the pool *)

                (* OK, SESSION REVALIDATED SUCCESSFULLY *)
                (* 2c. update session state *)

                (* session passed revalidation, let's update its last revalidation time *)
                Db.Session.set_validation_time ~__context ~self:session
                  ~value:now ;
                debug "updated validation time for session %s, sid %s "
                  (trackid session) authenticated_user_sid ;
                (* let's also update the session's subject ref *)
                try
                  let subject_in_intersection =
                    get_subject_in_intersection ~__context subjects_in_db
                      intersection
                  in
                  if
                    subject_in_intersection
                    <> Db.Session.get_subject ~__context ~self:session
                  then (
                    (* the subject in the intersection has changed!!! *)
                    Db.Session.set_subject ~__context ~self:session
                      ~value:subject_in_intersection ;
                    debug "updated subject for session %s, sid %s "
                      (trackid session) authenticated_user_sid
                  )
                with Not_found ->
                  (* subject ref for intersection's sid does not exist in our metadata!!! *)
                  (* this should never happen, it's an internal metadata inconsistency between steps 2b and 2c *)
                  let msg =
                    Printf.sprintf
                      "Subject (identifier %s) is not present in this pool, \
                       destroying session %s"
                      authenticated_user_sid (trackid session)
                  in
                  debug "%s" msg ;
                  (* we must destroy the session in this case *)
                  destroy_db_session ~__context ~self:session
              )
            with Auth_signature.Subject_cannot_be_resolved | Not_found ->
              (* user was not found in external directory in order to obtain group membership *)
              let msg =
                Printf.sprintf
                  "Subject %s not found in external directory while \
                   re-obtaining its group membership closure, destroying \
                   session %s"
                  authenticated_user_sid (trackid session)
              in
              debug "%s" msg ;
              (* user is not in the external directory anymore: we must destroy the session in this case *)
              destroy_db_session ~__context ~self:session
        ) ;
        debug "end revalidation of session %s " (trackid session)
    )
  with e ->
    (*unexpected exception: we absorb it and print out a debug line *)
    debug "Unexpected exception while revalidating session %s: %s"
      (trackid session)
      (ExnHelper.string_of_exn e)

(* CP-703: ensure that activate sessions are invalidated in a bounded time *)
(* in response to external authentication/directory services updates, such as *)
(* e.g. group membership changes, or even account disabled *)
let revalidate_all_sessions ~__context =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  try
    debug "revalidating all external sessions in the local host" ;
    (* obtain all sessions in the pool *)
    let sessions = Db.Session.get_all ~__context in
    (* filter out those sessions where is_local_superuser or client_certificate is true *)
    (* we only want to revalidate the sessions created using the external authentication service *)
    let external_sessions =
      List.filter
        (fun session ->
          (not (Db.Session.get_is_local_superuser ~__context ~self:session))
          && not (Db.Session.get_client_certificate ~__context ~self:session)
        )
        sessions
    in
    (* revalidate each external session *)
    List.iter
      (fun session -> revalidate_external_session ~__context ~session)
      external_sessions
  with e ->
    (*unexpected exception: we absorb it and print out a debug line *)
    debug "Unexpected exception while revalidating external sessions: %s"
      (ExnHelper.string_of_exn e)

let login_no_password_common_create_session ~__context ~uname ~originator ~host
    ~pool ~is_local_superuser ~subject ~auth_user_sid ~auth_user_name
    ~rbac_permissions ~db_ref ~client_certificate =
  Context.with_tracing ~originator ~__context __FUNCTION__ @@ fun __context ->
  let create_session () =
    let session_id = Ref.make_secret () in
    let uuid = Uuidx.to_string (Uuidx.make_uuid_urnd ()) in
    let user = Ref.null in
    (* always return a null reference to the deprecated user object *)
    let parent = try Context.get_session_id __context with _ -> Ref.null in
    (*match uname with   (* the user object is deprecated in favor of subject *)
      					Some uname -> Helpers.get_user ~__context uname
      				| None -> Ref.null in*)
    (* This info line is important for tracking, auditability and client accountability purposes on XenServer *)
    (* Never print the session id nor uuid: they are secret values that should be known only to the user that *)
    (* has just logged in. Instead, we print a non-invertible hash as the tracking id for the session id *)
    (* see also task creation in context.ml *)
    (* CP-982: promote tracking debug line to info status *)
    (* CP-982: create tracking id in log files to link username to actions *)
    info
      "Session.create %s pool=%b uname=%s originator=%s is_local_superuser=%b \
       auth_user_sid=%s parent=%s"
      (trackid session_id) pool
      (match uname with None -> "" | Some u -> u)
      originator is_local_superuser auth_user_sid (trackid parent) ;
    let now = Date.now () in
    Db.Session.create ~__context ~ref:session_id ~uuid ~this_user:user
      ~this_host:host ~pool ~last_active:now ~other_config:[] ~subject
      ~is_local_superuser ~auth_user_sid ~validation_time:now ~auth_user_name
      ~rbac_permissions ~parent ~originator ~client_certificate ;
    if not pool then
      Atomic.incr total_sessions ;
    Ref.string_of session_id
  in
  let session_id =
    Ref.of_secret_string
      ( match db_ref with
      | Some db_ref ->
          Xapi_database.Db_backend.create_registered_session create_session
            db_ref
      | None ->
          create_session ()
      )
  in
  Rbac_audit.session_create ~__context ~session_id ~uname ;
  (* At this point, the session is created, but with an incorrect time *)
  (* Force the time to be updated by calling an API function with this session *)
  let rpc = Helpers.make_rpc ~__context in
  ignore (Client.Pool.get_all ~rpc ~session_id) ;
  session_id

let login_no_password_common ~__context ~uname ~originator ~host ~pool
    ~is_local_superuser ~subject ~auth_user_sid ~auth_user_name
    ~rbac_permissions ~db_ref ~client_certificate =
  Context.with_tracing ~originator ~__context __FUNCTION__ @@ fun __context ->
  let is_valid_session session_id =
    match (session_id, !Xapi_globs.validate_reusable_pool_session) with
    | session, _ when session = Ref.null ->
        false
    | _, false ->
        true
    | session_id, true -> (
      try
        (* Call an API function to check the session is still valid *)
        let rpc = Helpers.make_rpc ~__context in
        ignore (Client.Pool.get_all ~rpc ~session_id) ;
        true
      with Api_errors.Server_error (err, _) ->
        debug "%s: Invalid session: %s" __FUNCTION__ err ;
        false
    )
  in
  let create_session () =
    login_no_password_common_create_session ~__context ~uname ~originator ~host
      ~pool ~is_local_superuser ~subject ~auth_user_sid ~auth_user_name
      ~rbac_permissions ~db_ref ~client_certificate
  in
  let rec get_session () =
    let session = Atomic.get reusable_pool_session in
    if is_valid_session session then (
      (* Check if the session changed during validation.
         Use our version regardless to avoid being stuck in a loop of session creation *)
      if Atomic.get reusable_pool_session <> session then
        debug "reusable_pool_session has changed, using the original anyway" ;
      session
    ) else
      let new_session = create_session () in
      if Atomic.compare_and_set reusable_pool_session session new_session then
        new_session
      else (
        (* someone else raced with us and created a session, destroy ours and attempt to use theirs *)
        destroy_db_session ~__context ~self:new_session ;
        (get_session [@tailcall]) ()
      )
  in
  if
    (originator, pool, is_local_superuser, uname)
    = (xapi_internal_originator, true, true, None)
    && !Xapi_globs.reuse_pool_sessions
  then
    get_session ()
  else
    create_session ()

(* XXX: only used internally by the code which grants the guest access to the API.
   Needs to be protected by a proper access control system *)
let login_no_password ~__context ~uname ~host ~pool ~is_local_superuser ~subject
    ~auth_user_sid ~auth_user_name ~rbac_permissions =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  login_no_password_common ~__context ~uname
    ~originator:xapi_internal_originator ~host ~pool ~is_local_superuser
    ~subject ~auth_user_sid ~auth_user_name ~rbac_permissions ~db_ref:None
    ~client_certificate:false

(** Cause the master to update the session last_active every 30s or so *)
let consider_touching_session rpc session_id =
  let time = ref (Unix.gettimeofday ()) in
  let interval = 30. in
  (* 30 seconds *)
  fun () ->
    if Unix.gettimeofday () -. !time > interval then (
      time := Unix.gettimeofday () ;
      (* a side-effect is that the master updates the session *)
      ignore (Client.Session.get_uuid ~rpc ~session_id ~self:session_id)
    )

(* Make sure the pool secret matches *)
let slave_login_common ~__context ~host_str ~psecret =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let f () =
    if not (Helpers.PoolSecret.is_authorized psecret) then (
      let msg = "Pool credentials invalid" in
      debug "Failed to authenticate slave %s: %s" host_str msg ;
      raise
        Api_errors.(
          Server_error (session_authentication_failed, [host_str; msg])
        )
    )
  in
  if !Constants.tgroups_enabled then (
    let open Xapi_stdext_threads.Threadext in
    let tgroup =
      Tgroup.of_creator (Tgroup.Description.Creator.make ~intrapool:true ())
    in
    let thread_ctx = ThreadRuntimeContext.get () in
    (* authenticated_root here should mean a group has not been set yet and
       we should set one. otherwise go with what has already been set.*)
    if
      thread_ctx.tgroup = Tgroup.Description.authenticated_root
      || thread_ctx.tgroup = Tgroup.Description.unauthenticated
    then
      ThreadRuntimeContext.update
        (fun thread_ctx -> {thread_ctx with tgroup})
        thread_ctx ;
    Tgroup.with_one_thread_of_group tgroup f
  ) else
    f ()

(* Normal login, uses the master's database *)
let slave_login ~__context ~host ~psecret =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  slave_login_common ~__context ~host_str:(Ref.string_of host) ~psecret ;
  login_no_password ~__context ~uname:None ~host ~pool:true
    ~is_local_superuser:true ~subject:Ref.null ~auth_user_sid:""
    ~auth_user_name:(Ref.string_of host) ~rbac_permissions:[]

(* Emergency mode login, uses local storage *)
let slave_local_login ~__context ~psecret =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  slave_login_common ~__context ~host_str:"localhost" ~psecret ;
  debug "Add session to local storage" ;
  Xapi_local_session.create ~__context ~pool:true

(* Emergency mode login, uses local storage *)
let slave_local_login_with_password ~__context ~uname ~pwd =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  if Context.preauth ~__context <> Some `root then (
    try
      (* CP696 - only tries to authenticate against LOCAL superuser account *)
      do_local_auth uname pwd
    with Failure msg ->
      debug "Failed to authenticate user %s: %s" uname msg ;
      raise
        (Api_errors.Server_error
           (Api_errors.session_authentication_failed, [uname; msg])
        )
  ) ;
  debug "Add session to local storage" ;
  Xapi_local_session.create ~__context ~pool:false

module Caching = struct
  type external_auth_result = {
      subject: [`subject] Ref.t
    ; subject_identifier: string
    ; subject_name: string
    ; rbac_permissions: string list
  }

  module type EXTERNAL_AUTH_CACHE =
    Helpers.AuthenticationCache.S
      with type user = string
       and type password = string
       and type session = external_auth_result

  let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

  let create_salt () =
    (* Creates a Cstruct of length 8. *)
    let data = Mirage_crypto_rng.generate 8 in
    let bytes = Cstruct.to_bytes data in
    (* Encode the salt as a hex string. Each byte becomes 2
       hexadecimal digits, so the length is 16 (the maximum for
       crypt_r). *)
    let hexify char acc = Printf.sprintf "%s%02x" acc (Char.code char) in
    Bytes.fold_right hexify bytes ""

  module AuthenticatedResult = struct
    type key = string

    type salt = string

    type digest = string

    type secret = external_auth_result

    type t = digest * salt * secret

    let create digest salt secret = (digest, salt, secret)

    let read ((_, _, _) as t) = t

    let create_salt = create_salt

    let hash key salt =
      match Pam.(crypt ~algo:SHA512 ~key ~salt) with
      | Ok hash ->
          hash
      | Error _ ->
          failwith ("Unable to compute hash in " ^ __FUNCTION__)

    let equal_digest = ( = )
  end

  module AuthenticationCache : EXTERNAL_AUTH_CACHE =
    Helpers.AuthenticationCache.Make (String) (AuthenticatedResult)

  let cache = ref None

  let lock = Mutex.create ()

  let ( let@ ) = ( @@ )

  (* Attain the extant cache or get nothing if caching is
     disabled. *)
  let get_or_init_cache ~__context =
    let pool = Helpers.get_pool ~__context in
    let cache_enabled =
      Db.Pool.get_ext_auth_cache_enabled ~__context ~self:pool
    in
    if not cache_enabled then
      None
    else
      let@ () = with_lock lock in
      match !cache with
      | Some _ as extant ->
          extant
      | _ ->
          let capacity =
            Db.Pool.get_ext_auth_cache_size ~__context ~self:pool
            |> Int64.to_int
          in
          let ttl =
            Db.Pool.get_ext_auth_cache_expiry ~__context ~self:pool
            |> Int64.unsigned_to_int
            |> Option.map (fun sec -> Mtime.Span.(sec * s))
            |> Option.value ~default:Mtime.Span.(5 * min)
          in
          let span = Format.asprintf "%a" Mtime.Span.pp ttl in
          info "Creating authentication cache of capacity %d and TTL of %s"
            capacity span ;
          let auth_cache = AuthenticationCache.create ~size:capacity ~ttl in
          let instance = Some auth_cache in
          cache := instance ;
          instance

  (* Try to insert into cache. The cache could have been disabled
     during query to external authentication plugin. *)
  let insert_into_cache ~__context username password result =
    match get_or_init_cache ~__context with
    | None ->
        ()
    | Some cache ->
        let@ () = with_lock lock in
        AuthenticationCache.cache cache username password result

  (* Consult the cache or rely on a provided "slow path". Each time
     the slow path is invoked, an attempt is made to cache its result. *)
  let memoize ~__context username password ~slow_path =
    let slow_path () =
      let ext_auth_result = slow_path () in
      insert_into_cache ~__context username password ext_auth_result ;
      ext_auth_result
    in
    match get_or_init_cache ~__context with
    | None ->
        slow_path ()
    | Some cache -> (
        let result =
          let@ () = with_lock lock in
          AuthenticationCache.cached cache username password
        in
        match result with
        | None ->
            slow_path ()
        | Some prev_result ->
            prev_result
      )

  let clear_cache () =
    info "Clearing authentication cache" ;
    let@ () = with_lock lock in
    cache := None
end

let clear_external_auth_cache = Caching.clear_cache

(* CP-714: Modify session.login_with_password to first try local super-user
   login; and then call into external auth plugin if this is enabled
   1. If the pool master's Host.external_auth_type field is not none, then the
      Session.login_with_password XenAPI method will:
      - try and authenticate locally (checking whether the supplied credentials
        refer to the local superuser account); and then if this authentication
        step fails
      - try and authenticate remotely, passing the supplied username/password
        to the external auth/directory service. (Note: see below for definition
        of 'authenticate remotely')
   2. otherwise, Session.login_with_password will only attempt to authenticate
      against the local superuser credentials
*)
let login_with_password ~__context ~uname ~pwd ~version:_ ~originator =
  Context.with_tracing ~originator ~__context __FUNCTION__ @@ fun __context ->
  (* !!! Do something with the version number *)
  match Context.preauth ~__context with
  | Some `root ->
      (* in this case, the context origin of this login request is a unix socket bound locally to a filename *)
      (* we trust requests from local unix filename sockets, so no need to authenticate them before login *)
      let f () =
        login_no_password_common ~__context ~uname:(Some uname) ~originator
          ~host:(Helpers.get_localhost ~__context)
          ~pool:false ~is_local_superuser:true ~subject:Ref.null
          ~auth_user_sid:"" ~auth_user_name:uname ~rbac_permissions:[]
          ~db_ref:None ~client_certificate:false
      in
      if !Constants.tgroups_enabled then (
        let open Xapi_stdext_threads.Threadext in
        let tgroup =
          Tgroup.of_creator
            Tgroup.Description.(Creator.make ~identity:Identity.root_identity ())
        in
        let thread_ctx = ThreadRuntimeContext.get () in
        (* authenticated_root here should mean a group has not been set yet and
           we should set one. otherwise go with what has already been set.*)
        if
          thread_ctx.tgroup = Tgroup.Description.authenticated_root
          || thread_ctx.tgroup = Tgroup.Description.unauthenticated
        then
          ThreadRuntimeContext.update
            (fun thread_ctx -> {thread_ctx with tgroup})
            thread_ctx ;
        Tgroup.with_one_thread_of_group tgroup f
      ) else
        f ()
  | Some `client_cert ->
      (* The session was authenticated by stunnel's verification of the client certificate,
         so we do not need to verify the username/password. Grant access to functions
         based on the special "client_cert" RBAC role. *)
      let role =
        match
          Xapi_role.get_by_name_label ~__context
            ~label:Datamodel_roles.role_client_cert
        with
        | role :: _ ->
            role
        | [] ->
            Helpers.internal_error "%s role not found"
              Datamodel_roles.role_client_cert
      in
      let rbac_permissions =
        Xapi_role.get_permissions_name_label ~__context ~self:role
      in
      login_no_password_common ~__context ~uname:(Some uname) ~originator
        ~host:(Helpers.get_localhost ~__context)
        ~pool:false ~is_local_superuser:false ~subject:Ref.null
        ~auth_user_sid:"" ~auth_user_name:uname ~rbac_permissions ~db_ref:None
        ~client_certificate:true
  | None -> (
      let () =
        if Pool_role.is_slave () then
          raise
            (Api_errors.Server_error
               (Api_errors.host_is_slave, [Pool_role.get_master_address ()])
            )
      in
      let login_as_local_superuser auth_type =
        if auth_type <> "" && uname <> local_superuser then
          (* makes local superuser = root only*)
          failwith ("Local superuser must be " ^ local_superuser)
        else (
          do_local_auth uname pwd ;
          debug "Success: local auth, user %s from %s" uname
            (Context.get_origin __context) ;

          let f () =
            login_no_password_common ~__context ~uname:(Some uname) ~originator
              ~host:(Helpers.get_localhost ~__context)
              ~pool:false ~is_local_superuser:true ~subject:Ref.null
              ~auth_user_sid:"" ~auth_user_name:uname ~rbac_permissions:[]
              ~db_ref:None ~client_certificate:false
          in
          if !Constants.tgroups_enabled then (
            let open Xapi_stdext_threads.Threadext in
            let tgroup =
              Tgroup.of_creator
                Tgroup.Description.(
                  Creator.make ~identity:Identity.root_identity ()
                )
            in
            let thread_ctx = ThreadRuntimeContext.get () in
            (* authenticated_root here should mean a group has not been set yet and
               we should set one. otherwise go with what has already been set.*)
            if
              thread_ctx.tgroup = Tgroup.Description.authenticated_root
              || thread_ctx.tgroup = Tgroup.Description.unauthenticated
            then
              ThreadRuntimeContext.update
                (fun thread_ctx -> {thread_ctx with tgroup})
                thread_ctx ;

            Tgroup.with_one_thread_of_group tgroup f
          ) else
            f ()
        )
      in
      let thread_delay_and_raise_error ~error uname msg =
        let some_seconds = 5.0 in
        Thread.delay some_seconds ;
        (* sleep a bit to avoid someone brute-forcing the password *)
        if error = Api_errors.session_authentication_failed then
          raise (Api_errors.Server_error (error, [uname; msg]))
        else if error = Api_errors.session_authorization_failed then
          raise Api_errors.(Server_error (error, [uname; msg]))
        else
          raise
            (Api_errors.Server_error
               (error, ["session.login_with_password"; msg])
            )
      in
      match
        Db.Host.get_external_auth_type ~__context
          ~self:(Helpers.get_localhost ~__context)
      with
      | "" as auth_type -> (
        try
          (* no external authentication *)

          (*debug "External authentication is disabled";*)
          (* only attempts to authenticate against the local superuser credentials *)
          login_as_local_superuser auth_type
        with Failure msg ->
          info "Failed to locally authenticate user %s from %s: %s" uname
            (Context.get_origin __context)
            msg ;
          thread_delay_and_raise_error
            ~error:Api_errors.session_authentication_failed uname msg
      )
      | _ as auth_type -> (
          (* external authentication required *)
          debug "External authentication %s is enabled" auth_type ;
          (* 1. first attempts to authenticate against the local superuser *)
          try login_as_local_superuser auth_type
          with Failure msg -> (
            try
              debug "Failed to locally authenticate user %s from %s: %s" uname
                (Context.get_origin __context)
                msg ;
              (* 2. then against the external auth service *)
              (* 2.1. we first check the external auth service status *)
              let rec waiting_event_hook_auth_on_xapi_initialize_succeeded
                  seconds =
                if not !Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded
                then (
                  if seconds <= 0 then (
                    let msg =
                      Printf.sprintf
                        "External authentication %s service still initializing"
                        auth_type
                    in
                    error "%s" msg ;
                    thread_delay_and_raise_error uname msg
                      ~error:Api_errors.internal_error
                  ) else
                    debug "External authentication %s service initializing..."
                      auth_type ;
                  Thread.delay 1.0 ;
                  waiting_event_hook_auth_on_xapi_initialize_succeeded
                    (seconds - 1)
                )
              in
              waiting_event_hook_auth_on_xapi_initialize_succeeded 120 ;
              let query_external_auth () : Caching.external_auth_result =
                (* 2.2. we then authenticate the usee using the external authentication plugin *)
                (* so that we know that he/she exists there *)
                let subject_identifier =
                  try
                    let _subject_identifier =
                      do_external_auth ~__context uname pwd
                    in
                    debug
                      "Successful external authentication user %s \
                       (subject_identifier, %s from %s)"
                      uname _subject_identifier
                      (Context.get_origin __context) ;
                    _subject_identifier
                  with Auth_signature.Auth_failure msg ->
                    info "Failed to externally authenticate user %s from %s: %s"
                      uname
                      (Context.get_origin __context)
                      msg ;
                    thread_delay_and_raise_error
                      ~error:Api_errors.session_authentication_failed uname msg
                in
                (* as per tests in CP-827, there should be no need to call is_subject_suspended function here, *)
                (* because the authentication server in 2.1 will already reflect if account/password expired, *)
                (* disabled, locked-out etc, but since likewise doesn't timely reflect this information *)
                (* at the same time for both authentication and subject info queries (modification in the AD *)
                (* reflects immediately for AD authentication, but can take 1 hour to reflect on subject info), *)
                (* we need to call it here in order to be consistent with the session revalidation function. *)
                (* Otherwise, there might be cases where the initial authentication/login succeeds, but *)
                (* then a few minutes later the revalidation finds that the user is 'suspended' (due to *)
                (* subject info caching problems in likewise) and closes the user's session *)
                let subject_suspended, subject_name =
                  try
                    let suspended, name =
                      is_subject_suspended ~__context ~cache:true
                        subject_identifier
                    in
                    if suspended then
                      is_subject_suspended ~__context ~cache:false
                        subject_identifier
                    else
                      (suspended, name)
                  with Auth_signature.Auth_service_error (_, msg) ->
                    debug
                      "Failed to find if user %s (subject_id %s, from %s) is \
                       suspended: %s"
                      uname subject_identifier
                      (Context.get_origin __context)
                      msg ;
                    thread_delay_and_raise_error
                      ~error:Api_errors.session_authorization_failed uname msg
                in
                if subject_suspended then (
                  let msg =
                    Printf.sprintf
                      "User %s (subject_id %s, from %s) suspended in external \
                       directory"
                      uname subject_identifier
                      (Context.get_origin __context)
                  in
                  debug "%s" msg ;
                  thread_delay_and_raise_error
                    ~error:Api_errors.session_authorization_failed uname msg
                ) else
                  (* 2.2. then, we verify if any elements of the the membership closure of the externally *)
                  (* authenticated subject_id is inside our local allowed-to-login subjects list *)
                  (* finds all the groups a user belongs to (non-reflexive closure of member-of relation) *)
                  let group_membership_closure =
                    try
                      (Ext_auth.d ()).query_group_membership ~__context
                        subject_identifier
                    with
                    | Not_found | Auth_signature.Subject_cannot_be_resolved ->
                        let msg =
                          Printf.sprintf
                            "Failed to obtain the group membership closure for \
                             user %s (subject_id %s, from %s): user not found \
                             in external directory"
                            uname
                            (Context.get_origin __context)
                            subject_identifier
                        in
                        debug "%s" msg ;
                        thread_delay_and_raise_error
                          ~error:Api_errors.session_authorization_failed uname
                          msg
                    | Auth_signature.Auth_service_error (_, msg) ->
                        debug
                          "Failed to obtain the group membership closure for \
                           user %s (subject_id %s, from %s): %s"
                          uname subject_identifier
                          (Context.get_origin __context)
                          msg ;
                        thread_delay_and_raise_error
                          ~error:Api_errors.session_authorization_failed uname
                          msg
                  in
                  (* finds the intersection between group_membership_closure and pool's table of subject_ids *)
                  let subjects_in_db = Db.Subject.get_all ~__context in
                  let subject_ids_in_db =
                    List.map
                      (fun subj ->
                        ( subj
                        , Db.Subject.get_subject_identifier ~__context
                            ~self:subj
                        )
                      )
                      subjects_in_db
                  in
                  let reflexive_membership_closure =
                    subject_identifier :: group_membership_closure
                  in
                  (* returns all elements of reflexive_membership_closure that are inside subject_ids_in_db *)
                  let intersect ext_sids db_sids =
                    List.filter
                      (fun (_, db_sid) -> List.mem db_sid ext_sids)
                      db_sids
                  in
                  let intersection =
                    intersect reflexive_membership_closure subject_ids_in_db
                  in
                  (* 2.3. finally, we create the session for the authenticated subject if any membership intersection was found *)
                  let in_intersection = intersection <> [] in
                  if not in_intersection then (
                    (* empty intersection: externally-authenticated subject has no login rights in the pool *)
                    let msg =
                      Printf.sprintf
                        "Subject %s (identifier %s, from %s) has no access \
                         rights in this pool"
                        uname subject_identifier
                        (Context.get_origin __context)
                    in
                    info "%s" msg ;
                    thread_delay_and_raise_error
                      ~error:Api_errors.session_authorization_failed uname msg
                  ) else (* compute RBAC structures for the session *)
                    let subject_membership = List.map fst intersection in
                    debug
                      "subject membership intersection with subject-list=[%s]"
                      (List.fold_left
                         (fun i (subj_ref, sid) ->
                           let subj_ref =
                             try
                               (* attempt to resolve subject_ref -> subject_name *)
                               List.assoc
                                 Auth_signature
                                 .subject_information_field_subject_name
                                 (Db.Subject.get_other_config ~__context
                                    ~self:subj_ref
                                 )
                             with _ -> Ref.string_of subj_ref
                           in
                           if i = "" then
                             subj_ref ^ " (" ^ sid ^ ")"
                           else
                             i ^ "," ^ subj_ref ^ " (" ^ sid ^ ")"
                         )
                         "" intersection
                      ) ;
                    let rbac_permissions =
                      get_permissions ~__context ~subject_membership
                    in
                    (* CP-1260: If a subject has no roles assigned, then authentication will fail with an error such as PERMISSION_DENIED.*)
                    if rbac_permissions = [] then (
                      let msg =
                        Printf.sprintf
                          "Subject %s (identifier %s) has no roles in this pool"
                          uname subject_identifier
                      in
                      info "%s" msg ;
                      thread_delay_and_raise_error uname msg
                        ~error:Api_errors.rbac_permission_denied
                    ) else
                      (* non-empty intersection: externally-authenticated subject has login rights in the pool *)
                      let subject =
                        (* return reference for the subject obj in the db *)
                        (* obs: this obj ref can point to either a user or a group contained in the local subject db list *)
                        try
                          List.find
                            (fun subj ->
                              (* is this the subject ref that returned the non-empty intersection?*)
                              List.hd intersection
                              = ( subj
                                , Db.Subject.get_subject_identifier ~__context
                                    ~self:subj
                                )
                            )
                            subjects_in_db
                          (* goes through exactly the same subject list that we went when computing the intersection, *)
                          (* so that no one is able to undetectably remove/add another subject with the same subject_identifier *)
                          (* between that time 2.2 and now 2.3 *)
                        with Not_found ->
                          (* this should never happen, it shows an inconsistency in the db between 2.2 and 2.3 *)
                          let msg =
                            Printf.sprintf
                              "Subject %s (identifier %s, from %s) is not \
                               present in this pool"
                              uname subject_identifier
                              (Context.get_origin __context)
                          in
                          debug "%s" msg ;
                          thread_delay_and_raise_error
                            ~error:Api_errors.session_authorization_failed uname
                            msg
                      in
                      {
                        subject
                      ; subject_identifier
                      ; subject_name
                      ; rbac_permissions
                      }
              in
              let Caching.
                    {
                      subject
                    ; subject_identifier
                    ; subject_name
                    ; rbac_permissions
                    } =
                Caching.memoize ~__context uname pwd
                  ~slow_path:query_external_auth
              in

              let f () =
                login_no_password_common ~__context ~uname:(Some uname)
                  ~originator
                  ~host:(Helpers.get_localhost ~__context)
                  ~pool:false ~is_local_superuser:false ~subject
                  ~auth_user_sid:subject_identifier ~auth_user_name:subject_name
                  ~rbac_permissions ~db_ref:None ~client_certificate:false
              in
              if !Constants.tgroups_enabled then (
                let open Xapi_stdext_threads.Threadext in
                let tgroup =
                  Tgroup.of_creator
                    Tgroup.Description.(
                      Creator.make
                        ~identity:(Identity.make subject_identifier)
                        ()
                    )
                in
                let thread_ctx = ThreadRuntimeContext.get () in
                (* authenticated_root here should mean a group has not been set yet and
                   we should set one. otherwise go with what has already been set.*)
                if
                  thread_ctx.tgroup = Tgroup.Description.authenticated_root
                  || thread_ctx.tgroup = Tgroup.Description.unauthenticated
                then
                  ThreadRuntimeContext.update
                    (fun thread_ctx -> {thread_ctx with tgroup})
                    thread_ctx ;

                Tgroup.with_one_thread_of_group tgroup f
              ) else
                f ()
              (* we only reach this point if for some reason a function above forgot to catch a possible exception in the Auth_signature module*)
            with
            | Not_found | Auth_signature.Subject_cannot_be_resolved ->
                let msg =
                  Printf.sprintf
                    "user %s from %s not found in external directory" uname
                    (Context.get_origin __context)
                in
                debug
                  "A function failed to catch this exception for user %s \
                   during external authentication: %s"
                  uname msg ;
                thread_delay_and_raise_error
                  ~error:Api_errors.session_authorization_failed uname msg
            | Auth_signature.Auth_failure msg ->
                debug
                  "A function failed to catch this exception for user %s. \
                   Auth_failure: %s"
                  uname msg ;
                thread_delay_and_raise_error
                  ~error:Api_errors.session_authentication_failed uname msg
            | Auth_signature.Auth_service_error (_, msg) ->
                debug
                  "A function failed to catch this exception for user %s from \
                   %s during external authentication: %s"
                  uname
                  (Context.get_origin __context)
                  msg ;
                thread_delay_and_raise_error
                  ~error:Api_errors.session_authorization_failed uname msg
            | Api_errors.Server_error _ as e ->
                (* bubble up any api_error already generated *)
                raise e
            | e ->
                (* generic catch-all for unexpected exceptions during external authentication *)
                let msg = ExnHelper.string_of_exn e in
                debug
                  "(generic) A function failed to catch this exception for \
                   user %s from %s during external authentication: %s"
                  uname
                  (Context.get_origin __context)
                  msg ;
                thread_delay_and_raise_error ~error:Api_errors.internal_error
                  uname msg
          )
        )
    )

let change_password ~__context ~old_pwd ~new_pwd =
  ignore old_pwd ;
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let session_id = Context.get_session_id __context in
  (*let user = Db.Session.get_this_user ~__context ~self:session_id in
    	let uname = Db.User.get_short_name ~__context ~self:user in*)
  let uname = local_superuser in
  (* user class has been deprecated *)
  if Db.Session.get_is_local_superuser ~__context ~self:session_id then (
    try
      (* CP-696: only change password if session has is_local_superuser bit set *)
      (*
  CA-13567: If you have root privileges then we do not authenticate old_pwd; right now, since we only
            ever have root privileges we just comment this out.

	begin
	  try
	    do_auth uname old_pwd
	  with (Failure msg) ->
	    debug "Failed to authenticate user %s: %s" uname msg;
	    raise (Api_errors.Server_error (Api_errors.session_authentication_failed,[uname;msg]))
	end;
*)
      do_local_change_password uname new_pwd ;
      info "Password changed successfully for user %s" uname ;
      info "Syncing password change across hosts in pool" ;
      (* tell all hosts (except me to sync new passwd file) *)
      let hash = Helpers.compute_hash () in
      let hosts = Db.Host.get_all ~__context in
      let hosts =
        List.filter (fun hostref -> hostref <> !Xapi_globs.localhost_ref) hosts
      in
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          List.iter
            (fun host ->
              try
                Client.Host.request_config_file_sync ~rpc ~session_id ~host
                  ~hash
              with e ->
                error "Failed to sync password to host %s: %s"
                  (Db.Host.get_name_label ~__context ~self:host)
                  (Printexc.to_string e)
            )
            hosts
      ) ;
      info "Finished syncing password across pool"
    with Failure msg ->
      error "Failed to change password for user %s: %s" uname msg ;
      raise
        (Api_errors.Server_error (Api_errors.change_password_rejected, [msg]))
  ) else
    (* CP-696: session does not have is_local_superuser bit set, so we must fail *)
    let msg = Printf.sprintf "Failed to change password for user %s" uname in
    debug "User %s is not local superuser: %s" uname msg ;
    raise
      (Api_errors.Server_error (Api_errors.user_is_not_local_superuser, [msg]))

let logout ~__context =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let session_id = Context.get_session_id __context in
  destroy_db_session ~__context ~self:session_id

let local_logout ~__context =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let session_id = Context.get_session_id __context in
  Xapi_local_session.destroy ~__context ~self:session_id

let get_group_subject_identifier_from_session ~__context ~session =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let subj = Db.Session.get_subject ~__context ~self:session in
  try Db.Subject.get_subject_identifier ~__context ~self:subj with
  | Db_exn.DBCache_NotFound ("missing row", _, _) ->
      (* expected error: subject was removed from subject list *)
      ""
  | e ->
      (* unexpected error *)
      debug "error obtaining sid from subject %s from session %s: %s"
        (Ref.string_of subj) (Ref.string_of session)
        (ExnHelper.string_of_exn e) ;
      ""

let get_all_subject_identifiers ~__context =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let all_sessions = Db.Session.get_all ~__context in
  let all_extauth_sessions =
    List.filter
      (fun session ->
        (* an externally-authenticated session is one which is not a local_superuser session *)
        not (Db.Session.get_is_local_superuser ~__context ~self:session)
      )
      all_sessions
  in
  (* we only want to return sids of externally-authenticated sessions *)
  let all_auth_user_sids_in_sessions =
    List.map
      (fun session -> Db.Session.get_auth_user_sid ~__context ~self:session)
      all_extauth_sessions
  in
  let all_subject_list_sids_in_sessions =
    List.filter
      (fun e -> e <> "")
      (List.map
         (fun session ->
           (* TODO: better to look up the membership closure *)
           get_group_subject_identifier_from_session ~__context ~session
         )
         all_extauth_sessions
      )
  in
  (* avoid returning repeated sids *)
  Listext.List.setify
    (all_auth_user_sids_in_sessions @ all_subject_list_sids_in_sessions)

let logout_subject_identifier ~__context ~subject_identifier =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let all_sessions = Db.Session.get_all ~__context in
  let current_session = Context.get_session_id __context in
  (* we filter the sessions to be destroyed *)
  (* 1. we never allow local_superuser sessions to be forcibly logged out *)
  let is_not_local_superuser s =
    not (Db.Session.get_is_local_superuser ~__context ~self:s)
  in
  (* 2. we remove the session associated with this function call from the list
        of all sessions to be destroyed *)
  let is_not_current_session s =
    Db.Session.get_uuid ~__context ~self:s
    <> Db.Session.get_uuid ~__context ~self:current_session
  in
  (* 3. we only consider those sessions associated with the specific subject_id
        received as parameter *)
  let is_associated_with_user_logging_out s =
    (* TODO: better to look up the membership closure *)
    (* 3.1. the sid of the authenticated user or
       3.2. any sids of the group that authenticated the user *)
    Db.Session.get_auth_user_sid ~__context ~self:s = subject_identifier
    || get_group_subject_identifier_from_session ~__context ~session:s
       = subject_identifier
  in
  let sessions =
    List.filter
      (fun s ->
        is_not_local_superuser s
        && is_not_current_session s
        && is_associated_with_user_logging_out s
      )
      all_sessions
  in
  debug
    "This session %s (user=%s subject_identifier=%s) is forcing the logout of \
     these other sessions associated with subject_identifier=%s: trackids=[%s]"
    (trackid current_session)
    ( if Db.Session.get_is_local_superuser ~__context ~self:current_session then
        local_superuser
      else
        ""
    )
    (Db.Session.get_auth_user_sid ~__context ~self:current_session)
    subject_identifier
    (List.fold_right (fun s str -> trackid s ^ "," ^ str) sessions "") ;
  (* kill all filtered sessions *)
  List.iter (fun s -> destroy_db_session ~__context ~self:s) sessions

(* returns the ancestry chain of session s, starting with s *)
let rec get_ancestry ~__context ~self =
  if self = Ref.null then
    [] (* top of session tree *)
  else
    let parent =
      try Db.Session.get_parent ~__context ~self
      with e ->
        debug "error %s getting ancestry for session %s"
          (ExnHelper.string_of_exn e)
          (trackid self) ;
        Ref.null
    in
    self :: get_ancestry ~__context ~self:parent

(* returns the original session up the ancestry chain that created s *)
let get_top ~__context ~self =
  let ancestry = get_ancestry ~__context ~self in
  match ancestry with
  | [] ->
      Ref.null
  | ancestry ->
      List.nth ancestry (List.length ancestry - 1)

(* This function should only be called from inside XAPI. *)
let create_readonly_session ~__context ~uname ~db_ref =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  debug "Creating readonly session." ;
  let role =
    List.hd
      (Xapi_role.get_by_name_label ~__context
         ~label:Datamodel_roles.role_read_only
      )
  in
  let rbac_permissions =
    Xapi_role.get_permissions_name_label ~__context ~self:role
  in
  let master = Helpers.get_master ~__context in
  login_no_password_common ~__context ~uname:(Some uname)
    ~originator:xapi_internal_originator ~host:master ~pool:false
    ~is_local_superuser:false ~subject:Ref.null ~auth_user_sid:"readonly-sid"
    ~auth_user_name:uname ~rbac_permissions ~db_ref ~client_certificate:false

(* Create a database reference from a DB dump, and register it with a new readonly session. *)
let create_from_db_file ~__context ~filename =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let db =
    Xapi_database.Db_xml.From.file (Datamodel_schema.of_datamodel ()) filename
    |> Xapi_database.Db_upgrade.generic_database_upgrade
  in
  let db_ref = Some (Xapi_database.Db_ref.in_memory (ref (ref db))) in
  create_readonly_session ~__context ~uname:"db-from-file" ~db_ref
