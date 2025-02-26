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

module D = Debug.Make (struct let name = "xapi_pool_helpers" end)

open D
open Client
open Record_util

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

type blocking_operations =
  [ `apply_updates
  | `cluster_create
  | `configure_repositories
  | `designate_new_master
  | `ha_disable
  | `ha_enable
  | `sync_bundle
  | `sync_updates
  | `tls_verification_enable ]

type waiting_operations =
  [ `cert_refresh
  | `copy_primary_host_certs
  | `eject
  | `exchange_ca_certificates_on_join
  | `exchange_certificates_on_join
  | `get_updates ]

type all_operations = [blocking_operations | waiting_operations]

(* Unused, ensure every API operation is statically partitioned here. *)
let _id (op : API.pool_allowed_operations) : all_operations = op

(* psr is not included as a pool op because it can be considered in progress
   in between api calls (i.e. wrapping it inside with_pool_operation won't work) *)

(* these ops will:
 * a) throw an error if any other blocked op is in progress
 * b) wait if only a wait op is in progress
 *)
let blocking_ops_table : (blocking_operations * string) list =
  [
    (`ha_enable, Api_errors.ha_enable_in_progress)
  ; (`ha_disable, Api_errors.ha_disable_in_progress)
  ; (`cluster_create, Api_errors.cluster_create_in_progress)
  ; (`designate_new_master, Api_errors.designate_new_master_in_progress)
  ; (`tls_verification_enable, Api_errors.tls_verification_enable_in_progress)
  ; (`configure_repositories, Api_errors.configure_repositories_in_progress)
  ; (`sync_updates, Api_errors.sync_updates_in_progress)
  ; (`sync_bundle, Api_errors.sync_bundle_in_progress)
  ; (`apply_updates, Api_errors.apply_updates_in_progress)
  ]

(* generally these ops will happen internally. example: rather than blocking
 * `ha_enable if a `copy_primary_host_certs is in progress, we should wait.
 *
 * waiting is symmetric: if `ha_enable is in progress, and we want to perform
 * `copy_primary_host_certs, then we wait in this case too *)
let waiting_ops : waiting_operations list =
  [
    `cert_refresh
  ; `exchange_certificates_on_join
  ; `exchange_ca_certificates_on_join
  ; `copy_primary_host_certs
  ; `eject
  ; `get_updates
  ]

(* Shadow with widening coercions to allow us to query using
   operations from either set, whilst maintaining the static guarantees
   of the original listings. *)
let blocking_ops_table : (all_operations * string) list =
  List.map (fun (op, v) -> ((op :> all_operations), v)) blocking_ops_table

let blocking_ops : all_operations list = List.map fst blocking_ops_table

let waiting_ops = List.map (fun op -> (op :> all_operations)) waiting_ops

let all_operations : all_operations list = blocking_ops @ waiting_ops

type validity = Unknown | Allowed | Disallowed of string * string list

(* Computes a function (all_operations -> validity) that maps each
   element of all_operations to a value indicating whether it would be
   valid for it to be executed in the inputted execution context. *)
let compute_valid_operations ~__context record pool :
    API.pool_allowed_operations -> validity =
  let ref = Ref.string_of pool in
  let current_ops = List.map snd record.Db_actions.pool_current_operations in
  let table = (Hashtbl.create 32 : (all_operations, validity) Hashtbl.t) in
  let set_validity = Hashtbl.replace table in
  (* Start by assuming all operations are allowed. *)
  List.iter (fun op -> set_validity op Allowed) all_operations ;
  (* Given a list of operations, map each to the given error. If an
     error has already been specified for a given operation, do
     nothing. *)
  let set_errors ops ((error, detail) : string * string list) =
    let populate op =
      match Hashtbl.find table op with
      | Allowed ->
          set_validity op (Disallowed (error, detail))
      | Disallowed _ | Unknown ->
          (* These cases should be impossible here. *)
          ()
    in
    List.iter populate ops
  in
  let other_operation_in_progress =
    (Api_errors.other_operation_in_progress, [Datamodel_common._pool; ref])
  in
  let is_current_op = Fun.flip List.mem current_ops in
  let blocking =
    List.find_opt (fun (op, _) -> is_current_op op) blocking_ops_table
  in
  let waiting = List.find_opt is_current_op waiting_ops in
  ( match (blocking, waiting) with
  | Some (_, reason), _ ->
      (* Mark all potentially blocking operations as invalid due
         to the specific blocking operation's "in progress" error. *)
      set_errors blocking_ops (reason, []) ;
      (* Mark all waiting operations as invalid for the generic
         "OTHER_OPERATION_IN_PROGRESS" reason. *)
      set_errors waiting_ops other_operation_in_progress
      (* Note that all_operations ⊆ blocking_ops ∪ waiting_ops, so this
         invalidates all operations (with the reason partitioned
         between whether the operation is blocking or waiting). *)
  | None, Some _ ->
      (* If there's no blocking operation in current operations, but
         there is a waiting operation, invalidate all operations for the
         generic reason. Again, this covers every operation. *)
      set_errors all_operations other_operation_in_progress
  | None, None -> (
      (* If there's no blocking or waiting operation in current
         operations (i.e. current operations is empty), we can report
         more precise reasons why operations would be invalid. *)
      let ha_enabled, current_stack =
        let self = Helpers.get_pool ~__context in
        Db.Pool.
          ( get_ha_enabled ~__context ~self
          , get_ha_cluster_stack ~__context ~self
          )
      in
      if ha_enabled then (
        (* Can't enable HA if it's already enabled. *)
        let ha_is_enabled = (Api_errors.ha_is_enabled, []) in
        set_errors [`ha_enable] ha_is_enabled ;
        (* TLS verification is not allowed to run if HA is enabled. *)
        set_errors [`tls_verification_enable] ha_is_enabled
      ) else (* Can't disable HA if it's not enabled. *)
        set_errors [`ha_disable] (Api_errors.ha_not_enabled, []) ;
      (* Cluster create cannot run during a rolling pool upgrade. *)
      if Helpers.rolling_upgrade_in_progress ~__context then (
        let not_supported_during_upgrade =
          (Api_errors.not_supported_during_upgrade, [])
        in
        set_errors [`cluster_create] not_supported_during_upgrade ;
        set_errors [`tls_verification_enable] not_supported_during_upgrade
      ) ;
      (* Cluster create cannot run if a cluster already exists on the pool. *)
      match Db.Cluster.get_all ~__context with
      | [_] ->
          set_errors [`cluster_create] (Api_errors.cluster_already_exists, [])
      (* Indicates a bug or a need to update this code (if we ever support multiple clusters in the pool). *)
      | _ :: _ ->
          failwith "Multiple clusters exist in the pool"
      (* Cluster create cannot run if HA is already enabled. *)
      | [] ->
          if ha_enabled then
            let error =
              (Api_errors.incompatible_cluster_stack_active, [current_stack])
            in
            set_errors [`cluster_create] error
    )
  ) ;
  fun op -> Hashtbl.find_opt table op |> Option.value ~default:Unknown

let assert_operation_valid ~__context ~self ~(op : API.pool_allowed_operations)
    =
  (* No pool operations allowed during a pending PSR. *)
  if Db.Pool.get_is_psr_pending ~__context ~self:(Helpers.get_pool ~__context)
  then
    raise Api_errors.(Server_error (pool_secret_rotation_pending, [])) ;
  let all = Db.Pool.get_record_internal ~__context ~self in
  let lookup = compute_valid_operations ~__context all self in
  match lookup op with
  | Allowed ->
      ()
  | Disallowed (error, detail) ->
      raise (Api_errors.Server_error (error, detail))
  | Unknown ->
      (* This should never happen and implies our validity algorithm is incomplete. *)
      let detail =
        let op = pool_allowed_operations_to_string op in
        Printf.sprintf "%s.%s unknown operation: %s" __MODULE__ __FUNCTION__ op
      in
      raise Api_errors.(Server_error (internal_error, [detail]))

let update_allowed_operations ~__context ~self : unit =
  let all = Db.Pool.get_record_internal ~__context ~self in
  let is_allowed_op =
    let lookup = compute_valid_operations ~__context all self in
    fun op -> lookup op = Allowed
  in
  let value = List.filter is_allowed_op all_operations in
  Db.Pool.set_allowed_operations ~__context ~self ~value

(** Add to the Pool's current operations, call a function and then remove from the
    current operations. Ensure the allowed_operations are kept up to date. *)
let with_pool_operation ~__context ~self ~doc ~op f =
  let task_id = Ref.string_of (Context.get_task_id __context) in
  Helpers.retry_with_global_lock ~__context ~doc (fun () ->
      assert_operation_valid ~__context ~self ~op ;
      Db.Pool.add_to_current_operations ~__context ~self ~key:task_id ~value:op
  ) ;
  update_allowed_operations ~__context ~self ;
  (* Then do the action with the lock released *)
  finally f (* Make sure to clean up at the end *) (fun () ->
      try
        Db.Pool.remove_from_current_operations ~__context ~self ~key:task_id ;
        update_allowed_operations ~__context ~self ;
        Helpers.Early_wakeup.broadcast
          (Datamodel_common._pool, Ref.string_of self)
      with _ -> ()
  )

let is_pool_op_in_progress op ~__context =
  let pool = Helpers.get_pool ~__context in
  let current_ops = Db.Pool.get_current_operations ~__context ~self:pool in
  List.exists (fun (_, op') -> op = op') current_ops

let ha_enable_in_progress = is_pool_op_in_progress `ha_enable

let ha_disable_in_progress = is_pool_op_in_progress `ha_disable

let assert_no_pool_ops ~__context =
  let pool = Helpers.get_pool ~__context in
  match Db.Pool.get_current_operations ~__context ~self:pool with
  | [] ->
      ()
  | ops ->
      let err =
        ops
        |> List.map snd
        |> List.map Record_util.pool_allowed_operations_to_string
        |> String.concat "; "
        |> Printf.sprintf "pool operations in progress: [ %s ]"
      in
      Helpers.internal_error "%s" err

let get_master_slaves_list_with_fn ~__context fn =
  let _unsorted_hosts = Db.Host.get_all ~__context in
  let master = Helpers.get_master ~__context in
  let slaves = List.filter (fun h -> h <> master) _unsorted_hosts in
  (* anything not a master *)
  debug "MASTER=%s, SLAVES=%s"
    (Db.Host.get_name_label ~__context ~self:master)
    (List.fold_left
       (fun str h -> str ^ "," ^ Db.Host.get_name_label ~__context ~self:h)
       "" slaves
    ) ;
  fn master slaves

(* returns the list of hosts in the pool, with the master being the first element of the list *)
let get_master_slaves_list ~__context =
  get_master_slaves_list_with_fn ~__context (fun master slaves ->
      master :: slaves
  )

(* returns the list of slaves in the pool *)
let get_slaves_list ~__context =
  get_master_slaves_list_with_fn ~__context (fun _ slaves -> slaves)

let call_fn_on_hosts ~__context hosts f =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let errs =
        List.filter_map
          (fun host ->
            try f ~rpc ~session_id ~host ; None with x -> Some (host, x)
          )
          hosts
      in
      List.iter
        (fun (host, exn) ->
          warn {|Exception raised while performing operation on host "%s": %s|}
            (Ref.string_of host)
            (ExnHelper.string_of_exn exn)
        )
        errs ;
      match errs with [] -> () | (_, first_exn) :: _ -> raise first_exn
  )

let call_fn_on_master_then_slaves ~__context f =
  let hosts = get_master_slaves_list ~__context in
  call_fn_on_hosts ~__context hosts f

(* Note: fn exposed in .mli *)

(** Call the function on the slaves first. When those calls have all
 *  returned, call the function on the master. *)
let call_fn_on_slaves_then_master ~__context f =
  (* Get list with master as LAST element: important for ssl_legacy calls *)
  let hosts = List.rev (get_master_slaves_list ~__context) in
  call_fn_on_hosts ~__context hosts f

let apply_guest_agent_config ~__context =
  let f ~rpc ~session_id ~host =
    try Client.Host.apply_guest_agent_config ~rpc ~session_id ~host
    with e ->
      error "Failed to apply guest agent config to host %s: %s"
        (Db.Host.get_uuid ~__context ~self:host)
        (Printexc.to_string e)
  in
  call_fn_on_slaves_then_master ~__context f
