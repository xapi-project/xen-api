(*
 * Copyright (C) Citrix Systems Inc.
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

module D = Debug.Make (struct let name = __MODULE__ end)

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let all_cluster_operations = [`add; `remove; `enable; `disable; `destroy]

(** check if [op] can be done while [current_ops] are already in progress.*)
let is_allowed_concurrently ~op:_ ~current_ops:_ =
  (* for now, disallow all concurrent operations *)
  false

let report_concurrent_operations_error ~current_ops ~ref_str =
  let current_ops_str =
    let op_to_str = Record_util.cluster_operation_to_string in
    match current_ops with
    | [] ->
        failwith "No concurrent operation to report"
    | [(_, cop)] ->
        op_to_str cop
    | l ->
        "{" ^ String.concat "," (List.map op_to_str (List.map snd l)) ^ "}"
  in
  Some
    ( Api_errors.other_operation_in_progress
    , ["Cluster." ^ current_ops_str; ref_str]
    )

(** Take an internal Cluster record and a proposed operation. Return None iff the operation
    would be acceptable; otherwise Some (Api_errors.<something>, [list of strings])
    corresponding to the first error found. Checking stops at the first error. *)
let get_operation_error ~__context ~self ~op =
  let cr = Db.Cluster.get_record_internal ~__context ~self in
  let ref_str = Ref.string_of self in
  let current_error = None in
  let check c f = match c with Some e -> Some e | None -> f () in
  let assert_allowed_during_rpu __context = function
    | (`add | `remove | `destroy)
      when Helpers.rolling_upgrade_in_progress ~__context ->
        Some (Api_errors.not_supported_during_upgrade, [])
    | _ ->
        None
  in
  (* if other operations are in progress, check that the new operation is allowed concurrently with them *)
  let current_error =
    check current_error (fun () ->
        let current_ops = cr.Db_actions.cluster_current_operations in
        match current_ops with
        | _ :: _ when not (is_allowed_concurrently ~op ~current_ops) ->
            report_concurrent_operations_error ~current_ops ~ref_str
        | _ ->
            check (assert_allowed_during_rpu __context op) (fun () -> None)
    )
  in
  current_error

let assert_operation_valid ~__context ~self ~op =
  match get_operation_error ~__context ~self ~op with
  | None ->
      ()
  | Some (a, b) ->
      raise (Api_errors.Server_error (a, b))

let update_allowed_operations ~__context ~self =
  let check accu op =
    match get_operation_error ~__context ~self ~op with
    | None ->
        op :: accu
    | _ ->
        accu
  in
  let allowed = List.fold_left check [] all_cluster_operations in
  Db.Cluster.set_allowed_operations ~__context ~self ~value:allowed

(** Add to the cluster's current_operations, call a function and then remove from the
    current operations. Ensure allowed_operations is kept up to date throughout. *)
let with_cluster_operation ~__context ~(self : [`Cluster] API.Ref.t) ~doc ~op
    ?policy f =
  let task_id = Ref.string_of (Context.get_task_id __context) in
  Helpers.retry_with_global_lock ~__context ~doc ?policy (fun () ->
      assert_operation_valid ~__context ~self ~op ;
      Db.Cluster.add_to_current_operations ~__context ~self ~key:task_id
        ~value:op ;
      update_allowed_operations ~__context ~self
  ) ;
  (* Then do the action with the lock released *)
  finally f (* Make sure to clean up at the end *) (fun () ->
      try
        Db.Cluster.remove_from_current_operations ~__context ~self ~key:task_id ;
        update_allowed_operations ~__context ~self ;
        Helpers.Early_wakeup.broadcast
          (Datamodel_common._cluster, Ref.string_of self)
      with _ -> ()
  )

let cluster_address_enabled ~__context =
  let r = Pool_features.is_enabled ~__context Features.Cluster_address in
  D.debug "%s extended cluster address is %s" __FUNCTION__
    (if r then "enabled" else "disabled") ;
  r

let corosync3_enabled ~__context =
  let pool = Helpers.get_pool ~__context in
  let restrictions = Db.Pool.get_restrictions ~__context ~self:pool in
  List.assoc_opt "restrict_corosync3" restrictions = Some "false"

let maybe_generate_alert ~__context ~num_hosts ~hosts_left ~hosts_joined ~quorum
    =
  let generate_alert join cluster_host =
    let generate_alert_body host num_hosts quorum join =
      let num_hosts = string_of_int num_hosts in
      let quorum = string_of_int quorum in
      let msg =
        if join then
          "<message>Host has joined the cluster</message>"
        else
          "<message>Host has left the cluster</message>"
      in
      String.concat ""
        [
          "<body>"
        ; msg
        ; "<host>"
        ; host
        ; "</host>"
        ; "<total_hosts>"
        ; num_hosts
        ; "</total_hosts>"
        ; "<quorum>"
        ; quorum
        ; "</quorum>"
        ; "</body>"
        ]
    in
    let host = Db.Cluster_host.get_host ~__context ~self:cluster_host in
    let host_uuid = Db.Host.get_uuid ~__context ~self:host in
    let host_name = Db.Host.get_name_label ~__context ~self:host in
    let body, name, priority =
      let body = generate_alert_body host_name num_hosts quorum join in
      match join with
      | true ->
          let name, priority = Api_messages.cluster_host_joining in
          (body, name, priority)
      | false ->
          let name, priority = Api_messages.cluster_host_leaving in
          (body, name, priority)
    in
    Xapi_alert.add ~msg:(name, priority) ~cls:`Host ~obj_uuid:host_uuid ~body
  in
  List.iter (generate_alert false) hosts_left ;
  List.iter (generate_alert true) hosts_joined ;
  (* only generate this alert when the number of hosts is decreasing *)
  if hosts_left <> [] && num_hosts <= quorum then
    let pool = Helpers.get_pool ~__context in
    let pool_uuid = Db.Pool.get_uuid ~__context ~self:pool in
    let name, priority = Api_messages.cluster_quorum_approaching_lost in
    let body =
      String.concat ""
        [
          "<body>"
        ; "<message>Cluster is losing quorum</message>"
        ; "<total_hosts>"
        ; string_of_int num_hosts
        ; "</total_hosts>"
        ; "<quorum>"
        ; string_of_int quorum
        ; "</quorum>"
        ; "</body>"
        ]
    in
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        ignore
        @@ Client.Client.Message.create ~rpc ~session_id ~name ~priority
             ~cls:`Pool ~obj_uuid:pool_uuid ~body
    )
