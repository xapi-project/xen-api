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

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let all_cluster_operations = [`add; `remove; `enable; `disable; `destroy]

(** check if [op] can be done while [current_ops] are already in progress.*)
let is_allowed_concurrently ~op ~current_ops =
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

module Pem = struct
  open Helpers
  open Cluster_interface
  module Client = Client.Client

  let init' cn = {cn; blobs= [Gencertlib.Selfcert.xapi_cluster ~cn]}

  let init ~__context ~cn =
    if unit_test ~__context then
      None
    else
      Some (init' cn)

  let get_existing' ~__context self =
    let cc_of_cluster_host h =
      call_api_functions ~__context @@ fun rpc session_id ->
      Client.Cluster_host.get_cluster_config rpc session_id h
      |> SecretString.json_rpc_of_t
      |> Rpcmarshal.unmarshal cluster_config.Rpc.Types.ty
      |> function
      | Ok x ->
          x
      | Error e ->
          raise
            Api_errors.(
              Server_error (internal_error, ["bad response from cluster host"])
            )
    in
    if unit_test ~__context then
      `unittest
    else
      let hs = Db.Cluster.get_cluster_hosts ~__context ~self in
      let enabled_hs =
        List.filter
          (fun self -> Db.Cluster_host.get_enabled ~__context ~self)
          hs
      in
      match (hs, enabled_hs) with
      | [], _ ->
          `no_cluster_hosts
      | _, [] ->
          `all_disabled
      | _, h :: _ ->
          if enabled_hs = hs then
            `all_enabled (h, cc_of_cluster_host h)
          else
            `not_all_enabled (h, cc_of_cluster_host h)

  let get_existing ~__context self =
    (* try to get existing pem, though this might not be possible for example if
     * every cluster host has been disabled
     *
     * create a new pem if we are the first cluster host *)
    let gen () =
      D.debug "Pem.get_existing: generating new" ;
      let cn = Db.Cluster.get_uuid ~__context ~self in
      Some (init' cn)
    in
    match get_existing' ~__context self with
    | `unittest ->
        None
    | `all_disabled ->
        D.debug "Pem.get_existing: all existing cluster hosts disabled" ;
        gen ()
    | `no_cluster_hosts ->
        D.debug "Pem.get_existing: there are no existing cluster hosts" ;
        gen ()
    | `all_enabled (_, cc) | `not_all_enabled (_, cc) -> (
      match cc.pems with
      | None ->
          (* this is not a problem unless tls verification is enabled! *)
          D.debug "Pem.get_existing: existing cluster does not have a pem" ;
          None
      | Some p ->
          D.debug "Pem.get_existing: found existing pem!" ;
          Some p
    )
end
