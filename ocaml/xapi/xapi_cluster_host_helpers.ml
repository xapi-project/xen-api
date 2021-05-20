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

let all_cluster_host_operations = [`enable; `disable]

(** check if [op] can be done while [current_ops] are already in progress.*)
let is_allowed_concurrently ~op ~current_ops =
  (* for now, disallow all concurrent operations *)
  false

let report_concurrent_operations_error ~current_ops ~ref_str =
  let current_ops_str =
    let op_to_str = Record_util.cluster_host_operation_to_string in
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
    , ["Cluster_host." ^ current_ops_str; ref_str]
    )

(** Take an internal Cluster_host record and a proposed operation. Return None iff the operation
    would be acceptable; otherwise Some (Api_errors.<something>, [list of strings])
    corresponding to the first error found. Checking stops at the first error. *)
let get_operation_error ~__context ~self ~op =
  let chr = Db.Cluster_host.get_record_internal ~__context ~self in
  let ref_str = Ref.string_of self in
  let current_error = None in
  let check c f = match c with Some e -> Some e | None -> f () in
  let assert_joined_cluster ~__context ~self = function
    | (`enable | `disable) when not (Db.Cluster_host.get_joined ~__context ~self)
      ->
        (* Cannot enable nor disable without joining cluster *)
        Some (Api_errors.cluster_host_not_joined, [ref_str])
    | _ ->
        None
  in
  (* if other operations are in progress, check that the new operation is allowed concurrently with them *)
  let current_error =
    check current_error (fun () ->
        let current_ops = chr.Db_actions.cluster_host_current_operations in
        match current_ops with
        | _ :: _ when not (is_allowed_concurrently ~op ~current_ops) ->
            report_concurrent_operations_error ~current_ops ~ref_str
        | _ ->
            check (assert_joined_cluster ~__context ~self op) (fun () -> None)
        (* replace this function if adding new checks *)
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
  let allowed = List.fold_left check [] all_cluster_host_operations in
  Db.Cluster_host.set_allowed_operations ~__context ~self ~value:allowed

(** Add to the cluster host's current_operations, call a function and then remove from the
    current operations. Ensure allowed_operations is kept up to date throughout. *)
let with_cluster_host_operation ~__context ~(self : [`Cluster_host] API.Ref.t)
    ~doc ~op ?policy f =
  let task_id = Ref.string_of (Context.get_task_id __context) in
  Helpers.retry_with_global_lock ~__context ~doc ?policy (fun () ->
      assert_operation_valid ~__context ~self ~op ;
      Db.Cluster_host.add_to_current_operations ~__context ~self ~key:task_id
        ~value:op ;
      update_allowed_operations ~__context ~self
  ) ;
  (* Then do the action with the lock released *)
  finally f (* Make sure to clean up at the end *) (fun () ->
      try
        Db.Cluster_host.remove_from_current_operations ~__context ~self
          ~key:task_id ;
        update_allowed_operations ~__context ~self ;
        Helpers.Early_wakeup.broadcast
          (Datamodel_common._cluster_host, Ref.string_of self)
      with _ -> ()
  )
