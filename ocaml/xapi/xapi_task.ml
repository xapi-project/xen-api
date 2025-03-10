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
(** Module that defines API functions for Task objects
 * @group XenAPI functions
*)

module D = Debug.Make (struct let name = "xapi_task" end)

open D

let create ~__context ~label ~description =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  (* This call will have a dummy task ID already but we need to make a fresh one *)
  let subtask_of = Context.get_task_id __context in
  let session_id =
    try Some (Context.get_session_id __context) with _ -> None
  in
  let c =
    Context.make ?session_id ~task_description:description ~subtask_of
      ~task_in_database:true label
  in
  let t = Context.get_task_id c in
  (*info "Task.create ref = %s; label = %s" (Ref.string_of t) label;*)
  t

let destroy ~__context ~self =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  TaskHelper.assert_op_valid ~__context self ;
  if TaskHelper.status_is_completed (Db.Task.get_status ~__context ~self) then
    Db.Task.destroy ~__context ~self
  else
    Db.Task.add_to_current_operations ~__context ~self ~key:"task"
      ~value:`destroy

let cancel ~__context ~task =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  let localhost = Helpers.get_localhost ~__context in
  let forwarded_to = Db.Task.get_forwarded_to ~__context ~self:task in
  if Db.is_valid_ref __context forwarded_to && localhost <> forwarded_to then
    failwith
      (Printf.sprintf
         "Task.cancel not forwarded to the correct host (expecting %s but this \
          is %s)"
         (Db.Host.get_hostname ~__context ~self:forwarded_to)
         (Db.Host.get_hostname ~__context ~self:localhost)
      ) ;
  TaskHelper.assert_op_valid ~__context task ;
  Db.Task.set_current_operations ~__context ~self:task
    ~value:[(Ref.string_of (Context.get_task_id __context), `cancel)] ;
  if
    (not (Xapi_xenops.task_cancel ~__context ~self:task))
    && not (Storage_access.task_cancel ~__context ~self:task)
  then
    info "Task.cancel is falling back to polling"

let set_status ~__context ~self ~value =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  TaskHelper.assert_op_valid ~__context self ;
  Db.Task.set_status ~__context ~self ~value

let set_progress ~__context ~self ~value =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  TaskHelper.assert_op_valid ~__context self ;
  Db.Task.set_progress ~__context ~self ~value

let set_result ~__context ~self ~value =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  TaskHelper.assert_op_valid ~__context self ;
  Db.Task.set_result ~__context ~self ~value

let set_error_info ~__context ~self ~value =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  TaskHelper.assert_op_valid ~__context self ;
  Db.Task.set_error_info ~__context ~self ~value

let set_resident_on ~__context ~self ~value =
  Context.with_tracing ~__context __FUNCTION__ @@ fun __context ->
  TaskHelper.assert_op_valid ~__context self ;
  Db.Task.set_resident_on ~__context ~self ~value

(* Simple trie data structure that performs a favoured lookup to
   implement a simple form of wildcard key matching. The trie is not
   pruned during (or after) construction. *)
module MatchTrie = struct
  type 'a node = {arrows: (string, 'a node) Hashtbl.t; mutable value: 'a option}

  let create_node () =
    let arrows = Hashtbl.create 16 in
    let value = None in
    {arrows; value}

  let create = create_node

  let insert root ~key ~value =
    let parts = String.split_on_char '.' key in
    let rec extend focused = function
      | part :: parts ->
          let next =
            match Hashtbl.find_opt focused.arrows part with
            | Some node ->
                node
            | _ ->
                let next = create_node () in
                Hashtbl.replace focused.arrows part next ;
                next
          in
          extend next parts
      | [] ->
          focused
    in
    let final = extend root parts in
    final.value <- Some value

  let find root ~key =
    let parts = String.split_on_char '.' key in
    let rec find focused = function
      | part :: parts -> (
        (* Wildcard edges override other edges. *)
        match Hashtbl.find_opt focused.arrows "*" with
        | Some _ as sink ->
            sink
        | _ -> (
          match Hashtbl.find_opt focused.arrows part with
          | Some next ->
              (find [@tailcall]) next parts
          | _ ->
              None
        )
      )
      | _ ->
          Some focused
    in
    match find root parts with Some node -> node.value | _ -> None
end

(* Given an input key, compare against the protected keys of the
   task.other_config field. If a protected key matches, return it.

   For example, if the datamodel specifies "foo.bar.*" as a protected
   key, then: match_protected_key ~key:"foo.bar.baz" = Some "foo.bar.*".

   It must return the protected key as that is what key-related RBAC
   entries are defined in terms of.
*)
let match_protected_key =
  (* Attain the listing of protected keys from the datamodel at module
     initialisation. Usually, this list is passed to Rbac.check by
     handlers inside the auto-generated server.ml file. *)
  let protected_keys =
    let api = Datamodel.all_api in
    let field =
      Dm_api.get_field_by_name api ~objname:"task" ~fieldname:"other_config"
    in
    List.map fst field.field_map_keys_roles
  in
  (* Define the lookup function in terms of a simple trie data
     structure - which is flexible to account for overlapping paths and
     presence of wildcards. *)
  let trie =
    let root = MatchTrie.create () in
    let add key = MatchTrie.insert root ~key ~value:key in
    List.iter add protected_keys ;
    root
  in
  MatchTrie.find trie

let assert_can_modify_other_config ~__context ~task =
  TaskHelper.assert_op_valid ~__context task

let add_to_other_config ~__context ~self ~key ~value =
  assert_can_modify_other_config ~__context ~task:self ;
  Db.Task.add_to_other_config ~__context ~self ~key ~value

let remove_from_other_config ~__context ~self ~key =
  assert_can_modify_other_config ~__context ~task:self ;
  Db.Task.remove_from_other_config ~__context ~self ~key

(* The behaviour of this function, with respect to RBAC checking, must
   match serial "remove_from" and "add_to" operations (for only the keys
   that are changing).

   There is normally no key-related RBAC checking for
   "set_other_config" because the required writer role for the entire
   field is usually higher than the role(s) required for
   individually-protected keys.

   Task's "set_other_config" is a special case where read-only
   sessions must be able to manipulate a subset of entries (those not
   protected by a more privileged role), along with this capability
   being restricted to only the task objects that they created.
*)
let set_other_config ~__context ~self ~value =
  let module S = Set.Make (String) in
  assert_can_modify_other_config ~__context ~task:self ;
  let create_lookup kvs =
    let table = List.to_seq kvs |> Hashtbl.of_seq in
    Hashtbl.find_opt table
  in
  let old_value = Db.Task.get_other_config ~__context ~self in
  let lookup_old, lookup_new = (create_lookup old_value, create_lookup value) in
  let keys_before, keys_after =
    let keys = List.map fst in
    let before = keys old_value in
    let after = keys value in
    S.(of_list before, of_list after)
  in
  let keys_removed =
    (* Keys no longer appearing in the map. The user must have the
       "remove_from" role for each of the protected keys in the set. *)
    S.diff keys_before keys_after
  in
  let keys_unchanged =
    (* Keys that persist across the update. If any key in this set is
       protected AND the value mapped to by the key would be changed by
       the update, the session must have the "add_to" role. *)
    let updated = S.inter keys_before keys_after in
    let is_entry_unchanged key =
      let is_same =
        let ( let* ) = Option.bind in
        let* old_value = lookup_old key in
        let* new_value = lookup_new key in
        Some (old_value = new_value)
      in
      Option.value ~default:false is_same
    in
    (* Filter out the unchanged entries, as you don't need any
       extra privileges to maintain an entry that's already there. *)
    S.filter is_entry_unchanged updated
  in
  let keys_added =
    (* Treat all keys as new, unless they're referring to entries that
       are unchanged across the update. *)
    S.diff keys_after keys_unchanged
  in
  let permissions =
    (* Map each of the added and removed keys to protected keys, if
       such a key exists. *)
    let filter keys =
      S.filter_map (fun key -> match_protected_key ~key) keys |> S.elements
    in
    let added = filter keys_added in
    let removed = filter keys_removed in
    let format operation key =
      (* All the permissions are stored in lowercase. *)
      let key = String.lowercase_ascii key in
      Printf.sprintf "task.%s_other_config/key:%s" operation key
    in
    (* The required permissions are defined in terms of those
       generated for "add_to" and "remove_from" (both implemented
       above). They can be defined as custom AND use RBAC checking within
       server.ml because their operation is purely destructive, so it's
       sufficient to guard the entire action with Rbac.check. *)
    let added_perms = List.map (format "add_to") added in
    let removed_perms = List.map (format "remove_from") removed in
    added_perms @ removed_perms
  in
  (* Find the first disallowed permission, indicating that we cannot
     perform the action. *)
  let session_id = Context.get_session_id __context in
  match
    Rbac.find_first_disallowed_permission ~__context ~session_id ~permissions
  with
  | None ->
      (* No disallowed permission, perform the update. *)
      Db.Task.set_other_config ~__context ~self ~value
  | Some disallowed ->
      (* Report it as an RBAC error. *)
      let action = "task.set_other_config" in
      let extra_msg = "" in
      let extra_dmsg = "" in
      raise
        (Rbac.disallowed_permission_exn ~extra_dmsg ~extra_msg ~__context
           ~permission:disallowed ~action
        )
