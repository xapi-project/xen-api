(* (C) 2006-2010 Citrix Systems Inc.
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

open Features
module D = Debug.Make(struct let name="pool_features" end)
open D

(*
	Terminology:
	- (Feature) flags: The keys in pool.restriction and host.license_params. Strings like "restrict_dmc".
	- Params: An instance of host.license_params.
	- Restrictions: A (string * string) list of feature flag to a Boolean string value ("true" or "false").
	- Features: Values of type Features.feature.
	- Core: Relating to features known by xapi, as define in features.ml.
	- Additional: Relating to features provided by v6d beyond the core ones.
*)

let all_flags = List.map (fun (k, v) -> k) (to_assoc_list all_features)

let get_pool_features ~__context =
  let pool = Helpers.get_pool ~__context in
  of_assoc_list (Db.Pool.get_restrictions ~__context ~self:pool)

let is_enabled ~__context f =
  let pool_features = get_pool_features ~__context in
  List.mem f pool_features

let assert_enabled ~__context ~f =
  if not (is_enabled ~__context f) then
    raise (Api_errors.Server_error(Api_errors.license_restriction, [name_of_feature f]))

(* The set of core restrictions of a pool is the intersection of the sets of features
   of the individual hosts. *)
let compute_core_features all_host_params =
  List.map of_assoc_list all_host_params
  |> List.fold_left Stdext.Listext.List.intersect all_features

(* Find the feature flags in the given license params that are not represented
   in the feature type. These are additional flags given to us by v6d.
   Assume that their names always start with "restrict_". *)
let find_additional_flags params =
  let kvs = List.filter (fun (k, v) ->
      try String.sub k 0 9 = "restrict_" && not (List.mem k all_flags)
      with Invalid_argument _ -> false
    ) params in
  List.map fst kvs

(* Determine the set of additional features. For each restrict_ flag,
   looks for matching flags on all hosts; if one of them is restricted ("true")
   or absent, then the feature on the pool level is marked as restricted. *)
let rec compute_additional_restrictions all_host_params = function
  | [] -> []
  | flag :: rest ->
    let switches =
      List.map
        (function params ->
           if List.mem_assoc flag params
           then bool_of_string (List.assoc flag params)
           else true)
        all_host_params
    in
    (flag, string_of_bool (List.fold_left (||) false switches)) ::
    compute_additional_restrictions all_host_params rest

(* Combine the host-level feature restrictions into pool-level ones, and write
   the result to the database. *)
let update_pool_features ~__context =
  (* Get information from the database *)
  let pool = Helpers.get_pool ~__context in
  let old_restrictions = Db.Pool.get_restrictions ~__context ~self:pool in
  let all_host_params = List.map
      (fun (_, host_r) -> host_r.API.host_license_params)
      (Db.Host.get_all_records ~__context) in
  let master_params =
    let master_ref = Db.Pool.get_master ~__context ~self:pool in
    Db.Host.get_license_params ~__context ~self:master_ref
  in

  (* Determine the set of core restrictions *)
  let new_core_features = compute_core_features all_host_params in
  let new_core_restrictions = to_assoc_list new_core_features in

  (* Determine the set of additional restrictions *)
  let additional_flags = find_additional_flags master_params in
  let new_additional_restrictions = compute_additional_restrictions all_host_params additional_flags in

  (* The complete set of restrictions is formed by the core feature plus the additional features *)
  let new_restrictions = new_additional_restrictions @ new_core_restrictions in

  (* Update the DB if the restrictions have changed *)
  if new_restrictions <> old_restrictions then begin
    let old_core_features = of_assoc_list old_restrictions in
    info "Old pool features enabled: %s" (to_compact_string old_core_features);
    info "New pool features enabled: %s" (to_compact_string new_core_features);
    Db.Pool.set_restrictions ~__context ~self:pool ~value:new_restrictions;
    Xapi_pool_helpers.apply_guest_agent_config ~__context
  end

