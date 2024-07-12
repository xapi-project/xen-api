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

(*
	Terminology:
	- (Feature) flags: The keys in pool.restriction and host.license_params. Strings like "restrict_dmc".
	- Params: An instance of host.license_params.
	- Restrictions: A (string * string) list of feature flag to a Boolean string value ("true" or "false").
	- Features: Values of type Features.feature.
*)

let get_pool_features ~__context =
  let pool = Helpers.get_pool ~__context in
  of_assoc_list (Db.Pool.get_restrictions ~__context ~self:pool)

let is_enabled ~__context f =
  let pool_features = get_pool_features ~__context in
  List.mem f pool_features

let assert_enabled ~__context ~f =
  if not (is_enabled ~__context f) then
    raise
      (Api_errors.Server_error
         (Api_errors.license_restriction, [name_of_feature f])
      )
