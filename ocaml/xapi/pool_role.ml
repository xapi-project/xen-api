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
(**
 * @group Pool Management
 *)

open Threadext

include Pool_role_shared

let set_role r =
  let old_role = get_role () in
  Mutex.execute role_m
    (fun () ->
       Unixext.write_string_to_file Constants.pool_config_file (string_of r));
  Localdb.put Constants.this_node_just_became_master (string_of_bool (old_role <> Master && r = Master))
