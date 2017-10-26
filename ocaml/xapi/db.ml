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
 * @group Database Operations
*)

include Db_actions.DB_Action
let is_valid_ref __context r =
  if r = Ref.null then false
  else
    let t = Context.database_of __context in
    let module DB = (val (Db_cache.get t) : Db_interface.DB_ACCESS) in
    DB.is_valid_ref t (Ref.string_of r)
