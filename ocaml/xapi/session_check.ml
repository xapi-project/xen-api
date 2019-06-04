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
(* Session checking **********************************************************)

open Stdext.Pervasiveext
exception Non_master_login_on_slave

module D=Debug.Make(struct let name="session_check" end)
open D

(* Allows us to hook in an optional "local session" predicate *)
let check_local_session_hook = ref None

let is_local_session __context session_id = default false
    (may (fun f -> f ~__context ~session_id) !check_local_session_hook)

(* intra_pool_only is true iff the call that's invoking this check can only be called from host<->host intra-pool communication *)
let check ~intra_pool_only ~session_id =
  Server_helpers.exec_with_new_task ~quiet:true "session_check"
    (fun __context ->
       (* First see if this is a "local" session *)
       if is_local_session __context session_id
       then () (* debug "Session is in the local database" *)
       else
         (* Assuming we're in master mode *)
         (try
            let pool = Db_actions.DB_Action.Session.get_pool ~__context ~self:session_id in

            (* If the session is not a pool login, but this call is only supported for pool logins then fail *)
            if (not pool) && intra_pool_only then
              raise (Api_errors.Server_error (Api_errors.internal_error,["Internal API call attempted with non-pool (external) session"]));

            (* If the session isn't a pool login, and we're a slave, fail *)
            if (not pool) && (not (Pool_role.is_master ())) then raise Non_master_login_on_slave;

            if (Pool_role.is_master ()) then
              Db_actions.DB_Action.Session.set_last_active ~__context ~self:session_id
                ~value:(Stdext.Date.of_float (Unix.time()))
          with
          | Db_exn.DBCache_NotFound (_, tblname, reference) ->
            debug "Session check failed: missing reference; tbl = %s, ref = %s" tblname reference;
            raise (Api_errors.Server_error (Api_errors.session_invalid,[reference]))
          | Non_master_login_on_slave ->
            let master = Db_actions.DB_Action.Pool.get_master ~__context ~self:
                (List.hd (Db_actions.DB_Action.Pool.get_all ~__context)) in
            let address = Db_actions.DB_Action.Host.get_address ~__context ~self:master in
            raise (Api_errors.Server_error (Api_errors.host_is_slave,[address]));
          | Api_errors.Server_error(code, params) as e ->
            debug "Session check failed: unexpected exception %s %s" code (String.concat " " params);
            raise e
          | exn ->
            debug "Session check failed: unexpected exception '%s'" (Printexc.to_string exn);
            raise (Api_errors.Server_error (Api_errors.session_invalid,[Ref.string_of session_id]))
         )
    )
