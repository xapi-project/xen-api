(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

let string_of_vdi_type vdi_type =
  Rpc.string_of_rpc (API.rpc_of_vdi_type vdi_type)

let vdi_type_of_string str =
  API.vdi_type_of_rpc (Rpc.String str)

module D=Debug.Make(struct let name="storage_utils" end)
open D

(** [redirectable_rpc ~srcstr ~dststr ~remote_url_of_ip ~local_fn call] is an RPC function
    that first attempts to call [local_fn call], and if that returns a [Redirect ip] exception then
    it sends a remote RPC to the url built by [remote_url_of_ip].
*)
let redirectable_rpc ~srcstr ~dststr ~remote_url_of_ip ~local_fn =
  let rec rpc ~f call =
    (* on first iteration this will be the [local_fn] supplied by the caller *)
    let result = f call in
    if result.Rpc.success then result
    else begin
      let rpcstr = Rpc.string_of_call call in
      debug "Got failure: checking for redirect, call was: %s, results.contents: %s"
        rpcstr (Jsonrpc.to_string result.Rpc.contents);
      match Rpcmarshal.unmarshal Storage_interface.Errors.error.Rpc.Types.ty result.Rpc.contents with
      | Ok Storage_interface.Errors.Redirect (Some ip) ->
        let newurl = remote_url_of_ip ip in
        debug "Redirecting %s to ip: %s" rpcstr ip;
        (* we need to do a remote call now, so replace [f] *)
        let f = Helpers.make_remote_rpc_of_url ~srcstr ~dststr newurl in
        let r = rpc ~f call in
        debug "Successfully redirected %s. Returning" rpcstr;
        r
      | _ ->
        debug "Not a redirect: %s" rpcstr;
        result
    end
  in rpc ~f:local_fn

let remote_url ip = Http.Url.(Http { host=ip; auth=None; port=None; ssl=true }, { uri = Constants.sm_uri; query_params=["pool_secret",!Xapi_globs.pool_secret] } )
