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

open Storage_interface

let s_of_sr = Storage_interface.Sr.string_of

let s_of_vdi = Storage_interface.Vdi.string_of

let string_of_vdi_type vdi_type =
  Rpc.string_of_rpc (API.rpc_of_vdi_type vdi_type)

let vdi_type_of_string str = API.vdi_type_of_rpc (Rpc.String str)

module D = Debug.Make (struct let name = "storage_utils" end)

open D

(** [redirectable_rpc ~redirect_to_ip ~original call] is an RPC function
    that first attempts to call [original call], and if that returns a [Redirect ip] exception then
    it sends a remote RPC to the function built by [redirect_to_ip].
*)
let redirectable_rpc ~redirect_to_ip ~original : Rpc.call -> Rpc.response =
  let rec rpc ~f call =
    (* on first iteration this will be the [original] supplied by the caller *)
    let result = f call in
    if result.Rpc.success then
      result
    else
      let rpcstr = Rpc.string_of_call call in
      debug
        "Got failure: checking for redirect, call was: %s, results.contents: %s"
        rpcstr
        (Jsonrpc.to_string result.Rpc.contents) ;
      match
        Rpcmarshal.unmarshal Storage_interface.Errors.error.Rpc.Types.ty
          result.Rpc.contents
      with
      | Ok (Storage_interface.Errors.Redirect (Some ip)) ->
          debug "Redirecting %s to ip: %s" rpcstr ip ;
          (* we need to do a remote call now, so replace [f] *)
          let f = redirect_to_ip ~ip in
          let r = rpc ~f call in
          debug "Successfully redirected %s. Returning" rpcstr ;
          r
      | _ ->
          debug "Not a redirect: %s" rpcstr ;
          result
  in
  rpc ~f:original

(* A type that encodes how to connect to a local or remote host *)
type connection_args = {
    url: Http.Url.t
  ; pool_secret: SecretString.t option
  ; verify_cert: Stunnel.verification_config option
}

(* HTTP, standard SM uri, to localhost, with pool secret *)
let localhost_connection_args () : connection_args =
  let url =
    Http.Url.
      ( Http {host= "127.0.0.1"; auth= None; port= None; ssl= false}
      , {path= Constants.sm_uri; query_params= []}
      )
  in

  {url; pool_secret= Some (Xapi_globs.pool_secret ()); verify_cert= None}

(* HTTPS (certificate checked), standard SM uri, to host in the pool with the
   given IP, with pool secret. *)
let intra_pool_connection_args_of_ip ip : connection_args =
  let url =
    Http.Url.
      ( Http
          {
            host= ip
          ; auth= None
          ; port= None
          ; ssl= !Xapi_globs.migration_https_only
          }
      , {path= Constants.sm_uri; query_params= []}
      )
  in

  {
    url
  ; pool_secret= Some (Xapi_globs.pool_secret ())
  ; verify_cert= Stunnel_client.pool ()
  }

(* Uses session_id in HTTP query (no pool secret). *)
(* Certificate verification for intra-pool remotes only. *)
let connection_args_of_uri ~verify_dest uri : connection_args =
  let verify_cert = if verify_dest then Stunnel_client.pool () else None in
  let url = Http.Url.of_string uri in
  {url; pool_secret= None; verify_cert}

let intra_pool_rpc_of_ip ~srcstr ~dststr ~ip : Rpc.call -> Rpc.response =
  let {url; pool_secret; verify_cert} = intra_pool_connection_args_of_ip ip in
  Helpers.make_remote_rpc_of_url ~verify_cert ~srcstr ~dststr (url, pool_secret)

(* Create an rpc function based on the given connection args (see above).
   We are making an RPC to a host that is potentially part of a different pool.
   It can tell us that we need to send the RPC elsewhere instead (e.g. to its master),
   so use the [redirectable_rpc] helper here *)
let rpc ~srcstr ~dststr {url; pool_secret; verify_cert} =
  let original =
    Helpers.make_remote_rpc_of_url ~verify_cert ~srcstr ~dststr
      (url, pool_secret)
  in
  let redirect_to_ip ~ip =
    let open Http.Url in
    match url with
    | Http h, d ->
        Helpers.make_remote_rpc_of_url ~srcstr ~dststr ~verify_cert
          ( (Http {h with host= ip; ssl= !Xapi_globs.migration_https_only}, d)
          , pool_secret
          )
    | _ ->
        (* The original URL referred to a local file and must be changed to HTTP *)
        intra_pool_rpc_of_ip ~srcstr ~dststr ~ip
  in
  redirectable_rpc ~original ~redirect_to_ip

let transform_storage_exn f =
  let get_sr_ref sr_uuid =
    Server_helpers.exec_with_new_task "transform_storage_exn" (fun __context ->
        Db.SR.get_by_uuid ~__context ~uuid:sr_uuid
    )
  in
  try f () with
  | Storage_error (Backend_error (code, params)) as e ->
      Backtrace.reraise e (Api_errors.Server_error (code, params))
  | Storage_error (Backend_error_with_backtrace (code, backtrace :: params)) as
    e ->
      let backtrace = Backtrace.Interop.of_json "SM" backtrace in
      Backtrace.add e backtrace ;
      Backtrace.reraise e (Api_errors.Server_error (code, params))
  | Storage_error (Sr_unhealthy (sr, health)) as e ->
      let advice =
        match health with
        | Unavailable ->
            "try reboot"
        | Unreachable ->
            "try again later"
        | _health ->
            ""
      in
      let sr = get_sr_ref sr in
      Backtrace.reraise e
        (Api_errors.Server_error
           ( Api_errors.sr_unhealthy
           , [Ref.string_of sr; Storage_interface.show_sr_health health; advice]
           )
        )
  | Api_errors.Server_error _ as e ->
      raise e
  | Storage_error (No_storage_plugin_for_sr sr) as e ->
      let sr = get_sr_ref sr in
      Backtrace.reraise e
        (Api_errors.Server_error (Api_errors.sr_not_attached, [Ref.string_of sr])
        )
  | e ->
      Backtrace.reraise e
        (Api_errors.Server_error
           (Api_errors.internal_error, [Printexc.to_string e])
        )

exception No_VDI

let find_vdi ~__context sr vdi =
  let sr = s_of_sr sr in
  let vdi = s_of_vdi vdi in
  let open Xapi_database.Db_filter_types in
  let sr = Db.SR.get_by_uuid ~__context ~uuid:sr in
  match
    Db.VDI.get_records_where ~__context
      ~expr:
        (And
           ( Eq (Field "location", Literal vdi)
           , Eq (Field "SR", Literal (Ref.string_of sr))
           )
        )
  with
  | x :: _ ->
      x
  | _ ->
      raise No_VDI
