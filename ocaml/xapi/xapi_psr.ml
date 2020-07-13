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

(** important invariants:
  * given a list of hosts containing both the master and
    the members, the master will always be at the head *)

module D = Debug.Make (struct let name = "xapi_psr" end)

module Unixext = Xapi_stdext_unix.Unixext
module Client = Client.Client
open Xapi_psr_util

type failure =
  | Failed_during_accept_new_pool_secret
  | Failed_during_send_new_pool_secret
  | Failed_during_cleanup

type 'a r = (unit, failure * 'a) result

let user_facing_error_message ~__context error =
  let failure, self = error in
  let host_name = Db.Host.get_name_label ~__context ~self in
  let try_again_msg = "Please check the management network and try again." in
  match failure with
  | Failed_during_accept_new_pool_secret ->
      Printf.sprintf
        "Operation to tell %s to begin accepting new pool secret failed. %s"
        host_name try_again_msg
  | Failed_during_send_new_pool_secret ->
      Printf.sprintf
        "Operation to tell %s to begin sending new pool secret failed. %s"
        host_name try_again_msg
  | Failed_during_cleanup ->
      Printf.sprintf
        "%s encountered an error whilst cleaning up after a pool secret \
         rotation. %s"
        host_name try_again_msg

module type Impl = sig
  type pool_secret

  type pool_secrets = pool_secret * pool_secret (* (old, new) *)

  type host

  val save_checkpoint : string -> unit

  val retrieve_checkpoint : unit -> string option

  val backup : pool_secrets -> unit

  val retrieve : unit -> pool_secrets

  val tell_accept_new_pool_secret : pool_secrets -> host -> unit

  val tell_send_new_pool_secret : pool_secrets -> host -> unit

  val tell_cleanup_old_pool_secret : host -> unit

  val cleanup_master : unit -> unit
end

module Make =
functor
  (Impl : Impl)
  ->
  struct
    let ( >>= ) = Rresult.( >>= )

    type checkpoint =
      | No_checkpoint (* ==> previous rotation was successful*)
      | Accept_new_pool_secret
      | Send_new_pool_secret
      | Cleanup_members
      | Cleanup_master
    [@@deriving rpcty]

    exception Cannot_parse_checkpoint of string

    let string_of_checkpoint x =
      Rpcmarshal.marshal checkpoint.Rpc.Types.ty x |> Jsonrpc.to_string
      |> (* remove leading and trailing '"' *)
      fun s -> String.sub s 1 (String.length s - 2)

    let checkpoint_of_string s =
      let rpc = Rpc.rpc_of_string s in
      match Rpcmarshal.unmarshal checkpoint.Rpc.Types.ty rpc with
      | Ok x ->
          x
      | Error _ ->
          raise (Cannot_parse_checkpoint s)

    let rec iter_break f = function
      | [] ->
          Ok ()
      | x :: xs ->
          f x >>= fun _ -> (iter_break [@tailcall]) f xs

    let rec go pool_secrets master members = function
      | No_checkpoint ->
          (* if we fail to backup the pool secrets, it doesn't really matter,
             you can simply restart the rotation *)
          Impl.backup pool_secrets ;
          (go [@tailcall]) pool_secrets master members Accept_new_pool_secret
      | Accept_new_pool_secret ->
          Impl.save_checkpoint (string_of_checkpoint Accept_new_pool_secret) ;
          master :: members
          |> iter_break (fun host ->
                 try
                   Impl.tell_accept_new_pool_secret pool_secrets host ;
                   Ok ()
                 with e ->
                   D.error
                     "failed while telling host to accept new pool secret. \
                      error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_accept_new_pool_secret, host))
          >>= fun () ->
          (go [@tailcall]) pool_secrets master members Send_new_pool_secret
      | Send_new_pool_secret ->
          Impl.save_checkpoint (string_of_checkpoint Send_new_pool_secret) ;
          master :: members
          |> iter_break (fun host ->
                 try
                   Impl.tell_send_new_pool_secret pool_secrets host ;
                   Ok ()
                 with e ->
                   D.error
                     "failed while telling hosts to send new pool secret. \
                      error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_send_new_pool_secret, host))
          >>= fun () ->
          (go [@tailcall]) pool_secrets master members Cleanup_members
      | Cleanup_members ->
          Impl.save_checkpoint (string_of_checkpoint Cleanup_members) ;
          members
          |> iter_break (fun member ->
                 try
                   Impl.tell_cleanup_old_pool_secret member ;
                   Ok ()
                 with e ->
                   D.error "failed while telling hosts to cleanup. error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_cleanup, member))
          >>= fun () ->
          (go [@tailcall]) pool_secrets master members Cleanup_master
      | Cleanup_master -> (
          Impl.save_checkpoint (string_of_checkpoint Cleanup_master) ;
          try Impl.cleanup_master () ; Ok ()
          with e ->
            D.error "failed to cleanup the master. error= %s"
              (Printexc.to_string e) ;
            Error (Failed_during_cleanup, master)
        )

    let start pool_secrets ~master ~members =
      try
        match
          Impl.retrieve_checkpoint () |> Option.map checkpoint_of_string
        with
        | None | Some No_checkpoint ->
            go pool_secrets master members No_checkpoint
        | Some checkpoint ->
            go (Impl.retrieve ()) master members checkpoint
      with e ->
        (* _in theory_ save_checkpoint or backup could fail, so
           catch that here. however we don't expect this to happen *)
        D.error "PSR.start: unexpected error: %s" (Printexc.to_string e) ;
        raise
          Api_errors.(
            Server_error (internal_error, ["PSR.start: unexpected error"]))
  end

let perm = 0o640

let cleanup_internal ~additional_files_to_remove () =
  if List.length !Xapi_globs.pool_secrets <= 1 then
    D.info "xapi_psr.ml:cleanup_internal: already cleaned up"
  else
    (* if we don't remove pool secret backups, they will be loaded into memory when xapi starts *)
    let files_to_remove =
      List.append additional_files_to_remove
        [old_pool_secret_backup_path; new_pool_secret_backup_path]
    in
    List.iter
      (fun path ->
        try Sys.remove path
        with e ->
          D.error "cleanup_internal: failed to remove %s. error: %s" path
            (Printexc.to_string e))
      files_to_remove ;
    (* psr done, so stop accepting old pool secret *)
    Xapi_globs.pool_secrets := [Xapi_globs.pool_secret ()]

module Impl =
functor
  (Ctx : sig
     val __context : Context.t
   end)
  ->
  struct
    open Ctx

    type pool_secret = SecretString.t

    type pool_secrets = pool_secret * pool_secret

    type host = API.ref_host

    let save_checkpoint checkpoint =
      Unixext.atomic_write_to_file checkpoint_path perm (fun fd ->
          let (_ : int) =
            Unix.write_substring fd checkpoint 0 (String.length checkpoint)
          in
          ())

    let retrieve_checkpoint () =
      match Unixext.read_lines ~path:checkpoint_path with
      | [x] ->
          Some x
      | [] | _ :: _ ->
          D.error "unexpected checkpoint file format: %s" checkpoint_path ;
          raise
            Api_errors.(
              Server_error
                (internal_error, ["unexpected checkpoint file format"]))
      | exception e ->
          D.info
            "tried to read %s, but encountered exception %s. assuming it \
             didn't exist"
            checkpoint_path (Printexc.to_string e) ;
          None

    let backup (old_pool_secret, new_pool_secret) =
      SecretString.write_to_file old_pool_secret_backup_path old_pool_secret ;
      SecretString.write_to_file new_pool_secret_backup_path new_pool_secret

    let retrieve () =
      match !Xapi_globs.pool_secrets with
      | x :: y :: _ ->
          (x, y)
      | _ ->
          (* do the backups exist? *)
          raise
            Api_errors.(
              Server_error
                (internal_error, ["can't retrieve backup pool secrets"]))

    let tell_accept_new_pool_secret (_, new_pool_secret) host =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Host.notify_accept_new_pool_secret rpc session_id host
            new_pool_secret)

    let tell_send_new_pool_secret (_, new_pool_secret) host =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Host.notify_send_new_pool_secret rpc session_id host
            new_pool_secret)

    let tell_cleanup_old_pool_secret host =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Host.cleanup_pool_secret rpc session_id host)

    let cleanup_master =
      cleanup_internal ~additional_files_to_remove:[checkpoint_path]
  end

let notify_new ~new_ps =
  match !Xapi_globs.pool_secrets with
  | _ :: _ :: _ ->
      D.info "xapi_psr.ml:notify_new: already accepting new pool secret"
  | _ ->
      let old_ps = Xapi_globs.pool_secret () in
      SecretString.write_to_file old_pool_secret_backup_path old_ps ;
      SecretString.write_to_file new_pool_secret_backup_path new_ps ;
      Xapi_globs.pool_secrets := [old_ps; new_ps]

let notify_send ~new_ps =
  if SecretString.equal (Xapi_globs.pool_secret ()) new_ps then
    D.info "xapi_psr.ml:notify_send: already sending new_ps"
  else
    match !Xapi_globs.pool_secrets with
    | [] | [_] ->
        raise
          Api_errors.(
            Server_error
              (internal_error, ["pool secret rotation not in progress"]))
    | old_pool_secret :: new_pool_secret :: _ ->
        let () =
          Unixext.with_file new_pool_secret_backup_path [Unix.O_RDONLY] perm
            (fun ifd ->
              Unixext.with_file !Xapi_globs.pool_secret_path
                [Unix.O_WRONLY] perm (fun ofd ->
                  try
                    let (_ : int64) = Unixext.copy_file ifd ofd in
                    ()
                  with e ->
                    D.error
                      "xapi_psr.ml:notify_send: copy pool_secret_path failed. \
                       from = %s, to = %s"
                      new_pool_secret_backup_path
                      !Xapi_globs.pool_secret_path ;
                    raise e))
        in
        Xapi_globs.pool_secrets := [new_pool_secret; old_pool_secret] ;
        Db_globs.pool_secret :=
          new_pool_secret |> SecretString.rpc_of_t |> Db_secret_string.t_of_rpc

let cleanup = cleanup_internal ~additional_files_to_remove:[]

module HostSet = Set.Make (struct
  type t = API.ref_host

  let compare = Stdlib.compare
end)

module Ptoken = Genptokenlib.Lib

(* at most one PSR should be
   running at any given time *)
let m = Mutex.create ()

let start ~__context =
  (* checked preconditions:
     * the feature is enabled (to prevent a PSR when not all hosts have upgraded to this version)
     * we are the master
     * HA disabled
     * RPU not running
     * all hosts in pool 'alive' *)
  let f () =
    Pool_features.assert_enabled ~__context ~f:Features.Pool_secret_rotation ;
    if
      not
        (Helpers.is_pool_master ~__context
           ~host:(Helpers.get_localhost ~__context))
    then
      raise
        Api_errors.(
          Server_error (host_is_slave, [Pool_role.get_master_address ()])) ;
    let live_hosts = Helpers.get_live_hosts ~__context |> HostSet.of_list in
    let all_hosts_list = Xapi_pool_helpers.get_master_slaves_list ~__context in
    let all_hosts = all_hosts_list |> HostSet.of_list in
    let offline_hosts = HostSet.diff all_hosts live_hosts in
    ( if not (HostSet.is_empty offline_hosts) then
        let offline_host = HostSet.min_elt offline_hosts in
        raise
          Api_errors.(
            Server_error (cannot_contact_host, [Ref.string_of offline_host]))
    ) ;
    let is_ha_enabled =
      Db.Pool.get_ha_enabled ~__context ~self:(Helpers.get_pool ~__context)
    in
    if is_ha_enabled then raise Api_errors.(Server_error (ha_is_enabled, [])) ;
    if Helpers.rolling_upgrade_in_progress ~__context then
      raise Api_errors.(Server_error (not_supported_during_upgrade, [])) ;
    let module PSR = Make (Impl (struct let __context = __context end)) in
    let[@warning "-8"] (master :: members) = all_hosts_list in
    let r =
      PSR.start
        (Xapi_globs.pool_secret (), Ptoken.gen_token ())
        ~master ~members
    in
    match r with
    | Ok () ->
        D.info "Xapi_psr.start: pool secret rotation successful"
    | Error e ->
        let err_msg = user_facing_error_message ~__context e in
        D.error "PSR failed: %s" err_msg ;
        raise Api_errors.(Server_error (internal_error, [err_msg]))
  in
  if Mutex.try_lock m then (
    try f () ; Mutex.unlock m with e -> Mutex.unlock m ; raise e
  ) else
    raise
      Api_errors.(
        Server_error (internal_error, ["pool secret rotation already running"]))
