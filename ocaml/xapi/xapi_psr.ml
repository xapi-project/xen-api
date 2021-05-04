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
  * given a list of hosts containing both the coordinator and
    supporters, the coordinator will always be at the head *)

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
  match failure with
  | Failed_during_accept_new_pool_secret ->
      Printf.sprintf
        "Operation to tell %s to begin accepting new pool secret failed."
        host_name
  | Failed_during_send_new_pool_secret ->
      Printf.sprintf
        "Operation to tell %s to begin sending new pool secret failed."
        host_name
  | Failed_during_cleanup ->
      Printf.sprintf
        "%s encountered an error whilst cleaning up after a pool secret \
         rotation."
        host_name

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

  val tell_cleanup_old_pool_secret : pool_secrets -> host -> unit

  val cleanup_coordinator : pool_secrets -> unit
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

    let rec go pool_secrets coordinator supporters = function
      | No_checkpoint ->
          (* if we fail to backup the pool secrets, it doesn't really matter,
             you can simply restart the rotation *)
          Impl.backup pool_secrets ;
          (go [@tailcall]) pool_secrets coordinator supporters
            Accept_new_pool_secret
      | Accept_new_pool_secret ->
          Impl.save_checkpoint (string_of_checkpoint Accept_new_pool_secret) ;
          coordinator :: supporters
          |> iter_break (fun host ->
                 try
                   Impl.tell_accept_new_pool_secret pool_secrets host ;
                   Ok ()
                 with e ->
                   D.error
                     "failed while telling host to accept new pool secret. \
                      error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_accept_new_pool_secret, host)
             )
          >>= fun () ->
          (go [@tailcall]) pool_secrets coordinator supporters
            Send_new_pool_secret
      | Send_new_pool_secret ->
          Impl.save_checkpoint (string_of_checkpoint Send_new_pool_secret) ;
          coordinator :: supporters
          |> iter_break (fun host ->
                 try
                   Impl.tell_send_new_pool_secret pool_secrets host ;
                   Ok ()
                 with e ->
                   D.error
                     "failed while telling hosts to send new pool secret. \
                      error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_send_new_pool_secret, host)
             )
          >>= fun () ->
          (go [@tailcall]) pool_secrets coordinator supporters Cleanup_members
      | Cleanup_members ->
          Impl.save_checkpoint (string_of_checkpoint Cleanup_members) ;
          supporters
          |> iter_break (fun member ->
                 try
                   Impl.tell_cleanup_old_pool_secret pool_secrets member ;
                   Ok ()
                 with e ->
                   D.error "failed while telling hosts to cleanup. error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_cleanup, member)
             )
          >>= fun () ->
          (go [@tailcall]) pool_secrets coordinator supporters Cleanup_master
      | Cleanup_master -> (
          Impl.save_checkpoint (string_of_checkpoint Cleanup_master) ;
          try
            Impl.cleanup_coordinator pool_secrets ;
            Ok ()
          with e ->
            D.error "failed to cleanup the coordinator. error= %s"
              (Printexc.to_string e) ;
            Error (Failed_during_cleanup, coordinator)
        )

    let start pool_secrets ~coordinator ~supporters =
      try
        match
          Impl.retrieve_checkpoint () |> Option.map checkpoint_of_string
        with
        | None | Some No_checkpoint ->
            go pool_secrets coordinator supporters No_checkpoint
        | Some checkpoint ->
            go (Impl.retrieve ()) coordinator supporters checkpoint
      with e ->
        (* _in theory_ save_checkpoint or backup could fail, so
           catch that here. however we don't expect this to happen *)
        D.error "PSR.start: unexpected error: %s" (Printexc.to_string e) ;
        raise
          Api_errors.(
            Server_error (internal_error, ["PSR.start: unexpected error"])
          )
  end

let perm = 0o640

(* we include some sanity checks for the following invariants:
     - only the coordinator should have a checkpoint
     - either (a) all the coordinator psr state exists (=> resuming a failed psr)
       or     (b) none of the state exists (=> starting a new psr)
     - existing pool secret backups are consistent with requested pool secret
       changes
     - the runtime state, i.e. Xapi_globs.pool_secrets (although this check
       is not done here, but at each stage separately)
   these invariants could be broken if for example a psr fails, the coordinator
   changes, and then a new psr starts on the new coordinator
*)
module Assert : sig
  val backups_match : old_ps:SecretString.t -> new_ps:SecretString.t -> unit

  val no_backups : unit -> unit

  val no_checkpoint : unit -> unit

  val coordinator_state_valid : unit -> unit
end = struct
  let do_backups_exist () =
    let does_old_backup_exist = Sys.file_exists old_pool_secret_backup_path in
    let does_new_backup_exist = Sys.file_exists new_pool_secret_backup_path in
    match (does_old_backup_exist, does_new_backup_exist) with
    | true, true ->
        true
    | false, false ->
        false
    | false, true | true, false ->
        raise
          Api_errors.(
            Server_error
              (internal_error, ["do_backups_exist: invalid backup state"])
          )

  let does_checkpoint_exist () = Sys.file_exists checkpoint_path

  let backups_match ~old_ps ~new_ps =
    let do_backups_match =
      D.info "Assert.do_backups_match" ;
      let old_backup, new_backup = read_backups () in
      SecretString.(equal old_backup old_ps && equal new_backup new_ps)
    in
    if not do_backups_match then
      raise Api_errors.(Server_error (internal_error, ["backups don't match"]))

  let no_backups () =
    if do_backups_exist () then
      raise
        Api_errors.(
          Server_error (internal_error, ["pool member should have no backups"])
        )

  let no_checkpoint () =
    (* we expect a checkpoint on the coordinator, but not supporters *)
    if Pool_role.is_supporter () && does_checkpoint_exist () then
      raise
        Api_errors.(
          Server_error (internal_error, ["supporter must not have a checkpoint"])
        )

  let coordinator_state_valid () =
    match (do_backups_exist (), does_checkpoint_exist ()) with
    | false, false | true, true ->
        ()
    | false, true | true, false ->
        raise
          Api_errors.(
            Server_error
              ( internal_error
              , ["coordinator pool secret rotation state is invalid"]
              )
          )
end

let cleanup_internal ~additional_files_to_remove ~old_ps ~new_ps =
  match !Xapi_globs.pool_secrets with
  | [ps] when ps = new_ps ->
      Assert.no_backups () ;
      D.info "%s: already cleaned up" __FUNCTION__
  | [_] ->
      raise
        Api_errors.(
          Server_error
            ( internal_error
            , [
                "cleanup_internal: host has been cleaned up, but pool secret \
                 doesn't match"
              ]
            )
        )
  | [priority_1_ps; priority_2_ps]
    when SecretString.(equal new_ps priority_1_ps && equal old_ps priority_2_ps)
    ->
      Assert.backups_match ~old_ps ~new_ps ;
      let files_to_remove =
        List.append additional_files_to_remove
          [old_pool_secret_backup_path; new_pool_secret_backup_path]
      in
      (* if we don't remove pool secret backups, they will be loaded into memory when xapi starts *)
      List.iter
        (fun path ->
          try Sys.remove path
          with e ->
            D.error "cleanup_internal: failed to remove %s. error: %s" path
              (Printexc.to_string e)
        )
        files_to_remove ;
      (* psr done, so stop accepting old pool secret *)
      Xapi_globs.pool_secrets := [priority_1_ps]
  | [_; _] ->
      raise
        Api_errors.(
          Server_error
            (internal_error, ["cleanup_internal: runtime secrets don't match"])
        )
  | l ->
      raise
        Api_errors.(
          Server_error
            ( internal_error
            , [
                Printf.sprintf
                  "cleanup_internal: expected 1 or 2 pool secrets, got: %i"
                  (List.length l)
              ]
            )
        )

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
          ()
      )

    let retrieve_checkpoint () =
      match Unixext.read_lines ~path:checkpoint_path with
      | [x] ->
          Some x
      | [] | _ :: _ ->
          D.error "unexpected checkpoint file format: %s" checkpoint_path ;
          raise
            Api_errors.(
              Server_error
                (internal_error, ["unexpected checkpoint file format"])
            )
      | exception e ->
          D.info
            "tried to read %s, but encountered exception %s. assuming it \
             didn't exist"
            checkpoint_path (Printexc.to_string e) ;
          None

    let backup (old_pool_secret, new_pool_secret) =
      Xapi_fist.hang_psr `backup ;
      SecretString.write_to_file old_pool_secret_backup_path old_pool_secret ;
      SecretString.write_to_file new_pool_secret_backup_path new_pool_secret

    let retrieve = read_backups

    let tell_accept_new_pool_secret (old_ps, new_ps) host =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Host.notify_accept_new_pool_secret ~rpc ~session_id ~host
            ~old_ps ~new_ps
      )

    let tell_send_new_pool_secret (old_ps, new_ps) host =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Host.notify_send_new_pool_secret ~rpc ~session_id ~host ~old_ps
            ~new_ps
      )

    let tell_cleanup_old_pool_secret (old_ps, new_ps) host =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Host.cleanup_pool_secret ~rpc ~session_id ~host ~old_ps ~new_ps
      )

    let cleanup_coordinator (old_ps, new_ps) =
      Xapi_fist.hang_psr `cleanup ;
      cleanup_internal ~additional_files_to_remove:[checkpoint_path] ~old_ps
        ~new_ps
  end

let notify_new ~__context ~old_ps ~new_ps =
  Xapi_fist.hang_psr `notify_new ;
  Assert.no_checkpoint () ;
  match !Xapi_globs.pool_secrets with
  | [priority_1_ps; priority_2_ps] ->
      (* check disk state *)
      Assert.backups_match ~old_ps ~new_ps ;
      (* check runtime state *)
      if SecretString.(equal priority_2_ps new_ps && equal priority_1_ps old_ps)
      then
        D.info "xapi_psr.ml:notify_new: already accepting new pool secret"
      else
        raise
          Api_errors.(
            Server_error
              ( internal_error
              , ["notify_new: existing pool secrets are inconsistent"]
              )
          )
  | [priority_1_ps] when SecretString.equal priority_1_ps old_ps ->
      if Pool_role.is_supporter () then Assert.no_backups () ;
      SecretString.write_to_file old_pool_secret_backup_path old_ps ;
      SecretString.write_to_file new_pool_secret_backup_path new_ps ;
      Xapi_globs.pool_secrets := [old_ps; new_ps]
  | [_] ->
      raise
        Api_errors.(
          Server_error
            (internal_error, ["notify_new: old pool secret doesn't match"])
        )
  | l ->
      raise
        Api_errors.(
          Server_error
            ( internal_error
            , [
                Printf.sprintf
                  "notify_new: expected 1 or 2 pool secrets, got: %i"
                  (List.length l)
              ]
            )
        )

let notify_send ~__context ~old_ps ~new_ps =
  Xapi_fist.hang_psr `notify_send ;
  Assert.no_checkpoint () ;
  Assert.backups_match ~old_ps ~new_ps ;
  match !Xapi_globs.pool_secrets with
  | [priority_1_ps; priority_2_ps]
    when SecretString.(equal priority_1_ps old_ps && equal priority_2_ps new_ps)
    ->
      (* reverse pool secret priorities, so that new_ps is sent in requests *)
      let () =
        Unixext.with_file new_pool_secret_backup_path [Unix.O_RDONLY] perm
          (fun ifd ->
            Unixext.with_file !Xapi_globs.pool_secret_path [Unix.O_WRONLY] perm
              (fun ofd ->
                try
                  let (_ : int64) = Unixext.copy_file ifd ofd in
                  ()
                with e ->
                  D.error
                    "xapi_psr.ml:notify_send: copy pool_secret_path failed. \
                     from = %s, to = %s"
                    new_pool_secret_backup_path
                    !Xapi_globs.pool_secret_path ;
                  raise e
            )
        )
      in
      Xapi_globs.pool_secrets := [priority_2_ps; priority_1_ps] ;
      Db_globs.pool_secret :=
        priority_2_ps |> SecretString.rpc_of_t |> Db_secret_string.t_of_rpc
  | [priority_1_ps; priority_2_ps]
    when SecretString.(equal priority_1_ps new_ps && equal priority_2_ps old_ps)
    ->
      D.info "xapi_psr.ml:notify_send: already sending new_ps"
  | [_; _] ->
      raise
        Api_errors.(
          Server_error
            (internal_error, ["notify_send: runtime secrets don't match"])
        )
  | l ->
      raise
        Api_errors.(
          Server_error
            ( internal_error
            , [
                Printf.sprintf "notify_send: expected 2 pool secrets, got: %i"
                  (List.length l)
              ]
            )
        )

let cleanup ~__context ~old_ps ~new_ps =
  Xapi_fist.hang_psr `cleanup ;
  Assert.no_checkpoint () ;
  cleanup_internal ~additional_files_to_remove:[] ~old_ps ~new_ps

module HostSet = Set.Make (struct
  type t = API.ref_host

  let compare = Stdlib.compare
end)

let start =
  let m = Mutex.create () in
  let with_lock f =
    (* prevents multiple concurrent PSRs *)
    if Mutex.try_lock m then (
      try f () ; Mutex.unlock m with e -> Mutex.unlock m ; raise e
    ) else
      raise
        Api_errors.(
          Server_error (internal_error, ["pool secret rotation already running"])
        )
  in
  fun ~__context ->
    let self = Helpers.get_pool ~__context in
    let set_up_psr_pending_flag asserts =
      let was_already_pending = Db.Pool.get_is_psr_pending ~__context ~self in
      let maybe_set_pending value =
        if not was_already_pending then
          Db.Pool.set_is_psr_pending ~__context ~self ~value
      in
      maybe_set_pending true ;
      try asserts ()
      with e ->
        (* only set pending to false if we just set it to true *)
        maybe_set_pending false ; raise e
    in
    let assert_no_ha () =
      let is_ha_enabled = Db.Pool.get_ha_enabled ~__context ~self in
      if is_ha_enabled then
        raise Api_errors.(Server_error (ha_is_enabled, []))
    in
    let assert_we_are_coordinator () =
      if
        not
          (Helpers.is_coordinator ~__context
             ~host:(Helpers.get_localhost ~__context)
          )
      then
        raise
          Api_errors.(
            Server_error
              (host_is_supporter, [Pool_role.get_address_of_coordinator_exn ()])
          )
    in
    let assert_all_hosts_alive () =
      let live_hosts = Helpers.get_live_hosts ~__context |> HostSet.of_list in
      let all_hosts_list =
        Xapi_pool_helpers.get_members_coordinator_first ~__context
      in
      let all_hosts = all_hosts_list |> HostSet.of_list in
      let offline_hosts = HostSet.diff all_hosts live_hosts in
      ( if not (HostSet.is_empty offline_hosts) then
          let offline_host = HostSet.min_elt offline_hosts in
          raise
            Api_errors.(
              Server_error (cannot_contact_host, [Ref.string_of offline_host])
            )
      ) ;
      all_hosts_list
    in
    let assert_no_rpu () =
      if Helpers.rolling_upgrade_in_progress ~__context then
        raise Api_errors.(Server_error (not_supported_during_upgrade, []))
    in
    with_lock (fun () ->
        let coordinator, supporters =
          set_up_psr_pending_flag (fun () ->
              Pool_features.assert_enabled ~__context
                ~f:Features.Pool_secret_rotation ;
              assert_we_are_coordinator () ;
              Assert.coordinator_state_valid () ;
              let[@warning "-8"] (coordinator :: supporters) =
                assert_all_hosts_alive ()
              in
              assert_no_ha () ;
              assert_no_rpu () ;
              Xapi_pool_helpers.assert_no_pool_ops ~__context ;
              (coordinator, supporters)
          )
        in
        let module PSR = Make (Impl (struct let __context = __context end)) in
        let r =
          PSR.start
            (Xapi_globs.pool_secret (), Helpers.PoolSecret.make ())
            ~coordinator ~supporters
        in
        match r with
        | Ok () ->
            (* if a rotation fails, then PSR should remain pending -
               the user is expected to re-run the rotation *)
            Db.Pool.set_is_psr_pending ~__context ~self ~value:false ;
            D.info "Xapi_psr.start: pool secret rotation successful"
        | Error e ->
            let err_msg = user_facing_error_message ~__context e in
            D.error "PSR failed: %s" err_msg ;
            raise Api_errors.(Server_error (internal_error, [err_msg]))
    )
