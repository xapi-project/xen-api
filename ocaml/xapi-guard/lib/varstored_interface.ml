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

open Rpc
open Lwt.Syntax

module D = Debug.Make (struct let name = "varstored_interface" end)

open D

type rpc_t = Rpc.t

let err = Xenops_interface.err

type nvram = (string * string) list [@@deriving rpcty]

let originator = "varstored-guard"

let version = "0.1"

type session = [`session] Ref.t

type rpc = call -> response Lwt.t

module SessionCache : sig
  type t

  val create :
       rpc:rpc
    -> login:(rpc:rpc -> session Lwt.t)
    -> logout:(rpc:rpc -> session -> unit Lwt.t)
    -> t
  (** [create ~login ~logout] will create a global session cache holding 1 session. *)

  val with_session :
    t -> (rpc:rpc -> session_id:session -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_session cache f] acquires a session (logging in if necessary) and calls [f].
   ** If [f] fails due to an invalid session then the session is removed from the cache,
   ** and [f] is retried with a new session *)

  val destroy : t -> unit Lwt.t
  (** [destroy cache] logs out all sessions from the cache *)
end = struct
  type t = {
      rpc: rpc
    ; valid_sessions: (session, unit) Hashtbl.t
    ; pool: session Lwt_pool.t
  }

  (* Do NOT log session IDs, they are secret *)

  let create ~rpc ~login ~logout =
    let valid_sessions = Hashtbl.create 3 in
    let validate session =
      let is_valid = Hashtbl.mem valid_sessions session in
      (* do not log success, to avoid too much logging *)
      if not is_valid then
        debug "SessionCache.validate failed: session is not valid" ;
      Lwt.return is_valid
    in
    let acquire () =
      let* session = login ~rpc in
      Hashtbl.add valid_sessions session () ;
      debug "SessionCache.acquired" ;
      Lwt.return session
    in
    let dispose session =
      debug "SessionCache.dispose" ;
      let+ () = logout ~rpc session in
      debug "SessionCache.disposed" ;
      Hashtbl.remove valid_sessions session
    in
    let pool = Lwt_pool.create 1 ~validate ~dispose acquire in
    {valid_sessions; pool; rpc}

  let invalidate t session_id =
    (* Remove just the specified expired session,
     * another Lwt promise might've added a new session already. *)
    Hashtbl.remove t.valid_sessions session_id ;
    debug "SessionCache.invalidated"

  let destroy t = Lwt_pool.clear t.pool

  let rec with_session t f =
    (* we can use the same session from multiple concurrent requests,
     * we just do not want to log in more than once *)
    let* session_id = Lwt_pool.use t.pool Lwt.return in
    Lwt.catch
      (fun () -> f ~rpc:t.rpc ~session_id)
      (function
        | Api_errors.Server_error (code, _)
          when code = Api_errors.session_invalid ->
            invalidate t session_id ;
            (with_session [@tailcall]) t f
        | e ->
            debug "XAPI call failed: %s" (Printexc.to_string e) ;
            Lwt.fail e
        )
end

open Xen_api_lwt_unix

let login ~rpc =
  Session.login_with_password ~rpc ~uname:"root" ~pwd:"" ~version ~originator

let logout ~rpc session_id =
  Lwt.catch
    (fun () -> Session.logout ~rpc ~session_id)
    (function
      | Api_errors.Server_error (code, _) when code = Api_errors.session_invalid
        ->
          (* ignore, already logged out or expired *)
          Lwt.return_unit
      | e ->
          Lwt.fail e
      )

let shutdown = Lwt_switch.create ()

let () =
  let cleanup n =
    debug "Triggering cleanup on signal %d, and waiting for servers to stop" n ;
    Lwt.async (fun () ->
        let* () = Lwt_switch.turn_off shutdown in
        info "Cleanup complete, exiting" ;
        exit 0
    )
  in
  Lwt_unix.on_signal Sys.sigterm cleanup |> ignore ;
  Lwt_unix.on_signal Sys.sigint cleanup |> ignore ;
  (* the default maximum is 1000, and idle threads never exit,
   * this is only needed for syscalls that would otherwise block *)
  Lwt_unix.set_pool_size 16

let with_xapi ~cache f =
  Lwt_unix.with_timeout 120. (fun () -> SessionCache.with_session cache f)

(* Unfortunately Cohttp doesn't provide us a way to know when it finished
 * creating the socket, and creating the socket is done asynchronously in an Lwt promise.
 * It only ever returns from server creation when the server is stopped.
 * Try actually connecting: the file could be present but nobody listening on the other side.
 * *)
let rec wait_connectable path =
  let* res =
    Lwt_result.catch
      (Conduit_lwt_unix.connect ~ctx:Conduit_lwt_unix.default_ctx
         (`Unix_domain_socket (`File path))
      )
  in
  match res with
  | Ok (_, ic, oc) ->
      D.debug "Socket at %s works" path ;
      (* close both channels, in non-SSL mode this would leak otherwise *)
      let* () = Lwt_io.close ic in
      Lwt_io.close oc
  | Error e ->
      D.debug "Waiting for socket to be connectable at %s: %s" path
        (Printexc.to_string e) ;
      (* just in case Lwt_unix.listen doesn't get called soon enough,
         avoid using up 100% CPU waiting for it,
         better than Lwt.pause ()
      *)
      let* () = Lwt_unix.sleep 0.001 in
      wait_connectable path

let with_inotify f =
  let* inotify = Lwt_inotify.create () in
  Lwt.finalize (fun () -> f inotify) (fun () -> Lwt_inotify.close inotify)

let wait_for_file_to_appear path =
  with_inotify @@ fun inotify ->
  (* we need to check for existence after setting up the watch to avoid race conditions:
     S_Create event on parent dir is only sent on creating a new file,
     and you cannot set up an inotify on a non-existent file.
  *)
  let* (_watch : Inotify.watch) =
    Lwt_inotify.add_watch inotify (Filename.dirname path) [Inotify.S_Create]
  in
  let rec loop () =
    let* exists = Lwt_unix.file_exists path in
    if exists then
      Lwt.return_unit
    else (
      D.debug "Waiting for file %s to appear" path ;
      let* (_event : Inotify.event) = Lwt_inotify.read inotify in
      (* we've got a create event knowing that *a* file got created,
         but not necessarily the one we were looking for *)
      loop ()
    )
  in
  loop ()

let wait_for_connectable_socket path =
  (* pause gives a chance for the conduit lwt promise to run and listen *)
  let* () = Lwt.pause () in
  let* () = wait_for_file_to_appear path in
  let* () = Lwt.pause () in
  wait_connectable path

let serve_forever_lwt rpc_fn path =
  let conn_closed _ = () in
  let on_exn e =
    log_backtrace () ;
    warn "Exception: %s" (Printexc.to_string e)
  in
  let callback _ req body =
    let uri = Cohttp.Request.uri req in
    match (Cohttp.Request.meth req, Uri.path uri) with
    | `POST, _ ->
        let* body = Cohttp_lwt.Body.to_string body in
        let* response =
          Dorpc.wrap_rpc err (fun () ->
              let call = Xmlrpc.call_of_string body in
              (* Do not log the request, it will contain NVRAM *)
              D.debug "Received request on %s, method %s" path call.Rpc.name ;
              rpc_fn call
          )
        in
        let body = response |> Xmlrpc.string_of_response in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
    | _, _ ->
        let body =
          "Not allowed"
          |> Rpc.rpc_of_string
          |> Rpc.failure
          |> Xmlrpc.string_of_response
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`Method_not_allowed ~body
          ()
  in
  let stop, do_stop = Lwt.task () in
  let server_wait_exit =
    Cohttp_lwt_unix.Server.make ~callback ~conn_closed ()
    |> Cohttp_lwt_unix.Server.create ~stop
         ~mode:(`Unix_domain_socket (`File path))
         ~on_exn
  in
  let cleanup () =
    (try Lwt.wakeup do_stop () with _ -> ()) ;
    server_wait_exit
  in
  Lwt_switch.add_hook (Some shutdown) cleanup ;
  (* if server_wait_exit fails then cancel waiting for file to appear
   * otherwise do not cancel the server if the file appeared (Lwt.protected) *)
  let* () =
    Lwt.pick
      [
        Lwt_unix.with_timeout 120. (fun () -> wait_for_connectable_socket path)
      ; Lwt.protected server_wait_exit
      ]
  in
  Lwt.return cleanup

(* Create a restricted RPC function and socket for a specific VM *)
let make_server_rpcfn ~cache path vm_uuid =
  let module Server =
    Varstore_deprivileged_interface.RPC_API (Rpc_lwt.GenServer ()) in
  let* vm = with_xapi ~cache @@ VM.get_by_uuid ~uuid:vm_uuid in
  let ret v =
    (* TODO: maybe map XAPI exceptions *)
    Lwt.bind v Lwt.return_ok |> Rpc_lwt.T.put
  in
  let get_nvram _ _ = ret @@ with_xapi ~cache @@ VM.get_NVRAM ~self:vm in
  let set_nvram _ _ nvram =
    ret @@ with_xapi ~cache @@ VM.set_NVRAM_EFI_variables ~self:vm ~value:nvram
  in
  let message_create _ _name priority _cls _uuid body =
    ret
      (let* (_ : _ Ref.t) =
         with_xapi ~cache
         @@ Message.create ~name:"VM_SECURE_BOOT_FAILED" ~priority ~cls:`VM
              ~obj_uuid:vm_uuid ~body
       in
       Lwt.return_unit
      )
  in
  (* we return a static string for these API calls:
     the sandboxed varstored/swtpm is not allowed to choose which VM/VTPM to talk to,
     it can only query its own, so we'll replace these parameters in calls anyway *)
  let get_by_uuid _ _ = ret @@ Lwt.return "DUMMYVM" in
  let dummy_login _ _ _ _ = ret @@ Lwt.return "DUMMYSESSION" in
  let dummy_logout _ = ret @@ Lwt.return_unit in
  Server.get_NVRAM get_nvram ;
  Server.set_NVRAM set_nvram ;
  Server.message_create message_create ;
  Server.session_login dummy_login ;
  Server.session_logout dummy_logout ;
  Server.get_by_uuid get_by_uuid ;
  serve_forever_lwt (Rpc_lwt.server Server.implementation) path
