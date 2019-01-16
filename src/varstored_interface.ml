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
open Lwt.Infix

module D = Debug.Make (struct
  let name = "varstored_interface"
end)

open D

type rpc_t = Rpc.t

let err = Xenops_interface.err

type nvram = (string * string) list [@@deriving rpcty]

(* make_json doesn't work here *)
let rpc = Xen_api_lwt_unix.make "file:///var/lib/xcp/xapi"
let originator = "varstored-guard"
let version = "0.1"

type session = [`session] Ref.t

module SessionCache : sig
  type t

  (** [create ~login ~logout] will create a global session cache holding 1 session. *)
  val create : login:(unit -> session Lwt.t) -> logout:(session -> unit Lwt.t) -> t

  (** [with_session cache f] acquires a session (logging in if necessary) and calls [f].
   ** If [f] fails due to an invalid session then the session is removed from the cache,
   ** and [f] is retried with a new session *)
  val with_session :
    t -> (rpc:(call -> response Lwt.t) -> session_id:session -> 'a Lwt.t) -> 'a Lwt.t

  (** [destroy cache] logs out all sessions from the cache *)
  val destroy : t -> unit Lwt.t
end = struct
  type t =
    { valid_sessions : (session, unit) Hashtbl.t
    ; pool : session Lwt_pool.t }

  (* Do NOT log session IDs, they are secret *)

  let create ~login ~logout =
    let valid_sessions = Hashtbl.create 3 in
    let validate session =
      let is_valid = Hashtbl.mem valid_sessions session in
      (* do not log success, to avoid too much logging *)
      if not is_valid then debug "SessionCache.validate failed: session is not valid";
      Lwt.return is_valid
    in
    let acquire () =
      login ()
      >>= fun session ->
      Hashtbl.add valid_sessions session ();
      debug "SessionCache.acquired";
      Lwt.return session
    in
    let dispose session =
      debug "SessionCache.dispose";
      logout session
      >|= fun () ->
      debug "SessionCache.disposed";
      Hashtbl.remove valid_sessions session
    in
    let pool = Lwt_pool.create 1 ~validate ~dispose acquire in
    {valid_sessions; pool}

  let invalidate t session_id =
    (* Remove just the specified expired session,
     * another Lwt promise might've added a new session already. *)
    Hashtbl.remove t.valid_sessions session_id;
    debug "SessionCache.invalidated"

  let destroy t = Lwt_pool.clear t.pool

  let rec with_session t f =
    (* we can use the same session from multiple concurrent requests,
     * we just do not want to log in more than once *)
    Lwt_pool.use t.pool Lwt.return
    >>= fun session_id ->
    Lwt.catch
      (fun () -> f ~rpc ~session_id)
      (function
        | Api_errors.Server_error (code, _) when code = Api_errors.session_invalid ->
          invalidate t session_id;
          (with_session [@tailcall]) t f
        | e ->
          debug "XAPI call failed: %s" (Printexc.to_string e);
          Lwt.fail e)
end

open Xen_api_lwt_unix

let login () =
  Session.login_with_password ~rpc ~uname:"root" ~pwd:"" ~version ~originator

let logout session_id =
  Lwt.catch
    (fun () -> Session.logout ~rpc ~session_id)
    (function
      | Api_errors.Server_error (code, _) when code = Api_errors.session_invalid ->
        (* ignore, already logged out or expired *)
        Lwt.return_unit
      | e -> Lwt.fail e)

let cache = SessionCache.create ~login ~logout
let shutdown = Lwt_switch.create ()

let () =
  Lwt_switch.add_hook (Some shutdown) (fun () ->
      debug "Cleaning up cache at exit";
      SessionCache.destroy cache )

let () =
  let cleanup n =
    debug "Triggering cleanup on signal %d, and waiting for servers to stop" n;
    Lwt.async (fun () ->
        Lwt_switch.turn_off shutdown
        >>= fun () ->
        info "Cleanup complete, exiting";
        exit 0 )
  in
  Lwt_unix.on_signal Sys.sigterm cleanup |> ignore;
  Lwt_unix.on_signal Sys.sigint cleanup |> ignore;
  (* the default maximum is 1000, and idle threads never exit,
   * this is only needed for syscalls that would otherwise block *)
  Lwt_unix.set_pool_size 16

let with_xapi f = Lwt_unix.with_timeout 120. (fun () -> SessionCache.with_session cache f)

(* Unfortunately Cohttp doesn't provide us a way to know when it finished
 * creating the socket, and creating the socket is done asynchronously in an Lwt promise.
 * It only ever returns from server creation when the server is stopped.
 * Try actually connecting: the file could be present but nobody listening on the other side.
 * *)
let rec wait_for_file_to_appear path =
  Lwt_unix.yield () >>= fun () ->
  Lwt.try_bind (fun () ->
    Conduit_lwt_unix.connect ~ctx:Conduit_lwt_unix.default_ctx (`Unix_domain_socket (`File path)))
    (fun (_, ic, _oc) ->
       D.debug "Socket at %s works" path;
       (* do not close both channels, or we get an EBADF *)
       Lwt_io.close ic)
    (fun e ->
       D.debug "Waiting for file %s to appear (%s)" path (Printexc.to_string e);
       Lwt_unix.sleep 0.1 >>= fun () ->
       wait_for_file_to_appear path)

let serve_forever_lwt rpc_fn path =
  let conn_closed _ = () in
  let on_exn e =
    log_backtrace ();
    warn "Exception: %s" (Printexc.to_string e)
  in
  let callback _ req body =
    let uri = Cohttp.Request.uri req in
    match Cohttp.Request.meth req, Uri.path uri with
    | `POST, _ ->
      Cohttp_lwt.Body.to_string body
      >>= fun body ->
      Dorpc.wrap_rpc err (fun () ->
          let call = Xmlrpc.call_of_string body in
          (* Do not log the request, it will contain NVRAM *)
          D.debug "Received request on %s, method %s" path call.Rpc.name;
          rpc_fn call )
      >>= fun response ->
      let body = response |> Xmlrpc.string_of_response in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
    | _, _ ->
      let body =
        "Not allowed" |> Rpc.rpc_of_string |> Rpc.failure |> Xmlrpc.string_of_response
      in
      Cohttp_lwt_unix.Server.respond_string ~status:`Method_not_allowed ~body ()
  in
  let stop, do_stop = Lwt.task () in
  let server_wait_exit =
    Cohttp_lwt_unix.Server.make ~callback ~conn_closed ()
    |> Cohttp_lwt_unix.Server.create
         ~stop
         ~mode:(`Unix_domain_socket (`File path))
         ~on_exn
  in
  let cleanup () =
    (try Lwt.wakeup do_stop () with _ -> ());
    server_wait_exit
  in
  Lwt_switch.add_hook (Some shutdown) cleanup;
  (* if server_wait_exit fails then cancel waiting for file to appear
   * otherwise do not cancel the server if the file appeared (Lwt.protected) *)
  Lwt.pick
    [ Lwt_unix.with_timeout 120. (fun () -> wait_for_file_to_appear path)
    ; Lwt.protected server_wait_exit ] >>= fun () ->
  Lwt.return cleanup

(* Create a restricted RPC function and socket for a specific VM *)
let make_server_rpcfn path vm_uuid =
  let module Server = Varstore_deprivileged_interface.RPC_API (Rpc_lwt.GenServer ()) in
  with_xapi @@ VM.get_by_uuid ~uuid:vm_uuid
  >>= fun vm ->
  let ret v =
    (* TODO: maybe map XAPI exceptions *)
    v >>= Lwt.return_ok |> Rpc_lwt.T.put
  in
  let get_nvram _ _ = ret @@ with_xapi @@ VM.get_NVRAM ~self:vm in
  let set_nvram _ _ nvram =
    ret @@ with_xapi @@ VM.set_NVRAM_EFI_variables ~self:vm ~value:nvram
  in
  let message_create _ _name priority _cls _uuid body =
    ret
      ( with_xapi
        @@ Message.create
             ~name:"VM_SECURE_BOOT_FAILED"
             ~priority
             ~cls:`VM
             ~obj_uuid:vm_uuid
             ~body
      >>= fun _ -> Lwt.return_unit )
  in
  let get_by_uuid _ _ = ret @@ Lwt.return "DUMMYVM" in
  let dummy_login _ _ _ _ = ret @@ Lwt.return "DUMMYSESSION" in
  let dummy_logout _ = ret @@ Lwt.return_unit in
  Server.get_NVRAM get_nvram;
  Server.set_NVRAM set_nvram;
  Server.message_create message_create;
  Server.session_login dummy_login;
  Server.session_logout dummy_logout;
  Server.get_by_uuid get_by_uuid;
  serve_forever_lwt (Rpc_lwt.server Server.implementation) path
