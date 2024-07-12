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

module D = Debug.Make (struct let name = __MODULE__ end)

open D
module Tpm = Xapi_guard.Types.Tpm

type rpc_t = Rpc.t

let err = Xenops_interface.err

type nvram = (string * string) list [@@deriving rpcty]

let originator = "xapi-guard"

type session = [`session] Ref.t

type rpc = call -> response Lwt.t

open Xen_api_client_lwt.Xen_api_lwt_unix

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

let with_xapi ~cache ?(timeout = 120.) f =
  Lwt_unix.with_timeout timeout (fun () -> SessionCache.with_session cache f)

let serve_forever_lwt path callback =
  let conn_closed _ = () in
  let on_exn e =
    log_backtrace () ;
    warn "Exception: %s" (Printexc.to_string e)
  in
  let stop, do_stop = Lwt.task () in
  let server = Cohttp_lwt_unix.Server.make ~callback ~conn_closed () in
  (* small backlog: this is a dedicated socket for a single client *)
  let* socket = Conduit_lwt_server.listen ~backlog:2 (Unix.ADDR_UNIX path) in
  let server_wait_exit =
    Cohttp_lwt_unix.Server.create ~stop
      ~mode:(`TCP (`Socket socket))
      ~on_exn server
  in
  let cleanup () =
    (try Lwt.wakeup do_stop () with _ -> ()) ;
    server_wait_exit
  in
  Lwt_switch.add_hook (Some shutdown) cleanup ;
  Lwt.return cleanup

let serve_forever_lwt_callback rpc_fn path _ req body =
  let uri = Cohttp.Request.uri req in
  match (Cohttp.Request.meth req, Uri.path uri) with
  | `POST, _ ->
      let* body = Cohttp_lwt.Body.to_string body in
      let* response =
        Xapi_guard.Dorpc.wrap_rpc err (fun () ->
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
      Cohttp_lwt_unix.Server.respond_string ~status:`Method_not_allowed ~body ()

let with_xapi_vtpm ~cache vm_uuid =
  let vm_uuid_str = Uuidm.to_string vm_uuid in
  let* vm = with_xapi ~cache @@ VM.get_by_uuid ~uuid:vm_uuid_str in
  let* vTPMs = with_xapi ~cache @@ VM.get_VTPMs ~self:vm in
  match vTPMs with
  | [] ->
      D.warn
        "%s: received a request from a VM that has no VTPM associated, \
         ignoring request"
        __FUNCTION__ ;
      let msg =
        Printf.sprintf "No VTPM associated with VM %s, nothing to do"
          vm_uuid_str
      in
      raise (Failure msg)
  | self :: _ ->
      Lwt.return self

let push_vtpm ~cache (vm_uuid, _timestamp, key) contents =
  let* self = with_xapi_vtpm ~cache vm_uuid in
  let* old_contents = with_xapi ~cache @@ VTPM.get_contents ~self in
  let contents =
    Tpm.(old_contents |> deserialize |> update key contents |> serialize)
  in
  let* () = with_xapi ~cache @@ VTPM.set_contents ~self ~contents in
  Lwt_result.return ()

let read_vtpm ~cache (vm_uuid, _timestamp, key) =
  let* self = with_xapi_vtpm ~cache vm_uuid in
  let* contents = with_xapi ~cache @@ VTPM.get_contents ~self in
  let body = Tpm.(contents |> deserialize |> lookup ~key) in
  Lwt_result.return body

let serve_forever_lwt_callback_vtpm ~cache mutex (read, persist) vm_uuid _ req
    body =
  let uri = Cohttp.Request.uri req in
  let timestamp = Mtime_clock.now () in
  (* in case the connection is interrupted/etc. we may still have pending operations,
     so use a per vTPM mutex to ensure we really only have 1 pending operation at a time for a vTPM
  *)
  Lwt_mutex.with_lock mutex @@ fun () ->
  (* TODO: some logging *)
  match (Cohttp.Request.meth req, Uri.path uri) with
  | `GET, path when path <> "/" ->
      let key = Tpm.key_of_swtpm path in
      let* body = read (vm_uuid, timestamp, key) in
      let headers =
        Cohttp.Header.of_list [("Content-Type", "application/octet-stream")]
      in
      Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()
  | `PUT, path when path <> "/" ->
      let* body = Cohttp_lwt.Body.to_string body in
      let key = Tpm.key_of_swtpm path in
      let* () = persist (vm_uuid, timestamp, key) body in
      Cohttp_lwt_unix.Server.respond ~status:`No_content
        ~body:Cohttp_lwt.Body.empty ()
  | `DELETE, path when path <> "/" ->
      let* self = with_xapi_vtpm ~cache vm_uuid in
      let* contents = with_xapi ~cache @@ VTPM.get_contents ~self in
      let key = Tpm.key_of_swtpm path in
      let contents =
        Tpm.(contents |> deserialize |> update key empty_state |> serialize)
      in
      let* () = with_xapi ~cache @@ VTPM.set_contents ~self ~contents in
      Cohttp_lwt_unix.Server.respond ~status:`No_content
        ~body:Cohttp_lwt.Body.empty ()
  | _, _ ->
      let body = "Not allowed" in
      Cohttp_lwt_unix.Server.respond_string ~status:`Method_not_allowed ~body ()

(* Create a restricted RPC function and socket for a specific VM *)
let make_server_varstored _persist ~cache path vm_uuid =
  let vm_uuid_str = Uuidm.to_string vm_uuid in
  let module Server =
    Xapi_idl_guard_varstored.Interface.RPC_API (Rpc_lwt.GenServer ()) in
  let get_vm_ref () = with_xapi ~cache @@ VM.get_by_uuid ~uuid:vm_uuid_str in
  let ret v =
    (* TODO: maybe map XAPI exceptions *)
    Lwt.bind v Lwt.return_ok |> Rpc_lwt.T.put
  in
  let get_nvram _ _ =
    (let* self = get_vm_ref () in
     with_xapi ~cache @@ VM.get_NVRAM ~self
    )
    |> ret
  in
  let set_nvram _ _ nvram =
    (let* self = get_vm_ref () in
     with_xapi ~cache @@ VM.set_NVRAM_EFI_variables ~self ~value:nvram
    )
    |> ret
  in
  let message_create _ _name priority _cls _uuid body =
    ret
      (let* (_ : _ Ref.t) =
         with_xapi ~cache
         @@ Message.create ~name:"VM_SECURE_BOOT_FAILED" ~priority ~cls:`VM
              ~obj_uuid:vm_uuid_str ~body
       in
       Lwt.return_unit
      )
  in
  let get_by_uuid _ _ = ret @@ Lwt.return "DUMMYVM" in
  let dummy_login _ _ _ _ = ret @@ Lwt.return "DUMMYSESSION" in
  let dummy_logout _ = ret @@ Lwt.return_unit in
  Server.get_NVRAM get_nvram ;
  Server.set_NVRAM set_nvram ;
  Server.message_create message_create ;
  Server.session_login dummy_login ;
  Server.session_logout dummy_logout ;
  Server.get_by_uuid get_by_uuid ;
  serve_forever_lwt_callback (Rpc_lwt.server Server.implementation) path
  |> serve_forever_lwt path

let make_server_vtpm_rest read_write ~cache path vm_uuid =
  let mutex = Lwt_mutex.create () in
  let callback =
    serve_forever_lwt_callback_vtpm ~cache mutex read_write vm_uuid
  in
  serve_forever_lwt path callback
