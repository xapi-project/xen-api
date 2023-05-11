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

type session = [`session] Ref.t

type rpc = call -> response Lwt.t

open Xen_api_lwt_unix

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
      Cohttp_lwt_unix.Server.respond_string ~status:`Method_not_allowed ~body ()

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
