(*
 *
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

open Xapi_guard
open Varstored_interface
open Lwt.Syntax

module D = Debug.Make (struct let name = "varstored-guard" end)

let ret v = Lwt.bind v Lwt.return_ok |> Rpc_lwt.T.put

let sockets = Hashtbl.create 127

module Persistent = struct
  type args = {
      vm_uuid: Varstore_privileged_interface.Uuidm.t
    ; path: string
    ; gid: int
  }
  [@@deriving rpcty]

  type t = args list [@@deriving rpcty]

  let saveto path data =
    let json = data |> Rpcmarshal.marshal typ_of |> Jsonrpc.to_string in
    Lwt_io.with_file ~mode:Lwt_io.Output path (fun ch -> Lwt_io.write ch json)

  let loadfrom path =
    let* exists = Lwt_unix.file_exists path in
    if exists then
      let* json =
        Lwt_io.with_file ~mode:Lwt_io.Input path (fun ch -> Lwt_io.read ch)
      in
      json |> Jsonrpc.of_string |> Rpcmarshal.unmarshal typ_of |> function
      | Ok result ->
          Lwt.return result
      | Error (`Msg m) ->
          Lwt.fail_with m
    else
      Lwt.return_nil
end

let recover_path = "/run/nonpersistent/varstored-guard-active.json"

let store_args sockets =
  Hashtbl.fold
    (fun path (_, (vm_uuid, gid)) acc -> {Persistent.vm_uuid; path; gid} :: acc)
    sockets []
  |> Persistent.saveto recover_path

let safe_unlink path =
  Lwt.catch
    (fun () -> Lwt_unix.unlink path)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit | e -> Lwt.fail e
      )

(* make_json doesn't work here *)
let rpc = Xen_api_lwt_unix.make "file:///var/lib/xcp/xapi"

let cache =
  SessionCache.create ~rpc ~login:Varstored_interface.login
    ~logout:Varstored_interface.logout

let () =
  Lwt_switch.add_hook (Some Varstored_interface.shutdown) (fun () ->
      D.debug "Cleaning up cache at exit" ;
      SessionCache.destroy cache
  )

let listen_for_vm {Persistent.vm_uuid; path; gid} =
  let vm_uuid_str = Uuidm.to_string vm_uuid in
  D.debug "resume: listening on socket %s for VM %s" path vm_uuid_str ;
  let* () = safe_unlink path in
  let* stop_server = make_server_rpcfn ~cache path vm_uuid_str in
  Hashtbl.add sockets path (stop_server, (vm_uuid, gid)) ;
  let* () = Lwt_unix.chmod path 0o660 in
  Lwt_unix.chown path 0 gid

let resume () =
  let* vms = Persistent.loadfrom recover_path in
  let+ () = Lwt_list.iter_p listen_for_vm vms in
  D.debug "resume completed"

(* caller here is trusted (xenopsd through message-switch *)
let depriv_create dbg vm_uuid gid path =
  if Hashtbl.mem sockets path then
    Lwt.return_error
      (Varstore_privileged_interface.InternalError
         (Printf.sprintf "Path %s is already in use" path)
      )
    |> Rpc_lwt.T.put
  else
    ret
    @@
    ( D.debug "[%s] creating deprivileged socket at %s, owned by group %d" dbg
        path gid ;
      let* () = listen_for_vm {Persistent.path; vm_uuid; gid} in
      store_args sockets
    )

let depriv_destroy dbg gid path =
  D.debug "[%s] stopping server for gid %d and path %s" dbg gid path ;
  ret
  @@
  match Hashtbl.find_opt sockets path with
  | None ->
      D.warn "[%s] asked to stop server for path %s, but it doesn't exist" dbg
        path ;
      Lwt.return_unit
  | Some (stop_server, _) ->
      let finally () =
        let+ () = safe_unlink path in
        Hashtbl.remove sockets path
      in
      let* () = Lwt.finalize stop_server finally in
      D.debug "[%s] stopped server for gid %d and removed socket" dbg gid ;
      Lwt.return_unit

let rpc_fn =
  let module Server = Varstore_privileged_interface.RPC_API (Rpc_lwt.GenServer ()) in
  (* bind APIs *)
  Server.create depriv_create ;
  Server.destroy depriv_destroy ;
  Rpc_lwt.server Server.implementation

let process body =
  let+ response =
    Dorpc.wrap_rpc Varstore_privileged_interface.E.error (fun () ->
        let call = Jsonrpc.call_of_string body in
        D.debug "Received request from message-switch, method %s" call.Rpc.name ;
        rpc_fn call
    )
  in
  Jsonrpc.string_of_response response

let make_message_switch_server () =
  let open Message_switch_lwt.Protocol_lwt in
  let wait_server, server_stopped = Lwt.task () in
  let* result =
    Server.listen ~process ~switch:!Xcp_client.switch_path
      ~queue:Varstore_privileged_interface.queue_name ()
  in
  match Server.error_to_msg result with
  | Ok t ->
      Lwt_switch.add_hook (Some shutdown) (fun () ->
          D.debug "Stopping message-switch queue server" ;
          let+ () = Server.shutdown ~t () in
          Lwt.wakeup server_stopped ()
      ) ;
      (* best effort resume *)
      let* () =
        Lwt.catch resume (fun e ->
            D.log_backtrace () ;
            D.warn "Resume failed: %s" (Printexc.to_string e) ;
            Lwt.return_unit
        )
      in
      wait_server
  | Error (`Msg m) ->
      Lwt.fail_with
        (Printf.sprintf "Failed to listen on message-switch queue: %s" m)

let main log_level =
  Debug.set_level log_level ;
  Debug.set_facility Syslog.Local5 ;

  let old_hook = !Lwt.async_exception_hook in
  (Lwt.async_exception_hook :=
     fun exn ->
       D.log_backtrace () ;
       D.error "Lwt caught async exception: %s" (Printexc.to_string exn) ;
       old_hook exn
  ) ;
  let () = Lwt_main.run @@ make_message_switch_server () in
  D.debug "Exiting varstored-guard"

let cmd =
  Cmdliner.(
    let info = Term.info "varstored-guard" ~exits:Term.default_exits in
    let log_level =
      let doc = "Syslog level. E.g. debug, info etc." in
      let level_conv =
        let parse s =
          try `Ok (Syslog.level_of_string s)
          with _ -> `Error (Format.sprintf "Unknown level: %s" s)
        in
        let print ppf level =
          Format.pp_print_string ppf (Syslog.string_of_level level)
        in
        (parse, print)
      in
      Arg.(
        value
        & opt level_conv Syslog.Info
        & info ["log-level"] ~docv:"LEVEL" ~doc
      )
    in

    let program = Term.(const main $ log_level) in
    (program, info)
  )

let () = Cmdliner.Term.(exit @@ eval cmd)
