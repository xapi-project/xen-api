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

open Lwt.Syntax
open Xapi_guard_server
module Types = Xapi_guard.Types
module SessionCache = Xen_api_lwt_unix.SessionCache

let daemon_name = "xapi-guard"

module D = Debug.Make (struct let name = daemon_name end)

let ret v = Lwt.bind v Lwt.return_ok |> Rpc_lwt.T.put

let log_fds () =
  let count stream = Lwt_stream.fold (fun _ n -> n + 1) stream 0 in
  let* has = Lwt_unix.file_exists "/proc/self/fd" in
  if has then (
    let* fds = Lwt_unix.files_of_directory "/proc/self/fd" |> count in
    D.info "file descriptors in use: %d" fds ;
    Lwt.return_unit
  ) else
    Lwt.return_unit

module Persistent = struct
  type args = {
      vm_uuid: Xapi_idl_guard_privileged.Interface.Uuidm.t
    ; path: string
    ; gid: int
    ; typ: Types.Service.t
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

let sockets = Hashtbl.create 127

let recover_path = "/run/nonpersistent/varstored-guard-active.json"

let store_args sockets =
  sockets
  |> Hashtbl.to_seq
  |> Seq.map (fun (path, (_, (vm_uuid, gid, typ))) ->
         Persistent.{vm_uuid; path; gid; typ}
     )
  |> List.of_seq
  |> Persistent.saveto recover_path

let safe_unlink path =
  Lwt.catch
    (fun () -> Lwt_unix.unlink path)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit | e -> Lwt.fail e
      )

let cache =
  Xen_api_lwt_unix.(
    SessionCache.create_uri ~switch:Server_interface.shutdown
      ~target:uri_local_json ~uname:"root" ~pwd:"" ~version:Xapi_version.version
      ~originator:Server_interface.originator ()
  )

let () =
  Lwt_switch.add_hook (Some Server_interface.shutdown) (fun () ->
      D.debug "Cleaning up cache at exit" ;
      Xen_api_lwt_unix.SessionCache.destroy cache
  )

let listen_for_vm {Persistent.vm_uuid; path; gid; typ} =
  let make_server =
    match typ with
    | Varstored ->
        Server_interface.make_server_varstored
    | Swtpm ->
        Server_interface.make_server_vtpm_rest
  in
  let vm_uuid_str = Uuidm.to_string vm_uuid in
  D.debug "%s: listening for %s on socket %s for VM %s" __FUNCTION__
    (Types.Service.to_string typ)
    path vm_uuid_str ;
  let* () = safe_unlink path in
  let* stop_server = make_server ~cache path vm_uuid_str in
  let* () = log_fds () in
  Hashtbl.add sockets path (stop_server, (vm_uuid, gid, typ)) ;
  let* () = Lwt_unix.chmod path 0o660 in
  Lwt_unix.chown path 0 gid

let resume () =
  let* vms = Persistent.loadfrom recover_path in
  let+ () = Lwt_list.iter_p listen_for_vm vms in
  D.debug "%s: completed" __FUNCTION__

(* caller here is trusted (xenopsd through message-switch) *)
let depriv_varstored_create dbg vm_uuid gid path =
  if Hashtbl.mem sockets path then
    Lwt.return_error
      (Xapi_idl_guard_privileged.Interface.InternalError
         (Printf.sprintf "Path %s is already in use" path)
      )
    |> Rpc_lwt.T.put
  else
    ret
    @@
    ( D.debug "[%s] creating deprivileged socket at %s, owned by group %d" dbg
        path gid ;
      let* () = listen_for_vm {Persistent.path; vm_uuid; gid; typ= Varstored} in
      store_args sockets
    )

let depriv_varstored_destroy dbg gid path =
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

let depriv_swtpm_create dbg vm_uuid gid path =
  if Hashtbl.mem sockets path then
    Lwt.return_error
      (Xapi_idl_guard_privileged.Interface.InternalError
         (Printf.sprintf "Path %s is already in use" path)
      )
    |> Rpc_lwt.T.put
  else
    ret
    @@
    ( D.debug "[%s] creating deprivileged socket at %s, owned by group %d" dbg
        path gid ;
      let* () = listen_for_vm {Persistent.path; vm_uuid; gid; typ= Swtpm} in
      store_args sockets
    )

let depriv_swtpm_destroy dbg gid path =
  D.debug "[%s] stopping server for gid %d and path %s" dbg gid path ;
  ret
  @@
  match Hashtbl.find_opt sockets path with
  | None ->
      D.warn "[%s] asked to swtpm stop server for path %s, but it doesn't exist"
        dbg path ;
      Lwt.return_unit
  | Some (stop_server, (_, _, Swtpm)) ->
      let finally () =
        let+ () = safe_unlink path in
        Hashtbl.remove sockets path
      in
      let* () = Lwt.finalize stop_server finally in
      D.debug "[%s] stopped swtpm server for gid %d and removed socket" dbg gid ;
      Lwt.return_unit
  | Some _ ->
      D.warn
        "[%s] asked to stop swtpm server for path %s, but it's not an swtpm \
         server"
        dbg path ;
      Lwt.return_unit

(* TODO: these 2 APIs need to be updated to go through the generic interface *)

let vtpm_set_contents dbg vtpm_uuid contents =
  let open Xen_api_lwt_unix in
  let open Lwt.Syntax in
  let uuid = Uuidm.to_string vtpm_uuid in
  D.debug "[%s] saving vTPM contents for %s" dbg uuid ;
  ret
  @@ let* self = Server_interface.with_xapi ~cache @@ VTPM.get_by_uuid ~uuid in
     Server_interface.with_xapi ~cache @@ VTPM.set_contents ~self ~contents

let vtpm_get_contents _dbg vtpm_uuid =
  let open Xen_api_lwt_unix in
  let open Lwt.Syntax in
  let uuid = Uuidm.to_string vtpm_uuid in
  ret
  @@ let* self = Server_interface.with_xapi ~cache @@ VTPM.get_by_uuid ~uuid in
     Server_interface.with_xapi ~cache @@ VTPM.get_contents ~self

let rpc_fn =
  let module Server =
    Xapi_idl_guard_privileged.Interface.RPC_API (Rpc_lwt.GenServer ()) in
  (* bind APIs *)
  Server.varstore_create depriv_varstored_create ;
  Server.varstore_destroy depriv_varstored_destroy ;
  Server.vtpm_create depriv_swtpm_create ;
  Server.vtpm_destroy depriv_swtpm_destroy ;
  Server.vtpm_set_contents vtpm_set_contents ;
  Server.vtpm_get_contents vtpm_get_contents ;
  Rpc_lwt.server Server.implementation

let process body =
  let+ response =
    Xapi_guard.Dorpc.wrap_rpc Xapi_idl_guard_privileged.Interface.E.error
      (fun () ->
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
      ~queue:Xapi_idl_guard_privileged.Interface.queue_name ()
  in
  match Server.error_to_msg result with
  | Ok t ->
      Lwt_switch.add_hook (Some Server_interface.shutdown) (fun () ->
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
  Debug.init_logs () ;

  let old_hook = !Lwt.async_exception_hook in
  (Lwt.async_exception_hook :=
     fun exn ->
       D.log_backtrace () ;
       D.error "Lwt caught async exception: %s" (Printexc.to_string exn) ;
       old_hook exn
  ) ;
  let () = Lwt_main.run @@ make_message_switch_server () in
  D.debug "Exiting %s" daemon_name

open! Cmdliner

let cmd =
  let info = Cmd.info daemon_name in
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
      value & opt level_conv Syslog.Info & info ["log-level"] ~docv:"LEVEL" ~doc
    )
  in

  let program = Term.(const main $ log_level) in
  Cmd.v info program

let () = exit (Cmdliner.Cmd.eval cmd)
