(*
 * Copyright (C) 2015 Citrix Inc
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

open Lwt.Infix
(* Xapi external interfaces: *)
module Xen_api = Xen_api_lwt_unix

let ignore_exn_delayed t () = Lwt.catch t (fun _ -> Lwt.return_unit)
let ignore_exn_log_error = Cleanup.ignore_exn_log_error

(* TODO share these "require" functions with the nbd package. *)
let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let require_str name arg =
  require name (if arg = "" then None else Some arg)

let with_attached_vdi vDI rpc session_id f =
  Lwt_log.notice "Looking up control domain UUID in xensource inventory" >>= fun () ->
  Inventory.inventory_filename := Consts.xensource_inventory_filename;
  let control_domain_uuid = Inventory.lookup Inventory._control_domain_uuid in
  Lwt_log.notice "Found control domain UUID" >>= fun () ->
  Xen_api.VM.get_by_uuid ~rpc ~session_id ~uuid:control_domain_uuid
  >>= fun control_domain ->
  Cleanup.VBD.with_vbd ~vDI ~vM:control_domain ~mode:`RO ~rpc ~session_id (fun vbd ->
      Xen_api.VBD.get_device ~rpc ~session_id ~self:vbd
      >>= fun device ->
      f ("/dev/" ^ device))

let handle_connection fd tls_role =

  let with_session rpc uri f =
    ( match Uri.get_query_param uri "session_id" with
      | Some session_str ->
        (* Validate the session *)
        let session_id = API.Ref.of_string session_str in
        Xen_api.Session.get_uuid ~rpc ~session_id ~self:session_id
        >>= fun _ ->
        Lwt.return session_id
      | None ->
        Lwt.fail_with "No session_id parameter provided"
    ) >>= fun session_id ->
    f uri rpc session_id
  in


  let serve t uri rpc session_id =
    let path = Uri.path uri in (* note preceeding / *)
    let vdi_uuid = if path <> "" then String.sub path 1 (String.length path - 1) else path in
    Xen_api.VDI.get_by_uuid ~rpc ~session_id ~uuid:vdi_uuid
    >>= fun vdi_ref ->
    with_attached_vdi vdi_ref rpc session_id
      (fun filename ->
         Cleanup.Block.with_block filename (Nbd_lwt_unix.Server.serve t ~read_only:true (module Block))
      )
  in

  Nbd_lwt_unix.with_channel fd tls_role
    (fun channel ->
       Nbd_lwt_unix.Server.with_connection channel
         (fun export_name svr ->
           let rpc = Xen_api.make Consts.xapi_unix_domain_socket_uri in
           let uri = Uri.of_string export_name in
           with_session rpc uri (serve svr)
         )
    )

(* TODO use the version from nbd repository *)
let init_tls_get_server_ctx ~certfile ~ciphersuites =
  let certfile = require_str "certfile" certfile in
  let ciphersuites = require_str "ciphersuites" ciphersuites in
  Some (Nbd_lwt_unix.TlsServer
    (Nbd_lwt_unix.init_tls_get_ctx ~certfile ~ciphersuites)
  )

let xapi_says_use_tls () =
  let refuse log msg = (
    log msg >>=
    (fun () -> Lwt.fail_with msg)
  ) in
  let ask_xapi rpc session_id =
    Xen_api.Network.get_all_records ~rpc ~session_id >>=
    fun all_nets ->
    let all_porpoises = List.map (fun (_str, net) -> net.API.network_purpose) all_nets |>
    List.flatten in
    let tls = List.mem `nbd all_porpoises in
    let no_tls = List.mem `insecure_nbd all_porpoises in
    match tls, no_tls with
    | true, true -> refuse Lwt_log.error "Contradictory XenServer configuration: nbd and insecure_nbd network purposes! Refusing connection."
    | true, false -> Lwt.return true
    | false, true -> Lwt.return false
    | false, false -> refuse Lwt_log.warning "Refusing connection: no network has purpose nbd or insecure_nbd:"
  in
  Local_xapi_session.with_session ask_xapi

let main port certfile ciphersuites =
  let t () =
    Lwt_log.notice_f "Starting xapi-nbd: port = '%d'; certfile = '%s'; ciphersuites = '%s'" port certfile ciphersuites >>= fun () ->
    (* We keep a persistent record of the VBDs that we've created but haven't
       yet cleaned up. At startup we go through this list in case some VBDs
       got leaked after the previous run due to a crash and clean them up. *)
    Cleanup.Persistent.cleanup () >>= fun () ->
    Lwt_log.notice "Initialising TLS" >>= fun () ->
    let tls_server_role = init_tls_get_server_ctx ~certfile ~ciphersuites in
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt.finalize
      (fun () ->
         Lwt_log.notice "Setting up server socket" >>= fun () ->
         Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
         let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
         Lwt_unix.bind sock sockaddr;
         Lwt_unix.listen sock 5;
         Lwt_log.notice "Listening for incoming connections" >>= fun () ->

         let conn_count = ref 0 in
         let conn_m = Lwt_mutex.create () in
         let inc_conn ?(i=1) () = Lwt_mutex.with_lock conn_m (fun () ->
           conn_count := !conn_count + i;
           if !conn_count > Consts.connection_limit && i > 0
           then Lwt.fail_with ("Server busy: already at maximum "^(string_of_int Consts.connection_limit)^" connections.")
           else Lwt.return ()
         ) in
         let dec_conn () = inc_conn ~i:(-1) () in

         let rec loop () =
           Lwt_unix.accept sock
           >>= fun (fd, _) ->
           Lwt_log.notice "Got new client" >>= fun () ->
           (* Background thread per connection *)
           let _ =
             ignore_exn_log_error "Caught exception while handling client"
               (fun () ->
                  Lwt.finalize
                    (fun () -> (
                      inc_conn () >>=
                      xapi_says_use_tls >>=
                      fun tls -> (
                        let tls_role = if tls then tls_server_role else None in
                        handle_connection fd tls_role)
                      )
                    )
                    (* ignore the exception resulting from double-closing the socket *)
                    (fun () ->
                      ignore_exn_delayed (fun () -> Lwt_unix.close fd) () >>=
                      dec_conn
                    )
               )
           in
           loop ()
         in
         loop ()
      )
      (ignore_exn_delayed (fun () -> Lwt_unix.close sock))
  in
  (* Log unexpected exceptions *)
  Lwt_main.run
    (Lwt.catch t
       (fun e ->
          Lwt_log.fatal_f "Caught unexpected exception: %s" (Printexc.to_string e) >>= fun () ->
          Lwt.fail e
       )
    );

  `Ok ()

open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [
  `S _common_options;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
  `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" Consts.project_url);
]

let certfile =
  let doc = "Path to file containing TLS certificate." in
  Arg.(value & opt string "" & info ["certfile"] ~doc)
let ciphersuites =
  let doc = "Set of ciphersuites for TLS (specified in the format accepted by OpenSSL, stunnel etc.)" in
  Arg.(value & opt string "!EXPORT:RSA+AES128-SHA256" & info ["ciphersuites"] ~doc)

let cmd =
  let doc = "Expose VDIs over authenticated NBD connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Expose all accessible VDIs over NBD. Every VDI is addressible through a URI, where the URI will be authenticated by xapi.";
  ] @ help in
  (* TODO for port, certfile, ciphersuites: use definitions from nbd repository. *)
  (* But consider making ciphersuites mandatory here in a local definition. *)
  let port =
    let doc = "Local port to listen for connections on" in
    Arg.(value & opt int Consts.standard_nbd_port & info [ "port" ] ~doc) in
  Term.(ret (pure main $ port $ certfile $ ciphersuites)),
  Term.info "xapi-nbd" ~version:"1.0.0" ~doc ~man ~sdocs:_common_options

let setup_logging () =
  Lwt_log.default := Lwt_log.syslog ~facility:`Daemon ();
  (* Display all log messages of level "notice" and higher (this is the default Lwt_log behaviour) *)
  Lwt_log.add_rule "*" Lwt_log.Notice

let () =
  (* We keep track of the VBDs we've created but haven't yet cleaned up, and
     when we receive a SIGTERM or SIGINT signal, we clean up these leftover
     VBDs first and then fail with an exception. *)
  Cleanup.Runtime.register_signal_handler ();
  setup_logging ();
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
