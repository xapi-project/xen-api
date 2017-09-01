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

open Lwt
(* Xapi external interfaces: *)
module Xen_api = Xen_api_lwt_unix

(* Xapi internal interfaces: *)
module SM = Storage_interface.ClientM(struct
    type 'a t = 'a Lwt.t
    let fail, return, bind = Lwt.(fail, return, bind)

    let (>>*=) m f = m >>= function
      | `Ok x -> f x
      | `Error e ->
        let b = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer b in
        Protocol_lwt.Client.pp_error fmt e;
        Format.pp_print_flush fmt ();
        fail (Failure (Buffer.contents b))

    (* A global connection for the lifetime of this process *)
    let switch =
      Protocol_lwt.Client.connect ~switch:!Xcp_client.switch_path ()
      >>*= fun switch ->
      return switch

    let rpc call =
      switch >>= fun switch ->
      Protocol_lwt.Client.rpc ~t:switch ~queue:!Storage_interface.queue_name ~body:(Jsonrpc.string_of_call call) ()
      >>*= fun result ->
      return (Jsonrpc.response_of_string result)
  end)

let capture_exception f x =
  Lwt.catch
    (fun () -> f x >>= fun r -> return (`Ok r))
    (fun e -> return (`Error e))

let release_exception = function
  | `Ok x -> return x
  | `Error e -> fail e

let with_block filename f =
  let open Lwt in
  Block.connect filename
  >>= function
  | `Error _ -> fail (Failure (Printf.sprintf "Unable to read %s" filename))
  | `Ok x ->
    capture_exception f x
    >>= fun r ->
    Block.disconnect x
    >>= fun () ->
    release_exception r

let with_attached_vdi sr vdi read_write f =
  let pid = Unix.getpid () in
  let connection_uuid = Uuidm.v `V4 |> Uuidm.to_string in
  let datapath_id = Printf.sprintf "xapi-nbd/%s/%s/%d" vdi connection_uuid pid in
  let dbg = Printf.sprintf "xapi-nbd:with_attached_vdi/%s" datapath_id in
  SM.DP.create ~dbg ~id:datapath_id
  >>= fun dp ->
  SM.VDI.attach ~dbg ~dp ~sr ~vdi ~read_write
  >>= fun attach_info ->
  SM.VDI.activate ~dbg ~dp ~sr ~vdi
  >>= fun () ->
  capture_exception f attach_info.Storage_interface.params
  >>= fun r ->
  SM.DP.destroy ~dbg ~dp ~allow_leak:true
  >>= fun () ->
  release_exception r


let ignore_exn t () = Lwt.catch t (fun _ -> Lwt.return_unit)

let handle_connection fd =

  let with_session rpc uri f =
    ( match Uri.get_query_param uri "session_id" with
      | Some session_id ->
        (* Validate the session *)
        Xen_api.Session.get_uuid ~rpc ~session_id ~self:session_id
        >>= fun _ ->
        return session_id
      | None ->
        fail (Failure "No session_id parameter provided")
    ) >>= fun session_id ->
    f uri rpc session_id
  in


  let serve t uri rpc session_id =
    let path = Uri.path uri in (* note preceeding / *)
    let vdi_uuid = if path <> "" then String.sub path 1 (String.length path - 1) else path in
    Xen_api.VDI.get_by_uuid ~rpc ~session_id ~uuid:vdi_uuid
    >>= fun vdi_ref ->
    Xen_api.VDI.get_record ~rpc ~session_id ~self:vdi_ref
    >>= fun vdi_rec ->
    Xen_api.SR.get_uuid ~rpc ~session_id ~self:vdi_rec.API.vDI_SR
    >>= fun sr_uuid ->
    with_attached_vdi sr_uuid vdi_rec.API.vDI_location (not vdi_rec.API.vDI_read_only)
      (fun filename ->
         with_block filename (Nbd_lwt_unix.Server.serve t (module Block))
      )
  in

  let channel = Nbd_lwt_unix.of_fd fd in
  Lwt.finalize
    (fun () ->
       Nbd_lwt_unix.Server.connect channel ()
       >>= fun (export_name, t) ->
       Lwt.finalize
         (fun () ->
            let rpc = Xen_api.make Consts.xapi_unix_domain_socket_uri in
            let uri = Uri.of_string export_name in
            with_session rpc uri (serve t)
         )
         (fun () -> Nbd_lwt_unix.Server.close t)
    )
    (* ignore the exception resulting from double-closing the socket *)
    (ignore_exn channel.close)

let main port =
  let t =
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt.finalize
      (fun () ->
         Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
         let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
         Lwt_unix.bind sock sockaddr;
         Lwt_unix.listen sock 5;
         let rec loop () =
           Lwt_unix.accept sock
           >>= fun (fd, _) ->
           (* Background thread per connection *)
           let _ =
             Lwt.catch
               (fun () ->
                  Lwt.finalize
                    (fun () -> handle_connection fd)
                    (* ignore the exception resulting from double-closing the socket *)
                    (ignore_exn (fun () -> Lwt_unix.close fd))
               )
               (fun e -> Lwt_io.eprintf "Caught %s\n%!" (Printexc.to_string e))
           in
           loop ()
         in
         loop ()
      )
      (ignore_exn (fun () -> Lwt_unix.close sock))
  in
  Lwt_main.run t;

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

let cmd =
  let doc = "Expose VDIs over authenticated NBD connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Expose all accessible VDIs over NBD. Every VDI is addressible through a URI, where the URI will be authenticated by xapi.";
  ] @ help in
  let port =
    let doc = "Local port to listen for connections on" in
    Arg.(value & opt int Consts.standard_nbd_port & info [ "port" ] ~doc) in
  Term.(ret (pure main $ port)),
  Term.info "xapi-nbd" ~version:"1.0.0" ~doc ~man ~sdocs:_common_options

let _ =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
