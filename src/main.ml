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

let project_url = "http://github.com/djs55/xapi-nbd"

open Lwt
open Xen_api
open Xen_api_lwt_unix

let uri = ref "http://127.0.0.1/"

let with_block filename f =
  let open Lwt in
  Block.connect filename
  >>= function
  | `Error _ -> fail (Failure (Printf.sprintf "Unable to read %s" filename))
  | `Ok x ->
    Lwt.catch (fun () -> f x) (fun e -> Block.disconnect x >>= fun () -> fail e)

let handle_connection fd =
  let rpc = make !uri in
  let channel = Nbd_lwt_channel.of_fd fd in
  Nbd_lwt_server.connect channel ()
  >>= fun (name, t) ->
  let uri = Uri.of_string name in
  ( match Uri.user uri, Uri.password uri, Uri.get_query_param uri "session_id" with
    | _, _, Some x ->
      (* Validate the session *)
      Session.get_uuid rpc x x
      >>= fun _ ->
      return (x, false)
    | Some user, Some password, _ ->
      Session.login_with_password rpc user password "1.0"
      >>= fun session_id ->
      return (session_id, true)
  ) >>= fun (session_id, need_to_logout) ->
  ( try_lwt
      let path = Uri.path uri in (* note preceeding / *)
      let vdi_uuid = if path <> "" then String.sub path 1 (String.length path - 1) else path in
      (* FIXME: attach VDI *)
      let filename = "/dev/null" in
      with_block filename (Nbd_lwt_server.serve t (module Block))
    finally
      if need_to_logout
      then Session.logout rpc session_id
      else return ()
  )

let main port =
  let t =
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
    let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
    Lwt_unix.bind sock sockaddr;
    Lwt_unix.listen sock 5;
    while_lwt true do
      Lwt_unix.accept sock
      >>= fun (fd, _) ->
      (* Background thread per connection *)
      let _ =
        Lwt.catch
          (fun () -> handle_connection fd)
          (fun e -> Printf.fprintf stderr "Caught %s\n%!" (Printexc.to_string e); return ())
        >>= fun () ->
        Lwt_unix.close fd in
      return ()
    done in
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
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

let cmd =
  let doc = "Expose VDIs over authenticated NBD connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Expose all accessible VDIs over NBD. Every VDI is addressible through a URI, where the URI will be authenticated by xapi.";
  ] @ help in
  let port =
    let doc = "Local port to listen for connections on" in
    Arg.(value & opt int 10809 & info [ "port" ] ~doc) in
  Term.(ret (pure main $ port)),
  Term.info "xapi-nbd" ~version:"1.0.0" ~doc ~man ~sdocs:_common_options

let _ =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
