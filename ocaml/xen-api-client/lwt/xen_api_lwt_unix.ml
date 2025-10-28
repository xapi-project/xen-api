(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

open Xen_api
open Lwt
open Lwt.Syntax

module Lwt_unix_IO = struct
  type 'a t = 'a Lwt.t

  let ( >>= ) = Lwt.bind

  let return = Lwt.return

  let ( >> ) m n = m >>= fun _ -> n

  type ic = (unit -> unit Lwt.t) * Lwt_io.input_channel

  type oc = (unit -> unit Lwt.t) * Lwt_io.output_channel

  type conn = Lwt_unix.file_descr

  let read_line (_, ic) = Lwt_io.read_line_opt ic

  let read (_, ic) count =
    Lwt.catch
      (fun () -> Lwt_io.read ~count ic)
      (function End_of_file -> return "" | e -> Lwt.fail e)

  (* let read_exactly (_, ic) buf off len =
       Lwt.catch
         (fun () -> Lwt_io.read_into_exactly ic buf off len >> return true)
         (function
           | End_of_file -> return false
           | e -> Lwt.fail e)

     let read_exactly ic len =
       let buf = Bytes.create len in
       read_exactly ic buf 0 len >>= function
       | true -> return (Some buf)
       | false -> return None *)

  let write (_, oc) = Lwt_io.write oc

  (* let write_line (_, oc) = Lwt_io.write_line oc *)

  let flush (_, oc) = Lwt_io.flush oc

  let close ((close1, _), (close2, _)) = close1 () >> close2 ()

  let sslctx =
    Ssl.init () ;
    Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context

  let open_connection uri =
    ( match Uri.scheme uri with
    | Some "file" ->
        return (Unix.PF_UNIX, Unix.ADDR_UNIX (Uri.path_unencoded uri), false)
    | Some "http+unix" ->
        return (Unix.PF_UNIX, Unix.ADDR_UNIX (Uri.host_with_default uri), false)
    | Some "http" | Some "https" ->
        Uri_util.sockaddr_of_uri uri >|= fun (sockaddr, ssl) ->
        (Unix.domain_of_sockaddr sockaddr, sockaddr, ssl)
    | Some x ->
        fail (Unsupported_scheme x)
    | None ->
        fail (Unsupported_scheme "")
    )
    >>= fun (domain, sockaddr, ssl) ->
    if ssl then
      let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
      Lwt.catch
        (fun () ->
          Lwt.catch
            (fun () -> Lwt_unix.connect fd sockaddr)
            (fun e -> Lwt_unix.close fd >>= fun () -> Lwt.fail e)
          >>= fun () ->
          Lwt_ssl.ssl_connect fd sslctx >>= fun sock ->
          let ic = Lwt_ssl.in_channel_of_descr sock in
          let oc = Lwt_ssl.out_channel_of_descr sock in
          return (Ok ((return, ic), ((fun () -> Lwt_ssl.close sock), oc)))
        )
        (fun e -> return (Error e))
    else
      let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
      Lwt.catch
        (fun () ->
          Lwt.catch
            (fun () -> Lwt_unix.connect fd sockaddr)
            (fun e -> Lwt_unix.close fd >>= fun () -> Lwt.fail e)
          >>= fun () ->
          let ic = Lwt_io.of_fd ~close:return ~mode:Lwt_io.input fd in
          let oc =
            Lwt_io.of_fd
              ~close:(fun () -> Lwt_unix.close fd)
              ~mode:Lwt_io.output fd
          in
          return
            (Ok
               ( ((fun () -> Lwt_io.close ic), ic)
               , ((fun () -> Lwt_io.close oc), oc)
               )
            )
        )
        (fun e -> return (Error e))

  let sleep = Lwt_unix.sleep

  let gettimeofday = Unix.gettimeofday
end

module M = Make (Lwt_unix_IO)

let exn_to_string = function
  | Api_errors.Server_error (code, params) ->
      Printf.sprintf "%s %s" code (String.concat " " params)
  | e ->
      Printexc.to_string e

let do_it uri string =
  let connection = M.make uri in
  Lwt.finalize
    (fun () ->
      M.rpc connection string >>= fun result ->
      match result with
      | Ok x ->
          return x
      | Error e ->
          Printf.fprintf stderr "Caught: %s\n%!" (exn_to_string e) ;
          fail e
    )
    (fun () -> M.disconnect connection)

(* TODO: modify do_it to accept the timeout and remove the warnings *)

[@@@ocaml.warning "-27"]

let make ?(timeout = 30.) uri call =
  let string = Xmlrpc.string_of_call call in
  do_it uri string >>= fun result ->
  Lwt.return (Xmlrpc.response_of_string result)

[@@@ocaml.warning "-27"]

let make_json ?(timeout = 30.) uri call =
  let string = Jsonrpc.string_of_call call in
  do_it uri string >>= fun result ->
  Lwt.return (Jsonrpc.response_of_string result)

let uri_local_json =
  Uri.make ~scheme:"http+unix" ~host:"/var/lib/xcp/xapi" ~path:"/jsonrpc" ()

(* TODO: https *)
let uri_ip_json ip = Uri.make ~scheme:"https" ~host:ip ~path:"/jsonrpc" ()

module Client = Client.ClientF (Lwt)
include Client

module SessionCache = struct
  type session = {session_id: API.ref_session; mutable valid: bool}

  type t = {
      session_pool: session Lwt_pool.t
    ; mutable rpc: Rpc.call -> Rpc.response Lwt.t
    ; timeout: float option
  }

  let make_rpc ?timeout target =
    let uri = Uri.with_path target "/jsonrpc" in
    make_json ?timeout uri

  let create_rpc ?timeout rpc ~uname ~pwd ~version ~originator () =
    let acquire () =
      let+ session_id =
        Session.login_with_password ~rpc ~uname ~pwd ~version ~originator
      in
      {session_id; valid= true}
    in
    let dispose t =
      if t.valid then
        Lwt.catch
          (fun () -> Session.logout ~rpc ~session_id:t.session_id)
          (function
            | Api_errors.Server_error (code, _)
              when code = Api_errors.session_invalid ->
                (* ignore logout failures on invalid sessions: these may have been GCed *)
                Lwt.return_unit
            | e ->
                Lwt.fail e
            )
      else
        Lwt.return_unit
    in
    let check t is_ok = is_ok t.valid in
    let validate t = Lwt.return t.valid in
    let session_pool = Lwt_pool.create 1 ~validate ~dispose ~check acquire in
    (* Try acquiring one session to give error messages early *)
    {rpc; session_pool; timeout}

  let destroy t = Lwt_pool.clear t.session_pool

  let create_uri ?timeout ~switch ~target ~uname ~pwd ~version ~originator () =
    let t =
      create_rpc (make_rpc ?timeout target) ~uname ~pwd ~version ~originator ()
    in
    Lwt_switch.add_hook (Some switch) (fun () -> destroy t) ;
    t

  let with_session t f =
    let rec retry n =
      (* we want to use the same session for multiple API calls concurrently *)
      let* session = Lwt_pool.use t.session_pool Lwt.return in
      Lwt.catch
        (fun () -> f ~rpc:t.rpc ~session_id:session.session_id)
        (function
          | Api_errors.Server_error (code, _) as e
            when code = Api_errors.session_invalid ->
              session.valid <- false ;
              (* the [check] function above will cause Lwt_pool to dispose of this *)
              if n > 0 then
                retry (n - 1)
              else
                Lwt.fail e
          | Api_errors.Server_error (code, [master]) as e
            when code = Api_errors.host_is_slave ->
              (* can happen with HA *)
              t.rpc <- make_rpc ?timeout:t.timeout @@ uri_ip_json master ;
              if n > 0 then
                retry (n - 1)
              else
                Lwt.fail e
          | e ->
              Lwt.fail e
          )
    in
    retry 2
end
