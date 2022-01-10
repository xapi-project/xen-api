(*
 * Copyright (C) Citrix Inc
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
module Xen_api = Xen_api_lwt_unix

let wait_for_xapi_and_login () =
  let rpc = Xen_api.make Consts.xapi_unix_domain_socket_uri in
  let rec loop () =
    Lwt.catch
      (fun () ->
        Xen_api.Session.login_with_password ~rpc ~uname:"" ~pwd:""
          ~version:"1.0" ~originator:"xapi-nbd")
      (fun e ->
        Lwt_log.warning_f
          "Failed to log in via xapi's Unix domain socket: %s; retrying in %f \
           seconds"
          (Printexc.to_string e) Consts.wait_for_xapi_retry_delay_seconds
        >>= fun () ->
        Lwt_unix.sleep Consts.wait_for_xapi_retry_delay_seconds >>= fun () ->
        loop ())
  in
  let timeout () =
    let timeout_s = Consts.wait_for_xapi_timeout_seconds in
    Lwt_unix.sleep timeout_s >>= fun () ->
    let msg =
      Printf.sprintf
        "Failed to log in via xapi's Unix domain socket in %f seconds" timeout_s
    in
    Lwt_log.fatal msg >>= fun () -> Lwt.fail_with msg
  in
  Lwt_log.notice_f
    "Will try to log in via xapi's Unix domain socket for %f seconds"
    Consts.wait_for_xapi_timeout_seconds
  >>= fun () ->
  Lwt.pick [loop (); timeout ()] >|= fun session_id -> (rpc, session_id)

let with_session f =
  wait_for_xapi_and_login () >>= fun (rpc, session_id) ->
  Lwt.finalize
    (fun () -> f rpc session_id)
    (fun () -> Xen_api.Session.logout ~rpc ~session_id)
