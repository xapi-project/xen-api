(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module Request = Http.Request

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let content_type = "application/data"

let xen_bugtool = "/usr/sbin/xen-bugtool"

let task_label = "Retrieving system status"

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let get_capabilities () =
  let cmd = Printf.sprintf "%s --capabilities" xen_bugtool in
  Helpers.get_process_output cmd

(* This fn outputs xen-bugtool straight to the socket, only
   for tar output. It should work on embedded edition *)
let send_via_fd __context s entries output =
  let s_uuid = Uuidx.to_string (Uuidx.make ()) in
  let params =
    [
      Printf.sprintf "--entries=%s" entries
    ; "--silent"
    ; "--yestoall"
    ; Printf.sprintf "--output=%s" output
    ; "--outfd=" ^ s_uuid
    ]
  in
  let cmd = Printf.sprintf "%s %s" xen_bugtool (String.concat " " params) in
  debug "running %s" cmd ;
  try
    let headers =
      Http.http_200_ok ~keep_alive:false ~version:"1.0" ()
      @ [
          "Server: " ^ Xapi_version.xapi_user_agent
        ; Http.Hdr.content_type ^ ": " ^ content_type
        ; "Content-Disposition: attachment; filename=\"system_status.tgz\""
        ]
    in
    Http_svr.headers s headers ;
    let result =
      Forkhelpers.with_logfile_fd "get-system-status" (fun log_fd ->
          let pid =
            Forkhelpers.safe_close_and_exec None (Some log_fd) (Some log_fd)
              [(s_uuid, s)]
              xen_bugtool params
          in
          Forkhelpers.waitpid_fail_if_bad_exit pid
      )
    in
    match result with
    | Success _ ->
        debug "xen-bugtool exited successfully"
    | Failure (log, exn) ->
        debug "xen-bugtool failed with output: %s" log ;
        raise exn
  with e ->
    let msg = "xen-bugtool failed: " ^ Printexc.to_string e in
    error "%s" msg ;
    raise
      (Api_errors.Server_error (Api_errors.system_status_retrieval_failed, [msg])
      )

(* This fn outputs xen-bugtool into a file and then write the
   file out to the socket, to deal with zipped bugtool outputs
   It will not work on embedded edition *)
let send_via_cp __context s entries output =
  let cmd =
    Printf.sprintf "%s --entries=%s --silent --yestoall --output=%s" xen_bugtool
      entries output
  in
  let () = debug "running %s" cmd in
  try
    let filepath = String.trim (Helpers.get_process_output cmd) in
    let filename = Filename.basename filepath in
    let hsts_time = !Xapi_globs.hsts_max_age in
    finally
      (fun () ->
        debug "bugball path: %s" filepath ;
        Http_svr.response_file ~mime_content_type:content_type ~hsts_time
          ~download_name:filename s filepath
      )
      (fun () ->
        Helpers.log_exn_continue "deleting xen-bugtool output" Unix.unlink
          filepath
      )
  with e ->
    let msg = "xen-bugtool failed: " ^ ExnHelper.string_of_exn e in
    error "%s" msg ;
    raise Api_errors.(Server_error (system_status_retrieval_failed, [msg]))

let handler (req : Request.t) s _ =
  debug "In system status http handler..." ;
  req.Request.close <- true ;
  let get_param s = try List.assoc s req.Request.query with _ -> "" in
  let entries = get_param "entries" in
  let output = get_param "output" in
  let () = debug "session_id: %s" (get_param "session_id") in
  Xapi_http.with_context task_label req s (fun __context ->
      if Helpers.on_oem ~__context && output <> "tar" then
        raise Api_errors.(Server_error (system_status_must_use_tar_on_oem, []))
      else if output = "tar" then
        send_via_fd __context s entries output
      else
        send_via_cp __context s entries output
  )
