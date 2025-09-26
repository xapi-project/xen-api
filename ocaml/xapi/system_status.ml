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

let task_label = "Retrieving system status"

module L = Debug.Make (struct let name = __MODULE__ end)

module Output = struct
  (** The output formats of xen-bugtool *)
  type t = Tar | TarBz2 | Zip

  let of_string = function
    | "tar" ->
        Some Tar
    | "tar.bz2" ->
        Some TarBz2
    | "zip" ->
        Some Zip
    | _ ->
        None

  let to_extension = function
    | Tar ->
        "tar"
    | TarBz2 ->
        "tar.bz2"
    | Zip ->
        "zip"

  let to_mime = function
    | Tar ->
        "appliation/x-tar"
    | TarBz2 ->
        "application/x-bzip2"
    | Zip ->
        "application/zip"
end

module Bugtool = struct
  let path = "/usr/sbin/xen-bugtool"

  let params_cp ~entries ~extension =
    [
      Printf.sprintf "--entries=%s" entries
    ; "--silent"
    ; "--yestoall"
    ; Printf.sprintf "--output=%s" extension
    ]

  let cmd_capabilities = Printf.sprintf "%s --capabilities" path

  let params_fd ~entries ~extension ~uuid =
    let params =
      params_cp ~entries ~extension @ [Printf.sprintf "--outfd=%s" uuid]
    in
    let cmd = String.concat " " (path :: params) in
    L.debug "%s: running %s" __FUNCTION__ cmd ;
    params

  let cmd_cp ~entries ~extension =
    let params = params_cp ~entries ~extension in
    let cmd = String.concat " " (path :: params) in
    L.debug "%s: running %s" __FUNCTION__ cmd ;
    cmd
end

let get_capabilities () = Helpers.get_process_output Bugtool.cmd_capabilities

(* This fn outputs xen-bugtool straight to the socket, only
   for tar output. It should work on embedded edition *)
let send_via_fd __context s entries output =
  let uuid = Uuidx.to_string (Uuidx.make ()) in
  let extension = Output.to_extension output in
  let content_type = Output.to_mime output in
  let params = Bugtool.params_fd ~entries ~extension ~uuid in
  let headers =
    Http.http_200_ok ~keep_alive:false ~version:"1.0" ()
    @ [
        "Server: " ^ Xapi_version.xapi_user_agent
      ; Http.Hdr.content_type ^ ": " ^ content_type
      ; Printf.sprintf {|%s: attachment; filename="system_status.%s"|}
          Http.Hdr.content_disposition extension
      ]
  in
  Http_svr.headers s headers ;
  let result =
    Forkhelpers.with_logfile_fd "get-system-status" (fun log_fd ->
        let pid =
          Forkhelpers.safe_close_and_exec None (Some log_fd) (Some log_fd)
            [(uuid, s)]
            Bugtool.path params
        in
        Forkhelpers.waitpid_fail_if_bad_exit pid
    )
  in
  match result with Success _ -> Ok () | Failure (log, exn) -> Error (log, exn)

(* This fn outputs xen-bugtool into a file and then write the
   file out to the socket, to deal with zipped bugtool outputs
   It will not work on embedded edition *)
let send_via_cp __context s entries output =
  let extension = Output.to_extension output in
  let content_type = Output.to_mime output in
  let cmd = Bugtool.cmd_cp ~entries ~extension in
  try
    let filepath = String.trim (Helpers.get_process_output cmd) in
    let filename = Filename.basename filepath in
    let hsts_time = !Xapi_globs.hsts_max_age in
    finally
      (fun () ->
        Http_svr.response_file ~mime_content_type:content_type ~hsts_time
          ~download_name:filename s filepath
      )
      (fun () ->
        Helpers.log_exn_continue "deleting xen-bugtool output" Unix.unlink
          filepath
      ) ;
    Ok ()
  with e -> Error ("(Not captured)", e)

let with_api_errors f ctx s entries output =
  match f ctx s entries output with
  | Ok () ->
      ()
  | Error (log, exn) ->
      L.debug "xen-bugtool failed with output: %s" log ;
      let msg = "xen-bugtool failed: " ^ Printexc.to_string exn in
      raise Api_errors.(Server_error (system_status_retrieval_failed, [msg]))

let handler (req : Request.t) s _ =
  req.Request.close <- true ;
  let get_param s = List.assoc_opt s req.Request.query in
  let entries = Option.value ~default:"" (get_param "entries") in
  let output =
    Option.bind (get_param "output") Output.of_string
    |> Option.value ~default:Output.Tar
  in
  Xapi_http.with_context task_label req s (fun __context ->
      if Helpers.on_oem ~__context && output <> Output.Tar then
        raise Api_errors.(Server_error (system_status_must_use_tar_on_oem, []))
      else if output = Output.Tar then
        with_api_errors send_via_fd __context s entries output
      else
        with_api_errors send_via_cp __context s entries output
  )
