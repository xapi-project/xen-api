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

open Http
open Forkhelpers

module D = Debug.Make (struct let name = "xapi_vncsnapshot" end)

open D

let vncsnapshot = "/usr/bin/vncsnapshot"

let timeout = Mtime.Span.(30 * s)

let vncsnapshot_handler (req : Request.t) s _ =
  debug "vncshapshot handler running" ;
  Xapi_http.with_context "Taking snapshot of VM console" req s (fun __context ->
      try
        let console = Console.console_of_request __context req in
        Console.rbac_check_for_control_domain __context req console
          Rbac_static.permission_http_get_vncsnapshot_host_console
            .Db_actions.role_name_label ;
        let tmp = Filename.temp_file "snapshot" ".jpg" in
        let filename = Filename.basename tmp in
        Xapi_stdext_pervasives.Pervasiveext.finally
          (fun () ->
            match Console.address_of_console __context console with
            | None ->
                error "Failed to find the VNC console address" ;
                Http_svr.headers s (Http.http_404_missing ())
            | Some address ->
                let target =
                  match address with
                  | Console.Port port ->
                      [Printf.sprintf "127.0.0.1::%d" port]
                  | Console.Path path ->
                      ["-unix"; path]
                in
                let args =
                  ["-allowblank"; "-encodings"; "raw"] @ target @ [tmp]
                in
                ( try
                    let out, err =
                      execute_command_get_output ~timeout vncsnapshot args
                    in
                    debug "vncsnapshot succeeded (stdout=%S stderr=%S)" out err
                  with
                | Subprocess_timeout as e ->
                    error "vncsnapshot timed out after %s"
                      (Fmt.to_to_string Mtime.Span.pp timeout) ;
                    raise e
                | Spawn_internal_error (err, out, status) as e ->
                    let status =
                      match status with
                      | Unix.WEXITED n ->
                          Printf.sprintf "exited with code %d" n
                      | Unix.WSIGNALED n ->
                          Printf.sprintf "was killed by signal %d" n
                      | Unix.WSTOPPED n ->
                          Printf.sprintf "was stopped by signal %d" n
                    in
                    error "vncsnapshot %s (stdout=%S stderr=%S)" status out err ;
                    raise e
                ) ;
                let hsts_time = !Xapi_globs.hsts_max_age in
                Http_svr.response_file ~hsts_time s tmp ~download_name:filename
          )
          (fun () -> try Unix.unlink tmp with _ -> ())
      with e ->
        req.Request.close <- true ;
        raise e
  )
