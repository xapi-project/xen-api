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

module D = Debug.Make(struct let name="xapi" end)
open D

let vncsnapshot = "/usr/bin/vncsnapshot"


let vncsnapshot_handler (req: Request.t) s _ =
  debug "vncshapshot handler running";
  Xapi_http.with_context "Taking snapshot of VM console" req s
    (fun __context ->
       try
         let console = Console.console_of_request __context req in
         Console.rbac_check_for_control_domain __context req console
           Rbac_static.permission_http_get_vncsnapshot_host_console.Db_actions.role_name_label;
         let tmp = Filename.temp_file "snapshot" "jpg" in
         Stdext.Pervasiveext.finally
           (fun () ->
              let vnc_port = Int64.to_int (Db.Console.get_port ~__context ~self:console) in

              let pid = safe_close_and_exec None None None [] vncsnapshot
                  [ "-quiet"; "-allowblank" ; "-encodings"; "\"raw\"";
                    Printf.sprintf "%s:%d" "127.0.0.1" (vnc_port-5900); tmp ] in
              waitpid_fail_if_bad_exit pid;
              Http_svr.response_file s tmp
           )
           (fun () -> try Unix.unlink tmp with _ -> ())
       with e ->
         req.Request.close <- true;
         raise e
    )
