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
(**
 * @group Virtual-Machine Management
*)

open Stdext
open Pervasiveext
open Client
open Forkhelpers
open Xapi_templates
open Attach_helpers

module D = Debug.Make(struct let name="xapi" end)
open D

let is_whitelisted script =
  let safe_char = function 'a'..'z'-> true |'-'->true |'/'->true |_ -> false in
  let safe_str str = List.fold_left (&&) true (List.map safe_char (Xstringext.String.explode str)) in
  (* make sure the script prefix is the allowed dom0 directory *)
  (Filename.dirname script = !Xapi_globs.post_install_scripts_dir)
  (* avoid ..-style attacks and other weird things *)
  &&(safe_str script)
let assert_script_is_whitelisted script =
  if not (is_whitelisted script) then
    raise (Api_errors.Server_error (Api_errors.permission_denied, [
        (Printf.sprintf "illegal provision script %s" script)]))

(** Execute the post install script of 'vm' having attached all the vbds to the 'install_vm' *)
let post_install_script rpc session_id __context install_vm vm (script, vbds) =
  (* Cancellable task *)
  TaskHelper.set_cancellable ~__context;

  let refresh_session = Xapi_session.consider_touching_session rpc session_id in

  match script with
  | None -> () (* nothing to do *)
  | Some script ->
    assert_script_is_whitelisted script;
    let vdis = List.map (fun self -> Client.VBD.get_VDI rpc session_id self) vbds in
    with_vbds rpc session_id __context install_vm vdis `RW
      (fun install_vm_vbds ->
         let devices = List.map
             (fun (install_vm_vbd, vbd) ->
                let hvm = Client.VM.get_domain_type rpc session_id vm = `hvm in
                let device = Vbdops.translate_vbd_device vbd (Client.VBD.get_userdevice rpc session_id vbd) hvm in
                Device_number.to_linux_device device,
                "/dev/" ^ (Client.VBD.get_device rpc session_id install_vm_vbd)) (List.combine install_vm_vbds vbds) in
         let env = ("vm", Ref.string_of vm) :: devices in
         let env = List.map (fun (k, v) -> k ^ "=" ^ v) env in
         debug "Executing script %s with env %s" script (String.concat "; " env);

         match with_logfile_fd "install-log"
                 (fun log ->
                    let pid = safe_close_and_exec ~env:(Array.of_list env) None (Some log) (Some log) [] script [] in
                    let starttime = Unix.time () in
                    let rec update_progress () =
                      (* Check for cancelling *)
                      if TaskHelper.is_cancelling ~__context
                      then
                        begin
                          Unix.kill (Forkhelpers.getpid pid) Sys.sigterm;
                          let _ = Forkhelpers.waitpid pid in
                          TaskHelper.raise_cancelled ~__context
                        end;

                      let (newpid,status) = Forkhelpers.waitpid_nohang pid in
                      if newpid <> 0
                      then
                        (match status with
                         | Unix.WEXITED 0 -> (newpid,status)
                         | (Unix.WEXITED n|Unix.WSIGNALED n|Unix.WSTOPPED n) -> raise (Subprocess_failed n))
                      else
                        begin
                          Thread.delay 1.0;
                          refresh_session ();
                          let curtime = Unix.time () in
                          let elapsed = curtime -. starttime in
                          let f x = 0.1 +. (0.9 -. 0.9 *. exp (-. elapsed /. 60.0)) in
                          let progress = f elapsed in
                          TaskHelper.set_progress ~__context progress;
                          update_progress ()
                        end
                    in update_progress ()
                 ) with
         | Success _ -> debug "Install script exited successfully."
         | Failure(log, Subprocess_failed n) ->
           error "post_install_script failed: message='%s' (assuming this was because the disk was too small)" log;
           raise (Api_errors.Server_error (Api_errors.provision_failed_out_of_space, []))
         | Failure(log, exn) ->
           raise exn
      )


