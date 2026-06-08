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
open Xapi_stdext_pervasives.Pervasiveext
open Client

module D = Debug.Make (struct let name = "attach_helpers" end)

open D

let timeout = 300. (* 5 minutes, should never take this long *)

(** Attempt an unplug, and if it fails because the device is in use, wait for it to
    detach by polling the currently-attached field. *)
let safe_unplug rpc session_id self =
  try Client.VBD.unplug ~rpc ~session_id ~self with
  | Api_errors.Server_error (error, _)
    when error = Api_errors.device_already_detached ->
      debug "safe_unplug caught DEVICE_ALREADY_DETACHED: this is safe to ignore"
  | Api_errors.Server_error (error, _) as e
    when error = Api_errors.device_detach_rejected ->
      debug
        "safe_unplug caught DEVICE_DETACH_REJECTED: polling the \
         currently_attached flag of the VBD" ;
      let start = Unix.gettimeofday () in
      let unplugged = ref false in
      while (not !unplugged) && Unix.gettimeofday () -. start < timeout do
        Thread.delay 5. ;
        unplugged :=
          not (Client.VBD.get_currently_attached ~rpc ~session_id ~self)
      done ;
      if not !unplugged then (
        debug "Timeout waiting for dom0 device to be unplugged" ;
        raise e
      )

(** Execute a function with a list of VBDs after attaching a bunch of VDIs to an vm *)
let with_vbds rpc session_id __context vm vdis mode f =
  let vbds = ref [] in
  finally
    (fun () ->
      List.iter
        (fun vdi ->
          let vbd =
            Client.VBD.create ~rpc ~session_id ~vM:vm ~empty:false ~vDI:vdi
              ~userdevice:"autodetect" ~bootable:false ~mode ~_type:`Disk
              ~unpluggable:true ~qos_algorithm_type:"" ~qos_algorithm_params:[]
              ~other_config:[] ~device:"" ~currently_attached:false
          in
          let vbd_uuid = Client.VBD.get_uuid ~rpc ~session_id ~self:vbd in
          let uuid = Client.VM.get_uuid ~rpc ~session_id ~self:vm in
          debug "created VBD (uuid %s); attempting to hotplug to VM (uuid: %s)"
            vbd_uuid uuid ;
          vbds := vbd :: !vbds ;
          Client.VBD.plug ~rpc ~session_id ~self:vbd
        )
        vdis ;
      vbds := List.rev !vbds ;
      f !vbds
    )
    (fun () ->
      (* Use a new session here to cover the case where the session has become invalid *)
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          List.iter
            (Helpers.log_exn_continue "unplugging disk from VM" (fun self ->
                 safe_unplug rpc session_id self
             )
            )
            !vbds ;
          List.iter
            (Helpers.log_exn_continue "destroying VBD on VM" (fun self ->
                 Client.VBD.destroy ~rpc ~session_id ~self
             )
            )
            !vbds
      )
    )

module NbdClient = struct
  let print_fork_error f =
    try f ()
    with Forkhelpers.Spawn_internal_error (stderr, stdout, status) as e -> (
      match status with
      | Unix.WEXITED n ->
          error "Forkhelpers.Spawn_internal_error(%s, %s, WEXITED %d)" stderr
            stdout n ;
          raise e
      | Unix.WSIGNALED n ->
          error "Forkhelpers.Spawn_internal_error(%s, %s, WSIGNALED %d)" stderr
            stdout n ;
          raise e
      | Unix.WSTOPPED n ->
          error "Forkhelpers.Spawn_internal_error(%s, %s, WSTOPPED %d)" stderr
            stdout n ;
          raise e
    )

  let run_command cmd args =
    debug "running %s %s" cmd (String.concat " " args) ;
    let stdout, _ =
      print_fork_error (fun () ->
          Forkhelpers.execute_command_get_output cmd args
      )
    in
    stdout

  let start_nbd_client ~unix_socket_path ~export_name =
    run_command
      !Xapi_globs.nbd_client_manager_script
      ["connect"; "--path"; unix_socket_path; "--exportname"; export_name]
    |> String.trim

  let stop_nbd_client ~nbd_device =
    run_command
      !Xapi_globs.nbd_client_manager_script
      ["disconnect"; "--device"; nbd_device]
    |> ignore
end
