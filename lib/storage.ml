(*
 * Copyright (C) Citrix Systems Inc.
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

open Xenops_interface
open Xenops_task
open Xenops_utils

module D = Debug.Make(struct let name = service_name end)
open D

open Storage_interface
module Client = Storage_client.Client

let transform_exception f x =
  try f x
  with
  | Storage_error Backend_error_with_backtrace(code, backtrace :: params) as e ->
    let backtrace = Backtrace.Interop.of_json "SM" backtrace in
    let exn = Xenopsd_error (Storage_backend_error(code, params)) in
    Backtrace.add exn backtrace;
    Backtrace.reraise e exn
  | Storage_error Backend_error(code, params) as e ->
    error "Re-raising exception %s: %s" code (String.concat "; " params);
    Backtrace.reraise e (Xenopsd_error (Storage_backend_error(code, params)))

(* Used to identify this VBD to the storage layer *)
let id_of frontend vbd = Printf.sprintf "vbd/%s/%s" frontend (snd vbd)

let epoch_begin task sr vdi persistent =
  transform_exception
    (fun () ->
       Client.VDI.epoch_begin (Xenops_task.get_dbg task) sr vdi persistent
    ) ()

let epoch_end task sr vdi =
  transform_exception
    (fun () ->
       Client.VDI.epoch_end (Xenops_task.get_dbg task) sr vdi
    ) ()

let attach_and_activate task _vm dp sr vdi read_write =
  let result =
    Xenops_task.with_subtask task (Printf.sprintf "VDI.attach2 %s" dp)
      (transform_exception (fun () -> Client.VDI.attach2 "attach_and_activate" dp sr vdi read_write)) in

  Xenops_task.with_subtask task (Printf.sprintf "VDI.activate %s" dp)
    (transform_exception (fun () -> Client.VDI.activate "attach_and_activate" dp sr vdi));
  result

let deactivate task dp sr vdi =
  debug "Deactivating disk %s %s" (Sr.string_of sr) (Vdi.string_of vdi);
  Xenops_task.with_subtask task (Printf.sprintf "VDI.deactivate %s" dp)
    (transform_exception (fun () -> Client.VDI.deactivate "deactivate" dp sr vdi))

let dp_destroy task dp =
  Xenops_task.with_subtask task (Printf.sprintf "DP.destroy %s" dp)
    (transform_exception (fun () ->
         let waiting_for_plugin = ref true in
         while !waiting_for_plugin do
           try
             Client.DP.destroy "dp_destroy" dp false;
             waiting_for_plugin := false
           with
           | Storage_interface.Storage_error No_storage_plugin_for_sr _sr as e ->
             (* Since we have an activated disk in this SR, assume we are still
                						   waiting for xapi to register the SR's plugin. *)
             debug "Caught %s - waiting for xapi to register storage plugins."
               (Printexc.to_string e);
             Thread.delay 5.0
           | e ->
             (* Backends aren't supposed to return exceptions on deactivate/detach, but they
                						   frequently do. Log and ignore *)
             warn "DP destroy returned unexpected exception: %s" (Printexc.to_string e);
             waiting_for_plugin := false
         done
       ))

let get_disk_by_name _task path =
  match Stdext.Xstringext.String.split ~limit:2 '/' path with
  | [ sr; vdi ] ->
    info "Processing disk SR=%s VDI=%s" sr vdi;
    (Sr.of_string sr), (Vdi.of_string vdi)
  | _ ->
    error "Failed to parse VDI name %s (expected SR/VDI)" path;
    raise (Storage_interface.Storage_error (Vdi_does_not_exist path))
