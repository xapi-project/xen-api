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
(** Manage VDIs which are attached to dom0 on boot (eg HA statefile, remote database)
 * @group Storage
*)

module D = Debug.Make(struct let name="xapi" end)
open D

open Stdext
open Xstringext
open Pervasiveext
include Static_vdis_list (* include the vdi type and the list() function *)

(** Generate the static configuration and attach the VDI now *)
let permanent_vdi_attach ~__context ~vdi ~reason =
  info "permanent_vdi_attach: vdi = %s; sr = %s"
    (Ref.string_of vdi) (Ref.string_of (Db.VDI.get_SR ~__context ~self:vdi));

  (** Disallow attaching VDIs that only have changed block tracking metadata *)
  if (Db.VDI.get_type ~__context ~self:vdi) = `cbt_metadata then begin
    error "Static_vdis.permanent_vdi_attach: the given VDI has type cbt_metadata";
    raise (Api_errors.Server_error(Api_errors.vdi_incompatible_type, [ Ref.string_of vdi; Record_util.vdi_type_to_string `cbt_metadata ]))
  end;

  ignore (Helpers.call_script !Xapi_globs.static_vdis [ "add"; Db.VDI.get_uuid ~__context ~self:vdi; reason ]);
  (* VDI will be attached on next boot; attach it now too *)
  String.rtrim (Helpers.call_script !Xapi_globs.static_vdis
                  [ "attach"; Db.VDI.get_uuid ~__context ~self:vdi ])

(** Detach the VDI (by reference) now and destroy the static configuration *)
let permanent_vdi_detach ~__context ~vdi =
  info "permanent_vdi_detach: vdi = %s; sr = %s"
    (Ref.string_of vdi) (Ref.string_of (Db.VDI.get_SR ~__context ~self:vdi));
  let vdi_uuid = Db.VDI.get_uuid ~__context ~self:vdi in
  log_and_ignore_exn(fun () ->
    ignore(Helpers.call_script !Xapi_globs.static_vdis
           [ "detach"; vdi_uuid ]));
  ignore(Helpers.call_script !Xapi_globs.static_vdis
           [ "del"; vdi_uuid ])

(** Detach the VDI (by uuid) now and destroy the static configuration *)
let permanent_vdi_detach_by_uuid ~__context ~uuid =
  info "permanent_vdi_detach: vdi-uuid = %s" uuid;
  (* This might fail because the VDI has been destroyed *)
  log_and_ignore_exn(fun () ->
    ignore(Helpers.call_script !Xapi_globs.static_vdis [ "detach"; uuid ]));
  ignore(Helpers.call_script !Xapi_globs.static_vdis [ "del"; uuid ])

let detach_only vdi =
  if vdi.currently_attached then begin
    info "vdi_detach_by_uuid: vdi-uuid = %s" vdi.uuid;
    ignore (Helpers.call_script !Xapi_globs.static_vdis ["detach"; vdi.uuid])
  end

(** Added for CA-48539. Deactivates a vdi. You should probably follow
    	this call with one of the previous vdi_detach functions. *)
let permanent_vdi_deactivate_by_uuid ~__context ~uuid =
  info "permanent_vdi_detach: vdi-uuid = %s" uuid ;
  try
    let vdi = Db.VDI.get_by_uuid ~__context ~uuid in
    Sm.call_sm_vdi_functions ~__context ~vdi
      (fun srconf srtype sr -> Sm.vdi_deactivate srconf srtype sr vdi)
  with e ->
    warn "Ignoring exception calling SM vdi_deactivate for VDI uuid %s: %s (possibly VDI has been deleted while we were offline"
      uuid
      (ExnHelper.string_of_exn e)

(** Detaches and removes records for VDIs which have been deleted *)
let gc () =
  Server_helpers.exec_with_new_task "GCing on-boot VDIs" (fun __context ->
      List.iter
        (fun vdi ->
           let exists = try ignore(Db.VDI.get_by_uuid ~__context ~uuid:vdi.uuid); true with _ -> false in
           if not(exists) then begin
             warn "static-vdi %s cannot be found in database; removing on-boot configuration" vdi.uuid;
             (* NB we can't call the SM functions since the record has gone *)
             ignore(Helpers.call_script !Xapi_globs.static_vdis [ "del"; vdi.uuid ])
           end
        ) (list ()))

(** If we just rebooted and failed to attach our static VDIs then this can be called to reattempt the attach:
    	this is necessary for HA to start. *)
let reattempt_on_boot_attach () =
  let script = "attach-static-vdis" in
  try
    ignore(Helpers.call_script "/sbin/service" [ script; "start" ])
  with e ->
    warn "Attempt to reattach static VDIs via '%s start' failed: %s" script (ExnHelper.string_of_exn e)
