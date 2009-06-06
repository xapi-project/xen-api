(** Manage VDIs which are attached to dom0 on boot (eg HA statefile, remote database) *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

open Stringext
open Pervasiveext
include Static_vdis_list (* include the vdi type and the list() function *)

let static_vdis = "/opt/xensource/bin/static-vdis"

(** Generate the static configuration and attach the VDI now *)
let permanent_vdi_attach ~__context ~vdi ~reason = 
  info "permanent_vdi_attach: vdi = %s; sr = %s" 
    (Ref.string_of vdi) (Ref.string_of (Db.VDI.get_SR ~__context ~self:vdi));
  Helpers.call_script static_vdis
    [ "add"; Db.VDI.get_uuid ~__context ~self:vdi; reason ];
  (* VDI will be attached on next boot; attach it now too *)
  String.rtrim (Helpers.call_script static_vdis
		  [ "attach"; Db.VDI.get_uuid ~__context ~self:vdi ])

 (** Detach the VDI (by reference) now and destroy the static configuration *)
let permanent_vdi_detach ~__context ~vdi = 
  info "permanent_vdi_detach: vdi = %s; sr = %s" 
    (Ref.string_of vdi) (Ref.string_of (Db.VDI.get_SR ~__context ~self:vdi));  
  Sm.call_sm_vdi_functions ~__context ~vdi 
    (fun srconf srtype sr -> Sm.vdi_detach srconf srtype sr vdi);
  ignore(Helpers.call_script static_vdis
           [ "del"; Db.VDI.get_uuid ~__context ~self:vdi ])

(** Detach the VDI (by uuid) now and destroy the static configuration *)
let permanent_vdi_detach_by_uuid ~__context ~uuid = 
  info "permanent_vdi_detach: vdi-uuid = %s" uuid;
  begin
    try
      (* This might fail because the VDI has been destroyed *)
      let vdi = Db.VDI.get_by_uuid ~__context ~uuid in
      Sm.call_sm_vdi_functions ~__context ~vdi 
	(fun srconf srtype sr -> Sm.vdi_detach srconf srtype sr vdi)
    with e ->
      warn "Ignoring exception calling SM vdi_detach for VDI uuid %s: %s (possibly VDI has been deleted while we were offline" uuid (ExnHelper.string_of_exn e)
  end;
  ignore(Helpers.call_script static_vdis [ "del"; uuid ])

(** Detaches and removes records for VDIs which have been deleted *)
let gc () = 
  Server_helpers.exec_with_new_task "GCing on-boot VDIs" (fun __context ->
  List.iter
    (fun vdi ->
       let exists = try ignore(Db.VDI.get_by_uuid ~__context ~uuid:vdi.uuid); true with _ -> false in
       if not(exists) then begin
	 warn "static-vdi %s cannot be found in database; removing on-boot configuration" vdi.uuid;
	 (* NB we can't call the SM functions since the record has gone *)
	 ignore(Helpers.call_script static_vdis [ "del"; vdi.uuid ])
       end
    ) (list ()))

(** If we just rebooted and failed to attach our static VDIs then this can be called to reattempt the attach:
    this is necessary for HA to start. *)
let reattempt_on_boot_attach () = 
  let script = "/etc/init.d/attach-static-vdis" in
  try
    ignore(Helpers.call_script script [ "start" ])
  with e ->
    warn "Attempt to reattach static VDIs via '%s start' failed: %s" script (ExnHelper.string_of_exn e)
