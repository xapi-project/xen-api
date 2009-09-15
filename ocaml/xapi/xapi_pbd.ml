(* API Calls *)

open Db_filter
open Db_filter_types

module D=Debug.Debugger(struct let name="xapi_pbd" end)
open D

(* Trivially "manipulate" passwords so they don't appear in plaintext.
   This is a temporary hack to avoid embarrasment of plaintext passwords appearing
   in db, API, CLI: we are under no illusions about it being secure.. ;) *)

let rot_str s r =
	let str = String.copy s in
	for i=0 to (String.length str)-1
		do
			str.[i] <- Char.chr (((Char.code (str.[i])) + r) mod 255)
		done;
	str

let transform_string str =
	Base64.encode (rot_str str 13)

let untransform_string str =
	rot_str (Base64.decode str) (0-13)

(* fns for hiding and restoring plaintext passwords in device config *)
let transform_password_device_config devconf =
	let plaintext_key_names = List.map fst Xapi_globs.hidden_fields in
	List.map (fun (k,v)->
		if List.mem k plaintext_key_names then
			let transformed_key_name = List.assoc k Xapi_globs.hidden_fields in
			(transformed_key_name, transform_string v)
		else (k,v)) devconf

let untransform_password_device_config devconf =
	let transformed_key_names = List.map snd Xapi_globs.hidden_fields in
	List.map (fun (k,v)->
		(k,if (List.mem k transformed_key_names) then untransform_string v else v)) devconf

let create_common ~__context ~host ~sR ~device_config ~currently_attached ~other_config =
	let pbds = Db.SR.get_PBDs ~__context ~self:sR in
	if List.exists (fun pbd -> Db.PBD.get_host ~__context ~self:pbd = host) pbds 
	then raise (Api_errors.Server_error (Api_errors.pbd_exists,
		[ Ref.string_of sR
		; Ref.string_of host
		; Ref.string_of (List.find (fun pbd -> Db.PBD.get_host ~__context ~self:pbd = host) pbds)
		]));
	let ref = Ref.make() in
	let uuid = Uuid.to_string (Uuid.make_uuid()) in
	(* The only way you can set a device_config string is via this call, so we apply the
	trivial password manipulation to CIFs passwords here, and then decode in the sm_iso backend *)
	let device_config = transform_password_device_config device_config in
	Db.PBD.create ~__context ~ref ~uuid ~host ~sR ~device_config ~currently_attached ~other_config:[];
	ref

let create ~__context ~host ~sR ~device_config ~other_config = create_common ~__context ~host ~sR ~device_config ~currently_attached:false ~other_config

(* Useful internal helpers *)

let create_thishost ~__context ~sR ~device_config ~currently_attached =
  create_common ~__context ~host:(Helpers.get_localhost ~__context) ~sR ~device_config ~currently_attached ~other_config:[]

let get_active_vdis_by_pbd ~__context ~self =
  let sr = Db.PBD.get_SR ~__context ~self in
  let host = Db.PBD.get_host ~__context ~self in
  let vms = Db.VM.get_records_where ~__context 
    ~expr:(Eq(Field "resident_on", Literal (Ref.string_of host))) in
  let vbds = List.flatten (List.map (fun (vm,vmr) -> vmr.API.vM_VBDs) vms) in
  let vbds_r = List.map (fun self -> Db.VBD.get_record_internal ~__context ~self) vbds in
  let active_vbds = List.filter
    (fun r -> 
       (r.Db_actions.vBD_currently_attached || r.Db_actions.vBD_reserved) && not(r.Db_actions.vBD_empty)) vbds_r in
  
  let vdis = List.map (fun r -> r.Db_actions.vBD_VDI) active_vbds in
  let vdis_in_sr = List.filter (fun vdi -> sr=Db.VDI.get_SR ~__context ~self:vdi) vdis in
  vdis_in_sr
  
(* CA-16480: abort if unplugging this PBD would cause a protected VM to become non-agile *)
let abort_if_storage_attached_to_protected_vms ~__context ~self =
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool && not(Db.Pool.get_ha_allow_overcommit ~__context ~self:pool) then begin
    let host = Db.PBD.get_host ~__context ~self in
    let sr = Db.PBD.get_SR ~__context ~self in
    let vdis = Db.SR.get_VDIs ~__context ~self:sr in
    let vms = Db.VM.get_all_records ~__context in
    let protected_vms = List.filter (fun (_, record) -> Helpers.is_xha_protected_r record) vms in
    List.iter
      (fun (vm_ref, vm_record) ->
	 let vbds = vm_record.API.vM_VBDs in
	 List.iter
	   (fun vbd ->
	      let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
	      if List.mem vdi vdis then begin
		warn "PBD.unplug will make protected VM %s not agile since it has a VBD attached to VDI %s" (Ref.string_of vm_ref) (Ref.string_of vdi);
		raise (Api_errors.Server_error(Api_errors.ha_operation_would_break_failover_plan, []))
	      end
	   ) vbds
      ) protected_vms
  end

let plug ~__context ~self =
  let currently_attached = Db.PBD.get_currently_attached ~__context ~self in
    if not currently_attached then
      begin
	let sr = Db.PBD.get_SR ~__context ~self in
	  Storage_access.SR.attach ~__context ~self:sr;
      end

let unplug ~__context ~self =
  let currently_attached = Db.PBD.get_currently_attached ~__context ~self in
    if currently_attached then
      begin
	let host = Db.PBD.get_host ~__context ~self in
	let sr = Db.PBD.get_SR ~__context ~self in

	if Db.Host.get_enabled ~__context ~self:host
	then abort_if_storage_attached_to_protected_vms ~__context ~self;

	(* If HA is enabled, prevent a PBD whose SR contains a statefile being unplugged *)
	let pool = List.hd (Db.Pool.get_all ~__context) in
	if Db.Pool.get_ha_enabled ~__context ~self:pool then begin
	  let statefiles = Db.Pool.get_ha_statefiles ~__context ~self:pool in
	  let statefile_srs = List.map (fun self -> Db.VDI.get_SR ~__context ~self:(Ref.of_string self)) statefiles in
	  if List.mem sr statefile_srs
	  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []))
	end;

	let vdis = get_active_vdis_by_pbd ~__context ~self in
	if List.length vdis > 0 
	then raise (Api_errors.Server_error(Api_errors.vdi_in_use,List.map Ref.string_of vdis));

	Storage_access.SR.detach ~__context ~self:sr
      end

let destroy ~__context ~self =
	if Db.PBD.get_currently_attached ~__context ~self
	then raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["PBD is currently attached"]));
	Db.PBD.destroy ~__context ~self

let set_device_config ~__context ~self ~value = 
  (* Only allowed from the SM plugin *)
  Db.PBD.set_device_config ~__context ~self ~value
