(*
 * Helpers for Xen-specific functionality (i.e. that which is not needed by
 * the fakeserver).
 *)

exception Device_has_no_VBD

let devid_of_vbd ~__context ~self =
	let device_name = Db.VBD.get_device ~__context ~self in
	Device.Vbd.device_number device_name

(** Returns the backend type ('vbd' or 'tap') used by this VDI *)
let kind_of_vdi ~__context ~self =
	let sr = Db.VDI.get_SR ~__context ~self in
	let physty = Device.Vbd.physty_of_string (Sm.sr_content_type ~__context ~sr) in
	Device.Vbd.kind_of_physty physty

let vbd_of_devid ~__context ~vm devid =
	let vbds = Db.VM.get_VBDs ~__context ~self:vm in
	let devs = List.map (fun self -> devid_of_vbd ~__context ~self) vbds in
	let table = List.combine devs vbds in
	let has_vbd = List.mem_assoc devid table in
	if not(has_vbd)
	then raise Device_has_no_VBD
	else List.assoc devid table 

(** Given a VBD, return a xenops device *)
let device_of_vbd ~__context ~self = 
  let vm = Db.VBD.get_VM ~__context ~self in
  let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
  let vdi = Db.VBD.get_VDI ~__context ~self in
  let kind = kind_of_vdi ~__context ~self:vdi in
  let devid = devid_of_vbd ~__context ~self in
  let backend = { Device_common.domid = 0; 
		  kind = kind;
		  devid = devid } in
  Device_common.device_of_backend backend domid 

(** Given a VIF, return a xenops device *)
let device_of_vif ~__context ~self = 
  let vm = Db.VIF.get_VM ~__context ~self in
  let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
  let devid = Helpers.devid_of_vif ~__context ~self in
  let backend = { Device_common.domid = 0; 
		  kind = Device_common.Vif;
		  devid = devid } in
  Device_common.device_of_backend backend domid 
