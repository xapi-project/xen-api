module D = Debug.Debugger(struct let name="xapi_vm_appliance" end)
open D

let create ~__context ~name_label ~name_description =
	let uuid = Uuid.make_uuid () in
	let ref = Ref.make() in
	Db.VM_appliance.create ~__context ~ref ~uuid:(Uuid.to_string uuid) ~name_label ~name_description;
	ref

let destroy ~__context ~self =
	Db.VM_appliance.destroy ~__context ~self

let start ~__context ~self = ()
let clean_shutdown ~__context ~self = ()
let hard_shutdown ~__context ~self = ()
