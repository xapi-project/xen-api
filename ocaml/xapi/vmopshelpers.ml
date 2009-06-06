open Pervasiveext

module D = Debug.Debugger(struct let name="xapi" end)
open D

(* xc and xs opening/cleaning interface helpers *)
let with_xc f = Xc.with_intf f

let with_xs f =
	let xs = Xs.daemon_open () in
	finally (fun () -> f xs) (fun () -> Xs.close xs)

let with_xal f =
	let xal = Xal.init () in
	finally (fun () -> f xal) (fun () -> Xal.close xal)

let with_xc_and_xs f =
	Xc.with_intf (fun xc -> with_xs (fun xs -> f xc xs))

let with_xc_and_xs_final f cf =
	with_xc_and_xs (fun xc xs -> finally (fun () -> f xc xs) cf)

exception Vm_corresponding_to_domid_not_in_db of int
let uuid_of_domid domid =
  Uuid.to_string (with_xc (fun xc -> Domain.get_uuid xc domid))

let vm_of_domid ~__context domid =
	try
		let uuid = Uuid.to_string (with_xc (fun xc -> Domain.get_uuid xc domid)) in
		Db.VM.get_by_uuid ~__context ~uuid
	with Xc.Error _
		-> raise (Vm_corresponding_to_domid_not_in_db domid)

