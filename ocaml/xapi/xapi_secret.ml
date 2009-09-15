open Stringext

module D = Debug.Debugger(struct let name = "xapi_secret" end)
open D

let introduce ~__context ~uuid ~secret =
	let ref = Ref.make () in
	Db.Secret.create ~__context ~ref ~uuid ~secret;
	ref

let create ~__context ~secret =
	let uuid = Uuid.to_string(Uuid.make_uuid()) in
	let ref = introduce ~__context ~uuid ~secret in
	ref

let destroy ~__context ~self =
	Db.Secret.destroy ~__context ~self

(* Delete the passwords references in a string2string map *)
let clean_out_passwds ~__context strmap =
	let delete_secret uuid =
		try
			let s = Db.Secret.get_by_uuid ~__context ~uuid in
			Db.Secret.destroy ~__context ~self:s
		with _ -> ()
	in
	let check_key (k, _) = String.endswith "password_secret" k in
	let secrets = List.map snd (List.filter check_key strmap) in
	List.iter delete_secret secrets
