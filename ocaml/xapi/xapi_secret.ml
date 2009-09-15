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
