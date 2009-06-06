let get_allowed_messages ~__context ~self = []

let create ~__context ~short_name ~fullname ~other_config =
	let uuid = Uuid.make_uuid () in
	let ref = Ref.make () in
	Db.User.create ~__context ~ref ~uuid:(Uuid.to_string uuid)
	               ~short_name ~fullname ~other_config;
	ref

let destroy ~__context ~self =
	Db.User.destroy ~__context ~self
