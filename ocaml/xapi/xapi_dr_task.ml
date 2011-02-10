let create ~__context ~_type ~device_config ~whitelist =
	if (not (Pool_features.is_enabled ~__context Features.DR)) then
		raise (Api_errors.Server_error(Api_errors.license_restriction, []))
	else
		let uuid = Uuid.make_uuid () in
		let ref = Ref.make() in
		Db.DR_task.create ~__context ~ref ~uuid:(Uuid.to_string uuid);
		ref

let destroy ~__context ~self = ()
