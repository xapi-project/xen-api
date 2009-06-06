include Db_actions.DB_Action
let is_valid_ref r =
	Db_cache.DBCache.is_valid_ref (Ref.string_of r)
