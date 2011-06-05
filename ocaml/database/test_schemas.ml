let schema = 
	let _ref = {
		Schema.Column.name = Db_names.ref;
		persistent = true;
		empty = "";
		default = None;
		issetref = false;
	} in
	let uuid = {
		Schema.Column.name = Db_names.uuid;
		persistent = true;
		empty = "";
		default = None;
		issetref = false;
	} in
	let name_label = {
		Schema.Column.name = Db_names.name_label;
		persistent = true;
		empty = "";
		default = None;
		issetref = false;
	} in
	let name_description = {
		Schema.Column.name = "name__description";
		persistent = true;
		empty = "";
		default = None;
		issetref = false;
	} in
	let vbds = {
		Schema.Column.name = "VBDs";
		persistent = false;
		empty = "()";
		default = Some("()");
		issetref = true;
	} in
	let other_config = {
		Schema.Column.name = "other_config";
		persistent = false;
		empty = "()";
		default = Some("()");
		issetref = false;
	} in
	let pp = {
		Schema.Column.name = "protection_policy";
		persistent = true;
		empty = "";
		default = Some("OpaqueRef:NULL");
		issetref = false;
	} in
	let tags = {
		Schema.Column.name = "tags";
		persistent = true;
		empty = "";
		default = Some("()");
		issetref = false;
	} in
	let vm = {
		Schema.Column.name = "VM";
		persistent = true;
		empty = "";
		default = None;
		issetref = false;
	} in

	let vm_table = {
		Schema.Table.name = "VM";
		columns = [ _ref; uuid; name_label; vbds; pp; name_description; tags; other_config ];
		persistent = true;
	} in
	let vbd_table = {
		Schema.Table.name = "VBD";
		columns = [ _ref; uuid; vm ];
		persistent = true;
	} in
	let database = { 
		Schema.Database.tables = [ vm_table; vbd_table ];
	} in
	let one_to_many = Schema.StringMap.add "VBD" [ "VM", "VM", "VBDs" ] (Schema.StringMap.empty) in
	{
		
		Schema.major_vsn = 1;
		minor_vsn = 1;
		database = database;
		(** indexed by table name, a list of (this field, foreign table, foreign field) *)
		one_to_many = one_to_many;
		many_to_many = Schema.StringMap.empty;
	}


let many_to_many =
	let bar_column = { Schema.Column.name = "bars";
					   persistent = false;
					   empty = "()";
					   default = None;
					   issetref = false;
					 } in
	let foo_column = { bar_column with Schema.Column.name = "foos" } in
	let foo_table = { Schema.Table.name = "foo"; columns = [ bar_column ]; persistent = true } in
	let bar_table = { Schema.Table.name = "bar"; columns = [ foo_column ]; persistent = true } in
	
	let database = { Schema.Database.tables = [ foo_table; bar_table ] } in
	let many_to_many = 
		Schema.StringMap.add "foo" [ "bars", "bar", "foos" ]
			(Schema.StringMap.add "bar" [ "foos", "foo", "bars" ]
				 Schema.StringMap.empty) in
	let schema = { Schema.empty with
					   Schema.database = database;
					   many_to_many = many_to_many 
				 } in
	schema
