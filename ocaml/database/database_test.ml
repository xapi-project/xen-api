(* Supported operations: *)

let path = ref "./database"

let rpc_common url content_type request = 
	let version = "1.1" in
	let content_length = String.length request in
	let headers = [
		Printf.sprintf "POST %s HTTP/%s" url version;
		Printf.sprintf "User-Agent: xapi/%s" Xapi_globs.api_version_string;
		"Content-Type: text/json";
		Printf.sprintf "Content-length: %d" content_length;
	] in
	Xmlrpcclient.do_http_rpc "" 0 headers ~unixsock:(Some (!path)) request
		(fun content_length _ fd ->
			let buffer = String.make content_length '\000' in
			Unixext.really_read fd buffer 0 content_length;
			buffer)

module Client_v1 = Db_rpc_client_v1.Make(struct
	let initialise () = ()
	let rpc request = rpc_common "/post_remote_db_access" "text/xml" request
end)

module Client_v2 = Db_rpc_client_v2.Make(struct
	let initialise () = ()
	let rpc request = rpc_common "/post_remote_db_access_v2" "text/json" request
end)

module Client = Client_v2

let name = "thevmname"
let invalid_name = "notavmname"

let make_vm r uuid = 
	[ 
		"ref", r;
		"uuid", uuid;
		"memory__static_max", "0";
		"memory__overhead", "0";
		"PV__ramdisk", ""; 
		"is_control_domain", "false";
		"actions__after_crash", "restart";
		"resident_on", "OpaqueRef:NULL";
		"snapshot_info",  "()";
		"PCI_bus", "";
		"PV__args", "";
		"last_boot_CPU_flags", "()";
		"memory__target", "536870912";
		"is_a_template", "true";
		"user_version", "1";
		"HVM__shadow_multiplier", "1";
		"affinity", "OpaqueRef:NULL";
		"name__description", "";
		"PV__legacy_args", "";
		"parent", "OpaqueRef:NULL";
		"snapshot_metadata", "";
		"memory__dynamic_max", "0";
		"ha_always_run", "false";
		"other_config", "()";
		"PV__bootloader_args" ,"";
		"VCPUs__at_startup", "1";
		"bios_strings", "()";
		"actions__after_shutdown", "destroy";
		"blocked_operations", "()";
		"tags", "()";
		"PV__kernel", "";
		"name__label", name;
		"is_a_snapshot", "false";
		"VCPUs__params", "()";
		"VCPUs__max", "1";
		"allowed_operations", "()";
		"protection_policy", "OpaqueRef:NULL";
		"memory__static_min", "268435456";
		"domid", "-1";
		"power_state", "Halted";
		"HVM__boot_policy", "";
		"ha_restart_priority", "";
		"suspend_VDI", "OpaqueRef:NULL";
		"HVM__boot_params", "()";
		"PV__bootloader", "eliloader";
		"transportable_snapshot_id", "";
		"snapshot_of", "OpaqueRef:NULL";
		"guest_metrics", "OpaqueRef:NULL";
		"platform", "()";
		"scheduled_to_be_resident_on", "OpaqueRef:NULL";
		"is_snapshot_from_vmpp", "false";
		"current_operations", "()";
		"recommendations", "";
		"last_booted_record", "";
		"blobs", "()";
		"domarch", "";
		"memory__dynamic_min", "0";
		"metrics", "OpaqueRef:NULL";
		"actions__after_reboot", "restart";
		"xenstore_data", "()";
		"snapshot_time", "19700101T00:00:00Z"
	]

let make_vbd vm r uuid = [
	"ref", r;
	"qos__supported_algorithms", "()";
	"other_config", "(('owner' ''))";
	"uuid", uuid;
	"allowed_operations", "('attach')";
	"qos__algorithm_params", "()";
	"type", "Disk";
	"VM", vm;
	"VDI", "OpaqueRef:NULL";
	"qos__algorithm_type", "";
	"metrics", "OpaqueRef:NULL";
	"device", "";
	"empty", "false";
	"bootable", "false";
	"current_operations", "()";
	"unpluggable", "true";
	"status_detail", "";
	"runtime_properties", "()";
	"userdevice", "0";
	"mode", "RW";
	"storage_lock", "false";
	"status_code", "0";
	"currently_attached", "false";
]

let expect_missing_row tbl r f = 
	try
		f ()
	with Db_exn.DBCache_NotFound("missing row", tbl', r') when tbl' = tbl && r = r' -> ()

let expect_missing_tbl tbl f = 
	try
		f ()
	with Db_exn.DBCache_NotFound("missing table", tbl', "") when tbl' = tbl -> ()

let expect_uniqueness_violation tbl fld v f = 
	try
		f ()
	with Db_exn.Uniqueness_constraint_violation(tbl', fld', v') when tbl' = tbl && fld' = fld && v' = v -> ()
	
let expect_missing_uuid tbl uuid f = 
	try
		f ()
	with Db_exn.Read_missing_uuid(tbl', "", uuid') when tbl' = tbl && uuid' = uuid -> ()

let expect_missing_field name f = 
	try
		f ()
	with Db_exn.DBCache_NotFound("missing field", name', "") when name' = name -> ()

let test_invalid_where_record fn_name fn = 
	Printf.printf "%s <invalid table> ...\n" fn_name;
	expect_missing_tbl "Vm"
		(fun () ->
			let (_: string list) = fn { Db_cache_types.table = "Vm"; return = ""; where_field = ""; where_value = "" } in
			failwith (Printf.sprintf "%s <invalid table>" fn_name)
		);
	Printf.printf "%s <valid table> <invalid return> <valid field> <valid value>\n" fn_name;
	expect_missing_field "wibble"
		(fun () ->
			let (_: string list) = fn { Db_cache_types.table = "VM"; return = "wibble"; where_field = Escaping.escape_id [ "name"; "label" ]; where_value = name } in
			failwith (Printf.sprintf "%s <valid table> <invalid return> <valid field> <valid value>" fn_name)
		);
	Printf.printf "%s <valid table> <valid return> <invalid field> <valid value>\n" fn_name;
	expect_missing_field "wibble"
		(fun () ->
			let (_: string list) = fn { Db_cache_types.table = "VM"; return = Escaping.escape_id [ "name"; "label" ]; where_field = "wibble"; where_value = "" } in
			failwith (Printf.sprintf "%s <valid table> <valid return> <invalid field> <valid value>" fn_name)
		)


let _ = 
	Printexc.record_backtrace true;
	Arg.parse [ 
		"--connect-to", Arg.Set_string path, Printf.sprintf "connect to server on path (default %s)" !path;
		] (fun x -> Printf.fprintf stderr "Ignoring unknown parameter: %s\n%!" x)
		"query a database server";

	(* reference which we create *)
	let valid_ref = "ref1" in
	let valid_uuid = "uuid1" in
	let invalid_ref = "foo" in
	let invalid_uuid = "bar" in

	let vbd_ref = "waz" in
	let vbd_uuid = "whatever" in

	(* Before we begin, clear out any old state: *)
	expect_missing_row "VM" valid_ref
		(fun () ->
			Client.delete_row "VM" valid_ref;
		);
	expect_missing_row "VBD" vbd_ref
		(fun () ->
			Client.delete_row "VBD" vbd_ref;
		);
	Printf.printf "Deleted stale state from previous test\n";

	Printf.printf "get_table_from_ref <invalid ref>\n";
	begin
		match Client.get_table_from_ref invalid_ref with
			| None -> Printf.printf "Reference '%s' has no associated table\n" invalid_ref
			| Some t -> failwith (Printf.sprintf "Reference '%s' exists in table '%s'" invalid_ref t)
	end;
	Printf.printf "is_valid_ref <invalid_ref>\n";
	if Client.is_valid_ref invalid_ref then failwith "is_valid_ref <invalid_ref> = true";

	Printf.printf "read_refs <valid tbl>\n";
	let existing_refs = Client.read_refs "VM" in
	Printf.printf "VM refs: [ %s ]\n" (String.concat "; " existing_refs);
	Printf.printf "read_refs <invalid tbl>\n";
	expect_missing_tbl "Vm"
		(fun () ->
			let (_: string list) = Client.read_refs "Vm" in
			()
		);
	Printf.printf "delete_row <invalid ref>\n";
	expect_missing_row "VM" invalid_ref
		(fun () ->
			Client.delete_row "VM" invalid_ref;
			failwith "delete_row of a non-existent row silently succeeded"
		);
	Printf.printf "create_row <unique ref> <unique uuid>\n";
	Client.create_row "VM" (make_vm valid_ref valid_uuid) valid_ref;
	Printf.printf "is_valid_ref <valid ref>\n";
	if not (Client.is_valid_ref valid_ref)
	then failwith "is_valid_ref <valid_ref> = false, after create_row";
	Printf.printf "get_table_from_ref <valid ref>\n";
	begin match Client.get_table_from_ref valid_ref with
		| Some "VM" -> ()
		| Some t -> failwith "get_table_from_ref <valid ref> : invalid table"
		| None -> failwith "get_table_from_ref <valid ref> : None"
	end;
	Printf.printf "read_refs includes <valid ref>\n";
	if not (List.mem valid_ref (Client.read_refs "VM"))
	then failwith "read_refs did not include <valid ref>";

	Printf.printf "create_row <duplicate ref> <unique uuid>\n";
	expect_uniqueness_violation "VM" "_ref" valid_ref
		(fun () ->
			Client.create_row "VM" (make_vm valid_ref (valid_uuid ^ "unique")) valid_ref;
			failwith "create_row <duplicate ref> <unique uuid>"
		);
	Printf.printf "create_row <unique ref> <duplicate uuid>\n";
	expect_uniqueness_violation "VM" "uuid" valid_uuid
		(fun () ->
			Client.create_row "VM" (make_vm (valid_ref ^ "unique") valid_uuid) (valid_ref ^ "unique");
			failwith "create_row <unique ref> <duplicate uuid>"
		);
	Printf.printf "db_get_by_uuid <valid uuid>\n";
	if Client.db_get_by_uuid "VM" valid_uuid <> valid_ref
	then failwith "db_get_by_uuid <valid uuid>";
	Printf.printf "db_get_by_uuid <invalid uuid>\n";
	expect_missing_uuid "VM" invalid_uuid
		(fun () ->
			let (_: string) = Client.db_get_by_uuid "VM" invalid_uuid in
			failwith "db_get_by_uuid <invalid uuid>"
		);
	Printf.printf "get_by_name_label <invalid name label>\n";
	if Client.db_get_by_name_label "VM" invalid_name <> []
	then failwith "db_get_by_name_label <invalid name label>";

	Printf.printf "get_by_name_label <valid name label>\n";
	if Client.db_get_by_name_label "VM" name <> [ valid_ref ]
	then failwith "db_get_by_name_label <valid name label>";

	Printf.printf "read_field <valid field> <valid objref>\n";
	if Client.read_field "VM" "name__label" valid_ref <> name
	then failwith "read_field <valid field> <valid objref> : invalid name";
	Printf.printf "read_field <valid field> <invalid objref>\n";
	expect_missing_row "VM" invalid_ref
		(fun () ->
			let (_: string) = Client.read_field "VM" "name__label" invalid_ref in
			failwith "read_field <valid field> <invalid objref>"
		);
	Printf.printf "read_field <invalid field> <valid objref>\n";
	expect_missing_field "name_label"
		(fun () ->
			let (_: string) = Client.read_field "VM" "name_label" valid_ref in
			failwith "read_field <invalid field> <valid objref>"
		);
	Printf.printf "read_field <invalid field> <invalid objref>\n";
	expect_missing_row "VM" invalid_ref
		(fun () ->
			let (_: string) = Client.read_field "VM" "name_label" invalid_ref in
			failwith "read_field <invalid field> <invalid objref>"
		);
	Printf.printf "read_field_where <valid table> <valid return> <valid field> <valid value>\n";
	let where_name_label = 
		{ Db_cache_types.table = "VM"; return = Escaping.escape_id(["name"; "label"]); where_field="uuid"; where_value = valid_uuid } in
	let xs = Client.read_field_where where_name_label in
	if not (List.mem name xs)
	then failwith "read_field_where <valid table> <valid return> <valid field> <valid value>";
	test_invalid_where_record "read_field_where" Client.read_field_where;

	let xs = Client.read_set_ref where_name_label in
	if not (List.mem name xs)
	then failwith "read_set_ref <valid table> <valid return> <valid field> <valid value>";
	test_invalid_where_record "read_set_ref" Client.read_set_ref;

	Printf.printf "write_field <invalid table>\n";
	expect_missing_tbl "Vm"
		(fun () ->
			let (_: unit) = Client.write_field "Vm" "" "" "" in
			failwith "write_field <invalid table>"
		);
	Printf.printf "write_field <valid table> <invalid ref>\n";
	expect_missing_row "VM" invalid_ref
		(fun () ->
			let (_: unit) = Client.write_field "VM" invalid_ref "" "" in
			failwith "write_field <valid table> <invalid ref>"
		);
	Printf.printf "write_field <valid table> <valid ref> <invalid field>\n";
	expect_missing_field "wibble"
		(fun () ->
			let (_: unit) = Client.write_field "VM" valid_ref "wibble" "" in
			failwith "write_field <valid table> <valid ref> <invalid field>"
		);
	Printf.printf "write_field <valid table> <valid ref> <valid field>\n";
	let (_: unit) = Client.write_field "VM" valid_ref (Escaping.escape_id ["name"; "description"]) "description" in

	Printf.printf "read_record <invalid table> <invalid ref>\n";
	expect_missing_tbl "Vm"
		(fun () ->
			let _ = Client.read_record "Vm" invalid_ref in
			failwith "read_record <invalid table> <invalid ref>"
		);
	Printf.printf "read_record <valid table> <valid ref>\n";
	expect_missing_row "VM" invalid_ref
		(fun () ->
			let _ = Client.read_record "VM" invalid_ref in
			failwith "read_record <valid table> <invalid ref>"
		);
	Printf.printf "read_record <valid table> <valid ref>\n";
	let fv_list, fvs_list = Client.read_record "VM" valid_ref in
	if not(List.mem_assoc (Escaping.escape_id [ "name"; "label" ]) fv_list)
	then failwith "read_record <valid table> <valid ref> 1";
	if List.assoc "VBDs" fvs_list <> []
	then failwith "read_record <valid table> <valid ref> 2";
	Printf.printf "read_record <valid table> <valid ref> foreign key\n";
	Client.create_row "VBD" (make_vbd valid_ref vbd_ref vbd_uuid) vbd_ref;
	let fv_list, fvs_list = Client.read_record "VM" valid_ref in
	if List.assoc "VBDs" fvs_list <> [ vbd_ref ]
	then failwith "read_record <valid table> <valid ref> 3";
	Printf.printf "read_record <valid table> <valid ref> deleted foreign key\n";
	Client.delete_row "VBD" vbd_ref;
	let fv_list, fvs_list = Client.read_record "VM" valid_ref in
	if List.assoc "VBDs" fvs_list <> []
	then failwith "read_record <valid table> <valid ref> 4";
	Printf.printf "read_record <valid table> <valid ref> overwritten foreign key\n";
	Client.create_row "VBD" (make_vbd valid_ref vbd_ref vbd_uuid) vbd_ref;
	let fv_list, fvs_list = Client.read_record "VM" valid_ref in
	if List.assoc "VBDs" fvs_list = []
	then failwith "read_record <valid table> <valid ref> 5";
	Client.write_field "VBD" vbd_ref (Escaping.escape_id [ "VM" ]) "overwritten";
	let fv_list, fvs_list = Client.read_record "VM" valid_ref in
	if List.assoc "VBDs" fvs_list <> []
	then failwith "read_record <valid table> <valid ref> 6";	

	expect_missing_tbl "Vm"
		(fun () ->
			let _ = Client.read_records_where "Vm" Db_filter_types.True in
			()
		);
	let xs = Client.read_records_where "VM" Db_filter_types.True in
	if List.length xs <> 1
	then failwith "read_records_where <valid table> 2";
	let xs = Client.read_records_where "VM" Db_filter_types.False in
	if xs <> []
	then failwith "read_records_where <valid table> 3";

	expect_missing_tbl "Vm"
		(fun () ->
			let xs = Client.find_refs_with_filter "Vm" Db_filter_types.True in
			failwith "find_refs_with_filter <invalid table>";
		);
	let xs = Client.find_refs_with_filter "VM" Db_filter_types.True in
	if List.length xs <> 1
	then failwith "find_refs_with_filter <valid table> 1";
	let xs = Client.find_refs_with_filter "VM" Db_filter_types.False in
	if xs <> []
	then failwith "find_refs_with_filter <valid table> 2";

	expect_missing_tbl "Vm"
		(fun () ->
			Client.process_structured_field ("","") "Vm" "wibble" invalid_ref Db_cache_types.AddSet;
			failwith "process_structure_field <invalid table> <invalid fld> <invalid ref>"
		);
	expect_missing_field "wibble"
		(fun () ->
			Client.process_structured_field ("","") "VM" "wibble" valid_ref Db_cache_types.AddSet;
			failwith "process_structure_field <valid table> <invalid fld> <valid ref>"
		);
	expect_missing_row "VM" invalid_ref
		(fun () ->
			Client.process_structured_field ("","") "VM" (Escaping.escape_id ["name"; "label"]) invalid_ref Db_cache_types.AddSet;
			failwith "process_structure_field <valid table> <valid fld> <invalid ref>"
		);
	Client.process_structured_field ("foo", "") "VM" "tags" valid_ref Db_cache_types.AddSet;
	if Client.read_field "VM" "tags" valid_ref <> "('foo')"
	then failwith "process_structure_field expected ('foo')";
	Client.process_structured_field ("foo", "") "VM" "tags" valid_ref Db_cache_types.AddSet;
	if Client.read_field "VM" "tags" valid_ref <> "('foo')"
	then failwith "process_structure_field expected ('foo') 2";
	Client.process_structured_field ("foo", "bar") "VM" "other_config" valid_ref Db_cache_types.AddMap;

	if Client.read_field "VM" "other_config" valid_ref <> "(('foo' 'bar'))"
	then failwith "process_structure_field expected (('foo' 'bar')) 3";

	begin
		try
			Client.process_structured_field ("foo", "bar") "VM" "other_config" valid_ref Db_cache_types.AddMap;
		with Db_exn.Duplicate_key("VM", "other_config", r', "foo") when r' = valid_ref -> ()
	end;
	if Client.read_field "VM" "other_config" valid_ref <> "(('foo' 'bar'))"
	then failwith "process_structure_field expected (('foo' 'bar')) 4";

	(* Performance test *)
	let start = Unix.gettimeofday () in
	let n = 10000 in
	for i = 0 to n do
		let (_: bool) = Client.is_valid_ref valid_ref in
		()
	done;
	let total = Unix.gettimeofday () -. start in
	Printf.printf "%.2f RPC calls/sec\n" (float_of_int n /. total)
