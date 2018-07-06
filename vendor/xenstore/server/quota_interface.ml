include Namespace.Unsupported

let ( |> ) a b = b a

let read t (perms: Perms.t) (path: Store.Path.t) =
	Perms.has perms Perms.CONFIGURE;
	match Store.Path.to_string_list path with
		| [] -> ""
		| "default" :: [] -> ""
		| "entries-per-domain" :: [] -> ""
		| "number-of-entries" :: [] -> ""
		| "number-of-registered-watches" :: [] -> ""
		| "number-of-active-transactions" :: [] -> ""
		| "number-of-queued-watch-events" :: [] -> ""
		| "default" :: "number-of-entries" :: [] ->
			string_of_int (!Quota.maxent)
		| "default" :: "entry-length" :: [] ->
			string_of_int (!Quota.maxsize)
		| "default" :: "number-of-registered-watches" :: [] ->
			string_of_int (!Quota.maxwatch)
		| "default" :: "number-of-active-transactions" :: [] ->
			string_of_int (!Quota.maxtransaction)
		| "default" :: "number-of-queued-watch-events" :: [] ->
			string_of_int (!Quota.maxwatchevent)
		| "entries-per-domain" :: domid :: [] ->
			let q = t.Transaction.store.Store.quota in
			let domid = int_of_string domid in
			let n = Quota.get q domid in
			string_of_int n
		| "number-of-entries" :: domid :: [] ->
			begin match Quota.get_override Quota.maxent_overrides (int_of_string domid) with
				| Some x -> string_of_int x
				| None -> Store.Path.doesnt_exist path
			end
		| "number-of-registered-watches" :: domid :: [] ->
			begin match Quota.get_override Quota.maxwatch_overrides (int_of_string domid) with
				| Some x -> string_of_int x
				| None -> Store.Path.doesnt_exist path
			end
		| "number-of-active-transactions" :: domid :: [] ->
			begin match Quota.get_override Quota.maxtransaction_overrides (int_of_string domid) with
				| Some x -> string_of_int x
				| None -> Store.Path.doesnt_exist path
			end
		| "number-of-queued-watch-events" :: domid :: [] ->
			begin match Quota.get_override Quota.maxwatchevent_overrides (int_of_string domid) with
				| Some x -> string_of_int x
				| None -> Store.Path.doesnt_exist path
			end
		| _ -> Store.Path.doesnt_exist path

let exists t perms path = try ignore(read t perms path); true with Store.Path.Doesnt_exist _ -> false

let write _t _creator perms path value =
	Perms.has perms Perms.CONFIGURE;
	match Store.Path.to_string_list path with
		| "default" :: "number-of-entries" :: [] ->
			Quota.maxent := int_of_string value
		| "default" :: "entry-length" :: [] ->
			Quota.maxsize := int_of_string value
		| "default" :: "number-of-registered-watches" :: [] ->
			Quota.maxwatch := int_of_string value
		| "default" :: "number-of-active-transactions" :: [] ->
			Quota.maxtransaction := int_of_string value
		| "default" :: "number-of-queued-watch-events" :: [] ->
			Quota.maxwatchevent := int_of_string value
		| "number-of-entries" :: domid :: [] ->
			Quota.set_override Quota.maxent_overrides (int_of_string domid) (Some (int_of_string value))
		| "number-of-registered-watches" :: domid :: [] ->
			Quota.set_override Quota.maxwatch_overrides (int_of_string domid) (Some (int_of_string value))
		| "number-of-active-transactions" :: domid :: [] ->
			Quota.set_override Quota.maxtransaction_overrides (int_of_string domid) (Some (int_of_string value))
		| "number-of-queued-watch-events" :: domid :: [] ->
			Quota.set_override Quota.maxwatchevent_overrides (int_of_string domid) (Some (int_of_string value))
		| _ -> Store.Path.doesnt_exist path

let list t perms path =
	Perms.has perms Perms.CONFIGURE;
	match Store.Path.to_string_list path with
	| [] -> [ "default"; "entries-per-domain"; "number-of-entries"; "number-of-registered-watches"; "number-of-active-transactions"; "number-of-queued-watch-events" ]
	| [ "default" ] -> [ "number-of-entries"; "entry-length"; "number-of-registered-watches"; "number-of-active-transactions"; "number-of-queued-watch-events" ]
	| [ "entries-per-domain" ] ->
		let q = t.Transaction.store.Store.quota in
		Quota.list q |> List.map fst |> List.map string_of_int
	| [ "number-of-entries" ] ->
		Quota.list_overrides Quota.maxent_overrides |> List.map fst |> List.map string_of_int
	| [ "number-of-registered-watches" ] ->
		Quota.list_overrides Quota.maxwatch_overrides |> List.map fst |> List.map string_of_int
	| [ "number-of-active-transactions" ] ->
		Quota.list_overrides Quota.maxtransaction_overrides |> List.map fst |> List.map string_of_int
	| [ "number-of-queued-watch-events" ] ->
		Quota.list_overrides Quota.maxwatchevent_overrides |> List.map fst |> List.map string_of_int
	| _ -> []


let rm _t perms path =
	Perms.has perms Perms.CONFIGURE;
	match Store.Path.to_string_list path with
	| "number-of-entries" :: domid :: [] ->
		Quota.set_override Quota.maxent_overrides (int_of_string domid) None
	| "number-of-registered-watches" :: domid :: [] ->
		Quota.set_override Quota.maxwatch_overrides (int_of_string domid) None
	| "number-of-active-transactions" :: domid :: [] ->
		Quota.set_override Quota.maxtransaction_overrides (int_of_string domid) None
	| "number-of-queued-watch-events" :: domid :: [] ->
		Quota.set_override Quota.maxwatchevent_overrides (int_of_string domid) None
	| _ -> raise Perms.Permission_denied
