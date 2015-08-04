open Db_filter_types
open Listext

module D = Debug.Make(struct let name="cluster_stack_constraints" end)
open D

(* sr <-> cluster stack (should eventually come from the SR backends) *)
let constraints = ref []

(* Check which cluster stack we can use based on the kinds of SRs that are attached *)
let required_cluster_stack ~__context =
	(* Check which PBDs are attached on the master (assume this is running on the master) *)
	let localhost = Helpers.get_localhost ~__context in
	let pbds = Db.PBD.get_refs_where ~__context ~expr:(And (
		Eq (Field "host", Literal (Ref.string_of localhost)),
		Eq (Field "currently_attached", Literal "true")
		)) in
	let required_stacks = List.filter_map (fun pbd ->
		let sr = Db.PBD.get_SR ~__context ~self:pbd in
		let sr_type = Db.SR.get_type ~__context ~self:sr in
		if List.mem_assoc sr_type !constraints then
			Some (List.assoc sr_type !constraints)
		else
			None
	) pbds in
	match required_stacks with
	| [] -> None
	| [stack] -> Some stack
	| _ :: _ ->
		(* This must be avoided by the PBD.plug code *)
		error "Conflicting cluster stack demands.";
		failwith "Conflicting cluster stack demands."

(* Choose a cluster stack given the constraints. Use default stack if there are no constaints. *)
let choose_cluster_stack ~__context =
	match required_cluster_stack ~__context with
	| Some stack -> stack
	| None -> !Xapi_globs.cluster_stack_default

(* Check whether the given SR is compatible with the given cluster stack *)
let assert_sr_compatible ~__context ~cluster_stack ~sr =
	try
		let sr_type = fst (List.find (fun (_, stack) -> stack = cluster_stack) !constraints) in
		if Db.SR.get_type ~__context ~self:sr <> sr_type then
			raise (Api_errors.Server_error (Api_errors.incompatible_statefile_sr, [sr_type]))
	with Not_found -> ()

(* Check whether we can attach the SR given the cluster stack that is currently in use *)
let assert_cluster_stack_compatible ~__context sr =
	let pool = Helpers.get_pool ~__context in
	if Db.Pool.get_ha_enabled ~__context ~self:pool then begin
		let current_stack = Db.Pool.get_ha_cluster_stack ~__context ~self:pool in
		let sr_type = Db.SR.get_type ~__context ~self:sr in
		if List.mem_assoc sr_type !constraints then begin
			let required_stack = List.assoc sr_type !constraints in
			if current_stack <> required_stack then
				raise (Api_errors.Server_error (Api_errors.incompatible_cluster_stack_active, [required_stack]))
		end
	end
