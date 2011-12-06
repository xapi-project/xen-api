(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(** Create miscellaneous DB records needed by both the real and fake servers.
 * @group Database Operations
 *)

open Xapi_vm_memory_constraints
open Vm_memory_constraints
open Printf
open Stringext
open Db_filter
open Db_filter_types

module D=Debug.Debugger(struct let name="xapi" end)
open D

type host_info = {
	name_label : string;
	xen_verstring : string;
	linux_verstring : string;
	hostname : string;
	uuid : string;
	dom0_uuid : string;
	oem_manufacturer : string option;
	oem_model : string option;
	oem_build_number : string option;
	machine_serial_number: string option;
	machine_serial_name: string option;
	total_memory_mib: int64;
	dom0_static_max: int64;
}

let read_localhost_info () =
	let xen_verstring =
		let xc = Xenctrl.interface_open () in
		let v = Xenctrl.version xc in
		Xenctrl.interface_close xc;
		Printf.sprintf "%d.%d%s" v.Xenctrl.major v.Xenctrl.minor v.Xenctrl.extra
	and linux_verstring =
		let verstring = ref "" in
		let f line =
			try verstring := List.nth (String.split ' ' line) 2
			with _ -> () in
		Unixext.readfile_line f "/proc/version";
		!verstring
	in
	let me = Helpers.get_localhost_uuid () in
	let lookup_inventory_nofail k = try Some (Xapi_inventory.lookup k) with _ -> None in
	let this_host_name = Helpers.get_hostname() in
	let total_memory_mib = 
		Vmopshelpers.with_xc
			(fun xc -> Memory.get_total_memory_mib ~xc) in
	let dom0_static_max = 
		(* Query the balloon driver to determine how much memory is available for domain 0. *)
		(* We cannot ask XenControl for this information, since for domain 0, the value of  *)
		(* max_memory_pages is hard-wired to the maximum native integer value ("infinity"). *)
		let map = Balloon.parse_proc_xen_balloon () in
		let lookup = fun x -> Opt.unbox (List.assoc x map) in
		let keys = [Balloon._low_mem_balloon; Balloon._high_mem_balloon; Balloon._current_allocation] in
		let values = List.map lookup keys in
		let result = List.fold_left Int64.add 0L values in
		Memory.bytes_of_kib result in

	  {name_label=this_host_name;
	   xen_verstring=xen_verstring;
	   linux_verstring=linux_verstring;
	   hostname=this_host_name;
	   uuid=me;
	   dom0_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid;
	   oem_manufacturer = lookup_inventory_nofail Xapi_inventory._oem_manufacturer;
	   oem_model = lookup_inventory_nofail Xapi_inventory._oem_model;
	   oem_build_number = lookup_inventory_nofail Xapi_inventory._oem_build_number;
	   machine_serial_number = lookup_inventory_nofail Xapi_inventory._machine_serial_number;
	   machine_serial_name = lookup_inventory_nofail Xapi_inventory._machine_serial_name;
	   total_memory_mib = total_memory_mib;
	   dom0_static_max = dom0_static_max;
	   }

(** Returns the maximum of two values. *)
let maximum x y = if x > y then x else y

(** Returns the minimum of two values. *)
let minimum x y = if x < y then x else y

let (+++) = Int64.add

(** Ensures that the database has all the necessary records for domain *)
(** zero, and that the records are up-to-date. Includes the following: *)
(**     1. The domain zero record.                                     *)
(**     2. The domain zero console record.                             *)
(**     3. The domain zero guest metrics record.                       *)
(**     4. The domain zero shadow record.                              *)
(** This function makes sure there is exactly one record of each type. *)
(** It updates existing records if they are found, or else creates new *)
(** records for any records that are missing.                          *)
let rec ensure_domain_zero_records ~__context (host_info: host_info) : unit =
	let domain_zero_ref = ensure_domain_zero_record ~__context host_info in
	ensure_domain_zero_console_record ~__context ~domain_zero_ref;
	ensure_domain_zero_guest_metrics_record ~__context ~domain_zero_ref host_info;
	ensure_domain_zero_shadow_record ~__context ~domain_zero_ref

and ensure_domain_zero_record ~__context (host_info: host_info): [`VM] Ref.t =
	let ref_lookup () = Helpers.get_domain_zero ~__context in
	let ref_create () = Ref.make () in
	let (domain_zero_ref, found) =
		try       ref_lookup (), true
		with _ -> ref_create (), false in
	if found
		then update_domain_zero_record ~__context ~domain_zero_ref host_info
		else create_domain_zero_record ~__context ~domain_zero_ref host_info;
	domain_zero_ref

and ensure_domain_zero_console_record ~__context ~domain_zero_ref : unit =
	match Db.VM.get_consoles ~__context ~self: domain_zero_ref with
		| [] ->
			(* if there are no consoles then make one *)
			create_domain_zero_console_record ~__context ~domain_zero_ref
		| [console_ref] ->
			(* if there's a single reference but it's invalid, make a new one: *)
			if not (Db.is_valid_ref __context console_ref) then
				create_domain_zero_console_record ~__context ~domain_zero_ref
		| _ ->
			(* if there's more than one console then something strange is *)
			(* going on; make a new one                                   *)
			create_domain_zero_console_record ~__context ~domain_zero_ref

and ensure_domain_zero_guest_metrics_record ~__context ~domain_zero_ref (host_info: host_info) : unit =
	if not (Db.is_valid_ref __context (Db.VM.get_metrics ~__context ~self:domain_zero_ref)) then
	begin
		debug "Domain 0 record does not have associated guest metrics record. Creating now";
		let metrics_ref = Ref.make() in
		create_domain_zero_guest_metrics_record ~__context ~domain_zero_metrics_ref:metrics_ref ~memory_constraints:(create_domain_zero_default_memory_constraints host_info)
		~vcpus:(calculate_domain_zero_vcpu_count ~__context);
		Db.VM.set_metrics ~__context ~self:domain_zero_ref ~value:metrics_ref
	end

and ensure_domain_zero_shadow_record ~__context ~domain_zero_ref : unit =
	(* Always create a new shadow record. *)
	let domain_zero_record = Db.VM.get_record ~__context ~self:domain_zero_ref in
	Helpers.set_boot_record ~__context ~self:domain_zero_ref domain_zero_record

and create_domain_zero_record ~__context ~domain_zero_ref (host_info: host_info) : unit =
	(* Determine domain 0 memory constraints. *)
	let memory = create_domain_zero_default_memory_constraints host_info in
	(* Determine information about the host machine. *)
	let domarch =
		let i = Int64.of_nativeint (Int64.to_nativeint 0xffffffffL) in
		Domain.string_of_domarch (if i > 0L then Domain.Arch_X64 else Domain.Arch_X32) in
	let localhost = Helpers.get_localhost ~__context in
	(* Read the control domain uuid from the inventory file *)
	let uuid = host_info.dom0_uuid in
	(* FIXME: Assume dom0 has 1 vCPU per Host_cpu for now *)
	let vcpus = calculate_domain_zero_vcpu_count ~__context in
	let metrics = Ref.make () in
	(* Now create the database record. *)
	Db.VM.create ~__context ~ref:domain_zero_ref
		~name_label:("Control domain on host: " ^ host_info.hostname) ~uuid
		~name_description:"The domain which manages physical devices and manages other domains"
		~hVM_boot_policy:"" ~hVM_boot_params:[] ~hVM_shadow_multiplier:1. ~platform:[] ~pCI_bus:""
		~pV_args:"" ~pV_ramdisk:"" ~pV_kernel:"" ~pV_bootloader:"" ~pV_bootloader_args:"" ~pV_legacy_args:""
		~actions_after_crash:`destroy ~actions_after_reboot:`destroy ~actions_after_shutdown:`destroy
		~allowed_operations:[] ~current_operations:[] ~blocked_operations:[] ~power_state:`Running
		~vCPUs_max:(Int64.of_int vcpus) ~vCPUs_at_startup:(Int64.of_int vcpus) ~vCPUs_params:[]
		~memory_overhead:0L
		~memory_static_min:memory.static_min ~memory_dynamic_min:memory.dynamic_min ~memory_target:memory.target
		~memory_static_max:memory.static_max ~memory_dynamic_max:memory.dynamic_max
		~resident_on:localhost ~scheduled_to_be_resident_on:Ref.null ~affinity:localhost ~suspend_VDI:Ref.null
		~is_control_domain:true ~is_a_template:false ~domid:0L ~domarch
		~is_a_snapshot:false ~snapshot_time:Date.never ~snapshot_of:Ref.null ~transportable_snapshot_id:""
		~snapshot_info:[] ~snapshot_metadata:""
		~parent:Ref.null
		~other_config:[] ~blobs:[] ~xenstore_data:[] ~tags:[] ~user_version:1L
		~ha_restart_priority:"" ~ha_always_run:false ~recommendations:""
		~last_boot_CPU_flags:[] ~last_booted_record:""
		~guest_metrics:Ref.null ~metrics
		~bios_strings:[] ~protection_policy:Ref.null
		~is_snapshot_from_vmpp:false
		~appliance:Ref.null
		~start_delay:0L
		~shutdown_delay:0L
		~order:0L
		~suspend_SR:Ref.null 
		~version:0L
	;
	Xapi_vm_helpers.update_memory_overhead ~__context ~vm:domain_zero_ref

and create_domain_zero_console_record ~__context ~domain_zero_ref : unit =
	debug "Domain 0 record does not have associated console record. Creating now";
	(* first delete any old dom0 console records that may be kicking around: *)
	let this_dom0s_consoles = Db.Console.get_refs_where ~__context ~expr: (Eq(Field "_ref", Literal (Ref.string_of domain_zero_ref))) in
	List.iter (fun console -> debug "Deleted old dom0 console record"; Db.Console.destroy ~__context ~self: console) this_dom0s_consoles;
	(* now make a new console record for dom0 *)
	let console_ref = Ref.make () in
	let address = Db.Host.get_address ~__context ~self: (Helpers.get_localhost ~__context) in
	let location = Printf.sprintf "https://%s%s?ref=%s" address Constants.console_uri (Ref.string_of domain_zero_ref) in
	Db.Console.create ~__context ~ref: console_ref
		~uuid: (Uuid.to_string (Uuid.make_uuid ()))
		~protocol:`rfb
		~location
		~vM: domain_zero_ref
		~other_config:[]
		~port: (Int64.of_int Xapi_globs.host_console_vncport)

and create_domain_zero_guest_metrics_record ~__context ~domain_zero_metrics_ref ~memory_constraints ~vcpus : unit =
	let rec mkints = function
		| 0 -> []
		| n -> (mkints (n - 1) @ [n]) in
	Db.VM_metrics.create ~__context ~ref: domain_zero_metrics_ref ~uuid: (Uuid.to_string (Uuid.make_uuid ()))
		~memory_actual: memory_constraints.target
		~vCPUs_utilisation: (List.map (fun x -> Int64.of_int x, 0.) (mkints vcpus))
		~vCPUs_number: (Int64.of_int vcpus)
		~vCPUs_CPU:[]
		~vCPUs_params:[]
		~vCPUs_flags: []
		~state: []
		~start_time: Date.never
		~install_time: Date.never
		~last_updated: Date.never
		~other_config:[];

and create_domain_zero_default_memory_constraints host_info : Vm_memory_constraints.t =
        try  
	  let constraints = {
	    static_min = Int64.of_string (Localdb.get Constants.pool_join_mem_stat_min);
	    static_max = Int64.of_string (Localdb.get Constants.pool_join_mem_stat_max);
	    dynamic_min = Int64.of_string (Localdb.get Constants.pool_join_mem_dyn_min);
	    dynamic_max = Int64.of_string (Localdb.get Constants.pool_join_mem_dyn_max);
	    target = Int64.of_string (Localdb.get Constants.pool_join_mem_target);
	  } in
	  Localdb.del Constants.pool_join_mem_stat_min;
	  Localdb.del Constants.pool_join_mem_stat_max;
	  Localdb.del Constants.pool_join_mem_dyn_min;
	  Localdb.del Constants.pool_join_mem_dyn_max;
	  Localdb.del Constants.pool_join_mem_target;
	  constraints 
	with _ -> 
	  let static_min, static_max = calculate_domain_zero_memory_static_range host_info in
	  let target = static_min +++ (Memory.bytes_of_mib 100L) in
	  let target = if target > static_max then static_max else target in
	  {
	    static_min  = static_min;
	    dynamic_min = target;
	    target      = target;
	    dynamic_max = target;
	    static_max  = static_max;
	  }

and update_domain_zero_record ~__context ~domain_zero_ref (host_info: host_info) : unit =
	(* Fetch existing memory constraints for domain 0. *)
	let constraints = Vm_memory_constraints.get ~__context ~vm_ref:domain_zero_ref in
	(* Generate new memory constraints from the old constraints. *)
	let constraints = update_domain_zero_memory_constraints host_info constraints in
	(* Write the updated memory constraints to the database. *)
	Vm_memory_constraints.set ~__context ~vm_ref:domain_zero_ref ~constraints

and update_domain_zero_memory_constraints (host_info: host_info) (constraints: Vm_memory_constraints.t) : Vm_memory_constraints.t =
	let static_min, static_max = calculate_domain_zero_memory_static_range host_info in
	let constraints = {constraints with
		static_min = static_min;
		static_max = static_max;} in
	match Vm_memory_constraints.transform constraints with
		| None ->
			(* The existing constraints are invalid, and cannot be transformed  *)
			(* into valid constraints. Reset the constraints to their defaults. *)
			create_domain_zero_default_memory_constraints host_info
		| Some constraints ->
			constraints

(** Calculates the range of memory to which domain 0 is constrained, in bytes. *)
and calculate_domain_zero_memory_static_range (host_info: host_info) : int64 * int64 =

	(** Calculates the minimum amount of memory needed by domain 0, in bytes. *)
	let calculate_domain_zero_memory_static_min () =
		(* Base our calculation on the total amount of host memory. *)
		let host_total_memory_mib = host_info.total_memory_mib in
		let minimum = 200L in            (*   lower hard limit                               *)
		let intercept = 126L in          (*   [domain 0 memory] when [total host memory] = 0 *)
		let gradient = 21.0 /. 1024.0 in (* d [domain 0 memory] /  d [total host memory]     *)
		let result = Int64.add (Int64.of_float (gradient *. (Int64.to_float host_total_memory_mib))) intercept in
		let result = if result < minimum then minimum else result in
		Memory.bytes_of_mib result in

	(* static_min must not be greater than static_max *)
	let static_min = calculate_domain_zero_memory_static_min () in
	let static_max = host_info.dom0_static_max in
	let static_min = minimum static_min static_max in
	static_min, static_max

and calculate_domain_zero_vcpu_count ~__context : int =
	List.length (Db.Host.get_host_CPUs ~__context ~self:(Helpers.get_localhost ~__context))

open Db_filter

(** Create a record for the "root" user if it doesn't exist already *)
let create_root_user ~__context =
	let fullname = "superuser"
	and short_name = "root"
	and uuid = Uuid.to_string (Uuid.make_uuid ())
	and ref = Ref.make () in

	let all = Db.User.get_records_where ~__context ~expr:(Eq(Field "short_name", Literal short_name)) in
	if all = [] then Db.User.create ~__context ~ref ~fullname ~short_name ~uuid ~other_config:[]

let get_xapi_verstring () =
  Printf.sprintf "%d.%d" Xapi_globs.version_major Xapi_globs.version_minor  
  
(** Create assoc list of Supplemental-Pack information.
 *  The package information is taking from the [XS-REPOSITORY] XML file in the package
 *  directory.
 *  The keys have the form "<originator>:<name>", the value is
 *  "<description>, version <version>", appended by ", build <build>" if the <build>
 *  number is present in the XML file, and appended by ", homogeneous" if the [enforce-homogeneity]
 *  attribute is present and set to "true".
 *  For backwards compatibility, the old [package-linux] key is also added
 *  when the linux pack (now [xs:linux]) is present (alongside the new key).
 *  The [package-linux] key is now deprecated and will be removed in the next version. *)
let make_packs_info () =
	try
		let packs = Sys.readdir Xapi_globs.packs_dir in
		let get_pack_details fname =
			try
				let xml = Xml.parse_file (Xapi_globs.packs_dir ^ "/" ^ fname ^ "/XS-REPOSITORY") in
				match xml with
				| Xml.Element (name, attr, children) -> 
					let originator = List.assoc "originator" attr in
					let name = List.assoc "name" attr in
					let version = List.assoc "version" attr in
					let build = 
						if List.mem_assoc "build" attr then Some (List.assoc "build" attr)
						else None
					in
					let homogeneous = 
						if List.mem_assoc "enforce-homogeneity" attr &&
							(List.assoc "enforce-homogeneity" attr) = "true" then true
						else false
					in
					let description = match children with
						| Xml.Element(_, _, (Xml.PCData s) :: _) :: _ -> s
						| _ -> failwith "error with parsing pack data"
					in
					let param_name = originator ^ ":" ^ name in
					let value = description ^ ", version " ^ version ^
						(match build with
						| Some build -> ", build " ^ build
						| None -> "") ^
						(if homogeneous then ", homogeneous"
						else "")
					in
					let kv = [(param_name, value)] in
					if originator = "xs" && name = "linux" then
						(* CA-29040: put old linux-pack key in there for backwards compatibility *)
						["package-linux", "installed"] @ kv
					else
						kv
				| _ -> failwith "error while parsing pack data!"
			with _ -> debug "error while parsing pack data for %s!" fname; []
		in
		Array.fold_left (fun l fname -> get_pack_details fname @ l) [] packs
	with _ -> []
  
(** Create a complete assoc list of version information *)
let make_software_version () =
	let option_to_list k o = match o with None -> [] | Some x -> [ k, x ] in
	let info = read_localhost_info () in
	let v6_version = V6client.get_version () in
	Xapi_globs.software_version @
	(if v6_version = "" then [] else ["dbv", v6_version]) @
	[
	"xapi", get_xapi_verstring ();
	"xen", info.xen_verstring;
	"linux", info.linux_verstring;
	"xencenter_min", Xapi_globs.xencenter_min_verstring;
	"xencenter_max", Xapi_globs.xencenter_max_verstring;
	"network_backend", Netdev.string_of_kind Netdev.network.Netdev.kind;
	] @
	(option_to_list "oem_manufacturer" info.oem_manufacturer) @
	(option_to_list "oem_model" info.oem_model) @
	(option_to_list "oem_build_number" info.oem_build_number) @
	(option_to_list "machine_serial_number" info.machine_serial_number) @
	(option_to_list "machine_serial_name" info.machine_serial_name) @
	make_packs_info ()

let create_host_cpu ~__context =
	let get_nb_cpus () =
		let xc = Xenctrl.interface_open () in
		let p = Xenctrl.physinfo xc in
		Xenctrl.interface_close xc;
		p.Xenctrl.nr_cpus
		in
	let trim_end s =
        	let i = ref (String.length s - 1) in
		while !i > 0 && (List.mem s.[!i] [ ' '; '\t'; '\n'; '\r' ])
		do
			decr i
		done;
		if !i >= 0 then String.sub s 0 (!i + 1) else "" in


	(* The boot-time CPU info is copied into a file in @ETCDIR@/ in the xenservices init script;
	   we use that to generate CPU records from. This ensures that if xapi is started after someone has
	   modified dom0's VCPUs we don't change out host config... [Important to get this right, otherwise
	   pool homogeneity checks fail] *)
	let get_cpuinfo () =
	        let cpu_info_file =
		  try Unix.access Xapi_globs.cpu_info_file [ Unix.F_OK ]; Xapi_globs.cpu_info_file
		  with _ -> "/proc/cpuinfo" in
		let in_chan = open_in cpu_info_file in
		let tbl = Hashtbl.create 32 in
		let rec get_lines () =
			let s = input_line in_chan in
			if s = "" then
				()
			else (
				let i = String.index s ':' in
				let k = trim_end (String.sub s 0 i) in
				let v = if String.length s < i + 2 then
					""
				else
					String.sub s (i + 2) (String.length s - i - 2) in
				Hashtbl.add tbl k v;
				get_lines ()
			)
			in
		get_lines ();
		close_in in_chan;
		Hashtbl.find tbl "vendor_id",
		Hashtbl.find tbl "model name",
		Hashtbl.find tbl "cpu MHz",
		Hashtbl.find tbl "flags",
	        Hashtbl.find tbl "stepping",
	        Hashtbl.find tbl "model",
	        Hashtbl.find tbl "cpu family"
		in
	let vendor, modelname, cpu_mhz, flags, stepping, model, family = get_cpuinfo () in
	let number = get_nb_cpus () in
	let host = Helpers.get_localhost ~__context in
	
	(* Fill in Host.cpu_info *)
	
	let cpuid = Cpuid.read_cpu_info () in
	let features = Cpuid.features_to_string cpuid.Cpuid.features in
	let physical_features = Cpuid.features_to_string cpuid.Cpuid.physical_features in
	let maskable = match cpuid.Cpuid.maskable with
		| Cpuid.No -> "no"
		| Cpuid.Base -> "base"
		| Cpuid.Full -> "full"
	in
	let cpu = [
		"cpu_count", string_of_int number;
		"vendor", vendor;
		"speed", cpu_mhz;
		"modelname", modelname;
		"family", family;
		"model", model;
		"stepping", stepping;
		"flags", flags;
		"features", features;
		"features_after_reboot", features;
		"physical_features", physical_features;
		"maskable", maskable;
	] in
	Db.Host.set_cpu_info ~__context ~self:host ~value:cpu;
 
 	(* Recreate all Host_cpu objects *)
	
	let speed = Int64.of_float (float_of_string cpu_mhz) in
	let model = Int64.of_string model in
	let family = Int64.of_string family in

	(* Recreate all Host_cpu objects *)
	let host_cpus = List.filter (fun (_, s) -> s.API.host_cpu_host = host) (Db.Host_cpu.get_all_records ~__context) in
	List.iter (fun (r, _) -> Db.Host_cpu.destroy ~__context ~self:r) host_cpus;
	for i = 0 to number - 1
	do
		let uuid = Uuid.to_string (Uuid.make_uuid ())
	    and ref = Ref.make () in
		debug "Creating CPU %d: %s" i uuid;
		ignore (Db.Host_cpu.create ~__context ~ref ~uuid ~host ~number:(Int64.of_int i)
			~vendor ~speed ~modelname
			~utilisation:0. ~flags ~stepping ~model ~family
			~features:"" ~other_config:[])
	done

let create_chipset_info ~__context =
	let host = Helpers.get_localhost ~__context in
	let current_info = Db.Host.get_chipset_info ~__context ~self:host in
	let iommu =
		let xen_dmesg = Vmopshelpers.with_xc (fun xc -> Xenctrl.readconsolering xc) in
		if String.has_substr xen_dmesg "I/O virtualisation enabled" then
			"true"
		else if String.has_substr xen_dmesg "I/O virtualisation disabled" then
			"false"
		else if List.mem_assoc "iommu" current_info then
			List.assoc "iommu" current_info
		else
			"false"
	in
	let info = ["iommu", iommu] in
	Db.Host.set_chipset_info ~__context ~self:host ~value:info

