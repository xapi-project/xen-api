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
(** Create miscellaneous DB records needed by both the real and fake servers *)

open Xapi_vm_memory_constraints
open Vm_memory_constraints
open Printf
open Stringext
open Db_filter
open Db_filter_types

module D=Debug.Debugger(struct let name="xapi" end)
open D

exception Cannot_read_hostname

type host_info = {
	name_label : string;
	xen_verstring : string;
	linux_verstring : string;
	hostname : string;
	uuid : string;
	oem_manufacturer : string option;
	oem_model : string option;
	oem_build_number : string option;
	machine_serial_number: string option;
	machine_serial_name: string option;
}

let read_localhost_info () =
	let xen_verstring =
		let xc = Xc.interface_open () in
		let v = Xc.version xc in
		Xc.interface_close xc;
		Printf.sprintf "%d.%d%s" v.Xc.major v.Xc.minor v.Xc.extra
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
	  {name_label=this_host_name;
	   xen_verstring=xen_verstring;
	   linux_verstring=linux_verstring;
	   hostname=this_host_name;
	   uuid=me;
	   oem_manufacturer = lookup_inventory_nofail Xapi_inventory._oem_manufacturer;
	   oem_model = lookup_inventory_nofail Xapi_inventory._oem_model;
	   oem_build_number = lookup_inventory_nofail Xapi_inventory._oem_build_number;
	   machine_serial_number = lookup_inventory_nofail Xapi_inventory._machine_serial_number;
	   machine_serial_name = lookup_inventory_nofail Xapi_inventory._machine_serial_name;
	   }

(** Extracts a value from an option that is assumed to have a value. *)
(** Fails at run time if there is no such value.                     *)
let val_of x = match x with (Some x) -> x

(** Returns the maximum of two values. *)
let maximum x y = if x > y then x else y

(** Returns the minimum of two values. *)
let minimum x y = if x < y then x else y

(** Returns the total amount of memory available in this host. *)
let host_get_total_memory_mib () =
	Vmopshelpers.with_xc
		(fun xc -> Memory.get_total_memory_mib ~xc)

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
let rec ensure_domain_zero_records ~__context : unit =
	let domain_zero_ref = ensure_domain_zero_record ~__context in
	ensure_domain_zero_console_record ~__context ~domain_zero_ref;
	ensure_domain_zero_guest_metrics_record ~__context ~domain_zero_ref;
	ensure_domain_zero_shadow_record ~__context ~domain_zero_ref

and ensure_domain_zero_record ~__context =
	let ref_lookup () = Helpers.get_domain_zero ~__context in
	let ref_create () = Ref.make () in
	let (domain_zero_ref, found) =
		try       ref_lookup (), true
		with _ -> ref_create (), false in
	if found
		then update_domain_zero_record ~__context ~domain_zero_ref
		else create_domain_zero_record ~__context ~domain_zero_ref;
	domain_zero_ref

and ensure_domain_zero_console_record ~__context ~domain_zero_ref =
	match Db.VM.get_consoles ~__context ~self: domain_zero_ref with
		| [] ->
			(* if there are no consoles then make one *)
			create_domain_zero_console_record ~__context ~domain_zero_ref
		| [console_ref] ->
			(* if there's a single reference but it's invalid, make a new one: *)
			if not (Db.is_valid_ref console_ref) then
				create_domain_zero_console_record ~__context ~domain_zero_ref
		| _ ->
			(* if there's more than one console then something strange is *)
			(* going on; make a new one                                   *)
			create_domain_zero_console_record ~__context ~domain_zero_ref

and ensure_domain_zero_guest_metrics_record ~__context ~domain_zero_ref =
	if not (Db.is_valid_ref (Db.VM.get_metrics ~__context ~self:domain_zero_ref)) then
	begin
		debug "Domain 0 record does not have associated guest metrics record. Creating now";
		let metrics_ref = Ref.make() in
		create_domain_zero_guest_metrics_record ~__context ~domain_zero_metrics_ref:metrics_ref ~memory_constraints:(create_domain_zero_default_memory_constraints ())
		~vcpus:(calculate_domain_zero_vcpu_count ~__context);
		Db.VM.set_metrics ~__context ~self:domain_zero_ref ~value:metrics_ref
	end

and ensure_domain_zero_shadow_record ~__context ~domain_zero_ref =
	(* Always create a new shadow record. *)
	let domain_zero_record = Db.VM.get_record ~__context ~self:domain_zero_ref in
	Helpers.set_boot_record ~__context ~self:domain_zero_ref domain_zero_record

and create_domain_zero_record ~__context ~domain_zero_ref =
	(* Determine domain 0 memory constraints. *)
	let memory = create_domain_zero_default_memory_constraints () in
	(* Determine information about the host machine. *)
	let domarch =
		let i = Int64.of_nativeint (Int64.to_nativeint 0xffffffffL) in
		Domain.string_of_domarch (if i > 0L then Domain.Arch_X64 else Domain.Arch_X32) in
	let localhost = Helpers.get_localhost ~__context in
	let hostname = Helpers.get_hostname() in
	(* Read the control domain uuid from the inventory file *)
	let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
	(* FIXME: Assume dom0 has 1 vCPU per Host_cpu for now *)
	let vcpus = calculate_domain_zero_vcpu_count ~__context in
	let metrics = Ref.make () in
	(* Now create the database record. *)
	Db.VM.create ~__context ~ref:domain_zero_ref
		~name_label:("Control domain on host: " ^ hostname) ~uuid
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
		~bios_strings:[];
	Xapi_vm_helpers.update_memory_overhead ~__context ~vm:domain_zero_ref

and create_domain_zero_console_record ~__context ~domain_zero_ref =
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

and create_domain_zero_guest_metrics_record ~__context ~domain_zero_metrics_ref ~memory_constraints ~vcpus =
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

and create_domain_zero_default_memory_constraints () =
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
	  let static_min, static_max = calculate_domain_zero_memory_static_range () in
	  let target = static_min +++ (Memory.bytes_of_mib 100L) in
	  let target = if target > static_max then static_max else target in
	  {
	    static_min  = static_min;
	    dynamic_min = target;
	    target      = target;
	    dynamic_max = target;
	    static_max  = static_max;
	  }

and update_domain_zero_record ~__context ~domain_zero_ref : unit =
	(* Fetch existing memory constraints for domain 0. *)
	let constraints = Vm_memory_constraints.get ~__context ~vm_ref:domain_zero_ref in
	(* Generate new memory constraints from the old constraints. *)
	let constraints = update_domain_zero_memory_constraints constraints in
	(* Write the updated memory constraints to the database. *)
	Vm_memory_constraints.set ~__context ~vm_ref:domain_zero_ref ~constraints

and update_domain_zero_memory_constraints constraints =
	let static_min, static_max = calculate_domain_zero_memory_static_range () in
	let constraints = {constraints with
		static_min = static_min;
		static_max = static_max;} in
	match Vm_memory_constraints.transform constraints with
		| None ->
			(* The existing constraints are invalid, and cannot be transformed  *)
			(* into valid constraints. Reset the constraints to their defaults. *)
			create_domain_zero_default_memory_constraints ()
		| Some constraints ->
			constraints

(** Calculates the range of memory to which domain 0 is constrained, in bytes. *)
and calculate_domain_zero_memory_static_range () =

	(** Calculates the minimum amount of memory needed by domain 0, in bytes. *)
	let calculate_domain_zero_memory_static_min () =
		(* Base our calculation on the total amount of host memory. *)
		let host_total_memory_mib = host_get_total_memory_mib () in
		let minimum = 200L in            (*   lower hard limit                               *)
		let intercept = 126L in          (*   [domain 0 memory] when [total host memory] = 0 *)
		let gradient = 21.0 /. 1024.0 in (* d [domain 0 memory] /  d [total host memory]     *)
		let result = Int64.add (Int64.of_float (gradient *. (Int64.to_float host_total_memory_mib))) intercept in
		let result = if result < minimum then minimum else result in
		Memory.bytes_of_mib result in

	(** Calculates the maximum amount of memory available to domain 0, in bytes. *)
	let calculate_domain_zero_memory_static_max () =
		(* Query the balloon driver to determine how much memory is available for domain 0. *)
		(* We cannot ask XenControl for this information, since for domain 0, the value of  *)
		(* max_memory_pages is hard-wired to the maximum native integer value ("infinity"). *)
		let map = Balloon.parse_proc_xen_balloon () in
		let lookup = fun x -> val_of (List.assoc x map) in
		let keys = [Balloon._low_mem_balloon; Balloon._high_mem_balloon; Balloon._current_allocation] in
		let values = List.map lookup keys in
		let result = List.fold_left Int64.add 0L values in
		Memory.bytes_of_kib result in

	(* static_min must not be greater than static_max *)
	let static_min = calculate_domain_zero_memory_static_min () in
	let static_max = calculate_domain_zero_memory_static_max () in
	let static_min = minimum static_min static_max in
	static_min, static_max

and calculate_domain_zero_vcpu_count ~__context =
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
  
(** Create assoc list of Supplemental-Pack information *)
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
					let description = match children with
						| Xml.Element(_, _, (Xml.PCData s) :: _) :: _ -> s
						| _ -> failwith "error with parsing pack data"
					in
					let param_name = originator ^ ":" ^ name in
					let value = description ^ ", version " ^ version ^
						match build with
						| Some build -> ", build " ^ build
						| None -> ""
					in
					let kv = [(param_name, value)] in
					if originator = "xs" && name = "linux" then
						(* CA-29040: put old linux-pack key in there for backwards
						 * compatibility. This should be removed in the next version *)
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
	Xapi_globs.software_version @
	["xapi", get_xapi_verstring ();
	"xen", info.xen_verstring;
	"linux", info.linux_verstring;
	"xencenter_min", Xapi_globs.xencenter_min_verstring;
	"xencenter_max", Xapi_globs.xencenter_max_verstring;
	] @
	(option_to_list "oem_manufacturer" info.oem_manufacturer) @
	(option_to_list "oem_model" info.oem_model) @
	(option_to_list "oem_build_number" info.oem_build_number) @
	(option_to_list "machine_serial_number" info.machine_serial_number) @
	(option_to_list "machine_serial_name" info.machine_serial_name) @
	make_packs_info ()

(** Create a record for a host if it doesn't exist already (identified by uuid) *)
let create_host ~__context ~name_label ~xen_verstring ~linux_verstring ~capabilities ~hostname ~uuid ~address =
        let existing_host = try Some (Db.Host.get_by_uuid __context uuid) with _ -> None in
	let make_new_metrics_object ref =
	  Db.Host_metrics.create ~__context ~ref
	    ~uuid:(Uuid.to_string (Uuid.make_uuid ())) ~live:false
	    ~memory_total:0L ~memory_free:0L ~last_updated:Date.never ~other_config:[] in
	let host = 
	  match existing_host with
	      None ->
	        debug "localhost record does not exist for this host. creating new one (uuid='%s')" uuid;
		let xapi_verstring = get_xapi_verstring() in
		let name_description = "Default install of XenServer"
		and ref = Ref.make () in

		let metrics = Ref.make () in
		make_new_metrics_object metrics;

		Db.Host.create ~__context ~ref 
			~current_operations:[] ~allowed_operations:[]
			~software_version:Xapi_globs.software_version
			~enabled:true
			~aPI_version_major:Xapi_globs.api_version_major
			~aPI_version_minor:Xapi_globs.api_version_minor
			~aPI_version_vendor:Xapi_globs.api_version_vendor
			~aPI_version_vendor_implementation:Xapi_globs.api_version_vendor_implementation
			~name_description ~name_label ~uuid ~other_config:[]
			~capabilities
			~cpu_configuration:[]   (* !!! FIXME hard coding *)
			~memory_total:0L        (* !!! FIXME hard coding *)
			~memory_overhead:0L     (* !!! FIXME hard coding *)
			~sched_policy:"credit"  (* !!! FIXME hard coding *)
			~supported_bootloaders:(List.map fst Xapi_globs.supported_bootloaders)
			~suspend_image_sr:Ref.null ~crash_dump_sr:Ref.null
			~logging:[] ~hostname ~address ~metrics
			~license_params:[] ~boot_free_mem:0L
			~ha_statefiles:[] ~ha_network_peers:[] ~blobs:[] ~tags:[]
			~external_auth_type:""
			~external_auth_service_name:""
			~external_auth_configuration:[]
			~edition:"free" ~license_server:["address", "localhost"; "port", "27000"]
                        ~bios_strings:[]
           ~power_on_mode:""
           ~power_on_config:[]
		;
		ref
	    | Some ref -> ref in
	let metrics = Db.Host.get_metrics ~__context ~self:host in
	if not (Db.is_valid_ref metrics) then
	  begin
	    let ref = Ref.make() in
	    make_new_metrics_object metrics;
	    Db.Host.set_metrics ~__context ~self:host ~value:metrics
	  end;
	(* If the host we're creating is us, make sure its set to live *)
	Db.Host_metrics.set_last_updated ~__context ~self:metrics ~value:(Date.of_float (Unix.gettimeofday ()));
	Db.Host_metrics.set_live ~__context ~self:metrics ~value:(uuid=(Helpers.get_localhost_uuid ()));
	host

let create_host_cpu ~__context =
	let get_nb_cpus () =
		let xc = Xc.interface_open () in
		let p = Xc.physinfo xc in
		Xc.interface_close xc;
		p.Xc.nr_cpus
		in
	let trim_end s =
        	let i = ref (String.length s - 1) in
		while !i > 0 && (List.mem s.[!i] [ ' '; '\t'; '\n'; '\r' ])
		do
			decr i
		done;
		if !i >= 0 then String.sub s 0 (!i + 1) else "" in


	(* The boot-time CPU info is copied into a file in /etc/xensource/ in the xenservices init script;
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

	let host = Helpers.get_localhost ~__context
	and number = get_nb_cpus ()
	and speed = Int64.of_float (float_of_string cpu_mhz)
	and model = Int64.of_string model 
	and family = Int64.of_string family in

	let existing = Db.Host.get_host_CPUs ~__context ~self:host in
	let numbers = List.map (fun self -> Int64.to_int (Db.Host_cpu.get_number ~__context ~self)) existing in
	let table = List.combine numbers existing in
	for i = 0 to number - 1
	do
	  if List.mem i numbers then begin
	    let self = List.assoc i table in
	    Db.Host_cpu.set_vendor ~__context ~self ~value:vendor;
	    Db.Host_cpu.set_speed ~__context ~self ~value:speed;
	    Db.Host_cpu.set_modelname ~__context ~self ~value:modelname;
	    Db.Host_cpu.set_flags ~__context ~self ~value:flags;
	    Db.Host_cpu.set_stepping ~__context ~self ~value:stepping;
	    Db.Host_cpu.set_model ~__context ~self ~value:model;
	    Db.Host_cpu.set_family ~__context ~self ~value:family;
	    Db.Host_cpu.set_features ~__context ~self ~value:"";
	  end else begin
	    let uuid = Uuid.to_string (Uuid.make_uuid ())
	    and ref = Ref.make () in
	    debug "Creating CPU %d: %s" i uuid;
	    let () = Db.Host_cpu.create ~__context ~ref ~uuid ~host ~number:(Int64.of_int i)
	      ~vendor ~speed ~modelname
	      ~utilisation:0. ~flags ~stepping ~model ~family
              ~features:"" ~other_config:[] in 
	    ()
	  end
	done

