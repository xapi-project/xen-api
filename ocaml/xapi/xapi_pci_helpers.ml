
open Stringext
open Opt

module PCI_DB = struct
	type sc_id = string

	type pci_subclass = {
		sc_id : string;
		sc_name : string;
	}

	type pci_class = {
		c_id : string;
		c_name : string;
		subclasses : (string, pci_subclass) Hashtbl.t
	}

	type subdevice = {
		subvendor_id : string;
		subdevice_id : string;
		sd_name : string;
	}

	type device = {
		d_id : string;
		d_name : string;
		subdevices : ((string * string), subdevice) Hashtbl.t
	}

	type vendor = {
		v_id : string;
		v_name : string;
		devices : (string, device) Hashtbl.t
	}

	type t = {
		classes : (string, pci_class) Hashtbl.t;
		vendors : (string, vendor) Hashtbl.t
	}

	let make c_size v_size =
		{classes = Hashtbl.create c_size; vendors = Hashtbl.create v_size}

	let add_class t c = Hashtbl.add t.classes c.c_id c

	let add_subclass t c_id sc =
		let c = Hashtbl.find t.classes c_id in
		Hashtbl.add c.subclasses sc.sc_id sc

	let add_vendor t v = Hashtbl.add t.vendors v.v_id v

	let add_device t v_id d =
		let v = Hashtbl.find t.vendors v_id in
		Hashtbl.add v.devices d.d_id d

	let add_subdevice t v_id d_id sd =
		let v = Hashtbl.find t.vendors v_id in
		let d = Hashtbl.find v.devices d_id in
		Hashtbl.add d.subdevices (sd.subvendor_id, sd.subdevice_id) sd
end

open PCI_DB

let parse_file path =
	let lines = Unixext.read_lines ~path in
	let lines =
		List.filter (fun s -> (not (String.startswith s "#" || s = ""))) lines in
	let strip = String.strip String.isspace in
	let cur_class, cur_vendor, cur_device = ref None, ref None, ref None in
	let parse_one_line line pci_db =
		match String.explode (String.sub line 0 2) with
		| 'C' :: _ -> (* this is a class definition *)
			cur_vendor := None; cur_device := None;
			let line = String.sub_to_end line 2 in
			let [id; name] = String.split ' ' line ~limit:2 in
			let pci_class =
				{c_id = id; c_name = strip name; subclasses = Hashtbl.create 8} in
			add_class pci_db pci_class;
			cur_class := Some id
		| ['\t'; '\t'] ->
			let line = String.sub_to_end line 2 in
			begin match !cur_device with
			| Some d -> (* this is a subdevice definition *)
				let [sv_id; sd_id; name] = String.split ' ' line ~limit:3 in
				let subdev =
					{subvendor_id = sv_id; subdevice_id = sd_id;
					sd_name = strip name} in
				add_subdevice pci_db (Opt.unbox !cur_vendor) d subdev
			| None -> ()
			end
		| '\t' :: _ ->
			let line = String.sub_to_end line 1 in
			begin match !cur_vendor with
			| Some v -> (* this is a device definition *)
				let [d_id; name] = String.split ' ' line ~limit:2 in
				let name = strip name in
				let device =
					{d_id = d_id; d_name = strip name; subdevices = Hashtbl.create 0} in
				add_device pci_db (Opt.unbox !cur_vendor) device;
				cur_device := Some d_id
			| None -> (* this is a subclass definition *)
				let [sc_id; name] = String.split ' ' line ~limit:2 in
				let pci_sc = {sc_id = sc_id; sc_name = strip name} in
				add_subclass pci_db (Opt.unbox !cur_class) pci_sc
			end
		| _ -> (* this is a vendor definition *)
			cur_class := None; cur_device := None;
			let [v_id; name] = String.split ' ' line ~limit:2 in
			let vendor =
				{v_id = v_id; v_name = strip name; devices = Hashtbl.create 8} in
			add_vendor pci_db vendor;
			cur_vendor := Some v_id
	in
	let rec parse_lines lines pci_db =
		match lines with
		| [] -> pci_db
		| line :: remaining ->
			parse_one_line line pci_db;
			parse_lines remaining pci_db
	in
	
	let pci_db = PCI_DB.make 24 2048 in
	parse_lines lines pci_db

let () =
	try
		ignore (parse_file "/usr/share/hwdata/pci.ids")
	with e ->
		failwith "Failed to parse pci database"
