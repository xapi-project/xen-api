(*
 * Temporarly give access to vm creation into the database until we
 * are able to get them from template.
 *)

open Printf

(* default connection parameters *)
let host = ref "gheeta"
let port = ref 8086
let username = ref "root"
let password = ref "xenroot"
let configfile = ref ""


(* default VM parameters *)
let is_hvm = ref false
let kernel_kernel = ref "/boot/vmlinuz-2.6-xenU"
let kernel_args = ref "root=/dev/sda1 ro"
let kernel_initrd = ref ""
let name_label = ref "Default name"
let name_description = ref "This is a default description"
let memory_max = ref 0L
let cpu_nb = ref 1L
let have_audio = ref false

let vbds = ref []
let vifs = ref []

let on_shutdown = ref "destroy"
let on_reboot = ref "restart"
let on_suspend = ref "destroy"
let on_crash = ref "destroy"

let string_to_action s =
	match s with
	| "destroy" -> `destroy
	| "restart" -> `restart
	| _ -> failwith "unknown action"

let string_to_action_crash s =
	match s with
	| "destroy" -> `destroy
	| "restart" -> `restart
	| "preserve" -> `preserve
	| _ -> failwith "unknown action crash"

let rec split ?limit:(limit=(-1)) c s =
	let i = try String.index s c with Not_found -> -1 in
	let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
	if i = -1 || nlimit = 0 then
		[ s ]
	else
		let a = String.sub s 0 i
		and b = String.sub s (i + 1) (String.length s - i - 1) in
		a :: (split ~limit: nlimit c b)


let rpc xml = Xmlrpcclient.do_secure_xml_rpc ~version:"1.0" ~host:!host ~port:!port ~path:"/" xml

(** Define functions that can be called on remote *)
module Fct = functor(Remote: API.API) -> struct
	let session_id = ref (Ref.make())

	let session_init () =
		let session = Remote.Session.login_with_password ~rpc
		                                         ~uname:!username
		                                         ~pwd:!password in
		session_id := session

	let get_VDI_on_SR vdi_uuid sr_uuid =
		Remote.VDI.get_by_uuid ~rpc ~session_id:!session_id
		                       ~uuid:(vdi_uuid)

	let create_VBD vDI vM vbd =
		let phytype, phypath, virttype, virtpath, mode = vbd in
		let device = virtpath
		and mode = mode
		and qos_algorithm_type = ""
		and qos_algorithm_params = [] in
		Remote.VBD.create ~rpc ~session_id:!session_id
		                  ~vM ~vDI ~device ~bootable:true ~mode
		                  ~qos_algorithm_type ~qos_algorithm_params ~_type:`Disk

	let create_VIF vM vif devid =
		let backend, bridge, ip, mac, script, ty, vifname = vif in

		let networks = Remote.Network.get_all ~rpc ~session_id:!session_id in
		let local_network = List.hd networks in
		let device = string_of_int devid
		and mTU = 1500L
		and qos_algorithm_type = ""
		and qos_algorithm_params = [] in

		Remote.VIF.create ~rpc ~session_id:!session_id
		                  ~device
		                  ~network:local_network
		                  ~vM ~mAC:mac ~mTU
		                  ~qos_algorithm_type ~qos_algorithm_params

	let create_vm () =
		let name_label = !name_label
		and name_description = !name_description
		and user_version = 1L
		and is_a_template = false
		and auto_power_on = false
		and memory_static_max = 429496320L
		and memory_dynamic_max = !memory_max
		and memory_dynamic_min = 134172672L
		and memory_static_min = 429496320L
		and vCPUs_at_startup = !cpu_nb
		and vCPUs_max = !cpu_nb
		and vCPUs_params = []
		and actions_after_shutdown = string_to_action !on_shutdown;
		and actions_after_reboot = string_to_action !on_reboot;
		and actions_after_crash = string_to_action_crash !on_crash;
		and hVM_boot_policy = if !is_hvm then "BIOS order" else "";
		and hVM_boot_params = if !is_hvm then [ ("order", "cd") ] else [];
		and pV_bootloader = ""
		and pV_kernel = !kernel_kernel
		and pV_ramdisk = !kernel_initrd
		and pV_args = !kernel_args
		and pV_bootloader_args = ""
		and pV_legacy_args = ""
		and platform = []
		and pCI_bus = ""
		and affinity = Ref.null
		and other_config = [] in
		Remote.VM.create ~rpc ~session_id:!session_id
		  ~name_label ~name_description
		  ~user_version ~is_a_template ~auto_power_on
		  ~memory_static_max ~memory_dynamic_max
		  ~memory_dynamic_min ~memory_static_min
		  ~vCPUs_params ~vCPUs_max ~vCPUs_at_startup
		  ~actions_after_shutdown ~actions_after_reboot
		  ~actions_after_crash
		  ~pV_bootloader ~pV_kernel ~pV_ramdisk ~pV_args
		  ~pV_bootloader_args ~pV_legacy_args
		  ~hVM_boot_policy ~hVM_boot_params ~platform
		  ~pCI_bus ~other_config ~affinity

	let create () =
		Printf.printf "creating vm\n%!";
		let vm = create_vm () in
		Printf.printf "creating vbds:\n%!";
		let i = ref 0 in
		List.iter (fun vbd ->
			let sr_uuid,vdi_uuid,_,_,_ = vbd in
			Printf.printf "  vbd %d [%s] ...\n%!" !i vdi_uuid; incr i;
			let vdi = get_VDI_on_SR vdi_uuid sr_uuid in
			ignore (create_VBD vdi vm vbd)
			) (List.rev !vbds);

		i := 0;
		Printf.printf "creating vifs:\n%!";
		List.iter (fun vif ->
			Printf.printf "  vif %d ...\n%!" !i; incr i;
			ignore (create_VIF vm vif (!i - 1))) (List.rev !vifs);
end

(* Remote Procedure *)
module RP = Fct(Client.Client)

let _ =
	(* parse args *)
	let args = [
		("--host", Arg.Set_string host, "set hostname");
		("--port", Arg.Set_int port, "set port");
		("--file", Arg.Set_string configfile, "set configfile"); ] in

	Arg.parse args (fun x -> eprintf "unknown arg: %s" x) "CVM";

	if !configfile = "" then (
		eprintf "no config file specified, aborting\n";
		exit 1
	);

	let set_action ref_var s =
		if s = "restart" || s = "destroy" || s = "preserve" then
			ref_var := s
		else
			failwith "unknown action state"
	and set_net s =
		let ls = split ',' s in

		let backend = ref ""
		and bridge = ref ""
		and ip = ref ""
		and mac = ref ""
		and script = ref ""
		and ty = ref ""
		and vifname = ref "" in

		List.iter (fun v ->
			let lv = split '=' v in
			let lvalue = List.nth lv 0
			and value = List.nth lv 1 in

			match lvalue with
			| "backend" -> backend := value
			| "bridge" -> bridge := value
			| "ip" -> ip := value
			| "mac" ->
				let x = split ':' value in
				if List.length x != 6 then
					failwith "mac address isn't recognized";
				(* FIXME: need to check every field *)
				mac := value
			| "script" ->
				script := value
			| "type" ->
				if value <> "para" && value <> "ioemu" then
					failwith (sprintf "vif type \"%s\" is unknown" value);
				ty := value
			| "vifname" -> vifname := value;
			| _ -> ()) ls;
		(* backend,bridge,ip,mac,script,ty,vifname *)
		let vif = (!backend, !bridge, !ip, !mac, !script, !ty, !vifname) in
		vifs := vif :: !vifs
	and set_disk s =
		(* sr_name:vdi_name:virttype:virtpath:mode *)
		let ls = split ':' s in

		let sr_name = List.nth ls 0
		and vdi_name = List.nth ls 1
		and virttype = List.nth ls 2
		and virtpath = List.nth ls 3
		and mode = List.nth ls 4 in

		if virttype <> "para" && virttype <> "ioemu" then
			failwith (sprintf "virt disk type \"%s\" is unknown" virttype);
		let mode = match mode with
		| "w" -> `RW
		| "r" -> `RO
		| _   -> failwith (sprintf "invalid disk mode \"%s\"" mode) in
		let newvbd = sr_name, vdi_name, virttype, virtpath, mode in
		vbds := newvbd :: !vbds in
	let cfg_args = [
		("hvm", Config.Set_bool is_hvm);
		("name", Config.Set_string name_label);
		("description", Config.Set_string name_description);
		("kernel", Config.Set_string kernel_kernel);
		("cmdline", Config.Set_string kernel_args);
		("initrd", Config.Set_string kernel_initrd);
		("cpus", Config.Int (fun i -> cpu_nb := Int64.of_int i));
		("memory", Config.Int (fun i ->
			memory_max := Int64.mul 1048576L (Int64.of_int i)));
		("audio", Config.Set_bool have_audio);
		("on_shutdown", Config.String (set_action on_shutdown));
		("on_reboot", Config.String (set_action on_reboot));
		("on_suspend", Config.String (set_action on_suspend));
		("on_crash", Config.String (set_action on_crash));
		("disk", Config.String (set_disk));
		("net", Config.String (set_net)); ] in

	begin try
		Config.read !configfile cfg_args (fun _ _ -> raise Not_found);
	with
		Config.Error ls -> List.iter (fun (p,s) ->
			eprintf "error: %s: %s\n" p s) ls;
		exit 2
	end;

	RP.session_init ();
	RP.create ()
