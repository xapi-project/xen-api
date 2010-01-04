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
(** Code to create a set of built-in templates (eg on the fakeserver).
 * @group Virtual-Machine Management
 *)

open API
open Xapi_templates
open Stringext

module D = Debug.Debugger(struct let name="xapi" end)
open D
let ( ** ) a b = Int64.mul a b
let kib = 1024L
let mib = 1024L ** kib
let gib = 1024L ** mib

let base_platform_flags = ["acpi","true";"apic","true";"pae","true";Xapi_globs.viridian_key_name,Xapi_globs.default_viridian_key_value]
let with_nx_platform_flags = ("nx","true") :: base_platform_flags
let no_nx_platform_flags = ("nx","false") :: base_platform_flags

let default_template = (Xapi_globs.default_template_key, "true")
let linux_template = (Xapi_globs.linux_template_key, "true")

(* template restrictions (added to recommendations field for UI) *)
let recommendations ?(memory=32) ?(vcpus=8) ?(vbds=7) ?(vifs=7) () =
  let ( ** ) = Int64.mul in
    "<restrictions>"
    ^"<restriction field=\"memory-static-max\" max=\""^(Int64.to_string ((Int64.of_int memory) ** 1024L ** 1024L ** 1024L))^"\" />"
    ^"<restriction field=\"vcpus-max\" max=\""^(string_of_int vcpus)^"\" />"
    ^"<restriction property=\"number-of-vbds\" max=\""^(string_of_int vbds)^"\" />"
    ^"<restriction property=\"number-of-vifs\" max=\""^(string_of_int vifs)^"\" />"
    ^"</restrictions>"


open Client

let find_or_create_template x rpc session_id = 
  let all = Client.VM.get_by_name_label rpc session_id x.vM_name_label in
  (* CA-30238: Filter out _default templates_ *)
  let all = List.filter (fun self -> Client.VM.get_is_a_template rpc session_id self) all in
  let all = List.filter (fun self -> List.mem default_template (Client.VM.get_other_config rpc session_id self)) all in
  if all = []
  then Client.VM.create_from_record rpc session_id x
  else List.hd all

let version_of_tools_vdi rpc session_id vdi =
  let sm_config = Client.VDI.get_sm_config rpc session_id vdi in
  let version = List.assoc "xs-tools-version" sm_config in
  let build = 
    if List.mem_assoc "xs-tools-build" sm_config 
    then int_of_string (List.assoc "xs-tools-build" sm_config) else 0 in
  match List.map int_of_string (String.split '.' version) with
  | [ major; minor; micro ] -> major, minor, micro, build
  | _                       -> 0, 0, 0, build (* only if filename is malformed *)

(** Return a reference to a VDI in the XenSource Tools SR, identified by its 
    sm-config keys. We always invoke an SR.scan to be sure in the upgrade
    case we find the latest version of the VDI. Nb. we require the tools VDI
    to have sm-config keys: 'xs-tools=true' and 'xs-tools-version=x.y.z' where
    x,y,z are numbers. If the tools VDI has key 'xs-tools-build=b' then this is
    used to distinguish between builds of the tools VDI.  *)
let find_xs_tools_vdi rpc session_id = 
  (* Find the SR first *)
  let srs = List.filter (fun sr -> List.mem_assoc Xapi_globs.tools_sr_tag (Client.SR.get_other_config rpc session_id sr)) (Client.SR.get_all rpc session_id) in
  
  let find_vdi sr = 
    begin 
      try
	Client.SR.scan rpc session_id sr
      with e ->
	error "Scan of tools SR failed - exception was '%s'" (ExnHelper.string_of_exn e);
	error "Ignorining error and continuing"
    end;
    
    let vdis = Client.SR.get_VDIs rpc session_id sr in
    begin 
	match List.filter (fun self -> 
	  let sm_config = Client.VDI.get_sm_config rpc session_id self in
	  try List.assoc "xs-tools" sm_config = "true" with _ -> false) vdis 
	with
	  | [] -> None
	  | [ vdi ] -> Some vdi
	  | vdis -> 
	      let sorted = List.sort 
		(fun vdi1 vdi2 ->
		  let major1, minor1, micro1, build1 = version_of_tools_vdi rpc session_id vdi1 in
		  let major2, minor2, micro2, build2 = version_of_tools_vdi rpc session_id vdi2 in
		  0 +
		    8 * (compare major1 major2) +
		    4 * (compare minor1 minor2) +
		    2 * (compare micro1 micro2) +
		    1 * (compare build1 build2)
		) vdis in
	      let newest = List.hd (List.rev sorted) in
	      Some newest
    end in
  match srs with
    | [] -> warn "No Tools SR could be found"; None
    | [ sr ] -> find_vdi sr
    | sr::_ -> warn "Multiple Tools SRs detected, choosing one at random"; find_vdi sr
	
(** Return a reference to the guest installer network *)
let find_guest_installer_network rpc session_id = 
  let networks = Client.Network.get_all rpc session_id in
  let findfn x = 
    let other_config = Client.Network.get_other_config rpc session_id x in
    try bool_of_string (List.assoc Xapi_globs.is_guest_installer_network other_config) with _ -> false
  in
  try Some (List.find findfn networks) 
  with Not_found ->
    warn "Guest installer network not found";
    None

(** Values of memory parameters for templates. *)
type template_memory_parameters = {
	memory_static_min_mib : int64;
	memory_dynamic_min_mib : int64;
	memory_dynamic_max_mib : int64;
	memory_static_max_mib : int64;
}

(** Makes a default set of memory parameters from the given minimum value such
that memory_dynamic_{min,max} = memory_static_max = 2 * memory_static_min. *)
let default_memory_parameters memory_static_min_mib = {
	memory_static_min_mib = memory_static_min_mib;
	memory_dynamic_min_mib = memory_static_min_mib ** 2L;
	memory_dynamic_max_mib = memory_static_min_mib ** 2L;
	memory_static_max_mib = memory_static_min_mib ** 2L;
}

let blank_template memory = {
	vM_name_label = "blank template";
	vM_name_description = "a simple template example";
	vM_blocked_operations = [];
	vM_user_version = 1L;
	vM_is_a_template = true;
	vM_is_a_snapshot = false;
	vM_snapshot_of = Ref.null;
	vM_snapshots = [];
	vM_transportable_snapshot_id = "";
	vM_parent = Ref.null;
	vM_children = [];
	vM_snapshot_time = Date.never;
	vM_snapshot_info = [];
	vM_snapshot_metadata = "";
	vM_memory_overhead = (0L ** mib);
	vM_memory_static_max  = memory.memory_static_max_mib  ** mib;
	vM_memory_dynamic_max = memory.memory_dynamic_max_mib ** mib;
	vM_memory_target      = memory.memory_dynamic_max_mib ** mib;
	vM_memory_dynamic_min = memory.memory_dynamic_min_mib ** mib;
	vM_memory_static_min  = memory.memory_static_min_mib  ** mib;
	vM_VCPUs_params = [];
	vM_VCPUs_max = 1L;
	vM_VCPUs_at_startup = 1L;
	vM_actions_after_shutdown = `destroy;
	vM_actions_after_reboot = `restart;
	vM_actions_after_crash = `restart;
	vM_PV_bootloader = pv_bootloader; 
	vM_PV_kernel = "";
	vM_PV_ramdisk = "";
	vM_PV_args = "";
	vM_PV_bootloader_args = "";
	vM_PV_legacy_args = "";
	vM_HVM_boot_policy = "";
	vM_HVM_boot_params = [ ];
	vM_HVM_shadow_multiplier = 1.;
	vM_platform = no_nx_platform_flags;
	vM_PCI_bus = "";
	vM_other_config = [];
	vM_is_control_domain = false;
	vM_ha_restart_priority = "";
	vM_ha_always_run = false;

	(* These are ignored by the create call but required by the record type *)
	vM_uuid = "Invalid";
	vM_power_state = `Running;
	vM_suspend_VDI = Ref.null;
	vM_resident_on = Ref.null;
	vM_affinity = Ref.null;
	vM_allowed_operations = [];
	vM_current_operations = [];
	vM_last_booted_record = "";
	vM_consoles = [];
	vM_VIFs = [];
	vM_VBDs = [];
	vM_crash_dumps = [];
	vM_VTPMs = []; 
	vM_domid = (-1L);
	vM_domarch = "";
	vM_last_boot_CPU_flags = [];
	vM_metrics = Ref.null;
	vM_guest_metrics = Ref.null;
	vM_xenstore_data = [];
	vM_recommendations = (recommendations ());
	vM_blobs = [];
	vM_tags = [];
	
	vM_bios_strings = [];
}

let other_install_media_template memory = 
{
	(blank_template memory) with
	vM_name_label = "Other install media";
	vM_name_description =
		"Template which allows VM installation from install media";
	vM_PV_bootloader = ""; 
	vM_HVM_boot_policy = Constants.hvm_boot_policy_bios_order;
	vM_HVM_boot_params = [ Constants.hvm_boot_params_order, "dc" ];
	vM_other_config = [ install_methods_otherconfig_key, "cdrom" ];
}

let preferred_sr = "" (* None *)

let eli_install_template memory name distro nfs pv_args =
  let root = { device = "0"; size = (8L ** gib); sr = preferred_sr; bootable = true; _type = `system } in
  let distro_text = match distro with
  | "rhlike" -> "EL"
  | "sleslike" -> "SLES"
  | "debianlike" -> "Debian" in
  let text = if nfs then " or nfs:server:/<path>"
                    else ""
  in
  { (blank_template memory) with
      vM_name_label = name;
      vM_name_description = Printf.sprintf "Template that allows VM installation from Xen-aware %s-based distros. To use this template from the CLI, install your VM using vm-install, then set other-config-install-repository to the path to your network repository, e.g. http://<server>/<path>%s" distro_text text;
      vM_PV_bootloader = "eliloader";
      vM_PV_args = pv_args;
      vM_HVM_boot_policy = "";
      vM_other_config =
        [
          disks_key, Xml.to_string (xml_of_disks [ root ]);
	  distros_otherconfig_key, distro
        ];
  }

let debian_xgt_dir   = "/opt/xensource/packages/xgt/"
let post_install_dir = "/opt/xensource/packages/post-install-scripts/"

let debian_xgt_template rpc session_id name_label short_name_label debian_xgt_name post_install_script =
  let script = post_install_dir ^ post_install_script in
  let xgt = debian_xgt_dir ^ debian_xgt_name in
  let xgt_installed = try Unix.access xgt [ Unix.F_OK ]; true with _ -> false in
  if not(xgt_installed)
  then debug "Skipping %s template because post install script is missing" name_label
  else begin
    let root = { device = "0"; size = (4L ** gib); sr = preferred_sr; bootable = true; _type = `system } 
    and swap = { device = "1"; size = (512L ** mib); sr = preferred_sr; bootable = false; _type = `system } in
    
    let (_: API.ref_VM) = find_or_create_template 
      { (blank_template (default_memory_parameters 128L)) with
	  vM_name_label = name_label;
	  vM_name_description = Printf.sprintf "Clones of this template will automatically provision their storage when first booted and install Debian %s. The disk configuration is stored in the other_config field." short_name_label;
	  vM_other_config =
	  [
	    disks_key, Xml.to_string (xml_of_disks [ root; swap ]);
	    post_install_key, script;
	    default_template;
	    linux_template;
            install_methods_otherconfig_key, ""
	  ]
      } rpc session_id in
    ()
  end
    
(* The P2V server template *)
(* Requires: the xs-tools.iso in the XenSource Tools SR *)
let p2v_server_template rpc session_id =
  (* Find the server ISO *)
  match find_xs_tools_vdi rpc session_id with
  | None ->
      debug "Skipping P2V server template because the xs-tools.iso is missing"
  | Some iso ->
      begin match find_guest_installer_network rpc session_id with
      | None ->
	  debug "Skipping P2V server template because guest installer network missing"
      | Some net ->
	  let vm = find_or_create_template
	    { (blank_template (default_memory_parameters 256L)) with
		vM_name_label = "XenSource P2V Server";
		vM_name_description = "An internal utility template for use by the XenSource P2V client";
		vM_other_config = [ Xapi_globs.grant_api_access, "internal";
				    Xapi_globs.xensource_internal, "true";
				    default_template
				  ]
	    } rpc session_id in

	  let vbds = Client.VM.get_VBDs rpc session_id vm in
	  (* make a table of userdevice -> VBD reference, to check whether each VBD looks correct. *)
	  let table = List.map (fun vbd -> Client.VBD.get_userdevice rpc session_id vbd, vbd) vbds in
	  (* Empty CD on userdevice '3' *)
	  if not(List.mem_assoc "3" table) then begin
	    ignore (Client.VBD.create ~rpc ~session_id ~vM:vm ~empty:true ~vDI:(Ref.of_string "cd") ~userdevice:"3" ~bootable:false ~mode:`RO ~_type:`CD ~unpluggable:true ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~other_config:[])
	  end;
	  (* Tools ISO on userdevice 'xvdp': it's either missing or pointing at the wrong VDI *)
	  let xvdp = "xvdp" in (* beware the deadly typo *)
	  if false
	    || not(List.mem_assoc xvdp table)
	    || (Client.VBD.get_VDI rpc session_id (List.assoc xvdp table) <> iso) then begin
	      (* destroy the existing broken one *)
	      if List.mem_assoc xvdp table then Client.VBD.destroy rpc session_id (List.assoc xvdp table);
	      ignore (Client.VBD.create ~rpc ~session_id ~vM:vm ~empty:false ~vDI:iso ~userdevice:xvdp ~bootable:true ~mode:`RO ~_type:`CD ~unpluggable:true ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~other_config:[]);	      
	    end;
	  
	  let vifs = Client.VM.get_VIFs rpc session_id vm in
	  if vifs = [] 
	  then ignore (Client.VIF.create ~rpc ~session_id ~device:"0" ~mAC:(Record_util.random_mac_local ()) ~vM:vm ~mTU:1500L ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~network:net ~other_config:[])
      end

(** Makes a Windows template using the given memory parameters in MiB, root disk
size in GiB, and version string. *)
let windows_template memory root_disk_size version = 
	let root = {
		device = "0";
		size = (root_disk_size ** gib);
		sr = preferred_sr;
		bootable = false;
		_type = `system
	} in {
		(other_install_media_template memory) with
		vM_name_label = Printf.sprintf "Windows %s" version;
		vM_name_description =
			Printf.sprintf "Clones of this template will automatically \
			provision their storage when first booted and then reconfigure \
			themselves with the optimal settings for Windows %s." version;
		vM_other_config = [
			disks_key, Xml.to_string (xml_of_disks [ root ]);
			install_methods_otherconfig_key, "cdrom"
		]
	}

(* Make a Windows template which is the same as the normal Windows
   one in everything except that the NX platform flag is turned on *)
let windows_template_nx memory root_disk_size version =
	let tmpl = windows_template memory root_disk_size version in 
	{ tmpl with vM_platform = with_nx_platform_flags }

(* Create a CPS template for either 32- or 64-bit, with a higher shadow
   multiplier and the application_template=1 set in other_config *)
let cps_template tmplfn name = 
  let shadow_multipler = 4.0 in 
  let descr = Printf.sprintf "Clones of this template will automatically provision their storage when first booted, and then reconfigure themselves with the optimal settings for running %s on Windows 2003 Server." name in
  let tmpl = tmplfn 8L "" in
  { tmpl with vM_HVM_shadow_multiplier = shadow_multipler; 
     vM_name_description = descr; vM_name_label = name;
     vM_other_config = ("application_template", "1") :: tmpl.vM_other_config }

let create_all_templates rpc session_id =
  let rhel45_install_template name =
      let bt = eli_install_template (default_memory_parameters 256L) name "rhlike" true "graphical utf8" in
      { bt with
	  vM_recommendations = recommendations ~vifs:3 ();
          vM_other_config = (install_methods_otherconfig_key, "cdrom,nfs,http,ftp") ::
	                    ("suppress-spurious-page-faults", "true") :: 
			    bt.vM_other_config;
      } in
  (* machine address space is limited to 64G initially for RHEL 4.7 *)
  let rhel47_install_template name =
    let bt = rhel45_install_template name in
    { bt with
	vM_other_config = (Xapi_globs.machine_address_size_key_name, Xapi_globs.machine_address_size_key_value) :: bt.vM_other_config;
    } in
  (* the install_arch param should be passed in as either "i386" or "x86_64" ("i386" only support up to 16GB memory) *)
  let rhel50_install_template name install_arch =
      let bt = eli_install_template (default_memory_parameters 512L) name "rhlike" true "graphical utf8" in
      let recommendations = if install_arch = "i386" then recommendations ~memory:16 () 
                                                     else recommendations ()
      in
      { bt with 
          vM_other_config = (install_methods_otherconfig_key, "cdrom,nfs,http,ftp") :: ("rhel5","true") :: bt.vM_other_config;
          vM_recommendations = recommendations;
      } in
  (* machine address space is limited to 64G initially for RHEL 5.2 *)
  let rhel52_install_template name install_arch =
    let bt = rhel50_install_template name install_arch in
    { bt with
	vM_other_config = (Xapi_globs.machine_address_size_key_name, Xapi_globs.machine_address_size_key_value) :: bt.vM_other_config;
    } in
  (* the install_arch param should be passed in as either "i386" or "x86_64" *)
  let sles9_install_template name install_arch =
      let bt = eli_install_template (default_memory_parameters 256L) name "sleslike" true "console=ttyS0 xencons=ttyS" in
      { bt with 
	  vM_recommendations = recommendations ~vifs:3 ();
          vM_other_config = (install_methods_otherconfig_key, "nfs,http,ftp") :: ("install-arch",install_arch) :: bt.vM_other_config;
      } in
  let sles10_install_template name install_arch =
      let bt = eli_install_template (default_memory_parameters 512L) name "sleslike" true "console=ttyS0 xencons=ttyS" in
      { bt with 
	  vM_recommendations = recommendations ~vifs:3 ();
          vM_other_config = (install_methods_otherconfig_key, "cdrom,nfs,http,ftp") :: ("install-arch",install_arch) :: bt.vM_other_config;
      } in
  let sles11_install_template = sles10_install_template in
  let debian_install_template name release install_arch =
      let bt = eli_install_template (default_memory_parameters 128L) name "debianlike" false " -- quiet console=hvc0" in
      { bt with 
          vM_other_config = (install_methods_otherconfig_key, "cdrom,http,ftp") :: ("install-arch", install_arch) :: ("debian-release", release) :: bt.vM_other_config;
      } in
  let linux_static_templates =
    [
      rhel45_install_template "Red Hat Enterprise Linux 4.5";
      rhel45_install_template "CentOS 4.5";
      rhel45_install_template "Red Hat Enterprise Linux 4.6";
      rhel45_install_template "CentOS 4.6";
      rhel47_install_template "Red Hat Enterprise Linux 4.7";
      rhel47_install_template "CentOS 4.7";
      rhel47_install_template "Red Hat Enterprise Linux 4.8";
      rhel47_install_template "CentOS 4.8";
      rhel50_install_template "Red Hat Enterprise Linux 5.0" "i386";
      rhel50_install_template "Oracle Enterprise Linux 5.0" "i386";
      rhel50_install_template "CentOS 5.0" "i386";
      rhel50_install_template "Red Hat Enterprise Linux 5.1" "i386";
      rhel50_install_template "Oracle Enterprise Linux 5.1" "i386";
      rhel50_install_template "CentOS 5.1" "i386";
      rhel52_install_template "Red Hat Enterprise Linux 5.2" "i386";
      rhel52_install_template "Oracle Enterprise Linux 5.2" "i386";
      rhel52_install_template "CentOS 5.2" "i386";
      rhel52_install_template "Red Hat Enterprise Linux 5.3" "i386";
      rhel52_install_template "CentOS 5.3" "i386";
      rhel50_install_template "Red Hat Enterprise Linux 5.0 x64" "x86_64";
      rhel50_install_template "Oracle Enterprise Linux 5.0 x64" "x86_64";
      rhel50_install_template "CentOS 5.0 x64" "x86_64";
      rhel50_install_template "Red Hat Enterprise Linux 5.1 x64" "x86_64";
      rhel50_install_template "Oracle Enterprise Linux 5.1 x64" "x86_64";
      rhel50_install_template "CentOS 5.1 x64" "x86_64";
      rhel52_install_template "Red Hat Enterprise Linux 5.2 x64" "x86_64";
      rhel52_install_template "Oracle Enterprise Linux 5.2 x64" "x86_64";
      rhel52_install_template "CentOS 5.2 x64" "x86_64";
      rhel52_install_template "Red Hat Enterprise Linux 5.3 x64" "x86_64";
      rhel52_install_template "CentOS 5.3 x64" "x86_64";
      sles9_install_template "SUSE Linux Enterprise Server 9 SP4" "i386";
      sles10_install_template "SUSE Linux Enterprise Server 10 SP1" "i386";
      sles10_install_template "SUSE Linux Enterprise Server 10 SP1 x64" "x86_64";
      sles10_install_template "SUSE Linux Enterprise Server 10 SP2" "i386";
      sles10_install_template "SUSE Linux Enterprise Server 10 SP2 x64" "x86_64";
      sles11_install_template "SUSE Linux Enterprise Server 11" "i386";
      sles11_install_template "SUSE Linux Enterprise Server 11 x64" "x86_64";
      debian_install_template "Debian Lenny 5.0" "lenny" "i386"
    ] in

	let static_templates = [
		other_install_media_template (default_memory_parameters 128L);
		windows_template    (default_memory_parameters  256L)  8L "XP SP2";
		windows_template    (default_memory_parameters  256L)  8L "XP SP3";
		windows_template    (default_memory_parameters  256L)  8L "Server 2003";
		windows_template_nx (default_memory_parameters  256L)  8L "Server 2003 x64";
		windows_template_nx (default_memory_parameters  512L) 24L "Server 2008";
		windows_template_nx (default_memory_parameters  512L) 24L "Server 2008 x64";
		windows_template_nx (default_memory_parameters 1024L) 24L "Vista";
		windows_template_nx (default_memory_parameters 1024L) 24L "7";
		windows_template_nx (default_memory_parameters 2048L) 24L "7 x64";
		windows_template_nx (default_memory_parameters 2048L) 24L "Server 2008 R2 x64";
		cps_template (windows_template    (default_memory_parameters 256L)) "Citrix XenApp";
		cps_template (windows_template_nx (default_memory_parameters 256L)) "Citrix XenApp x64";
		begin
			let w2000sp4 = windows_template (default_memory_parameters 128L) 8L "2000 SP4" in
			{
				w2000sp4 with vM_name_description =
					"Windows 2000 Server SP4. " ^
					(w2000sp4.vM_name_description)
			}
		end
	] in

  (* put default_template key in static_templates other_config of static_templates: *)
  let static_templates =
    List.map (fun t -> {t with vM_other_config = default_template::t.vM_other_config}) static_templates in

  (* put default_template key and linux_template key in other_config for linux_static_templates: *)
  let linux_static_templates =
    List.map (fun t -> {t with vM_other_config = default_template::linux_template::t.vM_other_config}) linux_static_templates in

  (* Create the windows templates *)
  List.iter (fun x -> ignore(find_or_create_template x rpc session_id)) static_templates;

  (* NB we now create the 'static' linux templates whether or not the 'linux pack' is 
     installed because these only depend on eliloader, which is always installed *)
  List.iter (fun x -> ignore(find_or_create_template x rpc session_id)) linux_static_templates;
  (* The remaining template-creation functions determine whether they have the 
     necessary resources (ISOs, networks) or not: *)
  debian_xgt_template rpc session_id "Debian Etch 4.0" "Etch" "debian-etch.xgt" "debian-etch";
  p2v_server_template rpc session_id      

