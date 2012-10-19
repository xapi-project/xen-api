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
(*
 * Provide some helpers for XAPI
 *)

open Stringext
open Pervasiveext
open Threadext
open Printf
open Xapi_globs
open Db_filter
open Db_filter_types
open Xmlrpc_sexpr
open Api_errors

include Helper_hostname
include Helper_process

module Net = (val (Network.get_client ()) : Network.CLIENT)

module D=Debug.Debugger(struct let name="helpers" end)
open D

module StringSet = Set.Make(String)

let log_exn_continue msg f x = try f x with e -> debug "Ignoring exception: %s while %s" (ExnHelper.string_of_exn e) msg

(** Construct a descriptive network name (used as name_label) for a give network interface. *)
let choose_network_name_for_pif device =
  Printf.sprintf "Pool-wide network associated with %s" device

(** Once the server functor has been instantiated, set this reference to the appropriate
    "fake_rpc" (loopback non-HTTP) rpc function. This is used by the CLI, which passes in
    the HTTP request headers it has already received together with its active file descriptor. *)
let rpc_fun : (Http.Request.t -> Unix.file_descr -> Xml.xml -> Xml.xml) option ref = ref None

let get_rpc () =
  match !rpc_fun with
      None -> failwith "No rpc set!"
    | Some f -> f

(* !! FIXME - trap proper MISSINGREFERENCE exception when this has been defined *)
(* !! FIXME(2) - this code could be shared with the CLI? *)
let checknull f =
  try f() with
      _ -> "<not in database>"

let get_primary_ip_addr ~__context iface primary_address_type =
	if iface = "" then
		None
	else
		try
			let dbg = Context.string_of_task __context in
			let addrs = match primary_address_type with
				| `IPv4 -> Net.Interface.get_ipv4_addr dbg ~name:iface
				| `IPv6 -> Net.Interface.get_ipv6_addr dbg ~name:iface
			in
			let addrs = List.map (fun (addr, _) -> Unix.string_of_inet_addr addr) addrs in
			(* Filter out link-local addresses *)
			let addrs = List.filter (fun addr -> String.sub addr 0 4 <> "fe80") addrs in
			Some (List.hd addrs)
		with _ -> None

let get_management_ip_addr ~__context =
	get_primary_ip_addr ~__context
		(Xapi_inventory.lookup Xapi_inventory._management_interface)
		(Record_util.primary_address_type_of_string (Xapi_inventory.lookup Xapi_inventory._management_address_type ~default:"ipv4"))

let get_localhost_uuid () =
  Xapi_inventory.lookup Xapi_inventory._installation_uuid

let get_localhost ~__context : API.ref_host  =
    let uuid = get_localhost_uuid () in
	Db.Host.get_by_uuid ~__context ~uuid

let update_pif_address ~__context ~self =
	let network = Db.PIF.get_network ~__context ~self in
	let bridge = Db.Network.get_bridge ~__context ~self:network in
	let dbg = Context.string_of_task __context in
	try
		begin
			match Net.Interface.get_ipv4_addr dbg bridge with
			| (addr, plen) :: _ ->
				let ip = Unix.string_of_inet_addr addr in
				let netmask = Network_interface.prefixlen_to_netmask plen in
				if ip <> Db.PIF.get_IP ~__context ~self || netmask <> Db.PIF.get_netmask ~__context ~self then begin
					debug "PIF %s bridge %s IP address changed: %s/%s" (Db.PIF.get_uuid ~__context ~self) bridge ip netmask;
					Db.PIF.set_IP ~__context ~self ~value:ip;
					Db.PIF.set_netmask ~__context ~self ~value:netmask
				end
			| _ -> ()
		end;
		let ipv6_addr = Net.Interface.get_ipv6_addr dbg ~name:bridge in
		let ipv6_addr' = List.map (fun (addr, plen) -> Printf.sprintf "%s/%d" (Unix.string_of_inet_addr addr) plen) ipv6_addr in
		if ipv6_addr' <> Db.PIF.get_IPv6 ~__context ~self then begin
			debug "PIF %s bridge %s IPv6 address changed: %s" (Db.PIF.get_uuid ~__context ~self)
				bridge (String.concat "; " ipv6_addr');
			Db.PIF.set_IPv6 ~__context ~self ~value:ipv6_addr'
		end
	with _ ->
		debug "Bridge %s is not up; not updating IP" bridge

let update_pif_addresses ~__context =
	debug "Updating IP addresses in DB for DHCP and autoconf PIFs";
	let host = get_localhost ~__context in
	let pifs = Db.PIF.get_refs_where ~__context ~expr:(
		And (
			Eq (Field "host", Literal (Ref.string_of host)),
			Or (
				Or (
					(Eq (Field "ip_configuration_mode", Literal "DHCP")),
					(Eq (Field "ipv6_configuration_mode", Literal "DHCP"))
				),
				(Eq (Field "ipv6_configuration_mode", Literal "Autoconf"))
			)
		)
	) in
	List.iter (fun self -> update_pif_address ~__context ~self) pifs

let make_rpc ~__context xml : XMLRPC.xmlrpc =
    let subtask_of = Ref.string_of (Context.get_task_id __context) in
	let open Xmlrpc_client in
	let http = xmlrpc ~subtask_of ~version:"1.1" "/" in
	let transport =
		if Pool_role.is_master ()
		then Unix(Xapi_globs.unix_domain_socket)
		else SSL(SSL.make ~use_stunnel_cache:true (), Pool_role.get_master_address(), !Xapi_globs.https_port) in
	XML_protocol.rpc ~srcstr:"xapi" ~dststr:"xapi" ~transport ~http xml

(* This one uses rpc-light *)
let make_remote_rpc remote_address xml =
	let open Xmlrpc_client in
	let transport = SSL(SSL.make (), remote_address, !Xapi_globs.https_port) in
	let http = xmlrpc ~version:"1.0" "/" in
	XML_protocol.rpc ~srcstr:"xapi" ~dststr:"remote_xapi" ~transport ~http xml

(** Log into pool master using the client code, call a function
    passing it the rpc function and session id, logout when finished. *)
let call_api_functions ~__context f =
  let rpc = make_rpc ~__context in
  (* let () = debug "logging into master" in *)
  (* If we're the master then our existing session may be a client one without 'pool' flag set, so
     we consider making a new one. If we're a slave then our existing session (if we have one) must
     have the 'pool' flag set because it would have been created for us in the message forwarding layer
     in the master, so we just re-use it. [If we haven't got an existing session in our context then
     we always make a new one *)
  let require_explicit_logout = ref false in
  let do_master_login () =
    let session = Client.Client.Session.slave_login rpc (get_localhost ~__context) !Xapi_globs.pool_secret in
      require_explicit_logout := true;
      session
  in
  let session_id =
      try
        if Pool_role.is_master() then
      begin
        let session_id = Context.get_session_id __context in
          if Db.Session.get_pool ~__context ~self:session_id
          then session_id
          else do_master_login ()
      end
      else
          let session_id = Context.get_session_id __context in
          (* read any attr to test if session is still valid *)
				ignore (Db.Session.get_pool ~__context ~self:session_id) ;
          session_id
      with _ ->
      do_master_login ()
  in
  (* let () = debug "login done" in *)
  finally
    (fun () -> f rpc session_id)
    (fun () ->
       (* debug "remote client call finished; logging out"; *)
       if !require_explicit_logout
      then Client.Client.Session.logout rpc session_id)

let call_emergency_mode_functions hostname f =
	let open Xmlrpc_client in
	let transport = SSL(SSL.make (), hostname, !Xapi_globs.https_port) in
	let http = xmlrpc ~version:"1.0" "/" in
	let rpc = XML_protocol.rpc ~srcstr:"xapi" ~dststr:"xapi" ~transport ~http in
  let session_id = Client.Client.Session.slave_local_login rpc !Xapi_globs.pool_secret in
  finally
    (fun () -> f rpc session_id)
    (fun () -> Client.Client.Session.local_logout rpc session_id)

let progress ~__context t =
    for i = 0 to int_of_float (t *. 100.) do
        let v = (float_of_int i /. 100.) /. t in

        TaskHelper.set_progress ~__context v;
        Thread.delay 1.
    done;
    TaskHelper.set_progress ~__context 1.

let get_user ~__context username =
    let uuids = Db.User.get_all ~__context in
    if List.length uuids = 0 then
        failwith "Failed to find any users";
    List.hd uuids (* FIXME! it assumes that there is only one element in the list (root), username is not used*)

(* Expects only 1 control domain per host; just return first in list for now if multiple.. *)
exception No_domain_zero of string
let domain_zero_ref_cache = ref None
let domain_zero_ref_cache_mutex = Mutex.create ()
let get_domain_zero ~__context : API.ref_VM =
  Threadext.Mutex.execute domain_zero_ref_cache_mutex
    (fun () ->
       match !domain_zero_ref_cache with
       Some r -> r
     | None ->
	 (* Read the control domain uuid from the inventory file *)
	 let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
	 try
	   let vm = Db.VM.get_by_uuid ~__context ~uuid in
	   if not (Db.VM.get_is_control_domain ~__context ~self:vm) then begin
	     error "VM uuid %s is not a control domain but the uuid is in my inventory file" uuid;
	     raise (No_domain_zero uuid);
	   end;
	   domain_zero_ref_cache := Some vm;
	   vm
	 with _ ->
	   error "Failed to find control domain (uuid = %s)" uuid;
	   raise (No_domain_zero uuid)
    )

let get_size_with_suffix s =
    let s, suffix = if String.length s > 0 then (
        let c = s.[String.length s - 1] in
        if List.mem c [ 'G'; 'g'; 'M'; 'm'; 'K'; 'k'; 'B'; 'b' ] then (
            let suffix = match c with
            | 'G' | 'g' -> 30
            | 'M' | 'm' -> 20
            | 'K' | 'k' -> 10
            | 'B' | 'b' -> 0
            | _ -> 10 in
            String.sub s 0 (String.length s - 1), suffix
        ) else
            s, 10
    ) else
        s, 10 in
    Int64.shift_left (if String.contains s '.' then
        (Int64.of_float (float_of_string s)) else Int64.of_string s) suffix


(** An HVM boot has the following user-settable parameters: *)
type hvm_boot_t = { timeoffset: string }

(** A 'direct' PV boot (one that is not indirected through a bootloader) has
    the following options: *)
type direct_pv_boot_t = { kernel: string; kernel_args: string; ramdisk: string option }

(** An 'indirect' PV boot (one that defers to a bootloader) has the following
    options: *)
type indirect_pv_boot_t =
    { bootloader: string;     (** bootloader to use (eg "pygrub") *)
      extra_args: string;     (** extra commandline arguments to pass bootloader for the kernel *)
      legacy_args: string;    (** "legacy" args to cope with Zurich/Geneva guests *)
      pv_bootloader_args: string; (** misc arguments for the bootloader itself *)
      vdis: API.ref_VDI list; (** list of bootable VDIs *)
    }

(** A type which represents the boot method a guest is configured to use *)
type boot_method =
    | HVM of hvm_boot_t
    | DirectPV of direct_pv_boot_t
    | IndirectPV of indirect_pv_boot_t

let string_of_option opt = match opt with None -> "(none)" | Some s -> s

let string_of_boot_method = function
	| HVM _ -> "HVM"
  | DirectPV x ->
      Printf.sprintf "Direct PV boot with kernel = %s; args = %s; ramdisk = %s"
    x.kernel x.kernel_args (string_of_option x.ramdisk)
  | IndirectPV x ->
      Printf.sprintf "Indirect PV boot via bootloader %s; extra_args = %s; legacy_args = %s; bootloader_args = %s; VDIs = [ %s ]"
    x.bootloader x.extra_args x.legacy_args x.pv_bootloader_args
    (String.concat "; " (List.map Ref.string_of  x.vdis))

(** Returns the current value of the pool configuration flag *)
(** that indicates whether a rolling upgrade is in progress. *)
(* Note: the reason it's OK to trap exceptions and return false is that -- an exn will only happen if the pool record
   is not present in the database; that only happens on firstboot (when you're a master with no db and you're creating
   the db for the first time). In that context you cannot be in rolling upgrade mode *)
let rolling_upgrade_in_progress ~__context =
	try
		let pool = List.hd (Db.Pool.get_all ~__context) in
		List.mem_assoc Xapi_globs.rolling_upgrade_in_progress (Db.Pool.get_other_config ~__context ~self:pool)
	with _ ->
		false

let parse_boot_record ~string:lbr =
	match Xmlrpc_sexpr.sexpr_str_to_xmlrpc lbr with
	| None     -> API.From.vM_t "ret_val" (Xml.parse_string lbr)
	| Some xml -> API.From.vM_t "ret_val" xml

(** Fetch the configuration the VM was booted with *)
let get_boot_record_of_record ~__context ~string:lbr ~uuid:current_vm_uuid =
  try
    parse_boot_record lbr
  with e ->
	(* warn "Warning: exception '%s' parsing last booted record (%s) - returning current record instead" lbr (ExnHelper.string_of_exn e); *)
    Db.VM.get_record ~__context ~self:(Db.VM.get_by_uuid ~__context ~uuid:current_vm_uuid)

let get_boot_record ~__context ~self =
  let r = Db.VM.get_record_internal ~__context ~self in
  let lbr = get_boot_record_of_record ~__context ~string:r.Db_actions.vM_last_booted_record ~uuid:r.Db_actions.vM_uuid in
  (* CA-31903: we now use an unhealthy mix of fields from the boot_records and the live VM.
     In particular the VM is currently using dynamic_min and max from the live VM -- not the boot-time settings. *)
  { lbr with
      API.vM_memory_target = 0L;
      API.vM_memory_dynamic_min = r.Db_actions.vM_memory_dynamic_min;
      API.vM_memory_dynamic_max = r.Db_actions.vM_memory_dynamic_max;
  }


let set_boot_record ~__context ~self newbootrec =
  (* blank last_booted_record field in newbootrec, so we don't just keep encapsulating
     old last_boot_records in new snapshots! *)
  let newbootrec = {newbootrec with API.vM_last_booted_record=""; API.vM_bios_strings=[]} in
  if rolling_upgrade_in_progress ~__context then
    begin
    (* during a rolling upgrade, there might be slaves in the pool
       who have not yet been upgraded to understand sexprs, so
       let's still talk using the legacy xmlrpc format.
    *)
    let xml = Xml.to_string (API.To.vM_t newbootrec) in
    Db.VM.set_last_booted_record ~__context ~self ~value:xml
  end
  else
  begin
    (* if it's not a rolling upgrade, then we know everyone
       else in the pool will understand s-expressions.
    *)
    let sexpr = Xmlrpc_sexpr.xmlrpc_to_sexpr_str (API.To.vM_t newbootrec) in
    Db.VM.set_last_booted_record ~__context ~self ~value:sexpr
  end;
  ()

(** Inspect the current configuration of a VM and return a boot_method type *)
let boot_method_of_vm ~__context ~vm =
    if vm.API.vM_HVM_boot_policy <> "" then begin
        (* hvm_boot describes the HVM boot order. How? as a qemu-dm -boot param? *)
	let timeoffset = try List.assoc "timeoffset" vm.API.vM_platform with _ -> "0" in
        HVM { timeoffset = timeoffset }
    end else begin
        (* PV *)
        if vm.API.vM_PV_bootloader = "" then begin
            let kern = vm.API.vM_PV_kernel
            and args = vm.API.vM_PV_args
            and ramdisk = if vm.API.vM_PV_ramdisk <> "" then (Some vm.API.vM_PV_ramdisk) else None in
            DirectPV { kernel = kern; kernel_args = args; ramdisk = ramdisk }
        end else begin
            (* Extract the default kernel from the boot disk via bootloader *)
            (* NB We allow multiple bootable VDIs, in which case the
               bootloader gets to choose. Note that a VM may have no
               bootable VDIs; this might happen for example if the
               bootloader intends to PXE boot *)
            let bootable = List.filter
              (fun self -> Db.VBD.get_bootable ~__context ~self)
              vm.API.vM_VBDs in
	    let non_empty = List.filter
	      (fun self -> not (Db.VBD.get_empty ~__context ~self))
	      bootable in
            let boot_vdis =
              List.map
                (fun self -> Db.VBD.get_VDI ~__context ~self) non_empty in
            IndirectPV
              { bootloader = vm.API.vM_PV_bootloader;
                extra_args = vm.API.vM_PV_args;
                legacy_args = vm.API.vM_PV_legacy_args;
                pv_bootloader_args = vm.API.vM_PV_bootloader_args;
                vdis = boot_vdis }
        end
    end

(** Returns true if the supplied VM configuration is HVM.
    NB that just because a VM's current configuration looks like HVM doesn't imply it
    actually booted that way; you must check the boot_record to be sure *)
let is_hvm (x: API.vM_t) = not(x.API.vM_is_control_domain) && x.API.vM_HVM_boot_policy <> ""

let will_boot_hvm ~__context ~self = Db.VM.get_HVM_boot_policy ~__context ~self <> ""

let has_booted_hvm ~__context ~self =
  (not (Db.VM.get_is_control_domain ~__context ~self))
  &&
    let boot_record = get_boot_record ~__context ~self in
    boot_record.API.vM_HVM_boot_policy <> ""

let has_booted_hvm_of_record ~__context r =
  (not (r.Db_actions.vM_is_control_domain))
  &&
    let boot_record = get_boot_record_of_record ~__context ~string:r.Db_actions.vM_last_booted_record ~uuid:r.Db_actions.vM_uuid in
    boot_record.API.vM_HVM_boot_policy <> ""

let is_running ~__context ~self = Db.VM.get_domid ~__context ~self <> -1L

let devid_of_vif ~__context ~self =
    int_of_string (Db.VIF.get_device ~__context ~self)

exception Device_has_no_VIF

let vif_of_devid ~__context ~vm devid =
    let vifs = Db.VM.get_VIFs ~__context ~self:vm in
    let devs = List.map (fun self -> devid_of_vif ~__context ~self) vifs in
    let table = List.combine devs vifs in
    let has_vif = List.mem_assoc devid table in
    if not(has_vif)
    then raise Device_has_no_VIF
    else List.assoc devid table

(** Return the domid on the *local host* associated with a specific VM.
	Note that if this is called without the VM lock then the result is undefined: the
	domid might immediately change after the call returns. Caller beware! *)
let domid_of_vm ~__context ~self =
  let uuid = Uuid.uuid_of_string (Db.VM.get_uuid ~__context ~self) in
  let all = Xenctrl.with_intf (fun xc -> Xenctrl.domain_getinfolist xc 0) in
  let open Xenctrl.Domain_info in
  let uuid_to_domid = List.map (fun di -> Uuid.uuid_of_int_array di.handle, di.domid) all in
  if List.mem_assoc uuid uuid_to_domid
  then List.assoc uuid uuid_to_domid
  else -1 (* for backwards compat with old behaviour *)


let get_special_network other_config_key ~__context =
  let nets = Db.Network.get_all ~__context in
  let findfn net =
    let other_config = Db.Network.get_other_config ~__context ~self:net in
    try bool_of_string (List.assoc other_config_key other_config) with _ -> false
  in
  (* Assume there's only one of these! *)
  List.find findfn nets

let get_guest_installer_network = get_special_network is_guest_installer_network
let get_host_internal_management_network = get_special_network is_host_internal_management_network

(* -------------------------------------------------------------------------------------------------
    Storage related helpers
   ------------------------------------------------------------------------------------------------- *)


let get_my_pbds __context =
  let localhost = get_localhost __context in
  let localhost = Ref.string_of localhost in
    Db.PBD.get_records_where ~__context ~expr:(Eq(Field "host", Literal localhost))

(* Return the PBD for specified SR on a specific host *)
(* Just say an SR is shared if it has more than one PBD *)
let is_sr_shared ~__context ~self = List.length (Db.SR.get_PBDs ~__context ~self) > 1
(* This fn is only executed by master *)
let get_shared_srs ~__context =
  let srs = Db.SR.get_all ~__context in
  List.filter (fun self -> is_sr_shared ~__context ~self) srs

let get_pool ~__context = List.hd (Db.Pool.get_all ~__context)

let get_main_ip_address ~__context =
  try Pool_role.get_master_address () with _ -> "127.0.0.1"

let is_pool_master ~__context ~host =
	let pool = get_pool ~__context in
	let host_id = Db.Host.get_uuid ~__context ~self:host in
	let master = Db.Pool.get_master ~__context ~self:pool in
	let master_id = Db.Host.get_uuid ~__context ~self:master in
	host_id = master_id

(* Host version compare helpers *)
let compare_int_lists : int list -> int list -> int =
	fun a b ->
		let first_non_zero is = List.fold_left (fun a b -> if (a<>0) then a else b) 0 is in
		first_non_zero (List.map2 compare a b)

let version_string_of : __context:Context.t -> API.ref_host -> string =
	fun ~__context host ->
		try
			List.assoc Xapi_globs._platform_version
				(Db.Host.get_software_version ~__context ~self:host)
		with Not_found ->
			Xapi_globs.default_platform_version

let version_of : __context:Context.t -> API.ref_host -> int list =
	fun ~__context host ->
		let vs = version_string_of ~__context host
		in List.map int_of_string (String.split '.' vs)

(* Compares host versions, analogous to Pervasives.compare. *)
let compare_host_platform_versions : __context:Context.t -> API.ref_host -> API.ref_host -> int =
	fun ~__context host_a host_b ->
		let version_of = version_of ~__context in
		compare_int_lists (version_of host_a) (version_of host_b)

let max_version_in_pool : __context:Context.t -> int list =
	fun ~__context ->
		let max_version a b = if a = [] then b else if (compare_int_lists a b) > 0 then a else b
		and versions = List.map (version_of ~__context) (Db.Host.get_all ~__context) in
		List.fold_left max_version [] versions

let rec string_of_int_list : int list -> string = function
		[]    -> ""
	| (x::xs) ->
			if xs == []
			then string_of_int x
			else string_of_int x ^ "." ^ string_of_int_list xs

let host_has_highest_version_in_pool : __context:Context.t -> host:API.ref_host -> bool =
	fun ~__context ~host ->
		let host_version = version_of ~__context host
		and max_version = max_version_in_pool ~__context in
		(compare_int_lists host_version max_version) >= 0

let host_versions_not_decreasing ~__context ~host_from ~host_to =
	compare_host_platform_versions ~__context host_from host_to <= 0

let is_platform_version_same_on_master ~__context ~host =
	if is_pool_master ~__context ~host then true else
	let pool = get_pool ~__context in
	let master = Db.Pool.get_master ~__context ~self:pool in
	compare_host_platform_versions ~__context master host = 0

let assert_platform_version_is_same_on_master ~__context ~host ~self =
	if not (is_platform_version_same_on_master ~__context ~host) then
		raise (Api_errors.Server_error (Api_errors.vm_host_incompatible_version,
			[Ref.string_of host; Ref.string_of self]))

(** PR-1007 - block operations during rolling upgrade *)

(* Assertion functions which raise an exception if certain invariants
   are broken during an upgrade. *)
let assert_rolling_upgrade_not_in_progress : __context:Context.t -> unit =
	fun ~__context ->
		if rolling_upgrade_in_progress ~__context then
			raise (Api_errors.Server_error (Api_errors.not_supported_during_upgrade, []))

let assert_host_has_highest_version_in_pool : __context:Context.t -> host:API.ref_host -> unit =
	fun ~__context ~host ->
		if not (host_has_highest_version_in_pool ~__context ~host:host) then
			raise (Api_errors.Server_error (Api_errors.not_supported_during_upgrade, []))

let assert_host_versions_not_decreasing :
		__context:Context.t -> host_from:API.ref_host -> host_to:API.ref_host -> unit =
	fun ~__context ~host_from ~host_to ->
		if not (host_versions_not_decreasing ~__context ~host_from ~host_to) then
			raise (Api_errors.Server_error (Api_errors.not_supported_during_upgrade, []))

(** Indicates whether ballooning is enabled for the given virtual machine. *)
let ballooning_enabled_for_vm ~__context vm_record =
	not vm_record.API.vM_is_control_domain

let get_vm_metrics ~__context ~self =
    let metrics = Db.VM.get_metrics ~__context ~self in
    if metrics = Ref.null
    then failwith "Could not locate VM_metrics object for VM: internal error"
    else metrics
let get_vbd_metrics ~__context ~self =
    let metrics = Db.VBD.get_metrics ~__context ~self in
    if metrics = Ref.null
    then failwith "Could not locate VBD_metrics object for VBD: internal error"
    else metrics
let get_vif_metrics ~__context ~self =
    let metrics = Db.VIF.get_metrics ~__context ~self in
    if metrics = Ref.null
    then failwith "Could not locate VIF_metrics object for VIF: internal error"
    else metrics

(* Lookup a VDI field from a list of pre-fetched records *)
let lookup_vdi_fields f vdi_refs l =
  let rec do_lookup ref l =
    match l with
    [] -> None
      | ((r,rcd)::rs) -> if ref=r then Some (f rcd) else do_lookup ref rs in
  let field_ops = List.map (fun r->do_lookup r l) vdi_refs in
  List.fold_right (fun m acc -> match m with None -> acc | Some x -> x :: acc) field_ops []

(* Read pool secret if it exists; otherwise, create a new one. *)
let get_pool_secret () =
	try
		Unix.access Constants.pool_secret_path [Unix.F_OK];
		pool_secret := Unixext.string_of_file Constants.pool_secret_path
	with _ -> (* No pool secret exists. *)
		let mk_rand_string () = Uuid.to_string (Uuid.make_uuid()) in
		pool_secret := (mk_rand_string()) ^ "/" ^ (mk_rand_string()) ^ "/" ^ (mk_rand_string());
		Unixext.write_string_to_file Constants.pool_secret_path !pool_secret

(* Checks that a host has a PBD for a particular SR (meaning that the
   SR is visible to the host) *)
let host_has_pbd_for_sr ~__context ~host ~sr =
	try
		let sr_pbds = Db.SR.get_PBDs ~__context ~self:sr in
		let sr_host_pbd = List.filter
			(fun pbd -> host = Db.PBD.get_host ~__context ~self:pbd)
			sr_pbds
		in sr_host_pbd <> [] (* empty list means no PBDs *)
	with _ -> false

(* Checks if an SR exists, returning an SR ref option (None if it is missing) *)
let check_sr_exists ~__context ~self =
	try ignore(Db.SR.get_uuid ~__context ~self); Some self with _ -> None

(* Checks that an SR exists, and is visible to a host *)
let check_sr_exists_for_host ~__context ~self ~host =
	if host_has_pbd_for_sr ~__context ~host ~sr:self
	then Some self
	else None

(* Returns an SR suitable for suspending this VM *)
let choose_suspend_sr ~__context ~vm =
    (* If the VM.suspend_SR exists, use that. If it fails, try the Pool.suspend_image_SR. *)
    (* If that fails, try the Host.suspend_image_SR. *)
    let vm_sr = Db.VM.get_suspend_SR ~__context ~self:vm in
    let pool = get_pool ~__context in
    let pool_sr = Db.Pool.get_suspend_image_SR ~__context ~self:pool in
    let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
    let host_sr = Db.Host.get_suspend_image_sr ~__context ~self:resident_on in

    match
      check_sr_exists_for_host ~__context ~self:vm_sr   ~host:resident_on,
      check_sr_exists_for_host ~__context ~self:pool_sr ~host:resident_on,
      check_sr_exists_for_host ~__context ~self:host_sr ~host:resident_on
    with
    | Some x, _, _ -> x
    | _, Some x, _ -> x
    | _, _, Some x -> x
    | None, None, None ->
        raise (Api_errors.Server_error (Api_errors.vm_no_suspend_sr, [Ref.string_of vm]))

(* Returns an SR suitable for receiving crashdumps of this VM *)
let choose_crashdump_sr ~__context ~vm =
    (* If the Pool.crashdump_SR exists, use that. Otherwise try the Host.crashdump_SR *)
    let pool = get_pool ~__context in
    let pool_sr = Db.Pool.get_crash_dump_SR ~__context ~self:pool in
    let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
    let host_sr = Db.Host.get_crash_dump_sr ~__context ~self:resident_on in
    match check_sr_exists ~__context ~self:pool_sr, check_sr_exists ~__context ~self:host_sr with
    | Some x, _ -> x
    | _, Some x -> x
    | None, None ->
        raise (Api_errors.Server_error (Api_errors.vm_no_crashdump_sr, [Ref.string_of vm]))

(* return the operations filtered for cancels functions *)
let cancel_tasks ~__context ~ops ~all_tasks_in_db (* all tasks in database *) ~task_ids (* all tasks to explicitly cancel *) ~set =
  let cancel_splitset_taskid set1 taskids =
    let su1 = ref [] and c = ref false in
    let into (e, _) l = List.mem e l in
    (* If it's a task we want to explicitly cancel or a task which doesn't exist in the
       database at all then we should cancel it. *)
    List.iter (fun s1 -> if into s1 taskids || not(List.mem (Ref.of_string (fst s1)) all_tasks_in_db) then c := true else su1 := s1 :: !su1) set1;
    !su1, !c
    in
  let unique_ops, got_common = cancel_splitset_taskid ops task_ids in
  if got_common then
    set unique_ops

(** Returns true if the media is removable.
    Currently this just means "CD" but might change in future? *)
let is_removable ~__context ~vbd = Db.VBD.get_type ~__context ~self:vbd = `CD

let is_tools_sr_cache = ref []
let is_tools_sr_cache_m = Mutex.create ()

let clear_tools_sr_cache () =
	Mutex.execute is_tools_sr_cache_m 
		(fun () -> is_tools_sr_cache := [])

(** Returns true if this SR is the XenSource Tools SR *)
let is_tools_sr ~__context ~sr =
	try
		Mutex.execute is_tools_sr_cache_m
			(fun () -> List.assoc sr !is_tools_sr_cache)
	with Not_found _ ->
		let other_config = Db.SR.get_other_config ~__context ~self:sr in
		(* Miami GA *)
		let result =
			List.mem_assoc Xapi_globs.tools_sr_tag other_config
			(* Miami beta2 and earlier: *)
			|| (List.mem_assoc Xapi_globs.xensource_internal other_config)
		in
		Mutex.execute is_tools_sr_cache_m
			(fun () ->
				let cache = !is_tools_sr_cache in
				if not (List.mem_assoc sr cache) then
					is_tools_sr_cache := (sr, result) :: !is_tools_sr_cache);
		result

(** Return true if the MAC is in the right format XX:XX:XX:XX:XX:XX *)
let is_valid_MAC mac =
    let l = String.split ':' mac in
    let validchar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') in
    List.length l = 6 && (List.fold_left (fun acc s -> acc && String.length s = 2 && validchar s.[0] && validchar s.[1]) true l)

(** Returns Some Unix.PF_INET or Some Unix.PF_INET6 if passed a valid IP address, otherwise returns None. *)
let validate_ip_address str =
	try
		let addr = Unix.inet_addr_of_string str in
		Some (Unix.domain_of_sockaddr (Unix.ADDR_INET (addr, 1)))
	with _ -> None

(** Returns true if the supplied IP address looks like one of mine *)
let this_is_my_address ~__context address =
  let dbg = Context.string_of_task __context in
  let inet_addrs = Net.Interface.get_ipv4_addr dbg ~name:(Xapi_inventory.lookup Xapi_inventory._management_interface) in
  let addresses = List.map Unix.string_of_inet_addr (List.map fst inet_addrs) in
  List.mem address addresses

(** Returns the list of hosts thought to be live *)
let get_live_hosts ~__context =
  let hosts = Db.Host.get_all ~__context in
  List.filter (fun self ->
         let metrics = Db.Host.get_metrics ~__context ~self in
         try Db.Host_metrics.get_live ~__context ~self:metrics with _ -> false) hosts

let gethostbyname_family host family =
  let throw_resolve_error() = failwith (Printf.sprintf "Couldn't resolve hostname: %s" host) in
  let getaddr x = match x with Unix.ADDR_INET (addr, port) -> addr | _ -> failwith "Expected ADDR_INET" in
  let he = Unix.getaddrinfo host "" [ Unix.AI_SOCKTYPE(Unix.SOCK_STREAM); Unix.AI_FAMILY(family) ] in
  if List.length he = 0
  then throw_resolve_error();
  Unix.string_of_inet_addr (getaddr (List.hd he).Unix.ai_addr)

(** Return the first address we find for a hostname *)
let gethostbyname host =
  let throw_resolve_error() = failwith (Printf.sprintf "Couldn't resolve hostname: %s" host) in
  let pref = Record_util.primary_address_type_of_string
      (Xapi_inventory.lookup Xapi_inventory._management_address_type) in
  try
    gethostbyname_family host (if (pref == `IPv4) then Unix.PF_INET else Unix.PF_INET6)
  with _ ->
    (try
      gethostbyname_family host (if (pref = `IPv4) then Unix.PF_INET6 else Unix.PF_INET)
     with _ -> throw_resolve_error())

(** Indicate whether VM.clone should be allowed on suspended VMs *)
let clone_suspended_vm_enabled ~__context =
  try
    let pool = get_pool ~__context in
    let other_config = Db.Pool.get_other_config ~__context ~self:pool in
    List.mem_assoc Xapi_globs.pool_allow_clone_suspended_vm other_config
      && (List.assoc Xapi_globs.pool_allow_clone_suspended_vm other_config = "true")
  with _ -> false

(* OEM Related helper functions *)
let is_oem ~__context ~host =
  let software_version = Db.Host.get_software_version ~__context ~self:host in
  List.mem_assoc "oem_build_number" software_version

let on_oem ~__context =
  let this_host = !Xapi_globs.localhost_ref in
    is_oem ~__context ~host:this_host

exception File_doesnt_exist of string

let find_partition_path = Filename.concat Fhs.libexecdir "find-partition"

let find_secondary_partition () =
	try
		let other_partition,_ = Forkhelpers.execute_command_get_output find_partition_path ["-p"; "alternate"] in
		(* Sanity check: does it exist? *)
		let () =
			if not (Sys.file_exists other_partition)
			then raise (File_doesnt_exist other_partition)
		in
		other_partition
	with e ->
		debug "Cannot find secondary system image partition: %s" (Printexc.to_string e);
		raise (Api_errors.Server_error(Api_errors.cannot_find_oem_backup_partition,
																	 [Printexc.to_string e]))

let call_script ?(log_successful_output=true) script args =
  try
    Unix.access script [ Unix.X_OK ];
	(* Use the same $PATH as xapi *)
	let env = [| "PATH=" ^ (Sys.getenv "PATH") |] in
    let output, _ = Forkhelpers.execute_command_get_output ~env script args in
    if log_successful_output then debug "%s %s succeeded [ output = '%s' ]" script (String.concat " " args) output;
    output
  with
  | Unix.Unix_error _ as e ->
      debug "Assuming script %s doesn't exist: caught %s" script (ExnHelper.string_of_exn e);
      raise e
  | Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n) as e->
      debug "%s %s exited with code %d [stdout = '%s'; stderr = '%s']" script (String.concat " " args) n stdout stderr;
      raise e

(* Repeatedly bisect a range to find the maximum value for which the monotonic function returns true *)
let rec bisect f lower upper =
  let ( /* ) = Int64.div and ( -* ) = Int64.sub and ( +* ) = Int64.add in
  assert (lower <= upper);
  if upper -* lower < 2L
  then (if f upper then upper else lower)
  else
    (* there must be a distinct midpoint integer *)
    let mid = (upper +* lower) /* 2L in
    assert ((lower < mid) && (mid < upper));
    if f mid
    then bisect f mid upper
    else bisect f lower mid

(* All non best-effort VMs which are running should be kept running at all costs *)
let vm_should_always_run always_run restart_priority =
  always_run && restart_priority = Constants.ha_restart

(* Returns true if the specified VM is "protected" (non best-effort) by xHA *)
let is_xha_protected ~__context ~self =
  vm_should_always_run (Db.VM.get_ha_always_run ~__context ~self) (Db.VM.get_ha_restart_priority ~__context ~self)
let is_xha_protected_r record = vm_should_always_run record.API.vM_ha_always_run record.API.vM_ha_restart_priority

open Listext

let subset a b = List.fold_left (fun acc x -> acc && (List.mem x b)) true a

(* Only returns true if the SR is marked as shared, all hosts have PBDs and all PBDs are currently_attached.
   Is used to prevent a non-shared disk being added to a protected VM *)
let is_sr_properly_shared ~__context ~self =
  let shared = Db.SR.get_shared ~__context ~self in
  if not shared then begin
    false
  end else begin
    let pbds = Db.SR.get_PBDs ~__context ~self in
    let plugged_pbds = List.filter (fun pbd -> Db.PBD.get_currently_attached ~__context ~self:pbd) pbds in
    let plugged_hosts = List.setify (List.map (fun pbd -> Db.PBD.get_host ~__context ~self:pbd) plugged_pbds) in
    let all_hosts = Db.Host.get_all ~__context in
    let enabled_hosts = List.filter (fun host -> Db.Host.get_enabled ~__context ~self:host) all_hosts in
    if not(subset enabled_hosts plugged_hosts) then begin
      warn "SR %s not shared properly: Not all enabled hosts have a currently_attached PBD" (Ref.string_of self);
      false
    end else true
  end

let get_pif_underneath_vlan ~__context vlan_pif_ref =
  let vlan_rec = Db.PIF.get_VLAN_master_of ~__context ~self:vlan_pif_ref in
  Db.VLAN.get_tagged_PIF ~__context ~self:vlan_rec

(* Only returns true if the network is shared properly: all (enabled) hosts in the pool must have a PIF on
 * the network, and none of these PIFs may be bond slaves. This ensures that a VM with a VIF on this
 * network can run on (and be migrated to) any (enabled) host in the pool. *)
let is_network_properly_shared ~__context ~self =
	let pifs = Db.Network.get_PIFs ~__context ~self in
	let non_slave_pifs = List.filter (fun pif ->
		not (Db.is_valid_ref __context (Db.PIF.get_bond_slave_of ~__context ~self:pif))) pifs in
	let hosts_with_pif = List.setify (List.map (fun pif -> Db.PIF.get_host ~__context ~self:pif) non_slave_pifs) in
	let all_hosts = Db.Host.get_all ~__context in
	let enabled_hosts = List.filter (fun host -> Db.Host.get_enabled ~__context ~self:host) all_hosts in
	let properly_shared = subset enabled_hosts hosts_with_pif in
	if not properly_shared then
		warn "Network %s not shared properly: Not all hosts have PIFs" (Ref.string_of self);
	properly_shared

let vm_assert_agile ~__context ~self =
  (* All referenced VDIs should be in shared SRs *)
  List.iter (fun vbd ->
	       if not(Db.VBD.get_empty ~__context ~self:vbd) then begin
		 let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
		 let sr = Db.VDI.get_SR ~__context ~self:vdi in
		 if not(is_sr_properly_shared ~__context ~self:sr)
		 then raise (Api_errors.Server_error(Api_errors.ha_constraint_violation_sr_not_shared, [ Ref.string_of sr ]))
	       end)
    (Db.VM.get_VBDs ~__context ~self);
  (* All referenced VIFs should be on shared networks *)
  List.iter (fun vif ->
	       let network = Db.VIF.get_network ~__context ~self:vif in
	       if not(is_network_properly_shared ~__context ~self:network)
	       then raise (Api_errors.Server_error(Api_errors.ha_constraint_violation_network_not_shared, [ Ref.string_of network ])))
    (Db.VM.get_VIFs ~__context ~self)

(* Select an item from a list with a probability proportional to the items weight / total weight of all items *)
let weighted_random_choice weighted_items (* list of (item, integer) weight *) =
  let total_weight, acc' = List.fold_left (fun (total, acc) (x, weight) -> (total + weight), (x, total + weight) :: acc) (0, []) weighted_items in
  let cumulative = List.rev acc' in

  let w = Random.int total_weight in (* w \in [0, total_weight-1] *)
  let a, b = List.partition (fun (_, cumulative_weight) -> cumulative_weight <= w) cumulative in
  fst (List.hd b)

let memusage () =
	let memtotal, memfree, swaptotal, swapfree, buffers, cached =
		ref None, ref None, ref None, ref None, ref None, ref None in
	let find_field key s v =
		if String.startswith key s then
			let vs = List.hd (List.filter ((<>) "") (List.tl (String.split ' ' s))) in
			v := Some (float_of_string vs) in
	try
		Unixext.file_lines_iter
			(fun s ->
				 find_field "MemTotal" s memtotal;
				 find_field "MemFree" s memfree;
				 find_field "SwapTotal" s swaptotal;
				 find_field "SwapFree" s swapfree;
				 find_field "Buffers" s buffers;
				 find_field "Cached" s cached)
			"/proc/meminfo";
		match !memtotal, !memfree, !swaptotal, !swapfree, !buffers, !cached with
		| Some mt, Some mf, Some st, Some sf, Some bu, Some ca ->
			  let su = if st = 0. then 0. else (st -. sf) /. st in
			  (mt -. mf -. (bu +. ca) *. (1. -. su)) /. mt
		| _ -> raise Exit
	with _ -> - 1.

let local_storage_exists () =
  (try ignore(Unix.stat (Xapi_globs.xapi_blob_location)); true
    with _ -> false)

let touch_file fname =
  try
    if fname <> "" then
      match Unixext.spawnvp "touch" [| "touch"; fname |] with
	Unix.WEXITED 0 -> ()
      | _ -> warn "Unable to touch ready file '%s': touch exited abnormally" fname
  with
  | e -> (warn "Unable to touch ready file '%s': %s" fname (Printexc.to_string e))

let vm_to_string __context vm =
	let str = Ref.string_of vm in

	if not (Db.is_valid_ref __context vm)
	then raise (Api_errors.Server_error(Api_errors.invalid_value ,[str]));
	let t = Context.database_of __context in
	let module DB = (val (Db_cache.get t) : Db_interface.DB_ACCESS) in
	let fields = fst (DB.read_record t Db_names.vm str) in
	let sexpr = SExpr.Node (List.map (fun (key,value) -> SExpr.Node [SExpr.String key; SExpr.String value]) fields) in
	SExpr.string_of sexpr

let vm_string_to_assoc vm_string =

	let assoc_of_node = function
		| SExpr.Node [SExpr.String s; SExpr.String t] -> (s,t)
		| _ -> raise (Api_errors.Server_error(Api_errors.invalid_value ,["Invalid vm_string"])) in

	match SExpr_TS.of_string vm_string with
	| SExpr.Node l -> List.map assoc_of_node l
	| _ -> raise (Api_errors.Server_error(Api_errors.invalid_value ,["Invalid vm_string"]))

let i_am_srmaster ~__context ~sr =
  (* Assuming there is a plugged in PBD on this host then
     we are an 'srmaster' IFF: we are a pool master and this is a shared SR
                               OR this is a non-shared SR *)
  let shared = Db.SR.get_shared ~__context ~self:sr in
  (Pool_role.is_master () && shared) || not(shared)

(* Copy the snapshot metadata from [src_record] to the VM whose reference is [dst_ref]. *)
(* If a lookup table is provided, then the field 'snapshot_of' is translated using this *)
(* lookup table. *)
let copy_snapshot_metadata rpc session_id ?lookup_table ~src_record ~dst_ref =
	let f = match lookup_table with
		| None   -> (fun x -> x)
		| Some t -> (fun x -> t x)
	in
	Client.Client.VM.update_snapshot_metadata ~rpc ~session_id ~vm:dst_ref
		~snapshot_of:(f src_record.API.vM_snapshot_of)
		~snapshot_time:src_record.API.vM_snapshot_time
		~transportable_snapshot_id:src_record.API.vM_transportable_snapshot_id

let update_vswitch_controller ~__context ~host =
	try call_api_functions ~__context (fun rpc session_id ->
		let result = Client.Client.Host.call_plugin ~rpc ~session_id ~host ~plugin:"openvswitch-cfg-update" ~fn:"update" ~args:[] in
		debug "openvswitch-cfg-update(on %s): %s"
			(Db.Host.get_name_label ~__context ~self:host)
			result)
	with e ->
		debug "Got '%s' while trying to update the vswitch configuration on host %s"
			(Printexc.to_string e)
			(Db.Host.get_name_label ~__context ~self:host)

let assert_vswitch_controller_not_active ~__context =
	let pool = get_pool ~__context in
	let controller = Db.Pool.get_vswitch_controller ~__context ~self:pool in
	let net_type = Netdev.network.Netdev.kind in
	if (controller <> "") && (net_type = Netdev.Vswitch) then
		raise (Api_errors.Server_error (Api_errors.operation_not_allowed, ["A vswitch controller is active"]))

(* Useful for making readable(ish) logs: *)
let short_string_of_ref x =
  let x' = Ref.string_of x in
  String.sub x' (String.length "OpaqueRef:") 8

let force_loopback_vbd ~__context =
	(* Workaround assumption in SMRT: if a global flag is set, force use
	   of loopback VBDs. *)
	let pool = get_pool ~__context in
	let other_config = Db.Pool.get_other_config ~__context ~self:pool in
	List.mem_assoc "force_loopback_vbd" other_config
