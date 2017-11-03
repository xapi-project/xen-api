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

open Stdext
open Xstringext
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

open Network

module D=Debug.Make(struct let name="helpers" end)
open D

module StringSet = Set.Make(String)

let log_exn_continue msg f x = try f x with e -> debug "Ignoring exception: %s while %s" (ExnHelper.string_of_exn e) msg

(** Construct a descriptive network name (used as name_label) for a give network interface. *)
let choose_network_name_for_pif device =
  Printf.sprintf "Pool-wide network associated with %s" device

(** Once the server functor has been instantiated, set this reference to the appropriate
    "fake_rpc" (loopback non-HTTP) rpc function. This is used by the CLI, which passes in
    the HTTP request headers it has already received together with its active file descriptor. *)
let rpc_fun : (Http.Request.t -> Unix.file_descr -> Rpc.call -> Rpc.response) option ref = ref None

let get_rpc () =
  match !rpc_fun with
    None -> failwith "No rpc set!"
  | Some f -> f

(* !! FIXME - trap proper MISSINGREFERENCE exception when this has been defined *)
(* !! FIXME(2) - this code could be shared with the CLI? *)
let checknull f =
  try f() with
    _ -> "<not in database>"

let get_pool ~__context = List.hd (Db.Pool.get_all ~__context)

let get_master ~__context =
  Db.Pool.get_master ~__context ~self:(get_pool ~__context)

let get_management_iface_is_connected ~__context =
  let dbg = Context.string_of_task __context in
  Net.Bridge.get_physical_interfaces dbg ~name:(Xapi_inventory.lookup Xapi_inventory._management_interface)
  |> List.exists (fun name -> Net.Interface.is_connected dbg ~name)

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

(* Determine the gateway and DNS PIFs:
 * If one of the PIFs with IP has other_config:defaultroute=true, then
 * pick this one as gateway PIF. If there are multiple, pick a random one of these.
 * If there are none, then pick the management interface. If there is no management
 * interface, pick a random PIF.
 * Similarly for the DNS PIF, but with other_config:peerdns. *)
let determine_gateway_and_dns_ifs ~__context ?(management_interface : API.ref_PIF option) () =
  let localhost = get_localhost ~__context in
  let ip_pifs = Db.PIF.get_records_where ~__context
      ~expr:(And (Eq (Field "host", Literal (Ref.string_of localhost)),
                  Not (Eq (Field "ip_configuration_mode", Literal "None")))) in
  if ip_pifs = [] then
    None, None
  else
    let gateway_pif, gateway_rc =
      let oc = List.filter (fun (_, r) ->
          List.mem_assoc "defaultroute" r.API.pIF_other_config &&
          List.assoc "defaultroute" r.API.pIF_other_config = "true"
        ) ip_pifs in
      match oc with
      | (ref, rc) :: tl ->
        if tl <> [] then
          warn "multiple PIFs with other_config:defaultroute=true - choosing %s" rc.API.pIF_device;
        (ref, rc)
      | [] ->
        match management_interface with
        | Some pif -> let rc = Db.PIF.get_record ~__context ~self:pif in (pif, rc)
        | None ->
          let mgmt = List.filter (fun (_, r) -> r.API.pIF_management) ip_pifs in
          match mgmt with
          | (ref, rc) :: _ -> (ref, rc)
          | [] ->
            let (ref, rc) = List.hd ip_pifs in
            warn "no gateway PIF found - choosing %s" rc.API.pIF_device;
            (ref, rc)
    in
    let dns_pif, dns_rc =
      let oc = List.filter (fun (_, r) ->
          List.mem_assoc "peerdns" r.API.pIF_other_config &&
          List.assoc "peerdns" r.API.pIF_other_config = "true"
        ) ip_pifs in
      match oc with
      | (ref, rc) :: tl ->
        if tl <> [] then
          warn "multiple PIFs with other_config:peerdns=true - choosing %s" rc.API.pIF_device;
        (ref, rc)
      | [] ->
        match management_interface with
        | Some pif -> let pif_rc = Db.PIF.get_record ~__context ~self:pif in (pif, pif_rc)
        | None ->
          let mgmt = List.filter (fun (_, r) -> r.API.pIF_management) ip_pifs in
          match mgmt with
          | (ref, rc) :: _ -> (ref, rc)
          | [] ->
            let (ref, rc) = List.hd ip_pifs in
            warn "no DNS PIF found - choosing %s" rc.API.pIF_device;
            (ref, rc)
    in
    let gateway_bridge = Db.Network.get_bridge ~__context ~self:gateway_rc.API.pIF_network in
    let dns_bridge = Db.Network.get_bridge ~__context ~self:dns_rc.API.pIF_network in
    Some (gateway_pif, gateway_bridge), Some (dns_pif, dns_bridge)

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

let update_getty () =
  (* Running update-issue service on best effort basis *)
  try
    ignore (Forkhelpers.execute_command_get_output !Xapi_globs.update_issue_script []);
    ignore (Forkhelpers.execute_command_get_output !Xapi_globs.kill_process_script ["-q"; "-HUP"; "mingetty"; "agetty"])
  with e ->
    debug "update_getty at %s caught exception: %s"
      __LOC__ (Printexc.to_string e)

let set_gateway ~__context ~pif ~bridge =
  let dbg = Context.string_of_task __context in
  try
    match Net.Interface.get_ipv4_gateway dbg bridge with
    | Some addr -> Db.PIF.set_gateway ~__context ~self:pif ~value:(Unix.string_of_inet_addr addr)
    | None -> ()
  with _ ->
    warn "Unable to get the gateway of PIF %s (%s)" (Ref.string_of pif) bridge

let set_DNS ~__context ~pif ~bridge =
  let dbg = Context.string_of_task __context in
  try
    match Net.Interface.get_dns dbg bridge with
    | (nameservers, _) when nameservers != [] ->
      let dns = String.concat "," (List.map (Unix.string_of_inet_addr) nameservers) in
      Db.PIF.set_DNS ~__context ~self:pif ~value:dns;
    | _ -> ()
  with _ ->
    warn "Unable to get the dns of PIF %s (%s)" (Ref.string_of pif) bridge

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
  let gateway_if, dns_if = determine_gateway_and_dns_ifs ~__context () in
  Opt.iter (fun (pif, bridge) -> set_gateway ~__context ~pif ~bridge) gateway_if;
  Opt.iter (fun (pif, bridge) -> set_DNS ~__context ~pif ~bridge) dns_if;
  List.iter (fun self -> update_pif_address ~__context ~self) pifs

let make_rpc ~__context rpc : Rpc.response =
  let subtask_of = Ref.string_of (Context.get_task_id __context) in
  let open Xmlrpc_client in
  let http = xmlrpc ~subtask_of ~version:"1.1" "/" in
  let transport =
    if Pool_role.is_master ()
    then Unix(Xapi_globs.unix_domain_socket)
    else SSL(SSL.make ~use_stunnel_cache:true (), Pool_role.get_master_address(), !Xapi_globs.https_port) in
  XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:"xapi" ~transport ~http rpc

(* This one uses rpc-light *)
let make_remote_rpc remote_address xml =
  let open Xmlrpc_client in
  let transport = SSL(SSL.make (), remote_address, !Xapi_globs.https_port) in
  let http = xmlrpc ~version:"1.0" "/" in
  XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:"remote_xapi" ~transport ~http xml

(* Helper type for an object which may or may not be in the local database. *)
type 'a api_object =
  | LocalObject of 'a Ref.t
  | RemoteObject of ((Rpc.call -> Rpc.response) * API.ref_session * ('a Ref.t))

(** Log into pool master using the client code, call a function
    passing it the rpc function and session id, logout when finished. *)
let call_api_functions_internal ~__context f =
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
       then
         try Client.Client.Session.logout rpc session_id
         with e ->
           debug "Helpers.call_api_functions failed to logout: %s (ignoring)" (Printexc.to_string e))


(* Note: `test_fn` here is a mechanism for providing an alternative to an API
   call _only for unit testing_ - if !test_mode above is true this function will
   call the test code instead of `f`, as API calls currently do not work in the
   unit test context. This is ONLY intended as a LAST RESORT mechanism if
   there's no other way of achieving this effect. We should move the code in
   the direction where this sort of thing is unnecessary. *)
let test_mode = ref false
let call_api_functions ~__context ?test_fn f =
  if not !test_mode
  then call_api_functions_internal ~__context f
  else begin
    match test_fn with
    | Some fn -> fn ()
    | None -> failwith "Test mode API function unset"
  end

let call_emergency_mode_functions hostname f =
  let open Xmlrpc_client in
  let transport = SSL(SSL.make (), hostname, !Xapi_globs.https_port) in
  let http = xmlrpc ~version:"1.0" "/" in
  let rpc = XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:"xapi" ~transport ~http in
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

let is_domain_zero ~__context vm_ref =
  let host_ref = Db.VM.get_resident_on ~__context ~self:vm_ref in
  (Db.VM.get_is_control_domain ~__context ~self:vm_ref)
  && (Db.is_valid_ref __context host_ref)
  && (Db.Host.get_control_domain ~__context ~self:host_ref = vm_ref)

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
           if not (is_domain_zero ~__context vm) then begin
             error "VM uuid %s is not domain zero but the uuid is in my inventory file" uuid;
             raise (No_domain_zero uuid);
           end;
           domain_zero_ref_cache := Some vm;
           vm
         with _ ->
           error "Failed to find domain zero (uuid = %s)" uuid;
           raise (No_domain_zero uuid)
    )

let update_domain_zero_name ~__context host hostname =
  let stem = "Control domain on host: " in
  let full_name = stem ^ hostname in
  let dom0 = get_domain_zero ~__context in
  (* Double check host *)
  let dom0_host = Db.VM.get_resident_on ~__context ~self:dom0 in
  if dom0_host <> host
  then
    error "Unexpectedly incorrect dom0 record in update_domain_zero_name"
  else begin
    let current_name = Db.VM.get_name_label ~__context ~self:dom0 in
    let is_default =
      try
        String.sub current_name 0 (String.length stem) = stem
      with _ -> false
    in
    if is_default && current_name <> full_name then
      Db.VM.set_name_label ~__context ~self:dom0 ~value:full_name
  end

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
    let pool = get_pool ~__context in
    List.mem_assoc Xapi_globs.rolling_upgrade_in_progress (Db.Pool.get_other_config ~__context ~self:pool)
  with _ ->
    false

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
    actually booted that way; you must check the VM_metrics to be sure *)
let will_boot_hvm_from_record (x: API.vM_t) = x.API.vM_HVM_boot_policy <> ""

let will_boot_hvm ~__context ~self = Db.VM.get_HVM_boot_policy ~__context ~self <> ""

let has_booted_hvm ~__context ~self =
  Db.VM_metrics.get_hvm ~__context ~self:(Db.VM.get_metrics ~__context ~self)

let is_hvm ~__context ~self =
  match Db.VM.get_power_state ~__context ~self with
  | `Paused | `Running | `Suspended -> has_booted_hvm ~__context ~self
  | `Halted | _ -> will_boot_hvm ~__context ~self

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
  let open Xenctrl in
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

let get_main_ip_address ~__context =
  try Pool_role.get_master_address () with _ -> "127.0.0.1"

let is_pool_master ~__context ~host =
  let host_id = Db.Host.get_uuid ~__context ~self:host in
  let master = get_master ~__context in
  let master_id = Db.Host.get_uuid ~__context ~self:master in
  host_id = master_id

(* Host version compare helpers *)
let compare_int_lists : int list -> int list -> int =
  fun a b ->
    let first_non_zero is = List.fold_left (fun a b -> if (a<>0) then a else b) 0 is in
    first_non_zero (List.map2 compare a b)

let group_by f list =
  let evaluated_list = List.map (fun x -> (x, f x)) list in
  let snd_equality (_, x) (_, y) = x = y in
  let snd_compare (_, x) (_, y) = compare x y in
  let sorted = List.sort snd_compare evaluated_list in
  let rec take_while p ac = function
    | [] -> (ac, [])
    | x :: xs ->
      if (p x) then take_while p (x :: ac) xs
      else (ac, x :: xs)
  in
  let rec group ac = function
    | [] -> ac
    | x :: xs ->
      let peers, rest = take_while (snd_equality x) [] (x :: xs) in
      group (peers :: ac) rest
  in
  group [] sorted

(** Groups list elements by equality of result of function application sorted
 *  in order of that result *)
let group_by ~ordering f list =
  match ordering with
  | `descending -> group_by f list
  | `ascending -> List.rev (group_by f list)

(** Schwarzian transform sort *)
let sort_by_schwarzian ?(descending=false) f list =
  let comp x y = if descending then compare y x else compare x y in
  let (|>) a f = f a in
  List.map (fun x -> (x, f x)) list |>
  List.sort (fun (_, x') (_, y') -> comp x' y') |>
  List.map (fun (x, _) -> x)

let version_string_of : __context:Context.t -> [`host] api_object -> string =
  fun ~__context host ->
    try
      let software_version = match host with
        | LocalObject host_ref -> (Db.Host.get_software_version ~__context ~self:host_ref)
        | RemoteObject (rpc, session_id, host_ref) ->
          Client.Client.Host.get_software_version ~rpc ~session_id ~self:host_ref
      in
      List.assoc Xapi_globs._platform_version software_version
    with Not_found ->
      Xapi_globs.default_platform_version

let version_of : __context:Context.t -> [`host] api_object -> int list =
  fun ~__context host ->
    let vs = version_string_of ~__context host
    in List.map int_of_string (String.split '.' vs)

(* Compares host versions, analogous to Pervasives.compare. *)
let compare_host_platform_versions : __context:Context.t -> [`host] api_object -> [`host] api_object -> int =
  fun ~__context host_a host_b ->
    let version_of = version_of ~__context in
    compare_int_lists (version_of host_a) (version_of host_b)

let max_version_in_pool : __context:Context.t -> int list =
  fun ~__context ->
    let max_version a b = if a = [] then b else if (compare_int_lists a b) > 0 then a else b
    and versions = List.map (fun host_ref -> version_of ~__context (LocalObject host_ref)) (Db.Host.get_all ~__context) in
    List.fold_left max_version [] versions

let rec string_of_int_list : int list -> string = function
    []    -> ""
  | (x::xs) ->
    if xs == []
    then string_of_int x
    else string_of_int x ^ "." ^ string_of_int_list xs

let host_has_highest_version_in_pool : __context:Context.t -> host:[`host] api_object -> bool =
  fun ~__context ~host ->
    let host_version = version_of ~__context host
    and max_version = max_version_in_pool ~__context in
    (compare_int_lists host_version max_version) >= 0

let host_versions_not_decreasing ~__context ~host_from ~host_to =
  compare_host_platform_versions ~__context host_from host_to <= 0

let is_platform_version_same_on_master ~__context ~host =
  if is_pool_master ~__context ~host then true else
    let master = get_master ~__context in
    compare_host_platform_versions ~__context (LocalObject master) (LocalObject host) = 0

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
    if not (host_has_highest_version_in_pool ~__context ~host:(LocalObject host)) then
      raise (Api_errors.Server_error (Api_errors.not_supported_during_upgrade, []))

let pool_has_different_host_platform_versions ~__context =
  let all_hosts = Db.Host.get_all ~__context in
  let platform_versions = List.map (fun host -> version_string_of ~__context (LocalObject host)) all_hosts in
  let is_different_to_me platform_version = platform_version <> Xapi_version.platform_version () in
  List.fold_left (||) false (List.map is_different_to_me platform_versions)

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

(* Read pool secret if it exists; otherwise, create a new one. *)
let get_pool_secret () =
  try
    Unix.access !Xapi_globs.pool_secret_path [Unix.F_OK];
    pool_secret := Unixext.string_of_file !Xapi_globs.pool_secret_path;
    Db_globs.pool_secret := !pool_secret;
  with _ -> (* No pool secret exists. *)
    let mk_rand_string () = Uuid.to_string (Uuid.make_uuid()) in
    pool_secret := (mk_rand_string()) ^ "/" ^ (mk_rand_string()) ^ "/" ^ (mk_rand_string());
    Db_globs.pool_secret := !pool_secret;
    Unixext.write_string_to_file !Xapi_globs.pool_secret_path !pool_secret

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

(* Port checks. *)
let is_valid_tcp_udp_port ~port =
  port >= 1 && port <= 65535

let assert_is_valid_tcp_udp_port ~port ~name =
  if is_valid_tcp_udp_port ~port then ()
  else raise Api_errors.(Server_error (value_not_supported, [
      name; string_of_int port; "Port out of range";
    ]))

let assert_is_valid_tcp_udp_port_range
    ~first_port ~first_name
    ~last_port ~last_name =
  assert_is_valid_tcp_udp_port ~port:first_port ~name:first_name;
  assert_is_valid_tcp_udp_port ~port:last_port ~name:last_name;
  if last_port < first_port
  then raise Api_errors.(Server_error (value_not_supported ,[
      last_name; string_of_int last_port;
      Printf.sprintf "%s smaller than %s" last_name first_name;
    ]))

(* IP address and CIDR checks *)

let is_valid_ip kind address =
  match Unixext.domain_of_addr address, kind with
  | Some x, `ipv4 when x = Unix.PF_INET -> true
  | Some x, `ipv6 when x = Unix.PF_INET6 -> true
  | _ -> false

let assert_is_valid_ip kind field address =
  if not (is_valid_ip kind address) then
    raise Api_errors.(Server_error (invalid_ip_address_specified, [field]))

let parse_cidr kind cidr =
  try
    let address, prefixlen = Scanf.sscanf cidr "%s@/%d" (fun a p -> a, p) in
    if not (is_valid_ip kind address) then
      (error "Invalid address in CIDR (%s)" address; None)
    else if prefixlen < 0 || (kind = `ipv4 && prefixlen > 32) || (kind = `ipv6 && prefixlen > 128) then
      (error "Invalid prefix length in CIDR (%d)" prefixlen; None)
    else
      Some (address, prefixlen)
  with _ ->
    (error "Invalid CIDR format (%s)" cidr; None)

let assert_is_valid_cidr kind field cidr =
  if parse_cidr kind cidr = None then
    raise Api_errors.(Server_error (invalid_cidr_address_specified, [field]))

(** Return true if the MAC is in the right format XX:XX:XX:XX:XX:XX *)
let is_valid_MAC mac =
  let l = String.split ':' mac in
  let validchar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') in
  List.length l = 6 && (List.fold_left (fun acc s -> acc && String.length s = 2 && validchar s.[0] && validchar s.[1]) true l)

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

(** Indicate whether run-script should be allowed on VM plugin guest-agent-operation *)
let guest_agent_run_script_enabled ~__context =
  try
    let pool = get_pool ~__context in
    let other_config = Db.Pool.get_other_config ~__context ~self:pool in
    List.mem_assoc Xapi_globs.pool_allow_guest_agent_run_script other_config
    && (List.assoc Xapi_globs.pool_allow_guest_agent_run_script other_config = "true")
  with _ -> false

(* OEM Related helper functions *)
let is_oem ~__context ~host =
  let software_version = Db.Host.get_software_version ~__context ~self:host in
  List.mem_assoc "oem_build_number" software_version

let on_oem ~__context =
  let this_host = !Xapi_globs.localhost_ref in
  is_oem ~__context ~host:this_host

exception File_doesnt_exist of string

let call_script ?(log_successful_output=true) ?env script args =
  try
    Unix.access script [ Unix.X_OK ];
    (* Use the same $PATH as xapi *)
    let env = match env with
      | None -> [| "PATH=" ^ (Sys.getenv "PATH") |]
      | Some env -> env
    in
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

let get_srmaster ~__context ~sr =
  let shared = Db.SR.get_shared ~__context ~self:sr in
  let pbds = Db.SR.get_PBDs ~__context ~self:sr in
  if shared
  then get_master ~__context
  else begin
    match List.length pbds with
    | 0 ->
      raise (Api_errors.Server_error
               (Api_errors.sr_no_pbds, []))
    | 1 ->
      Db.PBD.get_host ~__context ~self:(List.hd pbds)
    | n ->
      raise (Api_errors.Server_error
               (Api_errors.sr_has_multiple_pbds,
                List.map (fun pbd -> Db.PBD.get_uuid ~__context ~self:pbd) pbds))
  end

let i_am_srmaster ~__context ~sr =
  get_srmaster ~__context ~sr = get_localhost ~__context

let get_all_plugged_srs ~__context =
  let pbds_plugged_in = Db.PBD.get_refs_where ~__context ~expr:(
    Eq (Field "currently_attached", Literal "true")) in
  List.setify (List.map (fun self -> Db.PBD.get_SR ~__context ~self) pbds_plugged_in)

let get_all_plugged_srs_local ~__context =
  let localhost = get_localhost __context in
  let localhost = Ref.string_of localhost in
  let my_pbds_plugged_in = Db.PBD.get_refs_where  ~__context ~expr:(And (
    Eq (Field "host", Literal localhost),
    Eq (Field "currently_attached", Literal "true")
  ))
  in
  List.setify (List.map (fun self -> Db.PBD.get_SR ~__context ~self) my_pbds_plugged_in)

let find_health_check_task ~__context ~sr =
  Db.Task.get_refs_where ~__context ~expr:(And (
      Eq (Field "name__label", Literal Xapi_globs.sr_health_check_task_label),
      Eq (Field "name__description", Literal (Ref.string_of sr))
    ))

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
      let result = Client.Client.Host.call_plugin ~rpc ~session_id ~host ~plugin:"openvswitch-config-update" ~fn:"update" ~args:[] in
      debug "openvswitch-config-update(on %s): %s"
        (Db.Host.get_name_label ~__context ~self:host)
        result)
  with e ->
    debug "Got '%s' while trying to update the vswitch configuration on host %s"
      (Printexc.to_string e)
      (Db.Host.get_name_label ~__context ~self:host)

let assert_vswitch_controller_not_active ~__context =
  let sdn_controllers = Db.SDN_controller.get_all ~__context in
  let dbg = Context.string_of_task __context in
  let backend = Net.Bridge.get_kind dbg () in
  if (sdn_controllers <> []) && (backend = Network_interface.Openvswitch) then
    raise (Api_errors.Server_error (Api_errors.operation_not_allowed, ["A vswitch controller is active"]))

(* use the database rather than networkd so we can unit test the PVS functions that use this *)
let assert_using_vswitch ~__context =
  let host = get_localhost ~__context in
  let sw = Db.Host.get_software_version ~__context ~self:host in
  let using_vswitch =
    try
      List.assoc "network_backend" sw = Network_interface.(string_of_kind Openvswitch)
    with Not_found -> false
  in
  if not using_vswitch then raise Api_errors.(Server_error (openvswitch_not_active, []))

exception No_pvs_server_available

let assert_pvs_servers_available ~__context ~pvs_site =
  let pvs_servers = Db.PVS_site.get_servers ~__context ~self:pvs_site in
  if pvs_servers = [] then raise No_pvs_server_available

let assert_is_valid_ref ~__context ~name ~ref =
  if not (Db.is_valid_ref __context ref)
  then raise Api_errors.(Server_error (invalid_value, [
      name; Ref.string_of ref;
    ]))

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

(* We no longer care about the hash, but it's part of the API and we
   can't get rid of it. Put this here so clients don't need to know
   about this. *)
let compute_hash () = ""

(* [timebox ~timeout:x ~otherwise:ow f] will timebox the execution of [f ()].
   If the execution finishes within [x] seconds, we'll return the result from
   [f ()]; if not, we'll call [otherwise ()] to give the final result. Note
   that [otherwise] is a functional parameter, so there are plenty of things
   the caller can do with it, e.g

   - simply return a backup value in case of timeout
   - raise an exception of your choice
   - do some cleanup routines when timed out
   - collaboratively stop/cancel the execution of [f ()] --- Note that there is
   no universal method to cancel a function execution process which has already
   started. So when [f ()] times out, its underlying execution will be still
   ongoing, just the result will be discarded. However, it's possible to
   implement some collaboration mechanism, so that [f ()] would stop itself
   after receiving some message from [ow ()] if that is called.

   ... or a combination of the above. *)
let timebox ~timeout ~otherwise f =
  let fd_in, fd_out = Unix.pipe () in
  let result = ref otherwise in
  let _ =  Thread.create
      (fun () ->
         (try
            let r = f () in
            result := fun () -> r
          with e ->
            result := fun () -> raise e);
         Unix.close fd_out) () in
  let _ = Thread.wait_timed_read fd_in timeout in
  Unix.close fd_in;
  !result ()

(**************************************************************************************)
(* The master uses a global mutex to mark database records before forwarding messages *)

(** All must fear the global mutex *)
let __internal_mutex = Mutex.create ()
let __number_of_queueing_threads = ref 0
let max_number_of_queueing_threads = 100

let with_global_lock x = Mutex.execute __internal_mutex x

(** Call the function f having incremented the number of queueing threads counter.
    If we exceed a built-in threshold, throw TOO_MANY_PENDING_TASKS *)
let queue_thread f =
  with_global_lock
    (fun () ->
       if !__number_of_queueing_threads > max_number_of_queueing_threads
       then raise (Api_errors.Server_error(Api_errors.too_many_pending_tasks, []))
       else incr __number_of_queueing_threads);
  finally f (fun () -> with_global_lock (fun () -> decr __number_of_queueing_threads))

module type POLICY = sig
  type t
  val standard : t
  (** Used by operations like VM.start which want to paper over transient glitches but want to fail
      		    quickly if the objects are persistently locked (eg by a VDI.clone) *)
  val fail_quickly : t
  val fail_immediately: t
  val wait : __context:Context.t -> t -> exn -> t
end

(* Mechanism for early wakeup of blocked threads. When a thread goes to sleep having got an
   'other_operation_in_progress' exception, we use the interruptible sleep 'Delay.*' rather than
   'Thread.delay' and provide a mechanism for the other of the conflicting task to wake us up
   on the way out. *)
module Early_wakeup = struct
  let table : ((string*string), Delay.t) Hashtbl.t = Hashtbl.create 10
  let table_m = Mutex.create ()

  let wait ((a, b) as key) time =
    (* debug "Early_wakeup wait key = (%s, %s) time = %.2f" a b time; *)
    let d = Delay.make () in
    Mutex.execute table_m (fun () -> Hashtbl.add table key d);
    finally
      (fun () ->
         let (_: bool) = Delay.wait d time in
         ()
      )(fun () -> Mutex.execute table_m (fun () -> Hashtbl.remove table key))

  let broadcast (a, b) =
    (*debug "Early_wakeup broadcast key = (%s, %s)" a b;*)
    Mutex.execute table_m
      (fun () ->
         Hashtbl.iter (fun (a, b) d -> (*debug "Signalling thread blocked on (%s, %s)" a b;*) Delay.signal d) table
      )

  let signal ((a, b) as key) =
    (*debug "Early_wakeup signal key = (%s, %s)" a b;*)
    Mutex.execute table_m
      (fun () ->
         if Hashtbl.mem table key then ((*debug "Signalling thread blocked on (%s,%s)" a b;*) Delay.signal (Hashtbl.find table key))
      )
end

module Repeat_with_uniform_backoff : POLICY = struct
  type t = {
    minimum_delay: float;    (* seconds *)
    maximum_delay: float;    (* maximum backoff time *)
    max_total_wait: float;   (* max time to wait before failing *)
    wait_so_far: float;      (* time waited so far *)
  }
  let standard = {
    minimum_delay = 1.0;
    maximum_delay = 20.0;
    max_total_wait = 3600.0 *. 2.0; (* 2 hours *)
    wait_so_far = 0.0;
  }
  let fail_quickly = {
    minimum_delay = 2.;
    maximum_delay = 2.;
    max_total_wait = 120.;
    wait_so_far = 0.
  }
  let fail_immediately = {
    minimum_delay = 0.;
    maximum_delay = 3.;
    max_total_wait = min_float;
    wait_so_far = 0.;
  }
  let wait ~__context (state: t) (e: exn) =
    if state.wait_so_far >= state.max_total_wait then raise e;
    let this_timeout = state.minimum_delay +. (state.maximum_delay -. state.minimum_delay) *. (Random.float 1.0) in

    debug "Waiting for up to %f seconds before retrying..." this_timeout;
    let start = Unix.gettimeofday () in
    begin
      match e with
      | Api_errors.Server_error(code, [ cls; objref ]) when code = Api_errors.other_operation_in_progress ->
        Early_wakeup.wait (cls, objref) this_timeout;
      | _ ->
        Thread.delay this_timeout;
    end;
    { state with wait_so_far = state.wait_so_far +. (Unix.gettimeofday () -. start) }
end

(** Could replace this with something fancier which waits for objects to change at the
    database level *)
module Policy = Repeat_with_uniform_backoff

(** Attempts to retry a lock-acquiring function multiple times. If it catches another operation
    in progress error then it blocks before retrying. *)
let retry ~__context ~doc ?(policy = Policy.standard) f =
  (* This is a cancellable operation, so mark the allowed operations on the task *)
  TaskHelper.set_cancellable ~__context;

  let rec loop state =
    let result = ref None in
    let state = ref state in
    while !result = None do
      try
        if TaskHelper.is_cancelling ~__context then begin
          error "%s locking failed: task has been cancelled" doc;
          TaskHelper.cancel ~__context;
          raise (Api_errors.Server_error(Api_errors.task_cancelled, [ Ref.string_of (Context.get_task_id __context) ]))
        end;
        result := Some (f ())
      with
      | Api_errors.Server_error(code, objref :: _ ) as e when code = Api_errors.other_operation_in_progress ->
        debug "%s locking failed: caught transient failure %s" doc (ExnHelper.string_of_exn e);
        state := queue_thread (fun () -> Policy.wait ~__context !state e)
    done;
    match !result with
    | Some x -> x
    | None -> failwith "this should never happen" in
  loop policy

let retry_with_global_lock ~__context ~doc ?policy f =
  retry ~__context ~doc ?policy (fun () -> with_global_lock f)

let get_first_pusb ~__context usb_group =
  try
    List.hd (Db.USB_group.get_PUSBs ~__context ~self:usb_group)
  with _ ->
    raise Api_errors.(Server_error(internal_error,
      [Printf.sprintf "there is no PUSB associated with the USB_group: %s"
      (Ref.string_of usb_group)]))

let get_first_vusb ~__context usb_group =
  try
    List.hd (Db.USB_group.get_VUSBs ~__context ~self:usb_group)
  with _ ->
    raise Api_errors.(Server_error(internal_error,
      [Printf.sprintf "there is no VUSB associated with the USB_group: %s"
      (Ref.string_of usb_group)]))
