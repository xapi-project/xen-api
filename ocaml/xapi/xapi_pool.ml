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

open Stdext
open Client
open Db_filter_types
open Pervasiveext
open Threadext
open Xstringext
open Listext
open Network

module L = Debug.Make(struct let name="license" end)
module D=Debug.Make(struct let name="xapi" end)
open D
open Workload_balancing

(* Surpress exceptions *)
let no_exn f x =
  try ignore (f x)
  with exn ->
    debug "Ignoring exception: %s" (ExnHelper.string_of_exn exn)

let rpc host_address xml =
  try
    Helpers.make_remote_rpc host_address xml
  with Xmlrpc_client.Connection_reset ->
    raise (Api_errors.Server_error(Api_errors.pool_joining_host_connection_failed, []))

let get_master ~rpc ~session_id =
  let pool = List.hd (Client.Pool.get_all rpc session_id) in
  Client.Pool.get_master rpc session_id pool

(* Pre-join asserts *)
let pre_join_checks ~__context ~rpc ~session_id ~force =
  (* I cannot join a Pool unless my management interface exists in the db, otherwise
     	   Pool.eject will fail to rewrite network interface files. *)
  let assert_management_interface_exists () =
    try
      let (_: API.ref_PIF) = Xapi_host.get_management_interface ~__context ~host:(Helpers.get_localhost ~__context) in
      ()
    with _ ->
      error "Pool.join/Pool.eject requires a properly configured management interface. Wait for xapi/firstboot initialisation to complete and then retry.";
      raise (Api_errors.Server_error(Api_errors.host_still_booting, [])) in

  (* I cannot join a Pool if I have HA already enabled on me *)
  let ha_is_not_enable_on_me () =
    let pool = Helpers.get_pool ~__context in
    if Db.Pool.get_ha_enabled ~__context ~self:pool then begin
      error "Cannot join pool as HA is enabled";
      raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []))
    end in

  (* I Cannot join a Pool if it has HA enabled on it *)
  let ha_is_not_enable_on_the_distant_pool () =
    let pool = List.hd (Client.Pool.get_all rpc session_id) in
    if Client.Pool.get_ha_enabled rpc session_id pool then begin
      error "Cannot join pool which already has HA enabled";
      raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));
    end in

  (* CA-26975: Pool edition MUST match *)
  let assert_restrictions_match () =
    let my_edition = Db.Host.get_edition ~__context ~self:(Helpers.get_localhost ~__context) in
    let host_records = List.map snd (Client.Host.get_all_records ~rpc ~session_id) in
    let pool_editions = List.map (fun host_r -> host_r.API.host_edition) host_records in
    (* If all hosts have the same edition string, we need do no more. *)
    if List.fold_left (fun b edn -> edn = my_edition && b) true pool_editions then ()
    else try
        (* We have different edition strings so must consult v6d for their significance.
           			 * This will fail with v6d_failure if v6d is not running. *)
        let editions = V6_client.get_editions "assert_restrictions_match" in
        let edition_to_int e =
          try
            V6_interface.(match List.find (fun ed -> ed.title = e) editions with ed -> ed.order)
          with Not_found ->
            (* Happens if pool has edition "free/libre" (no v6d) *)
            error "Pool.join failed: pool has a host with edition unknown to v6d: %s" e;
            raise (Api_errors.Server_error(Api_errors.license_host_pool_mismatch,
                                           ["Edition \"" ^ e ^ "\" from pool is not known to v6d."]))
        in
        let min_edition l =
          List.fold_left (fun m e -> if edition_to_int e < edition_to_int m then e else m) (List.hd l) l
        in
        (* get pool edition: the "minimum" edition among all hosts *)
        let pool_edition = min_edition pool_editions in
        (* compare my edition to pool edition *)
        if (edition_to_int pool_edition) <> (edition_to_int my_edition) then begin
          error "Pool.join failed due to edition mismatch";
          error "Remote has %s" pool_edition;
          error "Local has  %s" my_edition;
          raise (Api_errors.Server_error(Api_errors.license_host_pool_mismatch,
                                         ["host edition = \""^my_edition^"\""; "pool edition = \""^pool_edition^"\""]))
        end
      with Api_errors.Server_error (code, []) when code = Api_errors.v6d_failure ->
        error "Pool.join failed because edition strings differ and local has no license daemon running.";
        let pool_edn_list_str = "[" ^ (String.concat "; " pool_editions) ^ "]" in
        error "Remote editions: %s" pool_edn_list_str;
        error "Local edition: %s" my_edition;
        raise (Api_errors.Server_error (code, ["The pool uses v6d. Pool edition list = " ^ pool_edn_list_str]))
  in

  let assert_api_version_matches () =
    let master = get_master rpc session_id in
    let candidate_slave = Helpers.get_localhost ~__context in
    let master_major = Client.Host.get_API_version_major ~rpc ~session_id ~self:master in
    let master_minor = Client.Host.get_API_version_minor ~rpc ~session_id ~self:master in
    let slave_major = Db.Host.get_API_version_major ~__context ~self:candidate_slave in
    let slave_minor = Db.Host.get_API_version_minor ~__context ~self:candidate_slave in
    if master_major <> slave_major || master_minor <> slave_minor then
      begin
        error "The joining host's API version is %Ld.%Ld while the master's is %Ld.%Ld"
          slave_major slave_minor master_major master_minor;
        raise (Api_errors.Server_error(Api_errors.pool_joining_host_must_have_same_api_version,
                                       [Printf.sprintf "%Ld.%Ld" slave_major slave_minor; Printf.sprintf "%Ld.%Ld" master_major master_minor;]))
      end
  in

  let assert_db_schema_matches () =
    let master = get_master rpc session_id in
    let candidate_slave = Helpers.get_localhost ~__context in
    let master_sw_version = Client.Host.get_software_version ~rpc ~session_id ~self:master in
    let slave_sw_version = Db.Host.get_software_version ~__context ~self:candidate_slave in
    let master_db_schema = try List.assoc Xapi_globs._db_schema master_sw_version with _ -> "" in
    let slave_db_schema = try List.assoc Xapi_globs._db_schema slave_sw_version with _ -> "" in
    if master_db_schema = "" || slave_db_schema = "" ||  master_db_schema <> slave_db_schema then
      begin
        error "The joining host's database schema is %s; the master's is %s"
          slave_db_schema master_db_schema;
        raise (Api_errors.Server_error(Api_errors.pool_joining_host_must_have_same_db_schema,
                                       [slave_db_schema; master_db_schema]))
      end
  in

  let assert_homogeneous_updates () =
    let module S      = Helpers.StringSet in
    let local_host    = Helpers.get_localhost ~__context in
    let local_uuid    = Db.Host.get_uuid ~__context ~self:local_host in
    let updates_on ~rpc ~session_id host =
      Client.Host.get_updates ~rpc ~session_id ~self:host
      |> List.map (fun self -> Client.Pool_update.get_record ~rpc ~session_id ~self)
      |> List.filter (fun upd -> upd.API.pool_update_enforce_homogeneity = true)
      |> List.map (fun upd -> upd.API.pool_update_uuid)
      |> S.of_list in
    let local_updates =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          updates_on ~rpc ~session_id local_host) in
    (* compare updates on host and pool master *)
    Client.Pool.get_all rpc session_id |> List.iter (fun pool ->
        let pool_host = Client.Pool.get_master rpc session_id pool in
        let remote_updates = updates_on rpc session_id pool_host in
        if not (S.equal local_updates remote_updates) then begin
          let remote_uuid  = Client.Host.get_uuid rpc session_id pool_host in
          let diff xs ys   = S.diff xs ys |> S.elements |> String.concat "," in
          let reason       =
            Printf.sprintf "Updates on local host %s and pool host %s differ"
              (Db.Host.get_name_label ~__context ~self:local_host)
              (Client.Host.get_name_label rpc session_id pool_host) in
          error
            "Pool join: Updates differ. Only on pool host %s: {%s} -- only on local host %s: {%s}"
            remote_uuid
            (diff remote_updates local_updates)
            local_uuid
            (diff local_updates remote_updates);
          raise Api_errors.(Server_error(pool_hosts_not_homogeneous,[reason]))
        end)
  in

  (* CP-700: Restrict pool.join if AD configuration of slave-to-be does not match *)
  (* that of master of pool-to-join *)
  let assert_external_auth_matches () =
    let master = get_master rpc session_id in
    let slavetobe = Helpers.get_localhost ~__context in
    let slavetobe_auth_type = Db.Host.get_external_auth_type ~__context ~self:slavetobe in
    let slavetobe_auth_service_name = Db.Host.get_external_auth_service_name ~__context ~self:slavetobe in
    let master_auth_type = Client.Host.get_external_auth_type ~rpc ~session_id ~self:master in
    let master_auth_service_name = Client.Host.get_external_auth_service_name ~rpc ~session_id ~self:master in
    debug "Verifying if external auth configuration of master %s (auth_type=%s service_name=%s) matches that of slave-to-be %s (auth-type=%s service_name=%s)"
      (Client.Host.get_name_label ~rpc ~session_id ~self:master) master_auth_type master_auth_service_name
      (Db.Host.get_name_label ~__context ~self:slavetobe) slavetobe_auth_type slavetobe_auth_service_name;
    if (slavetobe_auth_type <> master_auth_type)
    || (slavetobe_auth_service_name <> master_auth_service_name) then begin
      error "Cannot join pool whose external authentication configuration is different";
      raise (Api_errors.Server_error(Api_errors.pool_joining_external_auth_mismatch, []))
    end in

  let assert_i_know_of_no_other_hosts () =
    let hosts = Db.Host.get_all ~__context in
    if List.length hosts > 1 then begin
      error "The current host is already the master of other hosts: it cannot join a new pool";
      raise (Api_errors.Server_error(Api_errors.pool_joining_host_cannot_be_master_of_other_hosts, []))
    end in

  let assert_no_running_vms_on_me () =
    let my_vms = Db.VM.get_all_records ~__context in
    let my_running_vms =
      List.filter
        (fun (_,vmrec) -> (not (Helpers.is_domain_zero ~__context (Db.VM.get_by_uuid ~__context ~uuid:vmrec.API.vM_uuid)))
                          && vmrec.API.vM_power_state = `Running) my_vms in
    if List.length my_running_vms > 0 then begin
      error "The current host has running or suspended VMs: it cannot join a new pool";
      raise (Api_errors.Server_error(Api_errors.pool_joining_host_cannot_have_running_VMs, []))
    end in

  let assert_no_vms_with_current_ops () =
    let my_vms = Db.VM.get_all_records ~__context in
    let vms_with_current_ops =
      List.filter (fun (_,vmr) -> (List.length vmr.API.vM_current_operations)>0 ) my_vms in
    if List.length vms_with_current_ops > 0 then begin
      error "The current host has VMs with current operations: it cannot join a new pool";
      raise (Api_errors.Server_error(Api_errors.pool_joining_host_cannot_have_vms_with_current_operations, []))
    end in

  let assert_no_shared_srs_on_me () =
    let my_srs = Db.SR.get_all_records ~__context in
    let my_shared_srs = List.filter (fun (sr,srec)-> srec.API.sR_shared && not srec.API.sR_is_tools_sr) my_srs in
    if not (my_shared_srs = []) then begin
      error "The current host has shared SRs: it cannot join a new pool";
      raise (Api_errors.Server_error(Api_errors.pool_joining_host_cannot_contain_shared_SRs, []))
    end in

  (* Allow pool-join if host does not have any bonds *)
  let assert_no_bonds_on_me () =
    if Db.Bond.get_all ~__context <> [] then begin
      error "The current host has network bonds: it cannot join a new pool";
      raise (Api_errors.Server_error(Api_errors.pool_joining_host_has_bonds, []))
    end in

  (* Allow pool-join if host does not have any tunnels *)
  let assert_no_tunnels_on_me () =
    if Db.Tunnel.get_all ~__context <> [] then begin
      error "The current host has tunnels: it cannot join a new pool";
      raise (Api_errors.Server_error(Api_errors.pool_joining_host_has_tunnels, []))
    end in

  (* Allow pool-join if host does not have any non-management VLANs *)
  let assert_no_non_management_vlans_on_me () =
    List.iter (fun self ->
        let pif = Db.VLAN.get_untagged_PIF ~__context ~self in
        if Db.PIF.get_management ~__context ~self:pif <> true then begin
          error "The current host has non-management vlans: it cannot join a new pool";
          raise (Api_errors.Server_error(Api_errors.pool_joining_host_has_non_management_vlans, []))
        end
      ) (Db.VLAN.get_all ~__context)
  in

  (* Allow pool-join if the host and the pool are on the same management vlan *)
  let assert_management_vlan_are_same () =
    let management_pif = Xapi_host.get_management_interface ~__context ~host:(Helpers.get_localhost ~__context) in
    let vlan_tag = Db.PIF.get_VLAN ~__context ~self:management_pif in
    let remote_management_pif = Client.Host.get_management_interface rpc session_id (get_master ~rpc ~session_id) in
    let remote_vlan_tag = Client.PIF.get_VLAN rpc session_id remote_management_pif in
    if vlan_tag <> remote_vlan_tag then begin
      error "The current host and the pool management vlan does not match: it cannot join a new pool";
      raise (Api_errors.Server_error(Api_errors.pool_joining_host_management_vlan_does_not_match,
                                     [Int64.to_string vlan_tag; Int64.to_string remote_vlan_tag]))
    end in

  (* Used to tell XCP and XenServer apart - use PRODUCT_BRAND if present, else use PLATFORM_NAME. *)
  let get_compatibility_name software_version =
    if List.mem_assoc Xapi_globs._product_brand software_version then
      Some (List.assoc Xapi_globs._product_brand software_version)
    else if List.mem_assoc Xapi_globs._platform_name software_version then
      Some (List.assoc Xapi_globs._platform_name software_version)
    else
      None
  in

  let assert_hosts_compatible () =
    let me = Db.Host.get_record ~__context ~self:(Helpers.get_localhost ~__context) in
    let master_ref = get_master rpc session_id in
    let master = Client.Host.get_record ~rpc ~session_id ~self:master_ref in
    let my_software_version = me.API.host_software_version in
    let master_software_version = master.API.host_software_version in
    let compatibility_info x =
      let open Xapi_globs in
      let platform_version = if List.mem_assoc _platform_version x
        then Some (List.assoc _platform_version x)
        else None in
      let compatibility_name = get_compatibility_name x in
      (platform_version, compatibility_name)
    in
    let master_compatibility_info = compatibility_info master_software_version in
    let my_compatibility_info = compatibility_info my_software_version in
    if master_compatibility_info <> my_compatibility_info then begin
      debug "master PLATFORM_VERSION = %s, master compatibility name = %s; my PLATFORM_VERSION = %s, my compatibility name = %s; "
        (Opt.default "Unknown" (fst master_compatibility_info))
        (Opt.default "Unknown" (snd master_compatibility_info))
        (Opt.default "Unknown" (fst my_compatibility_info))
        (Opt.default "Unknown" (snd my_compatibility_info));
      raise (Api_errors.Server_error(Api_errors.pool_hosts_not_compatible, []))
    end in

  let assert_hosts_homogeneous () =
    let me = Helpers.get_localhost ~__context in
    let master_ref = get_master rpc session_id in
    let master = Client.Host.get_record ~rpc ~session_id ~self:master_ref in

    (* Check software version, but as of CA-249786 don't check the build number*)

    let get_software_version_fields fields =
      let open Xapi_globs in
      begin try List.assoc _platform_version fields with _ -> "" end,
      begin match get_compatibility_name fields with Some x -> x | None -> "" end,
      begin try List.assoc _git_id fields with _ -> "" end,
      begin try
          if List.mem_assoc linux_pack_vsn_key fields then "installed"
          else "not present"
        with _ -> "not present" end
    in

    let print_software_version (version,name,id,linux_pack) =
      debug "version:%s, name:%s, id:%s, linux_pack:%s" version name id linux_pack in

    let master_software_version = master.API.host_software_version in
    let my_software_version = Db.Host.get_software_version ~__context ~self:me in

    let my_software_compare = get_software_version_fields my_software_version in
    let master_software_compare = get_software_version_fields master_software_version in

    debug "Pool pre-join Software homogeneity check:";
    debug "Slave software:";
    print_software_version my_software_compare;
    debug "Master software:";
    print_software_version master_software_compare;

    if my_software_compare <> master_software_compare then
      raise (Api_errors.Server_error(Api_errors.pool_hosts_not_homogeneous,["Software version differs"]));

    (* Check CPUs *)

    let my_cpu_vendor = Db.Host.get_cpu_info ~__context ~self:me |> List.assoc "vendor" in
    let pool_cpu_vendor =
      let pool = List.hd (Client.Pool.get_all rpc session_id) in
      Client.Pool.get_cpu_info rpc session_id pool |> List.assoc "vendor"
    in
    debug "Pool pre-join CPU homogeneity check:";
    debug "Slave CPUs: %s" my_cpu_vendor;
    debug "Pool CPUs: %s" pool_cpu_vendor;

    if my_cpu_vendor <> pool_cpu_vendor then
      raise (Api_errors.Server_error(Api_errors.pool_hosts_not_homogeneous,["CPUs differ"])) in

  let assert_not_joining_myself () =
    let master = get_master rpc session_id in
    let master_uuid = Client.Host.get_uuid rpc session_id master in
    let my_uuid = Db.Host.get_uuid ~__context ~self:(Helpers.get_localhost ~__context) in
    if master_uuid = my_uuid then
      let error_msg =
        if 1 < List.length (Db.Host.get_all ~__context)
        then "Host is already part of a pool"
        else "Host cannot become slave of itself" in
      raise (Api_errors.Server_error(Api_errors.operation_not_allowed, [error_msg])) in

  let assert_homogeneous_vswitch_configuration () =
    (* The network backend must be the same as the remote master's backend *)
    let dbg = Context.string_of_task __context in
    let my_backend' = Net.Bridge.get_kind dbg () in
    let my_backend = Network_interface.string_of_kind my_backend' in
    let pool = List.hd (Client.Pool.get_all rpc session_id) in
    let remote_master = Client.Pool.get_master ~rpc ~session_id ~self:pool in
    let remote_masters_backend =
      let v = Client.Host.get_software_version ~rpc ~session_id ~self:remote_master in
      if not (List.mem_assoc "network_backend" v) then
        Network_interface.string_of_kind Network_interface.Bridge
      else
        List.assoc "network_backend" v
    in
    if my_backend <> remote_masters_backend then
      raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["Network backends differ"]));

    match my_backend' with
    | Network_interface.Openvswitch ->
      begin
        let remote_sdn_controllers = Client.SDN_controller.get_all ~rpc ~session_id in
        let my_sdn_controllers = Db.SDN_controller.get_all ~__context in
        (* We assume that each pool has _at most_ one SDN controller *)
        match remote_sdn_controllers, my_sdn_controllers with
        | _, [] -> ()
        | remote_sdn_controller :: _, my_sdn_controller :: _ ->
          (* check that protocol/address/port are identical *)
          let my_sdn_protocol = Db.SDN_controller.get_protocol ~__context ~self:my_sdn_controller in
          let my_sdn_address = Db.SDN_controller.get_address ~__context ~self:my_sdn_controller in
          let my_sdn_port = Db.SDN_controller.get_port ~__context ~self:my_sdn_controller in
          let remote_sdn_protocol = Client.SDN_controller.get_protocol ~rpc ~session_id ~self:remote_sdn_controller in
          let remote_sdn_address = Client.SDN_controller.get_address ~rpc ~session_id ~self:remote_sdn_controller in
          let remote_sdn_port = Client.SDN_controller.get_port ~rpc ~session_id ~self:remote_sdn_controller in
          if my_sdn_protocol <> remote_sdn_protocol
          || my_sdn_address <> remote_sdn_address
          || my_sdn_port <> remote_sdn_port then
            raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["SDN controller differs"]))
        | _ ->
          raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["SDN controller differs"]))
      end
    | _ -> ()
  in

  let assert_homogeneous_primary_address_type () =
    let mgmt_iface = Xapi_host.get_management_interface ~__context ~host:(Helpers.get_localhost ~__context) in
    let mgmt_addr_type = Db.PIF.get_primary_address_type ~__context ~self:mgmt_iface in
    let master = get_master rpc session_id in
    let master_mgmt_iface = Client.Host.get_management_interface rpc session_id master in
    let master_addr_type = Client.PIF.get_primary_address_type rpc session_id master_mgmt_iface in
    if (mgmt_addr_type <> master_addr_type) then
      raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["Primary address type differs"]));
  in

  let assert_compatible_network_purpose () = try (
    let my_nbdish =
      Db.Network.get_all ~__context |>
      List.map (fun nwk -> Db.Network.get_purpose ~__context ~self:nwk) |>
      List.flatten |>
      List.find (function `nbd | `insecure_nbd -> true | _ -> false) in
    let remote_nbdish =
      Client.Network.get_all rpc session_id |>
      List.map (fun nwk -> Client.Network.get_purpose ~rpc ~session_id ~self:nwk) |>
      List.flatten |>
      List.find (function `nbd | `insecure_nbd -> true | _ -> false) in
    if remote_nbdish <> my_nbdish then
      raise Api_errors.(Server_error(operation_not_allowed, ["Incompatible network purposes: nbd and insecure_nbd"]))
  ) with Not_found -> () (* If either side has no network with nbd-related purpose, then no problem. *)
  in

  let assert_pool_size_unrestricted () =
    let is_restricted = Xapi_host.pool_size_is_restricted ~__context in
    if is_restricted && (List.length (Client.Host.get_all ~rpc ~session_id) >= Xapi_globs.restricted_pool_size) then
      raise (Api_errors.Server_error(Api_errors.license_restriction, [Features.name_of_feature Features.Pool_size]))
  in

  (* call pre-join asserts *)
  assert_pool_size_unrestricted ();
  assert_management_interface_exists ();
  ha_is_not_enable_on_me ();
  ha_is_not_enable_on_the_distant_pool ();
  assert_not_joining_myself();
  assert_i_know_of_no_other_hosts();
  assert_no_running_vms_on_me ();
  assert_no_vms_with_current_ops ();
  assert_hosts_compatible ();
  if (not force) then assert_hosts_homogeneous ();
  assert_no_shared_srs_on_me ();
  assert_no_bonds_on_me ();
  assert_no_tunnels_on_me ();
  assert_no_non_management_vlans_on_me ();
  assert_management_vlan_are_same ();
  assert_external_auth_matches ();
  assert_restrictions_match ();
  assert_homogeneous_vswitch_configuration ();
  (* CA-247399: check first the API version and then the database schema *)
  assert_api_version_matches ();
  assert_db_schema_matches ();
  assert_homogeneous_updates ();
  assert_homogeneous_primary_address_type ();
  assert_compatible_network_purpose ()

let rec create_or_get_host_on_master __context rpc session_id (host_ref, host) : API.ref_host =
  let my_uuid = host.API.host_uuid in

  let new_host_ref =
    try Client.Host.get_by_uuid rpc session_id my_uuid
    with _ ->
      debug "Found no host with uuid = '%s' on the master, so creating one." my_uuid;

      (* CA-51925: Copy the local cache SR *)
      let my_local_cache_sr = Db.Host.get_local_cache_sr ~__context ~self:host_ref in
      let local_cache_sr = if my_local_cache_sr = Ref.null then Ref.null else
          begin
            let my_local_cache_sr_rec = Db.SR.get_record ~__context ~self:my_local_cache_sr in
            debug "Copying the local cache SR (uuid=%s)" my_local_cache_sr_rec.API.sR_uuid;
            create_or_get_sr_on_master __context rpc session_id (my_local_cache_sr, my_local_cache_sr_rec)
          end in

      (* Look up the value on the master of the pool we are about to join *)
      let master_ssl = Client.Host.get_ssl_legacy ~rpc ~session_id ~self:(get_master rpc session_id) in
      (* Set value in inventory (to control initial behaviour on next xapi start)
         			 * but not in the database of the current pool (the one we're about to leave) *)
      Xapi_inventory.update Xapi_inventory._stunnel_legacy (string_of_bool master_ssl);

      debug "Creating host object on master";
      let ref = Client.Host.create ~rpc ~session_id
          ~uuid:my_uuid
          ~name_label:host.API.host_name_label
          ~name_description:host.API.host_name_description
          ~hostname:host.API.host_hostname
          ~address:host.API.host_address
          ~external_auth_type:host.API.host_external_auth_type
          ~external_auth_service_name:host.API.host_external_auth_service_name
          ~external_auth_configuration:host.API.host_external_auth_configuration
          ~license_params:host.API.host_license_params
          ~edition:host.API.host_edition
          ~license_server:host.API.host_license_server
          (* CA-51925: local_cache_sr can only be written by Host.enable_local_caching_sr but this API
             				 * call is forwarded to the host in question. Since, during a pool-join, the host is offline,
             				 * we need an alternative way of preserving the value of the local_cache_sr field, so it's
             				 * been added to the constructor. *)
          ~local_cache_sr
          ~chipset_info:host.API.host_chipset_info
          ~ssl_legacy:master_ssl
      in

      (* Copy other-config into newly created host record: *)
      no_exn (fun () -> Client.Host.set_other_config ~rpc ~session_id ~self:ref ~value:host.API.host_other_config) ();

      (* Copy the crashdump SR *)
      let my_crashdump_sr = Db.Host.get_crash_dump_sr ~__context ~self:host_ref in
      if my_crashdump_sr <> Ref.null then begin
        let my_crashdump_sr_rec = Db.SR.get_record ~__context ~self:my_crashdump_sr in
        debug "Copying the crashdump SR (uuid=%s)" my_crashdump_sr_rec.API.sR_uuid;
        let crashdump_sr = create_or_get_sr_on_master __context rpc session_id (my_crashdump_sr, my_crashdump_sr_rec) in
        no_exn (fun () -> Client.Host.set_crash_dump_sr ~rpc ~session_id ~self:ref ~value:crashdump_sr) ()
      end;

      (* Copy the suspend image SR *)
      let my_suspend_image_sr = Db.Host.get_crash_dump_sr ~__context ~self:host_ref in
      if my_suspend_image_sr <> Ref.null then begin
        let my_suspend_image_sr_rec = Db.SR.get_record ~__context ~self:my_suspend_image_sr in
        debug "Copying the suspend-image SR (uuid=%s)" my_suspend_image_sr_rec.API.sR_uuid;
        let suspend_image_sr = create_or_get_sr_on_master __context rpc session_id (my_suspend_image_sr, my_suspend_image_sr_rec) in
        no_exn (fun () -> Client.Host.set_suspend_image_sr ~rpc ~session_id ~self:ref ~value:suspend_image_sr) ()
      end;

      ref in

  new_host_ref

and create_or_get_sr_on_master __context rpc session_id (sr_ref, sr) : API.ref_SR =
  let my_uuid = sr.API.sR_uuid in

  let new_sr_ref =
    try Client.SR.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      let my_pbd_ref = List.hd (Db.SR.get_PBDs ~__context ~self:sr_ref) in
      let my_pbd = Db.PBD.get_record ~__context ~self:my_pbd_ref in
      let pbds_on_master = Client.PBD.get_all_records ~rpc ~session_id in

      (* The only possible shared SRs are ISO, as other SRs cannot be shared properly accross pools. *)
      (* In this case, if we find a SR with a PBD having the same device_config field, we pick this SR instead of building a new one *)
      let iso_already_exists_on_master () = List.exists (fun (_,x) -> Listext.List.set_equiv x.API.pBD_device_config my_pbd.API.pBD_device_config) pbds_on_master in
      if sr.API.sR_shared && sr.API.sR_content_type = "iso" && iso_already_exists_on_master () then begin
        let similar_pbd_ref, similar_pbd = List.find (fun (_,x) -> Listext.List.set_equiv x.API.pBD_device_config my_pbd.API.pBD_device_config) pbds_on_master in
        similar_pbd.API.pBD_SR

      end else begin
        debug "Found no SR with uuid = '%s' on the master, so creating one." my_uuid;
        let ref = Client.SR.introduce ~rpc ~session_id
            ~uuid:my_uuid
            ~name_label:sr.API.sR_name_label
            ~name_description:sr.API.sR_name_description
            ~_type:sr.API.sR_type
            ~content_type:sr.API.sR_content_type
            ~shared:false
            ~sm_config:sr.API.sR_sm_config in
        (* copy other-config into newly created sr record: *)
        no_exn (fun () -> Client.SR.set_other_config ~rpc ~session_id ~self:ref ~value:sr.API.sR_other_config) ();
        ref
      end in

  new_sr_ref

let create_or_get_pbd_on_master __context rpc session_id (pbd_ref, pbd) : API.ref_PBD =
  let my_uuid = pbd.API.pBD_uuid in

  let new_pbd_ref =
    try Client.PBD.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      let my_host_ref = pbd.API.pBD_host in
      let my_host = Db.Host.get_record ~__context ~self:my_host_ref in
      let new_host_ref = create_or_get_host_on_master __context rpc session_id (my_host_ref, my_host) in

      let my_sr_ref = pbd.API.pBD_SR in
      let my_sr = Db.SR.get_record ~__context ~self:my_sr_ref in
      let new_sr_ref = create_or_get_sr_on_master __context rpc session_id (my_sr_ref, my_sr) in

      debug "Found no PBD with uuid = '%s' on the master, so creating one." my_uuid;
      Client.PBD.create ~rpc ~session_id
        ~host:new_host_ref
        ~sR:new_sr_ref
        ~other_config:pbd.API.pBD_other_config
        ~device_config:pbd.API.pBD_device_config in

  new_pbd_ref

let create_or_get_vdi_on_master __context rpc session_id (vdi_ref, vdi) : API.ref_VDI =
  let my_uuid = vdi.API.vDI_uuid in
  let my_sr_ref = vdi.API.vDI_SR in
  let my_sr = Db.SR.get_record ~__context ~self:my_sr_ref in

  let new_sr_ref = create_or_get_sr_on_master __context rpc session_id (my_sr_ref, my_sr) in

  let new_vdi_ref =
    try Client.VDI.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      debug "Found no VDI with uuid = '%s' on the master, so creating one." my_uuid;
      Client.VDI.pool_introduce ~rpc ~session_id
        ~uuid:my_uuid
        ~name_label:vdi.API.vDI_name_label
        ~name_description:vdi.API.vDI_name_description
        ~sR:new_sr_ref
        ~_type:vdi.API.vDI_type
        ~sharable:vdi.API.vDI_sharable
        ~read_only:vdi.API.vDI_read_only
        ~other_config:vdi.API.vDI_other_config
        ~location:(Db.VDI.get_location ~__context ~self:vdi_ref)
        ~xenstore_data:vdi.API.vDI_xenstore_data
        ~sm_config:vdi.API.vDI_sm_config
        ~managed:vdi.API.vDI_managed
        ~virtual_size:vdi.API.vDI_virtual_size
        ~physical_utilisation:vdi.API.vDI_physical_utilisation
        ~metadata_of_pool:vdi.API.vDI_metadata_of_pool
        ~is_a_snapshot:vdi.API.vDI_is_a_snapshot
        ~snapshot_time:vdi.API.vDI_snapshot_time
        ~snapshot_of:vdi.API.vDI_snapshot_of
        ~cbt_enabled:vdi.API.vDI_cbt_enabled in
  new_vdi_ref

let create_or_get_network_on_master __context rpc session_id (network_ref, network) : API.ref_network =
  let my_bridge = network.API.network_bridge in
  let is_physical = match network.API.network_PIFs with
    | [] -> false
    | hd :: _ -> Db.PIF.get_physical ~__context ~self:hd
  in
  let is_himn =
    (List.mem_assoc Xapi_globs.is_host_internal_management_network network.API.network_other_config) &&
    (List.assoc Xapi_globs.is_host_internal_management_network network.API.network_other_config = "true")
  in

  let new_network_ref =
    if is_physical || is_himn then
      (* Physical network or Host Internal Management Network:
         			 * try to join an existing network with the same bridge name, or create one.
         			 * This relies on the convention that physical PIFs with the same device name need to be connected.
         			 * Furthermore, there should be only one Host Internal Management Network in a pool. *)
      try
        let pool_networks = Client.Network.get_all_records ~rpc ~session_id in
        let net_ref, _ = List.find (fun (_, net) -> net.API.network_bridge = my_bridge) pool_networks in
        net_ref
      with _ ->
        debug "Found no network with bridge = '%s' on the master, so creating one." my_bridge;
        Client.Network.pool_introduce ~rpc ~session_id
          ~name_label:network.API.network_name_label
          ~name_description:network.API.network_name_description
          ~mTU:network.API.network_MTU
          ~other_config:network.API.network_other_config
          ~bridge:network.API.network_bridge
          ~managed:network.API.network_managed
          ~purpose:network.API.network_purpose
    else begin
      debug "Recreating network '%s' as internal network." network.API.network_name_label;
      (* This call will generate a new 'xapi#' bridge name rather than keeping the
         			 * current, possibly colliding one. *)
      Client.Network.create ~rpc ~session_id
        ~name_label:network.API.network_name_label
        ~name_description:network.API.network_name_description
        ~mTU:network.API.network_MTU
        ~other_config:network.API.network_other_config
        ~bridge:network.API.network_bridge
        ~managed:network.API.network_managed
        ~tags:network.API.network_tags
    end
  in

  new_network_ref

let create_or_get_pif_on_master __context rpc session_id (pif_ref, pif) : API.ref_PIF =
  let my_uuid = pif.API.pIF_uuid in

  let my_host_ref = pif.API.pIF_host in
  let my_host = Db.Host.get_record ~__context ~self:my_host_ref in
  let new_host_ref = create_or_get_host_on_master __context rpc session_id (my_host_ref, my_host) in

  let new_network_ref =
    if pif.API.pIF_VLAN <> -1L then begin
      (* Get the remote management network for management VLAN PIF *)
      let remote_mgmt_pif = Client.Host.get_management_interface rpc session_id (get_master ~rpc ~session_id) in
      Client.PIF.get_network rpc session_id remote_mgmt_pif
    end
    else begin
      let my_network_ref = pif.API.pIF_network in
      let my_network = Db.Network.get_record ~__context ~self:my_network_ref in
      create_or_get_network_on_master __context rpc session_id (my_network_ref, my_network)
    end
  in

  let new_pif_ref =
    try Client.PIF.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      debug "Found no PIF with uuid = '%s' on the master, so creating one." my_uuid;
      Client.PIF.pool_introduce ~rpc ~session_id
        ~device:pif.API.pIF_device
        ~network:new_network_ref
        ~host:new_host_ref
        ~mAC:pif.API.pIF_MAC
        ~mTU:pif.API.pIF_MTU
        ~vLAN:pif.API.pIF_VLAN
        ~physical:pif.API.pIF_physical
        ~ip_configuration_mode:pif.API.pIF_ip_configuration_mode
        ~iP:pif.API.pIF_IP
        ~netmask:pif.API.pIF_netmask
        ~gateway:pif.API.pIF_gateway
        ~dNS:pif.API.pIF_DNS
        ~bond_slave_of:pif.API.pIF_bond_slave_of
        ~vLAN_master_of:pif.API.pIF_VLAN_master_of
        ~management:pif.API.pIF_management
        ~other_config:pif.API.pIF_other_config
        ~disallow_unplug:pif.API.pIF_disallow_unplug
        ~ipv6_configuration_mode:pif.API.pIF_ipv6_configuration_mode
        ~iPv6:pif.API.pIF_IPv6
        ~ipv6_gateway:pif.API.pIF_ipv6_gateway
        ~primary_address_type:pif.API.pIF_primary_address_type
        ~managed:pif.API.pIF_managed
        ~properties:pif.API.pIF_properties in

  new_pif_ref

let create_or_get_vlan_on_master __context rpc session_id (vlan_ref, vlan) : API.ref_VLAN =
  (* Create a VLAN PIF record only if it is a management PIF *)

  let my_host_ref = Db.PIF.get_host ~__context ~self:vlan.API.vLAN_tagged_PIF in
  let my_host = Db.Host.get_record ~__context ~self:my_host_ref in
  let new_host_ref = create_or_get_host_on_master __context rpc session_id (my_host_ref, my_host) in

  (* Create the untagged PIF record on Pool *)
  let untagged_pif_record = Db.PIF.get_record ~__context ~self:vlan.API.vLAN_untagged_PIF in
  let remote_untagged_pif = create_or_get_pif_on_master __context rpc session_id
      (vlan.API.vLAN_untagged_PIF, untagged_pif_record) in

  (* Get the remote tagged pif network *)
  let tagged_pif_network = Db.PIF.get_network ~__context ~self:vlan.API.vLAN_tagged_PIF in
  let tagged_pif_network_record = Db.Network.get_record ~__context ~self:tagged_pif_network in
  let remote_tagged_pif_network = create_or_get_network_on_master __context rpc session_id
      (tagged_pif_network, tagged_pif_network_record) in

  (* Get the new physical PIF ref on Pool for the joining Host *)
  let expr =
    Printf.sprintf "field \"network\"=\"%s\" and field \"host\"=\"%s\""
      (Ref.string_of remote_tagged_pif_network)
      (Ref.string_of new_host_ref)
  in
  let remote_physical_pif =
    match Client.PIF.get_all_records_where ~rpc ~session_id ~expr with
    | [] -> Ref.null
    | (pif, _) :: _ -> pif
  in

  let new_vlan_ref =
    try
      Client.VLAN.get_by_uuid ~rpc ~session_id ~uuid:vlan.API.vLAN_uuid
    with _ ->
      debug "Found no VLAN with uuid = '%s' on the master, so creating one." vlan.API.vLAN_uuid;
      Client.VLAN.pool_introduce ~rpc ~session_id
        ~tagged_PIF:remote_physical_pif
        ~untagged_PIF:remote_untagged_pif
        ~tag:vlan.API.vLAN_tag
        ~other_config:vlan.API.vLAN_other_config
  in
  new_vlan_ref

let create_or_get_pvs_site_on_master __context rpc session_id (pvs_site_ref, pvs_site) : API.ref_PVS_site =
  let my_pvs_uuid = pvs_site.API.pVS_site_PVS_uuid in
  let new_pvs_site_ref =
    let expr = "field \"PVS_uuid\"=\"" ^ my_pvs_uuid ^ "\"" in
    match Client.PVS_site.get_all_records_where ~rpc ~session_id ~expr with
    | [] ->
      debug "Found no PVS site with PVS_uuid = '%s' on the master, so creating one." my_pvs_uuid;
      let new_pvs_site = Client.PVS_site.introduce ~rpc ~session_id
          ~name_label:pvs_site.API.pVS_site_name_label
          ~name_description:pvs_site.API.pVS_site_name_description
          ~pVS_uuid:pvs_site.API.pVS_site_PVS_uuid in
      (* Update PVS servers *)
      let (_ : API.ref_PVS_server list) = List.map (fun pvs_server ->
          let pvs_record = Db.PVS_server.get_record ~__context ~self:pvs_server in
          Client.PVS_server.introduce ~rpc ~session_id
            ~addresses:pvs_record.API.pVS_server_addresses
            ~first_port:pvs_record.API.pVS_server_first_port
            ~last_port:pvs_record.API.pVS_server_last_port
            ~site:new_pvs_site
        ) (Db.PVS_site.get_servers ~__context ~self:pvs_site_ref) in
      new_pvs_site
    | (pvs_site, _) :: _ -> pvs_site
  in
  new_pvs_site_ref

let create_or_get_pvs_cache_storage_on_master __context rpc session_id (pcs_ref, pcs) : API.ref_PVS_cache_storage =
  let my_host_ref = pcs.API.pVS_cache_storage_host in
  let my_host = Db.Host.get_record ~__context ~self:my_host_ref in
  let new_host_ref = create_or_get_host_on_master __context rpc session_id (my_host_ref, my_host) in

  let my_sr_ref = pcs.API.pVS_cache_storage_SR in
  let my_sr = Db.SR.get_record ~__context ~self:my_sr_ref in
  let new_sr_ref = create_or_get_sr_on_master __context rpc session_id (my_sr_ref, my_sr) in

  let my_pvs_site_ref = pcs.API.pVS_cache_storage_site in
  let my_pvs_site = Db.PVS_site.get_record ~__context ~self:my_pvs_site_ref in
  let new_pvs_site_ref = create_or_get_pvs_site_on_master __context rpc session_id (my_pvs_site_ref, my_pvs_site) in

  let new_pvs_cache_storage_ref =
    let expr =
      Printf.sprintf "field \"site\"=\"%s\" and field \"host\"=\"%s\""
        (Ref.string_of new_pvs_site_ref)
        (Ref.string_of new_host_ref)
    in
    match Client.PVS_cache_storage.get_all_records_where ~rpc ~session_id ~expr with
    | [] ->
      Client.PVS_cache_storage.create ~rpc ~session_id
        ~host:new_host_ref
        ~sR:new_sr_ref
        ~site:new_pvs_site_ref
        ~size:pcs.API.pVS_cache_storage_size
    | (pvs_cache_storage, _) :: _ -> pvs_cache_storage
  in
  new_pvs_cache_storage_ref

let create_or_get_secret_on_master __context rpc session_id (secret_ref, secret) : API.ref_secret =
  let my_uuid = secret.API.secret_uuid in
  let my_value = secret.API.secret_value in
  let new_secret_ref =
    try Client.Secret.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      debug "Found no secret with uuid = '%s' on master, so creating one." my_uuid;
      Client.Secret.introduce ~rpc ~session_id ~uuid:my_uuid ~value:my_value ~other_config:[]
  in
  new_secret_ref

let protect_exn f x =
  try Some (f x)
  with e ->
    debug "Ignoring exception: %s" (Printexc.to_string e);
    Debug.log_backtrace e (Backtrace.get e);
    None

(* Remark: the order in which we create the object in the distant database is not very important, as we have *)
(* an unique way to identify each object and thus we know if we need to create them or if it is already done *)
let update_non_vm_metadata ~__context ~rpc ~session_id =

  (* Update hosts *)
  let my_hosts = Db.Host.get_all_records ~__context in
  let (_ : API.ref_host option list) =
    List.map (protect_exn (create_or_get_host_on_master __context rpc session_id)) my_hosts in

  (* Update SRs *)
  let my_srs = Db.SR.get_all_records ~__context in
  let (_ : API.ref_SR option list) =
    List.map (protect_exn (create_or_get_sr_on_master __context rpc session_id)) my_srs in

  (* Update PBDs *)
  let my_pbds = Db.PBD.get_all_records ~__context in
  let (_ : API.ref_PBD option list) =
    List.map (protect_exn (create_or_get_pbd_on_master __context rpc session_id)) my_pbds in

  (* Update VDIs *)
  let my_vdis = Db.VDI.get_all_records ~__context in
  let (_ : API.ref_VDI option list) =
    List.map (protect_exn (create_or_get_vdi_on_master __context rpc session_id)) my_vdis in

  (* Update networks *)
  let my_networks = Db.Network.get_all_records ~__context in
  let (_ : API.ref_network option list) =
    List.map (protect_exn (create_or_get_network_on_master __context rpc session_id)) my_networks in

  (* update PIFs *)
  let my_pifs = Db.PIF.get_records_where ~__context ~expr:(
      Eq (Field "physical", Literal "true")
    ) in
  let (_ : API.ref_PIF option list) =
    List.map (protect_exn (create_or_get_pif_on_master __context rpc session_id)) my_pifs in

  (* update Management VLAN *)
  let mgmt_pif = Xapi_host.get_management_interface ~__context ~host:(Helpers.get_localhost ~__context) in
  let my_vlan = Db.PIF.get_VLAN_master_of ~__context ~self:mgmt_pif in
  if my_vlan <> Ref.null then
    let my_vlan_record = Db.VLAN.get_record ~__context ~self:my_vlan in
    let (_ : API.ref_VLAN option ) = protect_exn (create_or_get_vlan_on_master __context rpc session_id) (my_vlan, my_vlan_record) in

    (* update PVS sites *)
    let my_pvs_sites = Db.PVS_site.get_all_records ~__context in
    let (_ : API.ref_PVS_site option list) =
      List.map (protect_exn (create_or_get_pvs_site_on_master __context rpc session_id)) my_pvs_sites in

    (* update PVS_cache_storage *)
    let my_pvs_cache_storages = Db.PVS_cache_storage.get_all_records ~__context in
    let (_ : API.ref_PVS_cache_storage option list) =
      List.map (protect_exn (create_or_get_pvs_cache_storage_on_master __context rpc session_id)) my_pvs_cache_storages in

    (* update Secrets *)
    let my_secrets = Db.Secret.get_all_records ~__context in
    let (_ : API.ref_secret option list) =
      List.map (protect_exn (create_or_get_secret_on_master __context rpc session_id)) my_secrets
    in

    ()

let assert_pooling_licensed ~__context =
  if (not (Pool_features.is_enabled ~__context Features.Pooling))
  then raise (Api_errors.Server_error(Api_errors.license_restriction, []))

let join_common ~__context ~master_address ~master_username ~master_password ~force =
  assert_pooling_licensed ~__context;
  (* get hold of cluster secret - this is critical; if this fails whole pool join fails *)
  (* Note: this is where the license restrictions are checked on the other side.. if we're trying to join
     	a host that does not support pooling then an error will be thrown at this stage *)
  let rpc = rpc master_address in
  let session_id =
    try Client.Session.login_with_password rpc master_username master_password Datamodel.api_version_string Xapi_globs.xapi_user_agent
    with Http_client.Http_request_rejected _ | Http_client.Http_error _ ->
      raise (Api_errors.Server_error(Api_errors.pool_joining_host_service_failed, [])) in

  let cluster_secret = ref "" in
  (* If management is on a VLAN, then get the Pool master
     management network bridge before we logout the session *)
  let pool_master_bridge, mgmt_pif =
    let my_pif = Xapi_host.get_management_interface ~__context ~host:(Helpers.get_localhost ~__context) in
    if (Db.PIF.get_VLAN_master_of ~__context ~self:my_pif) <> Ref.null then
      let pif = Client.Host.get_management_interface rpc session_id (get_master ~rpc ~session_id) in
      let network = Client.PIF.get_network rpc session_id pif in
      Some (Client.Network.get_bridge rpc session_id network), my_pif
    else
      None, my_pif
  in

  finally (fun () ->
      pre_join_checks ~__context ~rpc ~session_id ~force;
      cluster_secret := Client.Pool.initial_auth rpc session_id;

      (* get pool db from new master so I have a backup ready if we failover to me *)
      begin try
          Pool_db_backup.fetch_database_backup ~master_address ~pool_secret:!cluster_secret ~force:None
        with e ->
          error "Failed fetching a database backup from the master: %s" (ExnHelper.string_of_exn e)
      end;

      (* this is where we try and sync up as much state as we can
         		with the master. This is "best effort" rather than
         		critical; if we fail part way through this then we carry
         		on with the join *)
      try
        update_non_vm_metadata ~__context ~rpc ~session_id;
        ignore(Importexport.remote_metadata_export_import ~__context ~rpc ~session_id ~remote_address:master_address ~restore:true `All)
      with e ->
        debug "Error whilst importing db objects into master; aborted: %s" (Printexc.to_string e);
        warn "Error whilst importing db objects to master. The pool-join operation will continue, but some of the slave's VMs may not be available on the master.")
    (fun () ->
       Client.Session.logout rpc session_id);

  (* Attempt to unplug all our local storage. This is needed because
     	   when we restart as a slave, all the references will be wrong
     	   and these may have been cached by the storage layer. *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let me = Helpers.get_localhost ~__context in
      List.iter
        (fun self ->
           Helpers.log_exn_continue (Printf.sprintf "Unplugging PBD %s" (Ref.string_of self))
             (fun () ->
                Client.PBD.unplug rpc session_id self
             ) ()
        ) (Db.Host.get_PBDs ~__context ~self:me)
    );

  (* If management is on a VLAN, then we might need to create a vlan bridge with the same name as the Pool master is using *)
  begin match pool_master_bridge with
    | None -> ()
    | Some bridge ->
      let network = Db.PIF.get_network ~__context ~self:mgmt_pif in
      let mgmt_bridge = Db.Network.get_bridge ~__context ~self:network in
      if mgmt_bridge <> bridge then begin
        debug "Changing management vlan bridge from=%s to=%s" mgmt_bridge bridge;
        Db.Network.set_bridge ~__context ~self:network ~value:bridge;
        Nm.bring_pif_up ~__context ~management_interface:true mgmt_pif;
        Xapi_mgmt_iface.change bridge (Db.PIF.get_primary_address_type ~__context ~self:mgmt_pif)
      end
  end;

  (* Rewrite the pool secret on every host of the current pool, and restart all the agent as slave of the distant pool master. *)
  Helpers.call_api_functions ~__context (fun my_rpc my_session_id ->
      List.iter
        (fun (host, _) ->
           Client.Host.update_pool_secret my_rpc my_session_id host !cluster_secret;
           Client.Host.update_master my_rpc my_session_id host master_address)
        (Db.Host.get_all_records ~__context));
  Xapi_hooks.pool_join_hook ~__context

let join ~__context ~master_address ~master_username ~master_password  =
  join_common ~__context ~master_address ~master_username ~master_password ~force:false
let join_force ~__context ~master_address ~master_username ~master_password  =
  join_common ~__context ~master_address ~master_username ~master_password ~force:true

(* Assume that db backed up from master will be there and ready to go... *)
let emergency_transition_to_master ~__context =
  if Localdb.get Constants.ha_armed = "true"
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));
  Xapi_pool_transition.become_master ()

let emergency_reset_master ~__context ~master_address =
  if Localdb.get Constants.ha_armed = "true"
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));
  let master_address = Helpers.gethostbyname master_address in
  Xapi_pool_transition.become_another_masters_slave master_address

let recover_slaves ~__context =
  let hosts = Db.Host.get_all ~__context in
  let my_address = Db.Host.get_address ~__context ~self:!Xapi_globs.localhost_ref in
  let recovered_hosts = ref [] in
  let recover_slave hostref =
    if not (hostref = !Xapi_globs.localhost_ref) then
      begin
        try
          let local_fn = emergency_reset_master ~master_address:my_address in

          (* We have to use a new context here because the slave is currently doing a
             	     Task.get_name_label on real tasks, which will block on slaves that we're
             	     trying to recover. Get around this by creating a dummy task, for which
             	     the name-label bit is bypassed *)
          let newcontext = Context.make "emergency_reset_master" in
          Message_forwarding.do_op_on_localsession_nolivecheck ~local_fn ~__context:newcontext ~host:hostref
            (fun session_id rpc -> Client.Pool.emergency_reset_master rpc session_id my_address);
          recovered_hosts := hostref::!recovered_hosts
        with _ -> ()
      end in
  List.iter recover_slave hosts;
  !recovered_hosts

exception Cannot_eject_master
let no_exn f = try f() with _ -> ()
let unplug_pbds ~__context host =
  let pbds = Db.Host.get_PBDs ~__context ~self:host in
  let srs = List.map (fun self -> Db.PBD.get_SR ~__context ~self) pbds in
  let srs_to_delete = List.filter (fun self -> List.length (Db.SR.get_PBDs ~__context ~self) = 1) srs in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       List.iter (fun pbd -> Client.PBD.unplug ~rpc ~session_id ~self:pbd) pbds;
       List.iter (fun sr -> Client.SR.forget ~rpc ~session_id ~sr) srs_to_delete)

(* This means eject me, since will have been forwarded from master  *)
let eject ~__context ~host =
  (* If HA is enabled then refuse *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

  if Pool_role.is_master () then raise Cannot_eject_master
  else begin
    (* Fail the operation if any VMs are running here (except dom0) *)
    let my_vms_with_records = Db.VM.get_records_where ~__context ~expr:(Eq(Field "resident_on", Literal (Ref.string_of host))) in
    List.iter (fun (_, x) ->
        if (not (Helpers.is_domain_zero ~__context (Db.VM.get_by_uuid ~__context ~uuid:x.API.vM_uuid))) && x.API.vM_power_state <>`Halted
        then begin
          error "VM uuid %s not in Halted state and resident_on this host" (x.API.vM_uuid);
          raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["VM resident on host"]))
        end) my_vms_with_records;

    (* all control domains resident on me should be destroyed once I leave the
       		pool, therefore pick them out as follows: if they have a valid resident_on,
       		the latter should be me; if they don't (e.g. they are halted), they should have
       		disks on my local storage *)
    let vm_is_resident_on_host vm_rec host =
      (Db.is_valid_ref __context vm_rec.API.vM_resident_on) && (vm_rec.API.vM_resident_on = host)
    in
    let vm_has_disks_on_local_sr_of_host vm_ref host =
      let is_sr_local x = not (Helpers.is_sr_shared ~__context ~self:x) in
      let host_has_sr x = (Helpers.check_sr_exists_for_host ~__context ~self:x ~host:host) <> None in
      Db.VM.get_VBDs ~__context ~self:vm_ref
      |> List.map (fun x -> Db.VBD.get_VDI ~__context ~self:x)
      |> List.filter (fun x -> x <> Ref.null) (* filter out null ref VDIs (can happen e.g. for CDs) *)
      |> List.map (fun x -> Db.VDI.get_SR ~__context ~self:x)
      |> List.exists (fun x -> (is_sr_local x) && (host_has_sr x))
    in
    let is_obsolete_control_domain (vm_ref, vm_rec) =
      vm_rec.API.vM_is_control_domain
      && ((vm_is_resident_on_host vm_rec host) || (vm_has_disks_on_local_sr_of_host vm_ref host))
    in
    let control_domains_to_destroy = List.filter is_obsolete_control_domain (Db.VM.get_all_records ~__context) in

    debug "Pool.eject: unplugging PBDs";
    (* unplug all my PBDs; will deliberately fail if any unplugs fail *)
    unplug_pbds ~__context host;

    debug "Pool.eject: disabling external authentication in slave-to-be-ejected";
    (* disable the external authentication of this slave being ejected *)
    (* this call will return an exception if something goes wrong *)
    Xapi_host.disable_external_auth_common ~during_pool_eject:true ~__context ~host
      ~config:[];
    (* FIXME: in the future, we should send the windows AD admin/pass here *)
    (* in order to remove the slave from the AD database during pool-eject *)

    debug "Pool.eject: rewrite networking first-boot files";
    let management_pif = Xapi_host.get_management_interface ~__context ~host in
    let pif = Db.PIF.get_record ~__context ~self:management_pif in
    let management_device =
      (* Assumes that the management interface is either physical or a bond or a vlan on physical or a vlan on bond *)
      if pif.API.pIF_bond_master_of <> [] then
        let bond = List.hd pif.API.pIF_bond_master_of in
        let primary_slave = Db.Bond.get_primary_slave ~__context ~self:bond in
        Db.PIF.get_device ~__context ~self:primary_slave
        (* If management on VLAN on a bond or physical NIC *)
      else if pif.API.pIF_VLAN_master_of <> Ref.null then
        let tagged_pif = Db.VLAN.get_tagged_PIF ~__context ~self:(pif.API.pIF_VLAN_master_of) in
        let bond_master = Db.PIF.get_bond_master_of ~__context ~self:tagged_pif in
        if bond_master <> [] then
          let bond = List.hd bond_master in
          let primary_slave = Db.Bond.get_primary_slave ~__context ~self:bond in
          Db.PIF.get_device ~__context ~self:primary_slave
        else
          pif.API.pIF_device
      else
        pif.API.pIF_device
    in
    let mode = match pif.API.pIF_ip_configuration_mode with
      | `None -> "none"
      | `DHCP -> "dhcp"
      | `Static -> "static"
    in

    let write_first_boot_management_interface_configuration_file () =
      (* During firstboot, now inventory has an empty MANAGEMENT_INTERFACE *)
      let bridge = "" in
      Xapi_inventory.update Xapi_inventory._management_interface bridge;
      let primary_address_type = Db.PIF.get_primary_address_type ~__context ~self:management_pif in
      Xapi_inventory.update Xapi_inventory._management_address_type
        (Record_util.primary_address_type_to_string primary_address_type);
      let sprintf = Printf.sprintf in
      (* If the management_interface exists on a vlan, write the vlan id into management.conf *)
      let vlan_id = Int64.to_int pif.API.pIF_VLAN in
      let config_base =
        [ sprintf "LABEL='%s'" management_device
        ; sprintf "MODE='%s'" mode
        ] in
      let config_static = if mode <> "static" then [] else
          [ sprintf "IP='%s'" pif.API.pIF_IP
          ; sprintf "NETMASK='%s'" pif.API.pIF_netmask
          ; sprintf "GATEWAY='%s'" pif.API.pIF_gateway
          ; sprintf "DNS='%s'" pif.API.pIF_DNS
          ] in
      let config_vlan = if vlan_id = -1 then [] else
          [ sprintf "VLAN='%d'" vlan_id
          ] in
      let configuration_file =
        List.concat
          [ config_base
          ; config_static
          ; config_vlan
          ]
        |> String.concat "\n"
      in
      Unixext.write_string_to_file
        (Xapi_globs.first_boot_dir ^ "data/management.conf")
        (configuration_file) in

    write_first_boot_management_interface_configuration_file ();

    Net.reset_state ();
    Xapi_inventory.update Xapi_inventory._current_interfaces "";

    debug "Pool.eject: deleting Host record (the point of no return)";
    (* delete me from the database - this will in turn cause PBDs and PIFs to be GCed *)
    Db.Host.destroy ~__context ~self:host;
    Create_misc.create_pool_cpuinfo ~__context;

    (* Update pool features, in case this host had a different license to the
       		 * rest of the pool. *)
    Pool_features.update_pool_features ~__context;

    (* and destroy my control domains, since you can't do this from the API [operation not allowed] *)
    begin try
        List.iter (fun x -> Db.VM.destroy ~__context ~self:(fst x)) control_domains_to_destroy;
      with _ -> () end;
    debug "Pool.eject: setting our role to be master";
    Pool_role.set_role Pool_role.Master;
    debug "Pool.eject: forgetting pool secret";
    Unixext.unlink_safe !Xapi_globs.pool_secret_path; (* forget current pool secret *)
    (* delete backup databases and any temporary restore databases *)
    Unixext.unlink_safe Xapi_globs.backup_db_xml;
    Unixext.unlink_safe Xapi_globs.db_temporary_restore_path;
    Unixext.unlink_safe Db_globs.ha_metadata_db;
    Unixext.unlink_safe Db_globs.gen_metadata_db;

    (* If we've got local storage, remove it *)
    if (Helpers.local_storage_exists ()) then begin
      ignore(Forkhelpers.execute_command_get_output "/bin/rm" ["-rf"; Xapi_globs.xapi_blob_location]);
      Unixext.mkdir_safe Xapi_globs.xapi_blob_location 0o700;
    end;

    (* delete /local/ databases specified in the db.conf, so they get recreated on restart.
       		 * We must leave any remote database alone because these are owned by the pool and
       		 * not by this node. *)
    (* get the slave backup lock so we know no more backups are going to be taken --
       		 * we keep this lock till the bitter end, where we restart below ;)
       		 *)
    Mutex.lock Pool_db_backup.slave_backup_m;
    finally
      (fun () ->
         let dbs = Parse_db_conf.parse_db_conf !Db_globs.db_conf_path in
         (* We need to delete all local dbs but leave remote ones alone *)
         let local = List.filter (fun db -> not db.Parse_db_conf.is_on_remote_storage) dbs in
         List.iter Unixext.unlink_safe (List.map (fun db->db.Parse_db_conf.path) local);
         List.iter Unixext.unlink_safe (List.map Parse_db_conf.generation_filename local);
         (* remove any shared databases from my db.conf *)
         (* XXX: on OEM edition the db.conf is rebuilt on every boot *)
         Parse_db_conf.write_db_conf local;
         (* Forget anything we know about configured remote databases: this prevents
            			any initscript reminding us about them after reboot *)
         Helpers.log_exn_continue
           (Printf.sprintf "Moving remote database file to backup: %s"
              !Xapi_globs.remote_db_conf_fragment_path)
           (fun () ->
              Unix.rename
                !Xapi_globs.remote_db_conf_fragment_path
                (!Xapi_globs.remote_db_conf_fragment_path ^ ".bak")) ();
         (* Reset the domain 0 network interface naming configuration
            			 * back to a fresh-install state for the currently-installed
            			 * hardware.
            			 *)
         ignore
           (Forkhelpers.execute_command_get_output
              "/etc/sysconfig/network-scripts/interface-rename.py"
              ["--reset-to-install"]);
      )
      (fun () -> Xapi_fuse.light_fuse_and_reboot_after_eject());
    Xapi_hooks.pool_eject_hook ~__context
  end

(* Prohibit parallel flushes since they're so expensive *)
let sync_m = Mutex.create ()

open Db_cache_types

let sync_database ~__context =
  Mutex.execute sync_m
    (fun () ->
       (* If HA is enabled I'll first try to flush to the LUN *)
       let pool = Helpers.get_pool ~__context in
       let flushed_to_vdi = Db.Pool.get_ha_enabled ~__context ~self:pool && (Db_lock.with_lock (fun () -> Xha_metadata_vdi.flush_database ~__context Xapi_ha.ha_redo_log)) in
       if flushed_to_vdi
       then debug "flushed database to metadata VDI: assuming this is sufficient."
       else begin
         debug "flushing database to all online nodes";
         let generation = Db_lock.with_lock (fun () -> Manifest.generation (Database.manifest (Db_ref.get_database (Context.database_of __context)))) in
         Threadext.thread_iter
           (fun host ->
              Helpers.call_api_functions ~__context
                (fun rpc session_id -> Client.Host.request_backup rpc session_id host generation true))
           (Db.Host.get_all ~__context)
       end
    )

(* This also means me, since call will have been forwarded from the current master *)
let designate_new_master ~__context ~host =
  if not (Pool_role.is_master ()) then begin
    let pool = Helpers.get_pool ~__context in
    if Db.Pool.get_ha_enabled ~__context ~self:pool
    then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

    (* Only the master can sync the *current* database; only the master
       		   knows the current generation count etc. *)
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         Client.Pool.sync_database rpc session_id);

    let all_hosts = Db.Host.get_all ~__context in
    (* We make no attempt to demand a quorum or anything. *)
    let addresses = List.map (fun self -> Db.Host.get_address ~__context ~self) all_hosts in
    let my_address = Db.Host.get_address ~__context ~self:(Helpers.get_localhost ~__context) in
    let peers = List.filter (fun x -> x <> my_address) addresses in
    Xapi_pool_transition.attempt_two_phase_commit_of_new_master ~__context true peers my_address
  end

let management_reconfigure ~__context ~network =
  (* Disallow if HA is enabled *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then
    raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

  (* Create a hash table for hosts with pifs for the network *)
  let pifs_on_network = Db.Network.get_PIFs ~__context ~self:network in
  let hosts_with_pifs = Hashtbl.create 16 in
  List.iter (fun self ->
      let host = Db.PIF.get_host ~__context ~self in
      Hashtbl.add hosts_with_pifs host self;
    ) pifs_on_network;

  (* All Hosts must have associated PIF on the network *)
  let all_hosts = Db.Host.get_all ~__context in
  List.iter (fun host ->
      if not(Hashtbl.mem hosts_with_pifs host) then
        raise (Api_errors.Server_error(Api_errors.pif_not_present, [Ref.string_of host; Ref.string_of network]));
    ) all_hosts;

  let address_type = Db.PIF.get_primary_address_type ~__context ~self:(List.hd pifs_on_network) in
  List.iter (fun self ->
      let primary_address_type = Db.PIF.get_primary_address_type ~__context ~self in
      if primary_address_type <> address_type then
        raise (Api_errors.Server_error(Api_errors.pif_incompatible_primary_address_type, [Ref.string_of self]));
      Xapi_pif.assert_usable_for_management ~__context ~primary_address_type ~self;
    ) pifs_on_network;

  (* Perform Host.management_reconfigure on slaves first and last on master *)
  let f ~rpc ~session_id ~host =
    Client.Host.management_reconfigure ~rpc ~session_id ~pif:(Hashtbl.find hosts_with_pifs host)
  in
  Xapi_pool_helpers.call_fn_on_slaves_then_master ~__context f;

  (* Perform Pool.recover_slaves *)
  let hosts_recovered =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.Pool.recover_slaves rpc session_id) in
  List.iter (fun host -> debug "Host recovered=%s" (Db.Host.get_uuid ~__context ~self:host)) hosts_recovered

let initial_auth ~__context =
  !Xapi_globs.pool_secret

(** This call is used during master startup so we should check to see whether we need to re-establish our database
    connection and resynchronise lost database state i.e. state which is non-persistent or reverted over a master crash *)
let is_slave ~__context ~host =
  let is_slave = not (Pool_role.is_master ()) in
  info "Pool.is_slave call received (I'm a %s)" (if is_slave then "slave" else "master");
  debug "About to kick the database connection to make sure it's still working...";
  let (_: bool) = Db.is_valid_ref __context (Ref.of_string "Pool.is_slave checking to see if the database connection is up") in
  is_slave

let hello ~__context ~host_uuid ~host_address =
  let host_exists = try (Some (Db.Host.get_by_uuid ~__context ~uuid:host_uuid)) with _ -> None in
  match host_exists with
    None -> `unknown_host
  | Some host_ref ->

    try
      let slave_current_address = Db.Host.get_address ~__context ~self:host_ref in
      if host_address<>slave_current_address then
        begin
          (* update slave address in master db because we know its changed *)
          Db.Host.set_address ~__context ~self:host_ref ~value:host_address;
          (* and refresh console URLs to reflect this change of address *)
          Dbsync_master.refresh_console_urls ~__context
        end;
      let local_fn = is_slave ~host:host_ref in
      (* Nb. next call is purely there to establish that we can talk back to the host that initiated this call *)
      (* We don't care about the return type, only that no exception is raised while talking to it *)
      (try
         ignore(Message_forwarding.do_op_on_nolivecheck_no_retry ~local_fn ~__context ~host:host_ref
                  (fun session_id rpc -> Client.Pool.is_slave rpc session_id host_ref))
       with Api_errors.Server_error(code, [ "pool.is_slave"; "1"; "2" ]) as e when code = Api_errors.message_parameter_count_mismatch ->
         debug "Caught %s: this host is a Rio box" (ExnHelper.string_of_exn e)
          | Api_errors.Server_error(code, _) as e when code = Api_errors.host_still_booting ->
            debug "Caught %s: this host is a Miami box" (ExnHelper.string_of_exn e)
      );

      (* Set the host to disabled initially: when it has finished initialising and is ready to
         	   host VMs it will mark itself as enabled again. *)
      info "Host.enabled: setting host %s (%s) to disabled" (Ref.string_of host_ref) (Db.Host.get_hostname ~__context ~self:host_ref);
      Db.Host.set_enabled ~__context ~self:host_ref ~value:false;
      let pool = Helpers.get_pool ~__context in
      if not (Db.Pool.get_ha_enabled ~__context ~self:pool) then begin
        debug "Host_metrics.live: setting host %s (%s) to alive" (Ref.string_of host_ref) (Db.Host.get_hostname ~__context ~self:host_ref);
        let metrics = Db.Host.get_metrics ~__context ~self:host_ref in
        Db.Host_metrics.set_live ~__context ~self:metrics ~value:true;
      end;
      (* Cancel tasks on behalf of slave *)
      debug "Hello message from slave OK: cancelling tasks on behalf of slave";
      Cancel_tasks.cancel_tasks_on_host ~__context ~host_opt:(Some host_ref);

      (* Make sure we mark this host as live again *)
      Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
        (fun () -> Xapi_globs.hosts_which_are_shutting_down := List.filter (fun x -> x <> host_ref) !Xapi_globs.hosts_which_are_shutting_down);

      (* Update the heartbeat timestamp for this host so we don't mark it as
         	   offline in the next db_gc *)
      let (_: (string * string) list) = Db_gc.tickle_heartbeat ~__context host_ref [] in
      `ok
    with e ->
      debug "Caught exception: %s" (ExnHelper.string_of_exn e);
      `cannot_talk_back

(** Create PIF on each pool host for specified VLAN/device pair.
    On error, destroy all of the PIFs that have already been created. *)
(* !!! THIS CALL IS FUNDAMENTALLY BROKEN wrt bonds -- see CA-22613; it should no longer be used.
   I have pulled together the function definitions specific to create_VLAN and moved them into create_VLAN definition
   itself. create_VLAN_from_PIF (below) is based on the code for create_VLAN; since create_VLAN is now dead (only here
   so we don't break existing API clients) there is no need to factor the commonality between these 2 fns.
*)
let create_VLAN ~__context ~device ~network ~vLAN =
  (* Destroy the list of PIFs - try destroying them with the client API, and if
     the host is offline, just destroy the record *)
  let safe_destroy_PIFs ~__context pifs =
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         List.iter
           (fun pif ->
              try
                (* This call destroys the metrics too *)
                Client.PIF.destroy rpc session_id pif
              with
              | Api_errors.Server_error (a,b) ->
                if a=Api_errors.host_offline
                then
                  Db.PIF.destroy ~__context ~self:pif
                else
                  (* If theres any other error, leave the PIF to be destroyed
                     			 manually. We certainly don't want the Db to be out of
                     			 sync with reality *)
                  ()
              | _ -> ()
           ) pifs) in
  let created = ref [] in
  let hosts = Db.Host.get_all ~__context in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       let pifs = List.map (
           fun host ->
             try
               let pif = Client.PIF.create_VLAN rpc session_id device network host vLAN in
               created := pif :: (!created);
               pif
             with
             | e ->
               (* Any error and we'll clean up and exit *)
               safe_destroy_PIFs ~__context !created;
               raise e
         ) hosts in
       (* CA-22381: best-effort plug of the newly-created VLAN PIFs. Note if any of these calls fail
          	  then nothing is rolled-back and the system will be left with some unplugged VLAN PIFs, which may
          	  confuse the HA agility calculation (but nothing else since everything else can plug on demand) *)
       List.iter (fun pif -> Helpers.log_exn_continue (Printf.sprintf "Plugging VLAN PIF %s" (Ref.string_of pif)) (fun () -> Client.PIF.plug rpc session_id pif) ()) pifs;
       pifs
    )

(* This call always runs on the master, client calls are spawned off and forwarded to slaves. By taking a PIF
   explicitly instead of a device name we ensure that this call works for creating VLANs on bonds across pools..
*)
let create_VLAN_from_PIF ~__context ~pif ~network ~vLAN =
  (* Destroy the list of VLANs, best-effort *)
  let safe_destroy_VLANs ~__context vlans =
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         List.iter
           (fun vlan ->
              try Client.VLAN.destroy rpc session_id vlan
              with _ -> ()
           ) vlans
      ) in
  (* Read the network that the pif is attached to; get the list of all pifs on that network
     	   -- that'll be the pif for each host that we want to make the vlan on. Then go and make
     	   the vlan on all these pifs. Then attempt to do a best-effort plug of the newly created pifs
     	   in order to satisfy ca-22381 *)
  let network_to_get_pifs_from = Db.PIF.get_network ~__context ~self:pif in
  let pifs_on_network = Db.Network.get_PIFs ~__context ~self:network_to_get_pifs_from in
  let is_host_live pif =
    let h = Db.PIF.get_host ~__context ~self:pif in
    let host_metric = Db.Host.get_metrics ~__context ~self:h in
    Db.Host_metrics.get_live ~__context ~self:host_metric in
  let pifs_on_live_hosts = List.filter is_host_live pifs_on_network in

  (* Keep track of what we've created *)
  let created = ref [] in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       let vlans =
         List.map
           (fun pif ->
              try
                let vlan = Client.VLAN.create rpc session_id pif vLAN network in
                created := vlan :: !created;
                vlan
              with
              | e ->
                (* Any error and we'll clean up and exit *)
                safe_destroy_VLANs ~__context !created;
                raise e
           ) pifs_on_live_hosts in
       let vlan_pifs = List.map (fun vlan -> Db.VLAN.get_untagged_PIF ~__context ~self:vlan) vlans in
       (* CA-22381: best-effort plug of the newly-created VLAN PIFs. Note if any of these calls fail
          			   then nothing is rolled-back and the system will be left with some unplugged VLAN PIFs, which may
          			   confuse the HA agility calculation (but nothing else since everything else can plug on demand) *)
       List.iter
         (fun pif ->
            Helpers.log_exn_continue
              (Printf.sprintf "Plugging VLAN PIF %s" (Ref.string_of pif))
              (fun () -> Client.PIF.plug rpc session_id pif) ()
         ) vlan_pifs;
       vlan_pifs
    )

let slave_network_report ~__context ~phydevs ~dev_to_mac ~dev_to_mtu ~slave_host =
  []
(*
  Dbsync_slave.create_physical_networks ~__context phydevs dev_to_mac dev_to_mtu slave_host
*)
(* Let's only process one enable/disable at a time. I would have used an allowed_operation for this but
   it would involve a datamodel change and it's too late for Orlando. *)
let enable_disable_m = Mutex.create ()
let enable_ha ~__context ~heartbeat_srs ~configuration =
  if not (Helpers.pool_has_different_host_platform_versions ~__context)
  then Mutex.execute enable_disable_m (fun () -> Xapi_ha.enable __context heartbeat_srs configuration)
  else
    raise (Api_errors.Server_error (Api_errors.not_supported_during_upgrade, []))

let disable_ha ~__context = Mutex.execute enable_disable_m (fun () -> Xapi_ha.disable __context)

let ha_prevent_restarts_for ~__context ~seconds = Xapi_ha.ha_prevent_restarts_for __context seconds

let ha_failover_plan_exists ~__context ~n =
  let n = Int64.to_int n in
  let all_protected_vms = Xapi_ha_vm_failover.all_protected_vms ~__context in
  match Xapi_ha_vm_failover.plan_for_n_failures ~__context ~all_protected_vms n with
  | Xapi_ha_vm_failover.Plan_exists_for_all_VMs ->
    info "HA failover plan exists for all protected VMs for up to %d host failures" n;
    true
  | Xapi_ha_vm_failover.Plan_exists_excluding_non_agile_VMs ->
    info "HA failover plan exists for all protected VMs, excluding some non-agile VMs, for up to %d host failures" n;
    false (* might define this as true later *)
  | Xapi_ha_vm_failover.No_plan_exists ->
    info "No HA failover plan exists for %d host failures" n;
    false

let ha_compute_max_host_failures_to_tolerate ~__context =
  let n = Xapi_ha_vm_failover.compute_max_host_failures_to_tolerate ~__context () in
  (* Update the Pool with this information if HA is currently enabled *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then begin
    let n' = Db.Pool.get_ha_host_failures_to_tolerate ~__context ~self:pool in
    let overcommitted = n' > n in
    if Db.Pool.get_ha_overcommitted ~__context ~self:pool <> overcommitted
    then Db.Pool.set_ha_overcommitted ~__context ~self:pool ~value:overcommitted;
    let current_plan_for = Db.Pool.get_ha_plan_exists_for ~__context ~self:pool in
    if current_plan_for <> n then begin
      Db.Pool.set_ha_plan_exists_for ~__context ~self:pool ~value:(min n' n);
      if n < current_plan_for
      then Xapi_alert.add ~msg:Api_messages.ha_pool_drop_in_plan_exists_for ~cls:`Pool ~obj_uuid:(Db.Pool.get_uuid ~__context ~self:pool) ~body:(Int64.to_string n);
    end;
  end;
  n

let ha_compute_hypothetical_max_host_failures_to_tolerate ~__context ~configuration =
  (* Check the restart priorities all look valid *)
  List.iter (fun (_, pri) ->
      if not(List.mem pri Constants.ha_valid_restart_priorities)
      then raise (Api_errors.Server_error(Api_errors.invalid_value, [ "ha_restart_priority"; pri ]))) configuration;

  let protected_vms = List.map fst (List.filter (fun (vm, priority) -> Helpers.vm_should_always_run true priority) configuration) in
  let protected_vms = List.map (fun vm -> vm, Db.VM.get_record ~__context ~self:vm) protected_vms in
  Xapi_ha_vm_failover.compute_max_host_failures_to_tolerate ~__context ~protected_vms ()

let ha_compute_vm_failover_plan ~__context ~failed_hosts ~failed_vms =
  let vms = List.map (fun vm -> vm, Db.VM.get_record ~__context ~self:vm) failed_vms in
  let all_hosts = Db.Host.get_all ~__context in
  let currently_live_hosts = List.filter (fun h -> try Db.Host_metrics.get_live ~__context ~self:(Db.Host.get_metrics ~__context ~self:h) with _ -> false) all_hosts in
  let live_hosts = List.filter (fun host -> not(List.mem host failed_hosts)) currently_live_hosts in
  debug "using live_hosts = [ %s ]" (String.concat "; " (List.map Ref.string_of live_hosts));
  (* All failed_vms must be agile *)
  let errors = List.concat
      (List.map
         (fun self ->
            try Agility.vm_assert_agile ~__context ~self; [ self, [ "error_code", Api_errors.host_not_enough_free_memory ] ] (* default *)
            with Api_errors.Server_error(code, params) -> [ self, [ "error_code", code ]]) failed_vms) in
  let plan = List.map (fun (vm, host) -> vm, [ "host", Ref.string_of host ])
      (Xapi_ha_vm_failover.compute_evacuation_plan ~__context (List.length all_hosts) live_hosts vms) in
  (List.filter (fun (vm, _) -> not(List.mem_assoc vm plan)) errors) @ plan

let create_new_blob ~__context ~pool ~name ~mime_type ~public =
  let blob = Xapi_blob.create ~__context ~mime_type ~public in
  Db.Pool.add_to_blobs ~__context ~self:pool ~key:name ~value:blob;
  blob

let set_ha_host_failures_to_tolerate ~__context ~self ~value =
  if value < 0L then raise (Api_errors.Server_error(Api_errors.invalid_value, [ "ha_host_failures_to_tolerate"; Int64.to_string value ]));

  (* Don't block changes if we have no plan at all *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_plan_exists_for ~__context ~self:pool > 0L
  then Xapi_ha_vm_failover.assert_nfailures_change_preserves_ha_plan ~__context (Int64.to_int value);
  Db.Pool.set_ha_host_failures_to_tolerate ~__context ~self ~value;
  let (_: bool) = Xapi_ha_vm_failover.update_pool_status ~__context () in ()

let ha_schedule_plan_recomputation ~__context =
  Xapi_ha.Monitor.plan_out_of_date := true

let call_fn_on_host ~__context f host =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      try
        f ~rpc ~session_id ~host
      with e -> begin
          warn "Exception raised while performing operation on host %s error: %s"
            (Ref.string_of host) (ExnHelper.string_of_exn e);
          raise e
        end
    )

let enable_binary_storage ~__context =
  Xapi_pool_helpers.call_fn_on_slaves_then_master ~__context Client.Host.enable_binary_storage

let disable_binary_storage ~__context =
  Xapi_pool_helpers.call_fn_on_slaves_then_master ~__context Client.Host.disable_binary_storage

let initialize_wlb ~__context ~wlb_url ~wlb_username ~wlb_password ~xenserver_username ~xenserver_password =
  init_wlb ~__context ~wlb_url ~wlb_username ~wlb_password ~xenserver_username ~xenserver_password

let deconfigure_wlb ~__context =
  decon_wlb ~__context

let send_wlb_configuration ~__context ~config =
  send_wlb_config ~__context ~config

let retrieve_wlb_configuration ~__context =
  retrieve_wlb_config ~__context

let retrieve_wlb_recommendations ~__context =
  get_opt_recommendations ~__context

let send_test_post = Remote_requests.send_test_post

let certificate_install = Certificates.pool_install true
let certificate_uninstall = Certificates.pool_uninstall true
let certificate_list ~__context = Certificates.local_list true

let crl_install = Certificates.pool_install false
let crl_uninstall = Certificates.pool_uninstall false
let crl_list ~__context = Certificates.local_list false

let certificate_sync = Certificates.pool_sync

(* destroy all subject not validated in external authentication *)
let revalidate_subjects ~__context =
  let revalidate_subject ~__context ~self =
    let subj_id = Db.Subject.get_subject_identifier ~__context ~self in
    debug "Revalidating subject %s" subj_id;
    try
      let open Auth_signature in
      ignore((Extauth.Ext_auth.d()).query_subject_information subj_id)
    with Not_found ->
      debug "Destroying subject %s" subj_id;
      Xapi_subject.destroy ~__context ~self in
  let subjects_in_db = Db.Subject.get_all ~__context in
  List.iter (fun subj -> revalidate_subject ~__context ~self:subj) subjects_in_db


(* CP-719: Enables external auth/directory service across a whole pool; *)
(* calling Host.enable_external_auth with the specified parameters in turn on each of the hosts in the pool
    * The call fails immediately if any of the pool hosts already have external auth enabled (must disable first)
    * If a call to a single host to enable external auth fails, then Pool.enable_external_auth fails, and there is
      a best-effort attempt to disable any hosts who had their external auth successfully enabled before the failure occured
*)
let enable_external_auth ~__context ~pool ~config ~service_name ~auth_type =

  (* CP-825: Serialize execution of pool-enable-extauth and pool-disable-extauth *)
  (* enabling/disabling the pool's extauth at the same time could produce inconsistent states for extauth in each host of the pool *)
  Mutex.execute Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->

      (* the first element in the hosts list needs to be the pool's master, because we *)
      (* always want to update first the master's record due to homogeneity checks in CA-24856 *)
      let hosts = Xapi_pool_helpers.get_master_slaves_list ~__context in

      (* 1. verifies if any of the pool hosts already have external auth enabled, and fails if so *)
      (* this step isn't strictly necessary, since we will anyway fail in (2) if that is the case, but *)
      (* it avoids unnecessary network roundtrips in the pool *)
      try
        let is_external_auth_enabled host = (Db.Host.get_external_auth_type ~__context ~self:host <> "") in
        let host = List.find is_external_auth_enabled hosts in
        begin
          let host_name_label = Db.Host.get_name_label ~__context ~self:host in
          let msg = ("external authentication service in host "^host_name_label^" is already enabled") in
          debug "Failed to enable external authentication type %s for service name %s in pool: %s" auth_type service_name msg;
          raise (Api_errors.Server_error(Api_errors.pool_auth_already_enabled, [(Ref.string_of host)]))
        end
      with Not_found -> () (* that's expected, no host had external_auth enabled*)
        ;
        (* 1b. assert that there are no duplicate hostnames in the pool *)
        if (List.length hosts)
           <>
           (List.length
              (Listext.List.setify
                 (List.map (fun h->Db.Host.get_hostname ~__context ~self:h) hosts))
           )
        then begin
          let errmsg = "At least two hosts in the pool have the same hostname" in
          debug "%s" errmsg;
          raise (Api_errors.Server_error(Api_errors.pool_auth_enable_failed_duplicate_hostname,
                                         [(Ref.string_of (List.hd hosts));errmsg]))
        end
        else
          (* 2. tries to enable the external authentication in each host of the pool *)
          let host_error_msg = ref ("","","") in
          let rollback_list =
            let _rollback_list = ref [] in
            (* builds a list of hosts to rollback, if any *)
            if List.for_all (*List.for_all goes through the list up to the point when the predicate fails, inclusive *)
                (fun h ->
                   try(* forward the call to the host in the pool *)
                     begin
                       debug "trying to enable external authentication on host %s" (Db.Host.get_name_label ~__context ~self:h);
                       call_fn_on_host ~__context (Client.Host.enable_external_auth ~config ~service_name ~auth_type) h;
                       _rollback_list := h::!_rollback_list; (* add h to potential rollback list *)
                       true (* h was successfully enabled. try next in the pool *)
                     end
                   with
                   | Api_errors.Server_error (err,[msg]) as e -> begin
                       debug "received exception while enabling external authentication for host %s: %s"
                         (Db.Host.get_name_label ~__context ~self:h) (err^": "^msg);
                       host_error_msg := (err,msg,ExnHelper.string_of_exn e);
                       (* error enabling h. we add h here so that we also explicitly disable it during rollback *)
                       (* [that's because it might be in an inconsistent external_auth state] *)
                       _rollback_list := h::!_rollback_list;
                       false
                     end
                   | e -> begin
                       debug "received exception while enabling external authentication for host %s: %s"
                         (Db.Host.get_name_label ~__context ~self:h) (ExnHelper.string_of_exn e);
                       host_error_msg := ("","",ExnHelper.string_of_exn e);
                       (* error enabling h. we add h here so that we also explicitly disable it during rollback *)
                       (* [that's because it might be in an inconsistent external_auth state] *)
                       _rollback_list := h::!_rollback_list;
                       false
                     end
                ) hosts
            then (* if List.for_all returned true, then we have successfully enabled all hosts in the pool *)
              begin
                _rollback_list := [] (* we do not need to rollback any hosts in this case *)
              end;
            !_rollback_list
          in
          (* 3. if any failed, then do a best-effort rollback, disabling any host that has been just enabled *)
          if (List.length rollback_list > 0)
          then begin (* FAILED *)
            let failed_host = (* the failed host is the first item in the rollback list *)
              (List.hd rollback_list) in
            let failed_host_name_label = Db.Host.get_name_label ~__context ~self:failed_host in
            match !host_error_msg with (err_of_e,msg_of_e,string_of_e) ->
              debug "Rolling back any enabled host, because failed to enable external authentication for host %s in the pool: %s" failed_host_name_label string_of_e;
              List.iter (fun host ->
                  (* best-effort attempt to disable all enabled hosts, swallowing any exceptions *)
                  try (call_fn_on_host ~__context (Client.Host.disable_external_auth ~config) host)
                  with e-> (debug "During rollback: Failed to disable external authentication for host %s: %s"
                              (Db.Host.get_name_label ~__context ~self:host) (ExnHelper.string_of_exn e)
                           )
                ) (List.rev rollback_list);
              (* we bubble up the exception returned by the failed host *)
              match err_of_e with
              | "" -> (* generic unknown exception *)
                raise (Api_errors.Server_error(Api_errors.pool_auth_enable_failed, [(Ref.string_of failed_host);string_of_e]))
              | err_of_e when err_of_e=Api_errors.auth_unknown_type ->
                raise (Api_errors.Server_error(Api_errors.auth_unknown_type, [msg_of_e]))
              | err_of_e when Xstringext.String.startswith Api_errors.auth_enable_failed err_of_e ->
                raise (Api_errors.Server_error(Api_errors.pool_auth_prefix^err_of_e, [(Ref.string_of failed_host);msg_of_e]))
              | _ -> (* Api_errors.Server_error *)
                raise (Api_errors.Server_error(Api_errors.pool_auth_enable_failed, [(Ref.string_of failed_host);string_of_e]))
          end

          else begin (* OK *)
            debug "External authentication enabled for all hosts in the pool";

            (* CA-59647: remove subjects that do not belong to the new domain *)
            revalidate_subjects ~__context;
          end
    )

(* CP-719: Calls Host.disable_external_auth() on each of the hosts in the pool
    * Reports failure if any of the individual Host.disable_external_auth calls failed or timed-out
    * Guarantees to call Host.disable_external_auth() on every pool host, regardless of whether some of these calls fail
*)
let disable_external_auth ~__context ~pool ~config =

  (* CP-825: Serialize execution of pool-enable-extauth and pool-disable-extauth *)
  (* enabling/disabling the pool's extauth at the same time could produce inconsistent states for extauth in each host of the pool *)
  Mutex.execute Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->

      (* the first element in the hosts list needs to be the pool's master, because we *)
      (* always want to update first the master's record due to homogeneity checks in CA-24856 *)
      let hosts = Xapi_pool_helpers.get_master_slaves_list ~__context in
      let host_msgs_list =
        List.map (fun host ->
            try	(* forward the call to the host in the pool *)
              call_fn_on_host ~__context (Client.Host.disable_external_auth ~config) host;
              (* no failed host to add to the filtered list, just visit next host *)
              (host,"","")
            with
            | Api_errors.Server_error (err,[host_msg]) -> begin
                let msg = (Printf.sprintf "%s: %s"
                             (Db.Host.get_name_label ~__context ~self:host) host_msg) in
                debug "Failed to disable the external authentication of pool in host %s" msg;
                (* no exception should be raised here, we want to visit every host in hosts *)
                (host,err,msg)
              end
            | e-> (* add failed host to the filtered list and visit next host *)
              let msg = (Printf.sprintf "%s: %s"
                           (Db.Host.get_name_label ~__context ~self:host) (ExnHelper.string_of_exn e)) in
              debug "Failed to disable the external authentication of pool in host %s" msg;
              (* no exception should be raised here, we want to visit every host in hosts *)
              (host,"err",msg)
          )
          hosts
      in
      let failedhosts_list = List.filter (fun (host,err,msg) -> err<>"") host_msgs_list in
      if (List.length failedhosts_list > 0)
      then begin (* FAILED *)
        match List.hd failedhosts_list with (host,err,msg) ->
          debug "Failed to disable the external authentication of at least one host in the pool";
          if Xstringext.String.startswith Api_errors.auth_disable_failed err
          then (* tagged exception *)
            raise (Api_errors.Server_error(Api_errors.pool_auth_prefix^err, [(Ref.string_of host);msg]))
          else (* generic exception *)
            raise (Api_errors.Server_error(Api_errors.pool_auth_disable_failed, [(Ref.string_of host);msg]));
      end
      else begin (* OK *)
        debug "The external authentication of all hosts in the pool was disabled successfully";
      end
    )

(* CA-24856: detect non-homogeneous external-authentication config in pool *)
let detect_nonhomogeneous_external_auth_in_pool ~__context =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let slaves = Xapi_pool_helpers.get_slaves_list ~__context in
      List.iter (fun slave ->
          (* check every *slave* in the pool... (the master is always homogeneous to the pool by definition) *)
          (* (also, checking the master inside this function would create an infinite recursion loop) *)
          Xapi_host.detect_nonhomogeneous_external_auth_in_host ~__context ~host:slave
        ) slaves
    )
let run_detect_nonhomogeneous_external_auth_in_pool () =
  (* we do not want to run this test while the pool's extauth is being enabled or disabled *)
  Threadext.Mutex.execute Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->
      ignore (Server_helpers.exec_with_new_task "run_detect_nonhomogeneous_external_auth"
                (fun __context ->
                   detect_nonhomogeneous_external_auth_in_pool ~__context
                )
             )
    )
let asynchronously_run_detect_nonhomogeneous_external_auth_in_pool =
  At_least_once_more.make "running detect_nonhomogeneous_external_auth" run_detect_nonhomogeneous_external_auth_in_pool

(* non-blocking asynchronous call to verify if the external authentication configuration of the pool is homogeneous *)
let detect_nonhomogeneous_external_auth () =
  At_least_once_more.again asynchronously_run_detect_nonhomogeneous_external_auth_in_pool

(* CA-24856: API call to detect non-homogeneous external-authentication config in pool *)
let detect_nonhomogeneous_external_auth ~__context ~pool =
  detect_nonhomogeneous_external_auth ()


let create_redo_log_vdi ~__context ~sr =
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       Client.VDI.create ~rpc ~session_id
         ~name_label:"Metadata redo-log"
         ~name_description:"Used when HA is disabled, while extra security is still desired"
         ~sR:sr
         ~virtual_size:Redo_log.minimum_vdi_size
         ~_type:`redo_log
         ~sharable:true
         ~read_only:false
         ~other_config:[]
         ~xenstore_data:[]
         ~sm_config:Redo_log.redo_log_sm_config
         ~tags:[]
    )

let find_or_create_redo_log_vdi ~__context ~sr =
  match
    List.filter
      (fun self -> true
                   && (Db.VDI.get_type ~__context ~self = `redo_log)
                   && (Db.VDI.get_virtual_size ~__context ~self >= Redo_log.minimum_vdi_size))
      (Db.SR.get_VDIs ~__context ~self:sr) with
  | x :: _ ->
    info "re-using existing redo-log VDI: %s" (Db.VDI.get_uuid ~__context ~self:x);
    x
  | [] ->
    info "no suitable existing redo-log VDI found; creating a fresh one";
    create_redo_log_vdi ~__context ~sr


let enable_redo_log ~__context ~sr =
  info "Enabling redo log...";

  (* find or create suitable VDI *)
  let vdi =
    try
      find_or_create_redo_log_vdi ~__context ~sr
    with e ->
      let msg = "failed to create a VDI for the redo log on the SR with the given UUID." in
      raise (Api_errors.Server_error(Api_errors.cannot_enable_redo_log, [msg]))
  in

  (* ensure VDI is static, and set a flag in the local DB, such that the redo log can be
     	 * re-enabled after a restart of xapi *)
  begin try
      debug "Ensuring redo-log VDI is static on all hosts in the pool";
      let hosts = Db.Host.get_all ~__context in
      let attach host =
        debug "Attaching VDI on host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Host.attach_static_vdis rpc session_id host [vdi, Xapi_globs.gen_metadata_vdi_reason]);
        debug "Setting redo-log local-DB flag on host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Host.set_localdb_key rpc session_id host Constants.redo_log_enabled "true");
      in
      List.iter attach hosts;
      debug "VDI is static on all hosts"
    with e ->
      let msg = "failed to make VDI static." in
      raise (Api_errors.Server_error(Api_errors.cannot_enable_redo_log, [msg]))
  end;

  (* update state *)
  debug "Updating state...";
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_redo_log_vdi ~__context ~self:pool ~value:vdi;
  Db.Pool.set_redo_log_enabled ~__context ~self:pool ~value:true;

  (* enable the new redo log, unless HA is enabled (which means a redo log
     	 * is already in use) *)
  if not (Db.Pool.get_ha_enabled ~__context ~self:pool) then begin
    Redo_log.enable Xapi_ha.ha_redo_log Xapi_globs.gen_metadata_vdi_reason;
    Localdb.put Constants.redo_log_enabled "true"
  end;
  info "The redo log is now enabled"

let disable_redo_log ~__context =
  info "Disabling redo log...";

  (* disable redo-log state flag and switch off redo log if HA is disabled *)
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_redo_log_enabled ~__context ~self:pool ~value:false;
  if not (Db.Pool.get_ha_enabled ~__context ~self:pool) then begin
    Redo_log_usage.stop_using_redo_log Xapi_ha.ha_redo_log;
    Redo_log.disable Xapi_ha.ha_redo_log;

    (* disable static-ness of the VDI and clear local-DB flags *)
    let vdi = Db.Pool.get_redo_log_vdi ~__context ~self:pool in
    let hosts = Db.Host.get_all ~__context in
    begin try
        let detach host =
          debug "Detaching VDI from host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              Client.Host.detach_static_vdis rpc session_id host [vdi]);
          debug "Clearing redo-log local-DB flag on host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              Client.Host.set_localdb_key rpc session_id host Constants.redo_log_enabled "false");
        in
        List.iter detach hosts;
      with e -> info "Failed to detach static VDIs from all hosts."
    end;
  end;
  info "The redo log is now disabled"

let set_vswitch_controller ~__context ~address =
  let dbg = Context.string_of_task __context in
  match Net.Bridge.get_kind dbg () with
  | Network_interface.Openvswitch ->
    let pool = Helpers.get_pool ~__context in
    let current_address = Db.Pool.get_vswitch_controller ~__context ~self:pool in
    if current_address <> address then begin
      if address <> "" then
        Helpers.assert_is_valid_ip `ipv4 "address" address;
      Db.Pool.set_vswitch_controller ~__context ~self:pool ~value:address;
      let sdn_controllers = Db.SDN_controller.get_all ~__context in
      if sdn_controllers <> [] then
        Xapi_sdn_controller.forget ~__context ~self:(List.hd sdn_controllers);
      if address <> "" then
        ignore (Xapi_sdn_controller.introduce ~__context ~protocol:`ssl ~address:address ~port:6632L);
    end
  | _ -> raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["host not configured for vswitch operation"]))


(* internal intra-pool call to allow slaves to log http actions on the master *)
let audit_log_append ~__context ~line =
  (* populate friendly names for the references of the call arguments *)
  (* this is necessary here because the slave doesn't have access to these names *)
  let line = Rbac_audit.populate_audit_record_with_obj_names_of_refs line in
  (* copy audit record from slave exactly as it is, without any new prefixes *)
  let (_: string) = Rbac_audit.append_line ~raw:true "%s" line in
  ()

let test_archive_target ~__context ~self ~config =
  raise (Api_errors.Server_error (Api_errors.message_removed, []))

let enable_local_storage_caching ~__context ~self =
  let srs = Db.SR.get_all_records ~__context in
  let pbds = Db.PBD.get_all_records ~__context in
  let hosts = Db.Host.get_all ~__context in

  (* Exception handler is to cope with transient PBDs with invalid references *)
  let hosts_and_srs = List.filter_map (fun (pbdref,pbdrec) ->
      try Some (pbdrec.API.pBD_host, pbdrec.API.pBD_SR, List.assoc pbdrec.API.pBD_SR srs) with _ -> None) pbds
  in

  let acceptable = List.filter (fun (href,srref,srrec) ->
      (not srrec.API.sR_shared) &&
      (List.length srrec.API.sR_PBDs = 1) &&
      (List.mem_assoc
         Smint.Sr_supports_local_caching
         (Sm.features_of_driver srrec.API.sR_type))
    ) hosts_and_srs in

  let failed_hosts =
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         let failed = List.filter_map (fun host ->
             let result = ref (Some host) in
             let acceptable_srs = List.filter (fun (href,srref,srrec) -> href=host) acceptable in
             List.iter (fun (href,ref,sr) ->
                 try Client.Host.enable_local_storage_caching rpc session_id host ref; result := None with _ -> ()) acceptable_srs;
             !result
           ) hosts in
         failed)
  in
  if List.length failed_hosts > 0 then
    raise (Api_errors.Server_error (Api_errors.hosts_failed_to_enable_caching, List.map Ref.string_of failed_hosts))
  else ()


let disable_local_storage_caching ~__context ~self =
  let hosts = Db.Host.get_all ~__context in
  let failed_hosts = Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         List.filter_map (fun host ->
             try
               Client.Host.disable_local_storage_caching ~rpc ~session_id ~host;
               None
             with _ ->
               Some host) hosts)
  in
  if List.length failed_hosts > 0 then
    raise (Api_errors.Server_error (Api_errors.hosts_failed_to_disable_caching, List.map Ref.string_of failed_hosts))
  else ()

let get_license_state ~__context ~self =
  let edition_to_int =
    List.map
      V6_interface.(fun ed -> ed.title, ed.order)
      (V6_client.get_editions "get_license_state") in
  let hosts = Db.Host.get_all ~__context in
  let pool_edition, expiry = Xapi_pool_license.get_lowest_edition_with_expiry ~__context ~hosts ~edition_to_int in
  let pool_expiry =
    match expiry with
    | None -> "never"
    | Some date -> if date = Date.of_float License_check.never then "never" else Date.to_string date
  in
  [
    "edition", pool_edition;
    "expiry", pool_expiry;
  ]

let apply_edition ~__context ~self ~edition =
  let hosts = Db.Host.get_all ~__context in
  let apply_fn =
    (fun ~__context ~host ~edition -> Helpers.call_api_functions ~__context
        (fun rpc session_id ->
           Client.Host.apply_edition ~rpc ~session_id ~host ~edition ~force:false))
  in
  Xapi_pool_license.apply_edition_with_rollback ~__context ~hosts ~edition ~apply_fn

(* This is expensive, so should always be run on the master. *)
let assert_mac_seeds_available ~__context ~self ~seeds =
  let module StringSet = Set.Make(String) in
  let all_guests =
    Db.VM.get_records_where
      ~__context
      ~expr:(Eq(Field "is_control_domain", Literal "false"))
  in
  (* Create a set of all MAC seeds in use by guests in the pool. *)
  let mac_seeds_in_use =
    List.fold_left
      (fun acc (_, vm_rec) ->
         try
           let mac_seed =
             List.assoc
               Xapi_globs.mac_seed
               vm_rec.API.vM_other_config
           in
           StringSet.add mac_seed acc
         with Not_found ->
           acc)
      StringSet.empty all_guests
  in
  (* Create a set of the MAC seeds we want to test. *)
  let mac_seeds_to_test =
    List.fold_left
      (fun acc mac_seed -> StringSet.add mac_seed acc) StringSet.empty seeds
  in
  (* Check if the intersection of these sets is non-empty. *)
  let problem_mac_seeds = StringSet.inter mac_seeds_in_use mac_seeds_to_test in
  if not(StringSet.is_empty problem_mac_seeds) then
    raise (Api_errors.Server_error
             (Api_errors.duplicate_mac_seed, [StringSet.choose problem_mac_seeds]))

let set_ssl_legacy_on_each_host ~__context ~self ~value =
  let f ~rpc ~session_id ~host =
    Client.Host.set_ssl_legacy ~rpc ~session_id ~self:host ~value
  in
  Xapi_pool_helpers.call_fn_on_slaves_then_master ~__context f

let disable_ssl_legacy = set_ssl_legacy_on_each_host ~value:false

let enable_ssl_legacy = set_ssl_legacy_on_each_host ~value:true

let set_igmp_snooping_enabled ~__context ~self ~value =
  if value then
    Pool_features.assert_enabled ~__context ~f:Features.IGMP_snooping;

  Helpers.assert_using_vswitch ~__context;

  Db.Pool.set_igmp_snooping_enabled ~__context ~self ~value;

  let hosts = Db.Host.get_all ~__context in
  let networks = Db.Network.get_all ~__context in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let failure = List.fold_left (fun fail host ->
          List.fold_left (fun fail' network ->
              let local_pifs = Xapi_network_attach_helpers.get_local_pifs ~__context ~network ~host in
              try
                match local_pifs with
                | pif :: _ -> (* There is at most one local PIF, by construction *)
                  let pif_record = Db.PIF.get_record ~__context ~self:pif in
                  if (pif_record.API.pIF_VLAN = -1L) && (pif_record.API.pIF_bond_slave_of = Ref.null) then
                    Client.Network.attach ~rpc ~session_id ~network ~host;
                  fail'
                | [] -> (* Internal network *)
                  fail'
              with _ ->
                error "set_igmp_snooping_enabled:Network.attach failed on host uuid=%s network uuid=%s" (Db.Host.get_uuid ~__context ~self:host) (Db.Network.get_uuid ~__context ~self:network);
                true
            ) fail networks
        ) false hosts
      in
      if failure then
        raise (Api_errors.Server_error(Api_errors.could_not_update_igmp_snooping_everywhere, []))
    )

let has_extension ~__context ~self ~name =
  let hosts = Db.Host.get_all ~__context in
  List.for_all (fun host ->
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Host.has_extension rpc session_id host name
        )
    ) hosts

let guest_agent_config_requirements =
  let open Map_check in
  [
    {
      key = Xapi_xenops.Guest_agent_features.Xapi.auto_update_enabled;
      default_value = None;
      is_valid_value = (fun x ->
          try let (_:bool) = bool_of_string x in true
          with Invalid_argument _ -> false);
    };
    {
      key = Xapi_xenops.Guest_agent_features.Xapi.auto_update_url;
      default_value = None;
      is_valid_value = (fun url ->
          match Uri.of_string url |> Uri.scheme with
          | Some "http" | Some "https" -> true
          | _ -> false)
    };
  ]

let add_to_guest_agent_config ~__context ~self ~key ~value =
  Map_check.validate_kvpair "guest_agent_config"
    guest_agent_config_requirements (key, value);
  Db.Pool.add_to_guest_agent_config ~__context ~self ~key ~value;
  Xapi_pool_helpers.apply_guest_agent_config ~__context

let remove_from_guest_agent_config ~__context ~self ~key =
  Db.Pool.remove_from_guest_agent_config ~__context ~self ~key;
  Xapi_pool_helpers.apply_guest_agent_config ~__context
