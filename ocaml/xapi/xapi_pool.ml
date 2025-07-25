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

open Client
module Date = Clock.Date
module Listext = Xapi_stdext_std.Listext
module Unixext = Xapi_stdext_unix.Unixext
module Xstringext = Xapi_stdext_std.Xstringext

module Pkgs = (val Pkg_mgr.get_pkg_mgr)

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

open Network
open Http

module L = Debug.Make (struct let name = "license" end)

module D = Debug.Make (struct let name = "xapi_pool" end)

open D
open Workload_balancing

(* Surpress exceptions *)
let no_exn f x =
  try ignore (f x)
  with exn -> debug "Ignoring exception: %s" (ExnHelper.string_of_exn exn)

let rpc ~__context ~verify_cert host_address xml =
  try Helpers.make_remote_rpc ~__context ~verify_cert host_address xml
  with Xmlrpc_client.Connection_reset ->
    raise
      (Api_errors.Server_error
         (Api_errors.pool_joining_host_connection_failed, [])
      )

let get_pool ~rpc ~session_id =
  match Client.Pool.get_all ~rpc ~session_id with
  | [] ->
      Helpers.internal_error "Remote host does not belong to a pool."
  | [pool] ->
      pool
  | pools ->
      Helpers.internal_error "Should get only one pool, but got %d: %s"
        (List.length pools)
        (pools |> List.map Ref.string_of |> String.concat ",")

let get_master ~rpc ~session_id =
  let pool = get_pool ~rpc ~session_id in
  Client.Pool.get_master ~rpc ~session_id ~self:pool

(* Pre-join asserts *)
let pre_join_checks ~__context ~rpc ~session_id ~force =
  (* I cannot join a Pool unless my management interface exists in the db, otherwise
     	   Pool.eject will fail to rewrite network interface files. *)
  let remote_pool = get_pool ~rpc ~session_id in
  let assert_management_interface_exists () =
    try
      let (_ : API.ref_PIF) =
        Xapi_host.get_management_interface ~__context
          ~host:(Helpers.get_localhost ~__context)
      in
      ()
    with _ ->
      error
        "Pool.join/Pool.eject requires a properly configured management \
         interface. Wait for xapi/firstboot initialisation to complete and \
         then retry." ;
      raise (Api_errors.Server_error (Api_errors.host_still_booting, []))
  in
  (* I cannot join a Pool if I have HA already enabled on me *)
  let ha_is_not_enable_on_me () =
    let pool = Helpers.get_pool ~__context in
    if Db.Pool.get_ha_enabled ~__context ~self:pool then (
      error "Cannot join pool as HA is enabled" ;
      raise (Api_errors.Server_error (Api_errors.ha_is_enabled, []))
    )
  in
  (* I Cannot join a Pool if it has HA enabled on it *)
  let ha_is_not_enable_on_the_distant_pool () =
    let pool = get_pool ~rpc ~session_id in
    if Client.Pool.get_ha_enabled ~rpc ~session_id ~self:pool then (
      error "Cannot join pool which already has HA enabled" ;
      raise (Api_errors.Server_error (Api_errors.ha_is_enabled, []))
    )
  in
  (* I cannot join a Pool if I have Clustering enabled on me *)
  let clustering_is_not_enabled_on_me () =
    let host = Helpers.get_localhost ~__context in
    match Xapi_clustering.find_cluster_host ~__context ~host with
    | None ->
        ()
    | Some cluster_host ->
        if Db.Cluster_host.get_enabled ~__context ~self:cluster_host then (
          error "Cannot join pool as Clustering is enabled" ;
          raise
            Api_errors.(
              Server_error (clustering_enabled, [Ref.string_of cluster_host])
            )
        )
  in
  let one_ip_configured_on_joining_cluster_network () =
    let one_ip_configured_on_joining_cluster_network' cluster_host =
      match Client.Cluster_host.get_PIF ~rpc ~session_id ~self:cluster_host with
      | pif when pif = Ref.null ->
          ()
      | pif -> (
        match
          ( Client.PIF.get_VLAN ~rpc ~session_id ~self:pif
          , Client.PIF.get_management ~rpc ~session_id ~self:pif
          )
        with
        | vlan, false when vlan > 0L ->
            error
              "Cannot join pool whose clustering is enabled on a \
               non-management VLAN network" ;
            raise
              (Api_errors.Server_error
                 ( Api_errors
                   .pool_joining_pool_cannot_enable_clustering_on_vlan_network
                 , [Int64.to_string vlan]
                 )
              )
        | _ -> (
            let clustering_bridges_in_pool =
              ( match
                  Client.PIF.get_bond_master_of ~rpc ~session_id ~self:pif
                with
              | [] ->
                  [pif]
              | bonds ->
                  List.concat_map
                    (fun bond ->
                      Client.Bond.get_slaves ~rpc ~session_id ~self:bond
                    )
                    bonds
              )
              |> List.map (fun self ->
                     Client.PIF.get_network ~rpc ~session_id ~self
                 )
              |> List.map (fun self ->
                     Client.Network.get_bridge ~rpc ~session_id ~self
                 )
            in
            match
              Db.Host.get_PIFs ~__context
                ~self:(Helpers.get_localhost ~__context)
              |> List.filter (fun p ->
                     List.exists
                       (fun b ->
                         let network = Db.PIF.get_network ~__context ~self:p in
                         Db.Network.get_bridge ~__context ~self:network = b
                       )
                       clustering_bridges_in_pool
                     && Db.PIF.get_IP ~__context ~self:p <> ""
                 )
            with
            | [_] ->
                ()
            | _ ->
                error
                  "Cannot join pool as the joining host needs to have one (and \
                   only one) IP address on the network that will be used for \
                   clustering." ;
                raise
                  (Api_errors.Server_error
                     ( Api_errors
                       .pool_joining_host_must_have_only_one_IP_on_clustering_network
                     , []
                     )
                  )
          )
      )
    in
    match Client.Cluster_host.get_all ~rpc ~session_id with
    | [] ->
        ()
    | ch :: _ -> (
        let cluster =
          Client.Cluster_host.get_cluster ~rpc ~session_id ~self:ch
        in
        match
          Client.Cluster.get_pool_auto_join ~rpc ~session_id ~self:cluster
        with
        | false ->
            ()
        | true ->
            one_ip_configured_on_joining_cluster_network' ch
      )
  in
  (* CA-26975: Pool edition MUST match *)
  let assert_restrictions_match () =
    let my_edition =
      Db.Host.get_edition ~__context ~self:(Helpers.get_localhost ~__context)
    in
    let host_records =
      List.map snd (Client.Host.get_all_records ~rpc ~session_id)
    in
    let pool_editions =
      List.map (fun host_r -> host_r.API.host_edition) host_records
    in
    (* If all hosts have the same edition string, we need do no more. *)
    if List.fold_left (fun b edn -> edn = my_edition && b) true pool_editions
    then
      ()
    else
      try
        (* We have different edition strings so must consult v6d for their significance.
           			 * This will fail with v6d_failure if v6d is not running. *)
        let editions = V6_client.get_editions "assert_restrictions_match" in
        let edition_to_int e =
          try
            V6_interface.(
              match List.find (fun ed -> ed.title = e) editions with
              | ed ->
                  ed.order
            )
          with Not_found ->
            (* Happens if pool has edition "free/libre" (no v6d) *)
            error
              "Pool.join failed: pool has a host with edition unknown to v6d: \
               %s"
              e ;
            raise
              (Api_errors.Server_error
                 ( Api_errors.license_host_pool_mismatch
                 , ["Edition \"" ^ e ^ "\" from pool is not known to v6d."]
                 )
              )
        in
        let min_edition l =
          List.fold_left
            (fun m e -> if edition_to_int e < edition_to_int m then e else m)
            (List.hd l) l
        in
        (* get pool edition: the "minimum" edition among all hosts *)
        let pool_edition = min_edition pool_editions in
        (* compare my edition to pool edition *)
        if edition_to_int pool_edition <> edition_to_int my_edition then (
          error "Pool.join failed due to edition mismatch" ;
          error "Remote has %s" pool_edition ;
          error "Local has  %s" my_edition ;
          raise
            (Api_errors.Server_error
               ( Api_errors.license_host_pool_mismatch
               , [
                   "host edition = \"" ^ my_edition ^ "\""
                 ; "pool edition = \"" ^ pool_edition ^ "\""
                 ]
               )
            )
        )
      with
      | Api_errors.Server_error (code, []) when code = Api_errors.v6d_failure ->
        error
          "Pool.join failed because edition strings differ and local has no \
           license daemon running." ;
        let pool_edn_list_str = "[" ^ String.concat "; " pool_editions ^ "]" in
        error "Remote editions: %s" pool_edn_list_str ;
        error "Local edition: %s" my_edition ;
        raise
          (Api_errors.Server_error
             ( code
             , ["The pool uses v6d. Pool edition list = " ^ pool_edn_list_str]
             )
          )
  in
  let assert_api_version_matches () =
    let master = get_master ~rpc ~session_id in
    let candidate_slave = Helpers.get_localhost ~__context in
    let master_major =
      Client.Host.get_API_version_major ~rpc ~session_id ~self:master
    in
    let master_minor =
      Client.Host.get_API_version_minor ~rpc ~session_id ~self:master
    in
    let slave_major =
      Db.Host.get_API_version_major ~__context ~self:candidate_slave
    in
    let slave_minor =
      Db.Host.get_API_version_minor ~__context ~self:candidate_slave
    in
    if master_major <> slave_major || master_minor <> slave_minor then (
      error
        "The joining host's API version is %Ld.%Ld while the master's is \
         %Ld.%Ld"
        slave_major slave_minor master_major master_minor ;
      raise
        (Api_errors.Server_error
           ( Api_errors.pool_joining_host_must_have_same_api_version
           , [
               Printf.sprintf "%Ld.%Ld" slave_major slave_minor
             ; Printf.sprintf "%Ld.%Ld" master_major master_minor
             ]
           )
        )
    )
  in
  let assert_db_schema_matches () =
    let master = get_master ~rpc ~session_id in
    let candidate_slave = Helpers.get_localhost ~__context in
    let master_sw_version =
      Client.Host.get_software_version ~rpc ~session_id ~self:master
    in
    let slave_sw_version =
      Db.Host.get_software_version ~__context ~self:candidate_slave
    in
    let master_db_schema =
      try List.assoc Xapi_globs._db_schema master_sw_version with _ -> ""
    in
    let slave_db_schema =
      try List.assoc Xapi_globs._db_schema slave_sw_version with _ -> ""
    in
    if
      master_db_schema = ""
      || slave_db_schema = ""
      || master_db_schema <> slave_db_schema
    then (
      error "The joining host's database schema is %s; the master's is %s"
        slave_db_schema master_db_schema ;
      raise
        (Api_errors.Server_error
           ( Api_errors.pool_joining_host_must_have_same_db_schema
           , [slave_db_schema; master_db_schema]
           )
        )
    )
  in
  let assert_homogeneous_updates () =
    let module S = Helpers.StringSet in
    let local_host = Helpers.get_localhost ~__context in
    let local_uuid = Db.Host.get_uuid ~__context ~self:local_host in
    let updates_on ~rpc ~session_id host =
      Client.Host.get_updates ~rpc ~session_id ~self:host
      |> List.map (fun self ->
             Client.Pool_update.get_record ~rpc ~session_id ~self
         )
      |> List.filter (fun upd -> upd.API.pool_update_enforce_homogeneity = true)
      |> List.map (fun upd -> upd.API.pool_update_uuid)
      |> S.of_list
    in
    let local_updates =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          updates_on ~rpc ~session_id local_host
      )
    in
    (* compare updates on host and pool master *)
    let pool_host = get_master ~rpc ~session_id in
    let remote_updates = updates_on ~rpc ~session_id pool_host in
    if not (S.equal local_updates remote_updates) then (
      let remote_uuid = Client.Host.get_uuid ~rpc ~session_id ~self:pool_host in
      let diff xs ys = S.diff xs ys |> S.elements |> String.concat "," in
      let reason =
        Printf.sprintf "Updates on local host %s and pool host %s differ"
          (Db.Host.get_name_label ~__context ~self:local_host)
          (Client.Host.get_name_label ~rpc ~session_id ~self:pool_host)
      in
      error
        "Pool join: Updates differ. Only on pool host %s: {%s} -- only on \
         local host %s: {%s}"
        remote_uuid
        (diff remote_updates local_updates)
        local_uuid
        (diff local_updates remote_updates) ;
      raise Api_errors.(Server_error (pool_hosts_not_homogeneous, [reason]))
    )
  in
  (* CP-700: Restrict pool.join if AD configuration of slave-to-be does not match *)
  (* that of master of pool-to-join *)
  let assert_external_auth_matches () =
    let master = get_master ~rpc ~session_id in
    let slavetobe = Helpers.get_localhost ~__context in
    let slavetobe_auth_type =
      Db.Host.get_external_auth_type ~__context ~self:slavetobe
    in
    let slavetobe_auth_service_name =
      Db.Host.get_external_auth_service_name ~__context ~self:slavetobe
    in
    let master_auth_type =
      Client.Host.get_external_auth_type ~rpc ~session_id ~self:master
    in
    let master_auth_service_name =
      Client.Host.get_external_auth_service_name ~rpc ~session_id ~self:master
    in
    debug
      "Verifying if external auth configuration of master %s (auth_type=%s \
       service_name=%s) matches that of slave-to-be %s (auth-type=%s \
       service_name=%s)"
      (Client.Host.get_name_label ~rpc ~session_id ~self:master)
      master_auth_type master_auth_service_name
      (Db.Host.get_name_label ~__context ~self:slavetobe)
      slavetobe_auth_type slavetobe_auth_service_name ;
    if
      slavetobe_auth_type <> master_auth_type
      || String.lowercase_ascii slavetobe_auth_service_name
         <> String.lowercase_ascii master_auth_service_name
    then (
      error
        "Cannot join pool whose external authentication configuration is \
         different" ;
      raise
        (Api_errors.Server_error
           (Api_errors.pool_joining_external_auth_mismatch, [])
        )
    )
  in
  let assert_i_know_of_no_other_hosts () =
    let hosts = Db.Host.get_all ~__context in
    if List.length hosts > 1 then (
      error
        "The current host is already the master of other hosts: it cannot join \
         a new pool" ;
      raise
        (Api_errors.Server_error
           (Api_errors.pool_joining_host_cannot_be_master_of_other_hosts, [])
        )
    )
  in
  let assert_no_running_vms_on_me () =
    let my_vms = Db.VM.get_all_records ~__context in
    let my_running_vms =
      List.filter
        (fun (_, vmrec) ->
          (not
             (Helpers.is_domain_zero ~__context
                (Db.VM.get_by_uuid ~__context ~uuid:vmrec.API.vM_uuid)
             )
          )
          && vmrec.API.vM_power_state = `Running
        )
        my_vms
    in
    if my_running_vms <> [] then (
      error
        "The current host has running or suspended VMs: it cannot join a new \
         pool" ;
      raise
        (Api_errors.Server_error
           (Api_errors.pool_joining_host_cannot_have_running_VMs, [])
        )
    )
  in
  let assert_no_vms_with_current_ops () =
    let my_vms = Db.VM.get_all_records ~__context in
    let vms_with_current_ops =
      List.filter (fun (_, vmr) -> vmr.API.vM_current_operations <> []) my_vms
    in
    if vms_with_current_ops <> [] then (
      error
        "The current host has VMs with current operations: it cannot join a \
         new pool" ;
      raise
        (Api_errors.Server_error
           ( Api_errors.pool_joining_host_cannot_have_vms_with_current_operations
           , []
           )
        )
    )
  in
  let assert_no_shared_srs_on_me () =
    let my_srs = Db.SR.get_all_records ~__context in
    let my_shared_srs =
      List.filter
        (fun (_, srec) -> srec.API.sR_shared && not srec.API.sR_is_tools_sr)
        my_srs
    in
    if not (my_shared_srs = []) then (
      error "The current host has shared SRs: it cannot join a new pool" ;
      raise
        (Api_errors.Server_error
           (Api_errors.pool_joining_host_cannot_contain_shared_SRs, [])
        )
    )
  in
  (* Allow pool-join if host does not have any bonds *)
  let assert_no_bonds_on_me () =
    if Db.Bond.get_all ~__context <> [] then (
      error "The current host has network bonds: it cannot join a new pool" ;
      raise
        (Api_errors.Server_error (Api_errors.pool_joining_host_has_bonds, []))
    )
  in
  (* Allow pool-join if host does not have any tunnels *)
  let assert_no_tunnels_on_me () =
    if Db.Tunnel.get_all ~__context <> [] then (
      error "The current host has tunnels: it cannot join a new pool" ;
      raise
        (Api_errors.Server_error (Api_errors.pool_joining_host_has_tunnels, []))
    )
  in
  (* Allow pool-join if host does not have any network-sriovs*)
  let assert_no_network_sriovs_on_me () =
    if Db.Network_sriov.get_all ~__context <> [] then (
      error
        "The current host has network SR-IOV enabled: it cannot join a new pool" ;
      raise
        (Api_errors.Server_error
           (Api_errors.pool_joining_host_has_network_sriovs, [])
        )
    )
  in
  (* Allow pool-join if host does not have any non-management VLANs *)
  let assert_no_non_management_vlans_on_me () =
    List.iter
      (fun self ->
        let pif = Db.VLAN.get_untagged_PIF ~__context ~self in
        if Db.PIF.get_management ~__context ~self:pif <> true then (
          error
            "The current host has non-management vlans: it cannot join a new \
             pool" ;
          raise
            (Api_errors.Server_error
               (Api_errors.pool_joining_host_has_non_management_vlans, [])
            )
        )
      )
      (Db.VLAN.get_all ~__context)
  in
  (* Allow pool-join if the host and the pool are on the same management vlan *)
  let assert_management_vlan_are_same () =
    let management_pif =
      Xapi_host.get_management_interface ~__context
        ~host:(Helpers.get_localhost ~__context)
    in
    let vlan_tag = Db.PIF.get_VLAN ~__context ~self:management_pif in
    let remote_management_pif =
      Client.Host.get_management_interface ~rpc ~session_id
        ~host:(get_master ~rpc ~session_id)
    in
    let remote_vlan_tag =
      Client.PIF.get_VLAN ~rpc ~session_id ~self:remote_management_pif
    in
    if vlan_tag <> remote_vlan_tag then (
      error
        "The current host and the pool management vlan does not match: it \
         cannot join a new pool" ;
      raise
        (Api_errors.Server_error
           ( Api_errors.pool_joining_host_management_vlan_does_not_match
           , [Int64.to_string vlan_tag; Int64.to_string remote_vlan_tag]
           )
        )
    )
  in
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
    let me =
      Db.Host.get_record ~__context ~self:(Helpers.get_localhost ~__context)
    in
    let master_ref = get_master ~rpc ~session_id in
    let master = Client.Host.get_record ~rpc ~session_id ~self:master_ref in
    let my_software_version = me.API.host_software_version in
    let master_software_version = master.API.host_software_version in
    let compatibility_info x =
      let open Xapi_globs in
      let platform_version =
        if List.mem_assoc _platform_version x then
          Some (List.assoc _platform_version x)
        else
          None
      in
      let compatibility_name = get_compatibility_name x in
      (platform_version, compatibility_name)
    in
    let master_compatibility_info =
      compatibility_info master_software_version
    in
    let my_compatibility_info = compatibility_info my_software_version in
    if master_compatibility_info <> my_compatibility_info then (
      debug
        "master PLATFORM_VERSION = %s, master compatibility name = %s; my \
         PLATFORM_VERSION = %s, my compatibility name = %s; "
        (Option.value ~default:"Unknown" (fst master_compatibility_info))
        (Option.value ~default:"Unknown" (snd master_compatibility_info))
        (Option.value ~default:"Unknown" (fst my_compatibility_info))
        (Option.value ~default:"Unknown" (snd my_compatibility_info)) ;
      raise (Api_errors.Server_error (Api_errors.pool_hosts_not_compatible, []))
    )
  in
  let assert_hosts_homogeneous () =
    let me = Helpers.get_localhost ~__context in
    let master_ref = get_master ~rpc ~session_id in
    let master = Client.Host.get_record ~rpc ~session_id ~self:master_ref in
    (* Check software version, but as of CA-249786 don't check the build number*)
    let get_software_version_fields fields =
      let open Xapi_globs in
      ( (try List.assoc _xapi_version fields with _ -> "")
      , (match get_compatibility_name fields with Some x -> x | None -> "")
      , (try List.assoc _git_id fields with _ -> "")
      , try
          if List.mem_assoc linux_pack_vsn_key fields then
            "installed"
          else
            "not present"
        with _ -> "not present"
      )
    in
    let print_software_version (version, name, id, linux_pack) =
      debug "version:%s, name:%s, id:%s, linux_pack:%s" version name id
        linux_pack
    in
    let master_software_version = master.API.host_software_version in
    let my_software_version =
      Db.Host.get_software_version ~__context ~self:me
    in
    let my_software_compare = get_software_version_fields my_software_version in
    let master_software_compare =
      get_software_version_fields master_software_version
    in
    debug "Pool pre-join Software homogeneity check:" ;
    debug "Slave software:" ;
    print_software_version my_software_compare ;
    debug "Master software:" ;
    print_software_version master_software_compare ;
    if my_software_compare <> master_software_compare then
      raise
        (Api_errors.Server_error
           (Api_errors.pool_hosts_not_homogeneous, ["Software version differs"])
        ) ;
    (* Check CPUs *)
    let my_cpu_vendor =
      Db.Host.get_cpu_info ~__context ~self:me |> List.assoc "vendor"
    in
    let pool_cpu_vendor =
      let pool = get_pool ~rpc ~session_id in
      Client.Pool.get_cpu_info ~rpc ~session_id ~self:pool
      |> List.assoc "vendor"
    in
    debug "Pool pre-join CPU homogeneity check:" ;
    debug "Slave CPUs: %s" my_cpu_vendor ;
    debug "Pool CPUs: %s" pool_cpu_vendor ;
    if my_cpu_vendor <> pool_cpu_vendor then
      raise
        (Api_errors.Server_error
           (Api_errors.pool_hosts_not_homogeneous, ["CPUs differ"])
        )
  in
  let assert_not_joining_myself () =
    let master = get_master ~rpc ~session_id in
    let master_uuid = Client.Host.get_uuid ~rpc ~session_id ~self:master in
    let my_uuid =
      Db.Host.get_uuid ~__context ~self:(Helpers.get_localhost ~__context)
    in
    if master_uuid = my_uuid then
      let error_msg =
        if 1 < List.length (Db.Host.get_all ~__context) then
          "Host is already part of a pool"
        else
          "Host cannot become slave of itself"
      in
      raise
        (Api_errors.Server_error (Api_errors.operation_not_allowed, [error_msg]))
  in
  let assert_homogeneous_vswitch_configuration () =
    (* The network backend must be the same as the remote master's backend *)
    let dbg = Context.string_of_task __context in
    let my_backend' = Net.Bridge.get_kind dbg () in
    let my_backend = Network_interface.string_of_kind my_backend' in
    let pool = get_pool ~rpc ~session_id in
    let remote_master = Client.Pool.get_master ~rpc ~session_id ~self:pool in
    let remote_masters_backend =
      let v =
        Client.Host.get_software_version ~rpc ~session_id ~self:remote_master
      in
      if not (List.mem_assoc "network_backend" v) then
        Network_interface.string_of_kind Network_interface.Bridge
      else
        List.assoc "network_backend" v
    in
    if my_backend <> remote_masters_backend then
      raise
        (Api_errors.Server_error
           (Api_errors.operation_not_allowed, ["Network backends differ"])
        ) ;
    match my_backend' with
    | Network_interface.Openvswitch -> (
        let remote_sdn_controllers =
          Client.SDN_controller.get_all ~rpc ~session_id
        in
        let my_sdn_controllers = Db.SDN_controller.get_all ~__context in
        (* We assume that each pool has _at most_ one SDN controller *)
        match (remote_sdn_controllers, my_sdn_controllers) with
        | _, [] ->
            ()
        | remote_sdn_controller :: _, my_sdn_controller :: _ ->
            (* check that protocol/address/port are identical *)
            let my_sdn_protocol =
              Db.SDN_controller.get_protocol ~__context ~self:my_sdn_controller
            in
            let my_sdn_address =
              Db.SDN_controller.get_address ~__context ~self:my_sdn_controller
            in
            let my_sdn_port =
              Db.SDN_controller.get_port ~__context ~self:my_sdn_controller
            in
            let remote_sdn_protocol =
              Client.SDN_controller.get_protocol ~rpc ~session_id
                ~self:remote_sdn_controller
            in
            let remote_sdn_address =
              Client.SDN_controller.get_address ~rpc ~session_id
                ~self:remote_sdn_controller
            in
            let remote_sdn_port =
              Client.SDN_controller.get_port ~rpc ~session_id
                ~self:remote_sdn_controller
            in
            if
              my_sdn_protocol <> remote_sdn_protocol
              || my_sdn_address <> remote_sdn_address
              || my_sdn_port <> remote_sdn_port
            then
              raise
                (Api_errors.Server_error
                   (Api_errors.operation_not_allowed, ["SDN controller differs"])
                )
        | _ ->
            raise
              (Api_errors.Server_error
                 (Api_errors.operation_not_allowed, ["SDN controller differs"])
              )
      )
    | _ ->
        ()
  in
  let assert_homogeneous_primary_address_type () =
    let mgmt_iface =
      Xapi_host.get_management_interface ~__context
        ~host:(Helpers.get_localhost ~__context)
    in
    let mgmt_addr_type =
      Db.PIF.get_primary_address_type ~__context ~self:mgmt_iface
    in
    let master = get_master ~rpc ~session_id in
    let master_mgmt_iface =
      Client.Host.get_management_interface ~rpc ~session_id ~host:master
    in
    let master_addr_type =
      Client.PIF.get_primary_address_type ~rpc ~session_id
        ~self:master_mgmt_iface
    in
    if mgmt_addr_type <> master_addr_type then
      raise
        (Api_errors.Server_error
           (Api_errors.operation_not_allowed, ["Primary address type differs"])
        )
  in
  let assert_compatible_network_purpose () =
    try
      let my_nbdish =
        Db.Network.get_all ~__context
        |> List.concat_map (fun nwk ->
               Db.Network.get_purpose ~__context ~self:nwk
           )
        |> List.find (function `nbd | `insecure_nbd -> true | _ -> false)
      in
      let remote_nbdish =
        Client.Network.get_all ~rpc ~session_id
        |> List.concat_map (fun nwk ->
               Client.Network.get_purpose ~rpc ~session_id ~self:nwk
           )
        |> List.find (function `nbd | `insecure_nbd -> true | _ -> false)
      in
      if remote_nbdish <> my_nbdish then
        raise
          Api_errors.(
            Server_error
              ( operation_not_allowed
              , ["Incompatible network purposes: nbd and insecure_nbd"]
              )
          )
    with Not_found -> ()
    (* If either side has no network with nbd-related purpose, then no problem. *)
  in
  let assert_pool_size_unrestricted () =
    let is_restricted = Xapi_host.pool_size_is_restricted ~__context in
    if
      is_restricted
      && List.length (Client.Host.get_all ~rpc ~session_id)
         >= Xapi_globs.restricted_pool_size
    then
      raise
        (Api_errors.Server_error
           ( Api_errors.license_restriction
           , [Features.name_of_feature Features.Pool_size]
           )
        )
  in
  let assert_tls_verification_matches () =
    let joiner_pool = Helpers.get_pool ~__context in
    let tls_enabled_pool =
      Client.Pool.get_tls_verification_enabled ~rpc ~session_id
        ~self:remote_pool
    in
    let tls_enabled_joiner =
      Db.Pool.get_tls_verification_enabled ~__context ~self:joiner_pool
    in
    if tls_enabled_pool <> tls_enabled_joiner then (
      let pp_bool = function true -> "enabled" | false -> "disabled" in
      error "Remote pool has TLS verification %s while this host has it %s"
        (pp_bool tls_enabled_pool)
        (pp_bool tls_enabled_joiner) ;
      raise
        Api_errors.(
          Server_error (pool_joining_host_tls_verification_mismatch, [])
        )
    )
  in
  let assert_not_in_updating_on_me () =
    let pool = Helpers.get_pool ~__context in
    if
      List.exists
        (fun (_, op) -> op = `apply_updates)
        (Db.Pool.get_current_operations ~__context ~self:pool)
    then
      raise Api_errors.(Server_error (not_supported_during_upgrade, []))
  in
  let assert_no_hosts_in_updating () =
    if
      List.exists
        (fun (_, op) -> op = `apply_updates)
        (Client.Pool.get_current_operations ~rpc ~session_id ~self:remote_pool)
    then
      raise Api_errors.(Server_error (not_supported_during_upgrade, []))
  in
  let assert_ca_certificates_compatible () =
    (* When both pools trust a different certificate using the same name
       joining is blocked. The conflict could be resolved by renaming one of
       the two certificates but this might break the assumptions of the
       tooling that installed the certificate. Instead make the user solve it
       before communications are broken.
       The code assumes different certificates have different fingerprints.
    *)
    let conflicting_names = ref [] in
    let module CertMap = Map.Make (String) in
    let expr = {|field "type"="ca"|} in
    let map_of_list list =
      list
      |> List.to_seq
      |> Seq.map (fun (_, record) ->
             ( record.API.certificate_name
             , record.API.certificate_fingerprint_sha256
             )
         )
      |> CertMap.of_seq
    in
    let remote_certs =
      Client.Certificate.get_all_records_where ~rpc ~session_id ~expr
      |> map_of_list
    in
    let local_certs =
      Db.Certificate.get_all_records_where ~__context ~expr |> map_of_list
    in
    let record_on_conflict key fprint = function
      | fprint' when String.equal fprint fprint' ->
          Some fprint
      | _ ->
          conflicting_names := key :: !conflicting_names ;
          None
    in
    let _ = CertMap.union record_on_conflict local_certs remote_certs in
    match !conflicting_names with
    | [] ->
        ()
    | _ ->
        raise
          Api_errors.(
            Server_error
              (pool_joining_host_ca_certificates_conflict, !conflicting_names)
          )
  in
  let assert_no_host_pending_mandatory_guidance () =
    (* Assert that there is no host pending mandatory guidance on the joiner or
       the remote pool coordinator.
    *)
    Repository_helpers.assert_no_host_pending_mandatory_guidance ~__context
      ~host:(Helpers.get_localhost ~__context) ;
    let remote_coordinator = get_master ~rpc ~session_id in
    let remote_coordinator_pending_mandatory_guidances =
      Client.Host.get_pending_guidances ~rpc ~session_id
        ~self:remote_coordinator
    in
    if remote_coordinator_pending_mandatory_guidances <> [] then (
      error
        "%s: %d mandatory guidances are pending for remote coordinator %s: [%s]"
        __FUNCTION__
        (List.length remote_coordinator_pending_mandatory_guidances)
        (Ref.string_of remote_coordinator)
        (remote_coordinator_pending_mandatory_guidances
        |> List.map Updateinfo.Guidance.of_pending_guidance
        |> List.map Updateinfo.Guidance.to_string
        |> String.concat ";"
        ) ;
      raise
        Api_errors.(
          Server_error
            ( host_pending_mandatory_guidances_not_empty
            , [Ref.string_of remote_coordinator]
            )
        )
    )
  in
  let assert_sm_features_compatible () =
    debug
      "%s Checking whether SM features on the joining host is compatible with \
       the pool"
      __FUNCTION__ ;
    (* We consider the case where the existing pool has FOO/m, and the candidate having FOO/n,
       where n >= m, to be compatible. Not vice versa. *)
    let features_compatible coor_features candidate_features =
      (* The pool features must not be reduced or downgraded, although it is fine
         the other way around. *)
      Smint.Feature.compat_features coor_features candidate_features
      = coor_features
    in
    let pool_sms = Client.SM.get_all_records ~rpc ~session_id in
    List.iter
      (fun (sm_ref, sm_rec) ->
        let pool_sm_type = sm_rec.API.sM_type in
        debug "%s Checking SM %s of name %s in the pool" __FUNCTION__
          (Ref.string_of sm_ref) sm_rec.sM_name_label ;
        let candidate_sm_ref, candidate_sm_rec =
          match
            Db.SM.get_records_where ~__context
              ~expr:(Eq (Field "type", Literal pool_sm_type))
          with
          | [(sm_ref, sm_rec)] ->
              (sm_ref, sm_rec)
          | _ ->
              raise
                Api_errors.(
                  Server_error
                    ( pool_joining_sm_features_incompatible
                    , [Ref.string_of sm_ref; ""]
                    )
                )
        in

        let pool_sm_features = sm_rec.sM_features in

        let candidate_sm_features = candidate_sm_rec.API.sM_features in
        if not (features_compatible pool_sm_features candidate_sm_features) then
          raise
            Api_errors.(
              Server_error
                ( pool_joining_sm_features_incompatible
                , [Ref.string_of sm_ref; Ref.string_of candidate_sm_ref]
                )
            )
      )
      pool_sms
  in

  (* call pre-join asserts *)
  assert_pool_size_unrestricted () ;
  assert_management_interface_exists () ;
  ha_is_not_enable_on_me () ;
  clustering_is_not_enabled_on_me () ;
  one_ip_configured_on_joining_cluster_network () ;
  ha_is_not_enable_on_the_distant_pool () ;
  assert_not_joining_myself () ;
  assert_i_know_of_no_other_hosts () ;
  assert_no_running_vms_on_me () ;
  assert_no_vms_with_current_ops () ;
  (* check first no host pending mandatory guidance then the hosts compatible,
     api version and db schema *)
  assert_no_host_pending_mandatory_guidance () ;
  assert_hosts_compatible () ;
  if not force then assert_hosts_homogeneous () ;
  assert_no_shared_srs_on_me () ;
  assert_no_bonds_on_me () ;
  assert_no_tunnels_on_me () ;
  assert_no_network_sriovs_on_me () ;
  assert_no_non_management_vlans_on_me () ;
  assert_management_vlan_are_same () ;
  assert_external_auth_matches () ;
  assert_restrictions_match () ;
  assert_homogeneous_vswitch_configuration () ;
  (* CA-247399: check first the API version and then the database schema *)
  assert_api_version_matches () ;
  assert_db_schema_matches () ;
  assert_homogeneous_updates () ;
  assert_homogeneous_primary_address_type () ;
  assert_compatible_network_purpose () ;
  assert_tls_verification_matches () ;
  assert_ca_certificates_compatible () ;
  assert_not_in_updating_on_me () ;
  assert_no_hosts_in_updating () ;
  assert_sm_features_compatible ()

let rec create_or_get_host_on_master __context rpc session_id (host_ref, host) :
    API.ref_host =
  let my_uuid = host.API.host_uuid in
  let new_host_ref =
    try Client.Host.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      debug "Found no host with uuid = '%s' on the master, so creating one."
        my_uuid ;
      (* CA-51925: Copy the local cache SR *)
      let my_local_cache_sr =
        Db.Host.get_local_cache_sr ~__context ~self:host_ref
      in
      let local_cache_sr =
        if my_local_cache_sr = Ref.null then
          Ref.null
        else
          let my_local_cache_sr_rec =
            Db.SR.get_record ~__context ~self:my_local_cache_sr
          in
          debug "Copying the local cache SR (uuid=%s)"
            my_local_cache_sr_rec.API.sR_uuid ;
          create_or_get_sr_on_master __context rpc session_id
            (my_local_cache_sr, my_local_cache_sr_rec)
      in
      debug "Creating host object on master" ;
      let ref =
        Client.Host.create ~rpc ~session_id ~uuid:my_uuid
          ~name_label:host.API.host_name_label
          ~name_description:host.API.host_name_description
          ~hostname:host.API.host_hostname ~address:host.API.host_address
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
          ~local_cache_sr ~chipset_info:host.API.host_chipset_info
          ~ssl_legacy:false
          ~last_software_update:host.API.host_last_software_update
          ~last_update_hash:host.API.host_last_update_hash
          ~ssh_enabled:host.API.host_ssh_enabled
          ~ssh_enabled_timeout:host.API.host_ssh_enabled_timeout
          ~ssh_expiry:host.API.host_ssh_expiry
          ~console_idle_timeout:host.API.host_console_idle_timeout
          ~ssh_auto_mode:host.API.host_ssh_auto_mode
      in
      (* Copy other-config into newly created host record: *)
      no_exn
        (fun () ->
          Client.Host.set_other_config ~rpc ~session_id ~self:ref
            ~value:host.API.host_other_config
        )
        () ;
      (* Copy the uefi-certificates into the newly created host record *)
      no_exn
        (fun () ->
          Client.Host.write_uefi_certificates_to_disk ~rpc ~session_id ~host:ref
        )
        () ;
      (* Copy the crashdump SR *)
      let my_crashdump_sr =
        Db.Host.get_crash_dump_sr ~__context ~self:host_ref
      in
      if my_crashdump_sr <> Ref.null then (
        let my_crashdump_sr_rec =
          Db.SR.get_record ~__context ~self:my_crashdump_sr
        in
        debug "Copying the crashdump SR (uuid=%s)"
          my_crashdump_sr_rec.API.sR_uuid ;
        let crashdump_sr =
          create_or_get_sr_on_master __context rpc session_id
            (my_crashdump_sr, my_crashdump_sr_rec)
        in
        no_exn
          (fun () ->
            Client.Host.set_crash_dump_sr ~rpc ~session_id ~self:ref
              ~value:crashdump_sr
          )
          ()
      ) ;
      (* Copy the suspend image SR *)
      let my_suspend_image_sr =
        Db.Host.get_crash_dump_sr ~__context ~self:host_ref
      in
      if my_suspend_image_sr <> Ref.null then (
        let my_suspend_image_sr_rec =
          Db.SR.get_record ~__context ~self:my_suspend_image_sr
        in
        debug "Copying the suspend-image SR (uuid=%s)"
          my_suspend_image_sr_rec.API.sR_uuid ;
        let suspend_image_sr =
          create_or_get_sr_on_master __context rpc session_id
            (my_suspend_image_sr, my_suspend_image_sr_rec)
        in
        no_exn
          (fun () ->
            Client.Host.set_suspend_image_sr ~rpc ~session_id ~self:ref
              ~value:suspend_image_sr
          )
          ()
      ) ;
      ref
  in
  new_host_ref

and create_or_get_sr_on_master __context rpc session_id (_sr_ref, sr) :
    API.ref_SR =
  let my_uuid = sr.API.sR_uuid in
  try Client.SR.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
  with _ ->
    if sr.API.sR_is_tools_sr then (* find the tools SR and return it *)
      try
        Client.SR.get_all_records ~rpc ~session_id
        |> List.find (fun (_, sr) -> sr.API.sR_is_tools_sr)
        |> fun (ref, _) -> ref
      with Not_found ->
        Helpers.internal_error "can't find SR %s of tools iso" my_uuid
    else (
      debug "Found no SR with uuid = '%s' on the master, so creating one."
        my_uuid ;
      let ref =
        Client.SR.introduce ~rpc ~session_id ~uuid:my_uuid
          ~name_label:sr.API.sR_name_label
          ~name_description:sr.API.sR_name_description ~_type:sr.API.sR_type
          ~content_type:sr.API.sR_content_type ~shared:false
          ~sm_config:sr.API.sR_sm_config
      in
      (* copy other-config into newly created sr record: *)
      no_exn
        (fun () ->
          Client.SR.set_other_config ~rpc ~session_id ~self:ref
            ~value:sr.API.sR_other_config
        )
        () ;
      ref
    )

let create_or_get_pbd_on_master __context rpc session_id (_pbd_ref, pbd) :
    API.ref_PBD =
  let my_uuid = pbd.API.pBD_uuid in
  let new_pbd_ref =
    try Client.PBD.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      let my_host_ref = pbd.API.pBD_host in
      let my_host = Db.Host.get_record ~__context ~self:my_host_ref in
      let new_host_ref =
        create_or_get_host_on_master __context rpc session_id
          (my_host_ref, my_host)
      in
      let my_sr_ref = pbd.API.pBD_SR in
      let my_sr = Db.SR.get_record ~__context ~self:my_sr_ref in
      let new_sr_ref =
        create_or_get_sr_on_master __context rpc session_id (my_sr_ref, my_sr)
      in
      debug "Found no PBD with uuid = '%s' on the master, so creating one."
        my_uuid ;
      Client.PBD.create ~rpc ~session_id ~host:new_host_ref ~sR:new_sr_ref
        ~other_config:pbd.API.pBD_other_config
        ~device_config:pbd.API.pBD_device_config
  in
  new_pbd_ref

let create_or_get_vdi_on_master __context rpc session_id (vdi_ref, vdi) :
    API.ref_VDI =
  let my_uuid = vdi.API.vDI_uuid in
  let my_sr_ref = vdi.API.vDI_SR in
  let my_sr = Db.SR.get_record ~__context ~self:my_sr_ref in
  let new_sr_ref =
    create_or_get_sr_on_master __context rpc session_id (my_sr_ref, my_sr)
  in
  let new_vdi_ref =
    try Client.VDI.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      debug "Found no VDI with uuid = '%s' on the master, so creating one."
        my_uuid ;
      Client.VDI.pool_introduce ~rpc ~session_id ~uuid:my_uuid
        ~name_label:vdi.API.vDI_name_label
        ~name_description:vdi.API.vDI_name_description ~sR:new_sr_ref
        ~_type:vdi.API.vDI_type ~sharable:vdi.API.vDI_sharable
        ~read_only:vdi.API.vDI_read_only ~other_config:vdi.API.vDI_other_config
        ~location:(Db.VDI.get_location ~__context ~self:vdi_ref)
        ~xenstore_data:vdi.API.vDI_xenstore_data
        ~sm_config:vdi.API.vDI_sm_config ~managed:vdi.API.vDI_managed
        ~virtual_size:vdi.API.vDI_virtual_size
        ~physical_utilisation:vdi.API.vDI_physical_utilisation
        ~metadata_of_pool:vdi.API.vDI_metadata_of_pool
        ~is_a_snapshot:vdi.API.vDI_is_a_snapshot
        ~snapshot_time:vdi.API.vDI_snapshot_time
        ~snapshot_of:vdi.API.vDI_snapshot_of
        ~cbt_enabled:vdi.API.vDI_cbt_enabled
  in
  new_vdi_ref

let create_or_get_network_on_master __context rpc session_id
    (_network_ref, network) : API.ref_network =
  let my_bridge = network.API.network_bridge in
  let is_physical =
    match network.API.network_PIFs with
    | [] ->
        false
    | hd :: _ ->
        Db.PIF.get_physical ~__context ~self:hd
  in
  let is_himn =
    List.mem_assoc Xapi_globs.is_host_internal_management_network
      network.API.network_other_config
    && List.assoc Xapi_globs.is_host_internal_management_network
         network.API.network_other_config
       = "true"
  in
  let new_network_ref =
    if is_physical || is_himn then (
      try
        (* Physical network or Host Internal Management Network:
           			 * try to join an existing network with the same bridge name, or create one.
           			 * This relies on the convention that physical PIFs with the same device name need to be connected.
           			 * Furthermore, there should be only one Host Internal Management Network in a pool. *)
        let pool_networks = Client.Network.get_all_records ~rpc ~session_id in
        let net_ref, _ =
          List.find
            (fun (_, net) -> net.API.network_bridge = my_bridge)
            pool_networks
        in
        net_ref
      with _ ->
        debug
          "Found no network with bridge = '%s' on the master, so creating one."
          my_bridge ;
        Client.Network.pool_introduce ~rpc ~session_id
          ~name_label:network.API.network_name_label
          ~name_description:network.API.network_name_description
          ~mTU:network.API.network_MTU
          ~other_config:network.API.network_other_config
          ~bridge:network.API.network_bridge
          ~managed:network.API.network_managed
          ~purpose:network.API.network_purpose
    ) else (
      debug "Recreating network '%s' as internal network."
        network.API.network_name_label ;
      (* This call will generate a new 'xapi#' bridge name rather than keeping the
         			 * current, possibly colliding one. *)
      Client.Network.create ~rpc ~session_id
        ~name_label:network.API.network_name_label
        ~name_description:network.API.network_name_description
        ~mTU:network.API.network_MTU
        ~other_config:network.API.network_other_config
        ~bridge:network.API.network_bridge ~managed:network.API.network_managed
        ~tags:network.API.network_tags
    )
  in
  new_network_ref

let create_or_get_pif_on_master __context rpc session_id (_pif_ref, pif) :
    API.ref_PIF =
  let my_uuid = pif.API.pIF_uuid in
  let my_host_ref = pif.API.pIF_host in
  let my_host = Db.Host.get_record ~__context ~self:my_host_ref in
  let new_host_ref =
    create_or_get_host_on_master __context rpc session_id (my_host_ref, my_host)
  in
  let new_network_ref =
    if pif.API.pIF_VLAN <> -1L then
      (* Get the remote management network for management VLAN PIF *)
      let remote_mgmt_pif =
        Client.Host.get_management_interface ~rpc ~session_id
          ~host:(get_master ~rpc ~session_id)
      in
      Client.PIF.get_network ~rpc ~session_id ~self:remote_mgmt_pif
    else
      let my_network_ref = pif.API.pIF_network in
      let my_network = Db.Network.get_record ~__context ~self:my_network_ref in
      create_or_get_network_on_master __context rpc session_id
        (my_network_ref, my_network)
  in
  let new_pif_ref =
    try Client.PIF.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      debug "Found no PIF with uuid = '%s' on the master, so creating one."
        my_uuid ;
      Client.PIF.pool_introduce ~rpc ~session_id ~device:pif.API.pIF_device
        ~network:new_network_ref ~host:new_host_ref ~mAC:pif.API.pIF_MAC
        ~mTU:pif.API.pIF_MTU ~vLAN:pif.API.pIF_VLAN
        ~physical:pif.API.pIF_physical
        ~ip_configuration_mode:pif.API.pIF_ip_configuration_mode
        ~iP:pif.API.pIF_IP ~netmask:pif.API.pIF_netmask
        ~gateway:pif.API.pIF_gateway ~dNS:pif.API.pIF_DNS
        ~bond_slave_of:pif.API.pIF_bond_slave_of
        ~vLAN_master_of:pif.API.pIF_VLAN_master_of
        ~management:pif.API.pIF_management
        ~other_config:pif.API.pIF_other_config
        ~disallow_unplug:pif.API.pIF_disallow_unplug
        ~ipv6_configuration_mode:pif.API.pIF_ipv6_configuration_mode
        ~iPv6:pif.API.pIF_IPv6 ~ipv6_gateway:pif.API.pIF_ipv6_gateway
        ~primary_address_type:pif.API.pIF_primary_address_type
        ~managed:pif.API.pIF_managed ~properties:pif.API.pIF_properties
  in
  new_pif_ref

let create_or_get_vlan_on_master __context rpc session_id (_vlan_ref, vlan) :
    API.ref_VLAN =
  (* Create a VLAN PIF record only if it is a management PIF *)
  let my_host_ref = Db.PIF.get_host ~__context ~self:vlan.API.vLAN_tagged_PIF in
  let my_host = Db.Host.get_record ~__context ~self:my_host_ref in
  let new_host_ref =
    create_or_get_host_on_master __context rpc session_id (my_host_ref, my_host)
  in
  (* Create the untagged PIF record on Pool *)
  let untagged_pif_record =
    Db.PIF.get_record ~__context ~self:vlan.API.vLAN_untagged_PIF
  in
  let remote_untagged_pif =
    create_or_get_pif_on_master __context rpc session_id
      (vlan.API.vLAN_untagged_PIF, untagged_pif_record)
  in
  (* Get the remote tagged pif network *)
  let tagged_pif_network =
    Db.PIF.get_network ~__context ~self:vlan.API.vLAN_tagged_PIF
  in
  let tagged_pif_network_record =
    Db.Network.get_record ~__context ~self:tagged_pif_network
  in
  let remote_tagged_pif_network =
    create_or_get_network_on_master __context rpc session_id
      (tagged_pif_network, tagged_pif_network_record)
  in
  (* Get the new physical PIF ref on Pool for the joining Host *)
  let expr =
    Printf.sprintf "field \"network\"=\"%s\" and field \"host\"=\"%s\""
      (Ref.string_of remote_tagged_pif_network)
      (Ref.string_of new_host_ref)
  in
  let remote_physical_pif =
    match Client.PIF.get_all_records_where ~rpc ~session_id ~expr with
    | [] ->
        Ref.null
    | (pif, _) :: _ ->
        pif
  in
  let new_vlan_ref =
    try Client.VLAN.get_by_uuid ~rpc ~session_id ~uuid:vlan.API.vLAN_uuid
    with _ ->
      debug "Found no VLAN with uuid = '%s' on the master, so creating one."
        vlan.API.vLAN_uuid ;
      Client.VLAN.pool_introduce ~rpc ~session_id
        ~tagged_PIF:remote_physical_pif ~untagged_PIF:remote_untagged_pif
        ~tag:vlan.API.vLAN_tag ~other_config:vlan.API.vLAN_other_config
  in
  new_vlan_ref

let create_or_get_pvs_site_on_master __context rpc session_id
    (pvs_site_ref, pvs_site) : API.ref_PVS_site =
  let my_pvs_uuid = pvs_site.API.pVS_site_PVS_uuid in
  let new_pvs_site_ref =
    let expr = "field \"PVS_uuid\"=\"" ^ my_pvs_uuid ^ "\"" in
    match Client.PVS_site.get_all_records_where ~rpc ~session_id ~expr with
    | [] ->
        debug
          "Found no PVS site with PVS_uuid = '%s' on the master, so creating \
           one."
          my_pvs_uuid ;
        let new_pvs_site =
          Client.PVS_site.introduce ~rpc ~session_id
            ~name_label:pvs_site.API.pVS_site_name_label
            ~name_description:pvs_site.API.pVS_site_name_description
            ~pVS_uuid:pvs_site.API.pVS_site_PVS_uuid
        in
        (* Update PVS servers *)
        let (_ : API.ref_PVS_server list) =
          List.map
            (fun pvs_server ->
              let pvs_record =
                Db.PVS_server.get_record ~__context ~self:pvs_server
              in
              Client.PVS_server.introduce ~rpc ~session_id
                ~addresses:pvs_record.API.pVS_server_addresses
                ~first_port:pvs_record.API.pVS_server_first_port
                ~last_port:pvs_record.API.pVS_server_last_port
                ~site:new_pvs_site
            )
            (Db.PVS_site.get_servers ~__context ~self:pvs_site_ref)
        in
        new_pvs_site
    | (pvs_site, _) :: _ ->
        pvs_site
  in
  new_pvs_site_ref

let create_or_get_pvs_cache_storage_on_master __context rpc session_id
    (_pcs_ref, pcs) : API.ref_PVS_cache_storage =
  let my_host_ref = pcs.API.pVS_cache_storage_host in
  let my_host = Db.Host.get_record ~__context ~self:my_host_ref in
  let new_host_ref =
    create_or_get_host_on_master __context rpc session_id (my_host_ref, my_host)
  in
  let my_sr_ref = pcs.API.pVS_cache_storage_SR in
  let my_sr = Db.SR.get_record ~__context ~self:my_sr_ref in
  let new_sr_ref =
    create_or_get_sr_on_master __context rpc session_id (my_sr_ref, my_sr)
  in
  let my_pvs_site_ref = pcs.API.pVS_cache_storage_site in
  let my_pvs_site = Db.PVS_site.get_record ~__context ~self:my_pvs_site_ref in
  let new_pvs_site_ref =
    create_or_get_pvs_site_on_master __context rpc session_id
      (my_pvs_site_ref, my_pvs_site)
  in
  let new_pvs_cache_storage_ref =
    let expr =
      Printf.sprintf "field \"site\"=\"%s\" and field \"host\"=\"%s\""
        (Ref.string_of new_pvs_site_ref)
        (Ref.string_of new_host_ref)
    in
    match
      Client.PVS_cache_storage.get_all_records_where ~rpc ~session_id ~expr
    with
    | [] ->
        Client.PVS_cache_storage.create ~rpc ~session_id ~host:new_host_ref
          ~sR:new_sr_ref ~site:new_pvs_site_ref
          ~size:pcs.API.pVS_cache_storage_size
    | (pvs_cache_storage, _) :: _ ->
        pvs_cache_storage
  in
  new_pvs_cache_storage_ref

let create_or_get_secret_on_master __context rpc session_id (_secret_ref, secret)
    : API.ref_secret =
  let my_uuid = secret.API.secret_uuid in
  let my_value = secret.API.secret_value in
  let new_secret_ref =
    try Client.Secret.get_by_uuid ~rpc ~session_id ~uuid:my_uuid
    with _ ->
      debug "Found no secret with uuid = '%s' on master, so creating one."
        my_uuid ;
      Client.Secret.introduce ~rpc ~session_id ~uuid:my_uuid ~value:my_value
        ~other_config:[]
  in
  new_secret_ref

let protect_exn f x =
  try Some (f x)
  with e ->
    Backtrace.is_important e ;
    debug "Ignoring exception: %s" (Printexc.to_string e) ;
    Debug.log_backtrace e (Backtrace.get e) ;
    None

(* Remark: the order in which we create the object in the distant database is not very important, as we have *)
(* an unique way to identify each object and thus we know if we need to create them or if it is already done *)
let update_non_vm_metadata ~__context ~rpc ~session_id =
  (* Update hosts *)
  let my_hosts = Db.Host.get_all_records ~__context in
  let (_ : API.ref_host option list) =
    List.map
      (protect_exn (create_or_get_host_on_master __context rpc session_id))
      my_hosts
  in
  (* Update SRs *)
  let my_srs = Db.SR.get_all_records ~__context in
  let (_ : API.ref_SR option list) =
    List.map
      (protect_exn (create_or_get_sr_on_master __context rpc session_id))
      my_srs
  in
  (* Update PBDs *)
  let my_pbds = Db.PBD.get_all_records ~__context in
  let (_ : API.ref_PBD option list) =
    List.map
      (protect_exn (create_or_get_pbd_on_master __context rpc session_id))
      my_pbds
  in
  (* Update VDIs *)
  let my_vdis = Db.VDI.get_all_records ~__context in
  let (_ : API.ref_VDI option list) =
    List.map
      (protect_exn (create_or_get_vdi_on_master __context rpc session_id))
      my_vdis
  in
  (* Update networks *)
  let my_networks = Db.Network.get_all_records ~__context in
  let (_ : API.ref_network option list) =
    List.map
      (protect_exn (create_or_get_network_on_master __context rpc session_id))
      my_networks
  in
  (* update PIFs *)
  let my_pifs =
    Db.PIF.get_records_where ~__context
      ~expr:(Eq (Field "physical", Literal "true"))
  in
  let (_ : API.ref_PIF option list) =
    List.map
      (protect_exn (create_or_get_pif_on_master __context rpc session_id))
      my_pifs
  in
  (* update Management VLAN *)
  let mgmt_pif =
    Xapi_host.get_management_interface ~__context
      ~host:(Helpers.get_localhost ~__context)
  in
  let my_vlan = Db.PIF.get_VLAN_master_of ~__context ~self:mgmt_pif in
  if my_vlan <> Ref.null then
    let my_vlan_record = Db.VLAN.get_record ~__context ~self:my_vlan in
    let (_ : API.ref_VLAN option) =
      protect_exn
        (create_or_get_vlan_on_master __context rpc session_id)
        (my_vlan, my_vlan_record)
    in
    (* update PVS sites *)
    let my_pvs_sites = Db.PVS_site.get_all_records ~__context in
    let (_ : API.ref_PVS_site option list) =
      List.map
        (protect_exn (create_or_get_pvs_site_on_master __context rpc session_id))
        my_pvs_sites
    in
    (* update PVS_cache_storage *)
    let my_pvs_cache_storages =
      Db.PVS_cache_storage.get_all_records ~__context
    in
    let (_ : API.ref_PVS_cache_storage option list) =
      List.map
        (protect_exn
           (create_or_get_pvs_cache_storage_on_master __context rpc session_id)
        )
        my_pvs_cache_storages
    in
    (* update Secrets *)
    let my_secrets = Db.Secret.get_all_records ~__context in
    let (_ : API.ref_secret option list) =
      List.map
        (protect_exn (create_or_get_secret_on_master __context rpc session_id))
        my_secrets
    in
    ()

let assert_pooling_licensed ~__context =
  if not (Pool_features.is_enabled ~__context Features.Pooling) then
    raise (Api_errors.Server_error (Api_errors.license_restriction, []))

let certificate_install ~__context ~name ~cert =
  let open Certificates in
  let certificate =
    let open Api_errors in
    match
      Gencertlib.Lib.validate_not_expired cert
        ~error_not_yet:ca_certificate_not_valid_yet
        ~error_expired:ca_certificate_expired
        ~error_invalid:ca_certificate_invalid
    with
    | Error e ->
        raise e
    | Ok x ->
        x
  in
  pool_install CA_Certificate ~__context ~name ~cert ;
  let (_ : API.ref_Certificate) =
    Db_util.add_cert ~__context ~type':(`ca name) certificate
  in
  ()

let install_ca_certificate = certificate_install

let uninstall_ca_certificate ~__context ~name ~force =
  let open Certificates in
  pool_uninstall CA_Certificate ~__context ~name ~force ;
  Db_util.remove_ca_cert_by_name ~__context name

let certificate_uninstall = uninstall_ca_certificate ~force:false

let certificate_list ~__context =
  let open Certificates in
  Db_util.get_ca_certs ~__context
  |> List.map @@ fun self -> Db.Certificate.get_name ~__context ~self

let crl_install = Certificates.(pool_install CRL)

let crl_uninstall = Certificates.(pool_uninstall CRL ~force:false)

let crl_list ~__context = Certificates.(local_list CRL)

let certificate_sync = Certificates.pool_sync

let join_common ~__context ~master_address ~master_username ~master_password
    ~force =
  assert_pooling_licensed ~__context ;
  let new_pool_secret = ref (SecretString.of_string "") in
  let unverified_rpc = rpc ~__context ~verify_cert:None master_address in
  let me = Helpers.get_localhost ~__context in
  let session_id =
    try
      Client.Session.login_with_password ~rpc:unverified_rpc
        ~uname:master_username ~pwd:master_password
        ~version:Datamodel_common.api_version_string
        ~originator:Xapi_version.xapi_user_agent
    with Http_client.Http_request_rejected _ | Http_client.Http_error _ ->
      raise
        (Api_errors.Server_error
           (Api_errors.pool_joining_host_service_failed, [])
        )
  in

  finally
    (fun () ->
      (* Note: this is where the license restrictions are checked on the other
         side. If we're trying to join a host that does not support pooling
         then an error will be thrown at this stage *)
      pre_join_checks ~__context ~rpc:unverified_rpc ~session_id ~force ;
      (* get hold of cluster secret - this is critical; if this fails whole pool join fails *)
      new_pool_secret :=
        Client.Pool.initial_auth ~rpc:unverified_rpc ~session_id ;

      (* Distribute the pool certificate so other members can connect to me
         and I can connect to them *)
      let my_uuid = Db.Host.get_uuid ~__context ~self:me in
      let my_certificate = Certificates.get_internal_server_certificate () in
      let pool_certs =
        Client.Pool.exchange_certificates_on_join ~rpc:unverified_rpc
          ~session_id ~uuid:my_uuid ~certificate:my_certificate
      in
      Cert_distrib.import_joining_pool_certs ~__context ~pool_certs
    )
    (fun () -> Client.Session.logout ~rpc:unverified_rpc ~session_id) ;

  (* Certificate exchange done, we must switch to verified pool connections as
     soon as possible *)
  let rpc =
    rpc ~__context ~verify_cert:(Stunnel_client.pool ()) master_address
  in
  let session_id =
    try
      Client.Session.login_with_password ~rpc ~uname:master_username
        ~pwd:master_password ~version:Datamodel_common.api_version_string
        ~originator:Xapi_version.xapi_user_agent
    with Http_client.Http_request_rejected _ | Http_client.Http_error _ ->
      raise
        (Api_errors.Server_error
           (Api_errors.pool_joining_host_service_failed, [])
        )
  in

  let remote_coordinator = get_master ~rpc ~session_id in
  (* If management is on a VLAN, then get the Pool master
     management network bridge before we logout the session *)
  let pool_master_bridge, mgmt_pif =
    let my_pif =
      Xapi_host.get_management_interface ~__context
        ~host:(Helpers.get_localhost ~__context)
    in
    if Db.PIF.get_VLAN_master_of ~__context ~self:my_pif <> Ref.null then
      let pif =
        Client.Host.get_management_interface ~rpc ~session_id
          ~host:remote_coordinator
      in
      let network = Client.PIF.get_network ~rpc ~session_id ~self:pif in
      (Some (Client.Network.get_bridge ~rpc ~session_id ~self:network), my_pif)
    else
      (None, my_pif)
  in
  finally
    (fun () ->
      (* Merge certificates used for trusting appliances, also known as ca
         certificates. At this point the names of certificates have been tested
         for uniqueness across pools, the name of the certificate is used to
         identify each certificate. *)
      let expr = {|field "type"="ca"|} in
      let module CertSet = Set.Make (String) in
      let get_name = function
        | _, {API.certificate_name; _} ->
            certificate_name
      in
      let remote_certs =
        Client.Certificate.get_all_records_where ~rpc ~session_id ~expr
      in
      let remote_names = List.map get_name remote_certs |> CertSet.of_list in
      let local_names =
        Db.Certificate.get_all_records_where ~__context ~expr
        |> List.map get_name
        |> CertSet.of_list
      in

      let from_pool = CertSet.(diff remote_names local_names) in
      let to_pool = CertSet.(diff local_names remote_names) in

      let remote_cert_refs =
        List.filter_map
          (function
            | ref, API.{certificate_name; _}
              when CertSet.mem certificate_name from_pool ->
                Some ref
            | _ ->
                None
            )
          remote_certs
      in

      let local_appliance_certs =
        Cert_distrib.collect_ca_certs ~__context
          ~names:(CertSet.to_seq to_pool |> List.of_seq)
      in
      let downloaded_certs =
        Client.Pool.exchange_ca_certificates_on_join ~rpc ~session_id
          ~import:local_appliance_certs ~export:remote_cert_refs
      in
      Cert_distrib.import_joining_pool_ca_certificates ~__context
        ~ca_certs:downloaded_certs ;

      (* get pool db from new master so I have a backup ready if we failover to me *)
      ( try
          Pool_db_backup.fetch_database_backup ~master_address
            ~pool_secret:!new_pool_secret ~force:None
        with e ->
          error "Failed fetching a database backup from the master: %s"
            (ExnHelper.string_of_exn e)
      ) ;
      (* Writes UEFI certificates of the pool we join to this host's disk. *)
      ( try
          let _uefi_certs =
            Client.Pool.get_uefi_certificates ~rpc ~session_id
              ~self:(get_pool ~rpc ~session_id)
          in
          Db.Pool.set_uefi_certificates ~__context
            ~self:(Helpers.get_pool ~__context)
            ~value:_uefi_certs ;
          let _custom_uefi_certs =
            Client.Pool.get_custom_uefi_certificates ~rpc ~session_id
              ~self:(get_pool ~rpc ~session_id)
          in
          Db.Pool.set_custom_uefi_certificates ~__context
            ~self:(Helpers.get_pool ~__context)
            ~value:_custom_uefi_certs ;
          Helpers.call_api_functions ~__context
            (fun local_rpc local_session_id ->
              Client.Host.write_uefi_certificates_to_disk ~rpc:local_rpc
                ~session_id:local_session_id
                ~host:(Helpers.get_localhost ~__context)
          )
        with e ->
          error
            "Unable to set the write the new pool certificates to the disk : %s"
            (ExnHelper.string_of_exn e)
      ) ;
      ( try
          let ssh_enabled_timeout =
            Client.Host.get_ssh_enabled_timeout ~rpc ~session_id
              ~self:remote_coordinator
          in
          let console_idle_timeout =
            Client.Host.get_console_idle_timeout ~rpc ~session_id
              ~self:remote_coordinator
          in
          let ssh_auto_mode =
            Client.Host.get_ssh_auto_mode ~rpc ~session_id
              ~self:remote_coordinator
          in
          Xapi_host.set_console_idle_timeout ~__context ~self:me
            ~value:console_idle_timeout ;
          Xapi_host.set_ssh_enabled_timeout ~__context ~self:me
            ~value:ssh_enabled_timeout ;
          Xapi_host.set_ssh_auto_mode ~__context ~self:me ~value:ssh_auto_mode ;
          let ssh_enabled =
            Client.Host.get_ssh_enabled ~rpc ~session_id
              ~self:remote_coordinator
          in
          (* As ssh_expiry will be updated by host.enable_ssh and host.disable_ssh,
             there is a corner case when the joiner's SSH state will not match SSH
             service state in its new coordinator exactly: if the joiner joins when
             SSH service has been enabled in the new coordinator, while not timed
             out yet, the joiner will start SSH service with timeout
             host.ssh_enabled_timeout, which means SSH service in the joiner will
             be disabled later than in the new coordinator. *)
          match ssh_enabled with
          | true ->
              Xapi_host.enable_ssh ~__context ~self:me
          | false ->
              Xapi_host.disable_ssh ~__context ~self:me
        with e ->
          error "Unable to configure SSH service on local host: %s"
            (ExnHelper.string_of_exn e)
      ) ;
      (* this is where we try and sync up as much state as we can
         with the master. This is "best effort" rather than
         critical; if we fail part way through this then we carry
         on with the join *)
      try
        update_non_vm_metadata ~__context ~rpc ~session_id ;
        ignore
          (Importexport.remote_metadata_export_import ~__context ~rpc
             ~session_id ~remote_address:master_address ~restore:true `All
          )
      with e ->
        debug "Error whilst importing db objects into master; aborted: %s"
          (Printexc.to_string e) ;
        warn
          "Error whilst importing db objects to master. The pool-join \
           operation will continue, but some of the slave's VMs may not be \
           available on the master."
    )
    (fun () -> Client.Session.logout ~rpc ~session_id) ;

  (* Attempt to unplug all our local storage. This is needed because
     when we restart as a slave, all the references will be wrong
     and these may have been cached by the storage layer. *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      List.iter
        (fun self ->
          Helpers.log_exn_continue
            (Printf.sprintf "Unplugging PBD %s" (Ref.string_of self))
            (fun () -> Client.PBD.unplug ~rpc ~session_id ~self)
            ()
        )
        (Db.Host.get_PBDs ~__context ~self:me)
  ) ;
  (* If management is on a VLAN, then we might need to create a vlan bridge with the same name as the Pool master is using *)
  ( match pool_master_bridge with
  | None ->
      ()
  | Some bridge ->
      let network = Db.PIF.get_network ~__context ~self:mgmt_pif in
      let mgmt_bridge = Db.Network.get_bridge ~__context ~self:network in
      if mgmt_bridge <> bridge then (
        debug "Changing management vlan bridge from=%s to=%s" mgmt_bridge bridge ;
        Db.Network.set_bridge ~__context ~self:network ~value:bridge ;
        Nm.bring_pif_up ~__context ~management_interface:true mgmt_pif ;
        Xapi_mgmt_iface.change bridge
          (Db.PIF.get_primary_address_type ~__context ~self:mgmt_pif)
      )
  ) ;
  (* Rewrite the pool secret on every host of the current pool, and restart all the agent as slave of the distant pool master. *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      List.iter
        (fun (host, _) ->
          Client.Host.update_pool_secret ~rpc ~session_id ~host
            ~pool_secret:!new_pool_secret ;
          Client.Host.update_master ~rpc ~session_id ~host ~master_address
        )
        (Db.Host.get_all_records ~__context)
  ) ;
  Xapi_hooks.pool_join_hook ~__context

let join ~__context ~master_address ~master_username ~master_password =
  join_common ~__context ~master_address ~master_username ~master_password
    ~force:false

let join_force ~__context ~master_address ~master_username ~master_password =
  join_common ~__context ~master_address ~master_username ~master_password
    ~force:true

let exchange_certificates_on_join ~__context ~uuid ~certificate :
    API.string_to_string_map =
  Xapi_pool_helpers.with_pool_operation ~__context
    ~op:`exchange_certificates_on_join ~doc:"Pool.exchange_certificates_on_join"
    ~self:(Helpers.get_pool ~__context)
  @@ fun () ->
  Cert_distrib.exchange_certificates_with_joiner ~__context ~uuid ~certificate

let exchange_ca_certificates_on_join ~__context ~import ~export :
    API.string_to_string_map =
  Xapi_pool_helpers.with_pool_operation ~__context
    ~op:`exchange_ca_certificates_on_join
    ~doc:"Pool.exchange_ca_certificates_on_join"
    ~self:(Helpers.get_pool ~__context)
  @@ fun () ->
  let export =
    List.map (fun self -> Db.Certificate.get_name ~__context ~self) export
  in
  Cert_distrib.exchange_ca_certificates_with_joiner ~__context ~import ~export

(* Assume that db backed up from master will be there and ready to go... *)
let emergency_transition_to_master ~__context =
  if Localdb.get Constants.ha_armed = "true" then
    raise (Api_errors.Server_error (Api_errors.ha_is_enabled, [])) ;
  Xapi_pool_transition.become_master ()

let emergency_reset_master ~__context ~master_address =
  if Localdb.get Constants.ha_armed = "true" then
    raise (Api_errors.Server_error (Api_errors.ha_is_enabled, [])) ;
  let master_address = Helpers.gethostbyname master_address in
  Xapi_pool_transition.become_another_masters_slave master_address

let recover_slaves ~__context =
  let hosts = Db.Host.get_all ~__context in
  let my_address =
    Db.Host.get_address ~__context ~self:!Xapi_globs.localhost_ref
  in
  let recovered_hosts = ref [] in
  let recover_slave hostref =
    if not (hostref = !Xapi_globs.localhost_ref) then
      try
        let local_fn = emergency_reset_master ~master_address:my_address in
        let remote_fn =
          Client.Pool.emergency_reset_master ~master_address:my_address
        in
        (* We have to use a new context here because the slave is currently doing a
           	     Task.get_name_label on real tasks, which will block on slaves that we're
           	     trying to recover. Get around this by creating a dummy task, for which
           	     the name-label bit is bypassed *)
        let newcontext = Context.make "emergency_reset_master" in
        Message_forwarding.do_op_on_localsession_nolivecheck ~local_fn
          ~__context:newcontext ~host:hostref ~remote_fn ;
        recovered_hosts := hostref :: !recovered_hosts
      with _ -> ()
  in
  List.iter recover_slave hosts ;
  !recovered_hosts

exception Cannot_eject_master

let no_exn f = try f () with _ -> ()

let unplug_pbds ~__context host =
  let pbds = Db.Host.get_PBDs ~__context ~self:host in
  let srs = List.map (fun self -> Db.PBD.get_SR ~__context ~self) pbds in
  let srs_to_delete =
    List.filter
      (fun self -> List.length (Db.SR.get_PBDs ~__context ~self) = 1)
      srs
  in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      List.iter (fun pbd -> Client.PBD.unplug ~rpc ~session_id ~self:pbd) pbds ;
      List.iter (fun sr -> Client.SR.forget ~rpc ~session_id ~sr) srs_to_delete
  )

(* This means eject me, since will have been forwarded from master  *)
let eject_self ~__context ~host =
  let open Xapi_database in
  (* If HA is enabled then refuse *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then
    raise (Api_errors.Server_error (Api_errors.ha_is_enabled, [])) ;
  if
    List.exists
      (fun (_, op) -> op = `apply_updates)
      (Db.Pool.get_current_operations ~__context ~self:pool)
  then
    raise Api_errors.(Server_error (not_supported_during_upgrade, [])) ;
  if Pool_role.is_master () then
    raise Cannot_eject_master
  else (* Fail the operation if any VMs are running here (except dom0) *)
    let my_vms_with_records =
      Db.VM.get_records_where ~__context
        ~expr:(Eq (Field "resident_on", Literal (Ref.string_of host)))
    in
    List.iter
      (fun (_, x) ->
        if
          (not
             (Helpers.is_domain_zero ~__context
                (Db.VM.get_by_uuid ~__context ~uuid:x.API.vM_uuid)
             )
          )
          && x.API.vM_power_state <> `Halted
        then (
          error "VM uuid %s not in Halted state and resident_on this host"
            x.API.vM_uuid ;
          raise
            (Api_errors.Server_error
               (Api_errors.operation_not_allowed, ["VM resident on host"])
            )
        )
      )
      my_vms_with_records ;
    (* all control domains resident on me should be destroyed once I leave the
       pool, therefore pick them out as follows: if they have a valid
       resident_on, the latter should be me; if they don't (e.g. they are
       halted), they should have disks on my local storage *)
    let vm_is_resident_on_host vm_rec host =
      Db.is_valid_ref __context vm_rec.API.vM_resident_on
      && vm_rec.API.vM_resident_on = host
    in
    let vm_has_disks_on_local_sr_of_host vm_rec host =
      let open! Helpers in
      let is_sr_local x =
        try not (is_sr_shared ~__context ~self:x)
        with Db_exn.DBCache_NotFound _ -> false
      in
      let host_has_sr x =
        check_sr_exists_for_host ~__context ~self:x ~host <> None
      in
      vm_rec.API.vM_VBDs
      |> List.filter_map
           (ignore_invalid_ref (fun x -> Db.VBD.get_VDI ~__context ~self:x))
      |> List.filter_map
           (ignore_invalid_ref (fun x -> Db.VDI.get_SR ~__context ~self:x))
      |> List.exists (fun x -> is_sr_local x && host_has_sr x)
    in
    let is_obsolete_control_domain (_, vm_rec) =
      vm_rec.API.vM_is_control_domain
      && (vm_is_resident_on_host vm_rec host
         || vm_has_disks_on_local_sr_of_host vm_rec host
         )
    in
    let control_domains_to_destroy =
      List.filter is_obsolete_control_domain (Db.VM.get_all_records ~__context)
    in
    debug "Pool.eject: unplugging PBDs" ;
    (* unplug all my PBDs; will deliberately fail if any unplugs fail *)
    unplug_pbds ~__context host ;
    Xapi_clustering.find_cluster_host ~__context ~host
    |> Option.iter (fun cluster_host ->
           debug "Pool.eject: leaving cluster" ;
           (* PBDs need to be unplugged first for this to succeed *)
           Helpers.call_api_functions ~__context (fun rpc session_id ->
               Client.Cluster_host.destroy ~rpc ~session_id ~self:cluster_host
           )
       ) ;
    debug "Pool.eject: disabling external authentication in slave-to-be-ejected" ;
    (* disable the external authentication of this slave being ejected *)
    (* this call will return an exception if something goes wrong *)
    Xapi_host.disable_external_auth_common ~during_pool_eject:true ~__context
      ~host ~config:[] () ;

    (* FIXME: in the future, we should send the windows AD admin/pass here *)
    (* in order to remove the slave from the AD database during pool-eject *)

    (* CA-293085 - shut down xapi-nbd now rather than as part of reboot. It
     * tries to talk to xapi during its own shutdown and when that's done
     * after the pool eject logic has finished it fails and enters a 90s
     * retry loop. *)
    ( try
        debug "Shutting down xapi-nbd" ;
        ignore (Helpers.call_script !Xapi_globs.systemctl ["stop"; "xapi-nbd"])
      with e ->
        warn "Caught %s while shutting down xapi-nbd. Ignoring"
          (Printexc.to_string e)
    ) ;
    debug "Pool.eject: rewrite networking first-boot files" ;
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
        let tagged_pif =
          Db.VLAN.get_tagged_PIF ~__context ~self:pif.API.pIF_VLAN_master_of
        in
        let bond_master =
          Db.PIF.get_bond_master_of ~__context ~self:tagged_pif
        in
        if bond_master <> [] then
          let bond = List.hd bond_master in
          let primary_slave = Db.Bond.get_primary_slave ~__context ~self:bond in
          Db.PIF.get_device ~__context ~self:primary_slave
        else
          pif.API.pIF_device
      else
        pif.API.pIF_device
    in
    let mode =
      match pif.API.pIF_ip_configuration_mode with
      | `None ->
          "none"
      | `DHCP ->
          "dhcp"
      | `Static ->
          "static"
    in
    let mode_v6 =
      Record_util.ipv6_configuration_mode_to_string
        pif.API.pIF_ipv6_configuration_mode
      |> String.uncapitalize_ascii
    in
    let write_first_boot_management_interface_configuration_file () =
      (* During firstboot, now inventory has an empty MANAGEMENT_INTERFACE *)
      let bridge = "" in
      Xapi_inventory.update Xapi_inventory._management_interface bridge ;
      let primary_address_type =
        Db.PIF.get_primary_address_type ~__context ~self:management_pif
      in
      Xapi_inventory.update Xapi_inventory._management_address_type
        (Record_util.primary_address_type_to_string primary_address_type) ;
      let sprintf = Printf.sprintf in
      (* If the management_interface exists on a vlan, write the vlan id into management.conf *)
      let vlan_id = Int64.to_int pif.API.pIF_VLAN in
      let config_base =
        [
          sprintf "LABEL='%s'" management_device
        ; sprintf "MODE='%s'" mode
        ; sprintf "MODEV6='%s'" mode_v6
        ]
      in
      let config_static =
        if mode <> "static" then
          []
        else
          [
            sprintf "IP='%s'" pif.API.pIF_IP
          ; sprintf "NETMASK='%s'" pif.API.pIF_netmask
          ; sprintf "GATEWAY='%s'" pif.API.pIF_gateway
          ]
      in
      let configv6_static =
        if mode_v6 <> "static" then
          []
        else
          [
            sprintf "IPv6='%s'" (String.concat "," pif.API.pIF_IPv6)
          ; sprintf "IPv6_GATEWAY='%s'" pif.API.pIF_ipv6_gateway
          ]
      in
      let config_dns =
        if mode = "static" || mode_v6 = "static" then
          [sprintf "DNS='%s'" pif.API.pIF_DNS]
        else
          []
      in
      let config_vlan =
        if vlan_id = -1 then
          []
        else
          [sprintf "VLAN='%d'" vlan_id]
      in
      let configuration_file =
        List.concat
          [config_base; config_static; configv6_static; config_dns; config_vlan]
        |> String.concat "\n"
      in
      Unixext.write_string_to_file
        (Xapi_globs.first_boot_dir ^ "data/management.conf")
        configuration_file
    in
    write_first_boot_management_interface_configuration_file () ;
    Net.reset_state () ;
    Xapi_inventory.update Xapi_inventory._current_interfaces "" ;
    (* Destroy my control domains, since you can't do this from the API [operation not allowed] *)
    ( try
        List.iter
          (fun x -> Db.VM.destroy ~__context ~self:(fst x))
          control_domains_to_destroy
      with _ -> ()
    ) ;
    ( try
        (* Restore console idle timeout *)
        Xapi_host.set_console_idle_timeout ~__context ~self:host
          ~value:Constants.default_console_idle_timeout ;
        (* Restore SSH service to default state *)
        Xapi_host.set_ssh_enabled_timeout ~__context ~self:host
          ~value:Constants.default_ssh_enabled_timeout ;
        Xapi_host.set_ssh_auto_mode ~__context ~self:host
          ~value:!Xapi_globs.ssh_auto_mode_default ;
        match Constants.default_ssh_enabled with
        | true ->
            Xapi_host.enable_ssh ~__context ~self:host
        | false ->
            Xapi_host.disable_ssh ~__context ~self:host
      with e ->
        warn "Caught %s while restoring ssh service. Ignoring"
          (Printexc.to_string e)
    ) ;

    debug "Pool.eject: setting our role to be master" ;
    Xapi_pool_transition.set_role Pool_role.Master ;
    debug "Pool.eject: forgetting pool secret" ;
    Unixext.unlink_safe !Xapi_globs.pool_secret_path ;
    (* forget current pool secret *)
    (* delete backup databases and any temporary restore databases *)
    Unixext.unlink_safe Xapi_globs.backup_db_xml ;
    Unixext.unlink_safe Xapi_globs.db_temporary_restore_path ;
    Unixext.unlink_safe Db_globs.ha_metadata_db ;
    Unixext.unlink_safe Db_globs.gen_metadata_db ;
    (* If we've got local storage, remove it *)
    if Helpers.local_storage_exists () then (
      ignore
        (Forkhelpers.execute_command_get_output "/bin/rm"
           ["-rf"; Xapi_globs.xapi_blob_location]
        ) ;
      Unixext.mkdir_safe Xapi_globs.xapi_blob_location 0o700
    ) ;
    (* delete /local/ databases specified in the db.conf, so they get recreated on restart.
       		 * We must leave any remote database alone because these are owned by the pool and
       		 * not by this node. *)
    (* get the slave backup lock so we know no more backups are going to be taken --
       		 * we keep this lock till the bitter end, where we restart below ;)
    *)
    Mutex.lock Pool_db_backup.slave_backup_m ;
    finally
      (fun () ->
        let dbs = Parse_db_conf.parse_db_conf !Db_globs.db_conf_path in
        (* We need to delete all local dbs but leave remote ones alone *)
        let local =
          List.filter (fun db -> not db.Parse_db_conf.is_on_remote_storage) dbs
        in
        List.iter Unixext.unlink_safe
          (List.map (fun db -> db.Parse_db_conf.path) local) ;
        List.iter Unixext.unlink_safe
          (List.map Parse_db_conf.generation_filename local) ;
        (* remove any shared databases from my db.conf *)
        (* XXX: on OEM edition the db.conf is rebuilt on every boot *)
        Parse_db_conf.write_db_conf local ;
        (* Forget anything we know about configured remote databases: this prevents
           			any initscript reminding us about them after reboot *)
        Helpers.log_exn_continue
          (Printf.sprintf "Moving remote database file to backup: %s"
             !Xapi_globs.remote_db_conf_fragment_path
          )
          (fun () ->
            Unix.rename
              !Xapi_globs.remote_db_conf_fragment_path
              (!Xapi_globs.remote_db_conf_fragment_path ^ ".bak")
          )
          () ;
        (* Reset the domain 0 network interface naming configuration
           			 * back to a fresh-install state for the currently-installed
           			 * hardware.
        *)
        ignore
          (Forkhelpers.execute_command_get_output
             "/etc/sysconfig/network-scripts/interface-rename.py"
             ["--reset-to-install"]
          )
      )
      (fun () -> Xapi_fuse.light_fuse_and_reboot_after_eject ()) ;
    Xapi_hooks.pool_eject_hook ~__context

(** eject [host] from the pool. This code is run on all hosts in the
pool but only [host] will eject itself. *)
let eject ~__context ~host =
  let local = Helpers.get_localhost ~__context in
  let master = Helpers.get_master ~__context in
  match (host = local, local = master) with
  | true, false ->
      eject_self ~__context ~host
  | false, false ->
      Certificates_sync.eject_certs_from_fs_for ~__context host
  | false, true ->
      let certs = Certificates_sync.host_certs_of ~__context host in
      info "about to eject certs of host %s on the master (1/2)"
        (Ref.string_of host) ;
      Certificates_sync.eject_certs_from_fs_for ~__context host ;
      Certificates_sync.eject_certs_from_db ~__context certs ;
      debug "Pool.eject: deleting Host record" ;
      Db.Host.destroy ~__context ~self:host ;
      info "ejected certs of host %s on the master (2/2)" (Ref.string_of host) ;
      (* Update pool_cpuinfo, in case this host had unique or lacked common CPU features *)
      Create_misc.create_pool_cpuinfo ~__context ;
      (* Update pool features, in case this host had a different license to the
       * rest of the pool. *)
      Pool_features_helpers.update_pool_features ~__context
  | true, true ->
      raise Cannot_eject_master

(* Prohibit parallel flushes since they're so expensive *)
let sync_m = Mutex.create ()

open Xapi_database.Db_cache_types

let sync_database ~__context =
  with_lock sync_m (fun () ->
      (* If HA is enabled I'll first try to flush to the LUN *)
      let pool = Helpers.get_pool ~__context in
      let flushed_to_vdi =
        Db.Pool.get_ha_enabled ~__context ~self:pool
        && Xapi_database.Db_lock.with_lock (fun () ->
               Xha_metadata_vdi.flush_database ~__context Xapi_ha.ha_redo_log
           )
      in
      if flushed_to_vdi then
        debug "flushed database to metadata VDI: assuming this is sufficient."
      else (
        debug "flushing database to all online nodes" ;
        let generation =
          Xapi_database.Db_lock.with_lock (fun () ->
              Manifest.generation
                (Database.manifest
                   (Xapi_database.Db_ref.get_database
                      (Context.database_of __context)
                   )
                )
          )
        in
        Xapi_stdext_threads.Threadext.thread_iter
          (fun host ->
            Helpers.call_api_functions ~__context (fun rpc session_id ->
                Client.Host.request_backup ~rpc ~session_id ~host ~generation
                  ~force:true
            )
          )
          (Db.Host.get_all ~__context)
      )
  )

(* This also means me, since call will have been forwarded from the current master *)
let designate_new_master ~__context ~host:_ =
  if not (Pool_role.is_master ()) then (
    let pool = Helpers.get_pool ~__context in
    if Db.Pool.get_ha_enabled ~__context ~self:pool then
      raise (Api_errors.Server_error (Api_errors.ha_is_enabled, [])) ;
    Db.Pool.set_last_update_sync ~__context ~self:pool ~value:Date.epoch ;
    (* Only the master can sync the *current* database; only the master
       knows the current generation count etc. *)
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.Pool.sync_database ~rpc ~session_id
    ) ;
    let all_hosts = Db.Host.get_all ~__context in
    (* We make no attempt to demand a quorum or anything. *)
    let addresses =
      List.map (fun self -> Db.Host.get_address ~__context ~self) all_hosts
    in
    let my_address =
      Db.Host.get_address ~__context ~self:(Helpers.get_localhost ~__context)
    in
    let peers = List.filter (fun x -> x <> my_address) addresses in
    Xapi_pool_transition.attempt_two_phase_commit_of_new_master ~__context true
      peers my_address
  )

let management_reconfigure ~__context ~network =
  (* Disallow if HA is enabled *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then
    raise (Api_errors.Server_error (Api_errors.ha_is_enabled, [])) ;
  (* Create a hash table for hosts with pifs for the network *)
  let pifs_on_network = Db.Network.get_PIFs ~__context ~self:network in
  let hosts_with_pifs = Hashtbl.create 16 in
  List.iter
    (fun self ->
      let host = Db.PIF.get_host ~__context ~self in
      Hashtbl.add hosts_with_pifs host self
    )
    pifs_on_network ;
  (* All Hosts must have associated PIF on the network *)
  let all_hosts = Db.Host.get_all ~__context in
  List.iter
    (fun host ->
      if not (Hashtbl.mem hosts_with_pifs host) then
        raise
          (Api_errors.Server_error
             ( Api_errors.pif_not_present
             , [Ref.string_of host; Ref.string_of network]
             )
          )
    )
    all_hosts ;
  let address_type =
    Db.PIF.get_primary_address_type ~__context ~self:(List.hd pifs_on_network)
  in
  List.iter
    (fun self ->
      let primary_address_type =
        Db.PIF.get_primary_address_type ~__context ~self
      in
      if primary_address_type <> address_type then
        raise
          (Api_errors.Server_error
             ( Api_errors.pif_incompatible_primary_address_type
             , [Ref.string_of self]
             )
          ) ;
      Xapi_pif.assert_usable_for_management ~__context ~primary_address_type
        ~self
    )
    pifs_on_network ;
  (* Perform Host.management_reconfigure on slaves first and last on master *)
  let f ~rpc ~session_id ~host =
    Client.Host.management_reconfigure ~rpc ~session_id
      ~pif:(Hashtbl.find hosts_with_pifs host)
  in
  Xapi_pool_helpers.call_fn_on_slaves_then_master ~__context f ;
  (* Perform Pool.recover_slaves *)
  let hosts_recovered =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.Pool.recover_slaves ~rpc ~session_id
    )
  in
  List.iter
    (fun host ->
      debug "Host recovered=%s" (Db.Host.get_uuid ~__context ~self:host)
    )
    hosts_recovered

let initial_auth ~__context = Xapi_globs.pool_secret ()

(** This call is used during master startup so we should check to see whether we need to re-establish our database
    connection and resynchronise lost database state i.e. state which is non-persistent or reverted over a master crash *)
let is_slave ~__context ~host:_ =
  let is_slave = not (Pool_role.is_master ()) in
  info "Pool.is_slave call received (I'm a %s)"
    (if is_slave then "slave" else "master") ;
  debug
    "About to kick the database connection to make sure it's still working..." ;
  let (_ : bool) =
    Scheduler.PipeDelay.signal Xapi_database.Master_connection.delay ;
    Db.is_valid_ref __context
      (Ref.of_string
         "Pool.is_slave checking to see if the database connection is up"
      )
  in
  is_slave

let hello ~__context ~host_uuid ~host_address =
  let host_exists =
    try Some (Db.Host.get_by_uuid ~__context ~uuid:host_uuid) with _ -> None
  in
  match host_exists with
  | None ->
      `unknown_host
  | Some host_ref -> (
    try
      let slave_current_address =
        Db.Host.get_address ~__context ~self:host_ref
      in
      if host_address <> slave_current_address then (
        (* update slave address in master db because we know its changed *)
        Db.Host.set_address ~__context ~self:host_ref ~value:host_address ;
        (* and refresh console URLs to reflect this change of address *)
        Dbsync_master.refresh_console_urls ~__context
      ) ;
      let local_fn = is_slave ~host:host_ref in
      let remote_fn = Client.Pool.is_slave ~host:host_ref in
      (* Nb. next call is purely there to establish that we can talk back to the host that initiated this call *)
      (* We don't care about the return type, only that no exception is raised while talking to it *)
      ( try
          let _ : bool =
            Server_helpers.exec_with_subtask ~__context "pool.hello.is_slave"
              (fun ~__context ->
                Message_forwarding.do_op_on_nolivecheck_no_retry ~local_fn
                  ~__context ~host:host_ref ~remote_fn
            )
          in
          ()
        with
      | Api_errors.Server_error (code, ["pool.is_slave"; "1"; "2"]) as e
        when code = Api_errors.message_parameter_count_mismatch ->
          debug "Caught %s: this host is a Rio box" (ExnHelper.string_of_exn e)
      | Api_errors.Server_error (code, _) as e
        when code = Api_errors.host_still_booting ->
          debug "Caught %s: this host is a Miami box" (ExnHelper.string_of_exn e)
      ) ;
      (* Set the host to disabled initially: when it has finished initialising and is ready to
         	   host VMs it will mark itself as enabled again. *)
      info "Host.enabled: setting host %s (%s) to disabled"
        (Ref.string_of host_ref)
        (Db.Host.get_hostname ~__context ~self:host_ref) ;
      Db.Host.set_enabled ~__context ~self:host_ref ~value:false ;
      let pool = Helpers.get_pool ~__context in
      if not (Db.Pool.get_ha_enabled ~__context ~self:pool) then (
        debug "Host_metrics.live: setting host %s (%s) to alive"
          (Ref.string_of host_ref)
          (Db.Host.get_hostname ~__context ~self:host_ref) ;
        let metrics = Db.Host.get_metrics ~__context ~self:host_ref in
        Db.Host_metrics.set_live ~__context ~self:metrics ~value:true
      ) ;
      (* Cancel tasks on behalf of slave *)
      debug "Hello message from slave OK: cancelling tasks on behalf of slave" ;
      Cancel_tasks.cancel_tasks_on_host ~__context ~host_opt:(Some host_ref) ;
      (* Make sure we mark this host as live again *)
      with_lock Xapi_globs.hosts_which_are_shutting_down_m (fun () ->
          Xapi_globs.hosts_which_are_shutting_down :=
            List.filter
              (fun x -> x <> host_ref)
              !Xapi_globs.hosts_which_are_shutting_down
      ) ;
      (* Update the heartbeat timestamp for this host so we don't mark it as
         	   offline in the next db_gc *)
      let (_ : (string * string) list) =
        Db_gc.tickle_heartbeat ~__context host_ref []
      in
      `ok
    with e ->
      debug "Caught exception: %s" (ExnHelper.string_of_exn e) ;
      `cannot_talk_back
  )

(* !!! THIS CALL IS FUNDAMENTALLY BROKEN wrt bonds -- see CA-22613; it should no longer be used.
   I have pulled together the function definitions specific to create_VLAN and moved them into create_VLAN definition
   itself. create_VLAN_from_PIF (below) is based on the code for create_VLAN; since create_VLAN is now dead (only here
   so we don't break existing API clients) there is no need to factor the commonality between these 2 fns.
*)

(** Create PIF on each pool host for specified VLAN/device pair.
    On error, destroy all of the PIFs that have already been created. *)
let create_VLAN ~__context ~device ~network ~vLAN =
  (* Destroy the list of PIFs - try destroying them with the client API, and if
     the host is offline, just destroy the record *)
  let safe_destroy_PIFs ~__context pifs =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        List.iter
          (fun pif ->
            (* This call destroys the metrics too *)
            try Client.PIF.destroy ~rpc ~session_id ~self:pif with
            | Api_errors.Server_error (a, _) ->
                if a = Api_errors.host_offline then
                  Db.PIF.destroy ~__context ~self:pif
                else
                  (* If theres any other error, leave the PIF to be destroyed
                     			 manually. We certainly don't want the Db to be out of
                     			 sync with reality *)
                  ()
            | _ ->
                ()
          )
          pifs
    )
  in
  let created = ref [] in
  let hosts = Db.Host.get_all ~__context in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let pifs =
        List.map
          (fun host ->
            try
              let pif =
                Client.PIF.create_VLAN ~rpc ~session_id ~device ~network ~host
                  ~vLAN
              in
              created := pif :: !created ;
              pif
            with e ->
              (* Any error and we'll clean up and exit *)
              safe_destroy_PIFs ~__context !created ;
              raise e
          )
          hosts
      in
      (* CA-22381: best-effort plug of the newly-created VLAN PIFs. Note if any of these calls fail
         	  then nothing is rolled-back and the system will be left with some unplugged VLAN PIFs, which may
         	  confuse the HA agility calculation (but nothing else since everything else can plug on demand) *)
      List.iter
        (fun pif ->
          Helpers.log_exn_continue
            (Printf.sprintf "Plugging VLAN PIF %s" (Ref.string_of pif))
            (fun () -> Client.PIF.plug ~rpc ~session_id ~self:pif)
            ()
        )
        pifs ;
      pifs
  )

(* This call always runs on the master, client calls are spawned off and forwarded to slaves. By taking a PIF
   explicitly instead of a device name we ensure that this call works for creating VLANs on bonds across pools..
*)
let create_VLAN_from_PIF ~__context ~pif ~network ~vLAN =
  (* Destroy the list of VLANs, best-effort *)
  let safe_destroy_VLANs ~__context vlans =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        List.iter
          (fun vlan ->
            try Client.VLAN.destroy ~rpc ~session_id ~self:vlan with _ -> ()
          )
          vlans
    )
  in
  (* Read the network that the pif is attached to; get the list of all pifs on that network
     	   -- that'll be the pif for each host that we want to make the vlan on. Then go and make
     	   the vlan on all these pifs. Then attempt to do a best-effort plug of the newly created pifs
     	   in order to satisfy ca-22381 *)
  let network_to_get_pifs_from = Db.PIF.get_network ~__context ~self:pif in
  let pifs_on_network =
    Db.Network.get_PIFs ~__context ~self:network_to_get_pifs_from
  in
  let is_host_live pif =
    let h = Db.PIF.get_host ~__context ~self:pif in
    let host_metric = Db.Host.get_metrics ~__context ~self:h in
    Db.Host_metrics.get_live ~__context ~self:host_metric
  in
  let pifs_on_live_hosts = List.filter is_host_live pifs_on_network in
  (* Keep track of what we've created *)
  let created = ref [] in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let vlans =
        List.map
          (fun pif ->
            try
              let vlan =
                Client.VLAN.create ~rpc ~session_id ~tagged_PIF:pif ~tag:vLAN
                  ~network
              in
              created := vlan :: !created ;
              vlan
            with e ->
              (* Any error and we'll clean up and exit *)
              safe_destroy_VLANs ~__context !created ;
              raise e
          )
          pifs_on_live_hosts
      in
      let vlan_pifs =
        List.map
          (fun vlan -> Db.VLAN.get_untagged_PIF ~__context ~self:vlan)
          vlans
      in
      (* CA-22381: best-effort plug of the newly-created VLAN PIFs. Note if any of these calls fail
         			   then nothing is rolled-back and the system will be left with some unplugged VLAN PIFs, which may
         			   confuse the HA agility calculation (but nothing else since everything else can plug on demand) *)
      List.iter
        (fun pif ->
          Helpers.log_exn_continue
            (Printf.sprintf "Plugging VLAN PIF %s" (Ref.string_of pif))
            (fun () -> Client.PIF.plug ~rpc ~session_id ~self:pif)
            ()
        )
        vlan_pifs ;
      vlan_pifs
  )

(*
  Dbsync_slave.create_physical_networks ~__context phydevs dev_to_mac dev_to_mtu slave_host
*)
(* Let's only process one enable/disable at a time. I would have used an allowed_operation for this but
   it would involve a datamodel change and it's too late for Orlando. *)
let enable_disable_m = Mutex.create ()

let enable_ha ~__context ~heartbeat_srs ~configuration =
  if not (Helpers.pool_has_different_host_platform_versions ~__context) then
    with_lock enable_disable_m (fun () ->
        Xapi_ha.enable __context heartbeat_srs configuration
    )
  else
    raise Api_errors.(Server_error (not_supported_during_upgrade, []))

let disable_ha ~__context =
  with_lock enable_disable_m (fun () -> Xapi_ha.disable __context)

let ha_prevent_restarts_for ~__context ~seconds =
  Xapi_ha.ha_prevent_restarts_for __context seconds

let ha_failover_plan_exists ~__context ~n =
  let n = Int64.to_int n in
  let all_protected_vms = Xapi_ha_vm_failover.all_protected_vms ~__context in
  match
    Xapi_ha_vm_failover.plan_for_n_failures ~__context ~all_protected_vms n
  with
  | Xapi_ha_vm_failover.Plan_exists_for_all_VMs ->
      info
        "HA failover plan exists for all protected VMs for up to %d host \
         failures"
        n ;
      true
  | Xapi_ha_vm_failover.Plan_exists_excluding_non_agile_VMs ->
      info
        "HA failover plan exists for all protected VMs, excluding some \
         non-agile VMs, for up to %d host failures"
        n ;
      false (* might define this as true later *)
  | Xapi_ha_vm_failover.No_plan_exists ->
      info "No HA failover plan exists for %d host failures" n ;
      false

let ha_compute_max_host_failures_to_tolerate ~__context =
  let n =
    Xapi_ha_vm_failover.compute_max_host_failures_to_tolerate ~__context ()
  in
  (* Update the Pool with this information if HA is currently enabled *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then (
    let n' = Db.Pool.get_ha_host_failures_to_tolerate ~__context ~self:pool in
    let overcommitted = n' > n in
    if Db.Pool.get_ha_overcommitted ~__context ~self:pool <> overcommitted then
      Db.Pool.set_ha_overcommitted ~__context ~self:pool ~value:overcommitted ;
    let current_plan_for =
      Db.Pool.get_ha_plan_exists_for ~__context ~self:pool
    in
    if current_plan_for <> n then (
      Db.Pool.set_ha_plan_exists_for ~__context ~self:pool ~value:(min n' n) ;
      if n < current_plan_for then
        Xapi_alert.add ~msg:Api_messages.ha_pool_drop_in_plan_exists_for
          ~cls:`Pool
          ~obj_uuid:(Db.Pool.get_uuid ~__context ~self:pool)
          ~body:(Int64.to_string n)
    )
  ) ;
  n

let ha_compute_hypothetical_max_host_failures_to_tolerate ~__context
    ~configuration =
  (* Check the restart priorities all look valid *)
  List.iter
    (fun (_, pri) ->
      if not (List.mem pri Constants.ha_valid_restart_priorities) then
        raise
          (Api_errors.Server_error
             (Api_errors.invalid_value, ["ha_restart_priority"; pri])
          )
    )
    configuration ;
  let protected_vms =
    List.map fst
      (List.filter
         (fun (_, priority) -> Helpers.vm_should_always_run true priority)
         configuration
      )
  in
  let protected_vms =
    List.map
      (fun vm -> (vm, Db.VM.get_record ~__context ~self:vm))
      protected_vms
  in
  Xapi_ha_vm_failover.compute_max_host_failures_to_tolerate ~__context
    ~protected_vms ()

let ha_compute_vm_failover_plan ~__context ~failed_hosts ~failed_vms =
  let vms =
    List.map (fun vm -> (vm, Db.VM.get_record ~__context ~self:vm)) failed_vms
  in
  let all_hosts = Db.Host.get_all ~__context in
  let currently_live_hosts =
    List.filter
      (fun h ->
        try
          Db.Host_metrics.get_live ~__context
            ~self:(Db.Host.get_metrics ~__context ~self:h)
        with _ -> false
      )
      all_hosts
  in
  let live_hosts =
    List.filter
      (fun host -> not (List.mem host failed_hosts))
      currently_live_hosts
  in
  debug "using live_hosts = [ %s ]"
    (String.concat "; " (List.map Ref.string_of live_hosts)) ;
  (* All failed_vms must be agile *)
  let errors =
    List.concat_map
      (fun self ->
        try
          Agility.vm_assert_agile ~__context ~self ;
          [(self, [("error_code", Api_errors.host_not_enough_free_memory)])]
          (* default *)
        with Api_errors.Server_error (code, _) ->
          [(self, [("error_code", code)])]
      )
      failed_vms
  in
  let plan =
    List.map
      (fun (vm, host) -> (vm, [("host", Ref.string_of host)]))
      (Xapi_ha_vm_failover.compute_evacuation_plan ~__context
         (List.length all_hosts) live_hosts vms
      )
  in
  List.filter (fun (vm, _) -> not (List.mem_assoc vm plan)) errors @ plan

let create_new_blob ~__context ~pool ~name ~mime_type ~public =
  let blob = Xapi_blob.create ~__context ~mime_type ~public in
  Db.Pool.add_to_blobs ~__context ~self:pool ~key:name ~value:blob ;
  blob

let set_ha_host_failures_to_tolerate ~__context ~self ~value =
  if value < 0L then
    raise
      (Api_errors.Server_error
         ( Api_errors.invalid_value
         , ["ha_host_failures_to_tolerate"; Int64.to_string value]
         )
      ) ;
  (* Don't block changes if we have no plan at all *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_plan_exists_for ~__context ~self:pool > 0L then
    Xapi_ha_vm_failover.assert_nfailures_change_preserves_ha_plan ~__context
      (Int64.to_int value) ;
  Db.Pool.set_ha_host_failures_to_tolerate ~__context ~self ~value ;
  let (_ : bool) = Xapi_ha_vm_failover.update_pool_status ~__context () in
  ()

let ha_schedule_plan_recomputation ~__context =
  Xapi_ha.Monitor.plan_out_of_date := true

let call_fn_on_host ~__context f host =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      try f ~rpc ~session_id ~host
      with e ->
        warn "Exception raised while performing operation on host %s error: %s"
          (Ref.string_of host)
          (ExnHelper.string_of_exn e) ;
        raise e
  )

let enable_binary_storage ~__context =
  Xapi_pool_helpers.call_fn_on_slaves_then_master ~__context
    Client.Host.enable_binary_storage

let disable_binary_storage ~__context =
  Xapi_pool_helpers.call_fn_on_slaves_then_master ~__context
    Client.Host.disable_binary_storage

let initialize_wlb ~__context ~wlb_url ~wlb_username ~wlb_password
    ~xenserver_username ~xenserver_password =
  init_wlb ~__context ~wlb_url ~wlb_username ~wlb_password ~xenserver_username
    ~xenserver_password

let deconfigure_wlb ~__context = decon_wlb ~__context

let send_wlb_configuration ~__context ~config =
  send_wlb_config ~__context ~config

let retrieve_wlb_configuration ~__context = retrieve_wlb_config ~__context

let retrieve_wlb_recommendations ~__context = get_opt_recommendations ~__context

let send_test_post = Remote_requests.send_test_post

(* destroy all subject not validated in external authentication *)
let revalidate_subjects ~__context =
  let revalidate_subject ~__context ~self =
    let subj_id = Db.Subject.get_subject_identifier ~__context ~self in
    debug "Revalidating subject %s" subj_id ;
    try
      Xapi_subject.query_subject_information_from_AD ~__context subj_id
      |> ignore
    with Not_found ->
      debug "Destroying subject %s" subj_id ;
      Xapi_subject.destroy ~__context ~self
  in
  let subjects_in_db = Db.Subject.get_all ~__context in
  List.iter
    (fun subj -> revalidate_subject ~__context ~self:subj)
    subjects_in_db

(* CP-719: Enables external auth/directory service across a whole pool; *)
(* calling Host.enable_external_auth with the specified parameters in turn on each of the hosts in the pool
    * The call fails immediately if any of the pool hosts already have external auth enabled (must disable first)
    * If a call to a single host to enable external auth fails, then Pool.enable_external_auth fails, and there is
      a best-effort attempt to disable any hosts who had their external auth successfully enabled before the failure occured
*)
let enable_external_auth ~__context ~pool:_ ~config ~service_name ~auth_type =
  (* CP-825: Serialize execution of pool-enable-extauth and pool-disable-extauth *)
  (* enabling/disabling the pool's extauth at the same time could produce inconsistent states for extauth in each host of the pool *)
  with_lock Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->
      (* the first element in the hosts list needs to be the pool's master, because we *)
      (* always want to update first the master's record due to homogeneity checks in CA-24856 *)
      let hosts = Xapi_pool_helpers.get_master_slaves_list ~__context in
      (* 1. verifies if any of the pool hosts already have external auth enabled, and fails if so *)
      (* this step isn't strictly necessary, since we will anyway fail in (2) if that is the case, but *)
      (* it avoids unnecessary network roundtrips in the pool *)
      try
        let is_external_auth_enabled host =
          Db.Host.get_external_auth_type ~__context ~self:host <> ""
        in
        let host = List.find is_external_auth_enabled hosts in
        let host_name_label = Db.Host.get_name_label ~__context ~self:host in
        let msg =
          "external authentication service in host "
          ^ host_name_label
          ^ " is already enabled"
        in
        debug
          "Failed to enable external authentication type %s for service name \
           %s in pool: %s"
          auth_type service_name msg ;
        raise
          (Api_errors.Server_error
             (Api_errors.pool_auth_already_enabled, [Ref.string_of host])
          )
      with Not_found ->
        () (* that's expected, no host had external_auth enabled*) ;
        (* 1b. assert that there are no duplicate hostnames in the pool *)
        if
          List.length hosts
          <> List.length
               (Listext.List.setify
                  (List.map
                     (fun h -> Db.Host.get_hostname ~__context ~self:h)
                     hosts
                  )
               )
        then (
          let errmsg =
            "At least two hosts in the pool have the same hostname"
          in
          debug "%s" errmsg ;
          raise
            (Api_errors.Server_error
               ( Api_errors.pool_auth_enable_failed_duplicate_hostname
               , [Ref.string_of (List.hd hosts); errmsg]
               )
            )
        ) else
          (* 2. tries to enable the external authentication in each host of the pool *)
          let host_error_msg = ref ("", "", "") in
          let rollback_list =
            let _rollback_list = ref [] in
            (* builds a list of hosts to rollback, if any *)
            if
              List.for_all
                (*List.for_all goes through the list up to the point when the predicate fails, inclusive *)
                  (fun h ->
                  (* forward the call to the host in the pool *)
                  try
                    debug "trying to enable external authentication on host %s"
                      (Db.Host.get_name_label ~__context ~self:h) ;
                    call_fn_on_host ~__context
                      (Client.Host.enable_external_auth ~config ~service_name
                         ~auth_type
                      )
                      h ;
                    _rollback_list := h :: !_rollback_list ;
                    (* add h to potential rollback list *)
                    true
                    (* h was successfully enabled. try next in the pool *)
                  with
                  | Api_errors.Server_error (err, [msg]) as e ->
                      debug
                        "received exception while enabling external \
                         authentication for host %s: %s"
                        (Db.Host.get_name_label ~__context ~self:h)
                        (err ^ ": " ^ msg) ;
                      host_error_msg := (err, msg, ExnHelper.string_of_exn e) ;
                      (* error enabling h. we add h here so that we also explicitly disable it during rollback *)
                      (* [that's because it might be in an inconsistent external_auth state] *)
                      _rollback_list := h :: !_rollback_list ;
                      false
                  | e ->
                      debug
                        "received exception while enabling external \
                         authentication for host %s: %s"
                        (Db.Host.get_name_label ~__context ~self:h)
                        (ExnHelper.string_of_exn e) ;
                      host_error_msg := ("", "", ExnHelper.string_of_exn e) ;
                      (* error enabling h. we add h here so that we also explicitly disable it during rollback *)
                      (* [that's because it might be in an inconsistent external_auth state] *)
                      _rollback_list := h :: !_rollback_list ;
                      false
                )
                hosts
            then
              (* if List.for_all returned true, then we have successfully enabled all hosts in the pool *)
              _rollback_list := []
            (* we do not need to rollback any hosts in this case *) ;
            !_rollback_list
          in
          (* 3. if any failed, then do a best-effort rollback, disabling any host that has been just enabled *)
          if rollback_list <> [] then (* FAILED *)
            let failed_host =
              (* the failed host is the first item in the rollback list *)
              List.hd rollback_list
            in
            let failed_host_name_label =
              Db.Host.get_name_label ~__context ~self:failed_host
            in
            match !host_error_msg with
            | err_of_e, msg_of_e, string_of_e -> (
                debug
                  "Rolling back any enabled host, because failed to enable \
                   external authentication for host %s in the pool: %s"
                  failed_host_name_label string_of_e ;
                List.iter
                  (fun host ->
                    (* best-effort attempt to disable all enabled hosts, swallowing any exceptions *)
                    try
                      call_fn_on_host ~__context
                        (Client.Host.disable_external_auth ~config)
                        host
                    with e ->
                      debug
                        "During rollback: Failed to disable external \
                         authentication for host %s: %s"
                        (Db.Host.get_name_label ~__context ~self:host)
                        (ExnHelper.string_of_exn e)
                  )
                  (List.rev rollback_list) ;
                (* we bubble up the exception returned by the failed host *)
                match err_of_e with
                | "" ->
                    (* generic unknown exception *)
                    raise
                      (Api_errors.Server_error
                         ( Api_errors.pool_auth_enable_failed
                         , [Ref.string_of failed_host; string_of_e]
                         )
                      )
                | err_of_e when err_of_e = Api_errors.auth_unknown_type ->
                    raise
                      (Api_errors.Server_error
                         (Api_errors.auth_unknown_type, [msg_of_e])
                      )
                | err_of_e
                  when String.starts_with ~prefix:Api_errors.auth_enable_failed
                         err_of_e ->
                    raise
                      (Api_errors.Server_error
                         ( Api_errors.pool_auth_prefix ^ err_of_e
                         , [Ref.string_of failed_host; msg_of_e]
                         )
                      )
                | _ ->
                    (* Api_errors.Server_error *)
                    raise
                      (Api_errors.Server_error
                         ( Api_errors.pool_auth_enable_failed
                         , [Ref.string_of failed_host; string_of_e]
                         )
                      )
              )
          else (
            (* OK *)
            debug "External authentication enabled for all hosts in the pool" ;
            (* CA-59647: remove subjects that do not belong to the new domain *)
            revalidate_subjects ~__context
          )
  )

(* CP-719: Calls Host.disable_external_auth() on each of the hosts in the pool
    * Reports failure if any of the individual Host.disable_external_auth calls failed or timed-out
    * Guarantees to call Host.disable_external_auth() on every pool host, regardless of whether some of these calls fail
*)
let disable_external_auth ~__context ~pool:_ ~config =
  (* CP-825: Serialize execution of pool-enable-extauth and pool-disable-extauth *)
  (* enabling/disabling the pool's extauth at the same time could produce inconsistent states for extauth in each host of the pool *)
  with_lock Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->
      (* the first element in the hosts list needs to be the pool's master, because we *)
      (* always want to update first the master's record due to homogeneity checks in CA-24856 *)
      let hosts = Xapi_pool_helpers.get_master_slaves_list ~__context in
      let host_msgs_list =
        List.map
          (fun host ->
            (* forward the call to the host in the pool *)
            try
              call_fn_on_host ~__context
                (Client.Host.disable_external_auth ~config)
                host ;
              (* no failed host to add to the filtered list, just visit next host *)
              (host, "", "")
            with
            | Api_errors.Server_error (err, [host_msg]) ->
                let msg =
                  Printf.sprintf "%s: %s"
                    (Db.Host.get_name_label ~__context ~self:host)
                    host_msg
                in
                debug
                  "Failed to disable the external authentication of pool in \
                   host %s"
                  msg ;
                (* no exception should be raised here, we want to visit every host in hosts *)
                (host, err, msg)
            | e ->
                (* add failed host to the filtered list and visit next host *)
                let msg =
                  Printf.sprintf "%s: %s"
                    (Db.Host.get_name_label ~__context ~self:host)
                    (ExnHelper.string_of_exn e)
                in
                debug
                  "Failed to disable the external authentication of pool in \
                   host %s"
                  msg ;
                (* no exception should be raised here, we want to visit every host in hosts *)
                (host, "err", msg)
          )
          hosts
      in
      let failedhosts_list =
        List.filter (fun (_, err, _) -> err <> "") host_msgs_list
      in
      if failedhosts_list <> [] then ((* FAILED *)
        match List.hd failedhosts_list with
        | host, err, msg ->
            debug
              "Failed to disable the external authentication of at least one \
               host in the pool" ;
            if String.starts_with ~prefix:Api_errors.auth_disable_failed err
            then (* tagged exception *)
              raise
                (Api_errors.Server_error
                   (Api_errors.pool_auth_prefix ^ err, [Ref.string_of host; msg])
                )
            else (* generic exception *)
              raise
                (Api_errors.Server_error
                   ( Api_errors.pool_auth_disable_failed
                   , [Ref.string_of host; msg]
                   )
                )
      ) else (* OK *)
        debug
          "The external authentication of all hosts in the pool was disabled \
           successfully"
  )

(* CA-24856: detect non-homogeneous external-authentication config in pool *)
let detect_nonhomogeneous_external_auth_in_pool ~__context =
  let slaves = Xapi_pool_helpers.get_slaves_list ~__context in
  List.iter
    (fun slave ->
      (* check every *slave* in the pool... (the master is always homogeneous to the pool by definition) *)
      (* (also, checking the master inside this function would create an infinite recursion loop) *)
      Xapi_host.detect_nonhomogeneous_external_auth_in_host ~__context
        ~host:slave
    )
    slaves

let run_detect_nonhomogeneous_external_auth_in_pool () =
  (* we do not want to run this test while the pool's extauth is being enabled or disabled *)
  with_lock Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->
      ignore
        (Server_helpers.exec_with_new_task
           "run_detect_nonhomogeneous_external_auth" (fun __context ->
             detect_nonhomogeneous_external_auth_in_pool ~__context
         )
        )
  )

let asynchronously_run_detect_nonhomogeneous_external_auth_in_pool =
  At_least_once_more.make "running detect_nonhomogeneous_external_auth"
    run_detect_nonhomogeneous_external_auth_in_pool

(* non-blocking asynchronous call to verify if the external authentication configuration of the pool is homogeneous *)
let detect_nonhomogeneous_external_auth () =
  At_least_once_more.again
    asynchronously_run_detect_nonhomogeneous_external_auth_in_pool

(* CA-24856: API call to detect non-homogeneous external-authentication config in pool *)
let detect_nonhomogeneous_external_auth ~__context ~pool:_ =
  detect_nonhomogeneous_external_auth ()

module Redo_log = Xapi_database.Redo_log

let create_redo_log_vdi ~__context ~sr =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.VDI.create ~rpc ~session_id ~name_label:"Metadata redo-log"
        ~name_description:
          "Used when HA is disabled, while extra security is still desired"
        ~sR:sr ~virtual_size:Redo_log.minimum_vdi_size ~_type:`redo_log
        ~sharable:true ~read_only:false ~other_config:[] ~xenstore_data:[]
        ~sm_config:Redo_log.redo_log_sm_config ~tags:[]
  )

let find_or_create_redo_log_vdi ~__context ~sr =
  match
    List.filter
      (fun self ->
        true
        && Db.VDI.get_type ~__context ~self = `redo_log
        && Db.VDI.get_virtual_size ~__context ~self >= Redo_log.minimum_vdi_size
      )
      (Db.SR.get_VDIs ~__context ~self:sr)
  with
  | x :: _ ->
      info "re-using existing redo-log VDI: %s"
        (Db.VDI.get_uuid ~__context ~self:x) ;
      x
  | [] ->
      info "no suitable existing redo-log VDI found; creating a fresh one" ;
      create_redo_log_vdi ~__context ~sr

let enable_redo_log ~__context ~sr =
  info "Enabling redo log..." ;
  (* find or create suitable VDI *)
  let vdi =
    try find_or_create_redo_log_vdi ~__context ~sr
    with _ ->
      let msg =
        "failed to create a VDI for the redo log on the SR with the given UUID."
      in
      raise (Api_errors.Server_error (Api_errors.cannot_enable_redo_log, [msg]))
  in
  (* ensure VDI is static, and set a flag in the local DB, such that the redo log can be
     	 * re-enabled after a restart of xapi *)
  ( try
      debug "Ensuring redo-log VDI is static on all hosts in the pool" ;
      let hosts = Db.Host.get_all ~__context in
      let attach host =
        debug "Attaching VDI on host '%s' ('%s')"
          (Db.Host.get_name_label ~__context ~self:host)
          (Ref.string_of host) ;
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Host.attach_static_vdis ~rpc ~session_id ~host
              ~vdi_reason_map:[(vdi, Xapi_globs.gen_metadata_vdi_reason)]
        ) ;
        debug "Setting redo-log local-DB flag on host '%s' ('%s')"
          (Db.Host.get_name_label ~__context ~self:host)
          (Ref.string_of host) ;
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Host.set_localdb_key ~rpc ~session_id ~host
              ~key:Constants.redo_log_enabled ~value:"true"
        )
      in
      List.iter attach hosts ;
      debug "VDI is static on all hosts"
    with _ ->
      let msg = "failed to make VDI static." in
      raise (Api_errors.Server_error (Api_errors.cannot_enable_redo_log, [msg]))
  ) ;
  (* update state *)
  debug "Updating state..." ;
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_redo_log_vdi ~__context ~self:pool ~value:vdi ;
  Db.Pool.set_redo_log_enabled ~__context ~self:pool ~value:true ;
  (* enable the new redo log, unless HA is enabled (which means a redo log
     	 * is already in use) *)
  if not (Db.Pool.get_ha_enabled ~__context ~self:pool) then (
    Redo_log.enable_and_flush
      (Context.database_of __context |> Xapi_database.Db_ref.get_database)
      Xapi_ha.ha_redo_log Xapi_globs.gen_metadata_vdi_reason ;
    Localdb.put Constants.redo_log_enabled "true"
  ) ;
  info "The redo log is now enabled"

let disable_redo_log ~__context =
  info "Disabling redo log..." ;
  (* disable redo-log state flag and switch off redo log if HA is disabled *)
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_redo_log_enabled ~__context ~self:pool ~value:false ;
  if not (Db.Pool.get_ha_enabled ~__context ~self:pool) then (
    Redo_log_usage.stop_using_redo_log Xapi_ha.ha_redo_log ;
    Redo_log.disable Xapi_ha.ha_redo_log ;
    (* disable static-ness of the VDI and clear local-DB flags *)
    let vdi = Db.Pool.get_redo_log_vdi ~__context ~self:pool in
    let hosts = Db.Host.get_all ~__context in
    try
      let detach host =
        debug "Detaching VDI from host '%s' ('%s')"
          (Db.Host.get_name_label ~__context ~self:host)
          (Ref.string_of host) ;
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Host.detach_static_vdis ~rpc ~session_id ~host ~vdis:[vdi]
        ) ;
        debug "Clearing redo-log local-DB flag on host '%s' ('%s')"
          (Db.Host.get_name_label ~__context ~self:host)
          (Ref.string_of host) ;
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Host.set_localdb_key ~rpc ~session_id ~host
              ~key:Constants.redo_log_enabled ~value:"false"
        )
      in
      List.iter detach hosts
    with _ -> info "Failed to detach static VDIs from all hosts."
  ) ;
  info "The redo log is now disabled"

let set_vswitch_controller ~__context ~address =
  let dbg = Context.string_of_task __context in
  match Net.Bridge.get_kind dbg () with
  | Network_interface.Openvswitch ->
      let pool = Helpers.get_pool ~__context in
      let current_address =
        Db.Pool.get_vswitch_controller ~__context ~self:pool
      in
      if current_address <> address then (
        if address <> "" then
          Helpers.assert_is_valid_ip `ipv4 "address" address ;
        Db.Pool.set_vswitch_controller ~__context ~self:pool ~value:address ;
        let sdn_controllers = Db.SDN_controller.get_all ~__context in
        if sdn_controllers <> [] then
          Xapi_sdn_controller.forget ~__context ~self:(List.hd sdn_controllers) ;
        if address <> "" then
          ignore
            (Xapi_sdn_controller.introduce ~__context ~protocol:`ssl ~address
               ~port:6632L
            )
      )
  | _ ->
      raise
        (Api_errors.Server_error
           ( Api_errors.operation_not_allowed
           , ["host not configured for vswitch operation"]
           )
        )

(* internal intra-pool call to allow slaves to log http actions on the master *)
let audit_log_append ~__context ~line =
  (* populate friendly names for the references of the call arguments *)
  (* this is necessary here because the slave doesn't have access to these names *)
  let line = Rbac_audit.populate_audit_record_with_obj_names_of_refs line in
  (* copy audit record from slave exactly as it is, without any new prefixes *)
  let (_ : string) = Rbac_audit.append_line ~raw:true "%s" line in
  ()

let test_archive_target ~__context ~self:_ ~config:_ =
  raise (Api_errors.Server_error (Api_errors.message_removed, []))

let enable_local_storage_caching ~__context ~self:_ =
  let srs = Db.SR.get_all_records ~__context in
  let pbds = Db.PBD.get_all_records ~__context in
  let hosts = Db.Host.get_all ~__context in
  (* Exception handler is to cope with transient PBDs with invalid references *)
  let hosts_and_srs =
    List.filter_map
      (fun (_, pbdrec) ->
        try
          Some
            ( pbdrec.API.pBD_host
            , pbdrec.API.pBD_SR
            , List.assoc pbdrec.API.pBD_SR srs
            )
        with _ -> None
      )
      pbds
  in
  let acceptable =
    List.filter
      (fun (_, _, srrec) ->
        (not srrec.API.sR_shared)
        && List.length srrec.API.sR_PBDs = 1
        && Smint.Feature.(
             has_capability Sr_supports_local_caching
               (Sm.features_of_driver srrec.API.sR_type)
           )
      )
      hosts_and_srs
  in
  let failed_hosts =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        let failed =
          List.filter_map
            (fun host ->
              let result = ref (Some host) in
              let acceptable_srs =
                List.filter (fun (href, _, _) -> href = host) acceptable
              in
              List.iter
                (fun (_, ref, _) ->
                  try
                    Client.Host.enable_local_storage_caching ~rpc ~session_id
                      ~host ~sr:ref ;
                    result := None
                  with _ -> ()
                )
                acceptable_srs ;
              !result
            )
            hosts
        in
        failed
    )
  in
  if failed_hosts <> [] then
    raise
      (Api_errors.Server_error
         ( Api_errors.hosts_failed_to_enable_caching
         , List.map Ref.string_of failed_hosts
         )
      )
  else
    ()

let disable_local_storage_caching ~__context ~self:_ =
  let hosts = Db.Host.get_all ~__context in
  let failed_hosts =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        List.filter_map
          (fun host ->
            try
              Client.Host.disable_local_storage_caching ~rpc ~session_id ~host ;
              None
            with _ -> Some host
          )
          hosts
    )
  in
  if failed_hosts <> [] then
    raise
      (Api_errors.Server_error
         ( Api_errors.hosts_failed_to_disable_caching
         , List.map Ref.string_of failed_hosts
         )
      )
  else
    ()

let get_license_state ~__context ~self:_ =
  let edition_to_int =
    List.map
      V6_interface.(fun ed -> (ed.title, ed.order))
      (V6_client.get_editions "get_license_state")
  in
  let hosts = Db.Host.get_all ~__context in
  let pool_edition, expiry =
    Xapi_pool_license.get_lowest_edition_with_expiry ~__context ~hosts
      ~edition_to_int
  in
  let pool_expiry = License_check.serialize_expiry expiry in
  [("edition", pool_edition); ("expiry", pool_expiry)]

let apply_edition ~__context ~self:_ ~edition =
  let hosts = Db.Host.get_all ~__context in
  let apply_fn ~__context ~host ~edition =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.Host.apply_edition ~rpc ~session_id ~host ~edition ~force:false
    )
  in
  Xapi_pool_license.apply_edition_with_rollback ~__context ~hosts ~edition
    ~apply_fn

(* This is expensive, so should always be run on the master. *)
let assert_mac_seeds_available ~__context ~self:_ ~seeds =
  let module StringSet = Set.Make (String) in
  let all_guests =
    Db.VM.get_records_where ~__context
      ~expr:(Eq (Field "is_control_domain", Literal "false"))
  in
  (* Create a set of all MAC seeds in use by guests in the pool. *)
  let mac_seeds_in_use =
    List.fold_left
      (fun acc (_, vm_rec) ->
        try
          let mac_seed =
            List.assoc Xapi_globs.mac_seed vm_rec.API.vM_other_config
          in
          StringSet.add mac_seed acc
        with Not_found -> acc
      )
      StringSet.empty all_guests
  in
  (* Create a set of the MAC seeds we want to test. *)
  let mac_seeds_to_test =
    List.fold_left
      (fun acc mac_seed -> StringSet.add mac_seed acc)
      StringSet.empty seeds
  in
  (* Check if the intersection of these sets is non-empty. *)
  let problem_mac_seeds = StringSet.inter mac_seeds_in_use mac_seeds_to_test in
  if not (StringSet.is_empty problem_mac_seeds) then
    raise
      (Api_errors.Server_error
         (Api_errors.duplicate_mac_seed, [StringSet.choose problem_mac_seeds])
      )

let disable_ssl_legacy ~__context ~self:_ =
  warn "disable_ssl_legacy: doing nothing"

let set_igmp_snooping_enabled ~__context ~self ~value =
  if value then
    Pool_features.assert_enabled ~__context ~f:Features.IGMP_snooping ;
  Helpers.assert_using_vswitch ~__context ;
  Db.Pool.set_igmp_snooping_enabled ~__context ~self ~value ;
  let hosts = Db.Host.get_all ~__context in
  let networks = Db.Network.get_all ~__context in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let failure =
        List.fold_left
          (fun fail host ->
            List.fold_left
              (fun fail' network ->
                let local_pifs =
                  Xapi_network_attach_helpers.get_local_pifs ~__context ~network
                    ~host
                in
                try
                  match local_pifs with
                  | pif :: _ ->
                      (* There is at most one local PIF, by construction *)
                      let pif_record = Db.PIF.get_record ~__context ~self:pif in
                      if
                        pif_record.API.pIF_VLAN = -1L
                        && pif_record.API.pIF_bond_slave_of = Ref.null
                        && pif_record.API.pIF_managed
                      then
                        Client.Network.attach ~rpc ~session_id ~network ~host ;
                      fail'
                  | [] ->
                      (* Internal network *)
                      fail'
                with _ ->
                  error
                    "set_igmp_snooping_enabled:Network.attach failed on host \
                     uuid=%s network uuid=%s"
                    (Db.Host.get_uuid ~__context ~self:host)
                    (Db.Network.get_uuid ~__context ~self:network) ;
                  true
              )
              fail networks
          )
          false hosts
      in
      if failure then
        raise
          (Api_errors.Server_error
             (Api_errors.could_not_update_igmp_snooping_everywhere, [])
          )
  )

let has_extension ~__context ~self:_ ~name =
  let hosts = Db.Host.get_all ~__context in
  List.for_all
    (fun host ->
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Host.has_extension ~rpc ~session_id ~host ~name
      )
    )
    hosts

let guest_agent_config_requirements =
  let open Map_check in
  [
    {
      key= Xapi_xenops.Guest_agent_features.Xapi.auto_update_enabled
    ; default_value= None
    ; is_valid_value=
        (fun x ->
          try
            let (_ : bool) = bool_of_string x in
            true
          with Invalid_argument _ -> false
        )
    }
  ; {
      key= Xapi_xenops.Guest_agent_features.Xapi.auto_update_url
    ; default_value= None
    ; is_valid_value=
        (fun url ->
          match Uri.of_string url |> Uri.scheme with
          | Some "http" | Some "https" ->
              true
          | _ ->
              false
        )
    }
  ]

let add_to_guest_agent_config ~__context ~self ~key ~value =
  Map_check.validate_kvpair "guest_agent_config" guest_agent_config_requirements
    (key, value) ;
  Db.Pool.add_to_guest_agent_config ~__context ~self ~key ~value ;
  Xapi_pool_helpers.apply_guest_agent_config ~__context

let remove_from_guest_agent_config ~__context ~self ~key =
  Db.Pool.remove_from_guest_agent_config ~__context ~self ~key ;
  Xapi_pool_helpers.apply_guest_agent_config ~__context

let rotate_secret = Xapi_psr.start

let alert_failed_login_attempts () =
  let now = Date.localtime () in
  let login_failures_between =
    Printf.sprintf "login failures between '%s' and last check"
      (Date.to_rfc3339 now)
  in
  match Xapi_session.get_failed_login_stats () with
  | None ->
      debug "alert_failed_login_attempts: no %s" login_failures_between
  | Some stats ->
      info "alert_failed_login_attempts: there have been %s"
        login_failures_between ;
      Server_helpers.exec_with_new_task "alert failed login attempts"
        (fun __context ->
          Xapi_alert.add ~msg:Api_messages.failed_login_attempts ~cls:`Pool
            ~obj_uuid:
              (Db.Pool.get_uuid ~__context ~self:(Helpers.get_pool ~__context))
            ~body:stats
      )

let perform ~local_fn ~__context ~host ~remote_fn =
  let rpc' _context hostname (task_opt : API.ref_task option) xml =
    let open Xmlrpc_client in
    let verify_cert = Some Stunnel.pool (* verify! *) in
    let task_id = Option.map Ref.string_of task_opt in
    let tracing = Context.set_client_span __context in
    let http = xmlrpc ?task_id ~version:"1.0" "/" in
    let http = Helpers.TraceHelper.inject_span_into_req tracing http in
    let port = !Constants.https_port in
    let transport = SSL (SSL.make ~verify_cert ?task_id (), hostname, port) in
    XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:"dst_xapi" ~transport ~http xml
  in
  let open Message_forwarding in
  do_op_on_common ~local_fn ~__context ~host ~remote_fn
    (call_slave_with_session rpc')

(** [ping_with_tls_verification host] calls [Pool.is_slave] using a TLS
connection that uses certficate checking. We ignore the result but are
interested in any failures that would indicate connection problems,
which would raise an exception. We can't use the standard
[Message_forwarding.do_op_on_common] because certificate checking is not
yet enabled. *)

let ping_with_tls_verification ~__context host =
  let local_fn = is_slave ~host in
  let remote_fn = Client.Pool.is_slave ~host in
  let this_uuid = Helpers.get_localhost_uuid () in
  let remote_uuid = Db.Host.get_uuid ~__context ~self:host in
  info "pinging host before enabling TLS: %s -> %s" this_uuid remote_uuid ;
  let _ : bool =
    Server_helpers.exec_with_subtask ~__context "pool.ping"
      (perform ~local_fn ~host ~remote_fn)
  in
  ()

(* TODO: move to xapi_host *)
let enable_tls_verification ~__context =
  let self = Helpers.get_localhost ~__context in
  let hosts = Db.Host.get_all ~__context in
  List.iter (ping_with_tls_verification ~__context) hosts ;
  Stunnel_client.set_verify_by_default true ;
  Db.Host.set_tls_verification_enabled ~__context ~self ~value:true ;
  Helpers.touch_file Constants.verify_certificates_path ;
  let host = Helpers.get_localhost ~__context in
  match Xapi_clustering.find_cluster_host ~__context ~host with
  | None ->
      ()
  | Some self ->
      Xapi_cluster_host.set_tls_config ~__context ~self ~verify:true

let assert_single_repo_can_be_enabled ~__context ~repos =
  let origins =
    repos
    |> List.filter_map (fun repo ->
           match Db.Repository.get_origin ~__context ~self:repo with
           | (`bundle | `remote_pool) as origin ->
               Some origin
           | `remote ->
               None
       )
    |> List.fold_left
         (fun acc origin -> if List.mem origin acc then acc else origin :: acc)
         []
  in
  match (repos, origins) with
  | _ :: _ :: _, _ :: _ ->
      raise Api_errors.(Server_error (repo_should_be_single_one_enabled, []))
  | _, _ ->
      ()

let assert_can_sync_updates ~__context ~repos =
  List.iter
    (fun repo ->
      match Db.Repository.get_origin ~__context ~self:repo with
      | `remote | `remote_pool ->
          ()
      | `bundle ->
          raise Api_errors.(Server_error (can_not_sync_updates, []))
    )
    repos

let can_periodic_sync_updates ~__context ~repos =
  List.for_all
    (fun repo -> Db.Repository.get_origin ~__context ~self:repo = `remote)
    repos

let disable_unsupported_periodic_sync_updates ~__context ~self ~repos =
  if not (can_periodic_sync_updates ~__context ~repos) then (
    Pool_periodic_update_sync.set_enabled ~__context ~value:false ;
    Db.Pool.set_update_sync_enabled ~__context ~self ~value:false
  )

let set_repositories ~__context ~self ~value =
  Xapi_pool_helpers.with_pool_operation ~__context ~self
    ~doc:"pool.set_repositories" ~op:`configure_repositories
  @@ fun () ->
  assert_single_repo_can_be_enabled ~__context ~repos:value ;
  let existings = Db.Pool.get_repositories ~__context ~self in
  (* To be removed *)
  List.iter
    (fun x ->
      if not (List.mem x value) then (
        Repository.cleanup_pool_repo ~__context ~self:x ;
        Repository.reset_updates_in_cache ()
      )
    )
    existings ;
  (* To be added *)
  List.iter
    (fun x ->
      if not (List.mem x existings) then (
        Db.Repository.set_hash ~__context ~self:x ~value:"" ;
        Repository.reset_updates_in_cache () ;
        Db.Pool.set_last_update_sync ~__context ~self ~value:Date.epoch
      )
    )
    value ;
  Db.Pool.set_repositories ~__context ~self ~value ;
  if Db.Pool.get_repositories ~__context ~self = [] then
    Db.Pool.set_last_update_sync ~__context ~self ~value:Date.epoch ;
  disable_unsupported_periodic_sync_updates ~__context ~self ~repos:value

let add_repository ~__context ~self ~value =
  Xapi_pool_helpers.with_pool_operation ~__context ~self
    ~doc:"pool.add_repository" ~op:`configure_repositories
  @@ fun () ->
  let existings = Db.Pool.get_repositories ~__context ~self in
  if not (List.mem value existings) then (
    assert_single_repo_can_be_enabled ~__context ~repos:(value :: existings) ;
    Db.Pool.add_repositories ~__context ~self ~value ;
    Db.Repository.set_hash ~__context ~self:value ~value:"" ;
    Repository.reset_updates_in_cache () ;
    Db.Pool.set_last_update_sync ~__context ~self ~value:Date.epoch ;
    disable_unsupported_periodic_sync_updates ~__context ~self
      ~repos:(value :: existings)
  )

let remove_repository ~__context ~self ~value =
  Xapi_pool_helpers.with_pool_operation ~__context ~self
    ~doc:"pool.remove_repository" ~op:`configure_repositories
  @@ fun () ->
  List.iter
    (fun x ->
      if x = value then (
        Repository.cleanup_pool_repo ~__context ~self:x ;
        Repository.reset_updates_in_cache ()
      )
    )
    (Db.Pool.get_repositories ~__context ~self) ;
  Db.Pool.remove_repositories ~__context ~self ~value ;
  if Db.Pool.get_repositories ~__context ~self = [] then
    Db.Pool.set_last_update_sync ~__context ~self ~value:Date.epoch

let sync_repos ~__context ~self ~repos ~force ~token ~token_id ~username
    ~password =
  let open Repository in
  repos
  |> List.iter (fun repo ->
         if force then cleanup_pool_repo ~__context ~self:repo ;
         let complete =
           sync ~__context ~self:repo ~token ~token_id ~username ~password
         in
         (* Dnf and custom yum-utils sync all the metadata including updateinfo,
          * Thus no need to re-create pool repository *)
         if Pkgs.manager = Yum && complete = false then
           create_pool_repository ~__context ~self:repo
     ) ;
  let checksum = set_available_updates ~__context in
  Db.Pool.set_last_update_sync ~__context ~self ~value:(Date.now ()) ;
  checksum

let sync_updates ~__context ~self ~force ~token ~token_id ~username ~password =
  Pool_features.assert_enabled ~__context ~f:Features.Updates ;
  Xapi_pool_helpers.with_pool_operation ~__context ~self
    ~doc:"pool.sync_updates" ~op:`sync_updates
  @@ fun () ->
  let repos = Repository_helpers.get_enabled_repositories ~__context in
  assert_can_sync_updates ~__context ~repos ;
  sync_repos ~__context ~self ~repos ~force ~token ~token_id ~username ~password

let check_update_readiness ~__context ~self:_ ~requires_reboot =
  (* Pool license check *)
  Pool_features.assert_enabled ~__context ~f:Features.Updates ;
  (* Pool checks *)
  let pool_errors =
    let pool = Helpers.get_pool ~__context in
    if Db.Pool.get_ha_enabled ~__context ~self:pool then
      [[Api_errors.ha_is_enabled]]
    else
      []
  in
  (* Host and VM checks *)
  let check host =
    (* Check if any host is offline. *)
    let alive =
      try
        let hm = Db.Host.get_metrics ~__context ~self:host in
        Db.Host_metrics.get_live ~__context ~self:hm
      with _ -> false
    in
    if not alive then
      [[Api_errors.host_offline; Ref.string_of host]]
    else if requires_reboot then
      Xapi_host.get_vms_which_prevent_evacuation_internal ~__context ~self:host
        ~ignore_ha:true
      |> List.map (fun (_, error) -> error)
    else
      [[]]
  in
  let host_errors =
    Db.Host.get_all ~__context
    |> List.map check
    |> List.concat
    |> List.filter (( <> ) [])
  in
  (* Return both *)
  pool_errors @ host_errors

let enable_client_certificate_auth ~__context ~self ~name =
  (* TODO: actually enable client-certificate auth *)
  Db.Pool.set_client_certificate_auth_enabled ~__context ~self ~value:true ;
  Db.Pool.set_client_certificate_auth_name ~__context ~self ~value:name ;
  Xapi_mgmt_iface.run ~__context ()

let disable_client_certificate_auth ~__context ~self =
  (* TODO: actually disable client-certificate auth *)
  Db.Pool.set_client_certificate_auth_enabled ~__context ~self ~value:false ;
  Db.Pool.set_client_certificate_auth_name ~__context ~self ~value:"" ;
  Xapi_mgmt_iface.run ~__context ()

let get_updates_handler (req : Http.Request.t) s _ =
  debug "Pool.get_updates_handler: received request" ;
  req.Http.Request.close <- true ;
  let query = req.Http.Request.query in
  Xapi_http.with_context "Getting available updates" req s (fun __context ->
      let all_hosts = Db.Host.get_all ~__context in
      let hs =
        match List.assoc "host_refs" query with
        | v -> (
            List.map
              (fun ref_str -> Ref.of_string ref_str)
              (Astring.String.cuts ~sep:";" v)
            |> fun l ->
            let is_invalid ref =
              not (Db.is_valid_ref __context ref && List.mem ref all_hosts)
            in
            match (List.exists is_invalid l, l) with
            | true, _ | false, [] ->
                []
            | false, l ->
                l
          )
        | exception Not_found ->
            all_hosts
      in
      match hs with
      | _ :: _ as hosts ->
          let failures_of_404 =
            [
              Api_errors.no_repository_enabled
            ; Api_errors.invalid_repomd_xml
            ; Api_errors.invalid_updateinfo_xml
            ]
          in
          Xapi_pool_helpers.with_pool_operation ~__context
            ~self:(Helpers.get_pool ~__context)
            ~doc:"pool.get_updates" ~op:`get_updates (fun () ->
              try
                let json_str =
                  Yojson.Basic.to_string
                    (Repository.get_pool_updates_in_json ~__context ~hosts)
                in
                let size = Int64.of_int (String.length json_str) in
                Http_svr.headers s
                  (Http.http_200_ok_with_content size ~keep_alive:false ()
                  @ [Http.Hdr.content_type ^ ": application/json"]
                  ) ;
                Unixext.really_write_string s json_str |> ignore
              with
              | Api_errors.(Server_error (failure, _)) as e
                when List.mem failure failures_of_404 ->
                  error "404: can't get updates for pool: %s"
                    (ExnHelper.string_of_exn e) ;
                  Http_svr.headers s (Http.http_404_missing ())
              | Api_errors.(Server_error (failure, _)) as e
                when not (List.mem failure failures_of_404) ->
                  error "getting updates for pool failed: %s"
                    (ExnHelper.string_of_exn e) ;
                  raise e
              | e ->
                  (* http_500_internal_server_error *)
                  error "getting updates for pool failed: %s"
                    (ExnHelper.string_of_exn e) ;
                  raise Api_errors.(Server_error (get_updates_failed, []))
          )
      | [] ->
          error "400: Invalid 'host_refs' in query" ;
          Http_svr.headers s (Http.http_400_badrequest ())
  )

let configure_repository_proxy ~__context ~self ~url ~username ~password =
  ( match Uri.scheme (Uri.of_string url) with
  | Some "http" | Some "https" ->
      ()
  | _ ->
      raise Api_errors.(Server_error (invalid_repository_proxy_url, [url]))
  ) ;
  ( match (username, password) with
  | "", p when p <> "" ->
      error "missing username of the repository proxy with a password specified" ;
      raise Api_errors.(Server_error (invalid_repository_proxy_credential, []))
  | u, "" when u <> "" ->
      error "missing password of the repository proxy with a username specified" ;
      raise Api_errors.(Server_error (invalid_repository_proxy_credential, []))
  | u, p when u <> "" && p <> "" ->
      if String.contains u '\n' || String.contains p '\n' then (
        error "getting invalid username/password of the repository proxy" ;
        raise Api_errors.(Server_error (invalid_repository_proxy_credential, []))
      )
  | _ ->
      ()
  ) ;
  let old_secret_ref = Db.Pool.get_repository_proxy_password ~__context ~self in
  let secret_ref =
    if password = "" then
      Ref.null
    else
      Xapi_secret.create ~__context ~value:password ~other_config:[]
  in
  Db.Pool.set_repository_proxy_url ~__context ~self ~value:url ;
  Db.Pool.set_repository_proxy_username ~__context ~self ~value:username ;
  Db.Pool.set_repository_proxy_password ~__context ~self ~value:secret_ref ;
  if old_secret_ref <> Ref.null then
    Xapi_stdext_pervasives.Pervasiveext.ignore_exn (fun _ ->
        Db.Secret.destroy ~__context ~self:old_secret_ref
    )

let disable_repository_proxy ~__context ~self =
  Db.Pool.set_repository_proxy_url ~__context ~self ~value:"" ;
  Db.Pool.set_repository_proxy_username ~__context ~self ~value:"" ;
  let old_secret_ref = Db.Pool.get_repository_proxy_password ~__context ~self in
  Db.Pool.set_repository_proxy_password ~__context ~self ~value:Ref.null ;
  if old_secret_ref <> Ref.null then
    Xapi_stdext_pervasives.Pervasiveext.ignore_exn (fun _ ->
        Db.Secret.destroy ~__context ~self:old_secret_ref
    )

let set_uefi_certificates ~__context ~self:_ ~value:_ =
  let msg =
    "Setting UEFI certificates is deprecated, please use \
     `set_custom_uefi_certificates`"
  in
  raise Api_errors.(Server_error (operation_not_allowed, [msg]))

let set_custom_uefi_certificates ~__context ~self ~value =
  match !Xapi_globs.allow_custom_uefi_certs with
  | false ->
      let msg =
        "Setting UEFI certificates is not possible when \
         allow_custom_uefi_certs is false"
      in
      raise Api_errors.(Server_error (operation_not_allowed, [msg]))
  | true ->
      Db.Pool.set_custom_uefi_certificates ~__context ~self ~value ;
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          List.iter
            (fun host ->
              Client.Host.write_uefi_certificates_to_disk ~rpc ~session_id ~host
            )
            (Db.Host.get_all ~__context)
      )

let set_https_only ~__context ~self:_ ~value =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      List.iter
        (fun host ->
          Client.Host.set_https_only ~rpc ~session_id ~self:host ~value
        )
        (Db.Host.get_all ~__context)
  )

let set_telemetry_next_collection ~__context ~self ~value =
  let max_days =
    match Db.Pool.get_telemetry_frequency ~__context ~self with
    | `daily ->
        2
    | `weekly ->
        14
    | `monthly ->
        62
  in
  let dt_of_max_sched, dt_of_value =
    match
      ( Ptime.Span.of_int_s (max_days * 24 * 3600)
        |> Ptime.add_span (Ptime_clock.now ())
      , value |> Date.to_ptime
      )
    with
    | Some dt1, dt2 ->
        (dt1, dt2)
    | _ | (exception _) ->
        Helpers.internal_error
          "Can't parse date and time for telemetry collection."
  in
  let ts = Date.to_rfc3339 value in
  match Ptime.is_later dt_of_value ~than:dt_of_max_sched with
  | true ->
      raise Api_errors.(Server_error (telemetry_next_collection_too_late, [ts]))
  | false ->
      debug "Set the next telemetry collection to %s" ts ;
      Db.Pool.set_telemetry_next_collection ~__context ~self ~value

let reset_telemetry_uuid ~__context ~self =
  debug "Creating new telemetry UUID" ;
  let old_ref = Db.Pool.get_telemetry_uuid ~__context ~self in
  let uuid = Uuidx.to_string (Uuidx.make ()) in
  let ref = Xapi_secret.create ~__context ~value:uuid ~other_config:[] in
  Db.Pool.set_telemetry_uuid ~__context ~self ~value:ref ;
  if old_ref <> Ref.null then
    debug "Destroying old telemetry UUID" ;
  Xapi_stdext_pervasives.Pervasiveext.ignore_exn (fun _ ->
      Db.Secret.destroy ~__context ~self:old_ref
  )

let configure_update_sync ~__context ~self ~update_sync_frequency
    ~update_sync_day =
  let day =
    match (update_sync_frequency, update_sync_day) with
    | `weekly, d when d < 0L || d > 6L ->
        error
          "For weekly schedule, cannot set the day when update sync will run \
           to an integer out of range: 0 ~ 6" ;
        raise
          Api_errors.(
            Server_error
              (invalid_update_sync_day, [Int64.to_string update_sync_day])
          )
    | `daily, d ->
        if d <> 0L then
          warn
            "For 'daily' schedule, the value of update_sync_day is ignored, \
             update_sync_day of the pool will be set to the default value 0." ;
        0L
    | `weekly, d ->
        d
  in
  Db.Pool.set_update_sync_frequency ~__context ~self
    ~value:update_sync_frequency ;
  Db.Pool.set_update_sync_day ~__context ~self ~value:day ;
  if Db.Pool.get_update_sync_enabled ~__context ~self then
    (* re-schedule periodic update sync with new configuration immediately *)
    Pool_periodic_update_sync.set_enabled ~__context ~value:true

let set_update_sync_enabled ~__context ~self ~value =
  ( if value then
      match Db.Pool.get_repositories ~__context ~self with
      | [] ->
          error
            "Cannot enable automatic update syncing if there are no \
             repositories." ;
          raise Api_errors.(Server_error (no_repositories_configured, []))
      | repos ->
          if not (can_periodic_sync_updates ~__context ~repos) then
            raise Api_errors.(Server_error (can_not_periodic_sync_updates, []))
  ) ;
  Pool_periodic_update_sync.set_enabled ~__context ~value ;
  Db.Pool.set_update_sync_enabled ~__context ~self ~value

let set_local_auth_max_threads ~__context:_ ~self:_ ~value =
  Xapi_session.set_local_auth_max_threads value

let set_ext_auth_max_threads ~__context:_ ~self:_ ~value =
  Xapi_session.set_ext_auth_max_threads value

let set_ext_auth_cache_enabled ~__context ~self ~value:enabled =
  Db.Pool.set_ext_auth_cache_enabled ~__context ~self ~value:enabled ;
  if not enabled then
    Xapi_session.clear_external_auth_cache ()

let set_ext_auth_cache_size ~__context ~self ~value:capacity =
  if capacity < 0L then
    raise
      Api_errors.(
        Server_error (invalid_value, ["size"; Int64.to_string capacity])
      )
  else
    Db.Pool.set_ext_auth_cache_size ~__context ~self ~value:capacity

let set_ext_auth_cache_expiry ~__context ~self ~value:expiry_seconds =
  if expiry_seconds <= 0L then
    raise
      Api_errors.(
        Server_error (invalid_value, ["expiry"; Int64.to_string expiry_seconds])
      )
  else
    Db.Pool.set_ext_auth_cache_expiry ~__context ~self ~value:expiry_seconds

let get_guest_secureboot_readiness ~__context ~self:_ =
  let auth_files = Sys.readdir !Xapi_globs.varstore_dir in
  let pk_present = Array.mem "PK.auth" auth_files in
  let kek_present = Array.mem "KEK.auth" auth_files in
  let db_present = Array.mem "db.auth" auth_files in
  let dbx_present = Array.mem "dbx.auth" auth_files in
  match (pk_present, kek_present, db_present, dbx_present) with
  | true, true, true, true ->
      `ready
  | true, true, true, false ->
      `ready_no_dbx
  | _, _, _, _ ->
      `not_ready

let put_bundle_handler (req : Request.t) s _ =
  req.Request.close <- true ;
  Xapi_http.with_context "Sync bundle" req s (fun __context ->
      (* This is the signal to say we've taken responsibility from the CLI server
         for completing the task *)
      (* The GUI can deal with this itself, but the CLI is complicated by the thin
         cli/cli server split *)
      TaskHelper.set_progress ~__context 0.0 ;
      Pool_features.assert_enabled ~__context ~f:Features.Updates ;
      let pool = Helpers.get_pool ~__context in
      Xapi_pool_helpers.with_pool_operation ~__context ~self:pool
        ~doc:"pool.sync_bundle" ~op:`sync_bundle
      @@ fun () ->
      Http_svr.headers s (Http.http_200_ok ()) ;
      let repo_opt =
        try
          let repo =
            Repository_helpers.get_single_enabled_update_repository ~__context
          in
          Some repo
        with e ->
          TaskHelper.failed ~__context e ;
          Http_svr.headers s (Http.http_400_badrequest ()) ;
          None
      in
      match repo_opt with
      | Some repo -> (
        match Db.Repository.get_origin ~__context ~self:repo with
        | `bundle -> (
            let result =
              Tar_ext.unpack_tar_file
                ~dir:!Xapi_globs.bundle_repository_dir
                ~ifd:s
                ~max_size_limit:!Xapi_globs.bundle_max_size_limit
            in
            match result with
            | Ok () ->
                TaskHelper.set_progress ~__context 0.8 ;
                finally
                  (fun () ->
                    try
                      sync_repos ~__context ~self:pool ~repos:[repo] ~force:true
                        ~token:"" ~token_id:"" ~username:"" ~password:""
                      |> ignore
                    with _ ->
                      raise Api_errors.(Server_error (bundle_sync_failed, []))
                  )
                  (fun () -> Unixext.rm_rec !Xapi_globs.bundle_repository_dir)
            | Error e ->
                error "%s: Failed to unpack bundle with error %s" __FUNCTION__
                  (Tar_ext.unpack_error_to_string e) ;
                TaskHelper.failed ~__context
                  Api_errors.(
                    Server_error
                      (bundle_unpack_failed, [Tar_ext.unpack_error_to_string e])
                  ) ;
                Http_svr.headers s (Http.http_400_badrequest ())
          )
        | `remote | `remote_pool ->
            error "%s: Bundle repo is not enabled" __FUNCTION__ ;
            TaskHelper.failed ~__context
              Api_errors.(Server_error (bundle_repo_not_enabled, [])) ;
            Http_svr.headers s (Http.http_400_badrequest ())
      )
      | None ->
          ()
  )

module Ssh = struct
  let operate ~__context ~action ~error =
    let hosts = Db.Host.get_all ~__context in
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        let failed_hosts =
          List.fold_left
            (fun failed_hosts host ->
              try
                action ~rpc ~session_id ~self:host ;
                failed_hosts
              with _ -> Ref.string_of host :: failed_hosts
            )
            [] hosts
        in
        match failed_hosts with
        | [] ->
            ()
        | _ ->
            raise (Api_errors.Server_error (error, failed_hosts))
    )

  let enable ~__context ~self:_ =
    operate ~__context ~action:Client.Host.enable_ssh
      ~error:Api_errors.enable_ssh_partially_failed

  let disable ~__context ~self:_ =
    operate ~__context ~action:Client.Host.disable_ssh
      ~error:Api_errors.disable_ssh_partially_failed

  let set_enabled_timeout ~__context ~self:_ ~value =
    operate ~__context
      ~action:(fun ~rpc ~session_id ~self ->
        Client.Host.set_ssh_enabled_timeout ~rpc ~session_id ~self ~value
      )
      ~error:Api_errors.set_ssh_timeout_partially_failed

  let set_console_timeout ~__context ~self:_ ~value =
    operate ~__context
      ~action:(fun ~rpc ~session_id ~self ->
        Client.Host.set_console_idle_timeout ~rpc ~session_id ~self ~value
      )
      ~error:Api_errors.set_console_timeout_partially_failed

  let set_ssh_auto_mode ~__context ~self:_ ~value =
    operate ~__context
      ~action:(fun ~rpc ~session_id ~self ->
        Client.Host.set_ssh_auto_mode ~rpc ~session_id ~self ~value
      )
      ~error:Api_errors.set_ssh_auto_mode_partially_failed
end

let enable_ssh = Ssh.enable

let disable_ssh = Ssh.disable

let set_ssh_enabled_timeout = Ssh.set_enabled_timeout

let set_console_idle_timeout = Ssh.set_console_timeout

let set_ssh_auto_mode = Ssh.set_ssh_auto_mode
