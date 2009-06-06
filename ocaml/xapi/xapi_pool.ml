open Client
open Db_filter_types
open Pervasiveext
open Threadext

module L = Debug.Debugger(struct let name="license" end)
module D=Debug.Debugger(struct let name="xapi" end)
open D
open Workload_balancing

(* Surpress exceptions *)
let no_exn f x = 
  try ignore (f x) 
  with exn ->
    debug "Ignoring exception: %s" (ExnHelper.string_of_exn exn)

let rpc host_address xml =
  try
    Xmlrpcclient.do_secure_xml_rpc ~version:"1.0" ~host:host_address
      ~port:!Xapi_globs.https_port ~path:"/" xml
  with Xmlrpcclient.Connection_reset ->
    raise (Api_errors.Server_error(Api_errors.pool_joining_host_connection_failed, []))

let join_common ~__context ~master_address ~master_username ~master_password ~force =
  (* I cannot join a Pool if I have HA already enabled on me *)
  let pool = List.hd (Db.Pool.get_all ~__context) in
  if Db.Pool.get_ha_enabled ~__context ~self:pool
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

  let master_address = Helpers.gethostbyname master_address in

  let rstr = Restrictions.get () in
  if rstr.Restrictions.enable_pooling then 
    begin
      let cluster_secret = ref "" in
      (* ... Can't use Helpers.call_api_functions here because we need to talk to host other than ourselves! *)
      let rpc = rpc master_address in
      
      (* read my non-shared SRs ready to copy to master *)
      let srs = Db.SR.get_records_where ~__context ~expr:Db_filter_types.True in
      let my_non_shared_srs = List.filter (fun (_,srec)->not srec.API.sR_shared) srs in

      let is_default_template trec =
	let other_config = trec.API.vM_other_config in
	(List.mem_assoc Xapi_globs.default_template_key other_config) &&
	  ((List.assoc Xapi_globs.default_template_key other_config) = "true") in
      
      (* read my (non-control-domain) VMs, VBDs and VIFs ready to copy to master *)
      let all_my_vm_recs = Db.VM.get_records_where ~__context ~expr:Db_filter_types.True in

      (* my_vms are the records I want to merge in the pool-join. It includes both vms and non-default templates: *)
      let my_vms = List.filter (fun (_,vmrec)->(not vmrec.API.vM_is_control_domain) && (not (is_default_template vmrec))) all_my_vm_recs in
      (* order the VM list to have the snapshots at the end. *)
      let my_vms = List.sort (fun (_,vm1) (_,vm2) -> if vm1.API.vM_is_a_snapshot then 1 else -1) my_vms in

      let my_running_vms = List.filter (fun (_,vmrec)->(vmrec.API.vM_power_state = `Running || vmrec.API.vM_power_state = `Paused)) my_vms in
      let my_suspended_vms = List.filter (fun (_,vmrec)->(vmrec.API.vM_power_state = `Suspended)) my_vms in
      let my_default_templates = List.filter (fun (_,vmrec)->vmrec.API.vM_is_a_template && (is_default_template vmrec)) all_my_vm_recs in
      let my_default_template_refs = List.map fst my_default_templates in
      let my_control_domains = List.filter (fun (_,vmrec)->vmrec.API.vM_is_control_domain) all_my_vm_recs in
      let my_control_domain_refs = List.map fst my_control_domains in

      let my_vifs = Db.VIF.get_records_where ~__context ~expr:Db_filter_types.True in
      let my_pifs = Db.PIF.get_records_where ~__context ~expr:Db_filter_types.True in
      let my_vdis = Db.VDI.get_records_where ~__context ~expr:Db_filter_types.True in

      let my_network_to_devname_map =
	List.map (fun (pref, prec) -> prec.API.pIF_network, prec.API.pIF_device) my_pifs in

      (* Get VBDs that aren't attached to default templates and aren't attached to control domains *)
      let my_vbds = Db.VBD.get_records_where ~__context ~expr:Db_filter_types.True in
      let my_vbds = List.filter
	(fun (_,vbdrec)->
	   not (List.mem vbdrec.API.vBD_VM my_default_template_refs) &&
	     not (List.mem vbdrec.API.vBD_VM my_control_domain_refs))
	my_vbds in

      let session_id =
      	try Client.Session.login_with_password rpc master_username master_password Xapi_globs.api_version_string
	with Xmlrpcclient.Http_request_rejected _ ->
	  raise (Api_errors.Server_error(Api_errors.pool_joining_host_service_failed, []))
      in

      let pool = List.hd (Client.Pool.get_all rpc session_id) in
      if Client.Pool.get_ha_enabled rpc session_id pool then begin
	error "Cannot join pool which already has HA enabled";
	raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));
      end;

      let create_or_get_host_on_master() =
	let me = Helpers.get_localhost ~__context in
	let my_uuid = Db.Host.get_uuid ~__context ~self:me in
	let existing_reference = try Some (Client.Host.get_by_uuid rpc session_id my_uuid) with _ -> None in
	match existing_reference with
	  None ->
	    debug "Found no suitable host record for me on master. Creating a new one with uuid='%s'" my_uuid;
	    let ref =
	      Client.Host.create ~rpc ~session_id
		~uuid:my_uuid
		~name_label:(Db.Host.get_name_label ~__context ~self:me)
		~name_description:(Db.Host.get_name_description ~__context ~self:me)
		~hostname:(Db.Host.get_hostname ~__context ~self:me)
		~address:(Db.Host.get_address ~__context ~self:me)
		~external_auth_type:(Db.Host.get_external_auth_type ~__context ~self:me)
		~external_auth_service_name:(Db.Host.get_external_auth_service_name ~__context ~self:me)
		~external_auth_configuration:(Db.Host.get_external_auth_configuration ~__context ~self:me) in
	    debug "Created new host record for me: ref='%s'" (Ref.string_of ref);
	    (* Copy other-config into newly created host record: *)
	    Client.Host.set_other_config ~rpc ~session_id ~self:ref ~value:(Db.Host.get_other_config ~__context ~self:me);
	    ref
	| Some r ->
	    debug "Found suitable host record for me already exists on master. Ref='%s'" (Ref.string_of r);
	    r
      in
      
      let create_sr_and_pbd_on_master host_ref s =
	let sref, srec = s in
	debug "Creating SR and PBD on master for SR: %s" srec.API.sR_uuid;
	(* Nb. since the joining xapi is certainly not pooled, there can be only one *)
	(* PBD linking the SR to the host *)
	let device_config = Db.PBD.get_device_config ~__context 
	  ~self:(List.hd (Db.SR.get_PBDs ~__context ~self:sref)) in
	let attempted_to_make_new_sr = ref false in
	let new_sr_ref =
	  try Client.SR.get_by_uuid rpc session_id srec.API.sR_uuid
	  with _ ->
	    attempted_to_make_new_sr := true;
	    Client.SR.introduce rpc session_id srec.API.sR_uuid srec.API.sR_name_label srec.API.sR_name_description
	      srec.API.sR_type srec.API.sR_content_type false srec.API.sR_sm_config in
	(* copy other-config into newly created sr record: *)
	Client.SR.set_other_config rpc session_id new_sr_ref srec.API.sR_other_config;
	if !attempted_to_make_new_sr then
	  ignore (Client.PBD.create rpc session_id host_ref new_sr_ref device_config [])
      in

      (* keep mapping of our VM uuids -> the ref of the equivalent VM we just made on the master *)
      let my_vm_uuid_to_new_ref_mapping = ref [] in
      (* keep mapping of our VM refs -> the ref of the equivalent VM we just made on the master *)
      let my_vm_ref_to_new_ref_mapping = ref [] in

      let create_vm_on_master (old_ref, vm) =
	let vm_ref = Client.VM.create_from_record rpc session_id vm in
	debug "Create VM %s on master" vm.API.vM_uuid;
	let lookup_table vm =
		if List.mem_assoc vm !my_vm_ref_to_new_ref_mapping then
			List.assoc vm !my_vm_ref_to_new_ref_mapping
		else
			Ref.null in
	if vm.API.vM_is_a_snapshot then begin
		debug "Updating snapshots metadata on master, from VM %s to VM %s" (Ref.string_of old_ref) (Ref.string_of vm_ref);
		Helpers.copy_snapshot_metadata ~lookup_table rpc session_id ~src_record:vm ~dst_ref:vm_ref
	end else
		my_vm_ref_to_new_ref_mapping := (old_ref, vm_ref) :: !my_vm_ref_to_new_ref_mapping;
	my_vm_uuid_to_new_ref_mapping := (vm.API.vM_uuid, vm_ref)::(!my_vm_uuid_to_new_ref_mapping) in

      let create_vbd_on_master vbd = Client.VBD.create_from_record rpc session_id vbd in
      let create_vif_on_master vif = Client.VIF.create_from_record rpc session_id vif in
      let create_vdi_on_master (vdi_ref, vdi) : API.ref_VDI =
	debug "Introducing VDI on master: %s" vdi.API.vDI_uuid;
	let r = Client.VDI.pool_introduce ~rpc ~session_id
	  ~uuid:vdi.API.vDI_uuid
	  ~name_label:vdi.API.vDI_name_label
	  ~name_description:vdi.API.vDI_name_description
	  ~sR:vdi.API.vDI_SR
	  ~_type:vdi.API.vDI_type
	  ~sharable:vdi.API.vDI_sharable
	  ~read_only:vdi.API.vDI_read_only
	  ~other_config:vdi.API.vDI_other_config
	  ~location:(Db.VDI.get_location ~__context ~self:vdi_ref)
	  ~xenstore_data:vdi.API.vDI_xenstore_data
	  ~sm_config:vdi.API.vDI_sm_config in
	debug "received reference: %s" (Ref.string_of r);
	r in
      let create_pif_on_master pif : API.ref_PIF =
	debug "Introducing PIF on master: %s" pif.API.pIF_uuid;
	let r = Client.PIF.pool_introduce ~rpc ~session_id
	  ~device:pif.API.pIF_device
	  ~network:pif.API.pIF_network
	  ~host:pif.API.pIF_host
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
	in
	debug "received reference: %s" (Ref.string_of r);
	r in
      let create_network_on_master device =
	debug "Creating new network on master";
	let r = Client.Network.pool_introduce ~rpc ~session_id
	  ~name_label:(Helpers.choose_network_name_for_pif device)
	  ~name_description:""
	  ~other_config:[]
	  ~bridge:(Xapi_pif.bridge_naming_convention device) in
	r in
      
      (* Take a VBD and remap it so it points to the equivalent VDI we just made on client-side, matching by uuid;
         and so it points to the equivalent VM we just made on the client-side, matching by uuid *)
      let remap_vbd client_vdi_recs vbd =
	try
	  begin
	    debug "Remapping VBD: %s" vbd.API.vBD_uuid;
	    let is_cd_vbd = vbd.API.vBD_type = `CD in
	    let my_vm = Db.VM.get_record ~__context ~self:vbd.API.vBD_VM in
	    let my_vm_uuid = my_vm.API.vM_uuid in
	    debug "Remapping VBD: searching for VM='%s' in client VM recs" my_vm_uuid;
	    let matching_client_vm_ref = List.assoc my_vm_uuid !my_vm_uuid_to_new_ref_mapping in

	    (* VDI ref might be null if the VBD is an empty CD drive, so catch that *)
	    try
	      let my_vdi = Db.VDI.get_record ~__context ~self:vbd.API.vBD_VDI in
	      let my_vdi_uuid = my_vdi.API.vDI_uuid in
	      debug "Remapping VBD: searching for VDI='%s' in client VDI recs" my_vdi_uuid;
	      let matching_client_vdi_ref, _ = List.find (fun (_,vdirec)->vdirec.API.vDI_uuid=my_vdi_uuid) client_vdi_recs in
	      Some {vbd with API.vBD_VDI=matching_client_vdi_ref; API.vBD_VM=matching_client_vm_ref }
	    with _ -> 
	      if is_cd_vbd then 
		(debug "Remapping VBD: VDI missing for CD VBD. Setting to empty";
		Some {vbd with API.vBD_empty=true; API.vBD_VDI = Ref.null; API.vBD_VM=matching_client_vm_ref })
	      else 
		(debug "VDI missing. VBD will not be created";
		 None)
	  end
	with _ -> 
	  debug "VM missing. VBD will not be recreated";
	  None
      in

      (* Try and remap my_sr_ref to matching client_sr_ref using client_sr_recs as a mapping; or throw an exception if could not find
	 my_sr_ref in map *)
      let remap_sr client_sr_recs my_sr_ref =
	let my_sr = Db.SR.get_record ~__context ~self:my_sr_ref in
	let my_sr_uuid = my_sr.API.sR_uuid in
	let matching_client_sr_ref, _ = List.find (fun (_,srrec)->srrec.API.sR_uuid=my_sr_uuid) client_sr_recs in
	matching_client_sr_ref in
      
      (* Take a VDI and remap it so its SR reference points to the equivalent SR we just made on the client-side, matching by uuid *)
      (* Since we haven't recreated shared SRs (e.g. XenSource Tools SR) on master, this will fail when we try to remap VDIs in a shared SR..
	 but since we don't want to recreate shared SRs or the VDIs contained therein on the master then that's great! ;) *)
      let remap_vdi client_sr_recs vdi =
	try
	  debug "Remapping VDI: %s" vdi.API.vDI_uuid;
	  let my_sr_ref = vdi.API.vDI_SR in
	  let matching_client_sr_ref = remap_sr client_sr_recs my_sr_ref in
	  Some {vdi with API.vDI_SR=matching_client_sr_ref}
	with _ -> None in

      (* Take a VIF and remap it so it points to the equivalent network on client-side, matching by PIF.device-name;
	 and so it points to the equivalent VM we just made on the client-side, matching by uuid *)
      let remap_vif client_devname_to_network_map vif =
	try
	  debug "Remapping VIF: %s" (vif.API.vIF_uuid);
	  let my_vm = Db.VM.get_record ~__context ~self:vif.API.vIF_VM in
	  let my_vm_uuid = my_vm.API.vM_uuid in
	  let matching_client_vm_ref = List.assoc my_vm_uuid !my_vm_uuid_to_new_ref_mapping in
	  debug "New VM reference: %s" (Ref.string_of matching_client_vm_ref);
	  let my_network = vif.API.vIF_network in
	  let my_network_devname = List.assoc my_network my_network_to_devname_map in
	  debug "Network devname = %s" my_network_devname;
	  let new_network = List.assoc my_network_devname client_devname_to_network_map in
	  debug "New network ref = %s" (Ref.string_of new_network);
	  Some {vif with API.vIF_VM=matching_client_vm_ref; API.vIF_network=new_network}
	with _ -> None in

      (* Create a physical PIF record on the master and, if there's already a PIF on the master with the same
	 'device' name, then attach the newly created pif to the same network; otherwise create a new network and attach
	 newly created PIF to that.
      *)
      let client_pifs = Client.PIF.get_all_records_where rpc session_id "true" in
      let create_pif_and_maybe_network hostref pif =
	let my_pif_record = Db.PIF.get_record ~__context ~self:pif in
	debug "Copying PIF record to master: %s" my_pif_record.API.pIF_uuid;
	let my_pif_device_name = my_pif_record.API.pIF_device in
	let my_pif_vlan_tag = my_pif_record.API.pIF_VLAN in
	(* if there's a pif with the same device (name) and VLAN tag on the master then we use that network;
	   otherwise we make a new network for this pif *)
	let master_pifs_with_same_name = 
	  List.filter (fun (_,master_pif) -> 
	    (master_pif.API.pIF_device = my_pif_device_name) && (master_pif.API.pIF_VLAN = my_pif_vlan_tag)) client_pifs in
	let master_network_to_use =
	  match master_pifs_with_same_name with
	    [] -> (* nothing suitable here, so make new network on master *)
	      debug "Master does not have corresponding PIF, creating new network";
	      create_network_on_master my_pif_device_name
	  | (_,mpiff)::_ -> (* master has pif with same name so we re-use that network *)
	      debug "Master has corresponding pif; re-using master's network accordingly";
	      mpiff.API.pIF_network in
	(* remap network field and host field in my_pif_record and then go and create it on the master *)
	let my_pif_record =
	  {my_pif_record with
	     API.pIF_network=master_network_to_use;
	     API.pIF_host=hostref
	  } in
	create_pif_on_master my_pif_record in

      let map_opt f l =
	let rec doit l acc =
	  match l with
	    [] -> acc
	  | (x::xs) ->
	      (match f x with
		 None -> doit xs acc
	       | Some r -> doit xs (r::acc)) in
	doit l [] in

      (* Pre-join asserts *)

      (* CA-26975: Pool restrictions (implied by pool_sku) MUST match *)
      let assert_restrictions_match () = 
	let pool_license_params = List.map (fun (_, host_r) -> host_r.API.host_license_params) (Client.Host.get_all_records ~rpc ~session_id) in
	let pool_restrictions = Restrictions.pool_restrictions_of_list (List.map Restrictions.of_assoc_list pool_license_params) in
	let my_restrictions = Restrictions.get() in
	if pool_restrictions <> my_restrictions then begin
	  error "Pool.join failed because of license restrictions mismatch";
	  error "Remote has %s" (Restrictions.to_compact_string pool_restrictions);
	  error "Local has  %s" (Restrictions.to_compact_string my_restrictions);
	  raise (Api_errors.Server_error(Api_errors.license_restriction, []))
	end in

      let assert_external_auth_matches master = 
        (* CP-700: Restrict pool.join if AD configuration of slave-to-be does not match *)
        (* that of master of pool-to-join *)
        let slavetobe = Helpers.get_localhost ~__context in
        let slavetobe_auth_type = Db.Host.get_external_auth_type ~__context ~self:slavetobe in
        let slavetobe_auth_service_name = Db.Host.get_external_auth_service_name ~__context ~self:slavetobe in
        let master_auth_type = Client.Host.get_external_auth_type ~rpc ~session_id ~self:master in
        let master_auth_service_name = Client.Host.get_external_auth_service_name ~rpc ~session_id ~self:master in
        begin
          debug "Verifying if external auth configuration of master %s (auth_type=%s service_name=%s) matches that of slave-to-be %s (auth-type=%s service_name=%s)" 
            (Client.Host.get_name_label ~rpc ~session_id ~self:master) master_auth_type master_auth_service_name 
            (Db.Host.get_name_label ~__context ~self:slavetobe) slavetobe_auth_type slavetobe_auth_service_name;
          if (slavetobe_auth_type <> master_auth_type)
             || (slavetobe_auth_service_name <> master_auth_service_name) then
          begin
           error "Cannot join pool whose external authentication configuration is different";
           raise (Api_errors.Server_error(Api_errors.pool_joining_external_auth_mismatch, []))
          end
        end in

      let assert_i_know_of_no_other_hosts () =
	let hosts = Db.Host.get_all ~__context in
	if List.length hosts > 1 then
	  raise (Api_errors.Server_error(Api_errors.pool_joining_host_cannot_be_master_of_other_hosts, [])) in

      let assert_no_running_or_suspended_vms_on_me () =
	if List.length my_running_vms > 0 || List.length my_suspended_vms > 0 then
	  raise (Api_errors.Server_error(Api_errors.pool_joining_host_cannot_have_running_or_suspended_VMs, [])) in

      let assert_no_vms_with_current_ops () =
	let vms_with_current_ops =
	  List.filter (fun (_,vmr) -> (List.length vmr.API.vM_current_operations)>0 ) my_vms in
	if List.length vms_with_current_ops > 0 then
	  raise (Api_errors.Server_error(Api_errors.pool_joining_host_cannot_have_vms_with_current_operations, [])) in
      
      let assert_no_shared_srs_on_me () =
	let srs = Db.SR.get_records_where ~__context ~expr:Db_filter_types.True in
	let my_shared_srs = List.filter (fun (_,srec)->srec.API.sR_shared) srs in
	let my_shared_srs = List.filter (fun (sr,_)->not(Helpers.is_tools_sr ~__context ~sr)) my_shared_srs in
	if List.length my_shared_srs > 0 then
	  raise (Api_errors.Server_error(Api_errors.pool_joining_host_cannot_contain_shared_SRs, [])) in

      let assert_management_interface_is_physical () =
	let pifs = Db.PIF.get_records_where ~__context ~expr:Db_filter_types.True in
	List.iter (fun (_,pifr)->
		     if pifr.API.pIF_management && (not pifr.API.pIF_physical) then
		       raise (Api_errors.Server_error(Api_errors.pool_joining_host_must_have_physical_managment_nic, []))) pifs in

      let assert_hosts_homogeneous master_ref =
	let me = Helpers.get_localhost ~__context in

	(* read local and remote cpu records *)
	let master = Client.Host.get_record ~rpc ~session_id ~self:master_ref in
	let master_cpu_recs = Client.Host_cpu.get_all_records_where ~rpc ~session_id ~expr:"true" in
	let master_cpus = List.map (fun cpu -> List.assoc cpu master_cpu_recs) master.API.host_host_CPUs in
	let master_software_version = master.API.host_software_version in

	let my_cpu_refs = Db.Host.get_host_CPUs ~__context ~self:me in
	let my_cpus = List.map (fun cpu -> Db.Host_cpu.get_record ~__context ~self:cpu) my_cpu_refs in
	let my_software_version = Db.Host.get_software_version ~__context ~self:me in

	(* CA-29511 filter irrelevant CPU flags *)
	let irrelevant_cpu_flags = [ "est" (* Enhanced Speed Step *) ] in
	let cpu_flags_of_string x = Stringext.String.split_f Stringext.String.isspace x in
	let string_of_cpu_flags x = String.concat " " x in

	let filtered_cpu_flags x = string_of_cpu_flags (List.filter (fun x -> not (List.mem x irrelevant_cpu_flags)) (cpu_flags_of_string x)) in

	let get_comparable_fields hcpu =
	  let raw_flags = hcpu.API.host_cpu_flags in
	  let filtered_flags = filtered_cpu_flags raw_flags in
	  (hcpu.API.host_cpu_vendor, hcpu.API.host_cpu_model, hcpu.API.host_cpu_family, filtered_flags) in
	let print_cpu_rec (vendor,model,family,flags) =
	  debug "%s, %Ld, %Ld, %s" vendor model family flags in
	let get_software_version_fields fields =
	  begin try List.assoc "product_version" fields with _ -> "" end,
	  begin try List.assoc "product_brand" fields with _ -> "" end,
	  begin try List.assoc "build_number" fields with _ -> "" end,
	  begin try List.assoc "hg_id" fields with _ -> "" end,
	  begin try List.assoc Xapi_globs.linux_pack_vsn_key fields with _ -> "not present" end
	in
	let print_software_version (version,brand,number,id,linux_pack) =
	  debug "version:%s, brand:%s, build:%s, id:%s, linux_pack:%s" version brand number id linux_pack
	in
	
	let is_subset s1 s2 = List.fold_left (&&) true (List.map (fun s->List.mem s s2) s1) in
	let set_equiv s1 s2 = (is_subset s1 s2) && (is_subset s2 s1) in

	let my_cpus_compare = List.map get_comparable_fields my_cpus in
	let master_cpus_compare = List.map get_comparable_fields master_cpus in

	let my_software_compare = get_software_version_fields my_software_version in
	let master_software_compare = get_software_version_fields master_software_version in

	debug "Pool pre-join Software homogeneity check:";
	debug "Slave software:";
	print_software_version my_software_compare;
	debug "Master software:";
	print_software_version master_software_compare;

	debug "Pool pre-join CPU homogeneity check:";
	debug "Slave cpus:";
	List.iter print_cpu_rec my_cpus_compare;
	debug "Master cpus:";
	List.iter print_cpu_rec master_cpus_compare;

	if my_software_compare <> master_software_compare then
	  raise (Api_errors.Server_error(Api_errors.pool_hosts_not_homogeneous,["software version differs"]));
	if not (set_equiv my_cpus_compare master_cpus_compare) then
	  raise (Api_errors.Server_error(Api_errors.pool_hosts_not_homogeneous,["cpus differ"])) in

      Pervasiveext.finally
	(fun () ->

	   let pool = List.hd (Client.Pool.get_all rpc session_id) in
	   let master = Client.Pool.get_master rpc session_id pool in
	   let master_uuid = Client.Host.get_uuid rpc session_id master in
	   let me = Helpers.get_localhost ~__context in
	   let my_uuid = Db.Host.get_uuid ~__context ~self:me in
	   let assert_not_joining_myself() =
	     if master_uuid=my_uuid then
	       raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["Host cannot become slave of itself"])) in

	   (* call pre-join asserts *)
	   assert_not_joining_myself();
	   assert_i_know_of_no_other_hosts();
	   assert_no_running_or_suspended_vms_on_me();
	   assert_no_vms_with_current_ops();
	   if (not force) then assert_hosts_homogeneous master;
	   assert_no_shared_srs_on_me();
	   assert_management_interface_is_physical ();
	   assert_external_auth_matches(master);
	   assert_restrictions_match ();
	   (* get hold of cluster secret - this is critical; if this fails whole pool join fails *)
	   (* Note: this is where the license restrictions are checked on the other side.. if we're trying to join
	      a host that does not support pooling then an error will be thrown at this stage *)
	   cluster_secret := Client.Pool.initial_auth rpc session_id;

	   (* get pool db from new master so I have a backup ready if we failover to me -- if 
	      this fails the whole join operation fails.
	      CA-22449: this is where a --force'd pool.join across differing product versions
	      will fail because the database schema has the wrong version. *)
	   begin
	     try
	       Pool_db_backup.fetch_database_backup ~master_address ~pool_secret:!cluster_secret ~force:None
	     with Api_errors.Server_error(code, _) when code = Api_errors.restore_incompatible_version ->
	       raise (Api_errors.Server_error(Api_errors.pool_joining_host_must_have_same_product_version, []))
	   end;
	   
	   (* this is where we try and sync up as much state as we can
	      with the master. This is "best effort" rather than
	      critical; if we fail part way through this then we carry
	      on with the join *)
	   try
	     let hostref = create_or_get_host_on_master() in
	     (* Create SRs and PBDs *)
	     List.iter (no_exn (create_sr_and_pbd_on_master hostref)) my_non_shared_srs;
	     (* Create VMs *)
	     ignore (List.map (no_exn create_vm_on_master) my_vms);
	     
	     (* Remap VDI.SR fields to newly created SR records [matching on uuid] *)
	     let client_sr_recs = Client.SR.get_all_records_where rpc session_id "true" in
	     let vdis_for_master =
	       map_opt
		 (fun (vref,vrec)->
		    match remap_vdi client_sr_recs vrec with
		      None -> None
		    | Some r -> Some (vref, r)) my_vdis in
	     (* Create VDIs *)
	     ignore (List.map (no_exn create_vdi_on_master) vdis_for_master);
	     
	     (* Remap VBDs to newly created VDI/VM records [matching on uuid] *)
	     let client_vdi_recs = Client.VDI.get_all_records_where rpc session_id "true" in
	     let vbds_for_master = map_opt (remap_vbd client_vdi_recs) (List.map snd my_vbds) in
	     (* Create VBDs *)
	     ignore (List.map (no_exn create_vbd_on_master) vbds_for_master);

	     (* Create physical PIFs and either create new or join up networks *)
	     let my_pif_refs = Db.PIF.get_all ~__context in
	     let my_physical_pifs = List.filter (fun pif -> Db.PIF.get_physical ~__context ~self:pif) my_pif_refs in
	     ignore (List.map (no_exn (create_pif_and_maybe_network hostref)) my_physical_pifs);
	     
	     (* Remap VIFs to master networks [matching on network device name] *)
	     let client_devname_to_network_map =
	       List.map (fun (pref, prec) -> prec.API.pIF_device, prec.API.pIF_network) client_pifs in
	     let vifs_for_master = map_opt (remap_vif client_devname_to_network_map) (List.map snd my_vifs) in
	     (* Create VIFs *)
	     ignore (List.map (no_exn create_vif_on_master) vifs_for_master);
	     
	     (* Set default crashdump and suspend-sr fields on newly created host *)
	     let my_crashdump_sr = Db.Host.get_crash_dump_sr ~__context ~self:me in
	     let my_suspend_image_sr = Db.Host.get_crash_dump_sr ~__context ~self:me in
	     
	     no_exn (fun () -> Client.Host.set_suspend_image_sr ~rpc ~session_id ~self:hostref ~value:(remap_sr client_sr_recs my_crashdump_sr)) ();
	     no_exn (fun () -> Client.Host.set_crash_dump_sr ~rpc ~session_id ~self:hostref ~value:(remap_sr client_sr_recs my_suspend_image_sr)) ()
	       
	   with
	   | e ->
	       debug "Error whilst importing db objects into master; aborted: %s" (Printexc.to_string e);
	       warn "Error whilst importing db objects to master. The pool-join operation will continue, but some of the slave's VMs may not be available on the master."
	)
	(fun () -> Client.Session.logout rpc session_id);
      Unixext.write_string_to_file Xapi_globs.pool_secret_path !cluster_secret;
      
      (* Here we dump our dom0 memory settings to the local db so that they'll be picked up by create_misc when we come to create our new control domain record *)
      let dom0 = Helpers.get_domain_zero ~__context in
      Localdb.put (Constants.pool_join_mem_stat_min) (Int64.to_string (Db.VM.get_memory_static_min ~__context ~self:dom0));
      Localdb.put (Constants.pool_join_mem_stat_max) (Int64.to_string (Db.VM.get_memory_static_max ~__context ~self:dom0));
      Localdb.put (Constants.pool_join_mem_dyn_min) (Int64.to_string (Db.VM.get_memory_dynamic_min ~__context ~self:dom0));
      Localdb.put (Constants.pool_join_mem_dyn_max) (Int64.to_string (Db.VM.get_memory_dynamic_max ~__context ~self:dom0));
      Localdb.put (Constants.pool_join_mem_target) (Int64.to_string (Db.VM.get_memory_target ~__context ~self:dom0));

      Pool_role.set_role (Pool_role.Slave master_address);
      Xapi_fuse.light_fuse_and_run()
    end
  else
    begin
      L.error "License does not allow pooling";
      raise (Api_errors.Server_error (Api_errors.license_restriction, []))
    end

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
  let pool = List.hd (Db.Pool.get_all ~__context) in
  if Db.Pool.get_ha_enabled ~__context ~self:pool
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

  if Pool_role.is_master () then raise Cannot_eject_master
  else
    begin
      (* Fail the operation if any VMs are running here (except control domains) *)
      let my_vms_with_records = Db.VM.get_records_where ~__context ~expr:(Eq(Field "resident_on", Literal (Ref.string_of host))) in
      List.iter (fun (_, x) -> 
		   if (not x.API.vM_is_control_domain) && x.API.vM_power_state<>`Halted
		   then begin
		     error "VM uuid %s not in Halted state and resident_on this host" (x.API.vM_uuid);
		     raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["VM resident on host"]))
		   end) my_vms_with_records;

      debug "Pool.eject: unplugging PBDs";
      (* unplug all my PBDs; will deliberately fail if any unplugs fail *)
      unplug_pbds ~__context host;

      debug "Pool.eject: disabling external authentication in slave-to-be-ejected";
      (* disable the external authentication of this slave being ejected *)
      (* this call will return an exception if something goes wrong *)
      Xapi_host.disable_external_auth_common ~during_pool_eject:true ~__context ~host 
        ~config:[]; (* FIXME: in the future, we should send the windows AD admin/pass here *)
                    (* in order to remove the slave from the AD database during pool-eject *)

      debug "Pool.eject: deleting Host record (the point of no return)";
      (* delete me from the database - this will in turn cause PBDs and PIFs to be GCed *)
      Db.Host.destroy ~__context ~self:host;

      (* and destroy my control domain, since you can't do this from the API [operation not allowed] *)
      begin
	try
	  let my_control_domain = List.find (fun x->x.API.vM_is_control_domain) (List.map snd my_vms_with_records) in
	  Db.VM.destroy ~__context ~self:(Db.VM.get_by_uuid ~__context ~uuid:my_control_domain.API.vM_uuid)
	with _ -> ()
      end;
      debug "Pool.eject: setting our role to be master";
      Pool_role.set_role Pool_role.Master;
      debug "Pool.eject: forgetting pool secret";
      Unixext.unlink_safe Xapi_globs.pool_secret_path; (* forget current pool secret *)
      (* delete backup databases and any temporary restore databases *)
      Unixext.unlink_safe Xapi_globs.backup_db_xml;
      Unixext.unlink_safe Xapi_globs.db_temporary_restore_path;
      (* delete /local/ databases specified in the db.conf, so they get recreated on restart.
	 We must leave any remote database alone because these are owned by the pool and
	 not by this node. *)
      (* get the slave backup lock so we know no more backups are going to be taken -- we keep this lock till the
	 bitter end, where we restart below ;)
      *)
      Mutex.lock Pool_db_backup.slave_backup_m;
      finally
	(fun () ->
	   let dbs = Parse_db_conf.parse_db_conf Xapi_globs.db_conf_path in
	   (* We need to delete all local dbs but leave remote ones alone *)
	   let local = List.filter (fun db -> not db.Parse_db_conf.is_on_remote_storage) dbs in
	   List.iter Unixext.unlink_safe (List.map (fun db->db.Parse_db_conf.path) local);
	   List.iter Unixext.unlink_safe (List.map Generation.gen_count_file local);
	   (* remove any shared databases from my db.conf *)
	   (* XXX: on OEM edition the db.conf is rebuilt on every boot *)
	   Parse_db_conf.write_db_conf local;
	   (* Forget anything we know about configured remote databases: this prevents
	      any initscript reminding us about them after reboot *)
	   Helpers.log_exn_continue
	     (Printf.sprintf "Moving remote database file to backup: %s"
		Xapi_globs.remote_db_conf_fragment_path)
	     (fun () ->
		Unix.rename 
		  Xapi_globs.remote_db_conf_fragment_path
		  (Xapi_globs.remote_db_conf_fragment_path ^ ".bak")) ()
	)
	(fun () -> Xapi_fuse.light_fuse_and_reboot_after_eject())
    end

(* Prohibit parallel flushes since they're so expensive *)
let sync_m = Mutex.create ()

let sync_database ~__context = 
  Mutex.execute sync_m
    (fun () ->
       (* If HA is enabled I'll first try to flush to the LUN *)
       let pool = Helpers.get_pool ~__context in
       let flushed_to_vdi = Db.Pool.get_ha_enabled ~__context ~self:pool && (Xha_metadata_vdi.flush_database ~__context) in
       if flushed_to_vdi
       then debug "flushed database to metadata VDI: assuming this is sufficient."
       else begin
	 debug "flushing database to all online nodes";
	 Threadext.thread_iter
	   (fun host ->
	      Helpers.call_api_functions ~__context
		(fun rpc session_id -> Client.Host.request_backup rpc session_id host (Generation.read_generation()) true))
	   (Db.Host.get_all ~__context)
       end
    )	 

(* This also means me, since call will have been forwarded from the current master *)
let designate_new_master ~__context ~host =
	if not (Pool_role.is_master ()) then begin
		let pool = Helpers.get_pool ~__context in
		if Db.Pool.get_ha_enabled ~__context ~self:pool
		then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

		sync_database ~__context;

		let all_hosts = Db.Host.get_all ~__context in
		(* We make no attempt to demand a quorum or anything. *)
		let addresses = List.map (fun self -> Db.Host.get_address ~__context ~self) all_hosts in
		let my_address = Db.Host.get_address ~__context ~self:(Helpers.get_localhost ()) in
		let peers = List.filter (fun x -> x <> my_address) addresses in
		Xapi_pool_transition.attempt_two_phase_commit_of_new_master ~__context true peers my_address
	end

let initial_auth ~__context =
  let rstr = Restrictions.get () in
  if rstr.Restrictions.enable_pooling then
    !Xapi_globs.pool_secret
  else
    raise (Api_errors.Server_error(Api_errors.license_restriction,[]))

(** This call is used during master startup so we should check to see whether we need to re-establish our database
    connection and resynchronise lost database state i.e. state which is non-persistent or reverted over a master crash *)
let is_slave ~__context ~host = 
  let is_slave = not (Pool_role.is_master ()) in
  info "Pool.is_slave call received (I'm a %s)" (if is_slave then "slave" else "master");
  debug "About to kick the database connection to make sure it's still working...";
  Db_cache.DBCache.is_valid_ref "Pool.is_slave checking to see if the database connection is up";
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

	(* Cancel tasks on behalf of slave *)
	debug "Hello message from slave OK: cancelling tasks on behalf of slave";
	Cancel_tasks.cancel_tasks_on_host ~__context ~host_opt:(Some host_ref);

	(* Make sure we mark this host as live again *)
	Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
	  (fun () -> Xapi_globs.hosts_which_are_shutting_down := List.filter (fun x -> x <> host_ref) !Xapi_globs.hosts_which_are_shutting_down);	

	(* Update the heartbeat timestamp for this host so we don't mark it as 
	   offline in the next db_gc *)
	Db_gc.tickle_heartbeat ~__context host_ref [];
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
		      let metrics = Db.PIF.get_metrics ~__context ~self:pif in
		      Db.PIF_metrics.destroy ~__context ~self:metrics;
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
	   (fun vlan -> try Client.VLAN.destroy rpc session_id vlan with _ -> ()) vlans) in
  (* Read the network that the pif is attached to; get the list of all pifs on that network
     -- that'll be the pif for each host that we want to make the vlan on. Then go and make
     the vlan on all these pifs. Then attempt to do a best-effort plug of the newly created pifs
     in order to satisfy ca-22381 *)
  let network_to_get_pifs_from = Db.PIF.get_network ~__context ~self:pif in
  let pifs_on_network = Db.Network.get_PIFs ~__context ~self:network_to_get_pifs_from in
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
	   )
	   pifs_on_network in
       let vlan_pifs = List.map (fun vlan -> Db.VLAN.get_untagged_PIF ~__context ~self:vlan) vlans in
       (* CA-22381: best-effort plug of the newly-created VLAN PIFs. Note if any of these calls fail
	  then nothing is rolled-back and the system will be left with some unplugged VLAN PIFs, which may
	  confuse the HA agility calculation (but nothing else since everything else can plug on demand) *)
       List.iter (fun pif -> Helpers.log_exn_continue (Printf.sprintf "Plugging VLAN PIF %s" (Ref.string_of pif)) (fun () -> Client.PIF.plug rpc session_id pif) ()) vlan_pifs;
       vlan_pifs)

let slave_network_report ~__context ~phydevs ~dev_to_mac ~dev_to_mtu ~slave_host =
  []
(*
  Dbsync_slave.create_physical_networks ~__context phydevs dev_to_mac dev_to_mtu slave_host
*)

(* Let's only process one enable/disable at a time. I would have used an allowed_operation for this but
   it would involve a datamodel change and it's too late for Orlando. *)
let enable_disable_m = Mutex.create ()
let enable_ha ~__context ~heartbeat_srs ~configuration = Mutex.execute enable_disable_m (fun () -> Xapi_ha.enable __context heartbeat_srs configuration)
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
      then Xapi_alert.add ~name:Api_messages.ha_pool_drop_in_plan_exists_for ~priority:1L ~cls:`Pool ~obj_uuid:(Db.Pool.get_uuid ~__context ~self:pool) ~body:(Int64.to_string n);
    end;
  end;
  n

let ha_compute_hypothetical_max_host_failures_to_tolerate ~__context ~configuration = 
  (* Check the restart priorities all look valid *)
  let valid_priorities = [ "1"; "2"; "3"; Constants.ha_restart_best_effort ] in
  List.iter (fun (_, pri) -> 
	       if not(List.mem pri valid_priorities)
	       then raise (Api_errors.Server_error(Api_errors.invalid_value, [ "ha_restart_priority"; pri ]))) configuration;

  let protected_vms = List.map fst (List.filter (fun (vm, priority) -> Xapi_ha_vm_failover.vm_should_always_run true priority) configuration) in
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
	  try Helpers.vm_assert_agile ~__context ~self; [ self, [ "error_code", Api_errors.host_not_enough_free_memory ] ] (* default *) 
	  with Api_errors.Server_error(code, params) -> [ self, [ "error_code", code ]]) failed_vms) in
  let plan = List.map (fun (vm, host) -> vm, [ "host", Ref.string_of host ]) 
    (Xapi_ha_vm_failover.compute_evacuation_plan ~__context (List.length all_hosts) live_hosts vms) in
  (List.filter (fun (vm, _) -> not(List.mem_assoc vm plan)) errors) @ plan

let create_new_blob ~__context ~pool ~name ~mime_type =
  let blob = Xapi_blob.create ~__context ~mime_type in
  Db.Pool.add_to_blobs ~__context ~self:pool ~key:name ~value:blob;
  blob

let set_ha_host_failures_to_tolerate ~__context ~self ~value = 
  if value < 0L then raise (Api_errors.Server_error(Api_errors.invalid_value, [ "ha_host_failures_to_tolerate"; Int64.to_string value ]));

  (* Don't block changes if we have no plan at all *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_plan_exists_for ~__context ~self:pool > 0L
  then Xapi_ha_vm_failover.assert_nfailures_change_preserves_ha_plan ~__context (Int64.to_int value);
  Db.Pool.set_ha_host_failures_to_tolerate ~__context ~self ~value;
  let (_: bool) = Xapi_ha_vm_failover.update_pool_status ~__context in ()

let ha_schedule_plan_recomputation ~__context = 
  Xapi_ha.Monitor.plan_out_of_date := true

let call_fn_on_hosts ~__context f =
  let hosts = Db.Host.get_all ~__context in
  Helpers.call_api_functions ~__context (fun rpc session_id -> 
    let errs = List.fold_left 
      (fun acc host -> 
	try
	  f ~rpc ~session_id ~host;
	  acc
	with x -> 
	  (host,x)::acc) [] hosts
    in
    if List.length errs > 0 then begin
      warn "Exception raised while performing operation on hosts:";
      List.iter (fun (host,x) -> warn "Host: %s error: %s" (Ref.string_of host) (ExnHelper.string_of_exn x)) errs;
      raise (snd (List.hd errs))
    end)

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
  call_fn_on_hosts ~__context Client.Host.enable_binary_storage

let disable_binary_storage ~__context =
  call_fn_on_hosts ~__context Client.Host.disable_binary_storage

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

let get_master_slaves_list_with_fn ~__context fn =
	let _unsorted_hosts = Db.Host.get_all ~__context in
	let pool = List.hd (Db.Pool.get_all ~__context) in
	let master = Db.Pool.get_master ~__context ~self:pool in
	let slaves = List.filter (fun h -> h <> master) _unsorted_hosts in (* anything not a master *)
	debug "MASTER=%s, SLAVES=%s" (Db.Host.get_name_label ~__context ~self:master)
		(List.fold_left (fun str h -> (str^","^(Db.Host.get_name_label ~__context ~self:h))) "" slaves);
	fn master slaves
	
(* returns the list of hosts in the pool, with the master being the first element of the list *)
let get_master_slaves_list ~__context =
	get_master_slaves_list_with_fn ~__context (fun master slaves -> master::slaves)

(* returns the list of slaves in the pool *)
let get_slaves_list ~__context =
	get_master_slaves_list_with_fn ~__context (fun master slaves -> slaves)

(* CP-719: Enables external auth/directory service across a whole pool; *)
(* calling Host.enable_external_auth with the specified parameters in turn on each of the hosts in the pool
    * The call fails immediately if any of the pool hosts already have external auth enabled (must disable first)
    * If a call to a single host to enable external auth fails, then Pool.enable_external_auth fails, and there is
      a best-effort attempt to disable any hosts who had their external auth succesfully enabled before the failure occured
*)
let enable_external_auth ~__context ~pool ~config ~service_name ~auth_type = 

	(* CP-825: Serialize execution of pool-enable-extauth and pool-disable-extauth *)
	(* enabling/disabling the pool's extauth at the same time could produce inconsistent states for extauth in each host of the pool *)
	Mutex.execute Xapi_globs.serialize_pool_enable_disable_extauth (fun () ->

	(* the first element in the hosts list needs to be the pool's master, because we *)
	(* always want to update first the master's record due to homogeneity checks in CA-24856 *)
	let hosts = get_master_slaves_list ~__context in

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
			| "AUTH_UNKNOWN_TYPE" ->
				raise (Api_errors.Server_error(Api_errors.auth_unknown_type, [msg_of_e]))
			| "AUTH_ENABLE_FAILED" ->
				raise (Api_errors.Server_error(Api_errors.pool_auth_enable_failed, [(Ref.string_of failed_host);msg_of_e]))
			| _ -> (* Api_errors.Server_error *)
				raise (Api_errors.Server_error(Api_errors.pool_auth_enable_failed, [(Ref.string_of failed_host);string_of_e]))
	end

	else begin (* OK *)
		debug "External authentication enabled for all hosts in the pool"
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
	let hosts = get_master_slaves_list ~__context in
	let host_msgs_list =
		List.map (fun host ->
			try	(* forward the call to the host in the pool *)
				call_fn_on_host ~__context (Client.Host.disable_external_auth ~config) host;
				(* no failed host to add to the filtered list, just visit next host *)
				(host,"")
			with 
			| Api_errors.Server_error (err,[host_msg]) as e -> begin
				let msg = (Printf.sprintf "%s: %s" 
					(Db.Host.get_name_label ~__context ~self:host) host_msg) in
				debug "Failed to disable the external authentication of pool in host %s" msg;
				(* no exception should be raised here, we want to visit every host in hosts *)
				(host,msg)
				end
			| e-> (* add failed host to the filtered list and visit next host *)
				let msg = (Printf.sprintf "%s: %s" 
					(Db.Host.get_name_label ~__context ~self:host) (ExnHelper.string_of_exn e)) in
				debug "Failed to disable the external authentication of pool in host %s" msg;
				(* no exception should be raised here, we want to visit every host in hosts *)
				(host,msg)
			) 
		hosts
	in
	let failedhosts_list = List.filter (fun (host,msg) -> msg<>"") host_msgs_list in 
	if (List.length failedhosts_list > 0)
	then begin (* FAILED *)
		match List.hd failedhosts_list with (host,msg) ->
		debug "Failed to disable the external authentication of at least one host in the pool";
		raise (Api_errors.Server_error(Api_errors.pool_auth_disable_failed, [(Ref.string_of host);msg]));
	end
	else begin (* OK *)
		debug "The external authentication of all hosts in the pool was disabled successfully";
	end
	)
	
(* CA-24856: detect non-homogeneous external-authentication config in pool *)
let detect_nonhomogeneous_external_auth_in_pool ~__context =
	Helpers.call_api_functions ~__context (fun rpc session_id ->
		let slaves = get_slaves_list ~__context in
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
