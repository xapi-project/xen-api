(*
 * Copyright (c) 2006,2007 XenSource Inc.
 * Author: David Scott <david.scott@xensource.com>
 *
 * Code to output a subset of database records, marshalled in XMLRPC format
 *)

(* The general plan:
 *
 * 1. Walk around the database and select the objects you want (see 'create_table')
 *    and make a table mapping internal ref -> fresh external references. It would
 *    be nice to generate a visitor thingimy for this.
 * 
 * 2. Select all the objects from each class, filter the subset you want (ie those whose
 *    reference exists as a key in the table) and convert them into instances of the 
 *    intermediate record 'type obj' via the functions make_{vm,sr,vbd,vif,network}.
 *
 *    The created 'obj record' includes the class name as a string (from the datamodel), 
 *    the fresh reference and the output of 'get_record' marshalled using the standard 
 *    XMLRPC functions with all the references converted either to the fresh external refs
 *    or NULL (so we aim not to export dangling pointers)
 * 
 * 3. Write out one big XML file containing an XMLRPC struct which has keys:
 *    version -> a structure of system version info (API versions, internal build numbers)
 *    state -> an XMLRPC array of XMLRPC serialised 'obj' records (see 'xmlrpc_of_obj')
 *)

(* The specific plan for VM export:
   Walk over the datamodel and mark VIFs, Networks connected to the VIFs, VBDs, VDIs connected
   to the VBDs, SRs connected to the VDIs (and maybe a suspend image?). *)

open Importexport
open Pervasiveext

module D=Debug.Debugger(struct let name="export" end)
open D


let make_id = 
  let counter = ref 0 in
  fun () ->
    let this = !counter in
    incr counter;
    "Ref:" ^ (string_of_int this)

let rec update_table ~__context ~include_snapshots ~table vm =
  let add r = 
	  if not (Hashtbl.mem table (Ref.string_of r)) then
		  Hashtbl.add table (Ref.string_of r)(make_id ()) in
 
  if Db.is_valid_ref vm && not (Hashtbl.mem table (Ref.string_of vm)) then begin
  add vm;
  let vm = Db.VM.get_record ~__context ~self:vm in
  List.iter 
	(fun vif -> if Db.is_valid_ref vif then begin
	       add vif;
	       let vif = Db.VIF.get_record ~__context ~self:vif in
	       add vif.API.vIF_network end) 
	vm.API.vM_VIFs;
  List.iter 
	(fun vbd -> if Db.is_valid_ref vbd then begin
	       add vbd;
	       let vbd = Db.VBD.get_record ~__context ~self:vbd in
	       if not(vbd.API.vBD_empty)
	       then
		 let vdi = vbd.API.vBD_VDI in
		 add vdi;
		 let vdi = Db.VDI.get_record ~__context ~self:vdi in
		 add vdi.API.vDI_SR end) 
	vm.API.vM_VBDs;
  (* If we need to include snapshots, update the table for VMs in the 'snapshots' field *) 
  if include_snapshots then
	  List.iter 
		  (fun snap -> update_table ~__context ~include_snapshots:false ~table snap)
		  vm.API.vM_snapshots;
  (* If VM is suspended then add the suspend_VDI *)
  let vdi = vm.API.vM_suspend_VDI in
  if vm.API.vM_power_state = `Suspended && Db.is_valid_ref vdi then begin
    add vdi;
    let vdi = Db.VDI.get_record ~__context ~self:vdi in
    add vdi.API.vDI_SR
  end;
  (* Add also the guest metrics *)
  add vm.API.vM_guest_metrics;

  (* Add the hosts links *)
  add vm.API.vM_resident_on;
  add vm.API.vM_affinity;

  (* Add the parent VM *)
  if include_snapshots then update_table ~__context ~include_snapshots:false ~table vm.API.vM_parent
  end

(** Walk the graph of objects and update the table of Ref -> ids for each object we wish
    to include in the output. Other object references will be purged. *)
let create_table () =
	Hashtbl.create 10

(** Convert an internal reference into an external one or NULL *)
let lookup table r = 
  if not(Hashtbl.mem table r) then Ref.null else Ref.of_string (Hashtbl.find table r)

(** Convert a list of internal references into external references, filtering out NULLs *)
let filter table rs = List.filter (fun x -> x <> Ref.null) (List.map (lookup table) rs)

(** Convert an Host to an obj *)
let make_host table __context self =
	let host = Db.Host.get_record ~__context ~self in
	let host = { host with
					API.host_PIFs = [];
					API.host_PBDs = [];
					API.host_host_CPUs = [];
					API.host_license_params = [];
					API.host_blobs = [];
					API.host_external_auth_type = "";
					API.host_external_auth_service_name = "";
					API.host_external_auth_configuration = [];
					API.host_metrics = Ref.null;
					API.host_patches = [];
					API.host_crashdumps = [];
					API.host_logging = [];
					API.host_supported_bootloaders = [];
					API.host_cpu_configuration = [];
					API.host_other_config = [];
					API.host_capabilities = [];
					API.host_software_version = [];
					API.host_sched_policy = "";
					API.host_ha_statefiles = [];
					API.host_ha_network_peers = [];
					API.host_tags = [];
					API.host_crash_dump_sr = lookup table (Ref.string_of host.API.host_crash_dump_sr);
					API.host_suspend_image_sr = lookup table (Ref.string_of host.API.host_suspend_image_sr);
					API.host_resident_VMs = List.filter ((<>) Ref.null) (List.map (fun vm -> lookup table (Ref.string_of vm)) host.API.host_resident_VMs) } in
	{ cls = Datamodel._host;
	  id  = Ref.string_of (lookup table (Ref.string_of self));
	  snapshot = API.To.host_t host }

(** Convert a VM reference to an obj *)
let make_vm ?(with_snapshot_metadata=false) ~preserve_power_state table __context self = 
  let vm = Db.VM.get_record ~__context ~self in
  let vm = { vm with
		API.vM_power_state = if preserve_power_state then vm.API.vM_power_state else `Halted;
 		API.vM_suspend_VDI = if preserve_power_state then lookup table (Ref.string_of vm.API.vM_suspend_VDI) else Ref.null;
		API.vM_is_a_snapshot = if with_snapshot_metadata then vm.API.vM_is_a_snapshot else false;
		API.vM_snapshot_of =
		  if with_snapshot_metadata 
		  then lookup table (Ref.string_of vm.API.vM_snapshot_of)
		  else Ref.null;
		API.vM_snapshots = if with_snapshot_metadata then vm.API.vM_snapshots else [];
		API.vM_snapshot_time = if with_snapshot_metadata then vm.API.vM_snapshot_time else Date.never;
		API.vM_transportable_snapshot_id = if with_snapshot_metadata then vm.API.vM_transportable_snapshot_id else "";
		API.vM_parent =
			if with_snapshot_metadata
			then lookup table (Ref.string_of vm.API.vM_parent)
			else Ref.null;
		API.vM_VIFs = filter table (List.map Ref.string_of vm.API.vM_VIFs);
		API.vM_VBDs = filter table (List.map Ref.string_of vm.API.vM_VBDs);
		API.vM_crash_dumps = [];
		API.vM_VTPMs = [];
		API.vM_resident_on = lookup table (Ref.string_of vm.API.vM_resident_on);
		API.vM_affinity = lookup table (Ref.string_of vm.API.vM_affinity);
		API.vM_consoles = [];
		API.vM_metrics = Ref.null;
		API.vM_guest_metrics = lookup table (Ref.string_of vm.API.vM_guest_metrics) } in
  { cls = Datamodel._vm; 
    id = Ref.string_of (lookup table (Ref.string_of self)); 
    snapshot = API.To.vM_t vm }

(** Convert a guest-metrics reference to an obj *)
let make_gm table __context self =
	let gm = Db.VM_guest_metrics.get_record ~__context ~self in
	{ cls = Datamodel._vm_guest_metrics;
	  id = Ref.string_of (lookup table (Ref.string_of self));
	  snapshot = API.To.vM_guest_metrics_t gm }

(** Convert a VIF reference to an obj *)
let make_vif table ~preserve_power_state __context self = 
  let vif = Db.VIF.get_record ~__context ~self in
  let vif = { vif with 
		API.vIF_currently_attached = if preserve_power_state then vif.API.vIF_currently_attached else false;
		API.vIF_network = lookup table (Ref.string_of vif.API.vIF_network);
		API.vIF_VM = lookup table (Ref.string_of vif.API.vIF_VM);
		API.vIF_metrics = Ref.null; } in
  { cls = Datamodel._vif; 
    id = Ref.string_of (lookup table (Ref.string_of self)); 
    snapshot = API.To.vIF_t vif }

(** Convert a Network reference to an obj *)
let make_network table __context self = 
  let net = Db.Network.get_record ~__context ~self in
  let net = { net with 
		API.network_VIFs = filter table (List.map Ref.string_of net.API.network_VIFs);
		API.network_PIFs = []; } in
  { cls = Datamodel._network; 
    id = Ref.string_of (lookup table (Ref.string_of self)); 
    snapshot = API.To.network_t net }

(** Convert a VBD reference to an obj *)
let make_vbd table ~preserve_power_state __context self = 
  let vbd = Db.VBD.get_record ~__context ~self in
  let vbd = { vbd with 
		API.vBD_currently_attached = if preserve_power_state then vbd.API.vBD_currently_attached else false;
		API.vBD_VDI = lookup table (Ref.string_of vbd.API.vBD_VDI); 
		API.vBD_VM = lookup table (Ref.string_of vbd.API.vBD_VM);
		API.vBD_metrics = Ref.null;
	    } in
  { cls = Datamodel._vbd; 
    id = Ref.string_of (lookup table (Ref.string_of self)); 
    snapshot = API.To.vBD_t vbd }  

(** Convert a VDI reference to an obj *)
let make_vdi table __context self = 
  let vdi = Db.VDI.get_record ~__context ~self in
  let vdi = { vdi with 
		API.vDI_VBDs = filter table (List.map Ref.string_of vdi.API.vDI_VBDs);
		API.vDI_crash_dumps = [];
		API.vDI_SR = lookup table (Ref.string_of vdi.API.vDI_SR); } in
  { cls = Datamodel._vdi; 
    id = Ref.string_of (lookup table (Ref.string_of self)); 
    snapshot = API.To.vDI_t vdi }  

(** Convert a SR reference to an obj *)
let make_sr table __context self = 
  let sr = Db.SR.get_record ~__context ~self in
  let sr = { sr with 
		API.sR_VDIs = filter table (List.map Ref.string_of sr.API.sR_VDIs);
		API.sR_PBDs = []; } in
  { cls = Datamodel._sr; 
    id = Ref.string_of (lookup table (Ref.string_of self)); 
    snapshot = API.To.sR_t sr }    

let make_all ~with_snapshot_metadata ~preserve_power_state table __context = 
	let filter table rs = List.filter (fun x -> lookup table (Ref.string_of x) <> Ref.null) rs in
	let hosts = List.map (make_host table __context) (filter table (Db.Host.get_all ~__context)) in
	let vms  = List.map (make_vm ~with_snapshot_metadata ~preserve_power_state table __context) (filter table (Db.VM.get_all ~__context)) in
	let gms  = List.map (make_gm table __context) (filter table (Db.VM_guest_metrics.get_all ~__context)) in
	let vbds = List.map (make_vbd ~preserve_power_state table __context) (filter table (Db.VBD.get_all ~__context)) in
	let vifs = List.map (make_vif ~preserve_power_state table __context) (filter table (Db.VIF.get_all ~__context)) in
	let nets = List.map (make_network table __context) (filter table (Db.Network.get_all ~__context)) in
	let vdis = List.map (make_vdi table __context) (filter table (Db.VDI.get_all ~__context)) in
	let srs  = List.map (make_sr table __context) (filter table (Db.SR.get_all ~__context)) in
  
	hosts @ vms @ gms @ vbds @ vifs @ nets @ vdis @ srs 

open Xapi_globs

(* on normal export, do not include snapshot metadata;
   on metadata-export, include snapshots fields of the exported VM as well as the VM records of VMs 
   which are snapshots of the exported VM. *)
let vm_metadata ~with_snapshot_metadata ~preserve_power_state ~__context ~vms =
  let table = create_table () in
  List.iter (update_table ~__context ~include_snapshots:with_snapshot_metadata ~table) vms;
  let objects = make_all ~with_snapshot_metadata ~preserve_power_state table __context in
  let header = { version = this_version __context;
		   objects = objects } in
  let ova_xml = Xml.to_bigbuffer (xmlrpc_of_header header) in
  table, ova_xml

let string_of_vm ~__context vm =
	try Printf.sprintf "'%s' ('%s')" 
		(Db.VM.get_uuid ~__context ~self:vm)
		(Db.VM.get_name_label ~__context ~self:vm)
	with _ -> "invalid"

(** Export a VM's metadata only *)
let export_metadata ~__context ~with_snapshot_metadata ~preserve_power_state ~vms s =
	begin match vms with
	| [] -> failwith "need to specify at least one VM"
	| [vm] -> info "VM.export_metadata: VM = %s; with_snapshot_metadata = '%b'; preserve_power_state = '%s" 
				(string_of_vm ~__context vm)
				with_snapshot_metadata
				(string_of_bool preserve_power_state)
	| vms -> info "VM.export_metadata: VM = %s; with_snapshot_metadata = '%b'; preserve_power_state = '%s"
				(String.concat ", " (List.map (string_of_vm ~__context) vms)) 
				with_snapshot_metadata
				(string_of_bool preserve_power_state) end;

	let _, ova_xml = vm_metadata ~with_snapshot_metadata ~preserve_power_state ~__context ~vms in
	let hdr = Tar.Header.make Xva.xml_filename (Bigbuffer.length ova_xml) in
	Tar.write_block hdr (fun s -> Tar.write_bigbuffer s ova_xml) s

let export refresh_session __context rpc session_id s vm_ref preserve_power_state =
  info "VM.export: VM = %s; preserve_power_state = '%s'"
	  (string_of_vm ~__context vm_ref)
	  (string_of_bool preserve_power_state);

  let table, ova_xml = vm_metadata ~with_snapshot_metadata:false  ~preserve_power_state ~__context ~vms:[vm_ref] in

  debug "Outputting ova.xml";

  let hdr = Tar.Header.make Xva.xml_filename (Bigbuffer.length ova_xml) in
  Tar.write_block hdr (fun s -> Tar.write_bigbuffer s ova_xml) s;

  (* Only stream the disks that are in the table AND which are not CDROMs (ie whose VBD.type <> CD
     and whose SR.content_type <> "iso" *)
  let vbds = Db.VM.get_VBDs ~__context ~self:vm_ref in
  let vbds = List.filter (fun x -> Db.VBD.get_type ~__context ~self:x <> `CD) vbds in
  let vdis = List.map (fun self -> Db.VBD.get_VDI ~__context ~self) vbds in
  (* Don't forget the suspend VDI (if we allow export of suspended VMs) *)
  let vdis = match Db.VM.get_power_state ~__context ~self:vm_ref with
    | `Suspended when not preserve_power_state -> Db.VM.get_suspend_VDI ~__context ~self:vm_ref :: vdis 
    | _ -> vdis in
  let vdis = List.filter (fun self -> Db.SR.get_content_type ~__context ~self:(Db.VDI.get_SR ~__context ~self) <> "iso") vdis in
  let vdis = List.filter (fun vdi -> Hashtbl.mem table (Ref.string_of vdi)) vdis in
  let vdis = List.map (fun vdi -> Hashtbl.find table (Ref.string_of vdi), vdi, Db.VDI.get_virtual_size ~__context ~self:vdi) vdis in
  Stream_vdi.send_all refresh_session s __context rpc session_id vdis;

  (* We no longer write the end-of-tar checksum table, preferring the inline ones instead *)

  Tar.write_end s;
  debug "export VM = %s completed successfully" (Ref.string_of vm_ref)

open Http
open Client

let lock_vm ~__context ~vm ~task_id op =
	(* Note slight race here because we haven't got the master lock *)
	Xapi_vm_lifecycle.assert_operation_valid ~__context ~self:vm ~op;
	(* ... small race lives here ... *)
	Db.VM.add_to_current_operations ~__context ~self:vm ~key:task_id ~value:op;
	Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm

let unlock_vm ~__context ~vm ~task_id =
    Db.VM.remove_from_current_operations ~__context ~self:vm ~key:task_id;
    Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm

let with_vm_locked ~__context ~vm ~task_id op f = 
	lock_vm ~__context ~vm ~task_id op;
	finally f
		(fun () -> unlock_vm ~__context ~vm ~task_id)

let vm_from_request ~__context (req: request) = 
  if List.mem_assoc "ref" req.query
  then Ref.of_string (List.assoc "ref" req.query) 
  else 
    let uuid = List.assoc "uuid" req.query in
      Helpers.call_api_functions 
        ~__context (fun rpc session_id -> Client.VM.get_by_uuid rpc session_id uuid) 

let export_all_vms_from_request ~__context (req: request) = 
	if List.mem_assoc "all" req.query
	then bool_of_string (List.assoc "all" req.query)
	else false

let metadata_handler (req: request) s = 
	debug "metadata_handler called";

	(* Xapi_http.with_context always completes the task at the end *)
	Xapi_http.with_context "VM.export_metadata" req s
		(fun __context ->
			let export_all = export_all_vms_from_request ~__context req in

			(* Get the VM refs. In case of exporting the metadata of a particular VM, return a singleton list containing the vm ref.  *)
			(* In case of exporting all the VMs metadata, get all the VM records which are not default templates. *)
			let vm_refs =
				if export_all then begin
					let is_default_template vm =
						vm.API.vM_is_a_template
						&& (List.mem_assoc Xapi_globs.default_template_key vm.API.vM_other_config)
						&& ((List.assoc Xapi_globs.default_template_key vm.API.vM_other_config) = "true") in
					let all_vms = Db.VM.get_all_records ~__context in
					let interesting_vms = List.filter (fun (_, vm) -> not (is_default_template vm)) all_vms in
					List.map fst interesting_vms
				end else
					[vm_from_request ~__context req]
				in

			if not export_all && Db.VM.get_is_a_snapshot ~__context ~self:(List.hd vm_refs) then
				raise (Api_errors.Server_error (Api_errors.operation_not_allowed, [ "Exporting metadata of a snapshot is not allowed" ]));

			let task_id = Ref.string_of (Context.get_task_id __context) in
			let headers = Http.http_200_ok ~keep_alive:false ~version:"1.0" () @
				[ Http.task_id_hdr ^ ": " ^ task_id;
				"Server: "^Xapi_globs.xapi_user_agent;
				content_type;
				"Content-Disposition: attachment; filename=\"export.xva\""] in

			Http_svr.headers s headers;

			(* lock all the VMs before exporting their metadata *)
			List.iter (fun vm -> lock_vm ~__context ~vm ~task_id `metadata_export) vm_refs;
			finally
 				(fun () -> export_metadata ~with_snapshot_metadata:true ~preserve_power_state:true ~__context ~vms:vm_refs s)
 				(fun () ->
 					 List.iter (fun vm -> unlock_vm ~__context ~vm ~task_id) vm_refs;
 					 Tar.write_end s);
		)

let handler (req: request) s = 
  debug "export handler";
  req.close := true;

  (* First things first, let's make sure that the request has a valid session or username/password *)
  
  Xapi_http.assert_credentials_ok "VM.export" req;
    
  (* Perform the SR reachability check using a fresh context/task because
     we don't want to complete the task in the forwarding case *)
  
  Server_helpers.exec_with_new_task "VM.export" 
    (fun __context -> 
       (* The VM Ref *)
       let vm_ref = vm_from_request ~__context req in
       let localhost = Helpers.get_localhost ~__context in
       let host_ok = check_vm_host_SRs ~__context vm_ref localhost in
      
       if not host_ok (* redirect *)
       then 
	 begin
	   try
	     (* We do this outside the Xapi_http.with_context below since that will complete the *)
	     (* task when it exits, and we don't want to do that *)

	     let host = find_host_for_VM ~__context vm_ref in

	     let address = Db.Host.get_address ~__context ~self:host in
	     let url = Printf.sprintf "https://%s%s?%s" address req.uri (String.concat "&" (List.map (fun (a,b) -> a^"="^b) req.query)) in
	     info "export VM = %s redirecting to: %s" (Ref.string_of vm_ref) url;
	     let headers = Http.http_302_redirect url in
	     Http_svr.headers s headers;
	   with
	     | Api_errors.Server_error (a,b) as e ->
		 error "Caught exception in export handler: %s" (ExnHelper.string_of_exn e);
		 (* If there's no host that can see the SRs, then it's actually our responsibility *)
		 (* to complete the task *)
		 let task_id =
       let all = req.cookie @ req.query in
       if List.mem_assoc "task_id" all 
       then Some (Ref.of_string (List.assoc "task_id" all))
       else None in
       begin match task_id with 
         | None -> Server_helpers.exec_with_new_task "export" ~task_in_database:true (fun __context -> TaskHelper.failed ~__context (a,b))
         | Some task_id -> Server_helpers.exec_with_forwarded_task task_id (fun __context -> TaskHelper.failed ~__context (a,b))
       end
	     | e ->
		 error "Caught exception in export handler: %s" (Printexc.to_string e);
		 raise e	       
	 end
       else
	 (* Xapi_http.with_context always completes the task at the end *)
	 begin
	   debug "Doing xapi_http.with_context now...";
	   Xapi_http.with_context "VM.export" req s
	     (fun __context -> Helpers.call_api_functions ~__context (fun rpc session_id ->
	       
	       (* This is the signal to say we've taken responsibility from the CLI server for completing the task *)
	       (* The GUI can deal with this itself, but the CLI is complicated by the thin cli/cli server split *)
	       TaskHelper.set_progress ~__context 0.0;
	       let refresh_session = Xapi_session.consider_touching_session rpc session_id in	      
	       let task_id = Ref.string_of (Context.get_task_id __context) in
	       let preserve_power_state = 
			   let all = req.cookie @ req.query in
			   List.mem_assoc "preserve_power_state" all && bool_of_string (List.assoc "preserve_power_state" all) in
	       let headers = Http.http_200_ok ~keep_alive:false ~version:"1.0" () @
		 [ Http.task_id_hdr ^ ": " ^ task_id;
		   "Server: "^Xapi_globs.xapi_user_agent;
		   content_type;
		   "Content-Disposition: attachment; filename=\"export.xva\""] in
	
 	       with_vm_locked ~__context ~vm:vm_ref ~task_id `export
		 (fun () -> 
		    Http_svr.headers s headers;
		    export refresh_session __context rpc session_id s vm_ref preserve_power_state)
		 
       	     (* Exceptions are handled by Server_helpers.with_context *)
	     ))
	 end
    )
