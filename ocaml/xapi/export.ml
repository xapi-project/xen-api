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
(** Code to output a subset of database records, marshalled in XMLRPC format
 * @group Import and Export
*)

(** The general plan:
    + Walk around the database and select the objects you want (see 'create_table')
     and make a table mapping internal ref -> fresh external references. It would
     be nice to generate a visitor thingimy for this.
    + Select all the objects from each class, filter the subset you want (ie those whose
     reference exists as a key in the table) and convert them into instances of the
     intermediate record 'type obj' via the functions make_{vm,sr,vbd,vif,network}.
                                                             The created 'obj record' includes the class name as a string (from the datamodel),
                                                             the fresh reference and the output of 'get_record' marshalled using the standard
                                                             XMLRPC functions with all the references converted either to the fresh external refs
                                                             or NULL (so we aim not to export dangling pointers)
                                                             + Write out one big XML file containing an XMLRPC struct which has keys:
                                                             version -> a structure of system version info (API versions, internal build numbers)
                                                             state -> an XMLRPC array of XMLRPC serialised 'obj' records (see 'xmlrpc_of_obj')
                                                           *)

(** The specific plan for VM export:
    Walk over the datamodel and mark VIFs, Networks connected to the VIFs, VBDs, VDIs connected
    to the VBDs, SRs connected to the VDIs (and maybe a suspend image?). *)

open Importexport
open Stdext
open Pervasiveext

module D=Debug.Make(struct let name="export" end)
open D


let make_id =
  let counter = ref 0 in
  fun () ->
    let this = !counter in
    incr counter;
    "Ref:" ^ (string_of_int this)

let rec update_table ~__context ~include_snapshots ~preserve_power_state ~include_vhd_parents ~table vm =
  let add r =
    if not (Hashtbl.mem table (Ref.string_of r)) then
      Hashtbl.add table (Ref.string_of r)(make_id ()) in

  let rec add_vdi v =
    add v;
    let r = Db.VDI.get_record ~__context ~self:v in
    add r.API.vDI_SR;
    if include_vhd_parents then begin
      let sm_config = r.API.vDI_sm_config in
      if List.mem_assoc Xapi_globs.vhd_parent sm_config then begin
        let parent_uuid = List.assoc Xapi_globs.vhd_parent sm_config in
        try
          let parent_ref = Db.VDI.get_by_uuid ~__context ~uuid:parent_uuid in
          (* Only recurse if we haven't already seen this VDI *)
          if not (Hashtbl.mem table (Ref.string_of parent_ref))
          then add_vdi parent_ref
        with _ ->
          warn "VM.export_metadata: lookup of parent VDI %s failed" parent_uuid
      end
    end
  in

  if Db.is_valid_ref __context vm && not (Hashtbl.mem table (Ref.string_of vm)) then begin
    add vm;
    let vm = Db.VM.get_record ~__context ~self:vm in

    List.iter
      (fun vif ->
         if Db.is_valid_ref __context vif then begin
           add vif;
           let vif = Db.VIF.get_record ~__context ~self:vif in
           add vif.API.vIF_network
         end)
      vm.API.vM_VIFs;

    List.iter
      (fun vbd ->
         if Db.is_valid_ref __context vbd then begin
           add vbd;
           let vbd = Db.VBD.get_record ~__context ~self:vbd in
           if not(vbd.API.vBD_empty)
           then add_vdi vbd.API.vBD_VDI
         end)
      vm.API.vM_VBDs;

    List.iter
      (fun vgpu ->
         if Db.is_valid_ref __context vgpu then begin
           add vgpu;
           let vgpu = Db.VGPU.get_record ~__context ~self:vgpu in
           add vgpu.API.vGPU_type;
           add vgpu.API.vGPU_GPU_group
         end)
      vm.API.vM_VGPUs;

    (* add all PVS proxies that have a VIF belonging to this VM, add their
       		 * PVS sites as well
       		 *)
    Db.PVS_proxy.get_all_records ~__context
    |> List.filter (fun (_,p)-> List.mem p.API.pVS_proxy_VIF vm.API.vM_VIFs)
    |> List.iter
      (fun (ref, proxy) ->
         if Db.is_valid_ref __context ref then begin
           add ref;
           add proxy.API.pVS_proxy_site;
         end);

    (* If we need to include snapshots, update the table for VMs in the 'snapshots' field *)
    if include_snapshots then
      List.iter
        (fun snap -> update_table ~__context ~include_snapshots:false ~preserve_power_state ~include_vhd_parents ~table snap)
        vm.API.vM_snapshots;

    (* If VM is suspended then add the suspend_VDI *)
    let vdi = vm.API.vM_suspend_VDI in
    if preserve_power_state && vm.API.vM_power_state = `Suspended && Db.is_valid_ref __context vdi
    then add_vdi vdi;

    (* Add also the guest metrics *)
    add vm.API.vM_guest_metrics;

    (* Add the hosts links *)
    add vm.API.vM_resident_on;
    add vm.API.vM_affinity;

    (* Add the parent VM *)
    if include_snapshots && Db.is_valid_ref __context vm.API.vM_parent
    then update_table ~__context ~include_snapshots:false ~preserve_power_state ~include_vhd_parents ~table vm.API.vM_parent
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

(** Convert a Host to an obj *)
let make_host table __context self =
  let host = Db.Host.get_record ~__context ~self in
  let host = { host with
               API.host_PIFs = [];
               API.host_PBDs = [];
               API.host_PGPUs = [];
               API.host_PCIs = [];
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
    snapshot = API.Legacy.To.host_t host }

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
             API.vM_current_operations = [];
             API.vM_allowed_operations = [];
             API.vM_VIFs = filter table (List.map Ref.string_of vm.API.vM_VIFs);
             API.vM_VBDs = filter table (List.map Ref.string_of vm.API.vM_VBDs);
             API.vM_VGPUs = filter table (List.map Ref.string_of vm.API.vM_VGPUs);
             API.vM_crash_dumps = [];
             API.vM_VTPMs = [];
             API.vM_resident_on = lookup table (Ref.string_of vm.API.vM_resident_on);
             API.vM_affinity = lookup table (Ref.string_of vm.API.vM_affinity);
             API.vM_consoles = [];
             API.vM_metrics = Ref.null;
             API.vM_guest_metrics = lookup table (Ref.string_of vm.API.vM_guest_metrics);
             API.vM_protection_policy = Ref.null;
             API.vM_bios_strings = vm.API.vM_bios_strings;
             API.vM_blobs = [];} in
  { cls = Datamodel._vm;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.vM_t vm }

(** Convert a guest-metrics reference to an obj *)
let make_gm table __context self =
  let gm = Db.VM_guest_metrics.get_record ~__context ~self in
  { cls = Datamodel._vm_guest_metrics;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.vM_guest_metrics_t gm }

(** Convert a VIF reference to an obj *)
let make_vif table ~preserve_power_state __context self =
  let vif = Db.VIF.get_record ~__context ~self in
  let vif = { vif with
              API.vIF_currently_attached = if preserve_power_state then vif.API.vIF_currently_attached else false;
              API.vIF_network = lookup table (Ref.string_of vif.API.vIF_network);
              API.vIF_VM = lookup table (Ref.string_of vif.API.vIF_VM);
              API.vIF_metrics = Ref.null;
              API.vIF_current_operations = [];
              API.vIF_allowed_operations = [];
            } in
  { cls = Datamodel._vif;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.vIF_t vif }

(** Convert a Network reference to an obj *)
let make_network table __context self =
  let net = Db.Network.get_record ~__context ~self in
  let net = { net with
              API.network_VIFs = filter table (List.map Ref.string_of net.API.network_VIFs);
              API.network_PIFs = [];
              API.network_current_operations = [];
              API.network_allowed_operations = [];
            } in
  { cls = Datamodel._network;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.network_t net }

(** Convert a VBD reference to an obj *)
let make_vbd table ~preserve_power_state __context self =
  let vbd = Db.VBD.get_record ~__context ~self in
  let vbd = { vbd with
              API.vBD_currently_attached = if preserve_power_state then vbd.API.vBD_currently_attached else false;
              API.vBD_VDI = lookup table (Ref.string_of vbd.API.vBD_VDI);
              API.vBD_VM = lookup table (Ref.string_of vbd.API.vBD_VM);
              API.vBD_metrics = Ref.null;
              API.vBD_current_operations = [];
              API.vBD_allowed_operations = [];
            } in
  { cls = Datamodel._vbd;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.vBD_t vbd }

(** Convert a VDI reference to an obj *)
let make_vdi table __context self =
  let vdi = Db.VDI.get_record ~__context ~self in
  let vdi = { vdi with
              API.vDI_VBDs = filter table (List.map Ref.string_of vdi.API.vDI_VBDs);
              API.vDI_crash_dumps = [];
              API.vDI_SR = lookup table (Ref.string_of vdi.API.vDI_SR);
              API.vDI_current_operations = [];
              API.vDI_allowed_operations = [];
            } in
  { cls = Datamodel._vdi;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.vDI_t vdi }

(** Convert a SR reference to an obj *)
let make_sr table __context self =
  let sr = Db.SR.get_record ~__context ~self in
  let sr = { sr with
             API.sR_VDIs = filter table (List.map Ref.string_of sr.API.sR_VDIs);
             API.sR_PBDs = [];
             API.sR_current_operations = [];
             API.sR_allowed_operations = [];
           } in
  { cls = Datamodel._sr;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.sR_t sr;
  }

(** Convert a VGPU_type reference to an obj *)
let make_vgpu_type table __context self =
  let vgpu_type = Db.VGPU_type.get_record ~__context ~self in
  {
    cls = Datamodel._vgpu_type;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.vGPU_type_t vgpu_type
  }

(** Convert a VGPU reference to an obj *)
let make_vgpu table ~preserve_power_state __context self =
  let vgpu = Db.VGPU.get_record ~__context ~self in
  let vgpu = { vgpu with
               API.vGPU_currently_attached = if preserve_power_state then vgpu.API.vGPU_currently_attached else false;
               API.vGPU_GPU_group = lookup table (Ref.string_of vgpu.API.vGPU_GPU_group);
               API.vGPU_type = lookup table (Ref.string_of vgpu.API.vGPU_type);
               API.vGPU_VM = lookup table (Ref.string_of vgpu.API.vGPU_VM);
             } in
  {
    cls = Datamodel._vgpu;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.vGPU_t vgpu
  }

(** Convert a GPU_group reference to an obj *)
let make_gpu_group table __context self =
  let group = Db.GPU_group.get_record ~__context ~self in
  let group = { group with
                API.gPU_group_VGPUs = filter table (List.map Ref.string_of group.API.gPU_group_VGPUs);
                API.gPU_group_PGPUs = [];
              } in
  {
    cls = Datamodel._gpu_group;
    id = Ref.string_of (lookup table (Ref.string_of self));
    snapshot = API.Legacy.To.gPU_group_t group
  }

let make_pvs_proxies table __context self =
  debug "exporting PVS Proxy %s" (Ref.string_of self);
  let lookup' ref = lookup table (Ref.string_of ref) in
  let proxy = Db.PVS_proxy.get_record ~__context ~self in
  let proxy =
    { proxy with
      API.pVS_proxy_site      = lookup' proxy.API.pVS_proxy_site
    ; API.pVS_proxy_VIF       = lookup' proxy.API.pVS_proxy_VIF
    ; API.pVS_proxy_currently_attached = false (* default on dest *)
    } in
  { cls      = Datamodel._pvs_proxy
  ; id       = Ref.string_of (lookup' self)
  ; snapshot = API.Legacy.To.pVS_proxy_t proxy
  }

let make_pvs_sites table __context self =
  debug "exporting PVS Site %s" (Ref.string_of self);
  let lookup'  ref  = lookup table (Ref.string_of ref) in
  let filter'  refs = filter table (List.map Ref.string_of refs) in
  let site = Db.PVS_site.get_record ~__context ~self in
  let site =
    { site with
      API.pVS_site_cache_storage = [] (* don't export *)
    ; API.pVS_site_servers       = [] (* don't export *)
    ; API.pVS_site_proxies       = filter' site.API.pVS_site_proxies
    } in
  { cls      = Datamodel._pvs_site
  ; id       = Ref.string_of (lookup' self)
  ; snapshot = API.Legacy.To.pVS_site_t site
  }


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
  let vgpu_types = List.map (make_vgpu_type table __context) (filter table (Db.VGPU_type.get_all ~__context)) in
  let vgpus = List.map (make_vgpu ~preserve_power_state table __context) (filter table (Db.VGPU.get_all ~__context)) in
  let gpu_groups = List.map (make_gpu_group table __context) (filter table (Db.GPU_group.get_all ~__context)) in
  let pvs_proxies = List.map (make_pvs_proxies table __context) (filter table (Db.PVS_proxy.get_all ~__context)) in
  let pvs_sites   = List.map (make_pvs_sites table __context) (filter table (Db.PVS_site.get_all ~__context)) in
  List.concat
    [ hosts
    ; vms
    ; gms
    ; vbds
    ; vifs
    ; nets
    ; vdis
    ; srs
    ; vgpu_types
    ; vgpus
    ; gpu_groups
    ; pvs_proxies
    ; pvs_sites
    ]

open Xapi_globs

(* on normal export, do not include snapshot metadata;
   on metadata-export, include snapshots fields of the exported VM as well as the VM records of VMs
   which are snapshots of the exported VM. *)
let vm_metadata ~with_snapshot_metadata ~preserve_power_state ~include_vhd_parents ~__context ~vms =
  let table = create_table () in
  List.iter (update_table ~__context ~include_snapshots:with_snapshot_metadata ~preserve_power_state ~include_vhd_parents ~table) vms;
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
let export_metadata ~__context ~with_snapshot_metadata ~preserve_power_state ~include_vhd_parents ~vms s =
  begin match vms with
    | [] -> failwith "need to specify at least one VM"
    | [vm] -> info "VM.export_metadata: VM = %s; with_snapshot_metadata = '%b'; include_vhd_parents = '%b'; preserve_power_state = '%s"
                (string_of_vm ~__context vm)
                with_snapshot_metadata
                include_vhd_parents
                (string_of_bool preserve_power_state)
    | vms -> info "VM.export_metadata: VM = %s; with_snapshot_metadata = '%b'; preserve_power_state = '%s"
               (String.concat ", " (List.map (string_of_vm ~__context) vms))
               with_snapshot_metadata
               (string_of_bool preserve_power_state) end;

  let _, ova_xml = vm_metadata ~with_snapshot_metadata ~preserve_power_state ~include_vhd_parents ~__context ~vms in
  let hdr = Tar_unix.Header.make Xva.xml_filename (Bigbuffer.length ova_xml) in
  Tar_unix.write_block hdr (fun s -> Bigbuffer.to_fct ova_xml (fun frag -> Unixext.really_write_string s frag)) s;
  Tar_unix.write_end s

let export refresh_session __context rpc session_id s vm_ref preserve_power_state =
  info "VM.export: VM = %s; preserve_power_state = '%s'"
    (string_of_vm ~__context vm_ref)
    (string_of_bool preserve_power_state);

  let table, ova_xml = vm_metadata ~with_snapshot_metadata:false  ~preserve_power_state ~include_vhd_parents:false ~__context ~vms:[vm_ref] in

  debug "Outputting ova.xml";

  let hdr = Tar_unix.Header.make Xva.xml_filename (Bigbuffer.length ova_xml) in
  Tar_unix.write_block hdr (fun s -> Bigbuffer.to_fct ova_xml (fun frag -> Unixext.really_write_string s frag)) s;

  (* Only stream the disks that are in the table AND which are not CDROMs (ie whose VBD.type <> CD
     and whose SR.content_type <> "iso" *)
  let vbds = Db.VM.get_VBDs ~__context ~self:vm_ref in
  let vbds = List.filter (fun x -> Db.VBD.get_type ~__context ~self:x <> `CD) vbds in
  let vdis = List.map (fun self -> Db.VBD.get_VDI ~__context ~self) vbds in
  (* Don't forget the suspend VDI (if we allow export of suspended VMs) *)
  let vdis = match Db.VM.get_power_state ~__context ~self:vm_ref with
    | `Suspended when preserve_power_state -> Db.VM.get_suspend_VDI ~__context ~self:vm_ref :: vdis
    | _ -> vdis in
  let vdis = List.filter (fun self -> Db.SR.get_content_type ~__context ~self:(Db.VDI.get_SR ~__context ~self) <> "iso") vdis in
  let vdis = List.filter (fun vdi -> Hashtbl.mem table (Ref.string_of vdi)) vdis in
  let vdis = List.map (fun vdi -> Hashtbl.find table (Ref.string_of vdi), vdi, Db.VDI.get_virtual_size ~__context ~self:vdi) vdis in
  Stream_vdi.send_all refresh_session s __context rpc session_id vdis;

  (* We no longer write the end-of-tar checksum table, preferring the inline ones instead *)

  Tar_unix.write_end s;
  debug "export VM = %s completed successfully" (Ref.string_of vm_ref)

open Http
open Client

let lock_vm ~__context ~vm ~task_id op =
  (* Note slight race here because we haven't got the master lock *)
  Xapi_vm_lifecycle.assert_operation_valid ~__context ~self:vm ~op ~strict:true;
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

let vm_from_request ~__context (req: Request.t) =
  if List.mem_assoc "ref" req.Request.query
  then Ref.of_string (List.assoc "ref" req.Request.query)
  else
    let uuid = List.assoc "uuid" req.Request.query in
    Helpers.call_api_functions
      ~__context (fun rpc session_id -> Client.VM.get_by_uuid rpc session_id uuid)

let bool_from_request ~__context (req: Request.t) default k =
  if List.mem_assoc k req.Request.query
  then bool_of_string (List.assoc k req.Request.query)
  else default

let export_all_vms_from_request ~__context (req: Request.t) =
  bool_from_request ~__context req false "all"

let include_vhd_parents_from_request ~__context (req: Request.t) =
  bool_from_request ~__context req false "include_vhd_parents"

let export_snapshots_from_request ~__context (req: Request.t) =
  bool_from_request ~__context req true "export_snapshots"

let include_dom0_from_request ~__context (req: Request.t) =
  bool_from_request ~__context req true "include_dom0"

let metadata_handler (req: Request.t) s _ =
  debug "metadata_handler called";
  req.Request.close <- true;

  (* Xapi_http.with_context always completes the task at the end *)
  Xapi_http.with_context "VM.export_metadata" req s
    (fun __context ->
       let include_vhd_parents = include_vhd_parents_from_request ~__context req in
       let export_all = export_all_vms_from_request ~__context req in
       let export_snapshots = export_snapshots_from_request ~__context req in
       let include_dom0 = include_dom0_from_request ~__context req in

       (* Get the VM refs. In case of exporting the metadata of a particular VM, return a singleton list containing the vm ref.  *)
       (* In case of exporting all the VMs metadata, get all the VM records which are not default templates. *)
       let vm_refs =
         if export_all then begin
           let is_default_template vm =
             vm.API.vM_is_default_template ||
             (vm.API.vM_is_a_template
              && (List.mem_assoc Xapi_globs.default_template_key vm.API.vM_other_config)
              && ((List.assoc Xapi_globs.default_template_key vm.API.vM_other_config) = "true")) 
             in
           let all_vms = Db.VM.get_all_records ~__context in
           let interesting_vms = List.filter (fun (vm, vmr) ->
               not (is_default_template vmr)
               && (not (Helpers.is_domain_zero ~__context vm) || include_dom0)
             ) all_vms in
           List.map fst interesting_vms
         end else
           [vm_from_request ~__context req]
       in

       if not export_all && Db.VM.get_is_a_snapshot ~__context ~self:(List.hd vm_refs) then
         raise (Api_errors.Server_error (Api_errors.operation_not_allowed, [ "Exporting metadata of a snapshot is not allowed" ]));

       let task_id = Ref.string_of (Context.get_task_id __context) in
       let read_fd, write_fd = Unix.pipe () in
       let export_error = ref None in
       let writer_thread = Thread.create (fun () ->
           (* lock all the VMs before exporting their metadata *)
           List.iter (fun vm -> lock_vm ~__context ~vm ~task_id `metadata_export) vm_refs;
           try
             finally
               (fun () -> export_metadata ~with_snapshot_metadata:export_snapshots ~preserve_power_state:true ~include_vhd_parents ~__context ~vms:vm_refs write_fd)
               (fun () ->
                  Unix.close write_fd;
                  List.iter (fun vm -> unlock_vm ~__context ~vm ~task_id) vm_refs)
           with e ->
             export_error := Some e)
           ()
       in
       let tar_data = Unixext.string_of_fd read_fd in
       Thread.join writer_thread;
       Unix.close read_fd;
       match !export_error with
       | None -> begin
           let content_length = String.length tar_data in

           let headers = Http.http_200_ok ~keep_alive:false ~version:"1.0" () @
                         [ Http.Hdr.task_id ^ ": " ^ task_id;
                           "Server: "^Xapi_globs.xapi_user_agent;
                           content_type;
                           "Content-Length: "^(string_of_int content_length);
                           "Content-Disposition: attachment; filename=\"export.xva\""] in

           Http_svr.headers s headers;

           Unixext.really_write_string s tar_data
         end
       | Some e -> begin
           let response_string = Http.Response.(to_wire_string internal_error) in
           Unixext.really_write_string s response_string;
           error
             "Caught %s while exporting metadata - responding with HTTP 500"
             (Printexc.to_string e);
           raise e
         end
    )

let handler (req: Request.t) s _ =
  debug "export handler";
  req.Request.close <- true;

  (* First things first, let's make sure that the request has a valid session or username/password *)

  Xapi_http.assert_credentials_ok "VM.export" ~http_action:"get_export" req s;

  let use_compression = List.mem_assoc Constants.use_compression req.Request.query && List.assoc Constants.use_compression req.Request.query = "true" in
  debug "Using compression: %b" use_compression;
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
             let url = Printf.sprintf "https://%s%s?%s" address req.Request.uri (String.concat "&" (List.map (fun (a,b) -> a^"="^b) req.Request.query)) in
             info "export VM = %s redirecting to: %s" (Ref.string_of vm_ref) url;
             let headers = Http.http_302_redirect url in
             Http_svr.headers s headers;
           with
           | Api_errors.Server_error (a,b) as e ->
             error "Caught exception in export handler: %s" (ExnHelper.string_of_exn e);
             (* If there's no host that can see the SRs, then it's actually our responsibility *)
             (* to complete the task *)
             let task_id =
               let all = req.Request.cookie @ req.Request.query in
               if List.mem_assoc "task_id" all
               then Some (Ref.of_string (List.assoc "task_id" all))
               else None in
             begin match task_id with
               | None -> Server_helpers.exec_with_new_task "export" ~task_in_database:true (fun __context -> TaskHelper.failed ~__context e)
               | Some task_id -> Server_helpers.exec_with_forwarded_task task_id (fun __context -> TaskHelper.failed ~__context e)
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
                    let all = req.Request.cookie @ req.Request.query in
                    List.mem_assoc "preserve_power_state" all && bool_of_string (List.assoc "preserve_power_state" all) in
                  let headers = Http.http_200_ok ~keep_alive:false ~version:"1.0" () @
                                [ Http.Hdr.task_id ^ ": " ^ task_id;
                                  "Server: "^Xapi_globs.xapi_user_agent;
                                  content_type;
                                  "Content-Disposition: attachment; filename=\"export.xva\""] in

                  with_vm_locked ~__context ~vm:vm_ref ~task_id `export
                    (fun () ->
                       Http_svr.headers s headers;
                       let go fd = export refresh_session __context rpc session_id fd vm_ref preserve_power_state in
                       if use_compression
                       then Gzip.compress s go
                       else go s
                    )

                 (* Exceptions are handled by Xapi_http.with_context *)
                ))
         end
    )
