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
open Xapi_stdext_pervasives.Pervasiveext
module Date = Clock.Date
module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "export" end)

open D

let make_id =
  let counter = ref 0 in
  fun () ->
    let this = !counter in
    incr counter ;
    "Ref:" ^ string_of_int this

let rec update_table ~__context ~include_snapshots ~preserve_power_state
    ~include_vhd_parents ~table ~excluded_devices vm =
  let add r =
    if not (Hashtbl.mem table (Ref.string_of r)) then
      Hashtbl.add table (Ref.string_of r) (make_id ())
  in
  let rec add_vdi v =
    add v ;
    let r = Db.VDI.get_record ~__context ~self:v in
    add r.API.vDI_SR ;
    if include_vhd_parents then
      let sm_config = r.API.vDI_sm_config in
      if List.mem_assoc Xapi_globs.vhd_parent sm_config then
        let parent_uuid = List.assoc Xapi_globs.vhd_parent sm_config in
        try
          let parent_ref = Db.VDI.get_by_uuid ~__context ~uuid:parent_uuid in
          (* Only recurse if we haven't already seen this VDI *)
          if not (Hashtbl.mem table (Ref.string_of parent_ref)) then
            add_vdi parent_ref
        with _ ->
          warn "VM.export_metadata: lookup of parent VDI %s failed" parent_uuid
  in
  if Db.is_valid_ref __context vm && not (Hashtbl.mem table (Ref.string_of vm))
  then (
    add vm ;
    let vm = Db.VM.get_record ~__context ~self:vm in
    if not (List.mem Devicetype.VIF excluded_devices) then
      List.iter
        (fun vif ->
          if Db.is_valid_ref __context vif then (
            add vif ;
            let vif = Db.VIF.get_record ~__context ~self:vif in
            add vif.API.vIF_network
          )
        )
        vm.API.vM_VIFs ;
    if not (List.mem Devicetype.VBD excluded_devices) then
      List.iter
        (fun vbd ->
          if Db.is_valid_ref __context vbd then (
            add vbd ;
            let vbd = Db.VBD.get_record ~__context ~self:vbd in
            if not vbd.API.vBD_empty then
              add_vdi vbd.API.vBD_VDI
          )
        )
        vm.API.vM_VBDs ;
    if not (List.mem Devicetype.VGPU excluded_devices) then
      List.iter
        (fun vgpu ->
          if Db.is_valid_ref __context vgpu then (
            add vgpu ;
            let vgpu = Db.VGPU.get_record ~__context ~self:vgpu in
            add vgpu.API.vGPU_type ;
            add vgpu.API.vGPU_GPU_group
          )
        )
        vm.API.vM_VGPUs ;
    (* add all PVS proxies that have a VIF belonging to this VM, add their
       PVS sites as well *)
    Db.PVS_proxy.get_all_records ~__context
    |> List.filter (fun (_, p) -> List.mem p.API.pVS_proxy_VIF vm.API.vM_VIFs)
    |> List.iter (fun (ref, proxy) ->
           if Db.is_valid_ref __context ref then (
             add ref ;
             add proxy.API.pVS_proxy_site
           )
       ) ;
    (* add VTPMs that belong to this VM *)
    if not (List.mem Devicetype.VTPM excluded_devices) then
      vm.API.vM_VTPMs
      |> List.iter (fun ref -> if Db.is_valid_ref __context ref then add ref) ;

    (* If we need to include snapshots, update the table for VMs in the 'snapshots' field *)
    if include_snapshots then
      List.iter
        (fun snap ->
          update_table ~__context ~include_snapshots:false ~preserve_power_state
            ~include_vhd_parents ~table ~excluded_devices snap
        )
        vm.API.vM_snapshots ;
    (* If VM is suspended then add the suspend_VDI *)
    let vdi = vm.API.vM_suspend_VDI in
    if
      preserve_power_state
      && vm.API.vM_power_state = `Suspended
      && Db.is_valid_ref __context vdi
    then
      add_vdi vdi ;
    (* Add also the metrics and guest metrics *)
    add vm.API.vM_metrics ;
    add vm.API.vM_guest_metrics ;
    (* Add the hosts links *)
    add vm.API.vM_resident_on ;
    add vm.API.vM_affinity ;
    (* Add the parent VM *)
    if include_snapshots && Db.is_valid_ref __context vm.API.vM_parent then
      update_table ~__context ~include_snapshots:false ~preserve_power_state
        ~include_vhd_parents ~table ~excluded_devices vm.API.vM_parent
  )

(** Walk the graph of objects and update the table of Ref -> ids for each object we wish
    to include in the output. Other object references will be purged. *)
let create_table () = Hashtbl.create 10

(** Convert an internal reference into an external one or NULL *)
let lookup table r =
  match Hashtbl.find_opt table r with
  | Some x ->
      Ref.of_string x
  | None ->
      Ref.null

(** Convert a list of internal references into external references, filtering out NULLs *)
let filter table rs =
  List.filter (fun x -> x <> Ref.null) (List.map (lookup table) rs)

(** Convert a Host to an obj *)
let make_host table __context self =
  let host = Db.Host.get_record ~__context ~self in
  let host =
    {
      host with
      API.host_PIFs= []
    ; API.host_PBDs= []
    ; API.host_PGPUs= []
    ; API.host_PCIs= []
    ; API.host_host_CPUs= []
    ; API.host_license_params= []
    ; API.host_blobs= []
    ; API.host_external_auth_type= ""
    ; API.host_external_auth_service_name= ""
    ; API.host_external_auth_configuration= []
    ; API.host_metrics= Ref.null
    ; API.host_patches= []
    ; API.host_crashdumps= []
    ; API.host_logging= []
    ; API.host_supported_bootloaders= []
    ; API.host_cpu_configuration= []
    ; API.host_other_config= []
    ; API.host_capabilities= []
    ; API.host_software_version= []
    ; API.host_sched_policy= ""
    ; API.host_ha_statefiles= []
    ; API.host_ha_network_peers= []
    ; API.host_tags= []
    ; API.host_crash_dump_sr=
        lookup table (Ref.string_of host.API.host_crash_dump_sr)
    ; API.host_suspend_image_sr=
        lookup table (Ref.string_of host.API.host_suspend_image_sr)
    ; API.host_resident_VMs=
        List.filter (( <> ) Ref.null)
          (List.map
             (fun vm -> lookup table (Ref.string_of vm))
             host.API.host_resident_VMs
          )
    }
  in
  {
    cls= Datamodel_common._host
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_host_t host
  }

(** Convert a VM reference to an obj *)
let make_vm ?(with_snapshot_metadata = false) ~preserve_power_state table
    __context self =
  let vm = Db.VM.get_record ~__context ~self in
  let vM_VTPMs = filter table (List.map Ref.string_of vm.API.vM_VTPMs) in
  let vm =
    {
      vm with
      API.vM_power_state=
        (if preserve_power_state then vm.API.vM_power_state else `Halted)
    ; API.vM_suspend_VDI=
        ( if preserve_power_state then
            lookup table (Ref.string_of vm.API.vM_suspend_VDI)
          else
            Ref.null
        )
    ; API.vM_is_a_snapshot=
        (if with_snapshot_metadata then vm.API.vM_is_a_snapshot else false)
    ; API.vM_snapshot_of=
        ( if with_snapshot_metadata then
            lookup table (Ref.string_of vm.API.vM_snapshot_of)
          else
            Ref.null
        )
    ; API.vM_snapshots=
        (if with_snapshot_metadata then vm.API.vM_snapshots else [])
    ; API.vM_snapshot_time=
        (if with_snapshot_metadata then vm.API.vM_snapshot_time else Date.epoch)
    ; API.vM_transportable_snapshot_id=
        ( if with_snapshot_metadata then
            vm.API.vM_transportable_snapshot_id
          else
            ""
        )
    ; API.vM_parent=
        ( if with_snapshot_metadata then
            lookup table (Ref.string_of vm.API.vM_parent)
          else
            Ref.null
        )
    ; API.vM_current_operations= []
    ; API.vM_allowed_operations= []
    ; API.vM_VIFs= filter table (List.map Ref.string_of vm.API.vM_VIFs)
    ; API.vM_VBDs= filter table (List.map Ref.string_of vm.API.vM_VBDs)
    ; API.vM_VGPUs= filter table (List.map Ref.string_of vm.API.vM_VGPUs)
    ; API.vM_crash_dumps= []
    ; API.vM_VTPMs
    ; API.vM_resident_on= lookup table (Ref.string_of vm.API.vM_resident_on)
    ; API.vM_affinity= lookup table (Ref.string_of vm.API.vM_affinity)
    ; API.vM_consoles= []
    ; API.vM_metrics= lookup table (Ref.string_of vm.API.vM_metrics)
    ; API.vM_guest_metrics= lookup table (Ref.string_of vm.API.vM_guest_metrics)
    ; API.vM_protection_policy= Ref.null
    ; API.vM_bios_strings= vm.API.vM_bios_strings
    ; API.vM_blobs= []
    }
  in
  {
    cls= Datamodel_common._vm
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_vM_t vm
  }

(** Convert a VM metrics reference to an obj *)
let make_vmm table __context self =
  let vmm = Db.VM_metrics.get_record ~__context ~self in
  {
    cls= Datamodel_common._vm_metrics
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_vM_metrics_t vmm
  }

(** Convert a guest-metrics reference to an obj *)
let make_gm table __context self =
  let gm = Db.VM_guest_metrics.get_record ~__context ~self in
  {
    cls= Datamodel_common._vm_guest_metrics
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_vM_guest_metrics_t gm
  }

(** Convert a VIF reference to an obj *)
let make_vif table ~preserve_power_state __context self =
  let vif = Db.VIF.get_record ~__context ~self in
  let vif =
    {
      vif with
      API.vIF_currently_attached=
        (if preserve_power_state then vif.API.vIF_currently_attached else false)
    ; API.vIF_network= lookup table (Ref.string_of vif.API.vIF_network)
    ; API.vIF_VM= lookup table (Ref.string_of vif.API.vIF_VM)
    ; API.vIF_metrics= Ref.null
    ; API.vIF_current_operations= []
    ; API.vIF_allowed_operations= []
    }
  in
  {
    cls= Datamodel_common._vif
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_vIF_t vif
  }

(** Convert a Network reference to an obj *)
let make_network table __context self =
  let net = Db.Network.get_record ~__context ~self in
  let net =
    {
      net with
      API.network_VIFs=
        filter table (List.map Ref.string_of net.API.network_VIFs)
    ; API.network_PIFs= []
    ; API.network_current_operations= []
    ; API.network_allowed_operations= []
    }
  in
  {
    cls= Datamodel_common._network
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_network_t net
  }

(** Convert a VBD reference to an obj *)
let make_vbd table ~preserve_power_state __context self =
  let vbd = Db.VBD.get_record ~__context ~self in
  let vbd =
    {
      vbd with
      API.vBD_currently_attached=
        (if preserve_power_state then vbd.API.vBD_currently_attached else false)
    ; API.vBD_VDI= lookup table (Ref.string_of vbd.API.vBD_VDI)
    ; API.vBD_VM= lookup table (Ref.string_of vbd.API.vBD_VM)
    ; API.vBD_metrics= Ref.null
    ; API.vBD_current_operations= []
    ; API.vBD_allowed_operations= []
    }
  in
  {
    cls= Datamodel_common._vbd
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_vBD_t vbd
  }

(** Convert a VDI reference to an obj *)
let make_vdi table __context self =
  let vdi = Db.VDI.get_record ~__context ~self in
  let vdi =
    {
      vdi with
      API.vDI_VBDs= filter table (List.map Ref.string_of vdi.API.vDI_VBDs)
    ; API.vDI_crash_dumps= []
    ; API.vDI_SR= lookup table (Ref.string_of vdi.API.vDI_SR)
    ; API.vDI_current_operations= []
    ; API.vDI_allowed_operations= []
    }
  in
  {
    cls= Datamodel_common._vdi
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_vDI_t vdi
  }

(** Convert a SR reference to an obj *)
let make_sr table __context self =
  let sr = Db.SR.get_record ~__context ~self in
  let sr =
    {
      sr with
      API.sR_VDIs= filter table (List.map Ref.string_of sr.API.sR_VDIs)
    ; API.sR_PBDs= []
    ; API.sR_current_operations= []
    ; API.sR_allowed_operations= []
    }
  in
  {
    cls= Datamodel_common._sr
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_sR_t sr
  }

(** Convert a VGPU_type reference to an obj *)
let make_vgpu_type table __context self =
  let vgpu_type = Db.VGPU_type.get_record ~__context ~self in
  {
    cls= Datamodel_common._vgpu_type
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_vGPU_type_t vgpu_type
  }

(** Convert a VGPU reference to an obj *)
let make_vgpu table ~preserve_power_state __context self =
  let vgpu = Db.VGPU.get_record ~__context ~self in
  let vgpu =
    {
      vgpu with
      API.vGPU_currently_attached=
        ( if preserve_power_state then
            vgpu.API.vGPU_currently_attached
          else
            false
        )
    ; API.vGPU_GPU_group= lookup table (Ref.string_of vgpu.API.vGPU_GPU_group)
    ; API.vGPU_type= lookup table (Ref.string_of vgpu.API.vGPU_type)
    ; API.vGPU_VM= lookup table (Ref.string_of vgpu.API.vGPU_VM)
    }
  in
  {
    cls= Datamodel_common._vgpu
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_vGPU_t vgpu
  }

(** Convert a GPU_group reference to an obj *)
let make_gpu_group table __context self =
  let group = Db.GPU_group.get_record ~__context ~self in
  let group =
    {
      group with
      API.gPU_group_VGPUs=
        filter table (List.map Ref.string_of group.API.gPU_group_VGPUs)
    ; API.gPU_group_PGPUs= []
    }
  in
  {
    cls= Datamodel_common._gpu_group
  ; id= Ref.string_of (lookup table (Ref.string_of self))
  ; snapshot= API.rpc_of_gPU_group_t group
  }

let make_pvs_proxies table __context self =
  debug "exporting PVS Proxy %s" (Ref.string_of self) ;
  let lookup' ref = lookup table (Ref.string_of ref) in
  let proxy = Db.PVS_proxy.get_record ~__context ~self in
  let proxy =
    {
      proxy with
      API.pVS_proxy_site= lookup' proxy.API.pVS_proxy_site
    ; API.pVS_proxy_VIF= lookup' proxy.API.pVS_proxy_VIF
    ; API.pVS_proxy_currently_attached= false (* default on dest *)
    }
  in
  {
    cls= Datamodel_common._pvs_proxy
  ; id= Ref.string_of (lookup' self)
  ; snapshot= API.rpc_of_pVS_proxy_t proxy
  }

let make_pvs_sites table __context self =
  debug "exporting PVS Site %s" (Ref.string_of self) ;
  let lookup' ref = lookup table (Ref.string_of ref) in
  let filter' refs = filter table (List.map Ref.string_of refs) in
  let site = Db.PVS_site.get_record ~__context ~self in
  let site =
    {
      site with
      API.pVS_site_cache_storage= [] (* don't export *)
    ; API.pVS_site_servers= [] (* don't export *)
    ; API.pVS_site_proxies= filter' site.API.pVS_site_proxies
    }
  in
  {
    cls= Datamodel_common._pvs_site
  ; id= Ref.string_of (lookup' self)
  ; snapshot= API.rpc_of_pVS_site_t site
  }

let make_vtpm table __context self =
  debug "exporting vtpm %s" (Ref.string_of self) ;
  let lookup' ref = lookup table (Ref.string_of ref) in
  let vtpm = Db.VTPM.get_record ~__context ~self in
  let secret = Xapi_vtpm.get_contents ~__context ~self in
  (* we are using a hand-crafted record that we serialise. The default
     API type does not include the value of the VTPM *)
  let vtpm' =
    {
      vTPM'_VM= lookup' vtpm.vTPM_VM
    ; vTPM'_uuid= vtpm.vTPM_uuid
    ; vTPM'_is_unique= vtpm.vTPM_is_unique
    ; vTPM'_is_protected= vtpm.vTPM_is_protected
    ; vTPM'_content= secret
    }
  in
  {
    cls= Datamodel_common._vtpm
  ; id= Ref.string_of (lookup' self)
  ; snapshot= rpc_of_vtpm' vtpm'
  }

let make_all ~with_snapshot_metadata ~preserve_power_state table __context =
  let filter table rs =
    List.filter (fun x -> lookup table (Ref.string_of x) <> Ref.null) rs
  in
  let hosts =
    List.map
      (make_host table __context)
      (filter table (Db.Host.get_all ~__context))
  in
  let vms =
    List.map
      (make_vm ~with_snapshot_metadata ~preserve_power_state table __context)
      (filter table (Db.VM.get_all ~__context))
  in
  let vmms =
    List.map (make_vmm table __context)
      (filter table (Db.VM_metrics.get_all ~__context))
  in
  let gms =
    List.map (make_gm table __context)
      (filter table (Db.VM_guest_metrics.get_all ~__context))
  in
  let vbds =
    List.map
      (make_vbd ~preserve_power_state table __context)
      (filter table (Db.VBD.get_all ~__context))
  in
  let vifs =
    List.map
      (make_vif ~preserve_power_state table __context)
      (filter table (Db.VIF.get_all ~__context))
  in
  let nets =
    List.map
      (make_network table __context)
      (filter table (Db.Network.get_all ~__context))
  in
  let vdis =
    List.map (make_vdi table __context)
      (filter table (Db.VDI.get_all ~__context))
  in
  let srs =
    List.map (make_sr table __context) (filter table (Db.SR.get_all ~__context))
  in
  let vgpu_types =
    List.map
      (make_vgpu_type table __context)
      (filter table (Db.VGPU_type.get_all ~__context))
  in
  let vgpus =
    List.map
      (make_vgpu ~preserve_power_state table __context)
      (filter table (Db.VGPU.get_all ~__context))
  in
  let gpu_groups =
    List.map
      (make_gpu_group table __context)
      (filter table (Db.GPU_group.get_all ~__context))
  in
  let pvs_proxies =
    List.map
      (make_pvs_proxies table __context)
      (filter table (Db.PVS_proxy.get_all ~__context))
  in
  let pvs_sites =
    List.map
      (make_pvs_sites table __context)
      (filter table (Db.PVS_site.get_all ~__context))
  in
  let vtpms =
    List.map
      (make_vtpm table __context)
      (filter table (Db.VTPM.get_all ~__context))
  in
  List.concat
    [
      hosts
    ; vms
    ; vmms
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
    ; vtpms
    ]

(* on normal export, do not include snapshot metadata;
   on metadata-export, include snapshots fields of the exported VM as well as the VM records of VMs
   which are snapshots of the exported VM. *)
let vm_metadata ~with_snapshot_metadata ~preserve_power_state
    ~include_vhd_parents ~__context ~vms ~excluded_devices =
  let table = create_table () in
  List.iter
    (update_table ~__context ~include_snapshots:with_snapshot_metadata
       ~preserve_power_state ~include_vhd_parents ~table ~excluded_devices
    )
    vms ;
  let objects =
    make_all ~with_snapshot_metadata ~preserve_power_state table __context
  in
  let header = {version= this_version __context; objects} in
  let ova_xml = Xmlrpc.to_string (rpc_of_header header) in
  (table, ova_xml)

let string_of_vm ~__context vm =
  try
    Printf.sprintf "'%s' ('%s')"
      (Db.VM.get_uuid ~__context ~self:vm)
      (Db.VM.get_name_label ~__context ~self:vm)
  with _ -> "invalid"

(** Export a VM's metadata only *)
let export_metadata ~__context ~with_snapshot_metadata ~preserve_power_state
    ~include_vhd_parents ~vms ~excluded_devices s =
  let infomsg vm =
    info
      "VM.export_metadata: VM = %s; with_snapshot_metadata = '%b'; \
       include_vhd_parents = '%b'; preserve_power_state = '%s'; \
       excluded_devices = '%s'"
      vm with_snapshot_metadata include_vhd_parents
      (string_of_bool preserve_power_state)
      (String.concat ", " (List.map Devicetype.to_string excluded_devices))
  in
  let now = Date.now () |> Date.to_unix_time |> Int64.of_float in
  ( match vms with
  | [] ->
      failwith "need to specify at least one VM"
  | [vm] ->
      infomsg (string_of_vm ~__context vm)
  | vms ->
      infomsg (String.concat ", " (List.map (string_of_vm ~__context) vms))
  ) ;
  let _, ova_xml =
    vm_metadata ~with_snapshot_metadata ~preserve_power_state
      ~include_vhd_parents ~__context ~vms ~excluded_devices
  in
  let hdr =
    Tar.Header.make ~mod_time:now Xapi_globs.ova_xml_filename
      (Int64.of_int @@ String.length ova_xml)
  in
  Tar_helpers.write_block hdr (fun s -> Unixext.really_write_string s ova_xml) s ;
  Tar_helpers.write_end s

let export refresh_session __context rpc session_id s vm_ref
    preserve_power_state =
  let now = Date.now () |> Date.to_unix_time |> Int64.of_float in
  info "VM.export: VM = %s; preserve_power_state = '%s'"
    (string_of_vm ~__context vm_ref)
    (string_of_bool preserve_power_state) ;
  let table, ova_xml =
    vm_metadata ~with_snapshot_metadata:false ~preserve_power_state
      ~include_vhd_parents:false ~__context ~vms:[vm_ref] ~excluded_devices:[]
  in
  debug "Outputting ova.xml" ;
  let hdr =
    Tar.Header.make ~mod_time:now Xapi_globs.ova_xml_filename
      (Int64.of_int @@ String.length ova_xml)
  in
  Tar_helpers.write_block hdr (fun s -> Unixext.really_write_string s ova_xml) s ;
  (* Only stream the disks that are in the table AND which are not CDROMs (ie whose VBD.type <> CD
     and whose SR.content_type <> "iso" *)
  let vbds = Db.VM.get_VBDs ~__context ~self:vm_ref in
  let vbds =
    List.filter (fun x -> Db.VBD.get_type ~__context ~self:x <> `CD) vbds
  in
  let vdis = List.map (fun self -> Db.VBD.get_VDI ~__context ~self) vbds in
  (* Don't forget the suspend VDI (if we allow export of suspended VMs) *)
  let vdis =
    match Db.VM.get_power_state ~__context ~self:vm_ref with
    | `Suspended when preserve_power_state ->
        Db.VM.get_suspend_VDI ~__context ~self:vm_ref :: vdis
    | _ ->
        vdis
  in
  let vdis =
    List.filter
      (fun self ->
        Db.SR.get_content_type ~__context ~self:(Db.VDI.get_SR ~__context ~self)
        <> "iso"
      )
      vdis
  in
  let vdis =
    List.filter (fun vdi -> Hashtbl.mem table (Ref.string_of vdi)) vdis
  in
  let vdis =
    List.map
      (fun vdi ->
        ( Hashtbl.find table (Ref.string_of vdi)
        , vdi
        , Db.VDI.get_virtual_size ~__context ~self:vdi
        )
      )
      vdis
  in
  Stream_vdi.send_all refresh_session s ~__context rpc session_id vdis ;
  (* We no longer write the end-of-tar checksum table, preferring the inline ones instead *)
  Tar_helpers.write_end s ;
  debug "export VM = %s completed successfully" (Ref.string_of vm_ref)

open Http
open Client

let lock_vm ~__context ~vm ~task_id op =
  Helpers.retry ~__context ~doc:task_id ~policy:Helpers.Policy.fail_quickly
    (fun () ->
      (* Note slight race here because we haven't got the master lock *)
      Xapi_vm_lifecycle.assert_operation_valid ~__context ~self:vm ~op
        ~strict:true ;
      (* ... small race lives here ... *)
      Db.VM.add_to_current_operations ~__context ~self:vm ~key:task_id ~value:op ;
      Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm
  )

let unlock_vm ~__context ~vm ~task_id =
  Db.VM.remove_from_current_operations ~__context ~self:vm ~key:task_id ;
  Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm

let with_vm_locked ~__context ~vm ~task_id op f =
  lock_vm ~__context ~vm ~task_id op ;
  finally f (fun () -> unlock_vm ~__context ~vm ~task_id)

let vm_from_request ~__context (req : Request.t) =
  if List.mem_assoc "ref" req.Request.query then
    Ref.of_string (List.assoc "ref" req.Request.query)
  else
    let uuid = List.assoc "uuid" req.Request.query in
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.VM.get_by_uuid ~rpc ~session_id ~uuid
    )

let arg_from_request (req : Request.t) k = List.assoc_opt k req.Request.query

let bool_from_request req default k =
  arg_from_request req k |> Option.fold ~none:default ~some:bool_of_string

let devicetypelist_from_request req default k =
  let to_list = function
    | "" ->
        []
    | x ->
        String.split_on_char ',' x |> List.map Devicetype.of_string
  in
  arg_from_request req k |> Option.fold ~none:default ~some:to_list

let export_all_vms_from_request req = bool_from_request req false "all"

let include_vhd_parents_from_request req =
  bool_from_request req false "include_vhd_parents"

let export_snapshots_from_request req =
  bool_from_request req true "export_snapshots"

let include_dom0_from_request req = bool_from_request req true "include_dom0"

let excluded_devices_from_request req =
  devicetypelist_from_request req [] "excluded_device_types"

let metadata_handler (req : Request.t) s _ =
  debug "metadata_handler called" ;
  req.Request.close <- true ;
  (* Xapi_http.with_context always completes the task at the end *)
  Xapi_http.with_context "VM.export_metadata" req s (fun __context ->
      let include_vhd_parents = include_vhd_parents_from_request req in
      let export_all = export_all_vms_from_request req in
      let export_snapshots = export_snapshots_from_request req in
      let include_dom0 = include_dom0_from_request req in
      let excluded_devices = excluded_devices_from_request req in
      (* Get the VM refs. In case of exporting the metadata of a particular VM, return a singleton list containing the vm ref.  *)
      (* In case of exporting all the VMs metadata, get all the VM records which are not default templates. *)
      let vm_refs =
        if export_all then
          let is_default_template vm =
            vm.API.vM_is_default_template
            || vm.API.vM_is_a_template
               && List.mem_assoc Xapi_globs.default_template_key
                    vm.API.vM_other_config
               && List.assoc Xapi_globs.default_template_key
                    vm.API.vM_other_config
                  = "true"
          in
          let all_vms = Db.VM.get_all_records ~__context in
          let interesting_vms =
            List.filter
              (fun (vm, vmr) ->
                (not (is_default_template vmr))
                && ((not (Helpers.is_domain_zero ~__context vm)) || include_dom0)
              )
              all_vms
          in
          List.map fst interesting_vms
        else
          [vm_from_request ~__context req]
      in
      let task_id = Ref.string_of (Context.get_task_id __context) in
      let read_fd, write_fd = Unix.pipe () in
      let export_error = ref None in
      let writer_thread =
        Thread.create
          (Debug.with_thread_named "metadata export writer thread" (fun () ->
               try
                 (* lock all the VMs before exporting their metadata *)
                 let locked_vms = ref [] in
                 finally
                   (fun () ->
                     List.iter
                       (fun vm ->
                         lock_vm ~__context ~vm ~task_id `metadata_export ;
                         locked_vms := vm :: !locked_vms
                       )
                       vm_refs ;
                     export_metadata ~with_snapshot_metadata:export_snapshots
                       ~preserve_power_state:true ~include_vhd_parents
                       ~excluded_devices ~__context ~vms:vm_refs write_fd
                   )
                   (fun () ->
                     Unix.close write_fd ;
                     List.iter
                       (fun vm -> unlock_vm ~__context ~vm ~task_id)
                       !locked_vms
                   )
               with e ->
                 Backtrace.is_important e ;
                 export_error := Some e ;
                 raise e
           )
          )
          ()
      in
      let tar_data = Unixext.string_of_fd read_fd in
      Thread.join writer_thread ;
      Unix.close read_fd ;
      match !export_error with
      | None ->
          let content_length = String.length tar_data in
          let headers =
            Http.http_200_ok ~keep_alive:false ~version:"1.0" ()
            @ [
                Http.Hdr.task_id ^ ": " ^ task_id
              ; "Server: " ^ Xapi_version.xapi_user_agent
              ; content_type
              ; "Content-Length: " ^ string_of_int content_length
              ; "Content-Disposition: attachment; filename=\"export.xva\""
              ]
          in
          Http_svr.headers s headers ;
          Unixext.really_write_string s tar_data
      | Some e ->
          let response_string = Http.Response.(to_wire_string internal_error) in
          Unixext.really_write_string s response_string ;
          error "Caught %s while exporting metadata - responding with HTTP 500"
            (Printexc.to_string e) ;
          raise e
  )

let handler (req : Request.t) s _ =
  debug "export handler" ;
  req.Request.close <- true ;
  (* First things first, let's make sure that the request has a valid session or username/password *)
  Xapi_http.assert_credentials_ok "VM.export" ~http_action:"get_export" req s ;
  let open Compression_algorithms in
  let compression_algorithm =
    if List.mem_assoc Constants.use_compression req.Request.query then
      match List.assoc Constants.use_compression req.Request.query with
      | "true" | "gzip" ->
          Some Gzip
      | "zstd" ->
          Some Zstd
      | _ ->
          None
    else
      None
  in
  debug "Using compression: %s"
    ( match compression_algorithm with
    | Some Gzip ->
        "Gzip"
    | Some Zstd ->
        "Zstd"
    | None ->
        "None"
    ) ;
  (* Perform the SR reachability check using a fresh context/task because
     we don't want to complete the task in the forwarding case *)
  Server_helpers.exec_with_new_task "VM.export" (fun __context ->
      (* The VM Ref *)
      let vm_ref = vm_from_request ~__context req in
      let localhost = Helpers.get_localhost ~__context in
      let host_ok = check_vm_host_SRs ~__context vm_ref localhost in
      if not host_ok (* redirect *) then (
        (* We do this outside the Xapi_http.with_context below since that will complete the *)
        (* task when it exits, and we don't want to do that *)
        try
          let host = find_host_for_VM ~__context vm_ref in
          let address = Db.Host.get_address ~__context ~self:host in
          let url =
            Uri.(
              make ~scheme:"https" ~host:address ~path:req.Request.path
                ~query:(List.map (fun (a, b) -> (a, [b])) req.Request.query)
                ()
              |> to_string
            )
          in
          info "export VM = %s redirecting to: %s" (Ref.string_of vm_ref) url ;
          let headers = Http.http_302_redirect url in
          Http_svr.headers s headers
        with
        | Api_errors.Server_error _ as e -> (
            error "Caught exception in export handler: %s"
              (ExnHelper.string_of_exn e) ;
            (* If there's no host that can see the SRs, then it's actually our responsibility *)
            (* to complete the task *)
            let task_id =
              let all = req.Request.cookie @ req.Request.query in
              if List.mem_assoc "task_id" all then
                Some (Ref.of_string (List.assoc "task_id" all))
              else
                None
            in
            match task_id with
            | None ->
                Server_helpers.exec_with_new_task "export"
                  ~task_in_database:true (fun __context ->
                    TaskHelper.failed ~__context e
                )
            | Some task_id ->
                Server_helpers.exec_with_forwarded_task task_id
                  (fun __context -> TaskHelper.failed ~__context e
                )
          )
        | e ->
            error "Caught exception in export handler: %s" (Printexc.to_string e) ;
            raise e
      ) else (
        (* Xapi_http.with_context always completes the task at the end *)
        debug "Doing xapi_http.with_context now..." ;
        Xapi_http.with_context "VM.export" req s (fun __context ->
            Helpers.call_api_functions ~__context (fun rpc session_id ->
                (* This is the signal to say we've taken responsibility from the CLI server for completing the task *)
                (* The GUI can deal with this itself, but the CLI is complicated by the thin cli/cli server split *)
                TaskHelper.set_progress ~__context 0.0 ;
                let refresh_session =
                  Xapi_session.consider_touching_session rpc session_id
                in
                let task_id = Ref.string_of (Context.get_task_id __context) in
                let preserve_power_state =
                  let all = req.Request.cookie @ req.Request.query in
                  List.mem_assoc "preserve_power_state" all
                  && bool_of_string (List.assoc "preserve_power_state" all)
                in
                let headers =
                  Http.http_200_ok ~keep_alive:false ~version:"1.0" ()
                  @ [
                      Http.Hdr.task_id ^ ": " ^ task_id
                    ; "Server: " ^ Xapi_version.xapi_user_agent
                    ; content_type
                    ; "Content-Disposition: attachment; filename=\"export.xva\""
                    ]
                in
                with_vm_locked ~__context ~vm:vm_ref ~task_id `export (fun () ->
                    Http_svr.headers s headers ;
                    let go fd =
                      export refresh_session __context rpc session_id fd vm_ref
                        preserve_power_state
                    in
                    match compression_algorithm with
                    | Some Gzip ->
                        Gzip.Default.compress s go
                    | Some Zstd ->
                        Zstd.Default.compress s go
                    | None ->
                        go s
                )
                (* Exceptions are handled by Xapi_http.with_context *)
            )
        )
      )
  )
