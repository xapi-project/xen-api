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
(** Code to bring the database up-to-date when a host starts up.
 *  @group Main Loop and Start-up
*)

module Rrdd = Rrd_client.Client

open Stdext
open Fun
open Xstringext
open Listext
open Printf
open Create_misc
open Client
open Pervasiveext

open Network

module D=Debug.Make(struct let name="dbsync" end)
open D

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( ** ) = Int64.mul
let ( // ) = Int64.div

let get_my_ip_addr ~__context =
  match (Helpers.get_management_ip_addr ~__context) with
    Some ip -> ip
  | None -> (error "Cannot read IP address. Check the control interface has an IP address"; "")


let create_localhost ~__context info =
  let ip = get_my_ip_addr ~__context in
  let me = try Some (Db.Host.get_by_uuid ~__context ~uuid:info.uuid) with _ -> None in
  (* me = None on firstboot only *)
  if me = None
  then
    let (_: API.ref_host) =
      Xapi_host.create ~__context ~uuid:info.uuid ~name_label:info.hostname ~name_description:""
        ~hostname:info.hostname ~address:ip
        ~external_auth_type:"" ~external_auth_service_name:"" ~external_auth_configuration:[]
        ~license_params:[] ~edition:"" ~license_server:["address", "localhost"; "port", "27000"]
        ~local_cache_sr:Ref.null ~chipset_info:[] ~ssl_legacy:info.ssl_legacy
    in ()

(* TODO cat /proc/stat for btime ? *)
let get_start_time () =
  try
    debug "Calculating boot time...";
    let now = Unix.time () in
    let uptime = Unixext.string_of_file "/proc/uptime" in
    let uptime = String.strip String.isspace uptime in
    let uptime = String.split ' ' uptime in
    let uptime = List.hd uptime in
    let uptime = float_of_string uptime in
    let boot_time = Date.of_float (now -. uptime) in
    debug " system booted at %s" (Date.to_string boot_time);
    boot_time
  with
    e ->
    debug "Calculating boot time failed with '%s'" (ExnHelper.string_of_exn e);
    Date.never

(** Update the information in the Host structure *)
(* not sufficient just to fill in this data on create time [Xen caps may change if VT enabled in BIOS etc.] *)
let refresh_localhost_info ~__context info =
  let host = !Xapi_globs.localhost_ref in

  (* Xapi_ha_flags.resync_host_armed_flag __context host; *)
  debug "Updating host software_version and updates_requiring_reboot";

  Create_misc.create_updates_requiring_reboot_info ~__context ~host;
  Create_misc.create_software_version ~__context;
  Db.Host.set_API_version_major ~__context ~self:host ~value:Datamodel.api_version_major;
  Db.Host.set_API_version_minor ~__context ~self:host ~value:Datamodel.api_version_minor;
  Db.Host.set_virtual_hardware_platform_versions ~__context ~self:host ~value:Xapi_globs.host_virtual_hardware_platform_versions;
  Db.Host.set_hostname ~__context ~self:host ~value:info.hostname;
  let caps = try
      String.split ' ' (Xenctrl.with_intf (fun xc -> Xenctrl.version_capabilities xc))
    with _ ->
      warn "Unable to query hypervisor capabilities";
      [] in
  Db.Host.set_capabilities ~__context ~self:host ~value:caps;
  Db.Host.set_address ~__context ~self:host ~value:(get_my_ip_addr ~__context);

  let boot_time_key = "boot_time" in
  let boot_time_value = string_of_float (Date.to_float (get_start_time ())) in

  Db.Host.remove_from_other_config ~__context ~self:host ~key:boot_time_key;
  Db.Host.add_to_other_config ~__context ~self:host ~key:boot_time_key ~value:boot_time_value;

  let agent_start_key = "agent_start_time" in
  let agent_start_time = string_of_float (Unix.time ()) in

  Db.Host.remove_from_other_config ~__context ~self:host ~key:agent_start_key;
  Db.Host.add_to_other_config ~__context ~self:host ~key:agent_start_key ~value:agent_start_time;

  (* Register whether we have local storage or not *)

  if not (Helpers.local_storage_exists ()) then begin
    Db.Host.remove_from_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage;
    Db.Host.add_to_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage ~value:"true"
  end else
    Db.Host.remove_from_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage

(*************** update database tools ******************)


(** Record host memory properties in database *)
let record_host_memory_properties ~__context =
  let self = !Xapi_globs.localhost_ref in
  let total_memory_bytes =
    try
      let xc = Xenctrl.interface_open () in
      Xenctrl.interface_close xc; (* we're on xen *)
      let dbg = Context.string_of_task __context in
      let open Xapi_xenops_queue in
      let module Client = (val make_client (default_xenopsd ()): XENOPS) in
      let mib = Client.HOST.get_total_memory_mib dbg in
      Int64.mul 1024L (Int64.mul 1024L mib)
    with _ ->
      warn "Failed to detect xen, querying /proc/meminfo";
      begin match Balloon.get_memtotal () with
        |  None -> 0L
        | Some x -> Int64.(div x (mul 1024L 1024L))
      end in

  let metrics = Db.Host.get_metrics ~__context ~self in
  Db.Host_metrics.set_memory_total ~__context ~self:metrics ~value:total_memory_bytes;
  let boot_memory_bytes =
    try
      let dbg = Context.string_of_task __context in
      Some (Memory_client.Client.get_host_initial_free_memory dbg)
    with e ->
      warn "Failed to get host free memory from ballooning service. This may \
        prevent VMs from being started on this host. (%s)" (Printexc.to_string e);
      None in
  maybe
    (fun boot_memory_bytes ->
       (* Host memory overhead comes from multiple sources:         *)
       (* 1. obvious overhead: (e.g. Xen, crash kernel).            *)
       (*    appears as used memory.                                *)
       (* 2. non-obvious overhead: (e.g. low memory emergency pool) *)
       (*    appears as free memory but can't be used in practice.  *)
       let obvious_overhead_memory_bytes =
         total_memory_bytes -- boot_memory_bytes in
       let nonobvious_overhead_memory_kib =
         try
           Memory_client.Client.get_host_reserved_memory "dbsync"
         with e ->
           error "Failed to contact ballooning service: \
                  						host memory overhead may be too small (%s)"
             (Printexc.to_string e);
           0L
       in
       let nonobvious_overhead_memory_bytes =
         Int64.mul 1024L nonobvious_overhead_memory_kib in
       Db.Host.set_boot_free_mem ~__context ~self
         ~value:boot_memory_bytes;
       Db.Host.set_memory_overhead ~__context ~self ~value:
         (obvious_overhead_memory_bytes ++ nonobvious_overhead_memory_bytes);
    )
    boot_memory_bytes

(* -- used this for testing uniqueness constraints executed on slave do not kill connection.
   Committing commented out vsn of this because it might be useful again..
   let test_uniqueness_doesnt_kill_us ~__context =
   let duplicate_uuid = Uuid.to_string (Uuid.make_uuid()) in
    Db.Network.create ~__context ~ref:(Ref.make()) ~uuid:duplicate_uuid
      ~current_operations:[] ~allowed_operations:[]
      ~name_label:"Test uniqueness constraint"
      ~name_description:"Testing"
      ~bridge:"bridge" ~other_config:[] ~purpose:[];
    Db.Network.create ~__context ~ref:(Ref.make()) ~uuid:duplicate_uuid
      ~current_operations:[] ~allowed_operations:[]
      ~name_label:"Test uniqueness constraint"
      ~name_description:"Testing"
      ~bridge:"bridge" ~other_config:[] ~purpose:[];
    ()
*)

(** Make sure the PIF we're using as a management interface is marked as attached
    otherwise we might blow it away by accident *)
(* CA-23803:
 * As well as marking the management interface as attached, mark any other important
 * interface (defined by what is brought up before xapi starts) as attached too.
 * For example, this will prevent needless glitches in storage interfaces.
*)
let resynchronise_pif_params ~__context =
  let localhost = Helpers.get_localhost ~__context in

  (* Determine all bridges that are currently up, and ask the master to sync the currently_attached
     	 * fields on all my PIFs *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let dbg = Context.string_of_task __context in
      let bridges = Net.Bridge.get_all dbg () in
      Client.Host.sync_pif_currently_attached rpc session_id localhost bridges
    );

  (* sync management *)
  Xapi_pif.update_management_flags ~__context ~host:localhost;

  (* sync MACs and MTUs *)
  Xapi_pif.refresh_all ~__context ~host:localhost;

  (* Ensure that all DHCP PIFs have their IP address updated in the DB *)
  Helpers.update_pif_addresses ~__context

(** Update the database to reflect current state. Called for both start of day and after
    an agent restart. *)
let update_env __context sync_keys =

  (* Helper function to allow us to switch off particular types of syncing *)
  let switched_sync key f =
    let task_id = Context.get_task_id __context in
    Db.Task.remove_from_other_config ~__context ~self:task_id ~key:"sync_operation";
    Db.Task.add_to_other_config ~__context ~self:task_id ~key:"sync_operation" ~value:key;
    let skip_sync =
      try
        List.assoc key sync_keys = Xapi_globs.sync_switch_off
      with _ -> false
    in
    let disabled_in_config_file = List.mem key !Xapi_globs.disable_dbsync_for in
    begin
      if (not skip_sync) && (not disabled_in_config_file)
      then (debug "Sync: %s" key; f ())
      else debug "Skipping sync keyed: %s" key
    end;
    Db.Task.remove_from_other_config ~__context ~self:task_id ~key:"sync_operation"
  in

  (* Ensure basic records exist: *)

  let info = Create_misc.read_localhost_info () in

  (* create localhost record if doesn't already exist *)
  switched_sync Xapi_globs.sync_create_localhost (fun () ->
      debug "creating localhost";
      create_localhost ~__context info;
    );

  (* record who we are in xapi_globs *)
  Xapi_globs.localhost_ref := Helpers.get_localhost ~__context;

  switched_sync Xapi_globs.sync_set_cache_sr (fun () ->
      try
        let cache_sr = Db.Host.get_local_cache_sr ~__context ~self:(Helpers.get_localhost ~__context) in
        let cache_sr_uuid = Db.SR.get_uuid ~__context ~self:cache_sr in
        Db.SR.set_local_cache_enabled ~__context ~self:cache_sr ~value:true;
        log_and_ignore_exn (fun () -> Rrdd.set_cache_sr ~sr_uuid:cache_sr_uuid)
      with _ -> log_and_ignore_exn Rrdd.unset_cache_sr
    );

  switched_sync Xapi_globs.sync_load_rrd (fun () ->
      (* Load the host rrd *)
      Rrdd_proxy.Deprecated.load_rrd ~__context
        ~uuid:(Helpers.get_localhost_uuid ())
    );

  (* maybe record host memory properties in database *)
  switched_sync Xapi_globs.sync_record_host_memory_properties (fun () ->
      record_host_memory_properties ~__context;
    );

  switched_sync Xapi_globs.sync_create_host_cpu (fun () ->
      debug "creating cpu";
      Create_misc.create_host_cpu ~__context;
    );

  let localhost = Helpers.get_localhost ~__context in

  switched_sync Xapi_globs.sync_create_domain_zero (fun () ->
      debug "creating domain 0";
      Create_misc.ensure_domain_zero_records ~__context ~host:localhost info;
    );

  switched_sync Xapi_globs.sync_crashdump_resynchronise (fun () ->
      debug "resynchronising host crashdumps";
      Xapi_host_crashdump.resynchronise ~__context ~host:localhost;
    );

  switched_sync Xapi_globs.sync_pbds (fun () ->
      debug "resynchronising host PBDs";
      Storage_access.resynchronise_pbds ~__context ~pbds:(Db.Host.get_PBDs ~__context ~self:localhost);
    );

(*
  debug "resynchronising db with host physical interfaces";
  update_physical_networks ~__context;
*)

  switched_sync Xapi_globs.sync_pif_params (fun () ->
      debug "resynchronising PIF params";
      resynchronise_pif_params ~__context;
    );

  switched_sync Xapi_globs.sync_bios_strings (fun () ->
      debug "get BIOS strings on startup";
      let current_bios_strings = Bios_strings.get_host_bios_strings ~__context in
      let db_host_bios_strings = Db.Host.get_bios_strings ~__context ~self:localhost in

      if current_bios_strings <> db_host_bios_strings then
        begin
          debug "BIOS strings obtained from the host and that present in DB are different. Updating BIOS strings in xapi-db.";
          Db.Host.set_bios_strings ~__context ~self:localhost ~value:current_bios_strings
        end
    );

  (* CA-35549: In a pool rolling upgrade, the master will detect the end of upgrade when the software versions
     	 of all the hosts are the same. It will then assume that (for example) per-host patch records have
     	 been tidied up and attempt to delete orphaned pool-wide patch records. *)

  (* refresh host info fields *)
  switched_sync Xapi_globs.sync_host_display (fun () ->
      Xapi_host.sync_display ~__context ~host:localhost
    );

  switched_sync Xapi_globs.sync_refresh_localhost_info (fun () ->
      refresh_localhost_info ~__context info;
    );

  switched_sync Xapi_globs.sync_local_vdi_activations (fun () ->
      Storage_access.refresh_local_vdi_activations ~__context;
    );

  switched_sync Xapi_globs.sync_chipset_info (fun () ->
      Create_misc.create_chipset_info ~__context;
    );

  switched_sync Xapi_globs.sync_pci_devices (fun () ->
      Xapi_pci.update_pcis ~__context;
    );

  switched_sync Xapi_globs.sync_gpus (fun () ->
      Xapi_pgpu.update_gpus ~__context;
    );

  switched_sync Xapi_globs.sync_cluster_hosts (fun () ->
      Xapi_cluster_host.sync_cluster_hosts ~__context
    );

