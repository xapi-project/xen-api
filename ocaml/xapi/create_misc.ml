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
(** Create miscellaneous DB records needed by both the real and fake servers.
 * @group Database Operations
*)

open Stdext
open Fun
open Xapi_vm_memory_constraints
open Vm_memory_constraints
open Printf
open Db_filter
open Db_filter_types
open Network
module XenAPI = Client.Client

module D=Debug.Make(struct let name="xapi" end)
open D

type host_info = {
  name_label : string;
  xen_verstring : string;
  linux_verstring : string;
  hostname : string;
  uuid : string;
  dom0_uuid : string;
  oem_manufacturer : string option;
  oem_model : string option;
  oem_build_number : string option;
  machine_serial_number: string option;
  machine_serial_name: string option;
  total_memory_mib: int64;
  dom0_static_max: int64;
  ssl_legacy: bool;
  cpu_info : Xenops_interface.Host.cpu_info;
  chipset_info : Xenops_interface.Host.chipset_info;
  hypervisor : Xenops_interface.Host.hypervisor;
}

(** The format of the response looks like
 *  # xen-livepatch list
 *   ID                                     | status
 *  ----------------------------------------+------------
 *  hp_1_1                                  | CHECKED
 *  hp_2_1                                  | APPLIED
 *  hp_3_2                                  | APPLIED *)
let make_xen_livepatch_list () =
  let lines = try Xstringext.String.split '\n' (Helpers.get_process_output !Xapi_globs.xen_livepatch_list) with _ -> [] in
  let patches = List.fold_left(
      fun acc l ->
        match List.map String.trim (Xstringext.String.split ~limit:2 '|' l) with
        | [ key; "APPLIED" ] -> key :: acc
        | _ -> acc;
    )[] lines in
  if List.length patches > 0 then Some(String.concat ", " patches) else None

(** The format of the response looks like
 *  # kpatch list
 *  Loaded patch modules:
 *  kpatch_hp_1_1
 *  kpatch_hp_2_1

 *  Installed patch modules: *)
let make_kpatch_list () =
  let start_line = "Loaded patch modules:" in
  let end_line = "Installed patch modules:" in
  let lines = try Xstringext.String.split '\n' (Helpers.get_process_output !Xapi_globs.kpatch_list) with _ -> []in
  let rec loop acc started = function
    | []                                  -> acc
    | line :: _    when line = end_line   -> acc
    | line :: rest when line = start_line -> loop acc true rest
    | line :: rest ->
      let line' = String.trim line in
      if line' <> "" && started then
        loop (line' :: acc) true rest
      else
        loop acc started rest
  in
  let patches = loop [] false lines in
  if List.length patches > 0 then Some(String.concat ", " patches) else None

open Xstringext

(** [count_cpus] returns the number of CPUs found in /proc/cpuinfo *)
let count_cpus () =
  let cpuinfo = "/proc/cpuinfo" in
  let re = Re.Perl.compile @@ Re.Perl.re {|^processor\s*:\s+\d+|} in
  let matches line = Re.matches re line <> [] in
  let count n line = if matches line then n+1 else n in
  Stdext.Unixext.file_lines_fold count 0 cpuinfo

(* NB: this is dom0's view of the world, not Xen's. *)
let read_dom0_memory_usage () =
  try
    let map = Balloon.parse_proc_xen_balloon () in
    let lookup = fun x -> Opt.unbox (List.assoc x map) in
    let keys = [Balloon._low_mem_balloon; Balloon._high_mem_balloon; Balloon._current_allocation] in
    let values = List.map lookup keys in
    let result = List.fold_left Int64.add 0L values in
    Some (Int64.mul 1024L result)
  with _ ->
    None

let read_localhost_info ~__context =
  let open Xapi_xenops_queue in
  let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
  let dbg = Context.string_of_task __context in
  let stat = Client.HOST.stat dbg in
  let total_memory_mib = Client.HOST.get_total_memory_mib dbg
  and linux_verstring =
    let verstring = ref "" in
    let f line =
      try verstring := List.nth (String.split ' ' line) 2
      with _ -> () in
    Unixext.readfile_line f "/proc/version";
    !verstring
  in
  let me = Helpers.get_localhost_uuid () in
  let lookup_inventory_nofail k = try Some (Xapi_inventory.lookup k) with _ -> None in
  let this_host_name = Helpers.get_hostname() in

  let dom0_static_max = match read_dom0_memory_usage () with
    | Some x -> x
    | None ->
      info "Failed to query balloon driver, assuming target = static_max";
      Int64.(mul total_memory_mib (mul 1024L 1024L)) in
  {
    name_label=this_host_name;
    xen_verstring=stat.hypervisor.version;
    linux_verstring=linux_verstring;
    hostname=this_host_name;
    uuid=me;
    dom0_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid;
    oem_manufacturer = lookup_inventory_nofail Xapi_inventory._oem_manufacturer;
    oem_model = lookup_inventory_nofail Xapi_inventory._oem_model;
    oem_build_number = lookup_inventory_nofail Xapi_inventory._oem_build_number;
    machine_serial_number = lookup_inventory_nofail Xapi_inventory._machine_serial_number;
    machine_serial_name = lookup_inventory_nofail Xapi_inventory._machine_serial_name;
    total_memory_mib = total_memory_mib;
    dom0_static_max = dom0_static_max;
    cpu_info = stat.cpu_info;
    chipset_info = stat.chipset_info;
    hypervisor = stat.hypervisor;
    ssl_legacy = try (
      bool_of_string (
        Xapi_inventory.lookup Xapi_inventory._stunnel_legacy ~default:"false")
    ) with _ -> true;
  }

(** Returns the maximum of two values. *)
let maximum x y = if x > y then x else y

(** Returns the minimum of two values. *)
let minimum x y = if x < y then x else y

let (+++) = Int64.add

(** Ensures that the database has all the necessary records for domain *)
(** zero, and that the records are up-to-date. Includes the following: *)
(**     1. The domain zero record.                                     *)
(**     2. The domain zero console record.                             *)
(**     3. The domain zero metrics record.                             *)
(** This function makes sure there is exactly one record of each type. *)
(** It updates existing records if they are found, or else creates new *)
(** records for any records that are missing.                          *)
let rec ensure_domain_zero_records ~__context ~host (host_info: host_info) : unit =
  maybe_upgrade_domain_zero_record ~__context ~host host_info;
  let domain_zero_ref = ensure_domain_zero_record ~__context host_info in
  ensure_domain_zero_console_record ~__context ~domain_zero_ref;
  ensure_domain_zero_metrics_record ~__context ~domain_zero_ref host_info

and maybe_upgrade_domain_zero_record ~__context ~host (host_info: host_info) =
  try
    let control_domain = Db.VM.get_by_uuid ~__context ~uuid:host_info.dom0_uuid in
    if Db.Host.get_control_domain ~__context ~self:host = Ref.null then begin
      debug "Setting control domain for host %s to %s"
        (Ref.string_of host) (Ref.string_of control_domain);
      Db.Host.set_control_domain ~__context ~self:host ~value:control_domain;
    end
  with Db_exn.Read_missing_uuid(_) -> ()

and ensure_domain_zero_record ~__context (host_info: host_info): [`VM] Ref.t =
  let ref_lookup () = Helpers.get_domain_zero ~__context in
  let ref_create () = Ref.make () in
  let (domain_zero_ref, found) =
    try       ref_lookup (), true
    with _ -> ref_create (), false in
  if found
  then update_domain_zero_record ~__context ~domain_zero_ref host_info
  else create_domain_zero_record ~__context ~domain_zero_ref host_info;
  domain_zero_ref

and ensure_domain_zero_console_record ~__context ~domain_zero_ref : unit =
  let dom0_consoles =  Db.VM.get_consoles ~__context ~self: domain_zero_ref in
  let console_records_rfb = List.filter (fun x -> Db.Console.get_protocol ~__context ~self:x = `rfb) dom0_consoles in
  let console_records_vt100 = List.filter (fun x -> Db.Console.get_protocol ~__context ~self:x = `vt100) dom0_consoles in

  match console_records_rfb, console_records_vt100 with
  | [rfb], [vt100] ->
    debug "1 RFB, 1 VT100 console found... ensuring correct port numbers";
    Db.Console.set_port ~__context ~self:rfb ~value:Xapi_globs.host_console_vncport;
    Db.Console.set_port ~__context ~self:vt100 ~value:Xapi_globs.host_console_textport;
  | _ ->
    (* if there's not more than one console of each type then something strange is happening*)
    create_domain_zero_console_record ~__context ~domain_zero_ref ~console_records_rfb ~console_records_vt100;

and ensure_domain_zero_metrics_record ~__context ~domain_zero_ref (host_info: host_info) : unit =
  if not (Db.is_valid_ref __context (Db.VM.get_metrics ~__context ~self:domain_zero_ref)) then
    begin
      debug "Domain 0 record does not have associated metrics record. Creating now";
      let metrics_ref = Ref.make() in
      create_domain_zero_metrics_record ~__context ~domain_zero_metrics_ref:metrics_ref ~memory_constraints:(create_domain_zero_memory_constraints host_info)
        ~vcpus:(count_cpus ());
      Db.VM.set_metrics ~__context ~self:domain_zero_ref ~value:metrics_ref
    end

and create_domain_zero_record ~__context ~domain_zero_ref (host_info: host_info) : unit =
  (* Determine domain 0 memory constraints. *)
  let memory = create_domain_zero_memory_constraints host_info in
  (* Determine information about the host machine. *)
  let domarch =
    let i = Int64.of_nativeint (Int64.to_nativeint 0xffffffffL) in
    if i > 0L then "x64" else "x32" in
  let localhost = Helpers.get_localhost ~__context in
  (* Read the control domain uuid from the inventory file *)
  let uuid = host_info.dom0_uuid in
  let vcpus = count_cpus () in
  let metrics = Ref.make () in
  (* Now create the database record. *)
  Db.VM.create ~__context ~ref:domain_zero_ref
    ~name_label:("Control domain on host: " ^ host_info.hostname) ~uuid
    ~name_description:"The domain which manages physical devices and manages other domains"
    ~hVM_boot_policy:"" ~hVM_boot_params:[] ~hVM_shadow_multiplier:1. ~platform:[] ~pCI_bus:""
    ~pV_args:"" ~pV_ramdisk:"" ~pV_kernel:"" ~pV_bootloader:"" ~pV_bootloader_args:"" ~pV_legacy_args:""
    ~actions_after_crash:`destroy ~actions_after_reboot:`destroy ~actions_after_shutdown:`destroy
    ~allowed_operations:[] ~current_operations:[] ~blocked_operations:[] ~power_state:`Running
    ~vCPUs_max:(Int64.of_int vcpus) ~vCPUs_at_startup:(Int64.of_int vcpus) ~vCPUs_params:[]
    ~memory_overhead:0L
    ~memory_static_min:memory.static_min ~memory_dynamic_min:memory.dynamic_min ~memory_target:memory.target
    ~memory_static_max:memory.static_max ~memory_dynamic_max:memory.dynamic_max
    ~resident_on:localhost ~scheduled_to_be_resident_on:Ref.null ~affinity:localhost ~suspend_VDI:Ref.null
    ~domid:0L ~domarch ~is_control_domain:true
    ~is_a_template:false ~is_default_template:false
    ~is_a_snapshot:false ~snapshot_time:Date.never ~snapshot_of:Ref.null ~transportable_snapshot_id:""
    ~snapshot_info:[] ~snapshot_metadata:""
    ~parent:Ref.null
    ~other_config:[] ~blobs:[] ~xenstore_data:[] ~tags:[] ~user_version:1L
    ~ha_restart_priority:"" ~ha_always_run:false ~recommendations:""
    ~last_boot_CPU_flags:[] ~last_booted_record:""
    ~guest_metrics:Ref.null ~metrics
    ~bios_strings:[] ~protection_policy:Ref.null
    ~is_snapshot_from_vmpp:false
    ~snapshot_schedule:Ref.null ~is_vmss_snapshot:false
    ~appliance:Ref.null
    ~start_delay:0L
    ~shutdown_delay:0L
    ~order:0L
    ~suspend_SR:Ref.null
    ~version:0L
    ~generation_id:""
    ~hardware_platform_version:0L
    ~has_vendor_device:false
    ~requires_reboot:false ~reference_label:""
    ~domain_type:Xapi_globs.domain_zero_domain_type
    ~nVRAM:[]
  ;
  ensure_domain_zero_metrics_record ~__context ~domain_zero_ref host_info;
  Db.Host.set_control_domain ~__context ~self:localhost ~value:domain_zero_ref;
  Xapi_vm_helpers.update_memory_overhead ~__context ~vm:domain_zero_ref

and create_domain_zero_console_record_with_protocol ~__context ~domain_zero_ref ~dom0_console_protocol =
  let console_ref = Ref.make () in
  let address = Db.Host.get_address ~__context ~self: (Helpers.get_localhost ~__context) in
  let location = Printf.sprintf "https://%s%s?ref=%s" address Constants.console_uri (Ref.string_of domain_zero_ref) in
  let port = match dom0_console_protocol with
    |`rfb -> Xapi_globs.host_console_vncport
    |`vt100 -> Xapi_globs.host_console_textport in
  Db.Console.create ~__context ~ref: console_ref
    ~uuid: (Uuid.to_string (Uuid.make_uuid ()))
    ~protocol: dom0_console_protocol
    ~location
    ~vM: domain_zero_ref
    ~other_config:[]
    ~port

and create_domain_zero_console_record ~__context ~domain_zero_ref ~console_records_rfb ~console_records_vt100 =
  if List.length console_records_rfb <> 1
  then begin
    List.iter (fun console -> Db.Console.destroy ~__context ~self: console ) console_records_rfb ;
    create_domain_zero_console_record_with_protocol ~__context ~domain_zero_ref ~dom0_console_protocol: `rfb ;
  end;
  if List.length console_records_vt100 <> 1
  then begin
    List.iter (fun console -> Db.Console.destroy ~__context ~self: console ) console_records_vt100 ;
    create_domain_zero_console_record_with_protocol ~__context ~domain_zero_ref ~dom0_console_protocol: `vt100 ;
  end

and create_domain_zero_metrics_record ~__context ~domain_zero_metrics_ref ~memory_constraints ~vcpus : unit =
  let rec mkints = function
    | 0 -> []
    | n -> (mkints (n - 1) @ [n]) in
  Db.VM_metrics.create
    ~__context
    ~ref:domain_zero_metrics_ref
    ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
    ~memory_actual: memory_constraints.target
    ~vCPUs_utilisation:(List.map (fun x -> Int64.of_int x, 0.) (mkints vcpus))
    ~vCPUs_number:(Int64.of_int vcpus)
    ~vCPUs_CPU:[]
    ~vCPUs_params:[]
    ~vCPUs_flags:[]
    ~state:[]
    ~start_time:Date.never
    ~install_time:Date.never
    ~last_updated:Date.never
    ~other_config:[]
    ~hvm:false
    ~nomigrate:false
    ~nested_virt:false
    ~current_domain_type:Xapi_globs.domain_zero_domain_type
  ;

and update_domain_zero_record ~__context ~domain_zero_ref (host_info: host_info) : unit =
  (* Write the updated memory constraints to the database, if the VM is not
     	   marked as requiring reboot. *)
  let constraints_in_db = Vm_memory_constraints.get ~__context ~vm_ref:domain_zero_ref in
  let constraints = create_domain_zero_memory_constraints host_info in
  if not (Xapi_host_helpers.Host_requires_reboot.get ()) then begin
    let constraints =
      (* Only update static_min if it is unset (i.e. 0) *)
      if constraints_in_db.static_min > 0L then
        {constraints with static_min = constraints_in_db.static_min}
      else
        constraints
    in
    Vm_memory_constraints.set ~__context ~vm_ref:domain_zero_ref ~constraints;
    Db.VM.set_requires_reboot ~__context ~self:domain_zero_ref ~value:false
  end;
  let localhost = Helpers.get_localhost ~__context in
  Db.VM.set_power_state ~__context ~self:domain_zero_ref ~value:`Running;
  Db.VM.set_domid ~__context ~self:domain_zero_ref ~value:0L;
  Helpers.update_domain_zero_name ~__context localhost host_info.hostname

and create_domain_zero_memory_constraints (host_info: host_info) : Vm_memory_constraints.t =
  try
    match Memory_client.Client.get_domain_zero_policy "create_misc" with
    | Memory_interface.Fixed_size x ->
      {
        static_min = x; static_max = x;
        dynamic_min = x; dynamic_max = x;
        target = x;
      }
    | Memory_interface.Auto_balloon(low, high) ->
      {
        static_min = low; static_max = high;
        dynamic_min = low; dynamic_max = high;
        target = high;
      }
  with e ->
    if Pool_role.is_unit_test ()
    then
      {
        static_min = 0L; static_max = 0L;
        dynamic_min = 0L; dynamic_max = 0L;
        target = 0L;
      }
    else raise e

open Db_filter

(** Create a record for the "root" user if it doesn't exist already *)
let create_root_user ~__context =
  let fullname = "superuser"
  and short_name = "root"
  and uuid = Uuid.to_string (Uuid.make_uuid ())
  and ref = Ref.make () in

  let all = Db.User.get_records_where ~__context ~expr:(Eq(Field "short_name", Literal short_name)) in
  if all = [] then Db.User.create ~__context ~ref ~fullname ~short_name ~uuid ~other_config:[]

let get_xapi_verstring () =
  Printf.sprintf "%d.%d" Xapi_globs.version_major Xapi_globs.version_minor

(** Create assoc list of Supplemental-Pack information.
 *  The package information is taking from the [XS-REPOSITORY] XML file in the package
 *  directory.
 *  The keys have the form "<originator>:<name>", the value is
 *  "<description>, version <version>", appended by ", build <build>" if the <build>
 *  number is present in the XML file, and appended by ", homogeneous" if the [enforce-homogeneity]
 *  attribute is present and set to "true".
 *  For backwards compatibility, the old [package-linux] key is also added
 *  when the linux pack (now [xs:linux]) is present (alongside the new key).
 *  The [package-linux] key is now deprecated and will be removed in the next version. *)
let make_packs_info () =
  try
    let packs = Sys.readdir !Xapi_globs.packs_dir in
    let get_pack_details fname =
      try
        let xml = Xml.parse_file (!Xapi_globs.packs_dir ^ "/" ^ fname ^ "/XS-REPOSITORY") in
        match xml with
        | Xml.Element (name, attr, children) ->
          let originator = List.assoc "originator" attr in
          let name = List.assoc "name" attr in
          let version = List.assoc "version" attr in
          let build =
            if List.mem_assoc "build" attr then Some (List.assoc "build" attr)
            else None
          in
          let homogeneous =
            if List.mem_assoc "enforce-homogeneity" attr &&
               (List.assoc "enforce-homogeneity" attr) = "true" then true
            else false
          in
          let description = match children with
            | Xml.Element(_, _, (Xml.PCData s) :: _) :: _ -> s
            | _ -> failwith "error with parsing pack data"
          in
          let param_name = originator ^ ":" ^ name in
          let value = description ^ ", version " ^ version ^
                      (match build with
                       | Some build -> ", build " ^ build
                       | None -> "") ^
                      (if homogeneous then ", homogeneous"
                       else "")
          in
          let kv = [(param_name, value)] in
          if originator = "xs" && name = "linux" then
            (* CA-29040: put old linux-pack key in there for backwards compatibility *)
            ["package-linux", "installed"] @ kv
          else
            kv
        | _ -> failwith "error while parsing pack data!"
      with _ -> debug "error while parsing pack data for %s!" fname; []
    in
    Array.fold_left (fun l fname -> get_pack_details fname @ l) [] packs
  with _ -> []

(** Create a complete assoc list of version information *)
let make_software_version ~__context host_info =
  let dbg = Context.string_of_task __context in
  let option_to_list k o = match o with None -> [] | Some x -> [ k, x ] in
  let v6_version =
    (* Best-effort attempt to read the date-based version from v6d *)
    try
      match V6_client.get_version "make_software_version" with
      | "" -> []
      | dbv -> ["dbv", dbv]
    with Api_errors.Server_error (code, []) when code = Api_errors.v6d_failure ->
      []
  in
  Xapi_globs.software_version () @
  v6_version @
  [
    "xapi", get_xapi_verstring ();
    "xen", host_info.xen_verstring;
    "linux", host_info.linux_verstring;
    "xencenter_min", Xapi_globs.xencenter_min_verstring;
    "xencenter_max", Xapi_globs.xencenter_max_verstring;
    "network_backend", Network_interface.string_of_kind (Net.Bridge.get_kind dbg ());
    Xapi_globs._db_schema, Printf.sprintf "%d.%d" Datamodel_common.schema_major_vsn Datamodel_common.schema_minor_vsn;
  ] @
  (option_to_list "oem_manufacturer" host_info.oem_manufacturer) @
  (option_to_list "oem_model" host_info.oem_model) @
  (option_to_list "oem_build_number" host_info.oem_build_number) @
  (option_to_list "machine_serial_number" host_info.machine_serial_number) @
  (option_to_list "machine_serial_name" host_info.machine_serial_name) @
  (option_to_list "xen_livepatches" (make_xen_livepatch_list ())) @
  (option_to_list "kernel_livepatches" (make_kpatch_list ())) @
  make_packs_info ()

let create_software_version ~__context host_info =
  let software_version = make_software_version ~__context host_info in
  let host = Helpers.get_localhost ~__context in
  Db.Host.set_software_version ~__context ~self:host ~value:software_version

let create_host_cpu ~__context host_info =
  let open Map_check in
  let open Cpuid_helpers in

  let cpu_info = host_info.cpu_info in
  let cpu = [
    "cpu_count", string_of_int cpu_info.cpu_count;
    "socket_count", string_of_int cpu_info.socket_count;
    "vendor", cpu_info.vendor;
    "speed", cpu_info.speed;
    "modelname", cpu_info.modelname;
    "family", cpu_info.family;
    "model", cpu_info.model;
    "stepping", cpu_info.stepping;
    "flags", cpu_info.flags;
    (* To support VMs migrated from hosts which do not support CPU levelling v2,
       		   set the "features" key to what it would be on such hosts. *)
    "features", Cpuid_helpers.string_of_features cpu_info.features_oldstyle;
    "features_pv", Cpuid_helpers.string_of_features cpu_info.features_pv;
    "features_hvm", Cpuid_helpers.string_of_features cpu_info.features_hvm;
  ] in
  let host = Helpers.get_localhost ~__context in
  let old_cpu_info = Db.Host.get_cpu_info ~__context ~self:host in
  debug "create_host_cpuinfo: setting host cpuinfo: socket_count=%d, cpu_count=%d, features_hvm=%s, features_pv=%s"
    (Map_check.getf Cpuid_helpers.socket_count cpu)
    (Map_check.getf Cpuid_helpers.cpu_count cpu)
    (Map_check.getf Cpuid_helpers.features_hvm cpu |> string_of_features)
    (Map_check.getf Cpuid_helpers.features_pv cpu |> string_of_features);
  Db.Host.set_cpu_info ~__context ~self:host ~value:cpu;

  let before = getf ~default:[||] features_hvm old_cpu_info in
  let after = cpu_info.features_hvm in
  if not (is_equal before after) && before <> [||] then begin
    let lost = is_strict_subset (intersect before after) before in
    let gained = is_strict_subset (intersect before after) after in
    info "The CPU features of this host have changed.%s%s Old features_hvm=%s."
        (if lost then " Some features have gone away." else "")
        (if gained then " Some features were added." else "")
        (string_of_features before);

    if not (Helpers.rolling_upgrade_in_progress ~__context) && lost then
      let (name, priority) = Api_messages.host_cpu_features_down in
      let obj_uuid = Db.Host.get_uuid ~__context ~self:host in
      let body = Printf.sprintf "The CPU features of this host have changed. Some features have gone away." in
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          ignore (XenAPI.Message.create rpc session_id name priority `Host obj_uuid body)
        )
  end;

  (* Recreate all Host_cpu objects *)

  (* Not all /proc/cpuinfo files contain MHz information. *)
  let speed = try Int64.of_float (float_of_string cpu_info.speed) with _ -> 0L in
  let model = try Int64.of_string cpu_info.model with _ -> 0L in
  let family = try Int64.of_string cpu_info.family with _ -> 0L in

  (* Recreate all Host_cpu objects *)
  let host_cpus = List.filter (fun (_, s) -> s.API.host_cpu_host = host) (Db.Host_cpu.get_all_records ~__context) in
  List.iter (fun (r, _) -> Db.Host_cpu.destroy ~__context ~self:r) host_cpus;
  for i = 0 to cpu_info.cpu_count - 1
  do
    let uuid = Uuid.to_string (Uuid.make_uuid ())
    and ref = Ref.make () in
    debug "Creating CPU %d: %s" i uuid;
    ignore (Db.Host_cpu.create ~__context ~ref ~uuid ~host ~number:(Int64.of_int i)
              ~vendor:cpu_info.vendor ~speed ~modelname:cpu_info.modelname
              ~utilisation:0. ~flags:cpu_info.flags ~stepping:cpu_info.stepping ~model ~family
              ~features:"" ~other_config:[])
  done


let create_pool_cpuinfo ~__context =
  let open Map_check in
  let open Cpuid_helpers in

  let all_host_cpus = List.map
      (fun (r, s) -> r, s.API.host_cpu_info)
      (Db.Host.get_all_records ~__context) in

  let merge pool (hostref, host) =
    try
      pool
      |> setf vendor (getf vendor host)
      |> setf cpu_count ((getf cpu_count host) + (getf cpu_count pool))
      |> setf socket_count ((getf socket_count host) + (getf socket_count pool))
      |> setf features_pv (Cpuid_helpers.intersect (getf features_pv host) (getf features_pv pool))
      |> fun pool' ->
           if Helpers.host_supports_hvm ~__context hostref then
             setf features_hvm (Cpuid_helpers.intersect (getf features_hvm host) (getf features_hvm pool)) pool'
           else
             pool'
    with Not_found ->
      (* If the host doesn't have all the keys we expect, assume that we
         			   are in the middle of an RPU and it has not yet been upgraded, so
         			   it should be ignored when calculating the pool level *)
      pool
  in

  let zero = ["vendor", ""; "socket_count", "0"; "cpu_count", "0"; "features_pv", ""; "features_hvm", ""] in
  let pool_cpuinfo = List.fold_left merge zero all_host_cpus in
  let pool = Helpers.get_pool ~__context in
  let old_cpuinfo = Db.Pool.get_cpu_info ~__context ~self:pool in
  debug "create_pool_cpuinfo: setting pool cpuinfo: socket_count=%d, cpu_count=%d, features_hvm=%s, features_pv=%s"
    (Map_check.getf Cpuid_helpers.socket_count pool_cpuinfo)
    (Map_check.getf Cpuid_helpers.cpu_count pool_cpuinfo)
    (Map_check.getf Cpuid_helpers.features_hvm pool_cpuinfo |> string_of_features)
    (Map_check.getf Cpuid_helpers.features_pv pool_cpuinfo |> string_of_features);
  Db.Pool.set_cpu_info ~__context ~self:pool ~value:pool_cpuinfo;

  let before = getf ~default:[||] features_hvm old_cpuinfo in
  let after = getf ~default:[||] features_hvm pool_cpuinfo in
  if not (is_equal before after) && before <> [||] then begin
    let lost = is_strict_subset (intersect before after) before in
    let gained = is_strict_subset (intersect before after) after in
    info "The pool-level CPU features have changed.%s%s Old features_hvm=%s."
        (if lost then " Some features have gone away." else "")
        (if gained then " Some features were added." else "")
        (string_of_features before);

    if not (Helpers.rolling_upgrade_in_progress ~__context) && List.length all_host_cpus > 1 && lost then
      let (name, priority) = Api_messages.pool_cpu_features_down in
      let obj_uuid = Db.Pool.get_uuid ~__context ~self:pool in
      let body = Printf.sprintf "The pool-level CPU features have changed. Some features have gone away." in
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          ignore (XenAPI.Message.create rpc session_id name priority `Pool obj_uuid body)
        )
  end


let create_chipset_info ~__context host_info =
  let host = Helpers.get_localhost ~__context in
  let iommu =
    if host_info.chipset_info.iommu then
      "true"
    else
      "false" in
  let info = ["iommu", iommu] in
  Db.Host.set_chipset_info ~__context ~self:host ~value:info

let create_updates_requiring_reboot_info ~__context ~host =
  let update_uuids = try Stdext.Listext.List.setify (Stdext.Unixext.read_lines !Xapi_globs.reboot_required_hfxs) with _ -> [] in
  let updates = List.fold_left (fun acc uuid ->
      try
        (Db.Pool_update.get_by_uuid ~__context ~uuid) :: acc
      with _ -> warn "Invalid Pool_update UUID [%s]" uuid; acc
    ) [] update_uuids in
  Db.Host.set_updates_requiring_reboot ~__context ~self:host ~value:updates
