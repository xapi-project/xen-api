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
(** Code for dealing with data read from the guest agent (via xenstore).
    Note this only deals with relatively static data (like version numbers) and
    not dynamic performance data. *)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module Date = Clock.Date

module D = Debug.Make (struct let name = "xapi_guest_metrics" end)

open D

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

(* A fixed set of standard keys are placed in the PV_drivers_version map.
   NB each key is annotated with whether it appears in windows and/or linux *)
let pv_drivers_version =
  [
    ("attr/PVAddons/MajorVersion", "major")
  ; (* linux + windows *)
    ("attr/PVAddons/MinorVersion", "minor")
  ; (* linux + windows *)
    ("attr/PVAddons/MicroVersion", "micro")
  ; (* linux + windows -- added in Miami Beta2 *)
    ("attr/PVAddons/BuildVersion", "build")
  ; (* windows *)
    ("attr/PVAddons/Application", "application") (* linux *)
  ]

(* A fixed set of standard keys placed in the os_version map. *)
let os_version =
  [
    ("data/os_name", "name")
  ; (* linux + windows *)
    ("data/os_uname", "uname")
  ; (* linux *)
    ("data/os_distro", "distro")
  ; (* linux + windows *)
    ("data/os_majorver", "major")
  ; (* linux + windows *)
    ("data/os_minorver", "minor")
  ; (* linux + windows *)
    ("attr/os/spmajor", "spmajor")
  ; (* windows *)
    ("attr/os/spminor", "spminor") (* windows *)
  ]

let netbios_name = [("data/host_name_dns", "host_name")]

let dns_domain = [("data/domain", "dns_domain")]

let memory = [("data/meminfo_free", "free"); ("data/meminfo_total", "total")]

let device_id = [("data/device_id", "device_id")]

let ( // ) = Filename.concat

(* This function is passed the 'attr' node and a function it can use to
 * find the directory listing of sub-nodes. It will return a map where the
 * keys are the xenstore paths of the VM's IP addresses, and the values are
 * the intended keys which will go into the VM_guest_metrics.networks field.
 * Example output for a single vif might be:
 * attr/eth0/ip -> 0/ip
 * attr/eth0/ipv6/0/addr -> 0/ipv6/0
 * attr/eth0/ipv6/1/addr -> 0/ipv6/1
 *
 * Example output on new xenstore protocol:
 * attr/vif/0/ipv4/0 -> 0/ipv4/0
 * attr/vif/0/ipv4/1 -> 0/ipv4/1
 * attr/vif/0/ipv6/0 -> 0/ipv6/0
 * attr/vif/0/ipv6/1 -> 0/ipv6/1
 *
 * For the compatibility of XAPI clients, outputs of both protocols
 * will be generated. I.E.
 * attr/eth0/ip -> 0/ip; 0/ipv4/0
 * attr/vif/0/ipv4/0 -> 0/ip; 0/ipv4/0
 *
 * Add support for SR-IOV VF, so there are two kinds of vif_type, either to be
 * `vif` or `net-sriov-vf`
 *)
let networks path vif_type (list : string -> string list) =
  (* Find all ipv6 addresses under a path. *)
  let find_ipv6 path prefix =
    List.map (fun str -> (path // str // "addr", prefix // str)) (list path)
  in
  (* Find the ipv4 address under a path, and the ipv6 addresses if they exist. *)
  let find_all_ips path prefix =
    let ipv4 = (path // "ip", prefix // "ip") in
    let ipv4_with_idx = (path // "ip", prefix // "ipv4" // "0") in
    if List.mem "ipv6" (list path) then
      ipv4 :: ipv4_with_idx :: find_ipv6 (path // "ipv6") (prefix // "ipv6")
    else
      [ipv4; ipv4_with_idx]
  in
  (* Find all "ethn", "xenbrn" or newer interface standard names
   * [see https://www.freedesktop.org/wiki/Software/systemd/PredictableNetworkInterfaceNames/]
   * under a path. *)
  let find_eths path =
    let extract_network_keys eth =
      let iface_prefixes = ["eth"; "xenbr"; "eno"; "ens"; "emp"; "enx"] in
      let string_after_prefix ~prefix str =
        let prefix_len = String.length prefix in
        let size = String.length str - prefix_len in
        let after_prefix = String.sub str prefix_len size in
        after_prefix
      in
      let rec extract prefixes eth =
        match prefixes with
        | [] ->
            None
        | prefix :: rest ->
            if Astring.String.is_prefix ~affix:prefix eth then
              let n = string_after_prefix ~prefix eth in
              Some (path // eth, n)
            else
              extract rest eth
      in
      extract iface_prefixes eth
    in
    List.fold_left
      (fun acc eth ->
        match extract_network_keys eth with
        | None ->
            acc
        | Some pair ->
            pair :: acc
      )
      [] (list path)
  in
  let find_vifs vif_path =
    let extract_vif acc vif_id = (vif_path // vif_id, vif_id) :: acc in
    List.fold_left extract_vif [] (list vif_path)
  in
  let cmp a b =
    try compare (int_of_string a) (int_of_string b)
    with Failure _ ->
      error
        "String (\"%s\" or \"%s\") can't be converted into an integer as index \
         of IP"
        a b ;
      raise (Failure "Failed to compare")
  in
  let find_all_vif_ips vif_path vif_id =
    (*  vif_path: attr/vif/0 *)
    (*  vif_id: 0 *)
    let extract_ip_ver vif_id acc ip_ver =
      let ip_addr_ids = list (vif_path // ip_ver) in
      let extract_ip_addr vif_id ip_ver acc ip_addr_id =
        let key_left = Printf.sprintf "%s/%s/%s" vif_path ip_ver ip_addr_id in
        let key_right = Printf.sprintf "%s/%s/%s" vif_id ip_ver ip_addr_id in
        match acc with
        | [] when ip_ver = "ipv4" ->
            [(key_left, vif_id // "ip"); (key_left, key_right)]
        | _ ->
            (key_left, key_right) :: acc
      in
      try
        List.fold_left
          (extract_ip_addr vif_id ip_ver)
          []
          (List.stable_sort cmp ip_addr_ids)
        @ acc
      with Failure _ ->
        error "Failed to extract IP address for vif %s." vif_id ;
        []
    in
    let ip_vers =
      List.filter (fun a -> a = "ipv4" || a = "ipv6") (list vif_path)
    in
    List.fold_left (extract_ip_ver vif_id) [] ip_vers
  in
  match find_vifs (path // vif_type) with
  | [] ->
      path
      |> find_eths
      |> List.concat_map (fun (path, prefix) -> find_all_ips path prefix)
  | vif_pair_list ->
      vif_pair_list
      |> List.concat_map (fun (vif_path, vif_id) ->
             find_all_vif_ips vif_path vif_id
         )

(* One key is placed in the other map per control/* key in xenstore. This
   catches keys like "feature-shutdown" "feature-hibernate" "feature-reboot"
   "feature-sysrq" *)
let other all_control = List.map (fun x -> ("control/" ^ x, x)) all_control

(* There are two memory keys: data/meminfo_free and data/meminfo_total. These are *)
(* inserted into the memory map with the keys 'free' and 'total' *)

(** Cache the results so that we only actually update the master's database when
    the results of these lookups differ *)

type m = (string * string) list

type guest_metrics_t = {
    pv_drivers_version: m
  ; os_version: m
  ; netbios_name: m
  ; networks: m
  ; other: m
  ; memory: m
  ; device_id: m
  ; services: m
  ; last_updated: float
  ; can_use_hotplug_vbd: API.tristate_type
  ; can_use_hotplug_vif: API.tristate_type
}

let cache : (int, guest_metrics_t) Hashtbl.t = Hashtbl.create 20

let dead_domains : IntSet.t ref = ref IntSet.empty

let mutex = Mutex.create ()

(* Parse data/service which has the following structure:
   data/service/<service_name>/<key> = <value>
   data/service/<service_name>/<key> = <value>
   ...
   data/service/<service_name>/<key> = <value>
   Read and convert to [(<service_name>/<key>, <value>)] pair list.
   The list is intended to store in VM_guest_metrics.services at last *)
let get_guest_services (lookup : string -> string option)
    (list : string -> string list) =
  let base_path = "data/service" in
  let services = list base_path in
  services
  |> List.concat_map (fun service ->
         let sub_path = base_path // service in
         list sub_path
         |> List.map (fun key ->
                let full_path_key = sub_path // key in
                let db_key = service // key in
                let value = lookup full_path_key in
                (db_key, Option.value ~default:"" value)
            )
     )

(* In the following functions, 'lookup' reads a key from xenstore and 'list' reads
   a directory from xenstore. Both are relative to the guest's domainpath. *)
let get_initial_guest_metrics (lookup : string -> string option)
    (list : string -> string list) =
  let all_control = list "control" in
  let cant_suspend_reason = lookup "data/cant_suspend_reason" in
  let to_map kvpairs =
    List.concat_map
      (fun (xskey, mapkey) ->
        match (lookup xskey, xskey, cant_suspend_reason) with
        | Some _, "control/feature-suspend", Some reason ->
            [("data-cant-suspend-reason", reason)]
        | Some xsval, _, _ ->
            [(mapkey, xsval)]
        | None, _, _ ->
            []
      )
      kvpairs
  in
  let get_tristate xskey =
    match lookup xskey with
    | Some "0" ->
        `no
    | Some "1" ->
        `yes
    | _ ->
        `unspecified
  in
  let ts =
    match lookup "data/ts" with
    | Some value ->
        [("data-ts", value)]
    | None ->
        []
  in
  (* enumerate all driver versions from xenstore, which are stored like
     drivers/0 = "XenServer XENBUS 9.1.9.105 "
     drivers/1 = "XenServer XENVBD 9.1.8.79 "
     drivers/2 = "XenServer XENVIF 9.1.12.101 "
     drivers/3 = "XenServer XENIFACE 9.1.10.87 "
     drivers/4 = "XenServer XENNET 9.1.7.65 "

     (see the format specified in xenstore-paths)
  *)
  let get_windows_driver_versions () =
    (* Only look into directories that are numbers (indices) *)
    let filter_dirs subdirs =
      List.filter_map
        (fun x ->
          match int_of_string_opt x with
          | Some _ ->
              Some ("drivers/" ^ x, x)
          | None ->
              None
        )
        subdirs
    in
    let versions = list "drivers" |> filter_dirs |> to_map in
    List.filter_map
      (fun (_, version_string) ->
        try
          Scanf.sscanf version_string "%s@ %s@ %s@ %s@\n"
            (fun vendor driver_name version attr ->
              Some
                ( String.lowercase_ascii driver_name
                , String.concat " " [vendor; version; attr]
                )
          )
        with _ -> None
      )
      versions
  in
  let pv_drivers_version =
    to_map pv_drivers_version @ get_windows_driver_versions ()
  and os_version = to_map os_version
  and netbios_name =
    match to_map dns_domain with
    | [] ->
        to_map netbios_name
    | (_, dns_domain) :: _ ->
        List.map
          (fun (k, v) -> (k, Printf.sprintf "%s.%s" v dns_domain))
          (to_map netbios_name)
  and device_id = to_map device_id
  and networks =
    to_map
      (List.concat
         [
           networks "attr" "vif" list
         ; networks "xenserver/attr" "net-sriov-vf" list
         ]
      )
  and services =
    let services = get_guest_services lookup list in
    let keys = !Xapi_globs.guest_service_keys in
    let res =
      List.fold_left
        (fun acc key ->
          match lookup ("data/" ^ key) with
          | Some value ->
              (key, value) :: acc
          | None ->
              acc
        )
        [] keys
    in
    List.rev_append services res
  and other = List.append (to_map (other all_control)) ts
  and memory = to_map memory
  and last_updated = Unix.gettimeofday () in
  let can_use_hotplug_vbd = get_tristate "feature/hotplug/vbd" in
  let can_use_hotplug_vif = get_tristate "feature/hotplug/vif" in
  (* to avoid breakage whilst 'micro' is added to linux and windows agents, default this field
     to -1 if it's not present in xenstore *)
  let pv_drivers_version =
    if pv_drivers_version = [] || List.mem_assoc "micro" pv_drivers_version then
      pv_drivers_version (* already there; do nothing *)
    else
      ("micro", "-1") :: pv_drivers_version
  in
  {
    pv_drivers_version
  ; os_version
  ; netbios_name
  ; networks
  ; other
  ; memory
  ; device_id
  ; services
  ; last_updated
  ; can_use_hotplug_vbd
  ; can_use_hotplug_vif
  }

let create_and_set_guest_metrics (lookup : string -> string option)
    (list : string -> string list) ~__context ~domid ~uuid ~pV_drivers_detected
    =
  let initial_gm = get_initial_guest_metrics lookup list in
  let self = Db.VM.get_by_uuid ~__context ~uuid in
  let new_gm_uuid = Uuidx.to_string (Uuidx.make ())
  and new_gm_ref = Ref.make () in
  Db.VM_guest_metrics.create ~__context ~ref:new_gm_ref ~uuid:new_gm_uuid
    ~os_version:initial_gm.os_version ~netbios_name:initial_gm.netbios_name
    ~pV_drivers_version:initial_gm.pv_drivers_version
    ~pV_drivers_up_to_date:pV_drivers_detected ~memory:[] ~disks:[]
    ~networks:initial_gm.networks ~services:initial_gm.services
    ~pV_drivers_detected ~other:initial_gm.other
    ~last_updated:(Date.of_unix_time initial_gm.last_updated)
    ~other_config:[] ~live:true
    ~can_use_hotplug_vbd:initial_gm.can_use_hotplug_vbd
    ~can_use_hotplug_vif:initial_gm.can_use_hotplug_vif ;
  Db.VM.set_guest_metrics ~__context ~self ~value:new_gm_ref ;
  (* Update the cache with the new values *)
  with_lock mutex (fun () -> Hashtbl.replace cache domid initial_gm) ;
  (* We've just set the thing to live, let's make sure it's not in the dead list *)
  with_lock mutex (fun () -> dead_domains := IntSet.remove domid !dead_domains) ;
  let sl xs = String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) xs) in
  info
    "Received initial update from guest agent in VM %s; os_version = [ %s ]; \
     pv_drivers_version = [ %s ]; networks = [ %s ]"
    (Ref.string_of self) (sl initial_gm.os_version)
    (sl initial_gm.pv_drivers_version)
    (sl initial_gm.networks) ;
  new_gm_ref

(** Reset all the guest metrics for a particular VM *)
let all (lookup : string -> string option) (list : string -> string list)
    ~__context ~domid ~uuid ~pV_drivers_detected =
  let {
    pv_drivers_version
  ; os_version
  ; netbios_name
  ; networks
  ; other
  ; memory
  ; device_id
  ; services
  ; last_updated
  ; can_use_hotplug_vbd
  ; can_use_hotplug_vif
  } =
    get_initial_guest_metrics lookup list
  in
  (* let num = with_lock mutex (fun () -> Hashtbl.fold (fun _ _ c -> 1 + c) cache 0) in
     debug "Number of entries in hashtbl: %d" num; *)
  let self = Db.VM.get_by_uuid ~__context ~uuid in
  let guest_metrics_cached =
    with_lock mutex (fun () ->
        match Hashtbl.find_opt cache domid with
        | Some x ->
            x
        | None ->
            (* Make sure our cached idea of whether the domain is live or not is correct *)
            let vm_guest_metrics = Db.VM.get_guest_metrics ~__context ~self in
            let live =
              true
              && Db.is_valid_ref __context vm_guest_metrics
              && Db.VM_guest_metrics.get_live ~__context ~self:vm_guest_metrics
            in
            if live then
              dead_domains := IntSet.remove domid !dead_domains
            else
              dead_domains := IntSet.add domid !dead_domains ;
            {
              pv_drivers_version= []
            ; os_version= []
            ; netbios_name= []
            ; networks= []
            ; other= []
            ; memory= []
            ; device_id= []
            ; services= []
            ; last_updated= 0.0
            ; can_use_hotplug_vbd= `unspecified
            ; can_use_hotplug_vif= `unspecified
            }
    )
  in
  (* Only if the data is valid, cache it (CA-20353) *)
  with_lock mutex (fun () ->
      Hashtbl.replace cache domid
        {
          pv_drivers_version
        ; os_version
        ; netbios_name
        ; networks
        ; other
        ; memory
        ; device_id
        ; services
        ; last_updated
        ; can_use_hotplug_vbd
        ; can_use_hotplug_vif
        }
  ) ;
  (* We update only if any actual data has changed *)
  if
    (guest_metrics_cached.pv_drivers_version <> pv_drivers_version
    || guest_metrics_cached.os_version <> os_version
    || guest_metrics_cached.netbios_name <> netbios_name
    || guest_metrics_cached.networks <> networks
    || guest_metrics_cached.other <> other
    || guest_metrics_cached.device_id <> device_id
    || guest_metrics_cached.services <> services
    )
    || guest_metrics_cached.can_use_hotplug_vbd <> can_use_hotplug_vbd
    || guest_metrics_cached.can_use_hotplug_vif <> can_use_hotplug_vif
    (* Nb. we're ignoring the memory updates as far as the VM_guest_metrics API object is concerned. We are putting them into an RRD instead *)
    (* ||
           guest_metrics_cached.memory <> memory)*)
  then (
    let gm =
      let existing = Db.VM.get_guest_metrics ~__context ~self in
      if
        try
          ignore (Db.VM_guest_metrics.get_uuid ~__context ~self:existing) ;
          true
        with _ -> false
      then
        existing
      else (* if it doesn't exist, make a fresh one *)
        create_and_set_guest_metrics lookup list ~__context ~domid ~uuid
          ~pV_drivers_detected
    in
    (* We unconditionally reset the database values but observe that the database
       	     checks whether a value has actually changed before doing anything *)
    if guest_metrics_cached.pv_drivers_version <> pv_drivers_version then
      Db.VM_guest_metrics.set_PV_drivers_version ~__context ~self:gm
        ~value:pv_drivers_version ;
    if guest_metrics_cached.os_version <> os_version then
      Db.VM_guest_metrics.set_os_version ~__context ~self:gm ~value:os_version ;
    if guest_metrics_cached.netbios_name <> netbios_name then
      Db.VM_guest_metrics.set_netbios_name ~__context ~self:gm
        ~value:netbios_name ;
    if guest_metrics_cached.networks <> networks then
      Db.VM_guest_metrics.set_networks ~__context ~self:gm ~value:networks ;
    if guest_metrics_cached.services <> services then
      Db.VM_guest_metrics.set_services ~__context ~self:gm ~value:services ;
    if guest_metrics_cached.other <> other then (
      Db.VM_guest_metrics.set_other ~__context ~self:gm ~value:other ;
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.VM.update_allowed_operations ~rpc ~session_id ~self
      )
    ) ;
    if guest_metrics_cached.can_use_hotplug_vbd <> can_use_hotplug_vbd then
      Db.VM_guest_metrics.set_can_use_hotplug_vbd ~__context ~self:gm
        ~value:can_use_hotplug_vbd ;
    if guest_metrics_cached.can_use_hotplug_vif <> can_use_hotplug_vif then
      Db.VM_guest_metrics.set_can_use_hotplug_vif ~__context ~self:gm
        ~value:can_use_hotplug_vif ;
    (* if(guest_metrics_cached.memory <> memory) then
         Db.VM_guest_metrics.set_memory ~__context ~self:gm ~value:memory; *)
    Db.VM_guest_metrics.set_last_updated ~__context ~self:gm
      ~value:(Date.of_unix_time last_updated) ;
    if guest_metrics_cached.device_id <> device_id then
      if List.mem_assoc Xapi_globs.device_id_key_name device_id then (
        let value = List.assoc Xapi_globs.device_id_key_name device_id in
        let platform = Db.VM.get_platform ~__context ~self in
        info "Updating VM %s platform:%s <- %s" (Ref.string_of self)
          Xapi_globs.device_id_key_name value ;
        ( if List.mem_assoc Xapi_globs.device_id_key_name platform then
            try
              Db.VM.remove_from_platform ~__context ~self
                ~key:Xapi_globs.device_id_key_name
            with _ -> ()
        ) ;
        try
          Db.VM.add_to_platform ~__context ~self
            ~key:Xapi_globs.device_id_key_name ~value
        with _ -> ()
      ) ;
    (* Update the 'up to date' flag afterwards *)
    let gmr = Db.VM_guest_metrics.get_record_internal ~__context ~self:gm in
    (* CA-18034: If viridian flag isn't in there and we have Orlando-or-newer Windows PV drivers then shove it in the metadata for next boot... *)
    ( if Xapi_pv_driver_version.is_windows_and_orlando_or_newer gmr then
        let platform = Db.VM.get_platform ~__context ~self in
        if not (List.mem_assoc Xapi_globs.viridian_key_name platform) then (
          info "Setting VM %s platform:%s <- %s" (Ref.string_of self)
            Xapi_globs.viridian_key_name Xapi_globs.default_viridian_key_value ;
          try
            Db.VM.add_to_platform ~__context ~self
              ~key:Xapi_globs.viridian_key_name
              ~value:Xapi_globs.default_viridian_key_value
          with _ -> ()
        )
    ) ;
    (* We base some of our allowed-operations decisions on these advertised features and the presence/absence of PV drivers. *)
    if
      guest_metrics_cached.pv_drivers_version <> pv_drivers_version
      || guest_metrics_cached.can_use_hotplug_vbd <> can_use_hotplug_vbd
      || guest_metrics_cached.can_use_hotplug_vif <> can_use_hotplug_vif
    then
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.VM.update_allowed_operations ~rpc ~session_id ~self
      )
  )

(* else debug "Ignored spurious guest agent update" *)
