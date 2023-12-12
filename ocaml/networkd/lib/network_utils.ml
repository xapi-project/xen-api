(*
 * Copyright (C) Citrix Systems Inc.
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

open Xapi_stdext_pervasives
open Xapi_stdext_unix
open Network_interface

module D = Debug.Make (struct let name = "network_utils" end)

open D

type util_error =
  | Bus_out_of_range
  | Not_enough_mmio_resources
  | Fail_to_set_vf_rate
  | Fail_to_set_vf_vlan
  | Fail_to_set_vf_mac
  | Parent_device_of_vf_not_found
  | Vf_index_not_found
  | Fail_to_rebuild_initrd
  | Fail_to_write_modprobe_cfg
  | Fail_to_get_driver_name
  | Fail_to_get_maxvfs
  | No_sriov_capability
  | Vf_sysfs_path_not_found
  | Fail_to_unbind_from_driver
  | Other

let iproute2 = "/sbin/ip"

let resolv_conf = "/etc/resolv.conf"

let dhclient = "/sbin/dhclient"

let sysctl = "/sbin/sysctl"

let ovs_vsctl = "/usr/bin/ovs-vsctl"

let ovs_ofctl = "/usr/bin/ovs-ofctl"

let ovs_appctl = "/usr/bin/ovs-appctl"

let ovs_vlan_bug_workaround = "/usr/sbin/ovs-vlan-bug-workaround"

let brctl = ref "/sbin/brctl"

let modprobe = "/sbin/modprobe"

let ethtool = ref "/sbin/ethtool"

let bonding_dir = "/proc/net/bonding/"

let uname = ref "/usr/bin/uname"

let dracut = ref "/sbin/dracut"

let modinfo = ref "/sbin/modinfo"

let dracut_timeout = ref 180.0

let fcoedriver = ref "/opt/xensource/libexec/fcoe_driver"

let inject_igmp_query_script = ref "/usr/libexec/xenopsd/igmp_query_injector.py"

let mac_table_size = ref 10000

let igmp_query_maxresp_time = ref "5000"

let enable_ipv6_mcast_snooping = ref false

let mcast_snooping_disable_flood_unregistered = ref true

let default_error_handler script args stdout stderr status =
  let message =
    match status with
    | Unix.WEXITED n ->
        Printf.sprintf "Exit code %d" n
    | Unix.WSIGNALED s ->
        Printf.sprintf "Signaled %d" s
        (* Note that this is the internal ocaml signal number, see Sys module *)
    | Unix.WSTOPPED s ->
        Printf.sprintf "Stopped %d" s
  in
  error "Call '%s %s' exited badly: %s [stdout = '%s'; stderr = '%s']" script
    (String.concat " " args) message stdout stderr ;
  raise
    (Network_error
       (Script_error
          [
            ("script", script)
          ; ("args", String.concat " " args)
          ; ("code", message)
          ; ("stdout", stdout)
          ; ("stderr", stderr)
          ]
       )
    )

let check_n_run ?(on_error = default_error_handler) ?(log = true) run_func
    script args =
  try
    Unix.access script [Unix.X_OK] ;
    (* Use the same $PATH as xapi *)
    let env = [|"PATH=" ^ Sys.getenv "PATH"|] in
    if log then
      info "%s %s" script (String.concat " " args) ;
    run_func env script args
  with
  | Unix.Unix_error (e, a, b) ->
      error "Caught unix error: %s [%s, %s]" (Unix.error_message e) a b ;
      error "Assuming script %s doesn't exist" script ;
      raise (Network_error (Script_missing script))
  | Forkhelpers.Spawn_internal_error (stderr, stdout, status) ->
      on_error script args stdout stderr status

let call_script ?(timeout = Some 60.0) ?on_error ?log script args =
  let call_script_internal env script args =
    let out, _err =
      Forkhelpers.execute_command_get_output ~env ?timeout script args
    in
    out
  in
  check_n_run ?on_error ?log call_script_internal script args

let fork_script ?on_error ?log script args =
  let fork_script_internal env script args =
    let pid =
      Forkhelpers.safe_close_and_exec ~env None None None [] script args
    in
    Forkhelpers.dontwaitpid pid
  in
  check_n_run ?on_error ?log fork_script_internal script args

module Sysfs = struct
  let list_drivers () =
    try Array.to_list (Sys.readdir "/sys/bus/pci/drivers")
    with _ ->
      warn "Failed to obtain list of drivers from sysfs" ;
      []

  let getpath dev attr = Printf.sprintf "/sys/class/net/%s/%s" dev attr

  let read_one_line file =
    try
      Unixext.string_of_file file |> Astring.String.take ~sat:(( <> ) '\n')
    with
    | End_of_file ->
        ""
    | Unix.Unix_error (Unix.EINVAL, _, _) ->
        (* The device is not yet up *)
        raise (Network_error (Read_error file))
    | exn ->
        error "Error in read one line of file: %s, exception %s\n%s" file
          (Printexc.to_string exn)
          (Printexc.get_backtrace ()) ;
        raise (Network_error (Read_error file))

  let write_one_line file l =
    let outchan = open_out file in
    try
      output_string outchan (l ^ "\n") ;
      close_out outchan
    with _ ->
      close_out outchan ;
      raise (Network_error (Write_error file))

  let is_physical name =
    try
      let devpath = getpath name "device" in
      let driver_link = Unix.readlink (devpath ^ "/driver") in
      (* filter out symlinks under device/driver which look like
         /../../../devices/xen-backend/vif- *)
      not
        (List.mem "xen-backend"
           (Astring.String.cuts ~empty:false ~sep:"/" driver_link)
        )
    with _ -> false

  (* device types are defined in linux/if_arp.h *)
  let is_ether_device name =
    match int_of_string (read_one_line (getpath name "type")) with
    | 1 ->
        true
    | _ ->
        false
    | exception _ ->
        false

  let list () =
    let is_dir name =
      try Sys.is_directory ("/sys/class/net/" ^ name) with _ -> false
    in
    Sys.readdir "/sys/class/net"
    |> Array.to_list
    |> List.filter (fun name -> is_dir name && is_ether_device name)

  let exists dev = List.mem dev @@ list ()

  let assert_exists dev =
    if not @@ exists dev then
      raise (Network_error (Interface_does_not_exist dev))

  let get_carrier name =
    try
      let i = int_of_string (read_one_line (getpath name "carrier")) in
      match i with 1 -> true | 0 -> false | _ -> false
    with _ -> false

  let get_pcibuspath name =
    try
      let devpath = Unix.readlink (getpath name "device") in
      Filename.basename devpath
    with _ -> "N/A"

  let get_pci_ids name =
    let read_id_from path =
      try
        let l = read_one_line path in
        (* trim 0x *)
        String.sub l 2 (String.length l - 2)
      with _ -> ""
    in
    ( read_id_from (getpath name "device/vendor")
    , read_id_from (getpath name "device/device")
    )

  (** Returns the name of the driver for network device [dev] *)
  let get_driver_name dev =
    try
      let driver_path = Unix.readlink (getpath dev "device/driver") in
      match Astring.String.cut ~sep:"/" ~rev:true driver_path with
      | Some (_prefix, suffix) ->
          Some suffix
      | None ->
          debug "get %s driver name: %s does not contain slash" dev driver_path ;
          None
    with _ ->
      debug "%s: could not read netdev's driver name" dev ;
      None

  let get_driver_name_err dev =
    match get_driver_name dev with
    | Some a ->
        Result.Ok a
    | None ->
        Result.Error
          (Fail_to_get_driver_name, "Failed to get driver name for: " ^ dev)

  (** Returns the features bitmap for the driver for [dev]. The features bitmap
      is a set of NETIF_F_ flags supported by its driver. *)
  let get_features dev =
    try Some (int_of_string (read_one_line (getpath dev "features")))
    with _ -> None

  (** Returns [true] if [dev] supports VLAN acceleration, [false] otherwise. *)
  let has_vlan_accel dev =
    let flag_NETIF_F_HW_VLAN_TX = 128 in
    let flag_NETIF_F_HW_VLAN_RX = 256 in
    let flag_NETIF_F_VLAN =
      flag_NETIF_F_HW_VLAN_TX lor flag_NETIF_F_HW_VLAN_RX
    in
    match get_features dev with
    | None ->
        false
    | Some features ->
        features land flag_NETIF_F_VLAN <> 0

  let set_multicast_snooping bridge value =
    try
      let path = getpath bridge "bridge/multicast_snooping" in
      write_one_line path (if value then "1" else "0")
    with _ ->
      warn "Could not %s IGMP-snooping on bridge %s"
        (if value then "enable" else "disable")
        bridge

  let bridge_to_interfaces bridge =
    try Array.to_list (Sys.readdir (getpath bridge "brif")) with _ -> []

  let get_all_bridges () =
    let ifaces = list () in
    List.filter (fun name -> Sys.file_exists (getpath name "bridge")) ifaces

  (** Returns (speed, duplex) for a given network interface: int megabits/s,
      Duplex. The units of speed are specified in pif_record in
      xen-api/xapi/records.ml.

      Note: these data are present in sysfs from kernel 2.6.33. *)
  let get_status name =
    let speed =
      getpath name "speed" |> fun p ->
      try read_one_line p |> int_of_string with _ -> 0
    in
    let duplex =
      getpath name "duplex" |> fun p ->
      try read_one_line p |> duplex_of_string with _ -> Duplex_unknown
    in
    (speed, duplex)

  let get_dev_nums_with_same_driver driver =
    try
      Sys.readdir ("/sys/bus/pci/drivers/" ^ driver)
      |> Array.to_list
      |> List.filter
           (Re.execp (Re.Perl.compile_pat {|\d+:[a-f\d]+:[a-f\d]+\.\d|}))
      |> List.length
    with _ -> 0

  let parent_device_of_vf pcibuspath =
    try
      let pf_net_path =
        Printf.sprintf "/sys/bus/pci/devices/%s/physfn/net" pcibuspath
      in
      let devices = Sys.readdir pf_net_path in
      Result.Ok devices.(0)
    with _ ->
      Result.Error
        ( Parent_device_of_vf_not_found
        , "Can not get parent device for " ^ pcibuspath
        )

  let get_child_vfs_sysfs_paths dev =
    try
      let device_path = getpath dev "device" in
      Result.Ok
        (Sys.readdir device_path
        |> Array.to_list
        |> List.filter (Re.execp (Re.Perl.compile_pat "virtfn(\\d+)"))
        (* List elements are like "virtfn1" *)
        |> List.map (Filename.concat device_path)
        )
    with _ ->
      Result.Error
        (Vf_sysfs_path_not_found, "Can not get child vfs sysfs paths for " ^ dev)

  let device_index_of_vf parent_dev pcibuspath =
    try
      let open Rresult.R.Infix in
      get_child_vfs_sysfs_paths parent_dev >>= fun paths ->
      let group =
        List.find
          (fun x -> Astring.String.is_infix ~affix:pcibuspath (Unix.readlink x))
          paths
        |> Re.exec_opt (Re.Perl.compile_pat "virtfn(\\d+)")
      in
      match group with
      | None ->
          Result.Error
            (Vf_index_not_found, "Can not get device index for " ^ pcibuspath)
      | Some x ->
          Ok (int_of_string (Re.Group.get x 1))
    with _ ->
      Result.Error
        (Vf_index_not_found, "Can not get device index for " ^ pcibuspath)

  let unbind_child_vfs dev =
    let open Rresult.R.Infix in
    let unbind vf_path =
      let driver_name =
        try
          Unix.readlink (Filename.concat vf_path "driver") |> Filename.basename
        with _ -> ""
      and vf_pcibuspath = Unix.readlink vf_path |> Filename.basename in
      if driver_name = "" then
        Result.Ok () (* not bind to any driver, Ok *)
      else (
        debug "unbinding %s from driver %s at %s" vf_path driver_name
          vf_pcibuspath ;
        let unbind_interface = Filename.concat vf_path "driver/unbind"
        and remove_slot_interface =
          Filename.concat vf_path "driver/remove_slot"
        in
        (try write_one_line remove_slot_interface vf_pcibuspath with _ -> ()) ;
        try
          write_one_line unbind_interface vf_pcibuspath ;
          Result.Ok ()
        with _ ->
          Result.Error
            ( Fail_to_unbind_from_driver
            , Printf.sprintf "%s: VF Fail to be unbound from driver %s" vf_path
                driver_name
            )
      )
    in
    get_child_vfs_sysfs_paths dev >>= fun paths ->
    List.fold_left ( >>= ) (Ok ()) (List.map (fun x _ -> unbind x) paths)

  let get_sriov_numvfs dev =
    try
      getpath dev "device/sriov_numvfs"
      |> read_one_line
      |> String.trim
      |> int_of_string
    with _ -> 0

  let get_sriov_maxvfs dev =
    try
      Ok
        (getpath dev "device/sriov_totalvfs"
        |> read_one_line
        |> String.trim
        |> int_of_string
        )
    with _ ->
      Error
        ( Fail_to_get_maxvfs
        , "Failed to get maxvfs from sysfs interface for device: " ^ dev
        )

  let set_sriov_numvfs dev num_vfs =
    let interface = getpath dev "device/sriov_numvfs" in
    try
      write_one_line interface (string_of_int num_vfs) ;
      if get_sriov_numvfs dev = num_vfs then
        Result.Ok ()
      else
        Result.Error (Other, "Error: set SR-IOV error on " ^ dev)
    with
    | Sys_error s when Astring.String.is_infix ~affix:"out of range of" s ->
        Result.Error
          ( Bus_out_of_range
          , "Error: bus out of range when setting SR-IOV numvfs on " ^ dev
          )
    | Sys_error s
      when Astring.String.is_infix ~affix:"not enough MMIO resources" s ->
        Result.Error
          ( Not_enough_mmio_resources
          , "Error: not enough mmio resources when setting SR-IOV numvfs on "
            ^ dev
          )
    | e ->
        let msg =
          Printf.sprintf
            "Error: set SR-IOV numvfs error with exception %s on %s"
            (Printexc.to_string e) dev
        in
        Result.Error (Other, msg)
end

module Ip = struct
  type ipversion = V4 | V6 | V46

  let string_of_version = function V4 -> ["-4"] | V6 -> ["-6"] | V46 -> []

  let call ?log args = call_script ?log iproute2 args

  let find output attr =
    let args = Astring.String.fields ~empty:false output in
    let indices =
      Xapi_stdext_std.Listext.List.position (fun s -> s = attr) args
    in
    List.map (fun i -> List.nth args (succ i)) indices

  let get_link_flags dev =
    Sysfs.assert_exists dev ;
    let output = call ~log:false ["link"; "show"; "dev"; dev] in
    let i = String.index output '<' in
    let j = String.index output '>' in
    let flags = String.sub output (i + 1) (j - i - 1) in
    Astring.String.cuts ~empty:false ~sep:"," flags

  let is_up dev = try List.mem "UP" (get_link_flags dev) with _ -> false

  let link_set dev args =
    Sysfs.assert_exists dev ;
    ignore (call ("link" :: "set" :: dev :: args))

  let link_set_mtu dev mtu =
    try ignore (link_set dev ["mtu"; string_of_int mtu])
    with Network_error (Script_error _) ->
      error "MTU size is not supported: %d" mtu

  let link_set_up dev = link_set dev ["up"]

  let link_set_down dev =
    if is_up dev then
      link_set dev ["down"]

  let with_links_down devs f =
    let up_links = List.filter (fun dev -> is_up dev) devs in
    List.iter (fun dev -> link_set dev ["down"]) up_links ;
    Pervasiveext.finally f (fun () -> List.iter link_set_up up_links)

  let link ?(version = V46) dev attr =
    Sysfs.assert_exists dev ;
    let v = string_of_version version in
    let output = call ~log:false (v @ ["link"; "show"; "dev"; dev]) in
    find output attr

  let addr ?(version = V46) dev attr =
    Sysfs.assert_exists dev ;
    let v = string_of_version version in
    let output = call ~log:false (v @ ["addr"; "show"; "dev"; dev]) in
    find output attr

  let get_mtu dev = int_of_string (List.hd (link dev "mtu"))

  let get_mac dev =
    match link dev "link/ether" with
    | [] -> (
      match link dev "link/infiniband" with
      | m :: _ ->
          m
      | [] ->
          let msg =
            Printf.sprintf "can't find mac address for %s (%s)" dev __LOC__
          in
          error "%s" msg ;
          raise (Network_error (Internal_error msg))
    )
    | m :: _ ->
        m

  let set_mac dev mac =
    try ignore (link_set dev ["address"; mac]) with _ -> ()

  let split_addr addr =
    match Astring.String.cut ~sep:"/" addr with
    | Some (ipstr, prefixlenstr) ->
        let ip = Unix.inet_addr_of_string ipstr in
        let prefixlen = int_of_string prefixlenstr in
        Some (ip, prefixlen)
    | None ->
        None

  (* see http://en.wikipedia.org/wiki/IPv6_address#Modified_EUI-64 *)
  let get_ipv6_interface_id dev =
    let mac = get_mac dev in
    let bytes =
      List.map
        (fun byte -> int_of_string ("0x" ^ byte))
        (Astring.String.cuts ~empty:false ~sep:":" mac)
    in
    let rec modified_bytes ac i = function
      | [] ->
          ac
      | head :: tail ->
          if i = 0 then
            let head' = head lxor 2 in
            modified_bytes (head' :: ac) 1 tail
          else if i = 2 then
            modified_bytes (254 :: 255 :: head :: ac) 3 tail
          else
            modified_bytes (head :: ac) (i + 1) tail
    in
    let bytes' = List.rev (modified_bytes [] 0 bytes) in
    [0; 0; 0; 0; 0; 0; 0; 0] @ bytes'

  let get_ipv6_link_local_addr dev =
    let id = get_ipv6_interface_id dev in
    let link_local = 0xfe :: 0x80 :: List.tl (List.tl id) in
    let rec to_string ac i = function
      | [] ->
          ac
      | hd :: tl ->
          let separator =
            if i = 0 || i mod 2 = 1 then
              ""
            else
              ":"
          in
          let ac' = ac ^ separator ^ Printf.sprintf "%02x" hd in
          to_string ac' (i + 1) tl
    in
    to_string "" 0 link_local ^ "/64"

  let get_ipv4 dev =
    let addrs = addr dev "inet" in
    List.filter_map split_addr addrs

  let get_ipv6 dev =
    let addrs = addr dev "inet6" in
    List.filter_map split_addr addrs

  let set_ip_addr dev (ip, prefixlen) =
    let addr = Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ip) prefixlen in
    let broadcast =
      (* Set the broadcast address when adding an IPv4 address *)
      if String.contains addr '.' then
        ["broadcast"; "+"]
      else
        []
    in
    try ignore (call (["addr"; "add"; addr; "dev"; dev] @ broadcast))
    with _ -> ()

  let set_ipv6_link_local_addr dev =
    let addr = get_ipv6_link_local_addr dev in
    try ignore (call ["addr"; "add"; addr; "dev"; dev; "scope"; "link"])
    with _ -> ()

  let flush_ip_addr ?(ipv6 = false) dev =
    try
      Sysfs.assert_exists dev ;
      let mode = if ipv6 then "-6" else "-4" in
      ignore (call [mode; "addr"; "flush"; "dev"; dev])
    with _ -> ()

  let del_ip_addr dev (ip, prefixlen) =
    let addr = Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ip) prefixlen in
    try
      Sysfs.assert_exists dev ;
      ignore (call ["addr"; "del"; addr; "dev"; dev])
    with _ -> ()

  let route_show ?(version = V46) dev =
    let v = string_of_version version in
    call ~log:false (v @ ["route"; "show"; "dev"; dev])

  let set_route ?network dev gateway =
    try
      Sysfs.assert_exists dev ;
      match network with
      | None ->
          ignore
            (call
               [
                 "route"
               ; "replace"
               ; "default"
               ; "via"
               ; Unix.string_of_inet_addr gateway
               ; "dev"
               ; dev
               ]
            )
      | Some (ip, prefixlen) ->
          let addr =
            Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ip) prefixlen
          in
          ignore
            (call
               [
                 "route"
               ; "replace"
               ; addr
               ; "via"
               ; Unix.string_of_inet_addr gateway
               ; "dev"
               ; dev
               ]
            )
    with _ -> ()

  let set_gateway dev gateway = set_route dev gateway

  let vlan_name interface vlan = Printf.sprintf "%s.%d" interface vlan

  let create_vlan interface vlan =
    if not (Sysfs.exists (vlan_name interface vlan)) then
      ignore
        (call
           [
             "link"
           ; "add"
           ; "link"
           ; interface
           ; "name"
           ; vlan_name interface vlan
           ; "type"
           ; "vlan"
           ; "id"
           ; string_of_int vlan
           ]
        )

  let destroy_vlan name =
    if Sysfs.exists name then
      ignore (call ["link"; "delete"; name])

  let set_vf_mac dev index mac =
    try
      debug "Setting VF MAC address for dev: %s, index: %d, MAC: %s" dev index
        mac ;
      Result.Ok (link_set dev ["vf"; string_of_int index; "mac"; mac])
    with _ ->
      Result.Error (Fail_to_set_vf_mac, "Failed to set VF MAC for: " ^ dev)

  let set_vf_vlan dev index vlan =
    try
      debug "Setting VF VLAN for dev: %s, index: %d, VLAN: %d" dev index vlan ;
      Result.Ok
        (link_set dev ["vf"; string_of_int index; "vlan"; string_of_int vlan])
    with _ ->
      Result.Error (Fail_to_set_vf_vlan, "Failed to set VF VLAN for: " ^ dev)

  (** [set_vf_rate dev index rate] Updates the VF rate for a specified network interface card (NIC).
    Note that this function converts the input rate from kilobits per second (kbps) to
    megabits per second (Mbps) using division followed by ceil.

    This function accounts for the fact that some NICs may not support VF rate configuration.
    Any encountered errors during the process are explicitly reported to XAPI.

    @param dev    The network device identifier.
    @param index  The index of the VF.
    @param rate   The desired rate in kbps, it will be converted into Mbps using division and ceil.

    @return [Result.Ok] with the updated configuration upon success, or [Result.Error] wrapping [Fail_to_set_vf_rate] with the specific error message upon failure.
*)
  let set_vf_rate dev index rate =
    let divide_and_ceil (numerator : int) (denominator : int) : int =
      let quotient = numerator / denominator in
      let remainder = numerator mod denominator in
      if remainder > 0 then
        quotient + 1
      else
        quotient
    in
    try
      let megabits_per_second = divide_and_ceil rate 1000 in
      debug "Setting VF rate for dev: %s, index: %d, rate: %d Mbps" dev index
        megabits_per_second ;
      Result.Ok
        (link_set dev
           [
             "vf"; string_of_int index; "rate"; string_of_int megabits_per_second
           ]
        )
    with _ ->
      Result.Error (Fail_to_set_vf_rate, "Failed to set VF rate for: " ^ dev)
end

module Linux_bonding = struct
  let bonding_masters = "/sys/class/net/bonding_masters"

  let load_bonding_driver () =
    debug "Loading bonding driver" ;
    try
      ignore (call_script modprobe ["bonding"]) ;
      (* is_bond_device() uses the contents of sysfs_bonding_masters to work out
         which devices have already been created. Unfortunately the driver
         creates "bond0" automatically at modprobe init. Get rid of this now or
         our accounting will go wrong. *)
      Sysfs.write_one_line bonding_masters "-bond0"
    with _ -> error "Failed to load bonding driver"

  let bonding_driver_loaded () =
    try
      Unix.access bonding_masters [Unix.F_OK] ;
      true
    with _ -> false

  let is_bond_device name =
    try
      List.exists (( = ) name)
        (Astring.String.cuts ~empty:false ~sep:" "
           (Sysfs.read_one_line bonding_masters)
        )
    with _ -> false

  (** Ensures that a bond master device exists in the kernel. *)
  let add_bond_master name =
    if not (bonding_driver_loaded ()) then
      load_bonding_driver () ;
    if is_bond_device name then
      debug "Bond master %s already exists, not creating" name
    else (
      debug "Adding bond master %s" name ;
      try Sysfs.write_one_line bonding_masters ("+" ^ name)
      with _ -> error "Failed to add bond master %s" name
    )

  (** No, Mr. Bond, I expect you to die. *)
  let remove_bond_master name =
    if is_bond_device name then
      let rec destroy retries =
        debug "Removing bond master %s (%d attempts remain)" name retries ;
        try Sysfs.write_one_line bonding_masters ("-" ^ name)
        with _ ->
          if retries > 0 then (
            Thread.delay 0.5 ;
            destroy (retries - 1)
          ) else
            error "Failed to remove bond master %s" name
      in
      destroy 10
    else
      error "Bond master %s does not exist; cannot destroy it" name

  let get_bond_slaves master =
    let path = Sysfs.getpath master "bonding/slaves" in
    let slaves = Sysfs.read_one_line path in
    if slaves = "" then
      []
    else
      Astring.String.cuts ~empty:false ~sep:" " slaves

  let add_bond_slaves master slaves =
    List.iter
      (fun slave ->
        debug "Adding slave %s to bond %s" slave master ;
        try
          Sysfs.write_one_line
            (Sysfs.getpath master "bonding/slaves")
            ("+" ^ slave)
        with _ -> error "Failed to add slave %s to bond %s" slave master
      )
      slaves

  let remove_bond_slaves master slaves =
    List.iter
      (fun slave ->
        debug "Removing slave %s from bond %s" slave master ;
        try
          Sysfs.write_one_line
            (Sysfs.getpath master "bonding/slaves")
            ("-" ^ slave)
        with _ -> error "Failed to remove slave %s from bond %s" slave master
      )
      slaves

  let set_bond_slaves master slaves =
    if is_bond_device master then
      let current_slaves = get_bond_slaves master in
      let slaves_to_remove =
        Xapi_stdext_std.Listext.List.set_difference current_slaves slaves
      in
      let slaves_to_add =
        Xapi_stdext_std.Listext.List.set_difference slaves current_slaves
      in
      Ip.with_links_down (slaves_to_add @ slaves_to_remove) (fun () ->
          remove_bond_slaves master slaves_to_remove ;
          add_bond_slaves master slaves_to_add
      )
    else
      error "Bond %s does not exist; cannot set slaves" master

  let with_slaves_removed master f =
    if is_bond_device master then
      try
        let slaves = get_bond_slaves master in
        Ip.with_links_down slaves (fun () ->
            remove_bond_slaves master slaves ;
            Pervasiveext.finally f (fun () -> add_bond_slaves master slaves)
        )
      with _ -> error "Failed to remove or re-add slaves from bond %s" master
    else
      error "Bond %s does not exist; cannot remove/add slaves" master

  let get_bond_master_of slave =
    try
      let master_symlink = Sysfs.getpath slave "master" in
      let master_path = Unix.readlink master_symlink in
      let slaves_path = Filename.concat master_symlink "bonding/slaves" in
      Unix.access slaves_path [Unix.F_OK] ;
      Some (Filename.basename master_path)
    with _ -> None

  let get_bond_active_slave master =
    try Some (Sysfs.read_one_line (Sysfs.getpath master "bonding/active_slave"))
    with _ ->
      error "Failed to get active_slave of bond %s" master ;
      None

  let known_props = ["mode"; "updelay"; "downdelay"; "miimon"; "use_carrier"]

  let get_bond_properties master =
    if is_bond_device master then
      let get_prop prop =
        try
          let bond_prop =
            Sysfs.read_one_line (Sysfs.getpath master ("bonding/" ^ prop))
          in
          if prop = "mode" then
            let get_mode line =
              let a_space char = char = ' ' in
              Astring.String.(
                line |> drop ~sat:a_space |> take ~sat:(Fun.negate a_space)
              )
            in
            Some (prop, get_mode bond_prop)
          else
            Some (prop, bond_prop)
        with _ ->
          debug "Failed to get property \"%s\" on bond %s" prop master ;
          None
      in
      List.filter_map get_prop known_props
    else (
      debug "Bond %s does not exist; cannot get properties" master ;
      []
    )

  let set_bond_properties master properties =
    if is_bond_device master then (
      let current_props = get_bond_properties master in
      debug "Current bond properties: %s"
        (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) current_props)) ;
      (* Find out which properties are known, but different from the current
         state, and only continue if there is at least one of those. *)
      let props_to_update =
        List.filter
          (fun (prop, value) ->
            (not (List.mem (prop, value) current_props))
            && List.mem prop known_props
          )
          properties
      in
      debug "Bond properties to update: %s"
        (String.concat ", "
           (List.map (fun (k, v) -> k ^ "=" ^ v) props_to_update)
        ) ;
      if props_to_update <> [] then
        let set_prop (prop, value) =
          try
            debug "Setting %s=%s on bond %s" prop value master ;
            Sysfs.write_one_line
              (Sysfs.getpath master ("bonding/" ^ prop))
              value
          with _ ->
            error "Failed to set property \"%s\" on bond %s" prop master
        in
        Ip.with_links_down [master] (fun () ->
            with_slaves_removed master (fun () ->
                List.iter set_prop props_to_update
            )
        )
    ) else
      error "Bond %s does not exist; cannot set properties" master
end

module Dhclient : sig
  type interface = string

  val remove_conf_file : ?ipv6:bool -> interface -> unit

  val is_running : ?ipv6:bool -> interface -> bool

  val stop : ?ipv6:bool -> interface -> unit

  val ensure_running :
       ?ipv6:bool
    -> interface
    -> [> `dns of string | `gateway of string] list
    -> unit
end = struct
  type interface = string

  let pid_file ?(ipv6 = false) interface =
    let ipv6' = if ipv6 then "6" else "" in
    Printf.sprintf "/var/run/dhclient%s-%s.pid" ipv6' interface

  let lease_file ?(ipv6 = false) interface =
    let ipv6' = if ipv6 then "6" else "" in
    Filename.concat "/var/lib/xcp"
      (Printf.sprintf "dhclient%s-%s.leases" ipv6' interface)

  let conf_file ?(ipv6 = false) interface =
    let ipv6' = if ipv6 then "6" else "" in
    Filename.concat "/var/lib/xcp"
      (Printf.sprintf "dhclient%s-%s.conf" ipv6' interface)

  let[@warning "-27"] generate_conf ?(ipv6 = false) interface options =
    let send = "host-name = gethostname()" in
    let minimal =
      [
        "subnet-mask"
      ; "broadcast-address"
      ; "time-offset"
      ; "host-name"
      ; "nis-domain"
      ; "nis-servers"
      ; "ntp-servers"
      ; "interface-mtu"
      ]
    in
    let set_gateway =
      if List.mem (`gateway interface) options then (
        debug "%s is the default gateway interface" interface ;
        ["routers"]
      ) else (
        debug "%s is NOT the default gateway interface" interface ;
        []
      )
    in
    let set_dns =
      if List.mem (`dns interface) options then (
        debug "%s is the DNS interface" interface ;
        ["domain-name"; "domain-name-servers"]
      ) else (
        debug "%s is NOT the DNS interface" interface ;
        []
      )
    in
    let request = minimal @ set_gateway @ set_dns in
    Printf.sprintf "interface \"%s\" {\n  send %s;\n  request %s;\n}\n"
      interface send
      (String.concat ", " request)

  let read_conf_file ?(ipv6 = false) interface =
    let file = conf_file ~ipv6 interface in
    try Some (Xapi_stdext_unix.Unixext.string_of_file file) with _ -> None

  let write_conf_file ?(ipv6 = false) interface options =
    let conf = generate_conf ~ipv6 interface options in
    Xapi_stdext_unix.Unixext.write_string_to_file
      (conf_file ~ipv6 interface)
      conf

  let remove_conf_file ?(ipv6 = false) interface =
    let file = conf_file ~ipv6 interface in
    try Unix.unlink file with _ -> ()

  let start ?(ipv6 = false) interface options =
    (* If we have a gateway interface, pass it to dhclient-script via -e *)
    (* This prevents the default route being set erroneously on CentOS *)
    (* Normally this wouldn't happen as we're not requesting routers, *)
    (* but some buggy DHCP servers ignore this *)
    (* Same story for DNS! *)
    (* See CA-137892 *)
    let gw_opt =
      List.fold_left
        (fun l x ->
          match x with `gateway y -> ["-e"; "GATEWAYDEV=" ^ y] | _ -> l
        )
        [] options
    in
    let dns_opt =
      if List.mem (`dns interface) options then [] else ["-e"; "PEERDNS=no"]
    in
    write_conf_file ~ipv6 interface options ;
    let ipv6' = if ipv6 then ["-6"] else [] in
    call_script ~timeout:None dhclient
      (ipv6'
      @ gw_opt
      @ dns_opt
      @ [
          "-q"
        ; "-pf"
        ; pid_file ~ipv6 interface
        ; "-lf"
        ; lease_file ~ipv6 interface
        ; "-cf"
        ; conf_file ~ipv6 interface
        ; interface
        ]
      )

  let stop ?(ipv6 = false) interface =
    try
      ignore
        (call_script dhclient
           [
             "-r"
           ; "-pf"
           ; pid_file ~ipv6 interface
           ; "-lf"
           ; lease_file ~ipv6 interface
           ; interface
           ]
        ) ;
      Unix.unlink (pid_file ~ipv6 interface)
    with _ -> ()

  let is_running ?(ipv6 = false) interface =
    try
      Unix.access (pid_file ~ipv6 interface) [Unix.F_OK] ;
      true
    with Unix.Unix_error _ -> false

  let ensure_running ?(ipv6 = false) interface options =
    if not (is_running ~ipv6 interface) then
      (* dhclient is not running, so we need to start it. *)
      ignore (start ~ipv6 interface options)
    else
      (* dhclient is running - if the config has changed, update the config file
         and restart. *)
      let current_conf = read_conf_file ~ipv6 interface in
      let new_conf = generate_conf ~ipv6 interface options in
      if current_conf <> Some new_conf then (
        ignore (stop ~ipv6 interface) ;
        ignore (start ~ipv6 interface options)
      )
end

module Fcoe = struct
  let call ?log args = call_script ?log ~timeout:(Some 10.0) !fcoedriver args

  let get_capabilities name =
    try
      let output = call ~log:false ["--xapi"; name; "capable"] in
      if Astring.String.is_infix ~affix:"True" output then ["fcoe"] else []
    with _ ->
      debug "Failed to get fcoe support status on device %s" name ;
      []
end

module Sysctl = struct
  let write value variable =
    ignore (call_script sysctl ["-q"; "-w"; variable ^ "=" ^ value])

  let set_ipv6_autoconf interface value =
    try
      let variables =
        [
          "net.ipv6.conf." ^ interface ^ ".autoconf"
        ; "net.ipv6.conf." ^ interface ^ ".accept_ra"
        ]
      in
      let value' = if value then "1" else "0" in
      List.iter (write value') variables
    with
    | e when value = true ->
        raise e
    | _ ->
        ()
end

module Proc = struct
  let get_bond_slave_info name key =
    try
      let raw = Xapi_stdext_unix.Unixext.string_of_file (bonding_dir ^ name) in
      let lines = Astring.String.cuts ~empty:false ~sep:"\n" raw in
      let check_lines lines =
        let rec loop current acc = function
          | [] ->
              acc
          | line :: tail -> (
            try
              Scanf.sscanf line "%s@: %s@\n" (fun k v ->
                  if k = "Slave Interface" then
                    let interface = Some (String.trim v) in
                    loop interface acc tail
                  else if k = key then
                    match current with
                    | Some interface ->
                        loop current ((interface, String.trim v) :: acc) tail
                    | None ->
                        loop current acc tail
                  else
                    loop current acc tail
              )
            with _ -> loop current acc tail
          )
        in
        loop None [] lines
      in
      check_lines lines
    with _ ->
      error "Error: could not read %s." (bonding_dir ^ name) ;
      []

  let get_bond_slave_mac name slave =
    let macs = get_bond_slave_info name "Permanent HW addr" in
    if List.mem_assoc slave macs then
      List.assoc slave macs
    else
      raise Not_found

  let get_vlans () =
    try
      Xapi_stdext_unix.Unixext.file_lines_fold
        (fun vlans line ->
          try
            let x =
              Scanf.sscanf line "%s | %d | %s" (fun device vlan parent ->
                  (device, vlan, parent)
              )
            in
            x :: vlans
          with _ -> vlans
        )
        [] "/proc/net/vlan/config"
    with _ ->
      error "Error: could not read /proc/net/vlan/config" ;
      []

  let get_ipv6_disabled () =
    try
      Unixext.string_of_file "/proc/sys/net/ipv6/conf/all/disable_ipv6"
      |> String.trim
      |> ( = ) "1"
    with _ -> false
end

module Ovs = struct
  let match_multiple patterns s =
    let rec loop = function
      | [] ->
          None
      | pattern :: rest -> (
        match Re.exec_opt pattern s with
        | Some groups ->
            Some groups
        | None ->
            loop rest
      )
    in
    loop patterns

  let patterns =
    List.map Re.Perl.compile_pat
      ["no bridge named (.*)\n"; "no row \"(.*)\" in table Bridge"]

  let error_handler script args stdout stderr exn =
    match match_multiple patterns stderr with
    | Some groups ->
        let bridge = Re.Group.get groups 1 in
        raise (Network_error (Bridge_does_not_exist bridge))
    | None ->
        default_error_handler script args stdout stderr exn

  module Cli : sig
    val vsctl : ?log:bool -> string list -> string

    val ofctl : ?log:bool -> string list -> string

    val appctl : ?log:bool -> string list -> string
  end = struct
    module Semaphore = Xapi_stdext_threads.Semaphore

    let s = Semaphore.create 5

    let vsctl ?log args =
      Semaphore.execute s (fun () ->
          call_script ~on_error:error_handler ?log ovs_vsctl
            ("--timeout=20" :: args)
      )

    let ofctl ?log args =
      call_script ~on_error:error_handler ?log ovs_ofctl args

    let appctl ?log args =
      call_script ~on_error:error_handler ?log ovs_appctl args
  end

  module type Cli_S = module type of Cli

  module Make (Cli : Cli_S) = struct
    include Cli

    let port_to_interfaces name =
      try
        let raw = vsctl ["get"; "port"; name; "interfaces"] in
        let raw = String.trim raw in
        if raw <> "[]" then
          let raw_list =
            Astring.String.cuts ~empty:false ~sep:","
              (String.sub raw 1 (String.length raw - 2))
          in
          let uuids = List.map String.trim raw_list in
          List.map
            (fun uuid ->
              let raw =
                String.trim (vsctl ~log:false ["get"; "interface"; uuid; "name"])
              in
              String.sub raw 1 (String.length raw - 2)
            )
            uuids
        else
          []
      with _ -> []

    let bridge_to_ports name =
      try
        let ports = String.trim (vsctl ~log:false ["list-ports"; name]) in
        let ports' =
          if ports <> "" then
            Astring.String.cuts ~empty:false ~sep:"\n" ports
          else
            []
        in
        List.map (fun port -> (port, port_to_interfaces port)) ports'
      with _ -> []

    let bridge_to_interfaces name =
      try
        let ifaces = String.trim (vsctl ~log:false ["list-ifaces"; name]) in
        if ifaces <> "" then
          Astring.String.cuts ~empty:false ~sep:"\n" ifaces
        else
          []
      with _ -> []

    let bridge_to_vlan name =
      try
        let parent = vsctl ~log:false ["br-to-parent"; name] |> String.trim in
        let vlan =
          vsctl ~log:false ["br-to-vlan"; name] |> String.trim |> int_of_string
        in
        Some (parent, vlan)
      with e ->
        debug "bridge_to_vlan: %s" (Printexc.to_string e) ;
        None

    let get_real_bridge name =
      match bridge_to_vlan name with
      | Some (parent, _vlan) ->
          parent
      | None ->
          name

    let get_bond_link_status name =
      try
        let raw = appctl ~log:false ["bond/show"; name] in
        let lines = Astring.String.cuts ~empty:false ~sep:"\n" raw in
        List.fold_left
          (fun (slaves, active_slave) line ->
            let slaves =
              try
                Scanf.sscanf line "slave %s@: %s" (fun slave state ->
                    (slave, state = "enabled") :: slaves
                )
              with _ -> slaves
            in
            let active_slave =
              try
                Scanf.sscanf line "active slave %s@(%s@)" (fun _ slave ->
                    Some slave
                )
              with _ -> active_slave
            in
            (slaves, active_slave)
          )
          ([], None) lines
      with _ -> ([], None)

    let get_bond_mode name =
      try
        let output =
          String.trim (vsctl ~log:false ["get"; "port"; name; "bond_mode"])
        in
        if output <> "[]" then Some output else None
      with _ -> None

    let set_max_idle t =
      try
        ignore
          (vsctl
             [
               "set"
             ; "Open_vSwitch"
             ; "."
             ; Printf.sprintf "other_config:max-idle=%d" t
             ]
          )
      with _ -> warn "Failed to set max-idle=%d on OVS" t

    let handle_vlan_bug_workaround override bridge =
      (* This is a list of drivers that do support VLAN tx or rx acceleration,
         but to which the VLAN bug workaround should not be applied. This could
         be because these are known-good drivers (that is, they do not have any
         of the bugs that the workaround avoids) or because the VLAN bug
         workaround will not work for them and may cause other problems.

         This is a very short list because few drivers have been tested. *)
      let no_vlan_workaround_drivers = ["bonding"] in
      let phy_interfaces =
        try
          let interfaces = bridge_to_interfaces bridge in
          List.filter Sysfs.is_physical interfaces
        with _ -> []
      in
      List.iter
        (fun interface ->
          let do_workaround =
            match override with
            | Some value ->
                value
            | None -> (
              match Sysfs.get_driver_name interface with
              | None ->
                  Sysfs.has_vlan_accel interface
              | Some driver ->
                  if List.mem driver no_vlan_workaround_drivers then
                    false
                  else
                    Sysfs.has_vlan_accel interface
            )
          in
          let setting = if do_workaround then "on" else "off" in
          try ignore (call_script ovs_vlan_bug_workaround [interface; setting])
          with _ -> ()
        )
        phy_interfaces

    let get_vlans name =
      try
        let vlans_with_uuid =
          let raw =
            vsctl ~log:false
              [
                "--bare"
              ; "-f"
              ; "table"
              ; "--"
              ; "--columns=name,_uuid"
              ; "find"
              ; "port"
              ; "fake_bridge=true"
              ]
          in
          if raw <> "" then
            let lines =
              Astring.String.cuts ~empty:false ~sep:"\n" (String.trim raw)
            in
            List.map
              (fun line -> Scanf.sscanf line "%s %s" (fun a b -> (a, b)))
              lines
          else
            []
        in
        let bridge_ports =
          let raw = vsctl ~log:false ["get"; "bridge"; name; "ports"] in
          let raw = String.trim raw in
          if raw <> "[]" then
            let raw_list =
              Astring.String.cuts ~empty:false ~sep:","
                (String.sub raw 1 (String.length raw - 2))
            in
            List.map String.trim raw_list
          else
            []
        in
        let vlans_on_bridge =
          List.filter (fun (_, br) -> List.mem br bridge_ports) vlans_with_uuid
        in
        List.map (fun (n, _) -> n) vlans_on_bridge
      with _ -> []

    let get_bridge_vlan_vifs ~name =
      try
        let vlan_fake_bridges = get_vlans name in
        List.fold_left
          (fun vifs br ->
            let vifs' = bridge_to_interfaces br in
            vifs' @ vifs
          )
          [] vlan_fake_bridges
      with _ -> []

    let get_mcast_snooping_enable ~name =
      try
        vsctl ~log:false ["--"; "get"; "bridge"; name; "mcast_snooping_enable"]
        |> String.trim
        |> bool_of_string
      with _ -> false

    let inject_igmp_query ~name =
      try
        let vvifs = get_bridge_vlan_vifs ~name in
        let bvifs = bridge_to_interfaces name in
        let bvifs' =
          List.filter
            (fun vif -> Astring.String.is_prefix ~affix:"vif" vif)
            bvifs
        in
        (* The vifs may be large. However considering current XS limit of
           1000VM*7NIC/VM + 800VLANs, the buffer of CLI should be sufficient for
           lots of vifxxxx.xx *)
        fork_script !inject_igmp_query_script
          ([
             "--no-check-snooping-toggle"
           ; "--max-resp-time"
           ; !igmp_query_maxresp_time
           ]
          @ bvifs'
          @ vvifs
          )
      with _ -> ()

    let create_port_arg ?ty name bridge =
      let type_args =
        match ty with
        | None | Some "" ->
            []
        | Some s ->
            ["--"; "set"; "interface"; name; Printf.sprintf "type=%s" s]
      in
      ["--"; "--may-exist"; "add-port"; bridge; name] @ type_args

    let create_bridge ?mac ?external_id ?disable_in_band ?igmp_snooping
        ~fail_mode vlan vlan_bug_workaround name =
      let vlan_arg =
        match vlan with
        | None ->
            []
        | Some (parent, tag) ->
            handle_vlan_bug_workaround vlan_bug_workaround parent ;
            [parent; string_of_int tag]
      in
      let mac_arg =
        match mac with
        | None ->
            []
        | Some mac ->
            if vlan = None then
              [
                "--"
              ; "set"
              ; "bridge"
              ; name
              ; Printf.sprintf "other-config:hwaddr=\"%s\"" (String.escaped mac)
              ]
            else
              [
                "--"
              ; "set"
              ; "interface"
              ; name
              ; Printf.sprintf "MAC=\"%s\"" (String.escaped mac)
              ]
      in
      let fail_mode_arg =
        if vlan = None then
          ["--"; "set"; "bridge"; name; "fail_mode=" ^ fail_mode]
        else
          []
      in
      let external_id_arg =
        match external_id with
        | None ->
            []
        | Some (key, value) -> (
          match vlan with
          | None ->
              ["--"; "br-set-external-id"; name; key; value]
          | Some (parent, _) ->
              ["--"; "br-set-external-id"; parent; key; value]
        )
      in
      let disable_in_band_arg =
        if vlan = None then
          match disable_in_band with
          | None ->
              []
          | Some None ->
              [
                "--"; "remove"; "bridge"; name; "other_config"; "disable-in-band"
              ]
          | Some (Some dib) ->
              [
                "--"
              ; "set"
              ; "bridge"
              ; name
              ; "other_config:disable-in-band=" ^ dib
              ]
        else
          []
      in
      let vif_arg =
        let existing_vifs =
          List.filter
            (fun iface -> not (Sysfs.is_physical iface))
            (bridge_to_interfaces name)
        in
        let ifaces_with_type =
          let raw =
            vsctl ~log:false
              [
                "--bare"
              ; "-f"
              ; "table"
              ; "--"
              ; "--columns=name,type"
              ; "find"
              ; "interface"
              ; {|type!=\"\"|}
              ]
          in
          let lines =
            Astring.String.cuts ~empty:false ~sep:"\n" (String.trim raw)
          in
          let parse l =
            match Astring.String.cut ~sep:" " l with
            | Some (k, v) ->
                let k' = String.trim k and v' = String.trim v in
                if k' = "" || v' = "" then None else Some (k', v')
            | None ->
                None
          in
          List.filter_map parse lines
        in
        List.flatten
          (List.map
             (fun vif ->
               create_port_arg
                 ?ty:(List.assoc_opt vif ifaces_with_type)
                 vif name
             )
             existing_vifs
          )
      in
      let del_old_arg =
        let real_bridge_exists () =
          try
            (* `ovs-vsctl br-to-parent <name>` returns <name> if <name> is a
               current "real" bridge *)
            vsctl ~log:false ["br-to-parent"; name] |> String.trim = name
          with _ -> false
        in
        if vlan <> None && real_bridge_exists () then
          (* This is to handle the case that a "real" bridge (not a "fake" VLAN
             bridge) already exists, while we need to create a VLAN bridge with
             the same name. The bridge will be destroyed and recreated, and the
             interfaces on it are put back. *)
          ["--"; "--if-exists"; "del-br"; name]
        else
          []
      in
      let set_mac_table_size =
        if vlan = None then
          [
            "--"
          ; "set"
          ; "bridge"
          ; name
          ; "other_config:mac-table-size=" ^ string_of_int !mac_table_size
          ]
        else
          []
      in
      let set_igmp_snooping =
        match (igmp_snooping, vlan) with
        | Some x, None ->
            [
              "--"
            ; "set"
            ; "bridge"
            ; name
            ; "mcast_snooping_enable=" ^ string_of_bool x
            ]
        | _ ->
            []
      in
      let set_ipv6_igmp_snooping =
        match (igmp_snooping, vlan) with
        | Some _, None ->
            [
              "--"
            ; "set"
            ; "bridge"
            ; name
            ; "other_config:enable-ipv6-mcast-snooping="
              ^ string_of_bool !enable_ipv6_mcast_snooping
            ]
        | _ ->
            []
      in
      let disable_flood_unregistered =
        match (igmp_snooping, vlan) with
        | Some _, None ->
            [
              "--"
            ; "set"
            ; "bridge"
            ; name
            ; "other_config:mcast-snooping-disable-flood-unregistered="
              ^ string_of_bool !mcast_snooping_disable_flood_unregistered
            ]
        | _ ->
            []
      in
      vsctl
        (del_old_arg
        @ ["--"; "--may-exist"; "add-br"; name]
        @ vlan_arg
        @ mac_arg
        @ fail_mode_arg
        @ disable_in_band_arg
        @ external_id_arg
        @ vif_arg
        @ set_mac_table_size
        @ set_igmp_snooping
        @ set_ipv6_igmp_snooping
        @ disable_flood_unregistered
        )

    let destroy_bridge name = vsctl ["--"; "--if-exists"; "del-br"; name]

    let list_bridges () =
      let bridges = String.trim (vsctl ~log:false ["list-br"]) in
      if bridges <> "" then
        Astring.String.cuts ~empty:false ~sep:"\n" bridges
      else
        []

    let create_port ?(internal = false) name bridge =
      let ty = if internal then Some "internal" else None in
      vsctl (create_port_arg ?ty name bridge)

    let destroy_port name =
      vsctl ["--"; "--with-iface"; "--if-exists"; "del-port"; name]

    let make_bond_properties name properties =
      let known_props =
        [
          "mode"
        ; "hashing-algorithm"
        ; "updelay"
        ; "downdelay"
        ; "miimon"
        ; "use_carrier"
        ; "rebalance-interval"
        ; "lacp-time"
        ; "lacp-aggregation-key"
        ; "lacp-fallback-ab"
        ]
      in
      let mode_args =
        let mode =
          if List.mem_assoc "mode" properties then
            List.assoc "mode" properties
          else
            "balance-slb"
        in
        let halgo =
          if List.mem_assoc "hashing-algorithm" properties then
            List.assoc "hashing-algorithm" properties
          else
            ""
        in
        if mode = "lacp" then
          "lacp=active"
          ::
          ( if halgo = "src_mac" then
              ["bond_mode=balance-slb"]
          else if halgo = "tcpudp_ports" then
            ["bond_mode=balance-tcp"]
          else (
            debug
              "bond %s has invalid bond-hashing-algorithm '%s'; defaulting to \
               balance-tcp"
              name halgo ;
            ["bond_mode=balance-tcp"]
          )
          )
        else
          ["lacp=off"; "bond_mode=" ^ mode]
      in
      (* "legacy" converter for bond properties *)
      let get_prop_legacy (prop, ovs_key) =
        if List.mem_assoc prop properties then
          let value = List.assoc prop properties in
          let value' = try int_of_string value with _ -> -1 in
          if value' < 0 then (
            debug "bond %s has invalid %s '%s'\n" name prop value ;
            []
          ) else if prop = "use_carrier" then
            [(ovs_key ^ "=" ^ if value' > 0 then "carrier" else "miimon")]
          else
            [ovs_key ^ "=" ^ string_of_int value']
        else
          []
      and get_prop (prop, ovs_key) =
        if List.mem_assoc prop properties then
          let value = List.assoc prop properties in
          [ovs_key ^ "=\"" ^ value ^ "\""]
        else
          []
      in
      (* Don't add new properties here, these use the legacy converter *)
      let extra_args_legacy =
        List.flatten
          (List.map get_prop_legacy
             [
               ("updelay", "bond_updelay")
             ; ("downdelay", "bond_downdelay")
             ; ("miimon", "other-config:bond-miimon-interval")
             ; ("use_carrier", "other-config:bond-detect-mode")
             ; ("rebalance-interval", "other-config:bond-rebalance-interval")
             ]
          )
      and extra_args =
        List.flatten
          (List.map get_prop
             [
               ("lacp-time", "other-config:lacp-time")
             ; ("lacp-fallback-ab", "other-config:lacp-fallback-ab")
             ]
          )
      and per_iface_args =
        List.flatten
          (List.map get_prop
             [
               ("lacp-aggregation-key", "other-config:lacp-aggregation-key")
             ; ("lacp-actor-key", "other-config:lacp-actor-key")
             ]
          )
      and other_args =
        List.filter_map
          (fun (k, v) ->
            if List.mem k known_props then
              None
            else
              Some
                (Printf.sprintf "other-config:\"%s\"=\"%s\""
                   (String.escaped ("bond-" ^ k))
                   (String.escaped v)
                )
          )
          properties
      in
      (mode_args @ extra_args_legacy @ extra_args @ other_args, per_iface_args)

    let create_bond ?mac name interfaces bridge properties =
      let args, per_iface_args = make_bond_properties name properties in
      let mac_args =
        match mac with
        | None ->
            []
        | Some mac ->
            ["--"; "set"; "port"; name; "MAC=\"" ^ String.escaped mac ^ "\""]
      in
      let per_iface_args =
        if per_iface_args = [] then
          []
        else
          List.flatten
            (List.map
               (fun iface -> ["--"; "set"; "interface"; iface] @ per_iface_args)
               interfaces
            )
      in
      vsctl
        (["--"; "--may-exist"; "add-bond"; bridge; name]
        @ interfaces
        @ mac_args
        @ args
        @ per_iface_args
        )

    let get_fail_mode bridge = vsctl ~log:false ["get-fail-mode"; bridge]

    let add_default_flows bridge mac interfaces =
      let ports =
        List.map
          (fun interface -> vsctl ["get"; "interface"; interface; "ofport"])
          interfaces
      in
      let flows =
        match ports with
        | [port] ->
            [
              Printf.sprintf
                "idle_timeout=0,priority=0,in_port=%s,arp,nw_proto=1,actions=local"
                port
            ; Printf.sprintf
                "idle_timeout=0,priority=0,in_port=local,arp,dl_src=%s,actions=%s"
                mac port
            ; Printf.sprintf
                "idle_timeout=0,priority=0,in_port=%s,dl_dst=%s,actions=local"
                port mac
            ; Printf.sprintf
                "idle_timeout=0,priority=0,in_port=local,dl_src=%s,actions=%s"
                mac port
            ]
        | ports ->
            List.flatten
              (List.map
                 (fun port ->
                   [
                     Printf.sprintf
                       "idle_timeout=0,priority=0,in_port=local,arp,dl_src=%s,actions=NORMAL"
                       mac
                   ; Printf.sprintf
                       "idle_timeout=0,priority=0,in_port=local,dl_src=%s,actions=NORMAL"
                       mac
                   ; Printf.sprintf
                       "idle_timeout=0,priority=0,in_port=%s,arp,nw_proto=1,actions=local"
                       port
                   ; Printf.sprintf
                       "idle_timeout=0,priority=0,in_port=%s,dl_dst=%s,actions=local"
                       port mac
                   ]
                 )
                 ports
              )
      in
      List.iter (fun flow -> ignore (ofctl ["add-flow"; bridge; flow])) flows

    let mod_port bridge port action =
      ofctl ["mod-port"; bridge; port; action] |> ignore

    let set_mtu interface mtu =
      vsctl ["set"; "interface"; interface; Printf.sprintf "mtu_request=%d" mtu]
  end

  include Make (Cli)
end

module Brctl = struct
  let call args = call_script !brctl args

  let create_bridge name =
    if not (List.mem name (Sysfs.list ())) then
      ignore (call ["addbr"; name])

  let destroy_bridge name =
    if List.mem name (Sysfs.list ()) then
      ignore (call ["delbr"; name])

  let create_port bridge name =
    if not (List.mem name (Sysfs.bridge_to_interfaces bridge)) then
      ignore (call ["addif"; bridge; name])

  let destroy_port bridge name =
    if List.mem name (Sysfs.bridge_to_interfaces bridge) then
      ignore (call ["delif"; bridge; name])

  let set_forwarding_delay bridge time =
    ignore (call ["setfd"; bridge; string_of_int time])
end

module Ethtool = struct
  let call args = call_script !ethtool args

  let set_options name options =
    if options <> [] then
      ignore
        (call
           ("-s"
           :: name
           :: List.concat (List.map (fun (k, v) -> [k; v]) options)
           )
        )

  let set_offload name options =
    if options <> [] then
      ignore
        (call
           ("-K"
           :: name
           :: List.concat (List.map (fun (k, v) -> [k; v]) options)
           )
        )
end

module Dracut = struct
  let call args = call_script ~timeout:(Some !dracut_timeout) !dracut args

  let rebuild_initrd () =
    try
      info "Building initrd..." ;
      let img_name = call_script !uname ["-r"] |> String.trim in
      call ["-f"; Printf.sprintf "/boot/initrd-%s.img" img_name; img_name]
      |> ignore ;
      Result.Ok ()
    with _ ->
      Result.Error (Fail_to_rebuild_initrd, "Error occurs in building initrd")
end

module Modinfo = struct
  let call args = call_script !modinfo args

  let is_param_array driver param_name =
    try
      let out =
        call ["--parameter"; driver] |> String.trim |> String.split_on_char '\n'
      in
      let re = Re.Perl.compile_pat "\\((.*)\\)$" in
      let has_array_of str =
        match Re.exec_opt re str with
        | None ->
            false
        | Some x ->
            Re.Group.get x 1 |> Astring.String.is_infix ~affix:"array of"
      in
      Ok
        (List.exists
           (fun line ->
             match Astring.String.cut ~sep:":" line with
             | None ->
                 false
             | Some (param, description) ->
                 String.trim param = param_name && has_array_of description
           )
           out
        )
    with _ ->
      Error
        ( Other
        , Printf.sprintf
            "Failed to determine if VF param of driver '%s' is an array" driver
        )
end

module Modprobe = struct
  let getpath driver = Printf.sprintf "/etc/modprobe.d/%s.conf" driver

  let write_conf_file driver content =
    try
      Unixext.write_string_to_file (getpath driver) (String.concat "\n" content) ;
      Result.Ok ()
    with _ ->
      Result.Error
        ( Fail_to_write_modprobe_cfg
        , "Failed to write modprobe configuration file for: " ^ driver
        )

  (* For a igb driver, the module config file will be at path
     `/etc/modprobe.d/igb.conf`

     The module config file is like:
     # VFs-param: max_vfs
     # VFs-maxvfs-by-default: 7
     # VFs-maxvfs-by-user: options igb max_vfs=7,7

     Example of calls:
     "igb" -> "VFs-param" -> Some "max_vfs"
     "igb" -> "VFs-maxvfs-by-default" -> Some "7"
     "igb" -> "VFs-maxvfs-by-user" -> None
     "igb" -> "Not existed comments" -> None
  *)
  let get_config_from_comments driver =
    try
      Unixext.read_lines ~path:(getpath driver)
      |> List.filter_map (fun x ->
             let line = String.trim x in
             if not (Astring.String.is_prefix ~affix:"# " line) then
               None
             else
               match
                 Astring.String.cut ~sep:":"
                   (Astring.String.with_range ~first:2 line)
               with
               | None ->
                   None
               | Some (k, v) when String.trim k = "" || String.trim v = "" ->
                   None
               | Some (k, v) ->
                   Some (String.trim k, String.trim v)
         )
    with _ -> []

  (* this function not returning None means that the driver doesn't suppport
     sysfs. If a driver doesn't support sysfs, then we add VF_param into its
     driver modprobe configuration. Therefore, from XAPI's perspective, if
     Modprobe.get_vf_param is not None, the driver definitely should use
     modprobe other than sysfs, and if Modprobe.get_vf_param is None, we just
     simple try sysfs. *)
  let get_vf_param config =
    try Some (List.assoc "VFs-param" config) with _ -> None

  let get_maxvfs driver config =
    let get_default_maxvfs config =
      try Some (List.assoc "VFs-maxvfs-by-default" config |> int_of_string)
      with _ -> None
    in
    let get_user_defined_maxvfs config =
      try Some (List.assoc "VFs-maxvfs-by-user" config |> int_of_string)
      with _ -> None
    in
    match (get_default_maxvfs config, get_user_defined_maxvfs config) with
    | Some a, None ->
        Result.Ok a
    | Some a, Some b ->
        Result.Ok (min a b)
        (* If users also define a maxvfs, we will use the smaller one *)
    | _ ->
        Result.Error (Fail_to_get_maxvfs, "Fail to get maxvfs for " ^ driver)

  let config_sriov driver vf_param maxvfs =
    let open Rresult.R.Infix in
    Modinfo.is_param_array driver vf_param >>= fun is_array ->
    (* To enable SR-IOV via modprobe configuration, we first determine if the
       driver requires in the configuration an array like `options igb
       max_vfs=7,7,7,7` or a single value like `options igb max_vfs=7`. If an
       array is required, this repeat times equals to the number of devices with
       the same driver. *)
    let repeat =
      if is_array then Sysfs.get_dev_nums_with_same_driver driver else 1
    in
    ( if repeat > 0 then
        Result.Ok
          (Array.make repeat (string_of_int maxvfs)
          |> Array.to_list
          |> String.concat ","
          )
    else
      Result.Error (Other, "Fail to generate options for maxvfs for " ^ driver)
    )
    >>= fun option ->
    let need_rebuild_initrd = ref false in
    let has_probe_conf = ref false in
    let parse_single_line s =
      let parse_driver_options s =
        match Astring.String.cut ~sep:"=" s with
        (* has SR-IOV configuration but the max_vfs is exactly what we want to
           set, so no changes and return s *)
        | Some (k, v) when k = vf_param && v = option ->
            has_probe_conf := true ;
            s
        (* has SR-IOV configuration and we need change it to expected option *)
        | Some (k, v) when k = vf_param ->
            has_probe_conf := true ;
            need_rebuild_initrd := true ;
            debug "change SR-IOV options from [%s=%s] to [%s=%s]" k v k option ;
            Printf.sprintf "%s=%s" vf_param option
        (* we do not care the lines without SR-IOV configurations *)
        | _ ->
            s
      in
      let trimed_s = String.trim s in
      if Re.execp (Re.Perl.compile_pat ("options[ \\t]+" ^ driver)) trimed_s
      then
        let driver_options =
          Re.split (Re.Perl.compile_pat "[ \\t]+") trimed_s
        in
        List.map parse_driver_options driver_options |> String.concat " "
      else
        trimed_s
    in
    let lines = try Unixext.read_lines ~path:(getpath driver) with _ -> [] in
    let new_conf = List.map parse_single_line lines in
    match (!has_probe_conf, !need_rebuild_initrd) with
    | true, true ->
        write_conf_file driver new_conf >>= fun () -> Dracut.rebuild_initrd ()
    | false, false ->
        let new_option_line =
          Printf.sprintf "options %s %s=%s" driver vf_param option
        in
        write_conf_file driver (new_conf @ [new_option_line]) >>= fun () ->
        Dracut.rebuild_initrd ()
    | true, false ->
        Result.Ok ()
        (* already have modprobe configuration and no need to change *)
    | false, true ->
        Result.Error
          (Other, "enabling SR-IOV via modprobe never comes here for: " ^ driver)
end
