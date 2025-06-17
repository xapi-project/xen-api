(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Xapi_stdext_std
open Xapi_stdext_unix
open Rrdd_plugin
open Blktap3_stats

module Process = Process (struct let name = "xcp-rrdd-iostat" end)

open Process
open Ezxenstore_core.Xenstore

(* A mapping of VDIs to the VMs they are plugged to, in which position, and the device-id *)
let vdi_to_vm_map : (string * (string * string * int)) list ref = ref []

let get_vdi_to_vm_map () = !vdi_to_vm_map

let vdi_to_vm_map_last_updated_counter = ref None

let update_vdi_to_vm_map () =
  let create_new_vdi_to_vm_map () =
    (* We get a VM's VDI information from xenstore, /local/domain/0/backend/vbd/<domid>/<vbdid> *)
    let base_paths =
      ["/local/domain/0/backend/vbd"; "/local/domain/0/backend/vbd3"]
    in
    try
      let _, domUs, _ = Xenctrl.with_intf Xenctrl_lib.domain_snapshot in
      D.debug "Running domUs: [%s]"
        (String.concat "; "
           (List.map
              (fun (_, uuid, domid) ->
                Printf.sprintf "%d (%s)" domid (String.sub uuid 0 8)
              )
              domUs
           )
        ) ;
      with_xs (fun xs ->
          List.map
            (fun (_, vm, domid) ->
              (* Get VBDs for this domain *)
              let enoents = ref 0 in
              let vbds =
                List.concat_map
                  (fun base_path ->
                    try
                      let path = Printf.sprintf "%s/%d" base_path domid in
                      D.debug "Getting path %s..." path ;
                      List.filter_map
                        (fun vbd ->
                          try
                            let devid = int_of_string vbd in
                            Some (Printf.sprintf "%s/%s" path vbd, devid)
                          with Failure _ ->
                            D.warn "Got non-integer vbd %s in domain %d" vbd
                              domid ;
                            None
                        )
                        (xs.Xs.directory path)
                    with Xs_protocol.Enoent _ ->
                      D.debug "Got ENOENT when listing VBDs in %s for domain %d"
                        base_path domid ;
                      incr enoents ;
                      []
                  )
                  base_paths
              in

              if !enoents = List.length base_paths then
                D.warn "Got ENOENT for each VBD backend path for domain %d"
                  domid ;

              List.filter_map
                (fun (vbd, devid) ->
                  try
                    let vdi =
                      xs.Xs.read (Printf.sprintf "%s/sm-data/vdi-uuid" vbd)
                    in
                    let device = xs.Xs.read (Printf.sprintf "%s/dev" vbd) in
                    D.debug "Found VDI %s at device %s in VM %s, device id %d"
                      vdi device vm devid ;
                    Some (vdi, (vm, device, devid))
                  with Xs_protocol.Enoent _ ->
                    (* CA-111132: an empty VBD (i.e. no ISO inserted) has no sm-data/vdi-uuid *)
                    D.debug
                      "Got ENOENT when reading info for vbd %s in domain %d \
                       (might be empty)"
                      vbd domid ;
                    None
                )
                vbds
            )
            domUs
          |> List.concat
      )
    with e ->
      D.error "Error while constructing VDI-to-VM map: %s" (Printexc.to_string e) ;
      []
  in
  vdi_to_vm_map_last_updated_counter := Some (Mtime_clock.counter ()) ;
  vdi_to_vm_map := create_new_vdi_to_vm_map ()

let remove_vdi_from_map vdi =
  vdi_to_vm_map := List.filter (fun (vdi', _) -> vdi <> vdi') !vdi_to_vm_map

module Iostat = struct
  (* Device:         rrqm/s   wrqm/s   r/s   w/s    rMB/s    wMB/s avgrq-sz avgqu-sz   await  svctm  %util *)
  type t = float list

  let get_unsafe (devs : string list) : (string * t) list =
    (* A map from device names to the parsed values *)
    let dev_values_map : (string, t) Hashtbl.t = Hashtbl.create 20 in

    (* Keep track of how many results headers we've seen so far *)
    let parsing_section = ref 0 in
    let read_out_line str =
      let res = Utils.cut str in
      (* Keep values from the second set of outputs *)
      ( if !parsing_section = 2 then
          match res with
          | dev :: vals -> (
            try
              Hashtbl.replace dev_values_map dev (List.map float_of_string vals)
            with _ -> (* ignore unparseable lines *) ()
          )
          | _ ->
              ()
      ) ;
      (* See if we have reached the header for a new section *)
      (try if List.hd res = "Device:" then incr parsing_section with _ -> ()) ;
      None
    in

    (* Make a list of the names of the devices we are interested in *)
    let dev_str =
      String.concat " " (List.map (fun dev -> Printf.sprintf "-d %s" dev) devs)
    in
    let cmdstring = Printf.sprintf "/usr/bin/iostat -x %s 1 2" dev_str in

    (* 2 iterations; 1 second between them *)

    (* Iterate through each line and populate dev_values_map *)
    let read_err_line _ = None in
    let _ =
      Utils.exec_cmd (module Process.D) ~cmdstring ~read_out_line ~read_err_line
    in

    (* Now read the values out of dev_values_map for devices for which we have data *)
    List.filter_map
      (fun dev ->
        Option.map
          (fun values -> (dev, values))
          (Hashtbl.find_opt dev_values_map dev)
      )
      devs
end

module Stat = struct
  (* Name            units         description									*)
  (* ----            -----         -----------									*)
  (* read I/Os       requests      number of read I/Os processed					*)
  (* read merges     requests      number of read I/Os merged with in-queue I/O	*)
  (* read sectors    sectors       number of sectors read							*)
  (* read ticks      milliseconds  total wait time for read requests				*)
  (* write I/Os      requests      number of write I/Os processed					*)
  (* write merges    requests      number of write I/Os merged with in-queue I/O	*)
  (* write sectors   sectors       number of sectors written						*)
  (* write ticks     milliseconds  total wait time for write requests				*)
  (* in_flight       requests      number of I/Os currently in flight				*)
  (* io_ticks        milliseconds  total time this block device has been active	*)
  (* time_in_queue   milliseconds  total wait time for all requests				*)

  (* In newer kernels only, ignored *)
  (* discard I/Os    requests      number of discard I/Os processed                             *)
  (* discard merges  requests      number of discard I/Os merged with in-queue I/O              *)
  (* discard sectors sectors       number of sectors discarded                                  *)
  (* discard ticks   milliseconds  total wait time for discard requests                         *)

  (* --- Added for our needs --- *)

  (* inflight_read   requests      read requests in flight                        *)
  (* inflight_write  requests      write requests in flight                       *)
  (* read bytes      bytes         = read sectors * hw_sector_size                *)
  (* write bytes     bytes         = write sectors * hw_sector_size               *)

  type t = int64 list
  (* /sys/block/tdX/stats @ /sys/block/tdX/inflight @ [read bytes; write bytes] *)

  let finally = Xapi_stdext_pervasives.Pervasiveext.finally

  (** open [path] to let [f] scan the contents using scanf *)
  let scanning path f =
    let io = Scanf.Scanning.open_in path in
    finally (fun () -> f io) (fun () -> Scanf.Scanning.close_in io)

  let sector_size dev =
    let path = String.concat "/" ["/sys/block"; dev; "queue/hw_sector_size"] in
    try scanning path @@ fun io -> Scanf.bscanf io " %Ld" @@ Fun.id
    with e ->
      D.error "%s: %s" __FUNCTION__ (Printexc.to_string e) ;
      0L

  (** read first 11 values from device stat *)
  let stat dev =
    let path = String.concat "/" ["/sys/block"; dev; "stat"] in
    try
      scanning path @@ fun io ->
      Scanf.bscanf io " %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld"
      @@ fun d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 ->
      [d1; d2; d3; d4; d5; d6; d7; d8; d9; d10; d11]
    with e ->
      D.error "%s: %s" __FUNCTION__ (Printexc.to_string e) ;
      [0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L]

  let inflight dev =
    let path = String.concat "/" ["/sys/block"; dev; "inflight"] in
    try
      scanning path @@ fun io ->
      Scanf.bscanf io " %Ld %Ld" @@ fun d1 d2 -> [d1; d2]
    with e ->
      D.error "%s: %s" __FUNCTION__ (Printexc.to_string e) ;
      [0L; 0L]

  let get_unsafe_dev (dev : string) : t =
    let hw_sector_size = sector_size dev in
    let stats = stat dev in
    let inflight_stats = inflight dev in
    let sectors_to_bytes = Int64.mul hw_sector_size in
    let read_bytes = sectors_to_bytes (List.nth stats 2) in
    let write_bytes = sectors_to_bytes (List.nth stats 6) in
    let res = List.concat [stats; inflight_stats; [read_bytes; write_bytes]] in
    assert (List.length res = 15) ;
    res

  let get_unsafe (devs : string list) : (string * t) list =
    List.map
      (fun dev ->
        let values = get_unsafe_dev dev in
        (dev, values)
      )
      devs
end

(* List of tapdisk pids seen on previous run of tap-ctl list *)
let previous_map : (int * (int * (string * string))) list ref = ref []

let phypath_to_sr_vdi : (string, string * string) Hashtbl.t = Hashtbl.create 20

let refresh_phypath_to_sr_vdi () =
  let iter_sr_dirs path f =
    try
      Sys.readdir path
      |> Array.iter (fun sruuid ->
             let sr_dir = Printf.sprintf "%s/%s" path sruuid in
             Sys.readdir sr_dir
             |> Array.iter (fun vdi_entry ->
                    (* in /dev/sm/phy vdi_entry __should__ be a vdiuuid
                       in /var/run/sr-mount vdi_entry might look like:
                         - "$vdiuuid.vhd",
                         - "$vdiuuid.vhdcache",
                         -  ".*" *)
                    f sruuid vdi_entry
                )
         )
    with e ->
      D.debug "refresh_phypath_to_sr_vdi: failed searching %s. error: %s" path
        (Printexc.to_string e)
  in

  Hashtbl.clear phypath_to_sr_vdi ;

  let module StringSet = Set.Make (String) in
  let extra_paths_to_search = ref StringSet.empty in

  (* search /dev/sm/phy for sr+vdis. these vdi files will be symbolic
     links (when testing they appear to be found in /var/run/sr-mount).
     we:
       1) add to the sr+vdi map the files that are symbolically linked to
       2) make a note of where we are, so that we can search for vhdcache files *)
  iter_sr_dirs "/dev/sm/phy" (fun sruuid vdiuuid ->
      let vdi_file = Printf.sprintf "/dev/sm/phy/%s/%s" sruuid vdiuuid in
      let phy_link = Unix.readlink vdi_file in

      (* add vdi listed in /dev/sm/phy (the symbolic link) *)
      Hashtbl.replace phypath_to_sr_vdi phy_link (sruuid, vdiuuid) ;

      (* make note of dirs to search for vhdcache files (should only appear when intellicache enabled) *)
      match Stringext.split phy_link ~on:'/' with
      | [] | [_] ->
          ()
      | path_segments ->
          let new_path_to_search =
            Listext.List.take (List.length path_segments - 2) path_segments
            |> String.concat "/"
          in
          extra_paths_to_search :=
            StringSet.add new_path_to_search !extra_paths_to_search
  ) ;

  (* add any vhdcache files *)
  !extra_paths_to_search
  |> StringSet.iter (fun path ->
         iter_sr_dirs path (fun sruuid vdi_entry ->
             let vdiuuid =
               try Some (Scanf.sscanf vdi_entry "%s@.vhdcache" Fun.id)
               with _ -> None
             in
             match vdiuuid with
             | None ->
                 ()
             | Some vdiuuid ->
                 Hashtbl.replace phypath_to_sr_vdi
                   (Printf.sprintf "%s/%s/%s" path sruuid vdi_entry)
                   (sruuid, vdiuuid)
         )
     )

(* Get a list of currently-active tapdisk processes as an assoc list from SR/VDI tuple
   to minor number. As a side-effect, this updates the VDI-to-VM in the process state,
   if needed. *)
let exec_tap_ctl_list () : ((string * string) * int) list =
  let tap_ctl = "/usr/sbin/tap-ctl list" in
  let extract_vdis pid minor _state kind phypath =
    if not (kind = "vhd" || kind = "aio" || kind = "qcow2") then (
      D.warn {|"%s" is not a known type.|} kind ;
      raise (Failure "Unknown type")
    ) ;
    (* Look up SR and VDI uuids from the physical path *)
    if not (Hashtbl.mem phypath_to_sr_vdi phypath) then
      refresh_phypath_to_sr_vdi () ;
    match Hashtbl.find_opt phypath_to_sr_vdi phypath with
    | Some (sr, vdi) ->
        Some (pid, (minor, (sr, vdi)))
    | None ->
        (* Odd: tap-ctl mentions a device that's not linked from /dev/sm/phy *)
        D.error "Could not find device with physical path %s" phypath ;
        None
  in
  let read_out_line str =
    try Scanf.sscanf str "pid=%d minor=%d state=%s args=%s@:%s" extract_vdis
    with Scanf.Scan_failure _ | Failure _ | End_of_file ->
      D.warn {|"%s" returned a line that could not be parsed. Ignoring.|}
        tap_ctl ;
      D.warn "Offending line: %s" str ;
      None
  in
  let read_err_line _ = None in
  let pid_and_minor_to_sr_and_vdi, _ =
    Utils.exec_cmd
      (module Process.D)
      ~cmdstring:tap_ctl ~read_out_line ~read_err_line
  in
  let sr_and_vdi_to_minor =
    List.map
      (fun (_, (minor, sr_and_vdi)) -> (sr_and_vdi, minor))
      pid_and_minor_to_sr_and_vdi
  in
  (let reason_for_updating_vdi_to_vm_map =
     let pid_vdis_to_string pids =
       pids
       |> List.map (fun (pid, (_, (_, vdi))) -> Printf.sprintf "%d (%s)" pid vdi)
       |> String.concat "; "
     in
     let unmapped_vdi_pids =
       List.filter
         (fun (_, (_, (_, vdi))) -> not (List.mem_assoc vdi !vdi_to_vm_map))
         pid_and_minor_to_sr_and_vdi
     in
     let newly_discovered =
       List.filter
         (fun (_, (_, (_, new_vdi))) ->
           List.for_all
             (fun (_, (_, (_, prev_vdi))) -> new_vdi <> prev_vdi)
             !previous_map
         )
         unmapped_vdi_pids
     in

     (* note that newly_discovered is a subset of unmapped_vdi_pids *)
     if newly_discovered <> [] then
       Some
         (Printf.sprintf "discovered new VDI(s): [%s]"
            (pid_vdis_to_string newly_discovered)
         )
     else if unmapped_vdi_pids <> [] then
       match !vdi_to_vm_map_last_updated_counter with
       | None ->
           Some "performing map initialization"
       | Some c ->
           let span_since_last_update = Mtime_clock.count c in
           let five_minutes = Mtime.Span.(5 * min) in
           if Mtime.Span.compare span_since_last_update five_minutes > 0 then
             Some "map was last updated over 5 minutes ago"
           else
             None
     else
       None
   in

   let disappeared_pids =
     Listext.List.set_difference !previous_map pid_and_minor_to_sr_and_vdi
   in
   List.iter
     (fun (pid, (_, (_, vdi))) ->
       try
         D.info
           "tapdisk process %d has disappeared; removing entries for %s in \
            VDI-to-VM map"
           pid vdi ;
         remove_vdi_from_map vdi
       with Not_found -> D.debug "no knowledge about pid %d; ignoring" pid
     )
     disappeared_pids ;

   match reason_for_updating_vdi_to_vm_map with
   | None ->
       ()
   | Some reason ->
       D.debug "Updating VDI-to-VM map because %s" reason ;
       update_vdi_to_vm_map ()
  ) ;
  previous_map := pid_and_minor_to_sr_and_vdi ;
  sr_and_vdi_to_minor

(* Get the minor number of the kernel tapdev device *)
let minor_of_tapdev_unsafe tapdev =
  int_of_string
    (Unixext.file_lines_fold
       (fun acc l -> acc ^ List.nth (Xstringext.String.split ':' l) 1)
       ""
       ("/sys/block/" ^ tapdev ^ "/dev")
    )

(* Get a list of current tapdev devices from the kernel *)
let get_tapdevs () =
  List.fold_left
    (fun acc entry ->
      if entry.[0] = 't' && entry.[1] = 'd' then entry :: acc else acc
    )
    []
    (Utils.list_directory_unsafe "/sys/block")

(* Get an assoc list from tapdisk minor number to stats value given a list of devices
   and a stat function `f`. *)
let get_minor_to_stats_fun ~f tapdevs : (int * 'a) list =
  tapdevs
  |> f
  |> List.map (fun (tapdev, stat) -> (minor_of_tapdev_unsafe tapdev, stat))

let get_minor_to_stats = get_minor_to_stats_fun ~f:Stat.get_unsafe

let get_minor_to_iostats = get_minor_to_stats_fun ~f:Iostat.get_unsafe

let sr_to_sth s_v_to_i =
  let fold_fun acc ((s, _v), sth) =
    try
      let cur = List.assoc s acc in
      Listext.List.replace_assoc s (sth :: cur) acc
    with Not_found -> (s, [sth]) :: acc
  in
  List.fold_left fold_fun [] s_v_to_i

module Blktap3_stats_wrapper = struct
  let shm_devices_dir = "/dev/shm"

  let m = Mutex.create ()

  let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

  let cache : (int * int, Blktap3_stats.t option) Hashtbl.t = Hashtbl.create 100

  let get_stats () =
    let pid_from_xs (domid, devid) =
      try
        let result =
          with_xs (fun xs ->
              let path =
                Printf.sprintf "/local/domain/0/backend/vbd3/%d/%d/kthread-pid"
                  domid devid
              in
              Some (int_of_string (xs.Xs.read path))
          )
        in
        result
      with _ -> None
      (* Most likely the pid hasn't been written yet *)
    in

    with_lock m (fun () ->
        Hashtbl.fold
          (fun (domid, devid) stats_opt acc ->
            match stats_opt with
            | Some stats ->
                ((domid, devid), Blktap3_stats.copy stats) :: acc
            | None -> (
              match pid_from_xs (domid, devid) with
              | Some pid -> (
                  let stat_file =
                    Printf.sprintf "%s/td3-%d/vbd-%d-%d" shm_devices_dir pid
                      domid devid
                  in
                  try
                    let stats = Blktap3_stats.of_file stat_file in
                    Hashtbl.replace cache (domid, devid) (Some stats) ;
                    ((domid, devid), Blktap3_stats.copy stats) :: acc
                  with e ->
                    D.warn "Caught exception trying to map stats file: %s"
                      (Printexc.to_string e) ;
                    acc
                )
              | None ->
                  acc
            )
          )
          cache []
    )

  let inotify_thread () =
    let domdev_of_file f =
      try Scanf.sscanf f "vbd3-%d-%d" (fun domid devid -> Some (domid, devid))
      with _ -> None
    in

    let operate f fn =
      match domdev_of_file f with
      | Some (domid, devid) ->
          with_lock m (fun () -> fn (domid, devid))
      | None ->
          ()
    in

    let add_file f = operate f (fun k -> Hashtbl.replace cache k None) in
    let remove_file f = operate f (fun k -> Hashtbl.remove cache k) in

    let initialise () =
      with_lock m (fun () -> Hashtbl.clear cache) ;
      let shm_dirs = Array.to_list (Sys.readdir shm_devices_dir) in
      List.iter add_file shm_dirs ;
      D.debug "Populated %d cache entries"
        (with_lock m (fun () -> Hashtbl.length cache))
    in

    let inotify = Inotify.create () in
    let _ =
      Inotify.add_watch inotify "/dev/shm/" Inotify.[S_Create; S_Delete]
    in
    initialise () ;

    let rec loop () =
      let result =
        try
          let evs = Inotify.read inotify in
          List.iter
            (fun (_w, kind_list, _, path) ->
              let is_create = List.mem Inotify.Create kind_list in
              let is_delete = List.mem Inotify.Delete kind_list in
              match (is_create, is_delete, path) with
              | _, true, Some p ->
                  remove_file p
              | true, false, Some p ->
                  add_file p
              | _, _, _ ->
                  ()
            )
            evs ;
          Ok ()
        with e ->
          D.debug "Unexpected other exception: %s" (Printexc.to_string e) ;
          Error ()
      in
      match result with Ok () -> loop () | Error () -> initialise () ; loop ()
    in
    loop ()

  let get_domid_devid_to_stats_blktap3 () : ((int * int) * Blktap3_stats.t) list
      =
    let stats = get_stats () in
    stats
end

module Stats_value = struct
  type t = {
      rd_bytes: int64
    ; wr_bytes: int64
    ; rd_avg_usecs: int64
    ; wr_avg_usecs: int64
    ; io_throughput_read_mb: float
    ; io_throughput_write_mb: float
    ; iops_read: int64
    ; iops_write: int64
    ; iowait: float
    ; inflight: int64
  }

  let empty =
    {
      rd_bytes= 0L
    ; wr_bytes= 0L
    ; rd_avg_usecs= 0L
    ; wr_avg_usecs= 0L
    ; io_throughput_read_mb= 0.
    ; io_throughput_write_mb= 0.
    ; iops_read= 0L
    ; iops_write= 0L
    ; iowait= 0.
    ; inflight= 0L
    }

  let make stats last_stats stats_blktap3 last_stats_blktap3 : t =
    let ( ++ ) = Int64.add
    and ( -- ) = Int64.sub
    and to_float = Int64.to_float in
    (* stats_blktap3 and stats won't both present at a time *)
    match (stats_blktap3, stats) with
    | None, Some stats ->
        let stats_get = List.nth stats in
        let stats_diff_get n =
          let stat = stats_get n in
          let last_stat =
            match last_stats with None -> 0L | Some s -> List.nth s n
          in
          if stat >= last_stat then Int64.sub stat last_stat else stat
        in
        {
          rd_bytes= stats_diff_get 13
        ; wr_bytes= stats_diff_get 14
        ; rd_avg_usecs=
            ( if stats_diff_get 0 > 0L then
                Int64.div (stats_diff_get 3) (stats_diff_get 0)
              else
                0L
            )
        ; wr_avg_usecs=
            ( if stats_diff_get 4 > 0L then
                Int64.div (stats_diff_get 7) (stats_diff_get 4)
              else
                0L
            )
        ; io_throughput_read_mb= to_float (stats_diff_get 13) /. 1048576.
        ; io_throughput_write_mb= to_float (stats_diff_get 14) /. 1048576.
        ; iops_read= stats_diff_get 0
        ; iops_write= stats_diff_get 4
        ; iowait= to_float (stats_diff_get 10) /. 1000.
        ; inflight= stats_get 8
        }
    | Some s3, _ ->
        let last_s3 = last_stats_blktap3 in
        let opt f = Option.fold ~none:0L ~some:f in
        let open Blktap3_stats in
        let avg_reqs_completed_last_five_secs get_reqs_completed get_total_ticks
            =
          last_s3
          |> Option.fold
               ~none:0L (* we don't have data from 5 secs ago, so give up *)
               ~some:(fun last_s3 ->
                 (* a 'tick' is the time spent waiting on reads (or writes, dep on context), in usec *)
                 let num_reqs_in_last_five_secs =
                   let now = get_reqs_completed s3 in
                   let last = get_reqs_completed last_s3 in
                   if now < last then (
                     D.error
                       "Stats_value.make: read_reqs_completed has decreased: \
                        now: %Li, last: %Li"
                       now last ;
                     0L
                   ) else
                     now -- last
                 in
                 let num_ticks_in_last_5_secs =
                   get_total_ticks s3 -- get_total_ticks last_s3
                 in
                 if num_reqs_in_last_five_secs = 0L then
                   0L
                 else
                   Int64.div num_ticks_in_last_5_secs num_reqs_in_last_five_secs
             )
        in
        {
          rd_bytes= Int64.mul (get_stats_read_sectors s3) 512L
        ; wr_bytes= Int64.mul (get_stats_write_sectors s3) 512L
        ; rd_avg_usecs=
            avg_reqs_completed_last_five_secs get_stats_read_reqs_completed
              get_stats_read_total_ticks
        ; wr_avg_usecs=
            avg_reqs_completed_last_five_secs get_stats_write_reqs_completed
              get_stats_write_total_ticks
        ; io_throughput_read_mb=
            to_float
              (get_stats_read_sectors s3 -- opt get_stats_read_sectors last_s3)
            *. 512.
            /. 1048576.
        ; io_throughput_write_mb=
            to_float
              (get_stats_write_sectors s3 -- opt get_stats_write_sectors last_s3)
            *. 512.
            /. 1048576.
        ; iops_read=
            get_stats_read_reqs_completed s3
            -- opt get_stats_read_reqs_completed last_s3
        ; iops_write=
            get_stats_write_reqs_completed s3
            -- opt get_stats_write_reqs_completed last_s3
        ; iowait=
            to_float
              (get_stats_read_total_ticks s3
              ++ get_stats_write_total_ticks s3
              -- (opt get_stats_read_total_ticks last_s3
                 ++ opt get_stats_write_total_ticks last_s3
                 )
              )
            /. 1000000.0
        ; inflight=
            get_stats_read_reqs_submitted s3
            ++ get_stats_write_reqs_submitted s3
            -- (opt get_stats_read_reqs_completed last_s3
               ++ opt get_stats_write_reqs_completed last_s3
               )
        }
    | None, None ->
        D.warn "No stats found" ; empty

  let accumulate (values : t list) : t =
    let ( ++ ) = Int64.add in
    List.fold_left
      (fun acc v ->
        {
          rd_bytes= acc.rd_bytes ++ v.rd_bytes
        ; rd_avg_usecs= acc.rd_avg_usecs ++ v.rd_avg_usecs
        ; wr_bytes= acc.wr_bytes ++ v.wr_bytes
        ; wr_avg_usecs= acc.wr_avg_usecs ++ v.wr_avg_usecs
        ; io_throughput_read_mb=
            acc.io_throughput_read_mb +. v.io_throughput_read_mb
        ; io_throughput_write_mb=
            acc.io_throughput_write_mb +. v.io_throughput_write_mb
        ; iops_read= acc.iops_read ++ v.iops_read
        ; iops_write= acc.iops_write ++ v.iops_write
        ; iowait= acc.iowait +. v.iowait
        ; inflight= acc.inflight ++ v.inflight
        }
      )
      empty values

  let make_ds ~owner ~name ~key_format (value : t) =
    let ds_make = Ds.ds_make ~default:true in
    [
      ( owner
      , ds_make ~name:(key_format "read")
          ~description:("Reads from device " ^ name ^ ", in B/s")
          ~value:(Rrd.VT_Int64 value.rd_bytes) ~ty:Rrd.Derive ~units:"B/s"
          ~min:0.0 ()
      )
    ; ( owner
      , ds_make ~name:(key_format "write")
          ~description:("Writes from device " ^ name ^ ", in B/s")
          ~value:(Rrd.VT_Int64 value.wr_bytes) ~ty:Rrd.Derive ~units:"B/s"
          ~min:0.0 ()
      )
    ; ( owner
      , ds_make
          ~name:(key_format "read_latency")
          ~description:("Read latency from device " ^ name ^ ", in microseconds")
          ~value:(Rrd.VT_Int64 value.rd_avg_usecs) ~ty:Rrd.Gauge ~units:"μs"
          ~min:0.0 ()
      )
    ; ( owner
      , ds_make
          ~name:(key_format "write_latency")
          ~description:
            ("Write latency from device " ^ name ^ ", in microseconds")
          ~value:(Rrd.VT_Int64 value.wr_avg_usecs) ~ty:Rrd.Gauge ~units:"μs"
          ~min:0.0 ()
      )
    ; ( owner
      , ds_make
          ~name:(key_format "io_throughput_read")
          ~description:("Data read from the " ^ name ^ ", in MiB/s")
          ~value:(Rrd.VT_Float value.io_throughput_read_mb) ~ty:Rrd.Absolute
          ~units:"MiB/s" ~min:0. ()
      )
    ; ( owner
      , ds_make
          ~name:(key_format "io_throughput_write")
          ~description:("Data written to the " ^ name ^ ", in MiB/s")
          ~value:(Rrd.VT_Float value.io_throughput_write_mb) ~ty:Rrd.Absolute
          ~units:"MiB/s" ~min:0. ()
      )
    ; ( owner
      , ds_make
          ~name:(key_format "io_throughput_total")
          ~description:("All " ^ name ^ " I/O, in MiB/s")
          ~value:
            (Rrd.VT_Float
               (value.io_throughput_read_mb +. value.io_throughput_write_mb)
            )
          ~ty:Rrd.Absolute ~units:"MiB/s" ~min:0. ()
      )
    ; ( owner
      , ds_make ~name:(key_format "iops_read")
          ~description:"Read requests per second"
          ~value:(Rrd.VT_Int64 value.iops_read) ~ty:Rrd.Absolute
          ~units:"requests/s" ~min:0. ()
      )
    ; ( owner
      , ds_make ~name:(key_format "iops_write")
          ~description:"Write requests per second"
          ~value:(Rrd.VT_Int64 value.iops_write) ~ty:Rrd.Absolute
          ~units:"requests/s" ~min:0. ()
      )
    ; ( owner
      , ds_make ~name:(key_format "iops_total")
          ~description:"I/O Requests per second"
          ~value:(Rrd.VT_Int64 (Int64.add value.iops_read value.iops_write))
          ~ty:Rrd.Absolute ~units:"requests/s" ~min:0. ()
      )
    ; ( owner
      , ds_make ~name:(key_format "iowait")
          ~description:"Total I/O wait time (all requests) per second"
          ~value:(Rrd.VT_Float value.iowait) ~ty:Rrd.Absolute ~units:"s/s"
          ~min:0. ()
      )
    ; ( owner
      , ds_make ~name:(key_format "inflight")
          ~description:"Number of I/O requests currently in flight"
          ~value:(Rrd.VT_Int64 value.inflight) ~ty:Rrd.Gauge ~units:"requests"
          ~min:0. ()
      )
    ]
end

module Iostats_value = struct
  type t = {latency: float; avgqu_sz: float}

  let empty = {latency= 0.; avgqu_sz= 0.}

  let make iostats _last_iostats stats_blktap3 last_stats_blktap3 : t =
    let ( ++ ) = Int64.add
    and ( -- ) = Int64.sub
    and to_float = Int64.to_float in
    (* stats_blktap3 and stats won't both present at a time *)
    match (stats_blktap3, iostats) with
    | None, Some iostats ->
        let iostats_get = List.nth iostats in
        {latency= iostats_get 9; avgqu_sz= iostats_get 7}
    | Some s3, _ ->
        let last_s3 = last_stats_blktap3 in
        let opt f x = match x with None -> 0L | Some x' -> f x' in
        let s3_usecs =
          get_stats_read_total_ticks s3
          ++ get_stats_write_total_ticks s3
          -- (opt get_stats_read_total_ticks last_s3
             ++ opt get_stats_write_total_ticks last_s3
             )
        in
        let s3_count =
          get_stats_read_reqs_completed s3
          ++ get_stats_write_reqs_completed s3
          -- (opt get_stats_read_reqs_completed last_s3
             ++ opt get_stats_write_reqs_completed last_s3
             )
        in
        let s3_latency_average =
          if s3_count = 0L then 0. else to_float s3_usecs /. to_float s3_count
        in
        (* refer to https://github.com/xenserver/xsiostat for the calculation below *)
        let avgqu_sz =
          to_float
            (get_stats_read_total_ticks s3
            ++ get_stats_write_total_ticks s3
            -- (opt get_stats_read_total_ticks last_s3
               ++ opt get_stats_write_total_ticks last_s3
               )
            )
          /. 1000_000.0
        in
        {
          latency= s3_latency_average
        ; (* divide by the interval as the ds-type is Gauge *)
          avgqu_sz= avgqu_sz /. 5.
        }
    | None, None ->
        D.warn "No iostats found" ; empty

  let accumulate (values : t list) : t =
    let max acc v =
      {
        latency= Float.max acc.latency v.latency
      ; avgqu_sz= Float.max acc.avgqu_sz v.avgqu_sz
      }
    in
    List.fold_left max empty values

  let[@warning "-27"] make_ds ~owner ~name ~key_format (value : t) =
    let ds_make = Ds.ds_make ~default:true in
    [
      ( owner
      , ds_make ~name:(key_format "latency") ~description:"Average I/O latency"
          ~value:(Rrd.VT_Float value.latency) ~ty:Rrd.Gauge ~units:"μs" ~min:0.
          ()
      )
    ; ( owner
      , ds_make ~name:(key_format "avgqu_sz")
          ~description:"Average I/O queue size"
          ~value:(Rrd.VT_Float value.avgqu_sz) ~ty:Rrd.Gauge ~units:"requests"
          ~min:0. ()
      )
    ]
end

let list_all_assocs key xs =
  List.map snd (List.filter (fun (k, _) -> k = key) xs)

let sr_vdi_to_last_iostats_values = ref None

let sr_vdi_to_last_stats_values = ref None

let domid_devid_to_last_stats_blktap3 = ref None

let gen_metrics () =
  let domid_devid_to_stats_blktap3 =
    Blktap3_stats_wrapper.get_domid_devid_to_stats_blktap3 ()
  in

  let sr_and_vdi_to_minor = exec_tap_ctl_list () in
  let tapdevs = get_tapdevs () in

  let map_sr_and_vdi_to_stats minor_to_tapdev_stat =
    List.map
      (fun (sr_and_vdi, minor) ->
        let data = List.assoc_opt minor minor_to_tapdev_stat in
        (sr_and_vdi, data)
      )
      sr_and_vdi_to_minor
  in

  (* Construct a mapping from SR+VDI to kernel stats/iostats. The kernel
     devices and therefore stats may not exist, so an option type is used. *)
  (* Get iostat data first, because this takes 1 second to complete *)
  let sr_vdi_to_iostats : ((string * string) * Iostat.t option) list =
    tapdevs |> get_minor_to_iostats |> map_sr_and_vdi_to_stats
  in
  let sr_vdi_to_stats : ((string * string) * Stat.t option) list =
    tapdevs |> get_minor_to_stats |> map_sr_and_vdi_to_stats
  in

  (* relations between dom-id, vm-uuid, device pos, dev-id, etc *)
  let _, domUs, _ = Xenctrl.with_intf Xenctrl_lib.domain_snapshot in
  let vdi_to_vm = get_vdi_to_vm_map () in

  let get_stats_blktap3_by_vdi vdi =
    if List.mem_assoc vdi vdi_to_vm then
      let vm_uuid, _pos, devid = List.assoc vdi vdi_to_vm in
      match List.filter (fun (_, vm_uuid', _) -> vm_uuid' = vm_uuid) domUs with
      | [] ->
          (None, None)
      | (_, _, domid) :: _ ->
          let find_blktap3 blktap3_assoc_list =
            let key = (domid, devid) in
            if List.mem_assoc key blktap3_assoc_list then
              Some (List.assoc key blktap3_assoc_list)
            else
              None
          in
          let stats_blktap3 = find_blktap3 domid_devid_to_stats_blktap3 in
          let last_stats_blktap3 =
            match !domid_devid_to_last_stats_blktap3 with
            | None ->
                None
            | Some last ->
                find_blktap3 last
          in
          (stats_blktap3, last_stats_blktap3)
    else
      (None, None)
  in

  (* Convert raw iostats/stats (and blktap3 stats) list to structured record *)
  let sr_vdi_to_iostats_values =
    List.map
      (fun (((_sr, vdi) as sr_vdi), iostats) ->
        let last_iostats =
          match !sr_vdi_to_last_iostats_values with
          | None ->
              None
          | Some s ->
              Hashtbl.find_opt s sr_vdi |> Option.join
        in
        let stats_blktap3, last_stats_blktap3 = get_stats_blktap3_by_vdi vdi in
        ( sr_vdi
        , Iostats_value.make iostats last_iostats stats_blktap3
            last_stats_blktap3
        )
      )
      sr_vdi_to_iostats
  in
  let sr_vdi_to_stats_values =
    List.map
      (fun (((_sr, vdi) as sr_vdi), stats) ->
        let last_stats =
          match !sr_vdi_to_last_stats_values with
          | None ->
              None
          | Some s ->
              Hashtbl.find_opt s sr_vdi |> Option.join
        in
        let stats_blktap3, last_stats_blktap3 = get_stats_blktap3_by_vdi vdi in
        ( sr_vdi
        , Stats_value.make stats last_stats stats_blktap3 last_stats_blktap3
        )
      )
      sr_vdi_to_stats
  in

  (* sum up to SR level stats values *)
  let get_sr_to_stats_values ~stats_values ~accumulate =
    let sr_to_stats_values = sr_to_sth stats_values in
    List.map
      (fun (sr, stats_values) -> (sr, accumulate stats_values))
      sr_to_stats_values
  in

  let sr_to_iostats_values =
    get_sr_to_stats_values ~stats_values:sr_vdi_to_iostats_values
      ~accumulate:Iostats_value.accumulate
  in
  let sr_to_stats_values =
    get_sr_to_stats_values ~stats_values:sr_vdi_to_stats_values
      ~accumulate:Stats_value.accumulate
  in

  (* create SR level data sources *)
  let data_sources_iostats =
    List.map
      (fun (sr, iostats_value) ->
        let key_format key = Printf.sprintf "%s_%s" key (String.sub sr 0 8) in
        Iostats_value.make_ds ~owner:Rrd.Host ~name:"SR" ~key_format
          iostats_value
      )
      sr_to_iostats_values
  in
  let data_sources_stats =
    List.map
      (fun (sr, stats_value) ->
        let key_format key = Printf.sprintf "%s_%s" key (String.sub sr 0 8) in
        Stats_value.make_ds ~owner:Rrd.Host ~name:"SR" ~key_format stats_value
      )
      sr_to_stats_values
  in

  (* Get the blktap3 stats and iterate the stats list to count the
     		number of tapdisks in low memory mode *)
  let data_sources_low_mem_mode =
    let ( ++ ) = Int64.add in
    let count =
      List.fold_left
        (fun acc ((_, _), stats) ->
          if
            Int64.logand (get_stats_flags stats) Blktap3_stats.flag_low_mem_mode
            = Blktap3_stats.flag_low_mem_mode
          then
            acc ++ 1L
          else
            acc
        )
        0L domid_devid_to_stats_blktap3
    in
    let ds_make = Ds.ds_make ~default:true in
    [
      [
        ( Rrd.Host
        , ds_make ~name:"Tapdisks_in_low_memory_mode"
            ~description:"Number of tapdisks in low memory mode"
            ~value:(Rrd.VT_Int64 count) ~ty:Rrd.Absolute ~units:"count" ~min:0.
            ()
        )
      ]
    ]
  in
  (* Lookup the VM(s) for this VDI and associate with the RRD for those VM(s) *)
  let data_sources_vm_iostats =
    List.concat_map
      (fun ((_sr, vdi), iostats_value) ->
        let create_metrics (vm, pos, _devid) =
          let key_format key = Printf.sprintf "vbd_%s_%s" pos key in
          Iostats_value.make_ds ~owner:(Rrd.VM vm) ~name:"VDI" ~key_format
            iostats_value
        in
        let vms = list_all_assocs vdi vdi_to_vm in
        List.map create_metrics vms
      )
      sr_vdi_to_iostats_values
  in
  let data_sources_vm_stats =
    List.concat_map
      (fun ((_sr, vdi), stats_value) ->
        let create_metrics (vm, pos, _devid) =
          let key_format key = Printf.sprintf "vbd_%s_%s" pos key in
          Stats_value.make_ds ~owner:(Rrd.VM vm) ~name:"VDI" ~key_format
            stats_value
        in
        let vms = list_all_assocs vdi vdi_to_vm in
        List.map create_metrics vms
      )
      sr_vdi_to_stats_values
  in

  (* convert recent stats data to hashtbl for next iterator use *)
  let to_hashtbl sr_vdi_to_stats =
    let hashtbl = Hashtbl.create 20 in
    List.iter
      (fun (sr_vdi, stats) -> Hashtbl.add hashtbl sr_vdi stats)
      sr_vdi_to_stats ;
    hashtbl
  in
  sr_vdi_to_last_iostats_values := Some (to_hashtbl sr_vdi_to_iostats) ;
  sr_vdi_to_last_stats_values := Some (to_hashtbl sr_vdi_to_stats) ;
  domid_devid_to_last_stats_blktap3 := Some domid_devid_to_stats_blktap3 ;

  List.concat
    (data_sources_stats
    @ data_sources_iostats
    @ data_sources_vm_stats
    @ data_sources_vm_iostats
    @ data_sources_low_mem_mode
    )

let _ =
  initialise () ;
  let _ = Thread.create Blktap3_stats_wrapper.inotify_thread () in

  (* Approx. one page per VBD, up to the limit. *)
  let shared_page_count = 2048 in
  (* It takes (at least) 1 second to get the iostat data, so start reading the data early enough *)
  main_loop ~neg_shift:1.5 ~target:(Reporter.Local shared_page_count)
    ~protocol:Rrd_interface.V2 ~dss_f:gen_metrics
