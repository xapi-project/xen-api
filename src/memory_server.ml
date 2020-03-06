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
(**
 * @group Memory
*)
open Xcp_service

open Memory_interface
open Squeezed_state
open Squeezed_xenstore
open Xapi_stdext_monadic
open Xapi_stdext_unix

module D = Debug.Make(struct let name = Memory_interface.service_name end)
open D

type context = unit

(** The main body of squeezed is single-threaded, so we protect it with a mutex here. *)
let big_server_mutex = Xapi_stdext_threads.Threadext.Mutex.create ()

let wrap dbg f =
  try
(*
		Debug.with_thread_associated dbg
		(fun () ->
*)
    Xapi_stdext_threads.Threadext.Mutex.execute big_server_mutex f
(*
		) ()
*)
  with
  | Squeeze.Cannot_free_this_much_memory (needed, free) ->
    (* NB both needed and free have been inflated by the lowmem_emergency_pool etc *)
    let needed = Int64.sub needed Squeeze_xen.target_host_free_mem_kib
    and free = Int64.sub free Squeeze_xen.target_host_free_mem_kib in
    raise (MemoryError (Memory_interface.Cannot_free_this_much_memory (needed, free)))
  | Squeeze.Domains_refused_to_cooperate domids ->
    raise (MemoryError (Memory_interface.Domains_refused_to_cooperate(domids)))



let start_balance_thread balance_check_interval =
  let body () =
    Xenctrl.with_intf
      (fun xc ->
         while true do
           Thread.delay !balance_check_interval;
           wrap "auto-balance"
             (fun () ->
                if Squeeze_xen.is_host_memory_unbalanced ~xc
                then Squeeze_xen.balance_memory ~xc
             )
         done
      ) in
  let (_: Thread.t) = Thread.create body () in
  ()


let get_diagnostics dbg = "diagnostics not yet available"

let login dbg service_name =
  wrap dbg
    (fun () ->
       (* We assume only one instance of a named service logs in at a time and therefore can use
          		   the service name as a session_id. *)
       (* remove any existing reservations associated with this service *)
       Xenctrl.with_intf
         (fun xc ->
            try Client.immediate (get_client ()) (fun xs -> Client.rm xs (state_path _service ^ "/" ^ service_name)) with Xs_protocol.Enoent _ -> ()
         );
       service_name
    )

let reserve_memory dbg session_id kib =
  let reservation_id = Uuidm.to_string (Uuidm.create `V4) in
  if kib < 0L
  then raise (MemoryError (Invalid_memory_value kib));
  wrap dbg
    (fun () ->
       Xenctrl.with_intf
         (fun xc ->
            Squeeze_xen.free_memory ~xc kib;
            debug "reserved %Ld kib for reservation %s" kib reservation_id;
            add_reservation _service session_id reservation_id (Int64.to_string kib)
         );
       reservation_id
    )

let reserve_memory_range dbg session_id min max =
  let reservation_id = Uuidm.to_string (Uuidm.create `V4) in
  if min < 0L
  then raise (MemoryError (Invalid_memory_value min));
  if max < 0L
  then raise (MemoryError (Invalid_memory_value max));
  wrap dbg
    (fun () ->
       Xenctrl.with_intf
         (fun xc ->
            let amount = Squeeze_xen.free_memory_range ~xc min max in
            debug "reserved %Ld kib for reservation %s" amount reservation_id;
            add_reservation _service session_id reservation_id (Int64.to_string amount);
            reservation_id, amount
         )
    )


let delete_reservation dbg session_id reservation_id =
  wrap dbg
    (fun () ->
       Xenctrl.with_intf
         (fun xc ->
            del_reservation _service session_id reservation_id
         )
    )

let transfer_reservation_to_domain dbg session_id reservation_id domid =
  wrap dbg
    (fun () ->
       Xenctrl.with_intf
         (fun xc ->
            try
              let reservation_id_path = reservation_path _service session_id reservation_id in
              let kib = Client.immediate (get_client ()) (fun xs -> Client.read xs reservation_id_path) in
              (* This code is single-threaded, no need to make this transactional: *)
              Client.immediate (get_client ()) (fun xs -> Client.write xs (Printf.sprintf "/local/domain/%d/memory/initial-reservation" domid) kib);
              Client.immediate (get_client ()) (fun xs -> Client.write xs (Printf.sprintf "/local/domain/%d/memory/reservation-id" domid) reservation_id);
              Client.immediate (get_client ()) (fun xs -> Client.write xs (path [reservation_id_path; "in-transfer"]) (string_of_int domid));
              Opt.iter
                (fun maxmem -> Squeeze_xen.Domain.set_maxmem_noexn xc domid maxmem)
                (try Some (Int64.of_string kib) with _ -> None);
            with Xs_protocol.Enoent _ ->
              raise (MemoryError (Unknown_reservation reservation_id))
         )
    )

let query_reservation_of_domain dbg session_id domid =
  wrap dbg
    (fun () ->
       try
         let reservation_id = Client.immediate (get_client ()) (fun xs -> Client.read xs (Printf.sprintf "/local/domain/%d/memory/reservation-id" domid)) in
         reservation_id
       with Xs_protocol.Enoent _ ->
         raise (MemoryError No_reservation)
    )

let balance_memory dbg =
  wrap dbg
    (fun () ->
       Xenctrl.with_intf
         (fun xc ->
            Squeeze_xen.balance_memory ~xc
         )
    )

let get_host_reserved_memory dbg = Squeeze_xen.target_host_free_mem_kib

let get_total_memory_from_xen () =
  try
    Xenctrl.with_intf (fun xc ->
        let di = Xenctrl.domain_getinfo xc 0 in
        Some (Int64.mul 1024L (Xenctrl.pages_to_kib (Int64.of_nativeint di.Xenctrl.total_memory_pages)))
      )
  with _ ->
    error "Failed get total memory from Xen";
    None

let sysfs_stem = "/sys/devices/system/xen_memory/xen_memory0/"

let _current_allocation = "info/current_kb"
let _requested_target = "target_kb"
let _low_mem_balloon = "info/low_kb"
let _high_mem_balloon = "info/high_kb"

(** Queries the balloon driver and forms a string * int64 association list *)
let parse_sysfs_balloon () =
  let keys = [
    _current_allocation;
    _requested_target;
    _low_mem_balloon;
    _high_mem_balloon] in
  List.map (fun key ->
      let s = Unixext.string_of_file (sysfs_stem ^ key) in
      key, Int64.of_string (String.trim s)
    ) keys

let get_total_memory_from_balloon_driver () =
  try
    let pairs = parse_sysfs_balloon () in
    let keys = [ _low_mem_balloon; _high_mem_balloon; _current_allocation ] in
    let vs = List.map (fun x -> List.assoc x pairs) keys in
    Some (Int64.mul 1024L (List.fold_left Int64.add 0L vs))
  with _ ->
    error "Failed to query balloon driver";
    None

let get_total_memory_from_proc_meminfo () =
  let ic = open_in "/proc/meminfo" in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       let rec loop () =
         match Astring.String.fields ~empty:false (input_line ic) with
         | [ "MemTotal:"; total; "kB" ] ->
           Some (Int64.(mul (of_string total) 1024L))
         | _ -> loop () in
       try
         loop ()
       with End_of_file ->
         error "Failed to read MemTotal from /proc/meminfo";
         None
    ) (fun () -> close_in ic)

(* The total amount of memory addressable by this OS. If we cannot get it
   from Xen (which may not be running if we've just installed
   the packages and are now setting them up), then we ask the balloon driver, or
   worst-case, /proc/meminfo. *)
let get_total_memory () =
  let (>>) x f =
    match x with
    | Some x -> Some x
    | None -> f ()
  in
  None >>
  get_total_memory_from_xen >>
  get_total_memory_from_balloon_driver >>
  get_total_memory_from_proc_meminfo

let get_domain_zero_policy dbg =
  wrap dbg
    (fun () ->
       match get_total_memory () with
       | None -> failwith "Failed to obtain total memory"
       | Some dom0_max ->
         if !Squeeze.manage_domain_zero
         then Auto_balloon(!Squeeze.domain_zero_dynamic_min, match !Squeeze.domain_zero_dynamic_max with
           | None -> dom0_max
           | Some x -> x)
         else Fixed_size dom0_max
    )

(* Calculates the amount of free memory on the host at boot time. *)
(* Returns a result that is equivalent to (T - X), where:         *)
(*     T = total memory in host.                                  *)
(*     X = host virtualization overhead:                          *)
(*         memory used by Xen code, heap and crash kernel.        *)
(* Actually returns the current value of (F + S + Z), where:      *)
(*     F = host free memory.                                      *)
(*     S = host scrub memory.                                     *)
(*     Z = host memory used by domain 0.                          *)
(* This relies on the equivalence (T = X + F + S + Z).            *)
(* Warning! This function assumes that:                           *)
(*     1. Domain 0 is currently in an unballooned state.          *)
(*     2. No other domains have been started.                     *)
let calculate_boot_time_host_free_memory () =
  let ( + ) = Nativeint.add in
  let open Xenctrl in
  let host_info = with_intf (fun xc -> physinfo xc) in
  let host_free_pages = host_info.free_pages in
  let host_scrub_pages = host_info.scrub_pages in
  match get_total_memory () with
  | None -> failwith "Failed to obtain total memory"
  | Some domain0_bytes ->
    let domain0_total_pages = Memory.pages_of_bytes_used domain0_bytes in
    let boot_time_host_free_pages =
      host_free_pages + host_scrub_pages + (Int64.to_nativeint domain0_total_pages) in
    let boot_time_host_free_kib =
      pages_to_kib (Int64.of_nativeint boot_time_host_free_pages) in
    Int64.mul 1024L boot_time_host_free_kib

(* Read the free memory on the host and record this in the db. This is used *)
(* as the baseline for memory calculations in the message forwarding layer. *)
let record_boot_time_host_free_memory () =
  if not (Unixext.file_exists initial_host_free_memory_file) then begin
    try
      let free_memory = calculate_boot_time_host_free_memory () in
      Unixext.mkdir_rec (Filename.dirname initial_host_free_memory_file) 0o700;
      Unixext.write_string_to_file initial_host_free_memory_file
        (Int64.to_string free_memory)
    with e ->
      error "Could not record host free memory. This may prevent \
             VMs from being started on this host. (%s)" (Printexc.to_string e)
  end

let get_host_initial_free_memory dbg =
  try
    Int64.of_string (Unixext.string_of_file initial_host_free_memory_file)
  with e ->
    error "Could not read host free memory file. This may prevent \
           VMs from being started on this host. (%s)" (Printexc.to_string e);
    0L
