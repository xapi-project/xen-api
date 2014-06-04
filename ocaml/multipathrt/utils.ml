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
open Xstringext
open Pervasiveext
open Listext
open Client

(* ----------- Ignore functions ----------- *)

let ignore_VBD v = let (_ : API.ref_VBD) = v in ()

(* --------------- Debugging --------------- *)

let stdout_m = Mutex.create () 

let debug (fmt: ('a , unit, string, unit) format4) =
  (* Convert calendar time, x, to tm in UTC *)
  let of_float x = 
    let time = Unix.gmtime x in
    Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
      (time.Unix.tm_year+1900)
      (time.Unix.tm_mon+1)
      time.Unix.tm_mday
      time.Unix.tm_hour
      time.Unix.tm_min
      time.Unix.tm_sec in

  Threadext.Mutex.execute stdout_m
    (fun () ->
       Printf.kprintf (fun s -> Printf.printf "%s [%d] %s\n" (of_float (Unix.gettimeofday ())) (Thread.id (Thread.self ())) s; flush stdout) fmt 
    )

(* --------------- Lazy lists --------------- *)

module LazyList = struct
  type 'a t = Cons of 'a * (unit -> 'a t) | Nil

  let rec of_list = function
    | [] -> Nil
    | x::xs -> Cons(x, fun () -> of_list xs)

  let hd = function
    | Cons (x,xf) -> x
    | Nil -> raise Not_found
end

(* --------------- Utility functions --------------- *)

let get_master rpc session =
  let pool = List.hd (Client.Pool.get_all rpc session) in
  let master = Client.Pool.get_master rpc session pool in
  master

(* return the list of all control domains of the pool *)
let get_all_control_domains rpc session =
  let vms = Client.VM.get_all_records rpc session in
  List.filter (fun (vm,vmr) -> vmr.API.vM_is_control_domain) vms

let get_control_domain rpc session =
  let master = get_master rpc session in
  let dom0 = List.filter (fun (vm,vmr) -> vmr.API.vM_resident_on=master) (get_all_control_domains rpc session) in
  List.hd dom0

let create_test_vdi rpc session ?(virtual_size=Globs.four_megs) sr =
  Client.VDI.create ~rpc ~session_id:session ~name_label:"test VDI" ~name_description:"" ~sR:sr ~virtual_size ~_type:`ephemeral (*!*) ~sharable:true ~read_only: false ~xenstore_data:[] ~other_config:[] ~sm_config:[] ~tags:[]

let destroy_vm rpc session vm =
  let vbds = Client.VM.get_VBDs rpc session vm in
  Client.VM.hard_shutdown rpc session vm;
  List.iter
    (fun vbd ->
      let vdi = Client.VBD.get_VDI rpc session vbd in
      (try Client.VDI.destroy rpc session vdi with _ -> ());
      (try Client.VBD.destroy rpc session vbd with _ -> ())
    ) vbds;
  Client.VM.destroy rpc session vm

let destroy_sr rpc session sr =
  let pbds = Client.SR.get_PBDs rpc session sr in
  List.iter
    (fun pbd ->
      Client.PBD.unplug rpc session pbd;
      Client.PBD.destroy rpc session pbd;
    ) pbds;
  Client.SR.forget rpc session sr

let plug_vdi_to_dom0 rpc session vdi =
  let dom0 = fst (get_control_domain rpc session) in
  let rec getvbd n =
    let devices = Client.VM.get_allowed_VBD_devices ~rpc ~session_id:session ~vm:dom0 in
    try
      let vbd = Client.VBD.create ~rpc ~session_id:session ~vM:dom0 ~vDI:vdi ~userdevice:(List.hd devices)
        ~bootable:false ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:""
        ~qos_algorithm_params:[] in
      vbd
    with e ->
      (* There's a race between xapi telling us a device to use and us trying to use it, so retry here *)
      if n>1 then raise e else getvbd (n+1)
  in
  let vbd = getvbd 0 in
  Client.VBD.plug ~rpc ~session_id:session ~self:vbd;
  vbd

let is_multipathing_enabled rpc session host =
  debug "Checking whether multipathing is enabled on the host...";
  let other_config = Client.Host.get_other_config rpc session host in
  let enabled = (List.mem_assoc "multipathing" other_config && List.assoc "multipathing" other_config = "true") &&
      (List.mem_assoc "multipathhandle" other_config && List.assoc "multipathhandle" other_config = "dmp") in
  if enabled then debug "Multipathing is enabled" else debug "Multipathing is disabled";
  enabled

let enable_multipathing rpc session host =
  debug "Enabling multipathing on the host";

  (* Unplug all plugged PBDs *)
  let pbds = Client.PBD.get_all_records rpc session in
  let pbds = List.filter (fun (_,pbd_rec) -> pbd_rec.API.pBD_currently_attached) pbds in
  debug "Unplugging %d PBDs..." (List.length pbds);
  List.iter (fun (pbd,_) -> Client.PBD.unplug rpc session pbd) pbds;

  finally
    (fun () ->
      (* Activate multipathing on the host *)
      debug "Setting multipathing other-config keys";
      Client.Host.remove_from_other_config rpc session host "multipathing";
      Client.Host.remove_from_other_config rpc session host "multipathhandle";
      Client.Host.add_to_other_config rpc session host "multipathing" "true";
      Client.Host.add_to_other_config rpc session host "multipathhandle" "dmp";
    )
    (fun () ->
      (* Replug the PBDs that were unplugged *)
      debug "Replugging %d unplugged PBDs..." (List.length pbds);
      List.iter (fun (pbd,_) -> Client.PBD.plug rpc session pbd) pbds;
    )

let get_vif_ip_addrs rpc session vm =
  let vifs = Client.VM.get_VIFs rpc session vm in
  List.mapi (fun i vif ->
    let guest_metrics = Client.VM.get_guest_metrics rpc session vm in
    let networks = Client.VM_guest_metrics.get_networks rpc session guest_metrics in
    let key = Printf.sprintf "%d/ip" i in
    if not(List.mem_assoc key networks) then raise (Multipathrt_exceptions.Test_error (Printf.sprintf "Could not find IP address of VIF %d in guest_metrics" i));
    let ip = List.assoc key networks in
    debug "IP address of VIF %d is %s" i ip;
    ip
  ) vifs

let modify_paths rpc session host ip_addrs remove =
  let num_paths = List.length ip_addrs in
  debug "Requesting to %s %d paths: [%s]" (if remove then "remove" else "reinstate") num_paths (String.concat "; " ip_addrs);
  let path_args = List.mapi (fun i ip -> ((Printf.sprintf "ip%d" i), ip)) ip_addrs in
  let args = ("num_paths", string_of_int num_paths) :: ("remove", if remove then "true" else "false") :: path_args in
  let ret = Client.Host.call_plugin ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn:"modify_paths" ~args in
  let exp_ret = String.concat "," (List.map (fun ip -> "0") ip_addrs) in
  debug "Got return codes '%s', expected '%s'" ret exp_ret;
  if ret <> exp_ret then raise (Multipathrt_exceptions.Test_error (Printf.sprintf "Could not modify paths: expected return codes '%s', got '%s'" exp_ret ret));
  ()

let time_data_transfer rpc session host vbd =
  debug "Timing data transfer...";
  let vbd_rec = Client.VBD.get_record rpc session vbd in
  let device = vbd_rec.API.vBD_device in
  let ret = Client.Host.call_plugin ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn:"time_data_transfer" ~args:["device",device] in
  float_of_string ret

(* Remove an element from a list at random. For example, [a;b;c;d] might become [a;b;d]. Fails with empty list. *)
let remove_random_element xs =
  let length = List.length xs in
  let random_element = Random.int length in
  debug "Randomly selected element %d as the path to keep" random_element;
  let (gone, remaining) = List.extract random_element xs in
  remaining

let select_random_element xs =
  let length = List.length xs in
  let random_element = Random.int length in
  List.nth xs random_element

