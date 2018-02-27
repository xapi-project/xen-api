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
open Client
open Pervasiveext

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

  Printf.kprintf (fun s -> Printf.printf "%s [%d] %s\n" (of_float (Unix.gettimeofday ())) (Thread.id (Thread.self ())) s; flush stdout) fmt
let host = ref "127.0.0.1"
let port = ref 80
let username = ref "root"
let password = ref ""
let vm = ref ""

let make_rpc ?dbg () xml =
  let open Xmlrpc_client in
  let http = {
    (xmlrpc ~version:"1.0" "/") with
    Http.Request.additional_headers = Opt.default [] (Opt.map (fun dbg -> [ "X-Http-other-config-dbg", dbg ]) dbg)
  } in
  XMLRPC_protocol.rpc ~srcstr:"graph" ~dststr:"xapi" ~transport:(TCP(!host, !port)) ~http xml

let wait_for_guest_agent ~rpc ~session_id ~vm =
  debug "prepare: waiting for guest agent in VM %s" (Ref.string_of vm);
  let classes = [ Printf.sprintf "VM/%s" (Ref.string_of vm) ] in
  let timeout = 5.0 in
  let rec wait ~token =
    let open Event_types in
    let event_from = Client.Event.from ~rpc ~session_id ~classes ~token ~timeout |> event_from_of_rpc in
    let records = List.map Event_helper.record_of_event event_from.events in
    let valid = function
      | Event_helper.VM (vm, Some vm_rec) ->
        vm_rec.API.vM_guest_metrics <> Ref.null
      | _ -> false in
    if not (List.fold_left (||) false (List.map valid records))
    then wait ~token:event_from.token in
  let token = "" in
  wait ~token

type operation =
  | Start
  | Shutdown
  | Reboot (* assume hard_ versions are strictly smaller than clean_ versions *)
  | Suspend
  | Resume
  | Pool_migrate
  | VBD_plug
  | VBD_unplug
  | VIF_plug
  | VIF_unplug
[@@deriving rpc]

let operations = [
  VBD_plug
; VBD_unplug
; VIF_plug
; VIF_unplug
; Start
; Shutdown
; Reboot
; Suspend
; Resume
(*
	; Pool_migrate
*)
]

type environment = {
  session_id: API.ref_session;
  vm: API.ref_VM;
  id: string;
  net: API.ref_network;
  vdi: API.ref_VDI;
}

let find_or_create_vif { session_id = session_id; vm = vm; net = net } =
  let rpc = make_rpc () in
  let vifs = Client.VM.get_VIFs ~rpc ~session_id ~self:vm in
  let vif_records = List.map (fun vif -> Client.VIF.get_record ~rpc ~session_id ~self:vif) vifs in
  let vif_idx = "4" in (* arbitrary *)
  try
    let vif, _ = List.find (fun (_, r) -> r.API.vIF_device = vif_idx) (List.combine vifs vif_records) in
    vif
  with Not_found ->
    Client.VIF.create ~rpc ~session_id ~vM:vm ~network:net ~mAC:"" ~device:vif_idx ~mTU:1500L ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~locking_mode:`network_default ~ipv4_allowed:[] ~ipv6_allowed:[]

let find_or_create_vbd { session_id = session_id; vm = vm; vdi = vdi } =
  let rpc = make_rpc () in
  let vbds = Client.VM.get_VBDs ~rpc ~session_id ~self:vm in
  let vbd_records = List.map (fun vbd -> Client.VBD.get_record ~rpc ~session_id ~self:vbd) vbds in
  let vbd_idx = "4" in (* arbitrary *)
  try
    let vbd, _ = List.find (fun (_, r) -> r.API.vBD_userdevice = vbd_idx) (List.combine vbds vbd_records) in
    vbd
  with Not_found ->
    Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:vdi ~userdevice:vbd_idx ~mode:`RO ~_type:`CD ~other_config:[] ~bootable:false ~unpluggable:true ~empty:false ~qos_algorithm_type:"" ~qos_algorithm_params:[]

(* Get the VM into a state for performing a particular op. If this fails
   it may be because of left-over parallel activity from the previous run;
   it should suffice to run the function a second time. *)
let prepare ({ session_id = session_id; vm = vm } as env) op rpc =
  let one () =
    let power_state = Client.VM.get_power_state ~rpc ~session_id ~self:vm in
    begin match op, power_state with
      | Start, `Halted -> ()
      | Start, `Running ->
        wait_for_guest_agent ~rpc ~session_id ~vm;
        debug "prepare: VM.clean_shutdown %s" (Ref.string_of vm);
        Client.VM.clean_shutdown ~rpc ~session_id ~vm
      | Start, `Paused ->
        debug "prepare: VM.unpause %s" (Ref.string_of vm);
        Client.VM.unpause ~rpc ~session_id ~vm;
        wait_for_guest_agent ~rpc ~session_id ~vm;
        debug "prepare: VM.clean_shutdown %s" (Ref.string_of vm);
        Client.VM.clean_shutdown ~rpc ~session_id ~vm
      | Start, `Suspended ->
        debug "prepare: VM.resume %s" (Ref.string_of vm);
        Client.VM.resume ~rpc ~session_id ~vm ~start_paused:false ~force:false;
        debug "prepare: VM.clean_shutdown %s" (Ref.string_of vm);
        Client.VM.clean_shutdown ~rpc ~session_id ~vm
      | Resume, `Halted ->
        debug "prepare: VM.start %s" (Ref.string_of vm);
        Client.VM.start ~rpc ~session_id ~vm ~start_paused:false ~force:false;
        wait_for_guest_agent ~rpc ~session_id ~vm;
        debug "prepare: VM.suspend %s" (Ref.string_of vm);
        Client.VM.suspend ~rpc ~session_id ~vm
      | Resume, `Running ->
        debug "prepare: VM.suspend %s" (Ref.string_of vm);
        Client.VM.suspend ~rpc ~session_id ~vm
      | Resume, `Paused ->
        debug "prepare: VM.unpause %s" (Ref.string_of vm);
        Client.VM.unpause ~rpc ~session_id ~vm;
        wait_for_guest_agent ~rpc ~session_id ~vm;
        debug "prepare: VM.suspend %s" (Ref.string_of vm);
        Client.VM.suspend ~rpc ~session_id ~vm
      | Resume, `Suspended -> ()
      | _, `Running ->
        wait_for_guest_agent ~rpc ~session_id ~vm
      | _, `Halted ->
        debug "prepare: VM.start %s" (Ref.string_of vm);
        Client.VM.start ~rpc ~session_id ~vm ~start_paused:false ~force:false;
        wait_for_guest_agent ~rpc ~session_id ~vm;
      | _, `Paused ->
        debug "prepare: VM.unpause %s" (Ref.string_of vm);
        Client.VM.unpause ~rpc ~session_id ~vm;
        wait_for_guest_agent ~rpc ~session_id ~vm;
      | _, `Suspended ->
        debug "prepare: VM.resume %s" (Ref.string_of vm);
        Client.VM.resume ~rpc ~session_id ~vm ~start_paused:false ~force:false;
    end;
    begin match op with
      | VIF_unplug ->
        let vif = find_or_create_vif env in
        if Client.VIF.get_currently_attached ~rpc ~session_id ~self:vif
        then Client.VIF.unplug ~rpc ~session_id ~self:vif;
        Client.VIF.plug ~rpc ~session_id ~self:vif
      | VIF_plug ->
        let vif = find_or_create_vif env in
        if not(Client.VIF.get_currently_attached ~rpc ~session_id ~self:vif)
        then Client.VIF.plug ~rpc ~session_id ~self:vif;
        Client.VIF.unplug ~rpc ~session_id ~self:vif
      | VBD_unplug ->
        let vbd = find_or_create_vbd env in
        if Client.VBD.get_currently_attached ~rpc ~session_id ~self:vbd
        then Client.VBD.unplug ~rpc ~session_id ~self:vbd;
        Client.VBD.plug ~rpc ~session_id ~self:vbd
      | VBD_plug ->
        let vbd = find_or_create_vbd env in
        if not(Client.VBD.get_currently_attached ~rpc ~session_id ~self:vbd)
        then Client.VBD.plug ~rpc ~session_id ~self:vbd;
        Client.VBD.unplug ~rpc ~session_id ~self:vbd
      | _ -> ()
    end
  in
  try
    one ()
  with Api_errors.Server_error(code, params) ->
    Printf.fprintf stderr "prepare: ignoring one-off error %s %s\n" code (String.concat " " params);
    one () (* a single second go should be enough *)

let execute ({ session_id = session_id; vm = vm } as env) op rpc = match op with
  | Start ->
    debug "execute: VM.start %s" (Ref.string_of vm);
    Client.Async.VM.start ~rpc ~session_id ~vm ~start_paused:false ~force:false
  | Shutdown ->
    debug "execute: VM.clean_shutdown %s" (Ref.string_of vm);
    Client.Async.VM.clean_shutdown ~rpc ~session_id ~vm
  | Reboot ->
    debug "execute: VM.clean_reboot %s" (Ref.string_of vm);
    Client.Async.VM.clean_reboot ~rpc ~session_id ~vm
  | Suspend ->
    debug "execute: VM.suspend %s" (Ref.string_of vm);
    Client.Async.VM.suspend ~rpc ~session_id ~vm
  | Resume ->
    debug "execute: VM.resume %s" (Ref.string_of vm);
    Client.Async.VM.resume ~rpc ~session_id ~vm ~start_paused:false ~force:false
  | Pool_migrate ->
    debug "execute: VM.pool_migrate %s to localhost" (Ref.string_of vm);
    let host = Client.VM.get_resident_on ~rpc ~session_id ~self:vm in
    Client.Async.VM.pool_migrate ~rpc ~session_id ~vm ~host ~options:["live", "true"]
  | VBD_plug ->
    let vbd = find_or_create_vbd env in
    Client.Async.VBD.plug ~rpc ~session_id ~self:vbd
  | VBD_unplug ->
    let vbd = find_or_create_vbd env in
    Client.Async.VBD.unplug ~rpc ~session_id ~self:vbd
  | VIF_plug ->
    let vif = find_or_create_vif env in
    Client.Async.VIF.plug ~rpc ~session_id ~self:vif
  | VIF_unplug ->
    let vif = find_or_create_vif env in
    Client.Async.VIF.unplug ~rpc ~session_id ~self:vif

module OpMap = Map.Make(struct type t = operation let compare = compare end)

type tc = operation * int (* operation * cancel point index *)

let test ({ session_id = session_id; vm = vm; id = id } as env) (op, n) =
  let dbg = "cancel_tests" in
  prepare env op (make_rpc ());
  let module XN = Xenops_client.Client in
  XN.DEBUG.trigger "cancel_tests" "set-cancel-trigger" [ dbg; string_of_int n ];
  let task = execute env op (make_rpc ~dbg ()) in
  let rpc = make_rpc () in
  Tasks.wait_for_all ~rpc ~session_id ~tasks:[task];
  begin match Client.Task.get_status ~rpc ~session_id ~self:task with
    | `pending -> failwith "task is pending (not cancelled)"
    | `success -> failwith "task succeed (not cancelled)"
    | `failure -> failwith "task failed (not cancelled)"
    | `cancelling -> failwith "task cancelling (not cancelled)"
    | `cancelled -> ()
  end;
  Client.Task.destroy ~rpc ~session_id ~self:task;
  (* Wait for the states to stabilise *)

  let suspend_vdi () = Client.VM.get_suspend_VDI ~rpc ~session_id ~self:vm in
  let xenopsd f =
    try
      let _, info = XN.VM.stat dbg id in
      f (Some info)
    with _ -> f None in
  let running_in_xenopsd () = xenopsd (function
      | Some info -> info.Xenops_interface.Vm.power_state = Xenops_interface.Running
      | None -> false) in
  let paused_in_xenopsd () = xenopsd (function
      | Some info -> info.Xenops_interface.Vm.power_state = Xenops_interface.Paused
      | None -> false) in
  let missing_in_xenopsd () = xenopsd (function
      | Some _ -> false
      | None -> true) in
  let domain f =
    let open Xenstore in
    Xenops_helpers.with_xs
      (fun xs ->
         try
           match xs.Xs.directory (Printf.sprintf "/vm/%s/domains" id) with
           | [ domid ] ->
             let open Xenctrl in
             with_intf
               (fun xc ->
                  let di = domain_getinfo xc (int_of_string domid) in
                  f (Some di)
               )
           | _ -> f None
         with _ -> f None
      ) in
  let running_domain () = domain (function
      | Some di -> not(di.Xenctrl.paused) && not(di.Xenctrl.shutdown)
      | None -> false) in
  let paused_domain () = domain (function
      | Some di -> di.Xenctrl.paused && not(di.Xenctrl.shutdown)
      | None -> false) in
  let missing_domain () = domain (function
      | Some _ -> false
      | None -> true) in

  let devices_in_sync () =
    let vifs_xenops =
      List.filter (fun (_, state) -> state.Xenops_interface.Vif.active)
        (XN.VIF.list dbg env.id) in
    let vifs_xapi =
      List.filter (fun vif -> Client.VIF.get_currently_attached ~rpc ~session_id ~self:vif)
        (Client.VM.get_VIFs ~rpc ~session_id ~self:env.vm) in

    let vbds_xenops =
      List.filter (fun (_, state) -> state.Xenops_interface.Vbd.active)
        (XN.VBD.list dbg env.id) in
    let vbds_xapi =
      List.filter (fun vbd -> Client.VBD.get_currently_attached ~rpc ~session_id ~self:vbd)
        (Client.VM.get_VBDs ~rpc ~session_id ~self:env.vm) in
    List.length vifs_xenops = (List.length vifs_xapi) && (List.length vbds_xenops = (List.length vbds_xapi)) in

  let finished = ref false in
  let start = Unix.gettimeofday () in
  let timeout = 30. in
  while not(!finished) && (Unix.gettimeofday () -. start < timeout) do
    finally
      (fun () ->
         finished :=
           match Client.VM.get_power_state ~rpc ~session_id ~self:vm with
           | `Halted ->
             missing_in_xenopsd () && missing_domain () && (suspend_vdi () = Ref.null)
           | `Running ->
             running_in_xenopsd () && running_domain () && (suspend_vdi () = Ref.null) && devices_in_sync ()
           | `Suspended ->
             missing_in_xenopsd () && missing_domain () && (suspend_vdi () <> Ref.null)
           | `Paused ->
             paused_in_xenopsd () && paused_domain () && (suspend_vdi () = Ref.null) && devices_in_sync ()
      ) (fun () -> Thread.delay 1.)
  done;
  if not !finished then failwith "State never stabilised"

let cancel_points_seen = "debug_info:cancel_points_seen"

let counter = ref 0

let cancel_points_of session_id f =
  incr counter;
  let rpc = make_rpc ~dbg:(Printf.sprintf "cancel_points_of:%d" !counter) () in
  let task : API.ref_task = f rpc in
  let rpc = make_rpc () in
  Tasks.wait_for_all ~rpc ~session_id ~tasks:[task];
  let status = Client.Task.get_status ~rpc ~session_id ~self:task in
  if status <> `success then begin
    let error = Client.Task.get_error_info ~rpc ~session_id ~self:task in
    failwith (Printf.sprintf "Failed with %s" (String.concat " " error))
  end;
  let other_config = Client.Task.get_other_config ~rpc ~session_id ~self:task in
  Client.Task.destroy ~rpc ~session_id ~self:task;
  if List.mem_assoc cancel_points_seen other_config
  then Some (int_of_string (List.assoc cancel_points_seen other_config))
  else None

let probe_tcs env =
  debug "probe: computing lists of test cases";
  let operation_to_cancel_points = ref OpMap.empty in
  List.iter
    (fun operation ->
       prepare env operation (make_rpc ());
       match cancel_points_of env.session_id (execute env operation) with
       | Some x ->
         operation_to_cancel_points := OpMap.add operation x !operation_to_cancel_points
       | None ->
         debug "Seen no cancel points"
    ) operations;
  OpMap.iter
    (fun k v ->
       debug "probe: %s has %d cancel points" (k |> rpc_of_operation |> Jsonrpc.to_string) v
    ) !operation_to_cancel_points;
  OpMap.fold
    (fun op num acc ->
       (* need to trigger cancel points [1..num] *)
       let rec integers first last =
         if first > last
         then []
         else first :: (integers (first + 1) last) in
       List.map (fun i -> op, i) (integers 1 num) @ acc
    ) !operation_to_cancel_points []

let run env =
  let all = probe_tcs env in
  debug "probe: there are a total of %d tests" (List.length all);
  List.iter
    (fun (k, v) ->
       debug "test: %s cancelling at %d" (k |> rpc_of_operation |> Jsonrpc.to_string) v;
       test env (k, v)
    ) all;
  debug "tests complete"

let _ =
  Arg.parse [
    "-h", Arg.Set_string host, "hostname to connect to";
    "-p", Arg.Set_int port, "port number to connect to";
    "-u", Arg.Set_string username, "username to connect with";
    "-pw", Arg.Set_string password, "password to connect with";
    "-vm", Arg.Set_string vm, "name of VM to manipulate";
  ]
    (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
    "Test VM lifecycle cancellation leaves the system in a valid state";

  let rpc = make_rpc () in
  let session_id = Client.Session.login_with_password ~rpc ~uname:!username ~pwd:!password ~version:"1.2" ~originator:"cancel_tests" in
  finally
    (fun () ->
       match Client.VM.get_by_name_label ~rpc ~session_id ~label:!vm with
       | [] ->
         failwith (Printf.sprintf "Failed to find a VM with name: %s" !vm)
       | [ v ] ->
         let net = Client.Network.get_by_name_label ~rpc ~session_id ~label:"Host internal management network" in
         if List.length net = 0 then failwith "Failed to find the host internal management network";
         let vdi =
           let tools_iso_filter = "field \"is_tools_iso\"=\"true\"" in
           begin match Client.VDI.get_all_records_where !rpc session_id tools_iso_filter with
             | (vdi, _)::_ -> vdi
             | [] -> failwith "Failed to find the tools ISO";
           end
         in
         let env = {
           session_id = session_id;
           vm = v;
           id = Client.VM.get_uuid ~rpc ~session_id ~self:v;
           net = List.hd net;
           vdi;
         } in
         run env
       | _ ->
         failwith (Printf.sprintf "Found multiple VMs with name: %s" !vm)
    ) (fun () ->
        Client.Session.logout ~rpc ~session_id
      )
