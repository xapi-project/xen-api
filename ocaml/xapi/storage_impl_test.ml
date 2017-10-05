(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
 * @group Storage
*)

open Stdext
open Pervasiveext
open Threadext
open Storage_interface

let total_errors = ref 0
let total_errors_m = Mutex.create ()

let debug = Storage_impl.debug
let error = Storage_impl.error
let info = Storage_impl.info

let inc_errors () =
  Mutex.execute total_errors_m
    (fun () ->
       incr total_errors;
    )

exception Api_error of string * (string list)

module Debug_print_impl = struct
  type context = Smint.request
  module Query = struct
    let query context ~dbg = assert false
    let diagnostics context ~dbg = assert false
  end
  module DP = struct
    let create context ~dbg ~id = assert false
    let destroy context ~dbg ~dp = assert false
    let diagnostics context () = assert false
    let attach_info context ~dbg ~sr ~vdi ~dp = assert false
    let stat_vdi context ~dbg ~sr ~vdi () = assert false
  end
  module VDI = struct
    let m = Mutex.create ()
    let attached = Hashtbl.create 10
    let activated = Hashtbl.create 10
    let created = Hashtbl.create 10
    let key_of sr vdi = Printf.sprintf "%s/%s" sr vdi

    let create context ~dbg ~sr ~vdi_info =
      let vdi = "newvdi" in
      let info =
        if List.mem_assoc "toosmall" vdi_info.sm_config
        then { vdi_info with virtual_size = Int64.sub vdi_info.virtual_size 1L }
        else vdi_info in
      Mutex.execute m
        (fun () ->
           let key = key_of sr vdi in
           Hashtbl.replace created key info
        );
      info

    let set_name_label context ~dbg ~sr ~vdi ~new_name_label = ()
    let set_name_description context ~dbg ~sr ~vdi ~new_name_description = ()

    let snapshot context ~dbg ~sr ~vdi_info =
      create context ~dbg ~sr ~vdi_info
    let clone = snapshot

    let destroy context ~dbg ~sr ~vdi =
      Mutex.execute m
        (fun () ->
           let key = key_of sr vdi in
           if not(Hashtbl.mem created key)
           then raise (Backend_error("ENOENT", [ sr; vdi ]))
           else if Hashtbl.mem activated key
           then raise (Backend_error("Still activated", [ sr; vdi]))
           else if Hashtbl.mem attached key
           then raise (Backend_error("Still attached", [ sr; vdi]))
           else begin
             Hashtbl.remove created key
           end
        )

    let epoch_begin context ~dbg ~sr ~vdi ~persistent = ()

    let stat context ~dbg ~sr ~vdi = assert false

    let introduce context ~dbg ~sr ~uuid ~sm_config ~location = assert false

    let set_persistent context ~dbg ~sr ~vdi ~persistent = ()

    let attach context ~dbg ~dp ~sr ~vdi ~read_write =
      info "VDI.attach dp:%s sr:%s vdi:%s read_write:%b" dp sr vdi read_write;
      if dp = "error"
      then raise (Api_error("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]));
      if dp = "error2"
      then raise (Backend_error ("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]))
      else begin
        Mutex.execute m
          (fun () ->
             let key = key_of sr vdi in
             if Hashtbl.mem attached key then begin
               inc_errors ();
               error "VDI.attach dp:%s sr:%s vdi:%s : double attach" dp sr vdi;
               failwith "double attach"
             end else Hashtbl.replace attached key ());
        { params="XXX"; o_direct=true; o_direct_reason=""; xenstore_data=[] }
      end
    let activate context ~dbg ~dp ~sr ~vdi =
      Mutex.execute m
        (fun () ->
           let key = key_of sr vdi in
           if Hashtbl.mem activated key then begin
             inc_errors ();
             error "VDI.detach dp:%s sr:%s vdi:%s : double activate" dp sr vdi;
             failwith "double activate"
           end else Hashtbl.replace activated key ());
      info "VDI.activate dp:%s sr:%s vdi:%s" dp sr vdi

    let working = ref false

    let epoch_end context ~dbg ~sr ~vdi = ()

    let detach context ~dbg ~dp ~sr ~vdi =
      if vdi = "error" && not(!working)
      then raise (Api_error("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]));
      if vdi = "error2" && not(!working)
      then raise (Backend_error ("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]))
      else begin
        Mutex.execute m
          (fun () ->
             let key = key_of sr vdi in
             if not (Hashtbl.mem attached key) then begin
               inc_errors ();
               error "VDI.detach dp:%s sr:%s vdi:%s : double detach" dp sr vdi;
               failwith "double detach"
             end else Hashtbl.remove attached key);
        info "VDI.detach dp:%s sr:%s vdi:%s" dp sr vdi
      end
    let deactivate context ~dbg ~dp ~sr ~vdi =
      Mutex.execute m
        (fun () ->
           let key = key_of sr vdi in
           if not (Hashtbl.mem activated key) then begin
             inc_errors ();
             error "VDI.deactivate dp:%s sr:%s vdi:%s : double deactivate" dp sr vdi;
             failwith "double deactivate"
           end else Hashtbl.remove activated key);
      info "VDI.deactivate dp:%s sr:%s vdi:%s" dp sr vdi

    let resize context ~dbg ~sr ~vdi ~new_size = assert false

    let get_url context ~dbg ~sr ~vdi = assert false
    let compose context ~dbg ~sr ~vdi1 ~vdi2 = assert false
    let add_to_sm_config context ~dbg ~sr ~vdi ~key ~value = assert false
    let remove_from_sm_config context ~dbg ~sr ~vdi ~key = assert false
    let set_content_id context ~dbg ~sr ~vdi ~content_id = assert false
    let get_by_name context ~dbg ~sr ~name = assert false
    let similar_content context ~dbg ~sr ~vdi = assert false
    let enable_cbt context ~dbg ~sr ~vdi = assert false
    let disable_cbt context ~dbg ~sr ~vdi = assert false
    let data_destroy context ~dbg ~sr ~vdi = assert false
    let list_changed_blocks context ~dbg ~sr ~vdi_from ~vdi_to = assert false


  end

  let get_by_name context ~dbg ~name = assert false

  module DATA = struct
    let copy context ~dbg ~sr ~vdi ~dp ~url ~dest = assert false
    let copy_into context ~dbg ~sr ~vdi ~url ~dest = assert false
    module MIRROR = struct
      let start context ~dbg ~sr ~vdi ~dp ~url ~dest = assert false
      let stop context ~dbg ~id = assert false
      let list context ~dbg = assert false
      let stat context ~dbg ~id = assert false
      let receive_start context ~dbg ~sr ~vdi_info ~id ~similar = assert false
      let receive_finalize context ~dbg ~id = assert false
      let receive_cancel context ~dbg ~id = assert false
    end
  end

  module SR = struct
    include Storage_skeleton.SR
    let list context ~dbg = assert false
    let scan context ~dbg ~sr = assert false
    let create context ~dbg ~sr ~name_label ~name_description ~device_config ~physical_size = assert false
    let attach context ~dbg ~sr ~device_config =
      info "SR.attach sr:%s" sr
    let fail_if_anything_leaked () =
      Mutex.execute VDI.m
        (fun () ->
           Hashtbl.iter
             (fun k _ ->
                error "leaked attach: %s" k;
                inc_errors ();
             ) VDI.attached;
           Hashtbl.iter
             (fun k _ ->
                error "leaked activate: %s" k;
                inc_errors ();
             ) VDI.activated
        )
    let detach context ~dbg ~sr =
      info "SR.detach sr:%s" sr;
      fail_if_anything_leaked ()
    let reset context ~dbg ~sr = assert false
    let stat context ~dbg ~sr = assert false
    let destroy context ~dbg ~sr =
      info "SR.destroy sr:%s" sr;
      fail_if_anything_leaked ()
    let update_snapshot_info_src context ~dbg ~sr ~vdi
        ~url ~dest ~dest_vdi ~snapshot_pairs =
      assert false
    let update_snapshot_info_dest context ~dbg ~sr ~vdi
        ~src_vdi ~snapshot_pairs =
      assert false
  end

  module Policy = struct
    let get_backend_vm context ~dbg ~vm ~sr ~vdi = assert false
  end

  module TASK = struct
    let stat context ~dbg ~task = assert false
    let destroy context ~dbg ~task = assert false
    let cancel context ~dbg ~task = assert false
    let list context ~dbg = assert false
  end

  module UPDATES = struct
    let get context ~dbg ~from ~timeout = assert false
  end
end


module Server=Server(Storage_impl.Wrapper(Debug_print_impl))

let path = "/tmp/storage"

let rpc_unix call =
  let open Xmlrpc_client in
  XMLRPC_protocol.rpc ~transport:(Unix path) ~http:(xmlrpc ~version:"1.0" "/") call
let rpc_inprocess call = Server.process (Some "") call

let use_inprocess_rpc = ref true

let rpc call = if !use_inprocess_rpc then rpc_inprocess call else rpc_unix call

let dbg = "dbg"

module Client=Client(struct let rpc=rpc end)

let datapath_of_id id = Client.DP.create ~dbg ~id

let expect expected f x =
  if not(f x) then begin
    error "error: expected %s" expected;
    inc_errors ();
  end

let backend_error f =
  try ignore(f ()); false with
  | (Backend_error(code, params)) when code = "SR_BACKEND_FAILURE_test" -> true
  | e ->
    debug "backend_error: Expecting SR_BACKEND_FAILURE_test, got '%s'" (Printexc.to_string e);
    false

let too_small_backend_error f =
  try ignore(f ()); false with
  | (Backend_error(code, params)) when code = "SR_BACKEND_FAILURE" && (List.hd params = "Disk too small") -> true
  | _ -> false

let internal_error f =
  try ignore(f ()); false with
  | (Internal_error "Storage_impl_test.Api_error(\"SR_BACKEND_FAILURE_test\", _)") -> true
  | _ -> false

let dp_is dp state s =
  if not (List.mem_assoc dp s.dps)
  then state = Vdi_automaton.Detached
  else
    let state' = List.assoc dp s.dps in
    let result = state = state' in
    if not result then begin
      debug "dp_is: returning false: actual state=%s passed state=%s"
        (Vdi_automaton.string_of_state state')
        (Vdi_automaton.string_of_state state)
    end;
    result

let test_vdis sr : unit =
  let num_users = 10 in
  let num_vdis = 10 in
  let iterations = 10 in
  let one id sr vdi () =
    let dp = datapath_of_id id in
    for i = 0 to iterations - 1 do
      expect "_" (function _ -> true)
        (Client.VDI.attach ~dbg ~dp ~sr ~vdi ~read_write:false);
      expect "Attached(RO) 1" (dp_is dp (Vdi_automaton.Attached Vdi_automaton.RO))
        (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
      expect "() 2" (fun x -> x = ())
        (Client.VDI.detach ~dbg ~dp ~sr ~vdi);
      expect "Detached 3" (dp_is dp Vdi_automaton.Detached)
        (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
      expect "()" (fun x -> x = ())
        (Client.VDI.detach ~dbg ~dp ~sr ~vdi);
      expect "Params _ 4" (function _ -> true)
        (Client.VDI.attach ~dbg ~dp ~sr ~vdi ~read_write:false);
      expect "Attached(RO) 5" (dp_is dp (Vdi_automaton.Attached Vdi_automaton.RO))
        (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
      expect "() 6" (fun x -> x = ())
        (Client.VDI.activate ~dbg ~dp ~sr ~vdi);
      expect "Activated(RO) 7" (dp_is dp (Vdi_automaton.Activated Vdi_automaton.RO))
        (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
      expect "() 8" (fun x -> x = ())
        (Client.VDI.deactivate ~dbg ~dp ~sr ~vdi);
      expect "Attached(RO) 9" (dp_is dp (Vdi_automaton.Attached Vdi_automaton.RO))
        (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
      expect "() 10" (fun x -> x = ())
        (Client.VDI.detach ~dbg ~dp ~sr ~vdi);
      expect "Detached 11" (dp_is dp Vdi_automaton.Detached)
        (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
    done in
  let vdis = Range.to_list (Range.make 0 num_vdis) in
  let users = Range.to_list (Range.make 0 num_users) in
  let bodies =
    List.concat (
      List.map (fun user ->
          List.map (fun vdi -> one (Printf.sprintf "vdi:%d/user:%d" vdi user) sr (string_of_int vdi)) vdis
        ) users
    ) in
  info "Starting %d threads%!" (List.length bodies);
  let threads = List.map (fun f -> Thread.create f ()) bodies in
  info "Joining %d threads%!" (List.length bodies);
  List.iter Thread.join threads

let leak dp sr activate vdi =
  info "Leaking some attaches and activates";
  expect "Params _" (function _ -> true)
    (Client.VDI.attach ~dbg ~dp ~sr ~vdi ~read_write:true);
  if activate
  then expect "()" (function () -> true)
      (Client.VDI.activate ~dbg ~dp ~sr ~vdi)

let test_sr sr =
  let dp = datapath_of_id "pbd" in
  expect "()" (fun x -> x = ())
    (Client.SR.attach ~dbg ~sr ~device_config:[]);
  test_vdis sr;
  leak dp sr false "leaked";
  leak dp sr true "leaked2";
  info "About to SR.detach";
  expect "()" (fun x -> x = ())
    (Client.SR.detach ~dbg ~sr);
  expect "()" (fun x -> x = ())
    (Client.SR.attach ~dbg ~sr ~device_config:[]);
  leak dp sr false "leaked";
  leak dp sr true "leaked2";
  info "About to logout";
  expect "()" (fun x -> x = ())
    (Client.DP.destroy ~dbg ~dp ~allow_leak:false);
  info "About to SR.detach";
  expect "()" (function () -> true)
    (Client.SR.detach ~dbg ~sr);
  (* About to check the error handling *)
  let dp = datapath_of_id "error" in
  expect "()" (fun x -> x = ())
    (Client.SR.attach ~dbg ~sr ~device_config:[]);
  debug "This VDI.attach should fail:";
  expect "internal_error" internal_error
    (fun () -> Client.VDI.attach ~dbg ~dp ~sr ~vdi:"leaked" ~read_write:true);
  let dp = datapath_of_id "error2" in
  debug "This VDI.attach should fail:";
  expect "backend_error" backend_error
    (fun () -> Client.VDI.attach ~dbg ~dp ~sr ~vdi:"leaked" ~read_write:true);
  debug "Detaching and cleaning up";
  expect "()" (fun x -> x = ())
    (Client.SR.detach ~dbg ~sr)

(* Check the DP.stat_vdi function works *)
let test_stat sr vdi =
  expect "()" (fun x -> x = ())
    (Client.SR.attach ~dbg ~sr ~device_config:[]);
  let dp1 = datapath_of_id "dp1" (* will be r/w *)
  and dp2 = datapath_of_id "dp2" (* will be r/o *) in
  expect "Params _" (function _ -> true)
    (Client.VDI.attach ~dbg ~dp:dp1 ~sr ~vdi ~read_write:true);
  (* dp1: Attached(RW) dp2: Detached superstate: Attached(RW) *)
  expect "Attached(RW)" (dp_is dp1 (Vdi_automaton.Attached Vdi_automaton.RW))
    (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
  expect "Attached(RW)" (function x -> x.superstate = Vdi_automaton.Attached Vdi_automaton.RW)
    (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
  expect "Params _" (function _ -> true)
    (Client.VDI.attach ~dbg ~dp:dp2 ~sr ~vdi ~read_write:false);
  (* dp1: Attached(RW) dp2: Attached(RO) superstate: Attached(RW) *)
  expect "Attached(RO)" (dp_is dp2 (Vdi_automaton.Attached Vdi_automaton.RO))
    (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
  expect "Attached(RW)" (function x -> x.superstate = Vdi_automaton.Attached Vdi_automaton.RW)
    (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
  expect "Illegal transition" (fun () ->
      try
        Client.VDI.detach ~dbg ~dp:dp1 ~sr ~vdi;
        false
      with
      | Illegal_transition(Vdi_automaton.Attached(Vdi_automaton.RW), Vdi_automaton.Attached(Vdi_automaton.RO)) -> true
      | e -> false) ();
  expect "()" (fun () -> true)
    (Client.VDI.detach ~dbg ~dp:dp2 ~sr ~vdi);
  (* dp1: Attached(RW) dp2: Detached superstate: Attached(RW) *)
  expect "Detached" (dp_is dp2 Vdi_automaton.Detached)
    (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
  expect "Attached(RW)" (function x -> x.superstate = Vdi_automaton.Attached Vdi_automaton.RW)
    (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
  expect "()" (fun x -> x = ())
    (Client.VDI.detach ~dbg ~dp:dp1 ~sr ~vdi);
  (* dp1: Detached dp1: Detached superstate: Detached *)
  expect "Detached" (function x -> x.superstate = Vdi_automaton.Detached)
    (Client.DP.stat_vdi ~dbg ~sr ~vdi ());
  expect "()" (fun x -> x = ())
    (Client.SR.detach ~dbg ~sr)

(* Manual cleanup with VDI.detach (fails) and then SR.detach (succeeds) *)
let test_sr_detach_cleanup_errors_1 sr vdi =
  Debug_print_impl.VDI.working := false;
  expect "()" (fun x -> x = ())
    (Client.SR.attach ~dbg ~sr ~device_config:[]);
  let dp = datapath_of_id "datapath" in
  leak dp sr true vdi;
  expect "()" (fun x -> x = ())
    (Client.VDI.deactivate ~dbg ~dp ~sr ~vdi);
  if vdi = "error2"
  then expect "backend_error in test_sr_detach_cleanup" backend_error
      (fun () -> Client.VDI.detach ~dbg ~dp ~sr ~vdi)
  else expect "internal_error in test sr detach cleanup" internal_error
      (fun () -> Client.VDI.detach ~dbg ~dp ~sr ~vdi);
  debug "Detaching and cleaning up";
  Debug_print_impl.VDI.working := true;
  (* Should succeed because the VDI.attach state will have been forgotten: FH2 *)
  expect "()" (fun x -> x = ())
    (Client.SR.detach ~dbg ~sr)

let test_sr_detach_cleanup_errors_2 sr vdi =
  Debug_print_impl.VDI.working := false;
  expect "()" (fun x -> x = ())
    (Client.SR.attach ~dbg ~sr ~device_config:[]);
  let dp = datapath_of_id "datapath" in
  leak dp sr true vdi;
  if vdi = "error2"
  then expect "backend_error" backend_error
      (fun () -> Client.DP.destroy ~dbg ~dp ~allow_leak:false)
  else expect "internal_error" internal_error
      (fun () -> Client.DP.destroy ~dbg ~dp ~allow_leak:false);
  Debug_print_impl.VDI.working := true;
  debug "Attempting to attach RO (having failed a detach of a RW detach)";
  expect "Params _" (function _ -> true)
    (Client.VDI.attach ~dbg ~dp ~sr ~vdi ~read_write:false);
  debug "Detaching and cleaning up";
  expect "()" (fun x -> x = ())
    (Client.SR.detach ~dbg ~sr)

let create_vdi_test sr =
  let dp = datapath_of_id "datapath" in
  expect "()" (fun x -> x = ())
    (Client.SR.attach ~dbg ~sr ~device_config:[]);
  let vdi_info = {
    vdi = "";
    uuid = None;
    content_id = "";
    name_label = "name_label";
    name_description = "name_description";
    virtual_size = 10L;
    ty = "user";
    is_a_snapshot = false;
    snapshot_time = "";
    snapshot_of = "";
    read_only = false;
    cbt_enabled = false;
    physical_utilisation = 10L;
    metadata_of_pool = "";
    persistent = true;
    sm_config = [];
  } in
  expect "too_small_backend_error" too_small_backend_error
    (fun () ->
       let vdi_info = { vdi_info with sm_config = ["toosmall", ""] } in
       Client.VDI.create ~dbg ~sr ~vdi_info);
  let vdi = Client.VDI.create ~dbg ~sr ~vdi_info in
  expect "attach_info" (fun _ -> true)
    (Client.VDI.attach ~dbg ~dp ~sr ~vdi:vdi.vdi ~read_write:false);
  debug "Detaching and cleaning up";
  expect "()" (fun x -> x = ())
    (Client.SR.detach ~dbg ~sr)

let _ =
  Storage_impl.print_debug := true;
  Storage_impl.host_state_path := "/tmp/storage.db";
  Vdi_automaton.test ();
  Unixext.unlink_safe !Storage_impl.host_state_path;
  let s = Xcp_service.make ~path:Xapi_globs.storage_unix_domain_socket ~queue_name:"org.xen.xapi.storage" ~rpc_fn:(Server.process None) () in
  info "Started service on org.xen.xapi.storage";
  let (_: Thread.t) = Thread.create (fun () -> Xcp_service.serve_forever s) () in

  info "Listening on %s" Xapi_globs.storage_unix_domain_socket;
  test_sr "sr";

  test_sr_detach_cleanup_errors_1 "sr" "error2";
  test_sr_detach_cleanup_errors_1 "sr" "error";

  test_sr_detach_cleanup_errors_2 "sr" "error2";
  test_sr_detach_cleanup_errors_2 "sr" "error";

  test_stat "sr" "vdi";

  create_vdi_test "sr";

  if !total_errors = 0 then begin
    info "OK";
    exit 0;
  end else begin
    info "%d errors detected" !total_errors;
    exit 1
  end

