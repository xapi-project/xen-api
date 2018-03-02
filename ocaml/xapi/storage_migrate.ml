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

module D=Debug.Make(struct let name="storage_migrate" end)
open D

module SMPERF=Debug.Make(struct let name="SMPERF" end)

open Stdext
open Listext
open Fun
open Xstringext
open Pervasiveext
open Xmlrpc_client
open Threadext

open Storage_interface
open Storage_task
open Storage_utils

let local_url () = Http.Url.(Http { host="127.0.0.1"; auth=None; port=None; ssl=false }, { uri = Constants.sm_uri; query_params=["pool_secret",!Xapi_globs.pool_secret] } )

module State = struct

  module Receive_state = struct
    type t = {
      sr : sr;
      dummy_vdi : vdi;
      leaf_vdi : vdi;
      leaf_dp : dp;
      parent_vdi : vdi;
      remote_vdi : vdi;
    } [@@deriving rpc]
  end

  module Send_state = struct
    type t = {
      url : string;
      dest_sr : sr;
      remote_dp : dp;
      local_dp : dp;
      mirror_vdi : vdi;
      remote_url : string;
      tapdev : Tapctl.tapdev;
      mutable failed : bool;
      mutable watchdog : Scheduler.handle option;
    } [@@deriving rpc]
  end

  module Copy_state = struct
    type t = {
      base_dp : dp;
      leaf_dp : dp;
      remote_dp : dp;
      dest_sr: sr;
      copy_vdi: vdi;
      remote_url : string;
    } [@@deriving rpc]
  end

  let loaded = ref false
  let mutex = Mutex.create ()

  type send_table = (string, Send_state.t) Hashtbl.t
  type recv_table = (string, Receive_state.t) Hashtbl.t
  type copy_table = (string, Copy_state.t) Hashtbl.t

  type osend
  type orecv
  type ocopy

  type _ operation =
    | Send_op : Send_state.t -> osend operation
    | Recv_op : Receive_state.t -> orecv operation
    | Copy_op : Copy_state.t -> ocopy operation

  type _ table =
    | Send_table : send_table -> osend table
    | Recv_table : recv_table -> orecv table
    | Copy_table : copy_table -> ocopy table

  let active_send : send_table = Hashtbl.create 10
  let active_recv : recv_table = Hashtbl.create 10
  let active_copy : copy_table = Hashtbl.create 10

  let table_of_op : type a. a operation -> a table = function
    | Send_op _ -> Send_table active_send
    | Recv_op _ -> Recv_table active_recv
    | Copy_op _ -> Copy_table active_copy

  let persist_root = ref "/var/run/nonpersistent"
  let path_of_table : type a. a table -> string = function
    | Send_table _ -> Filename.concat !persist_root "storage_mirrors_send.json"
    | Recv_table _ -> Filename.concat !persist_root "storage_mirrors_recv.json"
    | Copy_table _ -> Filename.concat !persist_root "storage_mirrors_copy.json"

  let rpc_of_table : type a. a table -> Rpc.t =
    let open Rpc_std_helpers in
    function
    | Send_table send_table -> rpc_of_hashtbl ~rpc_of:Send_state.rpc_of_t send_table
    | Recv_table recv_table -> rpc_of_hashtbl ~rpc_of:Receive_state.rpc_of_t recv_table
    | Copy_table copy_table -> rpc_of_hashtbl ~rpc_of:Copy_state.rpc_of_t copy_table

  let to_string : type a. a table -> string =
    (fun table -> rpc_of_table table |> Jsonrpc.to_string)

  let rpc_of_path path =
    Unixext.string_of_file path |> Jsonrpc.of_string

  let load_one : type a. a table -> unit = (fun table ->
      let rpc = path_of_table table |> rpc_of_path in
      let open Rpc_std_helpers in
      match table with
      | Send_table table ->
        Hashtbl.iter (Hashtbl.replace table) (hashtbl_of_rpc ~of_rpc:Send_state.t_of_rpc rpc)
      | Recv_table table ->
        Hashtbl.iter (Hashtbl.replace table) (hashtbl_of_rpc ~of_rpc:Receive_state.t_of_rpc rpc)
      | Copy_table table ->
        Hashtbl.iter (Hashtbl.replace table) (hashtbl_of_rpc ~of_rpc:Copy_state.t_of_rpc rpc))

  let load () =
    try load_one (Send_table active_send) with _ -> ();
      try load_one (Recv_table active_recv) with _ -> ();
        try load_one (Copy_table active_copy) with _ -> ();
          loaded := true

  let save_one : type a. a table -> unit = (fun table ->
      to_string table |> Unixext.write_string_to_file (path_of_table table))

  let save () =
    Unixext.mkdir_rec !persist_root 0o700;
    save_one (Send_table active_send);
    save_one (Recv_table active_recv);
    save_one (Copy_table active_copy)

  let access_table ~save_after f table =
    Mutex.execute mutex
      (fun () ->
         if not !loaded then load ();
         let result = f table in
         if save_after then save ();
         result)

  let map_of () =
    let contents_of table = Hashtbl.fold (fun k v acc -> (k,v)::acc) table [] in
    let send_ops = access_table ~save_after:false contents_of active_send in
    let recv_ops = access_table ~save_after:false contents_of active_recv in
    let copy_ops = access_table ~save_after:false contents_of active_copy in
    send_ops, recv_ops, copy_ops

  let add : type a. string -> a operation -> unit = fun id op ->
    let add' : type a. string -> a operation -> a table -> unit = fun id op table ->
      match (table, op) with
      | Send_table table, Send_op op -> Hashtbl.replace table id op
      | Recv_table table, Recv_op op -> Hashtbl.replace table id op
      | Copy_table table, Copy_op op -> Hashtbl.replace table id op
    in
    access_table ~save_after:true
      (fun table -> add' id op table) (table_of_op op)

  let find id table =
    access_table ~save_after:false
      (fun table -> try Some (Hashtbl.find table id) with Not_found -> None)
      table

  let remove id table =
    access_table ~save_after:true
      (fun table -> Hashtbl.remove table id)
      table

  let clear () =
    access_table ~save_after:true (fun table -> Hashtbl.clear table) active_send;
    access_table ~save_after:true (fun table -> Hashtbl.clear table) active_recv;
    access_table ~save_after:true (fun table -> Hashtbl.clear table) active_copy

  let remove_local_mirror id = remove id active_send
  let remove_receive_mirror id = remove id active_recv
  let remove_copy id = remove id active_copy

  let find_active_local_mirror id = find id active_send
  let find_active_receive_mirror id = find id active_recv
  let find_active_copy id = find id active_copy

  let mirror_id_of (sr,vdi) = Printf.sprintf "%s/%s" sr vdi
  let of_mirror_id id = match String.split '/' id with
    | sr::rest -> (sr,String.concat "/" rest)
    | _ -> failwith "Bad id"
  let copy_id_of (sr,vdi) = Printf.sprintf "copy/%s/%s" sr vdi
  let of_copy_id id =
    match String.split '/' id with
    | op :: sr :: rest when op="copy" -> (sr,(String.concat "/" rest))
    | _ -> failwith "Bad id"
end

(* We are making an RPC to a host that is potentially part of a different pool.
   It can tell us that we need to send the RPC elsewhere instead (e.g. to its master),
   so use the [redirectable_rpc] helper here *)
let rpc ~srcstr ~dststr url =
  let remote_url_of_ip ip =
    let open Http.Url in
    match url with
    | (Http h, d) ->
       (Http {h with host=ip}, d)
    | _ ->
       remote_url ip
  in
  let local_fn = Helpers.make_remote_rpc_of_url ~srcstr ~dststr url in
  Storage_utils.redirectable_rpc ~srcstr ~dststr ~remote_url_of_ip ~local_fn

let vdi_info x =
  match x with
  | Some (Vdi_info v) -> v
  | _ -> failwith "Runtime type error: expecting Vdi_info"

module Local = Client(struct let rpc call = rpc ~srcstr:"smapiv2" ~dststr:"smapiv2" (local_url ()) call end)

let tapdisk_of_attach_info attach_info =
  let path = attach_info.params in
  try
    match Tapctl.of_device (Tapctl.create ()) path with
    | tapdev, _, _ -> Some tapdev
  with Tapctl.Not_blktap ->
    debug "Device %s is not controlled by blktap" path;
    None
     | Tapctl.Not_a_device ->
       debug "%s is not a device" path;
       None
     | _ ->
       debug "Device %s has an unknown driver" path;
       None


let with_activated_disk ~dbg ~sr ~vdi ~dp f =
  let path =
    Opt.map (fun vdi ->
        let attach_info = Local.VDI.attach ~dbg ~dp ~sr ~vdi ~read_write:false in
        let path = attach_info.params in
        Local.VDI.activate ~dbg ~dp ~sr ~vdi;
        path) vdi in
  finally
    (fun () -> f path)
    (fun () ->
       Opt.iter
         (fun vdi ->
            Local.VDI.deactivate ~dbg ~dp ~sr ~vdi;
            Local.VDI.detach ~dbg ~dp ~sr ~vdi)
         vdi)

let perform_cleanup_actions =
  List.iter
    (fun f ->
       try f () with e -> error "Caught %s while performing cleanup actions" (Printexc.to_string e)
    )

let progress_callback start len t y =
  let new_progress = start +. (y *. len) in
  Storage_task.set_state t (Task.Pending new_progress);
  signal (Storage_task.id_of_handle t)

let copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi =
  let remote_url = Http.Url.of_string url in
  let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
  debug "copy local=%s/%s url=%s remote=%s/%s" sr vdi url dest dest_vdi;

  (* Check the remote SR exists *)
  let srs = Remote.SR.list ~dbg in
  if not(List.mem dest srs)
  then failwith (Printf.sprintf "Remote SR %s not found" dest);

  let vdis = Remote.SR.scan ~dbg ~sr:dest in
  let remote_vdi =
    try List.find (fun x -> x.vdi = dest_vdi) vdis
    with Not_found -> failwith (Printf.sprintf "Remote VDI %s not found" dest_vdi)
  in

  let dest_content_id = remote_vdi.content_id in

  (* Find the local VDI *)
  let vdis = Local.SR.scan ~dbg ~sr in
  let local_vdi =
    try List.find (fun x -> x.vdi = vdi) vdis
    with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

  debug "copy local=%s/%s content_id=%s" sr vdi local_vdi.content_id;
  debug "copy remote=%s/%s content_id=%s" dest dest_vdi remote_vdi.content_id;

  if local_vdi.virtual_size > remote_vdi.virtual_size then begin
    (* This should never happen provided the higher-level logic is working properly *)
    error "copy local=%s/%s virtual_size=%Ld > remote=%s/%s virtual_size = %Ld" sr vdi local_vdi.virtual_size dest dest_vdi remote_vdi.virtual_size;
    failwith "local VDI is larger than the remote VDI";
  end;

  let on_fail : (unit -> unit) list ref = ref [] in

  let base_vdi =
    try
      let x = (List.find (fun x -> x.content_id = dest_content_id) vdis).vdi in
      debug "local VDI %s has content_id = %s; we will perform an incremental copy" x dest_content_id;
      Some x
    with _ ->
      debug "no local VDI has content_id = %s; we will perform a full copy" dest_content_id;
      None
  in


  try
    let remote_dp = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    let base_dp = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    let leaf_dp = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    let dest_vdi_url = Http.Url.set_uri remote_url (Printf.sprintf "%s/nbd/%s/%s/%s" (Http.Url.get_uri remote_url) dest dest_vdi remote_dp) |> Http.Url.to_string in

    debug "copy remote=%s/%s NBD URL = %s" dest dest_vdi dest_vdi_url;

    let id=State.copy_id_of (sr,vdi) in
    debug "Persisting state for copy (id=%s)" id;
    State.add id State.(Copy_op Copy_state.({
        base_dp; leaf_dp; remote_dp; dest_sr=dest; copy_vdi=remote_vdi.vdi; remote_url=url}));

    SMPERF.debug "mirror.copy: copy initiated local_vdi:%s dest_vdi:%s" vdi dest_vdi;

    Pervasiveext.finally (fun () ->
        debug "activating RW datapath %s on remote=%s/%s" remote_dp dest dest_vdi;
        ignore(Remote.VDI.attach ~dbg ~sr:dest ~vdi:dest_vdi ~dp:remote_dp ~read_write:true);
        Remote.VDI.activate ~dbg ~dp:remote_dp ~sr:dest ~vdi:dest_vdi;

        with_activated_disk ~dbg ~sr ~vdi:base_vdi ~dp:base_dp
          (fun base_path ->
             with_activated_disk ~dbg ~sr ~vdi:(Some vdi) ~dp:leaf_dp
               (fun src ->
                  let dd = Sparse_dd_wrapper.start ~progress_cb:(progress_callback 0.05 0.9 task) ?base:base_path true (Opt.unbox src)
                      dest_vdi_url remote_vdi.virtual_size in
                  Storage_task.with_cancel task
                    (fun () -> Sparse_dd_wrapper.cancel dd)
                    (fun () ->
                       try Sparse_dd_wrapper.wait dd
                       with Sparse_dd_wrapper.Cancelled -> Storage_task.raise_cancelled task)
               )
          );
      )
      (fun () ->
         Remote.DP.destroy ~dbg ~dp:remote_dp ~allow_leak:false;
         State.remove_copy id
      );

    SMPERF.debug "mirror.copy: copy complete local_vdi:%s dest_vdi:%s" vdi dest_vdi;

    debug "setting remote=%s/%s content_id <- %s" dest dest_vdi local_vdi.content_id;
    Remote.VDI.set_content_id ~dbg ~sr:dest ~vdi:dest_vdi ~content_id:local_vdi.content_id;
    (* PR-1255: XXX: this is useful because we don't have content_ids by default *)
    debug "setting local=%s/%s content_id <- %s" sr local_vdi.vdi local_vdi.content_id;
    Local.VDI.set_content_id ~dbg ~sr ~vdi:local_vdi.vdi ~content_id:local_vdi.content_id;
    Some (Vdi_info remote_vdi)
  with e ->
    error "Caught %s: performing cleanup actions" (Printexc.to_string e);
    perform_cleanup_actions !on_fail;
    raise e


let copy_into ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi =
  copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi

let remove_from_sm_config vdi_info key =
  { vdi_info with sm_config = List.filter (fun (k,v) -> k <> key) vdi_info.sm_config }

let add_to_sm_config vdi_info key value =
  let vdi_info = remove_from_sm_config vdi_info key in
  { vdi_info with sm_config = (key,value) :: vdi_info.sm_config }

let stop ~dbg ~id =
  (* Find the local VDI *)
  let alm = State.find_active_local_mirror id in
  match alm with
  | Some alm ->
    let sr,vdi = State.of_mirror_id id in
    let vdis = Local.SR.scan ~dbg ~sr in
    let local_vdi =
      try List.find (fun x -> x.vdi = vdi) vdis
      with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
    let local_vdi = add_to_sm_config local_vdi "mirror" "null" in
    let local_vdi = remove_from_sm_config local_vdi "base_mirror" in
    (* Disable mirroring on the local machine *)
    let snapshot = Local.VDI.snapshot ~dbg ~sr ~vdi_info:local_vdi in
    Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi;
    (* Destroy the snapshot, if it still exists *)
    let snap = try Some (List.find (fun x -> List.mem_assoc "base_mirror" x.sm_config && List.assoc "base_mirror" x.sm_config = id) vdis) with _ -> None in
    begin
      match snap with
      | Some s ->
        debug "Found snapshot VDI: %s" s.vdi;
        Local.VDI.destroy ~dbg ~sr ~vdi:s.vdi
      | None ->
        debug "Snapshot VDI already cleaned up"
    end;
    let remote_url = Http.Url.of_string alm.State.Send_state.remote_url in
    let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
    (try Remote.DATA.MIRROR.receive_cancel ~dbg ~id with _ -> ());
    State.remove_local_mirror id
  | None ->
    raise (Does_not_exist ("mirror",id))

let start' ~task ~dbg ~sr ~vdi ~dp ~url ~dest =
  debug "Mirror.start sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
  SMPERF.debug "mirror.start called sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
  let remote_url = Http.Url.of_string url in
  let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in

  (* Find the local VDI *)
  let vdis = Local.SR.scan ~dbg ~sr in
  let local_vdi =
    try List.find (fun x -> x.vdi = vdi) vdis
    with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in

  let id = State.mirror_id_of (sr,local_vdi.vdi) in

  (* A list of cleanup actions to perform if the operation should fail. *)
  let on_fail : (unit -> unit) list ref = ref [] in
  try
    let similar_vdis = Local.VDI.similar_content ~dbg ~sr ~vdi in
    let similars = List.filter (fun x -> x <> "") (List.map (fun vdi -> vdi.content_id) similar_vdis) in
    debug "Similar VDIs to %s = [ %s ]" vdi (String.concat "; " (List.map (fun x -> Printf.sprintf "(vdi=%s,content_id=%s)" x.vdi x.content_id) similar_vdis));
    let result_ty = Remote.DATA.MIRROR.receive_start ~dbg ~sr:dest ~vdi_info:local_vdi ~id ~similar:similars in
    let result = match result_ty with
        Mirror.Vhd_mirror x -> x
    in

    (* Enable mirroring on the local machine *)
    let mirror_dp = result.Mirror.mirror_datapath in

    let uri = (Printf.sprintf "/services/SM/nbd/%s/%s/%s" dest result.Mirror.mirror_vdi.vdi mirror_dp) in
    let dest_url = Http.Url.set_uri remote_url uri in
    let request = Http.Request.make ~query:(Http.Url.get_query_params dest_url) ~version:"1.0" ~user_agent:"smapiv2" Http.Put uri in
    let transport = Xmlrpc_client.transport_of_url dest_url in
    debug "Searching for data path: %s" dp;
    let attach_info = Local.DP.attach_info ~dbg:"nbd" ~sr ~vdi ~dp in
    debug "Got it!";
    on_fail := (fun () -> Remote.DATA.MIRROR.receive_cancel ~dbg ~id) :: !on_fail;

    let tapdev = match tapdisk_of_attach_info attach_info with
      | Some tapdev ->
        debug "Got tapdev";
        let pid = Tapctl.get_tapdisk_pid tapdev in
        let path = Printf.sprintf "/var/run/blktap-control/nbdclient%d" pid in
        with_transport transport (with_http request (fun (response, s) ->
            debug "Here inside the with_transport";
            let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
            finally
              (fun () ->
                 debug "Connecting to path: %s" path;
                 Unix.connect control_fd (Unix.ADDR_UNIX path);
                 let msg = dp in
                 let len = String.length msg in
                 let written = Unixext.send_fd control_fd msg 0 len [] s in
                 debug "Sent fd";
                 if written <> len then begin
                   error "Failed to transfer fd to %s" path;
                   failwith "foo"
                 end)
              (fun () ->
                 Unix.close control_fd)));
        tapdev
      | None ->
        failwith "Not attached"
    in
    debug "Adding to active local mirrors: id=%s" id;
    let alm = State.Send_state.({
        url;
        dest_sr=dest;
        remote_dp=mirror_dp;
        local_dp=dp;
        mirror_vdi=result.Mirror.mirror_vdi.vdi;
        remote_url=url;
        tapdev;
        failed=false;
        watchdog=None}) in
    State.add id (State.Send_op alm);
    debug "Added";

    debug "About to snapshot VDI = %s" (string_of_vdi_info local_vdi);
    let local_vdi = add_to_sm_config local_vdi "mirror" ("nbd:" ^ dp) in
    let local_vdi = add_to_sm_config local_vdi "base_mirror" id in
    let snapshot =
    try
      Local.VDI.snapshot ~dbg ~sr ~vdi_info:local_vdi
    with
    | Storage_interface.Backend_error(code, _) when code = "SR_BACKEND_FAILURE_44" ->
      raise (Api_errors.Server_error(Api_errors.sr_source_space_insufficient, [ sr ]))
    | e ->
      raise e
    in
    debug "Done!";

    SMPERF.debug "mirror.start: snapshot created, mirror initiated vdi:%s snapshot_of:%s"
      snapshot.vdi local_vdi.vdi ;

    on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi) :: !on_fail;

    begin
      let rec inner () =
        debug "tapdisk watchdog";
        let alm_opt = State.find_active_local_mirror id in
        match alm_opt with
        | Some alm ->
          let stats = Tapctl.stats (Tapctl.create ()) tapdev in
          if stats.Tapctl.Stats.nbd_mirror_failed = 1 then
            Updates.add (Dynamic.Mirror id) updates;
          alm.State.Send_state.watchdog <- Some (Scheduler.one_shot scheduler (Scheduler.Delta 5) "tapdisk_watchdog" inner)
        | None -> ()
      in inner ()
    end;

    on_fail := (fun () -> stop ~dbg ~id) :: !on_fail;
    (* Copy the snapshot to the remote *)
    let new_parent = Storage_task.with_subtask task "copy" (fun () ->
        copy' ~task ~dbg ~sr ~vdi:snapshot.vdi ~url ~dest ~dest_vdi:result.Mirror.copy_diffs_to) |> vdi_info in
    debug "Local VDI %s == remote VDI %s" snapshot.vdi new_parent.vdi;
    Remote.VDI.compose ~dbg ~sr:dest ~vdi1:result.Mirror.copy_diffs_to ~vdi2:result.Mirror.mirror_vdi.vdi;
    Remote.VDI.remove_from_sm_config ~dbg ~sr:dest ~vdi:result.Mirror.mirror_vdi.vdi ~key:"base_mirror";
    debug "Local VDI %s now mirrored to remote VDI: %s" local_vdi.vdi result.Mirror.mirror_vdi.vdi;

    debug "Destroying dummy VDI %s on remote" result.Mirror.dummy_vdi;
    Remote.VDI.destroy ~dbg ~sr:dest ~vdi:result.Mirror.dummy_vdi;
    debug "Destroying snapshot %s on src" snapshot.vdi;
    Local.VDI.destroy ~dbg ~sr ~vdi:snapshot.vdi;

    Some (Mirror_id id)
  with
  | Sr_not_attached(sr_uuid) ->
    error " Caught exception %s:%s. Performing cleanup." Api_errors.sr_not_attached sr_uuid;
    perform_cleanup_actions !on_fail;
    raise (Api_errors.Server_error(Api_errors.sr_not_attached,[sr_uuid]))
  | e ->
    error "Caught %s: performing cleanup actions" (Api_errors.to_string e);
    perform_cleanup_actions !on_fail;
    raise e


(* XXX: PR-1255: copy the xenopsd 'raise Exception' pattern *)
let stop ~dbg ~id =
  try
    stop ~dbg ~id
  with
  | Backend_error(code, params)
  | Api_errors.Server_error(code, params) ->
    raise (Backend_error(code, params))
  | e ->
    raise e

let stat ~dbg ~id =
  let recv_opt = State.find_active_receive_mirror id in
  let send_opt = State.find_active_local_mirror id in
  let copy_opt = State.find_active_copy id in
  let open State in
  let failed = match send_opt with
    | Some send_state ->
      let failed =
        try
          let stats = Tapctl.stats (Tapctl.create ()) send_state.Send_state.tapdev in
          stats.Tapctl.Stats.nbd_mirror_failed = 1
        with e ->
          debug "Using cached copy of failure status";
          send_state.Send_state.failed
      in
      send_state.Send_state.failed <- failed;
      failed
    | None -> false
  in
  let open Mirror in
  let state =
    (match recv_opt with Some _ -> [Receiving] | None -> []) @
    (match send_opt with Some _ -> [Sending]   | None -> []) @
    (match copy_opt with Some _ -> [Copying]   | None -> [])
  in
  if state = [] then raise (Does_not_exist ("mirror", id));
  let src, dst = match (recv_opt, send_opt, copy_opt) with
    | (Some receive_state, _, _) ->
      receive_state.Receive_state.remote_vdi, receive_state.Receive_state.leaf_vdi
    | (_, Some send_state, _) ->
      snd (of_mirror_id id), send_state.Send_state.mirror_vdi
    | (_, _, Some copy_state) ->
      snd (of_copy_id id), copy_state.Copy_state.copy_vdi
    | _ -> failwith "Invalid" in
  { Mirror.source_vdi = src; dest_vdi = dst; state; failed; }

let list ~dbg =
  let send_ops, recv_ops, copy_ops = State.map_of () in
  let get_ids map = List.map fst map in
  let ids =
    (get_ids send_ops) @ (get_ids recv_ops) @ (get_ids copy_ops)
    |> Listext.List.setify
  in
  List.map (fun id ->
      (id,stat dbg id)) ids

let killall ~dbg =
  let send_ops, recv_ops, copy_ops = State.map_of () in
  List.iter
    (fun (id, send_state) ->
       begin
         debug "Send in progress: %s" id;
         List.iter log_and_ignore_exn
           [ (fun () -> stop dbg id);
             (fun () -> Local.DP.destroy ~dbg ~dp:send_state.State.Send_state.local_dp ~allow_leak:true) ]
       end)
    send_ops;
  List.iter
    (fun (id, copy_state) ->
       begin
         debug "Copy in progress: %s" id;
         List.iter log_and_ignore_exn
           [ (fun () -> Local.DP.destroy ~dbg ~dp:copy_state.State.Copy_state.leaf_dp ~allow_leak:true);
             (fun () -> Local.DP.destroy ~dbg ~dp:copy_state.State.Copy_state.base_dp ~allow_leak:true) ];
         let remote_url = Http.Url.of_string copy_state.State.Copy_state.remote_url in
         let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
         List.iter log_and_ignore_exn
           [ (fun () -> Remote.DP.destroy ~dbg ~dp:copy_state.State.Copy_state.remote_dp ~allow_leak:true);
             (fun () -> Remote.VDI.destroy ~dbg ~sr:copy_state.State.Copy_state.dest_sr ~vdi:copy_state.State.Copy_state.copy_vdi) ]
       end)
    copy_ops;
  List.iter
    (fun (id, recv_state) ->
       begin
         debug "Receive in progress: %s" id;
         log_and_ignore_exn (fun () -> Local.DATA.MIRROR.receive_cancel ~dbg ~id)
       end)
    recv_ops;
  State.clear ()

let receive_start ~dbg ~sr ~vdi_info ~id ~similar =
  let on_fail : (unit -> unit) list ref = ref [] in

  let vdis = Local.SR.scan ~dbg ~sr in
  (** We drop cbt_metadata VDIs that do not have any actual data *)
  let vdis = List.filter (fun vdi -> vdi.ty <> "cbt_metadata") vdis in

  let leaf_dp = Local.DP.create ~dbg ~id:(Uuid.string_of_uuid (Uuid.make_uuid ())) in

  try
    let vdi_info = { vdi_info with sm_config = ["base_mirror", id] } in
    let leaf = Local.VDI.create ~dbg ~sr ~vdi_info in
    info "Created leaf VDI for mirror receive: %s" (string_of_vdi_info leaf);
    on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:leaf.vdi) :: !on_fail;
    let dummy = Local.VDI.snapshot ~dbg ~sr ~vdi_info:leaf in
    on_fail := (fun () -> Local.VDI.destroy ~dbg ~sr ~vdi:dummy.vdi) :: !on_fail;
    debug "Created dummy snapshot for mirror receive: %s" (string_of_vdi_info dummy);

    let _ = Local.VDI.attach ~dbg ~dp:leaf_dp ~sr ~vdi:leaf.vdi ~read_write:true in
    Local.VDI.activate ~dbg ~dp:leaf_dp ~sr ~vdi:leaf.vdi;

    let nearest = List.fold_left
        (fun acc content_id -> match acc with
           | Some x -> acc
           | None ->
             try Some (List.find (fun vdi -> vdi.content_id = content_id && vdi.virtual_size <= vdi_info.virtual_size) vdis)
             with Not_found -> None) None similar in

    debug "Nearest VDI: content_id=%s vdi=%s"
      (Opt.default "None" (Opt.map (fun x -> x.content_id) nearest))
      (Opt.default "None" (Opt.map (fun x -> x.vdi) nearest));

    let parent = match nearest with
      | Some vdi ->
        debug "Cloning VDI %s" vdi.vdi;
        let vdi = add_to_sm_config vdi "base_mirror" id in
        let vdi_clone = Local.VDI.clone ~dbg ~sr ~vdi_info:vdi in
        if vdi_clone.virtual_size <> vdi_info.virtual_size then begin
          let new_size = Local.VDI.resize ~dbg ~sr ~vdi:vdi_clone.vdi ~new_size:vdi_info.virtual_size in
          debug "Resize local VDI %s to %Ld: result %Ld" vdi_clone.vdi vdi_info.virtual_size new_size;
        end;
        vdi_clone
      | None ->
        debug "Creating a blank remote VDI";
        Local.VDI.create ~dbg ~sr ~vdi_info
    in

    debug "Parent disk content_id=%s" parent.content_id;

    State.add id State.(Recv_op Receive_state.({
        sr;
        dummy_vdi=dummy.vdi;
        leaf_vdi=leaf.vdi;
        leaf_dp;
        parent_vdi=parent.vdi;
        remote_vdi=vdi_info.vdi}));

    let nearest_content_id = Opt.map (fun x -> x.content_id) nearest in

    Mirror.Vhd_mirror {
      Mirror.mirror_vdi = leaf;
      mirror_datapath = leaf_dp;
      copy_diffs_from = nearest_content_id;
      copy_diffs_to = parent.vdi;
      dummy_vdi = dummy.vdi }
  with e ->
    List.iter (fun op -> try op () with e -> debug "Caught exception in on_fail: %s" (Printexc.to_string e)) !on_fail;
    raise e

let receive_finalize ~dbg ~id =
  let recv_state = State.find_active_receive_mirror id in
  let open State.Receive_state in Opt.iter (fun r -> Local.DP.destroy ~dbg ~dp:r.leaf_dp ~allow_leak:false) recv_state;
  State.remove_receive_mirror id

let receive_cancel ~dbg ~id =
  let receive_state = State.find_active_receive_mirror id in
  let open State.Receive_state in Opt.iter (fun r ->
      log_and_ignore_exn (fun () -> Local.DP.destroy ~dbg ~dp:r.leaf_dp ~allow_leak:false);
      List.iter (fun v ->
          log_and_ignore_exn (fun () -> Local.VDI.destroy ~dbg ~sr:r.sr ~vdi:v)
        ) [r.dummy_vdi; r.leaf_vdi; r.parent_vdi]
    ) receive_state;
  State.remove_receive_mirror id

exception Timeout
let reqs_outstanding_timeout = 150.0 (* Tapdisk should time out after 2 mins. We can wait a little longer *)

let pre_deactivate_hook ~dbg ~dp ~sr ~vdi =
  let open State.Send_state in
  let id = State.mirror_id_of (sr,vdi) in
  let start = Mtime_clock.counter () in
  let get_delta () = Mtime_clock.count start |> Mtime.Span.to_s in
  State.find_active_local_mirror id |>
  Opt.iter (fun s ->
      try
        (* We used to pause here and then check the nbd_mirror_failed key. Now, we poll
           					   until the number of outstanding requests has gone to zero, then check the
           					   status. This avoids confusing the backend (CA-128460) *)
        let open Tapctl in
        let ctx = create () in
        let rec wait () =
          if get_delta () > reqs_outstanding_timeout then raise Timeout;
          let st = stats ctx s.tapdev in
          if st.Stats.reqs_outstanding > 0
          then (Thread.delay 1.0; wait ())
          else st
        in
        let st = wait () in
        debug "Got final stats after waiting %f seconds" (get_delta ());
        if st.Stats.nbd_mirror_failed = 1
        then begin
          error "tapdisk reports mirroring failed";
          s.failed <- true
        end;
      with
      | Timeout ->
        error "Timeout out after %f seconds waiting for tapdisk to complete all outstanding requests" (get_delta ());
        s.failed <- true
      | e ->
        error "Caught exception while finally checking mirror state: %s"
          (Printexc.to_string e);
        s.failed <- true
    )

let post_detach_hook ~sr ~vdi ~dp =
  let open State.Send_state in
  let id = State.mirror_id_of (sr,vdi) in
  State.find_active_local_mirror id |>
  Opt.iter (fun r ->
      let remote_url = Http.Url.of_string r.url in
      let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
      let t = Thread.create (fun () ->
          debug "Calling receive_finalize";
          log_and_ignore_exn
            (fun () -> Remote.DATA.MIRROR.receive_finalize ~dbg:"Mirror-cleanup" ~id);
          debug "Finished calling receive_finalize";
          State.remove_local_mirror id;
          debug "Removed active local mirror: %s" id
        ) () in
      Opt.iter (fun id -> Scheduler.cancel scheduler id) r.watchdog;
      debug "Created thread %d to call receive finalize and dp destroy" (Thread.id t))

let nbd_handler req s sr vdi dp =
  debug "sr=%s vdi=%s dp=%s" sr vdi dp;
  let attach_info = Local.DP.attach_info ~dbg:"nbd" ~sr ~vdi ~dp in
  req.Http.Request.close <- true;
  match tapdisk_of_attach_info attach_info with
  | Some tapdev ->
    let minor = Tapctl.get_minor tapdev in
    let pid = Tapctl.get_tapdisk_pid tapdev in
    let path = Printf.sprintf "/var/run/blktap-control/nbdserver%d.%d" pid minor in
    Http_svr.headers s (Http.http_200_ok () @ ["Transfer-encoding: nbd"]);
    let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    finally
      (fun () ->
         Unix.connect control_fd (Unix.ADDR_UNIX path);
         let msg = dp in
         let len = String.length msg in
         let written = Unixext.send_fd control_fd msg 0 len [] s in
         if written <> len then begin
           error "Failed to transfer fd to %s" path;
           Http_svr.headers s (Http.http_404_missing ~version:"1.0" ());
           req.Http.Request.close <- true
         end;
      )
      (fun () -> Unix.close control_fd)
  | None ->
    ()

let copy ~task ~dbg ~sr ~vdi ~dp ~url ~dest =
  debug "copy sr:%s vdi:%s url:%s dest:%s" sr vdi url dest;
  let remote_url = Http.Url.of_string url in
  let module Remote = Client(struct let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url end) in
  try
    (* Find the local VDI *)
    let vdis = Local.SR.scan ~dbg ~sr in
    let local_vdi =
      try List.find (fun x -> x.vdi = vdi) vdis
      with Not_found -> failwith (Printf.sprintf "Local VDI %s not found" vdi) in
    try
      let similar_vdis = Local.VDI.similar_content ~dbg ~sr ~vdi in
      let similars = List.map (fun vdi -> vdi.content_id) similar_vdis in
      debug "Similar VDIs to %s = [ %s ]" vdi (String.concat "; " (List.map (fun x -> Printf.sprintf "(vdi=%s,content_id=%s)" x.vdi x.content_id) similar_vdis));
      let remote_vdis = Remote.SR.scan ~dbg ~sr:dest in
      (** We drop cbt_metadata VDIs that do not have any actual data *)
      let remote_vdis = List.filter (fun vdi -> vdi.ty <> "cbt_metadata") remote_vdis in

      let nearest = List.fold_left
          (fun acc content_id -> match acc with
             | Some x -> acc
             | None ->
               try Some (List.find (fun vdi -> vdi.content_id = content_id && vdi.virtual_size <= local_vdi.virtual_size) remote_vdis)
               with Not_found -> None) None similars in

      debug "Nearest VDI: content_id=%s vdi=%s"
        (Opt.default "None" (Opt.map (fun x -> x.content_id) nearest))
        (Opt.default "None" (Opt.map (fun x -> x.vdi) nearest));
      let remote_base = match nearest with
        | Some vdi ->
          debug "Cloning VDI %s" vdi.vdi;
          let vdi_clone = Remote.VDI.clone ~dbg ~sr:dest ~vdi_info:vdi in
          if vdi_clone.virtual_size <> local_vdi.virtual_size then begin
            let new_size = Remote.VDI.resize ~dbg ~sr:dest ~vdi:vdi_clone.vdi ~new_size:local_vdi.virtual_size in
            debug "Resize remote VDI %s to %Ld: result %Ld" vdi_clone.vdi local_vdi.virtual_size new_size;
          end;
          vdi_clone
        | None ->
          debug "Creating a blank remote VDI";
          Remote.VDI.create ~dbg ~sr:dest ~vdi_info:{ local_vdi with sm_config = [] }  in
      let remote_copy = copy' ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi:remote_base.vdi |> vdi_info in
      let snapshot = Remote.VDI.snapshot ~dbg ~sr:dest ~vdi_info:remote_copy in
      Remote.VDI.destroy ~dbg ~sr:dest ~vdi:remote_copy.vdi;
      Some (Vdi_info snapshot)
    with e ->
      error "Caught %s: copying snapshots vdi" (Printexc.to_string e);
      raise (Internal_error (Printexc.to_string e))
  with
  | Backend_error(code, params)
  | Api_errors.Server_error(code, params) ->
    raise (Backend_error(code, params))
  | e ->
    raise (Internal_error(Printexc.to_string e))


let wrap ~dbg f =
  let task = Storage_task.add tasks dbg (fun task ->
      try
        f task
      with
      | Backend_error(code, params)
      | Api_errors.Server_error(code, params) ->
        raise (Backend_error(code, params))
      | Unimplemented msg -> raise (Unimplemented msg)
      | e ->
        raise (Internal_error(Printexc.to_string e))) in
  let _ = Thread.create
      (Debug.with_thread_associated dbg (fun () ->
           Storage_task.run task;
           signal (Storage_task.id_of_handle task)
         )) () in
  Storage_task.id_of_handle task

let start ~dbg ~sr ~vdi ~dp ~url ~dest =
  wrap ~dbg (fun task -> start' ~task ~dbg ~sr ~vdi ~dp ~url ~dest)

let copy ~dbg ~sr ~vdi ~dp ~url ~dest =
  wrap ~dbg (fun task -> copy ~task ~dbg ~sr ~vdi ~dp ~url ~dest)

let copy_into ~dbg ~sr ~vdi ~url ~dest ~dest_vdi =
  wrap ~dbg (fun task -> copy_into ~task ~dbg ~sr ~vdi ~url ~dest ~dest_vdi)

(* The remote end of this call, SR.update_snapshot_info_dest, is implemented in
 * the SMAPIv1 section of storage_migrate.ml. It needs to access the setters
 * for snapshot_of, snapshot_time and is_a_snapshot, which we don't want to add
 * to SMAPI. *)
let update_snapshot_info_src ~dbg ~sr ~vdi ~url ~dest ~dest_vdi ~snapshot_pairs =
  let remote_url = Http.Url.of_string url in
  let module Remote =
    Client(struct
      let rpc = rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url
    end)
  in
  let local_vdis = Local.SR.scan ~dbg ~sr in
  let find_vdi ~vdi ~vdi_info_list =
    try List.find (fun x -> x.vdi = vdi) vdi_info_list
    with Not_found -> raise (Vdi_does_not_exist vdi)
  in
  let snapshot_pairs_for_remote =
    List.map
      (fun (local_snapshot, remote_snapshot) ->
         (remote_snapshot,
          find_vdi ~vdi:local_snapshot ~vdi_info_list:local_vdis))
      snapshot_pairs
  in
  Remote.SR.update_snapshot_info_dest ~dbg ~sr:dest ~vdi:dest_vdi
    ~src_vdi:(find_vdi ~vdi ~vdi_info_list:local_vdis)
    ~snapshot_pairs:snapshot_pairs_for_remote
