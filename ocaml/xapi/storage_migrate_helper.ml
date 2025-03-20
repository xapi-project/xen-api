(*
 * Copyright (c) Cloud Software Group
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

(** As SXM is such a long running process, we dedicate this to log important 
  milestones during the SXM process *)
module SXM = Debug.Make (struct
  let name = "SXM"
end)

module Listext = Xapi_stdext_std.Listext
module Unixext = Xapi_stdext_unix.Unixext
open Storage_interface
open Xapi_stdext_pervasives.Pervasiveext
open Xmlrpc_client

module State = struct
  module Receive_state = struct
    type t = {
        sr: Sr.t
      ; dummy_vdi: Vdi.t
      ; leaf_vdi: Vdi.t
      ; leaf_dp: dp
      ; parent_vdi: Vdi.t
      ; remote_vdi: Vdi.t
      ; mirror_vm: Vm.t
    }
    [@@deriving rpcty]

    let rpc_of_t = Rpcmarshal.marshal t.Rpc.Types.ty

    let t_of_rpc x =
      match Rpcmarshal.unmarshal t.Rpc.Types.ty x with
      | Ok y ->
          y
      | Error (`Msg m) ->
          failwith (Printf.sprintf "Failed to unmarshal Receive_state.t: %s" m)
  end

  module Send_state = struct
    type remote_info = {
        dp: dp
      ; vdi: Vdi.t
      ; url: string
      ; verify_dest: bool [@default false]
    }
    [@@deriving rpcty]

    type tapdev = Tapctl.tapdev

    let typ_of_tapdev =
      Rpc.Types.(
        Abstract
          {
            aname= "tapdev"
          ; test_data= []
          ; rpc_of= Tapctl.rpc_of_tapdev
          ; of_rpc= (fun x -> Ok (Tapctl.tapdev_of_rpc x))
          }
      )

    type handle = Scheduler.handle

    let typ_of_handle =
      Rpc.Types.(
        Abstract
          {
            aname= "handle"
          ; test_data= []
          ; rpc_of= Scheduler.rpc_of_handle
          ; of_rpc= (fun x -> Ok (Scheduler.handle_of_rpc x))
          }
      )

    type t = {
        url: string
      ; dest_sr: Sr.t
      ; remote_info: remote_info option
      ; local_dp: dp
      ; tapdev: tapdev option
      ; mutable failed: bool
      ; mutable watchdog: handle option
    }
    [@@deriving rpcty]

    let rpc_of_t = Rpcmarshal.marshal t.Rpc.Types.ty

    let t_of_rpc x =
      match Rpcmarshal.unmarshal t.Rpc.Types.ty x with
      | Ok y ->
          y
      | Error (`Msg m) ->
          failwith (Printf.sprintf "Failed to unmarshal Send_state.t: %s" m)
  end

  module Copy_state = struct
    type t = {
        base_dp: dp
      ; leaf_dp: dp
      ; remote_dp: dp
      ; dest_sr: Sr.t
      ; copy_vdi: Vdi.t
      ; remote_url: string
      ; verify_dest: bool [@default false]
    }
    [@@deriving rpcty]

    let rpc_of_t = Rpcmarshal.marshal t.Rpc.Types.ty

    let t_of_rpc x =
      match Rpcmarshal.unmarshal t.Rpc.Types.ty x with
      | Ok y ->
          y
      | Error (`Msg m) ->
          failwith (Printf.sprintf "Failed to unmarshal Copy_state.t: %s" m)
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
    | Send_op _ ->
        Send_table active_send
    | Recv_op _ ->
        Recv_table active_recv
    | Copy_op _ ->
        Copy_table active_copy

  let persist_root = ref "/var/run/nonpersistent"

  let path_of_table : type a. a table -> string = function
    | Send_table _ ->
        Filename.concat !persist_root "storage_mirrors_send.json"
    | Recv_table _ ->
        Filename.concat !persist_root "storage_mirrors_recv.json"
    | Copy_table _ ->
        Filename.concat !persist_root "storage_mirrors_copy.json"

  let rpc_of_table : type a. a table -> Rpc.t =
    let open Rpc_std_helpers in
    function
    | Send_table send_table ->
        rpc_of_hashtbl ~rpc_of:Send_state.rpc_of_t send_table
    | Recv_table recv_table ->
        rpc_of_hashtbl ~rpc_of:Receive_state.rpc_of_t recv_table
    | Copy_table copy_table ->
        rpc_of_hashtbl ~rpc_of:Copy_state.rpc_of_t copy_table

  let to_string : type a. a table -> string =
   fun table -> rpc_of_table table |> Jsonrpc.to_string

  let rpc_of_path path = Unixext.string_of_file path |> Jsonrpc.of_string

  let load_one : type a. a table -> unit =
   fun table ->
    let rpc = path_of_table table |> rpc_of_path in
    let open Rpc_std_helpers in
    match table with
    | Send_table table ->
        Hashtbl.iter (Hashtbl.replace table)
          (hashtbl_of_rpc ~of_rpc:Send_state.t_of_rpc rpc)
    | Recv_table table ->
        Hashtbl.iter (Hashtbl.replace table)
          (hashtbl_of_rpc ~of_rpc:Receive_state.t_of_rpc rpc)
    | Copy_table table ->
        Hashtbl.iter (Hashtbl.replace table)
          (hashtbl_of_rpc ~of_rpc:Copy_state.t_of_rpc rpc)

  let load () =
    ignore_exn (fun () -> load_one (Send_table active_send)) ;
    ignore_exn (fun () -> load_one (Recv_table active_recv)) ;
    ignore_exn (fun () -> load_one (Copy_table active_copy)) ;
    loaded := true

  let save_one : type a. a table -> unit =
   fun table ->
    to_string table |> Unixext.write_string_to_file (path_of_table table)

  let save () =
    Unixext.mkdir_rec !persist_root 0o700 ;
    save_one (Send_table active_send) ;
    save_one (Recv_table active_recv) ;
    save_one (Copy_table active_copy)

  let access_table ~save_after f table =
    Xapi_stdext_threads.Threadext.Mutex.execute mutex (fun () ->
        if not !loaded then load () ;
        let result = f table in
        if save_after then save () ;
        result
    )

  let map_of () =
    let contents_of table =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) table []
    in
    let send_ops = access_table ~save_after:false contents_of active_send in
    let recv_ops = access_table ~save_after:false contents_of active_recv in
    let copy_ops = access_table ~save_after:false contents_of active_copy in
    (send_ops, recv_ops, copy_ops)

  let add : type a. string -> a operation -> unit =
   fun id op ->
    let add' : type a. string -> a operation -> a table -> unit =
     fun id op table ->
      match (table, op) with
      | Send_table table, Send_op op ->
          Hashtbl.replace table id op
      | Recv_table table, Recv_op op ->
          Hashtbl.replace table id op
      | Copy_table table, Copy_op op ->
          Hashtbl.replace table id op
    in
    access_table ~save_after:true
      (fun table -> add' id op table)
      (table_of_op op)

  let find id table =
    access_table ~save_after:false
      (fun table -> Hashtbl.find_opt table id)
      table

  let remove id table =
    access_table ~save_after:true (fun table -> Hashtbl.remove table id) table

  let clear () =
    access_table ~save_after:true (fun table -> Hashtbl.clear table) active_send ;
    access_table ~save_after:true (fun table -> Hashtbl.clear table) active_recv ;
    access_table ~save_after:true (fun table -> Hashtbl.clear table) active_copy

  let remove_local_mirror id = remove id active_send

  let remove_receive_mirror id = remove id active_recv

  let remove_copy id = remove id active_copy

  let find_active_local_mirror id = find id active_send

  let find_active_receive_mirror id = find id active_recv

  let find_active_copy id = find id active_copy

  let mirror_id_of (sr, vdi) =
    Printf.sprintf "%s/%s"
      (Storage_interface.Sr.string_of sr)
      (Storage_interface.Vdi.string_of vdi)

  let of_mirror_id id =
    match String.split_on_char '/' id with
    | sr :: rest ->
        Storage_interface.
          (Sr.of_string sr, Vdi.of_string (String.concat "/" rest))
    | _ ->
        failwith "Bad id"

  let copy_id_of (sr, vdi) =
    Printf.sprintf "copy/%s/%s"
      (Storage_interface.Sr.string_of sr)
      (Storage_interface.Vdi.string_of vdi)

  let of_copy_id id =
    match String.split_on_char '/' id with
    | op :: sr :: rest when op = "copy" ->
        Storage_interface.
          (Sr.of_string sr, Vdi.of_string (String.concat "/" rest))
    | _ ->
        failwith "Bad id"
end

let vdi_info x =
  match x with
  | Some (Vdi_info v) ->
      v
  | _ ->
      failwith "Runtime type error: expecting Vdi_info"

let remove_from_sm_config vdi_info key =
  {
    vdi_info with
    sm_config= List.filter (fun (k, _) -> k <> key) vdi_info.sm_config
  }

let add_to_sm_config vdi_info key value =
  let vdi_info = remove_from_sm_config vdi_info key in
  {vdi_info with sm_config= (key, value) :: vdi_info.sm_config}

let with_http request f s =
  try Http_client.rpc s request (fun response s -> f (response, s))
  with Unix.Unix_error (Unix.ECONNRESET, _, _) -> raise Connection_reset

module Local = StorageAPI (Idl.Exn.GenClient (struct
  let rpc call =
    Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"smapiv2"
      (Storage_utils.localhost_connection_args ())
      call
end))

module type SMAPIv2 = module type of Local

let get_remote_backend url verify_dest =
  let remote_url = Storage_utils.connection_args_of_uri ~verify_dest url in
  let module Remote = StorageAPI (Idl.Exn.GenClient (struct
    let rpc =
      Storage_utils.rpc ~srcstr:"smapiv2" ~dststr:"dst_smapiv2" remote_url
  end)) in
  (module Remote : SMAPIv2)
