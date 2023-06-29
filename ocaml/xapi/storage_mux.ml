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

module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "mux" end)

open D

let with_tracing ~name ~dbg f =
  let dbg, parent = Context.tracing_of_dbg dbg in
  let name = "SMAPIv2." ^ name in
  let tracer = Tracing.get_tracer ~name in
  let span = Tracing.Tracer.start ~tracer ~name ~parent () in
  match span with
  | Ok span_context ->
      let result = f dbg in
      let _ = Tracing.Tracer.finish span_context in
      result
  | Error e ->
      D.warn "Failed to start tracing: %s" (Printexc.to_string e) ;
      f dbg

type processor = Rpc.call -> Rpc.response

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

open Storage_interface

let s_of_sr = Sr.string_of

let s_of_vdi = Vdi.string_of

let s_of_vm = Vm.string_of

type plugin = {
    processor: processor
  ; backend_domain: string
  ; query_result: query_result
  ; features: Smint.feature list
}

let plugins : (sr, plugin) Hashtbl.t = Hashtbl.create 10

let m = Mutex.create ()

let debug_printer rpc call =
  (* debug "Rpc.call = %s" (Xmlrpc.string_of_call call); *)
  let result = rpc call in
  (* debug "Rpc.response = %s" (Xmlrpc.string_of_response result); *)
  result

let register sr rpc d info =
  with_lock m (fun () ->
      let features =
        Smint.parse_capability_int64_features info.Storage_interface.features
      in
      Hashtbl.replace plugins sr
        {
          processor= debug_printer rpc
        ; backend_domain= d
        ; query_result= info
        ; features
        } ;
      debug "register SR %s (currently-registered = [ %s ])" (s_of_sr sr)
        (String.concat ", "
           (Hashtbl.fold (fun sr _ acc -> s_of_sr sr :: acc) plugins [])
        )
  )

let unregister sr =
  with_lock m (fun () ->
      Hashtbl.remove plugins sr ;
      debug "unregister SR %s (currently-registered = [ %s ])" (s_of_sr sr)
        (String.concat ", "
           (Hashtbl.fold (fun sr _ acc -> s_of_sr sr :: acc) plugins [])
        )
  )

let query_result_of_sr sr =
  try with_lock m (fun () -> Some (Hashtbl.find plugins sr).query_result)
  with _ -> None

let sr_has_capability sr capability =
  try
    with_lock m (fun () ->
        Smint.has_capability capability (Hashtbl.find plugins sr).features
    )
  with _ -> false

(* This is the policy: *)
let of_sr sr =
  with_lock m (fun () ->
      if not (Hashtbl.mem plugins sr) then (
        error "No storage plugin for SR: %s (currently-registered = [ %s ])"
          (s_of_sr sr)
          (String.concat ", "
             (Hashtbl.fold (fun sr _ acc -> s_of_sr sr :: acc) plugins [])
          ) ;
        raise (Storage_error (No_storage_plugin_for_sr (s_of_sr sr)))
      ) else
        (Hashtbl.find plugins sr).processor
  )

type 'a sm_result = SMSuccess of 'a | SMFailure of exn

let multicast f =
  Hashtbl.fold
    (fun sr plugin acc ->
      (sr, try SMSuccess (f sr plugin.processor) with e -> SMFailure e) :: acc
    )
    plugins []

let success = function SMSuccess _ -> true | _ -> false

let string_of_sm_result f = function
  | SMSuccess x ->
      Printf.sprintf "Success: %s" (f x)
  | SMFailure e ->
      Printf.sprintf "Failure: %s" (Printexc.to_string e)

let partition l = List.partition (fun (_, x) -> success x) l

let choose x = snd (List.hd x)

let fail_or f results =
  let successes, errors = partition results in
  if errors <> [] then choose errors else f successes

let success_or f results =
  let successes, errors = partition results in
  if successes <> [] then f successes else f errors

module Mux = struct
  type context = unit

  let forall f =
    let combine results =
      let all =
        List.fold_left
          (fun acc (sr, result) ->
            Printf.sprintf "For SR: %s" (s_of_sr sr)
            :: string_of_sm_result (fun s -> s) result
            :: acc
          )
          [] results
      in
      SMSuccess (String.concat "\n" all)
    in
    let m = multicast f in
    match fail_or combine m with SMSuccess x -> x | SMFailure e -> raise e

  module Query = struct
    let query () ~dbg:_ =
      {
        driver= "mux"
      ; name= "storage multiplexor"
      ; description= "forwards calls to other plugins"
      ; vendor= "XCP"
      ; copyright= "see the source code"
      ; version= "2.0"
      ; required_api_version= "2.0"
      ; features= []
      ; configuration= []
      ; required_cluster_stack= []
      }

    let diagnostics () ~dbg =
      forall (fun sr _rpc ->
          let module C = StorageAPI (Idl.Exn.GenClient (struct
            let rpc = of_sr sr
          end)) in
          with_tracing ~name:"Query.diagnostics" ~dbg @@ fun dbg ->
          C.Query.diagnostics dbg
      )
  end

  module DP_info = struct
    type t = {sr: Sr.t; vdi: Vdi.t; vm: Vm.t; read_write: bool [@default true]}
    [@@deriving rpcty]

    let storage_dp_path = "/var/run/nonpersistent/xapi/storage-dps"

    let m = Mutex.create ()

    let filename_of dp = Xapi_stdext_std.Xstringext.String.replace "/" "-" dp

    let write dp info =
      let filename = filename_of dp in
      let data = Rpcmarshal.marshal t.Rpc.Types.ty info |> Jsonrpc.to_string in
      with_lock m (fun () ->
          Unixext.mkdir_rec storage_dp_path 0o600 ;
          Unixext.write_string_to_file
            (Filename.concat storage_dp_path filename)
            data
      )

    let read dp : t option =
      try
        with_lock m (fun () ->
            let x =
              let path = Filename.concat storage_dp_path (filename_of dp) in
              path |> Unixext.string_of_file |> Jsonrpc.of_string
            in
            match Rpcmarshal.unmarshal t.Rpc.Types.ty x with
            | Ok x ->
                Some x
            | Error (`Msg m) ->
                failwith (Printf.sprintf "Failed to unmarshal: %s" m)
        )
      with _ -> None

    let delete dp =
      try
        with_lock m (fun () ->
            let path = Filename.concat storage_dp_path (filename_of dp) in
            Unix.unlink path
        )
      with _ -> ()
  end

  module DP = struct
    let create _context ~dbg:_ ~id = id

    let destroy2 _context ~dbg ~dp ~sr ~vdi ~vm ~allow_leak =
      info "DP.destroy2 dbg:%s dp:%s sr:%s vdi:%s vm:%s allow_leak:%b" dbg dp
        (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) allow_leak ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"DP.destroy2" ~dbg @@ fun dbg ->
      C.DP.destroy2 dbg dp sr vdi vm allow_leak ;
      DP_info.delete dp

    let destroy _context ~dbg ~dp ~allow_leak =
      info "DP.destroy dbg:%s dp:%s allow_leak:%b" dbg dp allow_leak ;
      let open DP_info in
      with_tracing ~name:"DP.destroy" ~dbg @@ fun dbg ->
      match read dp with
      | Some {sr; vdi; vm; _} ->
          destroy2 _context ~dbg ~dp ~sr ~vdi ~vm ~allow_leak
      | None ->
          info
            "dp %s is not associated with a locally attached VDI; nothing to do"
            dp

    let diagnostics () = Storage_smapiv1_wrapper.Impl.DP.diagnostics ()

    let attach_info () = Storage_smapiv1_wrapper.Impl.DP.attach_info ()

    let stat_vdi () = Storage_smapiv1_wrapper.Impl.DP.stat_vdi ()
  end

  module SR = struct
    include Storage_skeleton.SR

    let device_config_str device_config =
      let censor_key = ["password"] in
      String.concat "; "
        (List.map
           (fun (k, v) ->
             let v' =
               if
                 List.exists
                   (fun censored -> Astring.String.is_infix ~affix:censored k)
                   censor_key
               then
                 "(omitted)"
               else
                 v
             in
             k ^ ":" ^ v'
           )
           device_config
        )

    let create () ~dbg ~sr ~name_label ~name_description ~device_config
        ~physical_size =
      info
        "SR.create dbg:%s sr:%s name_label:%s name_description:%s \
         device_config:[%s] physical_size:%Ld"
        dbg (s_of_sr sr) name_label name_description
        (device_config_str device_config)
        physical_size ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.create" ~dbg @@ fun dbg ->
      C.SR.create dbg sr name_label name_description device_config physical_size

    let attach () ~dbg ~sr ~device_config =
      info "SR.attach dbg:%s sr:%s device_config:[%s]" dbg (s_of_sr sr)
        (device_config_str device_config) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.attach" ~dbg @@ fun dbg ->
      C.SR.attach dbg sr device_config

    let set_name_label () ~dbg ~sr ~new_name_label =
      info "SR.set_name_label dbg:%s sr:%s new_name_label:%s" dbg (s_of_sr sr)
        new_name_label ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.set_name_label" ~dbg @@ fun dbg ->
      C.SR.set_name_label dbg sr new_name_label

    let set_name_description () ~dbg ~sr ~new_name_description =
      info "SR.set_name_description dbg:%s sr:%s new_name_description:%s" dbg
        (s_of_sr sr) new_name_description ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.set_name_description" ~dbg @@ fun dbg ->
      C.SR.set_name_description dbg sr new_name_description

    let detach () ~dbg ~sr =
      info "SR.detach dbg:%s sr:%s" dbg (s_of_sr sr) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.detach" ~dbg @@ fun dbg -> C.SR.detach dbg sr

    let destroy () ~dbg ~sr =
      info "SR.destroy dbg:%s sr:%s" dbg (s_of_sr sr) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.destroy" ~dbg @@ fun dbg -> C.SR.destroy dbg sr

    let stat () ~dbg ~sr =
      info "SR.stat dbg:%s sr:%s" dbg (s_of_sr sr) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.stat" ~dbg @@ fun dbg -> C.SR.stat dbg sr

    let scan () ~dbg ~sr =
      info "SR.scan dbg:%s sr:%s" dbg (s_of_sr sr) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.scan" ~dbg @@ fun dbg -> C.SR.scan dbg sr

    module SRSet = Set.Make (struct
      type t = Storage_interface.Sr.t

      let compare = compare
    end)

    let list () ~dbg =
      info "SR.list dbg:%s" dbg ;
      List.fold_left
        (fun acc (_, list) ->
          match list with
          | SMSuccess l ->
              List.fold_left (fun srs sr -> SRSet.add sr srs) acc l
          | _ ->
              acc
        )
        SRSet.empty
        (multicast (fun sr _rpc ->
             let module C = StorageAPI (Idl.Exn.GenClient (struct
               let rpc = of_sr sr
             end)) in
             with_tracing ~name:"SR.list" ~dbg @@ fun dbg -> C.SR.list dbg
         )
        )
      |> SRSet.elements

    let reset () ~dbg ~sr =
      info "SR.reset dbg:%s sr:%s" dbg (s_of_sr sr) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"SR.reset" ~dbg @@ fun dbg -> C.SR.reset dbg sr

    let update_snapshot_info_src () ~dbg ~sr ~vdi ~url ~dest ~dest_vdi
        ~snapshot_pairs =
      info
        "SR.update_snapshot_info_src dbg:%s sr:%s vdi:%s url:%s dest:%s \
         dest_vdi:%s snapshot_pairs:%s"
        dbg (s_of_sr sr) (s_of_vdi vdi) url (s_of_sr dest) (s_of_vdi dest_vdi)
        (List.map
           (fun (local_snapshot, dest_snapshot) ->
             Printf.sprintf "local:%s, dest:%s" (s_of_vdi local_snapshot)
               (s_of_vdi dest_snapshot)
           )
           snapshot_pairs
        |> String.concat "; "
        |> Printf.sprintf "[%s]"
        ) ;
      with_tracing ~name:"SR.update_snapshot_info_src" ~dbg @@ fun dbg ->
      Storage_migrate.update_snapshot_info_src ~dbg ~sr ~vdi ~url ~dest
        ~dest_vdi ~snapshot_pairs

    let update_snapshot_info_dest () ~dbg ~sr ~vdi ~src_vdi ~snapshot_pairs =
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      info
        "SR.update_snapshot_info_dest dbg:%s sr:%s vdi:%s ~src_vdi:%s \
         snapshot_pairs:%s"
        dbg (s_of_sr sr) (s_of_vdi vdi) (s_of_vdi src_vdi.vdi)
        (List.map
           (fun (local_snapshot, src_snapshot_info) ->
             Printf.sprintf "local:%s, src:%s" (s_of_vdi local_snapshot)
               (s_of_vdi src_snapshot_info.vdi)
           )
           snapshot_pairs
        |> String.concat "; "
        |> Printf.sprintf "[%s]"
        ) ;
      with_tracing ~name:"SR.update_snapshot_info_dest" ~dbg @@ fun dbg ->
      C.SR.update_snapshot_info_dest dbg sr vdi src_vdi snapshot_pairs
  end

  module VDI = struct
    let create () ~dbg ~sr ~vdi_info =
      info "VDI.create dbg:%s sr:%s vdi_info:%s" dbg (s_of_sr sr)
        (string_of_vdi_info vdi_info) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.create" ~dbg @@ fun dbg ->
      C.VDI.create dbg sr vdi_info

    let set_name_label () ~dbg ~sr ~vdi ~new_name_label =
      info "VDI.set_name_label dbg:%s sr:%s vdi:%s new_name_label:%s" dbg
        (s_of_sr sr) (s_of_vdi vdi) new_name_label ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.set_name_label" ~dbg @@ fun dbg ->
      C.VDI.set_name_label dbg sr vdi new_name_label

    let set_name_description () ~dbg ~sr ~vdi ~new_name_description =
      info
        "VDI.set_name_description dbg:%s sr:%s vdi:%s new_name_description:%s"
        dbg (s_of_sr sr) (s_of_vdi vdi) new_name_description ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.set_name_description" ~dbg @@ fun dbg ->
      C.VDI.set_name_description dbg sr vdi new_name_description

    let snapshot () ~dbg ~sr ~vdi_info =
      info "VDI.snapshot dbg:%s sr:%s vdi_info:%s" dbg (s_of_sr sr)
        (string_of_vdi_info vdi_info) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      try
        with_tracing ~name:"VDI.snapshot" ~dbg @@ fun dbg ->
        C.VDI.snapshot dbg sr vdi_info
      with Storage_interface.Storage_error (Activated_on_another_host uuid) ->
        Server_helpers.exec_with_new_task "smapiv2.snapshot.activated"
          ~subtask_of:(Ref.of_string dbg) (fun __context ->
            let hostref = Db.Host.get_by_uuid ~__context ~uuid in
            let addr = Db.Host.get_address ~__context ~self:hostref in
            debug
              "SM reports the VDI is activated elsewhere on %s, redirecting to \
               IP %s"
              uuid addr ;
            raise (Storage_error (Redirect (Some addr)))
        )

    let clone () ~dbg ~sr ~vdi_info =
      info "VDI.clone dbg:%s sr:%s vdi_info:%s" dbg (s_of_sr sr)
        (string_of_vdi_info vdi_info) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.clone" ~dbg @@ fun dbg ->
      C.VDI.clone dbg sr vdi_info

    let resize () ~dbg ~sr ~vdi ~new_size =
      info "VDI.resize dbg:%s sr:%s vdi:%s new_size:%Ld" dbg (s_of_sr sr)
        (s_of_vdi vdi) new_size ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.resize" ~dbg @@ fun dbg ->
      C.VDI.resize dbg sr vdi new_size

    let destroy () ~dbg ~sr ~vdi =
      info "VDI.destroy dbg:%s sr:%s vdi:%s" dbg (s_of_sr sr) (s_of_vdi vdi) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.destroy" ~dbg @@ fun dbg ->
      C.VDI.destroy dbg sr vdi

    let stat () ~dbg ~sr ~vdi =
      info "VDI.stat dbg:%s sr:%s vdi:%s" dbg (s_of_sr sr) (s_of_vdi vdi) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.stat" ~dbg @@ fun dbg -> C.VDI.stat dbg sr vdi

    let introduce () ~dbg ~sr ~uuid ~sm_config ~location =
      info "VDI.introduce dbg:%s sr:%s uuid:%s sm_config:%s location:%s" dbg
        (s_of_sr sr) uuid
        (String.concat ", " (List.map (fun (k, v) -> k ^ ":" ^ v) sm_config))
        location ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.introduce" ~dbg @@ fun dbg ->
      C.VDI.introduce dbg sr uuid sm_config location

    let set_persistent () ~dbg ~sr ~vdi ~persistent =
      info "VDI.set_persistent dbg:%s sr:%s vdi:%s persistent:%b" dbg
        (s_of_sr sr) (s_of_vdi vdi) persistent ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.set_persistent" ~dbg @@ fun dbg ->
      C.VDI.set_persistent dbg sr vdi persistent

    let epoch_begin () ~dbg ~sr ~vdi ~vm ~persistent =
      info "VDI.epoch_begin dbg:%s sr:%s vdi:%s vm:%s persistent:%b" dbg
        (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) persistent ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.epoch_begin" ~dbg @@ fun dbg ->
      C.VDI.epoch_begin dbg sr vdi vm persistent

    let attach () ~dbg ~dp ~sr ~vdi ~read_write =
      info "VDI.attach dbg:%s dp:%s sr:%s vdi:%s read_write:%b" dbg dp
        (s_of_sr sr) (s_of_vdi vdi) read_write ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      let vm = Vm.of_string "0" in
      DP_info.write dp DP_info.{sr; vdi; vm; read_write} ;
      with_tracing ~name:"VDI.attach" ~dbg @@ fun dbg ->
      let backend = C.VDI.attach3 dbg dp sr vdi vm read_write in
      (* VDI.attach2 should be used instead, VDI.attach is only kept for
         backwards-compatibility, because older xapis call Remote.VDI.attach during SXM.
         However, they ignore the return value, so in practice it does not matter what
         we return from here. *)
      let xendisks, blockdevs, files, _nbds =
        Storage_interface.implementations_of_backend backend
      in
      let response params =
        (* We've thrown o_direct info away from the SMAPIv1 info during the conversion to SMAPIv3 attach info *)
        (* The removal of these fields does not break read caching info propagation for SMAPIv1
         * (REQ-49), because we put this information into the VDI's sm_config elsewhere,
         * and XenCenter looks at the relevant sm_config keys. *)
        {params; xenstore_data= []; o_direct= true; o_direct_reason= ""}
      in
      (* If Nbd is returned, then XenDisk must also be returned from attach2 *)
      match (xendisks, files, blockdevs) with
      | xendisk :: _, _, _ ->
          response xendisk.Storage_interface.params
      | _, file :: _, _ ->
          response file.Storage_interface.path
      | _, _, blockdev :: _ ->
          response blockdev.Storage_interface.path
      | [], [], [] ->
          raise
            (Storage_interface.Storage_error
               (Backend_error
                  ( Api_errors.internal_error
                  , [
                      "No File, BlockDev, or XenDisk implementation in \
                       Datapath.attach response: "
                      ^ (Storage_interface.(rpc_of backend) backend
                        |> Jsonrpc.to_string
                        )
                    ]
                  )
               )
            )

    let attach2 () ~dbg ~dp ~sr ~vdi ~read_write =
      info "VDI.attach2 dbg:%s dp:%s sr:%s vdi:%s read_write:%b" dbg dp
        (s_of_sr sr) (s_of_vdi vdi) read_write ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      let vm = Vm.of_string "0" in
      DP_info.write dp DP_info.{sr; vdi; vm; read_write} ;
      with_tracing ~name:"VDI.attach2" ~dbg @@ fun dbg ->
      C.VDI.attach3 dbg dp sr vdi vm read_write

    let attach3 () ~dbg ~dp ~sr ~vdi ~vm ~read_write =
      info "VDI.attach3 dbg:%s dp:%s sr:%s vdi:%s vm:%s read_write:%b" dbg dp
        (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) read_write ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.attach3" ~dbg @@ fun dbg ->
      DP_info.write dp DP_info.{sr; vdi; vm; read_write} ;
      C.VDI.attach3 dbg dp sr vdi vm read_write

    let activate () ~dbg ~dp ~sr ~vdi =
      info "VDI.activate dbg:%s dp:%s sr:%s vdi:%s " dbg dp (s_of_sr sr)
        (s_of_vdi vdi) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.activate" ~dbg @@ fun dbg ->
      C.VDI.activate dbg dp sr vdi

    let activate3 () ~dbg ~dp ~sr ~vdi ~vm =
      info "VDI.activate3 dbg:%s dp:%s sr:%s vdi:%s vm:%s" dbg dp (s_of_sr sr)
        (s_of_vdi vdi) (s_of_vm vm) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      let read_write =
        let open DP_info in
        match read dp with
        | Some x ->
            x.read_write
        | None ->
            failwith "DP not found"
      in
      with_tracing ~name:"VDI.activate3" ~dbg @@ fun dbg ->
      if (not read_write) && sr_has_capability sr Smint.Vdi_activate_readonly
      then (
        info "The VDI was attached read-only: calling activate_readonly" ;
        C.VDI.activate_readonly dbg dp sr vdi vm
      ) else (
        info "The VDI was attached read/write: calling activate3" ;
        C.VDI.activate3 dbg dp sr vdi vm
      )

    let activate_readonly () ~dbg ~dp ~sr ~vdi ~vm =
      info "VDI.activate_readonly dbg:%s dp:%s sr:%s vdi:%s vm:%s" dbg dp
        (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.activate_readonly" ~dbg @@ fun dbg ->
      C.VDI.activate_readonly dbg dp sr vdi vm

    let deactivate () ~dbg ~dp ~sr ~vdi ~vm =
      info "VDI.deactivate dbg:%s dp:%s sr:%s vdi:%s vm:%s" dbg dp (s_of_sr sr)
        (s_of_vdi vdi) (s_of_vm vm) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.deativate" ~dbg @@ fun dbg ->
      C.VDI.deactivate dbg dp sr vdi vm

    let detach () ~dbg ~dp ~sr ~vdi ~vm =
      info "VDI.detach dbg:%s dp:%s sr:%s vdi:%s vm:%s" dbg dp (s_of_sr sr)
        (s_of_vdi vdi) (s_of_vm vm) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.detach" ~dbg @@ fun dbg ->
      C.VDI.detach dbg dp sr vdi vm ;
      DP_info.delete dp

    let epoch_end () ~dbg ~sr ~vdi ~vm =
      info "VDI.epoch_end dbg:%s sr:%s vdi:%s vm:%s" dbg (s_of_sr sr)
        (s_of_vdi vdi) (s_of_vm vm) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.epoch_end" ~dbg @@ fun dbg ->
      C.VDI.epoch_end dbg sr vdi vm

    let get_by_name () ~dbg ~sr ~name =
      info "VDI.get_by_name dbg:%s sr:%s name:%s" dbg (s_of_sr sr) name ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.get_by_name" ~dbg @@ fun dbg ->
      C.VDI.get_by_name dbg sr name

    let set_content_id () ~dbg ~sr ~vdi ~content_id =
      info "VDI.set_content_id dbg:%s sr:%s vdi:%s content_id:%s" dbg
        (s_of_sr sr) (s_of_vdi vdi) content_id ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.set_content_id" ~dbg @@ fun dbg ->
      C.VDI.set_content_id dbg sr vdi content_id

    let similar_content () ~dbg ~sr ~vdi =
      info "VDI.similar_content dbg:%s sr:%s vdi:%s" dbg (s_of_sr sr)
        (s_of_vdi vdi) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.similar_content" ~dbg @@ fun dbg ->
      C.VDI.similar_content dbg sr vdi

    let compose () ~dbg ~sr ~vdi1 ~vdi2 =
      info "VDI.compose dbg:%s sr:%s vdi1:%s vdi2:%s" dbg (s_of_sr sr)
        (s_of_vdi vdi1) (s_of_vdi vdi2) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.compose" ~dbg @@ fun dbg ->
      C.VDI.compose dbg sr vdi1 vdi2

    let add_to_sm_config () ~dbg ~sr ~vdi ~key ~value =
      info "VDI.add_to_sm_config dbg:%s sr:%s vdi:%s key:%s value:%s" dbg
        (s_of_sr sr) (s_of_vdi vdi) key value ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.add_to_sm_config" ~dbg @@ fun dbg ->
      C.VDI.add_to_sm_config dbg sr vdi key value

    let remove_from_sm_config () ~dbg ~sr ~vdi ~key =
      info "VDI.remove_from_sm_config dbg:%s sr:%s vdi:%s key:%s" dbg
        (s_of_sr sr) (s_of_vdi vdi) key ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.remove_from_sm_config" ~dbg @@ fun dbg ->
      C.VDI.remove_from_sm_config dbg sr vdi key

    let get_url () ~dbg ~sr ~vdi =
      info "VDI.get_url dbg:%s sr:%s vdi:%s" dbg (s_of_sr sr) (s_of_vdi vdi) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.get_url" ~dbg @@ fun dbg ->
      C.VDI.get_url dbg sr vdi

    let enable_cbt () ~dbg ~sr ~vdi =
      info "VDI.enable_cbt dbg:%s sr:%s vdi:%s" dbg (s_of_sr sr) (s_of_vdi vdi) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.enabled_cbt" ~dbg @@ fun dbg ->
      C.VDI.enable_cbt dbg sr vdi

    let disable_cbt () ~dbg ~sr ~vdi =
      info "VDI.disable_cbt dbg:%s sr:%s vdi:%s" dbg (s_of_sr sr) (s_of_vdi vdi) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.disable_cbt" ~dbg @@ fun dbg ->
      C.VDI.disable_cbt dbg sr vdi

    let data_destroy () ~dbg ~sr ~vdi =
      info "VDI.data_destroy dbg:%s sr:%s vdi:%s" dbg (s_of_sr sr) (s_of_vdi vdi) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.data_destroy" ~dbg @@ fun dbg ->
      C.VDI.data_destroy dbg sr vdi

    let list_changed_blocks () ~dbg ~sr ~vdi_from ~vdi_to =
      info "VDI.list_changed_blocks dbg:%s sr:%s vdi_from:%s vdi_to:%s" dbg
        (s_of_sr sr) (s_of_vdi vdi_from) (s_of_vdi vdi_to) ;
      let module C = StorageAPI (Idl.Exn.GenClient (struct
        let rpc = of_sr sr
      end)) in
      with_tracing ~name:"VDI.list_changed_blocks" ~dbg @@ fun dbg ->
      C.VDI.list_changed_blocks dbg sr vdi_from vdi_to
  end

  let get_by_name () ~dbg ~name =
    (* Assume it has either the format:
       SR/VDI -- for a particular SR and VDI
       content_id -- for a particular content *)
    let open Xapi_stdext_std.Xstringext in
    with_tracing ~name:"get_by_name" ~dbg @@ fun dbg ->
    match List.filter (fun x -> x <> "") (String.split ~limit:2 '/' name) with
    | [sr; name] ->
        let sr = Storage_interface.Sr.of_string sr in
        let module C = StorageAPI (Idl.Exn.GenClient (struct
          let rpc = of_sr sr
        end)) in
        (sr, C.VDI.get_by_name dbg sr name)
    | [name] -> (
      match
        success_or choose
          (multicast (fun sr _rpc ->
               let module C = StorageAPI (Idl.Exn.GenClient (struct
                 let rpc = of_sr sr
               end)) in
               (sr, C.VDI.get_by_name dbg sr name)
           )
          )
      with
      | SMSuccess (sr, vdi) ->
          (sr, vdi)
      | SMFailure e ->
          raise e
    )
    | _ ->
        raise (Storage_error (Vdi_does_not_exist name))

  module DATA = struct
    let copy () = Storage_migrate.copy

    let copy_into () = Storage_migrate.copy_into

    module MIRROR = struct
      let start () = Storage_migrate.start

      let stop () = Storage_migrate.stop

      let list () = Storage_migrate.list

      let stat () = Storage_migrate.stat

      let receive_start () = Storage_migrate.receive_start

      let receive_finalize () = Storage_migrate.receive_finalize

      let receive_cancel () = Storage_migrate.receive_cancel
    end
  end

  module Policy = struct
    let get_backend_vm () ~dbg:_ ~vm:_ ~sr ~vdi:_ =
      if not (Hashtbl.mem plugins sr) then (
        error "No registered plugin for sr = %s" (s_of_sr sr) ;
        raise (Storage_error (No_storage_plugin_for_sr (s_of_sr sr)))
      ) else
        (Hashtbl.find plugins sr).backend_domain
  end

  module TASK = Storage_smapiv1_wrapper.Impl.TASK
  module UPDATES = Storage_smapiv1_wrapper.Impl.UPDATES
end

module Server = Storage_interface.Server (Mux) ()

module Local_domain_socket = struct
  let path = Filename.concat "/var/lib/xcp" "storage"

  (* receives external requests on Constants.sm_uri *)
  let xmlrpc_handler process req bio _ =
    let body = Http_svr.read_body req bio in
    let s = Buf_io.fd_of bio in
    let rpc = Xmlrpc.call_of_string body in
    (* Printf.fprintf stderr "Request: %s %s\n%!" rpc.Rpc.name (Rpc.to_string (List.hd rpc.Rpc.params)); *)
    let result = process rpc in
    (* Printf.fprintf stderr "Response: %s\n%!" (Rpc.to_string result.Rpc.contents); *)
    let str = Xmlrpc.string_of_response result in
    Http_svr.response_str req s str
end
