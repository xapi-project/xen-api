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
 * @group Storage
*)

(** Notes on failure handling:
    FH1: we always perform SMAPI "side-effects" before
         persisting our state to disk. If the process restarts unexpectedly we
         will attempt to execute the same side-effect twice: this is better than
         zero times.
         Implication: the SMAPI "side-effects" must be idempotent functions.
    FH2: when destroying a "datapath" and a "side-effect" fails, we consider that
         the datapath has "leaked". We remember these, on the assumption that
         the client does not. Before each operation on affected VDIs, we try to
         re-destroy the "leaked" datapath.
         Therefore: if a VDI.detach fails, then subsequent VDI.attaches will try
         to re-perform the VDI.detach, rather than immediately fail with "VDI
         cannot be attached R/W since it is already attached R/O"
    FH3: whenever we detect a leak, it will have a corresponding Error record.

    Notes on each SMAPI call:
    SR.attach:
        if SMAPI sr_attach fails: Failure returned; user should fix problem and retry
    SR.detach:
        if SMAPI vdi_detach* fails: Failures ignored
        if SMAPI sr_detach fails: Failure returned; user should fix problem and retry
        provided sr_detach succeeds then the SR (and VDIs) are forgotten
    VDI.attach, VDI.activate, VDI.deactivate, VDI.detach:
        if VDI has associated leaked datapath, another attempt is made to remove
        if SMAPI calls fails: Failure returned; user should fix problem and retry
        -- in practice the user will call Datapath.destroy's cleanup coode
    DP.destroy:
        if SMAPI vdi_detach* fails: Datapath is "leaked", failure returned
        -- in practice the user will not call this again and rely on SR.detach /
           further VDI operations to clear the problem
*)

(* Possible improvements:
   Always block attach depending on the VDI.read_only not the VBD.mode: this prevents
   the bad pygrub stuck state and we can delete FH2. This would be safer than explicitlly
   leaking an active device node. Although blkback does respect the "mode=r" flag, this
   might have unexpected consequences for the snapshot attach provisioning code. *)

(** Notes on locking:
    We have the following locks:
      SR_1, SR_2, ... SR_n: one lock per SR for serialising SR.attach/SR.detach per-SR
      VDI_1_a, VDI_1, b, ... VDI_n_z: one lock per VDI for serialising all VDI.* ops per-SR
      + various locks to protect accesses to individual tables

    	We hold locks in one of the following sequences:
      VDI_p_q : for a VDI operation
      SR_p    : for an SR.attach
      SR_p, VDI_p_a, VDI_p_b, ..., VDI_p_z : for an SR.detach (to "quiesce" the SR)
*)

module Date = Clock.Date
module Listext = Xapi_stdext_std.Listext
module Unixext = Xapi_stdext_unix.Unixext

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

open Storage_interface
open Storage_task

let s_of_sr = Storage_interface.Sr.string_of

let s_of_vdi = Storage_interface.Vdi.string_of

let s_of_vm = Storage_interface.Vm.string_of

let vm_of_s = Storage_interface.Vm.of_string

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let host_state_path = ref "/var/run/nonpersistent/xapi/storage.db"

let indent x = "    " ^ x

let string_of_date x = Date.to_rfc3339 (Date.of_unix_time x)

let with_dbg ~name ~dbg f =
  Debug_info.with_dbg ~with_thread:true ~module_name:"SMAPIv1-Wrapper" ~name
    ~dbg f

let rpc_fns keyty valty =
  let rpc_of hashtbl =
    let v =
      Hashtbl.fold
        (fun k v acc ->
          let keystr =
            match Rpcmarshal.marshal keyty.Rpc.Types.ty k with
            | Rpc.String x ->
                x
            | _ ->
                failwith "Runtime error marshalling non-stringish key"
          in
          let valuerpc = Rpcmarshal.marshal valty.Rpc.Types.ty v in
          (keystr, valuerpc) :: acc
        )
        hashtbl []
    in
    Rpc.Dict v
  in
  let of_rpc rpc =
    let ( >>= ) m f = match m with Ok x -> f x | Error y -> Error y in
    let unm t v = Rpcmarshal.unmarshal t.Rpc.Types.ty v in
    match rpc with
    | Rpc.Dict kvs ->
        let res =
          List.fold_left
            (fun hashtbl (k, v) ->
              hashtbl >>= fun h ->
              unm keyty (Rpc.String k) >>= fun key ->
              unm valty v >>= fun value ->
              Hashtbl.replace h key value ;
              Ok h
            )
            (Ok (Hashtbl.create (List.length kvs)))
            kvs
        in
        res
    | _ ->
        Error (`Msg "Trying to unmarshal hashtblish thing from a non-dict")
  in
  (of_rpc, rpc_of)

module Dp = struct type t = string [@@deriving rpcty] end

module Vdi = struct
  (** Represents the information known about a VDI *)
  type t = {
      attach_info: backend option
          (** Some path when attached; None otherwise *)
    ; dps: (Dp.t * Vdi_automaton.state) list
          (** state of the VDI from each dp's PoV *)
    ; leaked: Dp.t list  (** "leaked" dps *)
    ; dpv: (Dp.t * Vm.t) list [@default []]  (** VM Datapath *)
  }
  [@@deriving rpcty]

  let empty () = {attach_info= None; dps= []; leaked= []; dpv= []}

  (** [superstate x] returns the actual state of the backing VDI by finding the "max" of
      	    the states from the clients' PsoV *)
  let superstate x = Vdi_automaton.superstate (List.map snd x.dps)

  let dp_on_vdi dp t = List.mem_assoc dp t.dps

  let get_dp_state dp t =
    if dp_on_vdi dp t then
      List.assoc dp t.dps
    else
      Vdi_automaton.Detached

  (** [get_dp_vm] returns [vm] of type Vm.t. If the mapping between a datapath and vm exists, it will return the vm. Owtherwise a blank vm will be returned*)
  let get_dp_vm dp t =
    let _vm = List.assoc_opt dp t.dpv in
    match _vm with
    | Some vm ->
        vm
    | None ->
        warn
          "%s: No vm was found associated with a datapath. Has xapi been \
           restarted since it was upgraded?"
          __FUNCTION__ ;
        vm_of_s ""

  let add_or_update_dp dp vm state t =
    let rests = List.remove_assoc dp t.dps in
    let restv = List.remove_assoc dp t.dpv in
    {
      t with
      dps=
        (if state = Vdi_automaton.Detached then rests else (dp, state) :: rests)
    ; dpv= (if state = Vdi_automaton.Detached then restv else (dp, vm) :: restv)
    }

  let get_leaked t = t.leaked

  let leaked t dp = List.mem dp t.leaked

  let all _ _ = true

  let remove_leaked dp t =
    {t with leaked= List.filter (fun u -> u <> dp) t.leaked}

  let add_leaked dp t =
    let t' = remove_leaked dp t in
    {t' with leaked= dp :: t'.leaked}

  let dp t = List.map fst t.dps

  let dpv t = t.dpv

  let dps t = t.dps

  (** [perform dp vm op t] updates VDI [t] given the request to perform [op] by [dp] and then updates the dp on the vdi *)

  let perform dp vm op t =
    let state = get_dp_state dp t in
    let state' = Vdi_automaton.( + ) state op in
    add_or_update_dp dp vm state' t

  let to_string_list x =
    let title =
      Printf.sprintf "%s (device=%s)"
        (Vdi_automaton.string_of_state (superstate x))
        (Option.fold ~none:"None"
           ~some:(fun x ->
             "Some " ^ Jsonrpc.to_string (Storage_interface.(rpc_of backend) x)
           )
           x.attach_info
        )
    in
    let of_dp (dp, state) =
      Printf.sprintf "DP: %s: %s%s" dp
        (Vdi_automaton.string_of_state state)
        (if List.mem dp x.leaked then "  ** LEAKED" else "")
    in
    title :: List.map indent (List.map of_dp x.dps)
end

module Sr = struct
  (** Represents the state of an SR *)
  type vdis = (Storage_interface.Vdi.t, Vdi.t) Hashtbl.t

  let typ_of_vdis =
    let of_rpc, rpc_of = rpc_fns Storage_interface.Vdi.t Vdi.t in
    Rpc.Types.(
      Abstract {aname= "vdis"; test_data= [Hashtbl.create 1]; of_rpc; rpc_of}
    )

  type t = {vdis: vdis  (** All tracked VDIs *)} [@@deriving rpcty]

  let empty () = {vdis= Hashtbl.create 10}

  let m = Mutex.create ()

  let find vdi sr = with_lock m (fun () -> Hashtbl.find_opt sr.vdis vdi)

  let add_or_replace vdi vdi_t sr =
    with_lock m (fun () -> Hashtbl.replace sr.vdis vdi vdi_t)

  let list sr =
    with_lock m (fun () ->
        Hashtbl.fold (fun k v acc -> (k, v) :: acc) sr.vdis []
    )

  let remove vdi sr = with_lock m (fun () -> Hashtbl.remove sr.vdis vdi)

  let to_string_list x =
    Hashtbl.fold
      (fun vdi vdi_t acc ->
        Printf.sprintf "VDI %s" (s_of_vdi vdi)
        :: List.map indent (Vdi.to_string_list vdi_t)
        @ acc
      )
      x.vdis []
end

module Host = struct
  type srs = (Storage_interface.Sr.t, Sr.t) Hashtbl.t

  let typ_of_srs =
    let of_rpc, rpc_of = rpc_fns Storage_interface.Sr.t Sr.t in
    Rpc.Types.(Abstract {aname= "srs"; test_data= []; of_rpc; rpc_of})

  (** Represents the state of a host *)
  type t = {srs: srs} [@@deriving rpcty]

  let empty () = {srs= Hashtbl.create 10}

  let m = Mutex.create ()

  let find sr h = with_lock m (fun () -> Hashtbl.find_opt h.srs sr)

  let remove sr h = with_lock m (fun () -> Hashtbl.remove h.srs sr)

  let add_or_replace sr sr_t h =
    with_lock m (fun () -> Hashtbl.replace h.srs sr sr_t)

  let list h =
    with_lock m (fun () -> Hashtbl.fold (fun k v acc -> (k, v) :: acc) h.srs [])

  (** All global state held here *)
  let host = ref (empty ())
end

module Errors = struct
  (** Used for remembering the last [max] errors *)
  type error = {
      dp: string  (** person who triggered the error *)
    ; time: float  (** time the error happened *)
    ; sr: Storage_interface.Sr.t
    ; vdi: Storage_interface.Vdi.t
    ; error: string
  }
  [@@deriving rpcty]

  type t = error list [@@deriving rpcty]

  let max = 100

  let errors = ref []

  let errors_m = Mutex.create ()

  let add dp sr vdi code =
    with_lock errors_m (fun () ->
        let t = {dp; time= Unix.gettimeofday (); sr; vdi; error= code} in
        errors := Listext.List.take 100 (t :: !errors)
    )

  let list () = with_lock errors_m (fun () -> !errors)

  let to_string x =
    Printf.sprintf "%s @ %s; sr:%s vdi:%s error:%s" x.dp (string_of_date x.time)
      (s_of_sr x.sr) (s_of_vdi x.vdi) x.error
end

module Everything = struct
  type t = {host: Host.t; errors: Errors.t} [@@deriving rpcty]

  let make () = {host= !Host.host; errors= !Errors.errors}

  let to_file filename h =
    let rpc = with_lock Host.m (fun () -> Rpcmarshal.marshal typ_of h) in
    let s = Jsonrpc.to_string rpc in
    Unixext.write_string_to_file filename s

  let of_file filename =
    let s = Unixext.string_of_file filename in
    let rpc = Jsonrpc.of_string s in
    match Rpcmarshal.unmarshal typ_of rpc with
    | Ok x ->
        x
    | Error (`Msg m) ->
        error "Failed to unmarshal Everything state: %s" m ;
        failwith "Internal error: failed to restore storage state"

  let set h =
    Host.host := h.host ;
    Errors.errors := h.errors
end

module Wrapper =
functor
  (Impl : Server_impl)
  ->
  struct
    type context = unit

    module Query = struct
      let query = Impl.Query.query

      let diagnostics = Impl.Query.diagnostics
    end

    module VDI = struct
      type vdi_locks = (string, unit) Storage_locks.t

      (** Map of SR name to vdi_locks table *)
      let locks : (string, vdi_locks) Hashtbl.t = Hashtbl.create 10

      (* This protects the 'locks' table only *)
      let locks_m = Mutex.create ()

      let locks_find sr =
        let sr_key = s_of_sr sr in
        with_lock locks_m (fun () ->
            match Hashtbl.find_opt locks sr_key with
            | Some x ->
                x
            | None ->
                let result = Storage_locks.make () in
                Hashtbl.replace locks sr_key result ;
                result
        )

      let locks_remove sr =
        with_lock locks_m (fun () -> Hashtbl.remove locks (s_of_sr sr))

      let with_vdi sr vdi f =
        let locks = locks_find sr in
        Storage_locks.with_instance_lock locks (s_of_vdi vdi) f

      let with_all_vdis sr f =
        let locks = locks_find sr in
        Storage_locks.with_master_lock locks f

      let side_effects context dbg dp sr sr_t vdi vdi_t vm ops =
        let perform_one vdi_t (op, _state_on_fail) =
          try
            let vdi_t = Vdi.perform dp vm op vdi_t in
            let new_vdi_t =
              match op with
              | Vdi_automaton.Nothing ->
                  vdi_t
              | Vdi_automaton.Attach ro_rw ->
                  let read_write = ro_rw = Vdi_automaton.RW in
                  let x =
                    Impl.VDI.attach3 context ~dbg ~dp ~sr ~vdi ~vm ~read_write
                  in
                  {vdi_t with Vdi.attach_info= Some x}
              | Vdi_automaton.Activate ->
                  Impl.VDI.activate3 context ~dbg ~dp ~sr ~vdi ~vm ;
                  vdi_t
              | Vdi_automaton.Deactivate ->
                  Storage_migrate.pre_deactivate_hook ~dbg ~dp ~sr ~vdi ;
                  Impl.VDI.deactivate context ~dbg ~dp ~sr ~vdi ~vm ;
                  Storage_migrate.post_deactivate_hook ~sr ~vdi ~dp ;
                  vdi_t
              | Vdi_automaton.Detach ->
                  Impl.VDI.detach context ~dbg ~dp ~sr ~vdi ~vm ;
                  vdi_t
            in
            Sr.add_or_replace vdi new_vdi_t sr_t ;
            new_vdi_t
          with
          | Storage_interface.Storage_error
              (Internal_error "Storage_access.No_VDI") as e
            when op = Vdi_automaton.Deactivate || op = Vdi_automaton.Detach ->
              error
                "Storage_smapiv1_wrapper: caught exception %s while doing %s . \
                 Continuing as if succesful, being optimistic"
                (Printexc.to_string e)
                (Vdi_automaton.string_of_op op) ;
              vdi_t
          | e ->
              error
                "Storage_smapiv1_wrapper: dp:%s sr:%s vdi:%s op:%s error:%s \
                 backtrace:%s"
                dp (s_of_sr sr) (s_of_vdi vdi)
                (Vdi_automaton.string_of_op op)
                (Printexc.to_string e)
                (Printexc.get_backtrace ()) ;
              raise e
        in
        List.fold_left perform_one vdi_t ops

      let perform_nolock context ~dbg ~dp ~sr ~vdi ~vm this_op =
        debug "%s dp=%s, sr=%s, vdi=%s, vm=%s, op=%s" __FUNCTION__ dp
          (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm)
          (Vdi_automaton.string_of_op this_op) ;
        match Host.find sr !Host.host with
        | None ->
            raise (Storage_error (Sr_not_attached (s_of_sr sr)))
        | Some sr_t ->
            let vdi_t =
              Option.value ~default:(Vdi.empty ()) (Sr.find vdi sr_t)
            in
            let vdi_t' =
              try
                (* Compute the overall state ('superstate') of the VDI *)
                let superstate = Vdi.superstate vdi_t in
                (* We first assume the operation succeeds and compute the new
                   						   datapath+VDI state *)
                let new_vdi_t = Vdi.perform dp vm this_op vdi_t in
                (* Compute the new overall state ('superstate') *)
                let superstate' = Vdi.superstate new_vdi_t in
                (* Compute the real operations which would drive the system from
                   						   superstate to superstate'. These may fail: if so we revert the
                   						   datapath+VDI state to the most appropriate value. *)
                let ops = Vdi_automaton.( - ) superstate superstate' in
                debug "%s %s -> %s: %s" __FUNCTION__
                  (Vdi_automaton.string_of_state superstate)
                  (Vdi_automaton.string_of_state superstate')
                  (String.concat ", "
                     (List.map
                        (fun (op, _) -> Vdi_automaton.string_of_op op)
                        ops
                     )
                  ) ;
                side_effects context dbg dp sr sr_t vdi vdi_t vm ops
              with e ->
                let e =
                  match e with
                  | Vdi_automaton.No_operation (a, b) ->
                      Storage_error (Illegal_transition (a, b))
                  | e ->
                      e
                in
                Errors.add dp sr vdi (Printexc.to_string e) ;
                raise e
            in
            (* Even if there were no side effects on the underlying VDI, we still need
               				   to update the SR to update this DP's view of the state.
               				   However if nothing changed (e.g. because this was the detach of a DP
               				   which had not attached this VDI) then we won't need to update our on-disk state *)
            let vdi_t' = Vdi.perform dp vm this_op vdi_t' in
            if vdi_t <> vdi_t' then (
              Sr.add_or_replace vdi vdi_t' sr_t ;
              (* If the new VDI state is "detached" then we remove it from the table
                 					   altogether *)
              debug "dbg:%s dp:%s sr:%s vdi:%s superstate:%s" dbg dp
                (s_of_sr sr) (s_of_vdi vdi)
                (Vdi_automaton.string_of_state (Vdi.superstate vdi_t')) ;
              if Vdi.superstate vdi_t' = Vdi_automaton.Detached then
                Sr.remove vdi sr_t ;
              (* FH1: Perform the side-effect first: in the case of a failure half-way
                 					   through we would rather perform the side-effect twice than never at
                 					   all. *)
              Everything.to_file !host_state_path (Everything.make ())
            ) ;
            vdi_t'

      (* Attempt to remove a possibly-active datapath associated with [vdi] *)
      let destroy_datapath_nolock context ~dbg ~dp ~sr ~vdi ~vm ~allow_leak =
        match Host.find sr !Host.host with
        | None ->
            raise (Storage_error (Sr_not_attached (s_of_sr sr)))
        | Some sr_t ->
            Option.iter
              (fun vdi_t ->
                let current_state = Vdi.get_dp_state dp vdi_t in
                let desired_state = Vdi_automaton.Detached in
                let ops =
                  List.map fst (Vdi_automaton.( - ) current_state desired_state)
                in
                try
                  ignore
                    (List.fold_left
                       (fun _ op ->
                         perform_nolock context ~dbg ~dp ~sr ~vdi ~vm op
                       )
                       vdi_t ops
                    )
                with e ->
                  if not allow_leak then (
                    Sr.add_or_replace vdi (Vdi.add_leaked dp vdi_t) sr_t ;
                    Everything.to_file !host_state_path (Everything.make ()) ;
                    raise e
                  ) else (
                    (* allow_leak means we can forget this dp *)
                    info
                      "setting dp:%s state to %s, even though operation failed \
                       because allow_leak set"
                      dp
                      (Vdi_automaton.string_of_state desired_state) ;
                    let vdi_t =
                      Vdi.add_or_update_dp dp vm desired_state vdi_t
                    in
                    if Vdi.superstate vdi_t = Vdi_automaton.Detached then
                      Sr.remove vdi sr_t
                    else
                      Sr.add_or_replace vdi vdi_t sr_t ;
                    Everything.to_file !host_state_path (Everything.make ())
                  )
              )
              (Sr.find vdi sr_t)

      (* Attempt to clear leaked datapaths associed with this vdi *)
      let remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi ?vm which next =
        let dpv =
          match Host.find sr !Host.host with
          | None ->
              []
          | Some sr_t -> (
            match Sr.find vdi sr_t with
            | Some vdi_t ->
                List.filter (fun (dp, _vm) -> which vdi_t dp) (Vdi.dpv vdi_t)
            | None ->
                []
          )
        in
        let failures =
          List.fold_left
            (fun acc (dp, vm') ->
              info "Attempting to destroy datapath dp:%s sr:%s vdi:%s" dp
                (s_of_sr sr) (s_of_vdi vdi) ;
              try
                let vm = match vm with None -> vm' | Some vm -> vm in
                destroy_datapath_nolock context ~dbg ~dp ~sr ~vdi ~vm
                  ~allow_leak:false ;
                acc
              with e -> e :: acc
            )
            [] dpv
        in
        match failures with [] -> next () | f :: _ -> raise f

      let epoch_begin context ~dbg ~sr ~vdi ~vm ~persistent =
        with_dbg ~name:"VDI.epoch_begin" ~dbg @@ fun di ->
        info "VDI.epoch_begin dbg:%s sr:%s vdi:%s vm:%s persistent:%b" di.log
          (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) persistent ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi ~vm Vdi.leaked
              (fun () ->
                Impl.VDI.epoch_begin context ~dbg ~sr ~vdi ~vm ~persistent
            )
        )

      let attach3 context ~dbg ~dp ~sr ~vdi ~vm ~read_write =
        with_dbg ~name:"VDI.attach3" ~dbg @@ fun di ->
        info "VDI.attach3 dbg:%s dp:%s sr:%s vdi:%s vm:%s read_write:%b" di.log
          dp (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) read_write ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi ~vm Vdi.leaked
              (fun () ->
                let state =
                  perform_nolock context ~dbg ~dp ~sr ~vdi ~vm
                    (Vdi_automaton.Attach
                       ( if read_write then
                           Vdi_automaton.RW
                         else
                           Vdi_automaton.RO
                       )
                    )
                in
                Option.get state.Vdi.attach_info
            )
        )

      let attach2 context ~dbg ~dp ~sr ~vdi ~read_write =
        with_dbg ~name:"VDI.attach2" ~dbg @@ fun di ->
        info "VDI.attach2 dbg:%s dp:%s sr:%s vdi:%s read_write:%b" di.log dp
          (s_of_sr sr) (s_of_vdi vdi) read_write ;
        (*Support calls from older XAPI during migrate operation (dom 0 attach )*)
        let vm = vm_of_s "0" in
        let dbg = Debug_info.to_string di in
        attach3 context ~dbg ~dp ~sr ~vdi ~vm ~read_write

      let attach context ~dbg ~dp ~sr ~vdi ~read_write =
        with_dbg ~name:"VDI.attach" ~dbg @@ fun di ->
        info "VDI.attach dbg:%s dp:%s sr:%s vdi:%s read_write:%b" di.log dp
          (s_of_sr sr) (s_of_vdi vdi) read_write ;
        let vm = vm_of_s "0" in
        let dbg = Debug_info.to_string di in
        let backend = attach3 context ~dbg ~dp ~sr ~vdi ~vm ~read_write in
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

      let activate3 context ~dbg ~dp ~sr ~vdi ~vm =
        with_dbg ~name:"VDI.activate3" ~dbg @@ fun di ->
        info "VDI.activate3 dbg:%s dp:%s sr:%s vdi:%s vm:%s" di.log dp
          (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi ~vm Vdi.leaked
              (fun () ->
                ignore
                  (perform_nolock context ~dbg ~dp ~sr ~vdi ~vm
                     Vdi_automaton.Activate
                  )
            )
        )

      let activate_readonly = activate3

      let activate context ~dbg ~dp ~sr ~vdi =
        with_dbg ~name:"VDI.activate" ~dbg @@ fun di ->
        info "VDI.activate dbg:%s dp:%s sr:%s vdi:%s " di.log dp (s_of_sr sr)
          (s_of_vdi vdi) ;
        (*Support calls from older XAPI during migrate operation (dom 0 attach )*)
        let vm = vm_of_s "0" in
        let dbg = Debug_info.to_string di in
        activate3 context ~dbg ~dp ~sr ~vdi ~vm

      let deactivate context ~dbg ~dp ~sr ~vdi ~vm =
        with_dbg ~name:"VDI.deactivate" ~dbg @@ fun di ->
        info "VDI.deactivate dbg:%s dp:%s sr:%s vdi:%s vm:%s" di.log dp
          (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi ~vm Vdi.leaked
              (fun () ->
                ignore
                  (perform_nolock context ~dbg ~dp ~sr ~vdi ~vm
                     Vdi_automaton.Deactivate
                  )
            )
        )

      let detach context ~dbg ~dp ~sr ~vdi ~vm =
        with_dbg ~name:"VDI.detach" ~dbg @@ fun di ->
        info "VDI.detach dbg:%s dp:%s sr:%s vdi:%s vm:%s" di.log dp (s_of_sr sr)
          (s_of_vdi vdi) (s_of_vm vm) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi ~vm Vdi.leaked
              (fun () ->
                ignore
                  (perform_nolock context ~dbg ~dp ~sr ~vdi ~vm
                     Vdi_automaton.Detach
                  )
            )
        )

      let epoch_end context ~dbg ~sr ~vdi ~vm =
        with_dbg ~name:"VDI.epoch_end" ~dbg @@ fun di ->
        info "VDI.epoch_end dbg:%s sr:%s vdi:%s vm:%s" di.log (s_of_sr sr)
          (s_of_vdi vdi) (s_of_vm vm) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi ~vm Vdi.leaked
              (fun () -> Impl.VDI.epoch_end context ~dbg ~sr ~vdi ~vm
            )
        )

      let create context ~dbg ~sr ~vdi_info =
        with_dbg ~name:"VDI.create" ~dbg @@ fun di ->
        info "VDI.create dbg:%s sr:%s vdi_info:%s" di.log (s_of_sr sr)
          (string_of_vdi_info vdi_info) ;
        let dbg = Debug_info.to_string di in
        let result = Impl.VDI.create context ~dbg ~sr ~vdi_info in
        match result with
        | {virtual_size= virtual_size'; _}
          when virtual_size' < vdi_info.virtual_size ->
            error "VDI.create dbg:%s created a smaller VDI (%Ld)" dbg
              virtual_size' ;
            raise
              (Storage_error
                 (Backend_error
                    ( "SR_BACKEND_FAILURE"
                    , [
                        "Disk too small"
                      ; Int64.to_string vdi_info.virtual_size
                      ; Int64.to_string virtual_size'
                      ]
                    )
                 )
              )
        | result ->
            result

      let snapshot_and_clone call_name call_f context ~dbg ~sr ~vdi_info =
        with_dbg ~name:call_name ~dbg @@ fun di ->
        info "%s dbg:%s sr:%s vdi_info:%s" call_name di.log (s_of_sr sr)
          (string_of_vdi_info vdi_info) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi_info.vdi (fun () -> call_f context ~dbg ~sr ~vdi_info)

      let snapshot = snapshot_and_clone "VDI.snapshot" Impl.VDI.snapshot

      let clone = snapshot_and_clone "VDI.clone" Impl.VDI.clone

      let set_name_label context ~dbg ~sr ~vdi ~new_name_label =
        with_dbg ~name:"VDI.set_name_label" ~dbg @@ fun di ->
        info "VDI.set_name_label dbg:%s sr:%s vdi:%s new_name_label:%s" di.log
          (s_of_sr sr) (s_of_vdi vdi) new_name_label ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            Impl.VDI.set_name_label context ~dbg ~sr ~vdi ~new_name_label
        )

      let set_name_description context ~dbg ~sr ~vdi ~new_name_description =
        with_dbg ~name:"VDI.set_name_description" ~dbg @@ fun di ->
        info
          "VDI.set_name_description dbg:%s sr:%s vdi:%s new_name_description:%s"
          di.log (s_of_sr sr) (s_of_vdi vdi) new_name_description ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            Impl.VDI.set_name_description context ~dbg ~sr ~vdi
              ~new_name_description
        )

      let resize context ~dbg ~sr ~vdi ~new_size =
        with_dbg ~name:"VDI.resize" ~dbg @@ fun di ->
        info "VDI.resize dbg:%s sr:%s vdi:%s new_size:%Ld" di.log (s_of_sr sr)
          (s_of_vdi vdi) new_size ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            Impl.VDI.resize context ~dbg ~sr ~vdi ~new_size
        )

      let destroy_and_data_destroy call_name call_f context ~dbg ~sr ~vdi =
        with_dbg ~name:call_name ~dbg @@ fun di ->
        info "%s dbg:%s sr:%s vdi:%s" call_name di.log (s_of_sr sr)
          (s_of_vdi vdi) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            remove_datapaths_andthen_nolock context ~dbg ~sr ~vdi Vdi.all
              (fun () -> call_f context ~dbg ~sr ~vdi
            )
        )

      let destroy = destroy_and_data_destroy "VDI.destroy" Impl.VDI.destroy

      let data_destroy =
        destroy_and_data_destroy "VDI.data_destroy" Impl.VDI.data_destroy

      let stat context ~dbg ~sr ~vdi =
        with_dbg ~name:"VDI.stat" ~dbg @@ fun di ->
        info "VDI.stat dbg:%s sr:%s vdi:%s" di.log (s_of_sr sr) (s_of_vdi vdi) ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.stat context ~dbg ~sr ~vdi

      let introduce context ~dbg ~sr ~uuid ~sm_config ~location =
        with_dbg ~name:"VDI.introduce" ~dbg @@ fun di ->
        info "VDI.introduce dbg:%s sr:%s uuid:%s sm_config:%s location:%s"
          di.log (s_of_sr sr) uuid
          (String.concat ", " (List.map (fun (k, v) -> k ^ ":" ^ v) sm_config))
          location ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.introduce context ~dbg ~sr ~uuid ~sm_config ~location

      let set_persistent context ~dbg ~sr ~vdi ~persistent =
        with_dbg ~name:"VDI.set_persistent" ~dbg @@ fun di ->
        info "VDI.set_persistent dbg:%s sr:%s vdi:%s persistent:%b" di.log
          (s_of_sr sr) (s_of_vdi vdi) persistent ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () ->
            Impl.VDI.set_persistent context ~dbg ~sr ~vdi ~persistent
        )

      let get_by_name context ~dbg ~sr ~name =
        with_dbg ~name:"VDI.get_by_name" ~dbg @@ fun di ->
        info "VDI.get_by_name dbg:%s sr:%s name:%s" di.log (s_of_sr sr) name ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.get_by_name context ~dbg ~sr ~name

      let set_content_id context ~dbg ~sr ~vdi ~content_id =
        with_dbg ~name:"VDI.set_content_id" ~dbg @@ fun di ->
        info "VDI.set_content_id dbg:%s sr:%s vdi:%s content_id:%s" di.log
          (s_of_sr sr) (s_of_vdi vdi) content_id ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.set_content_id context ~dbg ~sr ~vdi ~content_id

      let similar_content context ~dbg ~sr ~vdi =
        with_dbg ~name:"VDI.similar_content" ~dbg @@ fun di ->
        info "VDI.similar_content dbg:%s sr:%s vdi:%s" di.log (s_of_sr sr)
          (s_of_vdi vdi) ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.similar_content context ~dbg ~sr ~vdi

      let compose context ~dbg ~sr ~vdi1 ~vdi2 =
        with_dbg ~name:"VDI.compose" ~dbg @@ fun di ->
        info "VDI.compose dbg:%s sr:%s vdi1:%s vdi2:%s" di.log (s_of_sr sr)
          (s_of_vdi vdi1) (s_of_vdi vdi2) ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.compose context ~dbg ~sr ~vdi1 ~vdi2

      let add_to_sm_config context ~dbg ~sr ~vdi ~key ~value =
        with_dbg ~name:"VDI.add_to_sm_config" ~dbg @@ fun di ->
        info "VDI.add_to_sm_config dbg:%s sr:%s vdi:%s key:%s value:%s" di.log
          (s_of_sr sr) (s_of_vdi vdi) key value ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.add_to_sm_config context ~dbg ~sr ~vdi ~key ~value

      let remove_from_sm_config context ~dbg ~sr ~vdi ~key =
        with_dbg ~name:"VDI.remove_from_sm_config" ~dbg @@ fun di ->
        info "VDI.remove_from_sm_config dbg:%s sr:%s vdi:%s key:%s" di.log
          (s_of_sr sr) (s_of_vdi vdi) key ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.remove_from_sm_config context ~dbg ~sr ~vdi ~key

      let get_url context ~dbg ~sr ~vdi =
        with_dbg ~name:"VDI.get_url" ~dbg @@ fun di ->
        info "VDI.get_url dbg:%s sr:%s vdi:%s" di.log (s_of_sr sr) (s_of_vdi vdi) ;
        let dbg = Debug_info.to_string di in
        Impl.VDI.get_url context ~dbg ~sr ~vdi

      let enable_cbt context ~dbg ~sr ~vdi =
        with_dbg ~name:"VDI.enabled_cbt" ~dbg @@ fun di ->
        info "VDI.enable_cbt dbg:%s sr:%s vdi:%s" di.log (s_of_sr sr)
          (s_of_vdi vdi) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () -> Impl.VDI.enable_cbt context ~dbg ~sr ~vdi)

      let disable_cbt context ~dbg ~sr ~vdi =
        with_dbg ~name:"VDI.disable_cbt" ~dbg @@ fun di ->
        info "VDI.disable_cbt dbg:%s sr:%s vdi:%s" di.log (s_of_sr sr)
          (s_of_vdi vdi) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi (fun () -> Impl.VDI.disable_cbt context ~dbg ~sr ~vdi)

      (** The [sr] parameter is the SR of VDI [vdi_to]. *)
      let list_changed_blocks context ~dbg ~sr ~vdi_from ~vdi_to =
        with_dbg ~name:"VDI.list_changed_blocks" ~dbg @@ fun di ->
        info "VDI.list_changed_blocks dbg:%s sr:%s vdi_from:%s vdi_to:%s" di.log
          (s_of_sr sr) (s_of_vdi vdi_from) (s_of_vdi vdi_to) ;
        let dbg = Debug_info.to_string di in
        with_vdi sr vdi_to (fun () ->
            Impl.VDI.list_changed_blocks context ~dbg ~sr ~vdi_from ~vdi_to
        )
    end

    let get_by_name context ~dbg ~name =
      with_dbg ~name:"get_by_name" ~dbg @@ fun di ->
      debug "get_by_name dbg:%s name:%s" di.log name ;
      let dbg = Debug_info.to_string di in
      Impl.get_by_name context ~dbg ~name

    module DP = struct
      let create _context ~dbg:_ ~id = id

      (** [destroy_sr context dp sr allow_leak vdi_already_locked] attempts to free
        		    the resources associated with [dp] in [sr]. If [vdi_already_locked] then
        		    it is assumed that all VDIs are already locked. *)
      let destroy_sr context ~dbg ~dp ~sr ~sr_t ~allow_leak vdi_already_locked =
        (* Every VDI in use by this session should be detached and deactivated
           This code makes the assumption that a datapath is only on 0 or 1 VDIs. However, it retains debug code (identified below) to verify this.
           It also assumes that the VDIs associated with a datapath don't change during its execution - again it retains debug code to verify this.
        *)
        let vdis = Sr.list sr_t in
        (* Note that we assume this filter returns 0 or 1 items, but we need to verify that. *)
        let vdis_with_dp =
          List.filter (fun (_, vdi_t) -> Vdi.dp_on_vdi dp vdi_t) vdis
        in
        debug "[destroy_sr] Filtered VDI count:%d" (List.length vdis_with_dp) ;
        List.iter
          (fun (vdi, _) ->
            debug "[destroy_sr] VDI found with the dp is %s" (s_of_vdi vdi)
          )
          vdis_with_dp ;
        let locker vdi =
          if vdi_already_locked then
            fun f -> f ()
          else
            VDI.with_vdi sr vdi
        in
        (* This is debug code to verify that no more than 1 VDI matched the datapath. We also convert the 0 and 1 cases to an Option which is more natural to work with *)
        let vdi_to_remove =
          match vdis_with_dp with
          | [] ->
              None
          | [x] ->
              Some x
          | _ ->
              raise
                (Storage_interface.Storage_error
                   (Backend_error
                      ( Api_errors.internal_error
                      , [
                          Printf.sprintf
                            "Expected 0 or 1 VDI with datapath, had %d"
                            (List.length vdis_with_dp)
                        ]
                      )
                   )
                )
        in
        (* From this point if it didn't raise, the assumption of 0 or 1 VDIs holds *)
        let failure =
          match vdi_to_remove with
          | None ->
              None
          | Some (vdi, vdi_t) ->
              locker vdi (fun () ->
                  try
                    let vm = Vdi.get_dp_vm dp vdi_t in
                    VDI.destroy_datapath_nolock context ~dbg ~dp ~sr ~vdi ~vm
                      ~allow_leak ;
                    None
                  with e -> Some e
              )
        in
        (* This is debug code to assert that we removed the datapath from all VDIs by looking for a situation where a VDI not known about has the datapath at this point *)
        (* Can't just check for vdis_with_dp = 0, the actual removal isn't necessarily complete at this point *)
        let vdi_ident =
          match vdi_to_remove with None -> None | Some (vdi, _) -> Some vdi
        in
        let vdis = Sr.list sr_t in
        let vdis_with_dp =
          List.filter (fun (_, vdi_t) -> Vdi.dp_on_vdi dp vdi_t) vdis
        in
        (* Function to see if a (vdi, vdi_t) matches vdi_ident *)
        let matches (vdi, _) =
          match vdi_ident with None -> false | Some s -> vdi = s
        in
        let race_occured =
          match vdis_with_dp with
          | [] ->
              false
          | [v] ->
              not (matches v)
          | _ ->
              true
        in
        ( if race_occured then
            let message =
              [
                Printf.sprintf
                  "Expected no new VDIs with DP after destroy_sr. VDI expected \
                   with id %s"
                  ( match vdi_ident with
                  | None ->
                      "(not attached)"
                  | Some s ->
                      s_of_vdi s
                  )
              ]
              @ List.map
                  (fun (vdi, _) ->
                    Printf.sprintf "VDI found with the dp is %s" (s_of_vdi vdi)
                  )
                  vdis_with_dp
            in
            raise
              (Storage_interface.Storage_error
                 (Backend_error (Api_errors.internal_error, message))
              )
        ) ;
        failure

      let destroy' context ~dbg ~dp ~allow_leak =
        info "DP.destroy dbg:%s dp:%s allow_leak:%b" dbg dp allow_leak ;
        let failures =
          Host.list !Host.host
          |> List.filter_map (fun (sr, sr_t) ->
                 destroy_sr context ~dbg ~dp ~sr ~sr_t ~allow_leak false
             )
        in
        match (failures, allow_leak) with
        | [], _ ->
            ()
        | f :: _, false ->
            error "Leaked datapath: dp: %s" dp ;
            raise f
        | _ :: _, true ->
            info "Forgetting leaked datapath: dp: %s" dp ;
            ()

      let destroy _context ~dbg:_ ~dp:_ ~allow_leak:_ =
        (* This is no longer called. The mux redirects it to DP.destroy2. *)
        assert false

      let destroy2 context ~dbg ~dp ~sr ~vdi ~vm ~allow_leak =
        with_dbg ~name:"DP.destroy2" ~dbg @@ fun di ->
        info "DP.destroy2 dbg:%s dp:%s sr:%s vdi:%s vm:%s allow_leak:%b" di.log
          dp (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) allow_leak ;
        let dbg = Debug_info.to_string di in
        destroy' context ~dbg ~dp ~allow_leak

      let diagnostics _context () =
        let srs = Host.list !Host.host in
        let of_sr (sr, sr_t) =
          let title = Printf.sprintf "SR %s" (s_of_sr sr) in
          title :: List.map indent (Sr.to_string_list sr_t)
        in
        let srs = List.concat_map of_sr srs in
        let errors = List.map Errors.to_string (Errors.list ()) in
        let errors =
          ( if errors <> [] then
              "The following errors have been logged:"
            else
              "No errors have been logged."
          )
          :: errors
        in
        let lines =
          ["The following SRs are attached:"]
          @ List.map indent srs
          @ [""]
          @ errors
        in
        String.concat "" (List.map (fun x -> x ^ "\n") lines)

      let attach_info _context ~dbg:_ ~sr ~vdi ~dp ~vm:_ =
        let srs = Host.list !Host.host in
        let sr_state = List.assoc sr srs in
        let vdi_state = Hashtbl.find sr_state.Sr.vdis vdi in
        let dp_state = Vdi.get_dp_state dp vdi_state in
        debug "Looking for dp: %s" dp ;
        match (dp_state, vdi_state.Vdi.attach_info) with
        | Vdi_automaton.Activated _, Some attach_info ->
            attach_info
        | _ ->
            raise
              (Storage_error
                 (Internal_error
                    (Printf.sprintf "sr: %s vdi: %s Datapath %s not attached"
                       (s_of_sr sr) (s_of_vdi vdi) dp
                    )
                 )
              )

      let stat_vdi _context ~dbg ~sr ~vdi () =
        info "DP.stat_vdi dbg:%s sr:%s vdi:%s" dbg (s_of_sr sr) (s_of_vdi vdi) ;
        VDI.with_vdi sr vdi (fun () ->
            match Host.find sr !Host.host with
            | None ->
                raise (Storage_error (Sr_not_attached (s_of_sr sr)))
            | Some sr_t ->
                let vdi_t =
                  Option.value ~default:(Vdi.empty ()) (Sr.find vdi sr_t)
                in
                {
                  superstate= Vdi.superstate vdi_t
                ; dps=
                    List.map
                      (fun dp -> (dp, Vdi.get_dp_state dp vdi_t))
                      (Vdi.dp vdi_t)
                }
        )
    end

    module DATA = struct
      let u x = raise Storage_interface.(Storage_error (Errors.Unimplemented x))

      let copy context ~dbg ~sr ~vdi ~vm ~url ~dest =
        info "DATA.copy dbg:%s sr:%s vdi:%s url:%s dest:%s" dbg (s_of_sr sr)
          (s_of_vdi vdi) url (s_of_sr dest) ;
        Impl.DATA.copy context ~dbg ~sr ~vdi ~vm ~url ~dest

      let mirror _context ~dbg:_ ~sr:_ ~vdi:_ ~vm:_ ~dest:_ = u "DATA.mirror"

      let stat _context ~dbg:_ ~sr:_ ~vdi:_ ~vm:_ ~key:_ = u "DATA.stat"

      (* tapdisk supports three kind of nbd servers, the old style nbdserver,
         the new style nbd server and a real nbd server. The old and new style nbd servers
         are "special" nbd servers that accept fds passed via SCM_RIGHTS and handle
         connection based on that fd. The real nbd server is a "normal" nbd server
         that accepts nbd connections from nbd clients, and it does not support fd
         passing. *)
      let get_nbd_server_common context ~dbg ~dp ~sr ~vdi ~vm ~style =
        info "%s DATA.get_nbd_server dbg:%s dp:%s sr:%s vdi:%s vm:%s"
          __FUNCTION__ dbg dp (s_of_sr sr) (s_of_vdi vdi) (s_of_vm vm) ;
        let attach_info = DP.attach_info context ~dbg:"nbd" ~sr ~vdi ~dp ~vm in
        match Storage_smapiv1_migrate.tapdisk_of_attach_info attach_info with
        | Some tapdev ->
            let minor = Tapctl.get_minor tapdev in
            let pid = Tapctl.get_tapdisk_pid tapdev in
            let path =
              match style with
              | `newstyle ->
                  Printf.sprintf "/var/run/blktap-control/nbdserver-new%d.%d"
                    pid minor
              | `oldstyle ->
                  Printf.sprintf "/var/run/blktap-control/nbdserver%d.%d" pid
                    minor
              | `real ->
                  Printf.sprintf "/var/run/blktap-control/nbd%d.%d" pid minor
            in
            debug "%s nbd server path is %s" __FUNCTION__ path ;
            path
        | None ->
            raise
              (Storage_interface.Storage_error
                 (Backend_error
                    (Api_errors.internal_error, ["No tapdisk attach info found"])
                 )
              )

      let import_activate context ~dbg ~dp ~sr ~vdi ~vm =
        get_nbd_server_common context ~dbg ~dp ~sr ~vdi ~vm ~style:`oldstyle

      let get_nbd_server context ~dbg ~dp ~sr ~vdi ~vm =
        get_nbd_server_common context ~dbg ~dp ~sr ~vdi ~vm ~style:`real

      module MIRROR = struct
        type context = unit

        let send_start _ctx ~dbg:_ ~task_id:_ ~dp:_ ~sr:_ ~vdi:_ ~mirror_vm:_
            ~mirror_id:_ ~local_vdi:_ ~copy_vm:_ ~live_vm:_ ~url:_
            ~remote_mirror:_ ~dest_sr:_ ~verify_dest:_ =
          u "DATA.MIRROR.send_start"

        let receive_start context ~dbg ~sr ~vdi_info ~id ~similar =
          info "DATA.MIRROR.receive_start dbg:%s sr:%s id:%s similar:[%s]" dbg
            (s_of_sr sr) id
            (String.concat "," similar) ;
          Impl.DATA.MIRROR.receive_start context ~dbg ~sr ~vdi_info ~id ~similar

        let receive_start2 context ~dbg ~sr ~vdi_info ~id ~similar ~vm =
          info
            "DATA.MIRROR.receive_start2 dbg:%s sr:%s id:%s similar:[%s] vm:%s"
            dbg (s_of_sr sr) id
            (String.concat "," similar)
            (s_of_vm vm) ;
          Impl.DATA.MIRROR.receive_start2 context ~dbg ~sr ~vdi_info ~id
            ~similar ~vm

        let receive_start3 _context ~dbg:_ ~sr:_ ~vdi_info:_ ~mirror_id:_
            ~similar:_ ~vm:_ =
          (* See Storage_smapiv1_migrate.receive_start3 *)
          u __FUNCTION__

        let receive_finalize context ~dbg ~id =
          info "DATA.MIRROR.receive_finalize dbg:%s id:%s" dbg id ;
          Impl.DATA.MIRROR.receive_finalize context ~dbg ~id

        let receive_finalize2 context ~dbg ~id =
          info "DATA.MIRROR.receive_finalize2 dbg:%s id:%s" dbg id ;
          Impl.DATA.MIRROR.receive_finalize2 context ~dbg ~id

        let receive_finalize3 _context ~dbg:_ ~mirror_id:_ ~sr:_ ~url:_
            ~verify_dest:_ =
          (* see storage_smapiv{1,3}_migrate *)
          u __FUNCTION__

        let receive_cancel context ~dbg ~id =
          info "DATA.MIRROR.receive_cancel dbg:%s id:%s" dbg id ;
          Impl.DATA.MIRROR.receive_cancel context ~dbg ~id

        let receive_cancel2 _context ~dbg:_ ~mirror_id:_ ~url:_ ~verify_dest:_ =
          u __FUNCTION__

        let pre_deactivate_hook _context ~dbg:_ ~dp:_ ~sr:_ ~vdi:_ =
          u __FUNCTION__

        let has_mirror_failed _context ~dbg:_ ~mirror_id:_ ~sr:_ =
          u __FUNCTION__

        let list _context ~dbg:_ = u __FUNCTION__

        let stat _context ~dbg:_ ~id:_ = u __FUNCTION__
      end
    end

    module SR = struct
      include Storage_skeleton.SR

      let locks : (string, unit) Storage_locks.t = Storage_locks.make ()

      let with_sr sr f = Storage_locks.with_instance_lock locks (s_of_sr sr) f

      let probe context ~dbg ~queue ~device_config ~sm_config =
        with_dbg ~name:"SR.probe" ~dbg @@ fun di ->
        info "SR.probe dbg:%s" di.log ;
        let dbg = Debug_info.to_string di in
        Impl.SR.probe context ~dbg ~queue ~device_config ~sm_config

      let list _context ~dbg =
        with_dbg ~name:"SR.list" ~dbg @@ fun di ->
        info "SR.list dbg:%s" di.log ;
        List.map fst (Host.list !Host.host)

      let stat context ~dbg ~sr =
        with_dbg ~name:"SR.stat" ~dbg @@ fun di ->
        info "SR.stat dbg:%s sr:%s" di.log (s_of_sr sr) ;
        let dbg = Debug_info.to_string di in
        with_sr sr (fun () ->
            match Host.find sr !Host.host with
            | None ->
                raise (Storage_error (Sr_not_attached (s_of_sr sr)))
            | Some _ ->
                Impl.SR.stat context ~dbg ~sr
        )

      let scan context ~dbg ~sr =
        with_dbg ~name:"SR.scan" ~dbg @@ fun di ->
        info "SR.scan dbg:%s sr:%s" di.log (s_of_sr sr) ;
        let dbg = Debug_info.to_string di in
        with_sr sr (fun () ->
            match Host.find sr !Host.host with
            | None ->
                raise (Storage_error (Sr_not_attached (s_of_sr sr)))
            | Some _ ->
                Impl.SR.scan context ~dbg ~sr
        )

      let scan2 context ~dbg ~sr =
        with_dbg ~name:"SR.scan2" ~dbg @@ fun di ->
        info "SR.scan2 dbg:%s sr:%s" di.log (s_of_sr sr) ;
        let dbg = Debug_info.to_string di in
        with_sr sr (fun () ->
            match Host.find sr !Host.host with
            | None ->
                raise (Storage_error (Sr_not_attached (s_of_sr sr)))
            | Some _ ->
                let vs = Impl.SR.scan context ~dbg ~sr in
                let sr_info = Impl.SR.stat context ~dbg ~sr in
                (vs, sr_info)
        )

      let create context ~dbg ~sr ~name_label ~name_description ~device_config
          ~physical_size =
        with_dbg ~name:"SR.create" ~dbg @@ fun di ->
        info "SR.create dbg:%s sr:%s name_label:%s" di.log (s_of_sr sr)
          name_label ;
        let dbg = Debug_info.to_string di in
        with_sr sr (fun () ->
            match Host.find sr !Host.host with
            | None ->
                Impl.SR.create context ~dbg ~sr ~name_label ~name_description
                  ~device_config ~physical_size
            | Some _ ->
                error "SR %s is already attached" (s_of_sr sr) ;
                raise (Storage_error (Sr_attached (s_of_sr sr)))
        )

      let set_name_label context ~dbg ~sr ~new_name_label =
        with_dbg ~name:"SR.set_name_label" ~dbg @@ fun di ->
        info "SR.set_name_label dbg:%s sr:%s new_name_label:%s" di.log
          (s_of_sr sr) new_name_label ;
        let dbg = Debug_info.to_string di in
        Impl.SR.set_name_label context ~dbg ~sr ~new_name_label

      let set_name_description context ~dbg ~sr ~new_name_description =
        with_dbg ~name:"SR.set_name_description" ~dbg @@ fun di ->
        info "SR.set_name_description dbg:%s sr:%s new_name_description:%s"
          di.log (s_of_sr sr) new_name_description ;
        let dbg = Debug_info.to_string di in
        Impl.SR.set_name_description context ~dbg ~sr ~new_name_description

      let attach context ~dbg ~sr ~device_config =
        with_dbg ~name:"SR.attach" ~dbg @@ fun di ->
        let censor_key = ["password"] in
        let device_config_str =
          String.concat "; "
            (List.map
               (fun (k, v) ->
                 let v' =
                   if
                     List.exists
                       (fun censored ->
                         Astring.String.is_infix ~affix:censored k
                       )
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
        in
        info "SR.attach dbg:%s sr:%s device_config:[%s]" di.log (s_of_sr sr)
          device_config_str ;
        let dbg = Debug_info.to_string di in
        with_sr sr (fun () ->
            match Host.find sr !Host.host with
            | None ->
                Impl.SR.attach context ~dbg ~sr ~device_config ;
                Host.add_or_replace sr (Sr.empty ()) !Host.host ;
                (* FH1: Perform the side-effect first: in the case of a
                   						   failure half-way through we would rather perform the
                   						   side-effect twice than never at all. *)
                Everything.to_file !host_state_path (Everything.make ())
            | Some _ ->
                (* Operation is idempotent *)
                ()
        )

      let detach_destroy_common context ~dbg ~sr f =
        let active_dps sr_t =
          (* Enumerate all active datapaths *)
          List.concat_map (fun (_, vdi_t) -> Vdi.dp vdi_t) (Sr.list sr_t)
        in
        with_sr sr (fun () ->
            match Host.find sr !Host.host with
            | None ->
                raise (Storage_error (Sr_not_attached (s_of_sr sr)))
            | Some sr_t ->
                VDI.with_all_vdis sr (fun () ->
                    let dps = active_dps sr_t in
                    List.iter
                      (fun dp ->
                        let (_ : exn option) =
                          DP.destroy_sr context ~dbg ~dp ~sr ~sr_t
                            ~allow_leak:false true
                        in
                        ()
                      )
                      dps ;
                    let dps = active_dps sr_t in
                    if dps <> [] then
                      error "The following datapaths have leaked: %s"
                        (String.concat "; " dps) ;
                    f context ~dbg ~sr ;
                    Host.remove sr !Host.host ;
                    Everything.to_file !host_state_path (Everything.make ()) ;
                    VDI.locks_remove sr
                )
        )

      let detach context ~dbg ~sr =
        with_dbg ~name:"SR.detach" ~dbg @@ fun di ->
        info "SR.detach dbg:%s sr:%s" di.log (s_of_sr sr) ;
        let dbg = Debug_info.to_string di in
        detach_destroy_common context ~dbg ~sr Impl.SR.detach

      let reset _context ~dbg ~sr =
        with_dbg ~name:"SR.reset" ~dbg @@ fun di ->
        info "SR.reset dbg:%s sr:%s" di.log (s_of_sr sr) ;
        with_sr sr (fun () ->
            Host.remove sr !Host.host ;
            Everything.to_file !host_state_path (Everything.make ()) ;
            VDI.locks_remove sr
        )

      let destroy context ~dbg ~sr =
        with_dbg ~name:"SR.destroy" ~dbg @@ fun di ->
        info "SR.destroy dbg:%s sr:%s" di.log (s_of_sr sr) ;
        let dbg = Debug_info.to_string di in
        detach_destroy_common context ~dbg ~sr Impl.SR.destroy

      let update_snapshot_info_src context ~dbg ~sr ~vdi ~url ~dest ~dest_vdi
          ~snapshot_pairs =
        with_dbg ~name:"SR.update_snapshot_info_src" ~dbg @@ fun di ->
        info
          "SR.update_snapshot_info_src dbg:%s sr:%s vdi:%s url:%s dest:%s \
           dest_vdi:%s snapshot_pairs:%s"
          di.log (s_of_sr sr) (s_of_vdi vdi) url (s_of_sr dest)
          (s_of_vdi dest_vdi)
          (List.map
             (fun (local_snapshot, dest_snapshot) ->
               Printf.sprintf "local:%s, dest:%s" (s_of_vdi local_snapshot)
                 (s_of_vdi dest_snapshot)
             )
             snapshot_pairs
          |> String.concat "; "
          |> Printf.sprintf "[%s]"
          ) ;
        let dbg = Debug_info.to_string di in
        Impl.SR.update_snapshot_info_src context ~dbg ~sr ~vdi ~url ~dest
          ~dest_vdi ~snapshot_pairs

      let update_snapshot_info_dest context ~dbg ~sr ~vdi ~src_vdi
          ~snapshot_pairs =
        with_dbg ~name:"SR.update_snapshot_info_dest" ~dbg @@ fun di ->
        info
          "SR.update_snapshot_info_dest dbg:%s sr:%s vdi:%s ~src_vdi:%s \
           snapshot_pairs:%s"
          di.log (s_of_sr sr) (s_of_vdi vdi) (s_of_vdi src_vdi.vdi)
          (List.map
             (fun (local_snapshot, src_snapshot_info) ->
               Printf.sprintf "local:%s, src:%s" (s_of_vdi local_snapshot)
                 (s_of_vdi src_snapshot_info.vdi)
             )
             snapshot_pairs
          |> String.concat "; "
          |> Printf.sprintf "[%s]"
          ) ;
        let dbg = Debug_info.to_string di in
        Impl.SR.update_snapshot_info_dest context ~dbg ~sr ~vdi ~src_vdi
          ~snapshot_pairs
    end

    module Policy = struct
      let get_backend_vm context ~dbg ~vm ~sr ~vdi =
        Impl.Policy.get_backend_vm context ~dbg ~vm ~sr ~vdi
    end

    module TASK = struct
      open Storage_task

      let cancel _ ~dbg:_ ~task = handle_of_id tasks task |> Storage_task.cancel

      let stat' task = handle_of_id tasks task |> to_interface_task

      let stat _ ~dbg:_ ~task = stat' task

      let destroy' ~task =
        handle_of_id tasks task |> destroy ;
        Updates.remove (Dynamic.Task task) updates

      let destroy _ ~dbg:_ ~task = destroy' ~task

      let list _ ~dbg:_ = list tasks |> List.map to_interface_task
    end

    module UPDATES = struct
      let get _ ~dbg ~from ~timeout =
        let from = try Some (int_of_string from) with _ -> None in
        let _, ids, next = Updates.get dbg from timeout updates in
        (ids, string_of_int next)
    end
  end

let initialise () =
  Unixext.mkdir_rec (Filename.dirname !host_state_path) 0o700 ;
  if Sys.file_exists !host_state_path then (
    info "Loading storage state from: %s" !host_state_path ;
    try
      let state = Everything.of_file !host_state_path in
      Everything.set state
    with e ->
      error
        "Failed to load storage state from: %s; creating blank database \
         (error: %s)"
        !host_state_path (Printexc.to_string e)
  ) else
    info "No storage state is persisted in %s; creating blank database"
      !host_state_path

module Impl = Wrapper (Storage_smapiv1.SMAPIv1)
module Server = Storage_interface.Server (Impl) ()
