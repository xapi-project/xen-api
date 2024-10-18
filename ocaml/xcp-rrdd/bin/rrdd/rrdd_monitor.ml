open Rrdd_shared
open Rrd
open Ds

module D = Debug.Make (struct let name = "rrdd_monitor" end)

open D

let create_rras use_min_max =
  (* Create archives of type min, max and average and last *)
  Array.of_list
    (List.concat_map
       (fun (n, ns) ->
         if ns > 1 && use_min_max then
           [
             Rrd.rra_create Rrd.CF_Average n ns 1.0
           ; Rrd.rra_create Rrd.CF_Min n ns 1.0
           ; Rrd.rra_create Rrd.CF_Max n ns 1.0
           ]
         else
           [Rrd.rra_create Rrd.CF_Average n ns 0.5]
       )
       timescales
    )

let step = 5L

(** Create a rrd *)
let create_fresh_rrd use_min_max dss timestamp =
  let rras = create_rras use_min_max in
  let dss =
    Array.of_list
      (List.filter_map
         (fun ds ->
           if ds.ds_default then
             Some
               (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 ~max:ds.ds_max
                  ~min:ds.ds_min Rrd.VT_Unknown
               )
           else
             None
         )
         dss
      )
  in
  Rrd.rrd_create dss rras step timestamp

(* Check if new (enabled) datasources appeared, and add them to the RRD *)
let merge_new_dss rrdi dss =
  let should_enable_ds _ (_, ds) =
    !Rrdd_shared.enable_all_dss || ds.ds_default
  in
  let default_dss = StringMap.filter should_enable_ds dss in
  (* NOTE: It's enough to check if all the default datasources have been added
     to the RRD_INFO, because if a non-default one has been enabled at runtime,
     it's added to the RRD immediately and we don't need to bother *)
  let new_dss =
    StringMap.filter
      (fun ds_name _ -> not (StringMap.mem ds_name rrdi.dss))
      default_dss
  in
  (* fold on Map is not tail-recursive, but the depth of the stack should be
     log of the number of entries at worst, so this should be alright.
     Previous conversions to List are also not tail-recursive with identical
     stack depth *)
  let merge_keys _key a _b = Some a in
  let updated_dss = StringMap.union merge_keys dss rrdi.dss in
  ( updated_dss
  , StringMap.fold
      (fun _key (timestamp, ds) rrd ->
        rrd_add_ds rrd timestamp
          (Rrd.ds_create ds.ds_name ds.Ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)
      )
      new_dss rrdi.rrd
  )

module OwnerMap = Map.Make (struct
  type t = ds_owner

  let compare a b =
    match (a, b) with
    | Host, Host ->
        0
    | Host, _ | VM _, SR _ ->
        -1
    | _, Host | SR _, VM _ ->
        1
    | VM a, VM b | SR a, SR b ->
        String.compare a b
end)

(** Updates all of the hosts rrds. We are passed a list of uuids that is used as
    the primary source for which VMs are resident on us. When a new uuid turns
    up that we haven't got an RRD for in our hashtbl, we create a new one. When
    a uuid for which we have an RRD for doesn't appear to have any stats this
    update, we assume that the domain has gone and we stream the RRD to the
    master. We also have a list of the currently rebooting VMs to ensure we
    don't accidentally archive the RRD. *)
let update_rrds uuid_domids paused_vms (timestamp, dss) =
  let uuid_domids = List.to_seq uuid_domids |> StringMap.of_seq in
  let paused_vms = List.to_seq paused_vms |> StringSet.of_seq in
  let consolidate all (owner, ds) =
    let add_ds_to = StringMap.add ds.ds_name (timestamp, ds) in
    let merge = function
      | None ->
          Some (add_ds_to StringMap.empty)
      | Some dss ->
          Some (add_ds_to dss)
    in
    OwnerMap.update owner merge all
  in
  let dss = Seq.fold_left consolidate OwnerMap.empty dss in
  let to_named_updates (_, ds) = (ds.ds_value, ds.ds_pdp_transform_function) in
  let map_keys_to_list dss =
    StringMap.bindings dss |> List.map snd |> List.map snd
  in

  (* Here we do the synchronising between the dom0 view of the world and our
     Hashtbl. By the end of this execute block, the Hashtbl correctly represents
     the world *)
  Xapi_stdext_threads.Threadext.Mutex.execute mutex (fun _ ->
      let out_of_date, by_how_much =
        let reading_timestamp = Unix.gettimeofday () in
        match !host_rrd with
        | None ->
            (false, 0.)
        | Some rrdi ->
            ( rrdi.rrd.Rrd.last_updated > reading_timestamp
            , abs_float (reading_timestamp -. rrdi.rrd.Rrd.last_updated)
            )
      in
      if out_of_date then
        error
          "Clock just went backwards by %.0f seconds: RRD data may now be \
           unreliable"
          by_how_much ;
      let process_vm vm_uuid (dss : (float * Ds.ds) Rrd.StringMap.t) =
        match StringMap.find_opt vm_uuid uuid_domids with
        | Some domid -> (
          (* First, potentially update the rrd with any new default dss *)
          match Hashtbl.find_opt vm_rrds vm_uuid with
          | Some rrdi ->
              let updated_dss, rrd = merge_new_dss rrdi dss in
              Hashtbl.replace vm_rrds vm_uuid {rrd; dss= updated_dss; domid} ;
              (* CA-34383: Memory updates from paused domains serve no useful
                 purpose. During a migrate such updates can also cause undesirable
                 discontinuities in the observed value of memory_actual. Hence, we
                 ignore changes from paused domains: *)
              let named_updates = StringMap.map to_named_updates dss in
              if not (StringSet.mem vm_uuid paused_vms) then
                Rrd.ds_update_named rrd ~new_rrd:(domid <> rrdi.domid) timestamp
                  named_updates
          | None ->
              debug "%s: Creating fresh RRD for VM uuid=%s" __FUNCTION__ vm_uuid ;
              let dss_list = map_keys_to_list dss in
              let rrd = create_fresh_rrd !use_min_max dss_list timestamp in
              Hashtbl.replace vm_rrds vm_uuid {rrd; dss; domid}
        )
        | None ->
            info "%s: VM uuid=%s is not resident in this host, ignoring rrds"
              __FUNCTION__ vm_uuid
      in
      let process_sr sr_uuid dss =
        try
          (* First, potentially update the rrd with any new default dss *)
          match Hashtbl.find_opt sr_rrds sr_uuid with
          | Some rrdi ->
              let updated_dss, rrd = merge_new_dss rrdi dss in
              Hashtbl.replace sr_rrds sr_uuid {rrd; dss= updated_dss; domid= 0} ;
              let named_updates = StringMap.map to_named_updates dss in
              Rrd.ds_update_named rrd ~new_rrd:false timestamp named_updates
          | None ->
              debug "%s: Creating fresh RRD for SR uuid=%s" __FUNCTION__ sr_uuid ;
              let dss_list = map_keys_to_list dss in
              let rrd = create_fresh_rrd !use_min_max dss_list timestamp in
              Hashtbl.replace sr_rrds sr_uuid {rrd; dss; domid= 0}
        with _ -> log_backtrace ()
      in
      let process_host dss =
        match !host_rrd with
        | None ->
            debug "%s: Creating fresh RRD for localhost" __FUNCTION__ ;
            let dss_list = map_keys_to_list dss in
            let rrd = create_fresh_rrd true dss_list timestamp in
            (* Always always create localhost rrds with min/max enabled *)
            host_rrd := Some {rrd; dss; domid= 0}
        | Some rrdi ->
            let updated_dss, rrd = merge_new_dss rrdi dss in
            host_rrd := Some {rrd; dss= updated_dss; domid= 0} ;
            let named_updates = StringMap.map to_named_updates dss in
            Rrd.ds_update_named rrd ~new_rrd:false timestamp named_updates
      in

      let process_dss ds_owner dss =
        match ds_owner with
        | Host ->
            process_host dss
        | VM uuid ->
            process_vm uuid dss
        | SR uuid ->
            process_sr uuid dss
      in
      OwnerMap.iter process_dss dss
  )
