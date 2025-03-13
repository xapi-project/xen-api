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
  (* NOTE: Only add enabled dss to the live rrd, ignoring non-default ones.
     This is because non-default ones are added to the RRD when they are
     enabled. *)
  let new_enabled_dss =
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
        (* SAFETY: verified that these datasources aren't enabled above
           already, in a more efficient way than RRD does it *)
        rrd_add_ds_unsafe rrd timestamp
          (Rrd.ds_create ds.ds_name ds.Ds.ds_type ~mrhb:300.0 ~min:ds.ds_min
             ~max:ds.ds_max Rrd.VT_Unknown
          )
      )
      new_enabled_dss rrdi.rrd
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

(** Converts all the updates collected from various sources in the form of
    (uid * timestamp * (ds_owner * ds) Seq.t) Seq.t
    into two OwnerMaps, one mapping an owner to a (flattened) Set of its
    datasources (used to determine missing datasources), and another mapping
    the owner to a Map of datasources grouped by plugin (used during updates)
    *)
let convert_to_owner_map dss =
  let consolidate (per_owner_map, per_plugin_map) (source_uid, timestamp, dss) =
    let add_to_plugin (per_owner_map, per_plugin_map) (owner, ds) =
      let add_dsts_to = StringMap.add ds.ds_name (timestamp, ds) in
      let add_ds_to = StringSet.add ds.ds_name in
      let merge = function
        | None ->
            Some (add_ds_to StringSet.empty)
        | Some dss ->
            Some (add_ds_to dss)
      in
      let per_owner_map = OwnerMap.update owner merge per_owner_map in
      let add_plugin_ds_to =
        StringMap.update source_uid (function
          | None ->
              Some (timestamp, add_dsts_to StringMap.empty)
          | Some (timestamp, dss) ->
              Some (timestamp, add_dsts_to dss)
          )
      in
      let plugin_merge = function
        | None ->
            Some (add_plugin_ds_to StringMap.empty)
        | Some plugins_dss ->
            Some (add_plugin_ds_to plugins_dss)
      in
      let per_plugin_map :
          (float * (float * ds) StringMap.t) StringMap.t OwnerMap.t =
        OwnerMap.update owner plugin_merge per_plugin_map
      in
      (per_owner_map, per_plugin_map)
    in
    Seq.fold_left add_to_plugin (per_owner_map, per_plugin_map) dss
  in
  let per_owner_map, per_plugin_map =
    Seq.fold_left consolidate (OwnerMap.empty, OwnerMap.empty) dss
  in
  (per_owner_map, per_plugin_map)

(** Updates all of the hosts rrds. We are passed a list of uuids that is used as
    the primary source for which VMs are resident on us. When a new uuid turns
    up that we haven't got an RRD for in our hashtbl, we create a new one. When
    a uuid for which we have an RRD for doesn't appear to have any stats this
    update, we assume that the domain has gone and we stream the RRD to the
    master. We also have a list of the currently rebooting VMs to ensure we
    don't accidentally archive the RRD.
    Also resets the value of datasources that are enabled in the RRD, but
    weren't updated on this refresh cycle.
    *)
let update_rrds uuid_domids paused_vms plugins_dss =
  let uuid_domids = List.to_seq uuid_domids |> StringMap.of_seq in
  let paused_vms = List.to_seq paused_vms |> StringSet.of_seq in
  let per_owner_flattened_map, per_plugin_map =
    convert_to_owner_map plugins_dss
  in
  let to_named_updates (_, ds) =
    {value= ds.ds_value; transform= ds.ds_pdp_transform_function}
  in
  let map_keys_to_list dss =
    StringMap.bindings dss |> List.map snd |> List.map snd
  in

  (* Determine datasources missing from this batch for this RRD, reset
      them to default Unknown values *)
  let handle_missing_stats rrd dss =
    let named_update = {value= VT_Unknown; transform= Identity} in
    (* Check which of the enabled data sources are missing from the update batch *)
    let missing_dss =
      Array.fold_left
        (fun missing (ds : Rrd.ds) ->
          if StringSet.mem ds.ds_name dss then
            missing
          else
            StringMap.add ds.ds_name named_update missing
        )
        StringMap.empty rrd.rrd_dss
    in
    missing_dss
  in
  let reset_missing_data =
    (* NOTE: This processes already added and enabled datasources that have
       not been provided a value on this refresh cycle, so no data sources need
       to be added to RRDs *)
    (* NOTE: new_rrd is always false, since it's only 'true' currently if a VM's
       domid does not correspond to rrdi.domid, which would already have been
       fixed by replacing rrdi.domid with the current domid when updating with
       provided datasources before this function is called *)
    let missing_data_timestamp = Unix.gettimeofday () in
    fun rrd dss ->
      if not (StringMap.is_empty dss) then
        Rrd.ds_update_named rrd ~new_rrd:false missing_data_timestamp dss
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
      let process_vm vm_uuid
          (plugins_dss : (float * (float * Ds.ds) Rrd.StringMap.t) StringMap.t)
          available_dss =
        match StringMap.find_opt vm_uuid uuid_domids with
        | Some domid ->
            (* Deal with datasources per plugin *)
            let vm_rrdi = Hashtbl.find_opt vm_rrds vm_uuid in
            let vm_rrdi =
              (* SAFETY: Entries in String/OwnerMap are only present if
                 they contain a list of datasources, and thus the rrd is
                 definitely Some after .fold above.
                 This applies to all such constructs in process_* functions *)
              Option.get
                (StringMap.fold
                   (fun _uid (timestamp, dss) vm_rrd ->
                     (* First, potentially update the rrd with any new default dss *)
                     match vm_rrd with
                     | Some rrdi ->
                         let updated_dss, rrd = merge_new_dss rrdi dss in
                         (* CA-34383: Memory updates from paused domains serve no useful
                            purpose. During a migrate such updates can also cause undesirable
                            discontinuities in the observed value of memory_actual. Hence, we
                            ignore changes from paused domains: *)
                         ( if not (StringSet.mem vm_uuid paused_vms) then
                             let named_updates =
                               StringMap.map to_named_updates dss
                             in
                             Rrd.ds_update_named rrd
                               ~new_rrd:(domid <> rrdi.domid) timestamp
                               named_updates
                         ) ;
                         Some {rrd; dss= updated_dss; domid}
                     | None ->
                         debug "%s: Creating fresh RRD for VM uuid=%s"
                           __FUNCTION__ vm_uuid ;
                         let dss_list = map_keys_to_list dss in
                         let rrd =
                           create_fresh_rrd !use_min_max dss_list timestamp
                         in
                         Some {rrd; dss; domid}
                   )
                   plugins_dss vm_rrdi
                )
            in
            let missing_updates =
              handle_missing_stats vm_rrdi.rrd available_dss
            in
            reset_missing_data vm_rrdi.rrd missing_updates ;

            Hashtbl.replace vm_rrds vm_uuid vm_rrdi
        | None ->
            info "%s: VM uuid=%s is not resident in this host, ignoring rrds"
              __FUNCTION__ vm_uuid
      in
      let process_sr sr_uuid plugins_dss available_dss =
        try
          let sr_rrdi = Hashtbl.find_opt sr_rrds sr_uuid in
          (* Deal with datasources per plugin *)
          let sr_rrdi =
            Option.get
              (StringMap.fold
                 (fun _uid (timestamp, dss) sr_rrdi ->
                   (* First, potentially update the rrd with any new default dss *)
                   match sr_rrdi with
                   | Some rrdi ->
                       let updated_dss, rrd = merge_new_dss rrdi dss in
                       let named_updates = StringMap.map to_named_updates dss in
                       Rrd.ds_update_named rrd ~new_rrd:false timestamp
                         named_updates ;
                       Some {rrd; dss= updated_dss; domid= 0}
                   | None ->
                       debug "%s: Creating fresh RRD for SR uuid=%s"
                         __FUNCTION__ sr_uuid ;
                       let dss_list = map_keys_to_list dss in
                       let rrd =
                         create_fresh_rrd !use_min_max dss_list timestamp
                       in
                       Some {rrd; dss; domid= 0}
                 )
                 plugins_dss sr_rrdi
              )
          in
          let missing_updates =
            handle_missing_stats sr_rrdi.rrd available_dss
          in
          reset_missing_data sr_rrdi.rrd missing_updates ;

          Hashtbl.replace sr_rrds sr_uuid sr_rrdi
        with _ -> log_backtrace ()
      in
      let process_host plugins_dss available_dss =
        let host_rrdi = !host_rrd in
        (* Deal with datasources per plugin *)
        let host_rrdi =
          Option.get
            (StringMap.fold
               (fun _uid (timestamp, dss) host_rrdi ->
                 match host_rrdi with
                 | None ->
                     debug "%s: Creating fresh RRD for localhost" __FUNCTION__ ;
                     let dss_list = map_keys_to_list dss in
                     let rrd = create_fresh_rrd true dss_list timestamp in
                     (* Always always create localhost rrds with min/max enabled *)
                     Some {rrd; dss; domid= 0}
                 | Some rrdi ->
                     let updated_dss, rrd = merge_new_dss rrdi dss in
                     let named_updates = StringMap.map to_named_updates dss in
                     Rrd.ds_update_named rrd ~new_rrd:false timestamp
                       named_updates ;
                     Some {rrd; dss= updated_dss; domid= 0}
               )
               plugins_dss host_rrdi
            )
        in
        let missing_updates =
          handle_missing_stats host_rrdi.rrd available_dss
        in
        reset_missing_data host_rrdi.rrd missing_updates ;

        host_rrd := Some host_rrdi
      in

      let process_dss ds_owner dss =
        (* Flattened list of all datasources for this RRD owner, used to
           determine which datasources have gone missing. Not to be used in
           actual update process, since these mix up datasources with different
           timestamps *)
        let available_dss = OwnerMap.find ds_owner per_owner_flattened_map in
        match ds_owner with
        | Host ->
            process_host dss available_dss
        | VM uuid ->
            process_vm uuid dss available_dss
        | SR uuid ->
            process_sr uuid dss available_dss
      in
      OwnerMap.iter process_dss per_plugin_map
  )
