open Xapi_stdext_std.Listext
open Rrdd_shared
open Rrd
open Ds

module D = Debug.Make(struct let name = "rrdd_monitor" end)
open D

let create_rras use_min_max =
  (* Create archives of type min, max and average and last *)
  Array.of_list (List.flatten
                   (List.map (fun (n,ns) ->
                        if ns > 1 && use_min_max then [
                          Rrd.rra_create Rrd.CF_Average n ns 1.0;
                          Rrd.rra_create Rrd.CF_Min n ns 1.0;
                          Rrd.rra_create Rrd.CF_Max n ns 1.0;
                        ] else [Rrd.rra_create Rrd.CF_Average n ns 0.5]
                      ) timescales)
                )

let step = 5L

(** Create a rrd *)
let create_fresh_rrd use_min_max dss =
  let rras = create_rras use_min_max in
  let dss = Array.of_list (List.filter_map (fun ds ->
      if ds.ds_default then
        Some (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 ~max:ds.ds_max ~min:ds.ds_min Rrd.VT_Unknown)
      else None) dss)
  in
  Rrd.rrd_create dss rras step (Unix.gettimeofday())

let merge_new_dss rrd dss =
  let should_enable_ds ds = !Rrdd_shared.enable_all_dss || ds.ds_default in
  let enabled_dss = List.filter should_enable_ds dss in
  let current_dss = Rrd.ds_names rrd in
  let new_dss = List.filter (fun ds -> not (List.mem ds.ds_name current_dss)) enabled_dss in
  let now = Unix.gettimeofday () in
  List.fold_left (fun rrd ds ->
      rrd_add_ds rrd now (Rrd.ds_create ds.ds_name ds.Ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)
    ) rrd new_dss

(* Updates all of the hosts rrds. We are passed a list of uuids that
 * is used as the primary source for which VMs are resident on us.
 * When a new uuid turns up that we haven't got an RRD for in our
 * hashtbl, we create a new one. When a uuid for which we have an RRD
 * for doesn't appear to have any stats this update, we assume that the
 * domain has gone and we stream the RRD to the master. We also have a
 * list of the currently rebooting VMs to ensure we don't accidentally
 * archive the RRD. *)
let update_rrds timestamp dss (uuid_domids : (string * int) list) paused_vms =
  (* Here we do the synchronising between the dom0 view of the world
     		 and our Hashtbl. By the end of this execute block, the Hashtbl
     		 correctly represents the world *)
  let execute = Xapi_stdext_threads.Threadext.Mutex.execute in
  execute mutex (fun _ ->
      let out_of_date, by_how_much =
        match !host_rrd with
        | None -> false, 0.
        | Some rrdi -> rrdi.rrd.Rrd.last_updated > timestamp, abs_float (timestamp -. rrdi.rrd.Rrd.last_updated)
      in
      if out_of_date then
        error "Clock just went backwards by %.0f seconds: RRD data may now be unreliable" by_how_much;
      let do_vm (vm_uuid, domid) =
        try
          let dss = List.filter_map (fun (ty, ds) -> match ty with | VM x -> if x = vm_uuid then Some ds else None | _ -> None) dss in
          begin
            try
              (* First, potentially update the rrd with any new default dss *)
              let rrdi = Hashtbl.find vm_rrds vm_uuid in
              let rrd = merge_new_dss rrdi.rrd dss in
              Hashtbl.replace vm_rrds vm_uuid {rrd; dss; domid};
              (* CA-34383:
                 						 * Memory updates from paused domains serve no useful purpose.
                 						 * During a migrate such updates can also cause undesirable
                 						 * discontinuities in the observed value of memory_actual.
                 						 * Hence, we ignore changes from paused domains:
                 						 *)
              if not (List.mem vm_uuid paused_vms) then (
                Rrd.ds_update_named rrd timestamp ~new_domid:(domid <> rrdi.domid)
                  (List.map (fun ds -> (ds.ds_name, (ds.ds_value, ds.ds_pdp_transform_function))) dss);
                rrdi.dss <- dss;
                rrdi.domid <- domid;
              )
            with
            | Not_found ->
              debug "Creating fresh RRD for VM uuid=%s" vm_uuid;
              let rrd = create_fresh_rrd (!use_min_max) dss in
              Hashtbl.replace vm_rrds vm_uuid {rrd; dss; domid}
            | e -> raise e
          end
        with _ ->
          (*debug "Error: caught exception %s" (ExnHelper.string_of_exn e);*)
          log_backtrace ()
      in
      List.iter do_vm uuid_domids;

      let do_sr sr_uuid =
        try
          let dss = List.filter_map (fun (ty, ds) -> match ty with | SR x -> if x = sr_uuid then Some ds else None | _ -> None) dss in
          begin
            try
              (* First, potentially update the rrd with any new default dss *)
              let rrdi = Hashtbl.find sr_rrds sr_uuid in
              let rrd = merge_new_dss rrdi.rrd dss in
              Hashtbl.replace sr_rrds sr_uuid {rrd; dss; domid = 0};

              Rrd.ds_update_named rrd timestamp ~new_domid:false
                (List.map (fun ds -> (ds.ds_name, (ds.ds_value, ds.ds_pdp_transform_function))) dss);
              rrdi.dss <- dss;
              rrdi.domid <- 0;
            with
            | Not_found ->
              debug "Creating fresh RRD for SR uuid=%s" sr_uuid;
              let rrd = create_fresh_rrd (!use_min_max) dss in
              Hashtbl.replace sr_rrds sr_uuid {rrd; dss; domid = 0}
            | e -> raise e
          end
        with _ ->
          (*debug "Error: caught exception %s" (ExnHelper.string_of_exn e);*)
          log_backtrace ()
      in
      let uuid_srs =
        List.filter_map (fun (ty, _ds) -> match ty with SR x -> Some x | _ -> None) dss
        |> List.setify in
      List.iter do_sr uuid_srs;

      let host_dss = List.filter_map (fun (ty, ds) -> match ty with | Host -> Some ds | _ -> None) dss in
      begin
        match !host_rrd with
        | None ->
          begin
            debug "Creating fresh RRD for localhost";
            let rrd = create_fresh_rrd true host_dss in (* Always always create localhost rrds with min/max enabled *)
            host_rrd := Some {rrd; dss = host_dss; domid = 0}
          end
        | Some rrdi ->
          rrdi.dss <- host_dss;
          let rrd = merge_new_dss rrdi.rrd host_dss in
          host_rrd := Some {rrd; dss = host_dss; domid = 0};
          Rrd.ds_update_named rrd timestamp ~new_domid:false
            (List.map (fun ds -> (ds.ds_name, (ds.ds_value,ds.ds_pdp_transform_function))) host_dss)
      end;
    )
