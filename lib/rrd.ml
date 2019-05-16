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
(*
 * RRD module
 * This module provides a util that records data in a way that's compatable
 * with rrdtool (http://oss.oetiker.ch/rrdtool/index.en.html)
 *)
(**
 * @group Performance Monitoring
*)
module Fring = Rrd_fring
module Utils = Rrd_utils

exception No_RRA_Available
exception Invalid_data_source of string

type ds_owner = VM of string | Host | SR of string

(** Data source types - see ds datatype *)
type ds_type = Absolute | Gauge | Derive [@@deriving rpc]

(** Consolidation function - see RRA datatype *)
type cf_type = CF_Average | CF_Min | CF_Max | CF_Last

(** Container so that we can handle different typed inputs *)
type ds_value_type = VT_Float of float | VT_Int64 of int64 | VT_Unknown [@@deriving rpc]

type sampling_frequency = Five_Seconds [@@deriving rpc]

(* utility *)

let cf_type_of_string s =
  match s with
  | "AVERAGE" -> CF_Average
  | "MIN" -> CF_Min
  | "MAX" -> CF_Max
  | "LAST" -> CF_Last
  | x -> failwith (Printf.sprintf "Unknown cf_type: %s" x)

let cf_type_to_string cf =
  match cf with
  | CF_Average -> "AVERAGE"
  | CF_Max -> "MAX"
  | CF_Min -> "MIN"
  | CF_Last -> "LAST"

let cf_init_value cf =
  match cf with
  | CF_Average -> 0.0
  | CF_Min -> infinity
  | CF_Max -> neg_infinity
  | CF_Last -> nan

(** The CDP preparation scratch area.
    The 'value' field should be accumulated in such a way that it always
    contains the value that will eventually be the CDP. This means that
    for averages, we accumulate 1/n * the PDP, and renormalise when we
    have unknown PDPs. For the other types it's much easier *)

type cdp_prep = {
  mutable cdp_value: float;
  mutable cdp_unknown_pdps: int;         (** How may PDPs have been unknown so far *)
}

(** DS - a data source
    This defines how we deal with incoming data. Type is one of:

    - Absolute: meaning that the incoming data is an absolute rate
    - Derive:   meaning that the rate must come from the difference between the
                incoming data and the previous value
    - Gauge:    meaning that the value isn't a rate at all (e.g. temperature, load avg)

    Optionally, there is a maximum time greater than which we mark the PDPs
    as unknown. *)

type ds = {
  ds_name : string;                  (** Name *)
  ds_ty : ds_type;                   (** Type (as above) *)
  ds_min : float;
  ds_max : float;
  ds_mrhb : float;                   (** Maximum time between updates *)
  mutable ds_last : ds_value_type;   (** Last value *)
  mutable ds_value: float;           (** The accumulator for the PDP value *)
  mutable ds_unknown_sec: float;     (** Number of seconds that are unknown in the current PDP *)
} [@@deriving rpc]

(** RRA - RRD archive
    This is an archive that holds consolidated data points (CDPs). It
    defines the type of consolidation that happens (average, max, min or last),
    the number of primary data points (PDPs) that go to make a CDP, and
    the number of CDPs to store. *)

type rra = {
  rra_cf : cf_type;          (** consolidation function *)
  rra_row_cnt : int;         (** number of entries to store *)
  rra_pdp_cnt : int;         (** number of pdps per cdp *)
  rra_xff : float;           (** proportion of missing pdps at which we mark the cdp as unknown *)
  rra_data: Fring.t array;   (** stored data *)
  rra_cdps : cdp_prep array; (** scratch area for consolidated datapoint preparation *)

  mutable rra_updatehook : (rrd -> int -> unit) option; (** Hook that gets called when an update happens *)
}

(** The container for the DSs. Also specifies the period between pdps *)

and rrd = {
  mutable last_updated: float;  (** Last updated time in seconds *)
  timestep: int64;              (** Period between PDPs *)
  rrd_dss: ds array;
  rrd_rras: rra array;
}

let copy_cdp_prep x =
  {
    cdp_value = x.cdp_value;
    cdp_unknown_pdps = x.cdp_unknown_pdps;
  }

let copy_rra x =
  {
    rra_cf = x.rra_cf;
    rra_row_cnt = x.rra_row_cnt;
    rra_pdp_cnt = x.rra_pdp_cnt;
    rra_xff = x.rra_xff;
    rra_data = Array.map Fring.copy x.rra_data;
    rra_cdps = Array.map copy_cdp_prep x.rra_cdps;
    rra_updatehook = x.rra_updatehook
  }

let copy_ds x =
  {
    ds_name = x.ds_name; (* not mutable *)
    ds_ty = x.ds_ty;
    ds_min = x.ds_min;
    ds_max = x.ds_max;
    ds_mrhb = x.ds_mrhb;
    ds_last = x.ds_last;
    ds_value = x.ds_value;
    ds_unknown_sec = x.ds_unknown_sec;
  }

let copy_rrd x =
  {
    last_updated = x.last_updated;
    timestep = x.timestep;
    rrd_dss = Array.map copy_ds x.rrd_dss;
    rrd_rras = Array.map copy_rra x.rrd_rras;
  }

(** Helper function to get the start time and age of the current/last PDP *)
let get_times time timestep =
  let starttime = Int64.mul timestep (Int64.div (Int64.of_float time) timestep) in
  let age = time -. (Int64.to_float starttime) in
  (starttime, age)

(** Update the CDP value with a number (start_pdp_offset) of PDPs. *)
let do_cfs rra start_pdp_offset pdps =
  for i=0 to Array.length pdps - 1 do
    let cdp = rra.rra_cdps.(i) in
    if Utils.isnan pdps.(i)
    then begin
      (* CDP is an accumulator for the average. If we've got some unknowns, we need to
         			   renormalize. ie, CDP contains \sum_{i=0}^j{ (1/n) x_i} where n is the number of
         			   values we expect to have. If we have unknowns, we need to multiply the whole
         			   thing by \frac{n_{old}}{n_{new}} *)
      let olddiv = rra.rra_pdp_cnt - cdp.cdp_unknown_pdps in
      let newdiv = olddiv - start_pdp_offset in
      if newdiv > 0 then (
        cdp.cdp_value <- cdp.cdp_value *. (float_of_int olddiv) /. (float_of_int newdiv);
        cdp.cdp_unknown_pdps <- cdp.cdp_unknown_pdps + start_pdp_offset)
    end else
      let cdpv=cdp.cdp_value in
      cdp.cdp_value <- match rra.rra_cf with
        | CF_Average -> cdpv +. pdps.(i) *. (float_of_int start_pdp_offset) /. (float_of_int rra.rra_pdp_cnt )
        | CF_Min -> min cdpv pdps.(i)
        | CF_Max -> max cdpv pdps.(i)
        | CF_Last -> pdps.(i)
  done

(** Update the RRAs with a number of PDPs. *)
let rra_update rrd proc_pdp_st elapsed_pdp_st pdps =
  (*  debug "rra_update";*)
  let updatefn rra =
    let start_pdp_offset = rra.rra_pdp_cnt - (Int64.to_int (Int64.rem (Int64.div proc_pdp_st rrd.timestep) (Int64.of_int rra.rra_pdp_cnt))) in
    let rra_step_cnt = if elapsed_pdp_st < start_pdp_offset then 0 else (elapsed_pdp_st - start_pdp_offset) / rra.rra_pdp_cnt + 1 in
    do_cfs rra (min start_pdp_offset elapsed_pdp_st) pdps;
    if rra_step_cnt > 0 then
      begin
        (* When writing multiple CDP values into the archive, the
           				   first one (primary) is calculated using the values we
           				   already had accumulated from the last update, whereas any
           				   subsequent values (secondary) are calculated just using the
           				   current PDP. It turns out that the secondary values are
           				   simply the PDPs as whichever CF is used, a CDP of many
           				   repeated values is simply the value itself. *)
        let primaries = Array.map (fun cdp ->
            if cdp.cdp_unknown_pdps <= (int_of_float (rra.rra_xff *. float_of_int rra.rra_pdp_cnt))
            then cdp.cdp_value
            else nan) rra.rra_cdps
        in
        let secondaries = pdps in

        (* Push the primary and secondary values *)
        Array.iteri (fun i x -> Fring.push rra.rra_data.(i) x) primaries;
        for _=1 to min (rra_step_cnt-1) (rra.rra_row_cnt) do
          Array.iteri (fun i x -> Fring.push rra.rra_data.(i) x) secondaries
        done;

        (* Reinitialise the CDP preparation area *)
        let new_start_pdp_offset = (elapsed_pdp_st - start_pdp_offset) mod rra.rra_pdp_cnt in
        let cdp_init = cf_init_value rra.rra_cf in
        Array.iter (fun cdp -> cdp.cdp_unknown_pdps <- 0; cdp.cdp_value <- cdp_init) rra.rra_cdps;
        do_cfs rra new_start_pdp_offset pdps;
        match rra.rra_updatehook with None -> () | Some f -> f rrd rra_step_cnt
      end
  in
  Array.iter updatefn rrd.rrd_rras

(* We assume that the data being given is of the form of a rate; that is,
   it's dependent on the time interval between updates. To be able to
   deal with guage DSs, we multiply by the interval so that it cancels
   the subsequent divide by interval later on *)
let process_ds_value ds value interval new_domid =
  if interval > ds.ds_mrhb
  then nan
  else
    begin
      let rate =
        match ds.ds_ty with
        | Absolute ->
          begin
            match value with
            | VT_Int64 y -> Int64.to_float y
            | VT_Float y -> y
            | VT_Unknown -> nan
          end
        | Gauge ->
          begin
            match value with
            | VT_Int64 y -> (Int64.to_float y) *. interval
            | VT_Float y -> y *. interval
            | VT_Unknown -> nan
          end
        | Derive ->
          begin
            if new_domid then
              match value with
              | VT_Int64 y -> Int64.to_float y
              | VT_Float y -> y
              | VT_Unknown -> nan
            else
              match ds.ds_last, value with
              | VT_Int64 x, VT_Int64 y ->
                let result = (Int64.sub y x) in
                let result = if result < 0L then Int64.add result 0x100000000L else result in (* for wrapping 32 bit counters *)
                Int64.to_float result
              | VT_Float x, VT_Float y -> y -. x
              | VT_Unknown, _ -> nan
              | _, VT_Unknown -> nan
              | _ -> failwith ("Bad type updating ds: "^ds.ds_name)
          end
      in
      ds.ds_last <- value;
      rate
    end

let ds_update rrd timestamp values transforms new_domid =
  (* Interval is the time between this and the last update *)
  let interval = timestamp -. rrd.last_updated in
  (* Work around the clock going backwards *)
  let interval = if interval < 0. then 5. else interval in

  (* start time (st) and age of the last processed pdp and the currently occupied one *)
  let proc_pdp_st, _proc_pdp_age = get_times rrd.last_updated rrd.timestep in
  let occu_pdp_st, occu_pdp_age = get_times timestamp rrd.timestep in

  (* The number of pdps that should result from this update *)
  let elapsed_pdp_st = Int64.to_int (Int64.div (Int64.sub occu_pdp_st proc_pdp_st) rrd.timestep) in

  (* if we're due one or more PDPs, pre_int is the amount of the
     	   current update interval that will be used in calculating them, and
     	   post_int is the amount left over
      this step. If a PDP isn't post is what's left over *)
  let pre_int, post_int =
    if elapsed_pdp_st > 0 then
      let pre = interval -. occu_pdp_age in
      (pre, occu_pdp_age)
    else
      (interval, 0.0)
  in

  (* We're now done with the last_updated value, so update it *)
  rrd.last_updated <- timestamp;

  (* Calculate the values we're going to store based on the input data and the type of the DS *)
  let v2s = Array.mapi (fun i value -> process_ds_value rrd.rrd_dss.(i) value interval new_domid) values in
  (*  debug "Got values: %s\n" (String.concat "," (Array.to_list (Array.mapi (fun i p -> Printf.sprintf "(%s: %f)" rrd.rrd_dss.(i).ds_name p) v2s)));*)
  (* Update the PDP accumulators up until the most recent PDP *)
  Array.iteri
    (fun i value ->
       let ds=rrd.rrd_dss.(i) in
       if Utils.isnan value
       then ds.ds_unknown_sec <- pre_int
       else ds.ds_value <- ds.ds_value +. (pre_int *. value /. interval))
    v2s;

  (* If we've passed a PDP point, we need to update the RRAs *)
  if elapsed_pdp_st > 0 then
    begin
      (* Calculate the PDPs for each DS *)
      let pdps = Array.mapi (fun i ds ->
          if interval > ds.ds_mrhb
          then nan
          else
            let raw = ds.ds_value /. (Int64.to_float (Int64.sub occu_pdp_st proc_pdp_st) -. ds.ds_unknown_sec) in
            let raw =
              if raw < ds.ds_min
              then ds.ds_min
              else if raw > ds.ds_max
              then ds.ds_max
              else raw
            in
            (* Here is where we apply the transform *)
            transforms.(i) raw
        ) rrd.rrd_dss in

      rra_update rrd proc_pdp_st elapsed_pdp_st pdps;

      (* Reset the PDP accumulators *)
      Array.iteri
        (fun i value ->
           let ds = rrd.rrd_dss.(i) in
           if Utils.isnan value
           then (ds.ds_value <- 0.0; ds.ds_unknown_sec <- post_int)
           else (ds.ds_value <- post_int *. value /. interval; ds.ds_unknown_sec <- 0.0))
        v2s;
    end

(** Update the rrd with named values rather than just an ordered array *)
let ds_update_named rrd timestamp ~new_domid valuesandtransforms =
  let identity x = x in
  let ds_names = Array.map (fun ds -> ds.ds_name) rrd.rrd_dss in
  let ds_values = Array.map (fun name -> try fst (List.assoc name valuesandtransforms) with _ -> VT_Unknown) ds_names in
  let ds_transforms = Array.map (fun name -> try snd (List.assoc name valuesandtransforms) with _ -> identity) ds_names in
  ds_update rrd timestamp ds_values ds_transforms new_domid

(** Get registered DS names *)
let ds_names rrd =
  Array.to_list (Array.map (fun ds -> ds.ds_name) rrd.rrd_dss)

(** Return the last updated values along with the names of the dss, from the ith rra *)
let get_last_values rrd i =
  let ds_names = Array.map (fun ds -> ds.ds_name) rrd.rrd_dss in
  let rra_values = Array.map (fun ring -> Fring.top ring) rrd.rrd_rras.(i).rra_data in
  let values = Array.mapi (fun i ds -> (ds,rra_values.(i))) ds_names in
  let data = ("t",rrd.last_updated)::(Array.to_list values) in
  data

(** Return the last raw DS values as a (string * float) list *)
let get_last_ds_values rrd =
  Array.fold_left (fun acc ds -> (ds.ds_name, ds.ds_last)::acc) [] rrd.rrd_dss

(** create an rra structure *)
let rra_create cf row_cnt pdp_cnt xff =
  {
    rra_cf=cf;
    rra_row_cnt=row_cnt;
    rra_pdp_cnt=pdp_cnt;
    rra_xff=xff;
    rra_data=[| |]; (* defer creation of the data until we know how many dss we're storing *)
    rra_cdps=[| |]; (* defer creation of the data until we know how many dss we're storing *)
    rra_updatehook = None; (* DEPRECATED *)
  }

let ds_create name ty ?(min=neg_infinity) ?(max=infinity) ?(mrhb=infinity) init =
  {
    ds_name=name;
    ds_ty=ty;
    ds_min=min;
    ds_max=max;
    ds_mrhb=mrhb;
    ds_last=init;
    ds_value=0.0;
    ds_unknown_sec=0.0;
  }

let rrd_create dss rras timestep inittime =
  (* Use the standard update routines to initialise everything to correct values *)
  let rrd = {
    last_updated=0.0;
    timestep=timestep;
    rrd_dss=dss;
    rrd_rras=Array.map (fun rra ->
        { rra with
          rra_data = Array.init (Array.length dss) (fun _ -> Fring.make rra.rra_row_cnt nan);
          rra_cdps = Array.init (Array.length dss) (fun _ -> {cdp_value=0.0; cdp_unknown_pdps=0})
        }) rras;
  } in
  let values = Array.map (fun ds -> ds.ds_last) dss in
  let transforms = Array.make (Array.length values) (fun x -> x) in
  ds_update rrd inittime values transforms true;
  rrd

(** Add in a new DS into a pre-existing RRD. Preserves data of all the other archives
    and fills the new one full of NaNs. Note that this doesn't fill in the CDP values
    correctly at the moment!

    now = Unix.gettimeofday ()
*)

let rrd_add_ds rrd now newds =
  if List.mem newds.ds_name (ds_names rrd) then rrd else
    let npdps = Int64.div (Int64.of_float now) rrd.timestep in
    {rrd with
     rrd_dss = Array.append rrd.rrd_dss [|newds|];
     rrd_rras = Array.map (fun rra ->
         let nunknowns = Int64.to_int (Int64.rem npdps (Int64.of_int rra.rra_pdp_cnt)) in
         { rra with
           rra_data = Array.append rra.rra_data [| Fring.make rra.rra_row_cnt nan |];
           rra_cdps = Array.append rra.rra_cdps [| {cdp_value=cf_init_value rra.rra_cf; cdp_unknown_pdps=nunknowns} |]; }) rrd.rrd_rras;
    }

(** Remove the named DS from an RRD. Removes all of the data associated with it, too *)
let rrd_remove_ds rrd ds_name =
  let n = Utils.array_index ds_name (Array.map (fun ds -> ds.ds_name) rrd.rrd_dss) in
  if n = -1 then
    raise (Invalid_data_source ds_name)
  else
    { rrd with
      rrd_dss = Utils.array_remove n rrd.rrd_dss;
      rrd_rras = Array.map (fun rra ->
          { rra with
            rra_data = Utils.array_remove n rra.rra_data;
            rra_cdps = Utils.array_remove n rra.rra_cdps }) rrd.rrd_rras; }

(** Find the RRA with a particular CF that contains a particular start
    time, and also has a minimum pdp_cnt. If it can't find an
    appropriate one, either return the RRA with the correct CF that
    has the most ancient data, or raise No_RRA_Available if there's
    not archive with the correct CF. Assumes the RRAs are stored in
    increasing time-length *)
let find_best_rras rrd pdp_interval cf start =
  let rras =
    match cf with
    | Some realcf -> List.filter (fun rra -> rra.rra_cf=realcf) (Array.to_list rrd.rrd_rras)
    | None -> Array.to_list rrd.rrd_rras in
  (if List.length rras = 0 then raise No_RRA_Available);
  let (last_pdp_time, _age) = get_times rrd.last_updated rrd.timestep in
  let contains_time t rra =
    let lasttime = Int64.sub last_pdp_time (Int64.mul rrd.timestep (Int64.of_int (rra.rra_row_cnt * rra.rra_pdp_cnt))) in
    (rra.rra_pdp_cnt >= pdp_interval) && (t > lasttime)
  in
  try
    let first_ok_rra = List.find (contains_time start) rras in
    let pdp_cnt = first_ok_rra.rra_pdp_cnt in
    let row_cnt = first_ok_rra.rra_row_cnt in
    let ok_rras = List.filter (fun rra -> rra.rra_row_cnt = row_cnt && rra.rra_pdp_cnt=pdp_cnt) rras in
    ok_rras
  with _ ->
    let rra = List.hd (List.rev rras) in
    let newstarttime = Int64.add 1L (Int64.sub last_pdp_time (Int64.mul rrd.timestep (Int64.of_int (rra.rra_row_cnt * rra.rra_pdp_cnt)))) in
    List.filter (contains_time newstarttime) rras


(* now = Unix.gettimeofday () *)
let query_named_ds rrd now ds_name cf =
  let n = Utils.array_index ds_name (Array.map (fun ds -> ds.ds_name) rrd.rrd_dss) in
  if n = -1 then
    raise (Invalid_data_source ds_name)
  else
    let rras = find_best_rras rrd 0 (Some cf) (Int64.of_float now) in
    Fring.peek (List.hd rras).rra_data.(n) 0

(******************************************************************************)
(* Marshalling/Unmarshalling functions                                        *)
(******************************************************************************)

let from_xml input =
  let open Utils.Xmlm_utils in

  let read_header i =
    ignore (get_el "version" i);
    let step = get_el "step" i in
    let last_update = get_el "lastupdate" i in
    (step,last_update)
  in

  let read_dss i =
    let read_ds i =
      read_block "ds" (fun i ->
          let name = get_el "name" i in
          let type_ = get_el "type" i in
          let min_hb = get_el "minimal_heartbeat" i in
          let min = get_el "min" i in
          let max = get_el "max" i in
          ignore(get_el "last_ds" i);
          let value = get_el "value" i in
          let unknown_sec = get_el "unknown_sec" i in
          {
            ds_name = name;
            ds_ty = (match type_ with
                | "GAUGE" -> Gauge
                | "ABSOLUTE" -> Absolute
                | "DERIVE" -> Derive
                | _ -> failwith "Bad format");
            ds_mrhb = float_of_string min_hb;
            ds_min = float_of_string min;
            ds_max = float_of_string max;
            ds_last = VT_Unknown; (* float_of_string "last_ds"; *)
            ds_value = float_of_string value;
            ds_unknown_sec = float_of_string unknown_sec
          }) i
    in
    let dss = read_all "ds" read_ds i [] in
    dss
  in

  let read_rras i =
    let read_rra i =
      let read_cdp_prep i =
        let read_ds i =
          read_block "ds" (fun i ->
              ignore(get_el "primary_value" i);
              ignore(get_el "secondary_value" i);
              let value = get_el "value" i in
              let unknown_datapoints = get_el "unknown_datapoints" i in
              {
                cdp_value=float_of_string value;
                cdp_unknown_pdps=int_of_string unknown_datapoints
              }) i
        in
        let cdps = read_block "cdp_prep" (fun i -> read_all "ds" read_ds i []) i in
        cdps
      in
      let read_database i =
        let read_row i = (* should directly write in fring *)
          let row = read_block "row" (fun i -> Array.of_list (iter_seq (get_el "v") [] i)) i in
          row
        in
        let data = read_block "database" (fun i -> Array.of_list (read_all "row" read_row i [])) i in
        let rows = Array.length data in
        let cols = try Array.length data.(0) with _ -> -1 in
        let db = Array.init cols (fun _ -> Fring.make rows nan) in
        for i=0 to cols-1 do
          for j=0 to rows-1 do
            Fring.push db.(i) (float_of_string data.(j).(i))
          done
        done;
        db
      in
      let rra = read_block "rra" (fun i ->
          let cf = get_el "cf" i in
          let pdp_cnt = get_el "pdp_per_row" i in
          let xff = read_block "params" (fun i -> get_el "xff" i) i in
          let cdps = read_cdp_prep i in
          let database = read_database i in
          {
            rra_cf = (match cf with
                | "AVERAGE" -> CF_Average
                | "MIN" -> CF_Min
                | "MAX" -> CF_Max
                | "LAST" -> CF_Last
                | _ -> raise Utils.Parse_error);
            rra_row_cnt = Fring.length database.(0);
            rra_pdp_cnt = int_of_string pdp_cnt;
            rra_xff = float_of_string xff;
            rra_data = database;
            rra_cdps = Array.of_list cdps;
            rra_updatehook = None
          }) i in
      rra
    in
    let rras = read_all "rra" read_rra i [] in
    rras
  in

  accept (`Dtd None) input;
  read_block "rrd" (fun i ->
      let (step,last_update) = read_header i in (* ok *)
      let dss = read_dss i in (* ok *)
      let rras = read_rras i in
      let rrd = {
        last_updated = float_of_string last_update;
        timestep = Int64.of_string step;
        rrd_dss = Array.of_list dss;
        rrd_rras = Array.of_list rras
      } in

      (* Purge any repeated data sources from the RRD *)
      let ds_names = ds_names rrd in
      let ds_names_set = Utils.setify ds_names in
      let ds_name_counts = List.map (fun name ->
          let (x, _) = List.partition ((=) name) ds_names in
          (name,List.length x)) ds_names_set
      in
      let removals_required = List.filter (fun (_,x) -> x > 1) ds_name_counts in
      List.fold_left (fun rrd (name,n) ->
          (* Remove n-1 lots of this data source *)
          let rec inner rrd n =
            if n = 1
            then rrd
            else inner (rrd_remove_ds rrd name) (n - 1)
          in
          inner rrd n) rrd removals_required) input


let xml_to_output rrd output =
  (* We use an output channel for Xmlm-compat buffered output. Provided we flush
     	   at the end we should be safe. *)

  let tag n fn output =
    Xmlm.output output (`El_start (("",n),[]));
    fn output;
    Xmlm.output output (`El_end)
  in
  let data dat output = Xmlm.output output (`Data dat) in

  let do_ds ds output =
    tag "ds" (fun output ->
        tag "name" (data ds.ds_name) output;
        tag "type" (data (match ds.ds_ty with Gauge -> "GAUGE" | Absolute -> "ABSOLUTE" | Derive -> "DERIVE")) output;
        tag "minimal_heartbeat" (data (Utils.f_to_s ds.ds_mrhb)) output;
        tag "min" (data (Utils.f_to_s ds.ds_min)) output;
        tag "max" (data (Utils.f_to_s ds.ds_max)) output;
        tag "last_ds" (data (match ds.ds_last with VT_Float x -> Utils.f_to_s x | VT_Int64 x -> Printf.sprintf "%Ld" x | _ -> "0.0")) output;
        tag "value" (data (Utils.f_to_s ds.ds_value)) output;
        tag "unknown_sec" (data (Printf.sprintf "%d" (int_of_float ds.ds_unknown_sec))) output)
      output
  in

  let do_dss dss output =
    Array.iter (fun ds -> do_ds ds output) dss
  in

  let do_rra_cdp cdp output =
    tag "ds" (fun output ->
        tag "primary_value" (data "0.0") output;
        tag "secondary_value" (data "0.0") output;
        tag "value" (data (Utils.f_to_s cdp.cdp_value)) output;
        tag "unknown_datapoints" (data (Printf.sprintf "%d" cdp.cdp_unknown_pdps)) output)
      output
  in

  let do_rra_cdps cdps output =
    Array.iter (fun cdp -> do_rra_cdp cdp output) cdps
  in

  let do_database rings output =
    if Array.length rings = 0 then () else
      let rows = Fring.length rings.(0) in
      let cols = Array.length rings in
      for row=0 to rows-1 do
        tag "row" (fun output ->
            for col=0 to cols-1 do
              tag "v" (data (Utils.f_to_s (Fring.peek rings.(col) (rows-row-1)))) output
            done) output
      done
  in

  let do_rra rra output =
    tag "rra" (fun output ->
        tag "cf" (data (match rra.rra_cf with CF_Average -> "AVERAGE" | CF_Max -> "MAX" | CF_Min -> "MIN" | CF_Last -> "LAST")) output;
        tag "pdp_per_row" (data (string_of_int rra.rra_pdp_cnt)) output;
        tag "params" (tag "xff" (data (Utils.f_to_s rra.rra_xff))) output;
        tag "cdp_prep" (fun output ->
            do_rra_cdps rra.rra_cdps output) output;
        tag "database" (fun output ->
            do_database rra.rra_data output) output) output
  in

  let do_rras rras output =
    Array.iter (fun rra -> do_rra rra output) rras
  in

  Xmlm.output output (`Dtd None);
  tag "rrd" (fun output ->
      tag "version" (data "0003") output;
      tag "step" (data (Int64.to_string rrd.timestep)) output;
      tag "lastupdate" (data (Printf.sprintf "%Ld" (Int64.of_float (rrd.last_updated)))) output;
      do_dss rrd.rrd_dss output;
      do_rras rrd.rrd_rras output)
    output

module Json = struct

  let string fmt = Printf.ksprintf (fun msg -> `String msg) fmt
  let float x    = string "%.2f" x
  let record xs  = `O xs
  let array  xs  = `A xs

  let ty = function
    | Gauge     -> string "GAUGE"
    | Absolute  -> string "ABSOLUTE"
    | Derive    -> string "DERIVE"

  let datasource ds =
    record
      [ "name"               , string "%s" ds.ds_name
      ; "type"               , ty ds.ds_ty
      ; "minimal_hearbeat"   , string "%s" (Utils.f_to_s ds.ds_mrhb)
      ; "min"                , string "%s" (Utils.f_to_s ds.ds_min)
      ; "max"                , string "%s" (Utils.f_to_s ds.ds_max)
      ; "last_ds",           ( match ds.ds_last with
                             | VT_Float x -> string "%s" (Utils.f_to_s x)
                             | VT_Int64 x -> string "%Ld" x
                             | _          -> float 0.0
                             )
      ; "value"              , string "%s" (Utils.f_to_s ds.ds_value)
      ; "unknown_sec"        , string "%d" (int_of_float ds.ds_unknown_sec)
      ]

  let cdp x =
    record
      [ "primary_value"       , float 0.0
      ; "secondary_value"     , float 0.0
      ; "value"               , string "%s" (Utils.f_to_s x.cdp_value)
      ; "unknown_datapoints"  , float @@ float_of_int @@ x.cdp_unknown_pdps
      ]

  let get rings rows row col =
    Fring.peek rings.(col) (rows-row-1) |> Utils.f_to_s |> string "%s"

  let database = function
    | [||]  -> array []
    | rings ->
      let rows = Fring.length rings.(0) in
      let cols = Array.length rings in
      array @@ Array.to_list @@ Array.init rows (fun row ->
          array @@ Array.to_list @@ Array.init cols (fun col -> get rings rows row col))

  let rra x =
    record
      [ "cf"           , string "%s" (cf_type_to_string x.rra_cf)
      ; "pdp_per_row"  , string "%d" x.rra_pdp_cnt
      ; "params"       , record
          [ "xff", string "%s" (Utils.f_to_s x.rra_xff) ]
      ; "cdp_prep"     , record
          [ "ds" , array @@ List.map cdp @@ Array.to_list x.rra_cdps ]
      ; "database"     , database x.rra_data
      ]
  let rrd x =
    record
      [ "version"     , string "0003"
      ; "step"        , string "%Ld" x.timestep
      ; "lastupdate"  , string "%s" (Utils.f_to_s x.last_updated)
      ; "ds"          , array @@ List.map datasource @@ Array.to_list x.rrd_dss
      ; "rra"         , array @@ List.map rra @@ Array.to_list x.rrd_rras
      ]
end

let json_to_string rrd =
  let buf    = Buffer.create 4096 in
  let ()     = Ezjsonm.to_buffer buf (Json.rrd rrd) in
    Buffer.contents buf

module Statefile_latency = struct
  type t = {id: string; latency: float option} [@@deriving rpc]
end
