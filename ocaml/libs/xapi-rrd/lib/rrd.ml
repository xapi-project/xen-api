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
(** This module provides a util that records data in a way that's compatible
    with {{: http://oss.oetiker.ch/rrdtool/index.en.html} rrdtool}. *)

module Fring = Rrd_fring
module Utils = Rrd_utils
module StringMap = Map.Make (String)

exception No_RRA_Available

exception Invalid_data_source of string

(** Inverse is (fun x -> 1.0 - x) *)
type ds_transform_function = Inverse | Identity

let apply_transform_function f x =
  match f with Inverse -> 1.0 -. x | Identity -> x

type ds_owner = VM of string | Host | SR of string

(** Data source types - see ds datatype *)
type ds_type = Absolute | Gauge | Derive [@@deriving rpc]

(** Consolidation function - see RRA datatype *)
type cf_type = CF_Average | CF_Min | CF_Max | CF_Last

(** Container so that we can handle different typed inputs *)
type ds_value_type = VT_Float of float | VT_Int64 of int64 | VT_Unknown
[@@deriving rpc]

type sampling_frequency = Five_Seconds [@@deriving rpc]

(* utility *)

let ( +++ ) = Int64.add

let ( --- ) = Int64.sub

let ( *** ) = Int64.mul

let ( /// ) = Int64.div

let ds_type_to_string = function
  | Gauge ->
      "GAUGE"
  | Absolute ->
      "ABSOLUTE"
  | Derive ->
      "DERIVE"

let cf_type_of_string = function
  | "AVERAGE" ->
      CF_Average
  | "MIN" ->
      CF_Min
  | "MAX" ->
      CF_Max
  | "LAST" ->
      CF_Last
  | x ->
      failwith (Printf.sprintf "Unknown cf_type: %s" x)

let cf_type_to_string = function
  | CF_Average ->
      "AVERAGE"
  | CF_Max ->
      "MAX"
  | CF_Min ->
      "MIN"
  | CF_Last ->
      "LAST"

let ds_value_to_string = function
  | VT_Float x ->
      Utils.f_to_s x
  | VT_Int64 x ->
      Printf.sprintf "%Ld" x
  | _ ->
      "0.0"

let ds_transform_function_to_string = function
  | Inverse ->
      "inverse"
  | Identity ->
      "identity"

(** The CDP preparation scratch area.
    The 'value' field should be accumulated in such a way that it always
    contains the value that will eventually be the CDP. This means that
    for averages, we accumulate 1/n * the PDP, and renormalise when we
    have unknown PDPs. For the other types it's much easier *)

type cdp_prep = {
    mutable cdp_value: float
  ; mutable cdp_unknown_pdps: int  (** How may PDPs have been unknown so far *)
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
    ds_name: string  (** Name *)
  ; ds_ty: ds_type
        (** Type of rate the input must be processed as, see above *)
  ; ds_min: float
  ; ds_max: float
  ; ds_mrhb: float  (** Maximum time between updates *)
  ; mutable ds_last: ds_value_type  (** Last raw value that was processed *)
  ; mutable ds_value: float  (** Current calculated rate of the PDP *)
  ; mutable ds_unknown_sec: float
        (** Number of seconds that are unknown in the current PDP *)
  ; mutable ds_last_updated: float  (** Last time this datasource was updated *)
}
[@@deriving rpc]

(** RRA - RRD archive
    This is an archive that holds consolidated data points (CDPs) belonging to
    a single consolidation function. They are stored in rings buffers, each
    one related to a single  different data-source. It defines the type of
    consolidation that happens (average, max, min or last), the number of
    primary data points (PDPs) that go to make a CDP, and the number of CDPs
    to store.

    To better visualize how the datapoints are stored:

     │   Datasources   ┃                 ┃                 ┃
     └─────────────────┨     Memory      ┃     cputime     ┃
       Consolidators   ┃                 ┃                 ┃
     ━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━┫
       Average         ┃  Fring of CDPs  ┃ Fring of CDPs   ┃ ← RRA
     ━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━┫
       Max             ┃  Fring of CDPs  ┃ Fring of CDPs   ┃ ← RRA
     ━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━┛
    *)

type rra = {
    rra_cf: cf_type  (** consolidation function *)
  ; rra_row_cnt: int  (** number of entries to store *)
  ; rra_pdp_cnt: int  (** number of pdps per cdp *)
  ; rra_xff: float
        (** proportion of missing pdps at which we mark the cdp as unknown *)
  ; rra_data: Fring.t array  (** Stored data, one ring per datasource *)
  ; rra_cdps: cdp_prep array
        (** scratch area for consolidated datapoint preparation *)
}

(** The container for the DSs and RRAs. Also specifies the period between pdps *)

and rrd = {
    mutable last_updated: float  (** Last updated time in seconds *)
  ; timestep: int64  (** Period between PDPs *)
  ; rrd_dss: ds array
  ; rrd_rras: rra array
}

(** Parts of the datasources used in updating RRDs to minimize transferred data *)

and ds_value_and_transform = {
    value: ds_value_type
  ; transform: ds_transform_function
}

let copy_cdp_prep x =
  {cdp_value= x.cdp_value; cdp_unknown_pdps= x.cdp_unknown_pdps}

let copy_rra x =
  {
    rra_cf= x.rra_cf
  ; rra_row_cnt= x.rra_row_cnt
  ; rra_pdp_cnt= x.rra_pdp_cnt
  ; rra_xff= x.rra_xff
  ; rra_data= Array.map Fring.copy x.rra_data
  ; rra_cdps= Array.map copy_cdp_prep x.rra_cdps
  }

let copy_ds x =
  {
    ds_name= x.ds_name (* not mutable *)
  ; ds_ty= x.ds_ty
  ; ds_min= x.ds_min
  ; ds_max= x.ds_max
  ; ds_mrhb= x.ds_mrhb
  ; ds_last= x.ds_last
  ; ds_value= x.ds_value
  ; ds_unknown_sec= x.ds_unknown_sec
  ; ds_last_updated= x.ds_last_updated
  }

let copy_rrd x =
  {
    last_updated= x.last_updated
  ; timestep= x.timestep
  ; rrd_dss= Array.map copy_ds x.rrd_dss
  ; rrd_rras= Array.map copy_rra x.rrd_rras
  }

(* CA-329043: avoid producing out-of-range rates *)
let cf_init_value cf ds =
  let default =
    match cf with
    | CF_Average ->
        0.0
    | CF_Min ->
        infinity
    | CF_Max ->
        neg_infinity
    | CF_Last ->
        nan
  in
  min ds.ds_max (max ds.ds_min default)

(** Helper function to get the start time and age of the current/last PDP *)
let get_times time timestep =
  let starttime = timestep *** (Int64.of_float time /// timestep) in
  let age = time -. Int64.to_float starttime in
  (starttime, age)

let get_float_time time timestep =
  let timestep = Int64.to_float timestep in
  let starttime = timestep *. (time /. timestep) in
  starttime

(** Update the CDP value with a number (start_pdp_offset) of PDPs. *)
let do_cfs rra start_pdp_offset pdps =
  Array.iter
    (fun (i, pdp) ->
      let cdp = rra.rra_cdps.(i) in
      if Utils.isnan pdp then (
        (* CDP is an accumulator for the average. If we've got some unknowns, we need to
           renormalize. ie, CDP contains \sum_{i=0}^j{ (1/n) x_i} where n is the number of
           values we expect to have. If we have unknowns, we need to multiply the whole
           thing by \frac{n_{old}}{n_{new}} *)
        let olddiv = rra.rra_pdp_cnt - cdp.cdp_unknown_pdps in
        let newdiv = olddiv - start_pdp_offset in
        if newdiv > 0 then (
          cdp.cdp_value <-
            cdp.cdp_value *. float_of_int olddiv /. float_of_int newdiv ;
          cdp.cdp_unknown_pdps <- cdp.cdp_unknown_pdps + start_pdp_offset
        )
      ) else
        let cdpv = cdp.cdp_value in
        cdp.cdp_value <-
          ( match rra.rra_cf with
          | CF_Average ->
              cdpv
              +. pdp
                 *. float_of_int start_pdp_offset
                 /. float_of_int rra.rra_pdp_cnt
          | CF_Min ->
              min cdpv pdp
          | CF_Max ->
              max cdpv pdp
          | CF_Last ->
              pdp
          )
    )
    pdps

(** Update the RRAs with a number of PDPs. *)
let rra_update rrd proc_pdp_st elapsed_pdp_st pdps =
  let updatefn rra =
    let start_pdp_offset =
      rra.rra_pdp_cnt
      - Int64.(
          to_int (rem (proc_pdp_st /// rrd.timestep) (of_int rra.rra_pdp_cnt))
        )
    in
    let rra_step_cnt =
      if elapsed_pdp_st < start_pdp_offset then
        0
      else
        ((elapsed_pdp_st - start_pdp_offset) / rra.rra_pdp_cnt) + 1
    in
    do_cfs rra (min start_pdp_offset elapsed_pdp_st) pdps ;
    if rra_step_cnt > 0 then (
      (* When writing multiple CDP values into the archive, the
         first one (primary) is calculated using the values we
         already had accumulated from the last update, whereas any
         subsequent values (secondary) are calculated just using the
         current PDP. It turns out that the secondary values are
         simply the PDPs as whichever CF is used, a CDP of many
         repeated values is simply the value itself. *)
      let primaries =
        Array.map
          (fun (i, _) ->
            let cdp = rra.rra_cdps.(i) in
            if
              cdp.cdp_unknown_pdps
              <= int_of_float (rra.rra_xff *. float_of_int rra.rra_pdp_cnt)
            then
              (i, cdp.cdp_value)
            else
              (i, nan)
          )
          pdps
      in
      let secondaries = pdps in

      let push (i, value) = Fring.push rra.rra_data.(i) value in
      Array.iter push primaries ;
      for _ = 1 to min (rra_step_cnt - 1) rra.rra_row_cnt do
        Array.iter push secondaries
      done ;

      (* Reinitialise the CDP preparation area *)
      let new_start_pdp_offset =
        (elapsed_pdp_st - start_pdp_offset) mod rra.rra_pdp_cnt
      in
      Array.iter
        (fun (i, _) ->
          let cdp = rra.rra_cdps.(i) in
          let ds = rrd.rrd_dss.(i) in
          let cdp_init = cf_init_value rra.rra_cf ds in
          cdp.cdp_unknown_pdps <- 0 ;
          cdp.cdp_value <- cdp_init
        )
        pdps ;
      do_cfs rra new_start_pdp_offset pdps
    )
  in
  Array.iter updatefn rrd.rrd_rras

(* We assume that the data being given is of the form of a rate; that is,
   it's dependent on the time interval between updates.
   Gauge data sources are simply kept as is without any time-based
   calculations, while Absolute and Derive data sources will be changed
   according to the time passed since the last measurement. (see CA-404597) *)
let process_ds_value ds value interval new_rrd =
  if interval > ds.ds_mrhb then
    nan
  else
    let value_raw =
      match value with
      | VT_Int64 y ->
          Int64.to_float y
      | VT_Float y ->
          y
      | VT_Unknown ->
          nan
    in

    let rate =
      match (ds.ds_ty, new_rrd) with
      | Derive, true | Gauge, _ ->
          value_raw
      | Absolute, _ ->
          value_raw /. interval
      | Derive, false -> (
        match (ds.ds_last, value) with
        | VT_Int64 x, VT_Int64 y ->
            Int64.to_float (y --- x)
        | VT_Float x, VT_Float y ->
            y -. x
        | VT_Unknown, _ | _, VT_Unknown ->
            nan
        | _ ->
            failwith ("Bad type updating ds: " ^ ds.ds_name)
      )
    in
    ds.ds_last <- value ;
    rate

let ds_update rrd timestamp valuesandtransforms new_rrd =
  (* CA-408841 - don't update the rrd at all if list of datasources is empty *)
  if valuesandtransforms <> [||] then (
    (* Interval is the time between this and the last update

       Currently ds_update is called with datasources that belong to a single
       plugin, correspondingly they all have the same timestamp.
       Further refactoring is needed if timestamps per measurement are to be
       introduced. *)
    let first_ds_index, _ = valuesandtransforms.(0) in
    let last_updated = rrd.rrd_dss.(first_ds_index).ds_last_updated in
    let interval = timestamp -. last_updated in
    (* Work around the clock going backwards *)
    let interval = if interval < 0. then 5. else interval in

    (* start time (st) and age of the last processed pdp and the currently occupied one *)
    let proc_pdp_st, _proc_pdp_age = get_times last_updated rrd.timestep in
    let occu_pdp_st, occu_pdp_age = get_times timestamp rrd.timestep in

    (* The number of pdps that should result from this update *)
    let elapsed_pdp_st =
      Int64.to_int ((occu_pdp_st --- proc_pdp_st) /// rrd.timestep)
    in

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
    rrd.last_updated <- timestamp ;

    (* Calculate the values we're going to store based on the input data and the type of the DS *)
    let v2s =
      Array.map
        (fun (i, {value; _}) ->
          let v = process_ds_value rrd.rrd_dss.(i) value interval new_rrd in
          rrd.rrd_dss.(i).ds_last_updated <- timestamp ;
          (i, v)
        )
        valuesandtransforms
    in
    (* Update the PDP accumulators up until the most recent PDP *)
    Array.iter
      (fun (i, value) ->
        let ds = rrd.rrd_dss.(i) in
        if Utils.isnan value then
          ds.ds_unknown_sec <- pre_int
        else
          (* CA-404597 - Gauge and Absolute values should be passed as-is,
             without being involved in time-based calculations at all.
             This applies to calculations below as well *)
            match ds.ds_ty with
          | Gauge | Absolute ->
              ds.ds_value <- value
          | Derive ->
              ds.ds_value <- ds.ds_value +. (pre_int *. value /. interval)
      )
      v2s ;

    (* If we've passed a PDP point, we need to update the RRAs *)
    if elapsed_pdp_st > 0 then (
      (* Calculate the PDPs for each DS *)
      let pdps =
        Array.map
          (fun (i, {transform; _}) ->
            let ds = rrd.rrd_dss.(i) in
            if interval > ds.ds_mrhb then
              (i, nan)
            else
              let raw =
                let proc_pdp_st = get_float_time last_updated rrd.timestep in
                let occu_pdp_st = get_float_time timestamp rrd.timestep in

                match ds.ds_ty with
                | Gauge | Absolute ->
                    ds.ds_value
                | Derive ->
                    ds.ds_value
                    /. (occu_pdp_st -. proc_pdp_st -. ds.ds_unknown_sec)
              in
              (* Apply the transform after the raw value has been calculated *)
              let raw = apply_transform_function transform raw in

              (* Make sure the values are not out of bounds after all the processing *)
              match (ds.ds_ty, raw) with
              | Derive, _ when raw > ds.ds_max && raw < ds.ds_max *. (1. +. 0.05)
                ->
                  (* CA-411679: To handle deviations in CPU rates, Derive values
                     exceeding the maximum by up to 5% are capped at the maximum;
                     others are marked as unknown. This logic is specific to
                     Derive data sources because they represent rates derived
                     from differences over time, which can occasionally exceed
                     expected bounds due to measurement inaccuracies. *)
                  (i, ds.ds_max)
              | (Derive | Gauge | Absolute), _
                when raw < ds.ds_min || raw > ds.ds_max ->
                  (i, nan)
              | (Derive | Gauge | Absolute), _ ->
                  (i, raw)
          )
          valuesandtransforms
      in

      rra_update rrd proc_pdp_st elapsed_pdp_st pdps ;

      (* Reset the PDP accumulators *)
      Array.iter
        (fun (i, value) ->
          let ds = rrd.rrd_dss.(i) in
          if Utils.isnan value then (
            ds.ds_value <- 0.0 ;
            ds.ds_unknown_sec <- post_int
          ) else (
            ds.ds_unknown_sec <- 0.0 ;
            match ds.ds_ty with
            | Gauge | Absolute ->
                ds.ds_value <- value
            | Derive ->
                ds.ds_value <- post_int *. value /. interval
          )
        )
        v2s
    )
  )

(** Update the rrd with named values rather than just an ordered array
    Must be called with datasources coming from a single plugin, with
    [timestamp] and [uid] representing it *)
let ds_update_named rrd ~new_rrd timestamp valuesandtransforms =
  (* NOTE:
     RRD data is stored in several arrays, with the same index pointing to the
     same datasource's data in different arrays. This dependency is not always
     obvious and doesn't apply to everything, i.e. 'rrd_dss' stores datasources
     one after another, but the 'rrd_rras' are actually sideways matrices,
     with rrd_rras.(i).rra_data containing Frings for _all_ datasources, not
     just the i-th datasource. So if one datasource is removed or adjusted,
     one needs to update RRAs by iterating over all 'rrd_rras', not just
     changing the i-th array.

     rrdd_monitor processes datasources per plugin (and then per owner), so the
     list of 'valuesandtransforms' all come with a single timestamp. But these
     datasources can be located all over the 'rrd_dss' array, not necessarily
     consecutively. Non-exhaustive examples of why that can happen:
     1) initially disabled datasources can be enabled at runtime behind our
        back, which adds them to the end of the rrd_dss array
     2) on toolstack restart, RRDs are restored from the filesystem, but the
        new order of registration of plugins might not necessarily be the same
        as the one before the restart (so they might be consecutive, but static
        chunk indexes can't be assumed)
     3) rrd_monitor iterates over the hash table of registered plugins, which
        means that plugins registered later can end up earlier in its ordering

     All this means that plugin's datasources can not be assumed to be
     consecutive and each datasource should carry its index in rrd's arrays
     with itself, they can't just be processed in chunks.

     (This is due to how this used to be organized historically, with all of
     the RRD's datasources processed at once with the server's timestamp, even
     though they could have come from different plugins originally)
  *)
  let arr, _ =
    Array.fold_left
      (fun (arr, i) {ds_name; _} ->
        match StringMap.find_opt ds_name valuesandtransforms with
        | Some ds ->
            (Array.append arr [|(i, ds)|], i + 1)
        | None ->
            (arr, i + 1)
      )
      ([||], 0) rrd.rrd_dss
  in
  ds_update rrd timestamp arr new_rrd

(** Get registered DS names *)
let ds_names rrd = Array.to_list (Array.map (fun ds -> ds.ds_name) rrd.rrd_dss)

(** create an rra structure *)
let rra_create cf row_cnt pdp_cnt xff =
  {
    rra_cf= cf
  ; rra_row_cnt= row_cnt
  ; rra_pdp_cnt= pdp_cnt
  ; rra_xff= xff
  ; rra_data=
      [||]
      (* defer creation of the data until we know how many dss we're storing *)
  ; rra_cdps=
      [||]
      (* defer creation of the data until we know how many dss we're storing *)
  }

let ds_create name ty ~min ~max ~mrhb init =
  {
    ds_name= name
  ; ds_ty= ty
  ; ds_min= min
  ; ds_max= max
  ; ds_mrhb= mrhb
  ; ds_last= init
  ; ds_value= 0.0
  ; ds_unknown_sec= 0.0
  ; ds_last_updated= 0.0
  }

let rrd_create dss rras timestep timestamp =
  let rrd =
    {
      last_updated= 0.0
    ; timestep
    ; rrd_dss= dss
    ; rrd_rras=
        Array.map
          (fun rra ->
            {
              rra with
              rra_data=
                Array.map
                  (fun ds -> Fring.make rra.rra_row_cnt nan ds.ds_min ds.ds_max)
                  dss
            ; rra_cdps=
                Array.map
                  (fun ds ->
                    let cdp_init = cf_init_value rra.rra_cf ds in
                    {cdp_value= cdp_init; cdp_unknown_pdps= 0}
                  )
                  dss
            }
          )
          rras
    }
  in
  let valuesandtransforms =
    Array.mapi (fun i ds -> (i, {value= ds.ds_last; transform= Identity})) dss
  in
  (* Use the standard update routines to initialise everything to correct values *)
  ds_update rrd timestamp valuesandtransforms true ;
  rrd

(** Add the datasource even if it exists in the RRD already. *)
let rrd_add_ds_unsafe rrd timestamp newds =
  let npdps = Int64.of_float timestamp /// rrd.timestep in
  {
    rrd with
    rrd_dss= Array.append rrd.rrd_dss [|newds|]
  ; rrd_rras=
      Array.map
        (fun rra ->
          let cdp_init = cf_init_value rra.rra_cf newds in
          let fring =
            Fring.make rra.rra_row_cnt nan newds.ds_min newds.ds_max
          in
          let nunknowns =
            Int64.to_int (Int64.rem npdps (Int64.of_int rra.rra_pdp_cnt))
          in
          {
            rra with
            rra_data= Array.append rra.rra_data [|fring|]
          ; rra_cdps=
              Array.append rra.rra_cdps
                [|{cdp_value= cdp_init; cdp_unknown_pdps= nunknowns}|]
          }
        )
        rrd.rrd_rras
  }

(** Add in a new DS into a pre-existing RRD. Preserves data of all the other
    archives and fills the new one full of NaNs. Note that this doesn't fill
    in the CDP values correctly at the moment!
*)

let rrd_add_ds rrd timestamp newds =
  if List.mem newds.ds_name (ds_names rrd) then
    rrd
  else
    rrd_add_ds_unsafe rrd timestamp newds

(** Remove the named DS from an RRD. Removes all of the data associated with
    it, too. THe function is idempotent. *)
let rrd_remove_ds rrd ds_name =
  match Utils.find_index (fun ds -> ds.ds_name = ds_name) rrd.rrd_dss with
  | None ->
      rrd
  | Some n ->
      {
        rrd with
        rrd_dss= Utils.array_remove n rrd.rrd_dss
      ; rrd_rras=
          Array.map
            (fun rra ->
              {
                rra with
                rra_data= Utils.array_remove n rra.rra_data
              ; rra_cdps= Utils.array_remove n rra.rra_cdps
              }
            )
            rrd.rrd_rras
      }

(** Find the RRA with a particular CF that contains a particular start
    time, and also has a minimum pdp_cnt. If it can't find an
    appropriate one, either return the RRA with the correct CF that
    has the most ancient data, or raise No_RRA_Available if there's
    not archive with the correct CF. Assumes the RRAs are stored in
    increasing time-length *)
let find_best_rras rrd pdp_interval cf start =
  let rras =
    match cf with
    | Some realcf ->
        List.filter (fun rra -> rra.rra_cf = realcf) (Array.to_list rrd.rrd_rras)
    | None ->
        Array.to_list rrd.rrd_rras
  in
  if rras = [] then raise No_RRA_Available ;
  let last_pdp_time, _age = get_times rrd.last_updated rrd.timestep in
  let contains_time t rra =
    let lasttime =
      last_pdp_time
      --- (rrd.timestep *** Int64.of_int (rra.rra_row_cnt * rra.rra_pdp_cnt))
    in
    rra.rra_pdp_cnt >= pdp_interval && t > lasttime
  in
  try
    let first_ok_rra = List.find (contains_time start) rras in
    let pdp_cnt = first_ok_rra.rra_pdp_cnt in
    let row_cnt = first_ok_rra.rra_row_cnt in
    let ok_rras =
      List.filter
        (fun rra -> rra.rra_row_cnt = row_cnt && rra.rra_pdp_cnt = pdp_cnt)
        rras
    in
    ok_rras
  with _ ->
    let rra = List.hd (List.rev rras) in
    let newstarttime =
      1L
      +++ last_pdp_time
      --- (rrd.timestep *** Int64.of_int (rra.rra_row_cnt * rra.rra_pdp_cnt))
    in
    List.filter (contains_time newstarttime) rras

let query_named_ds rrd as_of_time ds_name cf =
  match Utils.find_index (fun ds -> ds.ds_name = ds_name) rrd.rrd_dss with
  | None ->
      raise (Invalid_data_source ds_name)
  | Some n -> (
      let rras = find_best_rras rrd 0 (Some cf) (Int64.of_float as_of_time) in
      match rras with
      | [] ->
          raise No_RRA_Available
      | rra :: _ ->
          Fring.peek rra.rra_data.(n) 0
    )

(******************************************************************************)
(* Marshalling/Unmarshalling functions                                        *)
(******************************************************************************)

let from_xml input =
  let open Utils.Xmlm_utils in
  let read_header i =
    ignore (get_el "version" i) ;
    let step = get_el "step" i in
    let last_update = float_of_string (get_el "lastupdate" i) in
    (step, last_update)
  in

  let read_dss i rrd_last_update =
    let read_ds i =
      read_block "ds"
        (fun i ->
          let name = get_el "name" i in
          let type_ = get_el "type" i in
          let min_hb = get_el "minimal_heartbeat" i in
          (* CA-408126 - work around negative data in historical RRDs
             where ds_min could have been incorrectly set to neg_infinity.
             Setting ds_min to 0. means Fring.make below will turn negative
             historical values to NaNs.*)
          let min = max (float_of_string (get_el "min" i)) 0. in
          let max = get_el "max" i in
          ignore (get_el "last_ds" i) ;
          let value = get_el "value" i in
          let unknown_sec = get_el "unknown_sec" i in
          let last_updated =
            try float_of_string (get_el "last_updated" i)
            with _ -> rrd_last_update
          in
          {
            ds_name= name
          ; ds_ty=
              ( match type_ with
              | "GAUGE" ->
                  Gauge
              | "ABSOLUTE" ->
                  Absolute
              | "DERIVE" ->
                  Derive
              | _ ->
                  failwith "Bad format"
              )
          ; ds_mrhb= float_of_string min_hb
          ; ds_min= min
          ; ds_max= float_of_string max
          ; ds_last= VT_Unknown
          ; (* float_of_string "last_ds"; *)
            ds_value= float_of_string value
          ; ds_unknown_sec= float_of_string unknown_sec
          ; ds_last_updated= last_updated
          }
        )
        i
    in
    let dss = Array.of_list (read_all "ds" read_ds i []) in
    dss
  in

  let read_rras dss i =
    let read_rra i =
      let read_cdp_prep i =
        let read_ds i =
          read_block "ds"
            (fun i ->
              ignore (get_el "primary_value" i) ;
              ignore (get_el "secondary_value" i) ;
              let value = get_el "value" i in
              let unknown_datapoints = get_el "unknown_datapoints" i in
              {
                cdp_value= float_of_string value
              ; cdp_unknown_pdps= int_of_string unknown_datapoints
              }
            )
            i
        in
        let cdps =
          read_block "cdp_prep" (fun i -> read_all "ds" read_ds i []) i
        in
        cdps
      in
      let read_database i =
        let read_row i =
          (* should directly write in fring *)
          let row =
            read_block "row"
              (fun i -> Array.of_list (iter_seq (get_el "v") [] i))
              i
          in
          row
        in
        let data =
          read_block "database"
            (fun i -> Array.of_list (read_all "row" read_row i []))
            i
        in
        let rows = Array.length data in
        let cols = try Array.length data.(0) with _ -> -1 in
        let db =
          Array.init cols (fun i ->
              let ds = dss.(i) in
              Fring.make rows nan ds.ds_min ds.ds_max
          )
        in
        for i = 0 to cols - 1 do
          for j = 0 to rows - 1 do
            let value = float_of_string data.(j).(i) in
            Fring.push db.(i) value
          done
        done ;
        db
      in
      let rra =
        read_block "rra"
          (fun i ->
            let cf = get_el "cf" i in
            let pdp_cnt = get_el "pdp_per_row" i in
            let xff = read_block "params" (fun i -> get_el "xff" i) i in
            let cdps = read_cdp_prep i in
            let database = read_database i in
            {
              rra_cf=
                ( match cf with
                | "AVERAGE" ->
                    CF_Average
                | "MIN" ->
                    CF_Min
                | "MAX" ->
                    CF_Max
                | "LAST" ->
                    CF_Last
                | _ ->
                    raise Utils.Parse_error
                )
            ; rra_row_cnt= Fring.length database.(0)
            ; rra_pdp_cnt= int_of_string pdp_cnt
            ; rra_xff= float_of_string xff
            ; rra_data= database
            ; rra_cdps= Array.of_list cdps
            }
          )
          i
      in
      rra
    in
    let rras = read_all "rra" read_rra i [] in
    rras
  in

  accept (`Dtd None) input ;
  read_block "rrd"
    (fun i ->
      let step, last_update = read_header i in
      let dss = read_dss i last_update in
      let rras = read_rras dss i in
      let rrd =
        {
          last_updated= last_update
        ; timestep= Int64.of_string step
        ; rrd_dss= dss
        ; rrd_rras= Array.of_list rras
        }
      in

      (* Purge any repeated data sources from the RRD *)
      let ds_names = ds_names rrd in
      List.sort_uniq String.compare ds_names
      |> List.filter_map (fun name ->
             match List.filter (String.equal name) ds_names with
             | [] | [_] ->
                 None
             | x ->
                 Some (name, List.length x)
         )
      |> List.fold_left
           (fun rrd (name, n) ->
             (* Remove n-1 lots of this data source *)
             let rec inner rrd n =
               if n = 1 then
                 rrd
               else
                 inner (rrd_remove_ds rrd name) (n - 1)
             in
             inner rrd n
           )
           rrd
    )
    input

let xml_to_output internal rrd output =
  (* We use an output channel for Xmlm-compat buffered output. Provided we flush
     at the end we should be safe. *)
  let tag n fn output =
    Xmlm.output output (`El_start (("", n), [])) ;
    fn output ;
    Xmlm.output output `El_end
  in
  let data dat output = Xmlm.output output (`Data dat) in

  let do_ds ds output =
    tag "ds"
      (fun output ->
        tag "name" (data ds.ds_name) output ;
        tag "type" (data (ds_type_to_string ds.ds_ty)) output ;
        tag "minimal_heartbeat" (data (Utils.f_to_s ds.ds_mrhb)) output ;
        tag "min" (data (Utils.f_to_s ds.ds_min)) output ;
        tag "max" (data (Utils.f_to_s ds.ds_max)) output ;
        tag "last_ds" (data (ds_value_to_string ds.ds_last)) output ;
        tag "value" (data (Utils.f_to_s ds.ds_value)) output ;
        tag "unknown_sec"
          (data (Printf.sprintf "%d" (int_of_float ds.ds_unknown_sec)))
          output ;
        if internal then
          tag "last_updated" (data (Utils.f_to_s ds.ds_last_updated)) output
      )
      output
  in

  let do_dss dss output = Array.iter (fun ds -> do_ds ds output) dss in

  let do_rra_cdp cdp output =
    tag "ds"
      (fun output ->
        tag "primary_value" (data "0.0") output ;
        tag "secondary_value" (data "0.0") output ;
        tag "value" (data (Utils.f_to_s cdp.cdp_value)) output ;
        tag "unknown_datapoints"
          (data (Printf.sprintf "%d" cdp.cdp_unknown_pdps))
          output
      )
      output
  in

  let do_rra_cdps cdps output =
    Array.iter (fun cdp -> do_rra_cdp cdp output) cdps
  in

  let do_database rings output =
    if Array.length rings = 0 then
      ()
    else
      let rows = Fring.length rings.(0) in
      let cols = Array.length rings in
      for row = 0 to rows - 1 do
        tag "row"
          (fun output ->
            for col = 0 to cols - 1 do
              tag "v"
                (data (Utils.f_to_s (Fring.peek rings.(col) (rows - row - 1))))
                output
            done
          )
          output
      done
  in

  let do_rra rra output =
    tag "rra"
      (fun output ->
        tag "cf" (data (cf_type_to_string rra.rra_cf)) output ;
        tag "pdp_per_row" (data (string_of_int rra.rra_pdp_cnt)) output ;
        tag "params" (tag "xff" (data (Utils.f_to_s rra.rra_xff))) output ;
        tag "cdp_prep" (fun output -> do_rra_cdps rra.rra_cdps output) output ;
        tag "database" (fun output -> do_database rra.rra_data output) output
      )
      output
  in

  let do_rras rras output = Array.iter (fun rra -> do_rra rra output) rras in

  Xmlm.output output (`Dtd None) ;
  tag "rrd"
    (fun output ->
      tag "version" (data "0003") output ;
      tag "step" (data (Int64.to_string rrd.timestep)) output ;
      (* XenCenter and other legacy users expect this to be an integer
         representing the number of seconds, so keep it an integer to avoid
         breaking them. Some other libraries (e.g. the Python rrd parser,
         the C rrd parser, xenrt) will expect a float and can easily convert
         an int to it. *)
      tag "lastupdate"
        (data (Printf.sprintf "%Ld" (Int64.of_float rrd.last_updated)))
        output ;
      do_dss rrd.rrd_dss output ;
      do_rras rrd.rrd_rras output
    )
    output

module Json = struct
  let fmt fmt x = Printf.ksprintf (fun msg -> `String msg) fmt x

  let string x = fmt "%s" x

  let float x = string (Utils.f_to_s x)

  let int x = fmt "%d" x

  let int64 x = fmt "%Ld" x

  let record xs = `Assoc xs

  let array xs = `List xs

  let datasource ds =
    record
      [
        ("name", string ds.ds_name)
      ; ("type", string (ds_type_to_string ds.ds_ty))
      ; ("minimal_heartbeat", float ds.ds_mrhb)
      ; ("min", float ds.ds_min)
      ; ("max", float ds.ds_max)
      ; ("last_ds", string (ds_value_to_string ds.ds_last))
      ; ("value", float ds.ds_value)
      ; ("unknown_sec", float ds.ds_unknown_sec)
      ; ("last_updated", float ds.ds_last_updated)
      ]

  let cdp x =
    record
      [
        ("primary_value", float 0.0)
      ; ("secondary_value", float 0.0)
      ; ("value", float x.cdp_value)
      ; ("unknown_datapoints", int x.cdp_unknown_pdps)
      ]

  let get rings rows row col = Fring.peek rings.(col) (rows - row - 1) |> float

  let database = function
    | [||] ->
        array []
    | rings ->
        let rows = Fring.length rings.(0) in
        let cols = Array.length rings in
        array
        @@ Array.to_list
        @@ Array.init rows (fun row ->
               array
               @@ Array.to_list
               @@ Array.init cols (fun col -> get rings rows row col)
           )

  let rra x =
    record
      [
        ("cf", string (cf_type_to_string x.rra_cf))
      ; ("pdp_per_row", int x.rra_pdp_cnt)
      ; ("params", record [("xff", float x.rra_xff)])
      ; ( "cdp_prep"
        , record [("ds", array @@ List.map cdp @@ Array.to_list x.rra_cdps)]
        )
      ; ("database", database x.rra_data)
      ]

  let rrd x =
    record
      [
        ("version", string "0003")
      ; ("step", int64 x.timestep)
      ; ("lastupdate", float x.last_updated)
      ; ("ds", array @@ List.map datasource @@ Array.to_list x.rrd_dss)
      ; ("rra", array @@ List.map rra @@ Array.to_list x.rrd_rras)
      ]
end

let json_to_string rrd = Yojson.to_string (Json.rrd rrd)

module Statefile_latency = struct
  type t = {id: string; latency: float option} [@@deriving rpc]
end
