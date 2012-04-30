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

open Arrayext
open Listext
open Pervasiveext

(* We're not currently printing any debug data in this module. This is commented out
   so that we can link a standalone binary with this without bringing in logs 

   module D=Debug.Debugger(struct let name="rrd" end)
   open D
*)

type ds_owner = VM of string | Host | SR of string

(** Data source types - see ds datatype *)									   
type ds_type = Absolute | Gauge | Derive with rpc

(** Consolidation function - see RRA datatype *)
type cf_type = CF_Average | CF_Min | CF_Max | CF_Last

(** Container so that we can handle different typed inputs *)
type ds_value_type = VT_Float of float | VT_Int64 of int64 | VT_Unknown with rpc

(* utility *)
let isnan x = match classify_float x with | FP_nan -> true | _ -> false

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
} with rpc 

(** RRA - RRD archive
    This is an archive that holds consolidated data points (CDPs). It 
    defines the type of consolidation that happens (average, max, min or last),
    the number of primary data points (PDPs) that go to make a CDP, and
    the number of CDPs to store. *)

type rra = {
  rra_cf : cf_type;                      (** consolidation function *)
  rra_row_cnt : int;                     (** number of entries to store *)
  rra_pdp_cnt : int;                     (** number of pdps per cdp *)
  rra_xff : float;                       (** proportion of missing pdps at which
					     we mark the cdp as unknown *)
  rra_data: Fring.t array;          (** stored data *)
  rra_cdps : cdp_prep array;             (** scratch area for consolidated datapoint preparation *)
  
  mutable rra_updatehook : (rrd -> int -> unit) option; (** Hook that gets called when an update happens *)
}

(** The container for the DSs. Also specifies the period between pdps *)

and rrd = {
  mutable last_updated: float;           (** Last updated time in seconds *)
  timestep: int64;                       (** Period between PDPs *)
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
    if isnan pdps.(i)
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
(*    debug "start_pdp_offset=%d rra_step_cnt=%d proc_pdp_st=%Ld elapsed_pdp_st=%d\n" start_pdp_offset rra_step_cnt proc_pdp_st elapsed_pdp_st;*)
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
	for i=1 to min (rra_step_cnt-1) (rra.rra_row_cnt) do
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
let process_ds_value ds value interval =
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

let ds_update rrd timestamp values transforms =
  (* Interval is the time between this and the last update *)
  let interval = timestamp -. rrd.last_updated in
  (* Work around the clock going backwards *)
  let interval = if interval < 0. then 5. else interval in

  (* start time (st) and age of the last processed pdp and the currently occupied one *)
  let proc_pdp_st, proc_pdp_age = get_times rrd.last_updated rrd.timestep in
  let occu_pdp_st, occu_pdp_age = get_times timestamp rrd.timestep in

(*  debug "proc_pdp_st: %Ld proc_pdp_age: %f occu_pdp_st: %Ld occu_pdp_age: %f\n" proc_pdp_st proc_pdp_age occu_pdp_st occu_pdp_age;*)

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
  
(*  debug "pre_int, post_int = %f,%f\n" pre_int post_int;*)

  (* We're now done with the last_updated value, so update it *)
  rrd.last_updated <- timestamp;

  (* Calculate the values we're going to store based on the input data and the type of the DS *)
  let v2s = Array.mapi (fun i value -> process_ds_value rrd.rrd_dss.(i) value interval) values in
  (*  debug "Got values: %s\n" (String.concat "," (Array.to_list (Array.mapi (fun i p -> Printf.sprintf "(%s: %f)" rrd.rrd_dss.(i).ds_name p) v2s)));*)

  (* Update the PDP accumulators up until the most recent PDP *)
  Array.iteri
    (fun i value -> 
      let ds=rrd.rrd_dss.(i) in
      if isnan value 
      then ds.ds_unknown_sec <- pre_int 
      else ds.ds_value <- ds.ds_value +. (pre_int *. value /. interval);
(*      debug "New ds value: %f (unknown=%f)\n" ds.ds_value ds.ds_unknown_sec*)) v2s;

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

      (*      debug "Got PDPs: %s\n" (String.concat "," (Array.to_list (Array.map (fun p -> Printf.sprintf "%f" p) pdps)));*)

      rra_update rrd proc_pdp_st elapsed_pdp_st pdps;

      (* Reset the PDP accumulators *)
      Array.iteri (fun i value ->
	let ds = rrd.rrd_dss.(i) in
	if isnan value 
	then (ds.ds_value <- 0.0; ds.ds_unknown_sec <- post_int)
	else (ds.ds_value <- post_int *. value /. interval; ds.ds_unknown_sec <- 0.0))
	v2s;
    end

(** Update the rrd with named values rather than just an ordered array *)
let ds_update_named rrd timestamp valuesandtransforms =
  let identity x = x in
  let ds_names = Array.map (fun ds -> ds.ds_name) rrd.rrd_dss in
(*  Array.iter (fun x -> debug "registered ds name: %s" x) ds_names;
  List.iter (fun (x,_) -> debug "ds value name: %s" x) values; *)
  let ds_values = Array.map (fun name -> try fst (List.assoc name valuesandtransforms) with _ -> VT_Unknown) ds_names in
  let ds_transforms = Array.map (fun name -> try snd (List.assoc name valuesandtransforms) with _ -> identity) ds_names in
  ds_update rrd timestamp ds_values ds_transforms

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
    rra_updatehook=None;
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
	rra_cdps = Array.init (Array.length dss) (fun i -> {cdp_value=0.0; cdp_unknown_pdps=0})
      }) rras;
  } in
  let values = Array.map (fun ds -> ds.ds_last) dss in
  let transforms = Array.make (Array.length values) (fun x -> x) in
  ds_update rrd inittime values transforms;
  rrd

(** Add in a new DS into a pre-existing RRD. Preserves data of all the other archives 
    and fills the new one full of NaNs. Note that this doesn't fill in the CDP values
    correctly at the moment! *)
let rrd_add_ds rrd newds =
  if List.mem newds.ds_name (ds_names rrd) then rrd else 
  let now = Unix.gettimeofday () in
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
	let n = Array.index ds_name (Array.map (fun ds -> ds.ds_name) rrd.rrd_dss) in
	if n = -1 then
		raise (Api_errors.Server_error(Api_errors.invalid_value, ["data-source"; ds_name]))
	else
		{ rrd with
			rrd_dss = Array.remove n rrd.rrd_dss;
			rrd_rras = Array.map (fun rra ->
				{ rra with
					rra_data = Array.remove n rra.rra_data;
					rra_cdps = Array.remove n rra.rra_cdps }) rrd.rrd_rras; }

exception No_RRA_Available

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
  let (last_pdp_time,age) = get_times rrd.last_updated rrd.timestep in
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

let query_named_ds rrd ds_name cf =
	let n = Array.index ds_name (Array.map (fun ds -> ds.ds_name) rrd.rrd_dss) in
	if n = -1 then
		raise (Api_errors.Server_error(Api_errors.invalid_value, ["data-source"; ds_name]))
	else
		let rras = find_best_rras rrd 0 (Some cf) (Int64.of_float (Unix.gettimeofday())) in
		Fring.peek (List.hd rras).rra_data.(n) 0

(******************************************************************************)
(* Marshalling/Unmarshalling functions                                        *)
(******************************************************************************)

(** This is for making an in-memory representation of the xml tree *)
exception Parse_error
type t = El of string * t list | D of string 

(** C# and JS representation of special floats are 'NaN' and 'Infinity' which
    are different from ocaml's native representation. Caml is fortunately more
    forgiving when doing a float_of_string, and can cope with these forms, so
    we make a generic float_to_string function here *)
let f_to_s f = 
  match classify_float f with 
    | FP_normal | FP_subnormal -> Printf.sprintf "%0.4f" f
    | FP_nan -> "NaN"
    | FP_infinite -> if f>0.0 then "Infinity" else "-Infinity"
    | FP_zero -> "0.0"

(** Export data from a bunch of rrds. Specify a prefix per rrd to be
    put onto legend. Note that each rrd *must* have the same timestep
    and have been updated at the same time, and *must* have
    homogeneous rras too. If not, those that dont look like the 1st
    one will be silently dropped. The export format is the rrdtool
    'xport' format. *)
let to_xml output rra_timestep rras first_rra last_cdp_time first_cdp_time start legends =   
  let tag tag next () = 
    Xmlm.output output (`El_start (("",tag),[])); 
    List.iter (fun x -> x ()) next; 
    Xmlm.output output (`El_end) 
  in
  
  let data dat () = Xmlm.output output (`Data dat) in  

  let rec do_data i accum =
    let time = Int64.sub (last_cdp_time) (Int64.mul (Int64.of_int i) rra_timestep) in
    if (time < start) || (i >= first_rra.rra_row_cnt) then (List.rev accum) else
      let values = 
	List.concat 
	  (List.map (fun rra -> 
	    List.map (fun ring -> tag "v" [data (f_to_s (Fring.peek ring i))]) (Array.to_list rra.rra_data)) 
	      rras) in
      do_data (i+1) ((tag "row" ((tag "t" [data (Printf.sprintf "%Ld" time)])::values))::accum)
  in

  let rows = do_data 0 [] in
  let mydata = tag "data" rows in
  
  let meta = tag "meta" [
    tag "start" [data (Printf.sprintf "%Ld" first_cdp_time)];
    tag "step" [data (Printf.sprintf "%Ld" rra_timestep)];
    tag "end" [data (Printf.sprintf "%Ld" last_cdp_time)];
    tag "rows" [data (Printf.sprintf "%d" (List.length rows))];
    tag "columns" [data (Printf.sprintf "%d" (Array.length legends))];
    tag "legend" (List.map (fun x -> tag "entry" [data x]) (Array.to_list legends))] in
  
  Xmlm.output output (`Dtd None);
  tag "xport" [meta; mydata] ()

let to_json rra_timestep rras first_rra last_cdp_time first_cdp_time start legends = 
  let rec do_data i accum =
    let time = Int64.sub (last_cdp_time) (Int64.mul (Int64.of_int i) rra_timestep) in
    if (time < start) || (i >= first_rra.rra_row_cnt) then (List.rev accum) else
      let values = "[" ^ 
	(String.concat "," 
	    (List.concat (List.map (fun rra -> 
	      List.map (fun ring -> f_to_s (Fring.peek ring i)) (Array.to_list rra.rra_data))
			     rras))) ^ "]" in
      do_data (i+1) (("{t:"^(Printf.sprintf "%Ld" time)^",values:"^values^"}")::accum)
  in

  let rows = do_data 0 [] in
  let data = "["^(String.concat "," rows)^"]" in
  
  "{meta: {start:"^(Printf.sprintf "%Ld" first_cdp_time)^
  ",step:"^(Printf.sprintf "%Ld" rra_timestep)^
  ",end:"^(Printf.sprintf "%Ld" last_cdp_time)^
  ",rows:"^(Printf.sprintf "%d" (List.length rows))^
  ",columns:"^(Printf.sprintf "%d" (Array.length legends))^
  ",legend:["^(String.concat "," (List.map (fun x -> "\"" ^ x ^ "\"") (Array.to_list legends)))^"]},"^
    "data:"^data^"}"


let real_export marshaller prefixandrrds start interval cfopt =
  let first_rrd = snd (List.hd prefixandrrds) in

  let pdp_interval = Int64.to_int (Int64.div interval first_rrd.timestep) in

  (* Sanity - make sure the RRDs are homogeneous *)
  let prefixandrrds = List.filter (fun (prefix,rrd) -> rrd.timestep = first_rrd.timestep) prefixandrrds in

  let rras = 
    (List.map
	(fun (prefix,rrd) ->
	  (* Find the rrds that satisfy the requirements *)
	  find_best_rras rrd pdp_interval cfopt start) prefixandrrds) in

  let legends = Array.concat (List.map2 (fun (prefix,rrd) rras ->
    let ds_legends = Array.map (fun ds -> prefix^ds.ds_name) rrd.rrd_dss in
    let ds_legends_with_cf_prefix = Array.concat
      (List.map (fun rra -> Array.map (fun name -> (cf_type_to_string rra.rra_cf)^":"^name) ds_legends) rras)
    in ds_legends_with_cf_prefix) prefixandrrds rras) in
  
  let rras = List.flatten rras in
  let first_rra = List.hd rras in
  
  (* More sanity - make sure our RRAs are homogeneous *)
(*  debug "Got %d rras..." (List.length rras);
  List.iter (fun rra -> debug "pdp_cnt=%d row_cnt=%d" rra.rra_pdp_cnt rra.rra_row_cnt) rras;*)

  let rras = List.filter (fun rra -> rra.rra_pdp_cnt=first_rra.rra_pdp_cnt && rra.rra_row_cnt = first_rra.rra_row_cnt) rras in
(*  debug "Got %d rras..." (List.length rras);*)
  (* The following timestep is that of the archive *)
  let rra_timestep = (Int64.mul first_rrd.timestep (Int64.of_int first_rra.rra_pdp_cnt)) in
  
  (* Get the last and first times of the CDPs to be returned *)
  let (last_cdp_time,age) = get_times first_rrd.last_updated rra_timestep in
  let (first_cdp_time_minus_one,age) = get_times (Int64.to_float start) rra_timestep in
  let first_cdp_time = Int64.add first_cdp_time_minus_one rra_timestep in

  marshaller rra_timestep rras first_rra last_cdp_time first_cdp_time start legends
  

let export ?(json=false) prefixandrrds start interval cfopt =
  if json then
    real_export to_json prefixandrrds start interval cfopt
  else
    let buffer = Buffer.create 10 in
    let output = Xmlm.make_output (`Buffer buffer) in
    real_export (to_xml output) prefixandrrds start interval cfopt;
    Buffer.contents buffer

let from_xml input =
  (* build tree in memory first *)
  let tree = 
    let data d = D d in
    let el ((prefix,tag_name),attr) children = El (tag_name, children) in
    match Xmlm.peek input with
    | `Dtd _ -> snd (Xmlm.input_doc_tree ~data ~el input)
    | _ -> Xmlm.input_tree ~data ~el input
  in

  let kvs elts = List.filter_map 
    (function | El(key,[D value]) -> Some (key,value) | _ -> None) elts in

  let find key elts =
    match List.find (function | El(k,elts) -> k=key | _ -> false) elts with
      | El(k,elts) -> elts
      | _ -> failwith "Can't happen!"
  in

  let process_ds elts =
    let kvs = kvs elts in
    {ds_name=List.assoc "name" kvs;
     ds_ty=(match List.assoc "type" kvs with | "GAUGE" -> Gauge | "ABSOLUTE" -> Absolute | "DERIVE" -> Derive | _ -> failwith "Bad format");
     ds_mrhb=float_of_string (List.assoc "minimal_heartbeat" kvs);
     ds_min=float_of_string (List.assoc "min" kvs);
     ds_max=float_of_string (List.assoc "max" kvs);
     ds_last=VT_Unknown; (* float_of_string (List.assoc "last_ds" kvs);*)
     ds_value=float_of_string (List.assoc "value" kvs);
     ds_unknown_sec=float_of_string (List.assoc "unknown_sec" kvs)}
  in

  let process_rra elts =

    let process_params elts =
      kvs elts in

    let process_cdp_prep elts =
      let kvs = kvs elts in
      {cdp_value=float_of_string (List.assoc "value" kvs);
       cdp_unknown_pdps=int_of_string (List.assoc "unknown_datapoints" kvs)}
    in

    let process_cdp_preps elts =
      let cdps =
	List.filter_map (function | El ("ds",elts) -> Some (process_cdp_prep elts) | _ -> None) elts in
      cdps
    in

    let database =
      let elts = find "database" elts in
      let rows = List.length elts in
      let cols = match List.hd elts with | El("row",cols) -> List.length cols | _ -> -1 in
      let db = Array.init cols (fun i -> Fring.make rows nan) in

      let data = Array.of_list 
	(List.map (function 
	  | El("row",cols) -> 
	      Array.of_list (List.map 
				(function 
				  | El ("v",[D x]) -> x 
				  | _ -> raise Parse_error) cols) 
	  | _ -> raise Parse_error) elts) in

      for i=0 to cols-1 do
	for j=0 to rows-1 do
	  Fring.push db.(i) (float_of_string data.(j).(i))
	done
      done;
      db
    in 
    let cdps = 
      let elts = find "cdp_prep" elts in
      process_cdp_preps elts
    in
    let main_kvs = kvs elts in
    let params = 
      let elts = find "params" elts in 
      process_params elts
    in
    {rra_cf = (match List.assoc "cf" main_kvs with | "AVERAGE" -> CF_Average | "MIN" -> CF_Min | "MAX" -> CF_Max | "LAST" -> CF_Last | _ -> raise Parse_error);
     rra_row_cnt = Fring.length database.(0);
     rra_pdp_cnt = int_of_string (List.assoc "pdp_per_row" main_kvs);
     rra_xff = float_of_string (List.assoc "xff" params);
     rra_data = database;
     rra_cdps = Array.of_list cdps;
     rra_updatehook = None
    }
  in
  match tree with
    | El ("rrd",elts) ->
	let dss = List.filter_map (function El ("ds",elts) -> Some (process_ds elts) | _ -> None) elts in
	let rras = List.filter_map (function El ("rra",elts) -> Some (process_rra elts) | _ -> None) elts in 	
	let kvs = kvs elts in
	let rrd = {last_updated=float_of_string (List.assoc "lastupdate" kvs);
	timestep=Int64.of_string (List.assoc "step" kvs);
	rrd_dss=Array.of_list dss;
	rrd_rras=Array.of_list rras} in
	(* Purge any repeated data sources from the RRD *)
	let ds_names = ds_names rrd in
	let ds_names_set = Listext.List.setify ds_names in
	let ds_name_counts = List.map (fun name ->
		let (x,y) = List.partition ((=) name) ds_names in
		(name,List.length x)) ds_names_set
	in
	let removals_required = List.filter (fun (_,x) -> x > 1) ds_name_counts in
	List.fold_left (fun rrd (name,n) -> 
		(* Remove n-1 lots of this data source *)
		let rec inner rrd n =
			if n=1 
			then rrd
			else inner (rrd_remove_ds rrd name) (n-1)
		in
		inner rrd n) rrd removals_required
    | _ -> failwith "Bad xml!"

(** Repeatedly call [f string] where [string] contains a fragment of the RRD XML *)
let xml_to_fd rrd fd =
	(* We use an output channel for Xmlm-compat buffered output. Provided we flush
	   at the end we should be safe. *)  
	let with_out_channel_output fd f = 
		let oc = Unix.out_channel_of_descr fd in
		finally
		(fun () ->
			let output = Xmlm.make_output (`Channel oc) in
			f output
		)
		(fun () -> flush oc) in

  let tag n next output = 
    Xmlm.output output (`El_start (("",n),[])); 
    List.iter (fun x -> x output) next; 
    Xmlm.output output (`El_end) 
  in
  let tags n next output =
    List.iter (fun next -> tag n next output) next
  in
  let data dat output = Xmlm.output output (`Data dat) in

  let do_dss ds_list =
    [tags "ds" (List.map (fun ds -> [
      tag "name" [data ds.ds_name];
      tag "type" [data (match ds.ds_ty with Gauge -> "GAUGE" | Absolute -> "ABSOLUTE" | Derive -> "DERIVE")];
      tag "minimal_heartbeat" [data (f_to_s ds.ds_mrhb)];
      tag "min" [data (f_to_s ds.ds_min)];
      tag "max" [data (f_to_s ds.ds_max)];
      tag "last_ds" [data (match ds.ds_last with VT_Float x -> f_to_s x | VT_Int64 x -> Printf.sprintf "%Ld" x | _ -> "0.0")];
      tag "value" [data (f_to_s ds.ds_value)];
      tag "unknown_sec" [data (Printf.sprintf "%d" (int_of_float ds.ds_unknown_sec))];
    ]) ds_list)] in

  let do_rra_cdps cdp_list =
    [tags "ds" (List.map (fun cdp -> [
      tag "primary_value" [data "0.0"];
      tag "secondary_value" [data "0.0"];
      tag "value" [data (f_to_s cdp.cdp_value)];
      tag "unknown_datapoints" [data (Printf.sprintf "%d" cdp.cdp_unknown_pdps)]
    ]) cdp_list)] in

  let do_database rings =
    if Array.length rings = 0 then [] else
      let rows = Fring.length rings.(0) in
      let cols = Array.length rings in
      let db = Array.to_list 
	(Array.init rows (fun row ->
	  [tags "v" 
	      (Array.to_list (Array.init cols 
				 (fun col ->
				   [ data (f_to_s (Fring.peek rings.(col) 
						      (rows-row-1))) ])))]))
      in [tags "row" db]
  in
  
  let do_rras rra_list =
    [tags "rra" (List.map (fun rra -> [
      tag "cf" [data (match rra.rra_cf with CF_Average -> "AVERAGE" | CF_Max -> "MAX" | CF_Min -> "MIN" | CF_Last -> "LAST")];
      tag "pdp_per_row" [data (string_of_int rra.rra_pdp_cnt)];
      tag "params" [tag "xff" [data (f_to_s rra.rra_xff)]];
      tag "cdp_prep" (do_rra_cdps (Array.to_list rra.rra_cdps));
      tag "database" (do_database rra.rra_data)]) rra_list)]
  in
	
	with_out_channel_output fd
	(fun output ->
		Xmlm.output output (`Dtd None);
		tag "rrd"
		(List.concat
			[[tag "version" [data "0003"];
			tag "step" [data (Int64.to_string rrd.timestep)];
			tag "lastupdate" [data (Printf.sprintf "%Ld" (Int64.of_float (rrd.last_updated)))]];
			do_dss (Array.to_list rrd.rrd_dss);
			do_rras (Array.to_list rrd.rrd_rras);
		]) output
	)

let json_to_fd rrd fd =
	let write x = if Unix.write fd x 0 (String.length x) <> String.length x then failwith "json_to_fd: short write" in

  let do_dss ds_list =
    "ds:["^(String.concat "," (List.map (fun ds -> 
      "{name:\""^ds.ds_name^"\",type:\""^(match ds.ds_ty with Gauge -> "GAUGE" | Absolute -> "ABSOLUTE" | Derive -> "DERIVE")^
	"\",minimal_heartbeat:" ^(f_to_s ds.ds_mrhb)^",min:"^(f_to_s ds.ds_min)^
	",max:"^(f_to_s ds.ds_max)^",last_ds:0.0,value:0.0,unknown_sec:0}") ds_list))^"]"
  in

  let do_rra_cdps cdp_list =
    "ds:["^(String.concat "," (List.map (fun cdp -> 
      "{primary_value:0.0,secondary_value:0.0,value:"^(f_to_s cdp.cdp_value)^
	",unknown_datapoints:"^(Printf.sprintf "%d" cdp.cdp_unknown_pdps)^"}") cdp_list))^"]"
  in

  let do_database rings =
    if Array.length rings = 0 then "[]" else
      let rows = Fring.length rings.(0) in
      let cols = Array.length rings in
      "["^(String.concat "," 
	      (Array.to_list
		  (Array.init rows (fun row ->
		    "["^(String.concat "," 
			    (Array.to_list 
				(Array.init cols 
				    (fun col ->
				      f_to_s 
					(Fring.peek rings.(col)
					    (rows-row-1))))))^"]"))))^"]"
  in
  
  let do_rras rra_list =
    "rra:[{" ^ (String.concat "},{" (List.map (fun rra -> 
      "cf:\""^(cf_type_to_string rra.rra_cf)^
	"\",pdp_per_row:"^(string_of_int rra.rra_pdp_cnt)^",params:{xff:"^(f_to_s rra.rra_xff)^"},cdp_prep:{"^(do_rra_cdps (Array.to_list rra.rra_cdps))^
	"},database:"^(do_database rra.rra_data)) rra_list))^"}]"
  in
	
	write "{version: \"0003\",step:";
	write (Int64.to_string rrd.timestep);
	write ",lastupdate:";
	write (f_to_s rrd.last_updated);
	write ",";
	write (do_dss (Array.to_list rrd.rrd_dss));
	write ",";
	write (do_rras (Array.to_list rrd.rrd_rras)^"}") (* XXX need to split this *)

let iter_to_string_list f x = 
   let acc = ref [] in
   f x (fun string -> acc := string :: !acc);
   List.rev !acc

(*
(* XXX: we copy and return to avoid holding locks: this is why we aren't exposing
   an iter/fold interface here. It would be better to copy the original (compact)
   data then top copy the expanded version. *)
let to_bigbuffer ?(json=false) rrd =
  let b = Bigbuffer.make () in
  begin
	if json 
	then Bigbuffer.append_string b (to_json rrd) (* XXX: might be too big *)
  	else iter_over_xml rrd (Bigbuffer.append_string b)
  end;
  b
*)

let to_fd ?(json=false) rrd fd = (if json then json_to_fd else xml_to_fd) rrd fd


(** WARNING WARNING: Do not call the following function from within xapi! *)
let text_export rrd grouping =
  for rra_i=0 to Array.length rrd.rrd_rras - 1 do
    let rra = rrd.rrd_rras.(rra_i) in
    let start = rrd.last_updated -. (Int64.to_float (Int64.mul (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt)) rrd.timestep)) in
    Printf.printf "start=%f\n" start;
    let rra_timestep = (Int64.mul rrd.timestep (Int64.of_int rra.rra_pdp_cnt)) in
    
    (* Get the last and first times of the CDPs to be returned *)
    let (last_cdp_time,age) = get_times rrd.last_updated rra_timestep in

    let time i = Int64.sub (last_cdp_time) (Int64.mul (Int64.of_int i) rra_timestep) in

    for j=0 to Array.length rrd.rrd_dss - 1 do
      Printf.printf "Doing ds: %s\n" rrd.rrd_dss.(j).ds_name;
      let oc = open_out (Printf.sprintf "rrd_data_%s_%s_%Ld.dat" rrd.rrd_dss.(j).ds_name (cf_type_to_string rra.rra_cf) (Int64.mul (Int64.of_int (rra.rra_pdp_cnt* rra.rra_row_cnt)) rrd.timestep)) in
      let rec do_data i accum =
	if (time i < Int64.of_float start) || (i >= rra.rra_row_cnt) then (List.rev accum) else
	  do_data (i+1) ((time i, Fring.peek rra.rra_data.(j)  i)::accum)
      in
      let data = do_data 0 [] in
      List.iter (fun (t,d) -> if not (isnan d) then Printf.fprintf oc "%Ld %f\n" t d) data;
      close_out oc
    done
  done


(*
let _ =
   let rra = rra_create CF_Average 100 1 0.5 in
   let rra2 = rra_create CF_Average 100 10 0.5 in
   let rra3 = rra_create CF_Average 100 100 0.5 in
   let rra4 = rra_create CF_Average 100 1000 0.5 in
   let ds = ds_create "foo" Gauge ~mrhb:10.0 (VT_Float 0.0) in
   let ds2 = ds_create "bar" Gauge ~mrhb:10.0 (VT_Float 0.0) in
   let rrd = rrd_create [|ds; ds2|] [|rra; rra2; rra3; rra4 |] 1L 1000000000.0 in
   let v = VT_Float 1.0 in
   let v2 = VT_Float 0.5 in
   for i=1 to 1000 do
    let t = 1000000000.0 +. 0.7 *. (float_of_int i) in
    let v1 = VT_Float (0.5 +. 0.5 *. sin ( t /. 10.0 )) in
    let v2 = VT_Float (0.5 +. 0.5 *. cos ( t /. 20.0 )) in
    ds_update rrd t [|v1; v2|]
   done;
   let rra = find_best_rra rrd CF_Average 1000000650L in
   Printf.printf "found rra: pdp_cnt=%d row_cnt=%d" rra.rra_pdp_cnt rra.rra_row_cnt
  let oc = open_out "rrd.xml" in
  let s = to_string rrd in
  Printf.fprintf oc "%s" s;
*)
