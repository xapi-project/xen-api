module Fring = Rrd.Fring

module Cb = Crowbar

(* cast double-precision float to single-precision *)
let castd2s min max =
  let _data = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1 in
  Bigarray.Array1.set _data 0 min;
  let min = Bigarray.Array1.get _data 0 in
  Bigarray.Array1.set _data 0 max;
  let max = Bigarray.Array1.get _data 0 in
  min, max

let in_range min max values =
  let between value =
    if Rrd.Utils.isnan value then
      true
    else if min > value then (
      Printf.printf "value (%f) lower than min (%f); " value min;
      false
    ) else if max < value then (
      Printf.printf "value (%f) higher than max (%f); " value max;
      false
    ) else
      true
    in
  Cb.check @@ List.for_all between values

let fring_to_list fring =
  Array.to_list @@ Fring.get fring

(* Checks if all the values in the archives are within the limits set by the data sources
 * Each archive (RRA) has a ring for each datasource (DS) *)
let test_ranges rrd =
  let open Rrd in
  let in_range_fring ds fring =
    in_range ds.ds_min ds.ds_max (fring_to_list fring) in
  let in_range_rra dss rra =
    List.iter2 in_range_fring dss (Array.to_list rra.rra_data) in
  List.iter (in_range_rra @@ Array.to_list rrd.rrd_dss) (Array.to_list rrd.rrd_rras)

let same_input_type vf vf' =
  let open Rrd in
  match vf vf' with
  | VT_Unknown, VT_Unknown -> true
  | VT_Int64 _, VT_Int64 _ -> true
  | VT_Float _, VT_Float _ -> true
  | _ -> false

let cf =
  Cb.choose [
    Cb.const Rrd.CF_Average;
    Cb.const Rrd.CF_Min;
    Cb.const Rrd.CF_Max;
    Cb.const Rrd.CF_Last;
  ]

let rra =
  Cb.map [cf] (fun consolidation ->
    Rrd.rra_create consolidation 10 1 0.5
  )

let ds_value =
  Cb.choose [
    Cb.const Rrd.VT_Unknown;
    Cb.map Cb.[int64] (fun v -> Rrd.VT_Int64 v);
  ]

(* Cast generated floats for min and max values to single-precision.
   This is done because all values that get into the RRAs get converted
   to single precision as well. In the case that one of these two values
   cannot be represented with single-recision the values will clamp to
   be infinity, leading to a comparison where the clamped value is out of range.
   This is not an issue when normally running as there are no data sources which
   such outrageous limits.
   *)
let ds =
  let open Rrd in
  Cb.choose [
    Cb.map Cb.[ds_value; float; float] (fun v min max ->
      let min, max = castd2s min max in
      Cb.guard (min < max); ds_create "derive" ~min ~max Derive v);
    Cb.map Cb.[ds_value; float; float] (fun v min max ->
      let min, max = castd2s min max in
      Cb.guard (min < max); ds_create "absolute" ~min ~max Absolute v);
    Cb.map Cb.[ds_value; float; float] (fun v min max ->
      let min, max = castd2s min max in
      Cb.guard (min < max); ds_create "gauge" ~min ~max Gauge v)
  ]

let rrd =
  Cb.map Cb.[list1 int64; rra; ds] (fun values rra ds ->

    let open Rrd in
    let init_time = 0. in

    let rrd = rrd_create [|ds|] [|rra|] 5L init_time in

    let id = fun x -> x in

    List.iteri (fun i v ->
      let t = 5. *. (init_time +. float_of_int i) in
      ds_update rrd t [|VT_Int64 v|] [|id|] (i = 0)
    ) values;
    rrd
  )
let () =
  Cb.add_test ~name:"Out-of-bounds rates in archives" [rrd] @@ fun rrd ->
    test_ranges rrd
