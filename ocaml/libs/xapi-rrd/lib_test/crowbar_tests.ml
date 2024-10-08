module Fring = Rrd.Fring
module Cb = Crowbar

(* cast double-precision floats to single-precision and return them in
   ascending order *)
let castd2s x y =
  let _data = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1 in
  _data.{0} <- x ;
  let x = _data.{0} in
  _data.{0} <- y ;
  let y = _data.{0} in
  if x > y then
    (y, x)
  else
    (x, y)

let in_range min max values =
  let between value =
    if Rrd.Utils.isnan value then
      true
    else if min > value then
      Cb.fail (Printf.sprintf "value (%f) lower than min (%f); " value min)
    else if max < value then
      Cb.fail (Printf.sprintf "value (%f) higher than max (%f); " value max)
    else
      true
  in
  Cb.check @@ Array.for_all between values

(* Checks if all the values in the archives are within the limits set by the data sources
 * Each archive (RRA) has a ring for each datasource (DS) *)
let test_ranges rrd =
  let open Rrd in
  let in_range_fring ds fring =
    in_range ds.ds_min ds.ds_max (Fring.get fring)
  in
  let in_range_rra dss rra = Array.iter2 in_range_fring dss rra.rra_data in
  Array.iter (in_range_rra rrd.rrd_dss) rrd.rrd_rras

let cf =
  Cb.choose
    [
      Cb.const Rrd.CF_Average
    ; Cb.const Rrd.CF_Min
    ; Cb.const Rrd.CF_Max
    ; Cb.const Rrd.CF_Last
    ]

let rra =
  Cb.map [cf] (fun consolidation -> Rrd.rra_create consolidation 10 1 0.5)

let ds_value =
  Cb.choose
    [Cb.const Rrd.VT_Unknown; Cb.map Cb.[int64] (fun v -> Rrd.VT_Int64 v)]

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
  let ds_type = Cb.(choose [const Derive; const Absolute; const Gauge]) in
  Cb.(
    map [ds_value; float; float; ds_type] (fun v x y typ ->
        let min, max = castd2s x y in
        ds_create (ds_type_to_string typ) ~min ~max typ v
    )
  )

let rrd =
  Cb.(map [list1 int64; rra; ds]) (fun values rra ds ->
      let open Rrd in
      let init_time = 0. in

      let rrd = rrd_create [|ds|] [|rra|] 5L init_time in

      List.iteri
        (fun i v ->
          let t = 5. *. (init_time +. float_of_int i) in
          ds_update rrd t [|VT_Int64 v|] [|Identity|] (i = 0)
        )
        values ;
      rrd
  )

let () = Cb.add_test ~name:"Out-of-bounds rates in archives" [rrd] test_ranges
