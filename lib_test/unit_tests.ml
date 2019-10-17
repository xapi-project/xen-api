open Rrd

(* Default alcotest checker fails when comparing NaNs and Infinities,
 * we need leeway to account for loss of precision during serialization. *)
let float eps =
  let same x y = Float.equal x y ||
                 Float.abs (x -. y) <= eps
  in
  Alcotest.testable Fmt.float same

(* pick between absolute or relative tolerance of a number *)
let tolerance x =
  max 1e-4 (abs_float x *. 1e-12)

let compare_float message x y =
  Alcotest.check (float @@ tolerance x) message x y

let assert_ds_equal d1 d2 =
  Alcotest.(check string) __LOC__ d1.ds_name d2.ds_name;
  assert (d1.ds_ty = d2.ds_ty);
  compare_float __LOC__ d1.ds_min d2.ds_min;
  compare_float __LOC__ d1.ds_max d2.ds_max;
  compare_float __LOC__ d1.ds_mrhb d2.ds_mrhb;
  compare_float __LOC__ d1.ds_value d2.ds_value;
  compare_float __LOC__ d1.ds_unknown_sec d2.ds_unknown_sec

let assert_dss_equal d1s d2s =
  let d1s = Array.to_list d1s in
  let d2s = Array.to_list d2s in
  List.iter2 assert_ds_equal d1s d2s

let assert_cdp_prep_equal c1 c2 =
  compare_float __LOC__ c1.cdp_value c2.cdp_value;
  Alcotest.(check int) __LOC__ c1.cdp_unknown_pdps c2.cdp_unknown_pdps

let assert_fring_equal f1 f2 =
  for i=0 to f1.Fring.size - 1 do
    let peek1 = Fring.peek f1 i in
    let peek2 = Fring.peek f2 i in
    Alcotest.check (float @@ tolerance peek1) "FRing value" peek1 peek2;
  done

let assert_rra_equal a1 a2 =
  assert (a1.rra_cf = a2.rra_cf);
  Alcotest.(check int) __LOC__ a1.rra_row_cnt a2.rra_row_cnt;
  Alcotest.(check int) __LOC__ a1.rra_pdp_cnt a2.rra_pdp_cnt;
  compare_float __LOC__ a1.rra_xff a2.rra_xff;
  List.iter2 assert_cdp_prep_equal (Array.to_list a1.rra_cdps) (Array.to_list a2.rra_cdps);
  List.iter2 assert_fring_equal (Array.to_list a1.rra_data) (Array.to_list a2.rra_data)

let assert_rras_equal a1s a2s =
  List.iter2 assert_rra_equal (Array.to_list a1s) (Array.to_list a2s)

let assert_rrds_equal r1 r2 =
  compare_float __LOC__ r1.last_updated r2.last_updated;
  Alcotest.(check int64) __LOC__ r1.timestep r2.timestep;
  assert_dss_equal r1.rrd_dss r2.rrd_dss;
  assert_rras_equal r1.rrd_rras r2.rrd_rras

let in_range min max values =
  let between value =
    if not (Utils.isnan value) then (
      Alcotest.(check bool) (Printf.sprintf "value (%f) higher than min (%f); " value min) true (min <= value);
      Alcotest.(check bool) (Printf.sprintf "value (%f) ≤ max (%f); " value max) true (max >= value)
    ) in
  List.iter between values

let fring_to_list fring =
  Array.to_list @@ Fring.get fring

(* Checks if all the values in the archives are within the limits set by the data sources
 * Each archive (RRA) has a ring for each datasource (DS) *)
let test_ranges rrd () =
  let in_range_fring ds fring =
    in_range ds.ds_min ds.ds_max (fring_to_list fring) in
  let in_range_rra dss rra =
    List.iter2 in_range_fring dss (Array.to_list rra.rra_data) in
  let range_is_not_empty ds =
    Alcotest.(check bool) (Printf.sprintf "min (%f) < max (%f); " ds.ds_min ds.ds_max) true (ds.ds_min < ds.ds_max)
  in

  Array.iter range_is_not_empty rrd.rrd_dss;
  List.iter (in_range_rra @@ Array.to_list rrd.rrd_dss) (Array.to_list rrd.rrd_rras)

let temp_rrd ~json () =
  let extension = match json with
    | true -> ".json"
    | false -> ".xml"
  in
  Filename.temp_file "rrd-" extension

let test_marshall rrd ~json () =
  let filename = temp_rrd ~json () in
  Rrd_unix.to_file ~json rrd filename;
  Unix.unlink filename

let test_marshall_unmarshall rrd () =
  let filename = temp_rrd ~json:false () in
  Rrd_unix.to_file rrd filename;
  let rrd' = Rrd_unix.of_file filename in
  assert_rrds_equal rrd rrd';
  Unix.unlink filename

let gauge_rrd =
  let rra = rra_create CF_Average 100 1 0.5 in
  let rra2 = rra_create CF_Average 100 10 0.5 in
  let rra3 = rra_create CF_Average 100 100 0.5 in
  let rra4 = rra_create CF_Average 100 1000 0.5 in
  let ds = ds_create "foo" Gauge ~mrhb:10.0 (VT_Float 0.0) in
  let ds2 = ds_create "bar" Gauge ~mrhb:10.0 (VT_Float 0.0) in
  let ds3 = ds_create "baz" Gauge ~mrhb:10.0 (VT_Float 0.0) in
  let ds4 = ds_create "boo" Gauge ~mrhb:10.0 (VT_Float 0.0) in
  let rrd = rrd_create [|ds; ds2; ds3; ds4|] [|rra; rra2; rra3; rra4 |] 1L 1000000000.0 in
  let id = fun x -> x in
  for i=1 to 100000 do
    let t = 1000000000.0 +. 0.7 *. (float_of_int i) in
    let v1 = VT_Float (0.5 +. 0.5 *. sin ( t /. 10.0 )) in
    let v2 = VT_Float (1.5 +. 0.5 *. cos ( t /. 80.0 )) in
    let v3 = VT_Float (3.5 +. 0.5 *. sin ( t /. 700.0 )) in
    let v4 = VT_Float (6.5 +. 0.5 *. cos ( t /. 5000.0 )) in
    ds_update rrd t [|v1; v2; v3; v4|] [| id; id; id; id |] false
  done;
  rrd

let ca_322008_rrd =
  let init_time = 0. in

  let rra1 = rra_create CF_Average 100 1 0.5 in
  let rra2 = rra_create CF_Min     100 1 0.5 in
  let rra3 = rra_create CF_Max     100 1 0.5 in
  let ds = ds_create "even or zero" Derive ~min:0. (VT_Int64 0L) in

  let rrd = rrd_create [|ds|] [|rra1; rra2; rra3|] 5L init_time in

  let id = fun x -> x in
  for i=1 to 100000 do
    let t = init_time +. float_of_int i in
    let t64 = Int64.of_float t in
    let v = VT_Int64 (Int64.mul t64 (Int64.rem t64 2L)) in
    ds_update rrd t [|v|] [|id|] false
  done;
  rrd


let ca_329043_rrd_1 =
  let init_time = 0. in

  let rra1 = rra_create CF_Average 3 1 0.5 in
  let rra2 = rra_create CF_Min     3 1 0.5 in
  let rra3 = rra_create CF_Max     3 1 0.5 in
  let ds = ds_create "derive_with_min" ~min:0. ~max:1. Derive VT_Unknown in

  let rrd = rrd_create [|ds|] [|rra1; rra2; rra3|] 5L init_time in

  let id = fun x -> x in

  let time_value_of_i i =
    let t = 5. *. (init_time +. float_of_int i) in
    if i = 1 then
      t, VT_Int64 0L
    else
      t, VT_Int64 Int64.(of_float t)
  in
  for i = 0 to 4 do
    let t, v = time_value_of_i i in
    ds_update rrd t [|v|] [|id|] (i = 0);
  done;
  rrd

let create_rrd values min max =

    let init_time = 0. in
    let rows = 2 in

    let rra1 = rra_create CF_Average rows 1 0.5 in
    let rra2 = rra_create CF_Min     rows 1 0.5 in
    let rra3 = rra_create CF_Max     rows 1 0.5 in
    let rra4 = rra_create CF_Last    rows 1 0.5 in
    let ds1 = ds_create "derive" ~min ~max Derive VT_Unknown in
    let ds2 = ds_create "absolute" ~min ~max Derive VT_Unknown in
    let ds3 = ds_create "gauge" ~min ~max Derive VT_Unknown in

    let rrd = rrd_create [|ds1; ds2; ds3|] [|rra1; rra2; rra3; rra4|] 5L init_time in

    let id = fun x -> x in

    List.iteri (fun i v ->
      let t = 5. *. (init_time +. float_of_int i) in
      ds_update rrd t [|VT_Int64 v|] [|id; id; id; id|] (i = 0)
    ) values;
    rrd

let ca_329043_rrd_2 = create_rrd [-3710420213458133667L; -4382108469022348614L] (-115833951388699606673086965578224992861890232359671476890007240704.000000) (-13815257.710330)

let test_ca_322008 () =
  let rrd = ca_322008_rrd in

  (* Check against the maximum reasonable value of this series,
   * the time in seconds when it was last updated, setting max
   * value may cause the bug to not trigger *)
  let in_range_fring ds fring =
    in_range ds.ds_min rrd.last_updated (fring_to_list fring) in
  let in_range_rra dss rra =
    List.iter2 in_range_fring dss (Array.to_list rra.rra_data) in
  List.iter (in_range_rra @@ Array.to_list rrd.rrd_dss) @@ Array.to_list rrd.rrd_rras


let rrd_suite rrd = [
  "Save xml to disk", `Quick, test_marshall ~json:false rrd;
  "Save json to disk", `Quick, test_marshall ~json:true rrd;
  (* there is no json deserializer implementation *)
  "Save and restore from disk", `Quick, test_marshall_unmarshall rrd;
  "Values in range",       `Quick, test_ranges rrd;
]

let regression_suite = [
  "CA-322008", `Quick, test_ca_322008;
  "CA-329043 (1)", `Quick, test_ranges ca_329043_rrd_1;
  "CA-329043 (2)", `Quick, test_ranges ca_329043_rrd_2;
]

let () =
  Alcotest.run "Test RRD library" [
    "Gauge RRD", rrd_suite gauge_rrd;
    "RRD for CA-322008", rrd_suite ca_322008_rrd;
    "RRD for CA-329043", rrd_suite ca_329043_rrd_1;
    "Regressions", regression_suite;
  ]
