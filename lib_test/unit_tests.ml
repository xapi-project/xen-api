
open OUnit
open Rrd

let compareish f1 f2 =
  let result = (Utils.isnan f1 && Utils.isnan f2) ||
               (f1 = f2) || 
               (abs_float (f1 -. f2) /. f1) < 0.001 in
  result

let compareish2 f1 f2 =
  let result = (Utils.isnan f1 && Utils.isnan f2) ||
               (f1 = f2) || 
               (abs_float (f1 -. f2)) < 0.0001 in
  result

let assert_ds_equal d1 d2 =
  assert (d1.ds_name = d2.ds_name);
  assert (d1.ds_ty = d2.ds_ty );
  assert (compareish d1.ds_min d2.ds_min);
  assert (compareish d1.ds_max d2.ds_max);
  assert (compareish d1.ds_mrhb d2.ds_mrhb);
  (*  assert (d1.ds_last = d2.ds_last);*)
  assert (compareish d1.ds_value d2.ds_value);
  assert (compareish d1.ds_unknown_sec d2.ds_unknown_sec)

let assert_dss_equal d1s d2s =
  let d1s = Array.to_list d1s in
  let d2s = Array.to_list d2s in
  List.iter2 assert_ds_equal d1s d2s

let assert_cdp_prep_equal c1 c2 =
  assert (compareish c1.cdp_value c2.cdp_value);
  assert (c1.cdp_unknown_pdps = c2.cdp_unknown_pdps)

let assert_fring_equal f1 f2 =
  for i=0 to f1.Fring.size - 1 do
    assert (compareish2 (Fring.peek f1 i) (Fring.peek f2 i))
  done

let assert_rra_equal a1 a2 =
  assert (a1.rra_cf = a2.rra_cf);
  assert (a1.rra_row_cnt = a2.rra_row_cnt);
  assert (a1.rra_pdp_cnt = a2.rra_pdp_cnt);
  assert (compareish a1.rra_xff a2.rra_xff);
  List.iter2 assert_cdp_prep_equal (Array.to_list a1.rra_cdps) (Array.to_list a2.rra_cdps);
  List.iter2 assert_fring_equal (Array.to_list a1.rra_data) (Array.to_list a2.rra_data)

let assert_rras_equal a1s a2s =
  List.iter2 assert_rra_equal (Array.to_list a1s) (Array.to_list a2s)

let assert_rrds_equal r1 r2 =
  assert (compareish r1.last_updated r2.last_updated);
  assert (r1.timestep=r2.timestep);
  assert_dss_equal r1.rrd_dss r2.rrd_dss;
  assert_rras_equal r1.rrd_rras r2.rrd_rras

let test_marshal rrd =
  "Save to disk" >:: (fun () ->
      Rrd_unix.to_file rrd "/tmp/output.xml")

let test_unmarshal rrd =
  "Restore from disk" >:: (fun () ->
      Rrd_unix.to_file rrd "/tmp/output.xml";
      let rrd' = Rrd_unix.of_file "/tmp/output.xml" in
      assert_rrds_equal rrd rrd')

let create_dummy_data () =
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

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test RRD library";

  let rrd = create_dummy_data () in

  let suite = "rrd" >::: [
      "read_write" >::: [test_marshal rrd; test_unmarshal rrd]
    ] in
  run_test_tt ~verbose:!verbose suite
