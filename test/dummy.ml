open Rrd

let _ =
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
   to_file rrd "output.xml"
